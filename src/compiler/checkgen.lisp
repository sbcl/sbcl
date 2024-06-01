;;;; This file implements type check generation. This is a phase that
;;;; runs at the very end of IR1. If a type check is too complex for
;;;; the back end to directly emit in-line, then we transform the check
;;;; into an explicit conditional using TYPEP.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

(defun weaken-integer-type (type &key range-only)
  ;; FIXME: Our canonicalization isn't quite ideal for this. We get
  ;; types such as:
  ;;
  ;;      (OR (AND (SATISFIES FOO) (INTEGER -100 -50))
  ;;          (AND (SATISFIES FOO) (INTEGER 100 200)))
  ;;
  ;; here, and weakening that into
  ;;
  ;;     (AND (SATISFIES FOO) (INTEGER -100 200))
  ;;
  ;; is too much work to do here ... but if we canonicalized things
  ;; differently, we could get it for free with trivial changes here.
  (labels ((weaken-integer-type-part (type base)
             (cond ((intersection-type-p type)
                    (let ((new (specifier-type base)))
                      (dolist (part (intersection-type-types type))
                        (when (if range-only
                                  (numeric-type-p part)
                                  (not (unknown-type-p part)))
                          (setf new (type-intersection
                                     new (weaken-integer-type-part part t)))))
                      new))
                   ((union-type-p type)
                    (let ((low t) (high t) (rest *empty-type*))
                      (flet ((maximize (bound)
                               (if (and bound high)
                                   (setf high (if (eq t high)
                                                  bound
                                                  (max high bound)))
                                   (setf high nil)))
                             (minimize (bound)
                               (if (and bound low)
                                   (setf low (if (eq t low)
                                                 bound
                                                 (min low bound)))
                                   (setf low nil))))
                        (dolist (part (union-type-types type))
                          (let ((weak (weaken-integer-type-part part t)))
                            (cond ((numeric-type-p weak)
                                   (minimize (numeric-type-low weak))
                                   (maximize (numeric-type-high weak)))
                                  ((not range-only)
                                   (setf rest (type-union rest weak)))))))
                      (if (eq t low)
                          rest
                          (type-union rest
                                      (specifier-type
                                       `(integer ,(or low '*) ,(or high '*)))))))
                   (t
                    type))))
    (weaken-integer-type-part type 'integer)))

(defun-cached
    (weaken-type :hash-bits 7 :hash-function #'sb-kernel::type-%bits)
    ((type eq))
  (declare (type ctype type))
  (cond ((named-type-p type)
         type)
        ((csubtypep type (specifier-type 'integer))
         ;; Simple range checks are not that expensive, and we *don't*
         ;; want to accidentally lose eg. array bounds checks due to
         ;; weakening, so for integer types we simply collapse all
         ;; ranges into one.
         (weaken-integer-type type))
        #+sb-unicode
        ((csubtypep type (specifier-type 'base-char))
         ;; Don't want to be putting CHARACTERs into BASE-STRINGs.
         (specifier-type 'base-char))
        (t
         type)))

(defun weaken-values-type (type)
  (declare (type ctype type))
  (cond ((eq type *wild-type*) type)
        ((not (values-type-p type))
         (weaken-type type))
        (t
         (make-values-type (mapcar #'weaken-type (values-type-required type))
                           (mapcar #'weaken-type (values-type-optional type))
                           (acond ((values-type-rest type) (weaken-type it)))))))

;;;; checking strategy determination

;;; Return the type we should test for when we really want to check
;;; for TYPE. If type checking policy is "fast", then we return a
;;; weaker type if it is easier to check. First we try the defined
;;; type weakenings, then look for any predicate that is cheaper.
(defun maybe-weaken-check (type policy)
  (declare (type ctype type))
  (typecase type
    ;; Can't do much functional type checking at run-time
    (fun-designator-type
     (specifier-type 'function-designator))
    (fun-type
     (specifier-type 'function))
    (t
     (ecase (policy policy type-check)
       (0 *wild-type*)
       (2 (weaken-values-type type))
       (3 type)))))

(defun lvar-types-to-check (types original-types n-required)
  (loop for type in types
        for original in original-types
        for i from 0
        collect (list (if (>= i n-required)
                          (type-union original (specifier-type 'null))
                          original)
                      type)))

;;; Determine if CAST can be checked.
;;; We may check only fixed number of values; in any case the number
;;; of generated values is trusted. If we know the number of produced
;;; values, all of them are checked; otherwise if we know the number
;;; of consumed -- only they are checked; otherwise the check is not
;;; performed.

;;; In the types are checkable it returns :SIMPLE and the second value
;;; of the form:
;;;    (NOT-P TYPE ORIGINAL-TYPE)
;;;
;;; If true, the NOT-P flag indicates a test that the corresponding
;;; value is *not* of the specified TYPE. ORIGINAL-TYPE is the type
;;; asserted on this value in the lvar, for use in error
;;; messages. When NOT-P is true, this will be different from TYPE.
;;;
;;; This allows us to take what has been proven about CAST's argument
;;; type into consideration. If it is cheaper to test for the
;;; difference between the derived type and the asserted type, then we
;;; check for the negation of this type instead.
(defun cast-check-types (cast)
  (declare (type cast cast))
  (let* ((ctype (coerce-to-values (cast-type-to-check cast)))
         (atype (coerce-to-values (cast-asserted-type cast)))
         (dtype (node-derived-type cast))
         (lvar (node-lvar cast))
         (dest (and lvar (lvar-dest lvar)))
         mv-vars
         (n-required (length (values-type-required dtype))))
    (aver (not (eq ctype *wild-type*)))
    (cond ((and (null (values-type-optional dtype))
                (not (values-type-rest dtype)))
           ;; we [almost] know how many values are produced
           (values :simple
                   (lvar-types-to-check (values-type-out ctype n-required)
                                        (values-type-out atype n-required)
                                        n-required)))
          ((lvar-single-value-p lvar)
           ;; exactly one value is consumed
           (principal-lvar-single-valuify lvar)
           (values :simple (lvar-types-to-check (list (single-value-type ctype))
                                                (list (single-value-type atype))
                                                n-required)))
          ((and (mv-combination-p dest)
                (eq (mv-combination-kind dest) :local)
                (lvar-uses (mv-combination-fun dest))
                (singleton-p (mv-combination-args dest))
                (let ((fun-ref (lvar-use (mv-combination-fun dest))))
                  (setf mv-vars (lambda-vars (ref-leaf fun-ref)))))
           ;; we know the number of consumed values
           (flet ((filter-unused (types)
                    (loop for var in mv-vars
                          for type in types
                          collect (if (lambda-var-deleted var)
                                      *universal-type*
                                      type))))
             (values :simple (lvar-types-to-check (filter-unused (values-type-out ctype (length mv-vars)))
                                                  (filter-unused (values-type-out atype (length mv-vars)))
                                                  n-required))))
          (t
           (values :hairy (list ctype atype))))))

;;; Return T is the cast appears to be from the declaration of the callee,
;;; and should be checked externally -- that is, by the callee and not the caller.
(defun cast-externally-checkable-p (cast)
  (declare (type cast cast))
  (let ((lvar (node-lvar cast)))
    (multiple-value-bind (dest lvar) (and lvar (immediately-used-let-dest lvar cast))
      (cond ((and (cast-context cast)
                  (policy cast (or (> debug 1)
                                   (and (> debug 0)
                                        (>= debug speed)))))
             nil)
            ((and (basic-combination-p dest)
                  (or (not (basic-combination-fun-info dest))
                      ;; fixed-args functions do not check their arguments.
                      (not (ir1-attributep (fun-info-attributes (basic-combination-fun-info dest))
                                           fixed-args
                                           always-translatable)))
                  ;; The theory is that the type assertion is from a declaration on the
                  ;; callee, so the callee should be able to do the check. We want to
                  ;; let the callee do the check, because it is possible that by the
                  ;; time of call that declaration will be changed and we do not want
                  ;; to make people recompile all calls to a function when they were
                  ;; originally compiled with a bad declaration.
                  ;;
                  ;; ALMOST-IMMEDIATELY-USED-P ensures that we don't delegate casts
                  ;; that occur before nodes that can cause observable side effects --
                  ;; most commonly other non-external casts: so the order in which
                  ;; possible type errors are signalled matches with the evaluation
                  ;; order.
                  ;;
                  ;; FIXME: We should let more cases be handled by the callee then we
                  ;; currently do, see: https://bugs.launchpad.net/sbcl/+bug/309104
                  ;; This is not fixable quite here, though, because flow-analysis has
                  ;; deleted the LVAR of the cast by the time we get here, so there is
                  ;; no destination. Perhaps we should mark cases inserted by
                  ;; ASSERT-CALL-TYPE explicitly, and delete those whose destination is
                  ;; deemed unreachable?
                  (cond ((and (lvar-fun-is (basic-combination-fun dest)
                                           '(hairy-data-vector-set/check-bounds
                                             hairy-data-vector-ref/check-bounds
                                             hairy-data-vector-ref
                                             hairy-data-vector-set))
                              (eq (car (basic-combination-args dest)) lvar)
                              (type= (specifier-type 'vector)
                                     (single-value-type (cast-type-to-check cast))))
                         (change-full-call dest
                                           (getf '(hairy-data-vector-set/check-bounds vector-hairy-data-vector-set/check-bounds
                                                   hairy-data-vector-ref/check-bounds vector-hairy-data-vector-ref/check-bounds
                                                   hairy-data-vector-ref vector-hairy-data-vector-ref
                                                   hairy-data-vector-set vector-hairy-data-vector-set)
                                                 (lvar-fun-name (basic-combination-fun dest) t))))
                        #+(or arm64 x86-64)
                        ((lvar-fun-is (basic-combination-fun dest) '(values-list)))
                        ;; Not great
                        ((lvar-fun-is (basic-combination-fun dest) '(%%primitive))
                         (destructuring-bind (vop &rest args) (basic-combination-args dest)
                           (and (constant-lvar-p vop)
                                (let ((name (vop-info-name (lvar-value vop))))
                                  (or (and (memq name '(sb-vm::overflow+t
                                                        sb-vm::overflow-t
                                                        sb-vm::overflow*t))
                                           (eq lvar (car args)))
                                      (and (memq name '(sb-vm::overflow-t-y))
                                           (eq lvar (cadr args))))))))
                        ((and (policy dest (= debug 3))
                              (let ((leaf (nth-value 2 (lvar-fun-type (basic-combination-fun dest)))))
                                (and leaf
                                     (memq (leaf-where-from leaf) '(:declared-verify :defined-here)))))
                         nil)
                        (t
                         (values-subtypep (lvar-externally-checkable-type lvar)
                                          (cast-type-to-check cast))))))
            ((and (cast-p dest)
                  (cast-type-check dest)
                  (atom (lvar-uses (node-lvar cast)))
                  (atom (lvar-uses (cast-value dest)))
                  (lvar-single-value-p (node-lvar cast))
                  (cond ((and (values-type-p (cast-asserted-type dest))
                              (values-type-p (cast-asserted-type cast)))
                         (values-subtypep (cast-asserted-type dest)
                                          (cast-asserted-type cast)))
                        ((not (or (values-type-p (cast-asserted-type dest))
                                  (values-type-p (cast-asserted-type cast))))
                         (csubtypep (cast-asserted-type dest)
                                    (cast-asserted-type cast)))))
             (setf (cast-asserted-type cast) (cast-asserted-type dest)
                   (cast-type-to-check cast) (cast-type-to-check dest)
                   (cast-%type-check dest) nil)
             nil)))))

;; Type specifiers handled by the general-purpose MAKE-TYPE-CHECK-FORM are often
;; trivial enough to have an internal error number assigned to them that can be
;; used in lieu of OBJECT-NOT-TYPE-ERROR. On x86-64 this saves 16 bytes: 1 word
;; for the symbol in the function's constant area, a MOV instruction to load it,
;; and an SC+OFFSET in the error trap.
(defglobal **type-spec-interr-symbols**
    (let* ((entries
            ;; read-time-eval so that during cold-init we can recreate the
            ;; table using the target's sxhash function, but without relying
            ;; on readiness of the type system for parsing/unparsing specifiers.
            #.(map 'vector
                   (lambda (entry)
                     (cons (type-specifier (specifier-type (car entry)))
                           (cadr entry)))
                   (remove-if (lambda (spec)
                                (or (stringp spec)
                                    (typep spec '(cons (eql or)))))
                              sb-c:+backend-internal-errors+
                              :key #'car)))
           ;; This is effectively a compact read-only binned hashtable.
           (hashtable (make-array (logior (length entries) 1)
                                  :initial-element nil)))
        (map nil
             (lambda (entry)
               (let* ((canon-type (car entry))
                      (bucket (mod (cl:sxhash canon-type) (length hashtable))))
                 (push entry (svref hashtable bucket))))
             entries)
      hashtable))

(defglobal **type-spec-unions-interr-symbols**
    #.(map 'vector
           (lambda (entry)
             (cons (type-specifier (specifier-type (car entry)))
                   (cadr entry)))
           (remove-if-not (lambda (spec)
                            (typep spec '(cons (eql or))))
                          sb-c:+backend-internal-errors+
                          :key #'car)))
(declaim (simple-vector **type-spec-unions-interr-symbols**))

;;; Different order of elements in (OR ...) specs make the hash-table
;;; approach inssuficient.
(defun %interr-symbol-for-union-type-spec (spec)
  (let* ((spec (cdr spec))
         (length (length spec))
         (bit-map (if (> length sb-vm:n-fixnum-bits)
                      (return-from %interr-symbol-for-union-type-spec)
                      (1- (ash 1 (truly-the (integer 1 #.sb-vm:n-fixnum-bits)
                                            length))))))
    (declare (list spec))
    (loop for entry across **type-spec-unions-interr-symbols**
          when
          (let ((current-map bit-map))
            ;; Check that each element is present and mark it in the bit map,
            ;; at the end if the map is zero the specs match.
            (declare (type fixnum current-map))
            (loop for element in (cdar entry)
                  for position = (position element spec :test #'equal)
                  always position
                  do
                  (setf (ldb (byte 1 (truly-the (integer 0 (#.sb-vm:n-fixnum-bits))
                                                position))
                             current-map)
                        0))
            (zerop current-map))
          return (cdr entry))))

(defun %interr-symbol-for-type-spec (spec)
  (let ((table **type-spec-interr-symbols**))
    (if (typep spec '(cons (eql or)))
        (%interr-symbol-for-union-type-spec spec)
        (cdr (assoc spec (svref table (rem (cl:sxhash spec) (length table)))
                    :test #'equal)))))
#+nil ; some meta-analysis to decide what types should be in "generic/interr"
(progn
  (defvar *checkgen-used-types* (make-hash-table :test 'equal))
  (defun interr-symbol-for-type-spec (spec)
    (let ((answer (%interr-symbol-for-type-spec spec))
          (meta (gethash spec *checkgen-used-types*)))
      ;; spec -> (count . primitive-p)
      (if meta
          (incf (car meta))
          (setf (gethash spec *checkgen-used-types*) (cons 1 answer)))
      answer)))

(defun internal-type-error-call (var type &optional context)
  (let* ((external-spec (if (ctype-p type)
                            (type-specifier type)
                            type))
         (interr-symbol
           (%interr-symbol-for-type-spec external-spec)))
    (if interr-symbol
        `(%type-check-error/c ,var ',interr-symbol ',context)
        `(%type-check-error
          ,var
          ',(typecase type
              ;; These are already loaded into the constants vector
              (structure-classoid
               ;; Can't use CLASSOID-LAYOUT as it may mismatch due to redefinition
               (info :type :compiler-layout (classoid-name type)))
              (standard-classoid
               (find-classoid-cell (classoid-name type) :create t))
              (t
               external-spec))
          ',context))))

;;; Return a lambda form that we can convert to do a type check
;;; of the specified TYPES. TYPES is a list of the format returned by
;;; CAST-CHECK-TYPES.
;;;
;;; Note that we don't attempt to check for required values being
;;; unsupplied. Such checking is impossible to efficiently do at the
;;; source level because our fixed-values conventions are optimized
;;; for the common MV-BIND case.
(defun make-type-check-form (types cast)
  (let* ((temps (make-gensym-list (length types)))
         (context (cast-context cast))
         (restart (and (eq context :restart)
                       (setf context (make-restart-location)))))
    (lambda (dummy)
      `(multiple-value-bind ,temps ,dummy
         ,@(mapcar
            (lambda (temp %type)
              (destructuring-bind (type-to-check type-to-report) %type
                `(progn
                   (unless (typep ,temp ',(type-specifier type-to-check t))
                     ,(internal-type-error-call temp
                                                (if (fun-designator-type-p type-to-report)
                                                    ;; Simplify
                                                    (specifier-type 'function-designator)
                                                    type-to-report)
                                                context))
                   ,@(and restart
                          `((restart-point ,restart))))))
            temps
            types)
         (values ,@temps)))))

;;; Splice in explicit type check code immediately before CAST. This
;;; code receives the value(s) that were being passed to CAST-VALUE,
;;; checks the type(s) of the value(s), then passes them further.
(defun convert-type-check (cast types)
  (declare (type cast cast) (type list types))
  (filter-lvar (cast-value cast)
               (make-type-check-form types cast))
  (setf (cast-%type-check cast) nil))

(defun convert-hairy-type-check (cast types)
  (filter-lvar (cast-value cast)
               (make-hairy-type-check-form types cast))
  (setf (cast-%type-check cast) nil))

(defun make-hairy-type-check-form (types cast)
  (let ((ctype (first types))
        (atype (second types))
        (context (cast-context cast)))
    (multiple-value-bind (types rest-type) (values-type-types ctype nil)
      (multiple-value-bind (report-types report-rest-type) (values-type-types atype nil)
        (let ((length (length types)))
          (flet ((check (type report-type index)
                   `(let ((value (fast-&rest-nth ,index args)))
                      (unless (typep value
                                     ',(type-specifier type t))
                        ,(internal-type-error-call 'value
                                                   (if (fun-designator-type-p report-type)
                                                       ;; Simplify
                                                       (specifier-type 'function-designator)
                                                       report-type)
                                                   context)))))
            (lambda (dummy)
              `(flet ((values-type-check (&rest args)
                        (prog ((length (length args)))
                           (cond
                             ,@(loop for n downfrom length to 1
                                     collect `((>= length ,n) (go ,n)))
                             (t
                              (go none)))
                           ,@(loop for type-to-check in (reverse types)
                                   for type-to-report in (reverse report-types)
                                   for n downfrom length
                                   collect n
                                   collect (check type-to-check type-to-report (1- n)))
                         none
                           ,@(when rest-type
                               `((loop for i from ,length below length
                                       do
                                       ,(check rest-type report-rest-type 'i)))))
                        (values-list args)))
                 (multiple-value-call #'values-type-check ,dummy)))))))))

;;; Check all possible arguments of CAST and emit type warnings for
;;; those with type errors. If the value of USE is being used for a
;;; variable binding, we figure out which one for source context. If
;;; the value is a constant, we print it specially.
(defun cast-check-uses (cast)
  (declare (type cast cast))
  (let* ((lvar (node-lvar cast))
         (dest (and lvar (lvar-dest lvar)))
         (value (cast-value cast))
         (atype (cast-asserted-type cast))
         bad)
    (do-uses (use value)
      (unless (values-types-equal-or-intersect (node-derived-type use) atype)
        (push use bad)))
    (loop for use in bad
          for path = (source-path-before-transforms use)
          ;; Are all uses from the same transform bad?
          when (or (not path)
                   (and
                    (do-uses (use value t)
                      (unless (or (memq use bad)
                                  (neq path (source-path-before-transforms use)))
                        (return)))
                    ;; maybe-delete-cast may have hoisted out a good use
                    lvar
                    (or (atom (lvar-uses lvar))
                        (do-uses (use lvar t)
                          (unless (eq path (source-path-before-transforms use))
                            (return))))))
          do
          (let* ((*compiler-error-context* use)
                 (dtype (node-derived-type use))
                 (what (when (and (combination-p dest)
                                  (eq (combination-kind dest) :local))
                         (let ((lambda (combination-lambda dest))
                               (pos (position-or-lose
                                     lvar (combination-args dest))))
                           (format nil "~:[A possible~;The~] binding of ~S"
                                   (and (lvar-has-single-use-p lvar)
                                        (functional-kind-eq lambda let))
                                   (leaf-source-name (elt (lambda-vars lambda)
                                                          pos)))))))
            (cond ((and (ref-p use) (constant-p (ref-leaf use)))
                   (warn 'type-style-warning
                         :format-control "~:[This~;~:*~A~] is not a ~
                       ~<~%~9T~:;~/sb-impl:print-type/:~>~% ~S"
                         :format-arguments
                         (list what atype (constant-value (ref-leaf use)))))
                  (t
                   (warn 'type-style-warning
                         :format-control
                         "~:[Result~;~:*~A~] is a ~/sb-impl:print-type/, ~
                       ~<~%~9T~:;not a ~/sb-impl:print-type/.~>"
                         :format-arguments (list what dtype atype)))))))
  (values))

;;; Loop over all blocks in COMPONENT that have TYPE-CHECK set,
;;; looking for CASTs with TYPE-CHECK T. We do two mostly unrelated
;;; things: detect compile-time type errors and determine if and how
;;; to do run-time type checks.
;;;
;;; If there is a compile-time type error, then we mark the CAST and
;;; emit a warning if appropriate. This part loops over all the uses
;;; of the continuation, since after we convert the check, the
;;; :DELETED kind will inhibit warnings about the types of other uses.
;;;
;;; If the cast is too complex to be checked by the back end, or is
;;; better checked with explicit code, then convert to an explicit
;;; test. Assertions that can checked by the back end are passed
;;; through. Assertions that can't be tested are flamed about and
;;; marked as not needing to be checked.
;;;
;;; If we determine that a type check won't be done, then we set
;;; TYPE-CHECK to :NO-CHECK. In the non-hairy cases, this is just to
;;; prevent us from wasting time coming to the same conclusion again
;;; on a later iteration. In the hairy case, we must indicate to LTN
;;; that it must choose a safe implementation, since IR2 conversion
;;; will choke on the check.
;;;
;;; The generation of the type checks is delayed until all the type
;;; check decisions have been made because the generation of the type
;;; checks creates new nodes whose derived types aren't always updated
;;; which may lead to inappropriate template choices due to the
;;; modification of argument types.
(defun generate-type-checks (component)
  (let (generated)
    (collect ((casts))
      (do-blocks (block component)
        (when (and (block-type-check block)
                   (not (block-delete-p block)))
          ;; CAST-EXTERNALLY-CHECKABLE-P wants the backward pass
          (do-nodes-backwards (node nil block)
            (when (and (cast-p node)
                       (cast-type-check node))
              (cast-check-uses node)
              (let ((external (cast-externally-checkable-p node)))
                (cond (external
                       (setf (cast-%type-check node) :external))
                      (t
                       ;; it is possible that NODE was marked :EXTERNAL by
                       ;; the previous pass
                       (setf (cast-%type-check node) t)
                       (casts node))))))
          (setf (block-type-check block) nil)))
      (dolist (cast (casts))
        ;; Disabled by cast-externally-checkable-p of a different cast.
        (when (cast-type-check cast)
          (multiple-value-bind (check types) (cast-check-types cast)
            (ecase check
              (:simple
               (convert-type-check cast types)
               (setf generated t))
              (:hairy
               (when (policy cast (>= safety inhibit-warnings))
                 (let* ((*compiler-error-context* cast)
                        (type (cast-asserted-type cast))
                        (value-type (coerce-to-values type)))
                   (compiler-notify
                    "Type assertion too complex to check efficiently:~@
                    ~/sb-impl:print-type/.~a"
                    type
                    (cond ((values-type-rest value-type)
                           (format nil
                                   "~%It allows an unknown number of values, consider using~@
                                    ~/sb-impl:print-type/."
                                   (make-values-type (values-type-required value-type)
                                                     (values-type-optional value-type))))
                          ((values-type-optional value-type)
                           (format nil
                                   "~%It allows a variable number of values, consider using~@
                                    ~/sb-impl:print-type/."
                                   (make-values-type (append (values-type-required value-type)
                                                             (values-type-optional value-type)))))
                          ("")))))
               (convert-hairy-type-check cast types)
               (setf generated t)))))))
    generated))

