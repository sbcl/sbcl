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

(in-package "SB!C")

;;;; cost estimation

;;; Return some sort of guess about the cost of a call to a function.
;;; If the function has some templates, we return the cost of the
;;; cheapest one, otherwise we return the cost of CALL-NAMED. Calling
;;; this with functions that have transforms can result in relatively
;;; meaningless results (exaggerated costs.)
;;;
;;; We special-case NULL, since it does have a source tranform and is
;;; interesting to us.
(defun fun-guessed-cost (name)
  (declare (symbol name))
  (let ((info (info :function :info name))
        (call-cost (template-cost (template-or-lose 'call-named))))
    (if info
        (let ((templates (fun-info-templates info)))
          (if templates
              (template-cost (first templates))
              (case name
                (null (template-cost (template-or-lose 'if-eq)))
                (t call-cost))))
        call-cost)))

;;; Return some sort of guess for the cost of doing a test against
;;; TYPE. The result need not be precise as long as it isn't way out
;;; in space. The units are based on the costs specified for various
;;; templates in the VM definition.
(defun type-test-cost (type)
  (declare (type ctype type))
  (or (when (eq type *universal-type*)
        0)
      (when (eq type *empty-type*)
        0)
      (let ((found (cdr (assoc type *backend-type-predicates*
                               :test #'type=))))
        (if found
            (+ (fun-guessed-cost found) (fun-guessed-cost 'eq))
            nil))
      (typecase type
        (compound-type
         (reduce #'+ (compound-type-types type) :key 'type-test-cost))
        (member-type
         (* (member-type-size type)
            (fun-guessed-cost 'eq)))
        (numeric-type
         (* (if (numeric-type-complexp type) 2 1)
            (fun-guessed-cost
             (if (csubtypep type (specifier-type 'fixnum)) 'fixnump 'numberp))
            (+ 1
               (if (numeric-type-low type) 1 0)
               (if (numeric-type-high type) 1 0))))
        (cons-type
         (+ (type-test-cost (specifier-type 'cons))
            (fun-guessed-cost 'car)
            (type-test-cost (cons-type-car-type type))
            (fun-guessed-cost 'cdr)
            (type-test-cost (cons-type-cdr-type type))))
        (t
         (fun-guessed-cost 'typep)))))

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
    (weaken-type :hash-bits 7 :hash-function #'type-hash-value)
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
        (t
         (let ((min-cost (type-test-cost type))
               (min-type type)
               (found-super nil))
           (dolist (x *backend-type-predicates*)
             (let* ((stype (car x))
                    (samep (type= stype type)))
               (when (or samep
                         (and (csubtypep type stype)
                              (not (union-type-p stype))))
                 (let ((stype-cost (type-test-cost stype)))
                   (when (or (< stype-cost min-cost)
                             samep)
                     ;; If the supertype is equal in cost to the type, we
                     ;; prefer the supertype. This produces a closer
                     ;; approximation of the right thing in the presence of
                     ;; poor cost info.
                     (setq found-super t
                           min-type stype
                           min-cost stype-cost))))))
           ;; This used to return the *UNIVERSAL-TYPE* if no supertype was found,
           ;; but that's too liberal: it's far too easy for the user to create
           ;; a union type (which are excluded above), and then trick the compiler
           ;; into trusting the union type... and finally ending up corrupting the
           ;; heap once a bad object sneaks past the missing type check.
           (if found-super
               min-type
               type)))))

(defun weaken-values-type (type)
  (declare (type ctype type))
  (cond ((eq type *wild-type*) type)
        ((not (values-type-p type))
         (weaken-type type))
        (t
         (make-values-type :required (mapcar #'weaken-type
                                             (values-type-required type))
                           :optional (mapcar #'weaken-type
                                             (values-type-optional type))
                           :rest (acond ((values-type-rest type)
                                         (weaken-type it)))))))

;;;; checking strategy determination

;;; Return the type we should test for when we really want to check
;;; for TYPE. If type checking policy is "fast", then we return a
;;; weaker type if it is easier to check. First we try the defined
;;; type weakenings, then look for any predicate that is cheaper.
(defun maybe-weaken-check (type policy)
  (declare (type ctype type))
  (ecase (policy policy type-check)
    (0 *wild-type*)
    (2 (weaken-values-type type))
    (3 type)))

;;; LVAR is an lvar we are doing a type check on and TYPES is a list
;;; of types that we are checking its values against. If we have
;;; proven that LVAR generates a fixed number of values, then for each
;;; value, we check whether it is cheaper to then difference between
;;; the proven type and the corresponding type in TYPES. If so, we opt
;;; for a :HAIRY check with that test negated. Otherwise, we try to do
;;; a simple test, and if that is impossible, we do a hairy test with
;;; non-negated types. If true, FORCE-HAIRY forces a hairy type check.
(defun maybe-negate-check (lvar types original-types n-required)
  (declare (type lvar lvar) (list types original-types))
  (let ((ptypes (values-type-out (lvar-derived-type lvar) (length types))))
    (loop for p in ptypes
          and c in types
          and a in original-types
          and i from 0
          for cc = (if (>= i n-required)
                       (type-union c (specifier-type 'null))
                       c)
          for diff = (type-difference p cc)
          collect (if (and diff
                           (< (type-test-cost diff)
                              (type-test-cost cc)))
                      (list t diff a)
                      (list nil cc a)))))

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
         (value (cast-value cast))
         (lvar (node-lvar cast))
         (dest (and lvar (lvar-dest lvar)))
         (n-consumed (cond ((not lvar)
                            nil)
                           ((lvar-single-value-p lvar)
                            1)
                           ((and (mv-combination-p dest)
                                 (eq (mv-combination-kind dest) :local))
                            (let ((fun-ref (lvar-use (mv-combination-fun dest))))
                              (length (lambda-vars (ref-leaf fun-ref)))))))
         (n-required (length (values-type-required dtype))))
    (aver (not (eq ctype *wild-type*)))
    (cond ((and (null (values-type-optional dtype))
                (not (values-type-rest dtype)))
           ;; we [almost] know how many values are produced
           (values :simple
                   (maybe-negate-check value
                                       (values-type-out ctype n-required)
                                       (values-type-out atype n-required)
                                       n-required)))
          ((lvar-single-value-p lvar)
           ;; exactly one value is consumed
           (principal-lvar-single-valuify lvar)
           (flet ((get-type (type)
                    (acond ((args-type-required type)
                            (car it))
                           ((args-type-optional type)
                            (car it))
                           (t (bug "type ~S is too hairy" type)))))
             (multiple-value-bind (ctype atype)
                 (values (get-type ctype) (get-type atype))
               (values :simple (maybe-negate-check value
                                                   (list ctype) (list atype)
                                                   n-required)))))
          ((and (mv-combination-p dest)
                (eq (mv-combination-kind dest) :local))
           ;; we know the number of consumed values
           (values :simple (maybe-negate-check value
                                               (adjust-list (values-type-types ctype)
                                                            n-consumed
                                                            *universal-type*)
                                               (adjust-list (values-type-types atype)
                                                            n-consumed
                                                            *universal-type*)
                                               n-required)))
          (t
           (values :too-hairy nil)))))

;;; Return T is the cast appears to be from the declaration of the callee,
;;; and should be checked externally -- that is, by the callee and not the caller.
(defun cast-externally-checkable-p (cast)
  (declare (type cast cast))
  (let* ((lvar (node-lvar cast))
         (dest (and lvar (lvar-dest lvar))))
    (and (combination-p dest)
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
         (almost-immediately-used-p lvar cast)
         (values (values-subtypep (lvar-externally-checkable-type lvar)
                                  (cast-type-to-check cast))))))

;; Type specifiers handled by the general-purpose MAKE-TYPE-CHECK-FORM are often
;; trivial enough to have an internal error number assigned to them that can be
;; used in lieu of OBJECT-NOT-TYPE-ERROR. On x86-64 this saves 16 bytes: 1 word
;; for the symbol in the function's constant area, a MOV instruction to load it,
;; and an sc-offset in the error trap.
(defglobal **type-spec-interr-symbols**
    (let* ((entries
            ;; read-time-eval so that during cold-init we can recreate the
            ;; table using the target's sxhash function, but without relying
            ;; on readiness of the type system for parsing/unparsing specifiers.
            #.(map 'vector
                   (lambda (entry)
                     (cons (type-specifier (specifier-type (car entry)))
                           (cdr entry)))
                   (remove-if #'stringp sb!c:+backend-internal-errors+
                              :key #'car)))
           ;; This is effectively a compact read-only binned hashtable.
           (hashtable (make-array (logior (length entries) 1)
                                  :initial-element nil)))
        (map nil
             (lambda (entry)
               (let* ((canon-type (car entry))
                      (bucket (mod (sxhash canon-type) (length hashtable))))
                 (push entry (svref hashtable bucket))))
             entries)
        hashtable))
(defun %interr-symbol-for-type-spec (spec)
  (let ((table **type-spec-interr-symbols**))
    (cdr (assoc spec (svref table (rem (sxhash spec) (length table)))
                :test #'equal))))
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

;;; Return a lambda form that we can convert to do a type check
;;; of the specified TYPES. TYPES is a list of the format returned by
;;; CAST-CHECK-TYPES.
;;;
;;; Note that we don't attempt to check for required values being
;;; unsupplied. Such checking is impossible to efficiently do at the
;;; source level because our fixed-values conventions are optimized
;;; for the common MV-BIND case.
(defun make-type-check-form (types)
  (let ((temps (make-gensym-list (length types))))
    `(multiple-value-bind ,temps 'dummy
       ,@(mapcar (lambda (temp type)
                   (let* ((spec
                            (let ((*unparse-fun-type-simplify* t))
                              (type-specifier (second type))))
                          (test (if (first type) `(not ,spec) spec))
                          (external-spec (type-specifier (third type)))
                          (interr-symbol
                            (%interr-symbol-for-type-spec external-spec)))
                     `(unless (typep ,temp ',test)
                        ,(if interr-symbol
                             `(%type-check-error/c ,temp ',interr-symbol)
                             `(%type-check-error ,temp ',external-spec)))))
                 temps
                 types)
       (values ,@temps))))

;;; Splice in explicit type check code immediately before CAST. This
;;; code receives the value(s) that were being passed to CAST-VALUE,
;;; checks the type(s) of the value(s), then passes them further.
(defun convert-type-check (cast types)
  (declare (type cast cast) (type list types))
  (let ((value (cast-value cast))
        (length (length types)))
    (filter-lvar value (make-type-check-form types))
    (reoptimize-lvar (cast-value cast))
    (setf (cast-type-to-check cast) *wild-type*)
    (setf (cast-%type-check cast) nil)
    (let* ((atype (cast-asserted-type cast))
           (atype (cond ((not (values-type-p atype))
                         atype)
                        ((= length 1)
                         (single-value-type atype))
                        (t
                         (make-values-type
                          :required (values-type-out atype length)))))
           (dtype (node-derived-type cast))
           (dtype (make-values-type
                   :required (values-type-out dtype length))))
      (setf (cast-asserted-type cast) atype)
      (setf (node-derived-type cast) dtype)))

  (values))

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
         (condition 'type-warning)
         (not-ok-uses '()))
    (do-uses (use value)
      (let ((dtype (node-derived-type use)))
        (if (values-types-equal-or-intersect dtype atype)
            (setf condition 'type-style-warning)
            (push use not-ok-uses))))
    (dolist (use (nreverse not-ok-uses))
      (let* ((*compiler-error-context* use)
             (dtype      (node-derived-type use))
             (atype-spec (type-specifier atype))
             (what (when (and (combination-p dest)
                              (eq (combination-kind dest) :local))
                     (let ((lambda (combination-lambda dest))
                           (pos (position-or-lose
                                 lvar (combination-args dest))))
                       (format nil "~:[A possible~;The~] binding of ~S"
                               (and (lvar-has-single-use-p lvar)
                                    (eq (functional-kind lambda) :let))
                               (leaf-source-name (elt (lambda-vars lambda)
                                                      pos)))))))
        (cond ((and (ref-p use) (constant-p (ref-leaf use)))
               (warn condition
                     :format-control
                     "~:[This~;~:*~A~] is not a ~<~%~9T~:;~S:~>~%  ~S"
                     :format-arguments
                     (list what atype-spec
                           (constant-value (ref-leaf use)))))
              (t
               (warn condition
                     :format-control
                     "~:[Result~;~:*~A~] is a ~S, ~<~%~9T~:;not a ~S.~>"
                     :format-arguments
                     (list what (type-specifier dtype) atype-spec)))))))
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
  (collect ((casts))
    (do-blocks (block component)
      (when (and (block-type-check block)
                 (not (block-delete-p block)))
        ;; CAST-EXTERNALLY-CHECKABLE-P wants the backward pass
        (do-nodes-backwards (node nil block)
          (when (and (cast-p node)
                     (cast-type-check node))
            (cast-check-uses node)
            (cond ((cast-externally-checkable-p node)
                   (setf (cast-%type-check node) :external))
                  (t
                   ;; it is possible that NODE was marked :EXTERNAL by
                   ;; the previous pass
                   (setf (cast-%type-check node) t)
                   (casts node)))))
        (setf (block-type-check block) nil)))
    (dolist (cast (casts))
      (unless (bound-cast-p cast)
        (multiple-value-bind (check types) (cast-check-types cast)
          (ecase check
            (:simple
             (convert-type-check cast types))
            (:too-hairy
             (let ((*compiler-error-context* cast))
               (when (policy cast (>= safety inhibit-warnings))
                 (compiler-notify
                  "type assertion too complex to check:~% ~S."
                  (type-specifier (coerce-to-values (cast-asserted-type cast))))))
             (setf (cast-type-to-check cast) *wild-type*)
             (setf (cast-%type-check cast) nil)))))))
  (values))
