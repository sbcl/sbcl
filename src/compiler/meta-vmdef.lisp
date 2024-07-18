;;;; This file contains the implementation-independent facilities used
;;;; for defining the compiler's interface to the VM in a given
;;;; implementation that are needed at meta-compile time. They are
;;;; separated out from vmdef.lisp so that they can be compiled and
;;;; loaded without trashing the running compiler.
;;;;
;;;; FIXME: The "trashing the running [CMU CL] compiler" motivation no
;;;; longer makes sense in SBCL, since we can cross-compile cleanly.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;;; storage class and storage base definition

;;; Define a storage base having the specified NAME. KIND may be :FINITE,
;;; :UNBOUNDED or :NON-PACKED. The following keywords are legal:
;;;    :SIZE specifies the number of locations in a :FINITE SB or
;;;          the initial size of an :UNBOUNDED SB.
;;;
;;; We enter the basic structure at meta-compile time, and then fill
;;; in the missing slots at load time.
(defmacro !define-storage-bases (&rest definitions &aux (index -1) forms)
  (dolist (def definitions)
    (destructuring-bind (name kind &key size (size-increment size)
                                             (size-alignment 1))
        (cdr def)

      (declare (type symbol name))
      (declare (type (member :finite :unbounded :non-packed) kind))

      ;; SIZE is either mandatory or forbidden.
      (ecase kind
        (:non-packed
         (when size
           (error "A size specification is meaningless in a ~S SB." kind)))
        ((:finite :unbounded)
         (unless size (error "Size is not specified in a ~S SB." kind))
         (aver (<= size sb-vm:finite-sc-offset-limit))
         (aver (= 1 (logcount size-alignment)))
         (aver (not (logtest size (1- size-alignment))))
         (aver (not (logtest size-increment (1- size-alignment))))))

      (push (if (eq kind :non-packed)
                `(make-storage-base :name ',name :kind ,kind)
                `(make-finite-sb-template
                                 :index ,(incf index) :name ',name
                                 :kind ,kind :size ,size
                                 :size-increment ,size-increment
                                 :size-alignment ,size-alignment))
            forms)))
  ;; Do not clobber the global var while running the cross-compiler.
  `(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
     (setf *backend-sbs* (vector ,@(nreverse forms)))))

;;; Define a storage class NAME that uses the named Storage-Base.
;;; NUMBER is a small, non-negative integer that is used as an alias.
;;; The following keywords are defined:
;;;
;;; :ELEMENT-SIZE Size
;;;   The size of objects in this SC in whatever units the SB uses.
;;;   This defaults to 1.
;;;
;;; :ALIGNMENT Size
;;;   The alignment restrictions for this SC. TNs will only be
;;;   allocated at offsets that are an even multiple of this number.
;;;   This defaults to 1.
;;;
;;; :LOCATIONS (Location*)
;;;   If the SB is :FINITE, then this is a list of the offsets within
;;;   the SB that are in this SC.
;;;
;;; :RESERVE-LOCATIONS (Location*)
;;;   A subset of the Locations that the register allocator should try to
;;;   reserve for operand loading (instead of to hold variable values.)
;;;
;;; :SAVE-P {T | NIL}
;;;   If T, then values stored in this SC must be saved in one of the
;;;   non-save-p :ALTERNATE-SCs across calls.
;;;
;;; :ALTERNATE-SCS (SC*)
;;;   Indicates other SCs that can be used to hold values from this SC across
;;;   calls or when storage in this SC is exhausted. The SCs should be
;;;   specified in order of decreasing \"goodness\". There must be at least
;;;   one SC in an unbounded SB, unless this SC is only used for restricted or
;;;   wired TNs.
;;;
;;; :CONSTANT-SCS (SC*)
;;;   A list of the names of all the constant SCs that can be loaded into this
;;;   SC by a move function.
(defmacro !define-storage-class (name number sb-name &key (element-size '1)
                                      (alignment '1) locations reserve-locations
                                      save-p alternate-scs constant-scs
                                      operand-size)
  (declare (type symbol name))
  (declare (type sc-number number))
  (declare (type symbol sb-name))
  (declare (type list locations reserve-locations alternate-scs constant-scs))
  (declare (type boolean save-p))
  (unless (= (logcount alignment) 1)
    (error "alignment not a power of two: ~W" alignment))

  (let ((sb (sb-or-lose sb-name)))
    (if (eq (sb-kind sb) :finite)
        (let ((size (sb-size sb))
              (element-size (eval element-size)))
          (declare (type unsigned-byte element-size))
          (dolist (el locations)
            (declare (type unsigned-byte el))
            (unless (<= 1 (+ el element-size) size)
              (error "SC element ~W out of bounds for ~S" el sb))))
        (when locations
          (error ":LOCATIONS is meaningless in a ~S SB." (sb-kind sb))))

    (unless (subsetp reserve-locations locations)
      (error "RESERVE-LOCATIONS not a subset of LOCATIONS."))

    (when (and (or alternate-scs constant-scs)
               (eq (sb-kind sb) :non-packed))
      (error
       "It's meaningless to specify alternate or constant SCs in a ~S SB."
       (sb-kind sb))))

  (let ((nstack-p
         (if (or (eq sb-name 'non-descriptor-stack)
                 (find 'non-descriptor-stack
                       (mapcar #'sc-or-lose alternate-scs)
                       :key (lambda (x)
                              (sb-name (sc-sb x)))))
             t nil)))
    `(progn
       (eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
         (let ((res (make-storage-class
                             :name ',name :number ',number
                             :sb (sb-or-lose ',sb-name)
                             :element-size ,element-size
                             :operand-size ,operand-size
                             :alignment ,alignment
                             :locations (make-sc-locations ',locations)
                             :reserve-locations (make-sc-locations ',reserve-locations)
                             :save-p ',save-p
                             :number-stack-p ,nstack-p
                             :alternate-scs (mapcar #'sc-or-lose
                                                    ',alternate-scs)
                             :constant-scs (mapcar #'sc-or-lose
                                                   ',constant-scs))))
           (setf (gethash ',name *backend-sc-names*) res)
           (setf (svref (sc-load-costs res) ',number) 0)))

       (let ((old (svref *backend-sc-numbers* ',number)))
         (when (and old (not (eq (sc-name old) ',name)))
           (warn "redefining SC number ~W from ~S to ~S" ',number
                 (sc-name old) ',name)))

       (setf (svref *backend-sc-numbers* ',number) (sc-or-lose ',name))
       (setf (gethash ',name *backend-sc-names*) (sc-or-lose ',name))
       (setf (sc-sb (sc-or-lose ',name)) (sb-or-lose ',sb-name))
       ',name)))

;;;; move/coerce definition

;;; Given a list of pairs of lists of SCs (as given to DEFINE-MOVE-VOP,
;;; etc.), bind TO-SC and FROM-SC to all the combinations.
(defmacro do-sc-pairs ((from-sc-var to-sc-var scs) &body body)
  `(do ((froms ,scs (cddr froms))
        (tos (cdr ,scs) (cddr tos)))
       ((null froms))
     (dolist (from (car froms))
       (let ((,from-sc-var (sc-or-lose from)))
         (dolist (to (car tos))
           (let ((,to-sc-var (sc-or-lose to)))
             ,@body))))))

;;; Define the function NAME and note it as the function used for
;;; moving operands from the From-SCs to the To-SCs. Cost is the cost
;;; of this move operation. The function is called with three
;;; arguments: the VOP (for context), and the source and destination
;;; TNs. An ASSEMBLE form is wrapped around the body. All uses of
;;; DEFINE-MOVE-FUN should be compiled before any uses of
;;; DEFINE-VOP.
(defmacro define-move-fun ((name cost) lambda-list scs &body body)
  (declare (type index cost))
  (when (or (oddp (length scs)) (null scs))
    (error "malformed SCs spec: ~S" scs))
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (do-sc-pairs (from-sc to-sc ',scs)
         (unless (eq from-sc to-sc)
           (let ((num (sc-number from-sc)))
             (setf (svref (sc-move-funs to-sc) num) ',name)
             (setf (svref (sc-load-costs to-sc) num) ',cost)))))

     (defun ,name ,lambda-list
       (declare (ignorable ,(car lambda-list)))
       (sb-assem:assemble ()
         ,@body))))

(defglobal *sc-vop-slots*
    '((:move . sc-move-vops)
      (:move-arg . sc-move-arg-vops)))

;;;; primitive type definition

;;; Define a primitive type NAME. Each SCS entry specifies a storage
;;; class that values of this type may be allocated in. TYPE is the
;;; type descriptor for the Lisp type that is equivalent to this type.
(defmacro !def-primitive-type (name scs &key (type name))
  (declare (type symbol name) (type list scs))
  (let ((scns (mapcar #'sc-number-or-lose scs)))
    `(progn
       (/show "doing !DEF-PRIMITIVE-TYPE" ,(string name))
       (assert (not (gethash ',name *backend-primitive-type-names*)))
       (setf (gethash ',name *backend-primitive-type-names*)
             (make-primitive-type :name ',name
                                  :scs ',scns
                                  :specifier ',type))
       (/show0 "done with !DEF-PRIMITIVE-TYPE")
       ',name)))

;;; Define NAME to be an alias for RESULT in VOP operand type restrictions.
(defmacro !def-primitive-type-alias (name result)
  ;; Just record the translation.
  `(progn
     (assert (not (assoc ',name *backend-primitive-type-aliases*)))
     (push (cons ',name ,result) *backend-primitive-type-aliases*)
     ',name))


;;;; VOP definition structures
;;;;
;;;; DEFINE-VOP uses some fairly complex data structures at
;;;; meta-compile time, both to hold the results of parsing the
;;;; elaborate syntax and to retain the information so that it can be
;;;; inherited by other VOPs.

;;; FIXME: all VOP-PARSE slots should be readonly.
;;; Unfortunately it acts as both mutable working storage for the DEFINE-VOP
;;; expander, and the immutable object finally produced.

;;; An OPERAND-PARSE object contains stuff we need to know about an
;;; operand or temporary at meta-compile time. Besides the obvious
;;; stuff, we also store the names of per-operand temporaries here.
(defstruct (operand-parse
             (:copier nil)
             #-sb-xc-host (:pure t))
  ;; name of the operand (which we bind to the TN)
  (name nil :type symbol :read-only t)
  ;; the way this operand is used:
  (kind (missing-arg) :read-only t
        :type (member :argument :result :temporary
                      :more-argument :more-result))
  ;; If true, the name of an operand that this operand is targeted to.
  ;; This is only meaningful in :ARGUMENT and :TEMPORARY operands.
  (target nil :type (or symbol null) :read-only t)
  ;; TEMP is a temporary that holds the TN-REF for this operand.
  (temp (make-operand-parse-temp) :type symbol)
  ;; the time that this operand is first live and the time at which it
  ;; becomes dead again. These are TIME-SPECs, as returned by
  ;; PARSE-TIME-SPEC.
  (born nil :read-only t)
  (dies nil :read-only t)
  ;; Variable that is bound to the load TN allocated for this operand, or to
  ;; NIL if no load-TN was allocated.
  (load-tn (make-operand-parse-load-tn) :type symbol :read-only t)
  ;; an expression that tests whether to do automatic operand loading
  (load t :read-only t)
  ;; In a wired or restricted temporary this is the SC the TN is to be
  ;; packed in. Otherwise, if a non-nil list, the names of the SCs that
  ;; this operand is allowed into. If NIL, there is no restriction.
  (scs nil :type (or symbol list) :read-only t)
  ;; If non-null, we are a temp wired to this offset in SC.
  (offset nil :type (or unsigned-byte null) :read-only t)
  (unused-if nil))
(declaim (freeze-type operand-parse))

(defun operand-parse-sc (parse) ; Enforce a single symbol
  (the (and symbol (not null)) (operand-parse-scs parse)))

;;; A VOP-PARSE object holds everything we need to know about a VOP at
;;; meta-compile time.
(defstruct (vop-parse #-sb-xc-host (:pure t))
  (source-location)
  ;; the name of this VOP
  (name nil :type symbol)
  ;; If true, then the name of the VOP we inherit from.
  (inherits nil :type (or symbol null))
  ;; lists of OPERAND-PARSE structures describing the arguments,
  ;; results and temporaries of the VOP
  (args nil :type list)
  (results nil :type list)
  (temps nil :type list)
  ;; OPERAND-PARSE structures containing information about more args
  ;; and results. If null, then there there are no more operands of
  ;; that kind
  (more-args nil :type (or operand-parse null))
  (more-results nil :type (or operand-parse null))
  ;; a list of all the above together
  (operands nil :type list)
  ;; Which results can accept :unused TNs
  (optional-results nil :type list)
  ;; names of variables that should be declared IGNORE
  (ignores () :type list)
  ;; true if this is a :CONDITIONAL VOP. T if a branchful VOP,
  ;; a list of condition descriptor otherwise. See $ARCH/pred.lisp
  ;; for more information.
  (conditional-p nil)
  ;; argument and result primitive types. These are pulled out of the
  ;; operands, since we often want to change them without respecifying
  ;; the operands.
  (arg-types :unspecified :type (or (member :unspecified) list))
  (result-types :unspecified :type (or (member :unspecified) list))
  ;; the guard expression specified, or NIL if none
  (guard nil)
  ;; the cost of and body code for the generator
  (cost 0 :type unsigned-byte)
  (body :unspecified :type (or (member :unspecified) list))
  ;; info for VOP variants. The list of forms to be evaluated to get
  ;; the variant args for this VOP, and the list of variables to be
  ;; bound to the variant args.
  (variant () :type list)
  (variant-vars () :type list)
  ;; variables bound to the VOP and Vop-Node when in the generator body
  (vop-var '.vop. :type symbol)
  (node-var nil :type (or symbol null))
  ;; a list of the names of the codegen-info arguments to this VOP
  (info-args () :type list)
  ;; an efficiency note associated with this VOP
  (note nil :type (or string null))
  ;; a list of the names of functions this VOP is a translation of and
  ;; the policy that allows this translation to be done. :FAST is a
  ;; safe default, since it isn't a safe policy.
  (translate () :type list)
  (ltn-policy :fast :type ltn-policy)
  ;; stuff used by life analysis
  (save-p nil :type (member t nil :compute-only :force-to-stack))
  ;; info about how to emit MOVE-ARG VOPs for the &MORE operand in
  ;; call/return VOPs
  (move-args nil :type (member nil :local-call :full-call :known-return :fixed))
  (before-load :unspecified :type (or (member :unspecified) list))
  (gc-barrier nil))
(declaim (freeze-type vop-parse))
(defprinter (vop-parse)
  name
  (inherits :test inherits)
  args
  results
  temps
  (more-args :test more-args)
  (more-results :test more-results)
  (conditional-p :test conditional-p)
  ignores
  arg-types
  result-types
  cost
  body
  (variant :test variant)
  (variant-vars :test variant-vars)
  (info-args :test info-args)
  (note :test note)
  translate
  ltn-policy
  (save-p :test save-p)
  (move-args :test move-args))

;;; The list of slots in the structure, not including the OPERANDS slot.
;;; Order here is insignificant; it happens to be alphabetical.
(defglobal vop-parse-slot-names
    '(arg-types args before-load body conditional-p cost gc-barrier guard ignores info-args inherits
      ltn-policy more-args more-results move-args name node-var note optional-results result-types
      results save-p source-location temps translate variant variant-vars vop-var))
;; A sanity-check. Of course if this fails, the likelihood is that you can't even
;; get this far in cross-compilaion. So it's probably not worth much.
(eval-when (#+sb-xc :compile-toplevel)
  (assert (equal (length (dd-slots (find-defstruct-description 'vop-parse)))
                 (1+ (length vop-parse-slot-names)))))

(defprinter (operand-parse)
  name
  kind
  (target :test target)
  born
  dies
  (scs :test scs)
  (load :test load)
  (offset :test offset))

;;; Make NAME be the VOP used to move values in the specified FROM-SCs
;;; to the representation of the TO-SCs of each SC pair in SCS.
;;;
;;; If KIND is :MOVE-ARG, then the VOP takes an extra argument,
;;; which is the frame pointer of the frame to move into.
;;;
;;; We record the VOP and costs for all SCs that we can move between
;;; (including implicit loading).
(defmacro define-move-vop (name kind &rest scs)
  (when (or (oddp (length scs)) (null scs))
    (error "malformed SCs spec: ~S" scs))
  (let ((accessor (or (cdr (assoc kind *sc-vop-slots*))
                      (error "unknown kind ~S" kind))))
    `(progn
       ,@(when (eq kind :move)
           `((eval-when (:compile-toplevel :load-toplevel :execute)
               (do-sc-pairs (from-sc to-sc ',scs)
                 (compute-move-costs from-sc to-sc
                                     ,(vop-parse-cost
                                       (vop-parse-or-lose name)))))))

       (let ((vop (template-or-lose ',name)))
         (setf (vop-info-move-vop-p vop) t)
         (do-sc-pairs (from-sc to-sc ',scs)
           (dolist (dest-sc (cons to-sc (sc-alternate-scs to-sc)))
             (let ((vec (,accessor dest-sc)))
               (let ((scn (sc-number from-sc)))
                 (setf (svref vec scn)
                       (adjoin-template vop (svref vec scn))))
               (dolist (sc (append (sc-alternate-scs from-sc)
                                   (sc-constant-scs from-sc)))
                 (let ((scn (sc-number sc)))
                   (setf (svref vec scn)
                         (adjoin-template vop (svref vec scn))))))))))))

;;;; miscellaneous utilities

;;; Find the operand or temporary with the specifed Name in the VOP
;;; Parse. If there is no such operand, signal an error. Also error if
;;; the operand kind isn't one of the specified Kinds. If Error-P is
;;; NIL, just return NIL if there is no such operand.
(defun find-operand (name parse &optional
                          (kinds '(:argument :result :temporary))
                          (error-p t))
  (declare (symbol name) (type vop-parse parse) (list kinds))
  (let ((found (find name (vop-parse-operands parse)
                     :key #'operand-parse-name)))
    (if found
        (unless (member (operand-parse-kind found) kinds)
          (error "Operand ~S isn't one of these kinds: ~S." name kinds))
        (when error-p
          (error "~S is not an operand to ~S." name (vop-parse-name parse))))
    found))

;;; Get the VOP-PARSE structure for NAME or die trying. For all
;;; meta-compile time uses, the VOP-PARSE should be used instead of
;;; the VOP-INFO.
(defun vop-parse-or-lose (name)
  (the vop-parse
       (or (gethash name *backend-parsed-vops*)
           (error "~S is not the name of a defined VOP." name))))

;;; Return a list of LET-forms to parse a TN-REF list into the temps
;;; specified by the operand-parse structures. MORE-OPERAND is the
;;; OPERAND-PARSE describing any more operand, or NIL if none. REFS is
;;; an expression that evaluates into the first TN-REF.
(defun access-operands (operands more-operand refs)
  (declare (list operands))
  (collect ((res))
    (let ((prev refs))
      (dolist (op operands)
        (let ((n-ref (operand-parse-temp op)))
          (res `(,n-ref ,prev))
          (setq prev `(tn-ref-across ,n-ref))))

      (when more-operand
        (res `(,(operand-parse-name more-operand) ,prev))))
    (res)))

;;; This is used with ACCESS-OPERANDS to prevent warnings for TN-REF
;;; temps not used by some particular function. It returns the name of
;;; the last operand, or NIL if OPERANDS is NIL.
(defun ignore-unreferenced-temps (operands)
  (when operands
    (operand-parse-temp (car (last operands)))))

;;; Grab an arg out of a VOP spec, checking the type and syntax and stuff.
(defun vop-spec-arg (spec type &optional (n 1) (last t))
  (let ((len (length spec)))
    (when (<= len n)
      (error "~:R argument missing: ~S" n spec))
    (when (and last (> len (1+ n)))
      (error "extra junk at end of ~S" spec))
    (let ((thing (elt spec n)))
      (unless (typep thing type)
        (error "~:R argument is not a ~S: ~S" n type spec))
      thing)))

;;;; time specs

;;; Return a time spec describing a time during the evaluation of a
;;; VOP, used to delimit operand and temporary lifetimes. The
;;; representation is a fixnum [phase][16-bit sub-phase].
;;; The sub-phase is 0 in the :LOAD and :SAVE phases.
(defun parse-time-spec (spec)
  (let ((dspec (if (atom spec) (list spec 0) spec)))
    (unless (and (= (length dspec) 2)
                 (typep (second dspec) 'unsigned-byte))
      (error "malformed time specifier: ~S" spec))
    (let ((phase (case (first dspec)
                   (:load 0)
                   (:argument 1)
                   (:eval 2)
                   (:result 3)
                   (:save 4)
                   (t
                    (error "unknown phase in time specifier: ~S" spec))) )
          (sub-phase (second dspec)))
      (+ (ash phase 16)
         sub-phase))))

;;;; generation of emit functions

(defun compute-temporaries-description (parse)
  (let ((temps (vop-parse-temps parse))
        (element-type '(unsigned-byte 16)))
    (when temps
      (let ((results (sb-xc:make-array (length temps) :element-type element-type))
            (index 0))
        (dolist (temp temps)
          (declare (type operand-parse temp))
          (let ((sc (operand-parse-sc temp))
                (offset (operand-parse-offset temp)))
            (aver sc)
            (setf (aref results index)
                  (if offset
                      (+ (ash offset (1+ sb-vm:sc-number-bits))
                         (ash (sc-number-or-lose sc) 1)
                         1)
                      (ash (sc-number-or-lose sc) 1))))
          (incf index))
        results))))

(defun compute-ref-ordering (parse)
  (let* ((num-args (+ (length (vop-parse-args parse))
                      (if (vop-parse-more-args parse) 1 0)))
         (num-results (+ (length (vop-parse-results parse))
                         (if (vop-parse-more-results parse) 1 0)))
         (index 0))
    (collect ((refs) (targets))
      (dolist (op (vop-parse-operands parse))
        (when (operand-parse-target op)
          (unless (member (operand-parse-kind op) '(:argument :temporary))
            (error "cannot target a ~S operand: ~S" (operand-parse-kind op)
                   (operand-parse-name op)))
          (let ((target (find-operand (operand-parse-target op) parse
                                      '(:temporary :result))))
            ;; KLUDGE: These formulas must be consistent with those in
            ;; EMIT-VOP, and this is currently maintained by
            ;; hand. -- WHN 2002-01-30, paraphrasing APD
            (targets (+ (* index max-vop-tn-refs)
                        (ecase (operand-parse-kind target)
                          (:result
                           (+ (position-or-lose target
                                                (vop-parse-results parse))
                              num-args))
                          (:temporary
                           (+ (* (position-or-lose target
                                                   (vop-parse-temps parse))
                                 2)
                              1
                              num-args
                              num-results)))))))
        (let ((born (operand-parse-born op))
              (dies (operand-parse-dies op)))
          (ecase (operand-parse-kind op)
            (:argument
             (refs (cons (cons dies nil) index)))
            (:more-argument
             (refs (cons (cons dies nil) index)))
            (:result
             (refs (cons (cons born t) index)))
            (:more-result
             (refs (cons (cons born t) index)))
            (:temporary
             (refs (cons (cons dies nil) index))
             (incf index)
             (refs (cons (cons born t) index))))
          (incf index)))
      (let* ((sorted (stable-sort (refs)
                                  (lambda (x y)
                                    (let ((x-time (car x))
                                          (y-time (car y)))
                                      (if (>= x-time y-time)
                                          (if (>= y-time x-time)
                                              (and (not (cdr x)) (cdr y))
                                              nil)
                                          t)))
                                  :key #'car))
             ;; :REF-ORDERING element type
             ;;
             ;; KLUDGE: was (MOD #.MAX-VOP-TN-REFS), which is still right
             (oe-type '(unsigned-byte 8))
             ;; :TARGETS element-type
             ;;
             ;; KLUDGE: was (MOD #.(* MAX-VOP-TN-REFS 2)), which does
             ;; not correspond to the definition in
             ;; src/compiler/vop.lisp.
             (te-type '(unsigned-byte 16))
             (ordering (sb-xc:make-array (length sorted) :element-type oe-type)))
        (let ((index 0))
          (dolist (ref sorted)
            (setf (aref ordering index) (cdr ref))
            (incf index)))
        `(:num-args ,num-args
          :num-results ,num-results
          :ref-ordering ,ordering
          ,@(when (targets)
              `(:targets ,(coerce (targets) `(vector ,te-type)))))))))

(defun make-emit-function-and-friends (parse)
  `(:temps ,(compute-temporaries-description parse)
    ,@(compute-ref-ordering parse)))

;;;; generator functions

;;; Return an alist that translates from lists of SCs we can load OP
;;; from to the move function used for loading those SCs. We quietly
;;; ignore restrictions to :non-packed (constant) and :unbounded SCs,
;;; since we don't load into those SCs.
(defun find-move-funs (op load-p)
  (collect ((funs))
    (dolist (sc-name (operand-parse-scs op))
      (unless (or (consp sc-name)
                  (getf *backend-cond-scs* sc-name))
        (let* ((sc (sc-or-lose sc-name))
               (scn (sc-number sc))
               (load-scs (append (when load-p
                                   (sc-constant-scs sc))
                                 (sc-alternate-scs sc))))
          (cond
            (load-scs
             (dolist (alt load-scs)
               (unless (member (sc-name alt) (operand-parse-scs op) :test #'eq)
                 (let* ((altn (sc-number alt))
                        (name (if load-p
                                  (svref (sc-move-funs sc) altn)
                                  (svref (sc-move-funs alt) scn)))
                        (found (or (assoc alt (funs) :test #'member)
                                   (rassoc name (funs)))))
                   (unless name
                     (error "no move function defined to ~:[save~;load~] SC ~S ~
                          ~:[to~;from~] from SC ~S"
                            load-p sc-name load-p (sc-name alt)))
                   (cond (found
                          (pushnew alt (car found)))
                         (t
                          (funs (cons (list alt) name))))))))
            ((member (sb-kind (sc-sb sc)) '(:non-packed :unbounded)))
            (t
             (error "SC ~S has no alternate~:[~; or constant~] SCs, yet it is~@
                  mentioned in the restriction for operand ~S"
                    sc-name load-p (operand-parse-name op)))))))
    (funs)))

;;; Return a form to load/save the specified operand when it has a
;;; load TN. For any given SC that we can load from, there must be a
;;; unique load function. If all SCs we can load from have the same
;;; move function, then we just call that when there is a load TN. If
;;; there are multiple possible move functions, then we dispatch off
;;; of the operand TN's type to see which move function to use.
(defun call-move-fun (parse op load-p)
  (let ((funs (find-move-funs op load-p))
        (load-tn (operand-parse-load-tn op)))
    (if funs
        (let* ((tn `(tn-ref-tn ,(operand-parse-temp op)))
               (n-vop (vop-parse-vop-var parse))
               (form (if (rest funs)
                         `(sc-case ,tn
                            ,@(mapcar (lambda (x)
                                        `(,(mapcar #'sc-name (car x))
                                          ,(if load-p
                                               `(,(cdr x) ,n-vop ,tn
                                                 ,load-tn)
                                               `(,(cdr x) ,n-vop ,load-tn
                                                 ,tn))))
                                      funs))
                         (if load-p
                             `(,(cdr (first funs)) ,n-vop ,tn ,load-tn)
                             `(,(cdr (first funs)) ,n-vop ,load-tn ,tn)))))
          (cond (load-p
                 form)
                ((eq (operand-parse-load op) t)
                 `(when ,load-tn ,form))
                (t
                 `(when (eq ,load-tn ,(operand-parse-name op))
                    ,form))))
        `(when ,load-tn
           (error "load TN allocated, but no move function?~@
                   VM definition is inconsistent, recompile and try again.")))))

;;; Return the TN that we should bind to the operand's var in the
;;; generator body. In general, this involves evaluating the :LOAD-IF
;;; test expression.
(defun decide-to-load (parse op)
  (let ((load (operand-parse-load op))
        (load-tn (operand-parse-load-tn op))
        (temp (operand-parse-temp op))
        (loads (and (eq (operand-parse-kind op) :argument)
                    (call-move-fun parse op t))))
    (if (eq load t)
        `(cond (,load-tn
                ,loads
                ,load-tn)
               (t
                (tn-ref-tn ,temp)))
        (collect ((binds)
                  (ignores))
          (dolist (x (vop-parse-operands parse))
            (when (member (operand-parse-kind x) '(:argument :result))
              (let ((name (operand-parse-name x)))
                (binds `(,name (tn-ref-tn ,(operand-parse-temp x))))
                (ignores name))))
          `(cond ((and ,load-tn
                       (let ,(binds)
                         (declare (ignorable ,@(ignores)))
                         ,load))
                  ,loads
                  ,load-tn)
                 (t
                  (tn-ref-tn ,temp)))))))

;;; Make a lambda that parses the VOP TN-REFS, does automatic operand
;;; loading, and runs the appropriate code generator.
(defun make-generator-function (parse)
  (declare (type vop-parse parse))
  (let ((n-vop (vop-parse-vop-var parse))
        (operands (vop-parse-operands parse))
        (n-info (gensym)) (n-variant (gensym))
        (dummy (gensym)))
    (collect ((binds)
              (loads)
              (saves))
      (dolist (op operands)
        (ecase (operand-parse-kind op)
          ((:argument :result)
           (let ((temp (operand-parse-temp op))
                 (name (operand-parse-name op)))
             (cond ((and (operand-parse-load op) (operand-parse-scs op))
                    (binds `(,(operand-parse-load-tn op)
                             (tn-ref-load-tn ,temp)))
                    (binds `(,name ,(decide-to-load parse op)))
                    (when (eq (operand-parse-kind op) :result)
                      (saves (call-move-fun parse op nil))))
                   (t
                    (binds `(,name (tn-ref-tn ,temp)))))))
          (:temporary
           (binds `(,(operand-parse-name op)
                    (tn-ref-tn ,(operand-parse-temp op)))))
          ((:more-argument :more-result))))

      `(named-lambda (vop ,(vop-parse-name parse)) (,n-vop)
         (declare (ignorable ,n-vop))
         (let* (,@(access-operands (vop-parse-args parse)
                                   (vop-parse-more-args parse)
                                   `(vop-args ,n-vop))
                ,@(access-operands (vop-parse-results parse)
                                   (vop-parse-more-results parse)
                                   `(vop-results ,n-vop))
                ,@(access-operands (vop-parse-temps parse) nil
                                   `(vop-temps ,n-vop))
                ,@(when (vop-parse-info-args parse)
                    `((,n-info (vop-codegen-info ,n-vop))
                      ,@(mapcar (lambda (x) `(,x (pop ,n-info)))
                                (vop-parse-info-args parse))))
                ,@(when (vop-parse-variant-vars parse)
                    `((,n-variant (vop-info-variant (vop-info ,n-vop)))
                      ,@(mapcar (lambda (x) `(,x (pop ,n-variant)))
                                (vop-parse-variant-vars parse))))
                ,@(when (vop-parse-node-var parse)
                    `((,(vop-parse-node-var parse) (vop-node ,n-vop))))
                ,@(and (neq (vop-parse-before-load parse) :unspecified)
                       `((,dummy (progn
                                   ,@(vop-parse-before-load parse)))))
                ,@(binds))
           (declare (ignore ,@(vop-parse-ignores parse)
                            ,@(and (neq (vop-parse-before-load parse) :unspecified)
                                   `(,dummy))))
           ,@(loads)
           ;; RETURN-FROM can exit the ASSEMBLE while continuing on with saves.
           (block ,(vop-parse-name parse)
             (assemble ()
               ,@(vop-parse-body parse)))
           ,@(saves))))))

(defun make-after-sc-function (parse)
  (let ((unused-temps
          (remove-if-not #'operand-parse-unused-if
                         (vop-parse-temps parse))))
    (when unused-temps
      (let* ((n-vop (vop-parse-vop-var parse))
             (n-info (gensym))
             (n-variant (gensym))
             (bindings
               `(,@(access-operands (vop-parse-args parse)
                                    (vop-parse-more-args parse)
                                    `(vop-args ,n-vop))
                 ,@(access-operands (vop-parse-results parse)
                                    (vop-parse-more-results parse)
                                    `(vop-results ,n-vop))
                 ,@(and unused-temps
                        (access-operands (vop-parse-temps parse) nil
                                         `(vop-temps ,n-vop)))
                 ,@(when (vop-parse-info-args parse)
                     `((,n-info (vop-codegen-info ,n-vop))
                       ,@(mapcar (lambda (x) `(,x (pop ,n-info)))
                                 (vop-parse-info-args parse))))
                 ,@(when (vop-parse-variant-vars parse)
                     `((,n-variant (vop-info-variant (vop-info ,n-vop)))
                       ,@(mapcar (lambda (x) `(,x (pop ,n-variant)))
                                 (vop-parse-variant-vars parse))))
                 ,@(loop for op in (vop-parse-operands parse)
                         when
                         (ecase (operand-parse-kind op)
                           ((:argument :result)
                            `(,(operand-parse-name op)
                              (tn-ref-tn ,(operand-parse-temp op))))
                           (:temporary
                            (and (operand-parse-unused-if op)
                                 `(,(operand-parse-name op)
                                   (tn-ref-tn ,(operand-parse-temp op)))))
                           ((:more-argument :more-result)))
                         collect it))))
        `(lambda (,n-vop)
           (let* ,bindings
             (declare (ignorable ,@(mapcar #'car bindings)))
             ,@(loop for op in unused-temps
                     collect `(when ,(operand-parse-unused-if op)
                                (setf (tn-kind ,(operand-parse-name op)) :unused)))))))))

(defvar *parse-vop-operand-count*)
(defun make-operand-parse-temp ()
  (symbolicate! #.(find-package "SB-C") "OPERAND-PARSE-TEMP-"
                *parse-vop-operand-count*))
(defun make-operand-parse-load-tn ()
  (symbolicate! #.(find-package "SB-C")
                "OPERAND-PARSE-LOAD-TN-" *parse-vop-operand-count*))

;;; Given a list of operand specifications as given to DEFINE-VOP,
;;; return a list of OPERAND-PARSE structures describing the fixed
;;; operands, and a single OPERAND-PARSE describing any more operand.
;;; If we are inheriting a VOP, we default attributes to the inherited
;;; operand of the same name.
(defun parse-vop-operands (parse specs kind)
  (declare (list specs)
           (type (member :argument :result) kind))
  (let ((num -1)
        (more nil))
    (collect ((operands))
      (dolist (spec specs)
        (unless (and (consp spec) (symbolp (first spec)) (oddp (length spec)))
          (error "malformed operand specifier: ~S" spec))
        (when more
          (error "The MORE operand isn't the last operand: ~S" specs))
        (incf *parse-vop-operand-count*)
        (incf num)
        (let* ((name (first spec))
               (old (if (vop-parse-inherits parse)
                        (find-operand name
                                      (vop-parse-or-lose
                                       (vop-parse-inherits parse))
                                      (list* kind
                                             (if (eq kind :argument)
                                                 '(:more-argument)
                                                 '(:more-result)))
                                      nil)
                        nil))
               (res
                 (nconc (list :kind kind)
                        (if old
                            (list
                             :target (operand-parse-target old)
                             :born (operand-parse-born old)
                             :dies (operand-parse-dies old)
                             :scs (operand-parse-scs old)
                             :load-tn (operand-parse-load-tn old)
                             :load (operand-parse-load old))
                            (ecase kind
                              (:argument
                               (list :born (parse-time-spec :load)
                                     :dies (parse-time-spec `(:argument ,num))))
                              (:result
                               (list :born (parse-time-spec `(:result ,num))
                                     :dies (parse-time-spec :save))))))))
          (do ((tail (rest spec) (cddr tail)))
              ((null tail))
            (let ((key (first tail))
                  (value (second tail)))
              (case key
                (:scs
                 (aver (typep value 'list))
                 (aver (= (length value) (length (remove-duplicates value))))
                 (setq value (copy-list value)))
                (:load-tn
                 (aver (typep value 'symbol)))
                (:load-if
                 (setq key :load))
                (:more
                 (aver (typep value 'boolean))
                 (setq key :kind
                       value (if (eq kind :argument) :more-argument :more-result))
                 (setf (getf res :load) nil)
                 (setq more t))
                (:target
                 (aver (typep value 'symbol)))
                (:from
                 (unless (eq kind :result)
                   (error "can only specify :FROM in a result: ~S" spec))
                 (setq key :born value (parse-time-spec value)))
                (:to
                 (unless (eq kind :argument)
                   (error "can only specify :TO in an argument: ~S" spec))
                 (setq key :dies value (parse-time-spec value)))
                (t
                 (error "unknown keyword in operand specifier: ~S" spec)))
              (setf (getf res key) value)))

          (setq res (apply #'make-operand-parse :name name res)
                more (if more res nil))
          (cond ((not more)
                 (operands res))
                ((operand-parse-target more)
                 (error "cannot specify :TARGET in a :MORE operand"))
                ((operand-parse-load more)
                 (error "cannot specify :LOAD-IF in a :MORE operand")))))
      (values (the list (operands)) more))))

;;; Parse a temporary specification, putting the OPERAND-PARSE
;;; structures in the PARSE structure.
(defun parse-temporary (spec parse)
  (declare (list spec)
           (type vop-parse parse))
  (let ((len (length spec)))
    (unless (>= len 2)
      (error "malformed temporary spec: ~S" spec))
    (unless (listp (second spec))
      (error "malformed options list: ~S" (second spec)))
    (unless (evenp (length (second spec)))
      (error "odd number of arguments in keyword options: ~S" spec))
    (unless (consp (cddr spec))
      (warn "temporary spec allocates no temps:~%  ~S" spec))
    (dolist (name (cddr spec))
      (unless (symbolp name)
        (error "bad temporary name: ~S" name))
      ;; It's almost always a mistake to have overlaps in the operand names.
      ;; But I guess that some users think it's fine?
      #+sb-xc-host
      (when (member name (vop-parse-temps parse) :key #'operand-parse-name)
        (warn "temp ~s already exists in ~s" name (vop-parse-name parse)))
      (incf *parse-vop-operand-count*)
      (let ((res (list :born (parse-time-spec :load)
                       :dies (parse-time-spec :save))))
        (do ((opt (second spec) (cddr opt)))
            ((null opt))
         (let ((key (first opt))
               (value (second opt)))
          (case (first opt)
            (:target
             (setf value (vop-spec-arg opt 'symbol 1 nil)))
            (:sc
             (setf key :scs value (vop-spec-arg opt 'symbol 1 nil)))
            (:offset
             (aver (typep (setq value (eval value)) 'unsigned-byte)))
            (:from
             (setf key :born value (parse-time-spec value)))
            (:to
             (setf key :dies value (parse-time-spec value)))
            ;; backward compatibility...
            (:scs
             (let ((scs (vop-spec-arg opt 'list 1 nil)))
               (unless (= (length scs) 1)
                 (error "must specify exactly one SC for a temporary"))
               (setf value (first scs))))
            (:unused-if)
            (t
             (error "unknown temporary option: ~S" opt)))
          (setf (getf res key) value)))

        (setq res (apply #'make-operand-parse :name name :kind :temporary res))
        (unless (and (>= (operand-parse-dies res)
                         (operand-parse-born res))
                     (< (operand-parse-born res)
                        (operand-parse-dies res)))
          (error "Temporary lifetime doesn't begin before it ends: ~S" spec))

        (unless (operand-parse-scs res)
          (error "must specify :SC for all temporaries: ~S" spec))

        (setf (vop-parse-temps parse)
              (cons res
                    (remove name (vop-parse-temps parse)
                            :key #'operand-parse-name))))))
  (values))

(defun compute-parse-vop-operand-count (parse)
  (declare (type vop-parse parse))
  (labels ((compute-count-aux (parse)
             (declare (type vop-parse parse))
             (if (null (vop-parse-inherits parse))
                 (length (vop-parse-operands parse))
                 (+ (length (vop-parse-operands parse))
                    (compute-count-aux
                     (vop-parse-or-lose (vop-parse-inherits parse)))))))
    (if (null (vop-parse-inherits parse))
        0
        (compute-count-aux (vop-parse-or-lose (vop-parse-inherits parse))))))

;;; the top level parse function: clobber PARSE to represent the
;;; specified options.
(defun parse-define-vop (parse specs inherits)
  (declare (type vop-parse parse) (list specs))
  (let ((*parse-vop-operand-count* (compute-parse-vop-operand-count parse))
        args-p
        results-p
        arg-refs
        arg-refs-p
        result-refs
        result-refs-p)
    (dolist (spec specs)
      (unless (consp spec)
        (error "malformed option specification: ~S" spec))
      (case (first spec)
        (:args
         (setf args-p t)
         (multiple-value-bind (fixed more)
             (parse-vop-operands parse (rest spec) :argument)
           (setf (vop-parse-args parse) fixed)
           (setf (vop-parse-more-args parse) more)))
        (:results
         (setf results-p t)
         (multiple-value-bind (fixed more)
             (parse-vop-operands parse (rest spec) :result)
           (setf (vop-parse-results parse) fixed)
           (setf (vop-parse-more-results parse) more))
         (setf (vop-parse-conditional-p parse) nil))
        (:conditional
         (setf (vop-parse-result-types parse) ())
         (setf (vop-parse-results parse) ())
         (setf (vop-parse-more-results parse) nil)
         (setf (vop-parse-conditional-p parse) (or (rest spec) t)))
        (:temporary
         (parse-temporary spec parse))
        (:generator
         (setf (vop-parse-cost parse)
               (vop-spec-arg spec 'unsigned-byte 1 nil))
         (setf (vop-parse-body parse) (cddr spec)))
        (:before-load
         (setf (vop-parse-before-load parse) (cdr spec)))
        (:info
         (setf (vop-parse-info-args parse) (rest spec)))
        (:ignore
         (setf (vop-parse-ignores parse)
               (append (vop-parse-ignores parse)
                       (rest spec))))
        (:variant
         (setf (vop-parse-variant parse) (rest spec)))
        (:variant-vars
         (let ((vars (rest spec)))
           (setf (vop-parse-variant-vars parse) vars)
           (setf (vop-parse-variant parse)
                 (make-list (length vars) :initial-element nil))))
        (:variant-cost
         (setf (vop-parse-cost parse) (vop-spec-arg spec 'unsigned-byte)))
        (:vop-var
         (setf (vop-parse-vop-var parse) (vop-spec-arg spec 'symbol)))
        (:arg-refs
         (setf arg-refs-p t
               arg-refs (cdr spec)))
        (:result-refs
         (setf result-refs-p t
               result-refs (cdr spec)))
        (:move-args
         (setf (vop-parse-move-args parse)
               (vop-spec-arg spec '(member nil :local-call :full-call
                                    :known-return :fixed))))
        (:node-var
         (setf (vop-parse-node-var parse) (vop-spec-arg spec 'symbol)))
        (:note
         (setf (vop-parse-note parse) (vop-spec-arg spec '(or string null))))
        (:arg-types
         (setf (vop-parse-arg-types parse)
               (parse-vop-operand-types (rest spec) t)))
        (:result-types
         (setf (vop-parse-result-types parse)
               (parse-vop-operand-types (rest spec) nil)))
        (:translate
         (setf (vop-parse-translate parse) (rest spec)))
        (:guard
         (setf (vop-parse-guard parse) (vop-spec-arg spec t)))
        ;; FIXME: :LTN-POLICY would be a better name for this. It
        ;; would probably be good to leave it unchanged for a while,
        ;; though, at least until the first port to some other
        ;; architecture, since the renaming would be a change to the
        ;; interface between
        (:policy
            (setf (vop-parse-ltn-policy parse)
                  (vop-spec-arg spec 'ltn-policy)))
        (:save-p
         (setf (vop-parse-save-p parse)
               (vop-spec-arg spec
                             '(member t nil :compute-only :force-to-stack))))
        (:optional-results
         (setf (vop-parse-optional-results parse)
               (append (vop-parse-optional-results parse)
                       (rest spec))))
        (:gc-barrier
         (setf (vop-parse-gc-barrier parse) (rest spec)))
        (t
         (error "unknown option specifier: ~S" (first spec)))))
    (cond (arg-refs-p
           (loop with refs = arg-refs
                 for arg in (if args-p
                                (vop-parse-args parse)
                                (setf (vop-parse-args parse)
                                      (mapcar #'copy-structure (vop-parse-args parse))))
                 for ref = (pop refs)
                 when ref
                 do (setf (operand-parse-temp arg) ref)))
          ((and inherits
                args-p)
           (loop for inherited-arg in (vop-parse-args inherits)
                 for arg in (vop-parse-args parse)
                 do (setf (operand-parse-temp arg)
                          (operand-parse-temp inherited-arg)))))
    (cond (result-refs-p
           (loop with refs = result-refs
                 for result in (if results-p
                                (vop-parse-results parse)
                                (setf (vop-parse-results parse)
                                      (mapcar #'copy-structure (vop-parse-results parse))))
                 for ref = (pop refs)
                 when ref
                 do (setf (operand-parse-temp result) ref)))
          ((and inherits
                results-p)
           (loop for inherited-result in (vop-parse-results inherits)
                 for result in (vop-parse-results parse)
                 do (setf (operand-parse-temp result)
                          (operand-parse-temp inherited-result)))))
    (values)))

;;;; making costs and restrictions

;;; Given an operand, returns two values:
;;; 1. A SC-vector of the cost for the operand being in that SC,
;;;    including both the costs for move functions and coercion VOPs.
;;; 2. A SC-vector holding the SC that we load into, for any SC
;;;    that we can directly load from.
;;;
;;; In both vectors, unused entries are NIL. LOAD-P specifies the
;;; direction: if true, we are loading, if false we are saving.
(defun compute-loading-costs (op load-p)
  (declare (type operand-parse op))
  (let ((scs (operand-parse-scs op))
        (costs (make-array sb-vm:sc-number-limit :initial-element nil))
        (load-scs (make-array sb-vm:sc-number-limit :initial-element nil))
        (cond-scs))
    (dolist (sc-name (reverse scs))
      (let ((load-sc (gethash sc-name *backend-sc-names*)))
        (cond (load-sc
               (let* ((load-scn (sc-number load-sc)))
                 (setf (svref costs load-scn) 0)
                 (setf (svref load-scs load-scn) t)
                 (dolist (op-sc (append (when load-p
                                          (sc-constant-scs load-sc))
                                        (sc-alternate-scs load-sc)))
                   (let* ((op-scn (sc-number op-sc))
                          (load (if load-p
                                    (aref (sc-load-costs load-sc) op-scn)
                                    (aref (sc-load-costs op-sc) load-scn))))
                     (unless load
                       (error "no move function defined to move ~:[from~;to~] SC ~
                      ~S~%~:[to~;from~] alternate or constant SC ~S"
                              load-p sc-name load-p (sc-name op-sc)))

                     (let ((op-cost (svref costs op-scn)))
                       (when (or (not op-cost) (< load op-cost))
                         (setf (svref costs op-scn) load)))

                     (let ((op-load (svref load-scs op-scn)))
                       (unless (eq op-load t)
                         (pushnew load-scn (svref load-scs op-scn))))))

                 (dotimes (i sb-vm:sc-number-limit)
                   (unless (svref costs i)
                     (let ((op-sc (svref *backend-sc-numbers* i)))
                       (when op-sc
                         (let ((cost (if load-p
                                         (svref (sc-move-costs load-sc) i)
                                         (svref (sc-move-costs op-sc) load-scn))))
                           (when cost
                             (setf (svref costs i) cost)))))))))
              ((let ((cond-sc (getf *backend-cond-scs* sc-name)))
                 (when cond-sc
                   (push cond-sc cond-scs))))
              ((consp sc-name)
               (push sc-name cond-scs))
              (t
               (error "~S is not a defined storage class." sc-name)))))

    (values costs load-scs
            (loop for (cond-sc . test) in cond-scs
                  collect
                  (let* ((load-sc (sc-or-lose cond-sc))
                         (load-scn (sc-number load-sc)))
                    `(setf (svref load-scs ,load-scn)
                           ,(if (symbolp test)
                                `(,test ',(svref load-scs load-scn))
                                `(lambda (tn)
                                   (if (progn ,@test)
                                       t
                                       ',(svref load-scs load-scn))))))))))

(defconstant-eqx +no-costs+
  (make-array sb-vm:sc-number-limit :initial-element 0)
  #'equalp)

(defconstant-eqx +no-loads+
  (make-array sb-vm:sc-number-limit :initial-element t)
  #'equalp)

;;; Pick off the case of operands with no restrictions.
(defun compute-loading-costs-if-any (op load-p)
  (declare (type operand-parse op))
  (if (operand-parse-scs op)
      (compute-loading-costs op load-p)
      (values +no-costs+ +no-loads+)))

(defun compute-costs-and-restrictions-list (ops load-p)
  (declare (list ops))
  (let ((fixups))
   (collect ((costs)
             (scs))
     (dolist (op ops)
       (multiple-value-bind (costs scs fixup) (compute-loading-costs-if-any op load-p)
         (costs costs)
         (cond (fixup
                (setf fixups t)
                (scs `(let ((load-scs (vector ,@(loop for sc across scs
                                                      collect `',sc))))
                        ,@fixup
                        load-scs)))
               (t
                (scs scs)))))
     (values (costs) (scs) fixups))))

(defun make-costs-and-restrictions (parse)
  (multiple-value-bind (arg-costs arg-scs fixups)
      (compute-costs-and-restrictions-list (vop-parse-args parse) t)
    (multiple-value-bind (result-costs result-scs)
        (compute-costs-and-restrictions-list (vop-parse-results parse) nil)
      (multiple-value-bind (more-arg-costs more-arg-scs)
          (and (vop-parse-more-args parse)
               (compute-loading-costs-if-any (vop-parse-more-args parse) t))
          `(:cost ,(vop-parse-cost parse)

            :arg-costs ',arg-costs
            :arg-load-scs ,(if fixups
                               `(list ,@arg-scs)
                               `',arg-scs)
            :result-costs ',result-costs
            :result-load-scs ',result-scs

            :more-arg-costs ',more-arg-costs
            :more-arg-load-scs ',(unless (eq more-arg-costs +no-costs+)
                                   (substitute-if nil #'listp more-arg-scs))

            :more-result-costs
            ',(if (vop-parse-more-results parse)
                  (compute-loading-costs-if-any (vop-parse-more-results parse) nil)
                  nil)
            :optional-results ',(loop for name in (vop-parse-optional-results parse)
                                      collect (position name (vop-parse-results parse) :key #'operand-parse-name)))))))

;;;; operand checking and stuff

;;; Given a list of arg/result restrictions, check for valid syntax
;;; and convert to canonical form.
(defun parse-vop-operand-types (specs args-p)
  (declare (list specs))
  (labels ((primtype-alias-p (spec)
             (cdr (assq spec *backend-primitive-type-aliases*)))
           (parse-operand-type (spec)
             (cond ((eq spec '*) spec)
                   ((symbolp spec)
                    (let ((alias (primtype-alias-p spec)))
                      (if alias
                          (parse-operand-type alias)
                          `(:or ,spec))))
                   ((atom spec)
                    (error "bad thing to be a operand type: ~S" spec))
                   (t
                    (case (first spec)
                      (:or
                       (collect ((results))
                         (dolist (item (cdr spec))
                           (unless (symbolp item)
                             (error "bad PRIMITIVE-TYPE name in ~S: ~S"
                                    spec item))
                           (let ((alias (primtype-alias-p item)))
                             (if alias
                                 (let ((alias (parse-operand-type alias)))
                                   (unless (eq (car alias) :or)
                                     (error "can't include primitive-type ~
                                             alias ~S in an :OR restriction: ~S"
                                            item spec))
                                   (dolist (x (cdr alias))
                                     (results x)))
                                 (results item))))
                         `(:or ,@(remove-duplicates (results) :test #'eq))))
                      (:constant
                       (unless args-p
                         (error "can't :CONSTANT for a result"))
                       (unless (= (length spec) 2)
                         (error "bad :CONSTANT argument type spec: ~S" spec))
                       spec)
                      (t
                       (error "bad thing to be a operand type: ~S" spec)))))))
    (mapcar #'parse-operand-type specs)))

;;; Check the consistency of OP's SC restrictions with the specified
;;; primitive-type restriction. :CONSTANT operands have already been
;;; filtered out, so only :OR and * restrictions are left.
;;;
;;; We check that every representation allowed by the type can be
;;; directly loaded into some SC in the restriction, and that the type
;;; allows every SC in the restriction. With *, we require that T
;;; satisfy the first test, and omit the second.
(defun check-operand-type-scs (parse op type load-p)
  (declare (type vop-parse parse) (type operand-parse op))
  (let ((ptypes (if (eq type '*) (list t) (rest type)))
        (scs (operand-parse-scs op)))
    (when scs
      (multiple-value-bind (costs load-scs) (compute-loading-costs op load-p)
        (declare (ignore costs))
        (dolist (ptype ptypes)
          (unless (dolist (rep (primitive-type-scs
                                (primitive-type-or-lose ptype))
                               nil)
                    (when (svref load-scs rep) (return t)))
            (error "In the ~A ~:[result~;argument~] to VOP ~S,~@
                    none of the SCs allowed by the operand type ~S can ~
                    directly be loaded~@
                    into any of the restriction's SCs:~%  ~S~:[~;~@
                    [* type operand must allow T's SCs.]~]"
                   (operand-parse-name op) load-p (vop-parse-name parse)
                   ptype
                   scs (eq type '*)))))

      (dolist (sc scs)
        (unless (or (eq type '*)
                    (dolist (ptype ptypes nil)
                      (when (sc-allowed-by-primitive-type
                             (or (gethash (if (consp sc)
                                              (car sc)
                                              sc)
                                          *backend-sc-names*)
                                 (sc-or-lose (car (getf *backend-cond-scs* sc)))
                                 (error "~S is not a defined storage class." sc))
                             (primitive-type-or-lose ptype))
                        (return t)))
                    #+arm64
                    (eq sc 'sb-vm::zero))
          (warn "~:[Result~;Argument~] ~A to VOP ~S~@
                 has SC restriction ~S which is ~
                 not allowed by the operand type:~%  ~S"
                load-p (operand-parse-name op) (vop-parse-name parse)
                sc type)))))

  (values))

;;; If the operand types are specified, then check the number specified
;;; against the number of defined operands.
(defun check-operand-types (parse ops more-op types load-p)
  (declare (type vop-parse parse) (list ops)
           (type (or list (member :unspecified)) types)
           (type (or operand-parse null) more-op))
  (unless (eq types :unspecified)
    (let ((num (+ (length ops) (if more-op 1 0))))
      (unless (= (count-if-not (lambda (x)
                                 (and (consp x)
                                      (eq (car x) :constant)))
                               types)
                 num)
        (error "expected ~W ~:[result~;argument~] type~P: ~S"
               num load-p types num)))

    (when more-op
      (let ((mtype (car (last types))))
        (when (and (consp mtype) (eq (first mtype) :constant))
          (error "can't use :CONSTANT on VOP more args")))))

  (when (vop-parse-translate parse)
    (let ((types (specify-operand-types types ops more-op)))
      (mapc (lambda (x y)
              (check-operand-type-scs parse x y load-p))
            (if more-op (butlast ops) ops)
            (remove-if (lambda (x)
                         (and (consp x)
                              (eq (car x) ':constant)))
                       (if more-op (butlast types) types)))))

  (values))

(defun set-vop-parse-operands (parse)
  (declare (type vop-parse parse))
  (setf (vop-parse-operands parse)
        (append (vop-parse-args parse)
                (if (vop-parse-more-args parse)
                    (list (vop-parse-more-args parse)))
                (vop-parse-results parse)
                (if (vop-parse-more-results parse)
                    (list (vop-parse-more-results parse)))
                (vop-parse-temps parse))))

;;;; function translation stuff

;;; Return forms to establish this VOP as a IR2 translation template
;;; for the :TRANSLATE functions specified in the VOP-PARSE. We also
;;; set the PREDICATE attribute for each translated function when the
;;; VOP is conditional, causing IR1 conversion to ensure that a call
;;; to the translated is always used in a predicate position.
(defun set-up-fun-translation (parse n-template)
  (declare (type vop-parse parse))
  (mapcar (lambda (name)
            `(let ((info (fun-info-or-lose ',name)))
               (setf (fun-info-templates info)
                     (adjoin-template ,n-template (fun-info-templates info)))
               ,@(when (vop-parse-conditional-p parse)
                   '((setf (fun-info-attributes info)
                           (attributes-union
                            (ir1-attributes predicate)
                            (fun-info-attributes info)))))))
          (vop-parse-translate parse)))

;;; Return a form that can be evaluated to get the TEMPLATE operand type
;;; restriction from the given specification.
(defun make-operand-type (type)
  (cond ((eq type '*) ''*)
        ((symbolp type)
         ``(:or ,(primitive-type-or-lose ',type)))
        (t
         (ecase (car type)
           (:or
            ``(:or ,,@(mapcar (lambda (type)
                                `(primitive-type-or-lose ',type))
                              (rest type))))
           (:constant
            ``(:constant . ,',(second type)))))))

(defun specify-operand-types (types ops more-ops)
  (if (eq types :unspecified)
      (make-list (+ (length ops) (if more-ops 1 0)) :initial-element '*)
      types))

;;; Return a list of forms to use as &KEY args to MAKE-VOP-INFO for
;;; setting up the template argument and result types. Here we make an
;;; initial dummy TEMPLATE-TYPE, since it is awkward to compute the
;;; type until the template has been made.
(defun make-vop-info-types (parse)
  (let* ((more-args (vop-parse-more-args parse))
         (all-args (specify-operand-types (vop-parse-arg-types parse)
                                          (vop-parse-args parse)
                                          more-args))
         (args (if more-args (butlast all-args) all-args))
         (more-arg (when more-args (car (last all-args))))
         (more-results (vop-parse-more-results parse))
         (all-results (specify-operand-types (vop-parse-result-types parse)
                                             (vop-parse-results parse)
                                             more-results))
         (results (if more-results (butlast all-results) all-results))
         (more-result (when more-results (car (last all-results))))
         (conditional (vop-parse-conditional-p parse)))

    `(:type (specifier-type '(function () nil))
      :arg-types (list ,@(mapcar #'make-operand-type args))
      :more-args-type ,(when more-args (make-operand-type more-arg))
      :result-types ,(cond ((eq conditional t)
                            :conditional)
                           (conditional
                            `'(:conditional . ,conditional))
                           (t
                            `(list ,@(mapcar #'make-operand-type results))))
      :more-results-type ,(when more-results
                            (make-operand-type more-result)))))

;;;; setting up VOP-INFO

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *slot-inherit-alist*
    '((:generator-function . vop-info-generator-function))))

;;; This is something to help with inheriting VOP-INFO slots. We
;;; return a keyword/value pair that can be passed to the constructor.
;;; SLOT is the keyword name of the slot, Parse is a form that
;;; evaluates to the VOP-PARSE structure for the VOP inherited. If
;;; PARSE is NIL, then we do nothing. If the TEST form evaluates to
;;; true, then we return a form that selects the named slot from the
;;; VOP-INFO structure corresponding to PARSE. Otherwise, we return
;;; the FORM so that the slot is recomputed.
(defmacro inherit-vop-info (slot parse test form)
  `(if (and ,parse ,test)
       (list ,slot `(,',(or (cdr (assoc slot *slot-inherit-alist*))
                            (error "unknown slot ~S" slot))
                     (template-or-lose ',(vop-parse-name ,parse))))
       (list ,slot ,form)))

;;; Return a form that creates a VOP-INFO structure which describes VOP.
(defun set-up-vop-info (iparse parse)
  (declare (type vop-parse parse) (type (or vop-parse null) iparse))
  (let ((same-operands
         (and iparse
              (equal (vop-parse-operands parse)
                     (vop-parse-operands iparse))
              (equal (vop-parse-info-args iparse)
                     (vop-parse-info-args parse))))
        (variant (vop-parse-variant parse)))

    (let ((nvars (length (vop-parse-variant-vars parse))))
      (unless (= (length variant) nvars)
        (error "expected ~W variant values: ~S" nvars variant)))

    `(make-vop-info
      :name ',(vop-parse-name parse)
      ,@(make-vop-info-types parse)
      :guard ,(awhen (vop-parse-guard parse)
                (if (typep it '(cons (eql lambda)))
                    it
                    `(lambda (node) (declare (ignore node)) ,it)))
      :note ',(vop-parse-note parse)
      :info-arg-count ,(- (length (vop-parse-info-args parse))
                          (if (vop-parse-gc-barrier parse)
                              1
                              0))
      :ltn-policy ',(vop-parse-ltn-policy parse)
      :save-p ',(vop-parse-save-p parse)
      :move-args ',(vop-parse-move-args parse)
      ,@(make-costs-and-restrictions parse)
      ,@(make-emit-function-and-friends parse)
      ,@(inherit-vop-info :generator-function iparse
          (and same-operands
               (equal (vop-parse-body parse) (vop-parse-body iparse)))
          (unless (eq (vop-parse-body parse) :unspecified)
            (make-generator-function parse)))
      :variant (list ,@variant)
      :after-sc-selection
      ;; TODO: inherit it?
      ,(make-after-sc-function parse)
      :gc-barrier ',(vop-parse-gc-barrier parse))))

;;; Define the symbol NAME to be a Virtual OPeration in the compiler.
;;; If specified, INHERITS is the name of a VOP that we default
;;; unspecified information from. Each SPEC is a list beginning with a
;;; keyword indicating the interpretation of the other forms in the
;;; SPEC:
;;;
;;; :ARGS {(Name {Key Value}*)}*
;;; :RESULTS {(Name {Key Value}*)}*
;;;     The Args and Results are specifications of the operand TNs passed
;;;     to the VOP. If there is an inherited VOP, any unspecified options
;;;     are defaulted from the inherited argument (or result) of the same
;;;     name. The following operand options are defined:
;;;
;;;     :SCs (SC*)
;;;         :SCs specifies good SCs for this operand. Other SCs will
;;;         be penalized according to move costs. A load TN will be
;;;         allocated if necessary, guaranteeing that the operand is
;;;         always one of the specified SCs.
;;;
;;;     :LOAD-TN Load-Name
;;;         Load-Name is bound to the load TN allocated for this
;;;         operand, or to NIL if no load TN was allocated.
;;;
;;;     :LOAD-IF EXPRESSION
;;;         Controls whether automatic operand loading is done.
;;;         EXPRESSION is evaluated with the fixed operand TNs bound.
;;;         If EXPRESSION is true, then loading is done and the variable
;;;         is bound to the load TN in the generator body. Otherwise,
;;;         loading is not done, and the variable is bound to the actual
;;;         operand.
;;;
;;;     :MORE T-or-NIL
;;;         If specified, NAME is bound to the TN-REF for the first
;;;         argument or result following the fixed arguments or results.
;;;         A :MORE operand must appear last, and cannot be targeted or
;;;         restricted.
;;;
;;;     :TARGET Operand
;;;         This operand is targeted to the named operand, indicating a
;;;         desire to pack in the same location. Not legal for results.
;;;
;;;     :FROM Time-Spec
;;;     :TO Time-Spec
;;;         Specify the beginning or end of the operand's lifetime.
;;;         :FROM can only be used with results, and :TO only with
;;;         arguments. The default for the N'th argument/result is
;;;         (:ARGUMENT N)/(:RESULT N). These options are necessary
;;;         primarily when operands are read or written out of order.
;;;
;;; :CONDITIONAL [Condition-descriptor+]
;;;     This is used in place of :RESULTS with conditional branch VOPs.
;;;     There are no result values: the result is a transfer of control.
;;;     The target label is passed as the first :INFO arg. The second
;;;     :INFO arg is true if the sense of the test should be negated.
;;;     A side effect is to set the PREDICATE attribute for functions
;;;     in the :TRANSLATE option.
;;;
;;;     If some condition descriptors are provided, this is a flag-setting
;;;     VOP. Descriptors are interpreted in an architecture-dependent
;;;     manner. See the BRANCH-IF VOP in $ARCH/pred.lisp.
;;;
;;; :TEMPORARY ({Key Value}*) Name*
;;;     Allocate a temporary TN for each Name, binding that variable to
;;;     the TN within the body of the generators. In addition to :TARGET
;;;     (which is is the same as for operands), the following options are
;;;     defined:
;;;
;;;     :SC SC-Name
;;;     :OFFSET SB-Offset
;;;         Force the temporary to be allocated in the specified SC
;;;         with the specified offset. Offset is evaluated at
;;;         macroexpand time. If Offset is omitted, the register
;;;         allocator chooses a free location in SC. If both SC and
;;;         Offset are omitted, then the temporary is packed according
;;;         to its primitive type.
;;;
;;;     :FROM Time-Spec
;;;     :TO Time-Spec
;;;         Similar to the argument/result option, this specifies the
;;;         start and end of the temporaries' lives. The defaults are
;;;         :LOAD and :SAVE, i.e. the duration of the VOP. The other
;;;         intervening phases are :ARGUMENT, :EVAL and :RESULT.
;;;         Non-zero sub-phases can be specified by a list, e.g. by
;;;         default the second argument's life ends at (:ARGUMENT 1).
;;;
;;; :GENERATOR Cost Form*
;;;     Specifies the translation into assembly code. Cost is the
;;;     estimated cost of the code emitted by this generator. The body
;;;     is arbitrary Lisp code that emits the assembly language
;;;     translation of the VOP. An ASSEMBLE form is wrapped around
;;;     the body, so code may be emitted by using the local INST macro.
;;;     During the evaluation of the body, the names of the operands
;;;     and temporaries are bound to the actual TNs.
;;;
;;; :INFO Name*
;;;     Define some magic arguments that are passed directly to the code
;;;     generator. The corresponding trailing arguments to VOP or
;;;     %PRIMITIVE are stored in the VOP structure. Within the body
;;;     of the generators, the named variables are bound to these
;;;     values. Except in the case of :CONDITIONAL VOPs, :INFO arguments
;;;     cannot be specified for VOPS that are the direct translation
;;;     for a function (specified by :TRANSLATE).
;;;
;;; :IGNORE Name*
;;;     Causes the named variables to be declared IGNORE in the
;;;     generator body.
;;;
;;; :VARIANT Thing*
;;; :VARIANT-VARS Name*
;;;     These options provide a way to parameterize families of VOPs
;;;     that differ only trivially. :VARIANT makes the specified
;;;     evaluated Things be the "variant" associated with this VOP.
;;;     :VARIANT-VARS causes the named variables to be bound to the
;;;     corresponding Things within the body of the generator.
;;;
;;; :VARIANT-COST Cost
;;;     Specifies the cost of this VOP, overriding the cost of any
;;;     inherited generator.
;;;
;;; :NOTE {String | NIL}
;;;     A short noun-like phrase describing what this VOP "does", i.e.
;;;     the implementation strategy. If supplied, efficiency notes will
;;;     be generated when type uncertainty prevents :TRANSLATE from
;;;     working. NIL inhibits any efficiency note.
;;;
;;; :ARG-TYPES    {* | PType | (:OR PType*) | (:CONSTANT Type)}*
;;; :RESULT-TYPES {* | PType | (:OR PType*)}*
;;;     Specify the template type restrictions used for automatic
;;;     translation. If there is a :MORE operand, the last type is the
;;;     more type. :CONSTANT specifies that the argument must be a
;;;     compile-time constant of the specified Lisp type. The constant
;;;     values of :CONSTANT arguments are passed as additional :INFO
;;;     arguments rather than as :ARGS.
;;;
;;; :TRANSLATE Name*
;;;     This option causes the VOP template to be entered as an IR2
;;;     translation for the named functions.
;;;
;;; :POLICY {:SMALL | :SMALL-SAFE | :FAST | :SAFE | :FAST-SAFE}
;;;     Specifies the policy under which this VOP is the best translation.
;;;
;;; :GUARD Form
;;;     Specifies a Form that is evaluated in the global environment.
;;;     If form returns NIL, then emission of this VOP is prohibited
;;;     even when all other restrictions are met.
;;;     As an additional possibility, if Form is a lambda expression,
;;;     then it is funcalled with the node under consideration.
;;;
;;; :VOP-VAR Name
;;; :NODE-VAR Name
;;;     In the generator, bind the specified variable to the VOP or
;;;     the Node that generated this VOP.
;;;
;;; :SAVE-P {NIL | T | :COMPUTE-ONLY | :FORCE-TO-STACK}
;;;     Indicates how a VOP wants live registers saved.
;;;
;;; :MOVE-ARGS {NIL | :FULL-CALL | :LOCAL-CALL | :KNOWN-RETURN}
;;;     Indicates if and how the more args should be moved into a
;;;     different frame.
(defmacro define-vop ((&optional name inherits) &body specs)
  (%define-vop name inherits specs t))

(defun %define-vop (name inherits specs set)
  (declare (type symbol name))
  ;; Parse the syntax into a VOP-PARSE structure, and then expand into
  ;; code that creates the appropriate VOP-INFO structure at load time.
  ;; We implement inheritance by copying the VOP-PARSE structure for
  ;; the inherited structure.
  (let* ((inherited-parse (when inherits
                            (vop-parse-or-lose inherits)))
         (parse (if inherits
                    (copy-vop-parse inherited-parse)
                    (make-vop-parse)))
         (n-res (gensym)))
    (unless name
      (let ((clause (assoc :translate specs)))
        (when (singleton-p (cdr clause))
          (setf name (cadr clause)))))
    (when set
      (aver (typep name '(and symbol (not null))))
      (setf (vop-parse-name parse) name))
    (setf (vop-parse-inherits parse) inherits)

    (parse-define-vop parse specs inherited-parse)
    (set-vop-parse-operands parse)
    (check-operand-types parse
                         (vop-parse-args parse)
                         (vop-parse-more-args parse)
                         (vop-parse-arg-types parse)
                         t)
    (check-operand-types parse
                         (vop-parse-results parse)
                         (vop-parse-more-results parse)
                         (vop-parse-result-types parse)
                         nil)
    (if set
        `(progn
           (eval-when (:compile-toplevel)
             (setf (gethash ',name *backend-parsed-vops*) ',parse))
           (register-vop-parse
            ,@(macrolet
                  ((quotify-slots ()
                     (collect ((forms))
                       (dolist (x vop-parse-slot-names (cons 'list (forms)))
                         (let ((reader (package-symbolicate (sb-xc:symbol-package 'vop-parse)
                                                            "VOP-PARSE-" x)))
                           (forms
                            (case x
                              (source-location ''(source-location))
                              ((temps args results) `(quotify-list (,reader parse)))
                              ((more-args more-results) `(quotify (,reader parse)))
                              (t `(list 'quote (,reader parse))))))))))
                (labels ((quotify (operand-or-nil)
                           (when operand-or-nil
                             (list 'quote (quotify-1 operand-or-nil))))
                         (quotify-list (operands)
                           (list 'quote (mapcar #'quotify-1 operands)))
                         (quotify-1 (x) ; Return everything except the KIND, quoted
                           `(,(operand-parse-name x)
                             ,(operand-parse-target x) ,(operand-parse-temp x)
                             ,(operand-parse-born x) ,(operand-parse-dies x)
                             ,(operand-parse-load-tn x) ,(operand-parse-load x)
                             ,(operand-parse-scs x) ,(operand-parse-offset x))))
                  (quotify-slots))))
           ,@(unless (eq (vop-parse-body parse) :unspecified)
               `((let ((,n-res ,(set-up-vop-info inherited-parse parse)))
                   (store-vop-info ,n-res)
                   ,@(set-up-fun-translation parse n-res))))
           ',name)
        `(let ((info ,(set-up-vop-info inherited-parse parse)))
           (setf (vop-info-type info)
                 (specifier-type (template-type-specifier info)))
           info))))

;;; (inline-vop
;;;     (((param unsigned-reg unsigned-num :to :save) arg)
;;;      ((temp unsigned-reg unsigned-num))
;;;      ((temp2))) ;; will reuse the previous specifications
;;;     ((result unsigned-reg unsigned-num))
;;;   (inst x result temp param))
(defmacro inline-vop (vars results &body body)
  (collect ((input)
            (args)
            (arg-types)
            (infos)
            (temps)
            (results)
            (result-types))
    (flet ((sc-to-primtype (sc)
             (case sc
               (sb-vm::any-reg
                'fixnum)
               (sb-vm::unsigned-reg
                'sb-vm::unsigned-num)
               (sb-vm::signed-reg
                'sb-vm::signed-num)
               (sb-vm::sap-reg
                'system-area-pointer)
               (sb-vm::descriptor-reg
                t)
               (sb-vm::single-reg
                'single-float)
               (sb-vm::double-reg
                'double-float)
               (sb-vm::complex-double-reg
                'complex-double-float)
               (sb-vm::complex-single-reg
                'complex-single-float)
               (t
                '*)))
           (primtype-to-type (type)
             (case type
               (sb-vm::unsigned-num
                'word)
               (sb-vm::signed-num
                'sb-vm:signed-word)
               (sb-vm::tagged-num
                'fixnum)
               (complex-double-float
                '(complex double-float))
               (complex-single-float
                '(complex single-float))
               (* t)
               (t (primitive-type-specifier (primitive-type-or-lose type))))))
      (loop for (var arg) in vars
            for (name this-sc) = var
            for (nil sc type . rest) = (if this-sc
                                           var
                                           prev)
            for prev = (if this-sc
                           var
                           prev)
            do (cond ((eq name :info)
                      (infos this-sc)
                      (input arg))
                     (arg
                      (args (list* name :scs (list sc) rest))
                      (let ((type (or type (sc-to-primtype sc))))
                        (arg-types type)
                        (input `(the ,(primtype-to-type type) ,arg))))
                     (t
                      (temps `(:temporary (:sc ,sc ,@rest)
                                          ,name)))))
      (loop for result in results
            for (name this-sc) = result
            for (nil sc type . rest) = (if this-sc
                                           result
                                           prev)
            for prev = (if this-sc
                           result
                           prev)
            do (results (list* name :scs (list sc) rest))
               (result-types (or type (sc-to-primtype sc))))
      `(truly-the
        (values ,@(mapcar #'primtype-to-type (result-types)) &optional)
        (inline-%primitive
         ,(eval (%define-vop nil nil
                             (delete nil
                                     (list* (and (args)
                                                 (list* :args (args)))
                                            (and (arg-types)
                                                 (list* :arg-types (arg-types)))
                                            (and (results)
                                                 (list* :results (results)))
                                            (and (result-types)
                                                 (list* :result-types (result-types)))
                                            (and (infos)
                                                 (list* :info (infos)))
                                            (list* :generator 0 body)
                                            (temps)))
                             nil))
         ,@(input))))))

(macrolet
    ((def ()
       `(defun register-vop-parse ,vop-parse-slot-names
          ;; Try to share each OPERAND-PARSE structure with a similar existing one.
          (labels ((share-list (operand-specs accessor kind)
                     (let ((new (mapcar (lambda (x) (share x kind)) operand-specs)))
                       (dohash ((key parse) *backend-parsed-vops* :result new)
                         (declare (ignore key))
                         (when (equal (funcall accessor parse) new)
                           (return (funcall accessor parse))))))
                   (share (operand-spec kind)
                     ;; OPERAND-PARSE structures are immutable. Scan all vops for one
                     ;; with an operand matching OPERAND-SPEC, and use that if found.
                     (destructuring-bind (name targ temp born dies load-tn load scs offs)
                         operand-spec
                       (let ((op (make-operand-parse
                                  :name name :kind kind :target targ :temp temp
                                  :born born :dies dies :load-tn load-tn :load load
                                  :scs scs :offset offs)))
                         (dohash ((key parse) *backend-parsed-vops* :result op)
                           (declare (ignore key))
                           (awhen (find op (vop-parse-operands parse) :test #'operand=)
                             (return it))))))
                   (operand= (a b)
                     ;; EQUALP is too weak a comparator for arbitrary sexprs,
                     ;; since (EQUALP "foo" #(#\F #\O #\O)) is T, not that
                     ;; we expect such weirdness in the LOAD-IF expression.
                     (and (equal (operand-parse-load a) (operand-parse-load b))
                          (equalp a b))))
            (setq temps (share-list temps #'vop-parse-temps :temporary)
                  args (share-list args #'vop-parse-args :argument)
                  results (share-list results #'vop-parse-results :result))
            (when more-args (setq more-args (share more-args :more-argument)))
            (when more-results (setq more-results (share more-results :more-result))))
          (let ((parse
                  (make-vop-parse ,@(mapcan (lambda (x) (list (keywordicate x) x))
                                            vop-parse-slot-names))))
            (set-vop-parse-operands parse)
            (setf (gethash name *backend-parsed-vops*) parse)))))
  (def))

(defun store-vop-info (vop-info)
  ;; This is an inefficent way to perform coalescing, but it doesn't matter.
  (let* ((my-type-spec (template-type-specifier vop-info))
         (my-type (specifier-type my-type-spec)))
    (unless (block found
              (maphash (lambda (name other)
                         (declare (ignore name))
                         ;; we get better coaelesecing by TYPE= rather than
                         ;; EQUALP on (template-type-specifier vop-info)
                         ;; because some types have multiple spellings.
                         (when (type= (vop-info-type other) my-type)
                           (setf (vop-info-type vop-info) (vop-info-type other))
                           (return-from found t)))
                       *backend-template-names*))
      (setf (vop-info-type vop-info) (specifier-type my-type-spec))))
  (flet ((find-equalp (accessor)
           ;; Read the slot from VOP-INFO and try to find any other vop-info
           ;; that has an EQUALP value in that slot, returning that value.
           ;; Failing that, try again at a finer grain.
           (let ((my-val (funcall accessor vop-info))) ; list of vectors
             (maphash (lambda (name other)
                        (declare (ignore name))
                        (let ((other-val (funcall accessor other)))
                          (when (equalp other-val my-val)
                            (return-from find-equalp other-val))))
                      *backend-template-names*)
             (unless (and (listp my-val) (vectorp (car my-val)))
               (return-from find-equalp my-val))
             (mapl (lambda (cell)
                     (let ((my-vector (car cell)))
                       (block found
                         (maphash (lambda (name other)
                                    (declare (ignore name))
                                    (dolist (other-vector
                                             (funcall accessor other))
                                      (when (equalp other-vector my-vector)
                                        (rplaca cell other-vector)
                                        (return-from found))))
                                  *backend-template-names*))))
                   (copy-list my-val))))) ; was a quoted constant, don't mutate
    (macrolet ((try-coalescing (accessor)
                 `(setf (,accessor vop-info) (find-equalp #',accessor))))
      (try-coalescing vop-info-arg-types)
      (try-coalescing vop-info-arg-costs)
      (try-coalescing vop-info-arg-load-scs)
      (try-coalescing vop-info-result-types)
      (try-coalescing vop-info-result-costs)
      (try-coalescing vop-info-result-load-scs)
      (try-coalescing vop-info-more-arg-costs)
      (try-coalescing vop-info-more-result-costs)
      (try-coalescing vop-info-temps)
      (try-coalescing vop-info-ref-ordering)
      (try-coalescing vop-info-targets)))
  ;; vop rdefinition should be allowed, but a dup in the cross-compiler
  ;; is probably a mistake. REGISTER-VOP-PARSE is the wrong place
  ;; to check this, because parsing has both compile-time and load-time
  ;; effects, since inheritance is computed at compile-time.
  ;; And there are false positives with any DEFINE-VOP in an assembler file
  ;; because those are processed twice. I don't know what to do.
  #+nil (when (gethash (vop-info-name vop-info) *backend-template-names*)
                 (warn "Duplicate vop name: ~s" vop-info))
  (setf (gethash (vop-info-name vop-info) *backend-template-names*)
        vop-info))

(defun undefine-vop (name)
  (let ((parse (gethash name *backend-parsed-vops*)))
    (dolist (translate (vop-parse-translate parse))
      (let ((info (info :function :info translate)))
        (setf (fun-info-templates info)
              (delete name (fun-info-templates info)
                      :key #'vop-info-name))
        (format t "~&~s has ~d templates~%" translate (length (fun-info-templates info)))))
    (remhash name *backend-parsed-vops*)
    (remhash name *backend-template-names*)))

;;;; emission macros

;;; Return code to make a list of VOP arguments or results, linked by
;;; TN-REF-ACROSS. The first value is code, the second value is LET*
;;; forms, and the third value is a variable that evaluates to the
;;; head of the list, or NIL if there are no operands. Fixed is a list
;;; of forms that evaluate to TNs for the fixed operands. TN-REFS will
;;; be made for these operands according using the specified value of
;;; WRITE-P. More is an expression that evaluates to a list of TN-REFS
;;; that will be made the tail of the list. If it is constant NIL,
;;; then we don't bother to set the tail.
(defun make-operand-list (fixed more write-p)
  (collect ((forms)
            (binds))
    (let ((n-head nil)
          (n-prev nil))
      (dolist (op fixed)
        (multiple-value-bind (op lvar)
            (if (typep op '(cons (eql :lvar)))
                (values (third op) (second op))
                op)
          (let ((n-ref (gensym)))
            (binds `(,n-ref (reference-tn ,op ,write-p)))
            (when lvar
              (forms `(setf (tn-ref-type ,n-ref) (lvar-type ,lvar))))
            (if n-prev
                (forms `(setf (tn-ref-across ,n-prev) ,n-ref))
                (setq n-head n-ref))
            (setq n-prev n-ref))))

      (when more
        (let ((n-more (gensym)))
          (binds `(,n-more ,more))
          (if n-prev
              (forms `(setf (tn-ref-across ,n-prev) ,n-more))
              (setq n-head n-more))))

      (values (forms) (binds) n-head))))

;;; Emit-Template Node Block Template Args Results [Info]
;;;
;;; Call the emit function for TEMPLATE, linking the result in at the
;;; end of BLOCK.
(defmacro emit-template (node block template args results &optional info)
  `(emit-and-insert-vop ,node ,block ,template ,args ,results nil
                        ,@(when info `(,info))))

;;; VOP Name Node Block Arg* Info* Result*
;;;
;;; Emit the VOP (or other template) NAME at the end of the IR2-BLOCK
;;; BLOCK, using NODE for the source context. The interpretation of
;;; the remaining arguments depends on the number of operands of
;;; various kinds that are declared in the template definition. VOP
;;; cannot be used for templates that have more-args or more-results,
;;; since the number of arguments and results is indeterminate for
;;; these templates. Use VOP* instead.
;;;
;;; ARGS and RESULTS are the TNs that are to be referenced by the
;;; template as arguments and results. If the template has
;;; codegen-info arguments, then the appropriate number of INFO forms
;;; following the arguments are used for codegen info.
(defmacro vop (name node block &rest operands)
  (let* ((parse (vop-parse-or-lose name))
         (arg-count (length (vop-parse-args parse)))
         (result-count (length (vop-parse-results parse)))
         (info-count (- (length (vop-parse-info-args parse))
                        (if (vop-parse-gc-barrier parse)
                            1
                            0)))
         (noperands (+ arg-count result-count info-count))
         (n-node (gensym))
         (n-block (gensym))
         (n-template (gensym)))

    (when (or (vop-parse-more-args parse) (vop-parse-more-results parse))
      (error "cannot use VOP with variable operand count templates"))
    (unless (= noperands (length operands))
      (error "called with ~W operands, but was expecting ~W"
             (length operands) noperands))

    (multiple-value-bind (acode abinds n-args)
        (make-operand-list (subseq operands 0 arg-count) nil nil)
      (multiple-value-bind (rcode rbinds n-results)
          (make-operand-list (subseq operands (+ arg-count info-count)) nil t)

        (collect ((ibinds)
                  (ivars))
          (dolist (info (subseq operands arg-count (+ arg-count info-count)))
            (let ((temp (gensym)))
              (ibinds `(,temp ,info))
              (ivars temp)))

          `(let* ((,n-node ,node)
                  (,n-block ,block)
                  (,n-template (template-or-lose ',name))
                  ,@abinds
                  ,@(ibinds)
                  ,@rbinds)
             ,@acode
             ,@rcode
             (emit-template ,n-node ,n-block ,n-template ,n-args
                            ,n-results
                            ,@(when (ivars)
                                `((list ,@(ivars)))))
             (values)))))))

;;; VOP* Name Node Block (Arg* More-Args) (Result* More-Results) Info*
;;;
;;; This is like VOP, but allows for emission of templates with
;;; arbitrary numbers of arguments, and for emission of templates
;;; using already-created TN-REF lists.
;;;
;;; The ARGS and RESULTS are TNs to be referenced as the first
;;; arguments and results to the template. More-Args and More-Results
;;; are heads of TN-REF lists that are added onto the end of the
;;; TN-REFS for the explicitly supplied operand TNs. The TN-REFS for
;;; the more operands must have the TN and WRITE-P slots correctly
;;; initialized.
;;;
;;; As with VOP, the INFO forms are evaluated and passed as codegen
;;; info arguments.
(defmacro vop* (name node block args results &rest info)
  (declare (type cons args results))
  (let* ((parse (vop-parse-or-lose name))
         (arg-count (length (vop-parse-args parse)))
         (result-count (length (vop-parse-results parse)))
         (info-count (length (vop-parse-info-args parse)))
         (fixed-args (butlast args))
         (fixed-results (butlast results))
         (n-node (gensym))
         (n-block (gensym))
         (n-template (gensym)))

    (unless (or (vop-parse-more-args parse)
                (<= (length fixed-args) arg-count))
      (error "too many fixed arguments"))
    (unless (or (vop-parse-more-results parse)
                (<= (length fixed-results) result-count))
      (error "too many fixed results"))
    (unless (= (length info) info-count)
      (error "expected ~W info args" info-count))

    (multiple-value-bind (acode abinds n-args)
        (make-operand-list fixed-args (car (last args)) nil)
      (multiple-value-bind (rcode rbinds n-results)
          (make-operand-list fixed-results (car (last results)) t)

        `(let* ((,n-node ,node)
                (,n-block ,block)
                (,n-template (template-or-lose ',name))
                ,@abinds
                ,@rbinds)
           ,@acode
           ,@rcode
           (emit-template ,n-node ,n-block ,n-template ,n-args ,n-results
                          ,@(when info
                              `((list ,@info))))
           (values))))))

;;;; miscellaneous macros

;;; SC-Case TN {({(SC-Name*) | SC-Name | T} Form*)}*
;;;
;;; Case off of TN's SC. The first clause containing TN's SC is
;;; evaluated, returning the values of the last form. A clause
;;; beginning with T specifies a default. If it appears, it must be
;;; last. If no default is specified, and no clause matches, then an
;;; error is signalled.
(defmacro sc-case (tn &body forms)
  (let ((n-sc (gensym))
        (n-tn (gensym)))
    (collect ((clauses))
      (do ((cases forms (rest cases)))
          ((null cases)
           (clauses `(t (locally (declare (optimize (safety 0))) ;; avoid NIL-FUN-RETURNED-ERROR
                          (unknown-sc-case ,n-tn)))))
        (let ((case (first cases)))
          (when (atom case)
            (error "illegal SC-CASE clause: ~S" case))
          (let ((head (first case)))
            (when (eq head t)
              (when (rest cases)
                (error "T case is not last in SC-CASE."))
              (clauses `(t nil ,@(rest case)))
              (return))
            (clauses `((or ,@(mapcar (lambda (x)
                                       `(eql ,(sc-number-or-lose x) ,n-sc))
                                     (if (atom head) (list head) head)))
                       nil ,@(rest case))))))

      `(let* ((,n-tn ,tn)
              (,n-sc (sc-number (tn-sc ,n-tn))))
         (cond ,@(clauses))))))

(defun unknown-sc-case (tn)
  (error "unknown SC to SC-CASE for ~S:~%  ~S" tn (sc-name (tn-sc tn))))

;;; Return true if TNs SC is any of the named SCs, false otherwise.
(defmacro sc-is (tn &rest scs)
  (once-only ((n-sc `(sc-number (tn-sc ,tn))))
    `(or ,@(mapcar (lambda (x)
                     `(eql ,n-sc ,(sc-number-or-lose x)))
                   scs))))

;;; Iterate over the IR2 blocks in component, in emission order.
(defmacro do-ir2-blocks ((block-var component &optional result)
                         &body forms)
  `(do ((,block-var (block-info (component-head ,component))
                    (ir2-block-next ,block-var)))
       ((null ,block-var) ,result)
     ,@forms))

;;; Iterate over all the TNs live at some point, with the live set
;;; represented by a local conflicts bit-vector and the IR2-BLOCK
;;; containing the location.
(defmacro do-live-tns ((tn-var live block &optional result) &body body)
  (with-unique-names (conf bod i ltns)
    (once-only ((n-live live)
                (n-block block))
      `(block nil
         (flet ((,bod (,tn-var) ,@body))
           ;; Do component-live TNs.
           (dolist (,tn-var (ir2-component-component-tns
                             (component-info
                              (block-component
                               (ir2-block-block ,n-block)))))
             (,bod ,tn-var))

           (let ((,ltns (ir2-block-local-tns ,n-block)))
             ;; Do TNs always-live in this block and live :MORE TNs.
             (do ((,conf (ir2-block-global-tns ,n-block)
                         (global-conflicts-next-blockwise ,conf)))
                 ((null ,conf))
               (when (or (eq (global-conflicts-kind ,conf) :live)
                         (let ((,i (global-conflicts-number ,conf)))
                           (and (eq (svref ,ltns ,i) :more)
                                (not (zerop (sbit ,n-live ,i))))))
                 (,bod (global-conflicts-tn ,conf))))
             ;; Do TNs locally live in the designated live set.
             (dotimes (,i (ir2-block-local-tn-count ,n-block) ,result)
               (unless (zerop (sbit ,n-live ,i))
                 (let ((,tn-var (svref ,ltns ,i)))
                   (when (and ,tn-var (not (eq ,tn-var :more)))
                     (,bod ,tn-var)))))))))))

;;; Iterate over all the IR2 blocks in the environment ENV, in emit
;;; order.
(defmacro do-environment-ir2-blocks ((block-var env &optional result)
                                     &body body)
  (once-only ((n-env env))
    (once-only ((n-first `(lambda-block (environment-lambda ,n-env))))
      (once-only ((n-tail `(block-info
                            (component-tail
                             (block-component ,n-first)))))
        `(do ((,block-var (block-info ,n-first)
                          (ir2-block-next ,block-var)))
             ((or (eq ,block-var ,n-tail)
                  (not (eq (ir2-block-environment ,block-var) ,n-env)))
              ,result)
           ,@body)))))
