;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is derived from software originally released by Xerox
;;;; Corporation. Copyright and release statements follow. Later modifications
;;;; to the software are in the public domain and are provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for more
;;;; information.

;;;; copyright information from original PCL sources:
;;;;
;;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;;; All rights reserved.
;;;;
;;;; Use and copying of this software and preparation of derivative works based
;;;; upon this software are permitted. Any distribution of this software or
;;;; derivative works must comply with all applicable United States export
;;;; control laws.
;;;;
;;;; This software is made available AS IS, and Xerox Corporation makes no
;;;; warranty about the software, its performance or its conformity to any
;;;; specification.

(in-package "SB-PCL")

;;; methods
;;;
;;; Methods themselves are simple inanimate objects. Most properties of
;;; methods are immutable, methods cannot be reinitialized. The following
;;; properties of methods can be changed:
;;;   METHOD-GENERIC-FUNCTION

;;; initialization
;;;
;;; Error checking is done in before methods. Because of the simplicity of
;;; standard method objects the standard primary method can fill the slots.
;;;
;;; Methods are not reinitializable.

(define-condition metaobject-initialization-violation
    (reference-condition simple-error)
  ())

(defun change-class-to-metaobject-violation (to-name
                                             &optional from-name references)
  (error 'metaobject-initialization-violation
         :format-control "~@<Cannot ~S~@[ ~S~] objects into ~S metaobjects.~@:>"
         :format-arguments (list 'change-class from-name to-name)
         :references references))

(macrolet ((def (name args control)
               `(defmethod ,name ,args
                 (declare (ignore initargs))
                 (error 'metaobject-initialization-violation
                        :format-control ,(format nil "~~@<~A~~@:>" control)
                        :format-arguments (list ',name)
                        :references '((:amop :initialization method))))))
  (def reinitialize-instance ((method method) &rest initargs)
    "Method objects cannot be redefined by ~S.")
  (def change-class ((method method) new &rest initargs)
    "Method objects cannot be redefined by ~S.")
  ;; NEW being a subclass of method is dealt with in the general
  ;; method of CHANGE-CLASS
  (def update-instance-for-redefined-class ((method method) added discarded
                                            plist &rest initargs)
    "No behaviour specified for ~S on method objects.")
  (def update-instance-for-different-class (old (new method) &rest initargs)
    "No behaviour specified for ~S on method objects.")
  (def update-instance-for-different-class ((old method) new &rest initargs)
    "No behaviour specified for ~S on method objects."))

(define-condition invalid-method-initarg (simple-program-error)
  ((method :initarg :method :reader invalid-method-initarg-method))
  (:report
   (lambda (c s)
     (format s "~@<In initialization of ~S:~2I~_~?~@:>"
             (invalid-method-initarg-method c)
             (simple-condition-format-control c)
             (simple-condition-format-arguments c)))))

(defun invalid-method-initarg (method format-control &rest args)
  (error 'invalid-method-initarg :method method
         :format-control format-control :format-arguments args))

(defun check-documentation (method doc)
  (unless (or (null doc) (stringp doc))
    (invalid-method-initarg method "~@<~S of ~S is neither ~S nor a ~S.~@:>"
                            :documentation doc 'null 'string)))
(defun check-lambda-list (method ll)
  (declare (ignore method ll))
  nil)

(defun check-method-function (method fun)
  (unless (functionp fun)
    (invalid-method-initarg method "~@<~S of ~S is not a ~S.~@:>"
                            :function fun 'function)))

(macrolet ((dolist-carefully ((var list improper-list-handler) &body body)
             `(let ((,var nil)
                    (.dolist-carefully. ,list))
                (loop (when (null .dolist-carefully.) (return nil))
                   (if (consp .dolist-carefully.)
                       (progn
                         (setq ,var (pop .dolist-carefully.))
                         ,@body)
                       (,improper-list-handler))))))

(defun check-qualifiers (method qualifiers)
  (flet ((improper-list ()
           (invalid-method-initarg method
                                   "~@<~S of ~S is an improper list.~@:>"
                                   :qualifiers qualifiers)))
    (dolist-carefully (q qualifiers improper-list)
      (unless (and q (atom q))
        (invalid-method-initarg method
                                "~@<~S, in ~S ~S, is not a non-~S atom.~@:>"
                                q :qualifiers qualifiers 'null)))))

(defun check-slot-name (method name)
  (declare (ignore method))
  (unless (symbolp name)
    (invalid-method-initarg "~@<~S of ~S is not a ~S.~@:>"
                            :slot-name name 'symbol)))

(defun check-specializers (method specializers)
  (flet ((improper-list ()
           (invalid-method-initarg method
                                   "~@<~S of ~S is an improper list.~@:>"
                                   :specializers specializers)))
    (dolist-carefully (s specializers improper-list)
      (unless (specializerp s)
        (invalid-method-initarg method
                                "~@<~S, in ~S ~S, is not a ~S.~@:>"
                                s :specializers specializers 'specializer)))
    ;; KLUDGE: ANSI says that it's not valid to have methods
    ;; specializing on classes which are "not defined", leaving
    ;; unclear what the definedness of a class is; AMOP suggests that
    ;; forward-referenced-classes, since they have proper names and
    ;; all, are at least worthy of some level of definition.  We allow
    ;; methods specialized on forward-referenced-classes, but it's
    ;; non-portable and potentially dubious, so
    (let ((frcs (remove-if-not #'forward-referenced-class-p specializers)))
      (unless (null frcs)
        (style-warn "~@<Defining a method using ~
                     ~V[~;~1{~S~}~;~1{~S and ~S~}~:;~{~#[~;and ~]~S~^, ~}~] ~
                     as ~2:*~V[~;a specializer~:;specializers~].~@:>"
                    (length frcs) frcs)))))
) ; end MACROLET

(defmethod shared-initialize :before
    ((method standard-method) slot-names &key
     qualifiers lambda-list specializers function documentation)
  (declare (ignore slot-names))
  ;; FIXME: it's not clear to me (CSR, 2006-08-09) why methods get
  ;; this extra paranoia and nothing else does; either everything
  ;; should be aggressively checking initargs, or nothing much should.
  ;; In either case, it would probably be better to have :type
  ;; declarations in slots, which would then give a suitable type
  ;; error (if we implement type-checking for slots...) rather than
  ;; this hand-crafted thing.
  (check-qualifiers method qualifiers)
  (check-lambda-list method lambda-list)
  (check-specializers method specializers)
  (check-method-function method function)
  (check-documentation method documentation))

(defmethod shared-initialize :before
    ((method standard-accessor-method) slot-names &key
     slot-name slot-definition)
  (declare (ignore slot-names))
  (unless slot-definition
    (check-slot-name method slot-name)))

(defmethod shared-initialize :after ((method standard-method) slot-names
                                     &rest initargs &key ((method-cell method-cell)))
  (declare (ignore slot-names method-cell))
  (initialize-method-function initargs method))

(define-load-time-global *the-class-standard-generic-function*
  (find-class 'standard-generic-function))

(defmethod shared-initialize :before
           ((generic-function standard-generic-function)
            slot-names
            &key (lambda-list () lambda-list-p)
                 argument-precedence-order
                 declarations
                 documentation
                 (method-class nil method-class-supplied-p)
                 (method-combination nil method-combination-supplied-p))
  (declare (ignore slot-names
                   declarations argument-precedence-order documentation
                   lambda-list lambda-list-p))

  (flet ((initarg-error (initarg value string)
           (error "when initializing the generic function ~S:~%~
                   The ~S initialization argument was: ~A.~%~
                   It must be ~A."
                  generic-function initarg value string)))
    (cond (method-class-supplied-p
           (when (symbolp method-class)
             (setq method-class (find-class method-class)))
           (unless (and (classp method-class)
                        (*subtypep (class-eq-specializer method-class)
                                   *the-class-method*))
             (initarg-error :method-class
                            method-class
                            "a subclass of the class METHOD"))
           (setf (slot-value generic-function 'method-class) method-class))
          ((slot-boundp generic-function 'method-class))
          (t
           (initarg-error :method-class
                          "not supplied"
                          "a subclass of the class METHOD")))
    (cond (method-combination-supplied-p
           (unless (method-combination-p method-combination)
             (initarg-error :method-combination
                            method-combination
                            "a method combination object")))
          ((slot-boundp generic-function '%method-combination))
          (t
           (initarg-error :method-combination
                          "not supplied"
                          "a method combination object")))))

(defun find-generic-function (name &optional (errorp t))
  (let ((fun (and (fboundp name) (fdefinition name))))
    (cond
      ((and fun (typep fun 'generic-function)) fun)
      (errorp (error "No generic function named ~S." name))
      (t nil))))

(defun real-add-named-method (generic-function-name qualifiers
                              specializers lambda-list &rest other-initargs)
  (let* ((existing-gf (find-generic-function generic-function-name nil))
         (generic-function
           (if existing-gf
               (ensure-generic-function
                generic-function-name
                :generic-function-class (class-of existing-gf))
               (ensure-generic-function generic-function-name)))
         (proto (method-prototype-for-gf generic-function-name)))
    ;; FIXME: Destructive modification of &REST list.
    (setf (getf (getf other-initargs 'plist) :name)
          (make-method-spec generic-function qualifiers specializers))
    (let ((new (apply #'make-instance (class-of proto)
                      :qualifiers qualifiers :specializers specializers
                      :lambda-list lambda-list other-initargs)))
      (add-method generic-function new)
      new)))

(defun real-get-method (generic-function qualifiers specializers
                        &optional (errorp t)
                                  always-check-specializers)
  (sb-thread::with-recursive-system-lock ((gf-lock generic-function))
    (let ((specializer-count (length specializers))
          (methods (generic-function-methods generic-function)))
      (when (or methods always-check-specializers)
        (let ((required-parameter-count
                (length (arg-info-metatypes (gf-arg-info generic-function)))))
          ;; Since we internally bypass FIND-METHOD by using GET-METHOD
          ;; instead we need to do this here or users may get hit by a
          ;; failed AVER instead of a sensible error message.
          (unless (= specializer-count required-parameter-count)
            (error
             'find-method-length-mismatch
             :format-control   "~@<The generic function ~S takes ~D ~
                              required argument~:P; was asked to ~
                              find a method with specializers ~:S~@:>"
             :format-arguments (list generic-function required-parameter-count
                                     (unparse-specializers generic-function specializers))))))
      (flet ((congruentp (other-method)
               (let ((other-specializers (method-specializers other-method)))
                 (aver (= specializer-count (length other-specializers)))
                 (and (equal qualifiers (safe-method-qualifiers other-method))
                      (every #'same-specializer-p specializers other-specializers)))))
        (declare (dynamic-extent #'congruentp))
        (cond ((find-if #'congruentp methods))
              ((null errorp) nil)
              (t
               (error "~@<There is no method on ~S with ~:[no ~
                     qualifiers~;~:*qualifiers ~:S~] and specializers ~
                     ~:S.~@:>"
                      generic-function qualifiers specializers)))))))

(defmethod find-method ((generic-function standard-generic-function)
                        qualifiers specializers &optional (errorp t))
  ;; ANSI about FIND-METHOD: "The specializers argument contains the
  ;; parameter specializers for the method. It must correspond in
  ;; length to the number of required arguments of the generic
  ;; function, or an error is signaled."
  ;;
  ;; This error checking is done by REAL-GET-METHOD.
  (real-get-method
   generic-function qualifiers
   ;; ANSI for FIND-METHOD seems to imply that in fact specializers
   ;; should always be passed in parsed form instead of being parsed
   ;; at this point.  Since there's no ANSI-blessed way of getting an
   ;; EQL specializer, that seems unnecessarily painful, so we are
   ;; nice to our users.  -- CSR, 2007-06-01
   ;; Note that INTERN-EQL-SPECIALIZER is exported from SB-MOP, but MOP isn't
   ;; part of the ANSI standard. Parsing introduces a tiny semantic problem in
   ;; the edge case of an EQL specializer whose object is literally (EQL :X).
   ;; That one must be supplied as a pre-parsed #<EQL-SPECIALIZER> because if
   ;; not, we'd parse it into a specializer whose object is :X.
   (parse-specializers generic-function specializers) errorp t))

;;; Compute various information about a generic-function's arglist by looking
;;; at the argument lists of the methods. The hair for trying not to use
;;; &REST arguments lives here.
;;;  The values returned are:
;;;    number-of-required-arguments
;;;       the number of required arguments to this generic-function's
;;;       discriminating function
;;;    &rest-argument-p
;;;       whether or not this generic-function's discriminating
;;;       function takes an &rest argument.
;;;    specialized-argument-positions
;;;       a list of the positions of the arguments this generic-function
;;;       specializes (e.g. for a classical generic-function this is the
;;;       list: (1)).
(defmethod compute-discriminating-function-arglist-info
           ((generic-function standard-generic-function))
  ;;(declare (values number-of-required-arguments &rest-argument-p
  ;;             specialized-argument-postions))
  (let ((number-required nil)
        (restp nil)
        (specialized-positions ())
        (methods (generic-function-methods generic-function)))
    (dolist (method methods)
      (multiple-value-setq (number-required restp specialized-positions)
        (compute-discriminating-function-arglist-info-internal
         generic-function method number-required restp specialized-positions)))
    (values number-required restp (sort specialized-positions #'<))))

(defun compute-discriminating-function-arglist-info-internal
       (generic-function method number-of-requireds restp
        specialized-argument-positions)
  (declare (ignore generic-function)
           (type (or null fixnum) number-of-requireds))
  (let ((requireds 0))
    (declare (fixnum requireds))
    ;; Go through this methods arguments seeing how many are required,
    ;; and whether there is an &rest argument.
    (dolist (arg (method-lambda-list method))
      (cond ((eq arg '&aux) (return))
            ((memq arg '(&optional &rest &key))
             (return (setq restp t)))
            ((memq arg lambda-list-keywords))
            (t (incf requireds))))
    ;; Now go through this method's type specifiers to see which
    ;; argument positions are type specified. Treat T specially
    ;; in the usual sort of way. For efficiency don't bother to
    ;; keep specialized-argument-positions sorted, rather depend
    ;; on our caller to do that.
    (let ((pos 0))
      (dolist (type-spec (method-specializers method))
        (unless (eq type-spec *the-class-t*)
          (pushnew pos specialized-argument-positions :test #'eq))
        (incf pos)))
    ;; Finally merge the values for this method into the values
    ;; for the exisiting methods and return them. Note that if
    ;; num-of-requireds is NIL it means this is the first method
    ;; and we depend on that.
    (values (min (or number-of-requireds requireds) requireds)
            (or restp
                (and number-of-requireds (/= number-of-requireds requireds)))
            specialized-argument-positions)))

(defmethod generic-function-argument-precedence-order
    ((gf standard-generic-function))
  (aver (eq **boot-state** 'complete))
  (loop with arg-info = (gf-arg-info gf)
        with lambda-list = (arg-info-lambda-list arg-info)
        for argument-position in (arg-info-precedence arg-info)
        collect (nth argument-position lambda-list)))

(defmethod generic-function-lambda-list ((gf generic-function))
  (gf-lambda-list gf))

(defmethod gf-fast-method-function-p ((gf standard-generic-function))
  (gf-info-fast-mf-p (slot-value gf 'arg-info)))

(defun add-to-weak-hashset (key set)
  (hashset-insert-if-absent set key #'identity)) ; implicitly locks
(defun remove-from-weak-hashset (key set)
  (with-system-mutex ((hashset-mutex set))
    (hashset-remove set key)))
(defun weak-hashset-memberp (key set)
  (with-system-mutex ((hashset-mutex set))
    (hashset-find set key)))

(defmethod initialize-instance :after ((gf standard-generic-function)
                                       &key (lambda-list nil lambda-list-p)
                                       argument-precedence-order)
  ;; FIXME: Because ARG-INFO is a STRUCTURE-OBJECT, it does not get
  ;; a permutation vector, and therefore the code that SLOT-VALUE transforms
  ;; to winds up punting to #'(SLOT-ACCESSOR :GLOBAL ARG-INFO READER).
  ;; Using SLOT-VALUE the "slow" way sidesteps some bootstrap issues.
  (declare (notinline slot-value))
  (progn ; WAS: with-slots (arg-info) gf
    (if lambda-list-p
        (set-arg-info gf
                      :lambda-list lambda-list
                      :argument-precedence-order argument-precedence-order)
        (set-arg-info gf))
    (let ((mc (generic-function-method-combination gf)))
      (add-to-weak-hashset gf (method-combination-%generic-functions mc)))
    (when (arg-info-valid-p (slot-value gf 'arg-info))
      (update-dfun gf))))

(defmethod reinitialize-instance :around
    ((gf standard-generic-function) &rest args &key
     (lambda-list nil lambda-list-p) (argument-precedence-order nil apo-p))
  (let* ((old-mc (generic-function-method-combination gf))
         (mc (getf args :method-combination old-mc))
         (keys (arg-info-keys (gf-arg-info gf))))
    (unless (eq mc old-mc)
      (aver (weak-hashset-memberp gf (method-combination-%generic-functions old-mc)))
      (aver (not (weak-hashset-memberp gf (method-combination-%generic-functions mc)))))
    (prog1 (call-next-method)
      (unless (eq mc old-mc)
        (remove-from-weak-hashset gf (method-combination-%generic-functions old-mc))
        (add-to-weak-hashset gf (method-combination-%generic-functions mc)))
      (sb-thread::with-recursive-system-lock ((gf-lock gf))
        (cond
          ((and lambda-list-p apo-p)
           (set-arg-info gf
                         :lambda-list lambda-list
                         :argument-precedence-order argument-precedence-order))
          (lambda-list-p (set-arg-info gf :lambda-list lambda-list))
          (t (set-arg-info gf)))
        (let ((arg-info (gf-arg-info gf)))
          (unless (and (eq mc old-mc) (equal keys (arg-info-keys arg-info)))
            (flush-effective-method-cache gf))
          (when (arg-info-valid-p arg-info)
            (update-dfun gf)))
        (map-dependents gf (lambda (dependent)
                             (apply #'update-dependent gf dependent args)))))))

(defun set-methods (gf methods)
  (setf (generic-function-methods gf) nil)
  (loop (when (null methods) (return gf))
        (real-add-method gf (pop methods) methods)))

(define-condition new-value-specialization (reference-condition error)
  ((%method :initarg :method :reader new-value-specialization-method))
  (:report
   (lambda (c s)
     (format s "~@<Cannot add method ~S to ~S, as it specializes the ~
                new-value argument.~@:>"
             (new-value-specialization-method c)
             #'(setf slot-value-using-class))))
  (:default-initargs :references
      (list '(:sbcl :node "Metaobject Protocol")
            '(:amop :generic-function (setf slot-value-using-class)))))

(defgeneric values-for-add-method (gf method)
  (:method ((gf standard-generic-function) (method standard-method))
    ;; KLUDGE: Just a single generic dispatch, and everything else
    ;; comes from permutation vectors. Would be nicer to define
    ;; REAL-ADD-METHOD with a proper method so that we could efficiently
    ;; use SLOT-VALUE there.
    ;;
    ;; Optimization note: REAL-ADD-METHOD has a lot of O(N) stuff in it (as
    ;; does PCL as a whole). It should not be too hard to internally store
    ;; many of the things we now keep in lists as either purely functional
    ;; O(log N) sets, or --if we don't mind the memory cost-- using
    ;; specialized hash-tables: most things are used to answer questions about
    ;; set-membership, not ordering.
    (values (slot-value gf '%lock)
            (slot-value method 'qualifiers)
            (slot-value method 'specializers)
            (slot-value method 'lambda-list)
            (slot-value method '%generic-function)
            (slot-value gf 'name))))

(define-condition print-object-stream-specializer (reference-condition simple-warning)
  ()
  (:default-initargs
   :references '((:ansi-cl :function print-object))
   :format-control "~@<Specializing on the second argument to ~S has ~
                    unportable effects, and also interferes with ~
                    precomputation of print functions for exceptional ~
                    situations.~@:>"
   :format-arguments (list 'print-object)))

(defun defer-ftype-computation (gf)
  ;; Is there any reason not to do this as soon as possible?
  ;; While doing it with every ADD/REMOVE-METHOD call could result in
  ;; wasted work, it seems like unnecessary complexity.
  ;; I think it's just to get through bootstrap, probably,
  ;; but if it's a semantics thing, it deserves some explanation.
  (let ((name (generic-function-name gf)))
    (when (legal-fun-name-p name) ; tautological ?
      (unless (eq (info :function :where-from name) :declared)
        (when (and (fboundp name) (eq (fdefinition name) gf))
          (setf (info :function :type name) :generic-function))))))

(defun compute-gf-ftype (name)
  (let ((gf (and (fboundp name) (fdefinition name)))
        (methods-in-compilation-unit (binding* ((cu sb-c::*compilation-unit* :exit-if-null)
                                                (methods (sb-c::cu-methods cu) :exit-if-null))
                                       (gethash name methods))))
    (cond ((generic-function-p gf)
           (let* ((ll (generic-function-lambda-list gf))
                  ;; If the GF has &REST without &KEY then we don't augment
                  ;; the FTYPE with keywords, so as not to complain about keywords
                  ;; which seem not to be accepted.
                  (type (sb-c::ftype-from-lambda-list
                         (if (and (member '&rest ll) (not (member '&key ll)))
                             ll
                             (generic-function-pretty-arglist gf methods-in-compilation-unit)))))

             ;; It would be nice if globaldb were transactional,
             ;; so that either both updates or neither occur.
             (setf (info :function :where-from name) :defined-method
                   (info :function :type name) type)))
          (methods-in-compilation-unit
           (setf (info :function :where-from name) :defined-method
                 (info :function :type name)
                 (sb-c::ftype-from-lambda-list
                  (gf-merge-arglists methods-in-compilation-unit))))
          (t
           ;; The defaulting expression for (:FUNCTION :TYPE) does not store
           ;; the default. For :GENERIC-FUNCTION that is not FBOUNDP we also
           ;; don't, however this branch should never be reached because the
           ;; info only stores :GENERIC-FUNCTION when methods are loaded.
           ;; Maybe AVER that it does not happen?
           (sb-c::ftype-from-definition name)))))

(defun real-add-method (generic-function method &optional skip-dfun-update-p)
  (flet ((similar-lambda-lists-p (old-method new-lambda-list)
           (binding* (((a-llks a-nreq a-nopt)
                       (analyze-lambda-list (method-lambda-list old-method)))
                      ((b-llks b-nreq b-nopt)
                       (analyze-lambda-list new-lambda-list)))
             (and (= a-nreq b-nreq)
                  (= a-nopt b-nopt)
                  (eq (ll-keyp-or-restp a-llks)
                      (ll-keyp-or-restp b-llks))))))
    (multiple-value-bind (lock qualifiers specializers new-lambda-list
                          method-gf name)
        (values-for-add-method generic-function method)
      (when method-gf
        (error "~@<The method ~S is already part of the generic ~
                function ~S; it can't be added to another generic ~
                function until it is removed from the first one.~@:>"
               method method-gf))
      (when (and (eq name 'print-object) (not (eq (second specializers) *the-class-t*)))
        (warn 'print-object-stream-specializer))
      (handler-case
          ;; System lock because interrupts need to be disabled as
          ;; well: it would be bad to unwind and leave the gf in an
          ;; inconsistent state.
          (sb-thread::with-recursive-system-lock (lock)
            (let ((existing (get-method generic-function
                                        qualifiers
                                        specializers
                                        nil)))

              ;; If there is already a method like this one then we must get
              ;; rid of it before proceeding.  Note that we call the generic
              ;; function REMOVE-METHOD to remove it rather than doing it in
              ;; some internal way.
              (when (and existing (similar-lambda-lists-p existing new-lambda-list))
                (remove-method generic-function existing))

              ;; KLUDGE: We have a special case here, as we disallow
              ;; specializations of the NEW-VALUE argument to (SETF
              ;; SLOT-VALUE-USING-CLASS).  GET-ACCESSOR-METHOD-FUNCTION is
              ;; the optimizing function here: it precomputes the effective
              ;; method, assuming that there is no dispatch to be done on
              ;; the new-value argument.
              (when (and (eq generic-function #'(setf slot-value-using-class))
                         (not (eq *the-class-t* (first specializers))))
                (error 'new-value-specialization :method  method))

              (setf (method-generic-function method) generic-function)
              (pushnew method (generic-function-methods generic-function) :test #'eq)
              (dolist (specializer specializers)
                (add-direct-method specializer method))

              ;; KLUDGE: SET-ARG-INFO contains the error-detecting logic for
              ;; detecting attempts to add methods with incongruent lambda
              ;; lists.  However, according to Gerd Moellmann on cmucl-imp,
              ;; it also depends on the new method already having been added
              ;; to the generic function.  Therefore, we need to remove it
              ;; again on error:
              (let ((remove-again-p t))
                (unwind-protect
                     (progn
                       (set-arg-info generic-function :new-method method)
                       (setq remove-again-p nil))
                  (when remove-again-p
                    (remove-method generic-function method))))

              ;; KLUDGE II: ANSI saith that it is not an error to add a
              ;; method with invalid qualifiers to a generic function of the
              ;; wrong kind; it's only an error at generic function
              ;; invocation time; I dunno what the rationale was, and it
              ;; sucks.  Nevertheless, it's probably a programmer error, so
              ;; let's warn anyway. -- CSR, 2003-08-20
              (let* ((mc (generic-function-method-combination generic-function))
                     (type-name (method-combination-type-name mc)))
                (flet ((invalid ()
                         (warn "~@<Invalid qualifiers for ~S method ~
                                combination in method ~S:~2I~_~S.~@:>"
                               type-name method qualifiers)))
                  (cond
                    ((and (eq mc *standard-method-combination*)
                          qualifiers
                          (or (cdr qualifiers)
                              (not (standard-method-combination-qualifier-p
                                    (car qualifiers)))))
                     (invalid))
                    ((and (short-method-combination-p mc)
                          (or (null qualifiers)
                              (cdr qualifiers)
                              (not (short-method-combination-qualifier-p
                                    type-name (car qualifiers)))))
                     (invalid)))))
              (unless skip-dfun-update-p
                (update-ctors 'add-method
                              :generic-function generic-function
                              :method method)
                (update-dfun generic-function))
              (defer-ftype-computation generic-function)
              (map-dependents generic-function
                              (lambda (dep)
                                (update-dependent generic-function
                                                  dep 'add-method method)))))
        (serious-condition (c)
          (error c)))))
  generic-function)

(defun real-remove-method (generic-function method)
  (when (eq generic-function (method-generic-function method))
    (flush-effective-method-cache generic-function)
    (let ((lock (gf-lock generic-function)))
      ;; System lock because interrupts need to be disabled as well:
      ;; it would be bad to unwind and leave the gf in an inconsistent
      ;; state.
      (sb-thread::with-recursive-system-lock (lock)
        (let* ((specializers (method-specializers method))
               (methods (generic-function-methods generic-function))
               (new-methods (remove method methods)))
          (setf (method-generic-function method) nil
                (generic-function-methods generic-function) new-methods)
          (dolist (specializer specializers)
            (remove-direct-method specializer method))
          (set-arg-info generic-function)
          (update-ctors 'remove-method
                        :generic-function generic-function
                        :method method)
          (update-dfun generic-function)
          (defer-ftype-computation generic-function)
          (map-dependents generic-function
                          (lambda (dep)
                            (update-dependent generic-function
                                              dep 'remove-method method)))))))
  generic-function)

(defun compute-applicable-methods-function (generic-function arguments)
  (values (compute-applicable-methods-using-types
           generic-function
           (types-from-args generic-function arguments 'eql))))

(defmethod compute-applicable-methods
    ((generic-function generic-function) arguments)
  (values (compute-applicable-methods-using-types
           generic-function
           (types-from-args generic-function arguments 'eql))))

(defmethod compute-applicable-methods-using-classes
    ((generic-function generic-function) classes)
  (compute-applicable-methods-using-types
   generic-function
   (types-from-args generic-function classes 'class-eq)))

(defun !proclaim-incompatible-superclasses (classes)
  (setq classes (mapcar (lambda (class)
                          (if (symbolp class)
                              (find-class class)
                              class))
                        classes))
  (dolist (class classes)
    (dolist (other-class classes)
      (unless (eq class other-class)
        (pushnew other-class (class-incompatible-superclass-list class) :test #'eq)))))

(defun superclasses-compatible-p (class1 class2)
  (let ((cpl1 (cpl-or-nil class1))
        (cpl2 (cpl-or-nil class2)))
    (dolist (sc1 cpl1 t)
      (dolist (ic (class-incompatible-superclass-list sc1))
        (when (memq ic cpl2)
          (return-from superclasses-compatible-p nil))))))

(mapc
 #'!proclaim-incompatible-superclasses
 '(;; superclass class
   (system-class std-class structure-class) ; direct subclasses of pcl-class
   (standard-class funcallable-standard-class)
   ;; superclass metaobject
   (class eql-specializer class-eq-specializer method method-combination
    generic-function slot-definition)
   ;; metaclass built-in-class
   (number sequence character           ; direct subclasses of t, but not array
    standard-object structure-object)   ;                        or symbol
   (number array character symbol       ; direct subclasses of t, but not
    standard-object structure-object)   ;                        sequence
   (complex float rational)             ; direct subclasses of number
   (integer ratio)                      ; direct subclasses of rational
   (list vector)                        ; direct subclasses of sequence
   (cons null)                          ; direct subclasses of list
   (string bit-vector)                  ; direct subclasses of vector
   ))

(defmethod same-specializer-p ((specl1 specializer) (specl2 specializer))
  (eql specl1 specl2))

(defmethod same-specializer-p ((specl1 class) (specl2 class))
  (eq specl1 specl2))

(defmethod specializer-class ((specializer class))
  specializer)

(defmethod same-specializer-p ((specl1 class-eq-specializer)
                               (specl2 class-eq-specializer))
  (eq (specializer-class specl1) (specializer-class specl2)))

;; FIXME: This method is wacky, and indicative of a coding style in which
;; metaphorically the left hand does not know what the right is doing.
;; If you want this to be the abstract comparator, and you "don't know"
;; that EQL-specializers are interned, then the comparator should be EQL.
;; But if you *do* know that they're interned, then why does this method
;; exist at all? The method on SPECIALIZER works fine.
(defmethod same-specializer-p ((specl1 eql-specializer)
                               (specl2 eql-specializer))
  ;; A bit of deception to confuse the enemy?
  (eq (specializer-object specl1) (specializer-object specl2)))

(defmethod specializer-class ((specializer eql-specializer))
  (class-of (slot-value specializer 'object)))

(defun specializer-class-or-nil (specializer)
  (and (standard-specializer-p specializer)
       (specializer-class specializer)))

(defun error-need-at-least-n-args (function n)
  (%program-error "~@<The function ~2I~_~S ~I~_requires at least ~W ~
                   argument~:P.~:>"
                  function n))

(defun types-from-args (generic-function arguments &optional type-modifier)
  (multiple-value-bind (nreq applyp metatypes nkeys arg-info)
      (get-generic-fun-info generic-function)
    (declare (ignore applyp metatypes nkeys))
    (let ((types-rev nil))
      (dotimes-fixnum (i nreq)
        (unless arguments
          (error-need-at-least-n-args (generic-function-name generic-function)
                                      nreq))
        (let ((arg (pop arguments)))
          (push (if type-modifier `(,type-modifier ,arg) arg) types-rev)))
      (values (nreverse types-rev) arg-info))))

(defun get-wrappers-from-classes (nkeys wrappers classes metatypes)
  (let* ((w wrappers) (w-tail w) (mt-tail metatypes))
    (dolist (class (ensure-list classes))
      (unless (eq t (car mt-tail))
        (let ((c-w (class-wrapper class)))
          (unless c-w (return-from get-wrappers-from-classes nil))
          (if (eql nkeys 1)
              (setq w c-w)
              (setf (car w-tail) c-w
                    w-tail (cdr w-tail)))))
      (setq mt-tail (cdr mt-tail)))
    w))

(defun sdfun-for-caching (gf classes)
  (let ((types (mapcar #'class-eq-type classes)))
    (multiple-value-bind (methods all-applicable-and-sorted-p)
        (compute-applicable-methods-using-types gf types)
      (let ((generator (get-secondary-dispatch-function1
                        gf methods types nil t all-applicable-and-sorted-p)))
        (make-callable generator
                       nil (mapcar #'class-wrapper classes))))))

(defun value-for-caching (gf classes)
  (let ((methods (compute-applicable-methods-using-types
                   gf (mapcar #'class-eq-type classes))))
    (method-plist-value (car methods) :constant-value)))

(defun default-secondary-dispatch-function (generic-function)
  (lambda (&rest args)
    (let ((methods (compute-applicable-methods generic-function args)))
      (if methods
          (let ((emf (get-effective-method-function generic-function
                                                    methods)))
            (invoke-emf emf args))
          (call-no-applicable-method generic-function args)))))

(define-load-time-global *std-cam-methods* nil)

(defun compute-applicable-methods-emf (generic-function)
  (if (eq **boot-state** 'complete)
      (let* ((cam (gdefinition 'compute-applicable-methods))
             (cam-methods (compute-applicable-methods-using-types
                           cam (list `(eql ,generic-function) t))))
        (values (get-effective-method-function cam cam-methods)
                (list-elts-eq cam-methods
                         (or *std-cam-methods*
                             (setq *std-cam-methods*
                                   (compute-applicable-methods-using-types
                                    cam (list `(eql ,cam) t)))))))
      (values #'compute-applicable-methods-function t)))

(defun compute-applicable-methods-emf-std-p (gf)
  (gf-info-c-a-m-emf-std-p (gf-arg-info gf)))

(defvar *old-c-a-m-gf-methods* nil)

(defun update-all-c-a-m-gf-info (c-a-m-gf)
  (let ((methods (generic-function-methods c-a-m-gf)))
    (if (and *old-c-a-m-gf-methods*
             (every (lambda (old-method)
                      (member old-method methods :test #'eq))
                    *old-c-a-m-gf-methods*))
        (let ((gfs-to-do nil)
              (gf-classes-to-do nil))
          (dolist (method methods)
            (unless (member method *old-c-a-m-gf-methods* :test #'eq)
              (let ((specl (car (method-specializers method))))
                (if (eql-specializer-p specl)
                    (pushnew (specializer-object specl) gfs-to-do :test #'eq)
                    (pushnew (specializer-class specl) gf-classes-to-do :test #'eq)))))
          (map-all-generic-functions
           (lambda (gf)
             (when (or (member gf gfs-to-do :test #'eq)
                       (dolist (class gf-classes-to-do nil)
                         (member class
                                 (class-precedence-list (class-of gf))
                                 :test #'eq)))
               (update-c-a-m-gf-info gf)))))
        (map-all-generic-functions #'update-c-a-m-gf-info))
    (setq *old-c-a-m-gf-methods* methods)))

(defun update-gf-info (gf)
  (update-c-a-m-gf-info gf)
  (update-gf-simple-accessor-type gf))

(defun update-c-a-m-gf-info (gf)
  (unless (early-gf-p gf)
    (multiple-value-bind (c-a-m-emf std-p)
        (compute-applicable-methods-emf gf)
      (let ((arg-info (gf-arg-info gf)))
        (setf (gf-info-static-c-a-m-emf arg-info) c-a-m-emf)
        (setf (gf-info-c-a-m-emf-std-p arg-info) std-p)))))

(defun update-gf-simple-accessor-type (gf)
  (let ((arg-info (gf-arg-info gf)))
    (setf (gf-info-simple-accessor-type arg-info)
          (let* ((methods (generic-function-methods gf))
                 (class (and methods (class-of (car methods))))
                 (type
                  (and class
                       (cond ((or (eq class *the-class-standard-reader-method*)
                                  (eq class *the-class-global-reader-method*))
                              'reader)
                             ((or (eq class *the-class-standard-writer-method*)
                                  (eq class *the-class-global-writer-method*))
                              'writer)
                             ((eq class *the-class-global-boundp-method*)
                              'boundp)
                             ((eq class *the-class-global-makunbound-method*)
                              'makunbound)))))
            (when (and (gf-info-c-a-m-emf-std-p arg-info)
                       type
                       (dolist (method (cdr methods) t)
                         (unless (eq class (class-of method)) (return nil)))
                       (eq (generic-function-method-combination gf)
                           *standard-method-combination*))
              type)))))


;;; CMUCL (Gerd's PCL, 2002-04-25) comment:
;;;
;;; Return two values.  First value is a function to be stored in
;;; effective slot definition SLOTD for reading it with
;;; SLOT-VALUE-USING-CLASS, setting it with (SETF
;;; SLOT-VALUE-USING-CLASS), testing it with SLOT-BOUNDP-USING-CLASS,
;;; or making it unbound with SLOT-MAKUNBOUND-USING-CLASS.  GF is one
;;; of these generic functions, TYPE is one of the symbols READER,
;;; WRITER, BOUNDP, MAKUNBOUND.  CLASS is SLOTD's class.
;;;
;;; Second value is true if the function returned is one of the
;;; optimized standard functions for the purpose, which are used
;;; when only standard methods are applicable.
;;;
;;; FIXME: Change all these wacky function names to something sane.
(defun get-accessor-method-function (gf type class slotd)
  (let* ((types1 `((eql ,class) (class-eq ,class) (eql ,slotd)))
         (types (if (eq type 'writer) `(t ,@types1) types1))
         (methods (compute-applicable-methods-using-types gf types))
         (std-p (null (cdr methods))))
    (values
     (if std-p
         (get-optimized-std-accessor-method-function class slotd type)
         (let* ((optimized-std-fun
                 (get-optimized-std-slot-value-using-class-method-function
                  class slotd type))
                (method-alist
                 `((,(or (find (standard-svuc-method type) methods :test #'eq)
                         (find (structure-svuc-method type) methods :test #'eq)
                         (find (condition-svuc-method type) methods :test #'eq)
                         (bug "error in ~S" 'get-accessor-method-function))
                    ,optimized-std-fun)))
                (wrappers
                 (let ((wrappers (list (layout-of class)
                                       (class-wrapper class)
                                       (layout-of slotd))))
                   (if (eq type 'writer)
                       (cons (class-wrapper *the-class-t*) wrappers)
                       wrappers)))
                (sdfun (get-secondary-dispatch-function
                        gf methods types method-alist wrappers)))
           (get-accessor-from-svuc-method-function class slotd sdfun type)))
     std-p)))

;;; used by OPTIMIZE-SLOT-VALUE-BY-CLASS-P (vector.lisp)
(defun update-slot-value-gf-info (gf type)
  (unless *new-class*
    (update-std-slot-methods gf type))
  (when (and (standard-svuc-method type) (structure-svuc-method type))
    (flet ((update-accessor-info (class)
             (when (class-finalized-p class)
               (dolist (slotd (class-slots class))
                 (compute-slot-accessor-info slotd type gf)))))
      (if *new-class*
          (update-accessor-info *new-class*)
          (map-all-classes #'update-accessor-info 'slot-object)))))

(define-load-time-global *standard-slot-value-using-class-method* nil)
(define-load-time-global *standard-setf-slot-value-using-class-method* nil)
(define-load-time-global *standard-slot-boundp-using-class-method* nil)
(define-load-time-global *standard-slot-makunbound-using-class-method* nil)
(define-load-time-global *condition-slot-value-using-class-method* nil)
(define-load-time-global *condition-setf-slot-value-using-class-method* nil)
(define-load-time-global *condition-slot-boundp-using-class-method* nil)
(define-load-time-global *condition-slot-makunbound-using-class-method* nil)
(define-load-time-global *structure-slot-value-using-class-method* nil)
(define-load-time-global *structure-setf-slot-value-using-class-method* nil)
(define-load-time-global *structure-slot-boundp-using-class-method* nil)
(define-load-time-global *structure-slot-makunbound-using-class-method* nil)

(defun standard-svuc-method (type)
  (case type
    (reader *standard-slot-value-using-class-method*)
    (writer *standard-setf-slot-value-using-class-method*)
    (boundp *standard-slot-boundp-using-class-method*)
    (makunbound *standard-slot-makunbound-using-class-method*)))

(defun set-standard-svuc-method (type method)
  (case type
    (reader (setq *standard-slot-value-using-class-method* method))
    (writer (setq *standard-setf-slot-value-using-class-method* method))
    (boundp (setq *standard-slot-boundp-using-class-method* method))
    (makunbound (setq *standard-slot-makunbound-using-class-method* method))))

(defun condition-svuc-method (type)
  (case type
    (reader *condition-slot-value-using-class-method*)
    (writer *condition-setf-slot-value-using-class-method*)
    (boundp *condition-slot-boundp-using-class-method*)
    (makunbound *condition-slot-makunbound-using-class-method*)))

(defun set-condition-svuc-method (type method)
  (case type
    (reader (setq *condition-slot-value-using-class-method* method))
    (writer (setq *condition-setf-slot-value-using-class-method* method))
    (boundp (setq *condition-slot-boundp-using-class-method* method))
    (makunbound (setq *condition-slot-makunbound-using-class-method* method))))

(defun structure-svuc-method (type)
  (case type
    (reader *structure-slot-value-using-class-method*)
    (writer *structure-setf-slot-value-using-class-method*)
    (boundp *structure-slot-boundp-using-class-method*)
    (makunbound *structure-slot-makunbound-using-class-method*)))

(defun set-structure-svuc-method (type method)
  (case type
    (reader (setq *structure-slot-value-using-class-method* method))
    (writer (setq *structure-setf-slot-value-using-class-method* method))
    (boundp (setq *structure-slot-boundp-using-class-method* method))
    (makunbound (setq *structure-slot-makunbound-using-class-method* method))))

(defun update-std-slot-methods (gf type)
  (dolist (method (generic-function-methods gf))
    (let ((specls (method-specializers method)))
      (when (and (or (not (eq type 'writer))
                     (eq (pop specls) *the-class-t*))
                 (every #'classp specls))
        (cond ((and (eq (class-name (car specls)) 'std-class)
                    (eq (class-name (cadr specls)) 'standard-object)
                    (eq (class-name (caddr specls))
                        'standard-effective-slot-definition))
               (aver (null (method-qualifiers method)))
               (set-standard-svuc-method type method))
              ((and (eq (class-name (car specls)) 'condition-class)
                    (eq (class-name (cadr specls)) 'condition)
                    (eq (class-name (caddr specls))
                        'condition-effective-slot-definition))
               (aver (null (method-qualifiers method)))
               (set-condition-svuc-method type method))
              ((and (eq (class-name (car specls)) 'structure-class)
                    (eq (class-name (cadr specls)) 'structure-object)
                    (eq (class-name (caddr specls))
                        'structure-effective-slot-definition))
               (aver (null (method-qualifiers method)))
               (set-structure-svuc-method type method)))))))

(defun mec-all-classes-internal (spec precompute-p)
  (let ((wrapper (class-wrapper (specializer-class spec))))
    (unless (or (not wrapper) (invalid-wrapper-p wrapper))
      (cons (specializer-class spec)
            (and (classp spec)
                 precompute-p
                 (not (or (eq spec *the-class-t*)
                          (eq spec *the-class-slot-object*)
                          (eq spec *the-class-standard-object*)
                          (eq spec *the-class-structure-object*)))
                 (let ((sc (class-direct-subclasses spec)))
                   (when sc
                     (mapcan (lambda (class)
                               (mec-all-classes-internal class precompute-p))
                             sc))))))))

(defun mec-all-classes (spec precompute-p)
  (let ((classes (mec-all-classes-internal spec precompute-p)))
    (if (null (cdr classes))
        classes
        (let* ((a-classes (cons nil classes))
               (tail classes))
          (loop (when (null (cdr tail))
                  (return (cdr a-classes)))
                (let ((class (cadr tail))
                      (ttail (cddr tail)))
                  (if (dolist (c ttail nil)
                        (when (eq class c) (return t)))
                      (setf (cdr tail) (cddr tail))
                      (setf tail (cdr tail)))))))))

(defun mec-all-class-lists (spec-list precompute-p)
  (if (null spec-list)
      (list nil)
      (let* ((car-all-classes (mec-all-classes (car spec-list)
                                               precompute-p))
             (all-class-lists (mec-all-class-lists (cdr spec-list)
                                                   precompute-p)))
        (mapcan (lambda (list)
                  (mapcar (lambda (c) (cons c list)) car-all-classes))
                all-class-lists))))

(defun make-emf-cache (generic-function valuep cache classes-list new-class)
  (let* ((arg-info (gf-arg-info generic-function))
         (nkeys (arg-info-nkeys arg-info))
         (metatypes (arg-info-metatypes arg-info))
         (wrappers (unless (eq nkeys 1) (make-list nkeys)))
         (precompute-p (gf-precompute-dfun-and-emf-p arg-info)))
    (flet ((add-class-list (classes)
             (when (or (null new-class) (memq new-class classes))
               (let ((%wrappers (get-wrappers-from-classes
                                 nkeys wrappers classes metatypes)))
                 (when (and %wrappers (not (probe-cache cache %wrappers)))
                   (let ((value (cond ((eq valuep t)
                                       (sdfun-for-caching generic-function
                                                          classes))
                                      ((eq valuep :constant-value)
                                       (value-for-caching generic-function
                                                          classes)))))
                     ;; need to get them again, as finalization might
                     ;; have happened in between, which would
                     ;; invalidate wrappers.
                     (let ((wrappers (get-wrappers-from-classes
                                      nkeys wrappers classes metatypes)))
                       (when (if (atom wrappers)
                                 (not (invalid-wrapper-p wrappers))
                                 (every (complement #'invalid-wrapper-p)
                                        wrappers))
                         (setq cache (fill-cache cache wrappers value))))))))))
      (if classes-list
          (mapc #'add-class-list classes-list)
          (dolist (method (generic-function-methods generic-function))
            (mapc #'add-class-list
                  (mec-all-class-lists (method-specializers method)
                                       precompute-p))))
      cache)))

(defmacro class-test (arg class)
  (cond
    ((eq class *the-class-t*) t)
    ((eq class *the-class-standard-object*)
     `(pcl-instance-p ,arg))
    ((eq class *the-class-funcallable-standard-object*)
     `(and (pcl-instance-p ,arg) (fsc-instance-p ,arg)))
    ;; This is going to be cached (in *fgens*),
    ;; and structure type tests do not check for invalid layout.
    ;; Cache the wrapper itself, which is going to be different after
    ;; redefinition.
    ((structure-class-p class)
     `(sb-c::%instance-typep ,arg ,(class-wrapper class)))
    (t
     `(typep ,arg ',(class-name class)))))

(defmacro class-eq-test (arg class)
  `(eq (class-of ,arg) ',class))

(defun dnet-methods-p (form)
  (and (consp form)
       (or (eq (car form) 'methods)
           (eq (car form) 'unordered-methods))))

;;; This is CASE, but without gensyms.
(defmacro scase (arg &rest clauses)
  `(let ((.case-arg. ,arg))
     (cond ,@(mapcar (lambda (clause)
                       (list* (cond ((null (car clause))
                                     nil)
                                    ((consp (car clause))
                                     (if (null (cdar clause))
                                         `(eql .case-arg.
                                               ',(caar clause))
                                         `(member .case-arg.
                                                  ',(car clause))))
                                    ((member (car clause) '(t otherwise))
                                     `t)
                                    (t
                                     `(eql .case-arg. ',(car clause))))
                              nil
                              (cdr clause)))
                     clauses))))

(defmacro mcase (arg &rest clauses) `(scase ,arg ,@clauses))

(defun generate-discrimination-net (generic-function methods types sorted-p)
  (let* ((arg-info (gf-arg-info generic-function))
         (c-a-m-emf-std-p (gf-info-c-a-m-emf-std-p arg-info))
         (precedence (arg-info-precedence arg-info)))
    (generate-discrimination-net-internal
     generic-function methods types
     (lambda (methods known-types)
       (if (or sorted-p
               (and c-a-m-emf-std-p
                    (block one-order-p
                      (let ((sorted-methods nil))
                        (map-all-orders
                         (copy-list methods) precedence
                         (lambda (methods)
                           (when sorted-methods (return-from one-order-p nil))
                           (setq sorted-methods methods)))
                        (setq methods sorted-methods))
                      t)))
           `(methods ,methods ,known-types)
           `(unordered-methods ,methods ,known-types)))
     (lambda (position type true-value false-value)
       (let ((arg (dfun-arg-symbol position)))
         (if (eq (car type) 'eql)
             (let* ((false-case-p (and (consp false-value)
                                       (or (eq (car false-value) 'scase)
                                           (eq (car false-value) 'mcase))
                                       (eq arg (cadr false-value))))
                    (false-clauses (if false-case-p
                                       (cddr false-value)
                                       `((t ,false-value))))
                    (case-sym (if (and (dnet-methods-p true-value)
                                       (if false-case-p
                                           (eq (car false-value) 'mcase)
                                           (dnet-methods-p false-value)))
                                  'mcase
                                  'scase))
                    (type-sym `(,(cadr type))))
               `(,case-sym ,arg
                           (,type-sym ,true-value)
                           ,@false-clauses))
             `(if ,(let ((arg (dfun-arg-symbol position)))
                     (case (car type)
                       (class    `(class-test    ,arg ,(cadr type)))
                       (class-eq `(class-eq-test ,arg ,(cadr type)))))
                  ,true-value
                  ,false-value))))
     #'identity)))

(defun class-from-type (type)
  (if (or (atom type) (eq (car type) t))
      *the-class-t*
      (case (car type)
        (and (dolist (type (cdr type) *the-class-t*)
               (when (and (consp type) (not (eq (car type) 'not)))
                 (return (class-from-type type)))))
        (not *the-class-t*)
        (eql (class-of (cadr type)))
        (class-eq (cadr type))
        (class (cadr type)))))

;;; We know that known-type implies neither new-type nor `(not ,new-type).
(defun augment-type (new-type known-type)
  (if (or (eq known-type t)
          (eq (car new-type) 'eql))
      new-type
      (let ((so-far (if (and (consp known-type) (eq (car known-type) 'and))
                        (cdr known-type)
                        (list known-type))))
        (unless (eq (car new-type) 'not)
          (setq so-far
                (mapcan (lambda (type)
                          (unless (*subtypep new-type type)
                            (list type)))
                        so-far)))
        (if (null so-far)
            new-type
            `(and ,new-type ,@so-far)))))

(defun generate-discrimination-net-internal
    (gf methods types methods-function test-fun type-function)
  (let* ((arg-info (gf-arg-info gf))
         (precedence (arg-info-precedence arg-info))
         (nreq (arg-info-number-required arg-info))
         (metatypes (arg-info-metatypes arg-info)))
    (labels ((do-column (p-tail contenders known-types)
               (if p-tail
                   (let* ((position (car p-tail))
                          (known-type (or (nth position types) t)))
                     (if (eq (nth position metatypes) t)
                         (do-column (cdr p-tail) contenders
                                    (cons (cons position known-type)
                                          known-types))
                         (do-methods p-tail contenders
                                     known-type () known-types)))
                   (funcall methods-function contenders
                            (let ((k-t (make-list nreq)))
                              (dolist (index+type known-types)
                                (setf (nth (car index+type) k-t)
                                      (cdr index+type)))
                              k-t))))
             (do-methods (p-tail contenders known-type winners known-types)
               ;; CONTENDERS
               ;;   is a (sorted) list of methods that must be discriminated.
               ;; KNOWN-TYPE
               ;;   is the type of this argument, constructed from tests
               ;;   already made.
               ;; WINNERS
               ;;   is a (sorted) list of methods that are potentially
               ;;   applicable after the discrimination has been made.
               (if (null contenders)
                   (do-column (cdr p-tail)
                              winners
                              (cons (cons (car p-tail) known-type)
                                    known-types))
                   (let* ((position (car p-tail))
                          (method (car contenders))
                          (specl (nth position (method-specializers method)))
                          (type (funcall type-function
                                         (type-from-specializer specl))))
                     (multiple-value-bind (app-p maybe-app-p)
                         (specializer-applicable-using-type-p type known-type)
                       (flet ((determined-to-be (truth-value)
                                (if truth-value app-p (not maybe-app-p)))
                              (do-if (truth &optional implied)
                                (let ((ntype (if truth type `(not ,type))))
                                  (do-methods p-tail
                                    (cdr contenders)
                                    (if implied
                                        known-type
                                        (augment-type ntype known-type))
                                    (if truth
                                        (append winners `(,method))
                                        winners)
                                    known-types))))
                         (cond ((determined-to-be nil) (do-if nil t))
                               ((determined-to-be t)   (do-if t   t))
                               (t (funcall test-fun position type
                                           (do-if t) (do-if nil))))))))))
      (do-column precedence methods ()))))

(defvar *eq-case-table-limit* 15)
(defvar *case-table-limit* 10)

(defun compute-mcase-parameters (case-list)
  (unless (eq t (caar (last case-list)))
    (error "The key for the last case arg to mcase was not T"))
  (let* ((eq-p (dolist (case case-list t)
                 (unless (or (eq (car case) t)
                             (symbolp (caar case)))
                   (return nil))))
         (len (1- (length case-list)))
         (type (cond ((= len 1)
                      :simple)
                     ((<= len
                          (if eq-p
                              *eq-case-table-limit*
                              *case-table-limit*))
                      :assoc)
                     (t
                      :hash-table))))
    (list eq-p type)))

(defmacro mlookup (key info default &optional eq-p type)
  (unless (or (eq eq-p t) (null eq-p))
    (bug "Invalid eq-p argument: ~S" eq-p))
  (ecase type
    (:simple
     `(if (locally
            (declare (optimize (inhibit-warnings 3)))
            (,(if eq-p 'eq 'eql) ,key (car ,info)))
          (cdr ,info)
          ,default))
    (:assoc
     `(dolist (e ,info ,default)
        (when (locally
                (declare (optimize (inhibit-warnings 3)))
                (,(if eq-p 'eq 'eql) (car e) ,key))
          (return (cdr e)))))
    (:hash-table
     `(gethash ,key ,info ,default))))

(defun net-test-converter (form)
  (if (atom form)
      (default-test-converter form)
      (case (car form)
        ((invoke-effective-method-function invoke-fast-method-call
          invoke-effective-narrow-method-function)
         '.call.)
        (methods
         '.methods.)
        (unordered-methods
         '.umethods.)
        (mcase
         `(mlookup ,(cadr form)
                   nil
                   nil
                   ,@(compute-mcase-parameters (cddr form))))
        (t (default-test-converter form)))))

(defun net-code-converter (form)
  (if (atom form)
      (default-code-converter form)
      (case (car form)
        ((methods unordered-methods)
         (let ((gensym (gensym)))
           (values gensym
                   (list gensym))))
        (mcase
         (let ((mp (compute-mcase-parameters (cddr form)))
               (gensym (gensym)) (default (gensym)))
           (values `(mlookup ,(cadr form) ,gensym ,default ,@mp)
                   (list gensym default))))
        (t
         (default-code-converter form)))))

(defun net-constant-converter (form generic-function)
  (or (let ((c (methods-converter form generic-function)))
        (when c (list c)))
      (if (atom form)
          (default-constant-converter form)
          (case (car form)
            (mcase
             (let* ((mp (compute-mcase-parameters (cddr form)))
                    (list (mapcar (lambda (clause)
                                    (let ((key (car clause))
                                          (meth (cadr clause)))
                                      (cons (if (consp key) (car key) key)
                                            (methods-converter
                                             meth generic-function))))
                                  (cddr form)))
                    (default (car (last list))))
               (list (list* :mcase mp (nbutlast list))
                     (cdr default))))
            (t
             (default-constant-converter form))))))

(defun methods-converter (form generic-function)
  (cond ((and (consp form) (eq (car form) 'methods))
         (cons '.methods.
               ;; force to heap since the method list is stored in the %CACHE slot
               (get-effective-method-function1 generic-function
                                               (ensure-heap-list (cadr form)))))
        ((and (consp form) (eq (car form) 'unordered-methods))
         (default-secondary-dispatch-function generic-function))))

(defun convert-methods (constant method-alist wrappers)
  (if (and (consp constant)
           (eq (car constant) '.methods.))
      (funcall (cdr constant) method-alist wrappers)
      constant))

(defun convert-table (constant method-alist wrappers)
  (cond ((and (consp constant)
              (eq (car constant) :mcase))
         (let ((alist (mapcar (lambda (k+m)
                                (cons (car k+m)
                                      (convert-methods (cdr k+m)
                                                       method-alist
                                                       wrappers)))
                              (cddr constant)))
               (mp (cadr constant)))
           (ecase (cadr mp)
             (:simple
              (car alist))
             (:assoc
              alist)
             (:hash-table
              (let ((table (sb-vm:without-arena
                            (make-hash-table :test (if (car mp) 'eq 'eql)))))
                (dolist (k+m alist)
                  (setf (gethash (car k+m) table) (cdr k+m)))
                table)))))))

(defun compute-secondary-dispatch-function1 (generic-function net &optional function-p)
  (cond
   ((and (eq (car net) 'methods) (not function-p))
    (get-effective-method-function1 generic-function (cadr net)))
   (t
    (let* ((name (generic-function-name generic-function))
           (arg-info (gf-arg-info generic-function))
           (metatypes (arg-info-metatypes arg-info))
           (nargs (length metatypes))
           (applyp (arg-info-applyp arg-info))
           (fmc-arg-info (cons nargs applyp))
           (arglist (if function-p
                        (make-dfun-lambda-list nargs applyp)
                        (make-fast-method-call-lambda-list nargs applyp))))
      (multiple-value-bind (cfunction constants)
          ;; We don't want NAMED-LAMBDA for any expressions handed to FNGEN,
          ;; because name mismatches will render the hashing ineffective.
          (get-fun `(lambda ,arglist
                      (declare (optimize (sb-c::store-closure-debug-pointer 3)))
                      ,@(unless function-p
                          `((declare (ignore .pv. .next-method-call.))))
                      (locally (declare #.*optimize-speed*)
                        (let ((emf ,net))
                          ,(make-emf-call nargs applyp 'emf))))
                   #'net-test-converter
                   #'net-code-converter
                   (lambda (form)
                     (net-constant-converter form generic-function)))
        (lambda (method-alist wrappers)
          (let* ((alist (list nil))
                 (alist-tail alist))
            (dolist (constant constants)
              (let* ((a (or (dolist (a alist nil)
                              (when (eq (car a) constant)
                                (return a)))
                            (cons constant
                                  (or (convert-table
                                       constant method-alist wrappers)
                                      (convert-methods
                                       constant method-alist wrappers)))))
                     (new (list a)))
                (setf (cdr alist-tail) new)
                (setf alist-tail new)))
            (let ((function (apply cfunction (mapcar #'cdr (cdr alist)))))
              (if function-p
                  (set-fun-name function `(gf-dispatch ,name))
                  (make-fast-method-call
                   :function (set-fun-name function `(sdfun-method ,name))
                   :arg-info fmc-arg-info))))))))))

(defvar *show-make-unordered-methods-emf-calls* nil)

(defun make-unordered-methods-emf (generic-function methods)
  (when *show-make-unordered-methods-emf-calls*
    (format t "~&make-unordered-methods-emf ~S~%"
            (generic-function-name generic-function)))
  (lambda (&rest args)
    (let* ((types (types-from-args generic-function args 'eql))
           (smethods (sort-applicable-methods generic-function
                                              methods
                                              types))
           (emf (get-effective-method-function generic-function smethods)))
      (invoke-emf emf args))))

;;; The value returned by compute-discriminating-function is a function
;;; object. It is called a discriminating function because it is called
;;; when the generic function is called and its role is to discriminate
;;; on the arguments to the generic function and then call appropriate
;;; method functions.
;;;
;;; A discriminating function can only be called when it is installed as
;;; the funcallable instance function of the generic function for which
;;; it was computed.
;;;
;;; More precisely, if compute-discriminating-function is called with
;;; an argument <gf1>, and returns a result <df1>, that result must
;;; not be passed to apply or funcall directly. Rather, <df1> must be
;;; stored as the funcallable instance function of the same generic
;;; function <gf1> (using SET-FUNCALLABLE-INSTANCE-FUNCTION). Then the
;;; generic function can be passed to funcall or apply.
;;;
;;; An important exception is that methods on this generic function are
;;; permitted to return a function which itself ends up calling the value
;;; returned by a more specific method. This kind of `encapsulation' of
;;; discriminating function is critical to many uses of the MOP.
;;;
;;; As an example, the following canonical case is legal:
;;;
;;;   (defmethod compute-discriminating-function ((gf my-generic-function))
;;;     (let ((std (call-next-method)))
;;;       (lambda (arg)
;;;         (print (list 'call-to-gf gf arg))
;;;         (funcall std arg))))
;;;
;;; Because many discriminating functions would like to use a dynamic
;;; strategy in which the precise discriminating function changes with
;;; time it is important to specify how a discriminating function is
;;; permitted itself to change the funcallable instance function of the
;;; generic function.
;;;
;;; Discriminating functions may set the funcallable instance function
;;; of the generic function, but the new value must be generated by making
;;; a call to COMPUTE-DISCRIMINATING-FUNCTION. This is to ensure that any
;;; more specific methods which may have encapsulated the discriminating
;;; function will get a chance to encapsulate the new, inner discriminating
;;; function.
;;;
;;; This implies that if a discriminating function wants to modify itself
;;; it should first store some information in the generic function proper,
;;; and then call compute-discriminating-function. The appropriate method
;;; on compute-discriminating-function will see the information stored in
;;; the generic function and generate a discriminating function accordingly.
;;;
;;; The following is an example of a discriminating function which modifies
;;; itself in accordance with this protocol:
;;;
;;;   (defmethod compute-discriminating-function ((gf my-generic-function))
;;;     (lambda (arg)
;;;      (cond (<some condition>
;;;             <store some info in the generic function>
;;;             (set-funcallable-instance-function
;;;               gf
;;;               (compute-discriminating-function gf))
;;;             (funcall gf arg))
;;;            (t
;;;             <call-a-method-of-gf>))))
;;;
;;; Whereas this code would not be legal:
;;;
;;;   (defmethod compute-discriminating-function ((gf my-generic-function))
;;;     (lambda (arg)
;;;      (cond (<some condition>
;;;             (set-funcallable-instance-function
;;;               gf
;;;               (lambda (a) ..))
;;;             (funcall gf arg))
;;;            (t
;;;             <call-a-method-of-gf>))))
;;;
;;; NOTE:  All the examples above assume that all instances of the class
;;;     my-generic-function accept only one argument.

(defun slot-value-using-class-dfun (class object slotd)
  (declare (ignore class))
  (funcall (slot-info-reader (slot-definition-info slotd)) object))

(defun setf-slot-value-using-class-dfun (new-value class object slotd)
  (declare (ignore class))
  (funcall (slot-info-writer (slot-definition-info slotd)) new-value object))

(defun slot-boundp-using-class-dfun (class object slotd)
  (declare (ignore class))
  (funcall (slot-info-boundp (slot-definition-info slotd)) object))

(defun slot-makunbound-using-class-dfun (class object slotd)
  (declare (ignore class))
  (funcall (slot-info-makunbound (slot-definition-info slotd)) object))

(defun special-case-for-compute-discriminating-function-p (gf)
  (or (eq gf #'slot-value-using-class)
      (eq gf #'(setf slot-value-using-class))
      (eq gf #'slot-boundp-using-class)
      (eq gf #'slot-makunbound-using-class)))

;;; this is the normal function for computing the discriminating
;;; function of a standard-generic-function
(let (initial-print-object-cache)
  (defun standard-compute-discriminating-function (gf)
    (declare (notinline slot-value))
    (let ((dfun-state (slot-value gf 'dfun-state)))
      (when (special-case-for-compute-discriminating-function-p gf)
            ;; if we have a special case for
            ;; COMPUTE-DISCRIMINATING-FUNCTION, then (at least for the
            ;; special cases implemented as of 2006-05-09) any information
            ;; in the cache is misplaced.
        (aver (null dfun-state)))
      (typecase dfun-state
        (null
         (when (eq gf (load-time-value #'compute-applicable-methods t))
           (update-all-c-a-m-gf-info gf))
         (cond ((eq gf (load-time-value #'slot-value-using-class t))
                (update-slot-value-gf-info gf 'reader)
                #'slot-value-using-class-dfun)
               ((eq gf (load-time-value #'(setf slot-value-using-class) t))
                (update-slot-value-gf-info gf 'writer)
                #'setf-slot-value-using-class-dfun)
               ((eq gf (load-time-value #'slot-boundp-using-class t))
                (update-slot-value-gf-info gf 'boundp)
                #'slot-boundp-using-class-dfun)
               ((eq gf (load-time-value #'slot-makunbound-using-class t))
                (update-slot-value-gf-info gf 'makunbound)
                #'slot-makunbound-using-class-dfun)
               ;; KLUDGE: PRINT-OBJECT is not a special-case in the sense
               ;; of having a desperately special discriminating function.
               ;; However, it is important that the machinery for printing
               ;; conditions for stack and heap exhaustion, and the
               ;; restarts offered by the debugger, work without consuming
               ;; many extra resources.  -- CSR, 2008-06-09
               ((eq gf (locally (declare (optimize (safety 0))) #'print-object))
                (let ((nkeys (nth-value 3 (get-generic-fun-info gf))))
                  (cond ((/= nkeys 1)
                         ;; KLUDGE: someone has defined a method
                         ;; specialized on the second argument: punt.
                         (setf initial-print-object-cache nil)
                         (make-initial-dfun gf))
                        (initial-print-object-cache
                         (multiple-value-bind (dfun cache info)
                             (make-caching-dfun gf (copy-cache initial-print-object-cache))
                           (set-dfun gf dfun cache info)))
                        ;; the relevant PRINT-OBJECT methods get defined
                        ;; late, by delayed DEFMETHOD.  We mustn't cache
                        ;; the effective method for our classes earlier
                        ;; than the relevant PRINT-OBJECT methods are
                        ;; defined...
                        ((boundp '*!delayed-defmethod-args*)
                         (make-initial-dfun gf))
                        (t (multiple-value-bind (dfun cache info)
                               (make-final-dfun-internal
                                gf
                                (mapcar (lambda (x) (list (find-class x)))
                                        '(sb-kernel::control-stack-exhausted
                                          sb-kernel::binding-stack-exhausted
                                          sb-kernel::alien-stack-exhausted
                                          sb-kernel::heap-exhausted-error
                                          restart)))
                             (setq initial-print-object-cache cache)
                             (set-dfun gf dfun (copy-cache cache) info))))))
               ((gf-precompute-dfun-and-emf-p (slot-value gf 'arg-info))
                (make-final-dfun gf))
               (t
                (make-initial-dfun gf))))
        (function dfun-state)
        (cons (car dfun-state))))))

;;; in general we need to support SBCL's encapsulation for generic
;;; functions: the default implementation of encapsulation changes the
;;; identity of the function bound to a name, which breaks anything
;;; class-based, so we implement the encapsulation ourselves in the
;;; discriminating function.
(defun sb-impl::encapsulate-generic-function (gf type function)
  (push (cons type function) (generic-function-encapsulations gf))
  (reinitialize-instance gf))

(defun sb-impl::unencapsulate-generic-function (gf type)
  (setf (generic-function-encapsulations gf)
        (remove type (generic-function-encapsulations gf)
                :key #'car :count 1))
  (reinitialize-instance gf))
(defun sb-impl::encapsulated-generic-function-p (gf type)
  (position type (generic-function-encapsulations gf) :key #'car))
(defun maybe-encapsulate-discriminating-function (gf encs std)
  (if (null encs)
      std
      (let ((inner (maybe-encapsulate-discriminating-function
                    gf (cdr encs) std))
            (function (cdar encs)))
        (lambda (&rest args)
          (apply function inner args)))))
(defmethod compute-discriminating-function ((gf standard-generic-function))
  (standard-compute-discriminating-function gf))
(defmethod compute-discriminating-function :around ((gf standard-generic-function))
  (maybe-encapsulate-discriminating-function
   gf (generic-function-encapsulations gf) (call-next-method)))

(defmethod (setf class-name) (new-value class)
  (let ((classoid (layout-classoid (class-wrapper class))))
    (if (and new-value (symbolp new-value))
        (setf (classoid-name classoid) new-value)
        (setf (classoid-name classoid) nil)))
  (reinitialize-instance class :name new-value)
  new-value)

(defmethod (setf generic-function-name) (new-value generic-function)
  (reinitialize-instance generic-function :name new-value)
  new-value)

(defmethod function-keywords ((method standard-method))
  (multiple-value-bind (llks nreq nopt keywords)
      (analyze-lambda-list (if (consp method)
                               (early-method-lambda-list method)
                               (method-lambda-list method)))
    (declare (ignore nreq nopt))
    (values keywords (ll-kwds-allowp llks))))

;;; This is based on the rules of method lambda list congruency
;;; defined in the spec. The lambda list it constructs is the pretty
;;; union of the lambda lists of the generic function and of all its
;;; methods.  It doesn't take method applicability into account; we
;;; also ignore non-public parts of the interface (e.g. &AUX, default
;;; and supplied-p parameters)
;;; The compiler uses this for type-checking that callers pass acceptable
;;; keywords, so don't make this do anything fancy like looking at effective
;;; methods without also fixing the compiler.
(defmethod generic-function-pretty-arglist ((gf standard-generic-function) &optional methods-in-compilation-unit)
  (let ((gf-lambda-list (generic-function-lambda-list gf))
        (methods (generic-function-methods gf)))
    (flet ((lambda-list (m)
             (or (and methods-in-compilation-unit
                      (gethash (cons (method-qualifiers m)
                                     (unparse-specializers gf (method-specializers m)))
                               methods-in-compilation-unit))
                 (method-lambda-list m)))
           (canonize (k)
             (multiple-value-bind (kw var)
                 (parse-key-arg-spec k)
               (if (and (eql (symbol-package kw) *keyword-package*)
                        (string= kw var))
                   var
                   (list (list kw var))))))
      (multiple-value-bind (llks required optional rest keys)
          (parse-lambda-list gf-lambda-list :silent t)
        (if (or (ll-kwds-keyp llks)
                (ll-kwds-restp llks))
            (collect ((keys (mapcar #'canonize keys)))
              ;; Possibly extend the keyword parameters of the gf by
              ;; additional key parameters of its methods:
              (flet ((process (lambda-list)
                       (binding* (((m.llks nil nil nil m.keys)
                                   (parse-lambda-list lambda-list :silent t)))
                         (setq llks (logior llks m.llks))
                         (dolist (k m.keys)
                           (unless (member (parse-key-arg-spec k) (keys)
                                           :key #'parse-key-arg-spec :test #'eq)
                             (keys (canonize k)))))))
                (dolist (m methods)
                  (process (lambda-list m))))
              (make-lambda-list llks nil required optional rest (keys)))
            (make-lambda-list llks nil required optional))))))

(defun gf-merge-arglists (methods-in-compilation-unit)
  (flet ((canonize (k)
           (multiple-value-bind (kw var)
               (parse-key-arg-spec k)
             (if (and (eql (symbol-package kw) *keyword-package*)
                      (string= kw var))
                 var
                 (list (list kw var))))))
    (with-hash-table-iterator (iterator methods-in-compilation-unit)
      (multiple-value-bind (llks required optional rest keys)
          (parse-lambda-list (nth-value 2 (iterator)) :silent t)
        (if (or (ll-kwds-keyp llks)
                (ll-kwds-restp llks))
            (collect ((keys (mapcar #'canonize keys)))
              ;; Possibly extend the keyword parameters of the gf by
              ;; additional key parameters of its methods:
              (flet ((process (lambda-list)
                       (binding* (((m.llks nil nil nil m.keys)
                                   (parse-lambda-list lambda-list :silent t)))
                         (setq llks (logior llks m.llks))
                         (dolist (k m.keys)
                           (unless (member (parse-key-arg-spec k) (keys)
                                           :key #'parse-key-arg-spec :test #'eq)
                             (keys (canonize k)))))))

                (loop
                 (multiple-value-bind (more key value) (iterator)
                   (declare (ignore key))
                   (unless more
                     (return))
                   (process value)))
                (make-lambda-list llks nil required optional rest (keys))))
            (make-lambda-list llks nil required optional))))))
