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


;;;; some support stuff for getting a hold of symbols that we need when
;;;; building the discriminator codes. It's OK for these to be interned
;;;; symbols because we don't capture any user code in the scope in which
;;;; these symbols are bound.

(declaim (list *dfun-arg-symbols*))
(define-load-time-global *dfun-arg-symbols* '(.ARG0. .ARG1. .ARG2. .ARG3.))

(defun dfun-arg-symbol (arg-number)
  (or (nth arg-number *dfun-arg-symbols*)
      (pcl-symbolicate ".ARG" arg-number ".")))

(declaim (list *slot-vector-symbols*))
(define-load-time-global *slot-vector-symbols* '(.SLOTS0. .SLOTS1. .SLOTS2. .SLOTS3.))

(defun slot-vector-symbol (arg-number)
  (or (nth arg-number *slot-vector-symbols*)
      (pcl-symbolicate ".SLOTS" arg-number ".")))

(declaim (inline make-dfun-required-args))
(defun make-dfun-required-args (count)
  (declare (type index count))
  ;; N.B.: don't PUSH and NREVERSE here. COLLECT will cons in the system TLAB,
  ;; but NREVERSE won't because we don't inline NREVERSE.
  (collect ((result))
    (dotimes (i count (result))
      (result (dfun-arg-symbol i)))))

(defun make-dfun-lambda-list (nargs applyp)
  (let ((required (make-dfun-required-args nargs)))
    (if applyp
        (nconc required
               ;; Use &MORE arguments to avoid consing up an &REST list
               ;; that we might not need at all. See MAKE-EMF-CALL and
               ;; INVOKE-EFFECTIVE-METHOD-FUNCTION for the other
               ;; pieces.
               '(&more .dfun-more-context. .dfun-more-count.))
        required)))

(defun make-dlap-lambda-list (nargs applyp)
  (let ((required (make-dfun-required-args nargs)))
    ;; Return the full lambda list, the required arguments, a form
    ;; that will generate a rest-list, and a list of the &MORE
    ;; parameters used.
    ;; Beware of deep voodoo! The DEFKNOWN for %LISTIFY-REST-ARGS says that its
    ;; second argument is INDEX, but the THE form below is "weaker" on account
    ;; of the vop operand restrictions or something that I don't understand.
    ;; Which is to say, PCL compilation reliably broke when changed to INDEX.
    (if applyp
        (values (sb-impl::sys-tlab-append required '(&more .more-context. .more-count.))
                required
                '((sb-c:%listify-rest-args
                   .more-context. (the (and unsigned-byte fixnum)
                                    .more-count.)))
                '(.more-context. .more-count.))
        (values required required nil nil))))

(defun make-emf-call (nargs applyp fn-variable &optional emf-type)
  (let ((required (make-dfun-required-args nargs)))
    `(,(if (eq emf-type 'fast-method-call)
           'invoke-effective-method-function-fast
           'invoke-effective-method-function)
       ,fn-variable
       ,applyp
       :required-args ,required
       ;; INVOKE-EFFECTIVE-METHOD-FUNCTION will decide whether to use
       ;; the :REST-ARG version or the :MORE-ARG version depending on
       ;; the type of the EMF.
       :rest-arg ,(if applyp
                      ;; Creates a list from the &MORE arguments.
                      '((sb-c:%listify-rest-args ; See above re. voodoo
                         .dfun-more-context.
                         (the (and unsigned-byte fixnum)
                           .dfun-more-count.)))
                      nil)
       :more-arg ,(when applyp
                    '(.dfun-more-context. .dfun-more-count.)))))

(defun make-fast-method-call-lambda-list (nargs applyp)
  (list* '.pv. '.next-method-call. (make-dfun-lambda-list nargs applyp)))

;;; Emitting various accessors.

(defun emit-one-class-reader (class-slot-p)
  (emit-reader/writer :reader 1 class-slot-p))

(defun emit-one-class-boundp (class-slot-p)
  (emit-reader/writer :boundp 1 class-slot-p))

(defun emit-one-class-writer (class-slot-p)
  (emit-reader/writer :writer 1 class-slot-p))

(defun emit-one-class-makunbound (class-slot-p)
  (emit-reader/writer :makunbound 1 class-slot-p))

(defun emit-two-class-reader (class-slot-p)
  (emit-reader/writer :reader 2 class-slot-p))

(defun emit-two-class-boundp (class-slot-p)
  (emit-reader/writer :boundp 2 class-slot-p))

(defun emit-two-class-writer (class-slot-p)
  (emit-reader/writer :writer 2 class-slot-p))

(defun emit-two-class-makunbound (class-slot-p)
  (emit-reader/writer :makunbound 2 class-slot-p))

;;; --------------------------------

(defun emit-one-index-readers (class-slot-p)
  (emit-one-or-n-index-reader/writer :reader nil class-slot-p))

(defun emit-one-index-boundps (class-slot-p)
  (emit-one-or-n-index-reader/writer :boundp nil class-slot-p))

(defun emit-one-index-writers (class-slot-p)
  (emit-one-or-n-index-reader/writer :writer nil class-slot-p))

(defun emit-one-index-makunbounds (class-slot-p)
  (emit-one-or-n-index-reader/writer :makunbound nil class-slot-p))

(defun emit-n-n-readers ()
  (emit-one-or-n-index-reader/writer :reader t nil))

(defun emit-n-n-boundps ()
  (emit-one-or-n-index-reader/writer :boundp t nil))

(defun emit-n-n-writers ()
  (emit-one-or-n-index-reader/writer :writer t nil))

(defun emit-n-n-makunbounds ()
  (emit-one-or-n-index-reader/writer :makunbound t nil))

;;; --------------------------------

(defun emit-checking (metatypes applyp)
  (emit-checking-or-caching nil nil metatypes applyp))

(defun emit-caching (metatypes applyp)
  (emit-checking-or-caching t nil metatypes applyp))

(defun emit-in-checking-cache-p (metatypes)
  (emit-checking-or-caching nil t metatypes nil))

(defun emit-constant-value (metatypes)
  (emit-checking-or-caching t t metatypes nil))

;;; --------------------------------

;;; FIXME: What do these variables mean?
(defvar *precompiling-lap* nil)

(defun emit-default-only (metatypes applyp)
  (multiple-value-bind (lambda-list args rest-arg more-arg)
      (make-dlap-lambda-list (length metatypes) applyp)
    (generating-lisp '(emf)
                     lambda-list
                     `(invoke-effective-method-function emf
                                                        ,applyp
                                                        :required-args ,args
                                                        :more-arg ,more-arg
                                                        :rest-arg ,rest-arg))))

;;; --------------------------------

(defun generating-lisp (closure-variables args form)
  (let ((lambda `(lambda ,closure-variables
                   ,@(when (member 'miss-fn closure-variables)
                       `((declare (type function miss-fn))))
                   (declare (optimize (sb-c:store-source-form 0)
                                      (sb-c::store-xref-data 0)))
                   (declare (optimize (sb-c::store-closure-debug-pointer 3)))
                   #'(lambda ,args
                       (let () ; What is this LET doing?
                         (declare #.*optimize-speed*)
                         ,form)))))
    (values (if *precompiling-lap*
                `#',lambda
                (pcl-compile lambda :safe))
            nil)))

;;; note on implementation for CMU 17 and later (including SBCL):
;;; Since STD-INSTANCE-P is weakened, that branch may run on non-PCL
;;; instances (structures). The result will be the non-wrapper layout
;;; for the structure, which will cause a miss. The "slots" will be
;;; whatever the first slot is, but will be ignored. Similarly,
;;; FSC-INSTANCE-P returns true on funcallable structures as well as
;;; PCL fins.
(defun emit-reader/writer (reader/writer 1-or-2-class class-slot-p)
  (let ((instance nil)
        (arglist  ())
        (closure-variables ())
        (read-form (emit-slot-read-form class-slot-p 'index 'slots)))
    (ecase reader/writer
      ((:reader :boundp :makunbound)
       (setq instance (dfun-arg-symbol 0)
             arglist  (list instance)))
      (:writer (setq instance (dfun-arg-symbol 1)
                     arglist  (list (dfun-arg-symbol 0) instance))))
    (ecase 1-or-2-class
      (1 (setq closure-variables '(wrapper-0 index miss-fn)))
      (2 (setq closure-variables '(wrapper-0 wrapper-1 index miss-fn))))
    (generating-lisp
     closure-variables
     arglist
     `(let* (,@(unless class-slot-p `((slots nil)))
               (wrapper (cond ((std-instance-p ,instance)
                               ,@(unless class-slot-p
                                   `((setq slots
                                           (std-instance-slots ,instance))))
                               (%instance-layout ,instance))
                              ((fsc-instance-p ,instance)
                               ,@(unless class-slot-p
                                   `((setq slots
                                           (fsc-instance-slots ,instance))))
                               (%fun-layout ,instance)))))
        (block access
          (when (and wrapper
                     (not (zerop (layout-clos-hash wrapper)))
                     ,@(if (eql 1 1-or-2-class)
                           `((eq wrapper wrapper-0))
                           `((or (eq wrapper wrapper-0)
                                 (eq wrapper wrapper-1)))))
            ,@(ecase reader/writer
                (:reader
                 `((let ((value ,read-form))
                     (unless (unbound-marker-p value)
                       (return-from access value)))))
                (:boundp
                 `((let ((value ,read-form))
                     (return-from access (not (unbound-marker-p value))))))
                (:makunbound
                 `(progn (setf ,read-form +slot-unbound+)
                         ,instance))
                (:writer
                 `((return-from access (setf ,read-form ,(car arglist)))))))
          (funcall miss-fn ,@arglist))))))

(defun emit-slot-read-form (class-slot-p index slots)
  (if class-slot-p
      `(cdr ,index)
      `(clos-slots-ref ,slots ,index)))

(defun emit-boundp-check (value-form miss-fn arglist)
  `(let ((value ,value-form))
     (if (unbound-marker-p value)
         (funcall ,miss-fn ,@arglist)
         value)))

(defun emit-slot-access (reader/writer class-slot-p slots
                         index miss-fn arglist)
  (let ((read-form (emit-slot-read-form class-slot-p index slots)))
    (ecase reader/writer
      (:reader (emit-boundp-check read-form miss-fn arglist))
      (:boundp `(not (unbound-marker-p ,read-form)))
      (:makunbound `(progn (setf ,read-form +slot-unbound+) ,(car arglist)))
      (:writer `(setf ,read-form ,(car arglist))))))

(defmacro emit-reader/writer-macro (reader/writer 1-or-2-class class-slot-p)
  (let ((*precompiling-lap* t))
    (values
     (emit-reader/writer reader/writer 1-or-2-class class-slot-p))))

;; If CACHED-INDEX-P is false, then the slot location is a constant and
;; the cache holds layouts eligible to use that index.
;; If true, then the cache is a map of layout -> index.
(defun emit-one-or-n-index-reader/writer (reader/writer
                                          cached-index-p
                                          class-slot-p)
  (multiple-value-bind (arglist metatypes)
      (ecase reader/writer
        ((:reader :boundp :makunbound)
         (values (list (dfun-arg-symbol 0))
                 '(standard-instance)))
        (:writer (values (list (dfun-arg-symbol 0) (dfun-arg-symbol 1))
                         '(t standard-instance))))
    (generating-lisp
     `(cache ,@(unless cached-index-p '(index)) miss-fn)
     arglist
     `(let (,@(unless class-slot-p '(slots))
            ,@(when cached-index-p '(index)))
        ,(emit-dlap 'cache arglist metatypes
                    (emit-slot-access reader/writer class-slot-p
                                      'slots 'index 'miss-fn arglist)
                    `(funcall miss-fn ,@arglist)
                    (when cached-index-p 'index)
                    (unless class-slot-p '(slots)))))))

(defmacro emit-one-or-n-index-reader/writer-macro
    (reader/writer cached-index-p class-slot-p)
  (let ((*precompiling-lap* t))
    (values
     (emit-one-or-n-index-reader/writer reader/writer
                                        cached-index-p
                                        class-slot-p))))

(defun emit-miss (miss-fn args applyp)
  (if applyp
      `(multiple-value-call ,miss-fn ,@args
                            (sb-c:%more-arg-values .more-context.
                                                    0
                                                    .more-count.))
      `(funcall ,miss-fn ,@args)))

;; (cache-emf, return-value):
;;  NIL / NIL : GF has a single EMF. Invoke it when layouts are in cache.
;;  NIL / T   : GF has a single EMF. Return T when layouts are in cache.
;;  T   / NIL : Look for the EMF for argument layouts. Invoke it when in cache.
;;  T   / T   : Look for the EMF for argument layouts. Return it when in cache.
;;
;;  METATYPES must be acceptable to EMIT-FETCH-WRAPPER.
;;  APPLYP says whether there is a &MORE context.
(defun emit-checking-or-caching (cached-emf-p return-value-p metatypes applyp)
  (multiple-value-bind (lambda-list args rest-arg more-arg)
      (make-dlap-lambda-list (length metatypes) applyp)
    (generating-lisp
     `(cache ,@(unless cached-emf-p '(emf)) miss-fn)
     lambda-list
     `(let (,@(when cached-emf-p '(emf)))
        ,(emit-dlap 'cache args metatypes
                    (if return-value-p
                        (if cached-emf-p 'emf t)
                        `(invoke-effective-method-function
                          emf ,applyp
                          :required-args ,args
                          :more-arg ,more-arg
                          :rest-arg ,rest-arg))
                    (emit-miss 'miss-fn args applyp)
                    (when cached-emf-p 'emf))))))

(defmacro emit-checking-or-caching-macro (cached-emf-p
                                          return-value-p
                                          metatypes
                                          applyp)
  (let ((*precompiling-lap* t))
    (values
     (emit-checking-or-caching cached-emf-p return-value-p metatypes applyp))))

(defun emit-dlap (cache-var args metatypes hit-form miss-form value-var
                  &optional slot-vars)
  (let* ((index -1)
         (miss-tag (gensym "MISSED"))
         (wrapper-bindings (mapcan (lambda (arg mt)
                                     (unless (eq mt t)
                                       (incf index)
                                       `((,(pcl-symbolicate "WRAPPER-" index)
                                          ,(emit-fetch-wrapper
                                            mt arg miss-tag (pop slot-vars))))))
                                   args metatypes))
         (wrapper-vars (mapcar #'car wrapper-bindings)))
    (declare (fixnum index))
    (unless wrapper-vars
      (error "Every metatype is T."))
    `(prog ()
        (return
          (let ,wrapper-bindings
            ,(emit-cache-lookup cache-var wrapper-vars miss-tag value-var)
            ,hit-form))
      ,miss-tag
        (return ,miss-form))))

;; SLOTS-VAR, if supplied, is the variable to update with instance-slots
;; by side-effect of fetching the wrapper for ARGUMENT.
(defun emit-fetch-wrapper (metatype argument miss-tag &optional slots-var)
  (ecase metatype
    ((standard-instance)
     ;; This branch may run on non-pcl instances (structures). The
     ;; result will be the non-wrapper layout for the structure, which
     ;; will cause a miss. Since refencing the structure is rather iffy
     ;; if it should have no slots, or only raw slots, we use FOR-STD-CLASS-P
     ;; to ensure that we have a wrapper.
     ;;
     ;; FIXME: If we unify layouts and wrappers we can use
     ;; instance-slots-layout instead of for-std-class-p, as if there
     ;; are no layouts there are no slots to worry about.
     (with-unique-names (wrapper)
       `(cond ((std-instance-p ,argument)
               ,(if slots-var
                    `(let ((,wrapper (%instance-layout ,argument)))
                       (when (layout-for-pcl-obj-p ,wrapper)
                         (setq ,slots-var (std-instance-slots ,argument)))
                       ,wrapper)
                    `(%instance-layout ,argument)))
              ((fsc-instance-p ,argument)
               ,(if slots-var
                    `(let ((,wrapper (%fun-layout ,argument)))
                       (when (layout-for-pcl-obj-p ,wrapper)
                         (setq ,slots-var (fsc-instance-slots ,argument)))
                       ,wrapper)
                    `(%fun-layout ,argument)))
               (t (go ,miss-tag)))))
    ;; Sep92 PCL used to distinguish between some of these cases (and
    ;; spuriously exclude others).  Since in SBCL
    ;; WRAPPER-OF/LAYOUT-OF/BUILT-IN-OR-STRUCTURE-WRAPPER are all
    ;; equivalent and inlined to each other, we can collapse some
    ;; spurious differences.
    ((class system-instance structure-instance condition-instance)
     (when slots-var
       (bug "SLOT requested for metatype ~S, but it isn't going to happen."
            metatype))
     `(layout-of ,argument))
    ;; a metatype of NIL should never be seen here, as NIL is only in
    ;; the metatypes before a generic function is fully initialized.
    ;; T should never be seen because we never need to get a wrapper
    ;; to do dispatch if all methods have T as the respective
    ;; specializer.
    ((t nil)
     (bug "~@<metatype ~S seen in ~S.~@:>" metatype 'emit-fetch-wrapper))))
