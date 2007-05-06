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
(defvar *dfun-arg-symbols* '(.ARG0. .ARG1. .ARG2. .ARG3.))

(defun dfun-arg-symbol (arg-number)
  (or (nth arg-number *dfun-arg-symbols*)
      (format-symbol *pcl-package* ".ARG~A." arg-number)))

(declaim (list *slot-vector-symbols*))
(defvar *slot-vector-symbols* '(.SLOTS0. .SLOTS1. .SLOTS2. .SLOTS3.))

(defun slot-vector-symbol (arg-number)
  (or (nth arg-number *slot-vector-symbols*)
      (format-symbol *pcl-package* ".SLOTS~A." arg-number)))

(declaim (inline make-dfun-required-args))
(defun make-dfun-required-args (metatypes)
  ;; Micro-optimizations 'R Us
  (labels ((rec (types i)
             (declare (fixnum i))
             (when types
               (cons (dfun-arg-symbol i)
                     (rec (cdr types) (1+ i))))))
    (rec metatypes 0)))

(defun make-dfun-lambda-list (metatypes applyp)
  (let ((required (make-dfun-required-args metatypes)))
    (if applyp
        (nconc required
               ;; Use &MORE arguments to avoid consing up an &REST list
               ;; that we might not need at all. See MAKE-EMF-CALL and
               ;; INVOKE-EFFECTIVE-METHOD-FUNCTION for the other
               ;; pieces.
               '(&more .dfun-more-context. .dfun-more-count.))
      required)))

(defun make-dlap-lambda-list (metatypes applyp)
  (let* ((required (make-dfun-required-args metatypes))
         (lambda-list (if applyp
                          (append required '(&more .more-context. .more-count.))
                          required)))
    ;; Return the full lambda list, the required arguments, a form
    ;; that will generate a rest-list, and a list of the &MORE
    ;; parameters used.
    (values lambda-list
            required
            (when applyp
              '((sb-c::%listify-rest-args
                 .more-context.
                 (the (and unsigned-byte fixnum)
                   .more-count.))))
            (when applyp
              '(.more-context. .more-count.)))))

(defun make-emf-call (metatypes applyp fn-variable &optional emf-type)
  (let ((required (make-dfun-required-args metatypes)))
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
                      '((sb-c::%listify-rest-args
                         .dfun-more-context.
                         (the (and unsigned-byte fixnum)
                           .dfun-more-count.)))
                      nil)
       :more-arg ,(when applyp
                    '(.dfun-more-context. .dfun-more-count.)))))

(defun make-fast-method-call-lambda-list (metatypes applyp)
  (list* '.pv-cell. '.next-method-call.
         (make-dfun-lambda-list metatypes applyp)))

;;; Emitting various accessors.

(defun emit-one-class-reader (class-slot-p)
  (emit-reader/writer :reader 1 class-slot-p))

(defun emit-one-class-boundp (class-slot-p)
  (emit-reader/writer :boundp 1 class-slot-p))

(defun emit-one-class-writer (class-slot-p)
  (emit-reader/writer :writer 1 class-slot-p))

(defun emit-two-class-reader (class-slot-p)
  (emit-reader/writer :reader 2 class-slot-p))

(defun emit-two-class-boundp (class-slot-p)
  (emit-reader/writer :boundp 2 class-slot-p))

(defun emit-two-class-writer (class-slot-p)
  (emit-reader/writer :writer 2 class-slot-p))

;;; --------------------------------

(defun emit-one-index-readers (class-slot-p)
  (emit-one-or-n-index-reader/writer :reader nil class-slot-p))

(defun emit-one-index-boundps (class-slot-p)
  (emit-one-or-n-index-reader/writer :boundp nil class-slot-p))

(defun emit-one-index-writers (class-slot-p)
  (emit-one-or-n-index-reader/writer :writer nil class-slot-p))

(defun emit-n-n-readers ()
  (emit-one-or-n-index-reader/writer :reader t nil))

(defun emit-n-n-boundps ()
  (emit-one-or-n-index-reader/writer :boundp t nil))

(defun emit-n-n-writers ()
  (emit-one-or-n-index-reader/writer :writer t nil))

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
(defvar *emit-function-p* t)

;;; FIXME: This variable is motivated by Gerd Moellman's observation,
;;; in <867kga1wra.fsf@gerd.free-bsd.org> on cmucl-imp 2002-10-22,
;;; that the functions returned from EMIT-xxx-FUNCTION can cause an
;;; order-of-magnitude slowdown.  We include this variable for now,
;;; but maybe its effect should rather be controlled by compilation
;;; policy if there is a noticeable space difference between the
;;; branches, or else maybe the EMIT-xxx-FUNCTION branches should be
;;; deleted.  It's not clear to me how all of this works, though, so
;;; until proper benchmarks are done it's probably safest simply to
;;; have this pseudo-constant to hide code.  -- CSR, 2003-02-14
(defvar *optimize-cache-functions-p* t)

(defun emit-default-only (metatypes applyp)
  (unless *optimize-cache-functions-p*
    (when (and (null *precompiling-lap*) *emit-function-p*)
      (return-from emit-default-only
        (emit-default-only-function metatypes applyp))))
  (multiple-value-bind (lambda-list args rest-arg more-arg)
      (make-dlap-lambda-list metatypes applyp)
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
                   #'(lambda ,args
                       (let ()
                         (declare #.*optimize-speed*)
                         ,form)))))
    (values (if *precompiling-lap*
                `#',lambda
                (compile nil lambda))
            nil)))

;;; note on implementation for CMU 17 and later (including SBCL):
;;; Since STD-INSTANCE-P is weakened, that branch may run on non-PCL
;;; instances (structures). The result will be the non-wrapper layout
;;; for the structure, which will cause a miss. The "slots" will be
;;; whatever the first slot is, but will be ignored. Similarly,
;;; FSC-INSTANCE-P returns true on funcallable structures as well as
;;; PCL fins.
(defun emit-reader/writer (reader/writer 1-or-2-class class-slot-p)
  (unless *optimize-cache-functions-p*
    (when (and (null *precompiling-lap*) *emit-function-p*)
      (return-from emit-reader/writer
        (emit-reader/writer-function
         reader/writer 1-or-2-class class-slot-p))))
  (let ((instance nil)
        (arglist  ())
        (closure-variables ())
        (field +first-wrapper-cache-number-index+)
        (read-form (emit-slot-read-form class-slot-p 'index 'slots)))
    ;;we need some field to do the fast obsolete check
    (ecase reader/writer
      ((:reader :boundp)
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
                               (std-instance-wrapper ,instance))
                              ((fsc-instance-p ,instance)
                               ,@(unless class-slot-p
                                   `((setq slots
                                           (fsc-instance-slots ,instance))))
                               (fsc-instance-wrapper ,instance)))))
        (block access
          (when (and wrapper
                     (/= (layout-clos-hash wrapper ,field) 0)
                     ,@(if (eql 1 1-or-2-class)
                           `((eq wrapper wrapper-0))
                           `((or (eq wrapper wrapper-0)
                                 (eq wrapper wrapper-1)))))
            ,@(ecase reader/writer
                (:reader
                 `((let ((value ,read-form))
                     (unless (eq value +slot-unbound+)
                       (return-from access value)))))
                (:boundp
                 `((let ((value ,read-form))
                      (return-from access (not (eq value +slot-unbound+))))))
                (:writer
                 `((return-from access (setf ,read-form ,(car arglist)))))))
          (funcall miss-fn ,@arglist))))))

(defun emit-slot-read-form (class-slot-p index slots)
  (if class-slot-p
      `(cdr ,index)
      `(clos-slots-ref ,slots ,index)))

(defun emit-boundp-check (value-form miss-fn arglist)
  `(let ((value ,value-form))
     (if (eq value +slot-unbound+)
         (funcall ,miss-fn ,@arglist)
         value)))

(defun emit-slot-access (reader/writer class-slot-p slots
                         index miss-fn arglist)
  (let ((read-form (emit-slot-read-form class-slot-p index slots)))
    (ecase reader/writer
      (:reader (emit-boundp-check read-form miss-fn arglist))
      (:boundp `(not (eq ,read-form +slot-unbound+)))
      (:writer `(setf ,read-form ,(car arglist))))))

(defmacro emit-reader/writer-macro (reader/writer 1-or-2-class class-slot-p)
  (let ((*emit-function-p* nil)
        (*precompiling-lap* t))
    (values
     (emit-reader/writer reader/writer 1-or-2-class class-slot-p))))

(defun emit-one-or-n-index-reader/writer (reader/writer
                                          cached-index-p
                                          class-slot-p)
  (unless *optimize-cache-functions-p*
    (when (and (null *precompiling-lap*) *emit-function-p*)
      (return-from emit-one-or-n-index-reader/writer
        (emit-one-or-n-index-reader/writer-function
         reader/writer cached-index-p class-slot-p))))
  (multiple-value-bind (arglist metatypes)
      (ecase reader/writer
        ((:reader :boundp)
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
  (let ((*emit-function-p* nil)
        (*precompiling-lap* t))
    (values
     (emit-one-or-n-index-reader/writer reader/writer
                                        cached-index-p
                                        class-slot-p))))

(defun emit-miss (miss-fn args applyp)
  (if applyp
      `(multiple-value-call ,miss-fn ,@args
                            (sb-c::%more-arg-values .more-context.
                                                    0
                                                    .more-count.))
      `(funcall ,miss-fn ,@args)))

(defun emit-checking-or-caching (cached-emf-p return-value-p metatypes applyp)
  (unless *optimize-cache-functions-p*
    (when (and (null *precompiling-lap*) *emit-function-p*)
      (return-from emit-checking-or-caching
        (emit-checking-or-caching-function
         cached-emf-p return-value-p metatypes applyp))))
  (multiple-value-bind (lambda-list args rest-arg more-arg)
      (make-dlap-lambda-list metatypes applyp)
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
  (let ((*emit-function-p* nil)
        (*precompiling-lap* t))
    (values
     (emit-checking-or-caching cached-emf-p return-value-p metatypes applyp))))

(defun emit-dlap (cache-var args metatypes hit-form miss-form value-var
                  &optional slot-vars)
  (let* ((index -1)
         (miss-tag (gensym "MISSED"))
         (wrapper-bindings (mapcan (lambda (arg mt)
                                     (unless (eq mt t)
                                       (incf index)
                                       `((,(format-symbol *pcl-package*
                                                          "WRAPPER-~D"
                                                          index)
                                          ,(emit-fetch-wrapper
                                            mt arg miss-tag (pop slot-vars))))))
                                   args metatypes))
         (wrapper-vars (mapcar #'car wrapper-bindings)))
    (declare (fixnum index))
    (unless wrapper-vars
      (error "Every metatype is T."))
    `(prog ()
        (return
          (let ((field (cache-field ,cache-var))
                (cache-vector (cache-vector ,cache-var))
                (mask (cache-mask ,cache-var))
                (size (cache-size ,cache-var))
                (overflow (cache-overflow ,cache-var))
                ,@wrapper-bindings)
            (declare (fixnum size field mask))
            ,(emit-cache-lookup wrapper-vars miss-tag value-var)
            ,hit-form))
      ,miss-tag
        (return ,miss-form))))

(defun emit-cache-lookup (wrapper-vars miss-tag value-reg)
  (cond ((cdr wrapper-vars)
         (emit-greater-than-1-dlap wrapper-vars miss-tag value-reg))
        (value-reg
         (emit-1-t-dlap (car wrapper-vars) miss-tag value-reg))
        (t
         (emit-1-nil-dlap (car wrapper-vars) miss-tag))))

(defun emit-1-nil-dlap (wrapper miss-label)
  `(let* ((primary ,(emit-1-wrapper-compute-primary-cache-location wrapper
                                                                   miss-label))
          (location primary))
     (declare (fixnum primary location))
     (block search
       (loop (when (eq ,wrapper (cache-vector-ref cache-vector location))
               (return-from search nil))
             (setq location (the fixnum (+ location 1)))
             (when (= location size)
               (setq location 0))
             (when (= location primary)
               (dolist (entry overflow)
                 (when (eq (car entry) ,wrapper)
                   (return-from search nil)))
               (go ,miss-label))))))

(defmacro get-cache-vector-lock-count (cache-vector)
  `(let ((lock-count (cache-vector-lock-count ,cache-vector)))
     (unless (typep lock-count 'fixnum)
       (error "My cache got freed somehow."))
     (the fixnum lock-count)))

(defun emit-1-t-dlap (wrapper miss-label value)
  `(let ((primary ,(emit-1-wrapper-compute-primary-cache-location wrapper
                                                                  miss-label))
         (initial-lock-count (get-cache-vector-lock-count cache-vector)))
     (declare (fixnum primary initial-lock-count))
     (let ((location primary))
       (declare (fixnum location))
       (block search
         (loop (when (eq ,wrapper (cache-vector-ref cache-vector location))
                 (setq ,value (cache-vector-ref cache-vector (1+ location)))
                 (return-from search nil))
               (setq location (the fixnum (+ location 2)))
               (when (= location size)
                 (setq location 0))
               (when (= location primary)
                 (dolist (entry overflow)
                   (when (eq (car entry) ,wrapper)
                     (setq ,value (cdr entry))
                     (return-from search nil)))
                 (go ,miss-label))))
       (unless (= initial-lock-count
                  (get-cache-vector-lock-count cache-vector))
         (go ,miss-label)))))

(defun emit-greater-than-1-dlap (wrappers miss-label value)
  (declare (type list wrappers))
  (let ((cache-line-size (compute-line-size (+ (length wrappers)
                                               (if value 1 0)))))
    `(let ((primary 0)
           (size-1 (the fixnum (- size 1))))
       (declare (fixnum primary size-1))
       ,(emit-n-wrapper-compute-primary-cache-location wrappers miss-label)
       (let ((initial-lock-count (get-cache-vector-lock-count cache-vector)))
         (declare (fixnum initial-lock-count))
         (let ((location primary)
               (next-location 0))
           (declare (fixnum location next-location))
           (block search
             (loop (setq next-location
                         (the fixnum (+ location ,cache-line-size)))
                   (when (and ,@(mapcar
                                 (lambda (wrapper)
                                   `(eq ,wrapper
                                        (cache-vector-ref
                                         cache-vector
                                         (setq location
                                               (the fixnum (+ location 1))))))
                                 wrappers))
                     ,@(when value
                         `((setq location (the fixnum (+ location 1)))
                           (setq ,value (cache-vector-ref cache-vector
                                                          location))))
                     (return-from search nil))
                   (setq location next-location)
                   (when (= location size-1)
                     (setq location 0))
                   (when (= location primary)
                     (dolist (entry overflow)
                       (let ((entry-wrappers (car entry)))
                         (when (and ,@(mapcar (lambda (wrapper)
                                                `(eq ,wrapper
                                                     (pop entry-wrappers)))
                                              wrappers))
                           ,@(when value
                               `((setq ,value (cdr entry))))
                           (return-from search nil))))
                     (go ,miss-label))))
           (unless (= initial-lock-count
                      (get-cache-vector-lock-count cache-vector))
             (go ,miss-label)))))))

(defun emit-1-wrapper-compute-primary-cache-location (wrapper miss-label)
  `(let ((wrapper-cache-no (layout-clos-hash ,wrapper field)))
     (declare (fixnum wrapper-cache-no))
     (when (zerop wrapper-cache-no) (go ,miss-label))
     ,(let ((form `(logand mask wrapper-cache-no)))
        `(the fixnum ,form))))

(defun emit-n-wrapper-compute-primary-cache-location (wrappers miss-label)
  (declare (type list wrappers))
  ;; This returns 1 less that the actual location.
  `(progn
     ,@(let ((adds 0) (len (length wrappers)))
         (declare (fixnum adds len))
         (mapcar (lambda (wrapper)
                   `(let ((wrapper-cache-no (layout-clos-hash ,wrapper field)))
                      (declare (fixnum wrapper-cache-no))
                      (when (zerop wrapper-cache-no) (go ,miss-label))
                      (setq primary (the fixnum (+ primary wrapper-cache-no)))
                      ,@(progn
                          (incf adds)
                          (when (or (zerop (mod adds
                                                wrapper-cache-number-adds-ok))
                                    (eql adds len))
                            `((setq primary
                                    ,(let ((form `(logand primary mask)))
                                       `(the fixnum ,form))))))))
                 wrappers))))

;;; CMU17 (and SBCL) note: Since STD-INSTANCE-P is weakened in the
;;; CMU/SBCL approach of using funcallable instances, that branch may
;;; run on non-pcl instances (structures). The result will be the
;;; non-wrapper layout for the structure, which will cause a miss. The
;;; "slots" will be whatever the first slot is, but will be ignored.
;;; Similarly, FSC-INSTANCE-P returns true on funcallable structures
;;; as well as PCL fins.
(defun emit-fetch-wrapper (metatype argument miss-label &optional slot)
  (ecase metatype
    ((standard-instance)
     `(cond ((std-instance-p ,argument)
             ,@(when slot `((setq ,slot (std-instance-slots ,argument))))
             (std-instance-wrapper ,argument))
            ((fsc-instance-p ,argument)
             ,@(when slot `((setq ,slot (fsc-instance-slots ,argument))))
             (fsc-instance-wrapper ,argument))
            (t
             (go ,miss-label))))
    (class
     (when slot (error "can't do a slot reg for this metatype"))
     `(wrapper-of ,argument))
    ((built-in-instance structure-instance)
     (when slot (error "can't do a slot reg for this metatype"))
     `(built-in-or-structure-wrapper
       ,argument))))
