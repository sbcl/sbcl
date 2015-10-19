;;;; functions and macros to define and deal with internal errors
;;;; (i.e. problems that can be signaled from assembler code)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;;; internal errors

(macrolet ((def-it ()
             (let ((n (1+ (position-if 'stringp sb!c:+backend-internal-errors+
                                       :key #'car :from-end t))))
               `(progn
                  (declaim ((simple-vector ,n) **internal-error-handlers**))
                  (defglobal **internal-error-handlers**
                    (make-array ,n :initial-element 0))))))
  (def-it))

(eval-when (:compile-toplevel :execute)
(sb!xc:defmacro deferr (name args &rest body)
  (multiple-value-bind (llks required optional rest) (parse-lambda-list args)
    (declare (ignore llks))
    (aver (not rest))
    (let ((max (+ (length required) (length optional))))
      (unless (<= max 3)
        (error "Update (DEFUN INTERNAL-ERROR) for ~D error arguments" max))))
  `(setf (svref **internal-error-handlers** ,(error-number-or-lose name))
         (named-lambda ,(string name) (,@args)
           (declare (optimize (sb!c::verify-arg-count 0)))
           ,@body)))) ; EVAL-WHEN

(deferr undefined-fun-error (fdefn-or-symbol)
  (error 'undefined-function
         :name (etypecase fdefn-or-symbol
                 (symbol fdefn-or-symbol)
                 (fdefn (fdefn-name fdefn-or-symbol)))))

#!+(or arm arm64 x86-64)
(deferr undefined-alien-fun-error (address)
  (error 'undefined-alien-function-error
         :name
         (and (integerp address)
              (sap-foreign-symbol (int-sap address)))))

#!-(or arm arm64 x86-64)
(defun undefined-alien-fun-error ()
  (error 'undefined-alien-function-error))

(deferr invalid-arg-count-error (nargs &optional (fname nil fnamep))
  (if fnamep
      (error 'simple-program-error
         :format-control "~S called with invalid number of arguments: ~S"
         :format-arguments (list fname nargs))
      (error 'simple-program-error
             :format-control "invalid number of arguments: ~S"
             :format-arguments (list nargs))))

(deferr bogus-arg-to-values-list-error (list)
  (error 'simple-type-error
         :datum list
         :expected-type 'list
         :format-control
         "~@<attempt to use VALUES-LIST on a dotted list: ~2I~_~S~:>"
         :format-arguments (list list)))

(deferr unbound-symbol-error (symbol)
  (error 'unbound-variable :name symbol))

(deferr invalid-unwind-error ()
  (error 'simple-control-error
         :format-control
         "attempt to RETURN-FROM a block or GO to a tag that no longer exists"))

(deferr unseen-throw-tag-error (tag)
  (let ((text "attempt to THROW to a tag that does not exist: ~S"))
    #!+sb-fasteval
    (when (listp tag)
      (multiple-value-bind (name frame)
          (sb!debug::find-interrupted-name-and-frame)
        (let ((down (and (eq name 'sb!c::unwind) ; is this tautological ?
                         (sb!di:frame-down frame))))
          (when frame
            ;; Is this really the canonical way to get a frame name?
            (let ((prev-frame-name
                   (sb!di:debug-fun-name (sb!di:frame-debug-fun down))))
              (when (and (listp prev-frame-name)
                         (eq (car prev-frame-name) 'sb!c::xep))
                (setq prev-frame-name (second prev-frame-name)))
              (cond ((equal prev-frame-name '(eval return-from))
                     (setq text "attempt to RETURN-FROM an exited block: ~S"
                           ;; block name was wrapped in a cons
                           tag (car tag)))
                    ((equal prev-frame-name '(eval go))
                     ;; FIXME: can we reverse-engineer the tag name from
                     ;; the object that was thrown, for a better diagnostic?
                     (setq text "attempt to GO into an exited tagbody"))))))))
    (error 'simple-control-error
           :format-control text :format-arguments (list tag))))

(deferr nil-fun-returned-error (function)
  (error 'simple-control-error
         :format-control
         "A function with declared result type NIL returned:~%  ~S"
         :format-arguments (list function)))

(deferr nil-array-accessed-error (array)
  (error 'nil-array-accessed-error
         :datum array :expected-type '(not (array nil))))

(deferr division-by-zero-error (this that)
  (error 'division-by-zero
         :operation '/
         :operands (list this that)))

(deferr object-not-type-error (object type)
  (if (invalid-array-p object)
      (invalid-array-error object)
      (error (if (and (%instancep object)
                      (layout-invalid (%instance-layout object)))
                 'layout-invalid
                 'type-error)
             :datum object
             :expected-type type)))

(deferr layout-invalid-error (object layout)
  (error 'layout-invalid
         :datum object
         :expected-type (layout-classoid layout)))

(deferr odd-key-args-error ()
  (error 'simple-program-error
         :format-control "odd number of &KEY arguments"))

(deferr unknown-key-arg-error (key-name)
  (error 'simple-program-error
         :format-control "unknown &KEY argument: ~S"
         :format-arguments (list key-name)))

;; FIXME: missing (deferr wrong-number-of-indices)
;; we don't ever raise that error through a primitive trap I guess.

;; TODO: make the arguments (ARRAY INDEX &optional BOUND)
;; and don't need the bound for vectors. Just read it.
(deferr invalid-array-index-error (array bound index)
  (invalid-array-index-error array index bound))

(deferr tls-exhausted-error ()
  ;; There is nothing we can do about it. A number of entries in the
  ;; tls could be reserved and made available for recovery but since
  ;; tls indices are never reused it would be kind of silly and
  ;; without it signalling an error is more than likely to end in a
  ;; recursive error.
  (%primitive print "Thread local storage exhausted.")
  (sb!impl::%halt))


;;;; fetching errorful function name

;;; This flag is used to prevent infinite recursive lossage when
;;; we can't find the caller for some reason.
(defvar *finding-frame* nil)

(defun find-caller-frame ()
  (unless *finding-frame*
    (handler-case
        (let* ((*finding-frame* t)
               (frame (sb!di:frame-down (sb!di:frame-down (sb!di:top-frame)))))
          (sb!di:flush-frames-above frame)
          frame)
      ((or error sb!di:debug-condition) ()))))

(defun find-interrupted-frame ()
  (/show0 "entering FIND-INTERRUPTED-FRAME")
  (unless *finding-frame*
    (handler-case
        (let ((*finding-frame* t))
          (/show0 "in ordinary case")
          (do ((frame (sb!di:top-frame) (sb!di:frame-down frame)))
              ((null frame)
               (/show0 "null frame")
               nil)
            (/noshow0 "at head of DO loop")
            (when (and (sb!di::compiled-frame-p frame)
                       (sb!di::compiled-frame-escaped frame))
              (sb!di:flush-frames-above frame)
              (/show0 "returning from within DO loop")
              (return frame))))
      (error ()
        (/show0 "trapped ERROR")
        nil)
      (sb!di:debug-condition ()
        (/show0 "trapped DEBUG-CONDITION")
        nil))))

(defun find-caller-of-named-frame (name)
  (unless *finding-frame*
    (handler-case
        (let ((*finding-frame* t))
          (do ((frame (sb!di:top-frame) (sb!di:frame-down frame)))
              ((null frame))
            (when (and (sb!di::compiled-frame-p frame)
                       (eq name (sb!di:debug-fun-name
                                 (sb!di:frame-debug-fun frame))))
              (let ((caller (sb!di:frame-down frame)))
                (sb!di:flush-frames-above caller)
                (return caller)))))
      ((or error sb!di:debug-condition) ()
        nil)
      (sb!di:debug-condition ()
        nil))))


;;; Returns true if number of arguments matches required/optional
;;; arguments handler expects.
(defun internal-error-args-ok (arguments handler)
  (multiple-value-bind (llks req opt)
      (parse-lambda-list (%simple-fun-arglist handler) :silent t)
    (declare (ignore llks))
    (let ((n (length arguments))
          (n-req (length req))
          (n-opt (length opt)))
      (and (>= n n-req) (<= n (+ n-req n-opt))))))

;;;; INTERNAL-ERROR signal handler

;;; Backtrace code may want to know the error that caused
;;; interruption, but there are other means to get code interrupted
;;; and inspecting code around PC for the error number may yield wrong
;;; results.
(defvar *current-internal-error* nil)

;;; This is needed for restarting XEPs, which do not bind anything but
;;; also do not save their own BSP, and we need to discard the
;;; bindings made by the error handling machinery.
#!+unwind-to-frame-and-call-vop
(defvar *interr-current-bsp* nil)

(defun internal-error (context continuable)
  (declare (type system-area-pointer context))
  (declare (ignore continuable))
  (/show0 "entering INTERNAL-ERROR, CONTEXT=..")
  (/hexstr context)
  (let (#!+unwind-to-frame-and-call-vop
        (*interr-current-bsp*
          ;; Needs to be done before anything is bound
          (%primitive sb!c:current-binding-pointer)))
   (infinite-error-protect
    (/show0 "about to bind ALIEN-CONTEXT")
    (let* ((alien-context (sap-alien context (* os-context-t)))
           #!+c-stack-is-control-stack
           (fp-and-pc (make-array 2 :element-type 'word)))
      #!+c-stack-is-control-stack
      (declare (truly-dynamic-extent fp-and-pc))
      #!+c-stack-is-control-stack
      (setf (aref fp-and-pc 0) (sb!vm:context-register alien-context sb!vm::cfp-offset)
            (aref fp-and-pc 1) (sb!sys:sap-int (sb!vm:context-pc alien-context)))
      (let (#!+c-stack-is-control-stack
            (*saved-fp-and-pcs* (cons fp-and-pc *saved-fp-and-pcs*)))
        #!+c-stack-is-control-stack
        (declare (truly-dynamic-extent *saved-fp-and-pcs*))
       (/show0 "about to bind ERROR-NUMBER and ARGUMENTS"))
      (multiple-value-bind (error-number arguments)
          (sb!vm:internal-error-args alien-context)
        (with-interrupt-bindings
          (let ((sb!debug:*stack-top-hint* (find-interrupted-frame))
                (*current-internal-error* error-number)
                (fp (int-sap (sb!vm:context-register alien-context
                                                     sb!vm::cfp-offset))))
            (if (and (>= error-number (length **internal-error-handlers**))
                     (< error-number (length sb!c:+backend-internal-errors+)))
                (error 'type-error
                       :datum (sb!di::sub-access-debug-var-slot
                               fp (first arguments) alien-context)
                       :expected-type
                       (car (svref sb!c:+backend-internal-errors+
                                   error-number)))
                (let ((handler
                        (and (typep error-number
                                    '#.`(mod ,(length **internal-error-handlers**)))
                             (svref **internal-error-handlers** error-number))))
                  (cond
                    ((and (functionp handler)
                          (internal-error-args-ok arguments handler))
                     (macrolet ((arg (n)
                                  `(sb!di::sub-access-debug-var-slot
                                    fp (nth ,n arguments) alien-context)))
                       (ecase (length arguments)
                         (0 (funcall handler))
                         (1 (funcall handler (arg 0)))
                         (2 (funcall handler (arg 0) (arg 1)))
                         (3 (funcall handler (arg 0) (arg 1) (arg 2))))))
                    ((eql handler 0) ; if (DEFERR x) was inadvertently omitted
                     (error 'simple-error
                            :format-control
                            "unknown internal error, ~D, args=~S"
                            :format-arguments
                            (list error-number
                                  (mapcar (lambda (sc-offset)
                                            (sb!di::sub-access-debug-var-slot
                                             fp sc-offset alien-context))
                                          arguments))))
                    (t                  ; wtf?
                     (error 'simple-error
                            :format-control "internal error ~D: ~A; args=~S"
                            :format-arguments
                            (list error-number
                                  handler
                                  (mapcar (lambda (sc-offset)
                                            (sb!di::sub-access-debug-var-slot
                                             fp sc-offset alien-context))
                                          arguments))))))))))))))

(defun control-stack-exhausted-error ()
  (let ((sb!debug:*stack-top-hint* nil))
    (infinite-error-protect
     (format *error-output*
             "Control stack guard page temporarily disabled: proceed with caution~%")
     (error 'control-stack-exhausted))))

(defun binding-stack-exhausted-error ()
  (let ((sb!debug:*stack-top-hint* nil))
    (infinite-error-protect
     (format *error-output*
             "Binding stack guard page temporarily disabled: proceed with caution~%")
     (error 'binding-stack-exhausted))))

(defun alien-stack-exhausted-error ()
  (let ((sb!debug:*stack-top-hint* nil))
    (infinite-error-protect
     (format *error-output*
             "Alien stack guard page temporarily disabled: proceed with caution~%")
     (error 'alien-stack-exhausted))))

;;; KLUDGE: we keep a single HEAP-EXHAUSTED-ERROR object around, so
;;; that we don't need to allocate it when running out of
;;; memory. Similarly we pass the amounts in special variables as
;;; there may be multiple threads running into trouble at the same
;;; time. The condition is created by GC-REINIT.
(defvar *heap-exhausted-error-condition*)
(defvar *heap-exhausted-error-available-bytes*)
(defvar *heap-exhausted-error-requested-bytes*)

(defun heap-exhausted-error (available requested)
  ;; Double word aligned bytes, can be passed as fixnums to avoid
  ;; allocating bignums on the C side.
  (declare (fixnum available requested))
  (infinite-error-protect
   (let ((*heap-exhausted-error-available-bytes*
           (ash available sb!vm:n-fixnum-tag-bits))
         (*heap-exhausted-error-requested-bytes*
           (ash requested sb!vm:n-fixnum-tag-bits)))
     (error *heap-exhausted-error-condition*))))

(defun undefined-alien-variable-error ()
  (error 'undefined-alien-variable-error))

#!-win32
(define-alien-variable current-memory-fault-address unsigned)

#!-win32
(defun memory-fault-error ()
  (error 'memory-fault-error
         :address current-memory-fault-address))

;;; This is SIGTRAP / EXCEPTION_BREAKPOINT that runtime could not deal
;;; with. Prior to Windows we just had a Lisp side handler for
;;; SIGTRAP, but now we need to deal with this portably.
(defun unhandled-trap-error (context-sap)
  (declare (type system-area-pointer context-sap))
  (infinite-error-protect
   (let ((context (sap-alien context-sap (* os-context-t))))
     (error 'breakpoint-error
            :context context
            :address (sap-int (sb!vm:context-pc context))))))
