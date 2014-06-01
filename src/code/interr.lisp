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

(defvar *internal-errors*
  #.(map 'vector
         (lambda (x) (if (typep (car x) '(or symbol cons)) (car x) 0))
         sb!c:*backend-internal-errors*))

(eval-when (:compile-toplevel :execute)
(sb!xc:defmacro deferr (name args &rest body)
  (let ((n (length args)))
    (unless (<= n 3)
      (error "Update (DEFUN INTERNAL-ERROR) for ~D error arguments" n)))
  `(setf (svref *internal-errors* ,(error-number-or-lose name))
         (lambda (name ,@args)
           (declare (optimize (sb!c::verify-arg-count 0)) (ignorable name))
           ,@body)))
) ; EVAL-WHEN

(deferr undefined-fun-error (fdefn-or-symbol)
  (error 'undefined-function
         :name (etypecase fdefn-or-symbol
                 (symbol fdefn-or-symbol)
                 (fdefn (fdefn-name fdefn-or-symbol)))))

#!+(or arm x86-64)
(deferr undefined-alien-fun-error (address)
  (error 'undefined-alien-function-error
         :name
         (and (integerp address)
              (sap-foreign-symbol (int-sap address)))))

#!-(or arm x86-64)
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
  (error 'simple-control-error
         :format-control "attempt to THROW to a tag that does not exist: ~S"
         :format-arguments (list tag)))

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
         :operation 'division
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
(defvar *finding-name* nil)

(defun find-caller-name-and-frame ()
  (if *finding-name*
      (values "<error finding caller name -- already finding name>" nil)
      (handler-case
          (let* ((*finding-name* t)
                 (frame (sb!di:frame-down (sb!di:frame-down (sb!di:top-frame))))
                 (name (sb!di:debug-fun-name
                        (sb!di:frame-debug-fun frame))))
            (sb!di:flush-frames-above frame)
            (values name frame))
        (error ()
          (values "<error finding caller name -- trapped error>" nil))
        (sb!di:debug-condition ()
          (values "<error finding caller name -- trapped debug-condition>"
                  nil)))))

(defun find-interrupted-name-and-frame ()
  (/show0 "entering FIND-INTERRUPTED-NAME-AND-FRAME")
  (if *finding-name*
      (values "<error finding interrupted name -- already finding name>" nil)
      (handler-case
          (let ((*finding-name* t))
            (/show0 "in ordinary case")
            (do ((frame (sb!di:top-frame) (sb!di:frame-down frame)))
                ((null frame)
                 (/show0 "null frame")
                 (values "<error finding interrupted name -- null frame>" nil))
              (/show0 "at head of DO loop")
              (when (and (sb!di::compiled-frame-p frame)
                         (sb!di::compiled-frame-escaped frame))
                (sb!di:flush-frames-above frame)
                (/show0 "returning from within DO loop")
                (return (values (sb!di:debug-fun-name
                                 (sb!di:frame-debug-fun frame))
                                frame)))))
        (error ()
          (/show0 "trapped ERROR")
          (values "<error finding interrupted name -- trapped error>" nil))
        (sb!di:debug-condition ()
          (/show0 "trapped DEBUG-CONDITION")
          (values "<error finding interrupted name -- trapped debug-condition>"
                  nil)))))

(defun find-caller-of-named-frame (name)
  (unless *finding-name*
    (handler-case
        (let ((*finding-name* t))
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
  (multiple-value-bind (req opt)
      (parse-lambda-list (%simple-fun-arglist handler) :silent t)
    ;; The handler always gets name as the first (extra) argument.
    (let ((n (1+ (length arguments)))
          (n-req (length req))
          (n-opt (length opt)))
      (and (>= n n-req) (<= n (+ n-req n-opt))))))

;;;; INTERNAL-ERROR signal handler

(defun internal-error (context continuable)
  (declare (type system-area-pointer context))
  (declare (ignore continuable))
  (/show0 "entering INTERNAL-ERROR, CONTEXT=..")
  (/hexstr context)
  (infinite-error-protect
   (/show0 "about to bind ALIEN-CONTEXT")
   (let* ((alien-context (locally
                             (declare (optimize (inhibit-warnings 3)))
                           (sap-alien context (* os-context-t))))
          #!+c-stack-is-control-stack
          (*saved-fp-and-pcs*
           (cons (cons (%make-lisp-obj (sb!vm:context-register
                                        alien-context
                                        sb!vm::cfp-offset))
                       (sb!vm:context-pc alien-context))
                 (when (boundp '*saved-fp-and-pcs*)
                   *saved-fp-and-pcs*))))
     (declare (truly-dynamic-extent *saved-fp-and-pcs*))
     (/show0 "about to bind ERROR-NUMBER and ARGUMENTS")
     (multiple-value-bind (error-number arguments)
         (sb!vm:internal-error-args alien-context)

       ;; There's a limit to how much error reporting we can usefully
       ;; do before initialization is complete, but try to be a little
       ;; bit helpful before we die.
       (/show0 "back from INTERNAL-ERROR-ARGS, ERROR-NUMBER=..")
       (/hexstr error-number)
       (/show0 "cold/low ARGUMENTS=..")
       (/hexstr arguments)
       (unless *cold-init-complete-p*
         (%primitive print "can't recover from error in cold init, halting")
         (%primitive sb!c:halt))

       (with-interrupt-bindings
         (multiple-value-bind (name sb!debug:*stack-top-hint*)
             (find-interrupted-name-and-frame)
           (/show0 "back from FIND-INTERRUPTED-NAME")
           (let ((fp (int-sap (sb!vm:context-register alien-context
                                                      sb!vm::cfp-offset)))
                 (handler (and (< -1 error-number (length *internal-errors*))
                               (svref *internal-errors* error-number))))
             (cond ((and (functionp handler)
                         (internal-error-args-ok arguments handler))
                    (macrolet ((arg (n)
                                 `(sb!di::sub-access-debug-var-slot
                                   fp (nth ,n arguments) alien-context)))
                      (ecase (length arguments)
                        (0 (funcall handler name))
                        (1 (funcall handler name (arg 0)))
                        (2 (funcall handler name (arg 0) (arg 1)))
                        (3 (funcall handler name (arg 0) (arg 1) (arg 2))))))
                   ((typep handler '(or symbol cons))
                    (error 'type-error
                           :datum (sb!di::sub-access-debug-var-slot
                                   fp (first arguments) alien-context)
                           :expected-type handler))
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
                   (t ; wtf?
                    (error 'simple-error
                           :format-control "internal error ~D: ~A; args=~S"
                           :format-arguments
                           (list error-number
                                 handler
                                 (mapcar (lambda (sc-offset)
                                           (sb!di::sub-access-debug-var-slot
                                            fp sc-offset alien-context))
                                         arguments))))))))))))

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
  (infinite-error-protect
   (let ((*heap-exhausted-error-available-bytes* available)
         (*heap-exhausted-error-requested-bytes* requested))
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
