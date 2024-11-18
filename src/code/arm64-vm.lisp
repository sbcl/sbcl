;;; This file contains the ARM specific runtime stuff.
;;;
(in-package "SB-VM")

(defun machine-type ()
  "Return a string describing the type of the local machine."
  "ARM64")

(defun return-machine-address (scp)
  (context-register scp lr-offset))

;;;; "Sigcontext" access functions, cut & pasted from sparc-vm.lisp,
;;;; then modified for ARM.
;;;;
;;;; See also x86-vm for commentary on signed vs unsigned.

(define-alien-routine ("os_context_float_register_addr" context-float-register-addr)
  (* unsigned) (context (* os-context-t)) (index int))

(defun context-float-register (context index format &optional integer)
  (let ((sap (alien-sap (context-float-register-addr context index))))
    (ecase format
      (single-float
       (if integer
           (values (sap-ref-32 sap 0) 4)
           (sap-ref-single sap 0)))
      (double-float
       (if integer
           (values (sap-ref-64 sap 0) 8)
           (sap-ref-double sap 0)))
      (complex-single-float
       (complex (sap-ref-single sap 0)
                (sap-ref-single sap 4)))
      (complex-double-float
       (if integer
           (values (dpb (sap-ref-64 sap 8)
                        (byte 64 64)
                        (sap-ref-64 sap 0))
                   16)
           (complex (sap-ref-double sap 0)
                    (sap-ref-double sap 8)))))))

(defun %set-context-float-register (context index format value)
  (let ((sap (alien-sap (context-float-register-addr context index))))
    (ecase format
      (single-float
       (setf (sap-ref-single sap 0) value))
      (double-float
       (setf (sap-ref-double sap 0) value))
      (complex-single-float
       (locally
           (declare (type (complex single-float) value))
         (setf (sap-ref-single sap 0) (realpart value)
               (sap-ref-single sap 4) (imagpart value))))
      (complex-double-float
       (locally
           (declare (type (complex double-float) value))
         (setf (sap-ref-double sap 0) (realpart value)
               (sap-ref-double sap 8) (imagpart value)))))))

;;;; INTERNAL-ERROR-ARGS.

;;; Given a (POSIX) signal context, extract the internal error
;;; arguments from the instruction stream.
;;;
;;; See EMIT-ERROR-BREAK for the scheme
(defun internal-error-args (context)
  (declare (type (alien (* os-context-t)) context))
  (let* ((pc (context-pc context))
         (instruction (sap-ref-32 pc 0))
         (trap-number (ldb (byte 8 5) instruction))
         (error-number (cond
                         ((>= trap-number sb-vm:error-trap)
                          (prog1
                              (- trap-number sb-vm:error-trap)
                            (setf trap-number sb-vm:error-trap)))
                         (t
                          (prog1 (sap-ref-8 pc 4)
                            (setf pc (sap+ pc 1))))))
         (first-arg (ldb (byte 8 13) instruction))
         (first-offset (ldb (byte 5 0) first-arg))
         (first-sc (ldb (byte 2 5) first-arg)))
    (declare (type system-area-pointer pc))
    (if (= trap-number invalid-arg-count-trap)
        (values #.(error-number-or-lose 'invalid-arg-count-error)
                '(#.arg-count-sc)
                trap-number)
        (let ((length (sb-kernel::error-length error-number)))
          (declare (type (unsigned-byte 8) length))
          (unless (or (= first-arg zr-offset)
                      (zerop length))
            (decf length))
          (setf pc (sap+ pc 4))
          (let ((args (loop with index = 0
                            repeat length
                            collect (sb-c:sap-read-var-integerf pc index))))
            (values error-number
                    (if (= first-offset zr-offset)
                        args
                        (cons (make-sc+offset (case first-sc
                                                (1 sb-vm:unsigned-reg-sc-number)
                                                (2 sb-vm:signed-reg-sc-number)
                                                (t sb-vm:descriptor-reg-sc-number))
                                              first-offset)
                              args))
                    trap-number))))))

;;; Undo the effects of XEP-ALLOCATE-FRAME
;;; and point PC to FUNCTION
(defun context-call-function (context function &optional arg-count)
  (with-pinned-objects (function)
    (with-pinned-context-code-object (context)
      (let* ((fun-addr (get-lisp-obj-address function))
             (entry (+ (sap-ref-word (int-sap fun-addr)
                                     (- (ash simple-fun-self-slot word-shift)
                                        fun-pointer-lowtag))
                       4))) ;; tail call
        (when arg-count
          (setf (context-register context nargs-offset)
                (get-lisp-obj-address arg-count)))
        (setf (context-register context lexenv-offset) fun-addr)
        (set-context-pc context entry)))))

#+darwin-jit
(progn
  (define-alien-routine jit-patch
    void
    (address unsigned)
    (value unsigned))

  (define-alien-routine jit-patch-code
    void
    (code unsigned)
    (value unsigned)
    (index unsigned))

  (define-alien-routine jit-memcpy
    void
    (dst (* char))
    (src (* char))
    (char signed))

  (define-alien-routine jit-copy-code-constants
    void
    (dst unsigned)
    (src unsigned))

  (defun (setf sap-ref-word-jit) (value sap offset)
    (jit-patch (+ (sap-int sap) offset) value))

  (defun (setf code-header-ref) (value code index)
    (with-pinned-objects (code value)
      (jit-patch-code (get-lisp-obj-address code)
                      (get-lisp-obj-address value)
                      index))
    value))

(defconstant n-bit 31)
(defconstant z-bit 30)
(defconstant c-bit 29)
(defconstant v-bit 28)

(defun context-overflow-carry-flags (context)
  (let ((flags (context-flags context)))
    (values (logbitp v-bit flags)
            (logbitp c-bit flags))))
