;;;; Alpha-specific implementation stuff

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;; See x86-vm.lisp for a description of this.
(define-alien-type os-context-t (struct os-context-t-struct))

;;;; MACHINE-TYPE

(defun machine-type ()
  "Return a string describing the type of the local machine."
  "Alpha")

(defun fixup-code-object (code offset value kind)
  (unless (zerop (rem offset n-word-bytes))
    (error "Unaligned instruction?  offset=#x~X." offset))
  (without-gcing
   (let ((sap (%primitive code-instructions code)))
     (ecase kind
       (:jmp-hint
        (aver (zerop (ldb (byte 2 0) value)))
        #+nil
        (setf (sap-ref-16 sap offset)
              (logior (sap-ref-16 sap offset)
                      (ldb (byte 14 0) (ash value -2)))))
       (:bits-63-48
        (let* ((value (if (logbitp 15 value) (+ value (ash 1 16)) value))
               (value (if (logbitp 31 value) (+ value (ash 1 32)) value))
               (value (if (logbitp 47 value) (+ value (ash 1 48)) value)))
          (setf (sap-ref-8 sap offset) (ldb (byte 8 48) value))
          (setf (sap-ref-8 sap (1+ offset)) (ldb (byte 8 56) value))))
       (:bits-47-32
        (let* ((value (if (logbitp 15 value) (+ value (ash 1 16)) value))
               (value (if (logbitp 31 value) (+ value (ash 1 32)) value)))
          (setf (sap-ref-8 sap offset) (ldb (byte 8 32) value))
          (setf (sap-ref-8 sap (1+ offset)) (ldb (byte 8 40) value))))
       (:ldah
        (let ((value (if (logbitp 15 value) (+ value (ash 1 16)) value)))
          (setf (sap-ref-8 sap offset) (ldb (byte 8 16) value))
          (setf (sap-ref-8 sap (1+ offset)) (ldb (byte 8 24) value))))
       (:lda
        (setf (sap-ref-8 sap offset) (ldb (byte 8 0) value))
        (setf (sap-ref-8 sap (1+ offset)) (ldb (byte 8 8) value)))))))

;;;; "sigcontext" access functions, cut & pasted from x86-vm.lisp then
;;;; hacked for types.
;;;;
;;;; KLUDGE: The alpha has 64-bit registers, so these potentially
;;;; return 64 bit numbers (which means bignums ... ew) We think that
;;;; 99 times of 100 (i.e. unless something is badly wrong) we'll get
;;;; answers that fit in 32 bits anyway. Which probably won't help us
;;;; stop passing bignums around as the compiler can't prove they fit
;;;; in 32 bits. But maybe the stuff it does on x86 to unbox 32-bit
;;;; constants happens magically for 64-bit constants here. Just
;;;; maybe. -- Dan Barlow, ca. 2001-05-05
;;;;
;;;; See also x86-vm for commentary on signed vs unsigned.

(define-alien-routine ("os_context_pc_addr" context-pc-addr) (* unsigned-long)
  (context (* os-context-t)))

(defun context-pc (context)
  (declare (type (alien (* os-context-t)) context))
  (int-sap (deref (context-pc-addr context))))

(define-alien-routine ("os_context_register_addr" context-register-addr)
  (* unsigned-long)
  (context (* os-context-t))
  (index int))

;;; FIXME: Should this and CONTEXT-PC be INLINE to reduce consing?
;;; (Are they used in anything time-critical, or just the debugger?)
(defun context-register (context index)
  (declare (type (alien (* os-context-t)) context))
  (deref (the (alien (* unsigned-long))
           (context-register-addr context index))))

(defun %set-context-register (context index new)
  (declare (type (alien (* os-context-t)) context))
  (setf (deref (the (alien (* unsigned-long))
                 (context-register-addr context index)))
        new))

;;; This is like CONTEXT-REGISTER, but returns the value of a float
;;; register. FORMAT is the type of float to return.

;;; FIXME: Whether COERCE actually knows how to make a float out of a
;;; long is another question. This stuff still needs testing.
(define-alien-routine ("os_context_float_register_addr"
                       context-float-register-addr)
  (* long)
  (context (* os-context-t))
  (index int))
(defun context-float-register (context index format)
  (declare (type (alien (* os-context-t)) context))
  (coerce (deref (context-float-register-addr context index)) format))
(defun %set-context-float-register (context index format new)
  (declare (type (alien (* os-context-t)) context))
  (setf (deref (context-float-register-addr context index))
        (coerce new format)))

;;; This sets the software fp_control word, which is not the same
;;; thing as the hardware fpcr.  We have to do this so that OS FPU
;;; completion works properly

;;; Note that this means we can't set rounding modes; we'd have to do
;;; that separately.  That said, almost everybody seems to agree that
;;; changing the rounding mode is rarely a good idea, because it upsets
;;; libm functions.  So adding that is not a priority.  Sorry.
;;; -dan 2001.02.06

(define-alien-routine
    ("arch_get_fp_control" floating-point-modes) (unsigned 64))

(define-alien-routine
    ("arch_set_fp_control" %floating-point-modes-setter) void (fp (unsigned 64)))

(defun (setf floating-point-modes) (val) (%floating-point-modes-setter val))

;;; Given a signal context, return the floating point modes word in
;;; the same format as returned by FLOATING-POINT-MODES.
(define-alien-routine ("os_context_fp_control" context-floating-point-modes)
    (unsigned 64) (context (* os-context-t)))


;;;; INTERNAL-ERROR-ARGS

;;; Given a (POSIX) signal context, extract the internal error
;;; arguments from the instruction stream.  This is e.g.
;;; 4       23      254     240     2       0       0       0
;;; |       ~~~~~~~~~~~~~~~~~~~~~~~~~
;;; length         data              (everything is an octet)
;;;  (pc)
;;; (example from undefined_tramp: "(gdb) x/40ub 0x10148" for yourself
;;; to replicate)
(defun internal-error-args (context)
  (declare (type (alien (* os-context-t)) context))
  (let ((pc (context-pc context)))
    (declare (type system-area-pointer pc))
    ;; pc is a SAP pointing at - or actually, shortly after -
    ;; the instruction that got us into this mess in the first place
    (let* ((length (sap-ref-8 pc 4))
           (vector (make-array length :element-type '(unsigned-byte 8))))
      (declare (type (unsigned-byte 8) length)
               (type (simple-array (unsigned-byte 8) (*)) vector))
      (copy-ub8-from-system-area pc 5 vector 0 length)
      (let* ((index 0)
             (error-number (sb!c:read-var-integer vector index)))
        (collect ((sc-offsets))
                 (loop
                  (when (>= index length)
                    (return))
                  (sc-offsets (sb!c:read-var-integer vector index)))
                 (values error-number (sc-offsets)))))))

