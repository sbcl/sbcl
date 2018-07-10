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

#-sb-xc-host
(defun machine-type ()
  "Return a string describing the type of the local machine."
  "Alpha")

(defconstant-eqx +fixup-kinds+
    #(:jmp-hint :bits-63-48 :bits-47-32 :ldah :lda :absolute32)
  #'equalp)
(!with-bigvec-or-sap
(defun fixup-code-object (code offset value kind flavor)
  (declare (ignore flavor))
  (unless (zerop (rem offset sb!assem:+inst-alignment-bytes+))
    (error "Unaligned instruction?  offset=#x~X." offset))
  (let ((sap (code-instructions code)))
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
        (setf (sap-ref-8 sap (1+ offset)) (ldb (byte 8 8) value)))
       (:absolute32
        (setf (sap-ref-32 sap offset) value))))
  nil))

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

#-sb-xc-host (progn
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


(defun internal-error-args (context)
  (declare (type (alien (* os-context-t)) context))
  (let* ((pc (context-pc context))
         (trap-number (sap-ref-8 pc 3)))
    (declare (type system-area-pointer pc))
    (sb!kernel::decode-internal-error-args (sap+ pc 4) trap-number)))
) ; end PROGN
