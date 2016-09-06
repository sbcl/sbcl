;;;; SPARC-specific runtime stuff

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
  "Returns a string describing the type of the local machine."
  "SPARC")

(defun fixup-code-object (code offset fixup kind)
  (declare (type index offset))
  (unless (zerop (rem offset n-word-bytes))
    (error "Unaligned instruction?  offset=#x~X." offset))
  (without-gcing
   (let ((sap (%primitive code-instructions code)))
     (ecase kind
       (:call
        (error "Can't deal with CALL fixups, yet."))
       (:sethi
        (setf (ldb (byte 22 0) (sap-ref-32 sap offset))
              (ldb (byte 22 10) fixup)))
       (:add
        (setf (ldb (byte 10 0) (sap-ref-32 sap offset))
              (ldb (byte 10 0) fixup)))
       (:absolute
        (setf (sap-ref-32 sap offset)
              fixup))))))


;;;; "Sigcontext" access functions, cut & pasted from alpha-vm.lisp.
;;;;
;;;; See also x86-vm for commentary on signed vs unsigned.

(define-alien-routine ("os_context_pc_addr" context-pc-addr) (* unsigned-int)
  (context (* os-context-t)))

(defun context-pc (context)
  (declare (type (alien (* os-context-t)) context))
  (int-sap (deref (context-pc-addr context))))

(define-alien-routine ("os_context_register_addr" context-register-addr)
  (* unsigned-int)
  (context (* os-context-t))
  (index int))

;;; FIXME: Should this and CONTEXT-PC be INLINE to reduce consing?
;;; (Are they used in anything time-critical, or just the debugger?)
(defun context-register (context index)
  (declare (type (alien (* os-context-t)) context))
  (deref (context-register-addr context index)))

(defun %set-context-register (context index new)
(declare (type (alien (* os-context-t)) context))
(setf (deref (context-register-addr context index))
      new))

;;; This is like CONTEXT-REGISTER, but returns the value of a float
;;; register. FORMAT is the type of float to return.

;;; FIXME: Whether COERCE actually knows how to make a float out of a
;;; long is another question. This stuff still needs testing.
#+nil
(define-alien-routine ("os_context_float_register_addr" context-float-register-addr)
  (* long)
  (context (* os-context-t))
  (index int))
#+nil
(defun context-float-register (context index format)
  (declare (type (alien (* os-context-t)) context))
  (coerce (deref (context-float-register-addr context index)) format))
#+nil
(defun %set-context-float-register (context index format new)
  (declare (type (alien (* os-context-t)) context))
  (setf (deref (context-float-register-addr context index))
        (coerce new format)))

;;; Given a signal context, return the floating point modes word in
;;; the same format as returned by FLOATING-POINT-MODES.

;;; Under SunOS, we have a straightforward implementation in C:
#!+sunos
(define-alien-routine ("os_context_fp_control" context-floating-point-modes)
    (unsigned 32)
  (context (* os-context-t)))

;;; Under Linux, we have to contend with utterly broken signal handling.
#!+linux
(defun context-floating-point-modes (context)
  (declare (ignore context))
  (warn "stub CONTEXT-FLOATING-POINT-MODES")
  0)

;;;; INTERNAL-ERROR-ARGS.

;;; Given a (POSIX) signal context, extract the internal error
;;; arguments from the instruction stream.  This is e.g.
;;; 4       23      254     240     2       0       0       0
;;; |       ~~~~~~~~~~~~~~~~~~~~~~~~~
;;; length         data              (everything is an octet)
;;;  (pc)
(defun internal-error-args (context)
  (declare (type (alien (* os-context-t)) context))
  (/show0 "entering INTERNAL-ERROR-ARGS")
  (let* ((pc (context-pc context))
         (error-number (sap-ref-8 pc 4)))
    (declare (type system-area-pointer pc))
    (values error-number
            (sb!kernel::decode-internal-error-args (sap+ pc 5) error-number))))

