;;;; X86-specific runtime stuff

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(defun machine-type ()
  "Return a string describing the type of the local machine."
  "X86")

;;;; :CODE-OBJECT fixups

;;; This gets called by LOAD to resolve newly positioned objects
;;; with things (like code instructions) that have to refer to them.
;;; Return T if and only if the fixup needs to be recorded in %CODE-FIXUPS
(defun fixup-code-object (code offset fixup kind flavor)
  (declare (type index offset))
  (declare (ignore flavor))
  (let* ((obj-start-addr (logandc2 (get-lisp-obj-address code) sb-vm:lowtag-mask))
         (sap (code-instructions code))
         (code-end-addr (+ (sap-int sap) (%code-code-size code))))
    (ecase kind
      (:absolute
       ;; Word at sap + offset contains a value to be replaced by
       ;; adding that value to fixup.
       (setf (sap-ref-32 sap offset) (+ fixup (sap-ref-32 sap offset)))
       ;; Record absolute fixups that point into CODE. An absolute fixup
       ;; can't point to another dynamic-space object, but it could point
       ;; to read-only or static space. Those don't need to be saved.
       (< obj-start-addr (sap-ref-32 sap offset) code-end-addr))
      (:relative
       ;; Fixup is the actual address wanted.
       ;; Replace word with value to add to that loc to get there.
       (let* ((loc-sap (+ (sap-int sap) offset))
              ;; Use modular arithmetic so that if the offset
              ;; doesn't fit into signed-byte-32 it'll wrap around
              ;; when added to EIP
              (rel-val (ldb (byte 32 0) (- fixup loc-sap n-word-bytes))))
         (declare (type (unsigned-byte 32) loc-sap rel-val))
         (setf (sap-ref-32 sap offset) rel-val))
       ;; Relative fixups point outside of this object. Keep them all.
       (aver (or (< fixup obj-start-addr) (> fixup code-end-addr)))
       t))))

;;;; low-level signal context access functions
;;;;
;;;; Note: In CMU CL, similar functions were hardwired to access
;;;; BSD-style sigcontext structures defined as alien objects. Our
;;;; approach is different in two ways:
;;;;   1. We use POSIX SA_SIGACTION-style signals, so our context is
;;;;      whatever the void pointer in the sigaction handler dereferences
;;;;      to, not necessarily a sigcontext.
;;;;   2. We don't try to maintain alien definitions of the context
;;;;      structure at Lisp level, but instead call alien C functions
;;;;      which take care of access for us. (Since the C functions can
;;;;      be defined in terms of system standard header files, they
;;;;      should be easier to maintain; and since Lisp code uses signal
;;;;      contexts only in interactive or exception code (like the debugger
;;;;      and internal error handling) the extra runtime cost should be
;;;;      negligible.

#+(or linux win32)
(define-alien-routine ("os_context_float_register_addr" context-float-register-addr)
  (* unsigned) (context (* os-context-t)) (index int))

(defun context-float-register (context index format)
  (declare (ignorable context index))
  #-(or linux win32)
  (progn
    (warn "stub CONTEXT-FLOAT-REGISTER")
    (coerce 0 format))
  #+(or linux win32)
  (let ((sap (alien-sap (context-float-register-addr context index))))
    (ecase format
      (single-float
       (coerce (sap-ref-long sap 0) 'single-float))
      (double-float
       (sap-ref-long sap 0))
      (complex-single-float
       (complex (coerce (sap-ref-long sap 0) 'single-float)
                (coerce (sap-ref-long sap 10) 'single-float)))
      (complex-double-float
       (complex (sap-ref-long sap 0)
                (sap-ref-long sap 10))))))

(defun %set-context-float-register (context index format new-value)
  (declare (ignore context index))
  (warn "stub %SET-CONTEXT-FLOAT-REGISTER")
  (coerce new-value format))

;;; Given a signal context, return the floating point modes word in
;;; the same format as returned by FLOATING-POINT-MODES.
#-(or linux sunos)
(defun context-floating-point-modes (context)
  ;; FIXME: As of sbcl-0.6.7 and the big rewrite of signal handling for
  ;; POSIXness and (at the Lisp level) opaque signal contexts,
  ;; this is stubified. It needs to be rewritten as an
  ;; alien function.
  (declare (ignore context)) ; stub!
  (warn "stub CONTEXT-FLOATING-POINT-MODES")
  0)

#+(or linux sunos)
(define-alien-routine ("os_context_fp_control" context-floating-point-modes)
    (sb-alien:unsigned 32)
  (context (* os-context-t)))

;;;; INTERNAL-ERROR-ARGS

;;; Given a (POSIX) signal context, extract the internal error
;;; arguments from the instruction stream.
(defun internal-error-args (context)
  (declare (type (alien (* os-context-t)) context))
  (let* ((pc (context-pc context))
         (trap-number (sap-ref-8 pc 0)))
    (declare (type system-area-pointer pc))
    (sb-kernel::decode-internal-error-args (sap+ pc 1) trap-number)))

;;; This is used in error.lisp to insure that floating-point exceptions
;;; are properly trapped. The compiler translates this to a VOP.
(defun float-wait ()
  (float-wait))

;;; float constants
;;;
;;; These are used by the FP MOVE-FROM-{SINGLE|DOUBLE} VOPs rather
;;; than the i387 load constant instructions to avoid consing in some
;;; cases. Note these are initialized by GENESIS as they are needed
;;; early.
(defvar *fp-constant-0f0*)
(defvar *fp-constant-1f0*)
(defvar *fp-constant-0d0*)
(defvar *fp-constant-1d0*)
;;; the long-float constants
(defvar *fp-constant-0l0*)
(defvar *fp-constant-1l0*)
(defvar *fp-constant-pi*)
(defvar *fp-constant-l2t*)
(defvar *fp-constant-l2e*)
(defvar *fp-constant-lg2*)
(defvar *fp-constant-ln2*)

;;; Support for the MT19937 random number generator. The update
;;; function is implemented as an assembly routine. This definition is
;;; transformed to a call to the assembly routine allowing its use in
;;; interpreted code.
(defun random-mt19937 (state)
  (declare (type (simple-array (unsigned-byte 32) (627)) state))
  (random-mt19937 state))
