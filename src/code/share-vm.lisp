;;;; arch-independent runtime stuff
;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

(defvar *current-internal-error-context*)

;;;; OS-CONTEXT-T

;;; a POSIX signal context, i.e. the type passed as the third
;;; argument to an SA_SIGACTION-style signal handler
;;;
;;; The real type does have slots, but at Lisp level, we never
;;; access them, or care about the size of the object. Instead, we
;;; always refer to these objects by pointers handed to us by the C
;;; runtime library, and ask the runtime library any time we need
;;; information about the contents of one of these objects. Thus, it
;;; works to represent this as an object with no slots.
;;;
;;; KLUDGE: It would be nice to have a type definition analogous to
;;; C's "struct os_context_t;", for an incompletely specified object
;;; which can only be referred to by reference, but I don't know how
;;; to do that in the FFI, so instead we just this bogus no-slots
;;; representation. -- WHN 20000730
;;;
;;; FIXME: Since SBCL, unlike CMU CL, uses this as an opaque type,
;;; it's no longer architecture-dependent, and probably belongs in
;;; some other package, perhaps SB-KERNEL.
(define-alien-type os-context-t (struct os-context-t-struct))

(declaim (inline context-pc-addr))
(define-alien-routine ("os_context_pc_addr" context-pc-addr) (* unsigned)
  (context (* os-context-t)))

(declaim (inline context-pc))
(defun context-pc (context)
  (declare (type (alien (* os-context-t)) context))
  (let ((addr (context-pc-addr context)))
    (declare (type (alien (* unsigned)) addr))
    (int-sap (deref addr))))

(declaim (inline incf-context-pc))
(defun incf-context-pc (context offset)
  (declare (type (alien (* os-context-t)) context))
  (let ((addr (context-pc-addr context)))
    (declare (type (alien (* unsigned)) addr))
    (setf (deref addr) (+ (deref addr) offset))))

(declaim (inline set-context-pc))
(defun set-context-pc (context new)
  (declare (type (alien (* os-context-t)) context))
  (let ((addr (context-pc-addr context)))
    (declare (type (alien (* unsigned)) addr))
    (setf (deref addr) new)))

(declaim (inline context-register-addr))
(define-alien-routine ("os_context_register_addr" context-register-addr)
    (* unsigned)
  (context (* os-context-t))
    (index int))

(declaim (inline context-register))
(defun context-register (context index)
  (declare (type (alien (* os-context-t)) context))
  (let ((addr (context-register-addr context index)))
    (declare (type (alien (* unsigned)) addr))
    (deref addr)))

(defun %set-context-register (context index new)
  (declare (type (alien (* os-context-t)) context))
  (let ((addr (context-register-addr context index)))
    (declare (type (alien (* unsigned)) addr))
        (setf (deref addr) new)))
