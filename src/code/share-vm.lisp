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

(defmacro with-pinned-context-code-object
      ((&optional (context '*current-internal-error-context*))
       &body body)
    (declare (ignorable context))
    #!+(or x86 x86-64)
    `(progn ,@body)
    #!-(or x86 x86-64)
    `(with-pinned-objects ((without-gcing
                             (sb!di::code-object-from-context ,context)))
       ,@body))

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

;; KLUDGE: Linux/MIPS signal context fields are 64 bits wide, even on
;; 32-bit systems.  On big-endian implementations, simply using a
;; pointer to a narrower type DOES NOT WORK because it reads the upper
;; half of the value, not the lower half.  On such systems, we must
;; either read the entire value (and mask down, in case the system
;; sign-extends the value, which has been seen to happen), or offset
;; the pointer in order to read the lower half.  This has been broken
;; at least twice in the past.  MIPS also appears to be the ONLY
;; system for which the signal context field size may differ from
;; n-word-bits (well, and ALPHA, but that's a separate matter), but
;; this entire thing will likely need to be revisited when we add x32
;; or n32 ABI support.
(defconstant kludge-big-endian-short-pointer-offset
  (+ 0
     #!+(and mips big-endian (not 64-bit)) 1))

(declaim (inline context-pc-addr))
(define-alien-routine ("os_context_pc_addr" context-pc-addr) (* unsigned)
  (context (* os-context-t)))

(declaim (inline context-pc))
(defun context-pc (context)
  (declare (type (alien (* os-context-t)) context))
  (let ((addr (context-pc-addr context)))
    (declare (type (alien (* unsigned)) addr))
    (int-sap (deref addr kludge-big-endian-short-pointer-offset))))

(declaim (inline incf-context-pc))
(defun incf-context-pc (context offset)
  (declare (type (alien (* os-context-t)) context))
  (with-pinned-context-code-object (context)
    (let ((addr (context-pc-addr context)))
      (declare (type (alien (* unsigned)) addr))
      (setf (deref addr kludge-big-endian-short-pointer-offset)
            (+ (deref addr kludge-big-endian-short-pointer-offset) offset)))))

(declaim (inline set-context-pc))
(defun set-context-pc (context new)
  (declare (type (alien (* os-context-t)) context))
  (with-pinned-context-code-object (context)
    (let ((addr (context-pc-addr context)))
      (declare (type (alien (* unsigned)) addr))
      (setf (deref addr kludge-big-endian-short-pointer-offset) new))))

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
    (deref addr kludge-big-endian-short-pointer-offset)))

(declaim (inline %set-context-register))
(defun %set-context-register (context index new)
  (declare (type (alien (* os-context-t)) context))
  (let ((addr (context-register-addr context index)))
    (declare (type (alien (* unsigned)) addr))
    (setf (deref addr kludge-big-endian-short-pointer-offset) new)))

;;; A note about the #!+arm special case below -
;;; Revision 678177b100bcf6aa caused new behavior in the register allocator.
;;; [Its author said "for backends that store registers in SCs in an order
;;; different from the one defined by offsets, (iterative) register allocation
;;; may yield different results after that commit."]
;;; Well, somehow we end up putting random bits in a boxed register.
;;; If this occurs in the foreign call sequence, and the foreign function
;;; happens to be undefined, then we hit UNDEFINED-ALIEN-TRAMP, which transfers
;;; control to SB-KERNEL:INTERNAL-ERROR which tries to deduce the code object
;;; containing the errant PC. It does that by calling CODE-OBJECT-FROM-CONTEXT
;;; which seems to think that it has the right to read *every* boxed register
;;; to see if the PC partakes of the interior pointer relative to that register.
;;; Problem is, if the boxed register contains junk, then SAP-REF-LISPOBJ
;;; returns junk, and things just go downhill from there, leading to
;;; infinite recursion and "CORRUPTION WARNING".
;;; So we must carefully pre-test the supplied register as a raw word
;;; to see if the low bits do not indicate that it is a pointer.
(declaim (inline boxed-context-register))
(defun boxed-context-register (context index)
  (declare (type (alien (* os-context-t)) context))
  (let ((addr (context-register-addr context index)))
    (declare (type (alien (* unsigned)) addr))
    ;; No LISPOBJ alien type, so grab the SAP and use SAP-REF-LISPOBJ.
    (macrolet ((ref-it ()
                 `(sap-ref-lispobj
                   (alien-sap addr)
                   (* kludge-big-endian-short-pointer-offset n-word-bytes))))
      #!+arm
      (if (= 2 (logand (sap-ref-word (alien-sap addr)
                                     (* kludge-big-endian-short-pointer-offset
                                        n-word-bytes))
                       #b11))
          0 ; anything we would return is illegal. So just fake it
          (ref-it))
      #!-arm
      (ref-it))))

(declaim (inline %set-boxed-context-register))
(defun %set-boxed-context-register (context index new)
  (declare (type (alien (* os-context-t)) context))
  (let ((addr (context-register-addr context index)))
    (declare (type (alien (* unsigned)) addr))
    ;; No LISPOBJ alien type, so grab the SAP and use SAP-REF-LISPOBJ.
    (setf (sap-ref-lispobj (alien-sap addr)
                           (* kludge-big-endian-short-pointer-offset
                              n-word-bytes))
          new)))
