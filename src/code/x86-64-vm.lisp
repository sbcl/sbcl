;;;; X86-64-specific runtime stuff

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

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

;;;; MACHINE-TYPE

(defun machine-type ()
  #!+sb-doc
  "Return a string describing the type of the local machine."
  "X86-64")

;;;; :CODE-OBJECT fixups

;;; This gets called by LOAD to resolve newly positioned objects
;;; with things (like code instructions) that have to refer to them.
(defun fixup-code-object (code offset fixup kind)
  (declare (type index offset))
  (without-gcing
    (let ((sap (truly-the system-area-pointer
                          (code-instructions code))))
      (unless (member kind '(:absolute :absolute64 :relative))
        (error "Unknown code-object-fixup kind ~S." kind))
      (ecase kind
        (:absolute64
         ;; Word at sap + offset contains a value to be replaced by
         ;; adding that value to fixup.
         (setf (sap-ref-64 sap offset) (+ fixup (sap-ref-64 sap offset))))
        (:absolute
         ;; Word at sap + offset contains a value to be replaced by
         ;; adding that value to fixup.
         (setf (sap-ref-32 sap offset) (+ fixup (sap-ref-32 sap offset))))
        (:relative
         ;; Fixup is the actual address wanted.
         ;; Replace word with value to add to that loc to get there.
         (let* ((loc-sap (+ (sap-int sap) offset))
                (rel-val (- fixup loc-sap (/ n-word-bytes 2))))
           (declare (type (unsigned-byte 64) loc-sap)
                    (type (signed-byte 32) rel-val))
           (setf (signed-sap-ref-32 sap offset) rel-val))))))
    nil)

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

(declaim (inline context-pc-addr))
(define-alien-routine ("os_context_pc_addr" context-pc-addr) (* unsigned)
  ;; (Note: Just as in CONTEXT-REGISTER-ADDR, we intentionally use an
  ;; 'unsigned *' interpretation for the 32-bit word passed to us by
  ;; the C code, even though the C code may think it's an 'int *'.)
  (context (* os-context-t)))

(declaim (inline context-pc))
(defun context-pc (context)
  (declare (type (alien (* os-context-t)) context))
  (let ((addr (context-pc-addr context)))
    (declare (type (alien (* unsigned)) addr))
    (int-sap (deref addr))))

(declaim (inline context-register-addr))
(define-alien-routine ("os_context_register_addr" context-register-addr)
  (* unsigned)
  ;; (Note the mismatch here between the 'int *' value that the C code
  ;; may think it's giving us and the 'unsigned *' value that we
  ;; receive. It's intentional: the C header files may think of
  ;; register values as signed, but the CMU CL code tends to think of
  ;; register values as unsigned, and might get bewildered if we ask
  ;; it to work with signed values.)
  (context (* os-context-t))
  (index int))

#!+(or linux win32)
(define-alien-routine ("os_context_float_register_addr" context-float-register-addr)
  (* unsigned) (context (* os-context-t)) (index int))

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

;;; This is like CONTEXT-REGISTER, but returns the value of a float
;;; register. FORMAT is the type of float to return.

(defun context-float-register (context index format)
  (declare (ignorable context index))
  #!-(or linux win32)
  (progn
    (warn "stub CONTEXT-FLOAT-REGISTER")
    (coerce 0 format))
  #!+(or linux win32)
  (let ((sap (alien-sap (context-float-register-addr context index))))
    (ecase format
      (single-float
       (sap-ref-single sap 0))
      (double-float
       (sap-ref-double sap 0))
      (complex-single-float
       (complex (sap-ref-single sap 0)
                (sap-ref-single sap 4)))
      (complex-double-float
       (complex (sap-ref-double sap 0)
                (sap-ref-double sap 8))))))

(defun %set-context-float-register (context index format value)
  (declare (ignorable context index format))
  #!-(or linux win32)
  (progn
    (warn "stub %SET-CONTEXT-FLOAT-REGISTER")
    value)
  #!+(or linux win32)
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

;;; Given a signal context, return the floating point modes word in
;;; the same format as returned by FLOATING-POINT-MODES.
#!-linux
(defun context-floating-point-modes (context)
  (declare (ignore context)) ; stub!
  (warn "stub CONTEXT-FLOATING-POINT-MODES")
  0)
#!+linux
(define-alien-routine ("os_context_fp_control" context-floating-point-modes)
    (unsigned 32)
  (context (* os-context-t)))

(define-alien-routine
    ("arch_get_fp_modes" floating-point-modes) (unsigned 32))

(define-alien-routine
    ("arch_set_fp_modes" %floating-point-modes-setter) void (fp (unsigned 32)))

(defun (setf floating-point-modes) (val) (%floating-point-modes-setter val))


;;;; INTERNAL-ERROR-ARGS

;;; Given a (POSIX) signal context, extract the internal error
;;; arguments from the instruction stream.
(defun internal-error-args (context)
  (declare (type (alien (* os-context-t)) context))
  (/show0 "entering INTERNAL-ERROR-ARGS, CONTEXT=..")
  (/hexstr context)
  (let* ((pc (context-pc context))
         (trap-number (sap-ref-8 pc 0) ))
    (declare (type system-area-pointer pc))
    (/show0 "got PC")
    ;; using INT3 the pc is .. INT3 <here> code length bytes...
    (if (= trap-number invalid-arg-count-trap)
        (values #.(error-number-or-lose 'invalid-arg-count-error)
                '(#.arg-count-sc))
        (let* ((length (sap-ref-8 pc 1))
               (vector (make-array length :element-type '(unsigned-byte 8))))
          (declare (type (unsigned-byte 8) length)
                   (type (simple-array (unsigned-byte 8) (*)) vector))
          (/show0 "LENGTH,VECTOR,ERROR-NUMBER=..")
          (/hexstr length)
          (/hexstr vector)
          (copy-ub8-from-system-area pc 2 vector 0 length)
          (let* ((index 0)
                 (error-number (sb!c:read-var-integer vector index)))
            (/hexstr error-number)
            (collect ((sc-offsets))
              (loop
               (/show0 "INDEX=..")
               (/hexstr index)
               (when (>= index length)
                 (return))
               (let ((sc-offset (sb!c:read-var-integer vector index)))
                 (/show0 "SC-OFFSET=..")
                 (/hexstr sc-offset)
                 (sc-offsets sc-offset)))
              (values error-number (sc-offsets))))))))


;;; the current alien stack pointer; saved/restored for non-local exits
(defvar *alien-stack-pointer*)
