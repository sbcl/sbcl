;;;; X86-specific runtime stuff

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
  "X86")

;;;; :CODE-OBJECT fixups

;;; a counter to measure the storage overhead of these fixups
(defvar *num-fixups* 0)
;;; FIXME: When the system runs, it'd be interesting to see what this is.

(declaim (inline adjust-fixup-array))
(defun adjust-fixup-array (array size)
  (let ((new (make-array size :element-type '(unsigned-byte 32))))
    (replace new array)
    new))

;;; This gets called by LOAD to resolve newly positioned objects
;;; with things (like code instructions) that have to refer to them.
;;;
;;; Add a fixup offset to the vector of fixup offsets for the given
;;; code object.
(defun fixup-code-object (code offset fixup kind)
  (declare (type index offset))
  (flet ((add-fixup (code offset)
           ;; (We check for and ignore fixups for code objects in the
           ;; read-only and static spaces. (In the old CMU CL code
           ;; this check was conditional on *ENABLE-DYNAMIC-SPACE-CODE*,
           ;; but in SBCL relocatable dynamic space code is always in
           ;; use, so we always do the check.)
           (incf *num-fixups*)
           (let ((fixups (code-header-ref code code-constants-offset)))
             (cond ((typep fixups '(simple-array (unsigned-byte 32) (*)))
                    (let ((new-fixups
                            (adjust-fixup-array fixups (1+ (length fixups)))))
                      (setf (aref new-fixups (length fixups)) offset)
                      (setf (code-header-ref code code-constants-offset)
                            new-fixups)))
                   (t
                    (unless (or (eq (widetag-of fixups)
                                    unbound-marker-widetag)
                                (zerop fixups))
                      (format t "** Init. code FU = ~S~%" fixups)) ; FIXME
                    (setf (code-header-ref code code-constants-offset)
                          (make-array
                           1
                           :element-type '(unsigned-byte 32)
                           :initial-element offset)))))))
    (without-gcing
      (let* ((sap (truly-the system-area-pointer
                             (code-instructions code)))
             (obj-start-addr (logand (get-lisp-obj-address code)
                                     #xfffffff8))
             ;; FIXME: what is this 5?
             #+nil (const-start-addr (+ obj-start-addr (* 5 n-word-bytes)))
             (code-start-addr (sap-int (code-instructions code)))
             (ncode-words (code-header-ref code 1))
             (code-end-addr (+ code-start-addr (* ncode-words n-word-bytes))))
        (unless (member kind '(:absolute :relative))
          (error "Unknown code-object-fixup kind ~S." kind))
        (ecase kind
          (:absolute
           ;; Word at sap + offset contains a value to be replaced by
           ;; adding that value to fixup.
           (setf (sap-ref-32 sap offset) (+ fixup (sap-ref-32 sap offset)))
           ;; Record absolute fixups that point within the code object.
           (when (> code-end-addr (sap-ref-32 sap offset) obj-start-addr)
             (add-fixup code offset)))
          (:relative
           ;; Fixup is the actual address wanted.
           ;;
           ;; Record relative fixups that point outside the code
           ;; object.
           (when (or (< fixup obj-start-addr) (> fixup code-end-addr))
             (add-fixup code offset))
           ;; Replace word with value to add to that loc to get there.
           (let* ((loc-sap (+ (sap-int sap) offset))
                  ;; Use modular arithmetic so that if the offset
                  ;; doesn't fit into signed-byte-32 it'll wrap around
                  ;; when added to EIP
                  (rel-val (ldb (byte 32 0)
                                (- fixup loc-sap n-word-bytes))))
             (declare (type (unsigned-byte 32) loc-sap rel-val))
             (setf (sap-ref-32 sap offset) rel-val))))))
    nil))

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
(define-alien-routine ("os_context_pc_addr" context-pc-addr) (* unsigned-int)
  ;; (Note: Just as in CONTEXT-REGISTER-ADDR, we intentionally use an
  ;; 'unsigned *' interpretation for the 32-bit word passed to us by
  ;; the C code, even though the C code may think it's an 'int *'.)
  (context (* os-context-t)))

(declaim (inline context-pc))
(defun context-pc (context)
  (declare (type (alien (* os-context-t)) context))
  (let ((addr (context-pc-addr context)))
    (declare (type (alien (* unsigned-int)) addr))
    (int-sap (deref addr))))

(declaim (inline context-register-addr))
(define-alien-routine ("os_context_register_addr" context-register-addr)
  (* unsigned-int)
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
    (declare (type (alien (* unsigned-int)) addr))
    (deref addr)))

(defun %set-context-register (context index new)
  (declare (type (alien (* os-context-t)) context))
  (let ((addr (context-register-addr context index)))
    (declare (type (alien (* unsigned-int)) addr))
    (setf (deref addr) new)))

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
#!-(or linux sunos)
(defun context-floating-point-modes (context)
  ;; FIXME: As of sbcl-0.6.7 and the big rewrite of signal handling for
  ;; POSIXness and (at the Lisp level) opaque signal contexts,
  ;; this is stubified. It needs to be rewritten as an
  ;; alien function.
  (declare (ignore context)) ; stub!
  (warn "stub CONTEXT-FLOATING-POINT-MODES")
  0)

#!+(or linux sunos)
(define-alien-routine ("os_context_fp_control" context-floating-point-modes)
    (sb!alien:unsigned 32)
  (context (* os-context-t)))

;;;; INTERNAL-ERROR-ARGS

;;; Given a (POSIX) signal context, extract the internal error
;;; arguments from the instruction stream.
(defun internal-error-args (context)
  (declare (type (alien (* os-context-t)) context))
  (/show0 "entering INTERNAL-ERROR-ARGS, CONTEXT=..")
  (/hexstr context)
  (let ((pc (context-pc context)))
    (declare (type system-area-pointer pc))
    (/show0 "got PC")
    ;; using INT3 the pc is .. INT3 <here> code length bytes...
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
          (values error-number (sc-offsets)))))))

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

;;; the current alien stack pointer; saved/restored for non-local exits
(defvar *alien-stack-pointer*)

;;; Support for the MT19937 random number generator. The update
;;; function is implemented as an assembly routine. This definition is
;;; transformed to a call to the assembly routine allowing its use in
;;; interpreted code.
(defun random-mt19937 (state)
  (declare (type (simple-array (unsigned-byte 32) (627)) state))
  (random-mt19937 state))
