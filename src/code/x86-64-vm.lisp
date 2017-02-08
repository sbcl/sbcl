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
  "Return a string describing the type of the local machine."
  "X86-64")

;;;; :CODE-OBJECT fixups

#!+immobile-space
(defun sb!kernel::immobile-space-obj-p (obj)
  (<= immobile-space-start (get-lisp-obj-address obj) immobile-space-end))

;;; This gets called by LOAD to resolve newly positioned objects
;;; with things (like code instructions) that have to refer to them.
(defun fixup-code-object (code offset fixup kind &optional flavor)
  (declare (type index offset) (ignorable flavor))
  (without-gcing
    (let ((sap (code-instructions code)))
      (ecase kind
        (:absolute64
         ;; Word at sap + offset contains a value to be replaced by
         ;; adding that value to fixup.
         (setf (sap-ref-64 sap offset) (+ fixup (sap-ref-64 sap offset))))
        (:absolute
         ;; Word at sap + offset contains a value to be replaced by
         ;; adding that value to fixup.
         (setf (sap-ref-32 sap offset) (+ fixup (signed-sap-ref-32 sap offset))))
        (:relative
         ;; Fixup is the actual address wanted.
         ;; Replace word with value to add to that loc to get there.
         ;; In the #!-immobile-code case, there's nothing to assert.
         ;; Relative fixups pretty much can't happen.
         #!+immobile-code
         (unless (<= immobile-space-start (get-lisp-obj-address code) immobile-space-end)
           (error "Can't compute fixup relative to movable object ~S" code))
         (setf (signed-sap-ref-32 sap offset)
               (etypecase fixup
                 (integer
                  ;; JMP/CALL are relative to the next instruction,
                  ;; so add 4 bytes for the size of the displacement itself.
                  (- fixup
                     (the (unsigned-byte 64) (+ (sap-int sap) offset 4))))))))))
  ;; An absolute fixup is stored in the code header if it
  ;; references an immobile-space (but not static-space) object.
  ;; This needn't be inside WITHOUT-GCING, because code fixups will point
  ;; only to objects that don't move except during save-lisp-and-die.
  ;; So there is no race with GC here.
  ;; Note that:
  ;;  (1) :NAMED-CALL occurs in both :RELATIVE and :ABSOLUTE kinds.
  ;;      We can ignore the :RELATIVE kind.
  ;;  (2) genesis does not need the :NAMED-CALL case,
  ;;      because COMPILE-FILE always places code in immobile space,
  ;;      and self-build does not use step instrumenting, so there are
  ;;      no named calls using the sequence "MOV RAX, imm32 ; CALL RAX"
  ;;      Hence SB!FASL::DO-COLD-FIXUP does not exactly mimic this logic.
  #!+immobile-space
  (when (and (eq kind :absolute) (member flavor '(:named-call :immobile-object)))
    (let ((fixups (%code-fixups code)))
      ;; Sanctifying the code component will compact these into a bignum.
      (setf (%code-fixups code) (cons offset (if (eql fixups 0) nil fixups)))))
  nil)

#!+immobile-space
(defun sanctify-for-execution (code)
  (let ((fixups (%code-fixups code)))
    (when (listp fixups)
      (setf (%code-fixups code) (sb!c::pack-code-fixup-locs fixups))))
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

(declaim (inline set-context-pc))
(defun incf-context-pc (context offset)
  (declare (type (alien (* os-context-t)) context))
  (let ((addr (context-pc-addr context)))
    (declare (type (alien (* unsigned)) addr))
    (setf (deref addr) (+ (deref addr) offset))))

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

#!+(or darwin linux win32)
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
  #!-(or darwin linux win32)
  (progn
    (warn "stub CONTEXT-FLOAT-REGISTER")
    (coerce 0 format))
  #!+(or darwin linux win32)
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
         (trap-number (sap-ref-8 pc 0)))
    (declare (type system-area-pointer pc))
    (/show0 "got PC")
    ;; using INT3 the pc is .. INT3 <here> code length bytes...
    (if (= trap-number invalid-arg-count-trap)
        (values #.(error-number-or-lose 'invalid-arg-count-error)
                '(#.arg-count-sc))
        (let ((error-number (sap-ref-8 pc 1)))
          (values error-number
                  (sb!kernel::decode-internal-error-args (sap+ pc 2) error-number)
                  trap-number)))))


;;; the current alien stack pointer; saved/restored for non-local exits
(defvar *alien-stack-pointer*)

(defun fun-immobilize (fun)
  (let ((code (allocate-code-object t 0 16)))
    (setf (%code-debug-info code) fun)
    (let ((sap (code-instructions code))
          (ea (+ (logandc2 (get-lisp-obj-address code) lowtag-mask)
                 (ash code-debug-info-slot word-shift))))
      ;; For a funcallable-instance, the instruction sequence is:
      ;;    MOV RAX, [RIP-n] ; load the function
      ;;    MOV RAX, [RAX+5] ; load the funcallable-instance-fun
      ;;    JMP [RAX-3]
      ;; Otherwise just instructions 1 and 3 will do.
      ;; We could use the #xA1 opcode to save a byte, but that would
      ;; be another headache do deal with when relocating this code.
      (setf (sap-ref-32 sap 0) #x058B48 ; REX MOV [RIP-n]
            (signed-sap-ref-32 sap 3) (- ea (+ (sap-int sap) 7))) ; disp
      (let ((i (if (/= (fun-subtype fun) funcallable-instance-header-widetag)
                   7
                   (let ((disp8 (- (ash funcallable-instance-function-slot
                                        word-shift)
                                   fun-pointer-lowtag))) ; = 5
                     (setf (sap-ref-32 sap 7) (logior (ash disp8 24) #x408B48))
                     11))))
        (setf (sap-ref-32 sap i) #xFD60FF))) ; JMP [RAX-3]
    code))

(defun %set-fdefn-fun (fdefn fun)
  (declare (type fdefn fdefn) (type function fun)
           (values function))
  (let ((trampoline (unless (and (simple-fun-p fun)
                                 (< (get-lisp-obj-address fun) (ash 1 32)))
                      (fun-immobilize fun)))) ; a newly made CODE object
    (with-pinned-objects (fdefn trampoline fun)
      (binding* (((fun-entry-addr nop-byte)
                  (if trampoline
                      (values (sap-int (code-instructions trampoline)) #x90)
                      (values (+ (get-lisp-obj-address fun)
                                 (- fun-pointer-lowtag)
                                 (ash simple-fun-code-offset word-shift)) 0)))
                 (fdefn-addr (- (get-lisp-obj-address fdefn) ; base of the object
                                other-pointer-lowtag))
                 (fdefn-entry-addr (+ fdefn-addr ; address that callers jump to
                                      (ash fdefn-raw-addr-slot word-shift)))
                 (displacement (the (signed-byte 32)
                                 (- fun-entry-addr (+ fdefn-entry-addr 5)))))
        (setf (sap-ref-word (int-sap fdefn-entry-addr) 0)
              (logior #xE9
                      ;; Allow negative displacement
                      (ash (ldb (byte 32 0) displacement) 8) ; JMP opcode
                      (ash nop-byte 40))
              (sap-ref-lispobj (int-sap fdefn-addr) (ash fdefn-fun-slot word-shift))
              fun)))))

;;; Find an immobile FDEFN or FUNCTION given an interior pointer to it.
#!+immobile-space
(defun find-called-object (address)
  (when (<= immobile-space-start address immobile-space-end)
    (let ((obj (alien-funcall (extern-alien "search_immobile_space" (function long long))
                              address)))
      (unless (eql obj 0)
        (case (sap-ref-8 (int-sap obj) 0)
         (#.fdefn-widetag
          (make-lisp-obj (logior obj other-pointer-lowtag)))
         (#.code-header-widetag
          (let ((code (make-lisp-obj (logior obj other-pointer-lowtag))))
            (dotimes (i (code-n-entries code))
              (let ((f (%code-entry-point code i)))
                (if (= (+ (get-lisp-obj-address f)
                          (ash simple-fun-code-offset word-shift)
                          (- fun-pointer-lowtag))
                       address)
                    (return f)))))))))))
