;;;; X86-64-specific runtime stuff

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
  "X86-64")

#+(or darwin linux openbsd win32 sunos (and freebsd x86-64))
(define-alien-routine ("os_context_float_register_addr" context-float-register-addr)
  (* unsigned) (context (* os-context-t)) (index int))

;;; This is like CONTEXT-REGISTER, but returns the value of a float
;;; register. FORMAT is the type of float to return.

(defun context-float-register (context index format)
  (declare (ignorable context index))
  #-(or darwin linux openbsd win32 sunos (and freebsd x86-64))
  (progn
    (warn "stub CONTEXT-FLOAT-REGISTER")
    (coerce 0 format))
  #+(or darwin linux openbsd win32 sunos (and freebsd x86-64))
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
                (sap-ref-double sap 8)))
      #+sb-simd-pack
      (simd-pack-int
       (%make-simd-pack-ub64
        (sap-ref-64 sap 0)
        (sap-ref-64 sap 8)))
      #+sb-simd-pack
      (simd-pack-single
       (%make-simd-pack-single
        (sap-ref-single sap 0)
        (sap-ref-single sap 4)
        (sap-ref-single sap 8)
        (sap-ref-single sap 12)))
      #+sb-simd-pack
      (simd-pack-double
       (%make-simd-pack-double
        (sap-ref-double sap 0)
        (sap-ref-double sap 8)))
      #+sb-simd-pack-256
      (simd-pack-256-int
       (%make-simd-pack-256-ub64
        (sap-ref-64 sap 0)
        (sap-ref-64 sap 8)
        (sap-ref-64 sap 16)
        (sap-ref-64 sap 24)))
      #+sb-simd-pack-256
      (simd-pack-256-single
       (%make-simd-pack-256-single
        (sap-ref-single sap 0)
        (sap-ref-single sap 4)
        (sap-ref-single sap 8)
        (sap-ref-single sap 12)
        (sap-ref-single sap 16)
        (sap-ref-single sap 20)
        (sap-ref-single sap 24)
        (sap-ref-single sap 28)))
      #+sb-simd-pack-256
      (simd-pack-256-double
       (%make-simd-pack-256-double
        (sap-ref-double sap 0)
        (sap-ref-double sap 8)
        (sap-ref-double sap 16)
        (sap-ref-double sap 24))))))

(defun %set-context-float-register (context index format value)
  (declare (ignorable context index format))
  #-(or linux win32)
  (progn
    (warn "stub %SET-CONTEXT-FLOAT-REGISTER")
    value)
  #+(or linux win32)
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
               (sap-ref-double sap 8) (imagpart value))))
      #+sb-simd-pack
      (simd-pack-int
       (multiple-value-bind (a b) (%simd-pack-ub64s value)
         (setf (sap-ref-64 sap 0) a
               (sap-ref-64 sap 8) b)))
      #+sb-simd-pack
      (simd-pack-single
       (multiple-value-bind (a b c d) (%simd-pack-singles value)
         (setf (sap-ref-single sap 0) a
               (sap-ref-single sap 4) b
               (sap-ref-single sap 8) c
               (sap-ref-single sap 12) d)))
      #+sb-simd-pack
      (simd-pack-double
       (multiple-value-bind (a b) (%simd-pack-doubles value)
         (setf (sap-ref-double sap 0) a
               (sap-ref-double sap 8) b)))
      #+sb-simd-pack-256
      (simd-pack-256-int
       (multiple-value-bind (a b c d) (%simd-pack-256-ub64s value)
         (setf (sap-ref-64 sap 0) a
               (sap-ref-64 sap 8) b
               (sap-ref-64 sap 16) c
               (sap-ref-64 sap 24) d)))
      #+sb-simd-pack-256
      (simd-pack-256-single
       (multiple-value-bind (a b c d e f g h) (%simd-pack-256-singles value)
         (setf (sap-ref-single sap 0) a
               (sap-ref-single sap 4) b
               (sap-ref-single sap 8) c
               (sap-ref-single sap 12) d
               (sap-ref-single sap 16) e
               (sap-ref-single sap 20) f
               (sap-ref-single sap 24) g
               (sap-ref-single sap 28) h)))
      #+sb-simd-pack-256
      (simd-pack-256-double
       (multiple-value-bind (a b c d) (%simd-pack-256-doubles value)
         (setf (sap-ref-double sap 0) a
               (sap-ref-double sap 8) b
               (sap-ref-double sap 16) c
               (sap-ref-double sap 24) d))))))

;;; Given a signal context, return the floating point modes word in
;;; the same format as returned by FLOATING-POINT-MODES.
#-linux
(defun context-floating-point-modes (context)
  (declare (ignore context)) ; stub!
  (warn "stub CONTEXT-FLOATING-POINT-MODES")
  0)
#+linux
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
  (let* ((pc (context-pc context))
         (trap-number (sap-ref-8 pc 0)))
    (declare (type system-area-pointer pc))
    (cond ((= trap-number invalid-arg-count-trap)
           (values #.(error-number-or-lose 'invalid-arg-count-error)
                   '(#.arg-count-sc)))
          #+linux
          ((= trap-number uninitialized-load-trap)
           (values #.(error-number-or-lose 'uninitialized-memory-error)
                   (locally
                       (declare (optimize (safety 0)))
                     (let* ((data (sap-ref-8 pc 1)) ; encodes dst register and size
                            (value (sb-vm:context-register context (ash data -2)))
                            (nbytes (ash 1 (logand data #b11)))
                            ;; EMIT-SAP-REF wires the EA to a predetermined register,
                            ;; which now points to the shadow space, not the user memory.
                            (ea (logxor (sb-vm:context-register context msan-temp-reg-number)
                                        msan-mem-to-shadow-xor-const)))
                       `(:raw ,ea ,nbytes ,value)))))
          (t
           (sb-kernel::decode-internal-error-args (sap+ pc 1) trap-number)))))


(defun write-funinstance-prologue (fin)
  ;; Encode: MOV RAX,[RIP+9] / JMP [RAX-3] / NOP / MOV EBX, #x0
  ;; and the #x0 is replaced with a hash code.
  (with-pinned-objects (fin)
    (let* ((sap (sap+ (int-sap (get-lisp-obj-address fin))
                      (- (ash 2 word-shift) fun-pointer-lowtag))))
      ;; Scavenging these words when you shouldn't is actually harmless
      ;; because by a stroke of luck, they all look fixnum-tagged.
      (setf (sap-ref-sap sap -8) sap
            (sap-ref-word sap 0) #xFF00000009058B48
            (sap-ref-word sap 8) #x00000000BB90FD60)))
  (update-dynamic-space-code-tree fin)
  fin)

(sb-kernel:!defstruct-with-alternate-metaclass closure-trampoline
  :slot-names ()
  :constructor %alloc-closure-trampoline
  :superclass-name function
  :metaclass-name static-classoid
  :metaclass-constructor make-static-classoid
  :dd-type funcallable-structure)

(defmethod print-object ((self closure-trampoline) stream)
  (print-unreadable-object (self stream :identity t)
    (let ((payload (%primitive slot self 'function
                               funcallable-instance-function-slot fun-pointer-lowtag)))
      (write-string (if (functionp payload) "Tramp " "Undefined-fun ") stream)
      (prin1 payload stream))))

(defun ensure-simplistic (function name)
  (when (and (functionp function) (not (closurep function)))
    (return-from ensure-simplistic function))
  (let ((tramp (%alloc-closure-trampoline)))
    (with-pinned-objects (tramp)
      (if (or (eql function 0) (null function))
          (let* ((asm-code (sb-fasl::get-asm-routine 'undefined-tramp))
                 (base (sap+ (int-sap (get-lisp-obj-address tramp)) (- fun-pointer-lowtag)))
                 (sap (sap+ base 23)))
            (setf (sap-ref-16 sap 1) #x2524
                  (sap-ref-32 sap 3) (asm-routine-indirect-address asm-code)
                  ;; The undefined function name is stored in the "function" slot.
                  ;; The slot setter doesn't like this of course.
                  (sap-ref-lispobj base (ash funcallable-instance-function-slot word-shift))
                  name))
          (setf (%funcallable-instance-fun tramp) function)))
    tramp))

(defun stepper-fun (closure) (ensure-simplistic closure nil))

;;; Find an immobile FUNCTION given an interior pointer to it.
#+immobile-space
(defun find-called-object (address)
  (let ((obj (alien-funcall (extern-alien "search_all_gc_spaces"
                                          (function unsigned unsigned))
                            address)))
    (unless (eql obj 0)
      (case (sap-ref-8 (int-sap obj) 0)
        (#.code-header-widetag
         (%simple-fun-from-entrypoint
          (make-lisp-obj (logior obj other-pointer-lowtag))
          address))
        (#.funcallable-instance-widetag ; FIXME: do we use this case?
         (make-lisp-obj (logior obj fun-pointer-lowtag)))))))

;;; Undo the effects of XEP-ALLOCATE-FRAME
;;; and point PC to FUNCTION
(defun context-call-function (context function &optional arg-count)
  (with-pinned-objects (function)
    (let ((rsp (decf (context-register context rsp-offset) n-word-bytes))
          (rbp (context-register context rbp-offset))
          (fun-addr (get-lisp-obj-address function)))
      (setf (sap-ref-word (int-sap rsp) 0)
            (sap-ref-word (int-sap rbp) 8))
      (when arg-count
        (setf (context-register context rcx-offset)
              (get-lisp-obj-address arg-count)))
      (setf (context-register context rax-offset) fun-addr)
      (set-context-pc context (sap-ref-word (int-sap fun-addr)
                                            (- (ash simple-fun-self-slot word-shift)
                                               fun-pointer-lowtag))))))

(defun validate-asm-routine-vector ()
  ;; If the jump table in static space does not match the jump table
  ;; in *assembler-routines*, fix the one one in static space.
  ;; It's OK that this is delayed until startup, because code pertinent to
  ;; core restart always uses relative jumps to asm code.
  #+immobile-space
  (let* ((code sb-fasl:*assembler-routines*)
         (external-table (truly-the (simple-array word (*))
                                    sb-fasl::*asm-routine-vector*))
         (insts (code-instructions code))
         (n (sb-impl::hash-table-%count (sb-fasl::%asm-routine-table code))))
    (declare (optimize (insert-array-bounds-checks 0)))
    (dotimes (i n)
      (unless (= (aref external-table i) 0)
        (setf (aref external-table i)
              (sap-ref-word insts (truly-the index (ash (1+ i) word-shift))))))))

(defconstant cf-bit 0)
(defconstant sf-bit 7)
(defconstant of-bit 11)

(defun context-overflow-carry-flags (context)
  (let ((flags (context-flags context)))
    (values (logbitp of-bit flags)
            (logbitp cf-bit flags))))

(def-cpu-feature :avx2
    (plusp (sb-alien:extern-alien "avx2_supported" int)))

(def-cpu-feature :ssse3+popcnt
    (when (>= (sb-vm::%cpu-identification 0 0) 1)
      (multiple-value-bind (eax ebx ecx) (sb-vm::%cpu-identification 1 0)
        (declare (ignore eax ebx))
        (= (logand #1=(logior (ash 1 9)   ;; ssse3
                              (ash 1 23)) ;; popcnt
                   ecx)
           #1#))))
