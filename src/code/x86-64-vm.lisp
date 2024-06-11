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

#+immobile-space
(defun alloc-immobile-fdefn ()
  (alloc-immobile-fixedobj fdefn-size
                           (logior (ash undefined-fdefn-header 16)
                                   fdefn-widetag))) ; word 0

(defun fdefn-has-static-callers (fdefn)
  (declare (type fdefn fdefn))
  (with-pinned-objects (fdefn)
    (logbitp 7 (sap-ref-8 (int-sap (get-lisp-obj-address fdefn))
                          (- 1 other-pointer-lowtag)))))

(eval-when (:compile-toplevel)
(define-vop (set-fdefn-has-static-callers)
  (:args (fdefn :scs (descriptor-reg)))
  (:generator 1
    ;; atomic because the immobile gen# is in the same byte
    (inst or :lock :byte (ea (- 1 other-pointer-lowtag) fdefn) #x80)))
(define-vop (unset-fdefn-has-static-callers)
  (:args (fdefn :scs (descriptor-reg)))
  (:generator 1
    ;; atomic because the immobile gen# is in the same byte
    (inst and :lock :byte (ea (- 1 other-pointer-lowtag) fdefn) #x7f))))

(defun set-fdefn-has-static-callers (fdefn newval)
  (declare (type fdefn fdefn) (type bit newval))
  (if (= newval 0)
      (%primitive unset-fdefn-has-static-callers fdefn)
      (%primitive set-fdefn-has-static-callers fdefn))
  fdefn)

#+immobile-code
(progn
(sb-kernel:!defstruct-with-alternate-metaclass closure-trampoline
  :slot-names ()
  :constructor %alloc-closure-trampoline
  :superclass-name function
  :metaclass-name static-classoid
  :metaclass-constructor make-static-classoid
  :dd-type funcallable-structure)

(defun set-fdefn-fun (fun fdefn)
  (declare (type fdefn fdefn) (type function fun))
  (let ((jmp-target (if (closurep fun)
                        (let ((instance (%alloc-closure-trampoline)))
                          (setf (%funcallable-instance-fun instance) fun)
                          instance)
                        fun)))
    (with-pinned-objects (jmp-target)
      ;; CLOSURE-CALLEE accesses the self pointer of a funcallable
      ;; instance w/ builtin trampoline, or a simple-fun.
      ;; But the result is shifted by N-FIXNUM-TAG-BITS because
      ;; CELL-REF yields a descriptor-reg, not an unsigned-reg.
      (%primitive set-direct-callable-fdefn-fun fdefn fun
                  (get-lisp-obj-address (%closure-callee jmp-target)))))
  nil)

) ; end PROGN

;;; Find an immobile FDEFN or FUNCTION given an interior pointer to it.
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
        (#.fdefn-widetag
         (make-lisp-obj (logior obj other-pointer-lowtag)))
        (#.funcallable-instance-widetag
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

(defun singly-occurs-p (thing things &aux (len (length things)))
  ;; Return T if THING occurs exactly once in vector THINGS,
  ;; assuming that it occurs at all.
  (declare (simple-vector things))
  (dotimes (i len)
    (when (eq (svref things i) thing)
      ;; re-using I as the index is OK because we leave the outer loop
      ;; after this.
      (return (loop (cond ((>= (incf i) len) (return t))
                          ((eq thing (svref things i)) (return nil))))))))

(define-load-time-global *static-linker-lock* (sb-thread:make-mutex :name "static linker"))

(define-load-time-global *never-statically-link* '(find-package))
;;; Remove calls via fdefns from CODE. This is called after compiling
;;; to memory, or when saving a core.
;;; Do not replace globally notinline functions, because notinline has
;;; an extra connotation of ensuring that replacement of the function
;;; under that name always works. It usually works to replace a statically
;;; linked function, but with a caveat: un-statically-linking requires calling
;;; MAP-OBJECTS-IN-RANGE, which is unreliable in the presence of
;;; multiple threads. Unfortunately, some users dangerously redefine
;;; builtin functions, and moreover, while there are multiple threads.
(defun statically-link-code-obj (code fixups &optional observable-fdefns)
  (declare (ignorable code fixups observable-fdefns))
  #+immobile-code
  (binding* (((fdefns-start fdefns-count) (code-header-fdefn-range code))
             (replacements (make-array fdefns-count :initial-element nil))
             (ambiguous (make-array fdefns-count :initial-element 0 :element-type 'bit))
             (any-replacements nil)
             (any-ambiguous nil))
    ;; For each fdefn, decide two things:
    ;; * whether the fdefn can be replaced by its function - possible only when
    ;;   that function is in immobile space and needs no trampoline.
    ;; * whether the replacement creates ambiguitity - if #'F and #'G are the same
    ;;   function, then substituting that function in for the fdefn of F and G
    ;;   requires storing locations at which replacement was done
    (dotimes (i fdefns-count)
      (let* ((fdefn (code-header-ref code (+ fdefns-start i)))
             (fun (when (fdefn-p fdefn) (fdefn-fun fdefn))))
        (when (and (immobile-space-obj-p fun)
                   (not (closurep fun))
                   (not (member (fdefn-name fdefn) *never-statically-link* :test 'equal))
                   (neq (info :function :inlinep (fdefn-name fdefn)) 'notinline))
          (setf any-replacements t (aref replacements i) fun))))
    (dotimes (i fdefns-count)
      (when (and (aref replacements i)
                 (not (singly-occurs-p (aref replacements i) replacements)))
        (setf any-ambiguous t (bit ambiguous i) 1)))
    (unless any-replacements
      (return-from statically-link-code-obj))
    ;; Map each fixup to an index in REPLACEMENTS (which currently holds functions,
    ;; not fdefns, so we have to scan the code header).
    ;; This can be done outside the lock
    (flet ((index-of (fdefn)
             (dotimes (i fdefns-count)
               (when (eq fdefn (code-header-ref code (+ fdefns-start i)))
                 (return i)))))
      (setq fixups (mapcar (lambda (fixup) ; = (offset . #<fdefn>)
                             (cons (index-of (cdr fixup)) (car fixup)))
                           fixups)))
    (let ((insts (code-instructions code)))
      ;; One final check: if any of the fixed-up instructions is "MOV EAX, #xNNNN"
      ;; instead of a CALL or JMP, we can't fixup that particular fdefn for any
      ;; of its call sites. (They should all use MOV if any one does).
      ;; This happens when *COMPILE-TO-MEMORY-SPACE* is set to :AUTOMATIC.
      ;; In that case we don't know that the code will be within an imm32 of
      ;; the target address, because the code might have gone into dynamic space.
      (dolist (fixup fixups)
        (binding* ((fdefn-index (car fixup) :exit-if-null)
                   (offset (cdr fixup)))
          (when (and (aref replacements fdefn-index)
                     (not (eql (logior (sap-ref-8 insts (1- offset)) 1) #xE9)))
            (setf (aref replacements fdefn-index) nil))))
      (let ((stored-locs (if any-ambiguous
                             (make-array fdefns-count :initial-element nil))))
        (with-system-mutex (*static-linker-lock*)
          (dolist (fixup fixups)
            (binding* ((fdefn-index (car fixup) :exit-if-null)
                       (offset (cdr fixup))
                       (fdefn (code-header-ref code (+ fdefns-start fdefn-index)))
                       (fun (aref replacements fdefn-index)))
              (when (and fun (/= (bit ambiguous fdefn-index) 1))
                ;; Set the statically-linked flag
                (set-fdefn-has-static-callers fdefn 1)
                (when (= (bit ambiguous fdefn-index) 1)
                  (push offset (aref stored-locs fdefn-index)))
                ;; Change the machine instruction
                ;; %CLOSURE-CALLEE reads the entry addresss word of any
                ;; kind of function, but as if it were a tagged fixnum.
                (let ((entry (descriptor-sap (%closure-callee fun))))
                  (setf (signed-sap-ref-32 insts offset)
                        (sap- entry (sap+ insts (+ offset 4))))))))
          ;; Replace ambiguous elements of the code header while still holding the lock
          #+statically-link-if-ambiguous ; never enabled
          (dotimes (i fdefns-count)
            (when (= (bit ambiguous i) 1)
              (let ((wordindex (+ fdefns-start i))
                    (locs (aref stored-locs i)))
                (setf (code-header-ref code wordindex)
                      (cons (code-header-ref code wordindex) locs)))))))))
  code)

(defmacro static-call-entrypoint-vector ()
  '(- (get-lisp-obj-address sb-fasl::*asm-routine-vector*)
      (ash (+ vector-data-offset (align-up (length +static-fdefns+) 2)) word-shift)))

;;; Return either the address to jump to when calling NAME, or the address
;;; containing the address, depending on FIXUP-KIND.
;;; Use FDEFINITION because it strips encapsulations - whether that's
;;; the right behavior for it or not is a separate concern.
;;; If somebody tries (TRACE LENGTH) for example, it should not cause
;;; compilations to fail on account of LENGTH becoming a closure.
(defun function-raw-address (name fixup-kind &aux (fun (fdefinition name)))
  (declare (type (member :abs32 :rel32) fixup-kind))
  (cond ((not (immobile-space-obj-p fun))
         (error "Can't statically link to ~S: code is movable" name))
        ((neq (%fun-pointer-widetag fun) simple-fun-widetag)
         (error "Can't statically link to ~S: non-simple function" name))
        ((eq fixup-kind :rel32)
         ;; if performing a relative fixup, return where the function really is,
         ;; given that calling from anywhere in immobile space to immobile space
         ;; needs only a signed imm32 operand.
         (sap-ref-word (int-sap (get-lisp-obj-address fun))
                       (- (ash simple-fun-self-slot word-shift) fun-pointer-lowtag)))
        #+immobile-space
        (t
         ;; if calling from dynamic space, it is emitted as "call [abs]" where the
         ;; absolute address is in static space. Return the address of the element
         ;; in the entrypoint vector, not the address of the function.
         (let ((vector-data (sap+ (vector-sap sb-fasl::*asm-routine-vector*)
                                  (- (ash (+ vector-data-offset
                                             (align-up (length +static-fdefns+) 2))
                                          word-shift))))
               (index (the (not null) (position name +static-fdefns+))))
           (the (signed-byte 32)
                (sap-int (sap+ vector-data (ash index word-shift))))))))

;; Return the address to which to jump when calling FDEFN,
;; which is either an fdefn or the name of an fdefn.
(defun fdefn-entry-address (fdefn)
  (let ((fdefn (if (fdefn-p fdefn) fdefn (find-or-create-fdefn fdefn))))
    (+ (get-lisp-obj-address fdefn)
       (- 2 other-pointer-lowtag))))

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
              (sap-ref-word insts (truly-the index (ash (1+ i) word-shift))))))
    ;; Preceding the asm routine vector is the vector of addresses of static-fdefns [sic].
    ;; These are functions deemed particularly important, so they can be called using 1 instruction
    ;; from any address in dynamic space. The fdefns aren't actually in static space.
    (let ((vector (truly-the (simple-array word (*))
                             (%make-lisp-obj (static-call-entrypoint-vector)))))
      (dotimes (i (length +static-fdefns+))
        (setf (aref vector i)
              (let ((fun (%symbol-function (truly-the symbol (aref +static-fdefns+ i)))))
                (sap-ref-word (int-sap (get-lisp-obj-address fun))
                              (- (ash simple-fun-self-slot word-shift) fun-pointer-lowtag))))))))

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
