;;;; x86 VM definitions of various system hacking operations

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;;; type frobbing VOPs

;;; For non-list pointer descriptors, return the header's widetag byte.
;;; For lists and non-pointers, return the low 8 descriptor bits.
;;; We need not return exactly list-pointer-lowtag for lists - the high 4 bits
;;; are arbitrary. Similarly we don't care that fixnums return other than 0.
;;; Provided that the result is the correct index to **PRIMITIVE-OBJECT-LAYOUTS**
;;; everything works out fine.  All backends should follow this simpler model,
;;; but might or might not opt to use the same technique of producing a native
;;; pointer and doing one memory access for all 3 non-list pointer types.
(define-vop (widetag-of)
  (:translate widetag-of)
  (:policy :fast-safe)
  (:args (object :scs (any-reg descriptor-reg)))
  (:temporary (:sc unsigned-reg :target result :to (:result 0)) temp)
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (inst lea :dword temp (ea -3 object))
    (inst test :byte temp 3)
    (inst jmp :nz IMMEDIATE)
    (inst and :byte temp lowtag-mask)
    (inst cmp :byte temp (- list-pointer-lowtag 3))
    (inst jmp :e IMMEDIATE)
    ;; It's a function, instance, or other pointer.
    (inst mov temp object)
    ;; OBJECT is implicitly pinned, TEMP can GC-safely point to it
    ;; with no lowtag.
    (inst and temp (lognot lowtag-mask)) ; native pointer
    (inst movzx '(:byte :dword) result (ea temp))
    (inst jmp DONE)
    IMMEDIATE
    (inst movzx '(:byte :dword) result object)
    DONE))

#+compact-instance-header
(progn
;; ~16 instructions vs. 35
(define-vop ()
    (:policy :fast-safe)
    (:translate layout-of)
    (:args (object :scs (descriptor-reg)))
    (:temporary (:sc unsigned-reg :offset rax-offset) rax)
    (:results (result :scs (descriptor-reg)))
    (:generator 6
      ;; Lowtag: #b0011 instance
      ;;         #b0111 list
      ;;         #b1011 fun
      ;;         #b1111 other
      (inst mov  rax object)
      (inst xor  :byte rax #b0011)
      (inst test :byte rax #b0111)
      (inst jmp  :ne try-other)
      ;; It's an instance or function. Both have the layout in the header.
      (inst and  :byte rax #b11110111)
      (inst mov  :dword result (ea 4 rax))
      (inst jmp  done)
      TRY-OTHER
      (inst xor  :byte rax #b1100)
      (inst test :byte rax #b1111)
      (inst jmp  :ne imm-or-list)
      ;; It's an other-pointer. Read the widetag.
      (inst movzx '(:byte :dword) rax (ea rax))
      (inst jmp  load-from-vector)
      IMM-OR-LIST
;;; There is a way to reduce these next 4 instructions to 3, but the encodings are longer
;;; and have an extra memory read. The trick is to cleverly use the header of NIL to CMOV
;;; from a constant 1, since CMOV can't take an immediate operand. (There are in fact a
;;; few places that a 1 is stashed at a known address in static space)
;;;    (inst cmp object nil-value)
;;;    (inst movzx '(:byte :dword) rax object)
;;;    (inst cmov :dword :e rax (EA something))
;;;  50000108: 000000000000012D = package-id #x0001 | symbol-widetag
;;;  50000110: 0000000050000117 = NIL-VALUE
;;;  50000118: 0000000050000117
      (inst cmp  object nil-value)
      (inst mov :byte rax sb-kernel::index-of-layout-for-NULL)
      (inst cmov :dword :ne rax object)
      (inst movzx '(:byte :dword) rax rax) ; same as "AND EAX,255" but shorter encoding
      LOAD-FROM-VECTOR
      (inst mov :dword result
            (ea (make-fixup '**primitive-object-layouts** :symbol-value
                           (- (ash vector-data-offset word-shift)
                              other-pointer-lowtag))
                nil rax 8)) ; no base register
      DONE))
(define-vop ()
    (:policy :fast-safe)
    (:translate %instanceoid-layout)
    (:args (object :scs (descriptor-reg) :to :save))
    (:temporary (:sc unsigned-reg) temp)
    (:results (result :scs (descriptor-reg)))
    (:generator 6
      (inst lea temp (ea -3 object))
      (inst and temp (lognot #b1000))
      (inst mov :dword result (make-fixup 't :layout))
      (inst test :byte temp 15)
      (inst jmp :ne DONE)
      (inst mov :dword result (ea 4 temp))
      DONE)))

(macrolet ((load-type (target source lowtag)
             `(inst movzx '(:byte :dword) ,target (ea (- ,lowtag) ,source))))
(define-vop (%other-pointer-widetag)
  (:translate %other-pointer-widetag)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 1 (load-type result object other-pointer-lowtag)))
(define-vop ()
  (:translate %fun-pointer-widetag)
  (:policy :fast-safe)
  (:args (function :scs (descriptor-reg)))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 1 (load-type result function fun-pointer-lowtag))))

(define-vop (get-header-data)
  (:translate get-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 other-pointer-lowtag)
    (inst shr res n-widetag-bits)))

;;; This operation is racy with GC and therefore slightly dangerous, especially
;;; on objects in immobile space which reserve byte 3 of the header for GC.
(define-vop ()
  (:translate set-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg) :to :eval)
         (data :scs (any-reg) :target temp))
  (:arg-types * positive-fixnum)
  (:temporary (:sc unsigned-reg :from (:argument 1)) temp)
  (:generator 6
    (move temp data)
    (inst shl temp (- n-widetag-bits n-fixnum-tag-bits))
    ;; merge in the widetag
    (inst mov :byte temp (ea (- other-pointer-lowtag) x))
    (storew temp x 0 other-pointer-lowtag)))
(flet ((header-byte-imm8 (bits)
         ;; return an imm8 and a shift amount expressed in bytes
         (cond ((typep bits '(unsigned-byte 8))
                (values bits 0))
               ((and (not (logtest bits #xff))
                     (typep (ash bits -8) '(unsigned-byte 8)))
                (values (ash bits -8) 1))
               ((and (not (logtest bits #xffff))
                     (typep (ash bits -16) '(unsigned-byte 8)))
                (values (ash bits -16) 2))
               (t
                (bug "Can't construct mask from ~x" bits)))))
(define-vop (logior-header-bits)
  (:translate logior-header-bits)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg))
         (bits :scs (unsigned-reg immediate)))
  (:arg-refs dummy bits-ref)
  (:arg-types * positive-fixnum)
  (:generator 1
    (cond ((sc-is bits immediate)
           (multiple-value-bind (imm8 shift) (header-byte-imm8 (tn-value bits))
             (inst or :lock :byte (ea (- (1+ shift) other-pointer-lowtag) x) imm8)))
          ((csubtypep (tn-ref-type bits-ref) (specifier-type '(unsigned-byte 16)))
           ;; don't need a :lock. Not sure where this case is needed,
           ;; but it surely isn't for storing into symbol headers.
           (inst or :word (ea (- 1 other-pointer-lowtag) x) bits))
          (t
           ;; This needs a temp to OR in the widetag and store only the low 4 header bytes.
           ;; We don't know whether BITS fits in 8,16, or 32 bits but a 4-byte unaligned store
           ;; at byte offset 1 collides with symbol-TLS-index in the high 4 bytes.
           (bug "Unhandled")))))
(define-vop ()
  (:translate assign-vector-flags)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:info bits)
  (:arg-types t (:constant (unsigned-byte 8)))
  (:generator 1
    (inst mov :byte (ea (- (/ array-flags-position n-word-bytes) other-pointer-lowtag) x)
      bits)))
(define-vop ()
  (:translate reset-header-bits)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:arg-types t (:constant (unsigned-byte 24)))
  (:info bits)
  (:generator 1
    (multiple-value-bind (imm8 shift) (header-byte-imm8 bits)
      (inst and :lock :byte (ea (- (1+ shift) other-pointer-lowtag) x) (logandc1 imm8 #xff)))))
(define-vop (test-header-data-bit)
  (:translate test-header-data-bit)
  (:policy :fast-safe)
  (:args (array :scs (descriptor-reg)))
  (:info mask)
  (:arg-types t (:constant t))
  (:conditional :ne)
  (:generator 1
    (multiple-value-bind (imm8 shift) (header-byte-imm8 mask)
      (inst test :byte (ea (- (1+ shift) other-pointer-lowtag) array) imm8)))))

;;;; allocation

(define-vop (binding-stack-pointer-sap)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate binding-stack-pointer-sap)
  (:policy :fast-safe)
  (:generator 1
    (load-binding-stack-pointer int)))

(define-vop (control-stack-pointer-sap)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate control-stack-pointer-sap)
  (:policy :fast-safe)
  (:generator 1
    (move int rsp-tn)))

;;;; code object frobbing

(define-vop (code-instructions)
  (:translate code-instructions)
  (:policy :fast-safe)
  (:args (code :scs (descriptor-reg) :to (:result 0)))
  (:results (sap :scs (sap-reg) :from (:argument 0)))
  (:result-types system-area-pointer)
  (:generator 10
    ;; load boxed header size in bytes
    (inst mov :dword sap (ea (- n-word-bytes other-pointer-lowtag) code))
    (inst lea sap (ea (- other-pointer-lowtag) code sap))))

(define-vop (code-trailer-ref)
  (:translate code-trailer-ref)
  (:policy :fast-safe)
  (:args (code :scs (descriptor-reg) :to (:result 0))
         (offset :scs (signed-reg immediate) :to (:result 0)))
  (:arg-types * fixnum)
  (:results (res :scs (unsigned-reg) :from (:argument 0)))
  (:result-types unsigned-num)
  (:generator 10
    ;; get the object size in words
    (inst mov :dword res (ea (- 4 other-pointer-lowtag) code))
    (cond ((sc-is offset immediate)
           (inst mov :dword res (ea (- (tn-value offset) other-pointer-lowtag)
                                    code res n-word-bytes)))
          (t
           ;; compute sum of object size in bytes + negative offset - lowtag
           (inst lea :dword res (ea (- other-pointer-lowtag) offset res n-word-bytes))
           (inst mov :dword res (ea code res))))))

(define-vop (compute-fun)
  (:args (code :scs (descriptor-reg) :to (:result 0))
         (offset :scs (signed-reg unsigned-reg) :to :eval :target func))
  (:arg-types * positive-fixnum)
  (:results (func :scs (descriptor-reg) :from :eval))
  (:generator 3
    (move func offset)
    ;; add boxed header size in bytes
    (inst add :dword func (ea (- n-word-bytes other-pointer-lowtag) code))
    (inst lea func (ea (- fun-pointer-lowtag other-pointer-lowtag) code func))))

;;; This vop is quite magical - because 'closure-fun' is a raw program counter,
;;; as soon as it's loaded into a register, it prevents the underlying fun from
;;; being transported by GC. It's even subtler in that sense than COMPUTE-FUN,
;;; which doesn't pin a *different* object produced from thin air.
;;; (It's output operand is embedded in the object pointed to by its input)
(define-vop (%closure-fun)
  (:policy :fast-safe)
  (:translate %closure-fun)
  (:args (function :scs (descriptor-reg)))
  (:results (result :scs (descriptor-reg)))
  (:generator 3
    (loadw result function closure-fun-slot fun-pointer-lowtag)
    (inst sub result (- (* simple-fun-insts-offset n-word-bytes) fun-pointer-lowtag))))


;;;; other miscellaneous VOPs

(defknown sb-unix::receive-pending-interrupt () (values))
(define-vop (sb-unix::receive-pending-interrupt)
  (:policy :fast-safe)
  (:translate sb-unix::receive-pending-interrupt)
  (:generator 1
    (inst break pending-interrupt-trap)))

(define-vop (current-thread-offset-sap/c)
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate current-thread-offset-sap)
  (:info n)
  (:arg-types (:constant signed-byte))
  (:policy :fast-safe)
  (:generator 1
    #-gs-seg (inst mov sap (if (= n thread-this-slot) thread-tn (thread-slot-ea n)))
    #+gs-seg (inst mov sap (thread-slot-ea n))))
(define-vop (current-thread-offset-sap)
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate current-thread-offset-sap)
  (:args (index :scs (any-reg) :target sap))
  (:arg-types tagged-num)
  (:policy :fast-safe)
  (:generator 2
    (let (#+gs-seg (thread-tn nil))
      (inst mov sap
            (ea thread-segment-reg thread-tn
                index (ash 1 (- word-shift n-fixnum-tag-bits)))))))

(define-vop (halt)
  (:generator 1
    (inst break halt-trap)))

;;;; Miscellany

;;; the RDTSC instruction (present on Pentium processors and
;;; successors) allows you to access the time-stamp counter, a 64-bit
;;; model-specific register that counts executed cycles. The
;;; instruction returns the low cycle count in EAX and high cycle
;;; count in EDX.
;;;
;;; In order to obtain more significant results on out-of-order
;;; processors (such as the Pentium II and later), we issue a
;;; serializing CPUID instruction before and after reading the cycle
;;; counter. This instruction is used for its side effect of emptying
;;; the processor pipeline, to ensure that the RDTSC instruction is
;;; executed once all pending instructions have been completed and
;;; before any others. CPUID writes to EBX and ECX in addition to EAX
;;; and EDX, so they need to be added as temporaries.
;;;
;;; Note that cache effects mean that the cycle count can vary for
;;; different executions of the same code (it counts cycles, not
;;; retired instructions). Furthermore, the results are per-processor
;;; and not per-process, so are unreliable on multiprocessor machines
;;; where processes can migrate between processors.
;;;
;;; This method of obtaining a cycle count has the advantage of being
;;; very fast (around 20 cycles), and of not requiring a system call.
;;; However, you need to know your processor's clock speed to translate
;;; this into real execution time.
;;;
;;; FIXME: This about the WITH-CYCLE-COUNTER interface a bit, and then
;;; perhaps export it from SB-SYS.

(defknown %read-cycle-counter () (values (unsigned-byte 32) (unsigned-byte 32)) ())

(define-vop (%read-cycle-counter)
  (:policy :fast-safe)
  (:translate %read-cycle-counter)
  (:temporary (:sc unsigned-reg :offset rax-offset :target lo) eax)
  (:temporary (:sc unsigned-reg :offset rdx-offset :target hi) edx)
  (:temporary (:sc unsigned-reg :offset rbx-offset) ebx)
  (:temporary (:sc unsigned-reg :offset rcx-offset) ecx)
  (:ignore ebx ecx)
  (:results (hi :scs (unsigned-reg))
            (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 5
     (zeroize eax)
     ;; Intel docs seem quite consistent on only using CPUID before RDTSC,
     ;; not both before and after. Go figure.
     (inst cpuid)
     (inst rdtsc)
     (move lo eax)
     (move hi edx)))

(defmacro with-cycle-counter (&body body)
  "Returns the primary value of BODY as the primary value, and the
number of CPU cycles elapsed as secondary value. EXPERIMENTAL."
  (with-unique-names (hi0 hi1 lo0 lo1)
    `(multiple-value-bind (,hi0 ,lo0) (%read-cycle-counter)
       (values (locally ,@body)
               (multiple-value-bind (,hi1 ,lo1) (%read-cycle-counter)
                 (+ (ash (- ,hi1 ,hi0) 32)
                    (- ,lo1 ,lo0)))))))

#+sb-dyncount
(define-vop (count-me)
  (:args (count-vector :scs (descriptor-reg)))
  (:info index)
  (:generator 0
    (inst inc (ea (- (* (+ vector-data-offset index) n-word-bytes)
                     other-pointer-lowtag)
                  count-vector))))

;;;; Memory barrier support

;;; Some of these might not really have to inhibit 'instcombine'
;;; but conservatively it's always the right choice.
;;; Certainly for redundant move elimination of the reg->reg, reg->reg form
;;; the barrier is irrelevant, but (a) that won't happen, and (b) we never
;;; had an instcombine pass so who cares if occasionally it fails to apply?
(define-vop (%compiler-barrier)
  (:policy :fast-safe)
  (:translate %compiler-barrier)
  (:generator 3
    ;; inhibit instcombine across any barrier
    (inst .skip 0)))

(define-vop (%memory-barrier)
  (:policy :fast-safe)
  (:translate %memory-barrier)
  (:generator 3
    (inst mfence)))

(define-vop (%read-barrier)
  (:policy :fast-safe)
  (:translate %read-barrier)
  (:generator 3
    ;; inhibit instcombine across any barrier
    (inst .skip 0)))

(define-vop (%write-barrier)
  (:policy :fast-safe)
  (:translate %write-barrier)
  (:generator 3
    ;; inhibit instcombine across any barrier
    (inst .skip 0)))

(define-vop (%data-dependency-barrier)
  (:policy :fast-safe)
  (:translate %data-dependency-barrier)
  (:generator 3
    ;; inhibit instcombine across any barrier
    (inst .skip 0)))

(define-vop ()
  (:translate spin-loop-hint)
  (:policy :fast-safe)
  (:generator 0
    (inst pause)))

(defknown %cpu-identification ((unsigned-byte 32) (unsigned-byte 32))
    (values (unsigned-byte 32) (unsigned-byte 32)
            (unsigned-byte 32) (unsigned-byte 32)))

;; This instruction does in fact not utilize all bits of the full width (Rxx)
;; regs so it would be wonderful to share this verbatim with x86 32-bit.
(define-vop (%cpu-identification)
  (:policy :fast-safe)
  (:translate %cpu-identification)
  (:args (function :scs (unsigned-reg) :target eax)
         (subfunction :scs (unsigned-reg) :target ecx))
  (:arg-types unsigned-num unsigned-num)
  (:results (a :scs (unsigned-reg))
            (b :scs (unsigned-reg))
            (c :scs (unsigned-reg))
            (d :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg :from (:argument 0) :to (:result 0)
               :offset rax-offset) eax)
  (:temporary (:sc unsigned-reg :from (:argument 1) :to (:result 2)
               :offset rcx-offset) ecx)
  (:temporary (:sc unsigned-reg :from :eval :to (:result 3)
               :offset rdx-offset) edx)
  (:temporary (:sc unsigned-reg :from :eval :to (:result 1)
               :offset rbx-offset) ebx)
  (:generator 5
   (move eax function)
   (move ecx subfunction)
   (inst cpuid)
   (move a eax)
   (move b ebx)
   (move c ecx)
   (move d edx)))

(define-vop (sb-c::mark-covered)
 (:info index)
 (:generator 1
   ;; Can't convert index to a code-relative index until the boxed header length
   ;; has been determined.
   (inst store-coverage-mark index)))

(define-vop ()
  (:translate sb-lockless:get-next)
  (:policy :fast-safe)
  (:args (node :scs (descriptor-reg)))
  (:results (next-tagged :scs (descriptor-reg))
            (next-bits :scs (descriptor-reg)))
  (:generator 10
    ;; Read the first user-data slot, which might be an untagged pointer
    ;; to the next node. Conservative GC implicitly pins objects pointed to by untagged
    ;; instance pointers now.
    (loadw next-bits node (+ instance-slots-offset instance-data-start) instance-pointer-lowtag)
    ;; Also returning the unadjusted bits as a secondary result.
    ;; This can'be use the LEA instruction because _usually_ NEXT-BITS
    ;; already hold a tagged pointer.
    (inst mov next-tagged next-bits)
    (inst or :byte next-tagged instance-pointer-lowtag)))

(define-vop (switch-to-arena)
  (:args (x :scs (descriptor-reg immediate)))
  (:temporary (:sc unsigned-reg :offset rdi-offset :from (:argument 0)) rdi)
  (:temporary (:sc unsigned-reg :offset rsi-offset) rsi)
  (:vop-var vop)
  (:ignore rsi)
  (:generator 1
    (inst mov rdi (if (sc-is x immediate) (tn-value x) x))
    ;; We could have the vop declare that all C volatile regs are clobbered,
    ;; but since this needs pseudo-atomic decoration anyway, it saves code size
    ;; to make an assembly routine that preserves all registers.
    (invoke-asm-routine 'call 'switch-to-arena vop)))

;;; Caution: this vop potentially clobbers all registers, but it doesn't declare them.
;;; It's safe to use only from DESTROY-ARENA which, being an ordinary full call,
;;; is presumed not to preserve registers.
#+sb-xc-host
(define-vop (delete-arena)
  (:args (x :scs (descriptor-reg)))
  (:temporary (:sc unsigned-reg :offset rdi-offset :from (:argument 0)) rdi)
  (:temporary (:sc unsigned-reg :offset rbx-offset) rsp-save)
  (:vop-var vop)
  (:generator 1
    (move rdi x)
    (pseudo-atomic ()
      (inst mov rsp-save rsp-tn)
      (inst and rsp-tn -16) ; align as required by some ABIs
      (call-c "sbcl_delete_arena" #+win32 rdi)
      (inst mov rsp-tn rsp-save))))

#+ultrafutex
(define-vop (quick-try-mutex)
  (:translate quick-try-mutex)
  (:policy :fast-safe)
  (:args (m :scs (descriptor-reg)))
  (:temporary (:sc unsigned-reg :offset 0) old) ; RAX
  (:temporary (:sc unsigned-reg) new)
  (:conditional :z)
  (:generator 1
    (inst mov :byte new 1)
    (zeroize old)
    (inst cmpxchg :lock :byte (mutex-slot m state) new)))
