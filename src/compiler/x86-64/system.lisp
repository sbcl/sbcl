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

(macrolet ((read-depthoid ()
             `(ea (- (+ 4 (ash (+ instance-slots-offset
                                  (get-dsd-index layout sb-kernel::flags))
                               word-shift))
                     instance-pointer-lowtag)
                  layout)))
  (define-vop ()
    (:translate layout-depthoid)
    (:policy :fast-safe)
    (:args (layout :scs (descriptor-reg)))
    (:results (res :scs (any-reg)))
    (:result-types fixnum)
    (:generator 1 (inst movsx '(:dword :qword) res (read-depthoid))))
  (define-vop ()
    (:translate sb-c::layout-depthoid-ge)
    (:policy :fast-safe)
    (:args (layout :scs (descriptor-reg)))
    (:info k)
    (:arg-types * (:constant (unsigned-byte 16)))
    (:conditional :ge)
    (:generator 1 (inst cmp :dword (read-depthoid) (fixnumize k)))))

(define-vop ()
  (:translate sb-c::%structure-is-a)
  (:args (x :scs (descriptor-reg)))
  (:arg-types * (:constant t))
  (:info test)
  (:policy :fast-safe)
  (:conditional :e)
  (:generator 1
    (inst cmp :dword
          (ea (+ (id-bits-offset)
                 (ash (- (wrapper-depthoid test) 2) 2)
                 (- instance-pointer-lowtag))
              x)
          ;; Small layout-ids can only occur for layouts made in genesis.
          ;; Therefore if the compile-time value of the ID is small,
          ;; it is permanently assigned to that type.
          ;; Otherwise, we allow for the possibility that the compile-time ID
          ;; is not the same as the load-time ID.
          ;; I don't think layout-id 0 can get here, but be sure to exclude it.
          (if (or (typep (layout-id test) '(and (signed-byte 8) (not (eql 0))))
                  (not (sb-c::producing-fasl-file)))
              (layout-id test)
              (make-fixup test :layout-id)))))

#+compact-instance-header
(progn
;; ~17 instructions vs. 35
(define-vop ()
    (:policy :fast-safe)
    (:translate wrapper-of)
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
      (inst cmp  object nil-value)
      (inst jmp  :eq NULL)
      (inst movzx '(:byte :dword) rax object)
      LOAD-FROM-VECTOR
      (inst mov :dword result
            (ea (make-fixup '**primitive-object-layouts**
                           :symbol-value
                           (- (ash vector-data-offset word-shift)
                              other-pointer-lowtag))
                nil rax 8)) ; no base register
      (inst jmp  done)
      NULL
      (inst mov  result (make-fixup 'null :layout))
      DONE
      #+metaspace (inst mov result (ea +5 result)))) ; layout->wrapper
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
    ;; merge in the widetag. We should really preserve bit 63 as well
    ;; which could be a GC mark bit, but it's not concurrent at the moment.
    (inst mov :byte temp (ea (- other-pointer-lowtag) x))
    (storew temp x 0 other-pointer-lowtag)))
(define-vop (logior-header-bits)
  (:translate logior-header-bits)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg))
         (bits :scs (unsigned-reg immediate)))
  (:arg-types * positive-fixnum)
  (:results (res :scs (descriptor-reg)))
  (:generator 1
    (if (sc-is bits immediate)
        (let ((bits (tn-value bits)))
          (cond ((typep bits '(unsigned-byte 8))
                 (inst or :byte (ea (- 1 other-pointer-lowtag) x) bits))
                ((not (logtest bits #xff))
                 (inst or :byte (ea (- 2 other-pointer-lowtag) x) (ash bits -8)))
                (t
                 (inst or :word (ea (- 1 other-pointer-lowtag) x) bits))))
        (inst or :word (ea (- 1 other-pointer-lowtag) x) bits))
    (move res x)))
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
  (:arg-types t (:constant (unsigned-byte 16)))
  (:info bits)
  (:generator 1
    (let ((byte 1))
      (when (> bits #xff)
        (setf bits (ash bits -8))
        (setf byte 2))
      (inst and :byte (ea (- byte other-pointer-lowtag) x) (logandc1 bits #xff)))))
(define-vop (test-header-data-bit)
  (:translate test-header-data-bit)
  (:policy :fast-safe)
  (:args (array :scs (descriptor-reg)))
  (:info mask)
  (:arg-types t (:constant t))
  (:conditional :ne)
  (:generator 1
    (let ((byte 1))
      (when (> mask #xff)
        (setf mask (ash mask -8))
        (setf byte 2))
      (inst test :byte (ea (- byte other-pointer-lowtag) array) mask))))

(define-vop (pointer-hash)
  (:translate pointer-hash)
  (:args (ptr :scs (any-reg descriptor-reg) :target res))
  (:results (res :scs (any-reg descriptor-reg)))
  (:policy :fast-safe)
  (:generator 1
    (move res ptr)
    (inst and res (lognot fixnum-tag-mask))))

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
    (inst lea result
          (ea  (- fun-pointer-lowtag (* simple-fun-insts-offset n-word-bytes))
               result))))

;;;; symbol frobbing
(defun load-symbol-dbinfo (result symbol)
  (loadw result symbol symbol-info-slot other-pointer-lowtag)
  ;; If RES has list-pointer-lowtag, take its CDR. If not, use it as-is.
  ;; This CMOV safely reads from memory when it does not move, because if
  ;; there is a PACKED-INFO in the slot, it has at least 4 data words in total
  ;; - the header, at least one info descriptor, and at least one datum.
  ;; And since 3 is odd, that would be aligned up to 4.
  ;; Use bit index 2 of the lowtag to distinguish list from instance.
  ;; An instance will have a 0 in that bit.
  ;; This would compile to almost the same code without a VOP,
  ;; but using a jmp around a mov instead.
  (aver (= (logior instance-pointer-lowtag #b0100) list-pointer-lowtag))
  (inst test :byte result #b0100)
  (inst cmov :nz result (object-slot-ea result cons-cdr-slot list-pointer-lowtag)))

(define-vop (symbol-dbinfo)
  (:policy :fast-safe)
  (:translate symbol-dbinfo)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (descriptor-reg)))
  (:generator 1 (load-symbol-dbinfo res x)))

;;;; other miscellaneous VOPs

(defknown sb-unix::receive-pending-interrupt () (values))
(define-vop (sb-unix::receive-pending-interrupt)
  (:policy :fast-safe)
  (:translate sb-unix::receive-pending-interrupt)
  (:generator 1
    (inst break pending-interrupt-trap)))

#+sb-thread
(progn
(define-vop (current-thread-offset-sap/c)
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate current-thread-offset-sap)
  (:info n)
  (:arg-types (:constant signed-byte))
  (:policy :fast-safe)
  (:generator 1
    (inst mov sap (thread-slot-ea n))))
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
                index (ash 1 (- word-shift n-fixnum-tag-bits))))))))

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

(define-vop (set-fdefn-has-static-callers)
  (:args (fdefn :scs (descriptor-reg)))
  (:generator 1
    ;; atomic because the immobile gen# is in the same byte
    (inst or :lock :byte (ea (- 1 other-pointer-lowtag) fdefn) #x80)))
(define-vop (unset-fdefn-has-static-callers)
  (:args (fdefn :scs (descriptor-reg)))
  (:generator 1
    ;; atomic because the immobile gen# is in the same byte
    (inst and :lock :byte (ea (- 1 other-pointer-lowtag) fdefn) #x7f)))
