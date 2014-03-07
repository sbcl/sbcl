;;;; x86 VM definitions of various system hacking operations

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; type frobbing VOPs

(define-vop (lowtag-of)
  (:translate lowtag-of)
  (:policy :fast-safe)
  (:args (object :scs (any-reg descriptor-reg control-stack)
                 :target result))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 1
    (move result object)
    (inst and result lowtag-mask)))

(define-vop (widetag-of)
  (:translate widetag-of)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:temporary (:sc unsigned-reg :offset eax-offset :to (:result 0)) eax)
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (inst mov eax object)
    (inst and al-tn lowtag-mask)
    (inst cmp al-tn other-pointer-lowtag)
    (inst jmp :e other-ptr)
    (inst cmp al-tn fun-pointer-lowtag)
    (inst jmp :e function-ptr)

    ;; Pick off structures and list pointers.
    (inst test al-tn 1)
    (inst jmp :ne done)

    ;; Pick off fixnums.
    (inst and al-tn fixnum-tag-mask)
    (inst jmp :e done)

    ;; must be an other immediate
    (inst mov eax object)
    (inst jmp done)

    FUNCTION-PTR
    (load-type al-tn object (- fun-pointer-lowtag))
    (inst jmp done)

    OTHER-PTR
    (load-type al-tn object (- other-pointer-lowtag))

    DONE
    (inst movzx result al-tn)))

(define-vop (fun-subtype)
  (:translate fun-subtype)
  (:policy :fast-safe)
  (:args (function :scs (descriptor-reg)))
  (:temporary (:sc byte-reg :from (:eval 0) :to (:eval 1)) temp)
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (load-type temp function (- fun-pointer-lowtag))
    (inst movzx result temp)))

(define-vop (set-fun-subtype)
  (:translate (setf fun-subtype))
  (:policy :fast-safe)
  (:args (type :scs (unsigned-reg) :target eax)
         (function :scs (descriptor-reg)))
  (:arg-types positive-fixnum *)
  (:temporary (:sc unsigned-reg :offset eax-offset :from (:argument 0)
                   :to (:result 0) :target result)
              eax)
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (move eax type)
    (storeb al-tn function 0 fun-pointer-lowtag)
    (move result eax)))

(define-vop (get-header-data)
  (:translate get-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 other-pointer-lowtag)
    (inst shr res n-widetag-bits)))

(define-vop (get-closure-length)
  (:translate get-closure-length)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 fun-pointer-lowtag)
    (inst shr res n-widetag-bits)))

(define-vop (set-header-data)
  (:translate set-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg) :target res :to (:result 0))
         (data :scs (any-reg) :target eax))
  (:arg-types * positive-fixnum)
  (:results (res :scs (descriptor-reg)))
  (:temporary (:sc unsigned-reg :offset eax-offset
                   :from (:argument 1) :to (:result 0)) eax)
  (:generator 6
    (move eax data)
    (inst shl eax (- n-widetag-bits 2))
    (inst mov al-tn (make-ea :byte :base x :disp (- other-pointer-lowtag)))
    (storew eax x 0 other-pointer-lowtag)
    (move res x)))

(define-vop (pointer-hash)
  (:translate pointer-hash)
  (:args (ptr :scs (any-reg descriptor-reg) :target res))
  (:results (res :scs (any-reg descriptor-reg)))
  (:policy :fast-safe)
  (:generator 1
    (move res ptr)
    ;; Mask the lowtag, and shift the whole address into a positive
    ;; fixnum.
    (inst and res (lognot lowtag-mask))
    (inst shr res 1)))

;;;; allocation

(define-vop (dynamic-space-free-pointer)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate dynamic-space-free-pointer)
  (:policy :fast-safe)
  (:generator 1
    (load-symbol-value int *allocation-pointer*)))

(define-vop (binding-stack-pointer-sap)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate binding-stack-pointer-sap)
  (:policy :fast-safe)
  (:generator 1
    (load-binding-stack-pointer int)))

(defknown (setf binding-stack-pointer-sap)
    (system-area-pointer) system-area-pointer ())

(define-vop (set-binding-stack-pointer-sap)
  (:args (new-value :scs (sap-reg) :target int))
  (:arg-types system-area-pointer)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate (setf binding-stack-pointer-sap))
  (:policy :fast-safe)
  (:generator 1
    (store-binding-stack-pointer new-value)
    (move int new-value)))

(define-vop (control-stack-pointer-sap)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate control-stack-pointer-sap)
  (:policy :fast-safe)
  (:generator 1
    (move int esp-tn)))

;;;; code object frobbing

(define-vop (code-instructions)
  (:translate code-instructions)
  (:policy :fast-safe)
  (:args (code :scs (descriptor-reg) :to (:result 0)))
  (:results (sap :scs (sap-reg) :from (:argument 0)))
  (:result-types system-area-pointer)
  (:generator 10
    (loadw sap code 0 other-pointer-lowtag)
    (inst shr sap n-widetag-bits)
    (inst lea sap (make-ea :byte :base code :index sap :scale 4
                           :disp (- other-pointer-lowtag)))))

(define-vop (compute-fun)
  (:args (code :scs (descriptor-reg) :to (:result 0))
         (offset :scs (signed-reg unsigned-reg) :to (:result 0)))
  (:arg-types * positive-fixnum)
  (:results (func :scs (descriptor-reg) :from (:argument 0)))
  (:generator 10
    (loadw func code 0 other-pointer-lowtag)
    (inst shr func n-widetag-bits)
    (inst lea func
          (make-ea :byte :base offset :index func :scale 4
                   :disp (- fun-pointer-lowtag other-pointer-lowtag)))
    (inst add func code)))

(define-vop (%simple-fun-self)
  (:policy :fast-safe)
  (:translate %simple-fun-self)
  (:args (function :scs (descriptor-reg)))
  (:results (result :scs (descriptor-reg)))
  (:generator 3
    (loadw result function simple-fun-self-slot fun-pointer-lowtag)
    (inst lea result
          (make-ea :byte :base result
                   :disp (- fun-pointer-lowtag
                            (* simple-fun-code-offset n-word-bytes))))))

;;; The closure function slot is a pointer to raw code on X86 instead
;;; of a pointer to the code function object itself. This VOP is used
;;; to reference the function object given the closure object.
(define-source-transform %closure-fun (closure)
  `(%simple-fun-self ,closure))

(define-vop (%set-fun-self)
  (:policy :fast-safe)
  (:translate (setf %simple-fun-self))
  (:args (new-self :scs (descriptor-reg) :target result :to :result)
         (function :scs (descriptor-reg) :to :result))
  (:temporary (:sc any-reg :from (:argument 0) :to :result) temp)
  (:results (result :scs (descriptor-reg)))
  (:generator 3
    (inst lea temp (make-ea-for-object-slot new-self simple-fun-code-offset
                                            fun-pointer-lowtag))
    (storew temp function simple-fun-self-slot fun-pointer-lowtag)
    (move result new-self)))

;;;; symbol frobbing

;; only define if the feature is enabled to test building without it
#!+symbol-info-vops
(progn
(define-vop (symbol-info-vector)
  (:policy :fast-safe)
  (:translate symbol-info-vector)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (descriptor-reg)))
  (:temporary (:sc unsigned-reg :offset eax-offset) eax)
  (:generator 1
    (loadw res x symbol-info-slot other-pointer-lowtag)
    ;; If RES has list-pointer-lowtag, take its CDR. If not, use it as-is.
    ;; This CMOV safely reads from memory when it does not move, because if
    ;; there is an info-vector in the slot, it has at least one element.
    ;; This would compile to almost the same code without a VOP,
    ;; but using a jmp around a mov instead.
    (inst lea eax (make-ea :dword :base res :disp (- list-pointer-lowtag)))
    (emit-optimized-test-inst eax lowtag-mask)
    (inst cmov :e res
          (make-ea-for-object-slot res cons-cdr-slot list-pointer-lowtag))))
(define-vop (symbol-plist)
  (:policy :fast-safe)
  (:translate symbol-plist)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (descriptor-reg)))
  (:temporary (:sc unsigned-reg) temp)
  (:generator 1
    (loadw res x symbol-info-slot other-pointer-lowtag)
    ;; Instruction pun: (CAR x) is the same as (VECTOR-LENGTH x)
    ;; so if the info slot holds a vector, this gets a fixnum- it's not a plist.
    (loadw res res cons-car-slot list-pointer-lowtag)
    (inst mov temp nil-value)
    (emit-optimized-test-inst res fixnum-tag-mask)
    (inst cmov :e res temp))))

;;;; other miscellaneous VOPs

(defknown sb!unix::receive-pending-interrupt () (values))
(define-vop (sb!unix::receive-pending-interrupt)
  (:policy :fast-safe)
  (:translate sb!unix::receive-pending-interrupt)
  (:generator 1
    (inst break pending-interrupt-trap)))

#!+sb-safepoint
(define-vop (insert-safepoint)
  (:policy :fast-safe)
  (:translate sb!kernel::gc-safepoint)
  (:generator 0
    (emit-safepoint)))

#!+sb-thread
(defknown current-thread-offset-sap ((unsigned-byte 32))
  system-area-pointer (flushable))

#!+sb-thread
(define-vop (current-thread-offset-sap)
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate current-thread-offset-sap)
  (:args (n :scs (unsigned-reg)
            #!+win32 #!+win32 :to :save
            #!-win32 #!-win32 :target sap))
  (:arg-types unsigned-num)
  (:policy :fast-safe)
  (:generator 2
    #!+win32
    (progn
      ;; Note that SAP conflicts with N in this case, hence the reader
      ;; conditionals above.
      (inst mov sap (make-ea :dword :disp +win32-tib-arbitrary-field-offset+) :fs)
      (inst mov sap (make-ea :dword :base sap :disp 0 :index n :scale 4)))
    #!-win32
    (inst mov sap (make-ea :dword :disp 0 :index n :scale 4) :fs)))

(define-vop (halt)
  (:generator 1
    (inst break halt-trap)))

(defknown float-wait () (values))
(define-vop (float-wait)
  (:policy :fast-safe)
  (:translate float-wait)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 1
    (note-next-instruction vop :internal-error)
    (inst wait)))

;;;; Miscellany

;;; the RDTSC instruction (present on Pentium processors and
;;; successors) allows you to access the time-stamp counter, a 64-bit
;;; model-specific register that counts executed cycles. The
;;; instruction returns the low cycle count in EAX and high cycle
;;; count in EDX.
;;;
;;; In order to obtain more significant results on out-of-order
;;; processors (such as the Pentium II and later), we issue a
;;; serializing CPUID instruction before reading the cycle counter.
;;; This instruction is used for its side effect of emptying the
;;; processor pipeline, to ensure that the RDTSC instruction is
;;; executed once all pending instructions have been completed.
;;; CPUID writes to EBX and ECX in addition to EAX and EDX, so
;;; they need to be added as temporaries.
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
  (:temporary (:sc unsigned-reg :offset eax-offset :target lo) eax)
  (:temporary (:sc unsigned-reg :offset edx-offset :target hi) edx)
  (:temporary (:sc unsigned-reg :offset ebx-offset) ebx)
  (:temporary (:sc unsigned-reg :offset ecx-offset) ecx)
  (:ignore ebx ecx)
  (:results (hi :scs (unsigned-reg))
            (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 5
     (inst xor eax eax)
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

#!+sb-dyncount
(define-vop (count-me)
  (:args (count-vector :scs (descriptor-reg)))
  (:info index)
  (:generator 0
    (inst inc (make-ea-for-vector-data count-vector :offset index))))

;;;; Memory barrier support

#!+memory-barrier-vops
(define-vop (%compiler-barrier)
  (:policy :fast-safe)
  (:translate %compiler-barrier)
  (:generator 3))

#!+memory-barrier-vops
(define-vop (%memory-barrier)
  (:policy :fast-safe)
  (:translate %memory-barrier)
  (:generator 3
    (inst add (make-ea :dword :base esp-tn) 0 :lock)))

#!+memory-barrier-vops
(define-vop (%read-barrier)
  (:policy :fast-safe)
  (:translate %read-barrier)
  (:generator 3))

#!+memory-barrier-vops
(define-vop (%write-barrier)
  (:policy :fast-safe)
  (:translate %write-barrier)
  (:generator 3))

#!+memory-barrier-vops
(define-vop (%data-dependency-barrier)
  (:policy :fast-safe)
  (:translate %data-dependency-barrier)
  (:generator 3))

(define-vop (pause)
  (:translate spin-loop-hint)
  (:policy :fast-safe)
  (:generator 0
    (inst pause)))

;;;;

(defknown %cons-cas-pair (cons t t t t) (values t t &optional))
;; These unsafely permits cmpxchg on any kind of vector, boxed or unboxed
;; and the same goes for instances.
(defknown %vector-cas-pair (simple-array index t t t t) (values t t &optional))
(defknown %instance-cas-pair (instance index t t t t) (values t t &optional))

(macrolet
    ((define-cmpxchg-vop (name memory-operand more-stuff &optional index-arg)
       `(define-vop (,name)
          (:policy :fast)
          ,@more-stuff
          (:args (data :scs (descriptor-reg) :to :eval)
                 ,@index-arg
                 (expected-old-lo :scs (descriptor-reg any-reg) :target eax)
                 (expected-old-hi :scs (descriptor-reg any-reg) :target edx)
                 (new-lo :scs (descriptor-reg any-reg) :target ebx)
                 (new-hi :scs (descriptor-reg any-reg) :target ecx))
          (:results (result-lo :scs (descriptor-reg any-reg))
                    (result-hi :scs (descriptor-reg any-reg)))
          (:temporary (:sc unsigned-reg :offset eax-offset
                       :from (:argument 2) :to (:result 0)) eax)
          (:temporary (:sc unsigned-reg :offset edx-offset
                       :from (:argument 3) :to (:result 0)) edx)
          (:temporary (:sc unsigned-reg :offset ebx-offset
                       :from (:argument 4) :to (:result 0)) ebx)
          (:temporary (:sc unsigned-reg :offset ecx-offset
                       :from (:argument 5) :to (:result 0)) ecx)
          (:generator 7
           (move eax expected-old-lo)
           (move edx expected-old-hi)
           (move ebx new-lo)
           (move ecx new-hi)
           (inst cmpxchg8b ,memory-operand :lock)
           ;; EDX:EAX  hold the actual old contents of memory.
           ;; Manually analyze result lifetimes to avoid clobbering.
           (cond ((and (location= result-lo edx) (location= result-hi eax))
                  (inst xchg eax edx)) ; unlikely, but possible
                 ((location= result-lo edx) ; result-hi is not eax
                  (move result-hi edx) ; move high part first
                  (move result-lo eax))
                 (t                    ; result-lo is not edx
                  (move result-lo eax) ; move low part first
                  (move result-hi edx)))))))
  (define-cmpxchg-vop compare-and-exchange-pair
      (make-ea :dword :base data :disp (- list-pointer-lowtag))
      ((:translate %cons-cas-pair)))
  (define-cmpxchg-vop compare-and-exchange-pair-indexed
      (make-ea :dword :base data :disp offset :index index
                      :scale (ash n-word-bytes (- n-fixnum-tag-bits)))
      ((:variant-vars offset))
      ((index :scs (descriptor-reg any-reg) :to :eval))))

(define-vop (%vector-cas-pair compare-and-exchange-pair-indexed)
  (:translate %vector-cas-pair)
  (:variant (- (* n-word-bytes vector-data-offset) other-pointer-lowtag)))

(define-vop (%instance-cas-pair compare-and-exchange-pair-indexed)
  (:translate %instance-cas-pair)
  (:variant (- (* n-word-bytes instance-slots-offset) instance-pointer-lowtag)))

(defknown %cpu-identification ((unsigned-byte 32) (unsigned-byte 32))
    (values (unsigned-byte 32) (unsigned-byte 32)
            (unsigned-byte 32) (unsigned-byte 32)))

;; The only use of CPUID heretofore was for its flushing of the I-pipeline.
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
               :offset eax-offset) eax)
  (:temporary (:sc unsigned-reg :from (:argument 1) :to (:result 2)
               :offset ecx-offset) ecx)
  (:temporary (:sc unsigned-reg :from :eval :to (:result 3)
               :offset edx-offset) edx)
  (:temporary (:sc unsigned-reg :from :eval :to (:result 1)
               :offset ebx-offset) ebx)
  (:generator 5
   (move eax function)
   (move ecx subfunction)
   (inst cpuid)
   (move a eax)
   (move b ebx)
   (move c ecx)
   (move d edx)))
