;;;; SAP operations for the x86 VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;;; moves and coercions

;;; Move a tagged SAP to an untagged representation.
(define-vop (move-to-sap)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (sap-reg)))
  (:note "pointer to SAP coercion")
  (:generator 1
    (loadw y x sap-pointer-slot other-pointer-lowtag)))
(define-move-vop move-to-sap :move
  (descriptor-reg) (sap-reg))

;;; Move an untagged SAP to a tagged representation.
(define-vop (move-from-sap)
  (:args (sap :scs (sap-reg) :to :result))
  (:results (res :scs (descriptor-reg) :from :argument))
  (:note "SAP to pointer coercion")
  (:node-var node)
  (:generator 20
    (alloc-other res sap-widetag sap-size node)
    (storew sap res sap-pointer-slot other-pointer-lowtag)))
(define-move-vop move-from-sap :move
  (sap-reg) (descriptor-reg))

;;; Move untagged sap values.
(define-vop (sap-move)
  (:args (x :target y
            :scs (sap-reg)
            :load-if (not (location= x y))))
  (:results (y :scs (sap-reg)
               :load-if (not (location= x y))))
  (:note "SAP move")
  (:generator 0
    (move y x)))
(define-move-vop sap-move :move
  (sap-reg) (sap-reg))

;;; Move untagged sap arguments/return-values.
(define-vop (move-sap-arg)
  (:args (x :target y
            :scs (sap-reg))
         (fp :scs (any-reg)
             :load-if (not (sc-is y sap-reg))))
  (:results (y))
  (:note "SAP argument move")
  (:generator 0
    (sc-case y
      (sap-reg
       (move y x))
      (sap-stack
       (if (= (tn-offset fp) esp-offset)
           (storew x fp (tn-offset y))  ; c-call
           (storew x fp (frame-word-offset (tn-offset y))))))))
(define-move-vop move-sap-arg :move-arg
  (descriptor-reg sap-reg) (sap-reg))

;;; Use standard MOVE-ARG + coercion to move an untagged sap to a
;;; descriptor passing location.
(define-move-vop move-arg :move-arg
  (sap-reg) (descriptor-reg))

;;;; SAP-INT and INT-SAP

;;; The function SAP-INT is used to generate an integer corresponding
;;; to the system area pointer, suitable for passing to the kernel
;;; interfaces (which want all addresses specified as integers). The
;;; function INT-SAP is used to do the opposite conversion. The
;;; integer representation of a SAP is the byte offset of the SAP from
;;; the start of the address space.
(define-vop (sap-int)
  (:args (sap :scs (sap-reg) :target int))
  (:arg-types system-area-pointer)
  (:results (int :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate sap-int)
  (:policy :fast-safe)
  (:generator 1
    (move int sap)))
(define-vop (int-sap)
  (:args (int :scs (unsigned-reg) :target sap))
  (:arg-types unsigned-num)
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate int-sap)
  (:policy :fast-safe)
  (:generator 1
    (move sap int)))

;;;; SAP+ and SAP-

(define-vop ()
  (:translate sap+)
  (:args (ptr :scs (sap-reg) :target res
              :load-if (not (location= ptr res)))
         (offset :scs (signed-reg immediate)))
  (:arg-types system-area-pointer signed-num)
  (:results (res :scs (sap-reg) :from (:argument 0)
                 :load-if (not (location= ptr res))))
  (:result-types system-area-pointer)
  (:temporary (:sc signed-reg) temp)
  (:policy :fast-safe)
  (:generator 1
    (cond ((and (sc-is ptr sap-reg) (sc-is res sap-reg)
                (not (location= ptr res)))
           (sc-case offset
             (signed-reg
              (inst lea res (ea ptr offset)))
             (immediate
              (let ((value (tn-value offset)))
                (cond ((typep value '(signed-byte 32))
                       (inst lea res (ea value ptr)))
                      (t
                       (inst mov temp value)
                       (inst lea res (ea ptr temp))))))))
          (t
           (move res ptr)
           (sc-case offset
             (signed-reg
              (inst add res offset))
             (immediate
              (let ((value (tn-value offset)))
                (cond ((typep value '(signed-byte 32))
                       (inst add res (tn-value offset)))
                      (t
                       (inst mov temp value)
                       (inst add res temp))))))))))

(define-vop ()
  (:translate sap-)
  (:args (ptr1 :scs (sap-reg) :target res)
         (ptr2 :scs (sap-reg)))
  (:arg-types system-area-pointer system-area-pointer)
  (:policy :fast-safe)
  (:results (res :scs (signed-reg) :from (:argument 0)))
  (:result-types signed-num)
  (:generator 1
    (move res ptr1)
    (inst sub res ptr2)))

;;;; mumble-SYSTEM-REF and mumble-SYSTEM-SET

;; from 'llvm/projects/compiler-rt/lib/msan/msan.h':
;;  "#define MEM_TO_SHADOW(mem) (((uptr)(mem)) ^ 0x500000000000ULL)"
#+linux ; shadow space differs by OS
(defconstant msan-mem-to-shadow-xor-const #x500000000000)

#|
https://llvm.org/doxygen/MemorySanitizer_8cpp.html
/// "We load the shadow _after_ the application load,
/// and we store the shadow _before_ the app store."
|#

(defun emit-sap-ref (size insn modifier result ea node vop)
  (declare (ignorable node size vop))
  (cond
   #+linux
   ((and (sb-c:msan-unpoison sb-c:*compilation*) (policy node (> safety 0)))
    ;; Must not clobber TEMP-REG-TN with the load.
    (aver (not (location= temp-reg-tn result)))
    (inst lea temp-reg-tn ea)
    (sb-assem:inst* insn modifier result (ea temp-reg-tn))
    (inst xor temp-reg-tn (thread-slot-ea thread-msan-xor-constant-slot))
    ;; Per the documentation, shadow is tested _after_
    (let ((mask (sb-c::masked-memory-load-p vop))
          (good (gen-label))
          (nbytes (size-nbyte size))
          bad)
      ;; If the load is going to be masked, then we must only check the
      ;; shadow bits under the mask.
      (cond ((not mask)
             (inst cmp size (ea temp-reg-tn) 0))
            ((or (neq size :qword) (plausible-signed-imm32-operand-p mask))
             (inst test size (ea temp-reg-tn)
                   (ldb (byte (* 8 nbytes) 0) mask)))
            (t
             ;; Test two 32-bit chunks of the shadow memory since we don't
             ;; have an available register to load a 64-bit constant.
             (inst test :dword (ea temp-reg-tn) (ldb (byte 32 0) mask))
             (setq bad (gen-label))
             (inst jmp :ne bad)
             (inst test :dword (ea 4 temp-reg-tn) (ldb (byte 32 32) mask))))
      (inst jmp :e good)
      (when bad (emit-label bad))
      (inst break sb-vm:uninitialized-load-trap)
      ;; Encode the target size and register. If XMM register loads were sanitized,
      ;; then this would need some more bits to indicate the register file.
      (let ((scale (1- (integer-length nbytes))))
        (inst byte (logior (ash (tn-offset result) 2) scale)))
      (emit-label good)))
   (t
    (sb-assem::inst* insn modifier result ea))))

(defun emit-sap-set (size ea value result)
  #+linux
  (when (sb-c:msan-unpoison sb-c:*compilation*)
    (inst lea temp-reg-tn ea)
    (inst xor temp-reg-tn (thread-slot-ea thread-msan-xor-constant-slot))
    (inst mov size (ea temp-reg-tn) 0))
  (inst mov size ea value)
  (move result value))

(macrolet ((def-system-ref-and-set (ref-name
                                    set-name
                                    ref-insn
                                    sc
                                    type
                                    size)
             (let ((ref-name-c (symbolicate ref-name "-C"))
                   (set-name-c (symbolicate set-name "-C"))
                   (modifier (if (eq ref-insn 'mov) size `(,size :qword))))
               `(progn
                  (define-vop (,ref-name)
                    (:translate ,ref-name)
                    (:policy :fast-safe)
                    (:args (sap :scs (sap-reg))
                           (offset :scs (signed-reg)))
                    (:arg-types system-area-pointer signed-num)
                    (:results (result :scs (,sc)))
                    (:result-types ,type)
                    (:node-var node)
                    (:vop-var vop)
                    (:generator 3 (emit-sap-ref ,size ',ref-insn
                                                ',modifier result (ea sap offset) node vop)))
                  (define-vop (,ref-name-c)
                    (:translate ,ref-name)
                    (:policy :fast-safe)
                    (:args (sap :scs (sap-reg)))
                    (:arg-types system-area-pointer (:constant (signed-byte 32)))
                    (:info offset)
                    (:results (result :scs (,sc)))
                    (:result-types ,type)
                    (:node-var node)
                    (:vop-var vop)
                    (:generator 2 (emit-sap-ref ,size ',ref-insn
                                                ',modifier result (ea offset sap) node vop)))
                  (define-vop (,set-name)
                    (:translate ,set-name)
                    (:policy :fast-safe)
                    (:args (sap :scs (sap-reg) :to (:eval 0))
                           (offset :scs (signed-reg) :to (:eval 0))
                           (value :scs (,sc) :target result))
                    (:arg-types system-area-pointer signed-num ,type)
                    (:results (result :scs (,sc)))
                    (:result-types ,type)
                    (:generator 5 (emit-sap-set ,size (ea sap offset) value result)))
                  (define-vop (,set-name-c)
                    (:translate ,set-name)
                    (:policy :fast-safe)
                    (:args (sap :scs (sap-reg) :to (:eval 0))
                           (value :scs (,sc) :target result))
                    (:arg-types system-area-pointer (:constant (signed-byte 32)) ,type)
                    (:info offset)
                    (:results (result :scs (,sc)))
                    (:result-types ,type)
                    (:generator 4 (emit-sap-set ,size (ea offset sap) value result)))))))

  (def-system-ref-and-set sap-ref-8 %set-sap-ref-8 movzx
    unsigned-reg positive-fixnum :byte)
  (def-system-ref-and-set signed-sap-ref-8 %set-signed-sap-ref-8 movsx
    signed-reg tagged-num :byte)
  (def-system-ref-and-set sap-ref-16 %set-sap-ref-16 movzx
    unsigned-reg positive-fixnum :word)
  (def-system-ref-and-set signed-sap-ref-16 %set-signed-sap-ref-16 movsx
    signed-reg tagged-num :word)
  (def-system-ref-and-set sap-ref-32 %set-sap-ref-32 movzx
    unsigned-reg unsigned-num :dword)
  (def-system-ref-and-set signed-sap-ref-32 %set-signed-sap-ref-32 movsx
    signed-reg signed-num :dword)
  (def-system-ref-and-set sap-ref-64 %set-sap-ref-64 mov
    unsigned-reg unsigned-num :qword)
  (def-system-ref-and-set signed-sap-ref-64 %set-signed-sap-ref-64 mov
    signed-reg signed-num :qword)
  (def-system-ref-and-set sap-ref-sap %set-sap-ref-sap mov
    sap-reg system-area-pointer :qword)
  (def-system-ref-and-set sap-ref-lispobj %set-sap-ref-lispobj mov
    descriptor-reg * :qword))

;;;; SAP-REF-SINGLE and SAP-REF-DOUBLE

(macrolet ((def-system-ref-and-set (ref-fun res-sc res-type insn
                                            &aux (set-fun (symbolicate "%SET-" ref-fun)))
             `(progn
                (define-vop (,ref-fun)
                  (:translate ,ref-fun)
                  (:policy :fast-safe)
                  (:args (sap :scs (sap-reg))
                         (offset :scs (signed-reg)))
                  (:arg-types system-area-pointer signed-num)
                  (:results (result :scs (,res-sc)))
                  (:result-types ,res-type)
                  (:generator 5 (inst ,insn result (ea sap offset))))
                (define-vop (,(symbolicate ref-fun "-C"))
                  (:translate ,ref-fun)
                  (:policy :fast-safe)
                  (:args (sap :scs (sap-reg)))
                  (:arg-types system-area-pointer (:constant (signed-byte 32)))
                  (:info offset)
                  (:results (result :scs (,res-sc)))
                  (:result-types ,res-type)
                  (:generator 4 (inst ,insn result (ea offset sap))))
                (define-vop (,set-fun)
                  (:translate ,set-fun)
                  (:policy :fast-safe)
                  (:args (sap :scs (sap-reg) :to (:eval 0))
                         (offset :scs (signed-reg) :to (:eval 0))
                         (value :scs (,res-sc)))
                  (:arg-types system-area-pointer signed-num ,res-type)
                  (:results (result :scs (,res-sc)))
                  (:result-types ,res-type)
                  (:generator 5 (inst ,insn (ea sap offset) value) (move result value)))
                (define-vop (,(symbolicate set-fun "-C"))
                  (:translate ,set-fun)
                  (:policy :fast-safe)
                  (:args (sap :scs (sap-reg) :to (:eval 0))
                         (value :scs (,res-sc)))
                  (:arg-types system-area-pointer (:constant (signed-byte 32)) ,res-type)
                  (:info offset)
                  (:results (result :scs (,res-sc)))
                  (:result-types ,res-type)
                  (:generator 4 (inst ,insn (ea offset sap) value) (move result value))))))
  (def-system-ref-and-set sap-ref-single single-reg single-float movss)
  (def-system-ref-and-set sap-ref-double double-reg double-float movsd))

;;; noise to convert normal lisp data objects into SAPs

(define-vop (vector-sap)
  (:translate vector-sap)
  (:policy :fast-safe)
  (:args (vector :scs (descriptor-reg) :target sap))
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (let ((disp (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
      (if (location= sap vector)
          (inst add sap disp)
          (inst lea sap (ea disp vector))))))

;;; Compare and swap
(define-vop (signed-sap-cas-32)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg) :to (:eval 0))
         (offset :scs (signed-reg) :to (:eval 0))
         (oldval :scs (signed-reg) :target eax)
         (newval :scs (signed-reg) :to (:eval 0)))
  (:temporary (:sc unsigned-reg :offset eax-offset
                   :from (:argument 2) :to (:result 0)) eax)
  (:arg-types system-area-pointer signed-num signed-num signed-num)
  (:results (result :scs (signed-reg)))
  (:result-types signed-num)
  (:generator 5
    (inst mov :dword eax oldval)
    (inst cmpxchg :dword (ea sap offset) newval :lock)
    (inst movsx '(:dword :qword) result eax)))

