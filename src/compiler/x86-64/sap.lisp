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
  #+gs-seg (:temporary (:sc unsigned-reg :offset 15) thread-tn)
  (:note "SAP to pointer coercion")
  (:node-var node)
  (:generator 20
    (alloc-other sap-widetag sap-size res node nil thread-tn)
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
       (if (= (tn-offset fp) rsp-offset)
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
  (:args (ptr :scs (sap-reg sap-stack) :target res :load-if nil)
         (offset :scs (signed-reg signed-stack immediate)))
  (:arg-types system-area-pointer signed-num)
  (:results (res :scs (sap-reg sap-stack) :load-if nil))
  (:vop-var vop)
  (:result-types system-area-pointer)
  (:temporary (:sc signed-reg) temp) ; TODO: add an :unused-if on this
  (:policy :fast-safe)
  (:generator 1
    (emit-inline-add-sub 'add ptr offset res temp vop 'identity)))

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

(defun offset-needs-temp (offset)
  (if (or (sc-is offset signed-reg)
          (typep (tn-value offset) '(signed-byte 32)))
      nil
      t))

(defun sap+offset-to-ea (sap offset temp)
  (multiple-value-bind (disp index)
      (cond ((sc-is offset signed-reg)
             (values 0 offset))
            ((typep (tn-value offset) '(signed-byte 32))
             (values (tn-value offset) nil))
            (t
             ;; Seriously who uses immediate offsets exceeding an int32?
             (aver temp)
             (inst mov temp (tn-value offset))
             (values 0 temp)))
    (ea disp sap index)))

(defun emit-sap-ref (size insn modifier result sap offset node vop temp)
  (declare (ignorable node size vop temp))
  (cond
   ;; MSAN as implemented can't correctly interact with the C sanitizer because
   ;; C msan shadow memory indicates bit-for-bit whether the user-visible memory
   ;; was written, whereas in Lisp we only have byte granularity. When Lisp users
   ;; need to read bit fields they have to mask off bits explicitly after doing
   ;; a 1, 2, 4, or 8-byte load. Unfortunately that load could spuriously report
   ;; that it was uninitialized if only _part_ was uninitialized, but we didn't
   ;; want to examine the uninitialized bits. The problem is, it doesn't know that.
   ;; Even though DEFINE-ALIEN-TYPE conveys the semantics of sub-byte fields, the
   ;; compiler is itself unable to extract partial bytes.
   ;; (You get "cannot extract 4-bit integers" for example)
   ;; So unfortunately, because the IR can't convey what we mean, we have to do
   ;; a larger load than wanted, which may fail.
   #+nil
   ((and (sb-c:msan-unpoison sb-c:*compilation*) (policy node (> safety 0)))
    ;; Must not clobber TEMP with the load.
    (aver (not (location= temp result)))
    (inst lea temp ea)
    (sb-assem:inst* insn modifier result (ea temp))
    (inst xor temp (thread-slot-ea thread-msan-xor-constant-slot))
    ;; Per the documentation, shadow is tested _after_
    (let ((mask (sb-c::masked-memory-load-p vop))
          (good (gen-label))
          (nbytes (size-nbyte size))
          bad)
      ;; If the load is going to be masked, then we must only check the
      ;; shadow bits under the mask.
      (cond ((not mask)
             (inst cmp size (ea temp) 0))
            ((or (neq size :qword) (plausible-signed-imm32-operand-p mask))
             (inst test size (ea temp)
                   (ldb (byte (* 8 nbytes) 0) mask)))
            (t
             ;; Test two 32-bit chunks of the shadow memory since we don't
             ;; have an available register to load a 64-bit constant.
             (inst test :dword (ea temp) (ldb (byte 32 0) mask))
             (setq bad (gen-label))
             (inst jmp :ne bad)
             (inst test :dword (ea 4 temp) (ldb (byte 32 32) mask))))
      (inst jmp :e good)
      (when bad (emit-label bad))
      (inst break sb-vm:uninitialized-load-trap)
      ;; Encode the target size and register. If XMM register loads were sanitized,
      ;; then this would need some more bits to indicate the register file.
      (let ((scale (1- (integer-length nbytes))))
        (inst byte (logior (ash (tn-offset result) 2) scale)))
      (emit-label good)))
   (t
    (let ((ea (sap+offset-to-ea sap offset temp)))
      (sb-assem:inst* insn modifier result ea)))))

(defun emit-sap-set (size sap offset value temp
                          &aux (ea (sap+offset-to-ea sap offset temp)))
  #+linux
  (when (sb-c:msan-unpoison sb-c:*compilation*)
    (inst lea temp ea)
    (inst xor temp (thread-slot-ea thread-msan-xor-constant-slot))
    (inst mov size (ea temp) 0))
  (when (sc-is value constant immediate)
    (cond ((plausible-signed-imm32-operand-p (tn-value value))
           (setq value (tn-value value)))
          (t
           (aver (not (offset-needs-temp offset)))
           (inst mov temp (tn-value value))
           (setq value temp))))
  (inst mov size ea value))

(defun emit-cas-sap-ref (size signedp sap offset oldval newval result rax temp)
  (let ((ea (sap+offset-to-ea sap offset temp)))
    (cond ((sc-is oldval immediate constant)
           (inst mov rax (tn-value oldval)))
          ((not (location= oldval rax))
           (inst mov (if (eq size :qword) :qword :dword) rax oldval)))
    (inst cmpxchg size :lock ea newval)
    (cond ((and signedp (neq size :qword)) ; MOVSX is required no matter what
           (inst movsx `(,size :qword) result rax))
          ((not (location= result rax))
           (inst mov (if (eq size :qword) :qword :dword) result rax)))))

;;; Note: these could potentially be arranged to have only one vop per
;;; result storage class. In particular, sap-ref-{8,16,32} can all produce tagged-num.
;;; The vop can examine the node to see which function it translates
;;; and select the appropriate modifier to movzx or movsx. I'm unsure if that's better.
(macrolet ((def-system-ref-and-set (ref-name
                                    set-name
                                    ref-insn
                                    sc
                                    type
                                    size)
             (let ((value-scs (cond ((member ref-name '(sap-ref-64 signed-sap-ref-64))
                                     `(,sc constant immediate))
                                    ((not (member ref-name '(sap-ref-single sap-ref-sap
                                                             sap-ref-lispobj)))
                                     `(,sc immediate))
                                    (t
                                     `(,sc))))
                   (modifier (if (eq ref-insn 'mov)
                                 size
                                 `(,size ,(if (eq ref-insn 'movzx) :dword :qword)))))
               `(progn
                  ,@(when (implements-cas-sap-ref ref-name)
                      `((define-vop (,(symbolicate "CAS-" ref-name))
                          (:translate (cas ,ref-name))
                          (:policy :fast-safe)
                          (:args (oldval :scs ,value-scs :target rax)
                                 (newval :scs ,(remove 'immediate value-scs))
                                 (sap :scs (sap-reg))
                                 (offset :scs (signed-reg immediate)))
                          (:arg-types ,type ,type system-area-pointer signed-num)
                          (:results (result :scs (,sc)))
                          (:result-types ,type)
                          (:temporary (:sc unsigned-reg :offset rax-offset
                                       :from (:argument 0) :to :result) rax)
                          (:temporary (:sc unsigned-reg) temp)
                          (:generator 3
                            (emit-cas-sap-ref ',size ,(eq sc 'signed-reg)
                                              sap offset oldval newval result rax temp)))))
                  (define-vop (,ref-name)
                    (:translate ,ref-name)
                    (:policy :fast-safe)
                    (:args (sap :scs (sap-reg))
                           (offset :scs (signed-reg immediate)))
                    (:arg-types system-area-pointer signed-num)
                    (:results (result :scs (,sc)))
                    (:result-types ,type)
                    (:node-var node)
                    (:vop-var vop)
                    (:temporary (:sc unsigned-reg :offset rax-offset
                                 :unused-if (and (not (sb-c:msan-unpoison sb-c:*compilation*))
                                                 (not (offset-needs-temp offset))))
                                temp)
                    (:generator 3 (emit-sap-ref ,size ',ref-insn
                                                ',modifier result sap offset node vop temp)))
                  (define-vop (,set-name)
                    (:translate ,set-name)
                    (:policy :fast-safe)
                    (:args (value :scs ,value-scs)
                           (sap :scs (sap-reg))
                           (offset :scs (signed-reg)))
                    (:arg-types ,type system-area-pointer signed-num)
                    ;; In theory TEMP could assist with loading either OFFSET or VALUE, but
                    ;; there is an AVER that it doesn't actually get used for both.
                    (:temporary (:sc unsigned-reg) temp)
                    (:generator 5
                      (emit-sap-set ,size sap offset value temp)))))))

  (def-system-ref-and-set sap-ref-8 %set-sap-ref-8 movzx
    unsigned-reg positive-fixnum :byte)
  (def-system-ref-and-set signed-sap-ref-8 %set-signed-sap-ref-8 movsx
    signed-reg tagged-num :byte)
  (def-system-ref-and-set sap-ref-16 %set-sap-ref-16 movzx
    unsigned-reg positive-fixnum :word)
  (def-system-ref-and-set signed-sap-ref-16 %set-signed-sap-ref-16 movsx
    signed-reg tagged-num :word)
  (def-system-ref-and-set sap-ref-32 %set-sap-ref-32 mov
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

(macrolet ((def-system-ref-and-set (ref-fun res-sc res-type insn cas
                                            &aux (set-fun (symbolicate "%SET-" ref-fun)))
             `(progn
                (define-vop (,ref-fun)
                  (:translate ,ref-fun)
                  (:policy :fast-safe)
                  (:args (sap :scs (sap-reg))
                         (offset :scs (signed-reg immediate)))
                  (:arg-types system-area-pointer signed-num)
                  (:results (result :scs (,res-sc)))
                  (:result-types ,res-type)
                  (:generator 1 (inst ,insn result (sap+offset-to-ea sap offset nil))))
                (define-vop (,set-fun)
                  (:translate ,set-fun)
                  (:policy :fast-safe)
                  (:args (value :scs (,res-sc immediate))
                         (sap :scs (sap-reg))
                         (offset :scs (signed-reg immediate)))
                  (:arg-types ,res-type system-area-pointer signed-num)
                  (:generator 1 (inst ,insn (sap+offset-to-ea sap offset nil) value)))
                (define-vop (,(symbolicate "CAS-" ref-fun))
                  (:translate (cas ,ref-fun))
                  (:policy :fast-safe)
                  ;; old and new could directly accept descriptor-reg
                  ;; but I doubt that CAS on floats sees enough usage to care.
                  (:args (oldval :scs (,res-sc))
                         (newval :scs (,res-sc))
                         (sap :scs (sap-reg))
                         (offset :scs (signed-reg immediate)))
                  (:arg-types ,res-type ,res-type system-area-pointer signed-num)
                  (:results (result :scs (,res-sc)))
                  (:result-types ,res-type)
                  (:temporary (:sc unsigned-reg :offset rax-offset) rax)
                  (:temporary (:sc unsigned-reg) newval-temp)
                  (:generator 3 ,@cas)))))
  (def-system-ref-and-set sap-ref-single single-reg single-float movss
    ((inst movd rax oldval)
     (inst movd newval-temp newval)
     (inst cmpxchg :dword :lock (sap+offset-to-ea sap offset nil) newval-temp)
     (inst movd result rax)))
  (def-system-ref-and-set sap-ref-double double-reg double-float movsd
    ((inst movq rax oldval)
     (inst movq newval-temp newval)
     (inst cmpxchg :lock (sap+offset-to-ea sap offset nil) newval-temp)
     (inst movq result rax))))

;;; noise to convert normal lisp data objects into SAPs

(define-vop (vector-sap)
  (:translate vector-sap)
  (:policy :fast-safe)
  (:args (vector :scs (descriptor-reg) :target sap))
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (let ((disp (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
      (flet ((2-operand-add (dst src)
               (cond ((eql src 1) (inst inc dst))
                     (t (inst add dst src)))))
        (if (location= sap vector)
            (2-operand-add sap disp)
            (inst lea sap (ea disp vector)))))))
