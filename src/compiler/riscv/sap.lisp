;;;; SAP operations for the RISC-V VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;;; Moves and coercions:

;;; Move a tagged SAP to an untagged representation.
(define-vop (move-to-sap)
  (:args (x :scs (any-reg descriptor-reg)))
  (:results (y :scs (sap-reg)))
  (:note "pointer to SAP coercion")
  (:generator 1
    (loadw y x sap-pointer-slot other-pointer-lowtag)))

(define-move-vop move-to-sap :move
  (descriptor-reg) (sap-reg))

;;; Move an untagged SAP to a tagged representation.
(define-vop (move-from-sap)
  (:args (sap :scs (sap-reg) :to :save))
  (:temporary (:sc non-descriptor-reg) pa-flag)
  (:results (res :scs (descriptor-reg)))
  (:note "SAP to pointer coercion")
  (:generator 20
    (with-fixed-allocation (res pa-flag sap-widetag sap-size)
      (storew sap res sap-pointer-slot other-pointer-lowtag))))

(define-move-vop move-from-sap :move
  (sap-reg) (descriptor-reg))

;;; Move untagged SAP values.
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

;;; Move untagged SAP arguments/return-values.
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
       (storew x fp (tn-offset y))))))

(define-move-vop move-sap-arg :move-arg
  (descriptor-reg sap-reg) (sap-reg))

;;; Use standard MOVE-ARG + coercion to move an untagged SAP to a
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

;;;; POINTER+ and POINTER-
(define-vop (pointer+)
  (:translate sap+)
  (:args (ptr :scs (sap-reg))
         (offset :scs (signed-reg)))
  (:arg-types system-area-pointer signed-num)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:policy :fast-safe)
  (:generator 2
    (inst add res ptr offset)))

(define-vop (pointer+-unsigned-c)
  (:translate sap+)
  (:args (ptr :scs (sap-reg)))
  (:info offset)
  (:arg-types system-area-pointer (:constant short-immediate))
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:policy :fast-safe)
  (:generator 1
    (inst addi res ptr offset)))

(define-vop (pointer-)
  (:translate sap-)
  (:args (ptr1 :scs (sap-reg))
         (ptr2 :scs (sap-reg)))
  (:arg-types system-area-pointer system-area-pointer)
  (:policy :fast-safe)
  (:results (res :scs (signed-reg)))
  (:result-types signed-num)
  (:generator 1
    (inst sub res ptr1 ptr2)))

;;;; mumble-SYSTEM-REF and mumble-SYSTEM-SET
(defun sap-ref (result sap offset signed size)
  (ecase size
    (:byte
     (if signed
         (inst lb result sap offset)
         (inst lbu result sap offset)))
    (:short
     (if signed
         (inst lh result sap offset)
         (inst lhu result sap offset)))
    (:word
     (if signed
         (inst lw result sap offset)
         (inst #-64-bit lw #+64-bit lwu result sap offset)))
    #+64-bit
    (:dword
     (inst ld result sap offset))
    (:single
     (inst fload :single result sap offset))
    (:double
     (inst fload :double result sap offset))))

(defun sap-set (value sap offset size)
  (ecase size
    (:byte
     (inst sb value sap offset))
    (:short
     (inst sh value sap offset))
    (:word
     (inst sw value sap offset))
    #+64-bit
    (:dword
     (inst sd value sap offset))
    (:single
     (inst fstore :single value sap offset))
    (:double
     (inst fstore :double value sap offset))))

(macrolet ((def-system-ref-and-set
               (ref-name set-name sc type size &key signed)
             `(progn
                  ,@(when (member ref-name '(sap-ref-32 sap-ref-64 signed-sap-ref-64
                                             sap-ref-lispobj sap-ref-sap))
                    (multiple-value-bind (load store)
                        (ecase size
                          (:word  (values 'lr.w 'sc.w))
                          (:dword (values 'lr.d 'sc.d)))
                      ;; riscv baseline architecture does not have cmpxchg on 1-byte or 2-byte int.
                      ;; There is an architectural extension that can do it
                      ;; https://github.com/riscv/riscv-isa-manual/blob/main/src/zabha.adoc
                      ;; I don't feel up to the task of emulating it in software.
                      `((define-vop (,(symbolicate "CAS-" ref-name))
                          (:translate (cas ,ref-name))
                          (:policy :fast-safe)
                          (:args (oldval :scs (,sc))
                                 (newval :scs (,sc))
                                 (sap :scs (sap-reg))
                                 (offset :scs (signed-reg)))
                          (:arg-types ,type ,type system-area-pointer signed-num)
                          (:temporary (:sc unsigned-reg) addr temp)
                          (:results (result :scs (,sc) :from :load))
                          (:result-types ,type)
                          (:generator 5
                           (inst add addr sap offset)
                           LOOP
                           (inst ,load result addr :aq)
                           (inst bne result oldval EXIT)
                           (inst ,store temp newval addr :aq :rl)
                           (inst bne temp zero-tn LOOP)
                           EXIT)))))
                  (define-vop (,ref-name)
                    (:translate ,ref-name)
                    (:policy :fast-safe)
                    (:args (object :scs (sap-reg) :target sap)
                           (offset :scs (signed-reg)))
                    (:arg-types system-area-pointer signed-num)
                    (:results (result :scs (,sc)))
                    (:result-types ,type)
                    (:temporary (:scs (sap-reg) :from (:argument 0)) sap)
                    (:generator 5
                      (inst add sap object offset)
                      (sap-ref result sap 0 ,signed ,size)))
                  (define-vop (,(symbolicate ref-name "-C"))
                    (:translate ,ref-name)
                    (:policy :fast-safe)
                    (:args (sap :scs (sap-reg)))
                    (:arg-types system-area-pointer
                                (:constant short-immediate))
                    (:info offset)
                    (:results (result :scs (,sc)))
                    (:result-types ,type)
                    (:generator 4
                      (sap-ref result sap offset ,signed ,size)))
                  (define-vop (,set-name)
                    (:translate ,set-name)
                    (:policy :fast-safe)
                    ;; this is untested, but it matches the MIPS vop
                    (:args (value :scs (,sc) :to :eval) ; VALUE has to conflict with SAP
                           (object :scs (sap-reg) :target sap)
                           (offset :scs (signed-reg)))
                    (:arg-types ,type system-area-pointer signed-num)
                    (:temporary (:scs (sap-reg) :from (:argument 1)) sap)
                    (:generator 5
                      (inst add sap object offset)
                      (sap-set value sap 0 ,size)))
                  (define-vop (,(symbolicate set-name "-C"))
                    (:translate ,set-name)
                    (:policy :fast-safe)
                    (:args (value :scs (,sc))
                           (sap :scs (sap-reg)))
                    (:arg-types ,type system-area-pointer (:constant short-immediate))
                    (:info offset)
                    (:generator 4
                      (sap-set value sap offset ,size))))))
  (def-system-ref-and-set sap-ref-8 %set-sap-ref-8
    unsigned-reg positive-fixnum :byte :signed nil)
  (def-system-ref-and-set signed-sap-ref-8 %set-signed-sap-ref-8
    signed-reg tagged-num :byte :signed t)
  (def-system-ref-and-set sap-ref-16 %set-sap-ref-16
    unsigned-reg positive-fixnum :short :signed nil)
  (def-system-ref-and-set signed-sap-ref-16 %set-signed-sap-ref-16
    signed-reg tagged-num :short :signed t)
  (def-system-ref-and-set sap-ref-32 %set-sap-ref-32
    unsigned-reg unsigned-num :word :signed nil)
  (def-system-ref-and-set signed-sap-ref-32 %set-signed-sap-ref-32
    signed-reg signed-num :word :signed t)
  #+64-bit
  (def-system-ref-and-set sap-ref-64 %set-sap-ref-64
    unsigned-reg unsigned-num :dword :signed nil)
  #+64-bit
  (def-system-ref-and-set signed-sap-ref-64 %set-signed-sap-ref-64
    signed-reg signed-num :dword :signed t)
  (def-system-ref-and-set sap-ref-sap %set-sap-ref-sap
    sap-reg system-area-pointer #-64-bit :word #+64-bit :dword)
  (def-system-ref-and-set sap-ref-lispobj %set-sap-ref-lispobj
    descriptor-reg * #-64-bit :word #+64-bit :dword)
  (def-system-ref-and-set sap-ref-single %set-sap-ref-single
    single-reg single-float :single)
  (def-system-ref-and-set sap-ref-double %set-sap-ref-double
    double-reg double-float :double))

;;; Noise to convert normal lisp data objects into SAPs.
(define-vop (vector-sap)
  (:translate vector-sap)
  (:policy :fast-safe)
  (:args (vector :scs (descriptor-reg)))
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (inst addi sap vector
          (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))))
