;;;; the MIPS VM definition of SAP operations

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
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
  (:results (res :scs (descriptor-reg)))
  (:note "SAP to pointer coercion")
  (:generator 20
    (with-fixed-allocation (res pa-flag ndescr sap-widetag sap-size nil)
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
  (:generator 2 (inst addu res ptr offset)))
(define-vop (pointer+/c)
  (:translate sap+)
  (:args (ptr :scs (sap-reg)))
  (:info offset)
  (:arg-types system-area-pointer (:constant (signed-byte 16)))
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:policy :fast-safe)
  ;; This generator is only superficially identical to the above because
  ;; assembly code need not distinguish between 'addiu' and 'addu' even though
  ;; technically those are different opcodes. The native assembler also
  ;; doesn't mind if you use an immediate operand to 'addu'.
  ;; But adding a constant is "cheaper" only if it doesn't require an "li"
  ;; instruction to produce all the bits, which the native assembler will
  ;; synthesize using reg $at. If we declared a single vop that took a 2nd arg
  ;; of either immediate or signed-reg, then we've have to declare a temp
  ;; (because $at is not a permanent temp in lisp) usually going to waste.
  (:generator 1 (inst addu res ptr offset)))

(define-vop (pointer-)
  (:translate sap-)
  (:args (ptr1 :scs (sap-reg))
         (ptr2 :scs (sap-reg)))
  (:arg-types system-area-pointer system-area-pointer)
  (:policy :fast-safe)
  (:results (res :scs (signed-reg)))
  (:result-types signed-num)
  (:generator 1
    (inst subu res ptr1 ptr2)))

;;;; mumble-SYSTEM-REF and mumble-SYSTEM-SET
(macrolet ((def-system-ref-and-set
               (ref-name set-name sc type size &optional signed)
    `(progn
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
           (inst addu sap object offset)
           ,@(ecase size
               (:byte
                (if signed
                    '((inst lb result sap 0))
                    '((inst lbu result sap 0))))
                 (:short
                  (if signed
                      '((inst lh result sap 0))
                      '((inst lhu result sap 0))))
                 (:long
                  '((inst lw result sap 0)))
                 (:single
                  '((inst lwc1 result sap 0)))
                 (:double
                  (ecase *backend-byte-order*
                    (:big-endian
                     '((inst lwc1 result sap n-word-bytes)
                       (inst lwc1-odd result sap 0)))
                    (:little-endian
                     '((inst lwc1 result sap 0)
                       (inst lwc1-odd result sap n-word-bytes))))))
           (inst nop)))
       (define-vop (,(symbolicate ref-name "-C"))
         (:translate ,ref-name)
         (:policy :fast-safe)
         (:args (object :scs (sap-reg)))
         (:arg-types system-area-pointer
                     (:constant ,(if (eq size :double)
                                     ;; We need to be able to add 4.
                                     `(integer ,(- (ash 1 16))
                                               ,(- (ash 1 16) 5))
                                     '(signed-byte 16))))
         (:info offset)
         (:results (result :scs (,sc)))
         (:result-types ,type)
         (:generator 4
           ,@(ecase size
               (:byte
                (if signed
                    '((inst lb result object offset))
                    '((inst lbu result object offset))))
               (:short
                (if signed
                    '((inst lh result object offset))
                    '((inst lhu result object offset))))
               (:long
                '((inst lw result object offset)))
               (:single
                '((inst lwc1 result object offset)))
               (:double
                (ecase *backend-byte-order*
                  (:big-endian
                   '((inst lwc1 result object (+ offset n-word-bytes))
                     (inst lwc1-odd result object offset)))
                  (:little-endian
                   '((inst lwc1 result object offset)
                     (inst lwc1-odd result object (+ offset n-word-bytes)))))))
           (inst nop)))
       (define-vop (,set-name)
         (:translate ,set-name)
         (:policy :fast-safe)
         (:args (value :scs (,sc) :to :eval) ; VALUE has to conflict with SAP
                (object :scs (sap-reg) :target sap)
                (offset :scs (signed-reg)))
         (:arg-types ,type system-area-pointer signed-num)
         (:temporary (:scs (sap-reg) :from (:argument 1)) sap)
         (:generator 5
           (inst addu sap object offset)
           ,@(ecase size
               (:byte
                '((inst sb value sap 0)))
               (:short
                '((inst sh value sap 0)))
               (:long
                '((inst sw value sap 0)))
               (:single
                '((inst swc1 value sap 0)))
               (:double
                (ecase *backend-byte-order*
                  (:big-endian
                   '((inst swc1 value sap n-word-bytes)
                     (inst swc1-odd value sap 0)))
                  (:little-endian
                   '((inst swc1 value sap 0)
                     (inst swc1-odd value sap n-word-bytes))))))))
       (define-vop (,(symbolicate set-name "-C"))
         (:translate ,set-name)
         (:policy :fast-safe)
         (:args (value :scs (,sc))
                (object :scs (sap-reg)))
         (:arg-types ,type system-area-pointer
                     (:constant ,(if (eq size :double)
                                     ;; We need to be able to add 4.
                                     `(integer ,(- (ash 1 16))
                                               ,(- (ash 1 16) 5))
                                     '(signed-byte 16))))
         (:info offset)
         (:generator 4
           ,@(ecase size
               (:byte
                '((inst sb value object offset)))
               (:short
                '((inst sh value object offset)))
               (:long
                '((inst sw value object offset)))
               (:single
                '((inst swc1 value object offset)))
               (:double
                (ecase *backend-byte-order*
                  (:big-endian
                   '((inst swc1 value object (+ offset n-word-bytes))
                     (inst swc1-odd value object offset)))
                  (:little-endian
                   '((inst swc1 value object offset)
                     (inst swc1-odd value object (+ offset n-word-bytes))))))))))))
  (def-system-ref-and-set sap-ref-8 %set-sap-ref-8
    unsigned-reg positive-fixnum :byte nil)
  (def-system-ref-and-set signed-sap-ref-8 %set-signed-sap-ref-8
    signed-reg tagged-num :byte t)
  (def-system-ref-and-set sap-ref-16 %set-sap-ref-16
    unsigned-reg positive-fixnum :short nil)
  (def-system-ref-and-set signed-sap-ref-16 %set-signed-sap-ref-16
    signed-reg tagged-num :short t)
  (def-system-ref-and-set sap-ref-32 %set-sap-ref-32
    unsigned-reg unsigned-num :long nil)
  (def-system-ref-and-set signed-sap-ref-32 %set-signed-sap-ref-32
    signed-reg signed-num :long t)
  (def-system-ref-and-set sap-ref-sap %set-sap-ref-sap
    sap-reg system-area-pointer :long)
  (def-system-ref-and-set sap-ref-lispobj %set-sap-ref-lispobj
    descriptor-reg * :long)
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
    (inst addu sap vector
          (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))))
