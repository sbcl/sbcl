;;;; the Alpha VM definition of SAP operations

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; moves and coercions

;;; Move a tagged SAP to an untagged representation.
(define-vop (move-to-sap)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (sap-reg)))
  (:note "system area pointer indirection")
  (:generator 1
    (loadq y x sap-pointer-slot other-pointer-lowtag)))
(define-move-vop move-to-sap :move
  (descriptor-reg) (sap-reg))

;;; Move an untagged SAP to a tagged representation.
(define-vop (move-from-sap)
  (:args (x :scs (sap-reg) :target sap))
  (:temporary (:scs (sap-reg) :from (:argument 0)) sap)
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:results (y :scs (descriptor-reg)))
  (:note "system area pointer allocation")
  (:generator 20
    (move x sap)
    (with-fixed-allocation (y ndescr sap-widetag sap-size)
      (storeq sap y sap-pointer-slot other-pointer-lowtag))))
(define-move-vop move-from-sap :move
  (sap-reg) (descriptor-reg))

;;; Move untagged SAP values.
(define-vop (sap-move)
  (:args (x :target y
            :scs (sap-reg)
            :load-if (not (location= x y))))
  (:results (y :scs (sap-reg)
               :load-if (not (location= x y))))
  (:effects)
  (:affected)
  (:generator 0
    (move x y)))
(define-move-vop sap-move :move
  (sap-reg) (sap-reg))

;;; Move untagged SAP arguments/return-values.
(define-vop (move-sap-arg)
  (:args (x :target y
            :scs (sap-reg))
         (fp :scs (any-reg)
             :load-if (not (sc-is y sap-reg))))
  (:results (y))
  (:generator 0
    (sc-case y
      (sap-reg
       (move x y))
      (sap-stack
       (storeq x fp (tn-offset y))))))
(define-move-vop move-sap-arg :move-arg
  (descriptor-reg sap-reg) (sap-reg))

;;; Use standard MOVE-ARG + coercion to move an untagged sap to a
;;; descriptor passing location.
(define-move-vop move-arg :move-arg
  (sap-reg) (descriptor-reg))

;;;; SAP-INT and INT-SAP

(define-vop (sap-int)
  (:args (sap :scs (sap-reg) :target int))
  (:arg-types system-area-pointer)
  (:results (int :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate sap-int)
  (:policy :fast-safe)
  (:generator 1
    (move sap int)))

(define-vop (int-sap)
  (:args (int :scs (unsigned-reg) :target sap))
  (:arg-types unsigned-num)
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate int-sap)
  (:policy :fast-safe)
  (:generator 1
    (move int sap)))

;;;; POINTER+ and POINTER-

(define-vop (pointer+)
  (:translate sap+)
  (:args (ptr :scs (sap-reg))
         (offset :scs (signed-reg immediate)))
  (:arg-types system-area-pointer signed-num)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:policy :fast-safe)
  (:generator 1
    (sc-case offset
      (signed-reg
       (inst addq offset ptr res))
      (immediate
       (inst lda res (tn-value offset) ptr)))))

(define-vop (pointer-)
  (:translate sap-)
  (:args (ptr1 :scs (sap-reg))
         (ptr2 :scs (sap-reg)))
  (:arg-types system-area-pointer system-area-pointer)
  (:policy :fast-safe)
  (:results (res :scs (signed-reg)))
  (:result-types signed-num)
  (:generator 1
    (inst subq ptr1 ptr2 res)))

;;;; mumble-SYSTEM-REF and mumble-SYSTEM-SET

(macrolet ((def-system-ref-and-set
             (ref-name set-name sc type size &optional signed)
             (let ((ref-name-c (symbolicate ref-name "-C"))
                   (set-name-c (symbolicate set-name "-C")))
               `(progn
                  (define-vop (,ref-name)
                    (:translate ,ref-name)
                    (:policy :fast-safe)
                    (:args (object :scs (sap-reg) :target sap)
                           (offset :scs (signed-reg)))
                    (:arg-types system-area-pointer signed-num)
                    ,@(when (or (eq size :byte) (eq size :short))
                        `((:temporary (:sc non-descriptor-reg) temp)
                          (:temporary (:sc non-descriptor-reg) temp1)))
                    (:results (result :scs (,sc)))
                    (:result-types ,type)
                    (:temporary (:scs (sap-reg) :from (:argument 0)) sap)
                    (:generator 5
                                (inst addq object offset sap)
                                ,@(ecase size
                                    (:byte
                                     (if signed
                                         '((inst ldq_u temp 0 sap)
                                           (inst lda temp1 1 sap)
                                           (inst extqh temp temp1 temp)
                                           (inst sra temp 56 result))
                                       '((inst ldq_u temp 0 sap)
                                         (inst lda temp1 0 sap)
                                         (inst extbl temp temp1 result))))
                                    (:short
                                     (if signed
                                         '((inst ldq_u temp 0 sap)
                                           (inst lda temp1 0 sap)
                                           (inst extwl temp temp1 temp)
                                           (inst sll temp 48 temp)
                                           (inst sra temp 48 result))
                                       '((inst ldq_u temp 0 sap)
                                         (inst lda temp1 0 sap)
                                         (inst extwl temp temp1 result))))
                                    (:long
                                     `((inst ldl result 0 sap)
                                       ,@(unless signed
                                           '((inst mskll result 4 result)))))
                                    (:quad
                                     '((inst ldq result 0 sap)))
                                    (:single
                                     '((inst lds result 0 sap)))
                                    (:double
                                     '((inst ldt result 0 sap))))))
                  (define-vop (,ref-name-c)
                    (:translate ,ref-name)
                    (:policy :fast-safe)
                    (:args (object :scs (sap-reg)))
                    (:arg-types system-area-pointer
                                (:constant ,(if (eq size :double)
                                                ;; We need to be able to add 4.
                                                `(integer ,(- (ash 1 16))
                                                          ,(- (ash 1 16) 5))
                                              '(signed-byte 16))))
                    ,@(when (or (eq size :byte) (eq size :short))
                        `((:temporary (:scs (non-descriptor-reg)) temp)
                          (:temporary (:sc non-descriptor-reg) temp1)))
                    (:info offset)
                    (:results (result :scs (,sc)))
                    (:result-types ,type)
                    (:generator 4
                                ,@(ecase size
                                    (:byte
                                     (if signed
                                         '((inst ldq_u temp offset object)
                                           (inst lda temp1 (1+ offset) object)
                                           (inst extqh temp temp1 temp)
                                           (inst sra temp 56 result))
                                       '((inst ldq_u temp offset object)
                                         (inst lda temp1 offset object)
                                         (inst extbl temp temp1 result))))
                                    (:short
                                     (if signed
                                         '((inst ldq_u temp offset object)
                                           (inst lda temp1 offset object)
                                           (inst extwl temp temp1 temp)
                                           (inst sll temp 48 temp)
                                           (inst sra temp 48 result))
                                       '((inst ldq_u temp offset object)
                                         (inst lda temp1 offset object)
                                         (inst extwl temp temp1 result))))
                                    (:long
                                     `((inst ldl result offset object)
                                       ,@(unless signed
                                           '((inst mskll result 4 result)))))
                                    (:quad
                                     '((inst ldq result offset object)))
                                    (:single
                                     '((inst lds result offset object)))
                                    (:double
                                     '((inst ldt
                                             result
                                             (+ offset n-word-bytes)
                                             object))))))
                  (define-vop (,set-name)
                    (:translate ,set-name)
                    (:policy :fast-safe)
                    (:args (object :scs (sap-reg) :target sap)
                           (offset :scs (signed-reg))
                           (value :scs (,sc) :target result))
                    (:arg-types system-area-pointer signed-num ,type)
                    (:results (result :scs (,sc)))
                    (:result-types ,type)
                    (:temporary (:scs (sap-reg) :from (:argument 0)) sap)
                    ,@(when (or (eq size :byte) (eq size :short))
                        `((:temporary (:sc non-descriptor-reg) temp)
                          (:temporary (:sc non-descriptor-reg) temp1)
                          (:temporary (:sc non-descriptor-reg) temp2)))
                    (:generator 5
                                (inst addq object offset sap)
                                ,@(ecase size
                                    (:byte
                                     '((inst lda temp 0 sap)
                                       (inst ldq_u temp1 0 sap)
                                       (inst insbl value temp temp2)
                                       (inst mskbl temp1 temp temp1)
                                       (inst bis temp1 temp2 temp1)
                                       (inst stq_u temp1 0 sap)
                                       (inst move value result)))
                                    (:short
                                     '((inst lda temp 0 sap)
                                       (inst ldq_u temp1 0 sap)
                                       (inst mskwl temp1 temp temp1)
                                       (inst inswl value temp temp2)
                                       (inst bis temp1 temp2 temp)
                                       (inst stq_u temp 0 sap)
                                       (inst move value result)))
                                    (:long
                                     '((inst stl value 0 sap)
                                       (move value result)))
                                    (:quad
                                     '((inst stq value 0 sap)
                                       (move value result)))
                                    (:single
                                     '((unless (location= result value)
                                         (inst fmove value result))
                                       (inst sts value 0 sap)))
                                    (:double
                                     '((unless (location= result value)
                                         (inst fmove value result))
                                       (inst stt value 0 sap))))))
                  (define-vop (,set-name-c)
                    (:translate ,set-name)
                    (:policy :fast-safe)
                    (:args (object :scs (sap-reg))
                           (value :scs (,sc) :target result))
                    (:arg-types system-area-pointer
                                (:constant ,(if (eq size :double)
                                                ;; We need to be able to add 4.
                                                `(integer ,(- (ash 1 16))
                                                          ,(- (ash 1 16) 5))
                                              '(signed-byte 16)))
                                ,type)
                    ,@(when (or (eq size :byte) (eq size :short))
                        `((:temporary (:sc non-descriptor-reg) temp)
                          (:temporary (:sc non-descriptor-reg) temp1)
                          (:temporary (:sc non-descriptor-reg) temp2)))
                    (:info offset)
                    (:results (result :scs (,sc)))
                    (:result-types ,type)
                    (:generator 5
                                ,@(ecase size
                                    (:byte
                                     '((inst lda temp offset object)
                                       (inst ldq_u temp1 offset object)
                                       (inst insbl value temp temp2)
                                       (inst mskbl temp1 temp temp1)
                                       (inst bis temp1 temp2 temp1)
                                       (inst stq_u temp1 offset object)
                                       (inst move value result)))
                                    (:short
                                     '((inst lda temp offset object)
                                       (inst ldq_u temp1 offset object)
                                       (inst mskwl temp1 temp temp1)
                                       (inst inswl value temp temp2)
                                       (inst bis temp1 temp2 temp)
                                       (inst stq_u temp offset object)
                                       (inst move value result)))
                                    (:long
                                     '((inst stl value offset object)
                                       (move value result)))
                                    (:quad
                                     '((inst stq value offset object)
                                       (move value result)))
                                    (:single
                                     '((unless (location= result value)
                                         (inst fmove value result))
                                       (inst sts value offset object)))
                                    (:double
                                     '((unless (location= result value)
                                         (inst fmove value result))
                                       (inst stt value offset object))))))))))
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
  (def-system-ref-and-set sap-ref-64 %set-sap-ref-64
    unsigned-reg unsigned-num :quad nil)
  (def-system-ref-and-set signed-sap-ref-64 %set-signed-sap-ref-64
    signed-reg signed-num :quad t)
  (def-system-ref-and-set sap-ref-sap %set-sap-ref-sap
    sap-reg system-area-pointer :quad)
  (def-system-ref-and-set sap-ref-single %set-sap-ref-single
    single-reg single-float :single)
  (def-system-ref-and-set sap-ref-double %set-sap-ref-double
    double-reg double-float :double))

;;; noise to convert normal Lisp data objects into SAPs

(define-vop (vector-sap)
  (:translate vector-sap)
  (:policy :fast-safe)
  (:args (vector :scs (descriptor-reg)))
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (inst lda sap
          (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)
          vector)))
