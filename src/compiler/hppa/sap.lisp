;;;; the HPPA VM definition of SAP operations

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; Moves and coercions:

;;; Move a tagged SAP to an untagged representation.
(define-vop (move-to-sap)
  (:args (x :scs (any-reg descriptor-reg)))
  (:results (y :scs (sap-reg)))
  (:note "system area pointer indirection")
  (:generator 1
    (loadw y x sap-pointer-slot other-pointer-lowtag)))

(define-move-vop move-to-sap :move
  (descriptor-reg) (sap-reg))

;;; Move an untagged SAP to a tagged representation.
(define-vop (move-from-sap)
  (:args (sap :scs (sap-reg) :to :save))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:results (res :scs (descriptor-reg)))
  (:note "system area pointer allocation")
  (:generator 20
    (with-fixed-allocation (res nil ndescr sap-widetag sap-size nil)
      (storew sap res sap-pointer-slot other-pointer-lowtag))))

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
  (:effects)
  (:affected)
  (:generator 0
    (move x y)))

(define-move-vop sap-move :move
  (sap-reg) (sap-reg))

;;; Move untagged sap args/return-values.
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
       (move x y))
      (sap-stack
       (storew x fp (tn-offset y))))))

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
        (inst add ptr offset res))
      (immediate
        (cond
          ((and (< (tn-value offset) (ash 1 10))
                (> (tn-value offset) (- (ash 1 10))))
            (inst addi (tn-value offset) ptr res))
          (t
            (inst li (tn-value offset) res)
            (inst add ptr res res)))))))

(define-vop (pointer-)
  (:translate sap-)
  (:args (ptr1 :scs (sap-reg))
         (ptr2 :scs (sap-reg)))
  (:arg-types system-area-pointer system-area-pointer)
  (:policy :fast-safe)
  (:results (res :scs (signed-reg)))
  (:result-types signed-num)
  (:generator 1
    (inst sub ptr1 ptr2 res)))

;;;; mumble-SYSTEM-REF and mumble-SYSTEM-SET
(macrolet ((def-system-ref-and-set
          (ref-name set-name sc type size &optional signed)
  (let ((ref-name-c (symbolicate ref-name "-C"))
        (set-name-c (symbolicate set-name "-C")))
    `(progn
       (define-vop (,ref-name)
         (:translate ,ref-name)
         (:policy :fast-safe)
         (:args (object :scs (sap-reg))
                (offset :scs (signed-reg)))
         (:arg-types system-area-pointer signed-num)
         (:results (result :scs (,sc)))
         (:result-types ,type)
         (:generator 5
           (inst ,(ecase size
                    (:byte 'ldbx)
                    (:short 'ldhx)
                    (:long 'ldwx)
                    (:float 'fldx))
                 offset object result)
           ,@(when (and signed (not (eq size :long)))
               `((inst extrs result 31 ,(ecase size
                          (:byte 8)
                          (:short 16))
                       result)))))
       (define-vop (,ref-name-c)
         (:translate ,ref-name)
         (:policy :fast-safe)
         (:args (object :scs (sap-reg)))
         (:arg-types system-area-pointer
                     (:constant ,(if (eq size :float)
                                     '(signed-byte 5)
                                     '(signed-byte 14))))
         (:info offset)
         (:results (result :scs (,sc)))
         (:result-types ,type)
         (:generator 4
           (inst ,(ecase size
                    (:byte 'ldb)
                    (:short 'ldh)
                    (:long 'ldw)
                    (:float 'flds))
                 offset object result)
           ,@(when (and signed (not (eq size :long)))
               `((inst extrs result 31 ,(ecase size
                          (:byte 8)
                          (:short 16))
                       result)))))
       (define-vop (,set-name)
         (:translate ,set-name)
         (:policy :fast-safe)
         (:args (object :scs (sap-reg)
                        ,@(unless (eq size :float) '(:target sap)))
                (offset :scs (signed-reg))
                (value :scs (,sc) :target result))
         (:arg-types system-area-pointer signed-num ,type)
         (:results (result :scs (,sc)))
         (:result-types ,type)
         ,@(unless (eq size :float)
             '((:temporary (:scs (sap-reg) :from (:argument 0)) sap)))
         (:generator 5
           ,@(if (eq size :float)
                 `((inst fstx value offset object)
                   (unless (location= value result)
                     (inst funop :copy value result)))
                 `((inst add object offset sap)
                   (inst ,(ecase size (:byte 'stb) (:short 'sth) (:long 'stw))
                         value 0 sap)
                   (move value result)))))
       (define-vop (,set-name-c)
         (:translate ,set-name)
         (:policy :fast-safe)
         (:args (object :scs (sap-reg))
                (value :scs (,sc) :target result))
         (:arg-types system-area-pointer
                     (:constant ,(if (eq size :float)
                                     '(signed-byte 5)
                                     '(signed-byte 14)))
                     ,type)
         (:info offset)
         (:results (result :scs (,sc)))
         (:result-types ,type)
         (:generator 5
           ,@(if (eq size :float)
                 `((inst fsts value offset object)
                   (unless (location= value result)
                     (inst funop :copy value result)))
                 `((inst ,(ecase size (:byte 'stb) (:short 'sth) (:long 'stw))
                         value offset object)
                   (move value result)))))))))
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
  (def-system-ref-and-set sap-ref-single %set-sap-ref-single
    single-reg single-float :float)
  (def-system-ref-and-set sap-ref-double %set-sap-ref-double
    double-reg double-float :float))

;;; Noise to convert normal lisp data objects into SAPs.
(define-vop (vector-sap)
  (:translate vector-sap)
  (:policy :fast-safe)
  (:args (vector :scs (descriptor-reg)))
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (inst addi (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)
               vector sap)))
