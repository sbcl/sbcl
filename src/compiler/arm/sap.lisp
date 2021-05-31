;;;; the ARM VM definition of SAP operations

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
  (:temporary (:sc non-descriptor-reg :offset ocfp-offset) pa-flag)
  (:results (res :scs (descriptor-reg)))
  (:note "SAP to pointer coercion")
  (:generator 20
    (with-fixed-allocation (res pa-flag sap-widetag sap-size)
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
       (store-stack-offset x fp y)))))

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
  (:arg-types system-area-pointer (:constant (unsigned-byte 8)))
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:policy :fast-safe)
  (:generator 1
    (inst add res ptr offset)))

(define-vop (pointer+-signed-c)
  (:translate sap+)
  (:args (ptr :scs (sap-reg)))
  (:info offset)
  (:arg-types system-area-pointer (:constant (integer -255 -1)))
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:policy :fast-safe)
  (:generator 1
    (inst sub res ptr (- offset))))

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
(macrolet ((def-system-ref-and-set
               ;; NOTE: The -C VOPs have been disabled, as the allowed
               ;; displacements for memory references vary by
               ;; instruction, are confusing to figure out, and might
               ;; be sign-magnitude encoded.  FIXME: Figure these
               ;; things out, and re-enable the VOPs.
               (ref-name set-name sc type size &key signed use-lip)
             `(progn
                   (define-vop (,ref-name)
                       (:translate ,ref-name)
                     (:policy :fast-safe)
                     (:args (sap :scs (sap-reg))
                            (offset :scs (signed-reg)))
                     (:arg-types system-area-pointer signed-num)
                     (:results (result :scs (,sc)))
                     (:result-types ,type)
                     ,@(when use-lip
                             '((:temporary (:sc interior-reg) lip)))
                     (:generator 5
                      ,@(when use-lip
                          '((inst add lip sap offset)))
                      (inst ,(ecase size
                                    (:byte (if signed 'ldrsb 'ldrb))
                                    (:short (if signed 'ldrsh 'ldrh))
                                    (:long 'ldr)
                                    (:single 'flds)
                                    (:double 'fldd))
                            result ,(if use-lip
                                        '(@ lip)
                                        '(@ sap offset)))))
                   #+(or)
                   (define-vop (,(symbolicate ref-name "-C"))
                     (:translate ,ref-name)
                     (:policy :fast-safe)
                     (:args (sap :scs (sap-reg)))
                     (:arg-types system-area-pointer (:constant (signed-byte 16)))
                     (:info offset)
                     (:results (result :scs (,sc)))
                     (:result-types ,type)
                     (:generator 4
                      (inst ,(ecase size
                                    (:byte (if signed 'ldrsb 'ldrb))
                                    (:short (if signed 'ldrsh 'ldrh))
                                    (:long 'ldr)
                                    (:single 'flds)
                                    (:double 'fldd))
                            result (@ sap offset))))
                   (define-vop (,set-name)
                     (:translate ,set-name)
                     (:policy :fast-safe)
                     (:args (value :scs (,sc))
                            (sap :scs (sap-reg))
                            (offset :scs (signed-reg)))
                     (:arg-types ,type system-area-pointer signed-num)
                     ,@(when use-lip
                             '((:temporary (:sc interior-reg) lip)))
                     (:generator 5
                      ,@(when use-lip
                          '((inst add lip sap offset)))
                      (inst ,(ecase size
                                    (:byte 'strb)
                                    (:short 'strh)
                                    (:long 'str)
                                    (:single 'fsts)
                                    (:double 'fstd))
                            value ,(if use-lip
                                       '(@ lip)
                                       '(@ sap offset)))))
                   #+(or)
                   (define-vop (,(symbolicate set-name "-C"))
                     (:translate ,set-name)
                     (:policy :fast-safe)
                     (:args (value :scs (,sc))
                            (sap :scs (sap-reg)))
                     (:arg-types ,type system-area-pointer (:constant (signed-byte 16)))
                     (:info offset)
                     (:generator 4
                      (inst ,(ecase size
                                    (:byte 'strb)
                                    (:short 'strh)
                                    (:long 'str)
                                    (:single 'fsts)
                                    (:double 'fstd))
                            value (@ sap offset)))))))
  (def-system-ref-and-set sap-ref-8 %set-sap-ref-8
    unsigned-reg positive-fixnum :byte :signed nil)
  (def-system-ref-and-set signed-sap-ref-8 %set-signed-sap-ref-8
    signed-reg tagged-num :byte :signed t)
  (def-system-ref-and-set sap-ref-16 %set-sap-ref-16
    unsigned-reg positive-fixnum :short :signed nil)
  (def-system-ref-and-set signed-sap-ref-16 %set-signed-sap-ref-16
    signed-reg tagged-num :short :signed t)
  (def-system-ref-and-set sap-ref-32 %set-sap-ref-32
    unsigned-reg unsigned-num :long :signed nil)
  (def-system-ref-and-set signed-sap-ref-32 %set-signed-sap-ref-32
    signed-reg signed-num :long :signed t)
  (def-system-ref-and-set sap-ref-sap %set-sap-ref-sap
    sap-reg system-area-pointer :long)
  (def-system-ref-and-set sap-ref-lispobj %set-sap-ref-lispobj
    descriptor-reg * :long)
  (def-system-ref-and-set sap-ref-single %set-sap-ref-single
    single-reg single-float :single :use-lip t)
  (def-system-ref-and-set sap-ref-double %set-sap-ref-double
    double-reg double-float :double :use-lip t))

;;; Noise to convert normal lisp data objects into SAPs.
(define-vop (vector-sap)
  (:translate vector-sap)
  (:policy :fast-safe)
  (:args (vector :scs (descriptor-reg)))
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (inst add sap vector
          (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))))
