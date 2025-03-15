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
  (:temporary (:scs (non-descriptor-reg) :offset lr-offset) lr)
  (:results (res :scs (descriptor-reg)))
  (:note "SAP to pointer coercion")
  (:generator 20
    (with-fixed-allocation (res lr sap-widetag sap-size
                            :store-type-code nil)
      (storew-pair lr 0 sap sap-pointer-slot tmp-tn))))

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
  (:arg-types system-area-pointer (:constant (satisfies add-sub-immediate-p)))
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:policy :fast-safe)
  (:generator 1
    (inst add res ptr offset)))

(define-vop (pointer+-signed-c)
  (:translate sap+)
  (:args (ptr :scs (sap-reg)))
  (:info offset)
  (:arg-types system-area-pointer (:constant (satisfies negative-add-sub-immediate-p)))
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
               (ref-name set-name sc type size &key signed)
             `(progn
                ,@(when (implements-cas-sap-ref ref-name)
                    (multiple-value-bind (load store cas)
                        (case size
                          (:byte  (values 'ldaxrb 'stlxrb 'casalb))
                          (:short (values 'ldaxrh 'stlxrh 'casalh))
                          (t      (values 'ldaxr  'stlxr 'casal)))
                      `((define-vop (,(symbolicate "CAS-" ref-name))
                          (:translate (cas ,ref-name))
                          (:policy :fast-safe)
                          (:args (oldval :scs (,sc zero))
                                 (newval :scs (,sc zero))
                                 (sap :scs (sap-reg))
                                 ;; This could accept an immediate 0 to avoid
                                 ;; the initial ADD but I don't care to optimize it out.
                                 (offset :scs (signed-reg)))
                          (:arg-types ,type ,type system-area-pointer signed-num)
                          (:temporary (:sc unsigned-reg) addr)
                          (:results (result :scs (,sc) :from :load))
                          (:result-types ,type)
                          (:generator 5
                           (inst add addr sap offset)
                           LOOP
                           (inst ,load ,(if (eq size :word) '(32-bit-reg result) 'result) addr)
                           ;; There is no instruction to perform signed load-acquire-exclusive,
                           ;; Therefore the loaded value has to get sign-extend in order for CMP not to fail
                           ;; on negatives. Alternatively there is (I think) a way to make the CMP
                           ;; do the right thing without this, but one way or another, the result
                           ;; requires sign-extension.
                          ,@(when (and signed (neq size :long))
                              (let ((bits (case size (:byte 8) (:short 16) (t 32))))
                                `((inst sbfm result result 0 ,(1- bits)))))
                           (inst cmp ,(if (eq size :long) 'result '(32-bit-reg result))
                                     ,(if (eq size :long) 'oldval '(32-bit-reg oldval)))
                           (inst b :ne EXIT)
                           (inst ,store (32-bit-reg tmp-tn)
                                 ,(if (eq size :word) '(32-bit-reg newval) 'newval) addr)
                           (inst cbnz (32-bit-reg tmp-tn) LOOP)
                           EXIT ; cargo-culted from WORD-INDEX-CAS
                           (inst clrex)
                           (inst dmb)))
                        (define-vop (,(symbolicate "CAS-" ref-name "-8.1"))
                          (:translate (cas ,ref-name))
                          (:policy :fast-safe)
                          (:args (oldval :scs (,sc) :target result)
                                 (newval :scs (,sc zero))
                                 (sap :scs (sap-reg))
                                 (offset :scs (signed-reg)))
                          (:arg-types ,type ,type system-area-pointer signed-num)
                          (:temporary (:sc unsigned-reg) addr)
                          (:results (result :scs (,sc) :from (:argument 0)))
                          (:result-types ,type)
                          (:guard (member :arm-v8.1 *backend-subfeatures*))
                          (:generator 3
                            (inst add addr sap offset)
                            (move result oldval)
                            (inst ,cas ,(if (eq size :word) '(32-bit-reg result) 'result)
                              ,(if (eq size :word) '(32-bit-reg newval) 'newval) addr)
                            ,@(when (and signed (neq size :long))
                                (let ((bits (case size (:byte 8) (:short 16) (t 32))))
                                  `((inst sbfm result result 0 ,(1- bits))))))))))
                (define-vop (,ref-name)
                  (:translate ,ref-name)
                  (:policy :fast-safe)
                  (:args (sap :scs (sap-reg))
                         (offset :scs (signed-reg)))
                  (:arg-types system-area-pointer signed-num)
                  (:results (result :scs (,sc)))
                  (:result-types ,type)
                  (:generator 5
                    (inst ,(case size
                             (:byte (if signed 'ldrsb 'ldrb))
                             (:short (if signed 'ldrsh 'ldrh))
                             (:word (if signed 'ldrsw 'ldr))
                             (t 'ldr))
                      ,(if (eq size :word)
                           '(32-bit-reg result)
                           'result)
                      (@ sap offset))))
                (define-vop (,set-name)
                  (:translate ,set-name)
                  (:policy :fast-safe)
                  (:args (value :scs (,sc zero))
                         (sap :scs (sap-reg))
                         (offset :scs (signed-reg)))
                  (:arg-types ,type system-area-pointer signed-num)
                  (:generator 5
                    (inst ,(case size
                             (:byte 'strb)
                             (:short 'strh)
                             (t 'str))
                      ,(if (eq size :word)
                           '(32-bit-reg value)
                           'value)
                      (@ sap offset))))
                (define-vop (,(symbolicate ref-name "-C"))
                  (:translate ,ref-name)
                  (:policy :fast-safe)
                  (:args (sap :scs (sap-reg)))
                  (:info offset)
                  (:arg-types system-area-pointer (:constant (satisfies ldr-str-offset-encodable)))
                  (:RESULTS (result :scs (,sc)))
                  (:result-types ,type)
                  (:generator 4
                    (inst ,(case size
                             (:byte (if signed 'ldrsb 'ldrb))
                             (:short (if signed 'ldrsh 'ldrh))
                             (:word (if signed 'ldrsw 'ldr))
                             (t 'ldr))
                      ,(if (eq size :word)
                           '(32-bit-reg result)
                           'result)
                      (@ sap offset))))
                (define-vop (,(symbolicate set-name "-C"))
                  (:translate ,set-name)
                  (:policy :fast-safe)
                  (:args (value :scs (,sc zero))
                         (sap :scs (sap-reg)))
                  (:info offset)
                  (:arg-types ,type system-area-pointer (:constant (satisfies ldr-str-offset-encodable)))
                  (:generator 4
                    (inst ,(case size
                             (:byte 'strb)
                             (:short 'strh)
                             (t 'str))
                      ,(if (eq size :word)
                           '(32-bit-reg value)
                           'value)
                      (@ sap offset)))))))
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
  (def-system-ref-and-set sap-ref-64 %set-sap-ref-64
    unsigned-reg unsigned-num :long :signed nil)
  (def-system-ref-and-set signed-sap-ref-64 %set-signed-sap-ref-64
    signed-reg signed-num :long :signed t)
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
    (inst add sap vector
          (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))))
