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

;;;; POINTER+ and POINTER-

(define-vop (pointer+)
  (:translate sap+)
  (:args (ptr :scs (sap-reg) :target res
              :load-if (not (location= ptr res)))
         (offset :scs (signed-reg immediate)))
  (:arg-types system-area-pointer signed-num)
  (:results (res :scs (sap-reg) :from (:argument 0)
                 :load-if (not (location= ptr res))))
  (:result-types system-area-pointer)
  (:policy :fast-safe)
  (:generator 1
    (cond ((and (sc-is ptr sap-reg) (sc-is res sap-reg)
                (not (location= ptr res)))
           (sc-case offset
             (signed-reg
              (inst lea res (make-ea :dword :base ptr :index offset)))
             (immediate
              (inst lea res (make-ea :dword :base ptr
                                     :disp (tn-value offset))))))
          (t
           (move res ptr)
           (sc-case offset
             (signed-reg
              (inst add res offset))
             (immediate
              (inst add res (tn-value offset))))))))

(define-vop (pointer-)
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

(defun emit-cas-sap-ref (size signedp sap offset oldval newval result eax)
  (move eax oldval)
  (inst cmpxchg (make-ea size :base sap :index offset :scale 1)
        (case size
          ;; FIXME: These cases are *very* broken and I don't know how to
          ;; make them do what I want. The underlying issue that not all GPRs
          ;; have an adressable byte-sized and word-sized register. So this call
          ;; to MAKE-RANDOM-TN something that doesn't match the low
          ;; 8 or 16 bits of the full-sized register, apparently. The thing is,
          ;; all we need is for the instruction emitter to emit the correct bits,
          ;; because the source data are actually in the right place in the
          ;; full register. Unfortunately, if I make no attempt to change the size
          ;; of the source register to match the EA, we get an error in
          ;; MATCHING-OPERAND-SIZE. It's entirely a problem in our x86 assembler.
          ;; There is no syntax to do what this should do.
          (:byte (make-random-tn (sc-or-lose 'byte-reg) (tn-offset newval)))
          (:word (make-random-tn (sc-or-lose 'word-reg) (tn-offset newval)))
          (t newval))
        :lock)
  (if (and signedp (neq size :dword))
      (inst movsx result (ecase size (:byte al-tn) (:word ax-tn)))
      (move result eax)))

(macrolet ((def-system-ref-and-set (ref-name
                                    set-name
                                    sc
                                    type
                                    size
                                    &optional signed)
             (let ((temp-sc (symbolicate size "-REG")))
               `(progn
                  ,@(when (implements-cas-sap-ref ref-name)
                      `((define-vop (,(symbolicate "CAS-" ref-name))
                          (:translate (cas ,ref-name))
                          (:policy :fast-safe)
                          (:args (oldval :scs (,sc) :target eax)
                                 (newval :scs (,sc))
                                 (sap :scs (sap-reg))
                                 (offset :scs (signed-reg)))
                          (:arg-types ,type ,type system-area-pointer signed-num)
                          (:results (result :scs (,sc)))
                          (:result-types ,type)
                          (:temporary (:sc unsigned-reg :offset eax-offset
                                       :from (:argument 0) :to :result) eax)
                          (:generator 3
                            (emit-cas-sap-ref ,size ,signed sap offset oldval newval result eax)))))
                  (define-vop (,ref-name)
                    (:translate ,ref-name)
                    (:policy :fast-safe)
                    (:args (sap :scs (sap-reg))
                           (offset :scs (signed-reg immediate)))
                    (:arg-types system-area-pointer signed-num)
                    (:results (result :scs (,sc)))
                    (:result-types ,type)
                    (:generator 5
                      ,(let ((mov-inst (cond
                                         ((eq size :dword) 'mov)
                                         (signed 'movsx)
                                         (t 'movzx))))
                         `(sc-case offset
                            (immediate
                             (inst ,mov-inst result
                                   (make-ea ,size :base sap
                                            :disp (tn-value offset))))
                            (t (inst ,mov-inst result
                                     (make-ea ,size :base sap
                                              :index offset)))))))
                  (define-vop (,set-name)
                    (:translate ,set-name)
                    (:policy :fast-safe)
                    (:args (value :scs (,sc))
                           (sap :scs (sap-reg))
                           (offset :scs (signed-reg immediate)))
                    (:arg-types ,type system-area-pointer signed-num)
                    ,@(unless (eq size :dword)
                        `((:temporary (:sc ,temp-sc :offset eax-offset) temp)))
                    (:generator 5
                      ,@(unless (eq size :dword) `((move eax-tn value)))
                      (inst mov (sc-case offset
                                         (immediate
                                          (make-ea ,size :base sap
                                                   :disp (tn-value offset)))
                                         (t (make-ea ,size
                                                     :base sap
                                                     :index offset)))
                            ,(if (eq size :dword) 'value 'temp))))))))
  (def-system-ref-and-set sap-ref-8 %set-sap-ref-8
    unsigned-reg positive-fixnum :byte nil)
  (def-system-ref-and-set signed-sap-ref-8 %set-signed-sap-ref-8
    signed-reg tagged-num :byte t)
  (def-system-ref-and-set sap-ref-16 %set-sap-ref-16
    unsigned-reg positive-fixnum :word nil)
  (def-system-ref-and-set signed-sap-ref-16 %set-signed-sap-ref-16
    signed-reg tagged-num :word t)
  (def-system-ref-and-set sap-ref-32 %set-sap-ref-32
    unsigned-reg unsigned-num :dword nil)
  (def-system-ref-and-set signed-sap-ref-32 %set-signed-sap-ref-32
    signed-reg signed-num :dword t)
  (def-system-ref-and-set sap-ref-sap %set-sap-ref-sap
    sap-reg system-area-pointer :dword)
  (def-system-ref-and-set sap-ref-lispobj %set-sap-ref-lispobj
    descriptor-reg * :dword))

;;;; SAP-REF-DOUBLE

(define-vop ()
  (:translate sap-ref-double)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg))
         (offset :scs (signed-reg immediate)))
  (:arg-types system-area-pointer signed-num)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:generator 5
     (sc-case offset
       (immediate
        (with-empty-tn@fp-top(result)
          (inst fldd (make-ea :dword :base sap :disp (tn-value offset)))))
       (t
        (with-empty-tn@fp-top(result)
          (inst fldd (make-ea :dword :base sap :index offset)))))))

(define-vop ()
  (:translate %set-sap-ref-double)
  (:policy :fast-safe)
  (:args (value :scs (double-reg))
         (sap :scs (sap-reg))
         (offset :scs (signed-reg)))
  (:arg-types double-float system-area-pointer signed-num)
  (:generator 5
    (with-tn@fp-top (value)
      (inst fstd (make-ea :dword :base sap :index offset)))))

;;;; SAP-REF-SINGLE

(define-vop ()
  (:translate sap-ref-single)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg))
         (offset :scs (signed-reg immediate)))
  (:arg-types system-area-pointer signed-num)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:generator 5
     (sc-case offset
       (immediate
        (with-empty-tn@fp-top(result)
          (inst fld (make-ea :dword :base sap :disp (tn-value offset)))))
       (t
        (with-empty-tn@fp-top(result)
          (inst fld (make-ea :dword :base sap :index offset)))))))

(define-vop ()
  (:translate %set-sap-ref-single)
  (:policy :fast-safe)
  (:args (value :scs (single-reg))
         (sap :scs (sap-reg))
         (offset :scs (signed-reg)))
  (:arg-types single-float system-area-pointer signed-num)
  (:generator 5
    (with-tn@fp-top (value)
      (inst fst (make-ea :dword :base sap :index offset)))))

(define-vop ()
  (:translate sap-ref-long)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg))
         (offset :scs (signed-reg)))
  (:arg-types system-area-pointer signed-num)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:generator 5
     (with-empty-tn@fp-top(result)
       (inst fldl (make-ea :dword :base sap :index offset)))))

;;; noise to convert normal lisp data objects into SAPs

(define-vop (vector-sap)
  (:translate vector-sap)
  (:policy :fast-safe)
  (:args (vector :scs (descriptor-reg) :target sap))
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (move sap vector)
    (inst add
          sap
          (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))))
