(in-package "SB!VM")


;;;; Random pointer comparison VOPs

(define-vop (pointer-compare)
  (:args (x :scs (sap-reg))
         (y :scs (sap-reg)))
  (:arg-types system-area-pointer system-area-pointer)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:note "inline comparison")
  (:variant-vars condition)
  (:generator 3
    (three-way-comparison x y condition :unsigned not-p target temp)))

#+nil
(macrolet ((frob (name cond)
             `(progn
                (def-primitive-translator ,name (x y) `(,',name ,x ,y))
                (defknown ,name (t t) boolean (movable foldable flushable))
                (define-vop (,name pointer-compare)
                  (:translate ,name)
                  (:variant ,cond)))))
  (frob pointer< :lt)
  (frob pointer> :gt))



;;;; Type frobbing VOPs

(define-vop (lowtag-of)
  (:translate lowtag-of)
  (:policy :fast-safe)
  (:args (object :scs (any-reg descriptor-reg)))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 1
    (inst and result object lowtag-mask)))

(define-vop (widetag-of)
  (:translate widetag-of)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    ;; Pick off objects with headers.
    (inst and ndescr object lowtag-mask)
    (inst xor ndescr other-pointer-lowtag)
    (inst beq ndescr other-ptr)
    (inst xor ndescr (logxor other-pointer-lowtag fun-pointer-lowtag))
    (inst beq ndescr function-ptr)

    ;; Pick off fixnums.
    (inst and result object 3)
    (inst beq result done)

    ;; Pick off structure and list pointers.
    (inst and result object 1)
    (inst bne result lowtag-only)
    (inst nop)

      ;; Must be an other immediate.
    (inst b done)
    (inst and result object widetag-mask)

    FUNCTION-PTR
    (load-type result object (- fun-pointer-lowtag))
    (inst b done)
    (inst nop)

    LOWTAG-ONLY
    (inst b done)
    (inst and result object lowtag-mask)

    OTHER-PTR
    (load-type result object (- other-pointer-lowtag))
    (inst nop)

    DONE))

(define-vop (fun-subtype)
  (:translate fun-subtype)
  (:policy :fast-safe)
  (:args (function :scs (descriptor-reg)))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (load-type result function (- fun-pointer-lowtag))
    (inst nop)))

(define-vop (set-fun-subtype)
  (:translate (setf fun-subtype))
  (:policy :fast-safe)
  (:args (type :scs (unsigned-reg) :target result)
         (function :scs (descriptor-reg)))
  (:arg-types positive-fixnum *)
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (inst sb type function (- fun-pointer-lowtag))
    (move result type)))


(define-vop (get-header-data)
  (:translate get-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 other-pointer-lowtag)
    (inst srl res res n-widetag-bits)))

(define-vop (get-closure-length)
  (:translate get-closure-length)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 fun-pointer-lowtag)
    (inst srl res res n-widetag-bits)))

(define-vop (set-header-data)
  (:translate set-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg) :target res)
         (data :scs (any-reg immediate zero)))
  (:arg-types * positive-fixnum)
  (:results (res :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) t1 t2)
  (:generator 6
    (loadw t1 x 0 other-pointer-lowtag)
    (inst and t1 widetag-mask)
    (sc-case data
      (any-reg
       (inst sll t2 data (- n-widetag-bits 2))
       (inst or t1 t2))
      (immediate
       (inst or t1 (ash (tn-value data) n-widetag-bits)))
      (zero))
    (storew t1 x 0 other-pointer-lowtag)
    (move res x)))

(define-vop (make-fixnum)
  (:args (ptr :scs (any-reg descriptor-reg)))
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    ;;
    ;; Some code (the hash table code) depends on this returning a
    ;; positive number so make sure it does.
    (inst sll res ptr 3)
    (inst srl res res 1)))

(define-vop (make-other-immediate-type)
  (:args (val :scs (any-reg descriptor-reg))
         (type :scs (any-reg descriptor-reg immediate)
               :target temp))
  (:results (res :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 2
    (sc-case type
      ((immediate)
       (inst sll temp val n-widetag-bits)
       (inst or res temp (tn-value type)))
      (t
       (inst sra temp type 2)
       (inst sll res val (- n-widetag-bits 2))
       (inst or res res temp)))))


;;;; Allocation

(define-vop (dynamic-space-free-pointer)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate dynamic-space-free-pointer)
  (:policy :fast-safe)
  (:generator 1
    (move int alloc-tn)))

(define-vop (binding-stack-pointer-sap)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate binding-stack-pointer-sap)
  (:policy :fast-safe)
  (:generator 1
    (move int bsp-tn)))

(define-vop (control-stack-pointer-sap)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate control-stack-pointer-sap)
  (:policy :fast-safe)
  (:generator 1
    (move int csp-tn)))


;;;; Code object frobbing.

(define-vop (code-instructions)
  (:translate code-instructions)
  (:policy :fast-safe)
  (:args (code :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 10
    (loadw ndescr code 0 other-pointer-lowtag)
    (inst srl ndescr n-widetag-bits)
    (inst sll ndescr word-shift)
    (inst subu ndescr other-pointer-lowtag)
    (inst addu sap code ndescr)))

(define-vop (compute-fun)
  (:args (code :scs (descriptor-reg))
         (offset :scs (signed-reg unsigned-reg)))
  (:arg-types * positive-fixnum)
  (:results (func :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:generator 10
    (loadw ndescr code 0 other-pointer-lowtag)
    (inst srl ndescr n-widetag-bits)
    (inst sll ndescr word-shift)
    (inst addu ndescr offset)
    (inst addu ndescr (- fun-pointer-lowtag other-pointer-lowtag))
    (inst addu func code ndescr)))


;;;; Other random VOPs.


(defknown sb!unix::receive-pending-interrupt () (values))
(define-vop (sb!unix::receive-pending-interrupt)
  (:policy :fast-safe)
  (:translate sb!unix::receive-pending-interrupt)
  (:generator 1
    (inst break pending-interrupt-trap)))


(define-vop (halt)
  (:generator 1
    (inst break halt-trap)))


;;;; Dynamic vop count collection support

(define-vop (count-me)
  (:args (count-vector :scs (descriptor-reg)))
  (:info index)
  (:temporary (:scs (non-descriptor-reg)) count)
  (:generator 1
    (let ((offset
           (- (* (+ index vector-data-offset) n-word-bytes) other-pointer-lowtag)))
      (inst lw count count-vector offset)
      (inst nop)
      (inst addu count 1)
      (inst sw count count-vector offset))))
