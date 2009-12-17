(in-package "SB!VM")


;;;; Type frobbing VOPs

(define-vop (lowtag-of)
  (:translate lowtag-of)
  (:policy :fast-safe)
  (:args (object :scs (any-reg descriptor-reg) :target result))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 1
    (inst extru object 31 n-lowtag-bits result)))

;FIX this vop got instruction-exploded after mips convert, look at old hppa
(define-vop (widetag-of)
  (:translate widetag-of)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp1 temp2)
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (inst li lowtag-mask temp1)
    (inst li other-pointer-lowtag temp2)
    (inst and temp1 object temp1)
    (inst xor temp1 temp2 temp1)
    (inst comb := temp1 zero-tn OTHER-PTR)
    (inst li (logxor other-pointer-lowtag fun-pointer-lowtag) temp2)
    (inst xor temp1 temp2 temp1)
    (inst comb := temp1 zero-tn FUNCTION-PTR)
    (inst li fixnum-tag-mask temp1)  ; pick off fixnums
    (inst li 1 temp2)
    (inst and temp1 object result)
    (inst comb := result zero-tn DONE)

    (inst and object temp2 result)
    (inst comb :<> result zero-tn LOWTAG-ONLY :nullify t)

    ;; must be an other immediate
    (inst li widetag-mask temp2)
    (inst b DONE)
    (inst and temp2 object result)

    FUNCTION-PTR
    (load-type result object (- fun-pointer-lowtag))
    (inst b done :nullify t)

    LOWTAG-ONLY
    (inst li lowtag-mask temp1)
    (inst b done)
    (inst and object temp1 result)

    OTHER-PTR
    (load-type result object (- other-pointer-lowtag))

    DONE))

(define-vop (fun-subtype)
  (:translate fun-subtype)
  (:policy :fast-safe)
  (:args (function :scs (descriptor-reg)))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (load-type result function (- fun-pointer-lowtag))))

(define-vop (set-fun-subtype)
  (:translate (setf fun-subtype))
  (:policy :fast-safe)
  (:args (type :scs (unsigned-reg) :target result)
         (function :scs (descriptor-reg)))
  (:arg-types positive-fixnum *)
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (inst stb type (- fun-pointer-lowtag) function)
    (move type result)))

(define-vop (get-header-data)
  (:translate get-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 other-pointer-lowtag)
    (inst srl res n-widetag-bits res)))

(define-vop (get-closure-length)
  (:translate get-closure-length)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 fun-pointer-lowtag)
    (inst srl res n-widetag-bits res)))
;;; FIXME-lav, not sure we need data of type immediate and zero, test without,
;;; if so revert to old hppa code
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
    ;; replace below 2 inst with: (mask widetag-mask t1 t1)
    (inst li widetag-mask t2)
    (inst and t1 t2 t1)
    (sc-case data
      (any-reg
        (inst sll data (- n-widetag-bits 2) t2)
        (inst or t1 t2 t1))
      (immediate
        (inst li (ash (tn-value data) n-widetag-bits) t2)
        (inst or t1 t2 t1))
      (zero))

    (storew t1 x 0 other-pointer-lowtag)
    (move x res)))

(define-vop (pointer-hash)
  (:translate pointer-hash)
  (:args (ptr :scs (any-reg descriptor-reg)))
  (:results (res :scs (any-reg descriptor-reg)))
  (:policy :fast-safe)
  (:generator 1
    (inst zdep ptr n-positive-fixnum-bits n-positive-fixnum-bits res)))

(define-vop (make-other-immediate-type)
  (:args (val :scs (any-reg descriptor-reg))
         (type :scs (any-reg descriptor-reg immediate) :target temp))
  (:results (res :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:scs (non-descriptor-reg)) t2)
  (:generator 2
    (sc-case type
      ((immediate)
        (inst sll val n-widetag-bits temp)
        (inst li (tn-value type) t2)
        (inst or temp t2 res))
      (t
        (inst sra type 2 temp)
        (inst sll val (- n-widetag-bits 2) res)
        (inst or res temp res)))))

;;;; Allocation

(define-vop (dynamic-space-free-pointer)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate dynamic-space-free-pointer)
  (:policy :fast-safe)
  (:generator 1
    (move alloc-tn int)))

(define-vop (binding-stack-pointer-sap)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate binding-stack-pointer-sap)
  (:policy :fast-safe)
  (:generator 1
    (move bsp-tn int)))

(define-vop (control-stack-pointer-sap)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate control-stack-pointer-sap)
  (:policy :fast-safe)
  (:generator 1
    (move csp-tn int)))


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
    (inst srl ndescr n-widetag-bits ndescr)
    (inst sll ndescr word-shift ndescr)
    (inst addi (- other-pointer-lowtag) ndescr ndescr)
    (inst add code ndescr sap)))

(define-vop (compute-fun)
  (:args (code :scs (descriptor-reg))
         (offset :scs (signed-reg unsigned-reg)))
  (:arg-types * positive-fixnum)
  (:results (func :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:generator 10
    (loadw ndescr code 0 other-pointer-lowtag)
    ;; FIXME-lav: replace below two with DEPW
    (inst srl ndescr n-widetag-bits ndescr)
    (inst sll ndescr word-shift ndescr)
    (inst add ndescr offset ndescr)
    (inst addi (- fun-pointer-lowtag other-pointer-lowtag) ndescr ndescr)
    (inst add ndescr code func)))


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

#!+hpux
(define-vop (setup-return-from-lisp-stub)
  (:results)
  (:save-p t)
  (:temporary (:sc any-reg :offset nl0-offset) nl0)
  (:temporary (:sc any-reg :offset cfunc-offset) cfunc)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:vop-var vop)
  (:generator 100
    (let ((stub (make-fixup 'return-from-lisp-stub :assembly-routine)))
      (inst li stub nl0))
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (store-stack-tn nfp-save cur-nfp))
      (inst li (make-fixup "setup_return_from_lisp_stub" :foreign) cfunc)
      (let ((fixup (make-fixup "call_into_c" :foreign)))
        (inst ldil fixup temp)
        (inst ble fixup c-text-space temp))
      (inst addi  64 nsp-tn nsp-tn)
      (inst addi -64 nsp-tn nsp-tn)
      (when cur-nfp
        (load-stack-tn cur-nfp nfp-save)))))

;;;; Dynamic vop count collection support

(define-vop (count-me)
  (:args (count-vector :scs (descriptor-reg)))
  (:info index)
  (:temporary (:scs (non-descriptor-reg)) count)
  (:generator 1
    (let ((offset
           (- (* (+ index vector-data-offset) n-word-bytes) other-pointer-lowtag)))
      (inst ldw offset count-vector count)
      (inst addi 1 count count)
      (inst stw count offset count-vector))))

