;;;; Alpha VM definitions of various system hacking operations

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")


;;;; Type frobbing VOPs

(define-vop (get-lowtag)
  (:translate get-lowtag)
  (:policy :fast-safe)
  (:args (object :scs (any-reg descriptor-reg)))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 1
    (inst and object lowtag-mask result)))

(define-vop (get-type)
  (:translate get-type)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    ;; Pick off objects with headers.
    (inst and object lowtag-mask result)
    (inst cmpeq result other-pointer-type ndescr)
    (inst bne ndescr other-ptr)
    (inst cmpeq result function-pointer-type ndescr)
    (inst bne ndescr function-ptr)

    ;; Pick off structure and list pointers.
    (inst blbs object done)

    ;; Pick off fixnums.
    (inst and object 3 result)
    (inst beq result done)

    ;; Must be an other immediate.
    (inst and object type-mask result)
    (inst br zero-tn done)

    FUNCTION-PTR
    (load-type result object (- function-pointer-type))
    (inst br zero-tn done)

    OTHER-PTR
    (load-type result object (- other-pointer-type))
      
    DONE))

(define-vop (function-subtype)
  (:translate function-subtype)
  (:policy :fast-safe)
  (:args (function :scs (descriptor-reg)))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (load-type result function (- function-pointer-type))))

(define-vop (set-function-subtype)
  (:translate (setf function-subtype))
  (:policy :fast-safe)
  (:args (type :scs (unsigned-reg) :target result)
	 (function :scs (descriptor-reg)))
  (:arg-types positive-fixnum *)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (inst ldl temp (- function-pointer-type) function)
    (inst and temp #xff temp)
    (inst bis type temp temp)
    (inst stl temp (- function-pointer-type) function)
    (move type result)))


(define-vop (get-header-data)
  (:translate get-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 other-pointer-type)
    (inst srl res type-bits res)))

(define-vop (get-closure-length)
  (:translate get-closure-length)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 function-pointer-type)
    (inst srl res type-bits res)))

(define-vop (set-header-data)
  (:translate set-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg) :target res)
	 (data :scs (any-reg immediate zero)))
  (:arg-types * positive-fixnum)
  (:results (res :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) t1 t2)
  (:generator 6
    (loadw t1 x 0 other-pointer-type)
    (inst and t1 type-mask t1)
    (sc-case data
      (any-reg
       (inst sll data (- type-bits 2) t2)
       (inst bis t1 t2 t1))
      (immediate
       (let ((c (ash (tn-value data) type-bits)))
	 (cond ((<= 0 c (1- (ash 1 8)))
		(inst bis t1 c t1))
	       (t
		(inst li c t2)
		(inst bis t1 t2 t1)))))
      (zero))
    (storew t1 x 0 other-pointer-type)
    (move x res)))

(define-vop (make-fixnum)
  (:args (ptr :scs (any-reg descriptor-reg)))
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    ;;
    ;; Some code (the hash table code) depends on this returning a
    ;; positive number so make sure it does.
    (inst sll ptr 35 res)
    (inst srl res 33 res)))

(define-vop (make-other-immediate-type)
  (:args (val :scs (any-reg descriptor-reg))
	 (type :scs (any-reg descriptor-reg immediate)
	       :target temp))
  (:results (res :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 2
    (sc-case type
      ((immediate)
       (inst sll val type-bits temp)
       (inst bis temp (tn-value type) res))
      (t
       (inst sra type 2 temp)
       (inst sll val (- type-bits 2) res)
       (inst bis res temp res)))))


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
    (loadw ndescr code 0 other-pointer-type)
    (inst srl ndescr type-bits ndescr)
    (inst sll ndescr word-shift ndescr)
    (inst subq ndescr other-pointer-type ndescr)
    (inst addq code ndescr sap)))

(define-vop (compute-function)
  (:args (code :scs (descriptor-reg))
	 (offset :scs (signed-reg unsigned-reg)))
  (:arg-types * positive-fixnum)
  (:results (func :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:generator 10
    (loadw ndescr code 0 other-pointer-type)
    (inst srl ndescr type-bits ndescr)
    (inst sll ndescr word-shift ndescr)
    (inst addq ndescr offset ndescr)
    (inst subq ndescr (- other-pointer-type function-pointer-type) ndescr)
    (inst addq code ndescr func)))


;;;; Other random VOPs.


(defknown sb!unix::do-pending-interrupt () (values))
(define-vop (sb!unix::do-pending-interrupt)
  (:policy :fast-safe)
  (:translate sb!unix::do-pending-interrupt)
  (:generator 1
    (inst gentrap pending-interrupt-trap)))


(define-vop (halt)
  (:generator 1
    (inst gentrap halt-trap)))


;;;; Dynamic vop count collection support

(define-vop (count-me)
  (:args (count-vector :scs (descriptor-reg)))
  (:info index)
  (:temporary (:scs (non-descriptor-reg)) count)
  (:generator 1
    (let ((offset
	   (- (* (+ index vector-data-offset) word-bytes) other-pointer-type)))
      (inst ldl count offset count-vector)
      (inst addq count 1 count)
      (inst stl count offset count-vector))))
