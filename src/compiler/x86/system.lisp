;;;; x86 VM definitions of various system hacking operations

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; type frobbing VOPs

(define-vop (get-lowtag)
  (:translate get-lowtag)
  (:policy :fast-safe)
  (:args (object :scs (any-reg descriptor-reg control-stack)
		 :target result))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 1
    (move result object)
    (inst and result lowtag-mask)))

(define-vop (get-type)
  (:translate get-type)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:temporary (:sc unsigned-reg :offset eax-offset :to (:result 0)) eax)
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (inst mov eax object)
    (inst and al-tn lowtag-mask)
    (inst cmp al-tn other-pointer-type)
    (inst jmp :e other-ptr)
    (inst cmp al-tn function-pointer-type)
    (inst jmp :e function-ptr)

    ;; pick off structures and list pointers
    (inst test al-tn 1)
    (inst jmp :ne done)

    ;; pick off fixnums
    (inst and al-tn 3)
    (inst jmp :e done)

    ;; must be an other immediate
    (inst mov eax object)
    (inst jmp done)

    FUNCTION-PTR
    (load-type al-tn object (- sb!vm:function-pointer-type))
    (inst jmp done)

    OTHER-PTR
    (load-type al-tn object (- sb!vm:other-pointer-type))

    DONE
    (inst movzx result al-tn)))

(define-vop (function-subtype)
  (:translate function-subtype)
  (:policy :fast-safe)
  (:args (function :scs (descriptor-reg)))
  (:temporary (:sc byte-reg :from (:eval 0) :to (:eval 1)) temp)
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (load-type temp function (- sb!vm:function-pointer-type))
    (inst movzx result temp)))

(define-vop (set-function-subtype)
  (:translate (setf function-subtype))
  (:policy :fast-safe)
  (:args (type :scs (unsigned-reg) :target eax)
	 (function :scs (descriptor-reg)))
  (:arg-types positive-fixnum *)
  (:temporary (:sc unsigned-reg :offset eax-offset :from (:argument 0)
		   :to (:result 0) :target result)
	      eax)
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (move eax type)
    (inst mov
	  (make-ea :byte :base function :disp (- function-pointer-type))
	  al-tn)
    (move result eax)))

(define-vop (get-header-data)
  (:translate get-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 other-pointer-type)
    (inst shr res type-bits)))

(define-vop (get-closure-length)
  (:translate get-closure-length)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 function-pointer-type)
    (inst shr res type-bits)))

(define-vop (set-header-data)
  (:translate set-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg) :target res :to (:result 0))
	 (data :scs (any-reg) :target eax))
  (:arg-types * positive-fixnum)
  (:results (res :scs (descriptor-reg)))
  (:temporary (:sc unsigned-reg :offset eax-offset
		   :from (:argument 1) :to (:result 0)) eax)
  (:generator 6
    (move eax data)
    (inst shl eax (- type-bits 2))
    (inst mov al-tn (make-ea :byte :base x :disp (- other-pointer-type)))
    (storew eax x 0 other-pointer-type)
    (move res x)))

(define-vop (make-fixnum)
  (:args (ptr :scs (any-reg descriptor-reg) :target res))
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    ;; Some code (the hash table code) depends on this returning a
    ;; positive number so make sure it does.
    (move res ptr)
    (inst shl res 3)
    (inst shr res 1)))

(define-vop (make-other-immediate-type)
  (:args (val :scs (any-reg descriptor-reg) :target res)
	 (type :scs (unsigned-reg immediate)))
  (:results (res :scs (any-reg descriptor-reg) :from (:argument 0)))
  (:generator 2
    (move res val)
    (inst shl res (- type-bits 2))
    (inst or res (sc-case type
		   (unsigned-reg type)
		   (immediate (tn-value type))))))

;;;; allocation

(define-vop (dynamic-space-free-pointer)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate dynamic-space-free-pointer)
  (:policy :fast-safe)
  (:generator 1
    (load-symbol-value int *allocation-pointer*)))

(define-vop (binding-stack-pointer-sap)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate binding-stack-pointer-sap)
  (:policy :fast-safe)
  (:generator 1
    (load-symbol-value int *binding-stack-pointer*)))

(defknown (setf binding-stack-pointer-sap)
    (system-area-pointer) system-area-pointer ())

(define-vop (set-binding-stack-pointer-sap)
  (:args (new-value :scs (sap-reg) :target int))
  (:arg-types system-area-pointer)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate (setf binding-stack-pointer-sap))
  (:policy :fast-safe)
  (:generator 1
    (store-symbol-value new-value *binding-stack-pointer*)
    (move int new-value)))

(define-vop (control-stack-pointer-sap)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate control-stack-pointer-sap)
  (:policy :fast-safe)
  (:generator 1
    (move int esp-tn)))

;;;; code object frobbing

(define-vop (code-instructions)
  (:translate code-instructions)
  (:policy :fast-safe)
  (:args (code :scs (descriptor-reg) :to (:result 0)))
  (:results (sap :scs (sap-reg) :from (:argument 0)))
  (:result-types system-area-pointer)
  (:generator 10
    (loadw sap code 0 other-pointer-type)
    (inst shr sap type-bits)
    (inst lea sap (make-ea :byte :base code :index sap :scale 4
			   :disp (- other-pointer-type)))))

(define-vop (compute-function)
  (:args (code :scs (descriptor-reg) :to (:result 0))
	 (offset :scs (signed-reg unsigned-reg) :to (:result 0)))
  (:arg-types * positive-fixnum)
  (:results (func :scs (descriptor-reg) :from (:argument 0)))
  (:generator 10
    (loadw func code 0 other-pointer-type)
    (inst shr func type-bits)
    (inst lea func
	  (make-ea :byte :base offset :index func :scale 4
		   :disp (- function-pointer-type other-pointer-type)))
    (inst add func code)))

;;; REMOVEME
(defknown %function-self (function) function (flushable))

(define-vop (%function-self)
  (:policy :fast-safe)
  (:translate %function-self)
  (:args (function :scs (descriptor-reg)))
  (:results (result :scs (descriptor-reg)))
  (:generator 3
    (loadw result function function-self-slot function-pointer-type)
    (inst lea result
	  (make-ea :byte :base result
		   :disp (- function-pointer-type
			    (* function-code-offset word-bytes))))))

;;; The closure function slot is a pointer to raw code on X86 instead
;;; of a pointer to the code function object itself. This VOP is used
;;; to reference the function object given the closure object.
(def-source-transform %closure-function (closure)
  `(%function-self ,closure))

(def-source-transform %funcallable-instance-function (fin)
  `(%function-self ,fin))

;;; REMOVEME
(defknown (setf %function-self) (function function) function  (unsafe))

(define-vop (%set-function-self)
  (:policy :fast-safe)
  (:translate (setf %function-self))
  (:args (new-self :scs (descriptor-reg) :target result :to :result)
	 (function :scs (descriptor-reg) :to :result))
  (:temporary (:sc any-reg :from (:argument 0) :to :result) temp)
  (:results (result :scs (descriptor-reg)))
  (:generator 3
    (inst lea temp
	  (make-ea :byte :base new-self
		   :disp (- (ash function-code-offset word-shift)
			    function-pointer-type)))
    (storew temp function function-self-slot function-pointer-type)
    (move result new-self)))

;;; REMOVEME
(defknown ((setf %funcallable-instance-function)) (function function) function
	(unsafe))

;;; CMU CL comment:
;;;   We would have really liked to use a source-transform for this, but
;;;   they don't work with SETF functions.
;;; FIXME: Can't we just use DEFSETF or something?
(deftransform (setf %funcallable-instance-function) ((value fin))
  '(setf (%function-self fin) value))

;;;; other miscellaneous VOPs

(defknown sb!unix::do-pending-interrupt () (values))
(define-vop (sb!unix::do-pending-interrupt)
  (:policy :fast-safe)
  (:translate sb!unix::do-pending-interrupt)
  (:generator 1
    (inst break pending-interrupt-trap)))

(define-vop (halt)
  (:generator 1
    (inst break halt-trap)))

(defknown float-wait () (values))
(define-vop (float-wait)
  (:policy :fast-safe)
  (:translate float-wait)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 1
    (note-next-instruction vop :internal-error)
    (inst wait)))

;;;; dynamic vop count collection support

#!+sb-dyncount
(define-vop (count-me)
  (:args (count-vector :scs (descriptor-reg)))
  (:info index)
  (:generator 0
    (inst inc (make-ea :dword :base count-vector
		       :disp (- (* (+ vector-data-offset index) word-bytes)
				other-pointer-type)))))
