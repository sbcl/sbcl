;;;; the VM definition of various primitive memory access VOPs for the
;;;; ARM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; Symbol hacking VOPs:

;;; The compiler likes to be able to directly SET symbols.
;;;
(define-vop (set cell-set)
  (:variant symbol-value-slot other-pointer-lowtag))

;;;; Binding and Unbinding.

;;; BIND -- Establish VAL as a binding for SYMBOL.  Save the old value and
;;; the symbol on the binding stack and stuff the new value into the
;;; symbol.

(define-vop (bind)
  (:args (val :scs (any-reg descriptor-reg))
         (symbol :scs (descriptor-reg)))
  (:temporary (:scs (descriptor-reg)) value-temp)
  (:temporary (:scs (any-reg)) bsp-temp)
  (:generator 5
    (loadw value-temp symbol symbol-value-slot other-pointer-lowtag)
    (load-symbol-value bsp-temp *binding-stack-pointer*)
    (inst add bsp-temp bsp-temp (* 2 n-word-bytes))
    (store-symbol-value bsp-temp *binding-stack-pointer*)
    (storew value-temp bsp-temp (- binding-value-slot binding-size))
    (storew symbol bsp-temp (- binding-symbol-slot binding-size))
    (storew val symbol symbol-value-slot other-pointer-lowtag)))

(define-vop (unbind)
  (:temporary (:scs (descriptor-reg)) symbol value)
  (:temporary (:scs (any-reg)) bsp-temp)
  (:temporary (:scs (any-reg)) zero-temp)
  (:generator 0
    (inst mov zero-temp 0)
    (load-symbol-value bsp-temp *binding-stack-pointer*)
    (loadw symbol bsp-temp (- binding-symbol-slot binding-size))
    (loadw value bsp-temp (- binding-value-slot binding-size))
    (storew value symbol symbol-value-slot other-pointer-lowtag)
    (storew zero-temp bsp-temp (- binding-symbol-slot binding-size))
    (storew zero-temp bsp-temp (- binding-value-slot binding-size))
    (inst sub bsp-temp bsp-temp (* 2 n-word-bytes))
    (store-symbol-value bsp-temp *binding-stack-pointer*)))

(define-vop (unbind-to-here)
  (:args (arg :scs (descriptor-reg any-reg) :target where))
  (:temporary (:scs (any-reg) :from (:argument 0)) where)
  (:temporary (:scs (descriptor-reg)) symbol value)
  (:temporary (:scs (any-reg)) bsp-temp zero-temp)
  (:generator 0
    (load-symbol-value bsp-temp *binding-stack-pointer*)
    (inst mov zero-temp 0)
    (move where arg)
    (inst cmp where bsp-temp)
    (inst b :eq DONE)

    LOOP
    (loadw symbol bsp-temp (- binding-symbol-slot binding-size))
    (inst cmp symbol 0)
    (loadw value bsp-temp (- binding-value-slot binding-size) 0 :ne)
    (storew value symbol symbol-value-slot other-pointer-lowtag :ne)
    (storew zero-temp bsp-temp (- binding-symbol-slot binding-size) 0 :ne)

    (storew zero-temp bsp-temp (- binding-value-slot binding-size))
    (inst sub bsp-temp bsp-temp (* 2 n-word-bytes))
    (inst cmp where bsp-temp)
    (inst b :ne LOOP)

    DONE
    (store-symbol-value bsp-temp *binding-stack-pointer*)))

;;;; Closure indexing.

(define-vop (closure-ref slot-ref)
  (:variant closure-info-offset fun-pointer-lowtag))

(define-vop (closure-init slot-set)
  (:variant closure-info-offset fun-pointer-lowtag))

(define-vop (closure-init-from-fp)
  (:args (object :scs (descriptor-reg)))
  (:info offset)
  (:generator 4
    (storew fp-tn object (+ closure-info-offset offset) fun-pointer-lowtag)))

;;;; Instance hackery:

(define-full-reffer instance-index-ref * instance-slots-offset
  instance-pointer-lowtag (descriptor-reg any-reg) * %instance-ref)
