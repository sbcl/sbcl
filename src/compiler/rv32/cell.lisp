;;;; various primitive memory access VOPs for the RV32 VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(define-vop (slot)
  (:args (object :scs (descriptor-reg)))
  (:info name offset lowtag)
  (:results (result :scs (descriptor-reg any-reg)))
  (:generator 1))

(define-vop (set-slot)
  (:args (object :scs (descriptor-reg))
         (value :scs (descriptor-reg any-reg)))
  (:info name offset lowtag)
  (:results)
  (:generator 1))

(define-vop (init-slot set-slot)
  (:info name dx-p offset lowtag))

(define-vop (checked-cell-ref)
  (:args (object :scs (descriptor-reg)))
  (:results (value :scs (descriptor-reg any-reg))))

(define-vop (set cell-set)
  (:variant symbol-value-slot other-pointer-lowtag))

(define-vop (symbol-value checked-cell-ref)
  (:translate symeval)
  (:generator 9))
(define-vop (fast-symbol-value cell-ref)
  (:variant symbol-value-slot other-pointer-lowtag)
  (:translate symeval))

(define-vop (%set-symbol-global-value cell-set)
  (:variant symbol-value-slot other-pointer-lowtag))

(define-vop (fast-symbol-global-value cell-ref)
  (:variant symbol-value-slot other-pointer-lowtag))

(define-vop (symbol-global-value)
  (:args (object :scs (descriptor-reg)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:generator 9))

(define-vop (fdefn-fun cell-ref)
  (:variant fdefn-fun-slot other-pointer-lowtag))

(define-vop (safe-fdefn-fun)
  (:args (object :scs (descriptor-reg)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:generator 10))

(define-vop (dynbind)
  (:args (value :scs (any-reg descriptor-reg)) (symbol :scs (descriptor-reg)))
  (:generator 5))

(define-vop (unbind)
  (:generator 0))

(define-vop (unbind-to-here)
  (:args (arg :scs (descriptor-reg any-reg)))
  (:generator 0))

(define-vop (closure-ref)
  (:args (object :scs (descriptor-reg)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:info offset)
  (:generator 4))

(define-vop (closure-init)
  (:args (object :scs (descriptor-reg)) (value :scs (descriptor-reg any-reg)))
  (:info offset)
  (:generator 4))

(define-vop (closure-init-from-fp)
  (:args (object :scs (descriptor-reg)))
  (:info offset)
  (:generator 4))

(define-vop (value-cell-ref cell-ref)
  (:variant value-cell-value-slot other-pointer-lowtag))

(define-vop (value-cell-set cell-set)
  (:variant value-cell-value-slot other-pointer-lowtag))
