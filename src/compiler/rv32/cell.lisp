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

;;;; Data object ref/set stuff.

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

;;; Do a cell ref with an error check for being unbound.
(define-vop (checked-cell-ref)
  (:args (object :scs (descriptor-reg)))
  (:results (value :scs (descriptor-reg any-reg))))

;;;; Symbol hacking VOPs:

;;; The compiler likes to be able to directly SET symbols.
(define-vop (set cell-set)
  (:variant symbol-value-slot other-pointer-lowtag))

;;; With Symbol-Value, we check that the value isn't the trap object.
;;; So Symbol-Value of NIL is NIL.
(define-vop (symbol-value checked-cell-ref)
  (:translate symeval)
  (:generator 9))

;;; Like CHECKED-CELL-REF, only we are a predicate to see if the cell is bound.
(define-vop (boundp-frob)
  (:args (object :scs (descriptor-reg)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe))

(define-vop (boundp boundp-frob)
  (:translate boundp)
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

;;;; Fdefinition (fdefn) objects.
(define-vop (fdefn-fun cell-ref)
  (:variant fdefn-fun-slot other-pointer-lowtag))

(define-vop (safe-fdefn-fun)
  (:args (object :scs (descriptor-reg)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:generator 10))

;;;; Binding and Unbinding.

;;; BIND -- Establish VAL as a binding for SYMBOL.  Save the old value and
;;; the symbol on the binding stack and stuff the new value into the
;;; symbol.
(define-vop (dynbind)
  (:args (value :scs (any-reg descriptor-reg)) (symbol :scs (descriptor-reg)))
  (:generator 5))

(define-vop (unbind)
  (:generator 0))

(define-vop (unbind-to-here)
  (:args (arg :scs (descriptor-reg any-reg)))
  (:generator 0))

;;;; Closure indexing.
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

;;;; Value Cell hackery.
(define-vop (value-cell-ref cell-ref)
  (:variant value-cell-value-slot other-pointer-lowtag))

(define-vop (value-cell-set cell-set)
  (:variant value-cell-value-slot other-pointer-lowtag))

;;;; Instance hackery:

(define-vop (instance-length)
  (:policy :fast-safe)
  (:translate %instance-length)
  (:args (struct :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 4))

(define-full-reffer instance-index-ref * instance-slots-offset
  instance-pointer-lowtag (descriptor-reg any-reg) * %instance-ref)

(define-full-setter instance-index-set * instance-slots-offset
  instance-pointer-lowtag (descriptor-reg any-reg) * %instance-set)

;;;; Code object frobbing.

(define-full-reffer code-header-ref * 0 other-pointer-lowtag
  (descriptor-reg any-reg) * code-header-ref)

(define-full-setter code-header-set * 0 other-pointer-lowtag
  (descriptor-reg any-reg) * code-header-set)

;;;; raw instance slot accessors

(defmacro define-raw-slot-vops (name value-primtype value-sc)
  (let ((ref-vop (symbolicate "RAW-INSTANCE-REF/" name))
        (set-vop (symbolicate "RAW-INSTANCE-SET/" name)))
    `(progn
       (define-vop (,ref-vop)
         (:translate ,(symbolicate "%" ref-vop))
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg))
                (index :scs (any-reg immediate)))
         (:arg-types * positive-fixnum)
         (:results (value :scs (,value-sc)))
         (:result-types ,value-primtype)
         (:generator 5))
       (define-vop (,set-vop)
         (:translate ,(symbolicate "%" set-vop))
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg))
                (index :scs (any-reg immediate))
                (value :scs (,value-sc) :target result))
         (:arg-types * positive-fixnum ,value-primtype)
         (:results (result :scs (,value-sc)))
         (:result-types ,value-primtype)
         (:generator 5)))))

(define-raw-slot-vops word unsigned-num unsigned-reg)
(define-raw-slot-vops single single-float single-reg)
(define-raw-slot-vops double double-float double-reg)
(define-raw-slot-vops complex-single complex-single-float complex-single-reg)
(define-raw-slot-vops complex-double complex-double-float complex-double-reg)
