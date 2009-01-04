(in-package "SB!VM")

;;; Cell-Ref and Cell-Set are used to define VOPs like CAR, where the offset to
;;; be read or written is a property of the VOP used.  Cell-Setf is similar to
;;; Cell-Set, but delivers the new value as the result.  Cell-Setf-Function
;;; takes its arguments as if it were a setf function (new value first, as
;;; apposed to a setf macro, which takes the new value last).
;;;
(define-vop (cell-ref)
  (:args (object :scs (descriptor-reg)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:generator 4
    (loadw value object offset lowtag)))
;;;
(define-vop (cell-set)
  (:args (object :scs (descriptor-reg))
         (value :scs (descriptor-reg any-reg null zero)))
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:generator 1
    (storew value object offset lowtag)))

;;; Slot-Ref and Slot-Set are used to define VOPs like Closure-Ref, where the
;;; offset is constant at compile time, but varies for different uses.  We add
;;; in the stardard g-vector overhead.
;;;
(define-vop (slot-ref)
  (:args (object :scs (descriptor-reg)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:variant-vars base lowtag)
  (:info offset)
  (:generator 4
    (loadw value object (+ base offset) lowtag)))
;;;
(define-vop (slot-set)
  (:args (object :scs (descriptor-reg))
         (value :scs (descriptor-reg any-reg null zero)))
  (:variant-vars base lowtag)
  (:info offset)
  (:generator 4
    (storew value object (+ base offset) lowtag)))

