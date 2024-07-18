(in-package "SB-VM")

;;; You should generally put WITHOUT-SCHEDULING around this because the fixup patcher
;;; wants to see the two shifts as consecutive instructions.
(defun emit-gengc-barrier (object cell-address temp &optional value-tn-ref allocator)
  (when (require-gengc-barrier-p object value-tn-ref allocator)
    (inst sll temp (or cell-address object) (make-fixup nil :card-table-index-mask))
    (inst srl temp temp 0)
    (inst addu temp temp cardbase-tn)
    (inst sb zero-tn temp 0)))

;;; Cell-Ref and Cell-Set are used to define VOPs like CAR, where the offset to
;;; be read or written is a property of the VOP used.
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
  (:temporary (:sc non-descriptor-reg) temp)
  (:vop-var vop)
  (:generator 4
    (without-scheduling ()
      (emit-gengc-barrier object nil temp (vop-nth-arg 1 vop))
      (storew value object offset lowtag))))
