;;;
;;; Converted by William Lott.
;;;

(in-package "SB-VM")


;;;; The Branch VOP.

;;; The unconditional branch, emitted when we can't drop through to the desired
;;; destination.  Dest is the continuation we transfer control to.
;;;
(define-vop (branch)
  (:info dest)
  (:generator 5
    (inst b dest)))


;;;; Generic conditional VOPs

;;; The generic conditional branch, emitted immediately after test
;;; VOPs that only set flags.

(define-vop (branch-if)
  (:info dest not-p flags)
  (:ignore dest not-p flags)
  (:generator 0
     (error "BRANCH-IF not yet implemented")))

(define-vop (jump-table)
  (:args (index :scs (signed-reg unsigned-reg any-reg)
                :target offset))
  (:info targets otherwise min max)
  (:temporary (:sc unsigned-reg) temp)
  (:temporary (:sc any-reg :from (:argument 0)) offset)
  (:temporary (:sc interior-reg :offset lip-offset) lip)
  (:generator 0
    (let ((fixnump (sc-is index any-reg)))
      (flet ((fix (x)
               (if fixnump
                   (fixnumize x)
                   x)))
        (unless (zerop min)
          (let ((start (- (fix min))))
            (cond ((typep start '(signed-byte 16))
                   (inst addi offset index start))
                  (t
                   (inst lr temp start)
                   (inst add offset index temp)))
            (setf index offset)))
        (when otherwise
          (let ((diff (fix (- max min))))
            (cond ((typep diff '(signed-byte 16))
                   (inst cmplwi index diff))
                  (t
                   (inst lr temp diff)
                   (inst cmplw index temp))))
          (inst b? :gt otherwise))
        (cond (fixnump
               (let ((s (- word-shift n-fixnum-tag-bits)))
                 (unless (zerop s)
                   (inst slwi offset index s)
                   (setf index offset))))
              (t
               (inst slwi offset index word-shift)
               (setf index offset)))

        (let ((table-label (register-inline-constant targets :jump-table)))
          (inst addi offset index (make-fixup nil :code-object table-label))
          (inst lwzx lip code-tn offset)
          (inst mtctr lip)
          (inst bctr))))))

(defun convert-conditional-move-p (dst-tn)
  (declare (ignore dst-tn))
  nil)


;;;; Conditional VOPs:

(define-vop (if-eq)
  (:args (x :scs (any-reg descriptor-reg zero null))
         (y :scs (any-reg descriptor-reg zero null)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:translate eq)
  (:generator 3
    (inst cmpw x y)
    (inst b? (if not-p :ne :eq) target)))
