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

(define-vop (multiway-branch-if-eq)
  ;; TODO: also accept signed-reg, unsigned-reg, character-reg
  (:args (x :scs (any-reg descriptor-reg)))
  (:info labels otherwise key-type keys test-vop-name)
  (:temporary (:sc unsigned-reg) index)
  (:temporary (:sc interior-reg :offset lip-offset) lip)
  (:ignore test-vop-name)
  (:generator 10
    (let* ((min (car keys)) ; keys are sorted
           (max (car (last keys)))
           (vector (make-array (1+ (- max min)) :initial-element otherwise)))
      (mapc (lambda (key label) (setf (aref vector (- key min)) label))
            keys labels)
      (ecase key-type
        (fixnum
         (inst andi. index x fixnum-tag-mask)
         (inst b? :ne otherwise)
         (inst addi index x (fixnumize (- min)))
         (inst cmpldi index (fixnumize (- max min)))
         (inst b? :gt otherwise)
         (let ((s (- word-shift n-fixnum-tag-bits)))
           (unless (zerop s)
             (inst sldi index index s))))
        (character
         (inst andi. index x widetag-mask)
         (inst cmpwi index character-widetag)
         (inst b? :ne otherwise)
         (inst srwi index x n-widetag-bits)
         (inst addi index index (- min))
         (inst cmplwi index (- max min))
         (inst b? :gt otherwise)
         (inst slwi index index word-shift)))
      (let ((table-label (register-inline-constant vector :jump-table)))
        (inst addi index index (make-fixup nil :code-object table-label)))
      (inst ldx lip code-tn index)
      (inst mtctr lip)
      (inst bctr))))

(defun convert-conditional-move-p (node dst-tn x-tn y-tn)
  (declare (ignore node dst-tn x-tn y-tn))
  nil)


;;;; Conditional VOPs:

(define-vop (if-eq)
  (:args (x :scs (any-reg descriptor-reg null))
         (y :scs (any-reg descriptor-reg null)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:translate eq)
  (:generator 3
    (inst cmpd x y)
    (inst b? (if not-p :ne :eq) target)))
