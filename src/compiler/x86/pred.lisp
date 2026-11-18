;;;; predicate VOPs for the x86 VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMUCL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;;; the branch VOP

;;; The unconditional branch, emitted when we can't drop through to the desired
;;; destination. Dest is the continuation we transfer control to.
(define-vop (branch)
  (:info dest)
  (:generator 5
    (inst jmp dest)))


;;;; Generic conditional VOPs

;;; The generic conditional branch, emitted immediately after test
;;; VOPs that only set flags.
;;;
;;; FLAGS is a list of condition descriptors. If the first descriptor
;;; is CL:NOT, the test was true if all the remaining conditions are
;;; false. Otherwise, the test was true if any of the conditions is.
;;;
;;; NOT-P flips the meaning of the test, as with regular :CONDITIONAL
;;; VOP. If NOT-P is true, the code must branch to dest if the test was
;;; false. Otherwise, the code must branch to dest if the test was true.

(define-vop (branch-if)
  (:info dest not-p flags)
  (:generator 0
    (let ((flags (conditional-flags-flags flags)))
      (aver (null (rest flags)))
      (inst jmp
            (if not-p
                (negate-condition (first flags))
                (first flags))
            dest))))

(define-vop (jump-table)
  (:args (index :scs (signed-reg unsigned-reg any-reg)
                :target offset))
  (:info targets otherwise min max)
  (:temporary (:sc any-reg :from (:argument 0)) offset)
  (:generator 0
    (let ((fixnump (sc-is index any-reg)))
      (flet ((fix (x)
               (if fixnump
                   (fixnumize x)
                   x)))
        (unless (zerop min)
          (let ((diff (- (fix min))))
            (cond ((location= offset index)
                   (inst add offset diff))
                  (t
                   (inst lea offset (make-ea :dword :base index :disp diff))
                   (setf index offset)))))
        (when otherwise
          (inst cmp index (fix (- max min)))
          (inst jmp :a otherwise))
        (let ((table (register-inline-constant :jump-table targets)))
          (inst jmp (make-ea :dword :disp (ea-disp table)
                                    :index index :scale (if fixnump 1 4))))))))

(defun convert-conditional-move-p (dst-tn)
  (declare (ignore dst-tn)))


;;;; conditional VOPs

;;; Note: a constant-tn is allowed in CMP; it uses an EA displacement,
;;; not immediate data.
(define-vop (if-eq)
  (:args (x :scs (any-reg descriptor-reg control-stack constant)
            :load-if (not (and (sc-is x immediate)
                               (sc-is y any-reg descriptor-reg
                                      control-stack constant))))
         (y :scs (any-reg descriptor-reg immediate)
            :load-if (not (and (sc-is x any-reg descriptor-reg immediate)
                               (sc-is y control-stack constant)))))
  (:conditional :e)
  (:info)
  (:translate eq)
  (:generator 6
    (let ((x-val (encode-value-if-immediate x))
          (y-val (encode-value-if-immediate y)))
      (cond
        ;; Shorter instruction sequences for these two cases.
        ((and (eql 0 y-val) (sc-is x any-reg descriptor-reg)) (inst test x x))
        ((and (eql 0 x-val) (sc-is y any-reg descriptor-reg)) (inst test y y))

        ;; An encoded value (literal integer) has to be the second argument.
        ((sc-is x immediate) (inst cmp y x-val))

        (t (inst cmp x y-val))))))

(macrolet ((def (eq-name eql-name cost)
             `(define-vop (,eq-name ,eql-name)
                (:translate eq)
                (:variant-cost ,cost))))
  (def if-eq-character char=/character 3)
  (def if-eq-character/c char=/character/c 2)
  (def if-eq-fixnum eql/fixnum 3)
  (def if-eq-fixnum/c eql-c/fixnum 2)
  (def if-eq-signed if-eql/signed 5)
  (def if-eq-signed/c if-eql-c/signed 4)
  (def if-eq-unsigned if-eql/unsigned 5)
  (def if-eq-unsigned/c if-eql-c/unsigned 4))
