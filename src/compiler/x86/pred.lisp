;;;; predicate VOPs for the x86 VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
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
  (when (memq :cmov *backend-subfeatures*)
    (sc-case dst-tn
      ((descriptor-reg any-reg)
       'move-if/t)
      (unsigned-reg
       'move-if/unsigned)
      (signed-reg
       'move-if/signed)
      ;; FIXME: Can't use CMOV with byte registers, and characters live
      ;; in such outside of unicode builds. A better solution then just
      ;; disabling MOVE-IF/CHAR should be possible, though.
      #+sb-unicode
      (character-reg
       'move-if/char)
      (sap-reg
       'move-if/sap)
      (t))))

(define-vop (move-if)
  (:args (then) (else))
  (:results (res))
  (:info flags)
  (:temporary (:sc unsigned-reg) temp)
  (:generator 0
    (let* ((flags (conditional-flags-flags flags))
           (not-p (eq (first flags) 'not)))
      (when not-p (pop flags))
      (when (location= res then)
        (rotatef then else)
        (setf not-p (not not-p)))
      (flet ((load-immediate (dst constant-tn &optional (sc-reg dst))
               (let ((val (tn-value constant-tn)))
                 ;; Shouldn't this just be ENCODE-VALUE-IF-IMMEDIATE ?
                 ;; (at least for a boxed SC ?)
                 (etypecase val
                   (integer
                    (if (sc-is sc-reg any-reg descriptor-reg)
                        (inst mov dst (fixnumize val))
                        (inst mov dst val)))
                   (symbol
                    (load-symbol dst val))
                   (character
                    (sc-case sc-reg
                      ((any-reg descriptor-reg)
                       (inst mov dst
                             (logior (ash (char-code val) n-widetag-bits)
                                     character-widetag)))
                      (character-reg
                       (inst mov sc-reg (char-code val)))))
                   (structure-object
                    (aver (eq val sb-lockless:+tail+))
                    (+ static-space-start lockfree-list-tail-value-offset))))))
        (aver (null (rest flags)))
        (if (sc-is else immediate)
            (load-immediate res else)
            (move res else))
        (when (sc-is then immediate)
          (load-immediate temp then res)
          (setf then temp))
        (inst cmov (if not-p
                       (negate-condition (first flags))
                       (first flags))
              res
              then)))))

(macrolet ((def-move-if (name type reg stack)
             `(define-vop (,name move-if)
                (:args (then :scs (immediate ,@(ensure-list reg) ,stack) :to :eval
                             :load-if (not (or (sc-is then immediate)
                                               (and (sc-is then ,stack)
                                                    (not (location= else res))))))
                       (else :scs (immediate ,@(ensure-list reg) ,stack) :target res
                             :load-if (not (sc-is else immediate ,stack))))
                (:arg-types ,type ,type)
                (:results (res :scs ,(ensure-list reg)))
                (:result-types ,type))))
  (def-move-if move-if/t t (descriptor-reg any-reg) control-stack)
  (def-move-if move-if/unsigned unsigned-num unsigned-reg unsigned-stack)
  (def-move-if move-if/signed signed-num signed-reg signed-stack)
  ;; FIXME: See convert-conditional-move-p above.
  #+sb-unicode
  (def-move-if move-if/char character character-reg character-stack)
  (def-move-if move-if/sap system-area-pointer sap-reg sap-stack))


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
  (:policy :fast-safe)
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
  (def fast-if-eq-character fast-char=/character 3)
  (def fast-if-eq-character/c fast-char=/character/c 2)
  (def fast-if-eq-fixnum fast-eql/fixnum 3)
  (def fast-if-eq-fixnum/c fast-eql-c/fixnum 2)
  (def fast-if-eq-signed fast-if-eql/signed 5)
  (def fast-if-eq-signed/c fast-if-eql-c/signed 4)
  (def fast-if-eq-unsigned fast-if-eql/unsigned 5)
  (def fast-if-eq-unsigned/c fast-if-eql-c/unsigned 4))
