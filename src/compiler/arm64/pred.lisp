;;;; predicate VOPs for the ARM VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

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
  (:generator 0
    (let ((flags (conditional-flags-flags flags)))
      (aver (null (rest flags)))
      (inst b
            (if not-p
                (negate-condition (first flags))
                (first flags))
            dest))))

(defun convert-conditional-move-p (dst-tn)
  (sc-case dst-tn
    ((descriptor-reg any-reg)
     'move-if/descriptor)
    ((unsigned-reg signed-reg)
     'move-if/word)
    (double-reg
     'move-if/double)
    (single-reg
     'move-if/single)
    (sap-reg
     'move-if/sap)
    (character-reg
     'move-if/char)
    (t)))

(define-vop (move-if)
  (:args (then) (else))
  (:results (res))
  (:info flags)
  (:generator 0
    (let* ((flags (conditional-flags-flags flags))
           (not-p (eq (first flags) 'not)))
      (when not-p (pop flags))
      (cond ((null (rest flags))
             (inst csel res then else (if not-p
                                          (negate-condition (car flags))
                                          (car flags))))
            (not-p
             (dolist (flag flags)
               (inst csel res else then flag)))
            (t
             (dolist (flag flags)
               (inst csel res then else flag)))))))

(macrolet ((def-move-if (name type reg)
             `(define-vop (,name move-if)
                (:args (then :scs ,reg)
                       (else :scs ,reg))
                (:arg-types ,type ,type)
                (:results (res :scs ,reg))
                (:result-types ,type))))
  (def-move-if move-if/descriptor * (descriptor-reg any-reg zero))
  (def-move-if move-if/word (:or unsigned-num signed-num) (unsigned-reg signed-reg zero))
  (def-move-if move-if/char character (character-reg zero))
  (def-move-if move-if/sap system-area-pointer (sap-reg zero)))

(define-vop (move-if/double move-if)
  (:args (then :scs (double-reg)) (else :scs (double-reg)))
  (:arg-types double-float double-float)
  (:results (res :scs (double-reg)))
  (:result-types double-float)
  (:generator 1
    (let* ((flags (conditional-flags-flags flags))
           (not-p (eq (first flags) 'not)))
      (when not-p (pop flags))
      (cond ((null (rest flags))
             (inst fcsel res then else (if not-p
                                          (negate-condition (car flags))
                                          (car flags))))
            (not-p
             (dolist (flag flags)
               (inst fcsel res else then flag)))
            (t
             (dolist (flag flags)
               (inst fcsel res then else flag)))))))

(define-vop (move-if/single move-if/double)
  (:args (then :scs (single-reg)) (else :scs (single-reg)))
  (:arg-types single-float single-float)
  (:results (res :scs (single-reg)))
  (:result-types single-float))

;;;; Conditional VOPs:

(define-vop (if-eq)
  (:args (x :scs (any-reg descriptor-reg))
         (y :scs (any-reg descriptor-reg)
            :load-if (sc-case y
                       ((any-reg descriptor-reg))
                       (immediate
                        (not (or
                              (eql (tn-value y) 0f0)
                              (and (integerp (tn-value y))
                                   (abs-add-sub-immediate-p (fixnumize (tn-value y)))))))
                       (t t))))
  (:conditional :eq)
  (:policy :fast-safe)
  (:translate eq)
  (:generator 7
    (let ((value (sc-case y
                   (immediate
                    (let ((value (tn-value y)))
                     (if (eql value 0f0)
                         single-float-widetag
                         (fixnumize (tn-value y)))))
                   (t y))))
      (cond ((or (not (integerp value))
                 (add-sub-immediate-p value))
             (inst cmp x value))
            ((minusp value)
             (inst cmn x (- value)))
            (t
             (inst cmn x (ldb (byte n-word-bits 0) (- value))))))))

(macrolet ((def (eq-name eql-name cost)
             `(define-vop (,eq-name ,eql-name)
                (:translate eq)
                (:variant-cost ,cost))))
  (def fast-if-eq-character fast-char=/character 3)
  (def fast-if-eq-character/c fast-char=/character/c 2)
  (def fast-if-eq-fixnum fast-if-eql/fixnum 3)
  (def fast-if-eq-integer/c fast-if-eql-integer/c 2)
  (def fast-if-eq-signed fast-if-eql/signed 5)
  (def fast-if-eq-unsigned fast-if-eql/unsigned 5))

(define-vop (jump-table)
  (:args (index :scs (signed-reg unsigned-reg any-reg)
                :target offset))
  (:info targets otherwise min max)
  (:temporary (:sc unsigned-reg) table)
  (:temporary (:sc any-reg :from (:argument 0)) offset)
  (:generator 0
    (let ((fixnump (sc-is index any-reg)))
      (flet ((fix (x)
               (if fixnump
                   (fixnumize x)
                   x)))
        (inst adr table (cdr (register-inline-constant :jump-table (coerce targets 'vector))))
        (unless (zerop min)
          (inst add-sub offset index (- (fix min)))
          (setf index offset))
        (when otherwise
          (inst cmp index (add-sub-immediate (fix (- max min))))
          (inst b :hi otherwise))
        (cond (fixnump
               (inst add table table (lsl index (- word-shift n-fixnum-tag-bits)))
               (inst ldr table (@ table)))
              (t
               (inst ldr table (@ table (extend index :lsl word-shift)))))
        (inst br table)))))
