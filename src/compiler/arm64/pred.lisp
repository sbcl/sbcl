;;;; predicate VOPs for the ARM VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")


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

;;; FIXME: Unlike the PPC (from whence this was cribbed), ARM actually
;;; has flags.  We should take advantage of them here.

(define-vop (branch-if)
  (:info dest flags not-p)
  (:generator 0
    (flet ((negate-condition (name)
             (let ((code (logxor 1 (conditional-opcode name))))
               (aref *condition-name-vec* code))))
      (aver (null (rest flags)))
      (inst b
            (if not-p
                (negate-condition (first flags))
                (first flags))
            dest))))

(defun convert-conditional-move-p (node dst-tn x-tn y-tn)
  (declare (ignore node dst-tn x-tn y-tn))
  nil)


;;;; Conditional VOPs:

(define-vop (if-eq)
  (:args (x :scs (any-reg descriptor-reg null))
         (y :scs (any-reg descriptor-reg null)
            :load-if (sc-case y
                       ((any-reg descriptor-reg null))
                       (immediate
                        (not (fixnum-add-sub-immediate-p (tn-value y))))
                       (t t))))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:translate eq)
  (:generator 3
    (inst cmp
          (sc-case x
            (null null-tn)  ;; FIXME: should it really be like that?
            (t x))
          (sc-case y
            (null null-tn)
            (immediate
             (fixnumize (tn-value y)))
            (t y)))
    (inst b (if not-p :ne :eq) target)))
