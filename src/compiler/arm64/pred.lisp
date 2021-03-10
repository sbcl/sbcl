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
      (aver (null (rest flags)))
      (inst b
            (if not-p
                (negate-condition (first flags))
                (first flags))
            dest)))

(defun convert-conditional-move-p (node dst-tn x-tn y-tn)
  (declare (ignore node dst-tn x-tn y-tn))
  nil)


;;;; Conditional VOPs:

(define-vop (if-eq)
  (:args (x :scs (any-reg descriptor-reg))
         (y :scs (any-reg descriptor-reg)
            :load-if (sc-case y
                       ((any-reg descriptor-reg))
                       (immediate
                        (not (fixnum-add-sub-immediate-p (tn-value y))))
                       (t t))))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:translate eq)
  (:generator 6
    (cond ((not (and (sc-is y immediate)
                     (eql 0 (tn-value y))))
           (inst cmp x
                 (sc-case y
                   (immediate
                    (fixnumize (tn-value y)))
                   (t y)))
           (inst b (if not-p :ne :eq) target))
          (not-p
           (inst cbnz x target))
          (t
           (inst cbz x target)))))

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
