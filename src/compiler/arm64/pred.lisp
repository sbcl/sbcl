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

(define-load-time-global *cmov-ptype-representation-vop*
  (mapcan (lambda (entry)
            (destructuring-bind (ptypes &optional sc vop)
                entry
              (mapcar (if (and vop sc)
                          (lambda (ptype)
                            (list ptype sc vop))
                          #'list)
                      (ensure-list ptypes))))
          '((t descriptor-reg move-if/descriptor)
            ((fixnum positive-fixnum) any-reg move-if/descriptor)
            ((unsigned-byte-64 unsigned-byte-63) unsigned-reg move-if/word)
            (signed-byte-64 signed-reg move-if/word)
            (character character-reg move-if/char)
            ((single-float complex-single-float
              double-float complex-double-float))
            (system-area-pointer sap-reg move-if/sap))))

(defun convert-conditional-move-p (node dst-tn x-tn y-tn)
  (declare (ignore node))
  (let* ((ptype (sb-c::tn-primitive-type dst-tn))
         (name  (sb-c:primitive-type-name ptype))
         (param (cdr (or (assoc name *cmov-ptype-representation-vop*)
                         '(t descriptor-reg move-if/descriptor)))))
    (when param
      (destructuring-bind (representation vop) param
        (let ((scn (sc-number-or-lose representation)))
          (labels ((make-tn (tn)
                     (cond ((and (tn-sc tn)
                                 (or
                                  (and
                                   (sc-is tn immediate)
                                   (eq (tn-value tn) 0))
                                  (and
                                   (sc-is tn descriptor-reg)
                                   (eql (tn-offset tn) null-offset))))
                            tn)
                           (t
                            (make-representation-tn ptype scn)))))
            (values vop
                    (make-tn x-tn) (make-tn y-tn)
                    (make-tn dst-tn)
                    nil)))))))


(define-vop (move-if)
  (:args (then) (else))
  (:results (res))
  (:info flags)
  (:generator 0
    (let ((not-p (eq (first flags) 'not)))
      (when not-p (pop flags))
      (flet ((zr (tn)
               (cond ((and (eq (tn-kind tn) :constant)
                           (eq (tn-value tn) 0))
                      zr-tn)
                     (t
                      tn))))
        (setf then (zr then)
              else (zr else))
        (cond ((null (rest flags))
               (inst csel res then else (if not-p
                                            (negate-condition (car flags))
                                            (car flags))))
              (not-p
               (dolist (flag flags)
                 (inst csel res else then flag)))
              (t
               (dolist (flag flags)
                 (inst csel res then else flag))))))))

(macrolet ((def-move-if (name type reg)
             `(define-vop (,name move-if)
                (:args (then :scs ,reg
                             :load-if (not (and (sc-is then immediate)
                                                (eql (tn-value then) 0))))
                       (else :scs ,reg
                             :load-if (not (and (sc-is else immediate)
                                                (eql (tn-value else) 0)))))
                (:arg-types ,type ,type)
                (:results (res :scs ,reg))
                (:result-types ,type))))
  (def-move-if move-if/descriptor * (descriptor-reg any-reg))
  (def-move-if move-if/word (:or unsigned-num signed-num) (unsigned-reg signed-reg))
  (def-move-if move-if/char character (character-reg))
  (def-move-if move-if/sap system-area-pointer (sap-reg)))


;;;; Conditional VOPs:

(define-vop (if-eq)
  (:args (x :scs (any-reg descriptor-reg))
         (y :scs (any-reg descriptor-reg)
            :load-if (sc-case y
                       ((any-reg descriptor-reg))
                       (immediate
                        (not (fixnum-add-sub-immediate-p (tn-value y))))
                       (t t))))
  (:conditional :eq)
  (:policy :fast-safe)
  (:translate eq)
  (:generator 6
    (inst cmp x
      (sc-case y
        (immediate
         (fixnumize (tn-value y)))
        (t y)))))

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
