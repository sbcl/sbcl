;;;; type testing and checking VOPs for the ARM VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

(defun %test-lowtag (value target not-p lowtag &key temp)
  (assemble ()
    (inst and temp value lowtag-mask)
    (inst cmp temp lowtag)
    (inst b (if not-p :ne :eq) target)))

;;; Type checking and testing (see also the use of !DEFINE-TYPE-VOPS
;;; in src/compiler/generic/late-type-vops.lisp):
;;;
;;; [FIXME: Like some of the other comments in this file, this one
;;; really belongs somewhere else]
(define-vop (check-type)
  (:args (value :target result :scs (any-reg descriptor-reg)))
  (:results (result :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg) :to (:result 0)) temp)
  (:vop-var vop)
  (:save-p :compute-only))

(define-vop (type-predicate)
  (:args (value :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe))

(defun cost-to-test-types (type-codes)
  (+ (* 2 (length type-codes))
     (if (> (apply #'max type-codes) lowtag-limit) 7 2)))

(defmacro !define-type-vops (pred-name check-name ptype error-code
                             (&rest type-codes)
                             &key &allow-other-keys)
  (let ((cost (cost-to-test-types (mapcar #'eval type-codes))))
    `(progn
       ,@(when pred-name
           `((define-vop (,pred-name type-predicate)
               (:translate ,pred-name)
               (:generator ,cost
                 (test-type value target not-p (,@type-codes)
                            :temp temp)))))
       ,@(when check-name
           `((define-vop (,check-name check-type)
               (:generator ,cost
                 (let ((err-lab
                        (generate-error-code vop ',error-code value)))
                   (test-type value err-lab t (,@type-codes)
                              :temp temp)
                   (move result value))))))
       ,@(when ptype
           `((primitive-type-vop ,check-name (:check) ,ptype))))))
