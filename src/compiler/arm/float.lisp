;;;; floating point support for the ARM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; Move functions:

(define-move-fun (load-single 1) (vop x y)
  ((single-stack) (single-reg))
  (inst flds y (@ (current-nfp-tn vop) (* (tn-offset x) n-word-bytes))))

(define-move-fun (store-single 1) (vop x y)
  ((single-reg) (single-stack))
  (inst fsts x (@ (current-nfp-tn vop) (* (tn-offset y) n-word-bytes))))

(define-move-fun (load-double 2) (vop x y)
  ((double-stack) (double-reg))
  (let ((nfp (current-nfp-tn vop))
        (offset (* (tn-offset x) n-word-bytes)))
    (inst fldd y (@ nfp offset))))

(define-move-fun (store-double 2) (vop x y)
  ((double-reg) (double-stack))
  (let ((nfp (current-nfp-tn vop))
        (offset (* (tn-offset y) n-word-bytes)))
    (inst fstd x (@ nfp offset))))

;;;; Move VOPs:

(macrolet ((frob (vop sc op)
             `(progn
                (define-vop (,vop)
                  (:args (x :scs (,sc)
                            :target y
                            :load-if (not (location= x y))))
                  (:results (y :scs (,sc)
                               :load-if (not (location= x y))))
                  (:note "float move")
                  (:generator 0
                    (unless (location= y x)
                      (inst ,op y x))))
                (define-move-vop ,vop :move (,sc) (,sc)))))
  (frob single-move single-reg fcpys)
  (frob double-move double-reg fcpyd))

(define-vop (move-from-float)
  (:args (x :to :save))
  (:results (y))
  (:note "float to pointer coercion")
  (:temporary (:sc non-descriptor-reg :offset ocfp-offset) pa-flag)
  (:temporary (:sc interior-reg) lip)
  (:variant-vars double-p size type data)
  (:generator 13
    (with-fixed-allocation (y pa-flag type size)
      (inst sub lip y other-pointer-lowtag)
      (if double-p
          (inst fstd x (@ lip (* data n-word-bytes)))
          (inst fsts x (@ lip (* data n-word-bytes)))))))

(macrolet ((frob (name sc &rest args)
             `(progn
                (define-vop (,name move-from-float)
                  (:args (x :scs (,sc) :to :save))
                  (:results (y :scs (descriptor-reg)))
                  (:variant ,@args))
                (define-move-vop ,name :move (,sc) (descriptor-reg)))))
  (frob move-from-single single-reg
    nil single-float-size single-float-widetag single-float-value-slot)
  (frob move-from-double double-reg
    t double-float-size double-float-widetag double-float-value-slot))

(macrolet ((frob (name sc double-p value)
             `(progn
                (define-vop (,name)
                  (:args (x :scs (descriptor-reg)))
                  (:results (y :scs (,sc)))
                  (:temporary (:sc interior-reg) lip)
                  (:note "pointer to float coercion")
                  (:generator 2
                     (inst sub lip x other-pointer-lowtag)
                     (inst ,(if double-p 'fldd 'flds) y
                           (@ lip (* ,value n-word-bytes)))))
                (define-move-vop ,name :move (descriptor-reg) (,sc)))))
  (frob move-to-single single-reg nil single-float-value-slot)
  (frob move-to-double double-reg t double-float-value-slot))

(macrolet ((frob (name sc stack-sc double-p)
             `(progn
                (define-vop (,name)
                  (:args (x :scs (,sc) :target y)
                         (nfp :scs (any-reg)
                              :load-if (not (sc-is y ,sc))))
                  (:results (y))
                  (:note "float arg move")
                  (:generator ,(if double-p 2 1)
                    (sc-case y
                      (,sc
                       (unless (location= x y)
                         (inst ,(if double-p 'fcpyd 'fcpys) y x)))
                      (,stack-sc
                       (let ((offset (* (tn-offset y) n-word-bytes)))
                         (inst ,(if double-p 'fstd 'fsts) x (@ nfp offset)))))))
                (define-move-vop ,name :move-arg
                  (,sc descriptor-reg) (,sc)))))
  (frob move-single-float-arg single-reg single-stack nil)
  (frob move-double-float-arg double-reg double-stack t))

;;;; Unboxed-to-boxed MOVE-ARG handling:

;; This little gem here says to use the VOP MOVE-ARG to move any float
;; registers to boxed data.  MOVE-ARG only takes boxed data as input,
;; which means that the :MOVE VOPs will be used to do the appropriate
;; conversion.
(define-move-vop move-arg :move-arg
  (single-reg double-reg #| complex-single-reg complex-double-reg |#)
  (descriptor-reg))
