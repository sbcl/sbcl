;;;; floating point support for the ARM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;;; Move functions:

(define-move-fun (load-single 1) (vop x y)
                 ((single-stack) (single-reg))
  (loadw y (current-nfp-tn vop) (tn-offset x)))

(define-move-fun (store-single 1) (vop x y)
                 ((single-reg) (single-stack))
  (storew x (current-nfp-tn vop) (tn-offset y)))

(define-move-fun (load-double 2) (vop x y)
                 ((double-stack) (double-reg))
  (loadw y (current-nfp-tn vop) (tn-offset x)))

(define-move-fun (store-double 2) (vop x y)
                 ((double-reg) (double-stack))
  (storew x (current-nfp-tn vop) (tn-offset y)))

(define-move-fun (load-fp-immediate 1) (vop x y)
                 ((single-immediate) (single-reg)
                  (double-immediate) (double-reg)
                  (complex-single-immediate) (complex-single-reg)
                  (complex-double-immediate) (complex-double-reg))
  (let ((x (tn-value x)))
    (cond ((eql x #c(0d0 0d0))
           (inst movi y 0 :2d))
          ((member x '(0f0 0d0 #c(0f0 0f0)))
           (inst fmov y zr-tn))
          ((encode-fp-immediate x)
           (inst fmov y x))
          ((block nil
             (load-immediate-word tmp-tn (typecase x
                                           (double-float
                                            (double-float-bits x))
                                           (single-float
                                            (single-float-bits x))
                                           ((complex single-float)
                                            (ldb (byte 64 0)
                                                 (logior (ash (single-float-bits (imagpart x)) 32)
                                                         (ldb (byte 32 0)
                                                              (single-float-bits (realpart x))))))
                                           (t
                                            (return)))
                                  t))
           (inst fmov y tmp-tn))
          (t
           (load-inline-constant y x)))))


;;;; Move VOPs:

(macrolet ((frob (vop sc)
             `(progn
                (define-vop (,vop)
                  (:args (x :scs (,sc)
                            :target y
                            :load-if (not (location= x y))))
                  (:results (y :scs (,sc)
                               :load-if (not (location= x y))))
                  (:note "float move")
                  (:generator 0
                    (move-float y x)))
                (define-move-vop ,vop :move (,sc) (,sc)))))
  (frob single-move single-reg)
  (frob double-move double-reg))

(define-vop (move-from-single)
  (:args (x :scs (single-reg) :to :save))
  (:temporary (:sc non-descriptor-reg) tmp)
  (:results (y :scs (descriptor-reg)))
  (:note "float to pointer coercion")
  (:generator 4
    (inst fmov tmp x)
    (inst lsl tmp tmp 32)
    (inst add y tmp single-float-widetag)))

(define-move-vop move-from-single :move
  (single-reg) (descriptor-reg))

(define-vop (move-from-double)
  (:args (x :scs (double-reg) :to :save))
  (:results (y))
  (:note "float to pointer coercion")
  (:temporary (:scs (non-descriptor-reg) :offset lr-offset) lr)
  (:results (y :scs (descriptor-reg)))
  (:generator 13
    (with-fixed-allocation (y lr
                            double-float-widetag
                            double-float-value-slot)
      (storew x y double-float-value-slot other-pointer-lowtag))))

(define-move-vop move-from-double :move (double-reg) (descriptor-reg))

(define-vop (move-to-double)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (double-reg)))
  (:note "pointer to float coercion")
  (:generator 2
    (loadw y x double-float-value-slot other-pointer-lowtag)))
(define-move-vop move-to-double :move (descriptor-reg) (double-reg))

(define-vop (move-to-single-reg)
  (:args (x :scs (descriptor-reg) :target tmp))
  (:temporary (:sc unsigned-reg :from :argument :to :result) tmp)
  (:results (y :scs (single-reg)))
  (:note "pointer to float coercion")
  (:generator 2
   (inst lsr tmp x 32)
   (inst fmov y tmp)))

(define-move-vop move-to-single-reg :move (descriptor-reg) (single-reg))

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
                       (move-float y x))
                      (,stack-sc
                       (storew x nfp (tn-offset y))))))
                (define-move-vop ,name :move-arg
                  (,sc descriptor-reg) (,sc)))))
  (frob move-single-float-arg single-reg single-stack nil)
  (frob move-double-float-arg double-reg double-stack t))

;;;; Complex float move functions



(define-move-fun (load-complex-single 2) (vop x y)
  ((complex-single-stack) (complex-single-reg))
  (loadw y (current-nfp-tn vop) (tn-offset x)))

(define-move-fun (store-complex-single 2) (vop x y)
  ((complex-single-reg) (complex-single-stack))
  (storew x (current-nfp-tn vop) (tn-offset y)))

(define-move-fun (load-complex-double 4) (vop x y)
  ((complex-double-stack) (complex-double-reg))
  (loadw y (current-nfp-tn vop) (tn-offset x)))

(define-move-fun (store-complex-double 4) (vop x y)
  ((complex-double-reg) (complex-double-stack))
  (storew x (current-nfp-tn vop) (tn-offset y)))


;;;
;;; Complex float register to register moves.
;;;

(define-vop (complex-single-move)
    (:args (x :scs (complex-single-reg) :target y
              :load-if (not (location= x y))))
    (:results (y :scs (complex-single-reg) :load-if (not (location= x y))))
    (:note "complex single float move")
    (:generator 0
      (move-complex-double y x)))
(define-move-vop complex-single-move :move
  (complex-single-reg) (complex-single-reg))

(define-vop (complex-double-move)
  (:args (x :scs (complex-double-reg)
            :target y :load-if (not (location= x y))))
  (:results (y :scs (complex-double-reg) :load-if (not (location= x y))))
  (:note "complex double float move")
  (:generator 0
    (move-complex-double y x)))
(define-move-vop complex-double-move :move
  (complex-double-reg) (complex-double-reg))


;;;
;;; Move from a complex float to a descriptor register allocating a
;;; new complex float object in the process.
;;;
(define-vop (move-from-complex-single)
  (:args (x :scs (complex-single-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg) :offset lr-offset) lr)
  (:note "complex single float to pointer coercion")
  (:generator 13
    (with-fixed-allocation (y lr complex-single-float-widetag
                            complex-single-float-size)
      (inst str x (@ y (- (* complex-single-float-data-slot
                             n-word-bytes)
                          other-pointer-lowtag))))))
(define-move-vop move-from-complex-single :move
  (complex-single-reg) (descriptor-reg))

(define-vop (move-from-complex-double)
  (:args (x :scs (complex-double-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg) :offset lr-offset) lr)
  (:note "complex double float to pointer coercion")
  (:generator 13
    (with-fixed-allocation (y lr complex-double-float-widetag
                            complex-double-float-size)
      (inst str x (@ y (- (* complex-double-float-real-slot
                             n-word-bytes)
                          other-pointer-lowtag))))))
(define-move-vop move-from-complex-double :move
  (complex-double-reg) (descriptor-reg))

;;;
;;; Move from a descriptor to a complex float register
;;;
(define-vop (move-to-complex-single)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (complex-single-reg)))
  (:note "pointer to complex float coercion")
  (:generator 2
    (loadw y x complex-single-float-data-slot
        other-pointer-lowtag)))
(define-move-vop move-to-complex-single :move
  (descriptor-reg) (complex-single-reg))

(define-vop (move-to-complex-double)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (complex-double-reg)))
  (:note "pointer to complex float coercion")
  (:generator 2
    (loadw y x complex-double-float-real-slot
        other-pointer-lowtag)))
(define-move-vop move-to-complex-double :move
  (descriptor-reg) (complex-double-reg))


;;;
;;; Complex float move-arg vop
;;;
(define-vop (move-complex-single-float-arg)
  (:args (x :scs (complex-single-reg) :target y)
         (nfp :scs (any-reg) :load-if (not (sc-is y complex-single-reg))))
  (:results (y))
  (:note "complex single-float arg move")
  (:generator 1
    (sc-case y
      (complex-single-reg
       (move-float y x))
      (complex-single-stack
       (storew x nfp (tn-offset y))))))
(define-move-vop move-complex-single-float-arg :move-arg
  (complex-single-reg descriptor-reg) (complex-single-reg))

(define-vop (move-complex-double-float-arg)
  (:args (x :scs (complex-double-reg) :target y)
         (nfp :scs (any-reg) :load-if (not (sc-is y complex-double-reg))))
  (:results (y))
  (:note "complex double-float arg move")
  (:generator 2
    (sc-case y
      (complex-double-reg
       (move-complex-double y x))
      (complex-double-stack
       (storew x nfp (tn-offset y))))))
(define-move-vop move-complex-double-float-arg :move-arg
  (complex-double-reg descriptor-reg) (complex-double-reg))

;;;; Unboxed-to-boxed MOVE-ARG handling:

;; This little gem here says to use the VOP MOVE-ARG to move any float
;; registers to boxed data.  MOVE-ARG only takes boxed data as input,
;; which means that the :MOVE VOPs will be used to do the appropriate
;; conversion.
(define-move-vop move-arg :move-arg
  (single-reg double-reg complex-single-reg complex-double-reg)
  (descriptor-reg))

;;;; Arithmetic VOPs:

(define-vop (float-op)
  (:args (x) (y))
  (:results (r))
  (:policy :fast-safe)
  (:note "inline float arithmetic")
  (:vop-var vop)
  (:save-p :compute-only))

(macrolet ((frob (name sc ptype)
             `(define-vop (,name float-op)
                (:args (x :scs (,sc))
                       (y :scs (,sc)))
                (:results (r :scs (,sc)))
                (:arg-types ,ptype ,ptype)
                (:result-types ,ptype))))
  (frob single-float-op single-reg single-float)
  (frob double-float-op double-reg double-float)
  (frob complex-single-op complex-single-reg complex-single-float)
  (frob complex-double-op complex-double-reg complex-double-float))

(macrolet ((frob (op inst sname scost dname dcost
                  &optional cinst csname cscost cdname cdcost)
             `(progn
                (define-vop (,sname single-float-op)
                  (:translate ,op)
                  (:generator ,scost
                    (inst ,inst r x y)))
                (define-vop (,dname double-float-op)
                  (:translate ,op)
                  (:generator ,dcost
                    (inst ,inst r x y)))
                ,(when csname
                   `(define-vop (,csname complex-single-op)
                      (:translate ,op)
                      (:generator ,cscost
                        (inst ,cinst r x y :2s))))
                ,(when cdname
                   `(define-vop (,cdname complex-double-op)
                      (:translate ,op)
                      (:generator ,cdcost
                        (inst ,cinst r x y :2d)))))))
  (frob + fadd +/single-float 2 +/double-float 2
          s-fadd +/complex-single-float 3 +/complex-double-float 3)
  (frob - fsub -/single-float 2 -/double-float 2
          s-fsub -/complex-single-float 3 -/complex-double-float 3)
  (frob * fmul */single-float 4  */double-float 5)
  (frob / fdiv //single-float 12 //double-float 19))

;;; Real-complex and complex-real arithmetic
(macrolet ((frob (op inst cost duplicatep
                  single-real-complex-name single-complex-real-name
                  double-real-complex-name double-complex-real-name)
             (flet ((gen (real-complex-name complex-real-name
                          real-type complex-type real-sc complex-sc
                          complex-inst-size)
                      (list
                       (when real-complex-name
                         `(define-vop (,real-complex-name)
                            (:translate ,op)
                            (:policy :fast-safe)
                            (:args (x :scs (,real-sc)) (y :scs (,complex-sc)))
                            (:results (r :scs (,complex-sc)))
                            ,@(when duplicatep `((:temporary (:sc ,complex-sc) dup)))
                            (:arg-types ,real-type ,complex-type)
                            (:result-types ,complex-type)
                            (:generator ,cost
                               ,@(if duplicatep
                                     `((inst zip1 dup x x ,complex-inst-size)
                                       (inst ,inst r dup y ,complex-inst-size))
                                     `((inst ,inst r x y ,complex-inst-size))))))
                       (when complex-real-name
                         `(define-vop (,complex-real-name)
                            (:translate ,op)
                            (:policy :fast-safe)
                            (:args (x :scs (,complex-sc)) (y :scs (,real-sc)))
                            (:results (r :scs (,complex-sc)))
                            ,@(when duplicatep `((:temporary (:sc ,complex-sc) dup)))
                            (:arg-types ,complex-type ,real-type)
                            (:result-types ,complex-type)
                            (:generator ,cost
                               ,@(if duplicatep
                                     `((inst zip1 dup y y ,complex-inst-size)
                                       (inst ,inst r x dup ,complex-inst-size))
                                     `((inst ,inst r x y ,complex-inst-size)))))))))
               `(progn
                  ,@(gen single-real-complex-name single-complex-real-name
                         'single-float 'complex-single-float
                         'single-reg 'complex-single-reg ':2s)
                  ,@(gen double-real-complex-name double-complex-real-name
                         'double-float 'complex-double-float
                         'double-reg 'complex-double-reg ':2d)))))
  (frob + s-fadd 3 nil
        +/real-complex-single-float +/complex-real-single-float
        +/real-complex-double-float +/complex-real-double-float)
  (frob - s-fsub 3 nil
        -/real-complex-single-float -/complex-real-single-float
        -/real-complex-double-float -/complex-real-double-float)
  (frob * s-fmul 6 t
        */real-complex-single-float */complex-real-single-float
        */real-complex-double-float */complex-real-double-float)
  (frob / s-fdiv 20 t
        nil //complex-real-single-float
        nil //complex-real-double-float))

(macrolet ((frob (name inst translate sc type &rest suffix)
             `(define-vop (,name)
                (:args (x :scs (,sc)))
                (:results (y :scs (,sc)))
                (:translate ,translate)
                (:policy :fast-safe)
                (:arg-types ,type)
                (:result-types ,type)
                (:note "inline float arithmetic")
                (:vop-var vop)
                (:save-p :compute-only)
                (:generator 1
                  (note-this-location vop :internal-error)
                  (inst ,inst y x ,@suffix)))))
  (frob abs/single-float fabs abs single-reg single-float)
  (frob abs/double-float fabs abs double-reg double-float)
  (frob %negate/single-float fneg %negate single-reg single-float)
  (frob %negate/double-float fneg %negate double-reg double-float)
  (frob %negate/complex-single-float s-fneg %negate
        complex-single-reg complex-single-float :2s)
  (frob %negate/complex-double-float s-fneg %negate
        complex-double-reg complex-double-float :2d))

(macrolet ((frob (name sc type real-inst-size complex-inst-size)
             `(define-vop (,name)
                (:args (x :scs (,sc)))
                (:results (r :scs (,sc) :from :load))
                (:translate conjugate)
                (:policy :fast-safe)
                (:arg-types ,type)
                (:result-types ,type)
                (:generator 1
                   (inst s-fneg r x ,complex-inst-size)
                   (inst ins r 0 x 0 ,real-inst-size)))))
  (frob conjugate/complex-single-float complex-single-reg complex-single-float :s :2s)
  (frob conjugate/complex-double-float complex-double-reg complex-double-float :d :2d))

(macrolet ((frob (name sc type complex-inst-size real-inst-size cost swap-y)
             `(define-vop (,name)
                (:args (x :scs (,sc)) (y :scs (,sc)))
                (:results (r :scs (,sc)))
                (:translate *)
                (:policy :fast-safe)
                (:arg-types ,type ,type)
                (:result-types ,type)
                (:temporary (:sc ,sc) real imag swap-y)
                (:generator ,cost
                   ;; We want x * y = (rx*ry - ix*iy) + (rx*iy + ix*ry)*i.
                   ;; We separate that into x * y = real + imag where
                   ;;   real =  rx*ry + rx*iy*i
                   ;;   imag = -ix*iy + ix*ry*i
                   ;; (named after the parts of x that appear in them).
                   ;; Denote a vector which holds real part r and
                   ;; imaginary part i as [r i].
                   ;; Start with [ rx rx]
                   (inst trn1 real x x ,complex-inst-size)
                   ;;        and [-ix ix]
                   (inst trn2 imag x x ,complex-inst-size)
                   (inst s-fneg imag imag ,complex-inst-size)
                   (inst ins imag 1 x 1 ,real-inst-size)
                   ,swap-y
                   ;; Then we compute real = [ rx*ry rx*iy]
                   (inst s-fmul real real y ,complex-inst-size)
                   ;;             and imag = [-ix*iy ix*ry]
                   (inst s-fmul imag imag swap-y ,complex-inst-size)
                   ;; and finally add those to obtain x * y.
                   (inst s-fadd r real imag ,complex-inst-size)))))
  (frob */complex-single-float complex-single-reg complex-single-float :2s :s 20
        (inst rev64 swap-y y :2s))
  (frob */complex-double-float complex-double-reg complex-double-float :2d :d 25
        (inst ext swap-y y y 8)))

(define-vop (fsqrtd)
  (:args (x :scs (double-reg)))
  (:results (y :scs (double-reg)))
  (:translate %sqrt)
  (:policy :fast-safe)
  (:arg-types double-float)
  (:result-types double-float)
  (:note "inline float arithmetic")
  (:save-p :compute-only)
  (:generator 1
     (inst fsqrt y x)))

(define-vop (fsqrts)
  (:args (x :scs (single-reg)))
  (:results (y :scs (single-reg)))
  (:translate %sqrtf)
  (:policy :fast-safe)
  (:arg-types single-float)
  (:result-types single-float)
  (:note "inline float arithmetic")
  (:save-p :compute-only)
  (:generator 1
    (inst fsqrt y x)))

;;;; Comparison:

(define-vop (float-compare)
  (:args (x) (y))
  (:variant-vars is-=)
  (:policy :fast-safe)
  (:note "inline float comparison")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 3
    (note-this-location vop :internal-error)
    (if is-=
        (inst fcmp x y)
        (inst fcmpe x y))))

(macrolet ((frob (name sc ptype)
             `(define-vop (,name float-compare)
                (:args (x :scs (,sc))
                       (y :scs (,sc)))
                (:arg-types ,ptype ,ptype))))
  (frob single-float-compare single-reg single-float)
  (frob double-float-compare double-reg double-float))

(macrolet ((frob (translate cond sname dname is-=)
             `(progn
                (define-vop (,sname single-float-compare)
                  (:translate ,translate)
                  (:conditional ,cond)
                  (:variant ,is-=))
                (define-vop (,dname double-float-compare)
                  (:translate ,translate)
                  (:conditional ,cond)
                  (:variant ,is-=)))))
  (frob < :mi </single-float </double-float nil)
  (frob > :gt >/single-float >/double-float nil)
  (frob <= :ls <=/single-float <=/double-float nil)
  (frob >= :ge >=/single-float >=/double-float nil)
  (frob = :eq =/single-float =/double-float t))

(define-vop (float-compare-zero)
  (:args (x))
  (:info y)
  (:ignore y)
  (:variant-vars is-=)
  (:policy :fast-safe)
  (:note "inline float comparison")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 2
    (note-this-location vop :internal-error)
    (if is-=
        (inst fcmp x 0)
        (inst fcmpe x 0))))

(macrolet ((frob (name sc ptype constant-type)
             `(define-vop (,name float-compare-zero)
                (:args (x :scs (,sc)))
                (:arg-types ,ptype (:constant, constant-type)))))
  (frob single-float-compare-zero single-reg single-float
        (single-float -0f0 0f0))
  (frob double-float-compare-zero double-reg double-float
        (double-float -0d0 0d0)))

(macrolet ((frob (translate cond sname dname is-=)
             `(progn
                (define-vop (,sname single-float-compare-zero)
                  (:translate ,translate)
                  (:conditional ,cond)
                  (:variant ,is-=))
                (define-vop (,dname double-float-compare-zero)
                  (:translate ,translate)
                  (:conditional ,cond)
                  (:variant ,is-=)))))
  (frob < :mi </single-float-zero </double-float-zero nil)
  (frob > :gt >/single-float-zero >/double-float-zero nil)
  (frob <= :ls <=/single-float-zero <=/double-float-zero nil)
  (frob >= :ge >=/single-float-zero >=/double-float-zero nil)
  (frob = :eq eql/single-float-zero eql/double-float-zero t))

(macrolet ((define-complex-float-=
               (complex-complex-name complex-real-name real-complex-name
                eql-complex-complex-name
                complex-sc complex-type real-sc real-type
                float-size byte-size)
             `(progn
                (define-vop (,complex-complex-name)
                  (:translate =)
                  (:args (x :scs (,complex-sc)) (y :scs (,complex-sc)))
                  (:arg-types ,complex-type ,complex-type)
                  (:temporary (:sc ,complex-sc) mask)
                  (:temporary (:sc unsigned-reg) min)
                  (:conditional :ne)
                  (:policy :fast-safe)
                  (:generator 3
                     (inst fcmeq mask x y ,float-size)
                     (inst uminv mask mask ,byte-size)
                     (inst umov min mask 0 :b)
                     (inst cmp min 0)))
                (define-vop (,eql-complex-complex-name)
                  (:translate eql)
                  (:args (x :scs (,complex-sc)) (y :scs (,complex-sc)))
                  (:arg-types ,complex-type ,complex-type)
                  (:temporary (:sc ,complex-sc) mask)
                  (:temporary (:sc unsigned-reg) min)
                  (:conditional :ne)
                  (:policy :fast-safe)
                  (:generator 3
                     (inst cmeq mask x y ,byte-size)
                     (inst uminv mask mask ,byte-size)
                     (inst umov min mask 0 :b)
                     (inst cmp min 0)))
                (define-vop (,real-complex-name ,complex-complex-name)
                  (:args (x :scs (,real-sc)) (y :scs (,complex-sc)))
                  (:arg-types ,real-type ,complex-type))
                (define-vop (,complex-real-name ,complex-complex-name)
                  (:args (x :scs (,complex-sc)) (y :scs (,real-sc)))
                  (:arg-types ,complex-type ,real-type)))))
  (define-complex-float-=
      =/complex-single-float =/complex-real-single-float
      =/real-complex-single-float eql/complex-single-float
    complex-single-reg complex-single-float single-reg single-float :2s :8b)
  (define-complex-float-=
      =/complex-double-float =/complex-real-double-float
      =/real-complex-double-float eql/complex-double-float
    complex-double-reg complex-double-float double-reg double-float :2d :16b))

;;;; Conversion:

(macrolet ((frob (name translate from-sc from-type to-sc to-type)
            `(define-vop (,name)
               (:args (x :scs (,from-sc)))
               (:results (y :scs (,to-sc)))
               (:arg-types ,from-type)
               (:result-types ,to-type)
               (:policy :fast-safe)
               (:note "inline float coercion")
               (:translate ,translate)
               (:vop-var vop)
               (:save-p :compute-only)
               (:generator 5
                 (inst ,(if (eq from-type 'signed-num)
                            'scvtf
                            'ucvtf) y x)))))
  (frob %single-float/signed %single-float
        signed-reg signed-num single-reg single-float)
  (frob %double-float/signed %double-float
        signed-reg signed-num double-reg double-float)
  (frob %single-float/unsigned %single-float
        unsigned-reg unsigned-num single-reg single-float)
  (frob %double-float/unsigned %double-float
        unsigned-reg unsigned-num double-reg double-float))

(macrolet ((frob (name translate from-sc from-type to-sc to-type)
             `(define-vop (,name)
                (:args (x :scs (,from-sc)))
                (:results (y :scs (,to-sc)))
                (:arg-types ,from-type)
                (:result-types ,to-type)
                (:policy :fast-safe)
                (:note "inline float coercion")
                (:translate ,translate)
                (:vop-var vop)
                (:save-p :compute-only)
                (:generator 2
                  (note-this-location vop :internal-error)
                  (inst fcvt y x)))))
  (frob %single-float/double-float %single-float
    double-reg double-float single-reg single-float)
  (frob %double-float/single-float %double-float
    single-reg single-float double-reg double-float))

(macrolet ((frob (trans from-sc from-type inst)
             `(define-vop (,(if (find #\/ (string trans))
                                trans
                                (symbolicate trans "/" from-type)))
                (:args (x :scs (,from-sc)))
                (:results (y :scs (signed-reg)))
                (:arg-types ,from-type)
                (:result-types signed-num)
                (:translate ,trans)
                (:policy :fast-safe)
                (:note "inline float truncate")
                (:vop-var vop)
                (:save-p :compute-only)
                (:generator 5
                  (note-this-location vop :internal-error)
                  (inst ,inst y x)))))
  (frob %unary-truncate/single-float single-reg single-float fcvtzs)
  (frob %unary-truncate/double-float double-reg double-float fcvtzs)
  (frob %unary-round single-reg single-float fcvtns)
  (frob %unary-round double-reg double-float fcvtns)
  (frob %unary-ceiling single-reg single-float fcvtps)
  (frob %unary-ceiling double-reg double-float fcvtps)
  (frob %unary-floor single-reg single-float fcvtms)
  (frob %unary-floor double-reg double-float fcvtms))

(define-vop (make-single-float)
  (:args (bits :scs (signed-reg) :target res
               :load-if (not (sc-is bits signed-stack))))
  (:results (res :scs (single-reg)
                 :load-if (not (sc-is res single-stack))))
  (:temporary (:scs (signed-reg) :from (:argument 0) :to (:result 0)) temp)
  (:arg-types signed-num)
  (:result-types single-float)
  (:translate make-single-float)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 4
    (sc-case bits
      (signed-reg
       (sc-case res
         (single-reg
          (move-float res bits))
         (single-stack
          (storew bits (current-nfp-tn vop) (tn-offset res)))))
      (signed-stack
       (sc-case res
         (single-reg
          (loadw res (current-nfp-tn vop) (tn-offset res)))
         (single-stack
          (unless (location= bits res)
            (loadw temp (current-nfp-tn vop) (tn-offset bits))
            (storew temp (current-nfp-tn vop) (tn-offset res)))))))))

(define-vop (make-double-float)
  (:args (hi-bits :scs (signed-reg))
         (lo-bits :scs (unsigned-reg)))
  (:results (res :scs (double-reg)
                 :load-if (not (sc-is res double-stack))))
  (:arg-types signed-num unsigned-num)
  (:result-types double-float)
  (:translate make-double-float)
  (:temporary (:sc unsigned-reg) temp)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (inst orr temp lo-bits (lsl hi-bits 32))
    (sc-case res
      (double-reg
       (inst fmov res temp))
      (double-stack
       (storew temp (current-nfp-tn vop) (tn-offset res))))))

(define-vop (%make-double-float)
  (:args (bits :scs (signed-reg)))
  (:results (res :scs (double-reg)
                 :load-if (not (sc-is res double-stack))))
  (:arg-types signed-num)
  (:result-types double-float)
  (:translate %make-double-float)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (sc-case res
      (double-reg
       (inst fmov res bits))
      (double-stack
       (storew bits (current-nfp-tn vop) (tn-offset res))))))

(define-vop (single-float-bits)
  (:args (float :scs (single-reg descriptor-reg)
                :load-if (not (sc-is float single-stack))))
  (:results (bits :scs (signed-reg)
                  :load-if (or (sc-is float descriptor-reg single-stack)
                               (not (sc-is bits signed-stack)))))
  (:arg-types single-float)
  (:result-types signed-num)
  (:translate single-float-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 4
    (sc-case bits
      (signed-reg
       (sc-case float
         (single-reg
          (inst fmov bits float)
          (inst sxtw bits bits))
         (single-stack
          (inst ldrsw (32-bit-reg bits)
                (@ (current-nfp-tn vop)
                   (load-store-offset (ash (tn-offset float) 3)))))
         (descriptor-reg
          (inst asr bits float 32))))
      (signed-stack
       (sc-case float
         (single-reg
          (storew (32-bit-reg float) (current-nfp-tn vop) (tn-offset bits)))
         ((single-stack descriptor-reg)
          ;; Fun and games: This also affects PPC, silently.
          ;; Hopefully it's a non-issue, but I'd rather have the
          ;; explicit error than a silent miscompilation.
          (bug "Unable to extract single-float bits from ~S to ~S" float bits)))))))

(define-vop (double-float-bits)
  (:args (float :scs (double-reg descriptor-reg)))
  (:results (bits :scs (signed-reg)))
  (:arg-types double-float)
  (:result-types signed-num)
  (:translate double-float-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case float
      (double-reg
       (inst fmov bits float))
      (double-stack
        (inst ldr bits
              (@ (current-nfp-tn vop)
                 (load-store-offset (tn-byte-offset float)))))
      (descriptor-reg
       (inst ldr bits
             (@ float (- (* double-float-value-slot n-word-bytes)
                         other-pointer-lowtag)))))))

(define-vop (double-float-high-bits)
  (:args (float :scs (double-reg descriptor-reg)))
  (:results (hi-bits :scs (signed-reg)))
  (:arg-types double-float)
  (:result-types signed-num)
  (:translate double-float-high-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case float
      (double-reg
       (inst fmov hi-bits float)
       (inst asr hi-bits hi-bits 32))
      (double-stack
        (inst ldr (32-bit-reg hi-bits)
              (@ (current-nfp-tn vop)
                 (load-store-offset
                  (+ (tn-byte-offset float)
                     (if (eq *backend-byte-order* :big-endian)
                         0
                         4))))))
      (descriptor-reg
       (inst ldrsw (32-bit-reg hi-bits)
             (@ float
                (- (+ (* double-float-value-slot n-word-bytes)
                      (if (eq *backend-byte-order* :big-endian)
                          0
                          4))
                   other-pointer-lowtag)))))))

(define-vop (double-float-low-bits)
  (:args (float :scs (double-reg descriptor-reg)
                :load-if (not (sc-is float double-stack))))
  (:results (lo-bits :scs (unsigned-reg)))
  (:arg-types double-float)
  (:result-types unsigned-num)
  (:translate double-float-low-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case float
      (double-reg
       (inst fmov lo-bits float)
       (inst and lo-bits lo-bits (ldb (byte 32 0) -1)))
      (double-stack
       (inst ldr (32-bit-reg lo-bits)
             (@ (current-nfp-tn vop)
                (load-store-offset
                 (+ (tn-byte-offset float)
                    (if (eq *backend-byte-order* :big-endian)
                        4
                        0))))))
      (descriptor-reg
       (inst ldr (32-bit-reg lo-bits)
             (@ float
                (- (+ (* double-float-value-slot n-word-bytes)
                      (if (eq *backend-byte-order* :big-endian)
                          4
                          0))
                   other-pointer-lowtag)))))))

;;;; Float mode hackery:

(sb-xc:deftype float-modes () '(unsigned-byte 32))
(defknown floating-point-modes () float-modes (flushable))
(defknown ((setf floating-point-modes)) (float-modes)
  float-modes)

(define-vop (floating-point-modes)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate floating-point-modes)
  (:temporary (:sc unsigned-reg) tmp2)
  (:policy :fast-safe)
  (:generator 3
    (inst mrs res :fpsr)
    (inst and res res (load-immediate-word tmp-tn #xf800009f))
    (inst mrs tmp-tn :fpcr)
    (inst and tmp-tn tmp-tn (load-immediate-word tmp2 #x3ff8f00))
    (inst orr res res tmp-tn)))

(define-vop (set-floating-point-modes)
  (:args (new :scs (unsigned-reg) :target res))
  (:results (res :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:result-types unsigned-num)
  (:translate (setf floating-point-modes))
  (:policy :fast-safe)
  (:generator 3
    (inst and tmp-tn new (load-immediate-word tmp-tn #xf800009f))
    (inst msr :fpsr tmp-tn)
    (inst and tmp-tn new (load-immediate-word tmp-tn #x3ff8f00))
    (inst msr :fpcr tmp-tn)
    (move res new)))

;;;; Complex float VOPs

(define-vop (make-complex-single-float)
  (:translate complex)
  (:args (real :scs (single-reg) :target r
               :load-if (not (location= real r)))
         (imag :scs (single-reg) :to :save))
  (:arg-types single-float single-float)
  (:results (r :scs (complex-single-reg) :from (:argument 0)
               :load-if (not (sc-is r complex-single-stack))))
  (:result-types complex-single-float)
  (:note "inline complex single-float creation")
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case r
      (complex-single-reg
       (unless (eql (tn-offset r) (tn-offset real))
         (inst s-mov r real))
       (inst ins r 1 imag 0 :s))
      (complex-single-stack
       (let ((nfp (current-nfp-tn vop))
             (offset (tn-offset r)))
         (cond ((location= real r)
                (inst str imag
                      (@ nfp (load-store-offset (+ (* offset n-word-bytes) 4)))))
               ((ldp-stp-offset-p offset 32)
                (inst stp real imag (@ nfp (* offset n-word-bytes))))
               (t
                (storew real nfp offset)
                (inst str imag
                      (@ nfp (load-store-offset (+ (* offset n-word-bytes) 4)))))))))))

(define-vop (make-complex-double-float)
  (:translate complex)
  (:args (real :scs (double-reg) :target r
               :load-if (not (location= real r)))
         (imag :scs (double-reg) :to :save))
  (:arg-types double-float double-float)
  (:results (r :scs (complex-double-reg) :from (:argument 0)
               :load-if (not (sc-is r complex-double-stack))))
  (:result-types complex-double-float)
  (:note "inline complex double-float creation")
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case r
      (complex-double-reg
       (unless (eql (tn-offset r) (tn-offset real))
         (inst s-mov r real))
       (inst ins r 1 imag 0 :d))
      (complex-double-stack
       (let ((nfp (current-nfp-tn vop))
             (offset (tn-offset r)))
         (cond ((location= real r)
                (storew imag nfp (1+ offset)))
               ((ldp-stp-offset-p offset 64)
                (inst stp real imag (@ nfp (* offset n-word-bytes))))
               (t
                (storew real nfp offset)
                (storew imag nfp (1+ offset)))))))))


(define-vop (complex-single-float-value)
  (:args (x :scs (complex-single-reg) :target r
            :load-if (not (sc-is x complex-single-stack))))
  (:arg-types complex-single-float)
  (:results (r :scs (single-reg)))
  (:result-types single-float)
  (:variant-vars slot)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 3
    (sc-case x
      (complex-single-reg
       (inst ins r 0 x (ecase slot
                           (:real 0)
                           (:imag 1))
             :s))
      (complex-single-stack
       (inst ldr r
             (@ (current-nfp-tn vop)
                (load-store-offset
                 (+ (* n-word-bytes (tn-offset x))
                    (ecase slot
                      (:real 0)
                      (:imag 4))))))))))

(define-vop (realpart/complex-single-float complex-single-float-value)
  (:translate realpart)
  (:note "complex single float realpart")
  (:variant :real))

(define-vop (imagpart/complex-single-float complex-single-float-value)
  (:translate imagpart)
  (:note "complex single float imagpart")
  (:variant :imag))

(define-vop (complex-double-float-value)
  (:args (x :scs (complex-double-reg) :target r
            :load-if (not (sc-is x complex-double-stack))))
  (:arg-types complex-double-float)
  (:results (r :scs (double-reg)))
  (:result-types double-float)
  (:variant-vars slot)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 3
    (sc-case x
      (complex-double-reg
       (inst ins r 0 x (ecase slot
                           (:real 0)
                           (:imag 1))
             :d))
      (complex-double-stack
       (loadw r (current-nfp-tn vop) (+ (ecase slot (:real 0) (:imag 1))
                                        (tn-offset x)))))))

(define-vop (realpart/complex-double-float complex-double-float-value)
  (:translate realpart)
  (:note "complex double float realpart")
  (:variant :real))

(define-vop (imagpart/complex-double-float complex-double-float-value)
  (:translate imagpart)
  (:note "complex double float imagpart")
  (:variant :imag))

(defknown swap-complex ((complex float)) (complex float)
    (foldable flushable movable always-translatable))
(defoptimizer (swap-complex derive-type) ((x))
  (sb-c::lvar-type x))
(defun swap-complex (x)
  (complex (imagpart x) (realpart x)))
(macrolet ((def (name sc type &body insts)
             `(define-vop (,name)
                (:translate swap-complex)
                (:policy :fast-safe)
                (:args (x :scs (,sc)))
                (:arg-types ,type)
                (:results (r :scs (,sc)))
                (:result-types ,type)
                (:generator 2 ,@insts))))
  (def swap-complex/complex-single-float complex-single-reg complex-single-float
    (inst rev64 r x :2s))
  (def swap-complex/complex-double-float complex-double-reg complex-double-float
    (inst ext r x x 8)))

(define-vop ()
  (:translate round-double)
  (:policy :fast-safe)
  (:args (x :scs (double-reg) :target r))
  (:arg-types double-float (:constant symbol))
  (:info mode)
  (:results (r :scs (double-reg)))
  (:result-types double-float)
  (:generator 2
    (ecase mode
      (:round (inst frintn r x))
      (:floor (inst frintm r x))
      (:ceiling (inst frintp r x))
      (:truncate (inst frintz r x)))))

(define-vop ()
  (:translate round-single)
  (:policy :fast-safe)
  (:args (x :scs (single-reg) :target r))
  (:arg-types single-float (:constant symbol))
  (:info mode)
  (:results (r :scs (single-reg)))
  (:result-types single-float)
  (:generator 2
    (ecase mode
      (:round (inst frintn r x))
      (:floor (inst frintm r x))
      (:ceiling (inst frintp r x))
      (:truncate (inst frintz r x)))))
