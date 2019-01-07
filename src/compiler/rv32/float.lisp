;;;; floating point support for the RV32

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
  (inst fload :single y (current-nfp-tn vop) (* (tn-offset x) n-word-bytes)))

(define-move-fun (store-single 1) (vop x y)
  ((single-reg) (single-stack))
  (inst fstore :single x (current-nfp-tn vop) (* (tn-offset y) n-word-bytes)))

(define-move-fun (load-double 2) (vop x y)
  ((double-stack) (double-reg))
  (inst fload :double x (current-nfp-tn vop) (* (tn-offset x) n-word-bytes)))

(define-move-fun (store-double 2) (vop x y)
  ((double-reg) (double-stack))
  (inst fstore :double x (current-nfp-tn vop) (* (tn-offset x) n-word-bytes)))

;;;; Move VOPs:

(macrolet ((frob (vop sc format)
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
                      (inst fmv ,format x y))))
                (define-move-vop ,vop :move (,sc) (,sc)))))
  (frob single-move single-reg :single)
  (frob double-move double-reg :double))

(define-vop (move-from-float)
  (:args (x :to :save))
  (:results (y))
  (:note "float to pointer coercion")
  (:variant-vars double-p size type data)
  (:generator 13
    (style-warn "MOVE-FROM-FLOAT is a stub.")))

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
                  (:note "pointer to float coercion")
                  (:generator 2
                    (style-warn "MOVE-TO-SINGLE/DOUBLE is a stub")))
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
                  (:generator 1))
                (define-move-vop ,name :move-arg
                  (,sc descriptor-reg) (,sc)))))
  (frob move-single-float-arg single-reg single-stack nil)
  (frob move-double-float-arg double-reg double-stack t))

;;;; Complex float move functions
(defun complex-single-reg-real-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'single-reg)
                  :offset (tn-offset x)))
(defun complex-single-reg-imag-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'single-reg)
                  :offset (1+ (tn-offset x))))

(defun complex-double-reg-real-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg)
                  :offset (tn-offset x)))
(defun complex-double-reg-imag-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg)
                  :offset (+ 2 (tn-offset x))))

(define-move-fun (load-complex-single 2) (vop x y)
  ((complex-single-stack) (complex-single-reg)))
(define-move-fun (store-complex-single 2) (vop x y)
  ((complex-single-reg) (complex-single-stack)))

(define-move-fun (load-complex-double 4) (vop x y)
  ((complex-double-stack) (complex-double-reg)))
(define-move-fun (store-complex-double 4) (vop x y)
  ((complex-double-reg) (complex-double-stack)))


;;;
;;; Complex float register to register moves.
;;;

(define-vop (complex-single-move)
  (:args (x :scs (complex-single-reg) :target y
            :load-if (not (location= x y))))
  (:results (y :scs (complex-single-reg) :load-if (not (location= x y))))
  (:note "complex single float move")
  (:generator 0
    (style-warn "COMPLEX-SINGLE-MOVE is a stub")))
(define-move-vop complex-single-move :move
  (complex-single-reg) (complex-single-reg))

(define-vop (complex-double-move)
  (:args (x :scs (complex-double-reg)
            :target y :load-if (not (location= x y))))
  (:results (y :scs (complex-double-reg) :load-if (not (location= x y))))
  (:note "complex double float move")
  (:generator 0
    (style-warn "COMPLEX-DOUBLE-MOVE is a stub")))
(define-move-vop complex-double-move :move
  (complex-double-reg) (complex-double-reg))


;;;
;;; Move from a complex float to a descriptor register allocating a
;;; new complex float object in the process.
;;;
(define-vop (move-from-complex-single)
  (:args (x :scs (complex-single-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:note "complex single float to pointer coercion")
  (:generator 13
    (style-warn "MOVE-FROM-COMPLEX-SINGLE is a stub")))
(define-move-vop move-from-complex-single :move
  (complex-single-reg) (descriptor-reg))

(define-vop (move-from-complex-double)
  (:args (x :scs (complex-double-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:note "complex double float to pointer coercion")
  (:generator 13
    (style-warn "MOVE-FROM-COMPLEX-DOUBLE is a stub")))
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
    (style-warn "MOVE-FROM-COMPLEX-SINGLE is a stub")))
(define-move-vop move-to-complex-single :move
  (descriptor-reg) (complex-single-reg))

(define-vop (move-to-complex-double)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (complex-double-reg)))
  (:note "pointer to complex float coercion")
  (:generator 2
    (style-warn "MOVE-FROM-COMPLEX-DOUBLE is a stub")))
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
    (style-warn "MOVE-COMPLEX-SINGLE-FLOAT-ARG is a stub")))
(define-move-vop move-complex-single-float-arg :move-arg
  (complex-single-reg descriptor-reg) (complex-single-reg))

(define-vop (move-complex-double-float-arg)
  (:args (x :scs (complex-double-reg) :target y)
         (nfp :scs (any-reg) :load-if (not (sc-is y complex-double-reg))))
  (:results (y))
  (:note "complex double-float arg move")
  (:generator 2
    (style-warn "MOVE-COMPLEX-DOUBLE-FLOAT-ARG is a stub")))

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
  (frob double-float-op double-reg double-float))

(macrolet ((frob (op inst sname scost dname dcost)
             `(progn
                (define-vop (,sname single-float-op)
                  (:translate ,op)
                  (:generator ,scost
                    (inst ,inst :single r x y)))
                (define-vop (,dname double-float-op)
                  (:translate ,op)
                  (:generator ,dcost
                    (inst ,inst :double r x y))))))
  (frob + fadd +/single-float 2 +/double-float 2)
  (frob - fsub -/single-float 2 -/double-float 2)
  (frob * fmul */single-float 4 */double-float 5)
  (frob / fdiv //single-float 12 //double-float 19))

(macrolet ((frob (name inst fmt translate sc type)
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
                  (inst ,inst ,fmt y x)))))
  (frob abs/single-float fabs :single abs single-reg single-float)
  (frob abs/double-float fabs :double abs double-reg double-float)
  (frob %negate/single-float fneg :single %negate single-reg single-float)
  (frob %negate/double-float fneg :double %negate double-reg double-float))

(macrolet ((frob (name fmt sc ptype)
             `(define-vop (,name)
                (:args (x :scs (,sc)))
                (:results (y :scs (,sc)))
                (:translate %sqrt)
                (:policy :fast-safe)
                (:arg-types ,ptype)
                (:result-types ,ptype)
                (:note "inline float arithmetic")
                (:save-p :compute-only)
                (:generator 1
                            (inst fsqrt ,fmt y x)))))
  (frob %sqrt/single-float :single single-reg single-float)
  (frob %sqrt/double-float :double double-reg double-float))


;;;; Comparison:
(define-vop (float-compare)
  (:args (x) (y))
  (:conditional)
  (:info target not-p)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:policy :fast-safe)
  (:note "inline float comparison")
  (:vop-var vop)
  (:save-p :compute-only))

(macrolet ((frob (name sc ptype)
             `(define-vop (,name float-compare)
                (:args (x :scs (,sc))
                       (y :scs (,sc)))
                (:arg-types ,ptype ,ptype))))
  (frob single-float-compare single-reg single-float)
  (frob double-float-compare double-reg double-float))

(macrolet ((frob (translate op complement sname dname)
             `(progn
                (define-vop (,sname single-float-compare)
                  (:translate ,translate)
                  (:generator 3
                    (note-this-location vop :internal-error)
                    (inst ,op :single temp x y)
                    ,(if complement
                         `(inst bne temp zero-tn target)
                         `(inst beq temp zero-tn target))))
                (define-vop (,dname double-float-compare)
                  (:translate ,translate)
                  (:generator 3
                    (note-this-location vop :internal-error)
                    (inst ,op :double temp x y)
                    ,(if complement
                         `(inst bne temp zero-tn target)
                         `(inst beq temp zero-tn target)))))))
  (frob < flt nil </single-float </double-float)
  (frob > fle t >/single-float >/double-float)
  (frob = feq nil =/single-float =/double-float))


;;;; Conversion:
(macrolet ((frob (name translate
                       from-sc from-type from-format
                       to-sc to-type to-format)
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
                (:generator ,2
                            (note-this-location vop :internal-error)
                            (inst fcvt ,to-format ,from-format y x)))))
  (frob %single-float/signed %single-float
    signed-reg signed-num :word
    single-reg single-float :single)
  (frob %single-float/unsigned %single-float
    signed-reg signed-num :unsigned-word
    single-reg single-float :single)
  (frob %double-float/signed %double-float
    signed-reg signed-num :word
    double-reg double-float :double)
  (frob %double-float/unsigned %double-float
    signed-reg signed-num :unsigned-word
    double-reg double-float :double)
  (frob %single-float/double-float %single-float
    double-reg double-float :double
    single-reg single-float :single)
  (frob %double-float/single-float %double-float
    single-reg single-float :single
    double-reg double-float :double))

(macrolet ((frob (name from-sc from-type from-format rm)
             `(define-vop (,name)
                (:args (x :scs (,from-sc)))
                (:results (y :scs (signed-reg)))
                (:arg-types ,from-type)
                (:result-types signed-num)
                (:translate %unary-round)
                (:policy :fast-safe)
                (:note "inline float round")
                (:vop-var vop)
                (:save-p :compute-only)
                (:generator 2
                  (note-this-location vop :internal-error)
                  (inst fcvt :word ,from-format y x ,rm)))))
  (frob %unary-round/single-float single-reg single-float :single :rne)
  (frob %unary-round/double-float double-reg double-float :double :rne)
  (frob %unary-truncate/single-float single-reg single-float :single :rtz)
  (frob %unary-truncate/double-float double-reg double-float :double :rtz))

(define-vop (make-single-float)
   (:args (bits :scs (signed-reg)))
   (:results (res :scs (single-reg)))
   (:arg-types signed-num)
   (:result-types single-float)
   (:translate make-single-float)
   (:policy :fast-safe)
   (:generator 1
     (inst fmvx-> :single res bits)))

#-64-bit
(define-vop (make-double-float)
  (:args (hi-bits :scs (signed-reg))
         (lo-bits :scs (unsigned-reg)))
  (:temporary (:scs (single-reg)) x y)
  (:results (res :scs (double-reg)))
  (:arg-types signed-num unsigned-num)
  (:result-types double-float)
  (:translate make-double-float)
  (:policy :fast-safe)
  (:generator 2
    (style-warn "MAKE-DOUBLE-FLOAT is a stub.")))

#+64-bit
(define-vop (make-double-float)
  (:args (hi-bits :scs (signed-reg))
         (lo-bits :scs (unsigned-reg)))
  (:results (res :scs (double-reg)))
  (:arg-types signed-num unsigned-num)
  (:result-types double-float)
  (:translate make-double-float)
  (:policy :fast-safe)
  (:generator 2
    (inst fmvx-> :double res bits)))

(define-vop (single-float-bits)
   (:args (float :scs (single-reg)))
   (:results (bits :scs (signed-reg)))
   (:arg-types single-float)
   (:result-types signed-num)
   (:translate single-float-bits)
   (:policy :fast-safe)
   (:generator 1
     (inst fmvx<- :single bits float)))

(define-vop (double-float-high-bits)
  (:args (float :scs (double-reg)))
  (:results (hi-bits :scs (signed-reg)))
  (:arg-types double-float)
  (:result-types signed-num)
  (:translate double-float-high-bits)
  (:policy :fast-safe)
  (:generator 2
    (style-warn "DOUBLE-FLOAT-HIGH-BITS is a stub")))

(define-vop (double-float-low-bits)
  (:args (float :scs (double-reg)))
  (:results (lo-bits :scs (unsigned-reg)))
  (:arg-types double-float)
  (:result-types unsigned-num)
  (:translate double-float-low-bits)
  (:policy :fast-safe)
  (:generator 2
    (style-warn "DOUBLE-FLOAT-LOW-BITS is a stub")))


;;;; Complex float VOPs

(define-vop (make-complex-single-float)
  (:translate complex)
  (:args (real :scs (single-reg) :target r)
         (imag :scs (single-reg) :to :save))
  (:arg-types single-float single-float)
  (:results (r :scs (complex-single-reg) :from (:argument 0)
               :load-if (not (sc-is r complex-single-stack))))
  (:result-types complex-single-float)
  (:note "inline complex single-float creation")
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (style-warn "MAKE-COMPLEX-SINGLE-FLOAT is a stub.")))

(define-vop (make-complex-double-float)
  (:translate complex)
  (:args (real :scs (double-reg) :target r)
         (imag :scs (double-reg) :to :save))
  (:arg-types double-float double-float)
  (:results (r :scs (complex-double-reg) :from (:argument 0)
               :load-if (not (sc-is r complex-double-stack))))
  (:result-types complex-double-float)
  (:note "inline complex double-float creation")
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (style-warn "MAKE-COMPLEX-DOUBLE-FLOAT is a stub")))

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
    (style-warn "COMPLEX-SINGLE-FLOAT-VALUE is a stub")))

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
    (style-warn "COMPLEX-DOUBLE-FLOAT-VALUE is a stub.")))

(define-vop (realpart/complex-double-float complex-double-float-value)
  (:translate realpart)
  (:note "complex double float realpart")
  (:variant :real))

(define-vop (imagpart/complex-double-float complex-double-float-value)
  (:translate imagpart)
  (:note "complex double float imagpart")
  (:variant :imag))
