;;;; floating point support for the RISC-V

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
  (inst fload :single y (current-nfp-tn vop) (tn-byte-offset x)))

(define-move-fun (store-single 1) (vop x y)
  ((single-reg) (single-stack))
  (inst fstore :single x (current-nfp-tn vop) (tn-byte-offset y)))

(define-move-fun (load-double 2) (vop x y)
  ((double-stack) (double-reg))
  (inst fload :double y (current-nfp-tn vop) (tn-byte-offset x)))

(define-move-fun (store-double 2) (vop x y)
  ((double-reg) (double-stack))
  (inst fstore :double x (current-nfp-tn vop) (tn-byte-offset y)))

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
                      (inst fmove ,format y x))))
                (define-move-vop ,vop :move (,sc) (,sc)))))
  (frob single-move single-reg :single)
  (frob double-move double-reg :double))

(define-vop (move-from-float)
  (:args (x :to :save))
  (:results (y))
  (:note "float to pointer coercion")
  (:temporary (:sc non-descriptor-reg) pa-flag)
  (:variant-vars fmt size type data)
  (:generator 13
    (with-fixed-allocation (y pa-flag type size)
      (inst fstore fmt x y (- (* data n-word-bytes) other-pointer-lowtag)))))

#+64-bit
(define-vop (move-from-single)
  (:args (x :scs (single-reg) :to :save))
  (:temporary (:sc non-descriptor-reg) tmp)
  (:results (y :scs (descriptor-reg)))
  (:note "float to pointer coercion")
  (:generator 4
    (inst fmvx<- :single tmp x)
    (inst slli tmp tmp 32)
    (inst addi y tmp single-float-widetag)))

(macrolet ((frob (name sc &rest args)
             `(progn
                (define-vop (,name move-from-float)
                  (:args (x :scs (,sc) :to :save))
                  (:results (y :scs (descriptor-reg)))
                  (:variant ,@args)))))
  #-64-bit
  (frob move-from-single single-reg
        :single single-float-size single-float-widetag single-float-value-slot)
  (frob move-from-double double-reg
        :double double-float-size double-float-widetag double-float-value-slot))

(define-move-vop move-from-single :move
  (single-reg) (descriptor-reg))

(define-move-vop move-from-double :move
  (double-reg) (descriptor-reg))

(macrolet ((frob (name sc fmt value)
             `(progn
                (define-vop (,name)
                  (:args (x :scs (descriptor-reg)))
                  (:results (y :scs (,sc)))
                  (:note "pointer to float coercion")
                  (:generator 2
                    (inst fload ,fmt y x (- (* ,value n-word-bytes) other-pointer-lowtag))))
                (define-move-vop ,name :move (descriptor-reg) (,sc)))))
  #-64-bit
  (frob move-to-single single-reg :single single-float-value-slot)
  (frob move-to-double double-reg :double double-float-value-slot))

#+64-bit
(progn
  (define-vop (move-to-single-reg)
    (:args (x :scs (descriptor-reg) :target tmp))
    (:temporary (:sc unsigned-reg :from :argument :to :result) tmp)
    (:results (y :scs (single-reg)))
    (:note "pointer to float coercion")
    (:generator 2
      (inst srli tmp x 32)
      (inst fmvx-> :single y tmp)))

  (define-move-vop move-to-single-reg :move (descriptor-reg) (single-reg)))

(macrolet ((frob (name sc stack-sc format)
             `(progn
                (define-vop (,name)
                  (:args (x :scs (,sc) :target y)
                         (nfp :scs (any-reg)
                              :load-if (not (sc-is y ,sc))))
                  (:results (y))
                  (:note "float arg move")
                  (:generator 1
                    (sc-case y
                      (,sc
                       (unless (location= x y)
                         (inst fmove ,format y x)))
                      (,stack-sc
                       (inst fstore ,format x nfp (tn-byte-offset y))))))
                (define-move-vop ,name :move-arg
                  (,sc descriptor-reg) (,sc)))))
  (frob move-single-float-arg single-reg single-stack :single)
  (frob move-double-float-arg double-reg double-stack :double))

;;;; Complex float move functions
(defun format-sc (format)
  (ecase format (:single 'single-reg) (:double 'double-reg)))
(defun complex-reg-real-tn (format x)
  (make-random-tn (sc-or-lose (format-sc format)) (tn-offset x)))
(defun complex-reg-imag-tn (format x)
  (make-random-tn (sc-or-lose (format-sc format)) (1+ (tn-offset x))))

(macrolet ((def (name cost stack-sc sc op format size a b)
             `(define-move-fun (,name ,cost) (vop x y)
                ((,stack-sc) (,sc))
                (let ((nfp (current-nfp-tn vop))
                      (offset (tn-byte-offset ,b)))
                  (let ((real-tn (complex-reg-real-tn ,format ,a)))
                    (inst ,op ,format real-tn nfp offset))
                  (let ((imag-tn (complex-reg-imag-tn ,format ,a)))
                    (inst ,op ,format imag-tn nfp (+ offset ,size)))))))
  #-64-bit
  (def load-complex-single 2 complex-single-stack complex-single-reg fload :single 4 y x)
  #-64-bit
  (def store-complex-single 2 complex-single-reg complex-single-stack fstore :single 4 x y)
  (def load-complex-double 2 complex-double-stack complex-double-reg fload :double 8 y x)
  (def store-complex-double 2 complex-double-reg complex-double-stack fstore :double 8 x y))

#+64-bit
(macrolet ((def (name cost stack-sc sc op a b)
             `(define-move-fun (,name ,cost) (vop x y)
                               ((,stack-sc) (,sc))
                (let ((nfp (current-nfp-tn vop))
                      (offset (tn-byte-offset ,b)))
                  (inst ,op :double ,a nfp offset)))))
  (def load-complex-single 2 complex-single-stack complex-single-reg fload y x)
  (def store-complex-single 2 complex-single-reg complex-single-stack fstore x y))


;;;
;;; Complex float register to register moves.
;;;

(defun move-complex (format y x)
  (ecase format
    #+64-bit
    (:single
     (unless (location= x y)
       (inst fmove :double y x)))
    ((#-64-bit :single :double)
     (unless (location= x y)
       ;; Note the complex-float-regs are aligned to every second
       ;; float register so there is not need to worry about overlap.
       (let ((x-real (complex-reg-real-tn format x))
             (y-real (complex-reg-real-tn format y)))
         (unless (location= x-real y-real)
           (inst fmove format y-real x-real)))
       (let ((x-imag (complex-reg-imag-tn format x))
             (y-imag (complex-reg-imag-tn format y)))
         (unless (location= x-imag y-imag)
           (inst fmove format y-imag x-imag)))))))

(define-vop (complex-single-move)
  (:args (x :scs (complex-single-reg) :target y
            :load-if (not (location= x y))))
  (:results (y :scs (complex-single-reg) :load-if (not (location= x y))))
  (:note "complex single float move")
  (:generator 0
    (move-complex :single y x)))

(define-move-vop complex-single-move :move
  (complex-single-reg) (complex-single-reg))

(define-vop (complex-double-move)
  (:args (x :scs (complex-double-reg)
            :target y :load-if (not (location= x y))))
  (:results (y :scs (complex-double-reg) :load-if (not (location= x y))))
  (:note "complex double float move")
  (:generator 0
    (move-complex :double y x)))

(define-move-vop complex-double-move :move
  (complex-double-reg) (complex-double-reg))


;;;
;;; Move from a complex float to a descriptor register allocating a
;;; new complex float object in the process.
;;;

(define-vop (move-from-complex-float)
  (:args (x))
  (:results (y :scs (descriptor-reg)))
  (:temporary (:sc non-descriptor-reg) pa-flag)
  (:variant-vars format real-slot imag-slot widetag size)
  (:generator 13
    (with-fixed-allocation (y pa-flag widetag size)
      (let ((real-tn (complex-reg-real-tn format x)))
        (inst fstore format real-tn y (- (* real-slot n-word-bytes)
                                         other-pointer-lowtag)))
      (let ((imag-tn (complex-reg-imag-tn format x)))
        (inst fstore format imag-tn y (- (* imag-slot n-word-bytes)
                                         other-pointer-lowtag))))))

#-64-bit
(define-vop (move-from-complex-single move-from-complex-float)
  (:args (x :scs (complex-single-reg) :to :save))
  (:note "complex single float to pointer coercion")
  (:variant :single complex-single-float-real-slot complex-single-float-imag-slot
            complex-single-float-widetag complex-single-float-size))

#+64-bit
(define-vop (move-from-complex-single)
  (:args (x :scs (complex-single-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:temporary (:sc non-descriptor-reg) pa-flag)
  (:note "complex single float to pointer coercion")
  (:generator 13
     (with-fixed-allocation (y pa-flag complex-single-float-widetag
                             complex-single-float-size)
       (inst fstore :double x y (- (* complex-single-float-data-slot
                                      n-word-bytes)
                                   other-pointer-lowtag)))))

(define-move-vop move-from-complex-single :move
  (complex-single-reg) (descriptor-reg))

(define-vop (move-from-complex-double move-from-complex-float)
  (:args (x :scs (complex-double-reg) :to :save))
  (:note "complex double float to pointer coercion")
  (:variant :double complex-double-float-real-slot complex-double-float-imag-slot
            complex-double-float-widetag complex-double-float-size))

(define-move-vop move-from-complex-double :move
  (complex-double-reg) (descriptor-reg))


;;;
;;; Move from a descriptor to a complex float register
;;;
(define-vop (move-to-complex-float)
  (:args (x :scs (descriptor-reg)))
  (:results (y))
  (:note "pointer to complex float coercion")
  (:variant-vars format real-slot imag-slot)
  (:generator 2
    (let ((real-tn (complex-reg-real-tn format y)))
      (inst fload format real-tn x (- (* real-slot n-word-bytes)
                                      other-pointer-lowtag)))
    (let ((imag-tn (complex-reg-imag-tn format y)))
      (inst fload format imag-tn x (- (* imag-slot n-word-bytes)
                                      other-pointer-lowtag)))))
#-64-bit
(define-vop (move-to-complex-single move-to-complex-float)
  (:results (y :scs (complex-single-reg)))
  (:variant :single complex-single-float-real-slot complex-single-float-imag-slot))

#+64-bit
(define-vop (move-to-complex-single)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (complex-single-reg)))
  (:note "pointer to complex float coercion")
  (:generator 2
    (inst fload :double y x (- (* complex-single-float-data-slot n-word-bytes)
                               other-pointer-lowtag))))

(define-move-vop move-to-complex-single :move
  (descriptor-reg) (complex-single-reg))

(define-vop (move-to-complex-double move-to-complex-float)
  (:results (y :scs (complex-double-reg)))
  (:variant :double complex-double-float-real-slot complex-double-float-imag-slot))
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
       (move-complex :single y x))
      (complex-single-stack
       (let ((offset (tn-byte-offset y)))
         #-64-bit
         (progn
           (let ((real-tn (complex-reg-real-tn :single x)))
             (inst fstore :single real-tn nfp offset))
           (let ((imag-tn (complex-reg-imag-tn :single x)))
             (inst fstore :single imag-tn nfp (+ offset 4))))
         #+64-bit
         (inst fstore :double x nfp offset))))))

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
       (move-complex :double y x))
      (complex-double-stack
       (let ((offset (tn-byte-offset y)))
         (let ((real-tn (complex-reg-real-tn :double x)))
           (inst fstore :double real-tn nfp offset))
         (let ((imag-tn (complex-reg-imag-tn :double x)))
           (inst fstore :double imag-tn nfp (+ offset 8))))))))

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
  (frob * fmul */single-float 4 */double-float 4)
  (frob / fdiv //single-float 12 //double-float 12))

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
                    (if ,(if complement '(not not-p) 'not-p)
                        (inst beq temp zero-tn target)
                        (inst bne temp zero-tn target))))
                (define-vop (,dname double-float-compare)
                  (:translate ,translate)
                  (:generator 3
                    (note-this-location vop :internal-error)
                    (inst ,op :double temp x y)
                    (if ,(if complement '(not not-p) 'not-p)
                        (inst beq temp zero-tn target)
                        (inst bne temp zero-tn target)))))))
  (frob < flt nil </single-float </double-float)
  (frob <= fle nil <=/single-float <=/double-float)
  (frob > fle t >/single-float >/double-float)
  (frob >= flt t >=/single-float >=/double-float)
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
                (:generator 2
                  (note-this-location vop :internal-error)
                  (inst fcvt ,to-format ,from-format y x)))))
  (frob %single-float/signed %single-float
    signed-reg signed-num :word
    single-reg single-float :single)
  (frob %single-float/unsigned %single-float
    unsigned-reg unsigned-num :unsigned-word
    single-reg single-float :single)
  (frob %double-float/signed %double-float
    signed-reg signed-num :word
    double-reg double-float :double)
  (frob %double-float/unsigned %double-float
    unsigned-reg unsigned-num :unsigned-word
    double-reg double-float :double)
  (frob %single-float/double-float %single-float
    double-reg double-float :double
    single-reg single-float :single)
  (frob %double-float/single-float %double-float
    single-reg single-float :single
    double-reg double-float :double))

(macrolet ((frob (name trans from-sc from-type from-format rm)
             `(define-vop (,name)
                (:args (x :scs (,from-sc)))
                (:results (y :scs (signed-reg)))
                (:arg-types ,from-type)
                (:result-types signed-num)
                (:translate ,trans)
                (:policy :fast-safe)
                (:note "inline float round/truncate")
                (:vop-var vop)
                (:save-p :compute-only)
                (:generator 2
                  (note-this-location vop :internal-error)
                  (inst fcvt :word ,from-format y x ,rm)))))
  (frob %unary-round/single-float %unary-round single-reg single-float :single :rne)
  (frob %unary-round/double-float %unary-round double-reg double-float :double :rne)
  (frob %unary-truncate/single-float %unary-truncate/single-float single-reg single-float :single :rtz)
  (frob %unary-truncate/double-float %unary-truncate/double-float double-reg double-float :double :rtz))

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
  (:results (res :scs (double-reg)))
  (:arg-types signed-num unsigned-num)
  (:result-types double-float)
  (:translate make-double-float)
  (:policy :fast-safe)
  (:generator 2
    (inst subi nsp-tn nsp-tn (* 2 n-word-bytes))
    (storew lo-bits nsp-tn 0)
    (storew hi-bits nsp-tn 1)
    (inst fload :double res nsp-tn 0)
    (inst addi nsp-tn nsp-tn (* 2 n-word-bytes))))

#+64-bit
(define-vop (make-double-float)
  (:args (hi-bits :scs (signed-reg))
         (lo-bits :scs (unsigned-reg)))
  (:results (res :scs (double-reg)))
  (:temporary (:sc unsigned-reg) temp)
  (:arg-types signed-num unsigned-num)
  (:result-types double-float)
  (:translate make-double-float)
  (:policy :fast-safe)
  (:generator 2
    (inst slli temp hi-bits 32)
    (inst add temp temp lo-bits)
    (inst fmvx-> :double res temp)))

#+64-bit
(define-vop (%make-double-float)
  (:args (bits :scs (signed-reg)))
  (:results (res :scs (double-reg)))
  (:arg-types signed-num)
  (:result-types double-float)
  (:translate %make-double-float)
  (:policy :fast-safe)
  (:generator 2
    (inst fmvx-> :double res bits)))

(define-vop (single-float-bits)
  (:args (float :scs (single-reg descriptor-reg)
                :load-if (not (sc-is float single-stack))))
  (:results (bits :scs (signed-reg)
                  :load-if (sc-is float descriptor-reg single-stack)))
  (:arg-types single-float)
  (:result-types signed-num)
  (:translate single-float-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 1
    (sc-case float
      (single-reg
       (inst fmvx<- :single bits float))
      (single-stack
       (inst lw bits (current-nfp-tn vop) (tn-byte-offset float)))
      (descriptor-reg
       #+64-bit
       (inst srai bits float 32)
       #-64-bit
       (loadw bits float single-float-value-slot other-pointer-lowtag)))))

#+64-bit
(define-vop (double-float-bits)
  (:args (float :scs (double-reg descriptor-reg)
                :load-if (not (sc-is float double-stack))))
  (:results (bits :scs (signed-reg)))
  (:arg-types double-float)
  (:result-types signed-num)
  (:translate double-float-bits)
  (:vop-var vop)
  (:policy :fast-safe)
  (:generator 2
    (sc-case float
      (double-reg
       (inst fmvx<- :double bits float))
      (double-stack
       (loadw bits (current-nfp-tn vop) (tn-byte-offset float)))
      (descriptor-reg
       (loadw bits float double-float-value-slot other-pointer-lowtag)))))

(define-vop (double-float-high-bits)
  (:args (float :scs (double-reg descriptor-reg)))
  (:results (hi-bits :scs (signed-reg)))
  (:arg-types double-float)
  (:result-types signed-num)
  #+64-bit
  (:temporary (:sc non-descriptor-reg) temp)
  (:translate double-float-high-bits)
  (:vop-var vop)
  (:policy :fast-safe)
  (:generator #-64-bit 2 #+64-bit 5
    (sc-case float
      (double-reg
       #-64-bit
       (progn
         (inst subi nsp-tn nsp-tn (* 2 n-word-bytes))
         (inst fstore :double float nsp-tn 0)
         (loadw hi-bits nsp-tn 1)
         (inst addi nsp-tn nsp-tn (* 2 n-word-bytes)))
       #+64-bit
       (progn
         (inst fmvx<- :double temp float)
         (inst srli hi-bits temp 32)))
      (double-stack
       (inst lw hi-bits (current-nfp-tn vop) (+ (tn-byte-offset float) 4)))
      (descriptor-reg
       (inst lw hi-bits float (- (+ (* double-float-value-slot n-word-bytes) 4)
                                 other-pointer-lowtag))))))

(define-vop (double-float-low-bits)
  (:args (float :scs (double-reg descriptor-reg)
                :load-if (not (sc-is float double-stack))))
  (:results (lo-bits :scs (unsigned-reg)))
  (:arg-types double-float)
  (:result-types unsigned-num)
  (:translate double-float-low-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator #-64-bit 2 #+64-bit 5
    (sc-case float
      (double-reg
       (inst fmvx<- :single lo-bits float)
       #+64-bit
       (progn
         ;; Shave off the sign.
         (inst slli lo-bits lo-bits 32)
         (inst srli lo-bits lo-bits 32)))
      (double-stack
       (inst #-64-bit lw #+64-bit lwu lo-bits (current-nfp-tn vop)
             (tn-byte-offset float)))
      (descriptor-reg
       (inst #-64-bit lw #+64-bit lwu lo-bits float
             (- (* double-float-value-slot n-word-bytes)
                other-pointer-lowtag))))))


;;;; Float mode hackery:
(sb-xc:deftype float-modes () '(unsigned-byte 32))
(defknown floating-point-modes () float-modes (flushable))
(defknown ((setf floating-point-modes)) (float-modes)
    float-modes)

(define-vop (floating-point-modes)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate floating-point-modes)
  (:policy :fast-safe)
  (:generator 3
    (inst csrr res :fcsr)))

(define-vop (set-floating-point-modes)
  (:args (new :scs (unsigned-reg) :target res))
  (:results (res :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:result-types unsigned-num)
  (:translate (setf floating-point-modes))
  (:policy :fast-safe)
  (:generator 3
    (inst csrw :fcsr new)
    (move res new)))

;;;; Complex float VOPs

(define-vop (make-complex-single-float)
  (:translate complex)
  (:args (real :scs (single-reg) :target r
               :load-if (not (location= real r)))
         (imag :scs (single-reg) :to :save))
  (:arg-types single-float single-float)
  #+64-bit
  (:temporary (:sc non-descriptor-reg) t1 t2)
  (:results (r :scs (complex-single-reg) :from (:argument 0)
               :load-if (not (sc-is r complex-single-stack))))
  (:result-types complex-single-float)
  (:note "inline complex single-float creation")
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case r
      (complex-single-reg
       #-64-bit
       (progn
         (let ((r-real (complex-reg-real-tn :single r)))
           (unless (location= real r-real)
             (inst fmove :single r-real real)))
         (let ((r-imag (complex-reg-imag-tn :single r)))
           (unless (location= imag r-imag)
             (inst fmove :single r-imag imag))))
       #+64-bit
       (progn
         (inst fmvx<- :single t1 real)
         (inst fmvx<- :single t2 imag)
         (inst slli t2 t2 32)
         ;; Shave off sign.
         (inst slli t1 t1 32)
         (inst srli t1 t1 32)
         (inst add t2 t2 t1)
         (inst fmvx-> :double r t2)))
      (complex-single-stack
       (let ((nfp (current-nfp-tn vop))
             (offset (tn-byte-offset r)))
         (unless (location= real r)
           (inst fstore :single real nfp offset))
         (inst fstore :single imag nfp (+ offset 4)))))))

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
       (let ((r-real (complex-reg-real-tn :double r)))
         (unless (location= real r-real)
           (inst fmove :double r-real real)))
       (let ((r-imag (complex-reg-imag-tn :double r)))
         (unless (location= imag r-imag)
           (inst fmove :double r-imag imag))))
      (complex-double-stack
       (let ((nfp (current-nfp-tn vop))
             (offset (tn-byte-offset r)))
         (unless (location= real r)
           (inst fstore :double real nfp offset))
         (inst fstore :double imag nfp (+ offset 8)))))))

(define-vop (complex-single-float-value)
  (:args (x :scs (complex-single-reg) :target r
            :load-if (not (sc-is x complex-single-stack))))
  (:arg-types complex-single-float)
  #+64-bit
  (:temporary (:sc non-descriptor-reg) temp)
  (:results (r :scs (single-reg)))
  (:result-types single-float)
  (:variant-vars slot)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 3
    (sc-case x
      (complex-single-reg
       #-64-bit
       (let ((value-tn (ecase slot
                         (:real (complex-reg-real-tn :single x))
                         (:imag (complex-reg-imag-tn :single x)))))
         (unless (location= value-tn r)
           (inst fmove :single r value-tn)))
       #+64-bit
       (progn
         (inst fmvx<- :double temp x)
         (ecase slot
           (:real)
           (:imag (inst srli temp temp 32)))
         (inst fmvx-> :single r temp)))
      (complex-single-stack
       (inst fload :single r (current-nfp-tn vop)
             (+ (ecase slot
                  (:real 0)
                  (:imag 4))
                (* n-word-bytes (tn-offset x))))))))

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
       (let ((value-tn (ecase slot
                         (:real (complex-reg-real-tn :double x))
                         (:imag (complex-reg-imag-tn :double x)))))
         (unless (location= value-tn r)
           (inst fmove :double r value-tn))))
      (complex-double-stack
       (inst fload :double r (current-nfp-tn vop)
             (+ (ecase slot
                  (:real 0)
                  (:imag 8))
                (tn-byte-offset x)))))))

(define-vop (realpart/complex-double-float complex-double-float-value)
  (:translate realpart)
  (:note "complex double float realpart")
  (:variant :real))

(define-vop (imagpart/complex-double-float complex-double-float-value)
  (:translate imagpart)
  (:note "complex double float imagpart")
  (:variant :imag))
