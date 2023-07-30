;;;; floating point support for the PPC

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
  (inst lfs y (current-nfp-tn vop) (tn-byte-offset x)))

(define-move-fun (store-single 1) (vop x y)
  ((single-reg) (single-stack))
  (inst stfs x (current-nfp-tn vop) (tn-byte-offset y)))


(define-move-fun (load-double 2) (vop x y)
  ((double-stack) (double-reg))
  (let ((nfp (current-nfp-tn vop))
        (offset (tn-byte-offset x)))
    (inst lfd y nfp offset)))

(define-move-fun (store-double 2) (vop x y)
  ((double-reg) (double-stack))
  (let ((nfp (current-nfp-tn vop))
        (offset (tn-byte-offset y)))
    (inst stfd x nfp offset)))



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
                    (unless (location= y x)
                      (inst fmr y x))))
                (define-move-vop ,vop :move (,sc) (,sc)))))
  (frob single-move single-reg)
  (frob double-move double-reg))


(define-vop (move-from-float)
  (:args (x :to :save))
  (:results (y))
  (:note "float to pointer coercion")
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:variant-vars double-p size type data)
  (:generator 13
    (with-fixed-allocation (y pa-flag ndescr type size)
      (if double-p
          (inst stfd x y (- (* data n-word-bytes) other-pointer-lowtag))
          (inst stfs x y (- (* data n-word-bytes) other-pointer-lowtag))))))

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
                    (inst ,(if double-p 'lfd 'lfs) y x
                          (- (* ,value n-word-bytes) other-pointer-lowtag))))
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
                         (inst fmr y x)))
                      (,stack-sc
                       (let ((offset (tn-byte-offset y)))
                         (inst ,(if double-p 'stfd 'stfs) x nfp offset))))))
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
                  :offset (1+ (tn-offset x))))


(define-move-fun (load-complex-single 2) (vop x y)
  ((complex-single-stack) (complex-single-reg))
  (let ((nfp (current-nfp-tn vop))
        (offset (tn-byte-offset x)))
    (let ((real-tn (complex-single-reg-real-tn y)))
      (inst lfs real-tn nfp offset))
    (let ((imag-tn (complex-single-reg-imag-tn y)))
      (inst lfs imag-tn nfp (+ offset n-word-bytes)))))

(define-move-fun (store-complex-single 2) (vop x y)
  ((complex-single-reg) (complex-single-stack))
  (let ((nfp (current-nfp-tn vop))
        (offset (tn-byte-offset y)))
    (let ((real-tn (complex-single-reg-real-tn x)))
      (inst stfs real-tn nfp offset))
    (let ((imag-tn (complex-single-reg-imag-tn x)))
      (inst stfs imag-tn nfp (+ offset n-word-bytes)))))


(define-move-fun (load-complex-double 4) (vop x y)
  ((complex-double-stack) (complex-double-reg))
  (let ((nfp (current-nfp-tn vop))
        (offset (tn-byte-offset x)))
    (let ((real-tn (complex-double-reg-real-tn y)))
      (inst lfd real-tn nfp offset))
    (let ((imag-tn (complex-double-reg-imag-tn y)))
      (inst lfd imag-tn nfp (+ offset (* 2 n-word-bytes))))))

(define-move-fun (store-complex-double 4) (vop x y)
  ((complex-double-reg) (complex-double-stack))
  (let ((nfp (current-nfp-tn vop))
        (offset (tn-byte-offset y)))
    (let ((real-tn (complex-double-reg-real-tn x)))
      (inst stfd real-tn nfp offset))
    (let ((imag-tn (complex-double-reg-imag-tn x)))
      (inst stfd imag-tn nfp (+ offset (* 2 n-word-bytes))))))


;;;
;;; Complex float register to register moves.
;;;
(define-vop (complex-single-move)
  (:args (x :scs (complex-single-reg) :target y
            :load-if (not (location= x y))))
  (:results (y :scs (complex-single-reg) :load-if (not (location= x y))))
  (:note "complex single float move")
  (:generator 0
     (unless (location= x y)
       ;; Note the complex-float-regs are aligned to every second
       ;; float register so there is not need to worry about overlap.
       (let ((x-real (complex-single-reg-real-tn x))
             (y-real (complex-single-reg-real-tn y)))
         (inst fmr y-real x-real))
       (let ((x-imag (complex-single-reg-imag-tn x))
             (y-imag (complex-single-reg-imag-tn y)))
         (inst fmr y-imag x-imag)))))
;;;
(define-move-vop complex-single-move :move
  (complex-single-reg) (complex-single-reg))

(define-vop (complex-double-move)
  (:args (x :scs (complex-double-reg)
            :target y :load-if (not (location= x y))))
  (:results (y :scs (complex-double-reg) :load-if (not (location= x y))))
  (:note "complex double float move")
  (:generator 0
     (unless (location= x y)
       ;; Note the complex-float-regs are aligned to every second
       ;; float register so there is not need to worry about overlap.
       (let ((x-real (complex-double-reg-real-tn x))
             (y-real (complex-double-reg-real-tn y)))
         (inst fmr y-real x-real))
       (let ((x-imag (complex-double-reg-imag-tn x))
             (y-imag (complex-double-reg-imag-tn y)))
         (inst fmr y-imag x-imag)))))
;;;
(define-move-vop complex-double-move :move
  (complex-double-reg) (complex-double-reg))


;;;
;;; Move from a complex float to a descriptor register allocating a
;;; new complex float object in the process.
;;;
(define-vop (move-from-complex-single)
  (:args (x :scs (complex-single-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:note "complex single float to pointer coercion")
  (:generator 13
     (with-fixed-allocation (y pa-flag ndescr complex-single-float-widetag
                               complex-single-float-size)
       (let ((real-tn (complex-single-reg-real-tn x)))
         (inst stfs real-tn y (- (* complex-single-float-real-slot
                                    n-word-bytes)
                                 other-pointer-lowtag)))
       (let ((imag-tn (complex-single-reg-imag-tn x)))
         (inst stfs imag-tn y (- (* complex-single-float-imag-slot
                                    n-word-bytes)
                                 other-pointer-lowtag))))))
;;;
(define-move-vop move-from-complex-single :move
  (complex-single-reg) (descriptor-reg))

(define-vop (move-from-complex-double)
  (:args (x :scs (complex-double-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:note "complex double float to pointer coercion")
  (:generator 13
     (with-fixed-allocation (y pa-flag ndescr complex-double-float-widetag
                               complex-double-float-size)
       (let ((real-tn (complex-double-reg-real-tn x)))
         (inst stfd real-tn y (- (* complex-double-float-real-slot
                                    n-word-bytes)
                                 other-pointer-lowtag)))
       (let ((imag-tn (complex-double-reg-imag-tn x)))
         (inst stfd imag-tn y (- (* complex-double-float-imag-slot
                                    n-word-bytes)
                                 other-pointer-lowtag))))))
;;;
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
    (let ((real-tn (complex-single-reg-real-tn y)))
      (inst lfs real-tn x (- (* complex-single-float-real-slot n-word-bytes)
                             other-pointer-lowtag)))
    (let ((imag-tn (complex-single-reg-imag-tn y)))
      (inst lfs imag-tn x (- (* complex-single-float-imag-slot n-word-bytes)
                             other-pointer-lowtag)))))
(define-move-vop move-to-complex-single :move
  (descriptor-reg) (complex-single-reg))

(define-vop (move-to-complex-double)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (complex-double-reg)))
  (:note "pointer to complex float coercion")
  (:generator 2
    (let ((real-tn (complex-double-reg-real-tn y)))
      (inst lfd real-tn x (- (* complex-double-float-real-slot n-word-bytes)
                             other-pointer-lowtag)))
    (let ((imag-tn (complex-double-reg-imag-tn y)))
      (inst lfd imag-tn x (- (* complex-double-float-imag-slot n-word-bytes)
                             other-pointer-lowtag)))))
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
       (unless (location= x y)
         (let ((x-real (complex-single-reg-real-tn x))
               (y-real (complex-single-reg-real-tn y)))
           (inst fmr y-real x-real))
         (let ((x-imag (complex-single-reg-imag-tn x))
               (y-imag (complex-single-reg-imag-tn y)))
           (inst fmr y-imag x-imag))))
      (complex-single-stack
       (let ((offset (tn-byte-offset y)))
         (let ((real-tn (complex-single-reg-real-tn x)))
           (inst stfs real-tn nfp offset))
         (let ((imag-tn (complex-single-reg-imag-tn x)))
           (inst stfs imag-tn nfp (+ offset n-word-bytes))))))))
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
       (unless (location= x y)
         (let ((x-real (complex-double-reg-real-tn x))
               (y-real (complex-double-reg-real-tn y)))
           (inst fmr y-real x-real))
         (let ((x-imag (complex-double-reg-imag-tn x))
               (y-imag (complex-double-reg-imag-tn y)))
           (inst fmr y-imag x-imag))))
      (complex-double-stack
       (let ((offset (tn-byte-offset y)))
         (let ((real-tn (complex-double-reg-real-tn x)))
           (inst stfd real-tn nfp offset))
         (let ((imag-tn (complex-double-reg-imag-tn x)))
           (inst stfd imag-tn nfp (+ offset (* 2 n-word-bytes)))))))))
(define-move-vop move-complex-double-float-arg :move-arg
  (complex-double-reg descriptor-reg) (complex-double-reg))


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

(macrolet ((frob (op sinst sname scost dinst dname dcost)
             `(progn
                (define-vop (,sname single-float-op)
                  (:translate ,op)
                  (:generator ,scost
                    (inst ,sinst r x y)))
                (define-vop (,dname double-float-op)
                  (:translate ,op)
                  (:generator ,dcost
                    (inst ,dinst r x y))))))
  (frob + fadds +/single-float 2 fadd +/double-float 2)
  (frob - fsubs -/single-float 2 fsub -/double-float 2)
  (frob * fmuls */single-float 4 fmul */double-float 5)
  (frob / fdivs //single-float 12 fdiv //double-float 19))

(macrolet ((frob (name inst translate sc type)
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
                  (inst ,inst y x)))))
  (frob abs/single-float fabs abs single-reg single-float)
  (frob abs/double-float fabs abs double-reg double-float)
  (frob %negate/single-float fneg %negate single-reg single-float)
  (frob %negate/double-float fneg %negate double-reg double-float))


;;;; Comparison:

(define-vop (float-compare)
  (:args (x) (y))
  (:conditional)
  (:info target not-p)
  (:variant-vars format yep nope)
  (:policy :fast-safe)
  (:note "inline float comparison")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 3
    (note-this-location vop :internal-error)
    (ecase format
      ((:single :double)
       (inst fcmpo :cr1 x y)))
    (inst b?  :cr1 (if not-p nope yep) target)))

(macrolet ((frob (name sc ptype)
             `(define-vop (,name float-compare)
                (:args (x :scs (,sc))
                       (y :scs (,sc)))
                (:arg-types ,ptype ,ptype))))
  (frob single-float-compare single-reg single-float)
  (frob double-float-compare double-reg double-float))

(macrolet ((frob (translate yep nope sname dname)
             `(progn
                (define-vop (,sname single-float-compare)
                  (:translate ,translate)
                  (:variant :single ,yep ,nope))
                (define-vop (,dname double-float-compare)
                  (:translate ,translate)
                  (:variant :double ,yep ,nope)))))
  (frob < :lt :ge </single-float </double-float)
  (frob > :gt :le >/single-float >/double-float)
  (frob = :eq :ne =/single-float =/double-float))


;;;; Conversion:

(macrolet ((frob (name translate inst to-sc to-type)
             `(define-vop (,name)
                (:args (x :scs (signed-reg)))
                (:temporary (:scs (double-stack)) temp)
                (:temporary (:scs (double-reg)) fmagic)
                (:temporary (:scs (signed-reg)) rtemp)
                (:results (y :scs (,to-sc)))
                (:arg-types signed-num)
                (:result-types ,to-type)
                (:policy :fast-safe)
                (:note "inline float coercion")
                (:translate ,translate)
                (:vop-var vop)
                (:save-p :compute-only)
                (:generator 5
                  (let* ((nfp-tn (current-nfp-tn vop))
                         (temp-offset-high (tn-byte-offset temp))
                         (temp-offset-low (+ temp-offset-high n-word-bytes)))
                    (inst lis rtemp #x4330) ; High word of magic constant
                    (inst stw rtemp nfp-tn temp-offset-high)
                    (inst lis rtemp #x8000)
                    (inst stw rtemp nfp-tn temp-offset-low)
                    (inst lfd fmagic nfp-tn temp-offset-high)
                    (inst xor rtemp rtemp x) ; invert sign bit of x : rtemp had #x80000000
                    (inst stw rtemp nfp-tn temp-offset-low)
                    (inst lfd y nfp-tn temp-offset-high)
                    (note-this-location vop :internal-error)
                    (inst ,inst y y fmagic))))))
  (frob %single-float/signed %single-float fsubs single-reg single-float)
  (frob %double-float/signed %double-float fsub double-reg double-float))

(macrolet ((frob (name translate inst to-sc to-type)
            `(define-vop (,name)
               (:args (x :scs (unsigned-reg)))
               (:temporary (:scs (double-stack)) temp)
               (:temporary (:scs (double-reg)) fmagic)
               (:temporary (:scs (signed-reg)) rtemp)
               (:results (y :scs (,to-sc)))
               (:arg-types unsigned-num)
               (:result-types ,to-type)
               (:policy :fast-safe)
               (:note "inline float coercion")
               (:translate ,translate)
               (:vop-var vop)
               (:save-p :compute-only)
               (:generator 5
                 (let* ((nfp-tn (current-nfp-tn vop))
                        (temp-offset-high (tn-byte-offset temp))
                        (temp-offset-low (+ temp-offset-high n-word-bytes)))
                    (inst lis rtemp #x4330)   ; High word of magic constant
                    (inst stw rtemp nfp-tn temp-offset-high)
                    (inst stw zero-tn nfp-tn temp-offset-low)
                    (inst lfd fmagic nfp-tn temp-offset-high)
                    (inst stw x nfp-tn temp-offset-low)
                    (inst lfd y nfp-tn temp-offset-high)
                   (note-this-location vop :internal-error)
                   (inst ,inst y y fmagic))))))
  (frob %single-float/unsigned %single-float fsubs single-reg single-float)
  (frob %double-float/unsigned %double-float fsub double-reg double-float))

(macrolet ((frob (name translate inst from-sc from-type to-sc to-type)
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
                  (inst ,inst y x)))))
  (frob %single-float/double-float %single-float frsp
    double-reg double-float single-reg single-float)
  (frob %double-float/single-float %double-float fmr
    single-reg single-float double-reg double-float))

(macrolet ((frob (trans from-sc from-type inst)
             `(define-vop (,(symbolicate trans "/" from-type))
                (:args (x :scs (,from-sc) :target temp))
                (:temporary (:from (:argument 0) :sc single-reg) temp)
                (:temporary (:scs (double-stack)) stack-temp)
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
                  (inst ,inst temp x)
                  (inst stfd temp (current-nfp-tn vop)
                        (tn-byte-offset stack-temp))
                  (inst lwz y (current-nfp-tn vop)
                        (+ 4 (tn-byte-offset stack-temp)))))))
  (frob %unary-truncate/single-float single-reg single-float fctiwz)
  (frob %unary-truncate/double-float double-reg double-float fctiwz)
  (frob %unary-round single-reg single-float fctiw)
  (frob %unary-round double-reg double-float fctiw))

(define-vop (make-single-float)
  (:args (bits :scs (signed-reg) :target res
               :load-if (not (sc-is bits signed-stack))))
  (:results (res :scs (single-reg)
                 :load-if (not (sc-is res single-stack))))
  (:temporary (:scs (signed-reg) :from (:argument 0) :to (:result 0)) temp)
  (:temporary (:scs (signed-stack)) stack-temp)
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
          (inst stw bits (current-nfp-tn vop)
                (tn-byte-offset stack-temp))
          (inst lfs res (current-nfp-tn vop)
                (tn-byte-offset stack-temp)))
         (single-stack
          (inst stw bits (current-nfp-tn vop)
                (tn-byte-offset res)))))
      (signed-stack
       (sc-case res
         (single-reg
          (inst lfs res (current-nfp-tn vop)
                (tn-byte-offset bits)))
         (single-stack
          (unless (location= bits res)
            (inst lwz temp (current-nfp-tn vop)
                  (tn-byte-offset bits))
            (inst stw temp (current-nfp-tn vop)
                  (tn-byte-offset res)))))))))

(define-vop (make-double-float)
  (:args (hi-bits :scs (signed-reg))
         (lo-bits :scs (unsigned-reg)))
  (:results (res :scs (double-reg)
                 :load-if (not (sc-is res double-stack))))
  (:temporary (:scs (double-stack)) temp)
  (:arg-types signed-num unsigned-num)
  (:result-types double-float)
  (:translate make-double-float)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (let ((stack-tn (sc-case res
                      (double-stack res)
                      (double-reg temp))))
      (inst stw hi-bits (current-nfp-tn vop)
            (tn-byte-offset stack-tn))
      (inst stw lo-bits (current-nfp-tn vop)
            (* (1+ (tn-offset stack-tn)) n-word-bytes)))
    (when (sc-is res double-reg)
      (inst lfd res (current-nfp-tn vop)
            (tn-byte-offset temp)))))

(define-vop (single-float-bits)
  (:args (float :scs (single-reg)
                :load-if (not (and (sc-is bits signed-reg)
                                   (sc-is float single-stack descriptor-reg)))))
  (:results (bits :scs (signed-reg)
                  :load-if (or (sc-is float descriptor-reg single-stack)
                               (not (sc-is bits signed-stack)))))
  (:temporary (:scs (signed-stack)) stack-temp)
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
          (inst stfs float (current-nfp-tn vop)
                (tn-byte-offset stack-temp))
          (inst lwz bits (current-nfp-tn vop)
                (tn-byte-offset stack-temp)))
         (single-stack
          (inst lwz bits (current-nfp-tn vop)
                (tn-byte-offset float)))
         (descriptor-reg
          (loadw bits float single-float-value-slot other-pointer-lowtag))))
      (signed-stack
       (inst stfs float (current-nfp-tn vop)
                (tn-byte-offset bits))))))

(define-vop (double-float-high-bits)
  (:args (float :scs (double-reg descriptor-reg)
                :load-if (not (sc-is float double-stack))))
  (:results (hi-bits :scs (signed-reg)))
  (:temporary (:scs (double-stack)) stack-temp)
  (:arg-types double-float)
  (:result-types signed-num)
  (:translate double-float-high-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case float
      (double-reg
        (inst stfd float (current-nfp-tn vop)
              (tn-byte-offset stack-temp))
        (inst lwz hi-bits (current-nfp-tn vop)
              (tn-byte-offset stack-temp)))
      (double-stack
        (inst lwz hi-bits (current-nfp-tn vop)
              (tn-byte-offset float)))
      (descriptor-reg
        (loadw hi-bits float double-float-value-slot
               other-pointer-lowtag)))))

(define-vop (double-float-low-bits)
  (:args (float :scs (double-reg descriptor-reg)
                :load-if (not (sc-is float double-stack))))
  (:results (lo-bits :scs (unsigned-reg)))
  (:temporary (:scs (double-stack)) stack-temp)
  (:arg-types double-float)
  (:result-types unsigned-num)
  (:translate double-float-low-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case float
      (double-reg
        (inst stfd float (current-nfp-tn vop)
              (tn-byte-offset stack-temp))
        (inst lwz lo-bits (current-nfp-tn vop)
              (* (1+ (tn-offset stack-temp)) n-word-bytes)))
      (double-stack
        (inst lwz lo-bits (current-nfp-tn vop)
              (* (1+ (tn-offset float)) n-word-bytes)))
      (descriptor-reg
        (loadw lo-bits float (1+ double-float-value-slot)
               other-pointer-lowtag)))))

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
  (:vop-var vop)
  (:temporary (:sc double-stack) temp)
  (:temporary (:sc single-reg) fp-temp)
  (:generator 3
    (let ((nfp (current-nfp-tn vop)))
      (inst mffs fp-temp)
      (inst stfd fp-temp nfp (* n-word-bytes (tn-offset temp)))
      (loadw res nfp (1+ (tn-offset temp))))))

(define-vop (set-floating-point-modes)
  (:args (new :scs (unsigned-reg) :target res))
  (:results (res :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:result-types unsigned-num)
  (:translate (setf floating-point-modes))
  (:policy :fast-safe)
  (:temporary (:sc double-stack) temp)
  (:temporary (:sc single-reg) fp-temp)
  (:vop-var vop)
  (:generator 3
    (let ((nfp (current-nfp-tn vop)))
      (storew new nfp (1+ (tn-offset temp)))
      (inst lfd fp-temp nfp (* n-word-bytes (tn-offset temp)))
      (inst mtfsf 255 fp-temp)
      (move res new))))


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
       (let ((r-real (complex-single-reg-real-tn r)))
         (unless (location= real r-real)
           (inst fmr r-real real)))
       (let ((r-imag (complex-single-reg-imag-tn r)))
         (unless (location= imag r-imag)
           (inst fmr r-imag imag))))
      (complex-single-stack
       (let ((nfp (current-nfp-tn vop))
             (offset (tn-byte-offset r)))
         (unless (location= real r)
           (inst stfs real nfp offset))
         (inst stfs imag nfp (+ offset n-word-bytes)))))))

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
       (let ((r-real (complex-double-reg-real-tn r)))
         (unless (location= real r-real)
           (inst fmr r-real real)))
       (let ((r-imag (complex-double-reg-imag-tn r)))
         (unless (location= imag r-imag)
           (inst fmr r-imag imag))))
      (complex-double-stack
       (let ((nfp (current-nfp-tn vop))
             (offset (tn-byte-offset r)))
         (unless (location= real r)
           (inst stfd real nfp offset))
         (inst stfd imag nfp (+ offset (* 2 n-word-bytes))))))))


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
       (let ((value-tn (ecase slot
                         (:real (complex-single-reg-real-tn x))
                         (:imag (complex-single-reg-imag-tn x)))))
         (unless (location= value-tn r)
           (inst fmr r value-tn))))
      (complex-single-stack
       (inst lfs r (current-nfp-tn vop) (* (+ (ecase slot (:real 0) (:imag 1))
                                              (tn-offset x))
                                           n-word-bytes))))))

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
                         (:real (complex-double-reg-real-tn x))
                         (:imag (complex-double-reg-imag-tn x)))))
         (unless (location= value-tn r)
           (inst fmr r value-tn))))
      (complex-double-stack
       (inst lfd r (current-nfp-tn vop) (* (+ (ecase slot (:real 0) (:imag 2))
                                              (tn-offset x))
                                           n-word-bytes))))))

(define-vop (realpart/complex-double-float complex-double-float-value)
  (:translate realpart)
  (:note "complex double float realpart")
  (:variant :real))

(define-vop (imagpart/complex-double-float complex-double-float-value)
  (:translate imagpart)
  (:note "complex double float imagpart")
  (:variant :imag))

;; This vop and the next are intended to be used only for moving a
;; float to an integer arg location (register or stack) for C callout.
;; See %alien-funcall ir2convert in aliencomp.lisp.

#+darwin
(define-vop (move-double-to-int-arg)
  (:args (float :scs (double-reg)))
  (:results (hi-bits :scs (signed-reg signed-stack))
            (lo-bits :scs (unsigned-reg unsigned-stack)))
  (:temporary (:scs (double-stack)) stack-temp)
  (:temporary (:scs (signed-reg)) temp)
  (:arg-types double-float)
  (:result-types signed-num unsigned-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case float
      (double-reg
       (inst stfd float (current-nfp-tn vop)
             (tn-byte-offset stack-temp))
       (sc-case hi-bits
         (signed-reg
          (inst lwz hi-bits (current-nfp-tn vop)
                (tn-byte-offset stack-temp)))
         (signed-stack
          (inst lwz temp (current-nfp-tn vop)
                (tn-byte-offset stack-temp))
          (inst stw temp nsp-tn
                (tn-byte-offset hi-bits))))
       (sc-case lo-bits
         (unsigned-reg
          (inst lwz lo-bits (current-nfp-tn vop)
                (* (1+ (tn-offset stack-temp)) n-word-bytes)))
         (unsigned-stack
          (inst lwz temp (current-nfp-tn vop)
                (* (1+ (tn-offset stack-temp)) n-word-bytes))
          (inst stw temp nsp-tn
                (tn-byte-offset lo-bits))))))))

#+darwin
(define-vop (move-single-to-int-arg)
  (:args (float :scs (single-reg)))
  (:results (bits :scs (signed-reg signed-stack)))
  (:temporary (:scs (double-stack)) stack-temp)
  (:temporary (:scs (signed-reg)) temp)
  (:arg-types single-float)
  (:result-types signed-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case float
      (single-reg
       (inst stfs float (current-nfp-tn vop)
             (tn-byte-offset stack-temp))
       (sc-case bits
         (signed-reg
          (inst lwz bits (current-nfp-tn vop)
                (tn-byte-offset stack-temp)))
         (signed-stack
          (inst lwz temp (current-nfp-tn vop)
                (tn-byte-offset stack-temp))
          (inst stw temp nsp-tn
                (tn-byte-offset bits))))))))

