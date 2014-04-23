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
  ((complex-single-stack) (complex-single-reg))
  (let ((nfp (current-nfp-tn vop))
        (offset (* (tn-offset x) n-word-bytes)))
    (let ((real-tn (complex-single-reg-real-tn y)))
      (inst flds real-tn (@ nfp offset)))
    (let ((imag-tn (complex-single-reg-imag-tn y)))
      (inst flds imag-tn (@ nfp (+ offset n-word-bytes))))))

(define-move-fun (store-complex-single 2) (vop x y)
  ((complex-single-reg) (complex-single-stack))
  (let ((nfp (current-nfp-tn vop))
        (offset (* (tn-offset y) n-word-bytes)))
    (let ((real-tn (complex-single-reg-real-tn x)))
      (inst fsts real-tn (@ nfp offset)))
    (let ((imag-tn (complex-single-reg-imag-tn x)))
      (inst fsts imag-tn (@ nfp (+ offset n-word-bytes))))))

(define-move-fun (load-complex-double 4) (vop x y)
  ((complex-double-stack) (complex-double-reg))
  (let ((nfp (current-nfp-tn vop))
        (offset (* (tn-offset x) n-word-bytes)))
    (let ((real-tn (complex-double-reg-real-tn y)))
      (inst fldd real-tn (@ nfp offset)))
    (let ((imag-tn (complex-double-reg-imag-tn y)))
      (inst fldd imag-tn (@ nfp (+ offset (* 2 n-word-bytes)))))))

(define-move-fun (store-complex-double 4) (vop x y)
  ((complex-double-reg) (complex-double-stack))
  (let ((nfp (current-nfp-tn vop))
        (offset (* (tn-offset y) n-word-bytes)))
    (let ((real-tn (complex-double-reg-real-tn x)))
      (inst fstd real-tn (@ nfp offset)))
    (let ((imag-tn (complex-double-reg-imag-tn x)))
      (inst fstd imag-tn (@ nfp (+ offset (* 2 n-word-bytes)))))))

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
       ;; Note the complex-single-float-regs are aligned to every
       ;; second float register so there is not need to worry about
       ;; overlap.
       (let ((x-real (complex-single-reg-real-tn x))
             (y-real (complex-single-reg-real-tn y)))
         (inst fcpys y-real x-real))
       (let ((x-imag (complex-single-reg-imag-tn x))
             (y-imag (complex-single-reg-imag-tn y)))
         (inst fcpys y-imag x-imag)))))
(define-move-vop complex-single-move :move
  (complex-single-reg) (complex-single-reg))

(define-vop (complex-double-move)
  (:args (x :scs (complex-double-reg)
            :target y :load-if (not (location= x y))))
  (:results (y :scs (complex-double-reg) :load-if (not (location= x y))))
  (:note "complex double float move")
  (:generator 0
     (unless (location= x y)
       ;; Note the complex-double-float-regs are aligned to every
       ;; fourth float register so there is not need to worry about
       ;; overlap.
       (let ((x-real (complex-double-reg-real-tn x))
             (y-real (complex-double-reg-real-tn y)))
         (inst fcpyd y-real x-real))
       (let ((x-imag (complex-double-reg-imag-tn x))
             (y-imag (complex-double-reg-imag-tn y)))
         (inst fcpyd y-imag x-imag)))))
(define-move-vop complex-double-move :move
  (complex-double-reg) (complex-double-reg))


;;;
;;; Move from a complex float to a descriptor register allocating a
;;; new complex float object in the process.
;;;
(define-vop (move-from-complex-single)
  (:args (x :scs (complex-single-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:temporary (:sc non-descriptor-reg :offset ocfp-offset) pa-flag)
  (:note "complex single float to pointer coercion")
  (:generator 13
     (with-fixed-allocation (y pa-flag complex-single-float-widetag
                               complex-single-float-size)
       (inst sub pa-flag y other-pointer-lowtag)
       (let ((real-tn (complex-single-reg-real-tn x)))
         (inst fsts real-tn (@ pa-flag (* complex-single-float-real-slot
                                          n-word-bytes))))
       (let ((imag-tn (complex-single-reg-imag-tn x)))
         (inst fsts imag-tn (@ pa-flag (* complex-single-float-imag-slot
                                          n-word-bytes)))))))
(define-move-vop move-from-complex-single :move
  (complex-single-reg) (descriptor-reg))

(define-vop (move-from-complex-double)
  (:args (x :scs (complex-double-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:temporary (:sc non-descriptor-reg :offset ocfp-offset) pa-flag)
  (:note "complex double float to pointer coercion")
  (:generator 13
     (with-fixed-allocation (y pa-flag complex-double-float-widetag
                               complex-double-float-size)
       (inst sub pa-flag y other-pointer-lowtag)
       (let ((real-tn (complex-double-reg-real-tn x)))
         (inst fstd real-tn (@ pa-flag (* complex-double-float-real-slot
                                          n-word-bytes))))
       (let ((imag-tn (complex-double-reg-imag-tn x)))
         (inst fstd imag-tn (@ pa-flag (* complex-double-float-imag-slot
                                          n-word-bytes)))))))
(define-move-vop move-from-complex-double :move
  (complex-double-reg) (descriptor-reg))


;;;
;;; Move from a descriptor to a complex float register
;;;
(define-vop (move-to-complex-single)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (complex-single-reg)))
  (:temporary (:sc interior-reg) lip)
  (:note "pointer to complex float coercion")
  (:generator 2
    (inst sub lip x other-pointer-lowtag)
    (let ((real-tn (complex-single-reg-real-tn y)))
      (inst flds real-tn (@ lip (* complex-single-float-real-slot n-word-bytes))))
    (let ((imag-tn (complex-single-reg-imag-tn y)))
      (inst flds imag-tn (@ lip (* complex-single-float-imag-slot n-word-bytes))))))
(define-move-vop move-to-complex-single :move
  (descriptor-reg) (complex-single-reg))

(define-vop (move-to-complex-double)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (complex-double-reg)))
  (:temporary (:sc interior-reg) lip)
  (:note "pointer to complex float coercion")
  (:generator 2
    (inst sub lip x other-pointer-lowtag)
    (let ((real-tn (complex-double-reg-real-tn y)))
      (inst fldd real-tn (@ lip (* complex-double-float-real-slot n-word-bytes))))
    (let ((imag-tn (complex-double-reg-imag-tn y)))
      (inst fldd imag-tn (@ lip (* complex-double-float-imag-slot n-word-bytes))))))
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
           (inst fcpys y-real x-real))
         (let ((x-imag (complex-single-reg-imag-tn x))
               (y-imag (complex-single-reg-imag-tn y)))
           (inst fcpys y-imag x-imag))))
      (complex-single-stack
       (let ((offset (* (tn-offset y) n-word-bytes)))
         (let ((real-tn (complex-single-reg-real-tn x)))
           (inst fsts real-tn (@ nfp offset)))
         (let ((imag-tn (complex-single-reg-imag-tn x)))
           (inst fsts imag-tn (@ nfp (+ offset n-word-bytes)))))))))
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
           (inst fcpyd y-real x-real))
         (let ((x-imag (complex-double-reg-imag-tn x))
               (y-imag (complex-double-reg-imag-tn y)))
           (inst fcpyd y-imag x-imag))))
      (complex-double-stack
       (let ((offset (* (tn-offset y) n-word-bytes)))
         (let ((real-tn (complex-double-reg-real-tn x)))
           (inst fstd real-tn (@ nfp offset)))
         (let ((imag-tn (complex-double-reg-imag-tn x)))
           (inst fstd imag-tn (@ nfp (+ offset (* 2 n-word-bytes))))))))))
(define-move-vop move-complex-double-float-arg :move-arg
  (complex-double-reg descriptor-reg) (complex-double-reg))

;;;; Unboxed-to-boxed MOVE-ARG handling:

;; This little gem here says to use the VOP MOVE-ARG to move any float
;; registers to boxed data.  MOVE-ARG only takes boxed data as input,
;; which means that the :MOVE VOPs will be used to do the appropriate
;; conversion.
(define-move-vop move-arg :move-arg
  (single-reg double-reg #| complex-single-reg complex-double-reg |#)
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
  (frob + fadds +/single-float 2 faddd +/double-float 2)
  (frob - fsubs -/single-float 2 fsubs -/double-float 2)
  (frob * fmuls */single-float 4 fmuld */double-float 5)
  (frob / fdivs //single-float 12 fdivd //double-float 19))

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
  (frob abs/single-float fabss abs single-reg single-float)
  (frob abs/double-float fabsd abs double-reg double-float)
  (frob %negate/single-float fnegs %negate single-reg single-float)
  (frob %negate/double-float fnegd %negate double-reg double-float))

;;;; Comparison:

(define-vop (float-compare)
  (:args (x) (y))
  (:conditional)
  (:info target not-p)
  (:variant-vars format yep nope is-=)
  (:policy :fast-safe)
  (:note "inline float comparison")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 3
    (note-this-location vop :internal-error)
    (ecase format
      (:single
       (if is-=
           (inst fcmps x y)
           (inst fcmpes x y)))
      (:double
       (if is-=
           (inst fcmpd x y)
           (inst fcmped x y))))
    ;; We'd like to use FMSTAT, but it's not defined.  Or FMRX r15,
    ;; FPSCR (equivalent encoding), but that's not well defined
    ;; either.  Hand-encoding the instruction as a WORD.
    ;(inst fmstat)
    (inst word #xeef1fa10)
    (inst b (if not-p nope yep) target)))

(macrolet ((frob (name sc ptype)
             `(define-vop (,name float-compare)
                (:args (x :scs (,sc))
                       (y :scs (,sc)))
                (:arg-types ,ptype ,ptype))))
  (frob single-float-compare single-reg single-float)
  (frob double-float-compare double-reg double-float))

(macrolet ((frob (translate yep nope sname dname is-=)
             `(progn
                (define-vop (,sname single-float-compare)
                  (:translate ,translate)
                  (:variant :single ,yep ,nope ,is-=))
                (define-vop (,dname double-float-compare)
                  (:translate ,translate)
                  (:variant :double ,yep ,nope ,is-=)))))
  (frob < :mi :pl </single-float </double-float nil)
  (frob > :gt :le >/single-float >/double-float nil)
  (frob = :eq :ne eql/single-float eql/double-float t))

;;;; Conversion:

(macrolet ((frob (name translate inst from-sc from-type to-sc to-type)
            `(define-vop (,name)
               (:args (x :scs (,from-sc)))
               (:temporary (:scs (single-reg)) rtemp)
               (:results (y :scs (,to-sc)))
               (:arg-types ,from-type)
               (:result-types ,to-type)
               (:policy :fast-safe)
               (:note "inline float coercion")
               (:translate ,translate)
               (:vop-var vop)
               (:save-p :compute-only)
               (:generator 5
                 (inst fmsr rtemp x)
                 (inst ,inst y rtemp)))))
  (frob %single-float/signed %single-float fsitos
        signed-reg signed-num single-reg single-float)
  (frob %double-float/signed %double-float fsitod
        signed-reg signed-num double-reg double-float)
  (frob %single-float/unsigned %single-float fuitos
        unsigned-reg unsigned-num single-reg single-float)
  (frob %double-float/unsigned %double-float fuitod
        unsigned-reg unsigned-num double-reg double-float))

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
  (frob %single-float/double-float %single-float fcvtsd
    double-reg double-float single-reg single-float)
  (frob %double-float/single-float %double-float fcvtds
    single-reg single-float double-reg double-float))

(macrolet ((frob (trans from-sc from-type inst)
             `(define-vop (,(symbolicate trans "/" from-type))
                (:args (x :scs (,from-sc) :target temp))
                (:temporary (:from (:argument 0) :sc single-reg) temp)
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
                  (inst fmrs y temp)))))
  (frob %unary-truncate/single-float single-reg single-float ftosizs)
  (frob %unary-truncate/double-float double-reg double-float ftosizd)
  (frob %unary-round single-reg single-float ftosis)
  (frob %unary-round double-reg double-float ftosid))

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
          (inst fmsr res bits))
         (single-stack
          (storew bits (current-nfp-tn vop) (tn-offset res)))))
      (signed-stack
       (sc-case res
         (single-reg
          (inst flds res
                (@ (current-nfp-tn vop)
                   (* (tn-offset bits) n-word-bytes))))
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
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (sc-case res
      (double-reg
       (inst fmdrr res lo-bits hi-bits))
      (double-stack
       (cond
         ((eq *backend-byte-order* :big-endian)
          (storew hi-bits (current-nfp-tn vop) (tn-offset res))
          (storew lo-bits (current-nfp-tn vop) (1+ (tn-offset res))))
         (t
          (storew lo-bits (current-nfp-tn vop) (tn-offset res))
          (storew hi-bits (current-nfp-tn vop) (1+ (tn-offset res)))))))))

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
          (inst fmrs bits float))
         (single-stack
          (loadw bits (current-nfp-tn vop) (tn-offset float)))
         (descriptor-reg
          (loadw bits float single-float-value-slot other-pointer-lowtag))))
      (signed-stack
       (sc-case float
         (single-reg
          (inst fsts float (@ (current-nfp-tn vop)
                              (* (tn-offset bits) n-word-bytes))))
         ((single-stack descriptor-reg)
          ;; Fun and games: This also affects PPC, silently.
          ;; Hopefully it's a non-issue, but I'd rather have the
          ;; explicit error than a silent miscompilation.
          (bug "Unable to extract single-float bits from ~S to ~S" float bits)))))))

(define-vop (double-float-high-bits)
  (:args (float :scs (double-reg descriptor-reg)
                :load-if (not (sc-is float double-stack))))
  (:results (hi-bits :scs (signed-reg)))
  (:arg-types double-float)
  (:result-types signed-num)
  (:translate double-float-high-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case float
      (double-reg
        (inst fmrdh hi-bits float))
      (double-stack
        (loadw hi-bits (current-nfp-tn vop)
               (+ (tn-offset float)
                  (if (eq *backend-byte-order* :big-endian)
                      0 1))))
      (descriptor-reg
        (loadw hi-bits float (+ double-float-value-slot
                                (if (eq *backend-byte-order* :big-endian)
                                    0 1))
               other-pointer-lowtag)))))

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
        (inst fmrdl lo-bits float))
      (double-stack
        (loadw lo-bits (current-nfp-tn vop)
               (+ (tn-offset float)
                  (if (eq *backend-byte-order* :big-endian)
                      1 0))))
      (descriptor-reg
        (loadw lo-bits float (+ double-float-value-slot
                                (if (eq *backend-byte-order* :big-endian)
                                    1 0))
               other-pointer-lowtag)))))

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
           (inst fcpys r-real real)))
       (let ((r-imag (complex-single-reg-imag-tn r)))
         (unless (location= imag r-imag)
           (inst fcpys r-imag imag))))
      (complex-single-stack
       (let ((nfp (current-nfp-tn vop))
             (offset (* (tn-offset r) n-word-bytes)))
         (unless (location= real r)
           (inst fsts real (@ nfp offset)))
         (inst fsts imag (@ nfp (+ offset n-word-bytes))))))))

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
           (inst fcpyd r-real real)))
       (let ((r-imag (complex-double-reg-imag-tn r)))
         (unless (location= imag r-imag)
           (inst fcpyd r-imag imag))))
      (complex-double-stack
       (let ((nfp (current-nfp-tn vop))
             (offset (* (tn-offset r) n-word-bytes)))
         (unless (location= real r)
           (inst fstd real (@ nfp offset)))
         (inst fstd imag (@ nfp (+ offset (* 2 n-word-bytes)))))))))


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
           (inst fcpys r value-tn))))
      (complex-single-stack
       (inst flds r (@ (current-nfp-tn vop)
                       (* (+ (ecase slot (:real 0) (:imag 1))
                             (tn-offset x))
                          n-word-bytes)))))))

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
           (inst fcpyd r value-tn))))
      (complex-double-stack
       (inst fldd r (@ (current-nfp-tn vop)
                       (* (+ (ecase slot (:real 0) (:imag 2))
                             (tn-offset x))
                          n-word-bytes)))))))

(define-vop (realpart/complex-double-float complex-double-float-value)
  (:translate realpart)
  (:note "complex double float realpart")
  (:variant :real))

(define-vop (imagpart/complex-double-float complex-double-float-value)
  (:translate imagpart)
  (:note "complex double float imagpart")
  (:variant :imag))
