;;;; floating point support for the Alpha

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; float move functions

(define-move-function (load-fp-zero 1) (vop x y)
  ((fp-single-zero) (single-reg)
   (fp-double-zero) (double-reg))
  (inst fmove x y))

(define-move-function (load-single 1) (vop x y)
  ((single-stack) (single-reg))
  (inst lds y (* (tn-offset x) word-bytes) (current-nfp-tn vop)))

(define-move-function (store-single 1) (vop x y)
  ((single-reg) (single-stack))
  (inst sts x (* (tn-offset y) word-bytes) (current-nfp-tn vop)))


(define-move-function (load-double 2) (vop x y)
  ((double-stack) (double-reg))
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset x) word-bytes)))
    (inst ldt y offset nfp)))

(define-move-function (store-double 2) (vop x y)
  ((double-reg) (double-stack))
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset y) word-bytes)))
    (inst stt x offset nfp)))

;;;; float move VOPs

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
		      (inst fmove x y))))
		(define-move-vop ,vop :move (,sc) (,sc)))))
  (frob single-move single-reg)
  (frob double-move double-reg))


(define-vop (move-from-float)
  (:args (x :to :save))
  (:results (y))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:variant-vars double-p size type data)
  (:note "float to pointer coercion")
  (:generator 13
    (with-fixed-allocation (y ndescr type size)
      (if double-p
	  (inst stt x (- (* data word-bytes) other-pointer-lowtag) y)
	  (inst sts x (- (* data word-bytes) other-pointer-lowtag) y)))))

(macrolet ((frob (name sc &rest args)
	     `(progn
		(define-vop (,name move-from-float)
		  (:args (x :scs (,sc) :to :save))
		  (:results (y :scs (descriptor-reg)))
		  (:variant ,@args))
		(define-move-vop ,name :move (,sc) (descriptor-reg)))))
  (frob move-from-single single-reg
    nil single-float-size single-float-type single-float-value-slot)
  (frob move-from-double double-reg
    t double-float-size double-float-type double-float-value-slot))

(macrolet ((frob (name sc double-p value)
	     `(progn
		(define-vop (,name)
		  (:args (x :scs (descriptor-reg)))
		  (:results (y :scs (,sc)))
		  (:note "pointer to float coercion")
		  (:generator 2
                    ,@(if double-p
			  `((inst ldt y (- (* ,value word-bytes)
					   other-pointer-lowtag)
				  x))
			  `((inst lds y (- (* ,value word-bytes)
					  other-pointer-lowtag)
				 x)))))
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
		  (:note "float argument move")
		  (:generator ,(if double-p 2 1)
		    (sc-case y
		      (,sc
		       (unless (location= x y)
			 (inst fmove x y)))
		      (,stack-sc
		       (let ((offset (* (tn-offset y) word-bytes)))
			 ,@(if double-p
			       '((inst stt x offset nfp))
			       '((inst sts x offset nfp))))))))
		(define-move-vop ,name :move-argument
		  (,sc descriptor-reg) (,sc)))))
  (frob move-single-float-argument single-reg single-stack nil)
  (frob move-double-float-argument double-reg double-stack t))

;;;; complex float move functions

(defun complex-single-reg-real-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'single-reg )
		  :offset (tn-offset x)))
(defun complex-single-reg-imag-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'single-reg )
		  :offset (1+ (tn-offset x))))

(defun complex-double-reg-real-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg )
		  :offset (tn-offset x)))
(defun complex-double-reg-imag-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg )
		  :offset (1+ (tn-offset x))))


(define-move-function (load-complex-single 2) (vop x y)
  ((complex-single-stack) (complex-single-reg))
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset x) sb!vm:word-bytes)))
    (let ((real-tn (complex-single-reg-real-tn y)))
      (inst lds real-tn offset nfp))
    (let ((imag-tn (complex-single-reg-imag-tn y)))
      (inst lds imag-tn (+ offset sb!vm:word-bytes) nfp))))

(define-move-function (store-complex-single 2) (vop x y)
  ((complex-single-reg) (complex-single-stack))
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset y) sb!vm:word-bytes)))
    (let ((real-tn (complex-single-reg-real-tn x)))
      (inst sts real-tn offset nfp))
    (let ((imag-tn (complex-single-reg-imag-tn x)))
      (inst sts imag-tn (+ offset sb!vm:word-bytes) nfp))))


(define-move-function (load-complex-double 4) (vop x y)
  ((complex-double-stack) (complex-double-reg))
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset x) sb!vm:word-bytes)))
    (let ((real-tn (complex-double-reg-real-tn y)))
      (inst ldt real-tn offset nfp))
    (let ((imag-tn (complex-double-reg-imag-tn y)))
      (inst ldt imag-tn (+ offset (* 2 sb!vm:word-bytes)) nfp))))

(define-move-function (store-complex-double 4) (vop x y)
  ((complex-double-reg) (complex-double-stack))
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset y) sb!vm:word-bytes)))
    (let ((real-tn (complex-double-reg-real-tn x)))
      (inst stt real-tn offset nfp))
    (let ((imag-tn (complex-double-reg-imag-tn x)))
      (inst stt imag-tn (+ offset (* 2 sb!vm:word-bytes)) nfp))))

;;;
;;; complex float register to register moves.
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
	 (inst fmove x-real y-real))
       (let ((x-imag (complex-single-reg-imag-tn x))
	     (y-imag (complex-single-reg-imag-tn y)))
	 (inst fmove x-imag y-imag)))))
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
	 (inst fmove x-real y-real))
       (let ((x-imag (complex-double-reg-imag-tn x))
	     (y-imag (complex-double-reg-imag-tn y)))
	 (inst fmove x-imag y-imag)))))
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
  (:note "complex single float to pointer coercion")
  (:generator 13
     (with-fixed-allocation (y ndescr sb!vm:complex-single-float-type
			       sb!vm:complex-single-float-size)
       (let ((real-tn (complex-single-reg-real-tn x)))
	 (inst sts real-tn (- (* sb!vm:complex-single-float-real-slot
				 sb!vm:word-bytes)
			      sb!vm:other-pointer-lowtag)
	       y))
       (let ((imag-tn (complex-single-reg-imag-tn x)))
	 (inst sts imag-tn (- (* sb!vm:complex-single-float-imag-slot
				 sb!vm:word-bytes)
			      sb!vm:other-pointer-lowtag)
	       y)))))
;;;
(define-move-vop move-from-complex-single :move
  (complex-single-reg) (descriptor-reg))

(define-vop (move-from-complex-double)
  (:args (x :scs (complex-double-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:note "complex double float to pointer coercion")
  (:generator 13
     (with-fixed-allocation (y ndescr sb!vm:complex-double-float-type
			       sb!vm:complex-double-float-size)
       (let ((real-tn (complex-double-reg-real-tn x)))
	 (inst stt real-tn (- (* sb!vm:complex-double-float-real-slot
				 sb!vm:word-bytes)
			      sb!vm:other-pointer-lowtag)
	       y))
       (let ((imag-tn (complex-double-reg-imag-tn x)))
	 (inst stt imag-tn (- (* sb!vm:complex-double-float-imag-slot
				 sb!vm:word-bytes)
			      sb!vm:other-pointer-lowtag)
	       y)))))
;;;
(define-move-vop move-from-complex-double :move
  (complex-double-reg) (descriptor-reg))

;;;
;;; Move from a descriptor to a complex float register.
;;;
(define-vop (move-to-complex-single)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (complex-single-reg)))
  (:note "pointer to complex float coercion")
  (:generator 2
    (let ((real-tn (complex-single-reg-real-tn y)))
      (inst lds real-tn (- (* complex-single-float-real-slot sb!vm:word-bytes)
			   sb!vm:other-pointer-lowtag)
	    x))
    (let ((imag-tn (complex-single-reg-imag-tn y)))
      (inst lds imag-tn (- (* complex-single-float-imag-slot sb!vm:word-bytes)
			   sb!vm:other-pointer-lowtag)
	    x))))
(define-move-vop move-to-complex-single :move
  (descriptor-reg) (complex-single-reg))

(define-vop (move-to-complex-double)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (complex-double-reg)))
  (:note "pointer to complex float coercion")
  (:generator 2
    (let ((real-tn (complex-double-reg-real-tn y)))
      (inst ldt real-tn (- (* complex-double-float-real-slot sb!vm:word-bytes)
			   sb!vm:other-pointer-lowtag)
	    x))
    (let ((imag-tn (complex-double-reg-imag-tn y)))
      (inst ldt imag-tn (- (* complex-double-float-imag-slot sb!vm:word-bytes)
			   sb!vm:other-pointer-lowtag)
	    x))))
(define-move-vop move-to-complex-double :move
  (descriptor-reg) (complex-double-reg))

;;;
;;; complex float move-argument vop
;;;
(define-vop (move-complex-single-float-argument)
  (:args (x :scs (complex-single-reg) :target y)
	 (nfp :scs (any-reg) :load-if (not (sc-is y complex-single-reg))))
  (:results (y))
  (:note "complex single float argument move")
  (:generator 1
    (sc-case y
      (complex-single-reg
       (unless (location= x y)
	 (let ((x-real (complex-single-reg-real-tn x))
	       (y-real (complex-single-reg-real-tn y)))
	   (inst fmove x-real y-real))
	 (let ((x-imag (complex-single-reg-imag-tn x))
	       (y-imag (complex-single-reg-imag-tn y)))
	   (inst fmove x-imag y-imag))))
      (complex-single-stack
       (let ((offset (* (tn-offset y) sb!vm:word-bytes)))
	 (let ((real-tn (complex-single-reg-real-tn x)))
	   (inst sts real-tn offset nfp))
	 (let ((imag-tn (complex-single-reg-imag-tn x)))
	   (inst sts imag-tn (+ offset word-bytes) nfp)))))))
(define-move-vop move-complex-single-float-argument :move-argument
  (complex-single-reg descriptor-reg) (complex-single-reg))

(define-vop (move-complex-double-float-argument)
  (:args (x :scs (complex-double-reg) :target y)
	 (nfp :scs (any-reg) :load-if (not (sc-is y complex-double-reg))))
  (:results (y))
  (:note "complex double float argument move")
  (:generator 2
    (sc-case y
      (complex-double-reg
       (unless (location= x y)
	 (let ((x-real (complex-double-reg-real-tn x))
	       (y-real (complex-double-reg-real-tn y)))
	   (inst fmove x-real y-real))
	 (let ((x-imag (complex-double-reg-imag-tn x))
	       (y-imag (complex-double-reg-imag-tn y)))
	   (inst fmove x-imag y-imag))))
      (complex-double-stack
       (let ((offset (* (tn-offset y) sb!vm:word-bytes)))
	 (let ((real-tn (complex-double-reg-real-tn x)))
	   (inst stt real-tn offset nfp))
	 (let ((imag-tn (complex-double-reg-imag-tn x)))
	   (inst stt imag-tn (+ offset (* 2 word-bytes)) nfp)))))))
(define-move-vop move-complex-double-float-argument :move-argument
  (complex-double-reg descriptor-reg) (complex-double-reg))


(define-move-vop move-argument :move-argument
  (single-reg double-reg complex-single-reg complex-double-reg)
  (descriptor-reg))


;;;; float arithmetic VOPs

(define-vop (float-op)
  (:args (x) (y))
  (:results (r))
  (:policy :fast-safe)
  (:note "inline float arithmetic")
  (:vop-var vop)
  (:save-p :compute-only))

;;; We need to insure that ops that can cause traps do not clobber an
;;; argument register with invalid results. This so the software trap
;;; handler can re-execute the instruction and produce correct IEEE
;;; result. The :from :load hopefully does that.
(macrolet ((frob (name sc ptype)
	     `(define-vop (,name float-op)
		(:args (x :scs (,sc))
		       (y :scs (,sc)))
		(:results (r :scs (,sc) :from :load))
		(:arg-types ,ptype ,ptype)
		(:result-types ,ptype))))
  (frob single-float-op single-reg single-float)
  (frob double-float-op double-reg double-float))

;; This is resumption-safe with underflow traps enabled,
;; with software handling and (notyet) dynamic rounding mode.
(macrolet ((frob (op sinst sname scost dinst dname dcost)
	     `(progn
		(define-vop (,sname single-float-op)
		  (:translate ,op)
		  (:variant-cost ,scost)
		  (:generator ,scost
                    (inst ,sinst x y r)
		    (note-this-location vop :internal-error)
		    (inst trapb)))
		(define-vop (,dname double-float-op)
		  (:translate ,op)
		  (:variant-cost ,dcost)
		  (:generator ,dcost
		    (inst ,dinst x y r)
		    (note-this-location vop :internal-error)
		    (inst trapb))))))
  ;; Not sure these cost number are right. +*- about same / is 4x
  (frob + adds_su +/single-float 1 addt_su +/double-float 1)
  (frob - subs_su -/single-float 1 subt_su -/double-float 1)
  (frob * muls_su */single-float 1 mult_su */double-float 1)
  (frob / divs_su //single-float 4 divt_su //double-float 4))

(macrolet ((frob (name inst translate sc type)
	     `(define-vop (,name)
		(:args (x :scs (,sc) :target y))
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
		  (inst ,inst x y)))))
  (frob abs/single-float fabs abs single-reg single-float)
  (frob abs/double-float fabs abs double-reg double-float)
  (frob %negate/single-float fneg %negate single-reg single-float)
  (frob %negate/double-float fneg %negate double-reg double-float))


;;;; float comparison

(define-vop (float-compare)
  (:args (x) (y))
  (:conditional)
  (:info target not-p)
  (:variant-vars eq complement)
  (:temporary (:scs (single-reg)) temp)
  (:policy :fast-safe)
  (:note "inline float comparison")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 3
    (note-this-location vop :internal-error)
    (if eq
	(inst cmpteq x y temp)
	(if complement
	    (inst cmptle x y temp)
	    (inst cmptlt x y temp)))
    (inst trapb)
    (if (if complement (not not-p) not-p)
	(inst fbeq temp target)
	(inst fbne temp target))))

(macrolet ((frob (name sc ptype)
	     `(define-vop (,name float-compare)
		(:args (x :scs (,sc))
		       (y :scs (,sc)))
		(:arg-types ,ptype ,ptype))))
  (frob single-float-compare single-reg single-float)
  (frob double-float-compare double-reg double-float))

(macrolet ((frob (translate complement sname dname eq)
	     `(progn
		(define-vop (,sname single-float-compare)
		  (:translate ,translate)
		  (:variant ,eq ,complement))
		(define-vop (,dname double-float-compare)
		  (:translate ,translate)
		  (:variant ,eq ,complement)))))
  (frob < nil </single-float </double-float nil)
  (frob > t >/single-float >/double-float nil)
  (frob = nil =/single-float =/double-float t))


;;;; float conversion

(macrolet
    ((frob (name translate inst ld-inst to-sc to-type &optional single)
           (declare (ignorable single))
           `(define-vop (,name)
              (:args (x :scs (signed-reg) :target temp
                        :load-if (not (sc-is x signed-stack))))
              (:temporary (:scs (single-stack)) temp)
              (:results (y :scs (,to-sc)))
              (:arg-types signed-num)
              (:result-types ,to-type)
              (:policy :fast-safe)
              (:note "inline float coercion")
              (:translate ,translate)
              (:vop-var vop)
              (:save-p :compute-only)
              (:generator 5
                          (let ((stack-tn
                                 (sc-case x
                                          (signed-reg
                                           (inst stl x
                                                 (* (tn-offset temp) sb!vm:word-bytes)
                                                 (current-nfp-tn vop))
                                           temp)
                                          (signed-stack
                                           x))))
                            (inst ,ld-inst y
                                  (* (tn-offset stack-tn) sb!vm:word-bytes)
                                  (current-nfp-tn vop))
                            (note-this-location vop :internal-error)
                            ,@(when single
                                `((inst cvtlq y y)))
                            (inst ,inst y y))))))
  (frob %single-float/signed %single-float cvtqs lds single-reg single-float t)
  (frob %double-float/signed %double-float cvtqt lds double-reg double-float t))

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
		  (inst ,inst x y)))))
  (frob %single-float/double-float %single-float cvtts
    double-reg double-float single-reg single-float)
  (frob %double-float/single-float %double-float fmove
    single-reg single-float double-reg double-float))

(macrolet
    ((frob (trans from-sc from-type inst &optional single)
           (declare (ignorable single))
           `(define-vop (,(symbolicate trans "/" from-type))
              (:args (x :scs (,from-sc) :target temp))
              (:temporary (:from (:argument 0) :sc single-reg) temp)
              (:temporary (:scs (signed-stack)) stack-temp)
              (:results (y :scs (signed-reg)
                           :load-if (not (sc-is y signed-stack))))
              (:arg-types ,from-type)
              (:result-types signed-num)
              (:translate ,trans)
              (:policy :fast-safe)
              (:note "inline float truncate")
              (:vop-var vop)
              (:save-p :compute-only)
              (:generator 5
                          (note-this-location vop :internal-error)
                          (inst ,inst x temp)
                          (sc-case y
                                   (signed-stack
                                    (inst stt temp
                                          (* (tn-offset y) sb!vm:word-bytes)
                                          (current-nfp-tn vop)))
                                   (signed-reg
                                    (inst stt temp
                                          (* (tn-offset stack-temp)
                                             sb!vm:word-bytes)
                                          (current-nfp-tn vop))
                                    (inst ldq y
			   (* (tn-offset stack-temp) sb!vm:word-bytes)
			   (current-nfp-tn vop))))))))
  (frob %unary-truncate single-reg single-float cvttq/c t)
  (frob %unary-truncate double-reg double-float cvttq/c)
  (frob %unary-round single-reg single-float cvttq t)
  (frob %unary-round double-reg double-float cvttq))

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
	  (inst stl bits
		(* (tn-offset stack-temp) sb!vm:word-bytes)
		(current-nfp-tn vop))
	  (inst lds res
		(* (tn-offset stack-temp) sb!vm:word-bytes)
		(current-nfp-tn vop)))
	 (single-stack
	  (inst stl bits
		(* (tn-offset res) sb!vm:word-bytes)
		(current-nfp-tn vop)))))
      (signed-stack
       (sc-case res
	 (single-reg
	  (inst lds res
		(* (tn-offset bits) sb!vm:word-bytes)
		(current-nfp-tn vop)))
	 (single-stack
	  (unless (location= bits res)
	    (inst ldl temp
		  (* (tn-offset bits) sb!vm:word-bytes)
		  (current-nfp-tn vop))
	    (inst stl temp
		  (* (tn-offset res) sb!vm:word-bytes)
		  (current-nfp-tn vop)))))))))

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
      (inst stl hi-bits
	    (* (1+ (tn-offset stack-tn)) sb!vm:word-bytes)
	    (current-nfp-tn vop))
      (inst stl lo-bits
	    (* (tn-offset stack-tn) sb!vm:word-bytes)
	    (current-nfp-tn vop)))
    (when (sc-is res double-reg)
      (inst ldt res
	    (* (tn-offset temp) sb!vm:word-bytes)
	    (current-nfp-tn vop)))))

(define-vop (single-float-bits)
  (:args (float :scs (single-reg descriptor-reg)
		:load-if (not (sc-is float single-stack))))
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
	  (inst sts float
		(* (tn-offset stack-temp) sb!vm:word-bytes)
		(current-nfp-tn vop))
	  (inst ldl bits
		(* (tn-offset stack-temp) sb!vm:word-bytes)
		(current-nfp-tn vop)))
	 (single-stack
	  (inst ldl bits
		(* (tn-offset float) sb!vm:word-bytes)
		(current-nfp-tn vop)))
	 (descriptor-reg
	  (loadw bits float sb!vm:single-float-value-slot
		 sb!vm:other-pointer-lowtag))))
      (signed-stack
       (sc-case float
	 (single-reg
	  (inst sts float
		(* (tn-offset bits) sb!vm:word-bytes)
		(current-nfp-tn vop))))))))

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
        (inst stt float
	      (* (tn-offset stack-temp) sb!vm:word-bytes)
	      (current-nfp-tn vop))
        (inst ldl hi-bits
	      (* (1+ (tn-offset stack-temp)) sb!vm:word-bytes)
	      (current-nfp-tn vop)))
      (double-stack
        (inst ldl hi-bits
	      (* (1+ (tn-offset float)) sb!vm:word-bytes)
	      (current-nfp-tn vop)))
      (descriptor-reg
        (loadw hi-bits float (1+ sb!vm:double-float-value-slot)
	       sb!vm:other-pointer-lowtag)))))

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
        (inst stt float
	      (* (tn-offset stack-temp) sb!vm:word-bytes)
	      (current-nfp-tn vop))
	(inst ldl lo-bits
	      (* (tn-offset stack-temp) sb!vm:word-bytes)
	      (current-nfp-tn vop)))
      (double-stack
       (inst ldl lo-bits
	     (* (tn-offset float) sb!vm:word-bytes)
	     (current-nfp-tn vop)))
      (descriptor-reg
       (loadw lo-bits float sb!vm:double-float-value-slot
	      sb!vm:other-pointer-lowtag)))
    (inst mskll lo-bits 4 lo-bits)))


;;;; float mode hackery

(sb!xc:deftype float-modes () '(unsigned-byte 32)) ;actually 24 -dan
(defknown floating-point-modes () float-modes (flushable))
(defknown ((setf floating-point-modes)) (float-modes)
  float-modes)

;;; Modes bits are (byte 12 52) of fpcr. Grab and return in low bits.
(define-vop (floating-point-modes)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate floating-point-modes)
  (:policy :fast-safe)
  (:vop-var vop)
  (:temporary (:sc double-stack) temp)
  (:temporary (:sc double-reg) temp1)
  (:generator 5
    (let ((nfp (current-nfp-tn vop)))
      (inst excb)
      (inst mf_fpcr temp1 temp1 temp1)
      (inst excb)
      (inst stt temp1 (* word-bytes (tn-offset temp)) nfp)
      (inst ldl res   (* (1+ (tn-offset temp)) sb!vm:word-bytes) nfp)
      (inst srl res 49 res))))

(define-vop (set-floating-point-modes)
  (:args (new :scs (unsigned-reg) :target res))
  (:results (res :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:result-types unsigned-num)
  (:translate (setf floating-point-modes))
  (:policy :fast-safe)
  (:temporary (:sc double-stack) temp)
  (:temporary (:sc double-reg) temp1)
  (:vop-var vop)
  (:generator 8
    (let ((nfp (current-nfp-tn vop)))
      (inst sll new  49 res)
      (inst stl zero-tn  (* (tn-offset temp) sb!vm:word-bytes) nfp)
      (inst stl res   (* (1+ (tn-offset temp)) sb!vm:word-bytes) nfp)
      (inst ldt temp1 (* (tn-offset temp) sb!vm:word-bytes) nfp)
      (inst excb)
      (inst mt_fpcr temp1 temp1 temp1)
      (inst excb)
      (move res new))))


;;;; complex float VOPs

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
    (sc-case r
      (complex-single-reg
       (let ((r-real (complex-single-reg-real-tn r)))
	 (unless (location= real r-real)
	   (inst fmove real r-real)))
       (let ((r-imag (complex-single-reg-imag-tn r)))
	 (unless (location= imag r-imag)
	   (inst fmove imag r-imag))))
      (complex-single-stack
       (let ((nfp (current-nfp-tn vop))
	     (offset (* (tn-offset r) sb!vm:word-bytes)))
	 (inst sts real offset nfp)
	 (inst sts imag (+ offset sb!vm:word-bytes) nfp))))))

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
    (sc-case r
      (complex-double-reg
       (let ((r-real (complex-double-reg-real-tn r)))
	 (unless (location= real r-real)
	   (inst fmove real r-real)))
       (let ((r-imag (complex-double-reg-imag-tn r)))
	 (unless (location= imag r-imag)
	   (inst fmove imag r-imag))))
      (complex-double-stack
       (let ((nfp (current-nfp-tn vop))
	     (offset (* (tn-offset r) sb!vm:word-bytes)))
	 (inst stt real offset nfp)
	 (inst stt imag (+ offset (* 2 sb!vm:word-bytes)) nfp))))))

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
	   (inst fmove value-tn r))))
      (complex-single-stack
       (inst lds r (* (+ (ecase slot (:real 0) (:imag 1)) (tn-offset x))
		      sb!vm:word-bytes)
	     (current-nfp-tn vop))))))

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
	   (inst fmove value-tn r))))
      (complex-double-stack
       (inst ldt r (* (+ (ecase slot (:real 0) (:imag 2)) (tn-offset x))
		      sb!vm:word-bytes)
	     (current-nfp-tn vop))))))

(define-vop (realpart/complex-double-float complex-double-float-value)
  (:translate realpart)
  (:note "complex double float realpart")
  (:variant :real))

(define-vop (imagpart/complex-double-float complex-double-float-value)
  (:translate imagpart)
  (:note "complex double float imagpart")
  (:variant :imag))
