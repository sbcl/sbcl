(in-package "SB!VM")


;;;; Move functions.

(define-move-fun (load-fp-zero 1) (vop x y)
  ((fp-single-zero) (single-reg)
   (fp-double-zero) (double-reg))
  (inst funop :copy x y))

(defun ld-float (offset base r)
  (cond ((< offset (ash 1 4))
	 (inst flds offset base r))
	(t
	 (inst ldo offset zero-tn lip-tn)
	 (inst fldx lip-tn base r))))

(define-move-fun (load-float 1) (vop x y)
  ((single-stack) (single-reg)
   (double-stack) (double-reg))
  (let ((offset (* (tn-offset x) n-word-bytes)))
    (ld-float offset (current-nfp-tn vop) y)))

(defun str-float (x offset base)
  (cond ((< offset (ash 1 4))
	 (inst fsts x offset base))
	(t
	 (inst ldo offset zero-tn lip-tn)
	 (inst fstx x lip-tn base))))

(define-move-fun (store-float 1) (vop x y)
  ((single-reg) (single-stack)
   (double-reg) (double-stack))
  (let ((offset (* (tn-offset y) n-word-bytes)))
    (str-float x offset (current-nfp-tn vop))))


;;;; Move VOPs

(define-vop (move-float)
  (:args (x :scs (single-reg double-reg)
	    :target y
	    :load-if (not (location= x y))))
  (:results (y :scs (single-reg double-reg)
	       :load-if (not (location= x y))))
  (:note "float move")
  (:generator 0
    (unless (location= y x)
      (inst funop :copy x y))))

(define-move-vop move-float :move (single-reg) (single-reg))
(define-move-vop move-float :move (double-reg) (double-reg))


(define-vop (move-from-float)
  (:args (x :to :save))
  (:results (y :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:variant-vars size type data)
  (:note "float to pointer coercion")
  (:generator 13
    (with-fixed-allocation (y ndescr type size))
    (inst fsts x (- (* data n-word-bytes) other-pointer-lowtag) y)))

(macrolet ((frob (name sc &rest args)
	     `(progn
		(define-vop (,name move-from-float)
		  (:args (x :scs (,sc) :to :save))
		  (:variant ,@args))
		(define-move-vop ,name :move (,sc) (descriptor-reg)))))
  (frob move-from-single single-reg
    single-float-size single-float-widetag single-float-value-slot)
  (frob move-from-double double-reg
    double-float-size double-float-widetag double-float-value-slot))

(define-vop (move-to-float)
  (:args (x :scs (descriptor-reg)))
  (:results (y))
  (:variant-vars offset)
  (:note "pointer to float coercion")
  (:generator 2
    (inst flds (- (* offset n-word-bytes) other-pointer-lowtag) x y)))

(macrolet ((frob (name sc offset)
	     `(progn
		(define-vop (,name move-to-float)
		  (:results (y :scs (,sc)))
		  (:variant ,offset))
		(define-move-vop ,name :move (descriptor-reg) (,sc)))))
  (frob move-to-single single-reg single-float-value-slot)
  (frob move-to-double double-reg double-float-value-slot))


(define-vop (move-float-argument)
  (:args (x :scs (single-reg double-reg) :target y)
	 (nfp :scs (any-reg)
	      :load-if (not (sc-is y single-reg double-reg))))
  (:results (y))
  (:note "float argument move")
  (:generator 1
    (sc-case y
      ((single-reg double-reg)
       (unless (location= x y)
	 (inst funop :copy x y)))
      ((single-stack double-stack)
       (let ((offset (* (tn-offset y) n-word-bytes)))
	 (str-float x offset nfp))))))

(define-move-vop move-float-argument :move-arg
  (single-reg descriptor-reg) (single-reg))
(define-move-vop move-float-argument :move-arg
  (double-reg descriptor-reg) (double-reg))


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
	(offset (* (tn-offset x) n-word-bytes)))
    (let ((real-tn (complex-single-reg-real-tn y)))
      (ld-float offset nfp real-tn))
    (let ((imag-tn (complex-single-reg-imag-tn y)))
      (ld-float (+ offset n-word-bytes) nfp imag-tn))))
  
(define-move-fun (store-complex-single 2) (vop x y)
  ((complex-single-reg) (complex-single-stack))
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset y) n-word-bytes)))
    (let ((real-tn (complex-single-reg-real-tn x)))
      (str-float real-tn offset nfp))
    (let ((imag-tn (complex-single-reg-imag-tn x)))
      (str-float imag-tn (+ offset n-word-bytes) nfp))))


(define-move-fun (load-complex-double 4) (vop x y)
  ((complex-double-stack) (complex-double-reg))
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset x) n-word-bytes)))
    (let ((real-tn (complex-double-reg-real-tn y)))
      (ld-float offset nfp real-tn))
    (let ((imag-tn (complex-double-reg-imag-tn y)))
      (ld-float (+ offset (* 2 n-word-bytes)) nfp imag-tn))))

(define-move-fun (store-complex-double 4) (vop x y)
  ((complex-double-reg) (complex-double-stack))
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset y) n-word-bytes)))
    (let ((real-tn (complex-double-reg-real-tn x)))
      (str-float real-tn offset nfp))
    (let ((imag-tn (complex-double-reg-imag-tn x)))
      (str-float imag-tn (+ offset (* 2 n-word-bytes)) nfp))))

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
	 (inst funop :copy x-real y-real))
       (let ((x-imag (complex-single-reg-imag-tn x))
	     (y-imag (complex-single-reg-imag-tn y)))
	 (inst funop :copy x-imag y-imag)))))
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
	 (inst funop :copy x-real y-real))
       (let ((x-imag (complex-double-reg-imag-tn x))
	     (y-imag (complex-double-reg-imag-tn y)))
	 (inst funop :copy x-imag y-imag)))))
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
     (with-fixed-allocation (y ndescr complex-single-float-widetag
			       complex-single-float-size))
     (let ((real-tn (complex-single-reg-real-tn x)))
       (inst fsts real-tn (- (* complex-single-float-real-slot n-word-bytes)
			     other-pointer-lowtag)
	     y))
     (let ((imag-tn (complex-single-reg-imag-tn x)))
       (inst fsts imag-tn (- (* complex-single-float-imag-slot n-word-bytes)
			     other-pointer-lowtag)
	     y))))
;;;
(define-move-vop move-from-complex-single :move
  (complex-single-reg) (descriptor-reg))

(define-vop (move-from-complex-double)
  (:args (x :scs (complex-double-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:note "complex double float to pointer coercion")
  (:generator 13
     (with-fixed-allocation (y ndescr complex-double-float-widetag
			       complex-double-float-size))
     (let ((real-tn (complex-double-reg-real-tn x)))
       (inst fsts real-tn (- (* complex-double-float-real-slot n-word-bytes)
			     other-pointer-lowtag)
	     y))
     (let ((imag-tn (complex-double-reg-imag-tn x)))
       (inst fsts imag-tn (- (* complex-double-float-imag-slot n-word-bytes)
			     other-pointer-lowtag)
	     y))))
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
      (inst flds (- (* complex-single-float-real-slot n-word-bytes)
		    other-pointer-lowtag)
	    x real-tn))
    (let ((imag-tn (complex-single-reg-imag-tn y)))
      (inst flds (- (* complex-single-float-imag-slot n-word-bytes)
		    other-pointer-lowtag)
	    x imag-tn))))
(define-move-vop move-to-complex-single :move
  (descriptor-reg) (complex-single-reg))

(define-vop (move-to-complex-double)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (complex-double-reg)))
  (:note "pointer to complex float coercion")
  (:generator 2
    (let ((real-tn (complex-double-reg-real-tn y)))
      (inst flds (- (* complex-double-float-real-slot n-word-bytes)
		    other-pointer-lowtag)
	    x real-tn))
    (let ((imag-tn (complex-double-reg-imag-tn y)))
      (inst flds (- (* complex-double-float-imag-slot n-word-bytes)
		    other-pointer-lowtag)
	    x imag-tn))))
(define-move-vop move-to-complex-double :move
  (descriptor-reg) (complex-double-reg))

;;;
;;; Complex float move-argument vop
;;;
(define-vop (move-complex-single-float-argument)
  (:args (x :scs (complex-single-reg) :target y)
	 (nfp :scs (any-reg) :load-if (not (sc-is y complex-single-reg))))
  (:results (y))
  (:note "float argument move")
  (:generator 1
    (sc-case y
      (complex-single-reg
       (unless (location= x y)
	 (let ((x-real (complex-single-reg-real-tn x))
	       (y-real (complex-single-reg-real-tn y)))
	   (inst funop :copy x-real y-real))
	 (let ((x-imag (complex-single-reg-imag-tn x))
	       (y-imag (complex-single-reg-imag-tn y)))
	   (inst funop :copy x-imag y-imag))))
      (complex-single-stack
       (let ((offset (* (tn-offset y) n-word-bytes)))
	 (let ((real-tn (complex-single-reg-real-tn x)))
	   (str-float real-tn offset nfp))
	 (let ((imag-tn (complex-single-reg-imag-tn x)))
	   (str-float imag-tn (+ offset n-word-bytes) nfp)))))))
;;;
(define-move-vop move-complex-single-float-argument :move-arg
  (complex-single-reg descriptor-reg) (complex-single-reg))

(define-vop (move-complex-double-float-argument)
  (:args (x :scs (complex-double-reg) :target y)
	 (nfp :scs (any-reg) :load-if (not (sc-is y complex-double-reg))))
  (:results (y))
  (:note "float argument move")
  (:generator 1
    (sc-case y
      (complex-double-reg
       (unless (location= x y)
	 (let ((x-real (complex-double-reg-real-tn x))
	       (y-real (complex-double-reg-real-tn y)))
	   (inst funop :copy x-real y-real))
	 (let ((x-imag (complex-double-reg-imag-tn x))
	       (y-imag (complex-double-reg-imag-tn y)))
	   (inst funop :copy x-imag y-imag))))
      (complex-double-stack
       (let ((offset (* (tn-offset y) n-word-bytes)))
	 (let ((real-tn (complex-double-reg-real-tn x)))
	   (str-float real-tn offset nfp))
	 (let ((imag-tn (complex-double-reg-imag-tn x)))
	   (str-float imag-tn (+ offset (* 2 n-word-bytes)) nfp)))))))
;;;
(define-move-vop move-complex-double-float-argument :move-arg
  (complex-double-reg descriptor-reg) (complex-double-reg))


(define-move-vop move-argument :move-arg
  (single-reg double-reg complex-single-reg complex-double-reg)
  (descriptor-reg))


;;;; Arithmetic VOPs.

(define-vop (float-op)
  (:args (x) (y))
  (:results (r))
  (:variant-vars operation)
  (:policy :fast-safe)
  (:note "inline float arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:node-var node)
  (:generator 0
    (inst fbinop operation x y r)
    (when (policy node (or (= debug 3) (> safety speed)))
      (note-next-instruction vop :internal-error)
      (inst fsts fp-single-zero-tn 0 csp-tn))))

(macrolet ((frob (name sc zero-sc ptype)
	     `(define-vop (,name float-op)
		(:args (x :scs (,sc ,zero-sc))
		       (y :scs (,sc ,zero-sc)))
		(:results (r :scs (,sc)))
		(:arg-types ,ptype ,ptype)
		(:result-types ,ptype))))
  (frob single-float-op single-reg fp-single-zero single-float)
  (frob double-float-op double-reg fp-double-zero double-float))

(macrolet ((frob (translate op sname scost dname dcost)
	     `(progn
		(define-vop (,sname single-float-op)
		  (:translate ,translate)
		  (:variant ,op)
		  (:variant-cost ,scost))
		(define-vop (,dname double-float-op)
		  (:translate ,translate)
		  (:variant ,op)
		  (:variant-cost ,dcost)))))
  (frob + :add +/single-float 2 +/double-float 2)
  (frob - :sub -/single-float 2 -/double-float 2)
  (frob * :mpy */single-float 4 */double-float 5)
  (frob / :div //single-float 12 //double-float 19))


(macrolet ((frob (name translate sc type inst)
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
		(:node-var node)
		(:generator 1
		  ,inst
		  (when (policy node (or (= debug 3) (> safety speed)))
		    (note-next-instruction vop :internal-error)
		    (inst fsts fp-single-zero-tn 0 csp-tn))))))
  (frob abs/single-float abs single-reg single-float
    (inst funop :abs x y))
  (frob abs/double-float abs double-reg double-float
    (inst funop :abs x y))
  (frob %negate/single-float %negate single-reg single-float
    (inst fbinop :sub fp-single-zero-tn x y))
  (frob %negate/double-float %negate double-reg double-float
    (inst fbinop :sub fp-double-zero-tn x y)))


;;;; Comparison:

(define-vop (float-compare)
  (:args (x) (y))
  (:conditional)
  (:info target not-p)
  (:variant-vars condition complement)
  (:policy :fast-safe)
  (:note "inline float comparison")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 3
    ;; This is the condition to nullify the branch, so it is inverted.
    (inst fcmp (if not-p condition complement) x y)
    (note-next-instruction vop :internal-error)
    (inst ftest)
    (inst b target :nullify t)))

(macrolet ((frob (name sc zero-sc ptype)
	     `(define-vop (,name float-compare)
		(:args (x :scs (,sc ,zero-sc))
		       (y :scs (,sc ,zero-sc)))
		(:arg-types ,ptype ,ptype))))
  (frob single-float-compare single-reg fp-single-zero single-float)
  (frob double-float-compare double-reg fp-double-zero double-float))

(macrolet ((frob (translate condition complement sname dname)
	     `(progn
		(define-vop (,sname single-float-compare)
		  (:translate ,translate)
		  (:variant ,condition ,complement))
		(define-vop (,dname double-float-compare)
		  (:translate ,translate)
		  (:variant ,condition ,complement)))))
  (frob < #b01001 #b10101 </single-float </double-float)
  (frob > #b10001 #b01101 >/single-float >/double-float)
  (frob = #b00101 #b11001 eql/single-float eql/double-float))


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
		(:node-var node)
		(:generator 2
		  (inst fcnvff x y)
		  (when (policy node (or (= debug 3) (> safety speed)))
		    (note-next-instruction vop :internal-error)
		    (inst fsts fp-single-zero-tn 0 csp-tn))))))
  (frob %single-float/double-float %single-float
    double-reg double-float
    single-reg single-float)
  (frob %double-float/single-float %double-float
    single-reg single-float
    double-reg double-float))

(macrolet ((frob (name translate to-sc to-type)
	     `(define-vop (,name)
		(:args (x :scs (signed-reg)
			  :load-if (not (sc-is x signed-stack))
			  :target stack-temp))
		(:arg-types signed-num)
		(:results (y :scs (,to-sc)))
		(:result-types ,to-type)
		(:policy :fast-safe)
		(:note "inline float coercion")
		(:translate ,translate)
		(:vop-var vop)
		(:save-p :compute-only)
		(:node-var node)
		(:temporary (:scs (signed-stack) :from (:argument 0))
			    stack-temp)
		(:temporary (:scs (single-reg) :to (:result 0) :target y)
			    fp-temp)
		(:temporary (:scs (any-reg) :from (:argument 0)
				  :to (:result 0)) index)
		(:generator 5
		  (let* ((nfp (current-nfp-tn vop))
			 (stack-tn
			  (sc-case x
			    (signed-stack
			     x)
			    (signed-reg
			     (storew x nfp (tn-offset stack-temp))
			     stack-temp)))
			 (offset (* (tn-offset stack-tn) n-word-bytes)))
		    (cond ((< offset (ash 1 4))
			   (inst flds offset nfp fp-temp))
			  (t
			   (inst ldo offset zero-tn index)
			   (inst fldx index nfp fp-temp)))
		    (inst fcnvxf fp-temp y)
		    (when (policy node (or (= debug 3) (> safety speed)))
		      (note-next-instruction vop :internal-error)
		      (inst fsts fp-single-zero-tn 0 csp-tn)))))))
  (frob %single-float/signed %single-float
    single-reg single-float)
  (frob %double-float/signed %double-float
    double-reg double-float))


(macrolet ((frob (trans from-sc from-type inst note)
	     `(define-vop (,(symbolicate trans "/" from-type))
		(:args (x :scs (,from-sc)
			  :target fp-temp))
		(:results (y :scs (signed-reg)
			     :load-if (not (sc-is y signed-stack))))
		(:arg-types ,from-type)
		(:result-types signed-num)
		(:translate ,trans)
		(:policy :fast-safe)
		(:note ,note)
		(:vop-var vop)
		(:save-p :compute-only)
		(:temporary (:scs (single-reg) :from (:argument 0)) fp-temp)
		(:temporary (:scs (signed-stack) :to (:result 0) :target y)
			    stack-temp)
		(:temporary (:scs (any-reg) :from (:argument 0)
				  :to (:result 0)) index)
		(:generator 3
		  (let* ((nfp (current-nfp-tn vop))
			 (stack-tn
			  (sc-case y
			    (signed-stack y)
			    (signed-reg stack-temp)))
			 (offset (* (tn-offset stack-tn) n-word-bytes)))
		    (inst ,inst x fp-temp)
		    (cond ((< offset (ash 1 4))
			   (note-next-instruction vop :internal-error)
			   (inst fsts fp-temp offset nfp))
			  (t
			   (inst ldo offset zero-tn index)
			   (note-next-instruction vop :internal-error)
			   (inst fstx fp-temp index nfp)))
		    (unless (eq y stack-tn)
		      (loadw y nfp (tn-offset stack-tn))))))))
  (frob %unary-round single-reg single-float fcnvfx "inline float round")
  (frob %unary-round double-reg double-float fcnvfx "inline float round")
  (frob %unary-truncate single-reg single-float fcnvfxt
    "inline float truncate")
  (frob %unary-truncate double-reg double-float fcnvfxt
    "inline float truncate"))


(define-vop (make-single-float)
  (:args (bits :scs (signed-reg)
	       :load-if (or (not (sc-is bits signed-stack))
			    (sc-is res single-stack))
	       :target res))
  (:results (res :scs (single-reg)
		 :load-if (not (sc-is bits single-stack))))
  (:arg-types signed-num)
  (:result-types single-float)
  (:translate make-single-float)
  (:policy :fast-safe)
  (:vop-var vop)
  (:temporary (:scs (single-stack) :from (:argument 0) :to (:result 0)) temp)
  (:temporary (:scs (any-reg) :from (:argument 0) :to (:result 0)) index)
  (:generator 2
    (let ((nfp (current-nfp-tn vop)))
      (sc-case bits
	(signed-reg
	 (sc-case res
	   (single-reg
	    (let ((offset (* (tn-offset temp) n-word-bytes)))
	      (inst stw bits offset nfp)
	      (cond ((< offset (ash 1 4))
		     (inst flds offset nfp res))
		    (t
		     (inst ldo offset zero-tn index)
		     (inst fldx index nfp res)))))
	   (single-stack
	    (inst stw bits (* (tn-offset res) n-word-bytes) nfp))))
	(signed-stack
	 (sc-case res
	   (single-reg
	    (let ((offset (* (tn-offset bits) n-word-bytes)))
	      (cond ((< offset (ash 1 4))
		     (inst flds offset nfp res))
		    (t
		     (inst ldo offset zero-tn index)
		     (inst fldx index nfp res)))))))))))

(define-vop (make-double-float)
  (:args (hi-bits :scs (signed-reg))
	 (lo-bits :scs (unsigned-reg)))
  (:results (res :scs (double-reg)
		 :load-if (not (sc-is res double-stack))))
  (:arg-types signed-num unsigned-num)
  (:result-types double-float)
  (:translate make-double-float)
  (:policy :fast-safe)
  (:temporary (:scs (double-stack) :to (:result 0)) temp)
  (:temporary (:scs (any-reg) :from (:argument 0) :to (:result 0)) index)
  (:vop-var vop)
  (:generator 2
    (let* ((nfp (current-nfp-tn vop))
	   (stack-tn (sc-case res
		       (double-stack res)
		       (double-reg temp)))
	   (offset (* (tn-offset stack-tn) n-word-bytes)))
      (inst stw hi-bits offset nfp)
      (inst stw lo-bits (+ offset n-word-bytes) nfp)
      (cond ((eq stack-tn res))
	    ((< offset (ash 1 4))
	     (inst flds offset nfp res))
	    (t
	     (inst ldo offset zero-tn index)
	     (inst fldx index nfp res))))))


(define-vop (single-float-bits)
  (:args (float :scs (single-reg)
		:load-if (not (sc-is float single-stack))))
  (:results (bits :scs (signed-reg)
		  :load-if (or (not (sc-is bits signed-stack))
			       (sc-is float single-stack))))
  (:arg-types single-float)
  (:result-types signed-num)
  (:translate single-float-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:temporary (:scs (signed-stack) :from (:argument 0) :to (:result 0)) temp)
  (:temporary (:scs (any-reg) :from (:argument 0) :to (:result 0)) index)
  (:generator 2
    (let ((nfp (current-nfp-tn vop)))
      (sc-case float
	(single-reg
	 (sc-case bits
	   (signed-reg
	    (let ((offset (* (tn-offset temp) n-word-bytes)))
	      (cond ((< offset (ash 1 4))
		     (inst fsts float offset nfp))
		    (t
		     (inst ldo offset zero-tn index)
		     (inst fstx float index nfp)))
	      (inst ldw offset nfp bits)))
	   (signed-stack
	    (let ((offset (* (tn-offset bits) n-word-bytes)))
	      (cond ((< offset (ash 1 4))
		     (inst fsts float offset nfp))
		    (t
		     (inst ldo offset zero-tn index)
		     (inst fstx float index nfp)))))))
	(single-stack
	 (sc-case bits
	   (signed-reg
	    (inst ldw (* (tn-offset float) n-word-bytes) nfp bits))))))))

(define-vop (double-float-high-bits)
  (:args (float :scs (double-reg)
		:load-if (not (sc-is float double-stack))))
  (:results (hi-bits :scs (signed-reg)
		     :load-if (or (not (sc-is hi-bits signed-stack))
				  (sc-is float double-stack))))
  (:arg-types double-float)
  (:result-types signed-num)
  (:translate double-float-high-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:temporary (:scs (signed-stack) :from (:argument 0) :to (:result 0)) temp)
  (:temporary (:scs (any-reg) :from (:argument 0) :to (:result 0)) index)
  (:generator 2
    (let ((nfp (current-nfp-tn vop)))
      (sc-case float
	(double-reg
	 (sc-case hi-bits
	   (signed-reg
	    (let ((offset (* (tn-offset temp) n-word-bytes)))
	      (cond ((< offset (ash 1 4))
		     (inst fsts float offset nfp :side 0))
		    (t
		     (inst ldo offset zero-tn index)
		     (inst fstx float index nfp :side 0)))
	      (inst ldw offset nfp hi-bits)))
	   (signed-stack
	    (let ((offset (* (tn-offset hi-bits) n-word-bytes)))
	      (cond ((< offset (ash 1 4))
		     (inst fsts float offset nfp :side 0))
		    (t
		     (inst ldo offset zero-tn index)
		     (inst fstx float index nfp :side 0)))))))
	(double-stack
	 (sc-case hi-bits
	   (signed-reg
	    (let ((offset (* (tn-offset float) n-word-bytes)))
	      (inst ldw offset nfp hi-bits)))))))))

(define-vop (double-float-low-bits)
  (:args (float :scs (double-reg)
		:load-if (not (sc-is float double-stack))))
  (:results (lo-bits :scs (unsigned-reg)
		     :load-if (or (not (sc-is lo-bits unsigned-stack))
				  (sc-is float double-stack))))
  (:arg-types double-float)
  (:result-types unsigned-num)
  (:translate double-float-low-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:temporary (:scs (unsigned-stack) :from (:argument 0) :to (:result 0)) temp)
  (:temporary (:scs (any-reg) :from (:argument 0) :to (:result 0)) index)
  (:generator 2
    (let ((nfp (current-nfp-tn vop)))
      (sc-case float
	(double-reg
	 (sc-case lo-bits
	   (unsigned-reg
	    (let ((offset (* (tn-offset temp) n-word-bytes)))
	      (cond ((< offset (ash 1 4))
		     (inst fsts float offset nfp :side 1))
		    (t
		     (inst ldo offset zero-tn index)
		     (inst fstx float index nfp :side 1)))
	      (inst ldw offset nfp lo-bits)))
	   (unsigned-stack
	    (let ((offset (* (tn-offset lo-bits) n-word-bytes)))
	      (cond ((< offset (ash 1 4))
		     (inst fsts float offset nfp :side 1))
		    (t
		     (inst ldo offset zero-tn index)
		     (inst fstx float index nfp :side 1)))))))
	(double-stack
	 (sc-case lo-bits
	   (unsigned-reg
	    (let ((offset (* (1+ (tn-offset float)) n-word-bytes)))
	      (inst ldw offset nfp lo-bits)))))))))



;;;; Float mode hackery:

(sb!xc:deftype float-modes () '(unsigned-byte 32))
(defknown floating-point-modes () float-modes (flushable))
(defknown ((setf floating-point-modes)) (float-modes)
  float-modes)

(define-vop (floating-point-modes)
  (:results (res :scs (unsigned-reg)
		 :load-if (not (sc-is res unsigned-stack))))
  (:result-types unsigned-num)
  (:translate floating-point-modes)
  (:policy :fast-safe)
  (:temporary (:scs (unsigned-stack) :to (:result 0)) temp)
  (:temporary (:scs (any-reg) :from (:argument 0) :to (:result 0)) index)
  (:vop-var vop)
  (:generator 3
    (let* ((nfp (current-nfp-tn vop))
	   (stack-tn (sc-case res
		       (unsigned-stack res)
		       (unsigned-reg temp)))
	   (offset (* (tn-offset stack-tn) n-word-bytes)))
      (cond ((< offset (ash 1 4))
	     (inst fsts fp-single-zero-tn offset nfp))
	    (t
	     (inst ldo offset zero-tn index)
	     (inst fstx fp-single-zero-tn index nfp)))
      (unless (eq stack-tn res)
	(inst ldw offset nfp res)))))

(define-vop (set-floating-point-modes)
  (:args (new :scs (unsigned-reg)
	      :load-if (not (sc-is new unsigned-stack))))
  (:results (res :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:result-types unsigned-num)
  (:translate (setf floating-point-modes))
  (:policy :fast-safe)
  (:temporary (:scs (unsigned-stack) :from (:argument 0) :to (:result 0)) temp)
  (:temporary (:scs (any-reg) :from (:argument 0) :to (:result 0)) index)
  (:vop-var vop)
  (:generator 3
    (let* ((nfp (current-nfp-tn vop))
	   (stack-tn (sc-case new
		       (unsigned-stack new)
		       (unsigned-reg temp)))
	   (offset (* (tn-offset stack-tn) n-word-bytes)))
      (unless (eq new stack-tn)
	(inst stw new offset nfp))
      (cond ((< offset (ash 1 4))
	     (inst flds offset nfp fp-single-zero-tn))
	    (t
	     (inst ldo offset zero-tn index)
	     (inst fldx index nfp fp-single-zero-tn)))
      (inst ldw offset nfp res))))


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
    (sc-case r
      (complex-single-reg
       (let ((r-real (complex-single-reg-real-tn r)))
	 (unless (location= real r-real)
	   (inst funop :copy real r-real)))
       (let ((r-imag (complex-single-reg-imag-tn r)))
	 (unless (location= imag r-imag)
	   (inst funop :copy imag r-imag))))
      (complex-single-stack
       (let ((nfp (current-nfp-tn vop))
	     (offset (* (tn-offset r) n-word-bytes)))
	 (str-float real offset nfp)
	 (str-float imag (+ offset n-word-bytes) nfp))))))

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
	   (inst funop :copy real r-real)))
       (let ((r-imag (complex-double-reg-imag-tn r)))
	 (unless (location= imag r-imag)
	   (inst funop :copy imag r-imag))))
      (complex-double-stack
       (let ((nfp (current-nfp-tn vop))
	     (offset (* (tn-offset r) n-word-bytes)))
	 (str-float real offset nfp)
	 (str-float imag (+ offset (* 2 n-word-bytes)) nfp))))))


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
	   (inst funop :copy value-tn r))))
      (complex-single-stack
       (ld-float (* (+ (ecase slot (:real 0) (:imag 1)) (tn-offset x))
		    n-word-bytes)
		 (current-nfp-tn vop) r)))))

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
	   (inst funop :copy value-tn r))))
      (complex-double-stack
       (ld-float (* (+ (ecase slot (:real 0) (:imag 2)) (tn-offset x))
		    n-word-bytes)
		 (current-nfp-tn vop) r)))))

(define-vop (realpart/complex-double-float complex-double-float-value)
  (:translate realpart)
  (:note "complex double float realpart")
  (:variant :real))

(define-vop (imagpart/complex-double-float complex-double-float-value)
  (:translate imagpart)
  (:note "complex double float imagpart")
  (:variant :imag))
