;;;; floating point support for the x86

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

(macrolet ((ea-for-xf-desc (tn slot)
	     `(make-ea
	       :qword :base ,tn
	       :disp (- (* ,slot n-word-bytes)
			other-pointer-lowtag))))
  (defun ea-for-sf-desc (tn)
    (ea-for-xf-desc tn single-float-value-slot))
  (defun ea-for-df-desc (tn)
    (ea-for-xf-desc tn double-float-value-slot))
  ;; complex floats
  (defun ea-for-csf-real-desc (tn)
    (ea-for-xf-desc tn complex-single-float-real-slot))
  (defun ea-for-csf-imag-desc (tn)
    (ea-for-xf-desc tn complex-single-float-imag-slot))
  (defun ea-for-cdf-real-desc (tn)
    (ea-for-xf-desc tn complex-double-float-real-slot))
  (defun ea-for-cdf-imag-desc (tn)
    (ea-for-xf-desc tn complex-double-float-imag-slot)))

(macrolet ((ea-for-xf-stack (tn kind)
	     (declare (ignore kind))
	     `(make-ea
	       :qword :base rbp-tn
	       :disp (- (* (+ (tn-offset ,tn) 1)
			   n-word-bytes)))))
  (defun ea-for-sf-stack (tn)
    (ea-for-xf-stack tn :single))
  (defun ea-for-df-stack (tn)
    (ea-for-xf-stack tn :double)))

;;; Telling the FPU to wait is required in order to make signals occur
;;; at the expected place, but naturally slows things down.
;;;
;;; NODE is the node whose compilation policy controls the decision
;;; whether to just blast through carelessly or carefully emit wait
;;; instructions and whatnot.
;;;
;;; NOTE-NEXT-INSTRUCTION, if supplied, is to be passed to
;;; #'NOTE-NEXT-INSTRUCTION.
(defun maybe-fp-wait (node &optional note-next-instruction)
  (when (policy node (or (= debug 3) (> safety speed))))
    (when note-next-instruction
      (note-next-instruction note-next-instruction :internal-error))
    #+nil
    (inst wait))

;;; complex float stack EAs
(macrolet ((ea-for-cxf-stack (tn kind slot &optional base)
	     (declare (ignore kind))
	     `(make-ea
	       :qword :base ,base
	       :disp (- (* (+ (tn-offset ,tn)
			      (* 1 (ecase ,slot (:real 1) (:imag 2))))
			   n-word-bytes)))))
  (defun ea-for-csf-real-stack (tn &optional (base rbp-tn))
    (ea-for-cxf-stack tn :single :real base))
  (defun ea-for-csf-imag-stack (tn &optional (base rbp-tn))
    (ea-for-cxf-stack tn :single :imag base))
  (defun ea-for-cdf-real-stack (tn &optional (base rbp-tn))
    (ea-for-cxf-stack tn :double :real base))
  (defun ea-for-cdf-imag-stack (tn &optional (base rbp-tn))
    (ea-for-cxf-stack tn :double :imag base)))


;;;; move functions

;;; X is source, Y is destination.

(define-move-fun (load-fp-zero 1) (vop x y)
  ((fp-single-zero) (single-reg)
   (fp-double-zero) (double-reg))
  (identity x) ; KLUDGE: IDENTITY as IGNORABLE...
  (inst movq y fp-double-zero-tn))

(define-move-fun (load-single 2) (vop x y)
  ((single-stack) (single-reg))
  (inst movss y (ea-for-sf-stack x)))

(define-move-fun (store-single 2) (vop x y)
  ((single-reg) (single-stack))
  (inst movss (ea-for-sf-stack y) x))

(define-move-fun (load-double 2) (vop x y)
  ((double-stack) (double-reg))
  (inst movsd y (ea-for-df-stack x)))

(define-move-fun (store-double 2) (vop x y)
  ((double-reg) (double-stack))
  (inst movsd  (ea-for-df-stack y) x))

(eval-when (:compile-toplevel :execute)
  (setf *read-default-float-format* 'single-float))

;;;; complex float move functions

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

;;; X is source, Y is destination.
(define-move-fun (load-complex-single 2) (vop x y)
  ((complex-single-stack) (complex-single-reg))
  (let ((real-tn (complex-single-reg-real-tn y)))
    (inst movss real-tn (ea-for-csf-real-stack x)))
  (let ((imag-tn (complex-single-reg-imag-tn y)))
    (inst movss imag-tn (ea-for-csf-imag-stack x))))

(define-move-fun (store-complex-single 2) (vop x y)
  ((complex-single-reg) (complex-single-stack))
  (let ((real-tn (complex-single-reg-real-tn x))
	(imag-tn (complex-single-reg-imag-tn x)))
    (inst movss (ea-for-csf-real-stack y) real-tn)
    (inst movss (ea-for-csf-imag-stack y) imag-tn)))

(define-move-fun (load-complex-double 2) (vop x y)
  ((complex-double-stack) (complex-double-reg))
  (let ((real-tn (complex-double-reg-real-tn y)))
    (inst movsd real-tn (ea-for-cdf-real-stack x)))
  (let ((imag-tn (complex-double-reg-imag-tn y)))
    (inst movsd imag-tn (ea-for-cdf-imag-stack x))))

(define-move-fun (store-complex-double 2) (vop x y)
  ((complex-double-reg) (complex-double-stack))
  (let ((real-tn (complex-double-reg-real-tn x))
	(imag-tn (complex-double-reg-imag-tn x)))
    (inst movsd (ea-for-cdf-real-stack y) real-tn)
    (inst movsd (ea-for-cdf-imag-stack y) imag-tn)))


;;;; move VOPs

;;; float register to register moves
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
		      (inst movq y x))))
		(define-move-vop ,vop :move (,sc) (,sc)))))
  (frob single-move single-reg)
  (frob double-move double-reg))

;;; complex float register to register moves
(define-vop (complex-float-move)
  (:args (x :target y :load-if (not (location= x y))))
  (:results (y :load-if (not (location= x y))))
  (:note "complex float move")
  (:generator 0
     (unless (location= x y)
       ;; Note the complex-float-regs are aligned to every second
       ;; float register so there is not need to worry about overlap.
       ;; (It would be better to put the imagpart in the top half of the 
       ;; register, or something, but let's worry about that later)
       (let ((x-real (complex-single-reg-real-tn x))
	     (y-real (complex-single-reg-real-tn y)))
	 (inst movq y-real x-real))
       (let ((x-imag (complex-single-reg-imag-tn x))
	     (y-imag (complex-single-reg-imag-tn y)))
	 (inst movq y-imag x-imag)))))

(define-vop (complex-single-move complex-float-move)
  (:args (x :scs (complex-single-reg) :target y
	    :load-if (not (location= x y))))
  (:results (y :scs (complex-single-reg) :load-if (not (location= x y)))))
(define-move-vop complex-single-move :move
  (complex-single-reg) (complex-single-reg))

(define-vop (complex-double-move complex-float-move)
  (:args (x :scs (complex-double-reg)
	    :target y :load-if (not (location= x y))))
  (:results (y :scs (complex-double-reg) :load-if (not (location= x y)))))
(define-move-vop complex-double-move :move
  (complex-double-reg) (complex-double-reg))


;;; Move from float to a descriptor reg. allocating a new float
;;; object in the process.
(define-vop (move-from-single)
  (:args (x :scs (single-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:node-var node)
  (:note "float to pointer coercion")
  (:generator 13
     (with-fixed-allocation (y
			     single-float-widetag
			     single-float-size node)
       (inst movss (ea-for-sf-desc y) x))))
(define-move-vop move-from-single :move
  (single-reg) (descriptor-reg))

(define-vop (move-from-double)
  (:args (x :scs (double-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:node-var node)
  (:note "float to pointer coercion")
  (:generator 13
     (with-fixed-allocation (y
			     double-float-widetag
			     double-float-size
			     node)
       (inst movsd (ea-for-df-desc y) x))))
(define-move-vop move-from-double :move
  (double-reg) (descriptor-reg))

#+nil
(define-vop (move-from-fp-constant)
  (:args (x :scs (fp-constant)))
  (:results (y :scs (descriptor-reg)))
  (:generator 2
     (ecase (sb!c::constant-value (sb!c::tn-leaf x))
       (0f0 (load-symbol-value y *fp-constant-0f0*))
       (1f0 (load-symbol-value y *fp-constant-1f0*))
       (0d0 (load-symbol-value y *fp-constant-0d0*))
       (1d0 (load-symbol-value y *fp-constant-1d0*)))))
#+nil
(define-move-vop move-from-fp-constant :move
  (fp-constant) (descriptor-reg))

;;; Move from a descriptor to a float register.
(define-vop (move-to-single)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (single-reg)))
  (:note "pointer to float coercion")
  (:generator 2
    (inst movss y (ea-for-sf-desc x))))
(define-move-vop move-to-single :move (descriptor-reg) (single-reg))

(define-vop (move-to-double)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (double-reg)))
  (:note "pointer to float coercion")
  (:generator 2
    (inst movsd y (ea-for-df-desc x))))
(define-move-vop move-to-double :move (descriptor-reg) (double-reg))


;;; Move from complex float to a descriptor reg. allocating a new
;;; complex float object in the process.
(define-vop (move-from-complex-single)
  (:args (x :scs (complex-single-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:node-var node)
  (:note "complex float to pointer coercion")
  (:generator 13
     (with-fixed-allocation (y
			     complex-single-float-widetag
			     complex-single-float-size
			     node)
       (let ((real-tn (complex-single-reg-real-tn x)))
	 (inst movss (ea-for-csf-real-desc y) real-tn))
       (let ((imag-tn (complex-single-reg-imag-tn x)))
	 (inst movss (ea-for-csf-imag-desc y) imag-tn)))))
(define-move-vop move-from-complex-single :move
  (complex-single-reg) (descriptor-reg))

(define-vop (move-from-complex-double)
  (:args (x :scs (complex-double-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:node-var node)
  (:note "complex float to pointer coercion")
  (:generator 13
     (with-fixed-allocation (y
			     complex-double-float-widetag
			     complex-double-float-size
			     node)
       (let ((real-tn (complex-double-reg-real-tn x)))
	 (inst movsd (ea-for-cdf-real-desc y) real-tn))
       (let ((imag-tn (complex-double-reg-imag-tn x)))
	 (inst movsd (ea-for-cdf-imag-desc y) imag-tn)))))
(define-move-vop move-from-complex-double :move
  (complex-double-reg) (descriptor-reg))

;;; Move from a descriptor to a complex float register.
(macrolet ((frob (name sc format)
	     `(progn
		(define-vop (,name)
		  (:args (x :scs (descriptor-reg)))
		  (:results (y :scs (,sc)))
		  (:note "pointer to complex float coercion")
		  (:generator 2
		    (let ((real-tn (complex-double-reg-real-tn y)))
		      ,@(ecase
			 format
			 (:single
			  '((inst movss real-tn (ea-for-csf-real-desc x))))
			 (:double
			  '((inst movsd real-tn (ea-for-cdf-real-desc x))))))
		    (let ((imag-tn (complex-double-reg-imag-tn y)))
		      ,@(ecase
			 format
			 (:single
			  '((inst movss imag-tn (ea-for-csf-imag-desc x))))
			 (:double 
			  '((inst movsd imag-tn (ea-for-cdf-imag-desc x))))))))
		(define-move-vop ,name :move (descriptor-reg) (,sc)))))
  (frob move-to-complex-single complex-single-reg :single)
  (frob move-to-complex-double complex-double-reg :double))

;;;; the move argument vops
;;;;
;;;; Note these are also used to stuff fp numbers onto the c-call
;;;; stack so the order is different than the lisp-stack.

;;; the general MOVE-ARG VOP
(macrolet ((frob (name sc stack-sc format)
	     `(progn
		(define-vop (,name)
		  (:args (x :scs (,sc) :target y)
			 (fp :scs (any-reg)
			     :load-if (not (sc-is y ,sc))))
		  (:results (y))
		  (:note "float argument move")
		  (:generator ,(case format (:single 2) (:double 3) )
		    (sc-case y
		      (,sc
		       (unless (location= x y)
			 (inst movq y x)))
		      (,stack-sc
		       (if (= (tn-offset fp) esp-offset)
			   (let* ((offset (* (tn-offset y) n-word-bytes))
				  (ea (make-ea :dword :base fp :disp offset)))
			     ,@(ecase format
				      (:single '((inst movss ea x)))
				      (:double '((inst movsd ea x)))))
			   (let ((ea (make-ea
				      :dword :base fp
				      :disp (- (* (+ (tn-offset y)
						     ,(case format
							    (:single 1)
							    (:double 2) ))
						  n-word-bytes)))))
			     (with-tn@fp-top(x)
			       ,@(ecase format
				    (:single '((inst movss ea x)))
				    (:double '((inst movsd ea x)))))))))))
		(define-move-vop ,name :move-arg
		  (,sc descriptor-reg) (,sc)))))
  (frob move-single-float-arg single-reg single-stack :single)
  (frob move-double-float-arg double-reg double-stack :double))

;;;; complex float MOVE-ARG VOP
(macrolet ((frob (name sc stack-sc format)
	     `(progn
		(define-vop (,name)
		  (:args (x :scs (,sc) :target y)
			 (fp :scs (any-reg)
			     :load-if (not (sc-is y ,sc))))
		  (:results (y))
		  (:note "complex float argument move")
		  (:generator ,(ecase format (:single 2) (:double 3))
		    (sc-case y
		      (,sc
		       (unless (location= x y)
			 (let ((x-real (complex-double-reg-real-tn x))
			       (y-real (complex-double-reg-real-tn y)))
			   (inst movsd y-real x-real))
			 (let ((x-imag (complex-double-reg-imag-tn x))
			       (y-imag (complex-double-reg-imag-tn y)))
			   (inst movsd y-imag x-imag))))
		      (,stack-sc
		       (let ((real-tn (complex-double-reg-real-tn x)))
			 ,@(ecase format
				  (:single
				   '((inst movss
				      (ea-for-csf-real-stack y fp)
				      real-tn)))
				  (:double
				   '((inst movsd
				      (ea-for-cdf-real-stack y fp)
				      real-tn)))))
		       (let ((imag-tn (complex-double-reg-imag-tn x)))
			 ,@(ecase format
				  (:single
				   '((inst movss
				      (ea-for-csf-imag-stack y fp) imag-tn)))
				  (:double
				   '((inst movsd
				      (ea-for-cdf-imag-stack y fp) imag-tn)))))))))
		(define-move-vop ,name :move-arg
		  (,sc descriptor-reg) (,sc)))))
  (frob move-complex-single-float-arg
	complex-single-reg complex-single-stack :single)
  (frob move-complex-double-float-arg
	complex-double-reg complex-double-stack :double))

(define-move-vop move-arg :move-arg
  (single-reg double-reg
   complex-single-reg complex-double-reg)
  (descriptor-reg))


;;;; arithmetic VOPs

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

(macrolet ((generate (movinst opinst commutative)
	     `(progn
		(cond
		  ((location= x r)
		   (inst ,opinst x y))
		  ((and ,commutative (location= y r))
		   (inst ,opinst y x))
		  ((not (location= r y))
		   (inst ,movinst r x)
		   (inst ,opinst r y))
		  (t
		   (inst ,movinst tmp x)
		   (inst ,opinst tmp y)
		   (inst ,movinst r tmp)))))
	   (frob (op sinst sname scost dinst dname dcost commutative)
	     `(progn
		(define-vop (,sname single-float-op)
		    (:translate ,op)
		  (:temporary (:sc single-reg) tmp)
		  (:generator ,scost
  	            (generate movss ,sinst ,commutative)))
		(define-vop (,dname double-float-op)
		  (:translate ,op)
		  (:temporary (:sc single-reg) tmp)
		  (:generator ,dcost
                    (generate movsd ,dinst ,commutative))))))
  (frob + addss +/single-float 2 addsd +/double-float 2 t)
  (frob - subss -/single-float 2 subsd -/double-float 2 nil)
  (frob * mulss */single-float 4 mulsd */double-float 5 t)
  (frob / divss //single-float 12 divsd //double-float 19 nil))


(macrolet ((frob ((name translate sc type) &body body)
	     `(define-vop (,name)
		  (:args (x :scs (,sc)))
		(:results (y :scs (,sc)))
		(:translate ,translate)
		(:policy :fast-safe)
		(:arg-types ,type)
		(:result-types ,type)
		(:temporary (:sc any-reg) hex8)
		(:temporary
		 (:sc ,sc) xmm)
		(:note "inline float arithmetic")
		(:vop-var vop)
		(:save-p :compute-only)
		(:generator 1
			    (note-this-location vop :internal-error)
			    ;; we should be able to do this better.  what we 
			    ;; really would like to do is use the target as the
			    ;; temp whenever it's not also the source
			    (unless (location= x y)
			      (inst movq y x))
			    ,@body))))
  (frob (%negate/double-float %negate double-reg double-float)
	(inst lea hex8 (make-ea :qword :disp 1))
	(inst ror hex8 1)		; #x8000000000000000
	(inst movd xmm hex8)
	(inst xorpd y xmm))
  (frob (%negate/single-float %negate single-reg single-float)
	(inst lea hex8 (make-ea :qword :disp 1))
	(inst rol hex8 31)
	(inst movd xmm hex8)
	(inst xorps y xmm))
  (frob (abs/double-float abs  double-reg double-float)
	(inst mov hex8 -1)
	(inst shr hex8 1)
	(inst movd xmm hex8)
	(inst andpd y xmm))
  (frob (abs/single-float abs  single-reg single-float)
	(inst mov hex8 -1)
	(inst shr hex8 33)
	(inst movd xmm hex8)
	(inst andps y xmm)))

;;;; comparison

(define-vop (float-compare)
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:vop-var vop)
  (:save-p :compute-only)
  (:note "inline float comparison"))

;;; comiss and comisd can cope with one or other arg in memory: we
;;; could (should, indeed) extend these to cope with descriptor args
;;; and stack args

(define-vop (single-float-compare float-compare)
  (:args (x :scs (single-reg)) (y :scs (single-reg)))
  (:conditional)
  (:arg-types single-float single-float))
(define-vop (double-float-compare float-compare)
  (:args (x :scs (double-reg)) (y :scs (double-reg)))
  (:conditional)
  (:arg-types double-float double-float))

(define-vop (=/single-float single-float-compare)
    (:translate =)
  (:info target not-p)
  (:vop-var vop)
  (:generator 3
    (note-this-location vop :internal-error)
    (inst comiss x y)
    ;; if PF&CF, there was a NaN involved => not equal
    ;; otherwise, ZF => equal
    (cond (not-p
	   (inst jmp :p target)
	   (inst jmp :ne target))
	  (t
	   (let ((not-lab (gen-label)))
	     (inst jmp :p not-lab)
	     (inst jmp :e target)
	     (emit-label not-lab))))))

(define-vop (=/double-float double-float-compare)
    (:translate =)
  (:info target not-p)
  (:vop-var vop)
  (:generator 3
    (note-this-location vop :internal-error)
    (inst comisd x y)
    (cond (not-p
	   (inst jmp :p target)
	   (inst jmp :ne target))
	  (t
	   (let ((not-lab (gen-label)))
	     (inst jmp :p not-lab)
	     (inst jmp :e target)
	     (emit-label not-lab))))))

;; XXX all of these probably have bad NaN behaviour
(define-vop (<double-float double-float-compare)
  (:translate <)
  (:info target not-p)
  (:generator 2
    (inst comisd x y)
    (inst jmp (if not-p :nc :c) target)))

(define-vop (<single-float single-float-compare)
  (:translate <)
  (:info target not-p)
  (:generator 2
    (inst comiss x y)
    (inst jmp (if not-p :nc :c) target)))

(define-vop (>double-float double-float-compare)
  (:translate >)
  (:info target not-p)
  (:generator 2
    (inst comisd x y)
    (inst jmp (if not-p :na :a) target)))

(define-vop (>single-float single-float-compare)
  (:translate >)
  (:info target not-p)
  (:generator 2
    (inst comiss x y)
    (inst jmp (if not-p :na :a) target)))



;;;; conversion

(macrolet ((frob (name translate inst to-sc to-type)
	     `(define-vop (,name)
		(:args (x :scs (signed-stack signed-reg) :target temp))
		(:temporary (:sc signed-stack) temp)
		(:results (y :scs (,to-sc)))
		(:arg-types signed-num)
		(:result-types ,to-type)
		(:policy :fast-safe)
		(:note "inline float coercion")
		(:translate ,translate)
		(:vop-var vop)
		(:save-p :compute-only)
		(:generator 5
		  (sc-case x
		    (signed-reg
		     (inst mov temp x)
		     (note-this-location vop :internal-error)
		     (inst ,inst y temp))
		    (signed-stack
		     (note-this-location vop :internal-error)
		     (inst ,inst y x)))))))
  (frob %single-float/signed %single-float cvtsi2ss single-reg single-float)
  (frob %double-float/signed %double-float cvtsi2sd double-reg double-float))

#+nil
(macrolet ((frob (name translate inst to-sc to-type)
	     `(define-vop (,name)
		(:args (x :scs (unsigned-reg)))
		(:results (y :scs (,to-sc)))
		(:arg-types unsigned-num)
		(:result-types ,to-type)
		(:policy :fast-safe)
		(:note "inline float coercion")
		(:translate ,translate)
		(:vop-var vop)
		(:save-p :compute-only)
		(:generator 6
		  (inst ,inst y x)))))
  (frob %single-float/unsigned %single-float cvtsi2ss single-reg single-float)
  (frob %double-float/unsigned %double-float cvtsi2sd double-reg double-float))

(macrolet ((frob (name translate inst from-sc from-type to-sc to-type)
	     `(define-vop (,name)
	       (:args (x :scs (,from-sc) :target y))
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
  (frob %single-float/double-float %single-float cvtsd2ss double-reg
	double-float single-reg single-float)

  (frob %double-float/single-float %double-float cvtss2sd 
	single-reg single-float double-reg double-float))

(macrolet ((frob (trans inst from-sc from-type round-p)
             (declare (ignore round-p))
	     `(define-vop (,(symbolicate trans "/" from-type))
	       (:args (x :scs (,from-sc)))
	       (:temporary (:sc any-reg) temp-reg)
	       (:results (y :scs (signed-reg)))
	       (:arg-types ,from-type)
	       (:result-types signed-num)
	       (:translate ,trans)
	       (:policy :fast-safe)
	       (:note "inline float truncate")
	       (:vop-var vop)
	       (:save-p :compute-only)
	       (:generator 5
		 (sc-case y
			  (signed-stack
			   (inst ,inst temp-reg x)
			   (move y temp-reg))
			  (signed-reg
			   (inst ,inst y x)
			   ))))))
  (frob %unary-truncate cvttss2si single-reg single-float nil)
  (frob %unary-truncate cvttsd2si double-reg double-float nil)

  (frob %unary-round cvtss2si single-reg single-float t)
  (frob %unary-round cvtsd2si double-reg double-float t))

#+nil ;; will we need this?
(macrolet ((frob (trans from-sc from-type round-p)
	     `(define-vop (,(symbolicate trans "/" from-type "=>UNSIGNED"))
	       (:args (x :scs (,from-sc) :target fr0))
	       (:temporary (:sc double-reg :offset fr0-offset
			    :from :argument :to :result) fr0)
	       ,@(unless round-p
		  '((:temporary (:sc unsigned-stack) stack-temp)
		    (:temporary (:sc unsigned-stack) scw)
		    (:temporary (:sc any-reg) rcw)))
	       (:results (y :scs (unsigned-reg)))
	       (:arg-types ,from-type)
	       (:result-types unsigned-num)
	       (:translate ,trans)
	       (:policy :fast-safe)
	       (:note "inline float truncate")
	       (:vop-var vop)
	       (:save-p :compute-only)
	       (:generator 5
		,@(unless round-p
		   '((note-this-location vop :internal-error)
		     ;; Catch any pending FPE exceptions.
		     (inst wait)))
		;; Normal mode (for now) is "round to best".
		(unless (zerop (tn-offset x))
		  (copy-fp-reg-to-fr0 x))
		,@(unless round-p
		   '((inst fnstcw scw)	; save current control word
		     (move rcw scw)	; into 16-bit register
		     (inst or rcw (ash #b11 10)) ; CHOP
		     (move stack-temp rcw)
		     (inst fldcw stack-temp)))
		(inst sub rsp-tn 8)
		(inst fistpl (make-ea :dword :base rsp-tn))
		(inst pop y)
		(inst fld fr0) ; copy fr0 to at least restore stack.
		(inst add rsp-tn 8)
		,@(unless round-p
		   '((inst fldcw scw)))))))
  (frob %unary-truncate single-reg single-float nil)
  (frob %unary-truncate double-reg double-float nil)
  (frob %unary-round single-reg single-float t)
  (frob %unary-round double-reg double-float t))

(define-vop (make-single-float)
  (:args (bits :scs (signed-reg) :target res
	       :load-if (not (or (and (sc-is bits signed-stack)
				      (sc-is res single-reg))
				 (and (sc-is bits signed-stack)
				      (sc-is res single-stack)
				      (location= bits res))))))
  (:results (res :scs (single-reg single-stack)))
 ; (:temporary (:sc signed-stack) stack-temp)
  (:arg-types signed-num)
  (:result-types single-float)
  (:translate make-single-float)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 4
    (sc-case res
       (single-stack
	(sc-case bits
	  (signed-reg
	   (inst mov res bits))
	  (signed-stack
	   (aver (location= bits res)))))
       (single-reg
	(sc-case bits
	  (signed-reg
	   (inst movd res bits))
	  (signed-stack
	   (inst movd res bits)))))))

(define-vop (make-double-float)
  (:args (hi-bits :scs (signed-reg))
	 (lo-bits :scs (unsigned-reg)))
  (:results (res :scs (double-reg)))
  (:temporary (:sc unsigned-reg) temp)
  (:arg-types signed-num unsigned-num)
  (:result-types double-float)
  (:translate make-double-float)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (move temp hi-bits)
    (inst shl temp 32)
    (inst or temp lo-bits)
    (inst movd res temp)))

(define-vop (single-float-bits)
  (:args (float :scs (single-reg descriptor-reg)
		:load-if (not (sc-is float single-stack))))
  (:results (bits :scs (signed-reg)))
  (:temporary (:sc signed-stack :from :argument :to :result) stack-temp)
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
	  (inst movss stack-temp float)
	  (move bits stack-temp))
	 (single-stack
	  (move bits float))
	 (descriptor-reg
	  (loadw
	   bits float single-float-value-slot
	   other-pointer-lowtag))))
      (signed-stack
       (sc-case float
	 (single-reg
	  (inst movss bits float)))))
    ;; Sign-extend
    (inst shl bits 32)
    (inst sar bits 32)))

(define-vop (double-float-high-bits)
  (:args (float :scs (double-reg descriptor-reg)
		:load-if (not (sc-is float double-stack))))
  (:results (hi-bits :scs (signed-reg)))
  (:temporary (:sc signed-stack :from :argument :to :result) temp)
  (:arg-types double-float)
  (:result-types signed-num)
  (:translate double-float-high-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
     (sc-case float
       (double-reg
	(inst movsd temp float)
	(move hi-bits temp))
       (double-stack
	(loadw hi-bits ebp-tn (- (tn-offset float))))
       (descriptor-reg
	(loadw hi-bits float double-float-value-slot
	       other-pointer-lowtag)))
     (inst sar hi-bits 32)))

(define-vop (double-float-low-bits)
  (:args (float :scs (double-reg descriptor-reg)
		:load-if (not (sc-is float double-stack))))
  (:results (lo-bits :scs (unsigned-reg)))
  (:temporary (:sc signed-stack :from :argument :to :result) temp)
  (:arg-types double-float)
  (:result-types unsigned-num)
  (:translate double-float-low-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
     (sc-case float
       (double-reg
	(inst movsd temp float)
	(move lo-bits temp))
       (double-stack
	(loadw lo-bits ebp-tn (- (tn-offset float))))
       (descriptor-reg
	(loadw lo-bits float double-float-value-slot
	       other-pointer-lowtag)))
     (inst shl lo-bits 32)
     (inst shr lo-bits 32)))


;;;; float mode hackery

(sb!xc:deftype float-modes () '(unsigned-byte 64)) ; really only 16
(defknown floating-point-modes () float-modes (flushable))
(defknown ((setf floating-point-modes)) (float-modes)
  float-modes)

(define-vop (floating-point-modes)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate floating-point-modes)
  (:policy :fast-safe)
  (:temporary (:sc unsigned-stack :from :argument :to :result) temp)
  (:generator 8
   (inst stmxcsr temp)
   (move res temp)
   ;; Extract status from bytes 0-5 to bytes 16-21
   (inst and temp (1- (expt 2 6)))
   (inst shl temp 16)
   ;; Extract mask from bytes 7-12 to bytes 0-5
   (inst shr res 7)
   (inst and res (1- (expt 2 6)))
   ;; Flip the bits to convert from "1 means exception masked" to 
   ;; "1 means exception enabled".
   (inst xor res (1- (expt 2 6)))
   (inst or res temp)))

(define-vop (set-floating-point-modes)
  (:args (new :scs (unsigned-reg) :to :result :target res))
  (:results (res :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:result-types unsigned-num)
  (:translate (setf floating-point-modes))
  (:policy :fast-safe)
  (:temporary (:sc unsigned-reg :from :argument :to :result) temp1)
  (:temporary (:sc unsigned-stack :from :argument :to :result) temp2)
  (:generator 3
   (move res new)	      
   (inst stmxcsr temp2)
   ;; Clear status + masks
   (inst and temp2 (lognot (logior (1- (expt 2 6))
				   (ash (1- (expt 2 6)) 7))))
   ;; Replace current status
   (move temp1 new)
   (inst shr temp1 16)
   (inst and temp1 (1- (expt 2 6)))
   (inst or temp2 temp1)
   ;; Replace exception masks
   (move temp1 new)
   (inst and temp1 (1- (expt 2 6)))
   (inst xor temp1 (1- (expt 2 6)))
   (inst shl temp1 7)
   (inst or temp2 temp1)
   (inst ldmxcsr temp2)))


;;;; complex float VOPs

(define-vop (make-complex-single-float)
  (:translate complex)
  (:args (real :scs (single-reg) :to :result :target r
	       :load-if (not (location= real r)))
	 (imag :scs (single-reg) :to :save))
  (:arg-types single-float single-float)
  (:results (r :scs (complex-single-reg) :from (:argument 0)
	       :load-if (not (sc-is r complex-single-stack))))
  (:result-types complex-single-float)
  (:note "inline complex single-float creation")
  (:policy :fast-safe)
  (:generator 5
    (sc-case r
      (complex-single-reg
       (let ((r-real (complex-single-reg-real-tn r)))
	 (unless (location= real r-real)
	   (inst movss r-real real)))
       (let ((r-imag (complex-single-reg-imag-tn r)))
	 (unless (location= imag r-imag)
	   (inst movss r-imag imag))))
      (complex-single-stack
       (inst movss (ea-for-csf-real-stack r) real)
       (inst movss (ea-for-csf-imag-stack r) imag)))))

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
  (:generator 5
    (sc-case r
      (complex-double-reg
       (let ((r-real (complex-double-reg-real-tn r)))
	 (unless (location= real r-real)
	   (inst movsd r-real real)))
       (let ((r-imag (complex-double-reg-imag-tn r)))
	 (unless (location= imag r-imag)
	   (inst movsd r-imag imag))))
      (complex-double-stack
       (inst movsd (ea-for-cdf-real-stack r) real)
       (inst movsd (ea-for-cdf-imag-stack r) imag)))))

(define-vop (complex-float-value)
  (:args (x :target r))
  (:results (r))
  (:variant-vars offset)
  (:policy :fast-safe)
  (:generator 3
    (cond ((sc-is x complex-single-reg complex-double-reg)
	   (let ((value-tn
		  (make-random-tn :kind :normal
				  :sc (sc-or-lose 'double-reg)
				  :offset (+ offset (tn-offset x)))))
	     (unless (location= value-tn r)
	       (if (sc-is x complex-single-reg)
		   (inst movss r value-tn)
		   (inst movsd r value-tn)))))
	  ((sc-is r single-reg)
	   (let ((ea (sc-case x
		       (complex-single-stack
			(ecase offset
			  (0 (ea-for-csf-real-stack x))
			  (1 (ea-for-csf-imag-stack x))))
		       (descriptor-reg
			(ecase offset
			  (0 (ea-for-csf-real-desc x))
			  (1 (ea-for-csf-imag-desc x)))))))
	     (inst movss r ea)))
	  ((sc-is r double-reg)
	   (let ((ea (sc-case x
		       (complex-double-stack
			(ecase offset
			  (0 (ea-for-cdf-real-stack x))
			  (1 (ea-for-cdf-imag-stack x))))
		       (descriptor-reg
			(ecase offset
			  (0 (ea-for-cdf-real-desc x))
			  (1 (ea-for-cdf-imag-desc x)))))))
	     (inst movsd r ea)))
	  (t (error "COMPLEX-FLOAT-VALUE VOP failure")))))

(define-vop (realpart/complex-single-float complex-float-value)
  (:translate realpart)
  (:args (x :scs (complex-single-reg complex-single-stack descriptor-reg)
	    :target r))
  (:arg-types complex-single-float)
  (:results (r :scs (single-reg)))
  (:result-types single-float)
  (:note "complex float realpart")
  (:variant 0))

(define-vop (realpart/complex-double-float complex-float-value)
  (:translate realpart)
  (:args (x :scs (complex-double-reg complex-double-stack descriptor-reg)
	    :target r))
  (:arg-types complex-double-float)
  (:results (r :scs (double-reg)))
  (:result-types double-float)
  (:note "complex float realpart")
  (:variant 0))

(define-vop (imagpart/complex-single-float complex-float-value)
  (:translate imagpart)
  (:args (x :scs (complex-single-reg complex-single-stack descriptor-reg)
	    :target r))
  (:arg-types complex-single-float)
  (:results (r :scs (single-reg)))
  (:result-types single-float)
  (:note "complex float imagpart")
  (:variant 1))

(define-vop (imagpart/complex-double-float complex-float-value)
  (:translate imagpart)
  (:args (x :scs (complex-double-reg complex-double-stack descriptor-reg)
	    :target r))
  (:arg-types complex-double-float)
  (:results (r :scs (double-reg)))
  (:result-types double-float)
  (:note "complex float imagpart")
  (:variant 1))


;;; hack dummy VOPs to bias the representation selection of their
;;; arguments towards a FP register, which can help avoid consing at
;;; inappropriate locations
(defknown double-float-reg-bias (double-float) (values))
(define-vop (double-float-reg-bias)
  (:translate double-float-reg-bias)
  (:args (x :scs (double-reg double-stack) :load-if nil))
  (:arg-types double-float)
  (:policy :fast-safe)
  (:note "inline dummy FP register bias")
  (:ignore x)
  (:generator 0))
(defknown single-float-reg-bias (single-float) (values))
(define-vop (single-float-reg-bias)
  (:translate single-float-reg-bias)
  (:args (x :scs (single-reg single-stack) :load-if nil))
  (:arg-types single-float)
  (:policy :fast-safe)
  (:note "inline dummy FP register bias")
  (:ignore x)
  (:generator 0))
