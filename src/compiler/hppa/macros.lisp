;;;; various useful macros for generating HPPA code

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.
(in-package "SB!VM")


;;; Instruction-like macros.

(defmacro move (src dst)
  "Move SRC into DST unless they are location=."
  (once-only ((src src) (dst dst))
    `(unless (location= ,src ,dst)
       (inst move ,src ,dst))))

(defmacro loadw (result base &optional (offset 0) (lowtag 0))
  (once-only ((result result) (base base))
    `(inst ldw (- (ash ,offset word-shift) ,lowtag) ,base ,result)))

(defmacro storew (value base &optional (offset 0) (lowtag 0))
  (once-only ((value value) (base base) (offset offset) (lowtag lowtag))
    `(inst stw ,value (- (ash ,offset word-shift) ,lowtag) ,base)))

(defmacro load-symbol (reg symbol)
  (once-only ((reg reg) (symbol symbol))
    `(inst addi (static-symbol-offset ,symbol) null-tn ,reg)))

(defmacro load-symbol-value (reg symbol)
  `(inst ldw
	 (+ (static-symbol-offset ',symbol)
	    (ash symbol-value-slot word-shift)
	    (- other-pointer-lowtag))
	 null-tn
	 ,reg))

(defmacro store-symbol-value (reg symbol)
  `(inst stw ,reg (+ (static-symbol-offset ',symbol)
		     (ash symbol-value-slot word-shift)
		     (- other-pointer-lowtag))
	 null-tn))

(defmacro load-type (target source &optional (offset 0))
  "Loads the type bits of a pointer into target independent of
   byte-ordering issues."
  (ecase *backend-byte-order*
    (:little-endian
     `(inst ldb ,offset ,source ,target))
    (:big-endian
     `(inst ldb (+ ,offset 3) ,source ,target))))

;;; Macros to handle the fact that we cannot use the machine native call and
;;; return instructions. 

(defmacro lisp-jump (function)
  "Jump to the lisp function FUNCTION.  LIP is an interior-reg temporary."
  `(progn
     (inst addi
	   (- (ash simple-fun-code-offset word-shift) fun-pointer-lowtag)
	   ,function
	   lip-tn)
     (inst bv lip-tn)
     (move ,function code-tn)))

(defmacro lisp-return (return-pc &key (offset 0) (frob-code t))
  "Return to RETURN-PC."
  `(progn
     (inst addi (- (* (1+ ,offset) n-word-bytes) other-pointer-lowtag)
	   ,return-pc lip-tn)
     (inst bv lip-tn ,@(unless frob-code '(:nullify t)))
     ,@(when frob-code
	 `((move ,return-pc code-tn)))))

(defmacro emit-return-pc (label)
  "Emit a return-pc header word.  LABEL is the label to use for this
   return-pc."
  `(progn
     (align n-lowtag-bits)
     (emit-label ,label)
     (inst lra-header-word)))


;;;; Stack TN's

;;; Move a stack TN to a register and vice-versa.
(defmacro load-stack-tn (reg stack)
  `(let ((reg ,reg)
	 (stack ,stack))
     (let ((offset (tn-offset stack)))
       (sc-case stack
	 ((control-stack)
	  (loadw reg cfp-tn offset))))))
(defmacro store-stack-tn (stack reg)
  `(let ((stack ,stack)
	 (reg ,reg))
     (let ((offset (tn-offset stack)))
       (sc-case stack
	 ((control-stack)
	  (storew reg cfp-tn offset))))))

(defmacro maybe-load-stack-tn (reg reg-or-stack)
  "Move the TN Reg-Or-Stack into Reg if it isn't already there."
  (once-only ((n-reg reg)
	      (n-stack reg-or-stack))
    `(sc-case ,n-reg
       ((any-reg descriptor-reg)
	(sc-case ,n-stack
	  ((any-reg descriptor-reg)
	   (move ,n-stack ,n-reg))
	  ((control-stack)
	   (loadw ,n-reg cfp-tn (tn-offset ,n-stack))))))))


;;;; Storage allocation:

(defmacro with-fixed-allocation ((result-tn temp-tn type-code size)
				 &body body)
  "Do stuff to allocate an other-pointer object of fixed Size with a single
  word header having the specified Type-Code.  The result is placed in
  Result-TN, and Temp-TN is a non-descriptor temp (which may be randomly used
  by the body.)  The body is placed inside the PSEUDO-ATOMIC, and presumably
  initializes the object."
  (once-only ((result-tn result-tn) (temp-tn temp-tn)
	      (type-code type-code) (size size))
    `(pseudo-atomic (:extra (pad-data-block ,size))
       (inst move alloc-tn ,result-tn)
       (inst dep other-pointer-lowtag 31 3 ,result-tn)
       (inst li (logior (ash (1- ,size) n-widetag-bits) ,type-code) ,temp-tn)
       (storew ,temp-tn ,result-tn 0 other-pointer-lowtag)
       ,@body)))


;;;; Error Code
(eval-when (compile load eval)
  (defun emit-error-break (vop kind code values)
    (let ((vector (gensym)))
      `((let ((vop ,vop))
	  (when vop
	    (note-this-location vop :internal-error)))
	(inst break ,kind)
	(with-adjustable-vector (,vector)
	  (write-var-integer (error-number-or-lose ',code) ,vector)
	  ,@(mapcar #'(lambda (tn)
			`(let ((tn ,tn))
			   (write-var-integer (make-sc-offset (sc-number
							       (tn-sc tn))
							      (tn-offset tn))
					      ,vector)))
		    values)
	  (inst byte (length ,vector))
	  (dotimes (i (length ,vector))
	    (inst byte (aref ,vector i))))
	(align word-shift)))))

(defmacro error-call (vop error-code &rest values)
  "Cause an error.  ERROR-CODE is the error to cause."
  (cons 'progn
	(emit-error-break vop error-trap error-code values)))


(defmacro cerror-call (vop label error-code &rest values)
  "Cause a continuable error.  If the error is continued, execution resumes at
  LABEL."
  `(progn
     (inst b ,label)
     ,@(emit-error-break vop cerror-trap error-code values)))

(defmacro generate-error-code (vop error-code &rest values)
  "Generate-Error-Code Error-code Value*
  Emit code for an error with the specified Error-Code and context Values."
  `(assemble (*elsewhere*)
     (let ((start-lab (gen-label)))
       (emit-label start-lab)
       (error-call ,vop ,error-code ,@values)
       start-lab)))

(defmacro generate-cerror-code (vop error-code &rest values)
  "Generate-CError-Code Error-code Value*
  Emit code for a continuable error with the specified Error-Code and
  context Values.  If the error is continued, execution resumes after
  the GENERATE-CERROR-CODE form."
  (let ((continue (gensym "CONTINUE-LABEL-"))
	(error (gensym "ERROR-LABEL-")))
    `(let ((,continue (gen-label)))
       (emit-label ,continue)
       (assemble (*elsewhere*)
	 (let ((,error (gen-label)))
	   (emit-label ,error)
	   (cerror-call ,vop ,continue ,error-code ,@values)
	   ,error)))))



;;; PSEUDO-ATOMIC -- Handy macro for making sequences look atomic.
;;;
(defmacro pseudo-atomic ((&key (extra 0)) &rest forms)
  (let ((n-extra (gensym)))
    `(let ((,n-extra ,extra))
       (inst addi 4 alloc-tn alloc-tn)
       ,@forms
       (inst addit (- ,n-extra 4) alloc-tn alloc-tn :od))))



;;;; Indexed references:

(deftype load/store-index (scale lowtag min-offset
				 &optional (max-offset min-offset))
  `(integer ,(- (truncate (+ (ash 1 14)
			     (* min-offset n-word-bytes)
			     (- lowtag))
			  scale))
	    ,(truncate (- (+ (1- (ash 1 14)) lowtag)
			  (* max-offset n-word-bytes))
		       scale)))

(defmacro define-full-reffer (name type offset lowtag scs el-type
				   &optional translate)
  `(progn
     (define-vop (,name)
       ,@(when translate
	   `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg) :to (:eval 0))
	      (index :scs (any-reg) :target temp))
       (:arg-types ,type tagged-num)
       (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) temp)
       (:results (value :scs ,scs))
       (:result-types ,el-type)
       (:generator 5
	 (inst addi (- (* ,offset n-word-bytes) ,lowtag) index temp)
	 (inst ldwx temp object value)))
     (define-vop (,(symbolicate name "-C"))
       ,@(when translate
	   `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg)))
       (:info index)
       (:arg-types ,type
		   (:constant (load/store-index ,n-word-bytes ,(eval lowtag)
						,(eval offset))))
       (:results (value :scs ,scs))
       (:result-types ,el-type)
       (:generator 4
	 (inst ldw (- (* (+ ,offset index) n-word-bytes) ,lowtag)
	       object value)))))

(defmacro define-full-setter (name type offset lowtag scs el-type
				   &optional translate)
  `(progn
     (define-vop (,name)
       ,@(when translate
	   `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
	      (index :scs (any-reg))
	      (value :scs ,scs :target result))
       (:arg-types ,type tagged-num ,el-type)
       (:temporary (:scs (interior-reg)) lip)
       (:results (result :scs ,scs))
       (:result-types ,el-type)
       (:generator 2
	 (inst add object index lip)
	 (inst stw value (- (* ,offset n-word-bytes) ,lowtag) lip)
	 (move value result)))
     (define-vop (,(symbolicate name "-C"))
       ,@(when translate
	   `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
	      (value :scs ,scs))
       (:info index)
       (:arg-types ,type
		   (:constant (load/store-index ,n-word-bytes ,(eval lowtag)
						,(eval offset)))
		   ,el-type)
       (:results (result :scs ,scs))
       (:result-types ,el-type)
       (:generator 1
	 (inst stw value (- (* (+ ,offset index) n-word-bytes) ,lowtag) object)
	 (move value result)))))


(defmacro define-partial-reffer (name type size signed offset lowtag scs
				      el-type &optional translate)
  (let ((scale (ecase size (:byte 1) (:short 2))))
    `(progn
       (define-vop (,name)
	 ,@(when translate
	     `((:translate ,translate)))
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg) :to (:eval 0))
		(index :scs (unsigned-reg)))
	 (:arg-types ,type positive-fixnum)
	 (:results (value :scs ,scs))
	 (:result-types ,el-type)
	 (:temporary (:scs (interior-reg)) lip)
	 (:generator 5
	   (inst ,(ecase size (:byte 'add) (:short 'sh1add))
		 index object lip)
	   (inst ,(ecase size (:byte 'ldb) (:short 'ldh))
		 (- (* ,offset n-word-bytes) ,lowtag) lip value)
	   ,@(when signed
	       `((inst extrs value 31 ,(* scale n-byte-bits) value)))))
       (define-vop (,(symbolicate name "-C"))
	 ,@(when translate
	     `((:translate ,translate)))
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg)))
	 (:info index)
	 (:arg-types ,type
		     (:constant (load/store-index ,scale
						  ,(eval lowtag)
						  ,(eval offset))))
	 (:results (value :scs ,scs))
	 (:result-types ,el-type)
	 (:generator 5
	   (inst ,(ecase size (:byte 'ldb) (:short 'ldh))
		 (- (+ (* ,offset n-word-bytes) (* index ,scale)) ,lowtag)
		 object value)
	   ,@(when signed
	       `((inst extrs value 31 ,(* scale n-byte-bits) value))))))))

(defmacro define-partial-setter (name type size offset lowtag scs el-type
				      &optional translate)
  (let ((scale (ecase size (:byte 1) (:short 2))))
    `(progn
       (define-vop (,name)
	 ,@(when translate
	     `((:translate ,translate)))
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg))
		(index :scs (unsigned-reg))
		(value :scs ,scs :target result))
	 (:arg-types ,type positive-fixnum ,el-type)
	 (:temporary (:scs (interior-reg)) lip)
	 (:results (result :scs ,scs))
	 (:result-types ,el-type)
	 (:generator 5
	   (inst ,(ecase size (:byte 'add) (:short 'sh1add))
		 index object lip)
	   (inst ,(ecase size (:byte 'stb) (:short 'sth))
		 value (- (* ,offset n-word-bytes) ,lowtag) lip)
	   (move value result)))
       (define-vop (,(symbolicate name "-C"))
	 ,@(when translate
	     `((:translate ,translate)))
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg))
		(value :scs ,scs :target result))
	 (:info index)
	 (:arg-types ,type
		     (:constant (load/store-index ,scale
						  ,(eval lowtag)
						  ,(eval offset)))
		     ,el-type)
	 (:results (result :scs ,scs))
	 (:result-types ,el-type)
	 (:generator 5
	   (inst ,(ecase size (:byte 'stb) (:short 'sth))
		 value
		 (- (+ (* ,offset n-word-bytes) (* index ,scale)) ,lowtag)
		 object)
	   (move value result))))))

