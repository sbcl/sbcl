;;; routines for call-out to C.
;;;
;;; Written by William Lott.
;;;
(in-package "SB!VM")

;;; Return the number of bytes needed for the current non-descriptor stack
;;; frame.  Non-descriptor stack frames must be multiples of 16 bytes under
;;; the PPC SVr4 ABI (though the EABI may be less restrictive.)  Two words
;;; are reserved for the stack backlink and saved LR (see SB!VM::NUMBER-STACK-
;;; DISPLACEMENT.)
;;;

(defconstant +stack-alignment-bytes+
  ;; Duh.  PPC Linux (and VxWorks) adhere to the EABI.
  #!-darwin 7
  ;; But Darwin doesn't
  #!+darwin 15)

(defun my-make-wired-tn (prim-type-name sc-name offset)
  (make-wired-tn (primitive-type-or-lose prim-type-name)
		 (sc-number-or-lose sc-name)
		 offset))

(defstruct arg-state
  (gpr-args 0)
  (fpr-args 0)
  ;; SVR4 [a]abi wants two words on stack (callee saved lr,
  ;; backpointer).
  #!-darwin (stack-frame-size 2)
  ;; PowerOpen ABI wants 8 words on the stack corresponding to GPR3-10
  ;; in addition to the 6 words of link area (see number-stack-displacement)
  #!+darwin (stack-frame-size (+ 8 6)))

(defun int-arg (state prim-type reg-sc stack-sc)
  (let ((reg-args (arg-state-gpr-args state)))
    (cond ((< reg-args 8)
	   (setf (arg-state-gpr-args state) (1+ reg-args))
	   (my-make-wired-tn prim-type reg-sc (+ reg-args nl0-offset)))
	  (t
	   (let ((frame-size (arg-state-stack-frame-size state)))
	     (setf (arg-state-stack-frame-size state) (1+ frame-size))
	     (my-make-wired-tn prim-type stack-sc frame-size))))))

(define-alien-type-method (integer :arg-tn) (type state)
  (if (alien-integer-type-signed type)
      (int-arg state 'signed-byte-32 'signed-reg 'signed-stack)
      (int-arg state 'unsigned-byte-32 'unsigned-reg 'unsigned-stack)))

(define-alien-type-method (system-area-pointer :arg-tn) (type state)
  (declare (ignore type))
  (int-arg state 'system-area-pointer 'sap-reg 'sap-stack))

;;; If a single-float arg has to go on the stack, it's promoted to
;;; double.  That way, C programs can get subtle rounding errors when
;;; unrelated arguments are introduced.

#!-darwin
(define-alien-type-method (single-float :arg-tn) (type state)
  (declare (ignore type))
  (let* ((fprs (arg-state-fpr-args state)))
    (cond ((< fprs 8)
	   (incf (arg-state-fpr-args state))
	   ;; Assign outgoing FPRs starting at FP1
	   (my-make-wired-tn 'single-float 'single-reg (1+ fprs)))
	  (t
	   (let* ((stack-offset (arg-state-stack-frame-size state)))
	     (if (oddp stack-offset)
	       (incf stack-offset))
	     (setf (arg-state-stack-frame-size state) (+ stack-offset 2))
	     (my-make-wired-tn 'double-float 'double-stack stack-offset))))))

#!+darwin
(define-alien-type-method (single-float :arg-tn) (type state)
  (declare (ignore type))
  (let* ((fprs (arg-state-fpr-args state))
	 (gprs (arg-state-gpr-args state)))
    (cond ((< gprs 8) ; and by implication also (< fprs 13)
	   ;; Corresponding GPR is kept empty for functions with fixed args
	   (incf (arg-state-gpr-args state))
	   (incf (arg-state-fpr-args state))
	   ;; Assign outgoing FPRs starting at FP1
	   (my-make-wired-tn 'single-float 'single-reg (1+ fprs)))
	  ((< fprs 13)
	   ;; According to PowerOpen ABI, we need to pass those both in the
	   ;; FPRs _and_ the stack.  However empiric testing on OS X/gcc
	   ;; shows they are only passed in FPRs, AFAICT.
	   ;;
	   ;; "I" in "AFAICT" probably refers to PRM.  -- CSR, still
	   ;; reverse-engineering comments in 2003 :-)
	   (incf (arg-state-fpr-args state))
	   (incf (arg-state-stack-frame-size state))
	   (my-make-wired-tn 'single-float 'single-reg (1+ fprs)))
	  (t
	   ;; Pass on stack only
	   (let ((stack-offset (arg-state-stack-frame-size state)))
	     (incf (arg-state-stack-frame-size state))
	     (my-make-wired-tn 'single-float 'single-stack stack-offset))))))
#!-darwin
(define-alien-type-method (double-float :arg-tn) (type state)
  (declare (ignore type))
  (let* ((fprs (arg-state-fpr-args state)))
    (cond ((< fprs 8)
	   (incf (arg-state-fpr-args state))
	   ;; Assign outgoing FPRs starting at FP1
	   (my-make-wired-tn 'double-float 'double-reg (1+ fprs)))
	  (t
	   (let* ((stack-offset (arg-state-stack-frame-size state)))
	     (if (oddp stack-offset)
	       (incf stack-offset))
	     (setf (arg-state-stack-frame-size state) (+ stack-offset 2))
	     (my-make-wired-tn 'double-float 'double-stack stack-offset))))))
	   
#!+darwin
(define-alien-type-method (double-float :arg-tn) (type state)
  (declare (ignore type))
  (let ((fprs (arg-state-fpr-args state))
	(gprs (arg-state-gpr-args state)))
    (cond ((< gprs 8) ; and by implication also (< fprs 13)
	   ;; Corresponding GPRs are also kept empty
	   (incf (arg-state-gpr-args state) 2)
	   (when (> (arg-state-gpr-args state) 8)
	     ;; Spill one word to stack
	     (decf (arg-state-gpr-args state))
	     (incf (arg-state-stack-frame-size state)))
	   (incf (arg-state-fpr-args state))
	   ;; Assign outgoing FPRs starting at FP1
	   (my-make-wired-tn 'double-float 'double-reg (1+ fprs)))
	  ((< fprs 13)
	   ;; According to PowerOpen ABI, we need to pass those both in the
	   ;; FPRs _and_ the stack.  However empiric testing on OS X/gcc
	   ;; shows they are only passed in FPRs, AFAICT.
	   (incf (arg-state-stack-frame-size state) 2)
	   (incf (arg-state-fpr-args state))
	   (my-make-wired-tn 'double-float 'double-reg (1+ fprs)))
	  (t
	   ;; Pass on stack only
	   (let ((stack-offset (arg-state-stack-frame-size state)))
	     (incf (arg-state-stack-frame-size state) 2)
	     (my-make-wired-tn 'double-float 'double-stack stack-offset))))))

;;; Result state handling

(defstruct result-state
  (num-results 0))

(defun result-reg-offset (slot)
  (ecase slot
    (0 nl0-offset)
    (1 nl1-offset)))

;;; FIXME: These #!-DARWIN methods should be adjusted to take a state
;;; argument, firstly because that's our "official" API (see
;;; src/code/host-alieneval) and secondly because that way we can
;;; probably have less duplication of code.  -- CSR, 2003-07-29

#!-darwin
(define-alien-type-method (system-area-pointer :result-tn) (type)
  (declare (ignore type))
  (my-make-wired-tn 'system-area-pointer 'sap-reg nl0-offset))

#!+darwin
(define-alien-type-method (system-area-pointer :result-tn) (type state)
  (declare (ignore type))
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (my-make-wired-tn 'system-area-pointer 'sap-reg
		      (result-reg-offset num-results))))

#!-darwin
(define-alien-type-method (single-float :result-tn) (type)
  (declare (ignore type state))
  (my-make-wired-tn 'single-float 'single-reg 1))

#!+darwin
(define-alien-type-method (single-float :result-tn) (type state)
  (declare (ignore type state))
  (my-make-wired-tn 'single-float 'single-reg 1))

#!-darwin
(define-alien-type-method (double-float :result-tn) (type)
  (declare (ignore type))
  (my-make-wired-tn 'double-float 'double-reg 1))

#!+darwin
(define-alien-type-method (double-float :result-tn) (type state)
  (declare (ignore type state))
  (my-make-wired-tn 'double-float 'double-reg 1))

#!-darwin
(define-alien-type-method (values :result-tn) (type)
  (mapcar #'(lambda (type)
	      (invoke-alien-type-method :result-tn type))
	  (alien-values-type-values type)))

#!+darwin
(define-alien-type-method (values :result-tn) (type state)
  (let ((values (alien-values-type-values type)))
    (when (> (length values) 2)
      (error "Too many result values from c-call."))
    (mapcar #'(lambda (type)
		(invoke-alien-type-method :result-tn type state))
	    values)))
#!-darwin
(define-alien-type-method (integer :result-tn) (type)
  (if (alien-integer-type-signed type)
      (my-make-wired-tn 'signed-byte-32 'signed-reg nl0-offset)
      (my-make-wired-tn 'unsigned-byte-32 'unsigned-reg nl0-offset)))

#!+darwin
(define-alien-type-method (integer :result-tn) (type state)
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (multiple-value-bind (ptype reg-sc)
	(if (alien-integer-type-signed type)
	    (values 'signed-byte-32 'signed-reg)
	    (values 'unsigned-byte-32 'unsigned-reg))
      (my-make-wired-tn ptype reg-sc (result-reg-offset num-results)))))
  

(!def-vm-support-routine make-call-out-tns (type)
  (declare (type alien-fun-type type))
  (let ((arg-state (make-arg-state)))
    (collect ((arg-tns))
      (dolist (arg-type (alien-fun-type-arg-types type))
	(arg-tns (invoke-alien-type-method :arg-tn arg-type arg-state)))
      (values (my-make-wired-tn 'positive-fixnum 'any-reg nsp-offset)
	      (* (arg-state-stack-frame-size arg-state) n-word-bytes)
	      (arg-tns)
	      (invoke-alien-type-method
	       :result-tn
	       (alien-fun-type-result-type type)
	       #!+darwin (make-result-state))))))

#!+darwin
(deftransform %alien-funcall ((function type &rest args))
  (assert (sb!c::constant-continuation-p type))
  (let* ((type (sb!c::continuation-value type))
	 (arg-types (alien-fun-type-arg-types type))
	 (result-type (alien-fun-type-result-type type)))
    (assert (= (length arg-types) (length args)))
    ;; We need to do something special for 64-bit integer arguments
    ;; and results.
    (if (or (some #'(lambda (type)
		      (and (alien-integer-type-p type)
			   (> (sb!alien::alien-integer-type-bits type) 32)))
		  arg-types)
	    (and (alien-integer-type-p result-type)
		 (> (sb!alien::alien-integer-type-bits result-type) 32)))
	(collect ((new-args) (lambda-vars) (new-arg-types))
		 (dolist (type arg-types)
		   (let ((arg (gensym)))
		     (lambda-vars arg)
		     (cond ((and (alien-integer-type-p type)
				 (> (sb!alien::alien-integer-type-bits type) 32))
			    ;; 64-bit long long types are stored in
			    ;; consecutive locations, most significant word
			    ;; first (big-endian).
			    (new-args `(ash ,arg -32))
			    (new-args `(logand ,arg #xffffffff))
			    (if (alien-integer-type-signed type)
				(new-arg-types (parse-alien-type '(signed 32) (sb!kernel:make-null-lexenv)))
				(new-arg-types (parse-alien-type '(unsigned 32) (sb!kernel:make-null-lexenv))))
			    (new-arg-types (parse-alien-type '(unsigned 32) (sb!kernel:make-null-lexenv))))
			   (t
			    (new-args arg)
			    (new-arg-types type)))))
		 (cond ((and (alien-integer-type-p result-type)
			     (> (sb!alien::alien-integer-type-bits result-type) 32))
			(let ((new-result-type
			       (let ((sb!alien::*values-type-okay* t))
				 (parse-alien-type
				  (if (alien-integer-type-signed result-type)
				      '(values (signed 32) (unsigned 32))
				      '(values (unsigned 32) (unsigned 32)))
				  (sb!kernel:make-null-lexenv)))))
			  `(lambda (function type ,@(lambda-vars))
			    (declare (ignore type))
			    (multiple-value-bind (high low)
				(%alien-funcall function
						',(make-alien-fun-type
						   :arg-types (new-arg-types)
						   :result-type new-result-type)
						,@(new-args))
			      (logior low (ash high 32))))))
		       (t
			`(lambda (function type ,@(lambda-vars))
			  (declare (ignore type))
			  (%alien-funcall function
			   ',(make-alien-fun-type
			      :arg-types (new-arg-types)
			      :result-type result-type)
			   ,@(new-args))))))
	(sb!c::give-up-ir1-transform))))

(define-vop (foreign-symbol-address)
  (:translate foreign-symbol-address)
  (:policy :fast-safe)
  (:args)
  (:arg-types (:constant simple-base-string))
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (inst lr res  (make-fixup (extern-alien-name foreign-symbol) :foreign))))

(define-vop (call-out)
  (:args (function :scs (sap-reg) :target cfunc)
	 (args :more t))
  (:results (results :more t))
  (:ignore args results)
  (:save-p t)
  (:temporary (:sc any-reg :offset cfunc-offset
		   :from (:argument 0) :to (:result 0)) cfunc)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:vop-var vop)
  (:generator 0
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(store-stack-tn nfp-save cur-nfp))
      (inst lr temp (make-fixup (extern-alien-name "call_into_c") :foreign))
      (inst mtctr temp)
      (move cfunc function)
      (inst bctrl)
      (when cur-nfp
	(load-stack-tn cur-nfp nfp-save)))))


(define-vop (alloc-number-stack-space)
  (:info amount)
  (:results (result :scs (sap-reg any-reg)))
  (:temporary (:scs (unsigned-reg) :to (:result 0)) temp)
  (:generator 0
    (unless (zerop amount)
      (let ((delta (- (logandc2 (+ amount 8 +stack-alignment-bytes+) 
				+stack-alignment-bytes+))))
	(cond ((>= delta (ash -1 16))
	       (inst stwu nsp-tn nsp-tn delta))
	      (t
	       (inst lr temp delta)
	       (inst stwux  nsp-tn nsp-tn temp)))))
    (unless (location= result nsp-tn)
      ;; They are only location= when the result tn was allocated by
      ;; make-call-out-tns above, which takes the number-stack-displacement
      ;; into account itself.
      (inst addi result nsp-tn number-stack-displacement))))

(define-vop (dealloc-number-stack-space)
  (:info amount)
  (:policy :fast-safe)
  (:generator 0
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount 8 +stack-alignment-bytes+) 
			     +stack-alignment-bytes+)))
	(cond ((< delta (ash 1 16))
	       (inst addi nsp-tn nsp-tn delta))
	      (t
	       (inst lwz nsp-tn nsp-tn 0)))))))
