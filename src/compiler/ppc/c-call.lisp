;;; routines for call-out to C.
;;;
;;; Written by William Lott.
;;;
(in-package "SB!VM")

(defun my-make-wired-tn (prim-type-name sc-name offset)
  (make-wired-tn (primitive-type-or-lose prim-type-name)
		 (sc-number-or-lose sc-name)
		 offset))

(defstruct arg-state
  (gpr-args 0)
  (fpr-args 0)
  ;SVR4 [a]abi wants two words on stack (callee saved lr, backpointer).
  (stack-frame-size 2))

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

; If a single-float arg has to go on the stack, it's promoted to
; double.  That way, C programs can get subtle rounding errors
; when unrelated arguments are introduced.

(define-alien-type-method (single-float :arg-tn) (type state)
  (declare (ignore type))
  (let* ((fprs (arg-state-fpr-args state)))
    (cond ((< fprs 8)
	   (incf (arg-state-fpr-args state))
	   ; Assign outgoing FPRs starting at FP1
	   (my-make-wired-tn 'single-float 'single-reg (1+ fprs)))
	  (t
	   (let* ((stack-offset (arg-state-stack-frame-size state)))
	     (if (oddp stack-offset)
	       (incf stack-offset))
	     (setf (arg-state-stack-frame-size state) (+ stack-offset 2))
	     (my-make-wired-tn 'double-float 'double-stack stack-offset))))))

(define-alien-type-method (double-float :arg-tn) (type state)
  (declare (ignore type))
  (let* ((fprs (arg-state-fpr-args state)))
    (cond ((< fprs 8)
	   (incf (arg-state-fpr-args state))
	   ; Assign outgoing FPRs starting at FP1
	   (my-make-wired-tn 'double-float 'double-reg (1+ fprs)))
	  (t
	   (let* ((stack-offset (arg-state-stack-frame-size state)))
	     (if (oddp stack-offset)
	       (incf stack-offset))
	     (setf (arg-state-stack-frame-size state) (+ stack-offset 2))
	     (my-make-wired-tn 'double-float 'double-stack stack-offset))))))
	   
(define-alien-type-method (integer :result-tn) (type)
  (if (alien-integer-type-signed type)
      (my-make-wired-tn 'signed-byte-32 'signed-reg nl0-offset)
      (my-make-wired-tn 'unsigned-byte-32 'unsigned-reg nl0-offset)))


(define-alien-type-method (system-area-pointer :result-tn) (type)
  (declare (ignore type))
  (my-make-wired-tn 'system-area-pointer 'sap-reg nl0-offset))

(define-alien-type-method (single-float :result-tn) (type)
  (declare (ignore type))
  (my-make-wired-tn 'single-float 'single-reg 1))

(define-alien-type-method (double-float :result-tn) (type)
  (declare (ignore type))
  (my-make-wired-tn 'double-float 'double-reg 1))

(define-alien-type-method (values :result-tn) (type)
  (mapcar #'(lambda (type)
	      (invoke-alien-type-method :result-tn type))
	  (alien-values-type-values type)))


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
	       (alien-fun-type-result-type type))))))


(define-vop (foreign-symbol-address)
  (:translate foreign-symbol-address)
  (:policy :fast-safe)
  (:args)
  (:arg-types (:constant simple-string))
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (inst lr res  (make-fixup foreign-symbol :foreign))))

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
      (inst lr temp (make-fixup "call_into_c" :foreign))
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
      (let ((delta (- (logandc2 (+ amount 8 7) 7))))
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
      (let ((delta (logandc2 (+ amount 8 7) 7)))
	(cond ((< delta (ash 1 16))
	       (inst addi nsp-tn nsp-tn delta))
	      (t
	       (inst lwz nsp-tn nsp-tn 0)))))))
