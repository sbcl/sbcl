;;;; the VOPs and other necessary machine specific support
;;;; routines for call-out to C

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;; The MOVE-ARGUMENT vop is going to store args on the stack for
;; call-out. These tn's will be used for that. move-arg is normally
;; used for things going down the stack but C wants to have args
;; indexed in the positive direction.

(defun my-make-wired-tn (prim-type-name sc-name offset)
  (make-wired-tn (primitive-type-or-lose prim-type-name)
		 (sc-number-or-lose sc-name)
		 offset))

(defstruct (arg-state (:copier nil))
  (stack-frame-size 0))

(def-alien-type-method (integer :arg-tn) (type state)
  (let ((stack-frame-size (arg-state-stack-frame-size state)))
    (setf (arg-state-stack-frame-size state) (1+ stack-frame-size))
    (multiple-value-bind (ptype stack-sc)
	(if (alien-integer-type-signed type)
	    (values 'signed-byte-32 'signed-stack)
	    (values 'unsigned-byte-32 'unsigned-stack))
      (my-make-wired-tn ptype stack-sc stack-frame-size))))

(def-alien-type-method (system-area-pointer :arg-tn) (type state)
  (declare (ignore type))
  (let ((stack-frame-size (arg-state-stack-frame-size state)))
    (setf (arg-state-stack-frame-size state) (1+ stack-frame-size))
    (my-make-wired-tn 'system-area-pointer
		      'sap-stack
		      stack-frame-size)))

#!+long-float
(def-alien-type-method (long-float :arg-tn) (type state)
  (declare (ignore type))
  (let ((stack-frame-size (arg-state-stack-frame-size state)))
    (setf (arg-state-stack-frame-size state) (+ stack-frame-size 3))
    (my-make-wired-tn 'long-float 'long-stack stack-frame-size)))

(def-alien-type-method (double-float :arg-tn) (type state)
  (declare (ignore type))
  (let ((stack-frame-size (arg-state-stack-frame-size state)))
    (setf (arg-state-stack-frame-size state) (+ stack-frame-size 2))
    (my-make-wired-tn 'double-float 'double-stack stack-frame-size)))

(def-alien-type-method (single-float :arg-tn) (type state)
  (declare (ignore type))
  (let ((stack-frame-size (arg-state-stack-frame-size state)))
    (setf (arg-state-stack-frame-size state) (1+ stack-frame-size))
    (my-make-wired-tn 'single-float 'single-stack stack-frame-size)))

(defstruct (result-state (:copier nil))
  (num-results 0))

(defun result-reg-offset (slot)
  (ecase slot
    (0 eax-offset)
    (1 edx-offset)))

(def-alien-type-method (integer :result-tn) (type state)
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (multiple-value-bind (ptype reg-sc)
	(if (alien-integer-type-signed type)
	    (values 'signed-byte-32 'signed-reg)
	    (values 'unsigned-byte-32 'unsigned-reg))
      (my-make-wired-tn ptype reg-sc (result-reg-offset num-results)))))

(def-alien-type-method (system-area-pointer :result-tn) (type state)
  (declare (ignore type))
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (my-make-wired-tn 'system-area-pointer 'sap-reg
		      (result-reg-offset num-results))))

#!+long-float
(def-alien-type-method (long-float :result-tn) (type state)
  (declare (ignore type))
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (my-make-wired-tn 'long-float 'long-reg (* num-results 2))))

(def-alien-type-method (double-float :result-tn) (type state)
  (declare (ignore type))
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (my-make-wired-tn 'double-float 'double-reg (* num-results 2))))

(def-alien-type-method (single-float :result-tn) (type state)
  (declare (ignore type))
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (my-make-wired-tn 'single-float 'single-reg (* num-results 2))))

#+nil ;;pfw obsolete now?
(def-alien-type-method (values :result-tn) (type state)
  (mapcar #'(lambda (type)
	      (invoke-alien-type-method :result-tn type state))
	  (alien-values-type-values type)))

;;; pfw - from alpha
(def-alien-type-method (values :result-tn) (type state)
  (let ((values (alien-values-type-values type)))
    (when (cdr values)
      (error "Too many result values from c-call."))
    (when values
      (invoke-alien-type-method :result-tn (car values) state))))

(!def-vm-support-routine make-call-out-tns (type)
  (let ((arg-state (make-arg-state)))
    (collect ((arg-tns))
      (dolist #+nil ;; this reversed list seems to cause the alien botches!!
	(arg-type (reverse (alien-fun-type-arg-types type)))
	(arg-type (alien-fun-type-arg-types type))
	(arg-tns (invoke-alien-type-method :arg-tn arg-type arg-state)))
      (values (my-make-wired-tn 'positive-fixnum 'any-reg esp-offset)
	      (* (arg-state-stack-frame-size arg-state) word-bytes)
	      (arg-tns)
	      (invoke-alien-type-method :result-tn
					(alien-fun-type-result-type type)
					(make-result-state))))))

(define-vop (foreign-symbol-address)
  (:translate foreign-symbol-address)
  (:policy :fast-safe)
  (:args)
  (:arg-types (:constant simple-string))
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
   (inst lea res (make-fixup (extern-alien-name foreign-symbol) :foreign))))

(define-vop (call-out)
  (:args (function :scs (sap-reg))
	 (args :more t))
  (:results (results :more t))
  ;; eax is already wired
  (:temporary (:sc unsigned-reg :offset ecx-offset) ecx)
  (:temporary (:sc unsigned-reg :offset edx-offset) edx)
  (:node-var node)
  (:vop-var vop)
  (:save-p t)
  (:ignore args ecx edx)
  (:generator 0
    (cond ((policy node (> space speed))
	   (move eax-tn function)
	   (inst call (make-fixup (extern-alien-name "call_into_c") :foreign)))
	  (t
	   ;; Setup the NPX for C; all the FP registers need to be
	   ;; empty; pop them all.
	   (inst fstp fr0-tn)
	   (inst fstp fr0-tn)
	   (inst fstp fr0-tn)
	   (inst fstp fr0-tn)
	   (inst fstp fr0-tn)
	   (inst fstp fr0-tn)
	   (inst fstp fr0-tn)
	   (inst fstp fr0-tn)

	   (inst call function)
	   ;; To give the debugger a clue. XX not really internal-error?
	   (note-this-location vop :internal-error)

	   ;; Restore the NPX for lisp.
	   (inst fldz) ; insure no regs are empty
	   (inst fldz)
	   (inst fldz)
	   (inst fldz)
	   (inst fldz)
	   (inst fldz)
	   (inst fldz)

	   (if (and results
		    (location= (tn-ref-tn results) fr0-tn))
	       ;; The return result is in fr0.
	       (inst fxch fr7-tn) ; move the result back to fr0
	       (inst fldz)) ; insure no regs are empty
	   ))))

(define-vop (alloc-number-stack-space)
  (:info amount)
  (:results (result :scs (sap-reg any-reg)))
  (:generator 0
    (aver (location= result esp-tn))
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount 3) 3)))
	(inst sub esp-tn delta)))
    (move result esp-tn)))

(define-vop (dealloc-number-stack-space)
  (:info amount)
  (:generator 0
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount 3) 3)))
	(inst add esp-tn delta)))))

(define-vop (alloc-alien-stack-space)
  (:info amount)
  (:results (result :scs (sap-reg any-reg)))
  (:generator 0
    (aver (not (location= result esp-tn)))
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount 3) 3)))
	(inst sub (make-ea :dword
			   :disp (+ nil-value
				    (static-symbol-offset '*alien-stack*)
				    (ash symbol-value-slot word-shift)
				    (- other-pointer-type)))
	      delta)))
    (load-symbol-value result *alien-stack*)))

(define-vop (dealloc-alien-stack-space)
  (:info amount)
  (:generator 0
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount 3) 3)))
	(inst add (make-ea :dword
			   :disp (+ nil-value
				    (static-symbol-offset '*alien-stack*)
				    (ash symbol-value-slot word-shift)
				    (- other-pointer-type)))
	      delta)))))
