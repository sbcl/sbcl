;;;; VOPs and other machine-specific support routines for call-out to C

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

(defconstant +number-stack-allocation-granularity+ n-word-bytes)

(defconstant +max-register-args+ 4)

(defun my-make-wired-tn (prim-type-name sc-name offset)
  (make-wired-tn (primitive-type-or-lose prim-type-name )
                 (sc-number-or-lose sc-name )
                 offset))

(defstruct arg-state
  (next-double-register 0)
  (next-single-register 0)
  (num-register-args 0)
  (stack-frame-size 0))

(defun register-args-offset (index)
  (elt '(#.ocfp-offset #.nargs-offset #.nl2-offset #.nl3-offset)
       index))

(defun int-arg (state prim-type reg-sc stack-sc)
  (let ((reg-args (arg-state-num-register-args state)))
    (cond ((< reg-args +max-register-args+)
           (setf (arg-state-num-register-args state) (1+ reg-args))
           (my-make-wired-tn prim-type reg-sc (register-args-offset reg-args)))
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

(define-alien-type-method (single-float :arg-tn) (type state)
  (declare (ignore type))
  (let ((register (arg-state-next-single-register state)))
    (when (> register 15)
      (error "Don't know how to handle alien single floats on stack."))
    (prog1
        (my-make-wired-tn 'single-float 'single-reg register)
      (setf (arg-state-next-single-register state)
            (max (1+ register)
                 (* 2 (arg-state-next-double-register state))))
      (setf (arg-state-next-double-register state)
            (+ (/ (arg-state-next-single-register state) 2)
               (if (evenp (arg-state-next-single-register state)) 0 1))))))

(define-alien-type-method (double-float :arg-tn) (type state)
  (declare (ignore type))
  (let ((register (arg-state-next-double-register state)))
    (when (> register 7)
      (error "Don't know how to handle alien double floats on stack."))
    (prog1
        (my-make-wired-tn 'double-float 'double-reg (* register 2))
      (incf (arg-state-next-double-register state))
      (when (evenp (arg-state-next-single-register state))
        (incf (arg-state-next-single-register state) 2)))))

(define-alien-type-method (integer :result-tn) (type state)
  (declare (ignore state))
  (multiple-value-bind
      (ptype reg-sc)
      (if (alien-integer-type-signed type)
          (values 'signed-byte-32 'signed-reg)
          (values 'unsigned-byte-32 'unsigned-reg))
    (my-make-wired-tn ptype reg-sc nargs-offset)))

(define-alien-type-method (system-area-pointer :result-tn) (type state)
  (declare (ignore type state))
  (my-make-wired-tn 'system-area-pointer 'sap-reg nargs-offset))

(define-alien-type-method (single-float :result-tn) (type state)
  (declare (ignore type state))
  (my-make-wired-tn 'single-float 'single-reg 0))

(define-alien-type-method (double-float :result-tn) (type state)
  (declare (ignore type state))
  (my-make-wired-tn 'double-float 'double-reg 0))

(define-alien-type-method (values :result-tn) (type state)
  (let ((values (alien-values-type-values type)))
    (when (cdr values)
      (error "Too many result values from c-call."))
    (when values
      (invoke-alien-type-method :result-tn (car values) state))))

(defun make-call-out-tns (type)
  (let ((arg-state (make-arg-state)))
    (collect ((arg-tns))
      (dolist (arg-type (alien-fun-type-arg-types type))
        (arg-tns (invoke-alien-type-method :arg-tn arg-type arg-state)))
      (values (make-normal-tn *fixnum-primitive-type*)
              (* (arg-state-stack-frame-size arg-state) n-word-bytes)
              (arg-tns)
              (invoke-alien-type-method :result-tn
                                        (alien-fun-type-result-type type)
                                        nil)))))

(define-vop (foreign-symbol-sap)
  (:translate foreign-symbol-sap)
  (:policy :fast-safe)
  (:args)
  (:arg-types (:constant simple-string))
  (:info foreign-symbol)
  (:temporary (:sc interior-reg) lip)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (let ((fixup-label (gen-label)))
      (inst load-from-label res lip fixup-label)
      (assemble (*elsewhere*)
        (emit-label fixup-label)
        (inst word (make-fixup foreign-symbol :foreign))))))

(define-vop (call-out)
  (:args (function :scs (sap-reg sap-stack))
         (args :more t))
  (:results (results :more t))
  (:ignore args results)
  (:save-p t)
  (:temporary (:sc any-reg :offset r8-offset
                   :from (:argument 0) :to (:result 0)) cfunc)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:temporary (:sc any-reg) temp)
  (:temporary (:sc interior-reg) lip)
  (:vop-var vop)
  (:generator 0
    (let ((call-into-c-fixup (gen-label))
          (cur-nfp (current-nfp-tn vop)))
      (assemble (*elsewhere*)
        (emit-label call-into-c-fixup)
        (inst word (make-fixup "call_into_c" :foreign)))
      (when cur-nfp
        (store-stack-tn nfp-save cur-nfp))
      (inst load-from-label temp lip call-into-c-fixup)
      (sc-case function
        (sap-reg (move cfunc function))
        (sap-stack (loadw cfunc cur-nfp (tn-offset function))))
      (inst blx temp)
      (when cur-nfp
        (load-stack-tn cur-nfp nfp-save)))))

(define-vop (alloc-number-stack-space)
  (:info amount)
  (:result-types system-area-pointer)
  (:results (result :scs (sap-reg any-reg)))
  (:generator 0
    (load-symbol-value result *number-stack-pointer*)
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount (1- +number-stack-allocation-granularity+))
                             (1- +number-stack-allocation-granularity+))))
        (inst sub result result delta)
        (store-symbol-value result *number-stack-pointer*)))))

(define-vop (dealloc-number-stack-space)
  (:info amount)
  (:policy :fast-safe)
  (:temporary (:scs (unsigned-reg) :to (:result 0)) temp)
  (:generator 0
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount (1- +number-stack-allocation-granularity+))
                             (1- +number-stack-allocation-granularity+))))
        (load-symbol-value temp *number-stack-pointer*)
        (inst add temp temp delta)
        (store-symbol-value temp *number-stack-pointer*)))))
