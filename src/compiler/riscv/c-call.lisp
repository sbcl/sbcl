;;;; function call for the RISC-V VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")


(defconstant-eqx c-saved-registers
    '#.`(,lip-offset 8 9 ,@(loop for i from 18 to 27 collect i))
  #'equal)

(defconstant-eqx c-unsaved-registers
    '#.`(,lip-offset
         ,@(loop for i from 5 to 7 collect i)
         ,@(loop for i from 10 to 17 collect i)
         ,@(loop for i from 28 to 31 collect i))
  #'equal)

(defconstant-eqx c-saved-float-registers
    '#.`(8 9 ,@(loop for i from 18 to 27 collect i))
  #'equal)

(defconstant-eqx c-unsaved-float-registers
    '#.`(,@(loop for i from 0 to 7 collect i)
         ,@(loop for i from 10 to 17 collect i)
         ,@(loop for i from 28 to 31 collect i))
  #'equal)

(defun make-reg-tn (offset &optional (sc 'any-reg))
  (make-random-tn :kind :normal
                  :sc (sc-or-lose sc)
                  :offset offset))

(defenum (:start 10)
  ca0-offset
  ca1-offset
  ca2-offset
  ca3-offset
  ca4-offset
  ca5-offset
  ca6-offset
  ca7-offset)

(defenum (:start 10)
  fa0-offset
  fa1-offset
  fa2-offset
  fa3-offset
  fa4-offset
  fa5-offset
  fa6-offset
  fa7-offset)

;; On RISC-V, the number stack is always aligned to 16-byte
;; boundraries, no matter the word size.
(defconstant +number-stack-alignment-mask+ (1- 16))
(defconstant foreign-register-arg-start 10)
(defconstant n-foreign-register-args 8)

(defstruct arg-state
  (int-arg-index 0)
  (stack-frame-size 0)
  (float-arg-index 0))

(defstruct (result-state (:copier nil))
  (num-results 0))

(defun result-reg-offset (slot)
  (ecase slot
    (0 nl1-offset)
    (1 nl0-offset)))

(defun register-args-offset (index) (+ foreign-register-arg-start index))

(macrolet ((def (name index-accessor)
             `(defun ,name (state prim-type reg-sc stack-sc)
                (let ((index (,index-accessor state)))
                  (cond ((< index n-foreign-register-args)
                         (setf (,index-accessor state) (1+ index))
                         (make-wired-tn* prim-type reg-sc (register-args-offset index)))
                        (t
                         (let ((frame-size (arg-state-stack-frame-size state)))
                           (setf (arg-state-stack-frame-size state) (1+ frame-size))
                           (make-wired-tn* prim-type stack-sc frame-size))))))))
  (def int-arg-tn arg-state-int-arg-index)
  (def float-arg-tn arg-state-float-arg-index))

(define-alien-type-method (integer :arg-tn) (type state)
  (if (alien-integer-type-signed type)
      (int-arg-tn state #-64-bit 'signed-byte-32 #+64-bit 'signed-byte-64 signed-reg-sc-number signed-stack-sc-number)
      (int-arg-tn state #-64-bit 'unsigned-byte-32  #+64-bit 'unsigned-byte-64 unsigned-reg-sc-number unsigned-stack-sc-number)))

(define-alien-type-method (system-area-pointer :arg-tn) (type state)
  (declare (ignore type))
  (int-arg-tn state 'system-area-pointer sap-reg-sc-number sap-stack-sc-number))

(define-alien-type-method (single-float :arg-tn) (type state)
  (declare (ignore type))
  (float-arg-tn state 'single-float single-reg-sc-number single-stack-sc-number))

(define-alien-type-method (double-float :arg-tn) (type state)
  (declare (ignore type))
  (float-arg-tn state 'double-float double-reg-sc-number double-stack-sc-number))

(define-alien-type-method (integer :result-tn) (type state)
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (multiple-value-bind (ptype reg-sc)
        (if (alien-integer-type-signed type)
            (values #-64-bit 'signed-byte-32 #+64-bit 'signed-byte-64 signed-reg-sc-number)
            (values #-64-bit 'unsigned-byte-32 #+64-bit 'unsigned-byte-64 unsigned-reg-sc-number))
      (make-wired-tn* ptype reg-sc
                      (result-reg-offset num-results)))))

(define-alien-type-method (system-area-pointer :result-tn) (type state)
  (declare (ignore type state))
  (make-wired-tn* 'system-area-pointer sap-reg-sc-number (result-reg-offset 0)))

(define-alien-type-method (single-float :result-tn) (type state)
  (declare (ignore type state))
  (make-wired-tn* 'single-float single-reg-sc-number fa0-offset))

(define-alien-type-method (double-float :result-tn) (type state)
  (declare (ignore type state))
  (make-wired-tn* 'double-float double-reg-sc-number fa0-offset))

(define-alien-type-method (values :result-tn) (type state)
  (let ((values (alien-values-type-values type)))
    (when (> (length values) 2)
      (error "Too many result values from c-call."))
    (mapcar (lambda (type)
              (invoke-alien-type-method :result-tn type state))
            values)))

(defun make-call-out-tns (type)
  (let ((arg-state (make-arg-state)))
    (collect ((arg-tns))
      (dolist (arg-type (alien-fun-type-arg-types type))
        (arg-tns (invoke-alien-type-method :arg-tn arg-type arg-state)))
      (values (make-wired-tn* 'positive-fixnum any-reg-sc-number nsp-offset)
              (* (arg-state-stack-frame-size arg-state) n-word-bytes)
              (arg-tns)
              (invoke-alien-type-method :result-tn
                                        (alien-fun-type-result-type type)
                                        (make-result-state))))))

(define-vop (foreign-symbol-sap)
  (:translate foreign-symbol-sap)
  (:policy :fast-safe)
  (:args)
  (:arg-types (:constant simple-string))
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (inst li res (make-fixup foreign-symbol :foreign))))

(define-vop (foreign-symbol-dataref-sap)
  (:translate foreign-symbol-dataref-sap)
  (:policy :fast-safe)
  (:args)
  (:arg-types (:constant simple-string))
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    ;; This probably has to be 3 instructions unless we can put some linkage entries
    ;; near enough to NULL-TN. Would only make a difference when compiling to memory
    ;; since compiling to file has to assume worst case.
    (inst li res (make-fixup foreign-symbol :foreign-dataref))
    (loadw res res)))

(define-vop (call-out)
  (:args (function :scs (sap-reg) :target cfunc)
         (args :more t))
  (:results (results :more t))
  (:ignore args results)
  (:save-p t)
  (:temporary (:sc any-reg :offset cfunc-offset
                   :from (:argument 0) :to (:result 0)) cfunc)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:vop-var vop)
  (:generator 0
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (store-stack-tn nfp-save cur-nfp))
      (move cfunc function)
      (invoke-asm-routine 'call-into-c)
      (when cur-nfp
        (load-stack-tn cur-nfp nfp-save)))))

(define-vop (alloc-number-stack-space)
  (:info amount)
  (:result-types system-area-pointer)
  (:results (result :scs (sap-reg any-reg)))
  (:generator 0
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount +number-stack-alignment-mask+)
                             +number-stack-alignment-mask+)))
        (inst subi nsp-tn nsp-tn delta)))
    (move result nsp-tn)))

(define-vop (dealloc-number-stack-space)
  (:info amount)
  (:policy :fast-safe)
  (:generator 0
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount +number-stack-alignment-mask+)
                             +number-stack-alignment-mask+)))
        (inst addi nsp-tn nsp-tn delta)))))

;;; Callback
#-sb-xc-host
(defun alien-callback-accessor-form (type sap offset)
  `(deref (sap-alien (sap+ ,sap ,offset) (* ,type))))

;;; Returns a vector in static space containing machine code for the
;;; callback wrapper.
#-sb-xc-host
(defun alien-callback-assembler-wrapper (index result-type argument-types)
  (declare (ignore index result-type argument-types))
  (error "please implement"))
