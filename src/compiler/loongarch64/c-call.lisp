;;;; function call for the LoongArch VM

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
  `(1 ,@(loop for i from 22 to 31 collect i))
  #'equal)

(defconstant-eqx c-unsaved-registers
  `(1 ,@(loop for i from 4 to 11 collect i)
      ,@(loop for i from 12 to 20 collect i))
  #'equal)

(defconstant-eqx c-saved-float-registers
  `(,@(loop for i from 24 to 31 collect i))
  #'equal)

(defconstant-eqx c-unsaved-float-registers
  `(,@(loop for i from 8 to 23 collect i)
      ,@(loop for i from 0 to 7 collect i))
  #'equal)

(defun make-reg-tn (offset &optional (sc 'any-reg))
  (make-random-tn (sc-or-lose sc) offset))

(defenum (:start 4)
  ca0-offset
  ca1-offset
  ca2-offset
  ca3-offset
  ca4-offset
  ca5-offset
  ca6-offset
  ca7-offset)

(defenum (:start 0)
  fa0-offset
  fa1-offset
  fa2-offset
  fa3-offset
  fa4-offset
  fa5-offset
  fa6-offset
  fa7-offset)

;; the number stack is always aligned to 16-byte
;; boundraries, no matter the word size.
(defconstant +number-stack-alignment-mask+ (1- 16))
(defconstant foreign-int-register-arg-start 4)
(defconstant n-foreign-register-args 8)
(defconstant foreign-float-register-arg-start 0)

(defstruct arg-state
  (int-arg-index 0)
  (stack-frame-size 0)
  (float-arg-index 0))

(defstruct (result-state (:copier nil))
  (num-results 0))

(defun result-reg-offset (slot)
  (ecase slot
    (0 nl0-offset)
    (1 nl1-offset)))

(defun int-register-args-offset (index)
  (+ foreign-int-register-arg-start index))

(defun float-register-args-offset (index)
  (+ foreign-float-register-arg-start index))

(defun int-arg-tn (state prim-type reg-sc stack-sc)
  (let ((index (arg-state-int-arg-index state)))
    (cond
     ((< index n-foreign-register-args)
      (setf (arg-state-int-arg-index state) (1+ index))
      (make-wired-tn* prim-type reg-sc (int-register-args-offset index)))
     (t
      (let ((frame-size (arg-state-stack-frame-size state)))
        (setf (arg-state-stack-frame-size state) (1+ frame-size))
        (make-wired-tn* prim-type stack-sc frame-size))))))

(defun float-arg-tn (state prim-type reg-sc stack-sc)
  (let ((index (arg-state-float-arg-index state)))
    (cond ((< index n-foreign-register-args)
           (setf (arg-state-float-arg-index state) (1+ index))
           (make-wired-tn* prim-type reg-sc (float-register-args-offset index)))
          ((let ((index (arg-state-int-arg-index state)))
             (cond
               ((< index n-foreign-register-args)
                (setf (arg-state-int-arg-index state) (1+ index))
                (list
                 (make-wired-tn* 'signed-byte-64 signed-reg-sc-number (int-register-args-offset index))
                 (if (eq prim-type 'single-float)
                     'single-float-bits
                     'double-float-bits)))
               (t
                (let ((frame-size (arg-state-stack-frame-size state)))
                  (setf (arg-state-stack-frame-size state) (1+ frame-size))
                  (make-wired-tn* prim-type stack-sc frame-size)))))))))

(define-alien-type-method (integer :arg-tn) (type state)
  (if (alien-integer-type-signed type)
      (int-arg-tn state 'signed-byte-64 signed-reg-sc-number signed-stack-sc-number)
      (int-arg-tn state 'unsigned-byte-64 unsigned-reg-sc-number unsigned-stack-sc-number)))

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
            (values 'signed-byte-64 signed-reg-sc-number)
            (values 'unsigned-byte-64 unsigned-reg-sc-number))
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
      (inst jal ra-tn (make-fixup 'call-into-c :assembly-routine))
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
        (inst addi.d nsp-tn nsp-tn delta)))))

;;; Callback
#-sb-xc-host
(defun alien-callback-accessor-form (type sap offset)
  `(deref (sap-alien (sap+ ,sap ,offset) (* ,type))))

;;; Returns a vector in static space containing machine code for the
;;; callback wrapper.
#-sb-xc-host
(defun alien-callback-assembler-wrapper (index result-type argument-types)
  "Generate code for an alien callback, fixing double alignment for mixed parameters."
  (flet ((make-gpr (n)
           (make-random-tn (sc-or-lose 'any-reg) n))
         (make-fpr (n)
           (make-random-tn (sc-or-lose 'double-reg) n)))
    (let* ((segment (make-segment))
           (n-argument-words
            (mapcar (lambda (arg)
                      (ceiling (alien-type-bits arg) n-word-bits))
                    argument-types))
           (n-linkage-area-bytes 16)
           (n-return-area-words
            (ceiling (or (alien-type-bits result-type) 0) n-word-bits))
           (n-return-area-bytes (* n-return-area-words n-word-bytes))
           (n-callee-register-args-bytes 16)
           (n-frame-bytes (logandc2 (+ n-linkage-area-bytes
                                       n-return-area-bytes
                                       n-callee-register-args-bytes
                                       7)
                                    7))
           (words-processed 0)
           (gprs (mapcar #'make-gpr '(4 5 6 7 8 9 10 11)))  ; a0-a7
           (fprs (mapcar #'make-fpr '(0 1 2 3 4 5 6 7))))  ; fa0-fa7
      (flet ((save-arg (type words)
               (let ((offset (* words-processed n-word-bytes)))
                 (cond
                   ((not (alien-float-type-p type))
                    (loop for i from 0 below words
                          do (when gprs
                               (let ((reg (pop gprs)))
                                 (inst st.d reg nsp-tn offset))
                               (incf words-processed)
                               (incf offset n-word-bytes))))
                   ;; single float
                   ((alien-single-float-type-p type)
                    (when fprs
                      (let ((fpr (pop fprs)))
                        (inst fst.d fpr nsp-tn offset)
                        (incf words-processed))))
                   ;; double float
                   ((alien-double-float-type-p type)
                    (when (oddp words-processed)
                      (when gprs (pop gprs))
                      (incf words-processed))
                    (when fprs
                      (let ((fpr (pop fprs)))
                        (inst fst.d fpr nsp-tn offset)
                        (incf words-processed 2)
                        (incf offset (* 2 n-word-bytes)))))
                   (t (bug "Unknown alien type: ~S" type))))))
        (assemble (segment 'nil)
          (mapc #'save-arg argument-types n-argument-words)
          (destructuring-bind (a0 a1 a2 a3 ra sp)
              (mapcar #'make-gpr '(4 5 6 7 1 3))
            (inst addi.d sp sp (- n-frame-bytes))
            (inst st.d ra sp (- n-frame-bytes n-word-bytes))
            (inst li a3 (foreign-symbol-address "callback_wrapper_trampoline"))
            (inst li a0 (fixnumize index))
            (inst addi.d a1 sp n-frame-bytes)
            (inst addi.d a2 sp n-callee-register-args-bytes)
            (inst jirl ra a3 0)
            (inst ld.d ra sp (- n-frame-bytes n-word-bytes))
            (cond
              ((alien-single-float-type-p result-type)
               (let ((fpr (make-fpr 0)))
                 (inst fld.s fpr sp n-callee-register-args-bytes)
                 ))
              ((alien-double-float-type-p result-type)
               (let ((fpr (make-fpr 0)))
                 (inst fld.d fpr sp n-callee-register-args-bytes)))
              ((or (alien-integer-type-p result-type)
                   (alien-pointer-type-p result-type)
                   (alien-type-= #.(parse-alien-type 'system-area-pointer nil)
                                 result-type))
               (inst ld.d (make-gpr 4) sp n-callee-register-args-bytes))
              ((alien-void-type-p result-type)))
            (inst addi.d sp sp n-frame-bytes)
            (inst jirl zero-tn ra 0))))
      (let* ((buffer (sb-assem:segment-buffer segment))
             (vector (make-static-vector (length buffer)
                                         :element-type '(unsigned-byte 8)
                                         :initial-contents buffer))
             (sap (vector-sap vector)))
        (alien-funcall
         (extern-alien "os_flush_icache"
                       (function void
                                 system-area-pointer
                                 unsigned-long))
         sap (length buffer))
        vector))))
