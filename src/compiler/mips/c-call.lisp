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

(defun my-make-wired-tn (prim-type-name sc-name offset)
  (make-wired-tn (primitive-type-or-lose prim-type-name)
                 (sc-number-or-lose sc-name)
                 offset))

(defstruct arg-state
  (stack-frame-size 0)
  (did-int-arg nil)
  (float-args 0))

(define-alien-type-method (integer :arg-tn) (type state)
  (let ((stack-frame-size (arg-state-stack-frame-size state)))
    (setf (arg-state-stack-frame-size state) (1+ stack-frame-size))
    (setf (arg-state-did-int-arg state) t)
    (multiple-value-bind
        (ptype reg-sc stack-sc)
        (if (alien-integer-type-signed type)
            (values 'signed-byte-32 'signed-reg 'signed-stack)
            (values 'unsigned-byte-32 'unsigned-reg 'unsigned-stack))
      (if (< stack-frame-size 4)
          (my-make-wired-tn ptype reg-sc (+ stack-frame-size 4))
          (my-make-wired-tn ptype stack-sc stack-frame-size)))))

(define-alien-type-method (system-area-pointer :arg-tn) (type state)
  (declare (ignore type))
  (let ((stack-frame-size (arg-state-stack-frame-size state)))
    (setf (arg-state-stack-frame-size state) (1+ stack-frame-size))
    (setf (arg-state-did-int-arg state) t)
    (if (< stack-frame-size 4)
        (my-make-wired-tn 'system-area-pointer
                          'sap-reg
                          (+ stack-frame-size 4))
        (my-make-wired-tn 'system-area-pointer
                          'sap-stack
                          stack-frame-size))))

(define-alien-type-method (double-float :arg-tn) (type state)
  (declare (ignore type))
  (let ((stack-frame-size (logandc2 (1+ (arg-state-stack-frame-size state)) 1))
        (float-args (arg-state-float-args state)))
    (setf (arg-state-stack-frame-size state) (+ stack-frame-size 2))
    (setf (arg-state-float-args state) (1+ float-args))
    (cond ((>= stack-frame-size 4)
           (my-make-wired-tn 'double-float
                             'double-stack
                             stack-frame-size))
          ((and (not (arg-state-did-int-arg state))
                (< float-args 2))
           (my-make-wired-tn 'double-float
                             'double-reg
                             (+ (* float-args 2) 12)))
          (t
           (my-make-wired-tn 'double-float
                             'double-int-carg-reg
                             (+ stack-frame-size 4))))))

(define-alien-type-method (single-float :arg-tn) (type state)
  (declare (ignore type))
  (let ((stack-frame-size (arg-state-stack-frame-size state))
        (float-args (arg-state-float-args state)))
    (setf (arg-state-stack-frame-size state) (1+ stack-frame-size))
    (setf (arg-state-float-args state) (1+ float-args))
    (cond ((>= stack-frame-size 4)
           (my-make-wired-tn 'single-float
                             'single-stack
                             stack-frame-size))
          ((and (not (arg-state-did-int-arg state))
                (< float-args 2))
           (my-make-wired-tn 'single-float
                             'single-reg
                             (+ (* float-args 2) 12)))
          (t
           (my-make-wired-tn 'single-float
                             'single-int-carg-reg
                             (+ stack-frame-size 4))))))

(defstruct result-state
  (num-results 0))

(defun result-reg-offset (slot)
  (ecase slot
    (0 nl0-offset)
    (1 nl1-offset)))

(define-alien-type-method (integer :result-tn) (type state)
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (multiple-value-bind (ptype reg-sc)
        (if (alien-integer-type-signed type)
            (values 'signed-byte-32 'signed-reg)
            (values 'unsigned-byte-32 'unsigned-reg))
      (my-make-wired-tn ptype reg-sc (result-reg-offset num-results)))))

(define-alien-type-method (system-area-pointer :result-tn) (type state)
  (declare (ignore type))
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (my-make-wired-tn 'system-area-pointer 'sap-reg (result-reg-offset num-results))))

;;; FIXME: do these still work? -- CSR, 2002-08-28
(define-alien-type-method (double-float :result-tn) (type state)
  (declare (ignore type))
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (my-make-wired-tn 'double-float 'double-reg (* num-results 2))))

(define-alien-type-method (single-float :result-tn) (type state)
  (declare (ignore type))
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (my-make-wired-tn 'single-float 'single-reg (* num-results 2))))

(define-alien-type-method (values :result-tn) (type state)
  (let ((values (alien-values-type-values type)))
    (when (> (length values) 2)
      (error "Too many result values from c-call."))
    (mapcar #'(lambda (type)
                (invoke-alien-type-method :result-tn type state))
            values)))

(!def-vm-support-routine make-call-out-tns (type)
  (let ((arg-state (make-arg-state)))
    (collect ((arg-tns))
      (dolist (arg-type (alien-fun-type-arg-types type))
        (arg-tns (invoke-alien-type-method :arg-tn arg-type arg-state)))
      (values (my-make-wired-tn 'positive-fixnum 'any-reg nsp-offset)
              (* (max (arg-state-stack-frame-size arg-state) 4) n-word-bytes)
              (arg-tns)
              (invoke-alien-type-method :result-tn
                                        (alien-fun-type-result-type type)
                                        (make-result-state))))))

(deftransform %alien-funcall ((function type &rest args))
  (aver (sb!c::constant-lvar-p type))
  (let* ((type (sb!c::lvar-value type))
         (env (sb!kernel:make-null-lexenv))
         (arg-types (alien-fun-type-arg-types type))
         (result-type (alien-fun-type-result-type type)))
    (aver (= (length arg-types) (length args)))
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
                            ;; consecutive locations, endian word order,
                            ;; aligned to 8 bytes.
                            (when (oddp (length (new-args)))
                              (new-args nil))
                            #!-little-endian
                            (progn (new-args `(ash ,arg -32))
                                   (new-args `(logand ,arg #xffffffff))
                                   (if (oddp (length (new-arg-types)))
                                       (new-arg-types (parse-alien-type '(unsigned 32) env)))
                                   (if (alien-integer-type-signed type)
                                       (new-arg-types (parse-alien-type '(signed 32) env))
                                       (new-arg-types (parse-alien-type '(unsigned 32) env)))
                                   (new-arg-types (parse-alien-type '(unsigned 32) env)))
                            #!+little-endian
                            (progn (new-args `(logand ,arg #xffffffff))
                                   (new-args `(ash ,arg -32))
                                   (if (oddp (length (new-arg-types)))
                                       (new-arg-types (parse-alien-type '(unsigned 32) env)))
                                   (new-arg-types (parse-alien-type '(unsigned 32) env))
                                   (if (alien-integer-type-signed type)
                                       (new-arg-types (parse-alien-type '(signed 32) env))
                                       (new-arg-types (parse-alien-type '(unsigned 32) env)))))
                           (t
                            (new-args arg)
                            (new-arg-types type)))))
                 (cond ((and (alien-integer-type-p result-type)
                             (> (sb!alien::alien-integer-type-bits result-type) 32))
                        (let ((new-result-type
                               (let ((sb!alien::*values-type-okay* t))
                                 (parse-alien-type
                                  (if (alien-integer-type-signed result-type)
                                      #!-little-endian
                                      '(values (signed 32) (unsigned 32))
                                      #!+little-endian
                                      '(values (unsigned 32) (signed 32))
                                      '(values (unsigned 32) (unsigned 32)))
                                  env))))
                          `(lambda (function type ,@(lambda-vars))
                            (declare (ignore type))
                             (multiple-value-bind
                               #!-little-endian
                               (high low)
                               #!+little-endian
                               (low high)
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

#!+linkage-table
(define-vop (foreign-symbol-dataref-sap)
  (:translate foreign-symbol-dataref-sap)
  (:policy :fast-safe)
  (:args)
  (:arg-types (:constant simple-string))
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:temporary (:scs (non-descriptor-reg)) addr)
  (:generator 2
    (inst li addr (make-fixup foreign-symbol :foreign-dataref))
    (loadw res addr)))

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
      (inst jal (make-fixup "call_into_c" :foreign))
      (move cfunc function t)
      (when cur-nfp
        (load-stack-tn cur-nfp nfp-save)))))

(define-vop (alloc-number-stack-space)
  (:info amount)
  (:results (result :scs (sap-reg any-reg)))
  (:temporary (:scs (unsigned-reg) :to (:result 0)) temp)
  (:generator 0
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount 7) 7)))
        (cond ((< delta (ash 1 15))
               (inst subu nsp-tn delta))
              (t
               (inst li temp delta)
               (inst subu nsp-tn temp)))))
    (move result nsp-tn)))

(define-vop (dealloc-number-stack-space)
  (:info amount)
  (:policy :fast-safe)
  (:temporary (:scs (unsigned-reg) :to (:result 0)) temp)
  (:generator 0
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount 7) 7)))
        (cond ((< delta (ash 1 15))
               (inst addu nsp-tn delta))
              (t
               (inst li temp delta)
               (inst addu nsp-tn temp)))))))
