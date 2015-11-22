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

;;; beware that we deal alot here with register-offsets directly
;;; instead of their symbol-name in vm.lisp
;;; offset works differently depending on sc-type
(defun my-make-wired-tn (prim-type-name sc-name offset state)
  (make-wired-tn (primitive-type-or-lose prim-type-name)
                 (sc-number-or-lose sc-name)
                 ;; try to utilize vm.lisp definitions of registers:
                 (ecase sc-name
                   ((any-reg sap-reg signed-reg unsigned-reg)
                     (ecase offset ; FIX: port to other arch ???
                       ;(:nfp-offset offset)
                       (0 nl0-offset) ; On other arch we can
                       (1 nl1-offset) ; just add an offset to
                       (2 nl2-offset) ; beginning of args, but on
                       (3 nl3-offset) ; hppa c-args are spread.
                       (4 nl4-offset) ; These two are for
                       (5 nl5-offset))) ; c-return values
                   ((single-int-carg-reg double-int-carg-reg)
                     (ecase offset ; FIX: port to other arch ???
                       (0 nl0-offset)
                       (1 nl1-offset)
                       (2 nl2-offset)
                       (3 nl3-offset)))
                   ((single-reg double-reg) ; only for return
                     (+ 4 offset))
                   ;; A tn of stack type tells us that we have data on
                   ;; stack. This offset is current argument number so
                   ;; -1 points to the correct place to write that data
                   ((sap-stack signed-stack unsigned-stack)
                     (- (arg-state-nargs state) offset 8 1)))))

(defstruct arg-state
  (stack-frame-size 0)
  (float-args 0)
  nargs)

(define-alien-type-method (integer :arg-tn) (type state)
  (let ((stack-frame-size (arg-state-stack-frame-size state)))
    (setf (arg-state-stack-frame-size state) (1+ stack-frame-size))
    (multiple-value-bind
      (ptype reg-sc stack-sc)
      (if (alien-integer-type-signed type)
        (values 'signed-byte-32 'signed-reg 'signed-stack)
        (values 'unsigned-byte-32 'unsigned-reg 'unsigned-stack))
      (if (< stack-frame-size 4)
        (my-make-wired-tn ptype reg-sc stack-frame-size state)
        (my-make-wired-tn ptype stack-sc stack-frame-size state)))))

(define-alien-type-method (system-area-pointer :arg-tn) (type state)
  (declare (ignore type))
  (let ((stack-frame-size (arg-state-stack-frame-size state)))
    (setf (arg-state-stack-frame-size state) (1+ stack-frame-size))
    (if (< stack-frame-size 4)
      (my-make-wired-tn 'system-area-pointer
                        'sap-reg
                        stack-frame-size state)
      (my-make-wired-tn 'system-area-pointer
                        'sap-stack
                        stack-frame-size state))))

(define-alien-type-method (double-float :arg-tn) (type state)
  (declare (ignore type))
  (let ((stack-frame-size (logandc2 (1+ (arg-state-stack-frame-size state)) 1))
        (float-args (arg-state-float-args state)))
    (setf (arg-state-stack-frame-size state) (+ stack-frame-size 2))
    (setf (arg-state-float-args state) (+ 2 float-args))
    (cond ((>= stack-frame-size 4)
           (my-make-wired-tn 'double-float
                             'double-stack
                             stack-frame-size state))
          (t
            (my-make-wired-tn 'double-float
                              'double-reg
                              (1+ float-args) state)))))

(define-alien-type-method (single-float :arg-tn) (type state)
  (declare (ignore type))
  (let ((stack-frame-size (arg-state-stack-frame-size state))
        (float-args (arg-state-float-args state)))
    (setf (arg-state-stack-frame-size state) (1+ stack-frame-size))
    (setf (arg-state-float-args state) (1+ float-args))
    (cond ((>= stack-frame-size 4)
           (my-make-wired-tn 'single-float
                             'single-stack
                             stack-frame-size state))
          (t
            (my-make-wired-tn 'single-float
                              'single-reg
                              float-args state)))))

(defstruct result-state
  (num-results 0))

(define-alien-type-method (integer :result-tn) (type state)
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (multiple-value-bind (ptype reg-sc)
      (if (alien-integer-type-signed type)
        (values 'signed-byte-32 'signed-reg)
        (values 'unsigned-byte-32 'unsigned-reg))
      (if (> num-results 1) (error "Too many result values from c-call."))
      (my-make-wired-tn ptype reg-sc (+ num-results 4) state))))

(define-alien-type-method (system-area-pointer :result-tn) (type state)
  (declare (ignore type))
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (if (> num-results 1) (error "Too many result values from c-call."))
    (my-make-wired-tn 'system-area-pointer 'sap-reg (+ num-results 4) state)))

(define-alien-type-method (double-float :result-tn) (type state)
  (declare (ignore type))
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (my-make-wired-tn 'double-float 'double-reg (* num-results 2) state)))

(define-alien-type-method (single-float :result-tn) (type state)
  (declare (ignore type))
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (my-make-wired-tn 'single-float 'single-reg (* num-results 2) state)))

(define-alien-type-method (values :result-tn) (type state)
  (let ((values (alien-values-type-values type)))
    (when (> (length values) 2)
      (error "Too many result values from c-call."))
    (mapcar (lambda (type)
              (invoke-alien-type-method :result-tn type state))
            values)))

(defun make-call-out-tns (type)
  (let ((arg-state (make-arg-state))
        (nargs 0))
    (dolist (arg-type (alien-fun-type-arg-types type))
      (cond
        ((alien-double-float-type-p arg-type)
          (incf nargs (logior (1+ nargs) 1)))
        (t (incf nargs))))
    (setf (arg-state-nargs arg-state) (logandc2 (+ nargs 8 15) 15))
    (collect ((arg-tns))
      (dolist (arg-type (alien-fun-type-arg-types type))
        (arg-tns (invoke-alien-type-method :arg-tn arg-type arg-state)))
      (values (make-normal-tn *fixnum-primitive-type*)
              (* n-word-bytes (logandc2 (+ nargs 8 15) 15))
              (arg-tns)
              (invoke-alien-type-method :result-tn
                                        (alien-fun-type-result-type type)
                                        (make-result-state))))))

(deftransform %alien-funcall ((function type &rest args))
  (aver (sb!c::constant-lvar-p type))
  (let* ((type (sb!c::lvar-value type))
         (env (make-null-lexenv))
         (arg-types (alien-fun-type-arg-types type))
         (result-type (alien-fun-type-result-type type)))
    (aver (= (length arg-types) (length args)))
    ;; We need to do something special for 64-bit integer arguments
    ;; and results.
    (if (or (some (lambda (type)
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
                            (progn (new-args `(ash ,arg -32))
                                   (new-args `(logand ,arg #xffffffff))
                                   (if (oddp (length (new-arg-types)))
                                       (new-arg-types (parse-alien-type '(unsigned 32) env)))
                                   (if (alien-integer-type-signed type)
                                       (new-arg-types (parse-alien-type '(signed 32) env))
                                       (new-arg-types (parse-alien-type '(unsigned 32) env)))
                                   (new-arg-types (parse-alien-type '(unsigned 32) env))))
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
                                  env))))
                          `(lambda (function type ,@(lambda-vars))
                            (declare (ignore type))
                             (multiple-value-bind
                               (high low)
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
    (inst li (make-fixup foreign-symbol :foreign) res)))

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
    (inst li (make-fixup foreign-symbol :foreign-dataref) addr)
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
  ;; Not sure if using nargs is safe ( have we saved it ).
  ;; but we cant use any non-descriptor-reg because c-args nl-4 is of that type
  (:temporary (:sc non-descriptor-reg :offset nargs-offset) temp)
  (:vop-var vop)
  (:generator 0
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (store-stack-tn nfp-save cur-nfp))
      (let ((fixup (make-fixup "call_into_c" :foreign)))
        (inst ldil fixup temp)
        (inst ble fixup c-text-space temp)
        (move function cfunc t))
      (when cur-nfp
        (load-stack-tn cur-nfp nfp-save)))))

(define-vop (alloc-number-stack-space)
  (:info amount)
  (:result-types system-area-pointer)
  (:results (result :scs (sap-reg any-reg)))
  (:temporary (:scs (unsigned-reg) :to (:result 0)) temp)
  (:generator 0
    ;; Because stack grows to higher addresses, we have the result
    ;; pointing to an lowerer address than nsp
    (move nsp-tn result)
    (unless (zerop amount)
      ;; hp-ux stack grows towards larger addresses and stack must be
      ;; allocated in blocks of 64 bytes
      (let ((delta (+ 0 (logandc2 (+ amount 63) 63)))) ; was + 16
        (cond ((< delta (ash 1 10))
               (inst addi delta nsp-tn nsp-tn))
              (t
               (inst li delta temp)
               (inst add nsp-tn temp nsp-tn)))))))

(define-vop (dealloc-number-stack-space)
  (:info amount)
  (:policy :fast-safe)
  (:temporary (:scs (unsigned-reg) :to (:result 0)) temp)
  (:generator 0
    (unless (zerop amount)
      (let ((delta (+ 0 (logandc2 (+ amount 63) 63)))) ; was + 16
        (cond ((< delta (ash 1 10))
               (inst addi (- delta) nsp-tn nsp-tn))
              (t
               (inst li (- delta) temp)
               (inst sub nsp-tn temp nsp-tn)))))))

#-sb-xc-host
(defun alien-callback-accessor-form (type sap offset)
  (let ((parsed-type type))
    (if (alien-integer-type-p parsed-type)
        (let ((bits (sb!alien::alien-integer-type-bits parsed-type)))
               (let ((byte-offset
                      (cond ((< bits n-word-bits)
                             (- n-word-bytes
                                (ceiling bits n-byte-bits)))
                            (t 0))))
                 `(deref (sap-alien (sap+ ,sap
                                          ,(+ byte-offset offset))
                                    (* ,type)))))
        `(deref (sap-alien (sap+ ,sap ,offset) (* ,type))))))

