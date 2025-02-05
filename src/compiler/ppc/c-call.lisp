;;;; VOPs and other machine-specific support routines for call-out to C

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;; Return the number of bytes needed for the current non-descriptor
;;; stack frame.  Non-descriptor stack frames must be multiples of 16
;;; bytes under the PPC SVr4 ABI (though the EABI may be less
;;; restrictive).  On linux, two words are reserved for the stack
;;; backlink and saved LR (see SB-VM::NUMBER-STACK-DISPLACEMENT).

(defconstant +stack-alignment-bytes+
  ;; Duh.  PPC Linux (and VxWorks) adhere to the EABI.
  #-darwin 7
  ;; But Darwin doesn't
  #+darwin 15)

(defstruct arg-state
  (gpr-args 0)
  (fpr-args 0)
  ;; SVR4 [a]abi wants two words on stack (callee saved lr,
  ;; backpointer).
  #-darwin (stack-frame-size 2)
  ;; PowerOpen ABI wants 8 words on the stack corresponding to GPR3-10
  ;; in addition to the 6 words of link area (see number-stack-displacement)
  #+darwin (stack-frame-size (+ 8 6)))

(defun int-arg (state prim-type reg-sc stack-sc)
  (let ((reg-args (arg-state-gpr-args state)))
    (cond ((< reg-args 8)
           (setf (arg-state-gpr-args state) (1+ reg-args))
           (make-wired-tn* prim-type reg-sc (+ reg-args nl0-offset)))
          (t
           (let ((frame-size (arg-state-stack-frame-size state)))
             (setf (arg-state-stack-frame-size state) (1+ frame-size))
             (make-wired-tn* prim-type stack-sc frame-size))))))

(define-alien-type-method (integer :arg-tn) (type state)
  (if (alien-integer-type-signed type)
      (int-arg state 'signed-byte-32 signed-reg-sc-number signed-stack-sc-number)
      (int-arg state 'unsigned-byte-32 unsigned-reg-sc-number unsigned-stack-sc-number)))

(define-alien-type-method (system-area-pointer :arg-tn) (type state)
  (declare (ignore type))
  (int-arg state 'system-area-pointer sap-reg-sc-number sap-stack-sc-number))

;;; The Linux/PPC 32bit ABI says:
;;;
;;;   If a single-float arg has to go on the stack, it's promoted to
;;;   a double.
;;;
;;; gcc does:
;;;
;;;   Excess floats stored on the stack are stored as floats.
;;;
;;; We follow gcc.
#-darwin
(define-alien-type-method (single-float :arg-tn) (type state)
  (declare (ignore type))
  (let* ((fprs (arg-state-fpr-args state)))
    (cond ((< fprs 8)
           (incf (arg-state-fpr-args state))
           ;; Assign outgoing FPRs starting at FP1
           (make-wired-tn* 'single-float single-reg-sc-number (1+ fprs)))
          (t
           (let* ((stack-offset (arg-state-stack-frame-size state)))
             (setf (arg-state-stack-frame-size state) (+ stack-offset 1))
             (make-wired-tn* 'single-float single-stack-sc-number stack-offset))))))

;;; If a single-float arg has to go on the stack, it's promoted to
;;; double.  That way, C programs can get subtle rounding errors when
;;; unrelated arguments are introduced.
#+darwin
(define-alien-type-method (single-float :arg-tn) (type state)
  (declare (ignore type))
  (let* ((fprs (arg-state-fpr-args state))
         (gprs (arg-state-gpr-args state)))
    (cond ((< gprs 8) ; and by implication also (< fprs 13)
           (incf (arg-state-fpr-args state))
           ;; Assign outgoing FPRs starting at FP1
           (list (make-wired-tn* 'single-float single-reg-sc-number (1+ fprs))
                 (int-arg state 'signed-byte-32 signed-reg-sc-number signed-stack-sc-number)))
          ((< fprs 13)
           ;; See comments below for double-float.
           (incf (arg-state-fpr-args state))
           (incf (arg-state-stack-frame-size state))
           (make-wired-tn* 'single-float single-reg-sc-number (1+ fprs)))
          (t
           ;; Pass on stack only
           (let ((stack-offset (arg-state-stack-frame-size state)))
             (incf (arg-state-stack-frame-size state))
             (make-wired-tn* 'single-float single-stack-sc-number stack-offset))))))

#-darwin
(define-alien-type-method (double-float :arg-tn) (type state)
  (declare (ignore type))
  (let* ((fprs (arg-state-fpr-args state)))
    (cond ((< fprs 8)
           (incf (arg-state-fpr-args state))
           ;; Assign outgoing FPRs starting at FP1
           (make-wired-tn* 'double-float double-reg-sc-number (1+ fprs)))
          (t
           (let* ((stack-offset (arg-state-stack-frame-size state)))
             (if (oddp stack-offset)
               (incf stack-offset))
             (setf (arg-state-stack-frame-size state) (+ stack-offset 2))
             (make-wired-tn* 'double-float double-stack-sc-number stack-offset))))))

#+darwin
(define-alien-type-method (double-float :arg-tn) (type state)
  (declare (ignore type))
  (let ((fprs (arg-state-fpr-args state))
        (gprs (arg-state-gpr-args state)))
    (cond ((< gprs 8) ; and by implication also (< fprs 13)
           (incf (arg-state-fpr-args state))
           ;; Assign outgoing FPRs starting at FP1
           ;;
           ;; The PowerOpen ABI says float values are stored in float
           ;; regs.  But if we're calling a varargs function, we also
           ;; need to put the float into some gprs.  We indicate this
           ;; to %alien-funcall ir2-convert by making a list of the
           ;; TNs for the float reg and for the int regs.
           ;;
           (list (make-wired-tn* 'double-float double-reg-sc-number (1+ fprs))
                 (int-arg state 'signed-byte-32 signed-reg-sc-number signed-stack-sc-number)
                 (int-arg state 'unsigned-byte-32 unsigned-reg-sc-number unsigned-stack-sc-number)))
          ((< fprs 13)
           (incf (arg-state-fpr-args state))
           (list (make-wired-tn* 'double-float double-reg-sc-number (1+ fprs))
                 (int-arg state 'signed-byte-32 signed-reg-sc-number signed-stack-sc-number)
                 (int-arg state 'unsigned-byte-32 unsigned-reg-sc-number unsigned-stack-sc-number)))
          (t
           ;; Pass on stack only
           (let ((stack-offset (arg-state-stack-frame-size state)))
             (incf (arg-state-stack-frame-size state) 2)
             (make-wired-tn* 'double-float double-stack-sc-number stack-offset))))))

;;; Result state handling

(defstruct result-state
  (num-results 0))

(defun result-reg-offset (slot)
  (ecase slot
    (0 nl0-offset)
    (1 nl1-offset)))

;;; FIXME: These #-DARWIN methods should be adjusted to take a state
;;; argument, firstly because that's our "official" API (see
;;; src/code/host-alieneval) and secondly because that way we can
;;; probably have less duplication of code.  -- CSR, 2003-07-29

(define-alien-type-method (system-area-pointer :result-tn) (type state)
  (declare (ignore type))
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (make-wired-tn* 'system-area-pointer sap-reg-sc-number
                      (result-reg-offset num-results))))

(define-alien-type-method (single-float :result-tn) (type state)
  (declare (ignore type state))
  (make-wired-tn* 'single-float single-reg-sc-number 1))

(define-alien-type-method (double-float :result-tn) (type state)
  (declare (ignore type state))
  (make-wired-tn* 'double-float double-reg-sc-number 1))

(define-alien-type-method (values :result-tn) (type state)
  (let ((values (alien-values-type-values type)))
    (when (> (length values) 2)
      (error "Too many result values from c-call."))
    (mapcar #'(lambda (type)
                (invoke-alien-type-method :result-tn type state))
            values)))

(define-alien-type-method (integer :result-tn) (type state)
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (multiple-value-bind (ptype reg-sc)
        (if (alien-integer-type-signed type)
            (values 'signed-byte-32 signed-reg-sc-number)
            (values 'unsigned-byte-32 unsigned-reg-sc-number))
      (make-wired-tn* ptype reg-sc (result-reg-offset num-results)))))

(defun make-call-out-tns (type)
  (declare (type alien-fun-type type))
  (let ((arg-state (make-arg-state)))
    (collect ((arg-tns))
      (dolist (arg-type (alien-fun-type-arg-types type))
        (arg-tns (invoke-alien-type-method :arg-tn arg-type arg-state)))
      (values (make-wired-tn* 'positive-fixnum any-reg-sc-number nsp-offset)
              (* (arg-state-stack-frame-size arg-state) n-word-bytes)
              (arg-tns)
              (invoke-alien-type-method
               :result-tn
               (alien-fun-type-result-type type)
               (make-result-state))))))


;;; Sort out long longs, by splitting them up.  However, need to take
;;; care about register/stack alignment and whether they will fully
;;; fit into registers or must go on the stack.
#-darwin
(deftransform %alien-funcall ((function type &rest args))
  (aver (sb-c:constant-lvar-p type))
  (let* ((type (sb-c:lvar-value type))
         (arg-types (alien-fun-type-arg-types type))
         (result-type (alien-fun-type-result-type type))
         (gprs 0)
         (fprs 0)
         (stack 0))
    (aver (= (length arg-types) (length args)))
    ;; We need to do something special for 64-bit integer arguments
    ;; and results.
    (if (or (some #'(lambda (type)
                      (and (alien-integer-type-p type)
                           (> (sb-alien::alien-integer-type-bits type) 32)))
                  arg-types)
            (and (alien-integer-type-p result-type)
                 (> (sb-alien::alien-integer-type-bits result-type) 32)))
        (collect ((new-args) (lambda-vars) (new-arg-types))
          (dolist (type arg-types)
            (let ((arg (gensym)))
              (lambda-vars arg)
              (cond ((and (alien-integer-type-p type)
                          (> (sb-alien::alien-integer-type-bits type) 32))
                     (when (or
                            (oddp gprs)
                            (and
                             (oddp stack)
                             (> gprs 7)))
                       ;; Need to pad for alignment.
                       (if (oddp gprs)
                           (incf gprs)
                           (incf stack))
                       (new-args 0)
                       (new-arg-types (parse-alien-type
                                       '(unsigned 32) nil)))
                     (if (< gprs 8)
                         (incf gprs 2)
                         (incf stack 2))
                     (new-args `(ash ,arg -32))
                     (new-args `(logand ,arg #xffffffff))
                     (if (alien-integer-type-signed type)
                         (new-arg-types (parse-alien-type
                                         '(signed 32) nil))
                         (new-arg-types (parse-alien-type
                                         '(unsigned 32) nil)))
                     (new-arg-types (parse-alien-type
                                     '(unsigned 32) nil)))
                    ((alien-single-float-type-p type)
                     (if (< fprs 8)
                         (incf fprs)
                         (incf stack))
                     (new-args arg)
                     (new-arg-types type))
                    ((alien-double-float-type-p type)
                     (if (< fprs 8)
                         (incf fprs)
                         (if (oddp stack)
                             (incf stack 3)   ; Doubles are aligned on
                             (incf stack 2))) ; the stack.
                     (new-args arg)
                     (new-arg-types type))
                    (t ;; integer or SAP
                     (if (< gprs 8)
                         (incf gprs 1)
                         (incf stack 1))
                     (new-args arg)
                     (new-arg-types type)))))
                 (cond ((and (alien-integer-type-p result-type)
                             (> (sb-alien::alien-integer-type-bits result-type) 32))
                        (let ((new-result-type
                               (let ((sb-alien::*values-type-okay* t))
                                 (parse-alien-type
                                  (if (alien-integer-type-signed result-type)
                                      '(values (signed 32) (unsigned 32))
                                      '(values (unsigned 32) (unsigned 32)))
                                  nil))))
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
        (sb-c::give-up-ir1-transform))))

#+darwin
(deftransform %alien-funcall ((function type &rest args))
  (aver (sb-c:constant-lvar-p type))
  (let* ((type (sb-c:lvar-value type))
         (arg-types (alien-fun-type-arg-types type))
         (result-type (alien-fun-type-result-type type)))
    (aver (= (length arg-types) (length args)))
    ;; We need to do something special for 64-bit integer arguments
    ;; and results.
    (if (or (some #'(lambda (type)
                      (and (alien-integer-type-p type)
                           (> (sb-alien::alien-integer-type-bits type) 32)))
                  arg-types)
            (and (alien-integer-type-p result-type)
                 (> (sb-alien::alien-integer-type-bits result-type) 32)))
        (collect ((new-args) (lambda-vars) (new-arg-types))
                 (dolist (type arg-types)
                   (let ((arg (gensym)))
                     (lambda-vars arg)
                     (cond ((and (alien-integer-type-p type)
                                 (> (sb-alien::alien-integer-type-bits type) 32))
                            ;; 64-bit long long types are stored in
                            ;; consecutive locations, most significant word
                            ;; first (big-endian).
                            (new-args `(ash ,arg -32))
                            (new-args `(logand ,arg #xffffffff))
                            (if (alien-integer-type-signed type)
                                (new-arg-types (parse-alien-type '(signed 32) nil))
                                (new-arg-types (parse-alien-type '(unsigned 32) nil)))
                            (new-arg-types (parse-alien-type '(unsigned 32) nil)))
                           (t
                            (new-args arg)
                            (new-arg-types type)))))
                 (cond ((and (alien-integer-type-p result-type)
                             (> (sb-alien::alien-integer-type-bits result-type) 32))
                        (let ((new-result-type
                               (let ((sb-alien::*values-type-okay* t))
                                 (parse-alien-type
                                  (if (alien-integer-type-signed result-type)
                                      '(values (signed 32) (unsigned 32))
                                      '(values (unsigned 32) (unsigned 32)))
                                  nil))))
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
        (sb-c::give-up-ir1-transform))))

(define-vop (foreign-symbol-sap)
  (:translate foreign-symbol-sap)
  (:policy :fast-safe)
  (:args)
  (:arg-types (:constant simple-string))
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (inst lr res  (make-fixup foreign-symbol :foreign))))

(define-vop (foreign-symbol-dataref-sap)
  (:translate foreign-symbol-dataref-sap)
  (:policy :fast-safe)
  (:args)
  (:arg-types (:constant simple-string))
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (inst lr res (make-fixup foreign-symbol :foreign-dataref))
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
  (:result-types system-area-pointer)
  (:temporary (:scs (unsigned-reg) :to (:result 0)) temp)
  (:generator 0
    (unless (zerop amount)
      ;; FIXME: I don't understand why we seem to be adding
      ;; NUMBER-STACK-DISPLACEMENT twice here.  Weird.  -- CSR,
      ;; 2003-08-20
      (let ((delta (- (logandc2 (+ amount number-stack-displacement
                                   +stack-alignment-bytes+)
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
      (let ((delta (logandc2 (+ amount number-stack-displacement
                                +stack-alignment-bytes+)
                             +stack-alignment-bytes+)))
        (cond ((< delta (ash 1 16))
               (inst addi nsp-tn nsp-tn delta))
              (t
               (inst lwz nsp-tn nsp-tn 0)))))))

#-sb-xc-host
(progn
  (defun alien-callback-accessor-form (type sap offset)
    (let ((parsed-type (parse-alien-type type nil)))
      (cond ((sb-alien::alien-integer-type-p parsed-type)
             ;; Unaligned access is slower, but possible, so this is nice and
             ;; simple. Also, we're a big-endian machine, so we need to get
             ;; byte offsets correct.
             (let ((bits (sb-alien::alien-type-bits parsed-type)))
               (let ((byte-offset
                      (cond ((< bits n-word-bits)
                             (- n-word-bytes
                                (ceiling bits n-byte-bits)))
                            (t 0))))
                 `(deref (sap-alien (sap+ ,sap
                                          ,(+ byte-offset offset))
                                    (* ,type))))))
            (t
             `(deref (sap-alien (sap+ ,sap ,offset) (* ,type)))))))

  ;;; The "Mach-O Runtime Conventions" document for OS X almost
  ;;; specifies the calling convention (it neglects to mention that
  ;;; the linkage area is 24 bytes).
  #+darwin
  (defconstant n-foreign-linkage-area-bytes 24)

  ;;; On linux only use 8 bytes for LR and Back chain.  JRXR
  ;;; 2006/11/10.
  #-darwin
  (defconstant n-foreign-linkage-area-bytes 8)

  ;;; Returns a vector in static space containing machine code for the
  ;;; callback wrapper.  Linux version.  JRXR.  2006/11/13
  #-darwin
  (defun alien-callback-assembler-wrapper (index result-type argument-types)
    (flet ((make-gpr (n)
             (make-random-tn (sc-or-lose 'any-reg) n))
           (make-fpr (n)
             (make-random-tn (sc-or-lose 'double-reg) n)))
      (let* ((segment (make-segment)))
        (assemble (segment 'nil)
          ;; Copy args from registers or stack to new position
          ;; on stack.
          (let* (
                 ;; Argument store.
                 (arg-store-size
                  (* n-word-bytes
                     (apply '+
                         (mapcar (lambda (type)
                                   (ceiling (alien-type-bits type)
                                            n-word-bits))
                                 argument-types ))))
                 ;; Return area allocation.
                 (n-return-area-words
                  (ceiling (or (alien-type-bits result-type) 0) n-word-bits))
                 (n-return-area-bytes (* n-return-area-words
                                         n-word-bytes))
                 ;; FIXME: magic constant, and probably n-args-bytes
                 ;; JRXR: What's this for?  Copied from Darwin.
                 (args-size (* 3 n-word-bytes))
                 (frame-size (logandc2
                              (+ arg-store-size
                                 n-return-area-bytes
                                 args-size
                                 number-stack-displacement
                                 +stack-alignment-bytes+)
                              +stack-alignment-bytes+))
                 (return-area-pos (- frame-size
                                     number-stack-displacement
                                     args-size))
                 (arg-store-pos (- return-area-pos
                                   n-return-area-bytes))
                 (stack-pointer (make-gpr 1))
                 (r0 (make-gpr 0))
                 (f0 (make-fpr 0))
                 (in-words-processed 0)
                 (out-words-processed 0)
                 (gprs (mapcar #'make-gpr '(3 4 5 6 7 8 9 10)))
                 (fprs (mapcar #'make-fpr
                               '(1 2 3 4 5 6 7 8))) )
            ;; Setup useful functions and then copy all args.
            (flet ((load-address-into (reg addr)
                       (let ((high (ldb (byte 16 16) addr))
                             (low (ldb (byte 16 0) addr)))
                         (inst lis reg high)
                         (inst ori reg reg low)))
                   (save-arg (type words)
                     (let ((integerp (not (alien-float-type-p type)))
                           (in-offset (+ (* in-words-processed n-word-bytes)
                                         n-foreign-linkage-area-bytes))
                           (out-offset (- (* out-words-processed n-word-bytes)
                                          arg-store-pos)))
                       (cond (integerp
                              (if (and
                                   ;; Only upto long longs are passed
                                   ;; in registers.
                                   (<= words 2)
                                   ;; And needs space for whole arg,
                                   ;; including alignment.
                                   (<= (+ words
                                          (rem (length gprs) words))
                                       (length gprs)))
                                  (progn
                                    (if (/= 0
                                            (rem (length gprs) words))
                                        (pop gprs))
                                    (dotimes (k words)
                                      (let ((gpr (pop gprs)))
                                        (inst stw gpr stack-pointer
                                              out-offset))
                                      (incf out-words-processed)
                                      (incf out-offset n-word-bytes)))
                                  (progn
                                    ;; First ensure alignment.
                                    ;; FIXME!  If passing structures
                                    ;; becomes allowable, then this is
                                    ;; broken.
                                    (if (/= 0
                                            (rem in-words-processed
                                                 words))
                                        (progn
                                          (incf in-words-processed)
                                          (incf in-offset
                                                n-word-bytes)))
                                    (dotimes (k words)
                                      ;; Copy from memory to memory.
                                      (inst lwz r0 stack-pointer
                                            in-offset)
                                      (inst stw r0 stack-pointer
                                            out-offset)
                                      (incf out-words-processed)
                                      (incf out-offset n-word-bytes)
                                      (incf in-words-processed)
                                      (incf in-offset n-word-bytes)))))
                             ;; The handling of floats is a little ugly
                             ;; because we hard-code the number of words
                             ;; for single- and double-floats.
                             ((alien-single-float-type-p type)
                              (let ((fpr (pop fprs)))
                                (if fpr
                                    (inst stfs fpr stack-pointer out-offset)
                                    (progn
                                      ;; The ABI says that floats
                                      ;; stored on the stack are
                                      ;; promoted to doubles.  gcc
                                      ;; stores them as floats.
                                      ;; Follow gcc here.
                                      ;;  => no alignment needed either.
                                      (inst lfs f0
                                            stack-pointer in-offset)
                                      (inst stfs f0
                                            stack-pointer out-offset)
                                      (incf in-words-processed))))
                              (incf out-words-processed))
                             ((alien-double-float-type-p type)
                              (let ((fpr (pop fprs)))
                                (if fpr
                                    (inst stfd fpr stack-pointer out-offset)
                                    (progn
                                      ;; Ensure alignment.
                                      (if (oddp in-words-processed)
                                          (progn
                                            (incf in-words-processed)
                                            (incf in-offset n-word-bytes)))
                                      (inst lfd f0
                                            stack-pointer in-offset)
                                      (inst stfd f0
                                            stack-pointer out-offset)
                                      (incf in-words-processed 2))))
                              (incf out-words-processed 2))
                             (t
                              (bug "Unknown alien floating point type: ~S" type))))))
              (mapc #'save-arg
                    argument-types
                    (mapcar (lambda (arg)
                              (ceiling (alien-type-bits arg) n-word-bits))
                            argument-types))

              ;; Arranged the args, allocated the return area.  Now
              ;; actuall call callback_wrapper_trampoline:  callback_wrapper_trampoline (
              ;; index, args, return-area)

              (destructuring-bind (arg1 arg2 arg3)
                  (mapcar #'make-gpr '(3 4 5))
                (inst li arg1 (fixnumize index))
                (inst addi arg2 stack-pointer (- arg-store-pos))
                (inst addi arg3 stack-pointer (- return-area-pos)))

              ;; Setup everything.  Now save sp, setup the frame.
              (inst mflr r0)
              (inst stw r0 stack-pointer n-word-bytes)
              (inst stwu stack-pointer stack-pointer (- frame-size))

              ;; And make the call.
              (load-address-into
               r0
               (foreign-symbol-address "callback_wrapper_trampoline"))
              (inst mtlr r0)
              (inst blrl)

              ;; We're back!  Restore sp and lr, load the
              ;; return value from just under sp, and return.
              (inst lwz stack-pointer stack-pointer 0)
              (inst lwz r0 stack-pointer n-word-bytes)
              (inst mtlr r0)
              (cond
                ((sb-alien::alien-single-float-type-p result-type)
                 (let ((f1 (make-fpr 1)))
                   (inst lfs f1 stack-pointer (- return-area-pos))))
                ((sb-alien::alien-double-float-type-p result-type)
                 (let ((f1 (make-fpr 1)))
                   (inst lfd f1 stack-pointer (- return-area-pos))))
                ((sb-alien::alien-void-type-p result-type)
                 ;; Nothing to do
                 )
                (t
                 (loop with gprs = (mapcar #'make-gpr '(3 4))
                       for gpr = (pop gprs)
                       for offset from (- return-area-pos)
                       by n-word-bytes
                       repeat n-return-area-words
                       do
                       (unless gpr
                         (bug "Out of return registers in alien-callback trampoline."))
                       (inst lwz gpr stack-pointer offset))))
              (inst blr))))
        (finalize-segment segment)

        ;; Now that the segment is done, convert it to a static
        ;; vector we can point foreign code to.
        (let* ((buffer (sb-assem:segment-buffer segment))
               (vector (make-static-vector (length buffer)
                                           :element-type '(unsigned-byte 8)
                                           :initial-contents buffer))
               (sap (vector-sap vector)))
          (alien-funcall
           (extern-alien "ppc_flush_icache"
                                  (function void
                                            system-area-pointer
                                            unsigned-long))
           sap (length buffer))
          vector))))

  ;;; Returns a vector in static space containing machine code for the
  ;;; callback wrapper
  #+darwin
  (defun alien-callback-assembler-wrapper (index result-type argument-types)
    (flet ((make-gpr (n)
             (make-random-tn (sc-or-lose 'any-reg) n))
           (make-fpr (n)
             (make-random-tn (sc-or-lose 'double-reg) n)))
      (let* ((segment (make-segment)))
        (assemble (segment)
          ;; To save our arguments, we follow the algorithm sketched in the
          ;; "PowerPC Calling Conventions" section of that document.
          ;;
          ;; CLH: There are a couple problems here. First, we bail if
          ;; we run out of registers. AIUI, we can just ignore the extra
          ;; args here and we will be ok...
          (let ((words-processed 0)
                (gprs (mapcar #'make-gpr '(3 4 5 6 7 8 9 10)))
                (fprs (mapcar #'make-fpr '(1 2 3 4 5 6 7 8 9 10 11 12 13)))
                (stack-pointer (make-gpr 1)))
            (labels ((save-arg (type words)
                       (let ((integerp (not (alien-float-type-p type)))
                             (offset (+ (* words-processed n-word-bytes)
                                        n-foreign-linkage-area-bytes)))
                         (cond (integerp
                                (dotimes (k words)
                                  (let ((gpr (pop gprs)))
                                    (when gpr
                                      (inst stw gpr stack-pointer offset))
                                    (incf words-processed)
                                    (incf offset n-word-bytes))))
                               ;; The handling of floats is a little ugly
                               ;; because we hard-code the number of words
                               ;; for single- and double-floats.
                               ((alien-single-float-type-p type)
                                (pop gprs)
                                (let ((fpr (pop fprs)))
                                  (when fpr
                                    (inst stfs fpr stack-pointer offset)))
                                (incf words-processed))
                               ((alien-double-float-type-p type)
                                (setf gprs (cddr gprs))
                                (let ((fpr (pop fprs)))
                                  (when fpr
                                    (inst stfd fpr stack-pointer offset)))
                                (incf words-processed 2))
                               (t
                                (bug "Unknown alien floating point type: ~S" type))))))
              (mapc #'save-arg
                    argument-types
                    (mapcar (lambda (arg)
                              (ceiling (alien-type-bits arg) n-word-bits))
                            argument-types))))
          ;; Set aside room for the return area just below sp, then
          ;; actually call funcall3: funcall3 (call-alien-function,
          ;; index, args, return-area)
          ;;
          ;; INDEX is fixnumized, ARGS and RETURN-AREA don't need to be
          ;; because they're word-aligned. Kinda gross, but hey ...
          (let* ((n-return-area-words
                  (ceiling (or (alien-type-bits result-type) 0) n-word-bits))
                 (n-return-area-bytes (* n-return-area-words n-word-bytes))
                 ;; FIXME: magic constant, and probably n-args-bytes
                 (args-size (* 3 n-word-bytes))
                 ;; FIXME: n-frame-bytes?
                 (frame-size (logandc2 (+ n-foreign-linkage-area-bytes
                                          n-return-area-bytes
                                          args-size
                                          +stack-alignment-bytes+)
                                       +stack-alignment-bytes+)))
            (destructuring-bind (sp r0 arg1 arg2 arg3)
                (mapcar #'make-gpr '(1 0 3 4 5 6))
              ;; FIXME: This is essentially the same code as LR in
              ;; insts.lisp, but attempting to use (INST LR ...) instead
              ;; of this function results in callbacks not working.  Why?
              ;;   --njf, 2006-01-04
              (flet ((load-address-into (reg addr)
                       (let ((high (ldb (byte 16 16) addr))
                             (low (ldb (byte 16 0) addr)))
                         (inst lis reg high)
                         (inst ori reg reg low))))
                ;; Setup the args
                (inst li arg1 (fixnumize index))
                (inst addi arg2 sp n-foreign-linkage-area-bytes)
                ;; FIXME: This was (- (* RETURN-AREA-SIZE N-WORD-BYTES)), while
                ;; RETURN-AREA-SIZE was (* N-RETURN-AREA-WORDS N-WORD-BYTES):
                ;; I assume the intention was (- N-RETURN-AREA-BYTES), but who knows?
                ;; --NS 2005-06-11
                (inst addi arg3 sp (- n-return-area-bytes))
                ;; FIXME! FIXME FIXME: What does this FIXME refer to?
                ;; Save sp, setup the frame
                (inst mflr r0)
                (inst stw r0 sp (* 2 n-word-bytes)) ; FIXME: magic constant
                (inst stwu sp sp (- frame-size))
                ;; Make the call
                (load-address-into r0 (foreign-symbol-address "callback_wrapper_trampoline"))
                (inst mtlr r0)
                (inst blrl))
              ;; We're back!  Restore sp and lr, load the return value from just
              ;; under sp, and return.
              (inst lwz sp sp 0)
              (inst lwz r0 sp (* 2 n-word-bytes))
              (inst mtlr r0)
              (cond
                ((sb-alien::alien-single-float-type-p result-type)
                 (let ((f1 (make-fpr 1)))
                   (inst lfs f1 sp (- (* n-return-area-words n-word-bytes)))))
                ((sb-alien::alien-double-float-type-p result-type)
                 (let ((f1 (make-fpr 1)))
                   (inst lfd f1 sp (- (* n-return-area-words n-word-bytes)))))
                ((sb-alien::alien-void-type-p result-type)
                 ;; Nothing to do
                 )
                (t
                 (loop with gprs = (mapcar #'make-gpr '(3 4))
                    repeat n-return-area-words
                    for gpr = (pop gprs)
                    for offset from (- (* n-return-area-words n-word-bytes))
                    by n-word-bytes
                    do
                      (unless gpr
                        (bug "Out of return registers in alien-callback trampoline."))
                      (inst lwz gpr sp offset))))
              (inst blr))))
        (finalize-segment segment)
        ;; Now that the segment is done, convert it to a static
        ;; vector we can point foreign code to.
        (let* ((buffer (sb-assem:segment-buffer segment))
               (vector (make-static-vector (length buffer)
                                           :element-type '(unsigned-byte 8)
                                           :initial-contents buffer))
               (sap (vector-sap vector)))
          (alien-funcall
           (extern-alien "ppc_flush_icache"
                         (function void
                                   system-area-pointer
                                   unsigned-long))
           sap (length buffer))
          vector)))))
