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

;;; Return the number of bytes needed for the current non-descriptor
;;; stack frame.  Non-descriptor stack frames must be multiples of 16
;;; bytes under the PPC SVr4 ABI (though the EABI may be less
;;; restrictive).  On linux, two words are reserved for the stack
;;; backlink and saved LR (see SB!VM::NUMBER-STACK-DISPLACEMENT).

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
  (aver (sb!c::constant-lvar-p type))
  (let* ((type (sb!c::lvar-value type))
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
    (inst lr addr (make-fixup foreign-symbol :foreign-dataref))
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
    ;; Unaligned access is slower, but possible, so this is nice and simple.
    `(deref (sap-alien (sap+ ,sap ,offset) (* ,type))))

  ;;; The "Mach-O Runtime Conventions" document for OS X almost specifies
  ;;; the calling convention (it neglects to mention that the linkage
  ;;; area is 24 bytes).
  (defconstant n-foreign-linkage-area-bytes 24)

  ;;; Returns a vector in static space containing machine code for the
  ;;; callback wrapper
  (defun alien-callback-assembler-wrapper (index result-type argument-types)
    (flet ((make-gpr (n)
             (make-random-tn :kind :normal :sc (sc-or-lose 'any-reg) :offset n))
           (make-fpr (n)
             (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg) :offset n)))
      (let* ((segment (make-segment)))
        (assemble (segment)
          ;; To save our arguments, we follow the algorithm sketched in the
          ;; "PowerPC Calling Conventions" section of that document.
          (let ((words-processed 0)
                (gprs (mapcar #'make-gpr '(3 4 5 6 7 8 9 10)))
                (fprs (mapcar #'make-fpr '(1 2 3 4 5 6 7 8 9 10 11 12 13)))
                (stack-pointer (make-gpr 1)))
            (labels ((out-of-registers-error ()
                       (error "Too many arguments in callback"))
                     (save-arg (type words)
                       (let ((integerp (not (alien-float-type-p type)))
                             (offset (+ (* words-processed n-word-bytes)
                                        n-foreign-linkage-area-bytes)))
                         (cond (integerp
                                (loop repeat words
                                   for gpr = (pop gprs)
                                   do
                                     (if gpr
                                         (inst stw gpr stack-pointer offset)
                                         (out-of-registers-error))
                                     (incf words-processed)))
                             ;; The handling of floats is a little ugly
                             ;; because we hard-code the number of words
                               ;; for single- and double-floats.
                               ((alien-single-float-type-p type)
                                (pop gprs)
                                (let ((fpr (pop fprs)))
                                  (if fpr
                                      (inst stfs fpr stack-pointer offset)
                                      (out-of-registers-error)))
                                (incf words-processed))
                               ((alien-double-float-type-p type)
                                (setf gprs (cddr gprs))
                                (let ((fpr (pop fprs)))
                                  (if fpr
                                      (inst stfd fpr stack-pointer offset)
                                      (out-of-registers-error)))
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
            (destructuring-bind (sp r0 arg1 arg2 arg3 arg4)
                (mapcar #'make-gpr '(1 0 3 4 5 6))
              (flet ((load-address-into (reg addr)
                       (let ((high (ldb (byte 16 16) addr))
                             (low (ldb (byte 16 0) addr)))
                         (inst li reg high)
                         (inst slwi reg reg 16)
                         (inst ori reg reg low))))
                ;; Setup the args
                (load-address-into
                 arg1 (get-lisp-obj-address #'enter-alien-callback))
                (inst li arg2 (fixnumize index))
                (inst addi arg3 sp n-foreign-linkage-area-bytes)
                ;; FIXME: This was (- (* RETURN-AREA-SIZE N-WORD-BYTES)), while
                ;; RETURN-AREA-SIZE was (* N-RETURN-AREA-WORDS N-WORD-BYTES):
                ;; I assume the intention was (- N-RETURN-AREA-BYTES), but who knows?
                ;; --NS 2005-06-11
                (inst addi arg4 sp (- n-return-area-bytes))
                ;; FIXME! FIXME FIXME: What does this FIXME refer to?
                ;; Save sp, setup the frame
                (inst mflr r0)
                (inst stw r0 sp (* 2 n-word-bytes)) ; FIXME: magic constant
                (inst stwu sp sp (- frame-size))
                ;; Make the call
                (load-address-into r0 (foreign-symbol-address "funcall3"))
                (inst mtlr r0)
                (inst blrl))
              ;; We're back!  Restore sp and lr, load the return value from just
              ;; under sp, and return.
              (inst lwz sp sp 0)
              (inst lwz r0 sp (* 2 n-word-bytes))
              (inst mtlr r0)
              (loop with gprs = (mapcar #'make-gpr '(3 4))
                 repeat n-return-area-words
                 for gpr = (pop gprs)
                 for offset downfrom (- n-word-bytes) by n-word-bytes
                 do
                   (unless gpr
                     (bug "Out of return registers in alien-callback trampoline."))
                   (inst lwz gpr sp offset))
              (inst blr))))
        (finalize-segment segment)
        ;; Now that the segment is done, convert it to a static
        ;; vector we can point foreign code to.
        (let ((buffer (sb!assem::segment-buffer segment)))
          (make-static-vector (length buffer)
                              :element-type '(unsigned-byte 8)
                              :initial-contents buffer))))))
