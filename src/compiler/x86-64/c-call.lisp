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

(in-package "SB-VM")

;; The MOVE-ARG vop is going to store args on the stack for
;; call-out. These tn's will be used for that. move-arg is normally
;; used for things going down the stack but C wants to have args
;; indexed in the positive direction.

(defstruct (arg-state (:copier nil))
  (register-args 0)
  (xmm-args 0)
  (stack-frame-size 0))
(declaim (freeze-type arg-state))

(defconstant max-int-args #.(length *c-call-register-arg-offsets*))
(defconstant max-xmm-args #+win32 4 #-win32 8)

(defun int-arg (state prim-type reg-sc stack-sc)
  (let ((reg-args (max (arg-state-register-args state)
                       #+win32 (arg-state-xmm-args state))))
    (cond ((< reg-args max-int-args)
           (setf (arg-state-register-args state) (1+ reg-args))
           (make-wired-tn* prim-type reg-sc
                             (nth reg-args *c-call-register-arg-offsets*)))
          (t
           (let ((frame-size (arg-state-stack-frame-size state)))
             (setf (arg-state-stack-frame-size state) (1+ frame-size))
             (make-wired-tn* prim-type stack-sc frame-size))))))

(define-alien-type-method (integer :arg-tn) (type state)
  (if (alien-integer-type-signed type)
      (int-arg state 'signed-byte-64 signed-reg-sc-number signed-stack-sc-number)
      (int-arg state 'unsigned-byte-64 unsigned-reg-sc-number unsigned-stack-sc-number)))

(define-alien-type-method (system-area-pointer :arg-tn) (type state)
  (declare (ignore type))
  (int-arg state 'system-area-pointer sap-reg-sc-number sap-stack-sc-number))

(defun float-arg (state prim-type reg-sc stack-sc)
  (let ((xmm-args (max (arg-state-xmm-args state)
                        #+win32 (arg-state-register-args state))))
    (cond ((< xmm-args max-xmm-args)
           (setf (arg-state-xmm-args state) (1+ xmm-args))
           (make-wired-tn* prim-type reg-sc
                             (nth xmm-args *float-regs*)))
          (t
           (let ((frame-size (arg-state-stack-frame-size state)))
             (setf (arg-state-stack-frame-size state) (1+ frame-size))
             (make-wired-tn* prim-type stack-sc frame-size))))))

(define-alien-type-method (double-float :arg-tn) (type state)
  (declare (ignore type))
  (float-arg state 'double-float double-reg-sc-number double-stack-sc-number))

(define-alien-type-method (single-float :arg-tn) (type state)
  (declare (ignore type))
  (float-arg state 'single-float single-reg-sc-number single-stack-sc-number))

(defstruct (result-state (:copier nil))
  (num-results 0))
(declaim (freeze-type result-state))

(defun result-reg-offset (slot)
  (ecase slot
    (0 rax-offset)
    (1 rdx-offset)))

(define-alien-type-method (integer :result-tn) (type state)
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (multiple-value-bind (ptype reg-sc)
        (if (alien-integer-type-signed type)
            (values 'signed-byte-64 signed-reg-sc-number)
            (values 'unsigned-byte-64 unsigned-reg-sc-number))
      (make-wired-tn* ptype reg-sc (result-reg-offset num-results)))))

(define-alien-type-method (integer :naturalize-gen) (type alien)
  (if (<= (alien-type-bits type) 32)
      (if (alien-integer-type-signed type)
          `(sign-extend ,alien ,(alien-type-bits type))
          `(logand ,alien ,(1- (ash 1 (alien-type-bits type)))))
      alien))

(define-alien-type-method (system-area-pointer :result-tn) (type state)
  (declare (ignore type))
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (make-wired-tn* 'system-area-pointer sap-reg-sc-number
                      (result-reg-offset num-results))))

(define-alien-type-method (double-float :result-tn) (type state)
  (declare (ignore type))
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (make-wired-tn* 'double-float double-reg-sc-number num-results)))

(define-alien-type-method (single-float :result-tn) (type state)
  (declare (ignore type))
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (make-wired-tn* 'single-float single-reg-sc-number num-results)))

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
      (values (make-wired-tn* 'positive-fixnum any-reg-sc-number rsp-offset)
              (* (arg-state-stack-frame-size arg-state) n-word-bytes)
              (arg-tns)
              (invoke-alien-type-method :result-tn
                                        (alien-fun-type-result-type type)
                                        (make-result-state))))))


(deftransform %alien-funcall ((function type &rest args) * * :node node)
  (aver (sb-c:constant-lvar-p type))
  (let* ((type (sb-c:lvar-value type))
         (env (sb-c::node-lexenv node))
         (arg-types (alien-fun-type-arg-types type))
         (result-type (alien-fun-type-result-type type)))
    (aver (= (length arg-types) (length args)))
    (if (or (some #'(lambda (type)
                      (and (alien-integer-type-p type)
                           (> (sb-alien::alien-integer-type-bits type) 64)))
                  arg-types)
            (and (alien-integer-type-p result-type)
                 (> (sb-alien::alien-integer-type-bits result-type) 64)))
        (collect ((new-args) (lambda-vars) (new-arg-types))
          (dolist (type arg-types)
            (let ((arg (gensym)))
              (lambda-vars arg)
              (cond ((and (alien-integer-type-p type)
                          (> (sb-alien::alien-integer-type-bits type) 64))
                     ;; CLH: FIXME! This should really be
                     ;; #xffffffffffffffff. nyef says: "Passing
                     ;; 128-bit integers to ALIEN functions on x86-64
                     ;; believed to be broken."
                     (new-args `(logand ,arg #xffffffff))
                     (new-args `(ash ,arg -64))
                     (new-arg-types (parse-alien-type '(unsigned 64) env))
                     (if (alien-integer-type-signed type)
                         (new-arg-types (parse-alien-type '(signed 64) env))
                         (new-arg-types (parse-alien-type '(unsigned 64) env))))
                    (t
                     (new-args arg)
                     (new-arg-types type)))))
          (cond ((and (alien-integer-type-p result-type)
                      (> (sb-alien::alien-integer-type-bits result-type) 64))
                 (let ((new-result-type
                        (let ((sb-alien::*values-type-okay* t))
                          (parse-alien-type
                           (if (alien-integer-type-signed result-type)
                               '(values (unsigned 64) (signed 64))
                               '(values (unsigned 64) (unsigned 64)))
                           env))))
                   `(lambda (function type ,@(lambda-vars))
                      (declare (ignore type))
                      (multiple-value-bind (low high)
                          (%alien-funcall function
                                          ',(make-alien-fun-type
                                             :arg-types (new-arg-types)
                                             :result-type new-result-type)
                                          ,@(new-args))
                        (logior low (ash high 64))))))
                (t
                 `(lambda (function type ,@(lambda-vars))
                    (declare (ignore type))
                    (%alien-funcall function
                                    ',(make-alien-fun-type
                                       :arg-types (new-arg-types)
                                       :result-type result-type)
                                    ,@(new-args))))))
        (sb-c::give-up-ir1-transform))))

;;; The ABI is vague about how signed sub-word integer return values
;;; are handled, but since gcc versions >=4.3 no longer do sign
;;; extension in the callee, we need to do it in the caller.  FIXME:
;;; If the value to be extended is known to already be of the target
;;; type at compile time, we can (and should) elide the extension.
(defknown sign-extend ((signed-byte 64) t) fixnum
    (foldable flushable movable))

(defoptimizer (sign-extend derive-type) ((x size))
  (when (sb-c:constant-lvar-p size)
    (specifier-type `(signed-byte ,(sb-c:lvar-value size)))))

(define-vop (sign-extend)
  (:translate sign-extend)
  (:policy :fast-safe)
  (:args (val :scs (signed-reg)))
  (:arg-types signed-num (:constant fixnum))
  (:info size)
  (:results (res :scs (signed-reg)))
  (:result-types fixnum)
  (:generator 1
    (inst movsx `(,(ecase size (8 :byte) (16 :word) (32 :dword)) :qword) res val)))

#-sb-xc-host
(defun sign-extend (x size)
  (declare (type (signed-byte 64) x))
  (ecase size
    (8 (sign-extend x size))
    (16 (sign-extend x size))
    (32 (sign-extend x size))))

;;; Note that if jumping _to_ the linkage entry, the jump is to the JMP instruction
;;; at entry + 0, but if jumping _via_ the linkage index, we can jump to [entry+8]
;;; which holds the ultimate address to jump to.
(define-vop (foreign-symbol-sap)
  (:translate foreign-symbol-sap)
  (:policy :fast-safe)
  (:args)
  (:arg-types (:constant simple-string))
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:vop-var vop)
  (:generator 2
    #-immobile-space ; non-relocatable alien linkage table
    (inst mov res (make-fixup foreign-symbol :foreign))
    #+immobile-space ; relocatable alien linkage table
    (cond ((sb-c::code-immobile-p vop)
           (inst lea res (rip-relative-ea (make-fixup foreign-symbol :foreign))))
          (t
           (inst mov res (thread-slot-ea thread-alien-linkage-table-base-slot))
           (inst lea res (ea (make-fixup foreign-symbol :alien-code-linkage-index) res))))))

(define-vop (foreign-symbol-dataref-sap)
  (:translate foreign-symbol-dataref-sap)
  (:policy :fast-safe)
  (:args)
  (:arg-types (:constant simple-string))
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:vop-var vop)
  (:generator 2
    #-immobile-space ; non-relocatable alien linkage table
    (inst mov res (ea (make-fixup foreign-symbol :foreign-dataref)))
    #+immobile-space ; relocatable alien linkage table
    (cond ((sb-c::code-immobile-p vop)
           (inst mov res (rip-relative-ea (make-fixup foreign-symbol :foreign-dataref))))
          (t
           (inst mov res (thread-slot-ea thread-alien-linkage-table-base-slot))
           (inst mov res (ea (make-fixup foreign-symbol :alien-data-linkage-index) res))))))

#+sb-safepoint
(defconstant thread-saved-csp-offset (- (1+ sb-vm::thread-header-slots)))

(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
  (defun destroyed-c-registers ()
    ;; Safepoints do not save interrupt contexts to be scanned during
    ;; GCing, it only looks at the stack, so if a register isn't
    ;; spilled it won't be visible to the GC.
    #+sb-safepoint
    '((:save-p t))
    #-sb-safepoint
    (let ((gprs (list '#:rcx '#:rdx #-win32 '#:rsi #-win32 '#:rdi
                      '#:r8 '#:r9 '#:r10 '#:r11))
          (vars))
      (append
       (loop for gpr in gprs
             for offset = (symbol-value (intern (concatenate 'string (symbol-name gpr) "-OFFSET") "SB-VM"))
             collect `(:temporary (:sc any-reg :offset ,offset :from :eval :to :result)
                                  ,(car (push gpr vars))))
       (loop for float to 15
             for varname = (format nil "FLOAT~D" float)
             collect `(:temporary (:sc single-reg :offset ,float :from :eval :to :result)
                                  ,(car (push (make-symbol varname) vars))))
       `((:ignore ,@vars))))))

(define-vop (call-out)
  (:args (function :scs (sap-reg)
                   :target rbx)
         (args :more t))
  (:results (results :more t))
  ;; RBX is used to first load the address, allowing the debugger to
  ;; determine which alien was accessed in case it's undefined.
  (:temporary (:sc sap-reg :offset rbx-offset :from (:argument 0)) rbx)
  (:temporary (:sc unsigned-reg :offset rax-offset :to :result) rax)
  #+sb-safepoint
  (:temporary (:sc unsigned-stack :from :eval :to :result) pc-save)
  #+win32
  (:temporary (:sc unsigned-reg :offset r15-offset :from :eval :to :result) r15)
  (:ignore results)
  (:vop-var vop)
  (:generator 0
    (move rbx function)
    (emit-c-call vop rax rbx args
                 sb-alien::*alien-fun-type-varargs-default*
                 #+sb-safepoint pc-save
                 #+win32 rbx))
  #+win32 (:ignore r15)
  . #.(destroyed-c-registers))

;;; Calls to C can generally be made without loading a register
;;; with the function. We receive the function name as an info argument.
(define-vop (call-out-named)
  (:args (args :more t))
  (:results (results :more t))
  (:info c-symbol varargsp)
  (:temporary (:sc unsigned-reg :offset rax-offset :to :result) rax)
  #+sb-safepoint
  (:temporary (:sc unsigned-stack :from :eval :to :result) pc-save)
  #+win32
  (:temporary (:sc unsigned-reg :offset r15-offset :from :eval :to :result) r15)
  #+win32
  (:ignore r15)
  #+win32
  (:temporary (:sc unsigned-reg :offset rbx-offset :from :eval :to :result) rbx)
  (:ignore results)
  (:vop-var vop)
  (:generator 0
    (emit-c-call vop rax c-symbol args varargsp
                 #+sb-safepoint pc-save
                 #+win32 rbx))
  . #.(destroyed-c-registers))

#+win32
(progn
(defconstant win64-seh-direct-thunk-addr win64-seh-data-addr)
(defconstant win64-seh-indirect-thunk-addr (+ win64-seh-data-addr 8)))

(defun emit-c-call (vop rax fun args varargsp #+sb-safepoint pc-save #+win32 rbx)
  (declare (ignorable varargsp))
  ;; Current PC - don't rely on function to keep it in a form that
  ;; GC understands
  #+sb-safepoint
  (let ((label (gen-label)))
    ;; This looks unnecessary. GC can look at the stack word physically below
    ;; the CSP-around-foreign-call, which must be a PC pointing into the lisp caller.
    ;; A more interesting question would arise if we had callee-saved registers
    ;; within lisp code, which we don't at the moment. If we did, those
    ;; wouldn't be anywhere on the stack unless C code decides to save them.
    (inst lea rax (rip-relative-ea label))
    (emit-label label)
    (move pc-save rax))
  (when (sb-c:msan-unpoison sb-c:*compilation*)
    (inst mov rax (thread-slot-ea thread-msan-param-tls-slot))
    ;; Unpoison parameters
    (do ((n 0 (+ n n-word-bytes))
         (arg args (tn-ref-across arg)))
        ((null arg))
      ;; KLUDGE: assume all parameters are 8 bytes or less
      (inst mov :qword (ea n rax) 0)))
  #-win32
  ;; ABI: AL contains amount of arguments passed in XMM registers
  ;; for vararg calls.
  (when varargsp
    (move-immediate rax
                    (loop for tn-ref = args then (tn-ref-across tn-ref)
                          while tn-ref
                          count (eq (sb-name (sc-sb (tn-sc (tn-ref-tn tn-ref))))
                                    'float-registers))))

  ;; Store SP in thread struct, unless the enclosing block says not to
  #+sb-safepoint
  (when (policy (sb-c::vop-node vop) (/= sb-c:insert-safepoints 0))
    (inst mov (thread-slot-ea thread-saved-csp-offset) rsp-tn))

  #+win32 (inst sub rsp-tn #x20)       ;MS_ABI: shadow zone

  ;; From immobile space we use the "CALL rel32" format to the linkage
  ;; table jump, and from dynamic space we use "CALL [ea]" format
  ;; where ea is the address of the linkage table entry's operand.
  ;; So while the former is a jump to a jump, we can optimize out
  ;; one jump in an ELF executable.
  ;; N.B.: if you change how the call is emitted, you will also have to adjust
  ;; the UNDEFINED-ALIEN-TRAMP lisp asm routine to recognize the various shapes
  ;; this instruction sequence can take.
  #-win32
  (pseudo-atomic (:elide-if (not (call-out-pseudo-atomic-p vop)))
    (inst call (if (tn-p fun)
                 fun
                 #-immobile-space (ea (make-fixup fun :foreign 8))
                 #+immobile-space
                 (cond ((sb-c::code-immobile-p vop) (make-fixup fun :foreign))
                       (t
                        ;; Pick r10 as the lowest unused clobberable register.
                        ;; RAX has a designated purpose, and RBX is nonvolatile (not always
                        ;; spilled by Lisp because a C function has to save it if used)
                        (inst mov r10-tn (thread-slot-ea thread-alien-linkage-table-base-slot))
                        (ea (make-fixup fun :alien-code-linkage-index 8) r10-tn))))))

  ;; On win64, calls go through one of the thunks defined in set_up_win64_seh_data().
  #+win32
  (cond ((tn-p fun)
         (move rbx fun)
         (inst mov rax win64-seh-direct-thunk-addr)
         (inst call rax))
        (t
         ;; RBX is loaded with the address of the word containing the address in the alien
         ;; linkage table of the alien function to call. This informs UNDEFINED-ALIEN-TRAMP
         ;; which table cell was referenced, if undefined.
         #+immobile-space ; relocatable table
         (cond ((sb-c::code-immobile-p vop)
                (inst lea rbx (rip-relative-ea (make-fixup fun :foreign 8))))
               (t
                (inst mov rbx (make-fixup fun :alien-code-linkage-index 8))
                (inst add rbx (thread-slot-ea thread-alien-linkage-table-base-slot))))
         ;; else, wired table base address
         #-immobile-space (inst mov rbx (make-fixup fun :foreign 8))
         (inst mov rax win64-seh-indirect-thunk-addr)
         (inst call rax)))

  ;; For the undefined alien error
  (note-this-location vop :internal-error)
  #+win32 (inst add rsp-tn #x20)       ;MS_ABI: remove shadow space

  ;; Zero the saved CSP, unless this code shouldn't ever stop for GC
  #+sb-safepoint
  (when (policy (sb-c::vop-node vop) (/= sb-c:insert-safepoints 0))
    (inst xor (thread-slot-ea thread-saved-csp-offset) rsp-tn)))

(define-vop (alloc-number-stack-space)
  (:info amount)
  (:results (result :scs (sap-reg any-reg)))
  (:result-types system-area-pointer)
  (:generator 0
    (aver (location= result rsp-tn))
    (unless (zerop amount)
      (let ((delta (align-up amount 8)))
        (inst sub rsp-tn delta)))
    ;; C stack must be 16 byte aligned
    (inst and rsp-tn -16)
    (move result rsp-tn)))

(macrolet ((alien-stack-ptr ()
             #+sb-thread '(symbol-known-tls-cell '*alien-stack-pointer*)
             #-sb-thread '(static-symbol-value-ea '*alien-stack-pointer*)))
  (define-vop (alloc-alien-stack-space)
    (:info amount)
    (:results (result :scs (sap-reg any-reg)))
    (:result-types system-area-pointer)
    (:generator 0
      (aver (not (location= result rsp-tn)))
      (unless (zerop amount)
        (let ((delta (align-up amount 8)))
          (inst sub :qword (alien-stack-ptr) delta)))
      (inst mov result (alien-stack-ptr)))))

;;; Callbacks

#-sb-xc-host
(defun alien-callback-accessor-form (type sp offset)
  `(deref (sap-alien (sap+ ,sp ,offset) (* ,type))))

#-sb-xc-host
(defun alien-callback-assembler-wrapper (index result-type argument-types)
  (labels ((make-tn-maker (sc-name)
             (lambda (offset)
               (make-random-tn :kind :normal
                               :sc (sc-or-lose sc-name)
                               :offset offset))))
    (let* ((segment (make-segment))
           (rax rax-tn)
           #+win32 (rcx rcx-tn)
           #-(and win32 sb-thread) (rdi rdi-tn)
           #-(and win32 sb-thread) (rsi rsi-tn)
           (rdx rdx-tn)
           (rbp rbp-tn)
           (rsp rsp-tn)
           #+(and win32 sb-thread) (r8 r8-tn)
           (xmm0 float0-tn)
           ([rsp] (ea rsp))
           ;; How many arguments have been copied
           (arg-count 0)
           ;; How many arguments have been copied from the stack
           (stack-argument-count #-win32 0 #+win32 4)
           (gprs (mapcar (make-tn-maker 'any-reg) *c-call-register-arg-offsets*))
           (fprs (mapcar (make-tn-maker 'double-reg)
                         ;; Only 8 first XMM registers are used for
                         ;; passing arguments
                         (subseq *float-regs* 0 #-win32 8 #+win32 4))))
      (assemble (segment 'nil)
        ;; Make room on the stack for arguments.
        (when argument-types
          (inst sub rsp (* n-word-bytes (length argument-types))))
        ;; Copy arguments from registers to stack
        (dolist (type argument-types)
          (let ((integerp (not (alien-float-type-p type)))
                ;; A TN pointing to the stack location where the
                ;; current argument should be stored for the purposes
                ;; of ENTER-ALIEN-CALLBACK.
                (target-tn (ea (* arg-count n-word-bytes) rsp))
                ;; A TN pointing to the stack location that contains
                ;; the next argument passed on the stack.
                (stack-arg-tn (ea (* (+ 1 (length argument-types) stack-argument-count)
                                     n-word-bytes) rsp)))
            (incf arg-count)
            (cond (integerp
                   (let ((gpr (pop gprs)))
                     #+win32 (pop fprs)
                     ;; Argument not in register, copy it from the old
                     ;; stack location to a temporary register.
                     (unless gpr
                       (incf stack-argument-count)
                       (setf gpr rax)
                       (inst mov gpr stack-arg-tn))
                     ;; Copy from either argument register or temporary
                     ;; register to target.
                     (inst mov target-tn gpr)))
                  ((or (alien-single-float-type-p type)
                       (alien-double-float-type-p type))
                   (let ((fpr (pop fprs)))
                     #+win32 (pop gprs)
                     (cond (fpr
                            ;; Copy from float register to target location.
                            (inst movq target-tn fpr))
                           (t
                            ;; Not in float register. Copy from stack to
                            ;; temporary (general purpose) register, and
                            ;; from there to the target location.
                            (incf stack-argument-count)
                            (inst mov rax stack-arg-tn)
                            (inst mov target-tn rax)))))
                  (t
                   (bug "Unknown alien floating point type: ~S" type)))))

        #-sb-thread
        (progn
          ;; arg0 to ENTER-ALIEN-CALLBACK (trampoline index)
          (inst mov rdx (fixnumize index))
          ;; arg1 to ENTER-ALIEN-CALLBACK (pointer to argument vector)
          (inst mov rdi rsp)
          ;; add room on stack for return value
          (inst sub rsp (if (evenp arg-count)
                            (* n-word-bytes 2)
                            n-word-bytes))
          ;; arg2 to ENTER-ALIEN-CALLBACK (pointer to return value)
          (inst mov rsi rsp)

          ;; Make new frame
          (inst push rbp)
          (inst mov  rbp rsp)

          ;; Call
          (inst mov  rax (foreign-symbol-address "funcall_alien_callback"))
          (inst call rax)

          ;; Back! Restore frame
          (inst leave))

        #+sb-thread
        (progn
          ;; arg0 to ENTER-ALIEN-CALLBACK (trampoline index)
          (inst mov #-win32 rdi #+win32 rcx (fixnumize index))
          ;; arg1 to ENTER-ALIEN-CALLBACK (pointer to argument vector)
          (inst mov #-win32 rsi #+win32 rdx rsp)
          ;; add room on stack for return value
          (inst sub rsp (if (evenp arg-count)
                            (* n-word-bytes 2)
                            n-word-bytes))
          ;; arg2 to ENTER-ALIEN-CALLBACK (pointer to return value)
          (inst mov #-win32 rdx #+win32 r8 rsp)
          ;; Make new frame
          (inst push rbp)
          (inst mov  rbp rsp)
          #+win32 (inst sub rsp #x20)
          #+win32 (inst and rsp #x-20)
          ;; Call
          #+immobile-space (inst call (static-symbol-value-ea 'callback-wrapper-trampoline))
          ;; do this without MAKE-FIXUP because fixup'ing does not happen when
          ;; assembling callbacks (probably could, but ...)
          #-immobile-space
          (inst call (ea (+ (foreign-symbol-address "callback_wrapper_trampoline") 8)))
          ;; Back! Restore frame
          (inst leave))

        ;; Result now on top of stack, put it in the right register
        (cond
          ((or (alien-integer-type-p result-type)
               (alien-pointer-type-p result-type)
               (alien-type-= #.(parse-alien-type 'system-area-pointer nil)
                             result-type))
           (inst mov rax [rsp]))
          ((or (alien-single-float-type-p result-type)
               (alien-double-float-type-p result-type))
           (inst movq xmm0 [rsp]))
          ((alien-void-type-p result-type))
          (t
           (error "Unrecognized alien type: ~A" result-type)))

        ;; Pop the arguments and the return value from the stack to get
        ;; the return address at top of stack.

        (inst add rsp (* (+ arg-count
                            ;; Plus the return value and make sure it's aligned
                            (if (evenp arg-count)
                                2
                                1))
                         n-word-bytes))
        ;; Return
        (inst ret))
      (finalize-segment segment)
      ;; Now that the segment is done, convert it to a static
      ;; vector we can point foreign code to.
      (let ((buffer (sb-assem:segment-buffer segment)))
        (make-static-vector (length buffer)
                            :element-type '(unsigned-byte 8)
                            :initial-contents buffer)))))
