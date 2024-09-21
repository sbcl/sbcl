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

(defconstant +number-stack-alignment-mask+ (1- (* n-word-bytes 2)))

(defconstant +max-register-args+ 8)

(defstruct arg-state
  (num-register-args 0)
  (fp-registers 0)
  (stack-frame-size 0))

(defstruct (result-state (:copier nil))
  (num-results 0))

(defun result-reg-offset (slot)
  (ecase slot
    (0 nl0-offset)
    (1 nl1-offset)))

(defun register-args-offset (index)
  (elt '#.(list nl0-offset nl1-offset nl2-offset nl3-offset nl4-offset nl5-offset nl6-offset
                      nl7-offset)
       index))

(define-vop (move-word-arg-stack)
  (:args (x :scs (signed-reg unsigned-reg single-reg))
         (fp :scs (any-reg)))
  (:info size offset)
  (:generator 0
    (let ((addr (@ fp (load-store-offset offset))))
      (ecase size
        (1
         (inst strb x addr))
        (2
         (inst strh x addr))
        (4
         (inst str (if (sc-is x single-reg)
                       x
                       (32-bit-reg x))
               addr))))))

(defun move-to-stack-location (value size offset prim-type sc node block nsp)
  (let ((temp-tn (sb-c:make-representation-tn
                  (primitive-type-or-lose prim-type)
                  sc)))
    (sb-c::emit-move node
                     block
                     (sb-c::lvar-tn node block value)
                     temp-tn)
    (sb-c::vop move-word-arg-stack node block temp-tn nsp size offset)))

(defun int-arg (state prim-type reg-sc stack-sc &optional (size 8))
  (let ((reg-args (arg-state-num-register-args state)))
    (cond ((< reg-args +max-register-args+)
           (setf (arg-state-num-register-args state) (1+ reg-args))
           (make-wired-tn* prim-type reg-sc (register-args-offset reg-args)))
          (t
           (let ((frame-size (align-up (arg-state-stack-frame-size state) size)))
             (setf (arg-state-stack-frame-size state) (+ frame-size size))
             (cond #+darwin
                   ((/= size n-word-bytes)
                    (lambda (value node block nsp)
                      (move-to-stack-location value size frame-size
                                              prim-type reg-sc node block nsp)))
                   (t
                    (make-wired-tn* prim-type stack-sc (truncate frame-size size)))))))))

(defun float-arg (state prim-type reg-sc stack-sc &optional (size 8))
  (let ((reg-args (arg-state-fp-registers state)))
    (cond ((< reg-args +max-register-args+)
           (setf (arg-state-fp-registers state) (1+ reg-args))
           (make-wired-tn* prim-type reg-sc reg-args))
          (t
           (let ((frame-size (align-up (arg-state-stack-frame-size state) size)))
             (setf (arg-state-stack-frame-size state) (+ frame-size size))
             (cond #+darwin
                   ((/= size n-word-bytes)
                    (lambda (value node block nsp)
                      (move-to-stack-location value size frame-size
                                              prim-type reg-sc node block nsp)))
                   (t
                    (make-wired-tn* prim-type stack-sc (truncate frame-size size)))))))))

(define-alien-type-method (integer :arg-tn) (type state)
 (let ((size #+darwin (truncate (alien-type-bits type) n-byte-bits)
             #-darwin n-word-bytes))
   (if (alien-integer-type-signed type)
       (int-arg state 'signed-byte-64 signed-reg-sc-number signed-stack-sc-number
                size)
       (int-arg state 'unsigned-byte-64 unsigned-reg-sc-number unsigned-stack-sc-number
                size))))

(define-alien-type-method (system-area-pointer :arg-tn) (type state)
  (declare (ignore type))
  (int-arg state 'system-area-pointer sap-reg-sc-number sap-stack-sc-number))

(define-alien-type-method (single-float :arg-tn) (type state)
  (declare (ignore type))
  (float-arg state 'single-float single-reg-sc-number single-stack-sc-number #+darwin 4))

(define-alien-type-method (double-float :arg-tn) (type state)
  (declare (ignore type))
  (float-arg state 'double-float double-reg-sc-number double-stack-sc-number))
;;;

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
    (check-type size (member 8 16 32))
    (inst sbfm res val 0 (1- size))))

#-sb-xc-host
(defun sign-extend (x size)
  (declare (type (signed-byte 64) x))
  (ecase size
    (8 (sign-extend x size))
    (16 (sign-extend x size))
    (32 (sign-extend x size))))

(define-alien-type-method (integer :naturalize-gen) (type alien)
  (if (<= (alien-type-bits type) 32)
      (if (alien-integer-type-signed type)
          `(sign-extend ,alien ,(alien-type-bits type))
          `(logand ,alien ,(1- (ash 1 (alien-type-bits type)))))
      alien))

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
  (make-wired-tn* 'single-float single-reg-sc-number 0))

(define-alien-type-method (double-float :result-tn) (type state)
  (declare (ignore type state))
  (make-wired-tn* 'double-float double-reg-sc-number 0))

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
      (let (#+darwin (variadic (sb-alien::alien-fun-type-varargs type)))
        (loop for i from 0
              for arg-type in (alien-fun-type-arg-types type)
              do
              #+darwin
              (when (eql i variadic)
                (setf (arg-state-num-register-args arg-state) +max-register-args+
                      (arg-state-fp-registers arg-state) +max-register-args+))
              (arg-tns (invoke-alien-type-method :arg-tn arg-type arg-state))))
      (values (make-normal-tn *fixnum-primitive-type*)
              (arg-state-stack-frame-size arg-state)
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
    (load-foreign-symbol res foreign-symbol)))

(define-vop (foreign-symbol-dataref-sap)
  (:translate foreign-symbol-dataref-sap)
  (:policy :fast-safe)
  (:args)
  (:arg-types (:constant simple-string))
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (load-foreign-symbol res foreign-symbol :dataref t)))

#+sb-safepoint
(defconstant thread-saved-csp-slot (- (1+ sb-vm::thread-header-slots)))

(defconstant-eqx +destroyed-c-registers+
  (loop for i from 0 to 18 collect i)
  #'equal)

(defun emit-c-call (vop nfp-save temp temp2 cfunc function)
  (let ((cur-nfp (current-nfp-tn vop)))
    (when cur-nfp
      (store-stack-tn nfp-save cur-nfp))
    (assemble ()
      #+sb-thread
      (progn
        (inst add temp csp-tn (* 2 n-word-bytes))
        ;; Build a new frame to stash a pointer to the current code object
        ;; for the GC to see.
        (inst adr temp2 return)
        (inst stp cfp-tn temp2 (@ csp-tn))
        (storew-pair csp-tn thread-control-frame-pointer-slot temp thread-control-stack-pointer-slot thread-tn)
        ;; OK to run GC without stopping this thread from this point
        ;; on.
        #+sb-safepoint
        (storew csp-tn thread-tn thread-saved-csp-slot)
        (cond ((stringp function)
               (invoke-foreign-routine function cfunc))
              (t
               (sc-case function
                 (sap-reg (move cfunc function))
                 (sap-stack
                  (load-stack-offset cfunc cur-nfp function)))
               (inst blr cfunc)))
        ;; Blank all boxed registers that potentially contain Lisp
        ;; pointers, not just volatile ones, since GC could
        ;; potentially observe stale pointers otherwise. FIXME: There
        ;; is some suboptimality here; instead of blanking all boxed
        ;; pointers, we can just blank the ones which are both qsaved
        ;; and restored by C and known to not contain a Lisp pointer
        ;; (i.e. can be moved).
        (loop for reg in descriptor-regs
              do (inst mov
                       (make-random-tn
                        :kind :normal
                        :sc (sc-or-lose 'descriptor-reg)
                        :offset reg)
                       0))
        ;; No longer OK to run GC except at safepoints.
        #+sb-safepoint
        (storew zr-tn thread-tn thread-saved-csp-slot)
        (storew zr-tn thread-tn thread-control-stack-pointer-slot))
      return
      #-sb-thread
      (progn
        temp2
        (if (stringp function)
            (load-foreign-symbol cfunc function)
            (sc-case function
              (sap-reg (move cfunc function))
              (sap-stack
              (load-stack-offset cfunc cur-nfp function))))
        (invoke-foreign-routine "call_into_c" temp))
      (when cur-nfp
        (load-stack-tn cur-nfp nfp-save)))))

(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
  (defun destroyed-c-registers ()
    (let ((gprs +destroyed-c-registers+)
          (vars))
      (append
       (loop for gpr in gprs
             collect `(:temporary (:sc any-reg :offset ,gpr :from :eval :to :result)
                                  ,(car (push (gensym) vars))))
       (loop for float to 31
             collect `(:temporary (:sc single-reg :offset ,float :from :eval :to :result)
                                  ,(car (push (gensym) vars))))
       `((:ignore ,@vars))))))

(define-vop (call-out)
  (:args (function :scs (sap-reg sap-stack))
         (args :more t))
  (:results (results :more t))
  (:ignore args results lr)
  (:temporary (:sc non-descriptor-reg :offset lr-offset) lr)
  (:temporary (:sc any-reg :offset r9-offset
               :from (:argument 0) :to (:result 0)) cfunc)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:temporary (:sc any-reg :offset r10-offset) temp)
  (:temporary (:sc any-reg :offset lexenv-offset) temp2)
  (:vop-var vop)
  (:generator 0
    (emit-c-call vop nfp-save temp temp2 cfunc function))
  .
  #. (destroyed-c-registers))

;;; Manually load the fixup instead of using foreign-symbol-sap,
;;; because it wants to go to r9, which is not compatible with sap-reg.
(define-vop (call-out-named call-out)
  (:args (args :more t))
  (:info function variadic)
  (:ignore args results variadic lr))

(define-vop (alloc-number-stack-space)
  (:info amount)
  (:result-types system-area-pointer)
  (:results (result :scs (sap-reg any-reg)))
  (:generator 0
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount +number-stack-alignment-mask+)
                             +number-stack-alignment-mask+)))
        (inst sub nsp-tn nsp-tn (add-sub-immediate delta))
        (inst mov-sp result nsp-tn)))))

(define-vop (dealloc-number-stack-space)
  (:info amount)
  (:policy :fast-safe)
  (:generator 0
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount +number-stack-alignment-mask+)
                             +number-stack-alignment-mask+)))
        (inst add nsp-tn nsp-tn (add-sub-immediate delta))))))

;;; Callback
#-sb-xc-host
(defun alien-callback-accessor-form (type sap offset)
  (let ((parsed-type type))
    (if (alien-integer-type-p parsed-type)
        (let ((bits (sb-alien::alien-integer-type-bits parsed-type)))
               (let ((byte-offset
                      (cond ((< bits n-word-bits)
                             (- n-word-bytes
                                (ceiling bits n-byte-bits)))
                            (t 0))))
                 `(deref (sap-alien (sap+ ,sap
                                          ,(+ byte-offset offset))
                                    (* ,type)))))
        `(deref (sap-alien (sap+ ,sap ,offset) (* ,type))))))

#-sb-xc-host
(defun alien-callback-assembler-wrapper (index result-type argument-types)
  (flet ((make-tn (offset &optional (sc-name 'any-reg))
           (make-random-tn :kind :normal
                           :sc (sc-or-lose sc-name)
                           :offset offset)))
    (let* ((segment (make-segment))
           ;; How many arguments have been copied
           (arg-count 0)
           ;; How many arguments have been copied from the stack
           (stack-argument-bytes 0)
           (r0-tn (make-tn 0))
           (r1-tn (make-tn 1))
           (r2-tn (make-tn 2))
           (r3-tn (make-tn 3))
           (r4-tn (make-tn 4))
           (temp-tn (make-tn 9))
           (nsp-save-tn (make-tn 10))
           (gprs (loop for i below 8
                       collect (make-tn i)))
           (fp-registers 0)
           (frame-size (* (length argument-types) n-word-bytes)))
      (setf frame-size (logandc2 (+ frame-size +number-stack-alignment-mask+)
                                 +number-stack-alignment-mask+))
      (assemble (segment 'nil)
        (inst mov-sp nsp-save-tn nsp-tn)
        (inst str lr-tn (@ nsp-tn -16 :pre-index))
        ;; Make room on the stack for arguments.
        (when (plusp frame-size)
          (inst sub nsp-tn nsp-tn frame-size))
        ;; Copy arguments
        (dolist (type argument-types)
          (let ((target-tn (@ nsp-tn (* arg-count n-word-bytes)))
                (size #+darwin (truncate (alien-type-bits type) n-byte-bits)
                      #-darwin n-word-bytes))
            (cond ((or (alien-integer-type-p type)
                       (alien-pointer-type-p type)
                       (alien-type-= #.(parse-alien-type 'system-area-pointer nil)
                                     type))
                   (let ((gpr (pop gprs)))
                     (cond (gpr
                            (inst str gpr target-tn))
                           (t
                            (setf stack-argument-bytes
                                  (align-up stack-argument-bytes size))
                            (let ((addr (@ nsp-save-tn stack-argument-bytes)))
                              (cond #+darwin
                                    ((/= size 8)
                                     (let ((signed (and (alien-integer-type-p type)
                                                        (alien-integer-type-signed type))))
                                       (ecase size
                                         (1
                                          (if signed
                                              (inst ldrsb temp-tn addr)
                                              (inst ldrb temp-tn addr)))
                                         (2
                                          (if signed
                                              (inst ldrsh temp-tn addr)
                                              (inst ldrh temp-tn addr)))
                                         (4
                                          (if signed
                                              (inst ldrsw (32-bit-reg temp-tn) addr)
                                              (inst ldr (32-bit-reg temp-tn) addr))))))
                                    (t
                                     (inst ldr temp-tn addr)))
                              (inst str temp-tn target-tn))
                            (incf stack-argument-bytes size))))
                   (incf arg-count))
                  ((alien-float-type-p type)
                   (cond ((< fp-registers 8)
                          (inst str (make-tn fp-registers
                                             (if (alien-single-float-type-p type)
                                                 'single-reg
                                                 'double-reg))
                                target-tn))
                         (t
                          (setf stack-argument-bytes
                                  (align-up stack-argument-bytes size))
                          (case size
                            #+darwin
                            (4
                             (let ((reg (32-bit-reg temp-tn)))
                              (inst ldr reg (@ nsp-save-tn stack-argument-bytes))
                              (inst str reg target-tn)))
                            (t
                             (inst ldr temp-tn (@ nsp-save-tn stack-argument-bytes))
                             (inst str temp-tn target-tn)))
                          (incf stack-argument-bytes size)))
                   (incf fp-registers)
                   (incf arg-count))
                  (t
                   (bug "Unknown alien type: ~S" type)))))
        ;; arg0 to FUNCALL3 (function)
        (load-immediate-word r0-tn (static-fdefn-fun-addr 'enter-alien-callback))
        (loadw r0-tn r0-tn)
        ;; arg0 to ENTER-ALIEN-CALLBACK (trampoline index)
        (inst mov r1-tn (fixnumize index))
        ;; arg1 to ENTER-ALIEN-CALLBACK (pointer to argument vector)
        (inst mov-sp r2-tn nsp-tn)
        ;; add room on stack for return value
        (inst sub nsp-tn nsp-tn (* n-word-bytes 2))
        ;; arg2 to ENTER-ALIEN-CALLBACK (pointer to return value)
        (inst mov-sp r3-tn nsp-tn)

        ;; Call
        (load-immediate-word r4-tn (foreign-symbol-address
                                    #-sb-thread "funcall3"
                                    #+sb-thread "callback_wrapper_trampoline"))
        (inst blr r4-tn)

        ;; Result now on top of stack, put it in the right register
        (cond
          ((or (alien-integer-type-p result-type)
               (alien-pointer-type-p result-type)
               (alien-type-= #.(parse-alien-type 'system-area-pointer nil)
                             result-type))
           (loadw r0-tn nsp-tn))
          ((alien-float-type-p result-type)
           (loadw (make-tn fp-registers
                              (if (alien-single-float-type-p result-type)
                                  'single-reg
                                  'double-reg))
                 nsp-tn))
          ((alien-void-type-p result-type))
          (t
           (error "Unrecognized alien type: ~A" result-type)))
        (inst add nsp-tn nsp-tn (+ frame-size (* n-word-bytes 2)))
        (inst ldr lr-tn (@ nsp-tn 16 :post-index))
        (inst ret))
      (finalize-segment segment)
      ;; Now that the segment is done, convert it to a static
      ;; vector we can point foreign code to.
      (let* ((buffer (sb-assem:segment-buffer segment))
             (vector #-darwin-jit
                     (make-static-vector (length buffer)
                                         :element-type '(unsigned-byte 8)
                                         :initial-contents buffer)
                     #+darwin-jit
                     (make-static-code-vector (length buffer)
                                              buffer))
             (sap (vector-sap vector)))
        (alien-funcall
         (extern-alien "os_flush_icache"
                       (function void
                                 system-area-pointer
                                 unsigned-long))
         sap (length buffer))
        vector))))
