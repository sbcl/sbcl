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

;;;; 2.2.2.1 General Stack Frame Requirements
;;;; The following general requirements apply to all stack frames:
;;;; * The stack shall be quadword aligned.
;;;; * The minimum stack frame size shall be 32 bytes.

;;;; "The stack pointer (stored in r1) shall maintain quadword alignment."
;;;; (quadword = 16 bytes)
(defconstant +stack-alignment-mask+ 15)
(defconstant +stack-frame-size+ #+little-endian 12 #+big-endian 14)

(defstruct arg-state
  (gpr-args 0)
  (fpr-args 0)
  (stack-frame-size +stack-frame-size+))

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
      (int-arg state 'signed-byte-64 signed-reg-sc-number signed-stack-sc-number)
      (int-arg state 'unsigned-byte-64 unsigned-reg-sc-number unsigned-stack-sc-number)))

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
(define-alien-type-method (single-float :arg-tn) (type state)
  (declare (ignore type))
  (let* ((fprs (arg-state-fpr-args state)))
    (cond ((< fprs 14)
           (incf (arg-state-gpr-args state))
           (incf (arg-state-fpr-args state))
           ;; Assign outgoing FPRs starting at FP1
           (make-wired-tn* 'single-float single-reg-sc-number (1+ fprs)))
          (t
           (let* ((stack-offset (arg-state-stack-frame-size state)))
             (setf (arg-state-stack-frame-size state) (+ stack-offset 1))
             (make-wired-tn* 'single-float single-stack-sc-number stack-offset))))))

(define-alien-type-method (double-float :arg-tn) (type state)
  (declare (ignore type))
  (let* ((fprs (arg-state-fpr-args state)))
    (cond ((< fprs 14)
           (incf (arg-state-gpr-args state))
           (incf (arg-state-fpr-args state))
           ;; Assign outgoing FPRs starting at FP1
           (make-wired-tn* 'double-float double-reg-sc-number (1+ fprs)))
          (t
           (let* ((stack-offset (arg-state-stack-frame-size state)))
             (if (oddp stack-offset)
               (incf stack-offset))
             (setf (arg-state-stack-frame-size state) (+ stack-offset 2))
             (make-wired-tn* 'double-float double-stack-sc-number stack-offset))))))

;;; Result state handling

(defstruct result-state
  (num-results 0))

(defun result-reg-offset (slot)
  (ecase slot
    (0 nl0-offset)
    (1 nl1-offset)))

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
            (values 'signed-byte-64 signed-reg-sc-number)
            (values 'unsigned-byte-64 unsigned-reg-sc-number))
      (make-wired-tn* ptype reg-sc (result-reg-offset num-results)))))

(defun make-call-out-tns (type)
  (declare (type alien-fun-type type))
  (let ((arg-state (make-arg-state)))
    (collect ((arg-tns))
      (dolist (arg-type (alien-fun-type-arg-types type))
        (arg-tns (invoke-alien-type-method :arg-tn arg-type arg-state)))
      (values (make-wired-tn* 'positive-fixnum any-reg-sc-number nsp-offset)
              (let ((size (arg-state-stack-frame-size arg-state)))
                (cond #+little-endian
                      ((= size +stack-frame-size+)
                       ;; no stack args
                       0)
                      (t
                       ;; it's later added by alloc-number-stack-space
                       (- size (/ number-stack-displacement n-word-bytes)))))
              (arg-tns)
              (invoke-alien-type-method
               :result-tn
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
    (let ((delta (- (logandc2 (+ (* amount n-word-bytes)
                                 number-stack-displacement +stack-alignment-mask+)
                              +stack-alignment-mask+))))
      (cond ((typep delta '(signed-byte 16))
             (inst stdu nsp-tn nsp-tn delta))
            (t
             (inst lr temp delta)
             (inst stdux nsp-tn nsp-tn temp))))
    (unless (location= result nsp-tn)
      ;; They are only location= when the result tn was allocated by
      ;; make-call-out-tns above, which takes the number-stack-displacement
      ;; into account itself.
      (inst addi result nsp-tn number-stack-displacement))))

(define-vop (dealloc-number-stack-space)
  (:info amount)
  (:policy :fast-safe)
  (:generator 0
    (let ((delta (logandc2 (+ (* amount n-word-bytes)
                              number-stack-displacement +stack-alignment-mask+)
                           +stack-alignment-mask+)))
      (cond ((typep delta '(signed-byte 16))
             (inst addi nsp-tn nsp-tn delta))
            (t
             (inst ld nsp-tn nsp-tn 0))))))

#-sb-xc-host
(progn
  (defun alien-callback-accessor-form (type sap offset)
    (let (#+big-endian
          (parsed-type (parse-alien-type type nil)))
      (cond #+big-endian
            ((sb-alien::alien-integer-type-p parsed-type)
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

  ;;; Returns a vector in static space containing machine code for the
  ;;; callback wrapper.
  (defun alien-callback-assembler-wrapper (index result-type argument-types)
    (flet ((make-gpr (n)
             (make-random-tn :kind :normal :sc (sc-or-lose 'any-reg) :offset n))
           (make-fpr (n)
             (make-random-tn :kind :normal :sc (sc-or-lose
                                                'double-reg) :offset
                                                n)))
      (let* ((segment (make-segment))
             #+big-endian
             (function-descriptor-size 24))
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
                                 +stack-alignment-mask+)
                              +stack-alignment-mask+))
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
                 (fprs (mapcar #'make-fpr '(1 2 3 4 5 6 7 8 9 10 11 12 13))))
            ;; Setup useful functions and then copy all args.
            (flet ((load-address-into (reg addr)
                     (let ((high (ldb (byte 16 16) addr))
                           (low (ldb (byte 16 0) addr)))
                       (inst lis reg high)
                       (inst ori reg reg low)))
                   (save-arg (type words)
                     (let ((integerp (not (alien-float-type-p type)))
                           (in-offset (+ (* in-words-processed n-word-bytes)
                                         (* +stack-frame-size+ n-word-bytes)))
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
                                        (inst std gpr stack-pointer
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
                                      (inst ld r0 stack-pointer in-offset)
                                      (inst std r0 stack-pointer out-offset)
                                      (incf out-words-processed)
                                      (incf out-offset n-word-bytes)
                                      (incf in-words-processed)
                                      (incf in-offset n-word-bytes)))))
                             ;; The handling of floats is a little ugly
                             ;; because we hard-code the number of words
                             ;; for single- and double-floats.
                             ((alien-single-float-type-p type)
                              (let ((fpr (pop fprs)))
                                (cond (fpr
                                       (pop gprs)
                                       (inst stfs fpr stack-pointer out-offset))
                                      (t
                                       ;; The ABI says that floats
                                       ;; stored on the stack are
                                       ;; promoted to doubles.  gcc
                                       ;; stores them as floats.
                                       ;; Follow gcc here.
                                       ;;  => no alignment needed either.
                                       (inst lfs f0 stack-pointer in-offset)
                                       (inst stfs f0 stack-pointer out-offset)
                                       (incf in-words-processed))))
                              (incf out-words-processed))
                             ((alien-double-float-type-p type)
                              (let ((fpr (pop fprs)))
                                (pop gprs)
                                (if fpr
                                    (inst stfd fpr stack-pointer out-offset)
                                    (progn
                                      (inst lfd f0 stack-pointer in-offset)
                                      (inst stfd f0 stack-pointer out-offset)
                                      (incf in-words-processed))))
                              (incf out-words-processed))
                             (t
                              (bug "Unknown alien floating point type: ~S" type))))))
              ;; Leave a gap for a PPC64ELF ABIv1 function descriptor,
              ;; to be filled in later relative to the SAP.
              #+big-endian
              (dotimes (k (/ function-descriptor-size 4)) ; nop is 4 bytes
                (inst nop))
              (mapc #'save-arg
                    argument-types
                    (mapcar (lambda (arg)
                              (ceiling (alien-type-bits arg) n-word-bits))
                            argument-types))

              ;; Arranged the args, allocated the return area.  Now
              ;; actuall call funcall3:  funcall3 (call-alien-function,
              ;; index, args, return-area)

              (destructuring-bind (arg1 arg2 arg3 arg4)
                  (mapcar #'make-gpr '(3 4 5 6))
                (load-address-into arg1 (static-fdefn-fun-addr 'enter-alien-callback))
                (loadw arg1 arg1)
                (inst li arg2 (fixnumize index))
                (inst addi arg3 stack-pointer (- arg-store-pos))
                (inst addi arg4 stack-pointer (- return-area-pos)))

              ;; Setup everything.  Now save sp, setup the frame.
              (inst mflr r0)
              (inst std r0 stack-pointer (* 2 n-word-bytes)) ; FIXME: magic
                                        ; constant, copied from Darwin.
              (inst stdu stack-pointer stack-pointer (- frame-size))

              ;; And make the call.
              #+little-endian
              (load-address-into
               r0
               (foreign-symbol-address "callback_wrapper_trampoline"))
              #+big-endian
              (destructuring-bind (r2 r12) (mapcar #'make-gpr '(2 12))
                (load-address-into
                 r12
                 (foreign-symbol-address "callback_wrapper_trampoline"))
                (inst ld r0 r12 0)
                (inst ld r2 r12 8))
              (inst mtlr r0)
              (inst blrl)

              ;; We're back!  Restore sp and lr, load the
              ;; return value from just under sp, and return.
              (inst ld stack-pointer stack-pointer 0)
              (inst ld r0 stack-pointer (* 2 n-word-bytes))
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
                       repeat n-return-area-words
                       for gpr = (pop gprs)
                       for offset from (- return-area-pos)
                       by n-word-bytes
                       do
                       (unless gpr
                         (bug "Out of return registers in alien-callback trampoline."))
                       (inst ld gpr stack-pointer offset))))
              (inst blr))))
        (finalize-segment segment)

        ;; Now that the segment is done, convert it to a static
        ;; vector we can point foreign code to.
        (let* ((buffer (sb-assem:segment-buffer segment))
               (vector (make-static-vector (length buffer)
                                           :element-type '(unsigned-byte 8)
                                           :initial-contents buffer))
               (sap (vector-sap vector)))
          ;; Fill in the PPC64ELF ABIv1 function descriptor that
          ;; points just past the end of itself, to the first
          ;; instruction of the wrapper.  This assembler wrapper only
          ;; cares about the address, so leave the other descriptor
          ;; fields filled with no-op instructions.
          #+big-endian
          (setf (sap-ref-64 sap 0) (+ (sap-int sap) function-descriptor-size))
          (alien-funcall
           (extern-alien "ppc_flush_icache"
                         (function void
                                   system-area-pointer
                                   unsigned-long))
           sap (length buffer))
          vector)))))

;;; For (CAS SAP-REF-{8,16,32})
(defknown sign-extend ((signed-byte 64) t) fixnum
    (foldable flushable movable))

(define-vop (sign-extend)
  (:translate sign-extend)
  (:policy :fast-safe)
  (:args (val :scs (signed-reg)))
  (:arg-types signed-num (:constant fixnum))
  (:info size)
  (:results (res :scs (signed-reg)))
  (:result-types fixnum)
  (:generator 1
    (inst* (ecase size (8 'extsb) (16 'extsh) (32 'extsw)) res val)))
