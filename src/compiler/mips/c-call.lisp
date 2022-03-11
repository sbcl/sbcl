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
            (values 'signed-byte-32 signed-reg-sc-number signed-stack-sc-number)
            (values 'unsigned-byte-32 unsigned-reg-sc-number unsigned-stack-sc-number))
      (if (< stack-frame-size 4)
          (make-wired-tn* ptype reg-sc (+ stack-frame-size 4))
          (make-wired-tn* ptype stack-sc stack-frame-size)))))

(define-alien-type-method (system-area-pointer :arg-tn) (type state)
  (declare (ignore type))
  (let ((stack-frame-size (arg-state-stack-frame-size state)))
    (setf (arg-state-stack-frame-size state) (1+ stack-frame-size))
    (setf (arg-state-did-int-arg state) t)
    (if (< stack-frame-size 4)
        (make-wired-tn* 'system-area-pointer sap-reg-sc-number
                          (+ stack-frame-size 4))
        (make-wired-tn* 'system-area-pointer sap-stack-sc-number
                          stack-frame-size))))

(define-alien-type-method (double-float :arg-tn) (type state)
  (declare (ignore type))
  (let ((stack-frame-size (logandc2 (1+ (arg-state-stack-frame-size state)) 1))
        (float-args (arg-state-float-args state)))
    (setf (arg-state-stack-frame-size state) (+ stack-frame-size 2))
    (setf (arg-state-float-args state) (1+ float-args))
    (cond ((>= stack-frame-size 4)
           (make-wired-tn* 'double-float double-stack-sc-number
                             stack-frame-size))
          ((and (not (arg-state-did-int-arg state))
                (< float-args 2))
           (make-wired-tn* 'double-float double-reg-sc-number
                             (+ (* float-args 2) 12)))
          (t
           (make-wired-tn* 'double-float double-int-carg-reg-sc-number
                             (+ stack-frame-size 4))))))

(define-alien-type-method (single-float :arg-tn) (type state)
  (declare (ignore type))
  (let ((stack-frame-size (arg-state-stack-frame-size state))
        (float-args (arg-state-float-args state)))
    (setf (arg-state-stack-frame-size state) (1+ stack-frame-size))
    (setf (arg-state-float-args state) (1+ float-args))
    (cond ((>= stack-frame-size 4)
           (make-wired-tn* 'single-float single-stack-sc-number
                             stack-frame-size))
          ((and (not (arg-state-did-int-arg state))
                (< float-args 2))
           (make-wired-tn* 'single-float single-reg-sc-number
                             (+ (* float-args 2) 12)))
          (t
           (make-wired-tn* 'single-float single-int-carg-reg-sc-number
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
            (values 'signed-byte-32 signed-reg-sc-number)
            (values 'unsigned-byte-32 unsigned-reg-sc-number))
      (make-wired-tn* ptype reg-sc (result-reg-offset num-results)))))

(define-alien-type-method (system-area-pointer :result-tn) (type state)
  (declare (ignore type))
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (make-wired-tn* 'system-area-pointer sap-reg-sc-number (result-reg-offset num-results))))

;;; FIXME: do these still work? -- CSR, 2002-08-28
(define-alien-type-method (double-float :result-tn) (type state)
  (declare (ignore type))
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (make-wired-tn* 'double-float double-reg-sc-number (* num-results 2))))

(define-alien-type-method (single-float :result-tn) (type state)
  (declare (ignore type))
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (make-wired-tn* 'single-float single-reg-sc-number (* num-results 2))))

(define-alien-type-method (values :result-tn) (type state)
  (let ((values (alien-values-type-values type)))
    (when (> (length values) 2)
      (error "Too many result values from c-call."))
    (mapcar #'(lambda (type)
                (invoke-alien-type-method :result-tn type state))
            values)))

(defun make-call-out-tns (type)
  (let ((arg-state (make-arg-state)))
    (collect ((arg-tns))
      (dolist (arg-type (alien-fun-type-arg-types type))
        (arg-tns (invoke-alien-type-method :arg-tn arg-type arg-state)))
      (values (make-wired-tn* 'positive-fixnum any-reg-sc-number nsp-offset)
              (* (max (arg-state-stack-frame-size arg-state) 4) n-word-bytes)
              (arg-tns)
              (invoke-alien-type-method :result-tn
                                        (alien-fun-type-result-type type)
                                        (make-result-state))))))

(deftransform %alien-funcall ((function type &rest args))
  (aver (sb-c:constant-lvar-p type))
  (let* ((type (sb-c:lvar-value type))
         (env (make-null-lexenv))
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
                            ;; consecutive locations, endian word order,
                            ;; aligned to 8 bytes.
                            (when (oddp (length (new-args)))
                              (new-args nil))
                            #-little-endian
                            (progn (new-args `(ash ,arg -32))
                                   (new-args `(logand ,arg #xffffffff))
                                   (if (oddp (length (new-arg-types)))
                                       (new-arg-types (parse-alien-type '(unsigned 32) env)))
                                   (if (alien-integer-type-signed type)
                                       (new-arg-types (parse-alien-type '(signed 32) env))
                                       (new-arg-types (parse-alien-type '(unsigned 32) env)))
                                   (new-arg-types (parse-alien-type '(unsigned 32) env)))
                            #+little-endian
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
                             (> (sb-alien::alien-integer-type-bits result-type) 32))
                        (let ((new-result-type
                               (let ((sb-alien::*values-type-okay* t))
                                 (parse-alien-type
                                  (if (alien-integer-type-signed result-type)
                                      #-little-endian
                                      '(values (signed 32) (unsigned 32))
                                      #+little-endian
                                      '(values (unsigned 32) (signed 32))
                                      '(values (unsigned 32) (unsigned 32)))
                                  env))))
                          `(lambda (function type ,@(lambda-vars))
                            (declare (ignore type))
                             (multiple-value-bind
                               #-little-endian
                               (high low)
                               #+little-endian
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
  (:temporary (:sc any-reg :offset nl3-offset) tramp) ; = $at
  (:vop-var vop)
  (:generator 0
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (store-stack-tn nfp-save cur-nfp))
      ;; (linkage-table-entry-address 0) is "call-into-c" in mips-assem.S
      (inst lw tramp null-tn (- (linkage-table-entry-address 0) nil-value))
      (inst nop)
      (inst jal tramp)
      (inst move cfunc function)
      (when cur-nfp
        (load-stack-tn cur-nfp nfp-save)))))

(define-vop (alloc-number-stack-space)
  (:info amount)
  (:result-types system-area-pointer)
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

#-sb-xc-host
(defun alien-callback-accessor-form (type sap offset)
  ;; KLUDGE: OFFSET is given to be word-aligned (32-bit aligned), but
  ;; double-float and long-long (both 64-bit wide) arguments need to
  ;; be 64-bit aligned.  And then there's the bit where sub-word-wide
  ;; values are aligned towards the END of the word on big-endian
  ;; systems.  For both cases, we need to parse the alien type, but we
  ;; aren't given the environment that our caller has access to, so we
  ;; substitute a null lexenv (KLUDGE number one), then we do some bit
  ;; twiddling to determine how much alignment we need.  Why, oh why
  ;; can't we have something like the :ARG-TN methods for all of this
  ;; mess?  -- AB, 2015-Nov-02
  (let* ((parsed-type (parse-alien-type type nil))
         (alignment-bits (sb-alien::alien-type-alignment parsed-type))
         (alignment-bytes (truncate alignment-bits n-byte-bits))
         ;; OFFSET is at least 32-bit aligned, we're trying to pick
         ;; out the cases where we need 64-bit alignment.
         (delta (logand offset (1- alignment-bytes))))
    (values
     (ecase *backend-byte-order*
       (:big-endian
        (if (alien-integer-type-p parsed-type)
            (let ((bits (sb-alien::alien-integer-type-bits parsed-type)))
              (let ((byte-offset
                     (cond ((< bits n-word-bits)
                            (- n-word-bytes
                               (ceiling bits n-byte-bits)))
                           (t 0))))
                `(deref (sap-alien (sap+ ,sap
                                         ,(+ byte-offset offset delta))
                                   (* ,type)))))
          `(deref (sap-alien (sap+ ,sap ,(+ offset delta)) (* ,type)))))
       (:little-endian
        `(deref (sap-alien (sap+ ,sap ,(+ offset delta)) (* ,type)))))
     delta)))

;;; Returns a vector in static space containing machine code for the
;;; callback wrapper
#-sb-xc-host
(defun alien-callback-assembler-wrapper (index result-type argument-types)
  "Cons up a piece of code which calls enter-alien-callback with INDEX
and a pointer to the arguments."
  (flet ((make-gpr (n)
           (make-random-tn :kind :normal :sc (sc-or-lose 'any-reg) :offset n))
         (make-fpr (n)
           (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg) :offset n)))
    (let* ((segment (make-segment))
           (n-argument-words
             (mapcar (lambda (arg) (ceiling (alien-type-bits arg) n-word-bits))
                     argument-types))
           (n-linkage-area-bytes 8)
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
           (int-seen)
           (gprs (mapcar #'make-gpr '(4 5 6 7)))
           (fprs (mapcar #'make-fpr '(12 14))))
      (flet ((save-arg (type words)
             (let ((offset (* words-processed n-word-bytes)))
               (cond ((not (alien-float-type-p type))
                      (when (and (alien-integer-type-p type)
                                 (> (sb-alien::alien-integer-type-bits type)
                                    n-word-bits)
                                 (oddp words-processed))
                        (pop gprs)
                        (incf words-processed)
                        (incf offset n-word-bytes))
                      (when gprs
                        (loop repeat words
                          for gpr = (pop gprs)
                          when gpr do
                            (inst sw gpr nsp-tn offset)
                          do
                            (setf int-seen t)
                            (incf words-processed)
                            (incf offset n-word-bytes))))
                     ((alien-single-float-type-p type)
                      (when gprs
                        (let ((gpr (pop gprs))
                              (fpr (pop fprs)))
                          (cond
                           ((and (not int-seen) fpr)
                            (inst swc1 fpr nsp-tn offset))
                           (gpr
                            (inst sw gpr nsp-tn offset))))
                        (incf words-processed)))
                     ((alien-double-float-type-p type)
                      (when (oddp words-processed)
                        (pop gprs)
                        (incf words-processed)
                        (incf offset n-word-bytes))
                      (when gprs
                        (let* ((gpr1 (pop gprs))
                               (gpr2 (pop gprs))
                               (fpr (pop fprs)))
                          (cond
                           ((and (not int-seen) fpr)
                            (ecase *backend-byte-order*
                              (:big-endian
                               (inst swc1 fpr nsp-tn (+ offset n-word-bytes))
                               (inst swc1-odd fpr nsp-tn offset))
                              (:little-endian
                               (inst swc1 fpr nsp-tn offset)
                               (inst swc1-odd fpr nsp-tn (+ offset n-word-bytes)))))
                           (gpr1
                            (ecase *backend-byte-order*
                              (:big-endian
                               (inst sw gpr1 nsp-tn offset)
                               (inst sw gpr2 nsp-tn (+ offset n-word-bytes)))
                              (:little-endian
                               (inst sw gpr2 nsp-tn offset)
                               (inst sw gpr1 nsp-tn (+ offset n-word-bytes)))))))
                        (incf words-processed 2)))
                     (t
                      (bug "Unknown alien floating point type: ~S" type))))))
        (assemble (segment 'nil)
          (mapc #'save-arg argument-types n-argument-words)
          ;; funcall3 (enter-alien-callback, index, args, return-area)
          ;;
          ;; INDEX is fixnumized, ARGS and RETURN-AREA don't need to be
          ;; because they're word-aligned. Kinda gross, but hey ...
          (destructuring-bind (v0 v1 a0 a1 a2 a3 t9 gp sp ra)
              (mapcar #'make-gpr '(2 3 4 5 6 7 25 28 29 31))
            ;; Allocate stack frame.
            (inst subu sp n-frame-bytes)

            ;; Save GP and RA.
            (inst sw gp sp (- n-frame-bytes (* 2 n-word-bytes)))
            (inst sw ra sp (- n-frame-bytes n-word-bytes))

            ;; Setup the args and make the call.
            (inst li a0 (static-fdefn-fun-addr 'enter-alien-callback))
            (inst lw a0 a0)
            (inst li t9 (foreign-symbol-address "funcall3"))
            (inst li a1 (fixnumize index))
            (inst addu a2 sp n-frame-bytes)
            (inst jal t9)
            (inst addu a3 sp n-callee-register-args-bytes)

            ;; We're back! Restore GP.
            (inst lw gp sp (- n-frame-bytes (* 2 n-word-bytes)))

            ;; Load the return value.
            (cond
              ((alien-single-float-type-p result-type)
               (inst lwc1 (make-fpr 0) sp n-callee-register-args-bytes))
              ((alien-double-float-type-p result-type)
               (ecase *backend-byte-order*
                 (:big-endian
                  (inst lwc1 (make-fpr 0) sp (+ n-callee-register-args-bytes
                                                n-word-bytes))
                  (inst lwc1 (make-fpr 1) sp n-callee-register-args-bytes))
                 (:little-endian
                  (inst lwc1 (make-fpr 0) sp n-callee-register-args-bytes)
                  (inst lwc1 (make-fpr 1) sp (+ n-callee-register-args-bytes
                                                n-word-bytes)))))
              ((and (alien-integer-type-p result-type)
                    (> (sb-alien::alien-integer-type-bits result-type)
                       n-word-bits))
               (inst lw v0 sp n-callee-register-args-bytes)
               (inst lw v1 sp (+ n-callee-register-args-bytes n-word-bytes)))
              ((or (alien-integer-type-p result-type)
                   (alien-pointer-type-p result-type)
                   (alien-type-= #.(parse-alien-type 'system-area-pointer nil)
                                   result-type))
               (inst lw v0 sp n-callee-register-args-bytes))
              ((alien-void-type-p result-type))
              (t
               (error "unrecognized alien type: ~A" result-type)))

            ;; Restore RA, free stack frame, and return.
            (inst lw ra sp (- n-frame-bytes n-word-bytes))
            (inst j ra)
            (inst addu sp n-frame-bytes))))
      (finalize-segment segment)
      ;; Now that the segment is done, convert it to a static
      ;; vector we can point foreign code to.
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
