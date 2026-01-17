;;;; the machine specific support routines needed by the file assembler

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;;; Return-multiple with other than one value
#+sb-assembling
(define-assembly-routine
    (return-multiple
     (:return-style :none))
    ((:temp nvals any-reg nargs-offset)
     (:temp vals any-reg nl0-offset)
     (:temp ocfp any-reg nl1-offset)
     (:temp ra non-descriptor-reg ra-offset)

     (:temp count any-reg nl2-offset)
     (:temp dst any-reg nl3-offset)
     (:temp temp descriptor-reg l0-offset)

     (:temp a0 descriptor-reg a0-offset)
     (:temp a1 descriptor-reg a1-offset))
  ;; Note, because of the way the return-multiple vop is
  ;; written, we can assume that we are never called
  ;; with nvals == 1 and that a0 has already been
  ;; loaded.
  (inst subi count nvals (fixnumize 2))
  (let ((defaulting-labels (loop repeat (- register-arg-count 1)
                                 collect (gen-label)))
        (loop (gen-label))
        (default-a0-and-on (gen-label)))
    (inst bge zero-tn nvals default-a0-and-on)
    (loop for label in defaulting-labels
          for an in (rest *register-arg-tns*)
          for i from 1
          do (progn
               (unless (= i 1)
                 (inst subi count count (fixnumize 1)))
               (loadw an vals i)
               (inst bge zero-tn count label)))
    ;; Copy the remaining args to the top of the stack.
    (inst addi.d vals vals (* register-arg-count n-word-bytes))
    (inst addi.d dst cfp-tn (* register-arg-count n-word-bytes))

    (emit-label loop)
    (loadw temp vals)
    (inst addi.d vals vals n-word-bytes)
    (inst subi count count (fixnumize 1))
    (storew temp dst)
    (inst addi.d dst dst n-word-bytes)
    (inst bne count zero-tn loop)

    (inst j (first (last defaulting-labels)))

    (emit-label default-a0-and-on)
    (move a0 null-tn)
    (move a1 null-tn)
    (loop for defaulting-label in defaulting-labels
          for an in (rest (rest *register-arg-tns*))
          do (progn
               (emit-label defaulting-label)
               (move an null-tn)))
    (emit-label (first (last defaulting-labels))))

  ;; Clear the stack.
  (move ocfp-tn cfp-tn)
  (move cfp-tn ocfp)
  (with-fixnum-as-word-index (nvals temp)
    (inst add.d csp-tn ocfp-tn nvals))
  (inst jirl zero-tn ra 0))

#+sb-assembling
(define-assembly-routine
    (tail-call-variable
     (:return-style :none))
    ((:temp args any-reg nl0-offset)
     (:temp lexenv descriptor-reg lexenv-offset)
     (:temp nargs any-reg nargs-offset)
     (:temp src any-reg nl1-offset)
     (:temp dst any-reg nl2-offset)
     (:temp count any-reg nl3-offset)
     (:temp temp descriptor-reg l0-offset)
     (:temp tmp descriptor-reg t7-offset)
     (:temp function descriptor-reg l0-offset))
  (inst sub.d nargs csp-tn args)
  (loop for an in *register-arg-tns*
        for i from 0
        do (loadw an args i))
  (inst subi count nargs (* register-arg-count n-word-bytes))
  (inst addi.d src args (* register-arg-count n-word-bytes))
  (inst bge zero-tn count done)
  (inst addi.d dst cfp-tn (* register-arg-count n-word-bytes))

  LOOP
  (loadw temp src)
  (inst addi.d src src n-word-bytes)
  (storew temp dst)
  (inst subi count count n-word-bytes)
  (inst addi.d dst dst n-word-bytes)
  (inst blt zero-tn count LOOP)

  DONE
  (with-word-index-as-fixnum (nargs nargs))
  (loadw function lexenv closure-fun-slot fun-pointer-lowtag)

  (inst addi.d tmp function (- fun-pointer-lowtag))
  (inst jirl   zero-tn tmp (ash simple-fun-insts-offset word-shift)))

;;;; Non-local exit noise.
(define-assembly-routine (throw
                          (:return-style :full-call-no-return)
                          (:save-p :compute-only))
    ((:arg target (descriptor-reg any-reg) a0-offset)
     (:arg start (descriptor-reg any-reg) ocfp-offset)
     (:arg count (descriptor-reg any-reg) nargs-offset)
     (:temp catch any-reg a1-offset)
     (:temp tag descriptor-reg a2-offset))
  (declare (ignore start count))
  (load-current-catch-block catch)
  LOOP
  (let ((error (generate-error-code nil 'unseen-throw-tag-error target)))
    (inst beq catch zero-tn error))

  (loadw tag catch catch-block-tag-slot)
  (inst beq tag target EXIT)
  (loadw catch catch catch-block-previous-catch-slot)
  (inst j LOOP)

  EXIT
  (move target catch)
  (inst jal zero-tn (make-fixup 'unwind :assembly-routine)))

(define-assembly-routine (unwind
                          (:translate %unwind)
                          (:policy :fast-safe)
                          (:return-style :full-call-no-return)
                          (:save-p :compute-only))
    ((:arg block (descriptor-reg any-reg) a0-offset)
     (:arg start (descriptor-reg any-reg) ocfp-offset)
     (:arg count (descriptor-reg any-reg) nargs-offset)
     (:temp cur-uwp any-reg nl0-offset)
     (:temp temp any-reg nl1-offset)
     (:temp target-uwp any-reg nl2-offset))
  (declare (ignore start count))
  (let ((error (generate-error-code nil 'invalid-unwind-error)))
    (inst beq block zero-tn error))

  (load-current-unwind-protect-block cur-uwp)
  (loadw target-uwp block unwind-block-uwp-slot)
  (inst bne cur-uwp target-uwp DO-UWP)
  (move cur-uwp block)

  DO-EXIT
  (loadw cfp-tn cur-uwp unwind-block-cfp-slot)
  (loadw code-tn cur-uwp unwind-block-code-slot)
  (loadw temp cur-uwp unwind-block-entry-pc-slot)
  (inst jirl zero-tn temp 0)

  DO-UWP
  (loadw target-uwp cur-uwp unwind-block-uwp-slot)
  (store-current-unwind-protect-block target-uwp)
  (inst j DO-EXIT))

;;;; Some runtime routines.
(let* ((n-saved-registers (length c-saved-registers))
       (n-saved-float-registers (length c-saved-float-registers))
       (framesize (+ (* n-word-bytes n-saved-registers)
                     (* 8 n-saved-float-registers))))
  (defun save-c-registers ()
    (inst subi nsp-tn nsp-tn framesize)
    (loop for offset from 0
          for saved-offset in c-saved-registers
          do (storew (make-reg-tn saved-offset) nsp-tn offset))
    (loop for offset from (* n-word-bytes n-saved-registers) by 8
          for saved-offset in c-saved-float-registers
          do (inst fstore :double (make-reg-tn saved-offset) nsp-tn offset)))

  (defun restore-c-registers ()
    (loop for offset from 0
          for saved-offset in c-saved-registers
          do (loadw (make-reg-tn saved-offset) nsp-tn offset))
    (loop for offset from (* n-word-bytes n-saved-registers) by 8
          for saved-offset in c-saved-float-registers
          do (inst fload :double (make-reg-tn saved-offset) nsp-tn offset))
    (inst addi.d nsp-tn nsp-tn framesize)))

(defun initialize-boxed-regs (&optional still-live)
  (dolist (boxed-reg-offset boxed-regs)
    (unless (member boxed-reg-offset still-live :key #'tn-offset)
      (inst li (make-reg-tn boxed-reg-offset) 0)))
  (inst li null-tn nil-value))

(defun set-up-lisp-context (stack-pointer frame-pointer temp)
  #+sb-thread
  (declare (ignore temp))
  ;; Initializing the allocation pointer is done already in
  ;; coreparse.
  #+sb-thread
  (progn
    (loadw stack-pointer thread-base-tn thread-control-stack-pointer-slot)
    (loadw frame-pointer thread-base-tn thread-control-frame-pointer-slot)
    (storew zero-tn thread-base-tn thread-ffcall-active-p-slot))
  #-sb-thread
  (progn
    (load-foreign-symbol-value stack-pointer "current_control_stack_pointer" temp)
    (load-foreign-symbol-value frame-pointer "current_control_frame_pointer" temp)
    (store-foreign-symbol-value zero-tn "foreign_function_call_active" temp)))

(defun save-lisp-context (stack-pointer frame-pointer temp)
  #+sb-thread
  (declare (ignore temp))
  #+sb-thread
  (progn
    (storew stack-pointer thread-base-tn thread-control-stack-pointer-slot)
    (storew frame-pointer thread-base-tn thread-control-frame-pointer-slot)
    (storew null-tn thread-base-tn thread-ffcall-active-p-slot))
  #-sb-thread
  (progn
    (store-foreign-symbol-value stack-pointer "current_control_stack_pointer" temp)
    (store-foreign-symbol-value frame-pointer "current_control_frame_pointer" temp)
    (store-foreign-symbol-value null-tn "foreign_function_call_active" temp)))

(define-assembly-routine (call-into-lisp (:return-style :none))
    ((:arg function (descriptor-reg any-reg) ca0-offset)
     (:arg arg-ptr (descriptor-reg any-reg) ca1-offset)
     (:arg nargs (any-reg) ca2-offset)
     #+sb-thread
     (:arg tls-ptr (any-reg) ca3-offset)
     (:arg ra (any-reg) ra-offset)
     (:temp a0 descriptor-reg a0-offset)
     (:temp value (descriptor-reg any-reg) ca0-offset)
     (:temp fun (descriptor-reg) l0-offset)
     (:temp pa-temp non-descriptor-reg nl4-offset)
     (:temp temp non-descriptor-reg nl5-offset)
     (:temp tmp non-descriptor-reg t7-offset))
  (save-c-registers)
  (initialize-boxed-regs (list function arg-ptr nargs #+sb-thread tls-ptr))
  #+sb-thread
  (move thread-base-tn tls-ptr)
  (inst slli.d nargs-tn nargs n-fixnum-tag-bits)

  (pseudo-atomic (pa-temp)
    (set-up-lisp-context csp-tn ocfp-tn temp))

  (move lexenv-tn function)
  (move cfp-tn arg-ptr)

  (loop for arg in *register-arg-tns*
        for index from 0
        do (loadw arg cfp-tn index))

  (loadw fun lexenv-tn closure-fun-slot fun-pointer-lowtag)

  (inst addi.d tmp fun (- fun-pointer-lowtag))
  (inst jirl ra tmp (* simple-fun-insts-offset n-word-bytes))

  (inst blt nargs-tn zero-tn single-value-return)
  (move csp-tn ocfp-tn)

  SINGLE-VALUE-RETURN
  (move value a0)

  (pseudo-atomic (pa-temp)
    (save-lisp-context csp-tn cfp-tn temp))

  (restore-c-registers)

  (inst jirl zero-tn ra 0))

(define-assembly-routine (call-into-c (:return-style :none))
    ((:arg cfunc (any-reg) cfunc-offset)
     (:temp ca0 (any-reg) ca0-offset)
     (:temp ca1 (any-reg) ca1-offset)
     (:temp value0-pass (any-reg) (result-reg-offset 0))
     (:temp value1-pass (any-reg) (result-reg-offset 1))
     (:temp pa-temp (any-reg) nl4-offset)
     (:temp temp (any-reg) nl5-offset))
  (move ocfp-tn cfp-tn)
  (move cfp-tn csp-tn)
  (inst addi.d csp-tn cfp-tn 32)
  (pseudo-atomic (pa-temp)
    (storew ocfp-tn cfp-tn)
    (storew ra-tn cfp-tn 1)
    (storew code-tn cfp-tn 2)
    (save-lisp-context csp-tn cfp-tn temp))
  (inst jirl ra-tn cfunc 0)
  (move value0-pass ca0)
  (move value1-pass ca1)

  (initialize-boxed-regs (list #+sb-thread thread-base-tn))

  (pseudo-atomic (pa-temp)
    (set-up-lisp-context csp-tn cfp-tn temp)
    (loadw ocfp-tn cfp-tn 0)
    (loadw ra-tn cfp-tn 1)
    (loadw code-tn cfp-tn 2))

  (move csp-tn cfp-tn)
  (move cfp-tn ocfp-tn)
  (inst jirl zero-tn ra-tn 0))

(define-assembly-routine (do-pending-interrupt (:return-style :none))
    ()
  (inst break 0)
  (inst byte pending-interrupt-trap)
  (emit-alignment 2))
