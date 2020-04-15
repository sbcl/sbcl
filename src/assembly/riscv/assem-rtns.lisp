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
#+sb-assembling ;; we don't want a vop for this one.
(define-assembly-routine
    (return-multiple
     (:return-style :none))
    ;; These four are really arguments.
    ((:temp nvals any-reg nargs-offset)
     (:temp vals any-reg nl0-offset)
     (:temp ocfp any-reg nl1-offset)
     (:temp lra descriptor-reg lra-offset)

     ;; These are just needed to facilitate the transfer
     (:temp count any-reg nl2-offset)
     (:temp dst any-reg nl3-offset)
     (:temp temp descriptor-reg l0-offset)

     ;; These are needed so we can get at the register arg
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
    (inst addi vals vals (* register-arg-count n-word-bytes))
    (inst addi dst cfp-tn (* register-arg-count n-word-bytes))

    (emit-label loop)
    (loadw temp vals)
    (inst addi vals vals n-word-bytes)
    (inst subi count count (fixnumize 1))
    (storew temp dst)
    (inst addi dst dst n-word-bytes)
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
    (inst add csp-tn ocfp-tn nvals))
  ;; Return.
  (lisp-return lra :multiple-values))

#+sb-assembling ;; no vop for this one either.
(define-assembly-routine
    (tail-call-variable
     (:return-style :none))

    ;; These are really args.
    ((:temp args any-reg nl0-offset)
     (:temp lexenv descriptor-reg lexenv-offset)

     ;; We need to compute this
     (:temp nargs any-reg nargs-offset)

     ;; These are needed by the blitting code.
     (:temp src any-reg nl1-offset)
     (:temp dst any-reg nl2-offset)
     (:temp count any-reg nl3-offset)
     (:temp temp descriptor-reg l0-offset))


  ;; Calculate NARGS (as a fixnum)
  (inst sub nargs csp-tn args)

  ;; Load the argument regs (must do this now, 'cause the blt might
  ;; trash these locations)
  (loop for an in *register-arg-tns*
        for i from 0
        do (loadw an args i))

  ;; Calc SRC, DST, and COUNT
  (inst subi count nargs (* register-arg-count n-word-bytes))
  (inst addi src args (* register-arg-count n-word-bytes))
  (inst bge zero-tn count done)
  (inst addi dst cfp-tn (* register-arg-count n-word-bytes))

  LOOP
  ;; Copy one arg.
  (loadw temp src)
  (inst addi src src n-word-bytes)
  (storew temp dst)
  (inst subi count count n-word-bytes)
  (inst addi dst dst n-word-bytes)
  (inst blt zero-tn count LOOP)

  DONE
  ;; We are done.  Do the jump.
  (with-word-index-as-fixnum (nargs nargs))
  (loadw temp lexenv closure-fun-slot fun-pointer-lowtag)
  (lisp-jump temp))


;;;; Non-local exit noise.

(define-assembly-routine (throw (:return-style :none))
  ((:arg target (descriptor-reg any-reg) a0-offset)
   (:arg start (descriptor-reg any-reg) ocfp-offset)
   (:arg count (descriptor-reg any-reg) nargs-offset)
   (:temp catch any-reg a1-offset)
   (:temp tag descriptor-reg a2-offset))
  (declare (ignore start count)) ; We only need them in the registers.

  (load-current-catch-block catch)

  LOOP
  (let ((error (generate-error-code nil 'unseen-throw-tag-error target)))
    (inst beq catch zero-tn error))

  (loadw tag catch catch-block-tag-slot)
  (inst beq tag target EXIT)
  (loadw catch catch catch-block-previous-catch-slot)
  (inst j loop)

  EXIT
  (move target catch) ;; TARGET coincides with UNWIND's BLOCK argument
  (invoke-asm-routine 'unwind))

(define-assembly-routine (unwind
                          (:return-style :none)
                          (:translate %unwind)
                          (:policy :fast-safe))
  ((:arg block (descriptor-reg any-reg) a0-offset)
   (:arg start (descriptor-reg any-reg) ocfp-offset)
   (:arg count (descriptor-reg any-reg) nargs-offset)
   (:temp lra descriptor-reg lra-offset)
   (:temp cur-uwp any-reg nl0-offset)
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
  (loadw lra cur-uwp unwind-block-entry-pc-slot)
  (lisp-return lra :known)

  DO-UWP
  (loadw target-uwp cur-uwp unwind-block-uwp-slot)
  (store-current-unwind-protect-blocK target-uwp)
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
    (inst addi nsp-tn nsp-tn framesize)))

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
    (storew zero-tn thread-base-tn thread-foreign-function-call-active-slot))
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
    (storew null-tn thread-base-tn thread-foreign-function-call-active-slot))
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

     (:temp a0 descriptor-reg a0-offset)

     (:temp lra descriptor-reg lra-offset)
     (:temp value (descriptor-reg any-reg) ca0-offset)

     ;; Don't want these to overlap with C args.
     (:temp pa-temp non-descriptor-reg nl6-offset)
     (:temp temp non-descriptor-reg nl7-offset))
  (save-c-registers)

  (initialize-boxed-regs (list function arg-ptr nargs #+sb-thread tls-ptr))
  #+sb-thread
  (move thread-base-tn tls-ptr)

  ;; Tag nargs.
  (inst slli nargs-tn nargs n-fixnum-tag-bits)
  (pseudo-atomic (pa-temp)
    (set-up-lisp-context csp-tn ocfp-tn temp))

  ;; Pass in the args.
  (move lexenv-tn function)
  (move cfp-tn arg-ptr)
  (loop for arg in *register-arg-tns*
        for index from 0
        do (loadw arg cfp-tn index))

  (inst compute-lra lra lip-tn lra-label)
  ;; Indirect closure.
  (loadw code-tn lexenv-tn closure-fun-slot fun-pointer-lowtag)
  ;; Call into Lisp!
  (inst jalr zero-tn code-tn (- (* simple-fun-insts-offset n-word-bytes)
                                fun-pointer-lowtag))
  (emit-alignment n-lowtag-bits)
  LRA-LABEL
  (inst machine-word return-pc-widetag)

  (inst blt nargs-tn zero-tn single-value-return)
  (move csp-tn ocfp-tn)

  SINGLE-VALUE-RETURN
  ;; Return the one value.
  (move value a0)
  ;; Save Lisp state.
  (pseudo-atomic (pa-temp)
    (save-lisp-context csp-tn cfp-tn temp))

  (restore-c-registers)
  (inst jalr zero-tn lip-tn 0))

(define-assembly-routine (call-into-c (:return-style :none))
    ((:arg cfunc (any-reg) cfunc-offset)

     (:temp ca0 (any-reg) ca0-offset)
     (:temp ca1 (any-reg) ca1-offset)

     (:temp value0-pass (any-reg) (result-reg-offset 0))
     (:temp value1-pass (any-reg) (result-reg-offset 1))
     (:temp pa-temp (any-reg) nl6-offset)
     (:temp temp (any-reg) nl7-offset))
  ;; The C stack frame and argument registers should have already been
  ;; set up in Lisp.

  ;; Setup Lisp stack to account for the "foreign"
  ;; frame. Look in code/debug-int.lisp to see how to do this.
  (move ocfp-tn cfp-tn)
  (move cfp-tn csp-tn)
  (inst addi csp-tn cfp-tn 32)

  (pseudo-atomic (pa-temp)
    ;; Convert the return address to an offset and save it on the stack.
    (inst sub nfp-tn lip-tn code-tn)
    (inst addi nfp-tn nfp-tn other-pointer-lowtag)
    ;; Ensure it has fixnum nature so GC doesn't croak.
    (when (> n-fixnum-tag-bits 2)
      (inst slli nfp-tn nfp-tn (- n-fixnum-tag-bits 2)))
    (storew ocfp-tn cfp-tn)
    (storew nfp-tn cfp-tn 1)
    (storew code-tn cfp-tn 2)

    (save-lisp-context csp-tn cfp-tn temp))

  ;; Call into C.
  (loadw cfunc cfunc) ; dereference the linkage table entry
  (inst jalr lip-tn cfunc 0)

  ;; Pass the return values to Lisp.
  (move value0-pass ca0)
  (move value1-pass ca1)

  ;; FIXME: Don't need to initialize all. e.g. callee saved or ones that get initialized anyway.
  (initialize-boxed-regs (list #+sb-thread thread-base-tn))

  (pseudo-atomic (pa-temp)
    ;; FIXME: We could do some trickery like in other backends where
    ;; we save the lisp context registers to C's callee saved
    ;; registers and not bother loading them out again..
    (set-up-lisp-context csp-tn cfp-tn temp)
    (loadw ocfp-tn cfp-tn 0)
    (loadw nfp-tn cfp-tn 1)
    (loadw code-tn cfp-tn 2)
    (when (> n-fixnum-tag-bits 2)
      (inst srli nfp-tn nfp-tn (- n-fixnum-tag-bits 2)))
    (inst add lip-tn nfp-tn code-tn))

  ;; Reset the Lisp stack.
  (move csp-tn cfp-tn)
  (move cfp-tn ocfp-tn)

  (inst jalr zero-tn lip-tn (- other-pointer-lowtag)))

(define-assembly-routine (do-pending-interrupt (:return-style :none))
    ()
  (inst ebreak pending-interrupt-trap)
  (emit-alignment 2))
