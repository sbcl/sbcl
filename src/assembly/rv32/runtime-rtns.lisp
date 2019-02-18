;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")


(defconstant-eqx c-saved-registers
    (list* lr-offset
           global-offset
           8 9 (loop for i from 18 to 27 collect i))
  #'equal)

(defenum (:start 10)
  ca0-offset
  ca1-offset
  ca2-offset
  ca3-offset)

(defun make-any-reg-tn (offset)
  (make-random-tn :kind :normal
                  :sc (sc-or-lose 'any-reg)
                  :offset offset))

(defun save-c-registers ()
  (let ((framesize (* n-word-bytes (length c-saved-registers))))
    (inst subi nsp-tn nsp-tn framesize)
    (loop for offset from 0
          for saved-offset in c-saved-registers
          do (storew (make-any-reg-tn saved-offset) nsp-tn offset))))

(defun restore-c-registers ()
  (let ((framesize (* n-word-bytes (length c-saved-registers))))
    (loop for offset from 0
          for saved-offset in c-saved-registers
          do (loadw (make-any-reg-tn saved-offset) nsp-tn offset))
    (inst addi nsp-tn nsp-tn framesize)))

(defun initialize-boxed-regs (&optional still-live)
  (dolist (boxed-reg-offset boxed-regs)
    (unless (member boxed-reg-offset still-live)
      (inst li (make-any-reg-tn boxed-reg-offset) 0)))
  ;; FIXME
  (inst li null-tn #x8000017))

(define-assembly-routine (call-into-lisp (:return-style :none))
    ((:arg function (descriptor-reg any-reg) ca0-offset)
     (:arg arg-ptr (descriptor-reg any-reg) ca1-offset)
     (:arg nargs (any-reg) ca2-offset)

     (:temp a0 descriptor-reg a0-offset)
     (:temp a1 descriptor-reg a1-offset)
     (:temp a2 descriptor-reg a2-offset)
     (:temp a3 descriptor-reg l3-offset)
     
     (:temp lra descriptor-reg lra-offset)
     (:temp value (descriptor-reg any-reg) ca0-offset)

     ;; Don't want these to overlap with C args.
     (:temp pa-temp any-reg nl6-offset)
     (:temp temp any-reg nl7-offset))
  (save-c-registers)
  ;; FIXME save floating point regs

  (initialize-boxed-regs (list ca0-offset ca1-offset ca2-offset))

  ;; Tag nargs.
  (inst slli nargs-tn nargs n-fixnum-tag-bits)
  (pseudo-atomic (pa-temp)
    (load-foreign-symbol-value temp "dynamic_space_free_pointer" temp)
    (store-symbol-value temp *allocation-pointer*)
    (load-foreign-symbol-value csp-tn "current_control_stack_pointer" temp)
    (load-foreign-symbol-value ocfp-tn "current_control_frame_pointer" temp)
    (store-foreign-symbol-value zero-tn "foreign_function_call_active" temp))

  ;; Pass in the args.
  (move lexenv-tn function)
  (move cfp-tn arg-ptr)
  (loadw a0 cfp-tn)
  (loadw a1 cfp-tn 1)
  (loadw a2 cfp-tn 2)
  (loadw a3 cfp-tn 3)

  (inst compute-lra lra lip-tn lra-label)
  ;; Indirect closure.
  (loadw code-tn lexenv-tn closure-fun-slot fun-pointer-lowtag)
  ;; Call into Lisp!
  (inst jalr zero-tn code-tn (- (* simple-fun-code-offset n-word-bytes)
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
    (store-foreign-symbol-value csp-tn "current_control_stack_pointer" temp)
    (store-foreign-symbol-value ocfp-tn "current_control_frame_pointer" temp)
    (store-foreign-symbol-value csp-tn "foreign_function_call_active" temp)
    (load-symbol-value temp *allocation-pointer*)
    ;; We can destroy csp-tn without harm now.
    (store-foreign-symbol-value temp "dynamic_space_free_pointer" csp-tn))

  (restore-c-registers)
  (inst jalr zero-tn lr-tn 0))

(define-assembly-routine (call-into-c (:return-style :none))
    ((:arg cfunc (any-reg) cfunc-offset)
     (:arg ca0 (any-reg) ca0-offset)
     (:arg ca1 (any-reg) ca1-offset)
     (:arg ca2 (any-reg) ca2-offset)

     (:temp value (any-reg) ca0-offset)
     (:temp pa-temp (any-reg) nl6-offset)
     (:temp temp (any-reg) nl7-offset)
     (:temp a0 (descriptor-reg any-reg) a0-offset))
  ;; The C stack frame and argument registers should have already been
  ;; set up in Lisp.

  ;; Setup Lisp stack to account for the "foreign"
  ;; frame. Look in code/debug-int.lisp to see how to do this.
  (move ocfp-tn cfp-tn)
  (move cfp-tn csp-tn)
  (inst addi csp-tn cfp-tn 32)

  (store-foreign-symbol-value csp-tn "foreign_function_call_active" temp)

  (pseudo-atomic (pa-temp)
    ;; Convert the return address to an offset and save it on the stack.
    (inst sub nfp-tn lr-tn code-tn)
    (inst addi nfp-tn nfp-tn other-pointer-lowtag)
    (storew ocfp-tn cfp-tn)
    (storew nfp-tn cfp-tn 1)
    (storew code-tn cfp-tn 2)
    (store-foreign-symbol-value csp-tn "current_control_frame_pointer" temp)
    (store-foreign-symbol-value cfp-tn "current_control_frame_pointer" temp)
    (load-symbol-value temp *allocation-pointer*)
    ;; We can destroy csp-tn without harm now.
    (store-foreign-symbol-value temp "dynamic_space_free_pointer" csp-tn))

  ;; Call into C.
  (inst jalr lr-tn cfunc 0)

  ;; Pass value to Lisp.
  (move a0 value)

  (initialize-boxed-regs)

  (pseudo-atomic (pa-temp)
    ;; FIXME: We could do some trickery like in other backends where
    ;; we allocate these registers to C's callee saved registers and
    ;; not bother loading out of the symbol.
    (load-foreign-symbol-value cfp-tn "current_control_frame_pointer" temp)
    (loadw ocfp-tn cfp-tn 0)
    (loadw nfp-tn cfp-tn 1)
    (loadw code-tn cfp-tn 2)
    (inst add lr-tn nfp-tn code-tn)
    (store-foreign-symbol-value zero-tn "foreign_function_call_active" temp)
    (load-foreign-symbol-value temp "dynamic_space_free_pointer" temp)
    (store-symbol-value temp *allocation-pointer*))

  ;; Reset the Lisp stack.
  (move csp-tn cfp-tn)
  (move cfp-tn ocfp-tn)

  (inst jalr zero-tn lr-tn (- other-pointer-lowtag)))

(define-assembly-routine (do-pending-interrupt (:return-style :none))
    ()
  (inst ebreak pending-interrupt-trap))
