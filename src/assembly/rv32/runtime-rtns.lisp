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
    (loop for offset from 0 by n-word-bytes
          for saved-offset in c-saved-registers
          do (loadw (make-any-reg-tn saved-offset) nsp-tn offset))
    (inst addi nsp-tn nsp-tn framesize)))

(defun initialize-boxed-regs (&optional still-live)
  (dolist (boxed-reg-offset boxed-regs)
    (unless (member boxed-reg-offset still-live)
      (inst li (make-any-reg-tn boxed-reg-offset) 0))))

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

     (:temp nl0 any-reg nl0-offset)
     (:temp nl1 any-reg nl1-offset))
  (save-c-registers)
  ;; FIXME save floating point regs

  (initialize-boxed-regs (list ca0-offset ca1-offset ca2-offset))
  (inst li null-tn #x8000017)

  ;; Tag nargs.
  (inst slli nargs-tn nargs n-fixnum-tag-bits)

  (pseudo-atomic (nl1)
    (load-symbol-value csp-tn *control-stack-pointer*)
    (inst li nl0 (make-fixup "current_control_frame_pointer" :foreign))
    (loadw ocfp-tn nl0)
    (inst li nl0 (make-fixup "foreign_function_call_active" :foreign))
    (storew zero-tn nl0))

  ;; Pass in the args.
  (move lexenv-tn function)
  (move cfp-tn arg-ptr)
  (loadw a0 cfp-tn)
  (loadw a1 cfp-tn 1)
  (loadw a2 cfp-tn 2)
  (loadw a3 cfp-tn 3)

  (inst compute-lra lra lip-tn lra-label)
    ;; Indirect closure.
  (flet ((real-offset (slot) (- (* slot n-word-bytes) fun-pointer-lowtag)))
    (loadw code-tn lexenv-tn (real-offset closure-fun-slot))
    ;; Call into Lisp!
    (inst jalr zero-tn code-tn (real-offset simple-fun-code-offset)))
  (emit-alignment n-lowtag-bits)
  LRA-LABEL
  (inst word return-pc-widetag)

  (inst blt nargs-tn zero-tn single-value-return)
  (move csp-tn ocfp-tn)
  
  SINGLE-VALUE-RETURN
  ;; Return the one value.
  (move value a0)
  ;; Save Lisp state.
  (pseudo-atomic (nl1)
    (store-symbol-value csp-tn *control-stack-pointer*)
    (inst li nl0 (make-fixup "current_control_frame_pointer" :foreign))
    (storew ocfp-tn nl0)
    (inst li nl0 (make-fixup "foreign_function_call_active" :foreign))
    (storew csp-tn nl0))

  (restore-c-registers)
  (inst jalr zero-tn lr-tn 0))

(define-assembly-routine (call-into-c (:return-style :none))
    ((:arg cfunc (descriptor-reg any-reg) cfunc-offset)

     (:temp temp (any-reg) nl0-offset)
     (:temp nl1 (any-reg) nl1-offset)
     
     (:temp ca0 (descriptor-reg any-reg) ca0-offset)
     (:temp a0 (descriptor-reg any-reg) a0-offset))

  ;; Setup Lisp stack.
  (move ocfp-tn cfp-tn)
  (move cfp-tn csp-tn)
  (inst addi csp-tn cfp-tn 32)

  (inst li temp (make-fixup "foreign_function_call_active" :foreign))
  (storew csp-tn temp)

  (pseudo-atomic (nl1)
    (inst sub nfp-tn lip-tn code-tn)
    (inst addi nfp-tn nfp-tn other-pointer-lowtag)
    (storew ocfp-tn cfp-tn)
    (storew nfp-tn cfp-tn 1)
    (storew code-tn cfp-tn 2)
    (store-symbol-value csp-tn *control-stack-pointer*)
    (inst li temp (make-fixup "current_control_frame_pointer" :foreign))
    (storew ocfp-tn temp))

  ;; Call into C.
  (inst jalr zero-tn cfunc 0)
  ;; Pass value to Lisp.
  (move a0 ca0)

  (initialize-boxed-regs)

  (pseudo-atomic (nl1)
    (loadw code-tn cfp-tn 2)
    (inst subi lip-tn nfp-tn other-pointer-lowtag)
    (inst add lip-tn lip-tn code-tn))

  ;; Reset the Lisp stack.
  ;; FIXME!!!!! We need cfp and ocfp to be in saved registers!!!
  (move csp-tn cfp-tn)
  (move cfp-tn ocfp-tn)

  (inst li temp (make-fixup "foreign_function_call_active" :foreign))
  (storew zero-tn temp)

  (inst jalr zero-tn lip-tn 0))

(define-assembly-routine (do-pending-interrupt (:return-style :none))
    ()
  (inst ebreak pending-interrupt-trap))
