;;;; Undefined-function and closure trampoline definitions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

(in-package "SB-VM")

(defun save-to-stack (tns sp-tn &optional floatp)
  (let* ((sign (if (eq nsp-tn sp-tn) -1 1))
         (amount (* sign (length tns))))
    (inst addi sp-tn sp-tn (* n-word-bytes amount))
    (loop for tn in tns
          for i from 0
          do (if floatp
                 (inst fstore :double tn sp-tn (* (- sign) i n-word-bytes))
                 (storew tn sp-tn (* (- sign) i))))
    amount))

(defun pop-from-stack (tns sp-tn &optional floatp)
  (let* ((sign (if (eq nsp-tn sp-tn) -1 1))
         (amount (* sign (length tns))))
    (loop for tn in tns
          for i from 0
          do (if floatp
                 (inst fload :double tn sp-tn (* (- sign) i n-word-bytes))
                 (loadw tn sp-tn (* (- sign) i))))
    (inst subi sp-tn sp-tn (* n-word-bytes amount))
    amount))

#+gencgc
(define-assembly-routine (alloc-tramp (:return-style :none))
    ((:temp nl0 unsigned-reg nl0-offset)

     (:temp ca0 unsigned-reg ca0-offset))
  ;; See comment in macros.lisp. Can fold this into the stack saving routine too.
  (inst subi nsp-tn nsp-tn n-word-bytes)
  (let* ((nl-registers (loop for i in (intersection
                                       non-descriptor-regs
                                       c-unsaved-registers)
                             collect (make-reg-tn i 'unsigned-reg)))
         (lisp-registers (loop for i in (intersection
                                         descriptor-regs
                                         c-unsaved-registers)
                               collect (make-reg-tn i 'unsigned-reg)))
         (float-registers (loop for i in c-unsaved-float-registers
                                collect (make-reg-tn i 'complex-double-reg)))
         (nl-framesize (save-to-stack nl-registers nsp-tn)))
    (store-foreign-symbol-value csp-tn "foreign_function_call_active" nl0)
    (store-foreign-symbol-value cfp-tn "current_control_frame_pointer" nl0)
    (store-foreign-symbol-value csp-tn "current_control_stack_pointer" nl0)
    ;; Create a new frame and save descriptor regs on the stack for GC
    ;; to see.
    (save-to-stack (list* ca0 cfp-tn null-tn code-tn lr-tn
                          lisp-registers)
                   csp-tn)
    (loadw ca0 nsp-tn (- nl-framesize))
    (save-to-stack float-registers nsp-tn t)
    (inst jal lr-tn (make-fixup "alloc" :foreign))
    (pop-from-stack float-registers nsp-tn t)
    (storew ca0 nsp-tn (- nl-framesize))
    (pop-from-stack (list* ca0 cfp-tn null-tn code-tn lr-tn
                           lisp-registers)
                    csp-tn)
    (store-foreign-symbol-value zero-tn "foreign_function_call_active" nl0)
    (pop-from-stack nl-registers nsp-tn)
    ;; Can fold this into above.
    (inst addi nsp-tn nsp-tn n-word-bytes)
    (inst jalr zero-tn lr-tn 0)))

(define-assembly-routine
    (xundefined-tramp (:return-style :none)
                      (:align n-lowtag-bits)
                      (:export (undefined-tramp
                                (+ xundefined-tramp fun-pointer-lowtag))))
    ((:temp lra descriptor-reg lra-offset))
  (inst machine-word simple-fun-widetag)
  (inst machine-word (make-fixup 'undefined-tramp :assembly-routine))
  (dotimes (i (- simple-fun-code-offset 2))
    (inst machine-word nil-value))

  ;; Point reg_CODE to the header and tag it as function, since
  ;; the debugger regards a function pointer in reg_CODE which
  ;; doesn't point to a code object as undefined function.
  (inst li code-tn (make-fixup 'undefined-tramp :assembly-routine))
  (storew ocfp-tn cfp-tn 0)
  (storew lra cfp-tn 1)
  (error-call nil 'undefined-fun-error lexenv-tn))

(define-assembly-routine
    (xclosure-tramp (:return-style :none)
                      (:align n-lowtag-bits)
                      (:export (closure-tramp
                                (+ xclosure-tramp fun-pointer-lowtag))))
    ()
  (inst machine-word simple-fun-widetag)
  (inst machine-word (make-fixup 'closure-tramp :assembly-routine))
  (dotimes (i (- simple-fun-code-offset 2))
    (inst machine-word nil-value))

  (loadw lexenv-tn lexenv-tn fdefn-fun-slot other-pointer-lowtag)
  (loadw code-tn lexenv-tn closure-fun-slot fun-pointer-lowtag)
  (inst jalr zero-tn code-tn (- (* simple-fun-code-offset n-word-bytes) fun-pointer-lowtag)))

(define-assembly-routine
    (xfuncallable-instance-tramp (:return-style :none)
                      (:align n-lowtag-bits)
                      (:export (funcallable-instance-tramp
                                (+ xfuncallable-instance-tramp fun-pointer-lowtag))))
    ()
  (inst machine-word simple-fun-widetag)
  (inst machine-word (make-fixup 'funcallable-instance-tramp :assembly-routine))
  (dotimes (i (- simple-fun-code-offset 2))
    (inst machine-word nil-value))

  (loadw lexenv-tn lexenv-tn funcallable-instance-function-slot fun-pointer-lowtag)
  (loadw code-tn lexenv-tn closure-fun-slot fun-pointer-lowtag)
  (inst jalr zero-tn code-tn (- (* simple-fun-code-offset n-word-bytes) fun-pointer-lowtag)))
