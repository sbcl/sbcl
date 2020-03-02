;;;; Undefined-function and closure trampoline definitions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

(in-package "SB-VM")

(defun save-to-stack (tns sp-tn &optional (start 0) floatp)
  (loop for tn in tns
        for i from start by (if floatp 8 n-word-bytes)
        do (if floatp
               (inst fstore :double tn sp-tn i)
               (inst #-64-bit sw #+64-bit sd tn sp-tn i))))

(defun pop-from-stack (tns sp-tn &optional (start 0) floatp)
  (loop for tn in tns
        for i from start by (if floatp 8 n-word-bytes)
        do (if floatp
               (inst fload :double tn sp-tn i)
               (inst #-64-bit lw #+64-bit ld tn sp-tn i))))

#+gencgc
(define-assembly-routine (alloc-tramp (:return-style :none))
    ((:temp nl0 unsigned-reg nl0-offset)

     (:temp ca0 unsigned-reg ca0-offset))
  (let* ((nl-registers
           (loop for i in (intersection non-descriptor-regs
                                        c-unsaved-registers)
                 collect (make-reg-tn i 'unsigned-reg)))
         (lisp-registers
           (loop for i in (intersection
                           (union (list ca0-offset lr-offset cfp-offset null-offset code-offset)
                                  descriptor-regs)
                           c-unsaved-registers)
                 collect (make-reg-tn i 'unsigned-reg)))
         (float-registers
           (loop for i in c-unsaved-float-registers
                 collect (make-reg-tn i 'complex-double-reg)))
         (float-framesize (* (length float-registers) 8))
         (nl-framesize (* (length nl-registers) n-word-bytes))
         (number-framesize
           ;; New space for the number stack, making sure to respect
           ;; number stack alignment. Don't forget about the nbytes
           ;; argument.
           (logandc2 (+ (+ n-word-bytes nl-framesize float-framesize)
                        +number-stack-alignment-mask+)
                     +number-stack-alignment-mask+))
         (lisp-framesize (* (length lisp-registers) n-word-bytes))
         (nbytes-start (- number-framesize n-word-bytes))
         (nl-start (- nbytes-start nl-framesize))
         (float-start (- nl-start float-framesize)))
    (inst subi nsp-tn nsp-tn number-framesize)
    (save-to-stack nl-registers nsp-tn nl-start)
    ;; Storing NULL-TN will put at least one 1 bit somewhere into the word
    (store-foreign-symbol-value null-tn "foreign_function_call_active" nl0)
    (store-foreign-symbol-value cfp-tn "current_control_frame_pointer" nl0)
    (store-foreign-symbol-value csp-tn "current_control_stack_pointer" nl0)
    ;; Create a new frame and save descriptor regs on the stack for GC
    ;; to see.
    (save-to-stack lisp-registers csp-tn)
    (inst addi csp-tn csp-tn lisp-framesize)
    (inst #-64-bit lw #+64-bit ld ca0 nsp-tn nbytes-start)
    (save-to-stack float-registers nsp-tn float-start t)
    ;; Load the address of the C 'alloc' function from the word following
    ;; the 'boxed_region' which is embedded in a lisp vector in static space.
    ;; We mustn't affect LIP (as jumping to a fixup would do),
    ;; or LINKAGE_TEMP_REG (as jumping to a linkage entry would do)
    ;; because primitive allocators can't mess up any state except that which
    ;; the caller understands to be the scratch registers or the alloc-tn.
    ;; This load could be moved backwards, but I'm trying to get it correct
    ;; before making microscopic improvements.
    (loadw lr-tn null-tn 6 (- nil-value static-space-start))
    (inst jalr lr-tn lr-tn 0)
    (pop-from-stack float-registers nsp-tn float-start t)
    (inst #-64-bit sw #+64-bit sd ca0 nsp-tn nbytes-start)
    (inst subi csp-tn csp-tn lisp-framesize)
    (pop-from-stack lisp-registers csp-tn)
    (store-foreign-symbol-value zero-tn "foreign_function_call_active" nl0)
    (pop-from-stack nl-registers nsp-tn nl-start)
    (inst addi nsp-tn nsp-tn number-framesize)
    (inst jalr zero-tn lr-tn 0)))

(define-assembly-routine
    (xundefined-tramp (:return-style :none)
                      (:align n-lowtag-bits)
                      (:export (undefined-tramp
                                (+ xundefined-tramp fun-pointer-lowtag))))
    ((:temp lra descriptor-reg lra-offset))
  (inst machine-word simple-fun-widetag)
  (inst machine-word (make-fixup 'undefined-tramp :assembly-routine))
  (dotimes (i (- simple-fun-insts-offset 2))
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
  (dotimes (i (- simple-fun-insts-offset 2))
    (inst machine-word nil-value))

  (loadw lexenv-tn lexenv-tn fdefn-fun-slot other-pointer-lowtag)
  (loadw code-tn lexenv-tn closure-fun-slot fun-pointer-lowtag)
  (inst jalr zero-tn code-tn (- (* simple-fun-insts-offset n-word-bytes) fun-pointer-lowtag)))

(define-assembly-routine
    (xfuncallable-instance-tramp (:return-style :none)
                      (:align n-lowtag-bits)
                      (:export (funcallable-instance-tramp
                                (+ xfuncallable-instance-tramp fun-pointer-lowtag))))
    ()
  (inst machine-word simple-fun-widetag)
  (inst machine-word (make-fixup 'funcallable-instance-tramp :assembly-routine))
  (dotimes (i (- simple-fun-insts-offset 2))
    (inst machine-word nil-value))

  (loadw lexenv-tn lexenv-tn funcallable-instance-function-slot fun-pointer-lowtag)
  (loadw code-tn lexenv-tn closure-fun-slot fun-pointer-lowtag)
  (inst jalr zero-tn code-tn (- (* simple-fun-insts-offset n-word-bytes) fun-pointer-lowtag)))
