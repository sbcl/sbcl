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
(defmacro define-alloc-tramp-stub (alloc-tn-offset)
  `(define-assembly-routine
       (,(alloc-tramp-stub-name alloc-tn-offset nil)
        (:export ,(alloc-tramp-stub-name alloc-tn-offset 'list))
        (:return-style :none))
       ((:temp free+size unsigned-reg ,alloc-tn-offset)
        (:temp object-end descriptor-reg ,alloc-tn-offset))
   ;; General-purpose entry point:
     ;; ALLOC-TRAMP needs 1 bit of extra information to select a linkage table
     ;; entry. It could be passed in the low bit of FREE+SIZE, but
     ;; because the object granularity is 2 words, it is ok to use any
     ;; bit under LOWTAG-MASK.  e.g. with 64-bit words:
     ;;             v--- ; use this bit
     ;;  #b________10000 ; 16 bytes is the smallest object
     ;; By passing it as such, it is actually an offset into the linkage table.
     (inst ori free+size free+size (ash 1 word-shift))
   ;; CONS entry point:
   ,(alloc-tramp-stub-name alloc-tn-offset 'list)
     (inst addi csp-tn csp-tn n-word-bytes)
     (storew free+size csp-tn -1)
     (move free+size lip-tn)
     (invoke-asm-routine 'alloc-tramp)
     (move lip-tn object-end)
     (loadw object-end csp-tn -1)
     (inst subi csp-tn csp-tn n-word-bytes)
     (inst jalr zero-tn lip-tn 0)))

#+gencgc
(define-assembly-routine (alloc-tramp (:return-style :none))
    ((:temp temp unsigned-reg nl0-offset)
     (:temp ca0 any-reg ca0-offset))
  (let* ((nl-registers
           (loop for i in (intersection (union (list nl0-offset)
                                               non-descriptor-regs)
                                        c-unsaved-registers)
                 collect (make-reg-tn i 'unsigned-reg)))
         (lisp-registers
           (loop for i in (intersection
                           (union (list ca0-offset lip-offset cfp-offset
                                        null-offset code-offset
                                        #+sb-thread thread-offset)
                                  descriptor-regs)
                           c-unsaved-registers)
                 collect (make-reg-tn i 'descriptor-reg)))
         (float-registers
           (loop for i in c-unsaved-float-registers
                 collect (make-reg-tn i 'double-reg)))
         (float-framesize (* (length float-registers) 8))
         (nl-framesize (* (length nl-registers) n-word-bytes))
         (number-framesize
           ;; New space for the number stack, making sure to respect
           ;; number stack alignment.
           (logandc2 (+ (+ nl-framesize float-framesize)
                        +number-stack-alignment-mask+)
                     +number-stack-alignment-mask+))
         (lisp-framesize (* (length lisp-registers) n-word-bytes))
         (nl-start (- number-framesize nl-framesize))
         (float-start (- nl-start float-framesize)))
    (inst subi nsp-tn nsp-tn number-framesize)
    (save-to-stack nl-registers nsp-tn nl-start)
    (save-lisp-context csp-tn cfp-tn temp)
    ;; Create a new frame and save descriptor regs on the stack for GC
    ;; to see.
    (save-to-stack lisp-registers csp-tn)
    (loadw ca0 csp-tn -1)
    ;; linkage entry 0 = alloc() and entry 1 = alloc_list().
    ;; Because the linkage table grows downward from NIL, entry 1 is
    ;; at a lower address than 0. Adding the entry point selector bit
    ;; from 'free+size' indexes to entry 0 or 1. If that bit was 1, it
    ;; picks out entry 0, if 0 it picks out 1.
    (inst andi temp ca0 (ash 1 sb-vm:word-shift))
    (inst add temp null-tn temp)
    (loadw lip-tn temp 0 (- nil-value (linkage-table-entry-address 1)))
    (inst andi ca0 ca0 (lognot (ash 1 sb-vm:word-shift))) ; clear the selector bit
    ;; Recover and save the size.
    (load-alloc-free-pointer temp)
    (inst sub ca0 ca0 temp)
    (storew ca0 csp-tn -1)
    (inst addi csp-tn csp-tn lisp-framesize)
    (save-to-stack float-registers nsp-tn float-start t)
    (inst jalr lip-tn lip-tn 0)
    (pop-from-stack float-registers nsp-tn float-start t)
    (inst subi csp-tn csp-tn lisp-framesize)
    (loadw temp csp-tn -1)
    ;; Point to the end of the object, so we can fold the lowtag and
    ;; size additions in the fast case.
    (inst add ca0 ca0 temp)
    (storew ca0 csp-tn -1)
    (pop-from-stack lisp-registers csp-tn)
    #-sb-thread
    (store-foreign-symbol-value zero-tn "foreign_function_call_active" temp)
    #+sb-thread
    (storew zero-tn thread-base-tn thread-foreign-function-call-active-slot)
    (pop-from-stack nl-registers nsp-tn nl-start)
    (inst addi nsp-tn nsp-tn number-framesize)
    (inst jalr zero-tn lip-tn 0)))

;;; Define allocation stubs. The purpose of creating these stubs is to
;;; reduce the number of instructions needed at the site of
;;; allocation, offloading the call-out setup onto assembly
;;; routines. The cost of indirection doesn't matter as this is in the
;;; slow case.
#+gencgc
(macrolet ((define-alloc-tramp-stubs ()
             `(progn
                ,@(mapcar (lambda (tn-offset)
                            `(define-alloc-tramp-stub ,tn-offset))
                          (union descriptor-regs non-descriptor-regs)))))
  (define-alloc-tramp-stubs))

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
