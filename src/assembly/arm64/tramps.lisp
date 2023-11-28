;;;; Undefined-function and closure trampoline definitions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

(in-package "SB-VM")

#+generational
(macrolet ((define-alloc-tramp (lisp-name c-name &body process-args)
 `(define-assembly-routine (,lisp-name (:return-style :none))
    ((:temp nl0 unsigned-reg nl0-offset)
     (:temp nl1 unsigned-reg nl1-offset)
     (:temp nl2 unsigned-reg nl2-offset))
    (declare (ignorable nl1))
  (flet ((reg (offset sc)
           (make-random-tn
            :kind :normal
            :sc (sc-or-lose sc)
            :offset offset))
         (reverse-pairs (list)
           (let (result)
             (loop for (a b) on list by #'cddr
                   do
                   (push b result)
                   (push a result))
             result)))
    (let ((nl-registers (loop for i to 9
                              collect (reg i 'unsigned-reg)))
          (lisp-registers (loop for i from r0-offset to r9-offset
                                collect (reg i 'unsigned-reg)))
          (float-registers (loop for i below 32
                                 collect (reg i 'complex-double-reg))))
      (macrolet  ((map-pairs (op base start offsets
                              &key pre-index post-index
                                   (delta 16))
                    `(let ((offsets ,(if (eq op 'ldp)
                                         `(reverse-pairs ,offsets)
                                         offsets)))
                       (loop with offset = ,start
                             for first = t then nil
                             for (a b . next) on offsets by #'cddr
                             for last = (not next)
                             do (inst ,op a b
                                      (@ ,base (or (and first ,pre-index)
                                                   (and last ,post-index)
                                                   offset)
                                               (cond ((and last ,post-index)
                                                      :post-index)
                                                     ((and first ,pre-index)
                                                      :pre-index)
                                                     (t
                                                      :offset))))
                                (incf offset ,delta)))))
        (map-pairs stp nsp-tn 0 nl-registers :pre-index -80)
        (inst mov nl0 tmp-tn) ;; size
        (progn ,@process-args)
        #+sb-thread
        (inst stp cfp-tn csp-tn (@ thread-tn (* thread-control-frame-pointer-slot n-word-bytes)))
        #-sb-thread
        (progn
          ;; Each of these loads of a fixup loads the address of a linkage table entry,
          ;; that is, if ALIEN-LINKAGE-TABLE-SPACE-START is #xF0200000, then the first load puts
          ;; #xF0200000+something, not the address of current_control_frame_pointer,
          ;; into register nl2.
          ;; This is kinda dumb because first of all we could have calculated the address
          ;; of the linkage entry instead of loading it. Second, it's unnecessarily
          ;; double-indirect. I wonder if there's a way to remove double-indirection.
          ;; The ohly way I can see doing that is for os_link_runtime() to poke bytes
          ;; into this assembly code in the manner of the system's dynamic loader.
          (load-inline-constant nl2 '(:fixup "current_control_frame_pointer" :foreign-dataref))
          (inst ldr nl2 (@ nl2)) ; now load the address of the C variable
          (inst str cfp-tn (@ nl2))
          (load-inline-constant nl2 '(:fixup "current_control_stack_pointer" :foreign-dataref))
          (inst ldr nl2 (@ nl2)) ; "
          (inst str csp-tn (@ nl2))
          (load-inline-constant nl2 '(:fixup "foreign_function_call_active" :foreign-dataref))
          (inst ldr nl2 (@ nl2)) ; "
          ;; storing NULL ensures at least 1 set bit somewhere in the low 4 bytes
          (inst str (32-bit-reg null-tn) (@ nl2))) ; (alien variable is 4 bytes, not 8)
        ;; Create a new frame
        (inst add csp-tn csp-tn (+ 32 80))
        (inst stp cfp-tn lr-tn (@ csp-tn -112))

        (map-pairs stp csp-tn -80 lisp-registers)
        (map-pairs stp nsp-tn 0 float-registers :pre-index -512 :delta 32)

        (invoke-foreign-routine ,c-name nl2)

        (map-pairs ldp nsp-tn 480 float-registers :post-index 512 :delta -32)
        (map-pairs ldp csp-tn -16 lisp-registers :delta -16)

        (inst ldr lr-tn (@ csp-tn -104))

        (inst sub csp-tn csp-tn (+ 32 80)) ;; deallocate the frame
        #+sb-thread
        (inst str zr-tn (@ thread-tn (* thread-control-stack-pointer-slot n-word-bytes)))
        #-sb-thread
        (progn
          ;; We haven't restored NL2 yet, so it's ok to use as a scratch register here.
          ;; It gets restored just prior to the RET instruction.
          (load-inline-constant nl2 '(:fixup "foreign_function_call_active" :foreign-dataref))
          (inst ldr nl2 (@ nl2))
          (inst str (32-bit-reg zr-tn) (@ nl2)))

        (inst mov tmp-tn nl0) ;; result

        (aver (location= tmp-tn (reg 9 'any-reg))) ; Change detector assertion
        ;; Do not reload r9 (= NL9 = TMP-TN) as it just got moved from NL0 which holds
        ;; the result from C and which will be returned from this asm routine.
        (inst ldr (reg 8 'any-reg) (@ nsp-tn 64))
        ;; I'm unsure how to utilize map-pairs on the others now :-(
        ;; (It took more lines of code to alter MAP-PAIRS to accept another keyword
        ;; asking it not to clobber TMP-TN than it did to just hand-write 4 instructions)
        (inst ldp (reg 6 'any-reg) (reg 7 'any-reg) (@ nsp-tn 48))
        (inst ldp (reg 4 'any-reg) (reg 5 'any-reg) (@ nsp-tn 32))
        (inst ldp (reg 2 'any-reg) (reg 3 'any-reg) (@ nsp-tn 16))
        (inst ldp (reg 0 'any-reg) (reg 1 'any-reg) (@ nsp-tn 80 :post-index))
        (inst ret)))))))
  (define-alloc-tramp alloc-tramp "alloc")
  (define-alloc-tramp list-alloc-tramp "alloc_list")
  (define-alloc-tramp listify-&rest "listify_rest_arg"
    (inst sub nl1 csp-tn tmp-tn)))

(define-assembly-routine
    (undefined-tramp (:return-style :none))
    ()
  (inst str lr-tn (@ cfp-tn 8))
  (emit-error-break nil cerror-trap (error-number-or-lose 'undefined-fun-error)
                    (list lexenv-tn))
  (loadw lr-tn lexenv-tn closure-fun-slot fun-pointer-lowtag)
  (inst add lr-tn lr-tn 4)
  (inst br lr-tn))

(define-assembly-routine
    (undefined-alien-tramp (:return-style :none))
    ((:temp r9-tn unsigned-reg r9-offset))
  (inst str lr-tn (@ cfp-tn 8))
  (error-call nil 'undefined-alien-fun-error r9-tn))

(define-assembly-routine
    (closure-tramp (:return-style :none))
    ()
  (inst str lr-tn (@ cfp-tn 8))
  (loadw lexenv-tn lexenv-tn fdefn-fun-slot other-pointer-lowtag)
  (loadw lr-tn lexenv-tn closure-fun-slot fun-pointer-lowtag)
  (inst add lr-tn lr-tn 4)
  (inst br lr-tn))

(define-assembly-routine
    (funcallable-instance-tramp (:return-style :none))
    ()
  (inst str lr-tn (@ cfp-tn 8))
  (loadw lexenv-tn lexenv-tn funcallable-instance-function-slot fun-pointer-lowtag)
  (loadw lr-tn lexenv-tn closure-fun-slot fun-pointer-lowtag)
  (inst add lr-tn lr-tn 4)
  (inst br lr-tn))
