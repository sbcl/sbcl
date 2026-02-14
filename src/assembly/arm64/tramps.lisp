;;;; Undefined-function and closure trampoline definitions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

(in-package "SB-VM")

#+generational
(flet ((reg (offset sc)
         (make-random-tn (sc-or-lose sc) offset))
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
      (macrolet ((define-alloc-tramp (lisp-name c-name &body process-args)
                   `(define-assembly-routine (,lisp-name (:return-style :none))
                        ((:temp nl0 unsigned-reg nl0-offset)
                         (:temp nl1 unsigned-reg nl1-offset)
                         (:temp nl2 unsigned-reg nl2-offset)
                         (:temp nl3 unsigned-reg nl3-offset))
                      (declare (ignorable nl1 nl2))
                      (map-pairs stp nsp-tn 0 nl-registers :pre-index -80)
                      (inst mov nl0 tmp-tn) ;; size
                      (progn ,@process-args)
                      #+sb-thread
                      (inst stp cfp-tn csp-tn (@ thread-tn (* thread-control-frame-pointer-slot n-word-bytes)))
                      #-sb-thread
                      (progn
                        ;; Each of these loads of a fixup loads the address of a linkage table entry,
                        ;; that is, if ALIEN-LINKAGE-SPACE-START is #xF0200000, then the first load puts
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

                      (invoke-foreign-routine ,c-name nl3)

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
                      (map-pairs ldp nsp-tn 48 (butlast nl-registers 2) :post-index 80 :delta -16)
                      (inst ret))))
        (macrolet ((def (sys)
                     `(progn
                        (define-alloc-tramp ,(symbolicate (if (plusp sys) "SYS-" "") 'alloc-tramp)
                          "alloc"
                          (inst mov nl1 ,sys))
                        (define-alloc-tramp ,(symbolicate (if (plusp sys) "SYS-" "") 'list-alloc-tramp)
                          "alloc_list"
                          (inst mov nl1 ,sys))
                        (define-alloc-tramp ,(symbolicate (if (plusp sys) "SYS-" "") 'listify-&rest)
                          "listify_rest_arg"
                          (inst sub nl1 csp-tn tmp-tn)
                          (inst mov nl2 ,sys)))))
          (def 0)
          (def 2)))

      #+debug-gc-barriers
      (define-assembly-routine (check-barrier (:return-style :none))
          ((:temp nl0 unsigned-reg nl0-offset)
           (:temp nl1 unsigned-reg nl1-offset)
           (:temp nl2 unsigned-reg nl2-offset)
           (:temp nl3 unsigned-reg nl3-offset))
        (map-pairs stp nsp-tn 0 nl-registers :pre-index -80)
        (pseudo-atomic (nl3)
          (inst ldr nl0 (@ csp-tn (- n-word-bytes) :pre-index))
          (inst ldr nl1 (@ csp-tn (- n-word-bytes) :pre-index))
          (inst ldr nl2 (@ csp-tn (- n-word-bytes) :pre-index))
          (inst stp cfp-tn csp-tn (@ thread-tn (* thread-control-frame-pointer-slot n-word-bytes)))
          (inst add csp-tn csp-tn (+ 32 80))
          (inst stp cfp-tn lr-tn (@ csp-tn -112))
          (map-pairs stp csp-tn -80 lisp-registers)
          (map-pairs stp nsp-tn 0 float-registers :pre-index -512 :delta 32)

          (invoke-foreign-routine "check_barrier" nl3)

          (map-pairs ldp nsp-tn 480 float-registers :post-index 512 :delta -32)
          (map-pairs ldp csp-tn -16 lisp-registers :delta -16)
          (inst ldr lr-tn (@ csp-tn -104))
          (inst sub csp-tn csp-tn (+ 32 80))
          (inst str zr-tn (@ thread-tn (* thread-control-stack-pointer-slot n-word-bytes))))
        (map-pairs ldp nsp-tn 64 nl-registers :post-index 80 :delta -16)
        (inst ret))

      (define-assembly-routine (switch-to-arena (:return-style :none))
          ((:temp nl3 unsigned-reg nl3-offset))
        (map-pairs stp nsp-tn 0 nl-registers :pre-index -80)
        (pseudo-atomic (nl3)

          (inst stp cfp-tn csp-tn (@ thread-tn (* thread-control-frame-pointer-slot n-word-bytes)))
          (inst add csp-tn csp-tn (+ 32 80))
          (inst stp cfp-tn lr-tn (@ csp-tn -112))
          (map-pairs stp csp-tn -80 lisp-registers)
          (map-pairs stp nsp-tn 0 float-registers :pre-index -512 :delta 32)

          (invoke-foreign-routine "switch_to_arena" nl3)

          (map-pairs ldp nsp-tn 480 float-registers :post-index 512 :delta -32)
          (map-pairs ldp csp-tn -16 lisp-registers :delta -16)
          (inst ldr lr-tn (@ csp-tn -104))
          (inst sub csp-tn csp-tn (+ 32 80))
          (inst str zr-tn (@ thread-tn (* thread-control-stack-pointer-slot n-word-bytes))))
        (map-pairs ldp nsp-tn 64 nl-registers :post-index 80 :delta -16)
        (inst ret)))))

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
