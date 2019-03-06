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
(macrolet ((frob ()
             (let ((a (loop repeat register-arg-count
                            collect (gensym)))
                   (defaulting-labels (loop repeat (- register-arg-count 1)
                                            collect (gen-label))))
               `(define-assembly-routine
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

                     ;; These are needed so we can get at the register args.
                     ,@(loop for an-offset in *register-arg-offsets*
                             for an in a
                             collect `(:temp ,an descriptor-reg ,an-offset)))
                  ;; Note, because of the way the return-multiple vop is
                  ;; written, we can assume that we are never called
                  ;; with nvals == 1 and that a0 has already been
                  ;; loaded.
                  (inst subi count nvals (fixnumize 2))
                  (inst bge zero-tn nvals DEFAULT-A0-AND-ON)
                  ,@(loop for label in defaulting-labels
                          for an in (rest a)
                          for i from 1
                          collect `(progn
                                     ,@(unless (= i 1)
                                         `((inst subi count count (fixnumize 1))))
                                     (loadw ,an vals ,i)
                                     (inst bge zero-tn count ,label)))

                  ;; Copy the remaining args to the top of the stack.
                  (inst addi vals vals (* register-arg-count n-word-bytes))
                  (inst addi dst cfp-tn (* register-arg-count n-word-bytes))

                  LOOP
                  (loadw temp vals)
                  (inst addi vals vals n-word-bytes)
                  (inst subi count count (fixnumize 1))
                  (storew temp dst)
                  (inst addi dst dst n-word-bytes)
                  (inst bne count zero-tn LOOP)

                  (inst j ,(first (last defaulting-labels)))

                  DEFAULT-A0-AND-ON
                  (move ,(first a) null-tn)
                  (move ,(second a) null-tn)
                  ,@(loop for defaulting-label in defaulting-labels
                          for an in (rest (rest a))
                          append `((emit-label ,defaulting-label)
                                   (move ,an null-tn)))
                  (emit-label ,(first (last defaulting-labels)))

                  ;; Clear the stack.
                  (move ocfp-tn cfp-tn)
                  (move cfp-tn ocfp)
                  (cond ((zerop (- word-shift n-fixnum-tag-bits))
                         (inst add ocfp-tn nvals))
                        (t
                         (inst slli temp nvals (- word-shift n-fixnum-tag-bits))
                         (inst add csp-tn ocfp-tn temp)))

                  ;; Return.
                  (lisp-return lra :multiple-values)))))
  (frob))

#+sb-assembling ;; no vop for this one either.
(macrolet ((frob ()
             (let ((a (loop repeat register-arg-count
                            collect (gensym))))
               `(define-assembly-routine
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
                     (:temp temp descriptor-reg l0-offset)

                     ;; These are needed so we can get at the register args.
                     ,@(loop for an-offset in *register-arg-offsets*
                             for an in a
                             collect `(:temp ,an descriptor-reg ,an-offset)))


                  ;; Calculate NARGS (as a fixnum)
                  (inst sub nargs csp-tn args)

                  ;; Load the argument regs (must do this now, 'cause the blt might
                  ;; trash these locations)
                  ,@(loop for an in a
                          for i from 0
                          collect `(loadw ,an args ,i))

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
                  (unless (zerop (- word-shift n-fixnum-tag-bits))
                    (inst srai nargs nargs (- word-shift n-fixnum-tag-bits)))
                  (loadw temp lexenv closure-fun-slot fun-pointer-lowtag)
                  (lisp-jump temp)))))
  (frob))


;;;; Non-local exit noise.

(define-assembly-routine (throw (:return-style :none))
  ((:arg target (descriptor-reg any-reg) a0-offset)
   (:arg start (descriptor-reg any-reg) ocfp-offset)
   (:arg count (descriptor-reg any-reg) nargs-offset)
   (:temp catch any-reg a1-offset)
   (:temp tag descriptor-reg a2-offset))
  (declare (ignore start count)) ; We only need them in the registers.

  (load-symbol-value catch *current-catch-block*)

  LOOP
  (let ((error (generate-error-code nil 'unseen-throw-tag-error target)))
    (inst beq catch zero-tn error))

  (loadw tag catch catch-block-tag-slot)
  (inst beq tag target EXIT)
  (loadw catch catch catch-block-previous-catch-slot)
  (inst j loop)

  EXIT
  (move target catch) ;; TARGET coincides with UNWIND's BLOCK argument
  (invoke-asm-routine 'unwind t))

(define-assembly-routine (unwind
                          (:return-style :none)
                          (:translate %continue-unwind)
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

  (load-symbol-value cur-uwp *current-unwind-protect-block*)
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
  (store-symbol-value target-uwp *current-unwind-protect-blocK*)
  (inst j DO-EXIT))
