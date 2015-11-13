;;;; the ARM definitions of VOPs used for non-local exit (throw,
;;;; lexical exit, etc.)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;; Make a TN for the argument count passing location for a
;;; non-local entry.
(defun make-nlx-entry-arg-start-location ()
  (make-wired-tn *fixnum-primitive-type* immediate-arg-scn r8-offset))

;;; Save and restore dynamic environment.
;;;
;;; These VOPs are used in the reentered function to restore the appropriate
;;; dynamic environment.  Currently we only save the Current-Catch and binding
;;; stack pointer.  We don't need to save/restore the current unwind-protect,
;;; since unwind-protects are implicitly processed during unwinding.  If there
;;; were any additional stacks, then this would be the place to restore the top
;;; pointers.

(define-vop (save-dynamic-state)
  (:results (catch :scs (descriptor-reg))
            (nfp :scs (descriptor-reg))
            (nsp :scs (descriptor-reg)))
  (:vop-var vop)
  (:generator 13
    (load-tl-symbol-value catch *current-catch-block*)
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (move nfp cur-nfp)))
    (inst mov-sp nsp nsp-tn)))

(define-vop (restore-dynamic-state)
  (:args (catch :scs (descriptor-reg))
         (nfp :scs (descriptor-reg))
         (nsp :scs (descriptor-reg)))
  (:vop-var vop)
  (:generator 10
    (store-tl-symbol-value catch *current-catch-block*)
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (move cur-nfp nfp)))
    (inst mov-sp nsp-tn nsp)))

(define-vop (current-stack-pointer)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    (move res csp-tn)))

(define-vop (current-binding-pointer)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    (load-binding-stack-pointer res)))

;;;; Unwind block hackery:

;;; Compute the address of the catch block from its TN, then store into the
;;; block the current Fp, Env, Unwind-Protect, and the entry PC.
;;;
(define-vop (make-unwind-block)
  (:args (tn))
  (:info entry-label)
  (:results (block :scs (any-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 22
    (inst add block cfp-tn (add-sub-immediate (* (tn-offset tn) n-word-bytes)))
    (load-tl-symbol-value temp *current-unwind-protect-block*)
    (storew temp block unwind-block-current-uwp-slot)
    (storew cfp-tn block unwind-block-current-cont-slot)
    (storew code-tn block unwind-block-current-code-slot)
    (inst compute-lra temp lip entry-label)
    (storew temp block catch-block-entry-pc-slot)))

;;; Like Make-Unwind-Block, except that we also store in the specified tag, and
;;; link the block into the Current-Catch list.
;;;
(define-vop (make-catch-block)
  (:args (tn) (tag :scs (any-reg descriptor-reg)))
  (:info entry-label)
  (:results (block :scs (any-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg) :target block :to (:result 0)) result)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 44
    (inst add result cfp-tn (add-sub-immediate (* (tn-offset tn) n-word-bytes)))
    (load-tl-symbol-value temp *current-unwind-protect-block*)
    (storew temp result catch-block-current-uwp-slot)
    (storew cfp-tn result catch-block-current-cont-slot)
    (storew code-tn result catch-block-current-code-slot)
    (inst compute-lra temp lip entry-label)
    (storew temp result catch-block-entry-pc-slot)

    (storew tag result catch-block-tag-slot)
    (load-tl-symbol-value temp *current-catch-block*)
    (storew temp result catch-block-previous-catch-slot)
    (store-tl-symbol-value result *current-catch-block*)

    (move block result)))

;;; Just set the current unwind-protect to TN's address.  This
;;; instantiates an unwind block as an unwind-protect.
(define-vop (set-unwind-protect)
  (:args (tn))
  (:temporary (:scs (descriptor-reg)) new-uwp)
  (:generator 7
    (inst add new-uwp cfp-tn (add-sub-immediate (* (tn-offset tn) n-word-bytes)))
    (store-tl-symbol-value new-uwp *current-unwind-protect-block*)))

(define-vop (unlink-catch-block)
  (:temporary (:scs (any-reg)) block)
  (:policy :fast-safe)
  (:translate %catch-breakup)
  (:generator 17
    (load-tl-symbol-value block *current-catch-block*)
    (loadw block block catch-block-previous-catch-slot)
    (store-tl-symbol-value block *current-catch-block*)))

(define-vop (unlink-unwind-protect)
  (:temporary (:scs (any-reg)) block)
  (:policy :fast-safe)
  (:translate %unwind-protect-breakup)
  (:generator 17
    (load-tl-symbol-value block *current-unwind-protect-block*)
    (loadw block block unwind-block-current-uwp-slot)
    (store-tl-symbol-value block *current-unwind-protect-block*)))

;;;; NLX entry VOPs:

(define-vop (nlx-entry)
  (:args (sp) ; Note: we can't list an sc-restriction, 'cause any load vops
              ; would be inserted before the LRA.
         (start)
         (count))
  (:results (values :more t :from :load))
  (:temporary (:scs (descriptor-reg)) move-temp)
  (:info label nvals)
  (:save-p :force-to-stack)
  (:vop-var vop)
  (:generator 30
    (emit-return-pc label)
    (note-this-location vop :non-local-entry)
    (cond ((zerop nvals))
          ((= nvals 1)
           (assemble ()
             (move (tn-ref-tn values) null-tn)
             (inst cbz count zero)
             (loadw (tn-ref-tn values) start)
             ZERO))
          (t
           (do ((i 0 (1+ i))
                (tn-ref values (tn-ref-across tn-ref)))
               ((null tn-ref))
             (let ((tn (tn-ref-tn tn-ref)))
               (inst subs count count (fixnumize 1))
               (sc-case tn
                 ((descriptor-reg any-reg)
                  (assemble ()
                    (move tn null-tn)
                    (inst b :lt LESS-THAN)
                    (loadw tn start i)
                    LESS-THAN))
                 (control-stack
                  (assemble ()
                    (move move-temp null-tn)
                    (inst b :lt LESS-THAN)
                    (loadw move-temp start i)
                    LESS-THAN
                    (store-stack-tn tn move-temp))))))))
    (load-stack-tn csp-tn sp)))

(define-vop (nlx-entry-multiple)
  (:args (top :target result)
         (src)
         (count :target count-words))
  ;; Again, no SC restrictions for the args, 'cause the loading would
  ;; happen before the entry label.
  (:info label)
  (:temporary (:scs (any-reg)) dst)
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg)) count-words)
  (:results (result :scs (any-reg) :from (:argument 0))
            (num :scs (any-reg) :from (:argument 0)))
  (:save-p :force-to-stack)
  (:vop-var vop)
  (:generator 30
    (emit-return-pc label)
    (note-this-location vop :non-local-entry)

    ;; Setup results, and test for the zero value case.
    (load-stack-tn result top)
    (inst mov num 0)
    ;; Shift and check for zero in one go
    (inst adds count-words zr-tn (lsl count (- word-shift n-fixnum-tag-bits)))
    (inst b :eq DONE)

    ;; Compute dst as one slot down from result, because we inc the index
    ;; before we use it.
    (inst sub dst result n-word-bytes)

    ;; Copy stuff down the stack.
    LOOP
    (inst ldr temp (@ src num))
    (inst add num num n-word-bytes)
    (inst cmp num count-words)
    (inst str temp (@ dst num))
    (inst b :ne LOOP)

    ;; Reset the CSP.
    DONE
    (inst add csp-tn result num)
    (inst lsr num num (- word-shift n-fixnum-tag-bits))))

;;; This VOP is just to force the TNs used in the cleanup onto the stack.
;;;
(define-vop (uwp-entry)
  (:info label)
  (:save-p :force-to-stack)
  (:results (block) (start) (count))
  (:ignore block start count)
  (:vop-var vop)
  (:generator 0
    (emit-return-pc label)
    (note-this-location vop :non-local-entry)))

(define-vop (unwind-to-frame-and-call)
  (:args (ofp :scs (descriptor-reg))
         (uwp :scs (descriptor-reg))
         (function :scs (descriptor-reg) :to :load :target saved-function))
  (:arg-types system-area-pointer system-area-pointer t)
  (:temporary (:sc unsigned-reg) temp)
  (:temporary (:sc descriptor-reg :offset r8-offset) saved-function)
  (:temporary (:sc unsigned-reg :offset r0-offset) block)
  (:temporary (:sc descriptor-reg :offset lexenv-offset) lexenv)
  (:temporary (:scs (interior-reg)) lip)
  (:temporary (:sc descriptor-reg :offset nargs-offset) nargs)
  (:vop-var vop)
  (:generator 22
    (let ((entry-label (gen-label)))
      ;; Store the function into a non-stack location, since we'll be
      ;; unwinding the stack and destroying register contents before we
      ;; use it.  It turns out that R8 is preserved as part of the
      ;; normal multiple-value handling of an unwind, so use that.
      (move saved-function function)

      ;; Allocate space for magic UWP block.
      (move block csp-tn)
      (inst add csp-tn block (* unwind-block-size n-word-bytes))

      ;; Set up magic catch / UWP block.

      (loadw temp uwp sap-pointer-slot other-pointer-lowtag)
      (storew temp block unwind-block-current-uwp-slot)
      (loadw temp ofp sap-pointer-slot other-pointer-lowtag)
      (storew temp block unwind-block-current-cont-slot)
      ;; Don't need to save code at unwind-block-current-code-slot since
      ;; it's not going to be used and will be overwritten after the
      ;; function call

      (inst compute-lra temp lip entry-label)
      (storew temp block catch-block-entry-pc-slot)

      ;; Run any required UWPs.
      (load-inline-constant tmp-tn '(:fixup unwind :assembly-routine) lip)
      (inst br tmp-tn)

      (emit-return-pc ENTRY-LABEL)
      (inst mov nargs 0)

      (move lexenv saved-function)

      (loadw saved-function lexenv closure-fun-slot fun-pointer-lowtag)
      (lisp-jump saved-function lip))))
