;;;; the ARM definitions of VOPs used for non-local exit (throw,
;;;; lexical exit, etc.)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMUCL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;; Make a TN for the argument count passing location for a
;;; non-local entry.
(defun make-nlx-entry-arg-start-location ()
  (make-wired-tn *fixnum-primitive-type* any-reg-sc-number r8-offset))

(define-vop (current-stack-pointer)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    (move res csp-tn)))

(define-vop (current-binding-pointer)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    (load-symbol-value res *binding-stack-pointer*)))

(define-vop (current-nsp)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    (move res nsp-tn)))

(define-vop (set-nsp)
  (:args (nsp :scs (any-reg descriptor-reg)))
  (:generator 1
    (move nsp-tn nsp)))

;;;; Unwind block hackery:

;;; Compute the address of the catch block from its TN, then store into the
;;; block the current Fp, Env, Unwind-Protect, and the entry PC.
;;;
(define-vop (make-unwind-block)
  (:args (tn))
  (:info entry-label)
  (:results (block :scs (any-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:vop-var vop)
  (:generator 22
    (composite-immediate-instruction add block cfp-tn
      (tn-byte-offset tn))
    (load-symbol-value temp *current-unwind-protect-block*)
    (storew temp block unwind-block-uwp-slot)
    (storew cfp-tn block unwind-block-cfp-slot)
    (inst compute-lra temp temp entry-label)
    (storew temp block catch-block-entry-pc-slot)
    (load-symbol-value temp *current-catch-block*)
    (storew temp block unwind-block-current-catch-slot)
    (load-symbol-value temp *binding-stack-pointer*)
    (storew temp block unwind-block-bsp-slot)
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (storew cur-nfp block unwind-block-nfp-slot)))
    (storew nsp-tn block unwind-block-nsp-slot)))

;;; Like Make-Unwind-Block, except that we also store in the specified tag, and
;;; link the block into the Current-Catch list.
;;;
(define-vop (make-catch-block)
  (:args (tn) (tag :scs (any-reg descriptor-reg) :to :save))
  (:info entry-label)
  (:results (block :scs (any-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:vop-var vop)
  (:generator 44
    (composite-immediate-instruction add block cfp-tn (tn-byte-offset tn))
    (load-symbol-value temp *current-unwind-protect-block*)
    (storew temp block catch-block-uwp-slot)
    (storew cfp-tn block catch-block-cfp-slot)
    (inst compute-lra temp temp entry-label)
    (storew temp block catch-block-entry-pc-slot)

    (storew tag block catch-block-tag-slot)
    (load-symbol-value temp *current-catch-block*)
    (storew temp block catch-block-previous-catch-slot)
    (store-symbol-value block *current-catch-block*)
    (load-symbol-value temp *binding-stack-pointer*)
    (storew temp block catch-block-bsp-slot)
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
        (storew cur-nfp block catch-block-nfp-slot)))
    (storew nsp-tn block catch-block-nsp-slot)))

;;; Just set the current unwind-protect to UWP.  This
;;; instantiates an unwind block as an unwind-protect.
(define-vop (set-unwind-protect)
  (:args (uwp :scs (any-reg)))
  (:generator 7
    (store-symbol-value uwp *current-unwind-protect-block*)))

(define-vop (%catch-breakup)
  (:args (current-block))
  (:ignore current-block)
  (:temporary (:scs (any-reg)) block)
  (:generator 17
    (load-symbol-value block *current-catch-block*)
    (loadw block block catch-block-previous-catch-slot)
    (store-symbol-value block *current-catch-block*)))

(define-vop (%unwind-protect-breakup)
  (:args (current-block))
  (:ignore current-block)
  (:temporary (:scs (any-reg)) block)
  (:generator 17
    (load-symbol-value block *current-unwind-protect-block*)
    (loadw block block unwind-block-uwp-slot)
    (store-symbol-value block *current-unwind-protect-block*)))

;;;; NLX entry VOPs:

(define-vop (nlx-entry)
  (:args (sp) ; Note: we can't list an sc-restriction, 'cause any load vops
              ; would be inserted before the LRA.
         (start)
         (count))
  (:results (values :more t))
  (:temporary (:scs (descriptor-reg)) move-temp)
  (:info label nvals)
  (:save-p :force-to-stack)
  (:vop-var vop)
  (:generator 30
    (emit-label label)
    (note-this-location vop :non-local-entry)
    (cond ((zerop nvals))
          ((= nvals 1)
           (inst cmp count 0)
           (move (tn-ref-tn values) null-tn :eq)
           (loadw (tn-ref-tn values) start 0 0 :ne))
          (t
           (do ((i 0 (1+ i))
                (tn-ref values (tn-ref-across tn-ref)))
               ((null tn-ref))
             (let ((tn (tn-ref-tn tn-ref)))
               (inst subs count count (fixnumize 1))
               (sc-case tn
                 ((descriptor-reg any-reg)
                  (loadw tn start i 0 :ge)
                  (move tn null-tn :lt))
                 (control-stack
                  (loadw move-temp start i 0 :ge)
                  (store-stack-tn tn move-temp :ge)
                  (store-stack-tn tn null-tn :lt)))))))
    (load-stack-tn csp-tn sp)))

(define-vop (nlx-entry-single)
  (:args (sp)
         (value))
  (:results (res :from :load))
  (:info label)
  (:save-p :force-to-stack)
  (:vop-var vop)
  (:generator 30
    (emit-label label)
    (note-this-location vop :non-local-entry)
    (move res value)
    (load-stack-tn csp-tn sp)))

(define-vop (nlx-entry-multiple)
  (:args (top :target result) (src) (count))
  ;; Again, no SC restrictions for the args, 'cause the loading would
  ;; happen before the entry label.
  (:info label)
  (:temporary (:scs (any-reg)) dst)
  (:temporary (:scs (descriptor-reg)) temp)
  (:results (result :scs (any-reg) :from (:argument 0))
            (num :scs (any-reg) :from (:argument 0)))
  (:save-p :force-to-stack)
  (:vop-var vop)
  (:generator 30
    (emit-label label)
    (note-this-location vop :non-local-entry)

    ;; Setup results, and test for the zero value case.
    (load-stack-tn result top)
    (inst cmp count 0)
    (inst mov num 0)
    (inst b :eq DONE)

    ;; Compute dst as one slot down from result, because we inc the index
    ;; before we use it.
    (inst sub dst result 4)

    ;; Copy stuff down the stack.
    LOOP
    (inst ldr temp (@ src num))
    (inst add num num (fixnumize 1))
    (inst cmp num count)
    (inst str temp (@ dst num))
    (inst b :ne LOOP)

    ;; Reset the CSP.
    DONE
    (inst add csp-tn result num)))

;;; This VOP is just to force the TNs used in the cleanup onto the stack.
;;;
(define-vop (uwp-entry)
  (:info label)
  (:save-p :force-to-stack)
  (:vop-var vop)
  (:generator 0
    (emit-label label)
    (note-this-location vop :non-local-entry)))

(define-vop (uwp-entry-block)
  (:info label)
  (:save-p :force-to-stack)
  (:results (block))
  (:vop-var vop)
  (:generator 0
    (emit-label label)
    (note-this-location vop :non-local-entry)
    ;; Get the block saved in UNWIND
    (inst ldr block (@ csp-tn (* -4 n-word-bytes)))))

;;; Doesn't handle NSP and is disabled.
#+unwind-to-frame-and-call-vop
(define-vop (unwind-to-frame-and-call)
  (:args (ofp :scs (descriptor-reg))
         (uwp :scs (descriptor-reg))
         (function :scs (descriptor-reg) :to :load :target saved-function)
         (bsp :scs (control-stack))
         (catch-block :scs (control-stack)))
  (:arg-types system-area-pointer system-area-pointer t)
  (:temporary (:sc unsigned-reg) temp)
  (:temporary (:sc descriptor-reg :offset r8-offset) saved-function)
  (:temporary (:sc unsigned-reg :offset r0-offset) block)
  (:temporary (:sc descriptor-reg :offset lexenv-offset) lexenv)
  (:temporary (:scs (non-descriptor-reg)) lip)
  (:temporary (:sc descriptor-reg :offset nargs-offset) nargs)
  (:vop-var vop)
  (:generator 22
    (let ((uwp-label (gen-label))
          (entry-label (gen-label)))
      ;; Store the function into a non-stack location, since we'll be
      ;; unwinding the stack and destroying register contents before we
      ;; use it.  It turns out that R8 is preserved as part of the
      ;; normal multiple-value handling of an unwind, so use that.
      (move saved-function function)

      ;; Allocate space for magic UWP block.
      (move block csp-tn)
      (inst add csp-tn csp-tn (* unwind-block-size n-word-bytes))

      ;; Set up magic catch / UWP block.

      (loadw temp uwp sap-pointer-slot other-pointer-lowtag)
      (storew temp block unwind-block-uwp-slot)
      (loadw temp ofp sap-pointer-slot other-pointer-lowtag)
      (storew temp block unwind-block-cfp-slot)
      (load-stack-tn temp bsp)
      (storew temp block unwind-block-bsp-slot)
      (storew nsp-tn block unwind-block-nsp-slot)
      (load-stack-tn temp catch-block)
      (storew temp block unwind-block-current-catch-slot)

      ;; Don't need to save code at unwind-block-code-slot since
      ;; it's not going to be used and will be overwritten after the
      ;; function call

      (inst compute-lra lip lip entry-label)
      (storew lip block catch-block-entry-pc-slot)

      ;; Run any required UWPs.
      (assemble (:elsewhere vop)
        (emit-label uwp-label)
        (inst word (make-fixup 'unwind :assembly-routine)))
      (inst load-from-label pc-tn lr-tn uwp-label)

      (emit-label ENTRY-LABEL)
      (inst mov nargs 0)

      (move lexenv saved-function)

      (loadw saved-function lexenv closure-fun-slot fun-pointer-lowtag)
      (lisp-jump saved-function))))
