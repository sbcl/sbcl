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

(in-package "SB-VM")

;;; Make a TN for the argument count passing location for a
;;; non-local entry.
(defun make-nlx-entry-arg-start-location ()
  (make-wired-tn *fixnum-primitive-type* immediate-arg-scn r9-offset))

;;; Save and restore dynamic environment.
(define-vop (current-stack-pointer)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    (move res csp-tn)))

(define-vop (current-binding-pointer)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    (load-binding-stack-pointer res)))

(define-vop (current-nsp)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    (inst mov-sp res nsp-tn)))

(define-vop (set-nsp)
  (:args (nsp :scs (any-reg descriptor-reg)))
  (:generator 1
     (inst mov-sp nsp-tn nsp)))

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
    (inst add block cfp-tn (add-sub-immediate (tn-byte-offset tn)))
    (load-tl-symbol-value temp *current-unwind-protect-block*)
    (storew-pair temp unwind-block-uwp-slot cfp-tn unwind-block-cfp-slot block)
    (inst adr temp entry-label)
    (storew temp block unwind-block-entry-pc-slot)
    #+sb-thread
    (loadw-pair
     temp (/ (info :variable :wired-tls '*binding-stack-pointer*) n-word-bytes)
     tmp-tn (/ (info :variable :wired-tls '*current-catch-block*) n-word-bytes)
     thread-tn)
    #-sb-thread
    (progn
      (load-binding-stack-pointer temp)
      (load-tl-symbol-value tmp-tn *current-catch-block*))
    (storew-pair temp unwind-block-bsp-slot tmp-tn unwind-block-current-catch-slot block)
    (inst mov-sp temp nsp-tn)
    (let ((nfp (current-nfp-tn vop)))
      (if nfp
          (storew-pair nfp unwind-block-nfp-slot
                       temp unwind-block-nsp-slot
                       block)
          (storew temp block unwind-block-nsp-slot)))))

;;; Like Make-Unwind-Block, except that we also store in the specified tag, and
;;; link the block into the Current-Catch list.
;;;
(define-vop (make-catch-block)
  (:args (tn) (tag :scs (any-reg descriptor-reg)))
  (:info entry-label)
  (:results (block :scs (any-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:vop-var vop)
  (:generator 44
    (inst add block cfp-tn (add-sub-immediate (tn-byte-offset tn)))
    (load-tl-symbol-value temp *current-unwind-protect-block*)
    (storew-pair temp catch-block-uwp-slot cfp-tn catch-block-cfp-slot block)
    (inst adr temp entry-label)
    (storew temp block catch-block-entry-pc-slot)

    #+sb-thread
    (loadw-pair
     temp (/ (info :variable :wired-tls '*binding-stack-pointer*) n-word-bytes)
     tmp-tn (/ (info :variable :wired-tls '*current-catch-block*) n-word-bytes)
     thread-tn)
    #-sb-thread
    (progn
      (load-binding-stack-pointer temp)
      (load-tl-symbol-value tmp-tn *current-catch-block*))
    (storew-pair tmp-tn catch-block-previous-catch-slot tag catch-block-tag-slot block)
    (storew temp block catch-block-bsp-slot)
    (inst mov-sp temp nsp-tn)
    (let ((nfp (current-nfp-tn vop)))
      (if nfp
          (storew-pair nfp unwind-block-nfp-slot
                       temp unwind-block-nsp-slot
                       block)
          (storew temp block unwind-block-nsp-slot)))
    (store-tl-symbol-value block *current-catch-block*)))

;;; Just set the current unwind-protect to UWP.  This
;;; instantiates an unwind block as an unwind-protect.
(define-vop (set-unwind-protect)
  (:args (uwp :scs (any-reg)))
  (:generator 7
    (store-tl-symbol-value uwp *current-unwind-protect-block*)))

(define-vop (%catch-breakup)
  (:args (current-block))
  (:ignore current-block)
  (:temporary (:scs (any-reg)) block)
  (:policy :fast-safe)
  (:generator 17
    (load-tl-symbol-value block *current-catch-block*)
    (loadw block block catch-block-previous-catch-slot)
    (store-tl-symbol-value block *current-catch-block*)))

(define-vop (%unwind-protect-breakup)
  (:args (current-block))
  (:ignore current-block)
  (:temporary (:scs (any-reg)) block)
  (:policy :fast-safe)
  (:generator 17
    (load-tl-symbol-value block *current-unwind-protect-block*)
    (loadw block block unwind-block-uwp-slot)
    (store-tl-symbol-value block *current-unwind-protect-block*)))

;;;; NLX entry VOPs:

(define-vop (nlx-entry)
  (:args (sp) ; Note: we can't list an sc-restriction, 'cause any load vops
              ; would be inserted before the label.
         (start)
         (count))
  (:results (values :more t :from :load))
  (:temporary (:scs (descriptor-reg)) move-temp)
  (:info label nvals)
  (:save-p :force-to-stack)
  (:vop-var vop)
  (:generator 30
    (emit-label label)
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
    (inst mov res value)
    (load-stack-tn csp-tn sp)))

(define-vop (nlx-entry-multiple)
  (:args (top :target result
              :scs (any-reg))
         (src :to :save)
         (count :target count-words))
  (:info label)
  (:temporary (:scs (any-reg)) dst)
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg)) count-words)
  (:results (result :scs (any-reg) :from (:argument 0))
            (num :scs (any-reg) :from (:argument 0)))
  (:save-p :force-to-stack)
  (:vop-var vop)
  (:before-load
    (emit-label label)
    (note-this-location vop :non-local-entry))
  (:generator 30

    ;; Setup results, and test for the zero value case.
    (if (eq (tn-kind result) :unused)
        (setf result top)
        (move result top))
    (when (eq (tn-kind num) :unused)
      (setf num tmp-tn))
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
    (unless (eq (tn-kind num) :unused)
      (inst lsr num num (- word-shift n-fixnum-tag-bits)))))

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

#+unwind-to-frame-and-call-vop
(define-vop (unwind-to-frame-and-call)
  (:args (ofp :scs (descriptor-reg))
         (uwp :scs (descriptor-reg))
         (function :scs (descriptor-reg) :to :load :target saved-function)
         (bsp :scs (any-reg descriptor-reg))
         (nsp :scs (any-reg descriptor-reg))
         (catch-block :scs (any-reg descriptor-reg)))
  (:arg-types system-area-pointer system-area-pointer t t t t)
  (:temporary (:sc unsigned-reg) temp)
  (:temporary (:sc descriptor-reg :offset r9-offset) saved-function)
  (:temporary (:sc unsigned-reg :offset r0-offset) block)
  (:temporary (:sc descriptor-reg :offset lexenv-offset) lexenv)
  (:temporary (:scs (interior-reg)) lip)
  (:temporary (:sc descriptor-reg :offset nargs-offset) nargs)
  (:vop-var vop)
  (:generator 22
    (let ((entry-label (gen-label)))
      ;; Store the function into a non-stack location, since we'll be
      ;; unwinding the stack and destroying register contents before we
      ;; use it.  It turns out that R9 is preserved as part of the
      ;; normal multiple-value handling of an unwind, so use that.
      (move saved-function function)

      ;; Allocate space for magic UWP block.
      (move block csp-tn)
      (inst add csp-tn block (* unwind-block-size n-word-bytes))

      ;; Set up magic catch / UWP block.

      (loadw temp uwp sap-pointer-slot other-pointer-lowtag)
      (storew temp block unwind-block-uwp-slot)
      (loadw temp ofp sap-pointer-slot other-pointer-lowtag)
      (storew temp block unwind-block-cfp-slot)
      (storew bsp block unwind-block-bsp-slot)
      (storew nsp block unwind-block-nsp-slot)
      (storew catch-block block unwind-block-current-catch-slot)

      (inst adr temp entry-label)
      (storew temp block catch-block-entry-pc-slot)

      ;; Run any required UWPs.
      (load-inline-constant tmp-tn '(:fixup unwind :assembly-routine) lip)
      (inst br tmp-tn)

      (emit-label ENTRY-LABEL)
      (inst mov nargs 0)

      (move lexenv saved-function)

      (loadw lip lexenv closure-fun-slot fun-pointer-lowtag)
      (lisp-jump lip))))

