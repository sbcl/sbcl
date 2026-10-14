;;;; the SPARC definitions of VOPs used for non-local exit (throw,
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

;;; Make a TN for the argument count passing location for a non-local
;;; entry.
(defun make-nlx-entry-arg-start-location ()
  (make-wired-tn *fixnum-primitive-type* any-reg-sc-number ocfp-offset))

(define-vop (current-stack-pointer)
    (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
              (move res csp-tn)))

(define-vop (current-binding-pointer)
    (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
              (move res bsp-tn)))

(define-vop (current-nsp)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    (move res nsp-tn)))

(define-vop (set-nsp)
  (:args (nsp :scs (any-reg descriptor-reg)))
  (:generator 1
    (move nsp-tn nsp)))

;;;; unwind block hackery:

;;; Compute the address of the catch block from its TN, then store
;;; into the block the current Fp, Env, Unwind-Protect, and the entry
;;; PC.
(define-vop (make-unwind-block)
  (:args (tn))
  (:info entry-label)
  (:results (block :scs (any-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:vop-var vop)
  (:generator 22
    (inst add block cfp-tn (tn-byte-offset tn))
    (load-symbol-value temp *current-unwind-protect-block*)
    (storew temp block unwind-block-uwp-slot)
    (storew cfp-tn block unwind-block-cfp-slot)
    (storew code-tn block unwind-block-code-slot)
    (inst compute-lra-from-code temp code-tn entry-label ndescr)
    (storew temp block unwind-block-entry-pc-slot)
    (load-symbol-value temp *current-catch-block*)
    (storew temp block unwind-block-current-catch-slot)
    (storew bsp-tn block unwind-block-bsp-slot)
    (let ((nfp (current-nfp-tn vop)))
      (when nfp
        (storew nfp block unwind-block-nfp-slot)))
    (storew nsp-tn block unwind-block-nsp-slot)))


;;; Like Make-Unwind-Block, except that we also store in the specified tag, and
;;; link the block into the Current-Catch list.
(define-vop (make-catch-block)
  (:args (tn)
         (tag :scs (any-reg descriptor-reg) :to :save))
  (:info entry-label)
  (:results (block :scs (any-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:vop-var vop)
  (:generator 44
    (inst add block cfp-tn (tn-byte-offset tn))
    (load-symbol-value temp *current-unwind-protect-block*)
    (storew temp block catch-block-uwp-slot)
    (storew cfp-tn block catch-block-cfp-slot)
    (storew code-tn block catch-block-code-slot)
    (inst compute-lra-from-code temp code-tn entry-label ndescr)
    (storew temp block catch-block-entry-pc-slot)

    (storew tag block catch-block-tag-slot)
    (load-symbol-value temp *current-catch-block*)
    (storew temp block catch-block-previous-catch-slot)
    (store-symbol-value block *current-catch-block*)
    (storew bsp-tn block catch-block-bsp-slot)
    (let ((nfp (current-nfp-tn vop)))
      (when nfp
        (storew nfp block catch-block-nfp-slot))
      (storew nsp-tn block catch-block-nsp-slot))))


;;; Just set the current unwind-protect to UWP.  This instantiates an
;;; unwind block as an unwind-protect.
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
  (:args (sp)
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
           (let ((no-values (gen-label)))
             (inst cmp count)
             (inst b :eq no-values)
             (move (tn-ref-tn values) null-tn)
             (loadw (tn-ref-tn values) start)
             (emit-label no-values)))
          (t
           (collect ((defaults))
             (inst subcc count (fixnumize 1))
             (do ((i 0 (1+ i))
                  (tn-ref values (tn-ref-across tn-ref)))
                 ((null tn-ref))
               (let ((default-lab (gen-label))
                     (tn (tn-ref-tn tn-ref)))
                 (defaults (cons default-lab tn))

                 (inst b :lt default-lab)
                 (inst subcc count (fixnumize 1))
                 (sc-case tn
                          ((descriptor-reg any-reg)
                           (loadw tn start i))
                          (control-stack
                           (loadw move-temp start i)
                           (store-stack-tn tn move-temp)))))

             (let ((defaulting-done (gen-label)))

               (emit-label defaulting-done)

               (assemble (:elsewhere)
                 (dolist (def (defaults))
                   (emit-label (car def))
                   (let ((tn (cdr def)))
                     (sc-case tn
                              ((descriptor-reg any-reg)
                               (move tn null-tn))
                              (control-stack
                               (store-stack-tn tn null-tn)))))
                 (inst b defaulting-done)
                 (inst nop))))))
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
    (let ((loop (gen-label))
          (done (gen-label)))

      ;; Setup results, and test for the zero value case.
      (load-stack-tn result top)
      (inst cmp count)
      (inst b :eq done)
      (inst li num 0)

      ;; Compute dst as one slot down from result, because we inc the index
      ;; before we use it.
      (inst sub dst result 4)

      ;; Copy stuff down the stack.
      (emit-label loop)
      (inst ld temp src num)
      (inst add num (fixnumize 1))
      (inst cmp num count)
      (inst b :ne loop)
      (inst st temp dst num)

      ;; Reset the CSP.
      (emit-label done)
      (inst add csp-tn result num))))


(define-vop (uwp-entry)
  (:info label)
  (:save-p :force-to-stack)
  (:vop-var vop)
  (:generator 0
    (emit-label label)
    (note-this-location vop :non-local-entry)
    (storew lra-tn csp-tn -1)))

(define-vop (uwp-entry-block)
  (:info label)
  (:save-p :force-to-stack)
  (:results (block))
  (:vop-var vop)
  (:generator 0
    (emit-label label)
    (note-this-location vop :non-local-entry)
    (storew lra-tn csp-tn -1)
    ;; Get the block saved in UNWIND
    (loadw block csp-tn -4)))
