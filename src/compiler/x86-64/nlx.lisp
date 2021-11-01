;;;; the definition of non-local exit for the x86 VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;; Make a TN for the argument count passing location for a non-local entry.
(defun make-nlx-entry-arg-start-location ()
    (make-wired-tn *fixnum-primitive-type* any-reg-sc-number rbx-offset))

(defun catch-block-ea (tn &optional (offset 0))
  (aver (sc-is tn catch-block))
  (ea (frame-byte-offset (- (+ -1 (tn-offset tn) catch-block-size) offset)) rbp-tn))

(defun unwind-block-ea (tn &optional (offset 0))
  (aver (sc-is tn unwind-block))
  (ea (frame-byte-offset (- (+ -1 (tn-offset tn) unwind-block-size offset) offset)) rbp-tn))

;;;; Save and restore dynamic environment.
(define-vop (current-stack-pointer)
  (:results (res :scs (any-reg control-stack)))
  (:generator 1
    (move res rsp-tn)))

(define-vop (current-binding-pointer)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    (load-binding-stack-pointer res)))

;;;; unwind block hackery

;;; Compute the address of the catch block from its TN, then store into the
;;; block the current Fp, Env, Unwind-Protect, and the entry PC.
#+sb-thread
(progn
  ;; MOVAPD instruction faults if not properly aligned
  (assert (evenp (/ (info :variable :wired-tls '*binding-stack-pointer*) n-word-bytes)))
  (assert (= (- (info :variable :wired-tls '*current-catch-block*)
                (info :variable :wired-tls '*binding-stack-pointer*))
             n-word-bytes))
  (assert (= (- unwind-block-current-catch-slot unwind-block-bsp-slot) 1)))

(define-vop (make-unwind-block)
  (:args (tn))
  (:info entry-label)
  (:temporary (:sc unsigned-reg) temp)
  #+sb-thread
  (:temporary (:sc complex-double-reg) xmm-temp)
  (:results (block :scs (any-reg)))
  (:generator 22
    (inst lea block (unwind-block-ea tn))
    (load-tl-symbol-value temp *current-unwind-protect-block*)
    (storew temp block unwind-block-uwp-slot)
    (storew rbp-tn block unwind-block-cfp-slot)
    (inst lea temp (rip-relative-ea entry-label))
    (storew temp block unwind-block-entry-pc-slot)
    #+sb-thread
    (let ((bsp (info :variable :wired-tls '*binding-stack-pointer*)))
      (inst movapd xmm-temp (thread-tls-ea bsp))
      (inst movupd (ea (* unwind-block-bsp-slot n-word-bytes) block) xmm-temp))
    #-sb-thread
    (progn
      (load-binding-stack-pointer temp)
      (storew temp block unwind-block-bsp-slot)
      (load-tl-symbol-value temp *current-catch-block*)
      (storew temp block unwind-block-current-catch-slot))))

;;; like MAKE-UNWIND-BLOCK, except that we also store in the specified
;;; tag, and link the block into the CURRENT-CATCH list
(define-vop (make-catch-block)
  (:args (tn)
         (tag :scs (any-reg descriptor-reg) :to (:result 1)))
  (:info entry-label)
  (:results (block :scs (any-reg)))
  (:temporary (:sc descriptor-reg) temp)
  #+sb-thread
  (:temporary (:sc complex-double-reg) xmm-temp)
  (:generator 44
    (inst lea block (catch-block-ea tn))
    (load-tl-symbol-value temp *current-unwind-protect-block*)
    (storew temp block catch-block-uwp-slot)
    (storew rbp-tn block catch-block-cfp-slot)
    (inst lea temp (rip-relative-ea entry-label))
    (storew temp block catch-block-entry-pc-slot)
    (storew tag block catch-block-tag-slot)
    #+sb-thread
    (let ((bsp #1=(info :variable :wired-tls '*binding-stack-pointer*)))
      #.(assert (and (= (- (info :variable :wired-tls '*current-catch-block*) #1#) n-word-bytes)
                     (= (- catch-block-previous-catch-slot catch-block-bsp-slot) 1)))
      (inst movapd xmm-temp (thread-tls-ea bsp))
      (inst movupd (ea (* catch-block-bsp-slot n-word-bytes) block) xmm-temp)
      (store-tl-symbol-value block *current-catch-block*))
    #-sb-thread
    (progn
      (load-tl-symbol-value temp *current-catch-block*)
      (storew temp block catch-block-previous-catch-slot)
      (store-tl-symbol-value block *current-catch-block*)
      (load-binding-stack-pointer temp)
      (storew temp block catch-block-bsp-slot))))

;;; Just set the current unwind-protect to UWP. This instantiates an
;;; unwind block as an unwind-protect.
(define-vop (set-unwind-protect)
  (:args (uwp :scs (any-reg)))
  (:generator 7
    (store-tl-symbol-value uwp *current-unwind-protect-block*)))

(define-vop (%catch-breakup)
  (:args (current-block))
  (:temporary (:sc unsigned-reg) block)
  (:policy :fast-safe)
  (:generator 17
    (inst mov block (catch-block-ea current-block
                                    catch-block-previous-catch-slot))
    (store-tl-symbol-value block *current-catch-block*)))

(define-vop (%unwind-protect-breakup)
  (:args (current-block))
  (:temporary (:sc unsigned-reg) block)
  (:policy :fast-safe)
  (:generator 17
     (inst mov block (unwind-block-ea current-block
                                      unwind-block-uwp-slot))
     (store-tl-symbol-value block *current-unwind-protect-block*)))

;;;; NLX entry VOPs
(define-vop (nlx-entry)
  ;; Note: we can't list an sc-restriction, 'cause any load vops would
  ;; be inserted before the return-pc label.
  (:args (sp)
         (start)
         (count))
  (:results (values :more t))
  (:temporary (:sc descriptor-reg) move-temp)
  (:info label nvals)
  (:save-p :force-to-stack)
  (:vop-var vop)
  (:generator 30
    (emit-label label)
    (note-this-location vop :non-local-entry)
    (cond ((zerop nvals))
          ((= nvals 1)
           (let ((no-values (gen-label)))
             (inst mov (tn-ref-tn values) nil-value)
             (inst test rcx-tn rcx-tn)
             (inst jmp :z no-values)
             (loadw (tn-ref-tn values) start -1)
             (emit-label no-values)))
          (t
           ;; FIXME: this is mostly copied from
           ;; DEFAULT-UNKNOWN-VALUES.
           (collect ((defaults))
             (do ((i 0 (1+ i))
                  (tn-ref values (tn-ref-across tn-ref)))
                 ((null tn-ref))
               (let ((default-lab (gen-label))
                     (tn (tn-ref-tn tn-ref))
                     (first-stack-arg-p (= i register-arg-count)))
                 (defaults (cons default-lab (cons tn first-stack-arg-p)))
                 (inst cmp count (fixnumize i))
                 (inst jmp :le default-lab)
                 (when first-stack-arg-p
                   (storew rdx-tn rbx-tn -1))
                 (sc-case tn
                   ((descriptor-reg any-reg)
                    (loadw tn start (frame-word-offset (+ sp->fp-offset i))))
                   ((control-stack)
                    (loadw move-temp start
                           (frame-word-offset (+ sp->fp-offset i)))
                    (inst mov tn move-temp)))))
             (let ((defaulting-done (gen-label)))
               (emit-label defaulting-done)
               (assemble (:elsewhere)
                 (dolist (default (defaults))
                   (emit-label (car default))
                   (when (cddr default)
                     (inst push rdx-tn))
                   (inst mov (second default) nil-value))
                 (inst jmp defaulting-done))))))
    (inst mov rsp-tn sp)))

(define-vop (nlx-entry-single)
  (:args (sp)
         (start))
  (:results (res :from :load))
  (:info label)
  (:save-p :force-to-stack)
  (:vop-var vop)
  (:generator 30
    (emit-label label)
    (note-this-location vop :non-local-entry)
    (inst mov res start)
    (inst mov rsp-tn sp)))

(define-vop (nlx-entry-multiple)
  (:args (top :target result
              :scs (any-reg))
         (source :to :save)
         (count :target rcx))
  (:info label)
  (:before-load
    (emit-label label)
    (note-this-location vop :non-local-entry))
  (:temporary (:sc unsigned-reg :offset rcx-offset :from (:argument 2)) rcx)
  (:temporary (:sc unsigned-reg) loop-index temp)
  (:results (result :scs (any-reg))
            (num :scs (any-reg control-stack)))
  (:save-p :force-to-stack)
  (:args-var top-tn-ref)
  (:vop-var vop)
  (:generator 30
    ;; The 'top' arg contains the %esp value saved at the time the
    ;; catch block was created and points to where the thrown values
    ;; should sit.
    (if (eq (tn-kind result) :unused)
        (setf result top)
        (move result top))

    (unless (eq (tn-kind num) :unused)
      (move num count))
    (move rcx count)
    (zeroize loop-index)
    (inst test rcx rcx)
    (inst jmp :z DONE)
    LOOP
    (inst sub loop-index n-word-bytes)
    (inst mov temp (ea source loop-index))
    (inst mov (ea result loop-index) temp)

    (inst sub rcx (fixnumize 1))
    (inst jmp :nz LOOP)
    DONE
    ;; Reset the CSP at last moved arg.
    (inst lea rsp-tn (ea result loop-index))))


;;; This VOP is just to force the TNs used in the cleanup onto the stack.
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
    ;; Get the saved block in UNWIND
    (inst mov block (ea (* 3 n-word-bytes) rsp-tn))))

(define-vop (unwind-to-frame-and-call)
    (:args (ofp :scs (descriptor-reg))
           (uwp :scs (descriptor-reg))
           (function :scs (descriptor-reg) :to :load :target saved-function)
           (bsp :scs (any-reg descriptor-reg))
           (catch-block :scs (any-reg descriptor-reg)))
  (:arg-types system-area-pointer system-area-pointer t t t)
  (:temporary (:sc sap-reg) temp)
  (:temporary (:sc descriptor-reg :offset rbx-offset) saved-function)
  (:temporary (:sc unsigned-reg :offset rax-offset) block)
  (:temporary (:sc unsigned-reg :offset r11-offset) extra-temp-reg)
  (:vop-var vop)
  (:generator 22
    ;; Store the function into a non-stack location, since we'll be
    ;; unwinding the stack and destroying register contents before we
    ;; use it.  It turns out that RBX is preserved as part of the
    ;; normal multiple-value handling of an unwind, so use that.
    (move saved-function function)

    ;; Allocate space for magic UWP block.
    (inst sub rsp-tn (* unwind-block-size n-word-bytes))
    ;; Set up magic catch / UWP block.
    (move block rsp-tn)
    (loadw temp uwp sap-pointer-slot other-pointer-lowtag)
    (storew temp block unwind-block-uwp-slot)
    (loadw temp ofp sap-pointer-slot other-pointer-lowtag)
    (storew temp block unwind-block-cfp-slot)

    (inst lea extra-temp-reg (rip-relative-ea entry-label))
    (storew extra-temp-reg block unwind-block-entry-pc-slot)
    (storew bsp block unwind-block-bsp-slot)
    (storew catch-block block unwind-block-current-catch-slot)

    ;; Run any required UWPs.
    (invoke-asm-routine 'jmp 'unwind vop)
    ENTRY-LABEL

    ;; Move our saved function to where we want it now.
    (move block saved-function)

    ;; No parameters
    (zeroize rcx-tn)

    ;; Clear the stack
    (inst lea rsp-tn (ea (* (- sp->fp-offset 3) n-word-bytes) rbp-tn))

    ;; Push the return-pc so it looks like we just called.
    (pushw rbp-tn (frame-word-offset return-pc-save-offset))

    ;; Call it
    (inst jmp (ea (- (* closure-fun-slot n-word-bytes) fun-pointer-lowtag)
                  block))))
