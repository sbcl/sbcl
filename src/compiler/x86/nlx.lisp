;;;; the definition of non-local exit for the x86 VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;; Make an environment-live stack TN for saving the SP for NLX entry.
(!def-vm-support-routine make-nlx-sp-tn (env)
  (physenv-live-tn
   (make-representation-tn *fixnum-primitive-type* any-reg-sc-number)
   env))

;;; Make a TN for the argument count passing location for a non-local entry.
(!def-vm-support-routine make-nlx-entry-arg-start-location ()
  (make-wired-tn *fixnum-primitive-type* any-reg-sc-number ebx-offset))

(defun catch-block-ea (tn)
  (aver (sc-is tn catch-block))
  (make-ea :dword :base ebp-tn
           :disp (- (* (+ (tn-offset tn) catch-block-size) n-word-bytes))))


;;;; Save and restore dynamic environment.
;;;;
;;;; These VOPs are used in the reentered function to restore the
;;;; appropriate dynamic environment. Currently we only save the
;;;; Current-Catch and the alien stack pointer. (Before sbcl-0.7.0,
;;;; when there were IR1 and byte interpreters, we had to save
;;;; the interpreter "eval stack" too.)
;;;;
;;;; We don't need to save/restore the current UNWIND-PROTECT, since
;;;; UNWIND-PROTECTs are implicitly processed during unwinding.
;;;;
;;;; We don't need to save the BSP, because that is handled automatically.

(define-vop (save-dynamic-state)
  (:results (catch :scs (descriptor-reg))
            (alien-stack :scs (descriptor-reg)))
  (:generator 13
    (load-tl-symbol-value catch *current-catch-block*)
    (load-tl-symbol-value alien-stack *alien-stack*)))

(define-vop (restore-dynamic-state)
  (:args (catch :scs (descriptor-reg))
         (alien-stack :scs (descriptor-reg)))
  #!+sb-thread (:temporary (:sc unsigned-reg) temp)
  (:generator 10
    (store-tl-symbol-value catch *current-catch-block* temp)
    (store-tl-symbol-value alien-stack *alien-stack* temp)))

(define-vop (current-stack-pointer)
  (:results (res :scs (any-reg control-stack)))
  (:generator 1
    (move res esp-tn)))

(define-vop (current-binding-pointer)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    (load-binding-stack-pointer res)))

;;;; unwind block hackery

;;; Compute the address of the catch block from its TN, then store into the
;;; block the current Fp, Env, Unwind-Protect, and the entry PC.
(define-vop (make-unwind-block)
  (:args (tn))
  (:info entry-label)
  (:temporary (:sc unsigned-reg) temp)
  (:results (block :scs (any-reg)))
  (:generator 22
    (inst lea block (catch-block-ea tn))
    (load-tl-symbol-value temp *current-unwind-protect-block*)
    (storew temp block unwind-block-current-uwp-slot)
    (storew ebp-tn block unwind-block-current-cont-slot)
    (storew (make-fixup nil :code-object entry-label)
            block catch-block-entry-pc-slot)))

;;; like MAKE-UNWIND-BLOCK, except that we also store in the specified
;;; tag, and link the block into the CURRENT-CATCH list
(define-vop (make-catch-block)
  (:args (tn)
         (tag :scs (any-reg descriptor-reg) :to (:result 1)))
  (:info entry-label)
  (:results (block :scs (any-reg)))
  (:temporary (:sc descriptor-reg) temp)
  (:generator 44
    (inst lea block (catch-block-ea tn))
    (load-tl-symbol-value temp *current-unwind-protect-block*)
    (storew temp block  unwind-block-current-uwp-slot)
    (storew ebp-tn block  unwind-block-current-cont-slot)
    (storew (make-fixup nil :code-object entry-label)
            block catch-block-entry-pc-slot)
    (storew tag block catch-block-tag-slot)
    (load-tl-symbol-value temp *current-catch-block*)
    (storew temp block catch-block-previous-catch-slot)
    (store-tl-symbol-value block *current-catch-block* temp)))

;;; Just set the current unwind-protect to TN's address. This instantiates an
;;; unwind block as an unwind-protect.
(define-vop (set-unwind-protect)
  (:args (tn))
  (:temporary (:sc unsigned-reg) new-uwp #!+sb-thread tls)
  (:generator 7
    (inst lea new-uwp (catch-block-ea tn))
    (store-tl-symbol-value new-uwp *current-unwind-protect-block* tls)))

(define-vop (unlink-catch-block)
  (:temporary (:sc unsigned-reg) #!+sb-thread tls block)
  (:policy :fast-safe)
  (:translate %catch-breakup)
  (:generator 17
    (load-tl-symbol-value block *current-catch-block*)
    (loadw block block catch-block-previous-catch-slot)
    (store-tl-symbol-value block *current-catch-block* tls)))

(define-vop (unlink-unwind-protect)
    (:temporary (:sc unsigned-reg) block #!+sb-thread tls)
  (:policy :fast-safe)
  (:translate %unwind-protect-breakup)
  (:generator 17
    (load-tl-symbol-value block *current-unwind-protect-block*)
    (loadw block block unwind-block-current-uwp-slot)
    (store-tl-symbol-value block *current-unwind-protect-block* tls)))

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
             (inst jecxz no-values)
             (loadw (tn-ref-tn values) start -1)
             (emit-label no-values)))
          (t
           (collect ((defaults))
             (do ((i 0 (1+ i))
                  (tn-ref values (tn-ref-across tn-ref)))
                 ((null tn-ref))
               (let ((default-lab (gen-label))
                     (tn (tn-ref-tn tn-ref)))
                 (defaults (cons default-lab tn))

                 (inst cmp count (fixnumize i))
                 (inst jmp :le default-lab)
                 (sc-case tn
                   ((descriptor-reg any-reg)
                    (loadw tn start (- (1+ i))))
                   ((control-stack)
                    (loadw move-temp start (- (1+ i)))
                    (inst mov tn move-temp)))))
             (let ((defaulting-done (gen-label)))
               (emit-label defaulting-done)
               (assemble (*elsewhere*)
                 (dolist (def (defaults))
                   (emit-label (car def))
                   (inst mov (cdr def) nil-value))
                 (inst jmp defaulting-done))))))
    (inst mov esp-tn sp)))

(define-vop (nlx-entry-multiple)
  (:args (top)
         (source)
         (count :target ecx))
  ;; Again, no SC restrictions for the args, 'cause the loading would
  ;; happen before the entry label.
  (:info label)
  (:temporary (:sc unsigned-reg :offset ecx-offset :from (:argument 2)) ecx)
  (:temporary (:sc unsigned-reg :offset esi-offset) esi)
  (:temporary (:sc unsigned-reg :offset edi-offset) edi)
  (:results (result :scs (any-reg) :from (:argument 0))
            (num :scs (any-reg control-stack)))
  (:save-p :force-to-stack)
  (:vop-var vop)
  (:generator 30
    (emit-label label)
    (note-this-location vop :non-local-entry)

    (inst lea esi (make-ea :dword :base source :disp (- n-word-bytes)))
    ;; The 'top' arg contains the %esp value saved at the time the
    ;; catch block was created and points to where the thrown values
    ;; should sit.
    (move edi top)
    (move result edi)

    (inst sub edi n-word-bytes)
    (move ecx count)                    ; fixnum words == bytes
    (move num ecx)
    (inst shr ecx word-shift)           ; word count for <rep movs>
    ;; If we got zero, we be done.
    (inst jecxz done)
    ;; Copy them down.
    (inst std)
    (inst rep)
    (inst movs :dword)

    DONE
    ;; solaris requires DF being zero.
    #!+sunos (inst cld)
    ;; Reset the CSP at last moved arg.
    (inst lea esp-tn (make-ea :dword :base edi :disp n-word-bytes))))


;;; This VOP is just to force the TNs used in the cleanup onto the stack.
(define-vop (uwp-entry)
  (:info label)
  (:save-p :force-to-stack)
  (:results (block) (start) (count))
  (:ignore block start count)
  (:vop-var vop)
  (:generator 0
    (emit-label label)
    (note-this-location vop :non-local-entry)))
