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

;;; Make a TN for the argument count passing location for a non-local entry.
(defun make-nlx-entry-arg-start-location ()
  (make-wired-tn *fixnum-primitive-type* any-reg-sc-number ebx-offset))

(defun catch-block-ea (tn)
  (aver (sc-is tn catch-block))
  (make-ea :dword :base ebp-tn
           :disp (frame-byte-offset (+ -1 (tn-offset tn) catch-block-size))))


;;;; Save and restore dynamic environment.
;;;;
;;;; These VOPs are used in the reentered function to restore the
;;;; appropriate dynamic environment. Currently we only save the
;;;; Current-Catch. (Before sbcl-0.7.0,
;;;; when there were IR1 and byte interpreters, we had to save
;;;; the interpreter "eval stack" too.)
;;;;
;;;; We don't need to save/restore the current UNWIND-PROTECT, since
;;;; UNWIND-PROTECTs are implicitly processed during unwinding.
;;;;
;;;; We don't need to save the BSP, because that is handled automatically.

(define-vop (save-dynamic-state)
  (:results (catch :scs (descriptor-reg)))
  (:generator 13
    (load-tl-symbol-value catch *current-catch-block*)))

(define-vop (restore-dynamic-state)
  (:args (catch :scs (descriptor-reg)))
  #!+sb-thread (:temporary (:sc unsigned-reg) temp)
  (:generator 10
    (store-tl-symbol-value catch *current-catch-block* temp)))

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
            block catch-block-entry-pc-slot)
    #!+win32
    (progn
      (inst mov temp (make-ea :dword :disp 0) :fs)
      (storew temp block unwind-block-next-seh-frame-slot))))

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
    #!+win32
    (progn
      (inst mov temp (make-ea :dword :disp 0) :fs)
      (storew temp block unwind-block-next-seh-frame-slot))
    (storew tag block catch-block-tag-slot)
    (load-tl-symbol-value temp *current-catch-block*)
    (storew temp block catch-block-previous-catch-slot)
    (store-tl-symbol-value block *current-catch-block* temp)))

;;; Just set the current unwind-protect to TN's address. This instantiates an
;;; unwind block as an unwind-protect.
(define-vop (set-unwind-protect)
  (:args (tn))
  (:temporary (:sc unsigned-reg) new-uwp #!+sb-thread tls #!+win32 seh-frame)
  (:generator 7
    (inst lea new-uwp (catch-block-ea tn))
    #!+win32
    (progn
      (storew (make-fixup 'uwp-seh-handler :assembly-routine)
              new-uwp unwind-block-seh-frame-handler-slot)
      (inst lea seh-frame
            (make-ea-for-object-slot new-uwp
                                     unwind-block-next-seh-frame-slot 0))
      (inst mov (make-ea :dword :disp 0) seh-frame :fs))
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
    ;; NOTE: When we have both #!+sb-thread and #!+win32, we only need one temp
    (:temporary (:sc unsigned-reg) block #!+sb-thread tls #!+win32 seh-frame)
  (:policy :fast-safe)
  (:translate %unwind-protect-breakup)
  (:generator 17
    (load-tl-symbol-value block *current-unwind-protect-block*)
    #!+win32
    (progn
      (loadw seh-frame block unwind-block-next-seh-frame-slot)
      (inst mov (make-ea :dword :disp 0) seh-frame :fs))
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
                   (storew edx-tn ebx-tn -1))
                 (sc-case tn
                   ((descriptor-reg any-reg)
                    (loadw tn start (frame-word-offset (+ sp->fp-offset i))))
                   ((control-stack)
                    (loadw move-temp start
                           (frame-word-offset (+ sp->fp-offset i)))
                    (inst mov tn move-temp)))))
             (let ((defaulting-done (gen-label)))
               (emit-label defaulting-done)
               (assemble (*elsewhere*)
                 (dolist (default (defaults))
                   (emit-label (car default))
                   (when (cddr default)
                     (inst push edx-tn))
                   (inst mov (second default) nil-value))
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
    (inst jecxz DONE)
    ;; Copy them down.
    (inst std)
    (inst rep)
    (inst movs :dword)
    (inst cld)
    DONE
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

(define-vop (unwind-to-frame-and-call)
    (:args (ofp :scs (descriptor-reg))
           (uwp :scs (descriptor-reg))
           (function :scs (descriptor-reg) :to :load :target saved-function))
  (:arg-types system-area-pointer system-area-pointer t)
  (:temporary (:sc sap-reg) temp)
  (:temporary (:sc descriptor-reg :offset ebx-offset) saved-function)
  (:temporary (:sc unsigned-reg :offset eax-offset) block)
  (:generator 22
    ;; Store the function into a non-stack location, since we'll be
    ;; unwinding the stack and destroying register contents before we
    ;; use it.  It turns out that EBX is preserved as part of the
    ;; normal multiple-value handling of an unwind, so use that.
    (move saved-function function)

    ;; Allocate space for magic UWP block.
    (inst sub esp-tn (* unwind-block-size n-word-bytes))
    ;; Set up magic catch / UWP block.
    (move block esp-tn)
    (loadw temp uwp sap-pointer-slot other-pointer-lowtag)
    (storew temp block unwind-block-current-uwp-slot)
    (loadw temp ofp sap-pointer-slot other-pointer-lowtag)
    (storew temp block unwind-block-current-cont-slot)

    (storew (make-fixup nil :code-object entry-label)
            block
            catch-block-entry-pc-slot)

    ;; Run any required UWPs.
    (inst jmp (make-fixup 'unwind :assembly-routine))
    ENTRY-LABEL

    ;; Move our saved function to where we want it now.
    (move block saved-function)

    ;; No parameters
    (inst xor ecx-tn ecx-tn)

    ;; Clear the stack
    (inst lea esp-tn
          (make-ea :dword :base ebp-tn
                   :disp (* (- sp->fp-offset 3) n-word-bytes)))

    ;; Push the return-pc so it looks like we just called.
    (pushw ebp-tn (frame-word-offset return-pc-save-offset))

    ;; Call it
    (inst jmp (make-ea :dword :base block
                       :disp (- (* closure-fun-slot n-word-bytes)
                                fun-pointer-lowtag)))))
