;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(macrolet ((static-fun-addr (name optimize)
             (declare (ignorable optimize))
             #-immobile-code `(ea (make-fixup ,name :linkage-cell) null-tn)
             ;; With immobile code, there is a choice of calling to the simple-fun
             ;; entrypoint ("optimized"), or through the linkage cell ("deoptimized").
             ;; Only the latter is TRACEable.
             #+immobile-code
             `(if (not ,optimize)
                  (rip-relative-ea (make-fixup ,name :linkage-cell))
                  ;; Caution: this looks like it jumps to the linkage cell's address,
                  ;; and that is indeed what it would do if it were not for the fact that
                  ;; genesis recognizes that this isn't right, and instead does what you mean.
                  ;; i.e. genesis resolves a :LINKAGE-CELL fixup differently in asm code.
                  ;; Though a new fixup flavor would be technically correct, it would not
                  ;; be generally usable after self-build because we don't have direct jumps
                  ;; between simple-funs. (However, editcore can and will cause it to occur)
                  (make-fixup ,name :linkage-cell))))

(defun call-static-fun (fun arg-count &optional (optimize t))
  (declare (ignorable optimize))
  (inst push rbp-tn)
  (inst mov rbp-tn rsp-tn)
  (inst sub rsp-tn (* n-word-bytes 2))
  (inst mov (ea rsp-tn) rbp-tn)
  (inst mov rbp-tn rsp-tn)
  (inst mov rcx-tn (fixnumize arg-count))
  (inst call (static-fun-addr fun optimize))
  (inst pop rbp-tn))

(defun tail-call-static-fun (fun arg-count &optional (optimize t))
  (declare (ignorable optimize))
  (inst push rbp-tn)
  (inst mov rbp-tn rsp-tn)
  (inst sub rsp-tn n-word-bytes)
  (inst push (ea (frame-byte-offset return-pc-save-offset) rbp-tn))
  (inst mov rcx-tn (fixnumize arg-count))
  (inst jmp (static-fun-addr fun optimize))))

(defun test-cpu-feature (bit-number)
  (multiple-value-bind (byte bit) (floor bit-number n-byte-bits)
    (inst test :byte (static-constant-ea cpu-feature-bits byte) (ash 1 bit))))

(defun uniquify-fixup (name &aux (asmstream *asmstream*))
  (or (cdr (assoc name (sb-assem::asmstream-indirection-table asmstream)))
      (let ((label (gen-label)))
        (assemble (:elsewhere)
          (emit-label label)
          (inst jmp (ea (make-fixup name :assembly-routine) null-tn)))
        (push (cons name label) (sb-assem::asmstream-indirection-table asmstream))
        label)))

(defun invoke-asm-routine (inst routine vop)
  (inst* (the (member jmp call) inst)
         (if (or (null vop) (code-immobile-p vop))
             (make-fixup routine :assembly-routine)
             (ea (make-fixup routine :assembly-routine) null-tn))))

(defmacro call-reg-specific-asm-routine (node prefix tn &optional (suffix ""))
  `(invoke-asm-routine
    'call
    (aref ,(map 'vector
                (lambda (x)
                  (unless (member x '(rsp rbp) :test 'string=)
                    (symbolicate prefix x suffix)))
                +qword-register-names+)
          (tn-offset ,tn))
    ,node))

(defun generate-call-sequence (name style vop options)
  (declare (ignore options))
  (ecase style
    ((:raw :full-call :full-call-no-return)
     (values
      `((note-this-location ,vop :call-site)
        (invoke-asm-routine 'call ',name ,vop)
        (note-this-location ,vop :single-value-return))
      (if (eql style :raw) nil '((:save-p :compute-only)))))
    (:none
     (values
      `((invoke-asm-routine 'jmp ',name ,vop))
      nil))))

(defun generate-return-sequence (style)
  (ecase style
    (:raw
     `((inst ret)))
    (:full-call
     `((inst clc)
       (inst ret)))
    ((:none :full-call-no-return))))

(defconstant xsave-area-size (+ 512 64 256))
(defconstant xsave-area-alignment 64)

;;; Save or restore all FPRs at the stack pointer as it existed just prior
;;; to the call to the asm routine.
(defun call-fpr-save/restore-routine (selector)
  (let ((routine (ecase selector
                   (:save 'fpr-save)
                   (:restore 'fpr-restore))))
    (if (or (not (boundp 'sb-c:*component-being-compiled*))
            (code-immobile-p sb-c:*component-being-compiled*))
        ;; direct call from asm routine or immobile code
        (inst call (make-fixup routine :assembly-routine))
        ;; indirect call from dynamic space
        (inst call (ea (make-fixup routine :assembly-routine) null-tn)))))

(defmacro regs-pushlist (&rest regs)
  `(progn ,@(mapcar (lambda (stem) `(inst push ,(symbolicate stem "-TN"))) regs)))
(defmacro regs-poplist (&rest regs)
  `(progn ,@(mapcar (lambda (stem) `(inst pop ,(symbolicate stem "-TN")))
                    (reverse regs))))

;;; Invoke BODY, saving and restoring all registers other than those mentioned in EXCEPT,
;;; and not EFLAGS unless specifically requested.
;;; If this is utilized at other than an assembly routine entry point, it's best to
;;; specify a frame register, because backtracing might become confused otherwise.
;;; Consider the case where the stack pointer has already been decremented to make space
;;; for local storage. Then the word at the stack pointer is not a return address.
;;; After the customary 2-instruction prologue of "PUSH RBP ; MOV RBP,RSP"
;;; there is a correct chain of saved RBP values, but 1 word up from the current RBP
;;; is probably not a saved program counter, and will look weird to treat it as such.
(defmacro with-registers-preserved ((convention &key eflags except (frame-reg 'rbp))
                                    &body body)
  ;: Convention:
  ;;   C    = save GPRs that C call can change
  ;;   Lisp = save GPRs that lisp call can change
  (aver (member convention '(lisp c)))
  (let* ((save-fpr (neq except 'fp))
         (fpr-align xsave-area-alignment)
         (except (if (eq except 'fp) nil (ensure-list except)))
         (clobberables
          (remove (intern (aref +qword-register-names+ card-table-reg))
           (remove frame-reg
                   `(rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12
                         ;; 13 is usable only if not permanently wired to the thread base
                         #+gs-seg r13 r14 r15))))
         (frame-tn (when frame-reg (symbolicate frame-reg "-TN"))))
    (aver (subsetp except clobberables)) ; Catch spelling mistakes
    ;; Since FPR-SAVE / -RESTORE utilize RAX, returning RAX from an assembly
    ;; routine (by *not* preserving it) will be meaningless.
    ;; You'd have to modify -SAVE / -RESTORE to avoid clobbering RAX.
    ;; This is a bit limiting: if you ask not to preserve RAX, what you mean is exactly that:
    ;; it does not matter what value is gets. But EXCEPT has a dual purpose of also
    ;; propagating the value out from BODY. We _should_ allow RAX in the list of things
    ;; not to save, in case the caller wants to be maximally efficient and specify that RAX
    ;; can be trashed with impunity. But it helps with incorrect usage for now
    ;; to raise this error.
    (when (member 'rax except)
      (error "Excluding RAX from preserved GPRs probably will not do what you want."))
    (let* ((gprs ; take SET-DIFFERENCE with EXCEPT but in a predictable order
             (remove-if (lambda (x) (member x except))
                        (ecase convention
                          ;; RBX and R12..R15 are preserved across C call
                          (c '(rax rcx rdx rsi rdi r8 r9 r10 r11))
                          ;; all GPRs are potentially destroyed across lisp call
                          (lisp clobberables))))
           ;; each 8 registers pushed preserves 64-byte alignment
           (alignment-bytes
             (-  (nth-value 1 (ceiling (* n-word-bytes (length gprs)) fpr-align)))))
      (when eflags (aver frame-reg)) ; don't need to support no-frame-tn,yes-flags
      `(progn
         ;; Obviously we have to save EFLAGS before messing them up by doing arithmetic.
         ;; So if requested, save them inside the new frame. It would mess up backtrace
         ;; to PUSHF before PUSH RBP because then the stack word at 1 slot above RBP
         ;; would not be the return PC pushed by a preceding CALL. It would similarly be
         ;; wrong to push flags in between the push of RBP and the MOV.
         ,@(when frame-tn
             `((inst push ,frame-tn)
               (inst mov ,frame-tn rsp-tn)
               ,@(when eflags '((inst pushf)))))
         ,@(when save-fpr
             `((inst and rsp-tn ,(- fpr-align))))
         (regs-pushlist ,@gprs)
         ,@(when save-fpr
             `((inst sub rsp-tn ,(+ alignment-bytes xsave-area-size))
               (call-fpr-save/restore-routine :save)))
         (assemble () ,@body)
         ,@(when save-fpr
             `((call-fpr-save/restore-routine :restore)
               (inst add rsp-tn ,(+ alignment-bytes xsave-area-size))))
         (regs-poplist ,@gprs)
         ,@(cond ((and (eq frame-tn 'rbp-tn) (not eflags))
                  '((inst leave)))
             ;; If EFLAGS got pushed, restoring RBP can't be done with LEAVE,
             ;; because RSP has to be decremented by 1 word.
             ;;    return-PC
             ;;    saved RBP     <-- new RBP points here
             ;;    saved EFLAGS
                 (frame-tn
                  `(,@(if eflags
                          `((inst lea rsp-tn (ea -8 ,frame-tn))
                            (inst popf))
                          `((inst mov rsp-tn ,frame-tn)))
                    (inst pop ,frame-tn))))))))

(defmacro call-c (fun &rest args)
  (aver (stringp fun))
  (setq fun #-immobile-space (if (member :sb-assembling sb-xc:*features*)
                                 `(make-fixup ,fun :foreign) ; PC-relative call
                                 `(ea (make-fixup ,fun :foreign ,+nil-indirect+) null-tn))
            #+immobile-space `(make-fixup ,fun :foreign)) ; always PC-relative
  `(progn
     #+win32 (inst sub rsp-tn 32)
     ,@(loop for arg in args
             for c-arg in
             #+win32 '(rcx-tn rdx-tn r8-tn r9-tn)
             #-win32 '(rdi-tn rsi-tn rdx-tn rcx-tn r8-tn r9-tn)
             collect
             (if (typep arg '(cons (eql addressof)))
                 `(inst lea ,c-arg ,(cadr arg))
                 `(inst mov ,c-arg ,arg)))
     (inst call ,fun)
     #+win32 (inst add rsp-tn 32)))

;; A multi-value return of more than REGISTER-ARG-COUNT values used to be emitted
;; as "PUSH return-address ; RET"
;; but I don't see that it's better than a JMP instruction. It may have been thought
;; to help because the CPU predicts where to fetch instructions after a RET. That's true
;; in so far as RET utilizes a hidden (internal to the CPU) call/return stack to avoid
;; referencing the memory-based stack.  But an explicit PUSH is an extra memory store
;; beyond the price already paid for a CALL instruction, while the operand of JMP is either
;; already in a register, or may be speculatively loaded.
;; One might argue that failure to execute RET causes imbalance to the hidden stack,
;; but Lisp causes inefficiencies for other reasons, and it it seems overly hopeful to claim
;; that 4-valued return is suboptimal because it didn't use 2 instructions to do the job of 1.
;; But if a microbenchmark can show that to be true, it suffices to change it only here
;; instead of in a few places.
(defmacro emit-mv-return (reg/mem &optional emit-stc)
  ;; All mv returns (incl. 0) need CF set, but it could occur outside (before) the macro
  `(progn ,@(if emit-stc '((inst stc)))
          (inst jmp ,reg/mem)))
