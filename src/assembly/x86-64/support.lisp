;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(defun test-cpu-feature (feature-bit)
  (multiple-value-bind (byte bit)
      (floor (+ feature-bit n-fixnum-tag-bits) n-byte-bits)
    (inst test :byte (static-symbol-value-ea '*cpu-feature-bits* byte) (ash 1 bit))))

(defun uniquify-fixup (name &aux (asmstream *asmstream*))
  (or (cdr (assoc name (sb-assem::asmstream-indirection-table asmstream)))
      (let ((label (gen-label)))
        (assemble (:elsewhere)
          (emit-label label)
          (inst jmp (ea (make-fixup name :assembly-routine*))))
        (push (cons name label) (sb-assem::asmstream-indirection-table asmstream))
        label)))

(defun invoke-asm-routine (inst routine vop)
  (inst* (the (member jmp call) inst)
         (if (or (null vop) (sb-c::code-immobile-p vop))
             (make-fixup routine :assembly-routine)
             (ea (make-fixup routine :assembly-routine*)))))

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
;;; Save or restore all FPRs at the stack pointer as it existed just prior
;;; to the call to the asm routine.
(defun call-fpr-save/restore-routine (selector)
  (let ((routine (ecase selector
                   (:save 'fpr-save)
                   (:restore 'fpr-restore))))
    (if (or (not (boundp 'sb-c:*component-being-compiled*))
            (sb-c::code-immobile-p sb-c:*component-being-compiled*))
        ;; direct call from asm routine or immobile code
        (inst call (make-fixup routine :assembly-routine))
        ;; indirect call from dynamic space
        (inst call (ea (make-fixup routine :assembly-routine*))))))

(defmacro regs-pushlist (&rest regs)
  `(progn ,@(mapcar (lambda (stem) `(inst push ,(symbolicate stem "-TN"))) regs)))
(defmacro regs-poplist (&rest regs)
  `(progn ,@(mapcar (lambda (stem) `(inst pop ,(symbolicate stem "-TN")))
                    (reverse regs))))

;;; Invoke BODY, saving and restoring all registers except FLAGS and anything in EXCEPT.
;;; If this is utilized at other than an assembly routine entry point, it's best to
;;; specify a frame register, because backtracing might become confused otherwise.
;;; Consider the case where the stack pointer has already been decremented to make space
;;; for local storage. Then the word at the stack pointer is not a return address.
;;; After the customary 2-instruction prologue of "PUSH RBP ; MOV RBP,RSP"
;;; there is a correct chain of saved RBP values, but 1 word up from the current RBP
;;; is probably not a saved program counter, and will look weird to treat it as such.
(defmacro with-registers-preserved ((convention &key except (frame-reg 'rbp)) &body body)
  ;: Convention:
  ;;   C    = save GPRs that C call can change
  ;;   Lisp = save GPRs that lisp call can change
  (aver (member convention '(lisp c)))
  (aver (eql card-table-reg 12)) ; change detector
  (let ((fpr-align 64)
        (except (ensure-list except))
        (clobberables
         (remove frame-reg
                 `(rax rbx rcx rdx rsi rdi r8 r9 r10 r11
           ;; 13 is usable only if not permanently wired to the thread base
                       #+gs-seg r13
                       r14 r15)))
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
      `(progn
         ,@(when frame-tn
             `((inst push ,frame-tn)
               (inst mov ,frame-tn rsp-tn)))
         (inst and rsp-tn ,(- fpr-align))
         (regs-pushlist ,@gprs)
         (inst sub rsp-tn ,(+ alignment-bytes xsave-area-size))
         (call-fpr-save/restore-routine :save)
         (assemble () ,@body)
         (call-fpr-save/restore-routine :restore)
         (inst add rsp-tn ,(+ alignment-bytes xsave-area-size))
         (regs-poplist ,@gprs)
         ,@(when frame-tn
             `((inst mov rsp-tn ,frame-tn)
               (inst pop ,frame-tn)))))))
