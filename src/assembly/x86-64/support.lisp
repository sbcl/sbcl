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
        ;; This has to be separate from the :ELSEWHERE section because we could be
        ;; emitting code into :ELSEWHERE when requesting a unique label.
        (assemble (:indirections)
          (emit-label label)
          (inst jmp (ea (make-fixup name :assembly-routine*))))
        (push (cons name label) (sb-assem::asmstream-indirection-table asmstream))
        label)))

(defun invoke-asm-routine (inst routine vop &optional uniquify)
  (declare (ignorable vop))
  (let ((fixup
         (cond ((sb-c::code-immobile-p vop)
                (make-fixup routine :assembly-routine))
               (uniquify
                (uniquify-fixup routine))
               (t
                (ea (make-fixup routine :assembly-routine*))))))
    (ecase inst
      (jmp  (inst jmp fixup))
      (call (inst call fixup)))))

(defun generate-call-sequence (name style vop options)
  (declare (ignore options))
  (ecase style
      (:raw
       (values
        `((note-this-location ,vop :call-site)
          (invoke-asm-routine 'call ',name ,vop)
          (note-this-location ,vop :single-value-return))
        nil))
      ((:full-call :full-call-no-return)
       (values
        `((note-this-location ,vop :call-site)
          (invoke-asm-routine 'call ',name ,vop)
          (note-this-location ,vop :single-value-return))
        '((:save-p :compute-only))))
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

(defmacro with-registers-preserved ((convention &key except) &body body)
  ;: Convention:
  ;;   C    = save GPRs that C call can change
  ;;   Lisp = save GPRs that lisp call can change
  (let ((fpr-align 32))
    (flet ((gpr-save/restore (operation except)
             (declare (type (member push pop) operation))
             (let ((registers (ecase convention
                               ;; RBX and R12..R15 are preserved across C call
                               (c '#1=(rax-tn rcx-tn rdx-tn rsi-tn rdi-tn r8-tn r9-tn r10-tn r11-tn))
                               ;; all GPRs are potentially destroyed across lisp call
                               (lisp '(rbx-tn r12-tn #-sb-thread r13-tn r14-tn r15-tn . #1#)))))
               (when except
                 (setf registers (remove except registers)))
               ;; Preserve alignment
               (when (oddp (length registers))
                 (push (car registers) registers))
               (mapcar (lambda (reg)
                         `(inst ,operation ,reg))
                       (if (eq operation 'pop) (reverse registers) registers)))))
    `(progn
       (inst push rbp-tn)
       (inst mov rbp-tn rsp-tn)
       (inst and rsp-tn ,(- fpr-align))
       (inst sub rsp-tn ,(* 16 fpr-align))
       ;; Using rip-relative call indirect makes shrinkwrapped cores work
       ;; with no modification whatsoever to editcore.
       ;; It wouldn't work straightforwardly using a call indirect
       ;; with an absolute EA.
       ;; KLUDGE: index of FPR-SAVE is 4
       ;; (inst call (ea (make-fixup 'fpr-save :assembly-routine*)))
       (inst call (ea (make-fixup nil :code-object
                                  (+ (ash code-constants-offset word-shift)
                                     (* 4 sb-vm:n-word-bytes)
                                     (- other-pointer-lowtag)))
                      rip-tn))
       ,@(gpr-save/restore 'push except)
       ,@body
       ,@(gpr-save/restore 'pop except)
       ;; KLUDGE: index of FPR-RESTORE is 6
       ;; (inst call (ea (make-fixup 'fpr-restore :assembly-routine*)))
       (inst call (ea (make-fixup nil :code-object
                                  (+ (ash code-constants-offset word-shift)
                                     (* 6 sb-vm:n-word-bytes)
                                     (- other-pointer-lowtag)))
                      rip-tn))
       (inst mov rsp-tn rbp-tn)
       (inst pop rbp-tn)))))
