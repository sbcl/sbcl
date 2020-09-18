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

(defun invoke-asm-routine (inst routine vop)
  (declare (ignorable vop))
  (let ((fixup
         (cond ((sb-c::code-immobile-p vop)
                (make-fixup routine :assembly-routine))
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

(defmacro with-registers-preserved ((convention fpr-size &key except) &body body)
  ;: Convention:
  ;;   C    = save GPRs that C call can change
  ;;   Lisp = save GPRs that lisp call can change
  (multiple-value-bind (mnemonic fpr-align getter)
      (ecase fpr-size
        (xmm (values 'movaps 16 'sb-x86-64-asm::get-fpr))
        (ymm (values 'vmovaps 32 'sb-x86-64-asm::get-avx2)))
    (flet ((fpr-save/restore (operation)
             (loop for regno below 16
                   collect
                   (ecase operation
                     (push
                      `(inst ,mnemonic (ea ,(* regno fpr-align) rsp-tn) (,getter ,regno)))
                     (pop
                      `(inst ,mnemonic (,getter ,regno) (ea ,(* regno fpr-align) rsp-tn))))))
           (gpr-save/restore (operation except)
             (declare (type (member push pop) operation))
             (let ((registers (ecase convention
                               (c '#1=(rax-tn rcx-tn rdx-tn rsi-tn rdi-tn r8-tn r9-tn r10-tn r11-tn))
                               (lisp '(rbx-tn r12-tn r14-tn r15-tn . #1#)))))
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
       ,@(fpr-save/restore 'push)
       ,@(gpr-save/restore 'push except)
       ,@body
       ,@(gpr-save/restore 'pop except)
       ,@(fpr-save/restore 'pop)
       (inst mov rsp-tn rbp-tn)
       (inst pop rbp-tn)))))
