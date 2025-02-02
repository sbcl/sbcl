;;;; the machine-specific support routines needed by the file assembler

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(defun invoke-asm-routine (name reg &key tail)
  (cond ((or (not (boundp '*component-being-compiled*))
             (code-immobile-p *component-being-compiled*))
         (if tail
             (inst b (make-fixup name :assembly-routine))
             (inst bl (make-fixup name :assembly-routine))))
        (t
         (load-inline-constant reg `(:fixup ,name :assembly-routine))
         (if tail
             (inst br reg)
             (inst blr reg)))))

(defun load-asm-routine (reg name)
  (if (or (not (boundp '*component-being-compiled*))
          (code-immobile-p *component-being-compiled*))
      (inst adr reg (make-fixup name :assembly-routine))
      (load-inline-constant reg `(:fixup ,name :assembly-routine))))

(defun invoke-foreign-routine (name reg &key tail)
  (cond ((or (not (boundp '*component-being-compiled*))
             (code-immobile-p *component-being-compiled*))
         (if tail
             (inst b (make-fixup name :foreign))
             (inst bl (make-fixup name :foreign))))
        (t
         (load-inline-constant reg `(:fixup ,name :foreign))
         (if tail
             (inst br reg)
             (inst blr reg)))))

(defun load-foreign-symbol (reg name &key dataref)
  (let ((kind (if dataref :foreign-dataref :foreign)))
    (if (or (not (boundp '*component-being-compiled*))
            (code-immobile-p *component-being-compiled*))
        (if dataref
            (inst ldr reg (make-fixup name kind))
            (inst adr reg (make-fixup name kind)))
        (progn
          (load-inline-constant reg `(:fixup ,name ,kind))
          (when dataref
            (loadw reg reg))))))

(defun generate-call-sequence (name style vop options)
  (declare (ignore options vop))
  (ecase style
    ((:none :raw :full-call-no-return)
     (let ((lr (gensym)))
       (values
        `((progn
            ,lr
            ,@(if (eq style :none)
                  `((invoke-asm-routine ',name tmp-tn :tail t))
                  `((invoke-asm-routine ',name ,lr)))))
        `((:temporary (:sc non-descriptor-reg :from (:eval 0) :to (:eval 1) :offset lr-offset)
                      ,lr)))))))

(defun generate-return-sequence (style)
  (ecase style
    (:raw
     `((inst ret)))
    ((:none :full-call-no-return))))
