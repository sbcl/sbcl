;;;; target-only stuff from CMU CL's src/compiler/x86/insts.lisp
;;;;
;;;; i.e. stuff which was in CMU CL's insts.lisp file, but which in
;;;; the SBCL build process can't be compiled into code for the
;;;; cross-compilation host

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!X86-ASM")

(defun print-mem-access (value stream print-size-p dstate)
  (declare (type list value)
           (type stream stream)
           (type (member t nil) print-size-p)
           (type disassem-state dstate))
  (when print-size-p
    (princ (inst-operand-size dstate) stream)
    (princ '| PTR | stream))
  (maybe-print-segment-override stream dstate)
  (write-char #\[ stream)
  (let ((firstp t))
    (macrolet ((pel ((var val) &body body)
                 ;; Print an element of the address, maybe with
                 ;; a leading separator.
                 `(let ((,var ,val))
                    (when ,var
                      (unless firstp
                        (write-char #\+ stream))
                      ,@body
                      (setq firstp nil)))))
      (pel (base-reg (first value))
        (print-addr-reg base-reg stream dstate))
      (pel (index-reg (third value))
        (print-addr-reg index-reg stream dstate)
        (let ((index-scale (fourth value)))
          (when (and index-scale (not (= index-scale 1)))
            (write-char #\* stream)
            (princ index-scale stream))))
      (let ((offset (second value)))
        (when (and offset (or firstp (not (zerop offset))))
          (unless (or firstp (minusp offset))
            (write-char #\+ stream))
          (if firstp
            (progn
              (princ16 offset stream)
              (or (minusp offset)
                  (nth-value 1 (note-code-constant-absolute offset dstate))
                  (maybe-note-assembler-routine offset nil dstate)))
            (princ offset stream))))))
  (write-char #\] stream))
