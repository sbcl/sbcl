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

(in-package "SB!VM")

;;; Prints a memory reference to STREAM. VALUE is a list of
;;; (BASE-REG OFFSET INDEX-REG INDEX-SCALE), where any component may be
;;; missing or nil to indicate that it's not used or has the obvious
;;; default value (e.g., 1 for the index-scale). BASE-REG can be the
;;; symbol RIP or a full register, INDEX-REG a full register. If WIDTH
;;; is non-nil it should be one of the symbols :BYTE, :WORD, :DWORD or
;;; :QWORD and a corresponding size indicator is printed first.
(defun print-mem-access (value width stream dstate)
  (declare (type list value)
           (type (member nil :byte :word :dword :qword) width)
           (type stream stream)
           (type sb!disassem:disassem-state dstate))
  (when width
    (princ width stream)
    (princ '| PTR | stream))
  (write-char #\[ stream)
  (let ((firstp t) (rip-p nil))
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
        (cond ((eql 'rip base-reg)
               (setf rip-p t)
               (princ base-reg stream))
              (t
               (print-addr-reg base-reg stream dstate))))
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
          (cond
            (rip-p
             (princ offset stream)
             (let ((addr (+ offset (sb!disassem:dstate-next-addr dstate))))
               (when (plusp addr)
                 (or (nth-value 1
                                (sb!disassem::note-code-constant-absolute
                                 addr dstate))
                     (sb!disassem:maybe-note-assembler-routine addr
                                                               nil
                                                               dstate)))))
            (firstp
             (progn
               (sb!disassem:princ16 offset stream)
               (or (minusp offset)
                   (nth-value 1
                              (sb!disassem::note-code-constant-absolute offset dstate))
                   (sb!disassem:maybe-note-assembler-routine offset
                                                             nil
                                                             dstate))))
            (t
             (princ offset stream)))))))
  (write-char #\] stream))
