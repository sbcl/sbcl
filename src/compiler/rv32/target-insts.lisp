;;;; This file is for stuff which was in CMU CL's insts.lisp
;;;; file, but which in the SBCL build process can't be compiled
;;;; into code for the cross-compilation host.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-RV32-ASM")

(defvar *disassem-use-lisp-reg-names* t
  "If non-NIL, print registers using the Lisp register names.
Otherwise, use the RISC-V register names")

(defconstant-eqx lisp-reg-symbols
  (map 'vector
       (lambda (name)
         (and name (make-symbol (concatenate 'string "$" name))))
       sb-vm::*register-names*)
  #'equalp)

(defconstant-eqx riscv-reg-symbols
  (coerce
   (loop for n from 0 to 31 collect (make-symbol (format nil "x~d" n)))
   'vector)
  #'equalp)

(defun print-reg (value stream dstate)
  (declare (stream stream) (fixnum value))
  (let ((regname (aref (if *disassem-use-lisp-reg-names*
                           lisp-reg-symbols
                           riscv-reg-symbols)
                       value)))
    (princ regname stream)
    (maybe-note-associated-storage-ref
     value 'registers regname dstate)))

(defconstant-eqx float-reg-symbols
  (coerce
   (loop for n from 0 to 31 collect (make-symbol (format nil "ft~d" n)))
   'vector)
  #'equalp)

(defun print-fp-reg (value stream dstate)
  (declare (stream stream) (fixnum value))
  (let ((regname (aref float-reg-symbols value)))
    (princ regname stream)
    (maybe-note-associated-storage-ref
     value 'float-registers regname dstate)))

(defun reconstruct-s-immediate (value)
  (coerce-signed (dpb (first value) (byte 7 5) (second value)) 12))

(defun print-s-imm (value stream dstate)
  (declare (stream stream) (ignore dstate))
  (princ (reconstruct-s-immediate value) stream))

(defun use-b-label (value dstate)
  (declare (type disassem-state dstate))
  (let* ((value (coerce-signed
                 (logior (ash (first value) 12)
                         (ash (second value) 11)
                         (ash (third value) 5)
                         (ash (fourth value) 1))
                 12))
         (address (+ value (dstate-cur-addr dstate))))
    ;; LRA pointer
    (if (= (logand address lowtag-mask) other-pointer-lowtag)
        (- address (- other-pointer-lowtag n-word-bytes))
        address)))

(defun use-j-label (value dstate)
  (declare (type disassem-state dstate))
  (let* ((value (coerce-signed
                 (logior (ash (first value) 20)
                         (ash (second value) 12)
                         (ash (third value) 11)
                         (ash (fourth value) 1))
                 20))
         (address (+ value (dstate-cur-addr dstate))))
    ;; LRA pointer
    (if (= (logand address lowtag-mask) other-pointer-lowtag)
        (- address (- other-pointer-lowtag n-word-bytes))
        address)))

(defun print-float-fmt (value stream dstate)
  (declare (ignore dstate)
           (stream stream)
           (type (unsigned-byte 3) value))
  (princ (case value
           (#b00 's)
           (#b01 'd)
           (#b10 'q)
           (t '?))
         stream))

(defun print-float-rm (value stream dstate)
  (declare (ignore dstate)
           (stream stream)
           (type (unsigned-byte 3) value))
  (princ (aref #(rne rtz rdn rup rmm unused1 unused2 dynamic)
               value)
         stream))

(defun print-jump-target (value stream dstate)
  (let ((addr (ash value 2)))
    (maybe-note-assembler-routine addr t dstate)
    (write addr :base 16 :radix t :stream stream)))

(defun annotate-load-store (register offset dstate)
  (case register
    (#.sb-vm::code-offset
     (note-code-constant offset dstate))
    (#.sb-vm::null-offset
     (let ((offset (+ sb-vm::nil-value offset)))
       (maybe-note-assembler-routine offset nil dstate)
       (maybe-note-static-symbol (logior offset other-pointer-lowtag)
                                 dstate)))
    #+sb-thread
    (#.sb-vm::thread-offset
     (let* ((thread-slots
              (load-time-value
               (primitive-object-slots
                (find 'sb-vm::thread *primitive-objects*
                      :key #'primitive-object-name)) t))
            (slot (find (ash offset (- word-shift)) thread-slots
                        :key #'slot-offset)))
       (when slot
         (note (lambda (stream)
                 (format stream "thread.~(~A~)" (slot-name slot)))
               dstate))))))

(defun print-load-annotation (value stream dstate)
  (declare (ignore stream))
  (annotate-load-store (first value)
                       (coerce-signed (second value) 12)
                       dstate))

(defun print-store-annotation (value stream dstate)
  (declare (ignore stream))
  (annotate-load-store (first value)
                       (reconstruct-s-immediate (rest value))
                       dstate))

;;;; interrupt instructions

(defun break-control (chunk inst stream dstate)
  (declare (ignore inst))
  (flet ((nt (x) (if stream (note x dstate))))
    (let ((trap (trap-code chunk dstate)))
     (case trap
       (#.breakpoint-trap
        (nt "breakpoint trap"))
       (#.pending-interrupt-trap
        (nt "pending interrupt trap"))
       (#.halt-trap
        (nt "halt trap"))
       (#.fun-end-breakpoint-trap
        (nt "function end breakpoint trap"))
       (#.single-step-around-trap
        (nt "single-step trap (around)"))
       (#.single-step-before-trap
        (nt "single-step trap (before)"))
       (#.invalid-arg-count-trap
        (nt "Invalid argument count trap"))
       (#.cerror-trap
        (nt "cerror trap")
        (handle-break-args #'snarf-error-junk trap stream dstate))
       (t
        (handle-break-args #'snarf-error-junk trap stream dstate))))))
