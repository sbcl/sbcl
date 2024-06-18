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

(in-package "SB-SPARC-ASM")

(defmacro seen-sethi (dstate)
  `(sb-disassem::dstate-known-register-contents ,dstate))

(defun sethi-arg-printer (value stream dstate)
    (format stream "%hi(#x~8,'0x)" (ash value 10))
    ;; Save the immediate value and the destination register from this
    ;; sethi instruction.  This is used later to print some possible
    ;; notes about the value loaded by sethi.
    (let* ((word (sap-ref-int (dstate-segment-sap dstate)
                              (dstate-cur-offs dstate) n-word-bytes
                              (dstate-byte-order dstate)))
           (imm22 (ldb (byte 22 0) word))
           (rd (ldb (byte 5 25) word)))
      (push (cons rd imm22) (seen-sethi dstate))))

;; Look at the current instruction and see if we can't add some notes
;; about what's happening.

(defun maybe-add-notes (reg dstate)
  ;; FIXME: these accessors should all be defined using the :READER option
  ;; of DEFINE-INSTRUCTION-FORMAT.
  (let* ((word (sap-ref-int (dstate-segment-sap dstate)
                            (dstate-cur-offs dstate) n-word-bytes
                            (dstate-byte-order dstate)))
         (format (ldb (byte 2 30) word))
         (op3 (ldb (byte 6 19) word))
         (rs1 (ldb (byte 5 14) word))
         (rd (ldb (byte 5 25) word))
         (immed-p (not (zerop (ldb (byte 1 13) word))))
         (immed-val (sign-extend-immed-value (ldb (byte 13 0) word))))
    (declare (ignore immed-p))
    ;; Only the value of format and rd are guaranteed to be correct
    ;; because the disassembler is trying to print out the value of a
    ;; register.  The other values may not be right.
    (case format
      (2
       (case op3
         (#b000000
          (when (= reg rs1)
            (handle-add-inst rs1 immed-val rd dstate)))
         (#b111000
          (when (= reg rs1)
            (handle-jmpl-inst rs1 immed-val rd dstate)))
         (#b010001
          (when (= reg rs1)
            (handle-andcc-inst rs1 immed-val rd dstate)))))
      (3
       (case op3
         ((#b000000 #b000100)
          (when (= reg rs1)
            (handle-ld/st-inst rs1 immed-val rd dstate))))))
    ;; If this is not a SETHI instruction, and RD is the same as some
    ;; register used by SETHI, we delete the entry.  (In case we have
    ;; a SETHI without any additional instruction because the low bits
    ;; were zero.)
    (unless (and (zerop format) (= #b100 (ldb (byte 3 22) word)))
      (let ((sethi (assoc rd (seen-sethi dstate))))
        (when sethi
          (setf (seen-sethi dstate) (delete sethi (seen-sethi dstate))))))))

(defun handle-add-inst (rs1 immed-val rd dstate)
  (let* ((sethi (assoc rs1 (seen-sethi dstate))))
    (cond
      (sethi
       ;; RS1 was used in a SETHI instruction.  Assume that
       ;; this is the offset part of the SETHI instruction for
       ;; a full 32-bit address of something.  Make a note
       ;; about this usage as a Lisp assembly routine or
       ;; foreign routine, if possible.  If not, just note the
       ;; final value.
       (let ((addr (+ immed-val (ash (cdr sethi) 10))))
         (or (note-code-constant addr dstate :absolute)
             (maybe-note-assembler-routine addr t dstate)
             (note (format nil "~A = #x~8,'0X" (get-reg-name rd) addr) dstate)))
       (setf (seen-sethi dstate) (delete sethi (seen-sethi dstate))))
      ((= rs1 null-offset)
       ;; We have an ADD %NULL, <n>, RD instruction.  This is a
       ;; reference to a static symbol.
       (maybe-note-nil-indexed-object immed-val dstate))
      #+nil ((and (= rs1 zero-offset) *pseudo-atomic-set*)
       ;; "ADD %ZERO, num, RD" inside a pseudo-atomic is very
       ;; likely loading up a header word.  Make a note to that
       ;; effect.
       (let ((type (second (assoc (logand immed-val #xff) header-word-type-alist)))
             (size (ldb (byte 24 8) immed-val)))
         (when type
           (note (format nil "Header word ~A, size ~D?" type size) dstate)))))))

(defun handle-jmpl-inst (rs1 immed-val rd dstate)
  (declare (ignore rd))
  (let ((sethi (assoc rs1 (seen-sethi dstate))))
    (when sethi
      ;; RS1 was used in a SETHI instruction.  Assume that
      ;; this is the offset part of the SETHI instruction for
      ;; a full 32-bit address of something.  Make a note
      ;; about this usage as a Lisp assembly routine or
      ;; foreign routine, if possible.  If not, just note the
      ;; final value.
      (let ((addr (+ immed-val (ash (cdr sethi) 10))))
        (maybe-note-assembler-routine addr t dstate)
        (setf (seen-sethi dstate) (delete sethi (seen-sethi dstate)))))))

(defun handle-ld/st-inst (rs1 immed-val rd dstate)
  (declare (ignore rd))
  ;; Got an LDUW/LD or STW instruction, with immediate offset.
  (case rs1
    (29
     ;; A reference to a code constant (reg = %CODE)
     (note-code-constant immed-val dstate))
    (2
     ;; A reference to a static symbol or static function (reg =
     ;; %NULL)
     (or (maybe-note-nil-indexed-symbol-slot-ref immed-val dstate)
         #+nil (sb-disassem::maybe-note-static-function immed-val dstate)))
    (t
     (let ((sethi (assoc rs1 (seen-sethi dstate))))
       (when sethi
         (let ((addr (+ immed-val (ash (cdr sethi) 10))))
           (maybe-note-assembler-routine addr nil dstate)
           (setf (seen-sethi dstate) (delete sethi (seen-sethi dstate)))))))))

(defun handle-andcc-inst (rs1 immed-val rd dstate)
  (declare (ignorable rs1 immed-val rd dstate))
  ;; ANDCC %ALLOC, 3, %ZERO instruction
  (when nil
    (note "pseudo-atomic interrupted?" dstate)))

(defun unimp-control (chunk inst stream dstate)
  (declare (ignore inst))
  (flet ((nt (x) (if stream (note x dstate))))
    (let ((trap (format-2-unimp-data chunk dstate)))
     (case trap
       (#.breakpoint-trap
        (nt "Breakpoint trap"))
       (#.pending-interrupt-trap
        (nt "Pending interrupt trap"))
       (#.halt-trap
        (nt "Halt trap"))
       (#.fun-end-breakpoint-trap
        (nt "Function end breakpoint trap"))
       (t
        (when (or (and (= trap cerror-trap) (progn (nt "cerror trap") t))
                  (>= trap error-trap))
          (handle-break-args #'snarf-error-junk trap stream dstate)))))))
