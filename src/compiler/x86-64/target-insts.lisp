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

(in-package "SB!X86-64-ASM")

;;; Return the operand size depending on the prefixes and width bit as
;;; stored in DSTATE.
(defun inst-operand-size (dstate)
  (declare (type disassem-state dstate))
  (cond ((dstate-getprop dstate +operand-size-8+) :byte)
        ((dstate-getprop dstate +rex-w+) :qword)
        ((dstate-getprop dstate +operand-size-16+) :word)
        (t +default-operand-size+)))

;;; The same as INST-OPERAND-SIZE, but for those instructions (e.g.
;;; PUSH, JMP) that have a default operand size of :qword. It can only
;;; be overwritten to :word.
(defun inst-operand-size-default-qword (dstate)
  (declare (type disassem-state dstate))
  (if (dstate-getprop dstate +operand-size-16+) :word :qword))

;;; This prefilter is used solely for its side effect, namely to put
;;; the property OPERAND-SIZE-8 into the DSTATE if VALUE is 0.
(defun prefilter-width (dstate value)
  (declare (type bit value) (type disassem-state dstate))
  (when (zerop value)
    (dstate-setprop dstate +operand-size-8+))
  value)

;;; A register field that can be extended by REX.R.
;;; As noted in 'insts', this prefilter (and others) could return a REG.
(defun prefilter-reg-r (dstate value)
  (declare (type (mod 8) value) (type disassem-state dstate))
  (if (dstate-getprop dstate +rex-r+) (+ value 8) value))

;;; A register field that can be extended by REX.B.
(defun prefilter-reg-b (dstate value)
  (declare (type (mod 8) value) (type disassem-state dstate))
  (if (dstate-getprop dstate +rex-b+) (+ value 8) value))

;; This reader extracts the 'imm' operand in "MOV reg,imm" format.
;; KLUDGE: the REG instruction format can not define a reader
;; because it has no field specification and no prefilter.
;; (It's specified directly in the MOV instruction definition)
(defun reg-imm-data (dchunk dstate) dchunk
  (aref (sb!disassem::dstate-filtered-values dstate) 4))

(defun regrm-inst-reg (dchunk dstate)
  (logior (if (logtest (sb!disassem::dstate-inst-properties dstate) +rex-r+) 8 0)
          (!regrm-inst-reg dchunk dstate)))
(defun ext-regrm-inst-reg (dchunk dstate)
  (logior (if (logtest (sb!disassem::dstate-inst-properties dstate) +rex-r+) 8 0)
          (!ext-regrm-inst-reg dchunk dstate)))

(defstruct (machine-ea (:include sb!disassem::filtered-arg)
                       (:copier nil)
                       (:constructor %make-machine-ea))
  base disp index scale)

;;; Print to STREAM the name of the general-purpose register encoded by
;;; VALUE and of size WIDTH. For robustness, the high byte registers
;;; (AH, BH, CH, DH) are correctly detected, too, although the compiler
;;; does not use them.
(defun print-reg-with-width (value width stream dstate)
  (declare (type full-reg value)
           (type stream stream)
           (type disassem-state dstate))
  (princ (if (and (eq width :byte)
                  (<= 4 value 7)
                  (not (dstate-getprop dstate +rex+)))
             (aref #("AH" "CH" "DH" "BH") (- value 4))
             (aref (ecase width
                     (:byte sb!vm::+byte-register-names+)
                     (:word sb!vm::+word-register-names+)
                     (:dword sb!vm::+dword-register-names+)
                     (:qword sb!vm::+qword-register-names+))
                   value))
         stream)
  ;; XXX plus should do some source-var notes
  )

(defun print-reg (value stream dstate)
  (print-reg-with-width value
                        (inst-operand-size dstate)
                        stream
                        dstate))

(defun print-reg-default-qword (value stream dstate)
  (print-reg-with-width value
                        (inst-operand-size-default-qword dstate)
                        stream
                        dstate))

;; Print a reg that can only be a :DWORD or :QWORD.
;; Avoid use of INST-OPERAND-SIZE because it's wrong for this type of operand.
(defun print-d/q-word-reg (value stream dstate)
  (print-reg-with-width value
                        (if (dstate-getprop dstate +rex-w+) :qword :dword)
                        stream
                        dstate))

(defun print-byte-reg (value stream dstate)
  (print-reg-with-width value :byte stream dstate))

(defun print-addr-reg (value stream dstate)
  (print-reg-with-width value +default-address-size+ stream dstate))

;;; Print a register or a memory reference of the given WIDTH.
;;; If SIZED-P is true, add an explicit size indicator for memory
;;; references.
(defun print-reg/mem-with-width (value width sized-p stream dstate)
  (declare (type (or machine-ea full-reg) value)
           (type (member :byte :word :dword :qword) width)
           (type boolean sized-p))
  (if (typep value 'full-reg)
      (print-reg-with-width value width stream dstate)
      (print-mem-ref (if sized-p :sized-ref :ref) value width stream dstate)))

;;; Print a register or a memory reference. The width is determined by
;;; calling INST-OPERAND-SIZE.
(defun print-reg/mem (value stream dstate)
  (print-reg/mem-with-width
   value (inst-operand-size dstate) nil stream dstate))

;; Same as print-reg/mem, but prints an explicit size indicator for
;; memory references.
(defun print-sized-reg/mem (value stream dstate)
  (print-reg/mem-with-width
   value (inst-operand-size dstate) t stream dstate))

;;; Same as print-sized-reg/mem, but with a default operand size of
;;; :qword.
(defun print-sized-reg/mem-default-qword (value stream dstate)
  (print-reg/mem-with-width
   value (inst-operand-size-default-qword dstate) t stream dstate))

(defun print-jmp-ea (value stream dstate)
  (print-sized-reg/mem-default-qword value stream dstate))

(defun print-sized-byte-reg/mem (value stream dstate)
  (print-reg/mem-with-width value :byte t stream dstate))

(defun print-sized-word-reg/mem (value stream dstate)
  (print-reg/mem-with-width value :word t stream dstate))

(defun print-sized-dword-reg/mem (value stream dstate)
  (print-reg/mem-with-width value :dword t stream dstate))

(defun print-label (value stream dstate)
  (declare (ignore dstate))
  (princ16 value stream))

(defun print-xmmreg (value stream dstate)
  (declare (type xmmreg value) (type stream stream) (ignore dstate))
  (format stream "XMM~d" value))

(defun print-xmmreg/mem (value stream dstate)
  (if (typep value 'xmmreg)
      (print-xmmreg value stream dstate)
      (print-mem-ref :ref value nil stream dstate)))

(defun print-imm/asm-routine (value stream dstate)
  (if (or #!+immobile-space (maybe-note-lisp-callee value dstate)
          (maybe-note-assembler-routine value nil dstate)
          (maybe-note-static-symbol value dstate))
      (princ16 value stream)
      (princ value stream)))

;;; Return either a MACHINE-EA or a register (a fixnum).
;;; MOD and R/M are the extracted bits from the instruction's ModRM byte.
;;; Depending on MOD and R/M, a SIB byte and/or displacement may be read.
;;; The REX.B and REX.X from dstate are appropriately consumed.
(defun prefilter-reg/mem (dstate mod r/m)
  (declare (type disassem-state dstate)
           (type (unsigned-byte 2) mod)
           (type (unsigned-byte 3) r/m))
  (flet ((make-machine-ea (base &optional disp index scale)
           (let ((ea (the machine-ea
                       (sb!disassem::new-filtered-arg dstate #'%make-machine-ea))))
             (setf (machine-ea-base ea) base
                   (machine-ea-disp ea) disp
                   (machine-ea-index ea) index
                   (machine-ea-scale ea) scale)
             ea))
         (displacement ()
           (case mod
             (#b01 (read-signed-suffix 8 dstate))
             (#b10 (read-signed-suffix 32 dstate))))
         (extend (bit-name reg)
           (logior (if (dstate-getprop dstate bit-name) 8 0) reg)))
    (declare (inline extend))
    (let ((full-reg (extend +rex-b+ r/m)))
      (cond ((= mod #b11) full-reg) ; register direct mode
            ((= r/m #b100) ; SIB byte - rex.b is "don't care"
             (let* ((sib (the (unsigned-byte 8) (read-suffix 8 dstate)))
                    (index-reg (extend +rex-x+ (ldb (byte 3 3) sib)))
                    (base-reg (ldb (byte 3 0) sib)))
               ;; mod=0 and base=RBP means no base reg
               (make-machine-ea (unless (and (= mod #b00) (= base-reg #b101))
                                  (extend +rex-b+ base-reg))
                                (cond ((/= mod #b00) (displacement))
                                      ((= base-reg #b101) (read-signed-suffix 32 dstate)))
                                (unless (= index-reg #b100) index-reg) ; index can't be RSP
                                (ash 1 (ldb (byte 2 6) sib)))))
            ((/= mod #b00) (make-machine-ea full-reg (displacement)))
            ;; rex.b is not decoded in determining RIP-relative mode
            ((= r/m #b101) (make-machine-ea :rip (read-signed-suffix 32 dstate)))
            (t (make-machine-ea full-reg))))))

#!+sb-thread
(defun static-symbol-from-tls-index (index)
  (dovector (sym +static-symbols+)
    (when (= (symbol-tls-index sym) index)
      (return sym))))

;;; Return contents of memory if either it refers to an unboxed code constant
;;; or is RIP-relative with a displacement of 0.
(defun unboxed-constant-ref (dstate segment-offset addr disp)
  ;; FIXME: why in one case do we read via SEGMENT-SAP and the other via ADDR?
  (cond ((< segment-offset
            (sb!disassem::seg-initial-raw-bytes (dstate-segment dstate)))
         (sap-ref-word (dstate-segment-sap dstate) segment-offset))
        ((eql disp 0)
            ;; Assume this is safe to read, since we're disassembling
            ;; from the memory just a few bytes preceding 'addr'.
         (sap-ref-word (int-sap addr) 0))))

(define-load-time-global thread-slot-names
    (let* ((slots (primitive-object-slots
                   (find 'sb!vm::thread *primitive-objects*
                         :key #'primitive-object-name)))
           (a (make-array (1+ (slot-offset (car (last slots))))
                          :initial-element nil)))
      (dolist (slot slots a)
        (setf (aref a (slot-offset slot)) (slot-name slot)))))

;;; Prints a memory reference to STREAM. VALUE is a list of
;;; (BASE-REG OFFSET INDEX-REG INDEX-SCALE), where any component may be
;;; missing or nil to indicate that it's not used or has the obvious
;;; default value (e.g., 1 for the index-scale). BASE-REG can be the
;;; symbol RIP or a full register, INDEX-REG a full register. If WIDTH
;;; is non-nil it should be one of the symbols :BYTE, :WORD, :DWORD or
;;; :QWORD; a corresponding size indicator is printed if MODE is :SIZED-REF.
;;; The rationale for supplying WIDTH while eliding a pointer-size qualifier
;;; is that proper dereferencing of RIP-relative constants requires a size,
;;; but in other cases would only add clutter, since a source/destination
;;; register implies a size.
;;;
(defun print-mem-ref (mode value width stream dstate)
  ;; :COMPUTE is used for the LEA instruction - it informs this function
  ;; that we're not loading from the address and that the contents should not
  ;; be printed. It'll usually be a reference to code within the disasembly
  ;; segment, as LEA is employed to compute the entry point for local call.
  (declare (type (member :ref :sized-ref :compute) mode)
           (type machine-ea value)
           (type (member nil :byte :word :dword :qword) width)
           (type stream stream)
           (type disassem-state dstate))
  (let ((base-reg (machine-ea-base value))
        (disp (machine-ea-disp value))
        (index-reg (machine-ea-index value))
        (firstp t))
    (when (and width (eq mode :sized-ref))
      (princ width stream)
      (princ '| PTR | stream))
    (when (dstate-getprop dstate +fs-segment+)
      (princ "FS:" stream))
    (write-char #\[ stream)
    (when base-reg
      (if (eql :rip base-reg)
          (princ base-reg stream)
          (print-addr-reg base-reg stream dstate))
      (setq firstp nil))
    (when index-reg
      (unless firstp (write-char #\+ stream))
      (print-addr-reg index-reg stream dstate)
      (let ((scale (machine-ea-scale value)))
        (unless (= scale 1)
          (write-char #\* stream)
          (princ scale stream)))
      (setq firstp nil))
    (when (and disp (or firstp (not (zerop disp))))
      (unless (or firstp (minusp disp))
        (write-char #\+ stream))
      (cond ((eq (machine-ea-base value) :rip)
             (princ disp stream))
            (firstp
               (princ16 disp stream)
               (or (minusp disp)
                   (nth-value 1 (note-code-constant-absolute disp dstate))
                   (maybe-note-assembler-routine disp nil dstate)
                   ;; Static symbols coming from CELL-REF
                   (maybe-note-static-symbol (+ disp (- other-pointer-lowtag
                                                        n-word-bytes))
                                             dstate)))
            (t
             (princ disp stream))))
    (write-char #\] stream)
    (when (and (eq (machine-ea-base value) :rip) (neq mode :compute))
      ;; Always try to print the EA as a note
      (let ((addr (+ disp (dstate-next-addr dstate))))
        ;; The origin is zero when disassembling into a trace-file.
        ;; Don't crash on account of it.
        (when (plusp addr)
          (or (nth-value
               1 (note-code-constant-absolute addr dstate width))
              (maybe-note-assembler-routine addr nil dstate)
              ;; Show the absolute address and maybe the contents.
              (note (format nil "[#x~x]~@[ = #x~x~]"
                            addr
                            (case width
                              (:qword
                               (unboxed-constant-ref
                                dstate
                                (+ (dstate-next-offs dstate) disp)
                                addr disp))))
                    dstate)))))
    #!+sb-thread
    (flet ((guess-symbol (predicate)
             (binding* ((code-header (seg-code (dstate-segment dstate)) :exit-if-null)
                        (header-n-words (code-header-words code-header)))
               (loop for word-num from code-constants-offset below header-n-words
                     for obj = (code-header-ref code-header word-num)
                     when (and (symbolp obj) (funcall predicate obj))
                     do (return obj)))))
      ;; Try to reverse-engineer which thread-local binding this is
      (cond ((and disp ; Test whether disp looks aligned to an object header
                  (not (logtest (- disp 4) sb!vm:lowtag-mask))
                  (not base-reg) (not index-reg))
             (let* ((addr (+ disp (- 4) sb!vm:other-pointer-lowtag))
                    (symbol
                     (guess-symbol (lambda (s) (= (get-lisp-obj-address s) addr)))))
               (when symbol
                 (note (lambda (stream) (format stream "tls_index: ~S" symbol))
                       dstate))))
            ((and (eql base-reg #.(ash (tn-offset sb!vm::thread-base-tn) -1))
                  (not (dstate-getprop dstate +fs-segment+)) ; not system TLS
                  (not index-reg) ; no index
                  (typep disp '(integer 0 *)) ; positive displacement
                  (zerop (logand disp 7))) ; lispword-aligned
             (let ((index (ash disp -3)))
               (when (< index (length thread-slot-names))
                 (awhen (aref thread-slot-names index)
                   (return-from print-mem-ref
                     (note (lambda (stream) (format stream "thread.~(~A~)" it))
                           dstate)))))
             (let* ((tls-index (ash disp (- n-fixnum-tag-bits)))
                    (symbol (or (guess-symbol
                                 (lambda (s) (= (symbol-tls-index s) tls-index)))
                                ;; static symbols aren't in the code header
                                (static-symbol-from-tls-index tls-index))))
               (when symbol
                 (return-from print-mem-ref
                   (note (lambda (stream) (format stream "tls: ~S" symbol))
                         dstate)))))))))

(defun lea-compute-label (value dstate)
  ;; If VALUE should be regarded as a label, return the address.
  ;; If not, just return VALUE.
  (if (and (typep value 'machine-ea) (eq (machine-ea-base value) :rip))
      (+ (dstate-next-addr dstate) (machine-ea-disp value))
      value))

;; Figure out whether LEA should print its EA with just the stuff in brackets,
;; or additionally show the EA as either a label or a hex literal.
(defun lea-print-ea (value stream dstate)
  (let ((width (inst-operand-size dstate))
        (addr nil)
        (fmt "= #x~x"))
    (etypecase value
      (machine-ea
       ;; Indicate to PRINT-MEM-REF that this is not a memory access.
       (print-mem-ref :compute value width stream dstate)
       (when (eq (machine-ea-base value) :rip)
         (setq addr (+ (dstate-next-addr dstate) (machine-ea-disp value)))))

      ;; We're robust in allowing VALUE to be an integer (a register),
      ;; though LEA Rx,Ry is an illegal instruction.
      ;; Test this before INTEGER since the types overlap.
      (full-reg
       (print-reg-with-width value width stream dstate))

      ((or string integer)
       ;; A label for the EA should not print as itself, but as the decomposed
       ;; addressing mode so that [ADDR] and [RIP+disp] are unmistakable.
       ;; We can see an INTEGER here because LEA-COMPUTE-LABEL is always called
       ;; on the operand to LEA, and it will compute an absolute address based
       ;; off RIP when possible. If :use-labels NIL was specified, there is
       ;; no hashtable of address to string, so we get the address.
       ;; But ordinarily we get the string. Either way, the r/m arg reveals the
       ;; EA calculation. DCHUNK-ZERO is a meaningless value - any would do -
       ;; because the EA was computed in a prefilter.
       (print-mem-ref :compute (regrm-inst-r/m dchunk-zero dstate)
                      width stream dstate)
       (setq addr value)
       (when (stringp value) (setq fmt "= ~A"))))
    (when addr
      (unless (maybe-note-assembler-routine addr nil dstate)
        (note (lambda (s) (format s fmt addr)) dstate)))))

;;;; interrupt instructions

(defun break-control (chunk inst stream dstate)
  (declare (ignore inst))
  (flet ((nt (x) (if stream (note x dstate))))
    (let ((trap #!-ud2-breakpoints (byte-imm-code chunk dstate)
           #!+ud2-breakpoints (word-imm-code chunk dstate)))
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

(defun sb!c::convert-alloc-point-fixups (code locs)
  ;; Find the instruction which jumps over the profiling code,
  ;; and record the offset, and not the instruction that makes the call
  ;; to enable the counter. The instructions preceding the call comprise
  ;; a test, jmp, and long nop. Luckily a long nop encoding never
  ;; has the byte #xEB in it, so just scan backwards looking for that.
  (pack-code-fixup-locs
   (mapcar (lambda (loc)
             (loop (cond ((zerop (decf loc))
                          (bug "Failed to find allocation point"))
                         ((eql (sap-ref-8 (code-instructions code) loc) #xEB)
                          (return loc)))))
           locs)
   nil))
