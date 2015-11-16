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

;;; This type is used mostly in disassembly and represents legacy
;;; registers only. R8-R15 are handled separately.
(deftype reg () '(unsigned-byte 3))

;;; This includes legacy registers and R8-R15.
(deftype full-reg () '(unsigned-byte 4))

;;; The XMM registers XMM0 - XMM15.
(deftype xmmreg () '(unsigned-byte 4))

(defparameter *byte-reg-names*
  #(al cl dl bl spl bpl sil dil r8b r9b r10b r11b r12b r13b r14b r15b))
(defparameter *high-byte-reg-names*
  #(ah ch dh bh))
(defparameter *word-reg-names*
  #(ax cx dx bx sp bp si di r8w r9w r10w r11w r12w r13w r14w r15w))
(defparameter *dword-reg-names*
  #(eax ecx edx ebx esp ebp esi edi r8d r9d r10d r11d r12d r13d r14d r15d))
(defparameter *qword-reg-names*
  #(rax rcx rdx rbx rsp rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15))

;;; This prefilter is used solely for its side effects, namely to put
;;; the bits found in the REX prefix into the DSTATE for use by other
;;; prefilters and by printers.
(defun prefilter-wrxb (value dstate)
  (declare (type (unsigned-byte 4) value)
           (type sb!disassem:disassem-state dstate))
  (sb!disassem:dstate-put-inst-prop dstate 'rex)
  (when (plusp (logand value #b1000))
    (sb!disassem:dstate-put-inst-prop dstate 'rex-w))
  (when (plusp (logand value #b0100))
    (sb!disassem:dstate-put-inst-prop dstate 'rex-r))
  (when (plusp (logand value #b0010))
    (sb!disassem:dstate-put-inst-prop dstate 'rex-x))
  (when (plusp (logand value #b0001))
    (sb!disassem:dstate-put-inst-prop dstate 'rex-b))
  value)

;;; The two following prefilters are used instead of prefilter-wrxb when
;;; the bits of the REX prefix need to be treated individually. They are
;;; always used together, so only the first one sets the REX property.
(defun prefilter-rex-w (value dstate)
  (declare (type bit value)
           (type sb!disassem:disassem-state dstate))
  (sb!disassem:dstate-put-inst-prop dstate 'rex)
  (when (plusp value)
    (sb!disassem:dstate-put-inst-prop dstate 'rex-w)))
(defun prefilter-rex-b (value dstate)
  (declare (type bit value)
           (type sb!disassem:disassem-state dstate))
  (when (plusp value)
    (sb!disassem:dstate-put-inst-prop dstate 'rex-b)))

;;; This prefilter is used solely for its side effect, namely to put
;;; the property OPERAND-SIZE-8 into the DSTATE if VALUE is 0.
(defun prefilter-width (value dstate)
  (declare (type bit value)
           (type sb!disassem:disassem-state dstate))
  (when (zerop value)
    (sb!disassem:dstate-put-inst-prop dstate 'operand-size-8))
  value)

;;; This prefilter is used solely for its side effect, namely to put
;;; the property OPERAND-SIZE-16 into the DSTATE.
(defun prefilter-x66 (value dstate)
  (declare (type (eql #x66) value)
           (ignore value)
           (type sb!disassem:disassem-state dstate))
  (sb!disassem:dstate-put-inst-prop dstate 'operand-size-16))

;;; A register field that can be extended by REX.R.
(defun prefilter-reg-r (value dstate)
  (declare (type reg value)
           (type sb!disassem:disassem-state dstate))
  (if (sb!disassem::dstate-get-inst-prop dstate 'rex-r)
      (+ value 8)
      value))

;;; A register field that can be extended by REX.B.
(defun prefilter-reg-b (value dstate)
  (declare (type reg value)
           (type sb!disassem:disassem-state dstate))
  (if (sb!disassem::dstate-get-inst-prop dstate 'rex-b)
      (+ value 8)
      value))

;;; Returns either an integer, meaning a register, or a list of
;;; (BASE-REG OFFSET INDEX-REG INDEX-SCALE), where any component
;;; may be missing or nil to indicate that it's not used or has the
;;; obvious default value (e.g., 1 for the index-scale). VALUE is a list
;;; of the mod and r/m field of the ModRM byte of the instruction.
;;; Depending on VALUE a SIB byte and/or an offset may be read. The
;;; REX.B bit from DSTATE is used to extend the sole register or the
;;; BASE-REG to a full register, the REX.X bit does the same for the
;;; INDEX-REG.
(defun prefilter-reg/mem (value dstate)
  (declare (type list value)
           (type sb!disassem:disassem-state dstate))
  (flet ((extend (bit-name reg)
           (logior (if (sb!disassem:dstate-get-inst-prop dstate bit-name) 8 0)
                   reg)))
    (declare (inline extend))
    (let* ((mod (the (unsigned-byte 2) (first value)))
           (r/m (the (unsigned-byte 3) (second value)))
           (full-reg (extend 'rex-b r/m)))
      (cond ((= mod #b11)
             ;; registers
             full-reg)
            ((= r/m #b100) ; SIB byte - rex.b is "don't care"
             (let* ((sib (the (unsigned-byte 8)
                              (sb!disassem:read-suffix 8 dstate)))
                    (base-reg (ldb (byte 3 0) sib))
                    (index-reg (extend 'rex-x (ldb (byte 3 3) sib)))
                    (offset
                         (case mod
                               (#b00
                                (if (= base-reg #b101)
                                    (sb!disassem:read-signed-suffix 32 dstate)
                                  nil))
                               (#b01
                                (sb!disassem:read-signed-suffix 8 dstate))
                               (#b10
                                (sb!disassem:read-signed-suffix 32 dstate)))))
               (list (unless (and (= mod #b00) (= base-reg #b101))
                       (extend 'rex-b base-reg))
                     offset
                     (unless (= index-reg #b100) index-reg) ; index can't be RSP
                     (ash 1 (ldb (byte 2 6) sib)))))
            ;; rex.b is not decoded in determining RIP-relative mode
            ((and (= mod #b00) (= r/m #b101))
             (list 'rip (sb!disassem:read-signed-suffix 32 dstate)))
            ((= mod #b00)
             (list full-reg))
            ((= mod #b01)
             (list full-reg (sb!disassem:read-signed-suffix 8 dstate)))
            (t                            ; (= mod #b10)
             (list full-reg (sb!disassem:read-signed-suffix 32 dstate)))))))

(defun read-address (value dstate)
  (declare (ignore value))              ; always nil anyway
  (sb!disassem:read-suffix (width-bits (inst-operand-size dstate)) dstate))

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
  ;; that the address is not a memory reference below which is confined
  ;; the disassembly - the heuristic for detecting the start of unboxed data.
  ;; LEA is sometimes used to compute the start of a local function for
  ;; allocate-closures, and it points to valid instructions, not data.
  (declare (type (member :ref :sized-ref :compute) mode)
           (type list value)
           (type (member nil :byte :word :dword :qword) width)
           (type stream stream)
           (type sb!disassem:disassem-state dstate))
  (when (and width (eq mode :sized-ref))
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
             (unless (eq mode :compute)
               (let ((addr (+ offset (sb!disassem:dstate-next-addr dstate))))
                 (when (plusp addr) ; FIXME: what does this test achieve?
                    (let ((hook (sb!disassem:dstate-get-prop
                                 dstate :rip-relative-mem-ref-hook)))
                      (when hook
                        (funcall hook offset width)))
                    (or (nth-value
                         1 (sb!disassem::note-code-constant-absolute
                            addr dstate width))
                        (sb!disassem:maybe-note-assembler-routine
                         addr nil dstate)
                        ;; Show the absolute address and maybe the contents.
                        (sb!disassem:note
                         (format nil "[#x~x]~@[ = ~x~]"
                                 addr
                                 (case width
                                  (:qword (unboxed-constant-ref
                                           dstate
                                           (+ (sb!disassem:dstate-next-offs
                                               dstate) offset)))))
                         dstate))))))
            (firstp
               (sb!disassem:princ16 offset stream)
               (or (minusp offset)
                   (nth-value 1
                              (sb!disassem::note-code-constant-absolute offset dstate))
                   (sb!disassem:maybe-note-assembler-routine offset
                                                             nil
                                                             dstate)))
            (t
             (princ offset stream)))))))
  (write-char #\] stream)
  #!+sb-thread
  (let ((disp (second value)))
    (when (and (eql (first value) #.(ash (tn-offset thread-base-tn) -1))
               (not (third value)) ; no index
               (typep disp '(integer 0 *)) ; positive displacement
               (sb!disassem::seg-code (sb!disassem:dstate-segment dstate)))
      ;; Try to reverse-engineer which thread-local binding this is
      (let* ((code (sb!disassem::seg-code (sb!disassem:dstate-segment dstate)))
             (header-n-words
              (ash (sap-ref-word (int-sap (get-lisp-obj-address code))
                                 (- other-pointer-lowtag)) -8))
             (tls-index (ash disp (- n-fixnum-tag-bits))))
        (loop for word-num from code-constants-offset below header-n-words
              for obj = (code-header-ref code word-num)
              when (and (symbolp obj) (= (symbol-tls-index obj) tls-index))
              do (return-from print-mem-ref
                   (sb!disassem:note
                    (lambda (stream) (format stream "tls: ~S" obj))
                    dstate))))
      ;; Or maybe we're looking at the 'struct thread' itself
      (when (< disp max-interrupts)
        (let* ((thread-slots (primitive-object-slots
                              (find 'thread *primitive-objects*
                                    :key #'primitive-object-name)))
               (slot (find (ash disp (- word-shift)) thread-slots
                           :key #'slot-offset)))
          (when slot
            (return-from print-mem-ref
              (sb!disassem:note
               (lambda (stream)
                 (format stream "thread.~(~A~)" (slot-name slot)))
               dstate))))))))

(defun unboxed-constant-ref (dstate segment-offset)
  (let* ((seg (sb!disassem:dstate-segment dstate))
         (code-offset
          (sb!disassem::segment-offs-to-code-offs segment-offset seg))
         (unboxed-range (sb!disassem::seg-unboxed-data-range seg)))
    (and unboxed-range
         (<= (car unboxed-range) code-offset (cdr unboxed-range))
         (sb!disassem::sap-ref-int
          (sb!disassem:dstate-segment-sap dstate)
          segment-offset
          n-word-bytes
          (sb!disassem::dstate-byte-order dstate)))))

;;; Print a register or a memory reference of the given WIDTH.
;;; If SIZED-P is true, add an explicit size indicator for memory
;;; references.
(defun print-reg/mem-with-width (value width sized-p stream dstate)
  (declare (type (or list full-reg) value)
           (type (member :byte :word :dword :qword) width)
           (type boolean sized-p)
           (type stream stream)
           (type sb!disassem:disassem-state dstate))
  (if (typep value 'full-reg)
      (print-reg-with-width value width stream dstate)
      (print-mem-ref (if sized-p :sized-ref :ref) value width stream dstate)))

;;; Print a register or a memory reference. The width is determined by
;;; calling INST-OPERAND-SIZE.
(defun print-reg/mem (value stream dstate)
  (declare (type (or list full-reg) value)
           (type stream stream)
           (type sb!disassem:disassem-state dstate))
  (print-reg/mem-with-width
   value (inst-operand-size dstate) nil stream dstate))

;; Same as print-reg/mem, but prints an explicit size indicator for
;; memory references.
(defun print-sized-reg/mem (value stream dstate)
  (declare (type (or list full-reg) value)
           (type stream stream)
           (type sb!disassem:disassem-state dstate))
  (print-reg/mem-with-width
   value (inst-operand-size dstate) t stream dstate))

;;; Same as print-sized-reg/mem, but with a default operand size of
;;; :qword.
(defun print-sized-reg/mem-default-qword (value stream dstate)
  (declare (type (or list full-reg) value)
           (type stream stream)
           (type sb!disassem:disassem-state dstate))
  (print-reg/mem-with-width
   value (inst-operand-size-default-qword dstate) t stream dstate))

;; FIXME: the design of the disassembler is so incredibly stupid
;; that it requires compile-time references to functions to
;; compare them for equality in the cache. THIS MAKES NO SENSE!
(eval-when (:compile-toplevel :load-toplevel :execute)
(defun print-sized-byte-reg/mem (value stream dstate)
  (declare (type (or list full-reg) value)
           (type stream stream)
           (type sb!disassem:disassem-state dstate))
  (print-reg/mem-with-width value :byte t stream dstate))

(defun print-sized-word-reg/mem (value stream dstate)
  (declare (type (or list full-reg) value)
           (type stream stream)
           (type sb!disassem:disassem-state dstate))
  (print-reg/mem-with-width value :word t stream dstate))

(defun print-sized-dword-reg/mem (value stream dstate)
  (declare (type (or list full-reg) value)
           (type stream stream)
           (type sb!disassem:disassem-state dstate))
  (print-reg/mem-with-width value :dword t stream dstate))

(defun print-xmmreg (value stream dstate)
  (declare (type xmmreg value)
           (type stream stream)
           (ignore dstate))
  (format stream "XMM~d" value))

(defun print-xmmreg/mem (value stream dstate)
  (declare (type (or list xmmreg) value)
           (type stream stream)
           (type sb!disassem:disassem-state dstate))
  (if (typep value 'xmmreg)
      (print-xmmreg value stream dstate)
      (print-mem-ref :ref value nil stream dstate)))

(defun print-reg (value stream dstate)
  (declare (type full-reg value)
           (type stream stream)
           (type sb!disassem:disassem-state dstate))
  (print-reg-with-width value
                        (inst-operand-size dstate)
                        stream
                        dstate))

(defun print-reg-default-qword (value stream dstate)
  (declare (type full-reg value)
           (type stream stream)
           (type sb!disassem:disassem-state dstate))
  (print-reg-with-width value
                        (inst-operand-size-default-qword dstate)
                        stream
                        dstate))

(defun print-label (value stream dstate)
  (declare (ignore dstate))
  (sb!disassem:princ16 value stream))

(defun print-imm/asm-routine (value stream dstate)
  (sb!disassem:maybe-note-assembler-routine value nil dstate)
  (sb!disassem:maybe-note-static-symbol value dstate)
  (princ value stream))
)

;;; Print to STREAM the name of the general-purpose register encoded by
;;; VALUE and of size WIDTH. For robustness, the high byte registers
;;; (AH, BH, CH, DH) are correctly detected, too, although the compiler
;;; does not use them.
(defun print-reg-with-width (value width stream dstate)
  (declare (type full-reg value)
           (type stream stream)
           (type sb!disassem:disassem-state dstate))
  (princ (if (and (eq width :byte)
                  (<= 4 value 7)
                  (not (sb!disassem:dstate-get-inst-prop dstate 'rex)))
             (aref *high-byte-reg-names* (- value 4))
             (aref (ecase width
                     (:byte *byte-reg-names*)
                     (:word *word-reg-names*)
                     (:dword *dword-reg-names*)
                     (:qword *qword-reg-names*))
                   value))
         stream)
  ;; XXX plus should do some source-var notes
  )

(defun print-byte-reg (value stream dstate)
  (declare (type full-reg value)
           (type stream stream)
           (type sb!disassem:disassem-state dstate))
  (print-reg-with-width value :byte stream dstate))

(defun print-addr-reg (value stream dstate)
  (declare (type full-reg value)
           (type stream stream)
           (type sb!disassem:disassem-state dstate))
  (print-reg-with-width value +default-address-size+ stream dstate))

;; If the filtered VALUE (R/M field of LEA) should be treated as a label,
;; return the virtual address, otherwise the value unchanged.
(defun lea-compute-label (value dstate)
  (if (and (listp value) (eq (first value) 'rip))
      (+ (sb!disassem:dstate-next-addr dstate) (second value))
      value))

;; Figure out whether LEA should print its EA with just the stuff in brackets,
;; or additionally show the EA as either a label or a hex literal.
(defun lea-print-ea (value stream dstate)
  (let ((width (inst-operand-size dstate)))
    (etypecase value
      (list
       ;; Indicate to PRINT-MEM-REF that this is not a memory access.
       (print-mem-ref :compute value width stream dstate)
       (when (eq (first value) 'rip)
         (let ((addr (+ (sb!disassem:dstate-next-addr dstate) (second value))))
           (sb!disassem:note (lambda (s) (format s "= #x~x" addr))
                             dstate))))

      ;; We're robust in allowing VALUE to be an integer (a register),
      ;; though LEA Rx,Ry is an illegal instruction.
      ;; A label should never have memory address of 0 to 15 so this case is
      ;; unambiguous for the most part, except maybe in a compiler trace file
      ;; which starts disassembling as if the origin were zero.
      (full-reg (print-reg-with-width value width stream dstate))

      ;; Unfortunately the "label" case sees either an integer or string,
      ;; because MAP-SEGMENT-INSTRUCTIONS happens twice (really thrice).
      ;;  - DETERMINE-OPCODE-BOUNDS in target-insts. Label = integer from prefilter.
      ;;  - ADD-SEGMENT-LABELS. Never calls instruction printers.
      ;;  - DISASSEMBLE-SEGMENT. Label = string
      ;; and we need a different 'arg-form-kind' than the one in VALUE,
      ;; because :USE-LABEL forces the printing pass to see only a label string.
      ;; Unlike for JMP and CALL, this isn't reasonable, as no one instruction
      ;; corresponds to, say, "LEA RAX,L1". We want [RIP+disp] or [mem_absolute]
      ;; so extract the filtered not-labelized value for PRINT-MEM-REF.
      ((or string integer)
       (print-mem-ref :compute
                      (reg-r/m-inst-r/m-arg sb!disassem::dchunk-zero dstate)
                      width stream dstate)
       (when (stringp value) ; Don't note anything during -OPCODE-BOUNDS pass
         (sb!disassem:note (lambda (s) (format s "= ~A" value)) dstate))))))

(defun snarf-error-junk (sap offset &optional length-only)
  (let* ((length (sap-ref-8 sap offset))
         (vector (make-array length :element-type '(unsigned-byte 8))))
    (declare (type system-area-pointer sap)
             (type (unsigned-byte 8) length)
             (type (simple-array (unsigned-byte 8) (*)) vector))
    (cond (length-only
           (values 0 (1+ length) nil nil))
          (t
           (copy-ub8-from-system-area sap (1+ offset) vector 0 length)
           (collect ((sc-offsets)
                     (lengths))
             (lengths 1)                ; the length byte
             (let* ((index 0)
                    (error-number (read-var-integer vector index)))
               (lengths index)
               (loop
                 (when (>= index length)
                   (return))
                 (let ((old-index index))
                   (sc-offsets (read-var-integer vector index))
                   (lengths (- index old-index))))
               (values error-number
                       (1+ length)
                       (sc-offsets)
                       (lengths))))))))

(defun break-control (chunk inst stream dstate)
  (declare (ignore inst))
  (flet ((nt (x) (if stream (sb!disassem:note x dstate))))
    (case #!-ud2-breakpoints (byte-imm-code chunk dstate)
          #!+ud2-breakpoints (word-imm-code chunk dstate)
      (#.error-trap
       (nt "error trap")
       (sb!disassem:handle-break-args #'snarf-error-junk stream dstate))
      (#.cerror-trap
       (nt "cerror trap")
       (sb!disassem:handle-break-args #'snarf-error-junk stream dstate))
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
       (nt "Invalid argument count trap")))))
