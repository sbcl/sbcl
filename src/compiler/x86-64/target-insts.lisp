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
                  (not (dstate-get-inst-prop dstate 'rex)))
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

(defun print-reg (value stream dstate)
  (declare (type full-reg value)
           (type stream stream)
           (type disassem-state dstate))
  (print-reg-with-width value
                        (inst-operand-size dstate)
                        stream
                        dstate))

(defun print-reg-default-qword (value stream dstate)
  (declare (type full-reg value)
           (type stream stream)
           (type disassem-state dstate))
  (print-reg-with-width value
                        (inst-operand-size-default-qword dstate)
                        stream
                        dstate))

;; Print a reg that can only be a :DWORD or :QWORD.
;; Avoid use of INST-OPERAND-SIZE because it's wrong for this type of operand.
(defun print-d/q-word-reg (value stream dstate)
  (declare (type full-reg value)
           (type stream stream)
           (type disassem-state dstate))
  (print-reg-with-width value
                        (if (dstate-get-inst-prop dstate 'rex-w) :qword :dword)
                        stream
                        dstate))

(defun print-byte-reg (value stream dstate)
  (declare (type full-reg value)
           (type stream stream)
           (type disassem-state dstate))
  (print-reg-with-width value :byte stream dstate))

(defun print-addr-reg (value stream dstate)
  (declare (type full-reg value)
           (type stream stream)
           (type disassem-state dstate))
  (print-reg-with-width value +default-address-size+ stream dstate))

;;; Print a register or a memory reference of the given WIDTH.
;;; If SIZED-P is true, add an explicit size indicator for memory
;;; references.
(defun print-reg/mem-with-width (value width sized-p stream dstate)
  (declare (type (or list full-reg) value)
           (type (member :byte :word :dword :qword) width)
           (type boolean sized-p)
           (type stream stream)
           (type disassem-state dstate))
  (if (typep value 'full-reg)
      (print-reg-with-width value width stream dstate)
      (print-mem-ref (if sized-p :sized-ref :ref) value width stream dstate)))

;;; Print a register or a memory reference. The width is determined by
;;; calling INST-OPERAND-SIZE.
(defun print-reg/mem (value stream dstate)
  (declare (type (or list full-reg) value)
           (type stream stream)
           (type disassem-state dstate))
  (print-reg/mem-with-width
   value (inst-operand-size dstate) nil stream dstate))

;; Same as print-reg/mem, but prints an explicit size indicator for
;; memory references.
(defun print-sized-reg/mem (value stream dstate)
  (declare (type (or list full-reg) value)
           (type stream stream)
           (type disassem-state dstate))
  (print-reg/mem-with-width
   value (inst-operand-size dstate) t stream dstate))

;;; Same as print-sized-reg/mem, but with a default operand size of
;;; :qword.
(defun print-sized-reg/mem-default-qword (value stream dstate)
  (declare (type (or list full-reg) value)
           (type stream stream)
           (type disassem-state dstate))
  (print-reg/mem-with-width
   value (inst-operand-size-default-qword dstate) t stream dstate))

(defun print-sized-byte-reg/mem (value stream dstate)
  (declare (type (or list full-reg) value)
           (type stream stream)
           (type disassem-state dstate))
  (print-reg/mem-with-width value :byte t stream dstate))

(defun print-sized-word-reg/mem (value stream dstate)
  (declare (type (or list full-reg) value)
           (type stream stream)
           (type disassem-state dstate))
  (print-reg/mem-with-width value :word t stream dstate))

(defun print-sized-dword-reg/mem (value stream dstate)
  (declare (type (or list full-reg) value)
           (type stream stream)
           (type disassem-state dstate))
  (print-reg/mem-with-width value :dword t stream dstate))

(defun print-label (value stream dstate)
  (declare (ignore dstate))
  (princ16 value stream))

(defun print-xmmreg (value stream dstate)
  (declare (type xmmreg value) (type stream stream) (ignore dstate))
  (format stream "XMM~d" value))

(defun print-xmmreg/mem (value stream dstate)
  (declare (type (or list xmmreg) value)
           (type stream stream)
           (type disassem-state dstate))
  (if (typep value 'xmmreg)
      (print-xmmreg value stream dstate)
      (print-mem-ref :ref value nil stream dstate)))

(defun print-imm/asm-routine (value stream dstate)
  (maybe-note-assembler-routine value nil dstate)
  (maybe-note-static-symbol value dstate)
  (princ value stream))

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
           (type disassem-state dstate))
  (when (and width (eq mode :sized-ref))
    (princ width stream)
    (princ '| PTR | stream))
  (write-char #\[ stream)
  (let ((firstp t) (rip-p nil))
    (macrolet ((pel ((var val) &body body)
                 ;; Print an element of the address, maybe with
                 ;; a leading separator.
                 `(let ((,var ,val))
                    ;; Compiler knows that FIRSTP is T in first call to PEL.
                    #-sb-xc-host
                    (declare (muffle-conditions code-deletion-note))
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
               (let ((addr (+ offset (dstate-next-addr dstate))))
                 ;; The origin is zero when disassembling into a trace-file.
                 ;; Don't crash on account of it.
                 (when (plusp addr)
                   (or (nth-value
                        1 (note-code-constant-absolute addr dstate width))
                       (maybe-note-assembler-routine addr nil dstate)
                       ;; Show the absolute address and maybe the contents.
                       (note (format nil "[#x~x]~@[ = ~x~]"
                                     addr
                                     (case width
                                       (:qword
                                        (unboxed-constant-ref
                                         dstate
                                         (+ (dstate-next-offs dstate) offset)))))
                             dstate))))))
            (firstp
               (princ16 offset stream)
               (or (minusp offset)
                   (nth-value 1 (note-code-constant-absolute offset dstate))
                   (maybe-note-assembler-routine offset nil dstate)))
            (t
             (princ offset stream)))))))
  (write-char #\] stream)
  #!+sb-thread
  (let ((disp (second value)))
    (when (and (eql (first value) #.(ash (tn-offset sb!vm::thread-base-tn) -1))
               (not (third value)) ; no index
               (typep disp '(integer 0 *)) ; positive displacement
               (seg-code (dstate-segment dstate)))
      ;; Try to reverse-engineer which thread-local binding this is
      (let* ((code (seg-code (dstate-segment dstate)))
             (header-n-words
              (ash (sap-ref-word (int-sap (get-lisp-obj-address code))
                                 (- other-pointer-lowtag)) -8))
             (tls-index (ash disp (- n-fixnum-tag-bits))))
        (loop for word-num from code-constants-offset below header-n-words
              for obj = (code-header-ref code word-num)
              when (and (symbolp obj) (= (symbol-tls-index obj) tls-index))
              do (return-from print-mem-ref
                   (note (lambda (stream) (format stream "tls: ~S" obj))
                         dstate))))
      ;; Or maybe we're looking at the 'struct thread' itself
      (when (< disp max-interrupts)
        (let* ((thread-slots
                (load-time-value
                 (primitive-object-slots
                  (find 'sb!vm::thread *primitive-objects*
                        :key #'primitive-object-name)) t))
               (slot (find (ash disp (- word-shift)) thread-slots
                           :key #'slot-offset)))
          (when slot
            (return-from print-mem-ref
              (note (lambda (stream)
                      (format stream "thread.~(~A~)" (slot-name slot)))
                    dstate))))))))

;; Figure out whether LEA should print its EA with just the stuff in brackets,
;; or additionally show the EA as either a label or a hex literal.
(defun lea-print-ea (value stream dstate)
  (let ((width (inst-operand-size dstate)))
    (etypecase value
      (list
       ;; Indicate to PRINT-MEM-REF that this is not a memory access.
       (print-mem-ref :compute value width stream dstate)
       (when (eq (first value) 'rip)
         (let ((addr (+ (dstate-next-addr dstate) (second value))))
           (note (lambda (s) (format s "= #x~x" addr)) dstate))))

      (string
       ;; A label for the EA should not print as itself, but as the decomposed
       ;; addressing mode so that [ADDR] and [RIP+disp] are unmistakable.
       (print-mem-ref :compute (reg-r/m-inst-r/m-arg dchunk-zero dstate)
                      width stream dstate)
       (note (lambda (s) (format s "= ~A" value)) dstate))

      ;; We're robust in allowing VALUE to be an integer (a register),
      ;; though LEA Rx,Ry is an illegal instruction.
      (full-reg
       (print-reg-with-width value width stream dstate)))))

(defun unboxed-constant-ref (dstate segment-offset)
  (let* ((seg (dstate-segment dstate))
         (code-offset
          (sb!disassem::segment-offs-to-code-offs segment-offset seg))
         (unboxed-range (sb!disassem::seg-unboxed-data-range seg)))
    (and unboxed-range
         (<= (car unboxed-range) code-offset (cdr unboxed-range))
         (sap-ref-int (dstate-segment-sap dstate)
                      segment-offset n-word-bytes
                      (dstate-byte-order dstate)))))

;;;; interrupt instructions

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
  (flet ((nt (x) (if stream (note x dstate))))
    (case #!-ud2-breakpoints (byte-imm-code chunk dstate)
          #!+ud2-breakpoints (word-imm-code chunk dstate)
      (#.error-trap
       (nt "error trap")
       (handle-break-args #'snarf-error-junk stream dstate))
      (#.cerror-trap
       (nt "cerror trap")
       (handle-break-args #'snarf-error-junk stream dstate))
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
