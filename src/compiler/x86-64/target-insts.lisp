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
(defun prefilter-reg-r (dstate value)
  (declare (type reg value) (type disassem-state dstate))
  (if (dstate-getprop dstate +rex-r+) (+ value 8) value))

;;; A register field that can be extended by REX.B.
(defun prefilter-reg-b (dstate value)
  (declare (type reg value) (type disassem-state dstate))
  (if (dstate-getprop dstate +rex-b+) (+ value 8) value))

(defstruct (machine-ea (:include sb!disassem::filtered-arg)
                       (:copier nil)
                       (:predicate nil)
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
  (or  (and (eql disp 0)
            ;; Assume this is safe to read, since we're disassembling
            ;; from the memory just a few bytes preceding 'addr'.
            (sap-ref-word (int-sap addr) 0))
       (let* ((seg (dstate-segment dstate))
              (code-offset
               (sb!disassem::segment-offs-to-code-offs segment-offset seg))
              (unboxed-range (sb!disassem::seg-unboxed-data-range seg)))
         (or (and unboxed-range
                  (<= (car unboxed-range) code-offset (cdr unboxed-range))
                  (sap-ref-word (dstate-segment-sap dstate) segment-offset))))))

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
                   ;; Static symbols coming frorm CELL-REF
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
                  (not index-reg) ; no index
                  (typep disp '(integer 0 *))) ; positive displacement
             (let* ((tls-index (ash disp (- n-fixnum-tag-bits)))
                    (symbol (or (guess-symbol
                                 (lambda (s) (= (symbol-tls-index s) tls-index)))
                                ;; static symbols aren't in the code header
                                (static-symbol-from-tls-index tls-index))))
               (when symbol
                 (return-from print-mem-ref
                   (note (lambda (stream) (format stream "tls: ~S" symbol))
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
                   (note (lambda (stream)
                           (format stream "thread.~(~A~)" (slot-name slot)))
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
       (print-mem-ref :compute (reg-r/m-inst-r/m-arg dchunk-zero dstate)
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

;;;;

#!+immobile-code
(progn
(defun sb!vm::collect-immobile-code-relocs ()
  (let ((code-components
         (make-array 20000 :element-type '(unsigned-byte 32)
                           :fill-pointer 0 :adjustable t))
        (relocs
         (make-array 100000 :element-type '(unsigned-byte 32)
                            :fill-pointer 0 :adjustable t))
        ;; Look for these three instruction formats.
        (lea-inst (find-inst #x8D (get-inst-space)))
        (jmp-inst (find-inst #b11101001 (get-inst-space)))
        (call-inst (find-inst #b11101000 (get-inst-space)))
        (seg (sb!disassem::%make-segment
              :sap-maker (lambda () (error "Bad sap maker")) :virtual-location 0))
        (dstate (make-dstate nil)))
    (flet ((scan-function (fun-entry-addr fun-end-addr predicate)
             (setf (seg-virtual-location seg) fun-entry-addr
                   (seg-length seg) (- fun-end-addr fun-entry-addr)
                   (seg-sap-maker seg)
                   (let ((sap (int-sap fun-entry-addr))) (lambda () sap)))
             (map-segment-instructions
              (lambda (dchunk inst)
                (cond ((and (or (eq inst jmp-inst) (eq inst call-inst))
                            (funcall predicate
                                     (+ (near-jump-displacement dchunk dstate)
                                        (dstate-next-addr dstate))))
                       (vector-push-extend (dstate-cur-addr dstate) relocs))
                      ((eq inst lea-inst)
                       (let ((sap (funcall (seg-sap-maker seg))))
                         (aver (eql (sap-ref-8 sap (dstate-cur-offs dstate)) #x8D))
                         (let ((modrm (sap-ref-8 sap (1+ (dstate-cur-offs dstate)))))
                           (when (and (= (logand modrm #b11000111) #b00000101) ; RIP-relative mode
                                      (funcall predicate
                                               (+ (signed-sap-ref-32
                                                   sap (+ (dstate-cur-offs dstate) 2))
                                                  (dstate-next-addr dstate))))
                             (aver (eql (logand (sap-ref-8 sap (1- (dstate-cur-offs dstate))) #xF0)
                                        #x40)) ; expect a REX prefix
                             (vector-push-extend (dstate-cur-addr dstate) relocs)))))))
              seg dstate nil))
           (finish-component (code start-relocs-index)
             (when (> (fill-pointer relocs) start-relocs-index)
               (vector-push-extend (get-lisp-obj-address code) code-components)
               (vector-push-extend start-relocs-index code-components))))

      ;; Assembler routines contain jumps to immobile code.
      (let* ((code sb!fasl:*assembler-routines*)
             (origin (sap-int (code-instructions code)))
             (relocs-index (fill-pointer relocs)))
        (dolist (range (sort (loop for range being each hash-value
                                   of (car (%code-debug-info code)) collect range)
                             #'< :key #'car))
          ;; byte range is inclusive bound on both ends
          (scan-function (+ origin (car range))
                         (+ origin (cdr range) 1) #'immobile-space-addr-p))
        (finish-component code relocs-index))

      ;; Immobile space - code components can jump to immobile space,
      ;; read-only space, and C runtime routines.
      (sb!vm::map-allocated-objects
       (lambda (code type size)
         (declare (ignore size))
         (when (= type code-header-widetag)
           (let* ((text-origin (sap-int (code-instructions code)))
                  (text-end (+ text-origin (%code-code-size code)))
                  (relocs-index (fill-pointer relocs)))
             (dotimes (i (code-n-entries code) (finish-component code relocs-index))
               (let ((fun (%code-entry-point code i)))
                 (scan-function
                  (+ (get-lisp-obj-address fun) (- fun-pointer-lowtag)
                     (ash simple-fun-code-offset word-shift))
                  (if (< (1+ i) (code-n-entries code))
                      (- (get-lisp-obj-address (%code-entry-point code (1+ i)))
                         fun-pointer-lowtag)
                      text-end)
                ;; Exclude transfers within this code component
                  (lambda (jmp-targ-addr)
                    (not (<= text-origin jmp-targ-addr text-end)))))))))
       :immobile))

    ;; Write a delimiter into the array passed to C
    (vector-push-extend 0 code-components)
    (vector-push-extend (fill-pointer relocs) code-components)
    (values code-components relocs)))

(defmacro do-immobile-functions ((code-var fun-var addr-var &key (if t)) &body body)
  ;; Loop over all code objects
  `(let* ((call (find-inst #xE8 (get-inst-space)))
          (jmp  (find-inst #xE9 (get-inst-space)))
          (dstate (make-dstate nil))
          (sap (int-sap 0))
          (seg (sb!disassem::%make-segment :sap-maker (lambda () sap))))
     (sb!vm::map-objects-in-range
      (lambda (,code-var obj-type obj-size)
        (declare (ignore obj-size))
        (when (and (= obj-type code-header-widetag) ,if)
          ;; Loop over all embedded functions
          (dotimes (fun-index (code-n-entries ,code-var))
            (let* ((,fun-var (%code-entry-point ,code-var fun-index))
                   (,addr-var (+ (get-lisp-obj-address ,fun-var)
                                 (- fun-pointer-lowtag)
                                 (ash simple-fun-code-offset word-shift))))
              (with-pinned-objects (sap) ; Mutate SAP to point to fun
                (setf (sap-ref-word (int-sap (get-lisp-obj-address sap))
                                    (- n-word-bytes other-pointer-lowtag))
                      ,addr-var))
              (setf (seg-virtual-location seg) ,addr-var
                    (seg-length seg)
                    (- (let ((next (%code-entry-point ,code-var (1+ fun-index))))
                         (if next
                             (- (get-lisp-obj-address next) fun-pointer-lowtag)
                             (+ (sap-int (code-instructions ,code-var))
                                (%code-code-size ,code-var))))
                       ,addr-var))
              ,@body))))
      ;; Slowness here is bothersome, especially for SB!VM::REMOVE-STATIC-LINKS,
      ;; so skip right over all fixedobj pages.
      (ash varyobj-space-start (- n-fixnum-tag-bits))
      (%make-lisp-obj (sap-int *varyobj-space-free-pointer*)))))

(defun sb!vm::statically-link-core (&key callers exclude-callers
                                         callees exclude-callees
                                         verbose)
  (flet ((match-p (name include exclude)
           (and (not (member name exclude :test 'equal))
                (or (not include) (member name include :test 'equal)))))
    (do-immobile-functions (code fun addr)
      (when (match-p (%simple-fun-name fun) callers exclude-callers)
        (let ((printed-fun-name nil))
          ;; Loop over function's assembly code
          (map-segment-instructions
           (lambda (chunk inst)
             (when (or (eq inst jmp) (eq inst call))
               (let ((fdefn (sb!vm::find-called-object
                             (+ (near-jump-displacement chunk dstate)
                                (dstate-next-addr dstate)))))
                 (when (and (fdefn-p fdefn)
                            (let ((callee (fdefn-fun fdefn)))
                              (and (immobile-space-obj-p callee)
                                   (not (sb!vm::fun-requires-simplifying-trampoline-p callee))
                                   (match-p (%fun-name callee)
                                            callees exclude-callees))))
                   (let ((entry (sb!vm::fdefn-call-target fdefn)))
                     (when verbose
                       (let ((*print-pretty* nil))
                         (unless printed-fun-name
                           (format t "#x~X ~S~%" (get-lisp-obj-address fun) fun)
                           (setq printed-fun-name t))
                         (format t "  @~x -> ~s [~x]~%"
                                 (dstate-cur-addr dstate) (fdefn-name fdefn) entry)))
                     ;; Set the statically-linked flag
                     (setf (sb!vm::fdefn-has-static-callers fdefn) 1)
                     ;; Change the machine instruction
                     (setf (signed-sap-ref-32 (int-sap (dstate-cur-addr dstate)) 1)
                           (- entry (dstate-next-addr dstate))))))))
           seg dstate))))))

;;; While concurrent use of un-statically-link is unlikely, misuse could easily
;;; cause heap corruption. It's preventable by ensuring that this is atomic
;;; with respect to GC and other code attempting to change the same fdefn.
;;; The issue is that if the fdefn loses the pointer to the underlying code
;;; via (setf fdefn-fun) before we were done removing the static links,
;;; then there could be no remaining pointers visible to GC.
;;; The only way to detect the current set of references is to find uses of the
;;; current jump address, which means we need to fix them *all* before anyone
;;; else gets an opportunity to change the fdefn-fun of this same fdefn again.
(defglobal *static-linker-lock* (sb!thread:make-mutex :name "static linker"))
(defun sb!vm::remove-static-links (fdefn)
  ; (warn "undoing static linkage of ~S" (fdefn-name fdefn))
  (sb!thread::with-system-mutex (*static-linker-lock* :without-gcing t)
    ;; If the jump is to FUN-ENTRY, change it back to FDEFN-ENTRY
    (let ((fun-entry (sb!vm::fdefn-call-target fdefn))
          (fdefn-entry (+ (get-lisp-obj-address fdefn)
                          (ash fdefn-raw-addr-slot word-shift)
                          (- other-pointer-lowtag))))
      ;; Examine only those code components which potentially use FDEFN.
      (do-immobile-functions
         (code fun addr :if (loop for i downfrom (1- (sb!kernel:code-header-words code))
                                  to sb!vm:code-constants-offset
                                  thereis (eq (sb!kernel:code-header-ref code i)
                                              fdefn)))
        (map-segment-instructions
         (lambda (chunk inst)
           (when (or (eq inst jmp) (eq inst call))
             ;; TRULY-THE because near-jump-displacement isn't a known fun.
             (let ((disp (truly-the (signed-byte 32)
                                    (near-jump-displacement chunk dstate))))
               (when (= fun-entry (+ disp (dstate-next-addr dstate)))
                 (let ((new-disp
                        (the (signed-byte 32)
                             (- fdefn-entry (dstate-next-addr dstate)))))
                   ;; CMPXCHG is atomic even when misaligned, and x86-64 promises
                   ;; that self-modifying code works correctly, so the fetcher
                   ;; should never see a torn write.
                   (%primitive sb!vm::signed-sap-cas-32
                               (int-sap (dstate-cur-addr dstate))
                               1 disp new-disp))))))
         seg dstate)))
    (setf (sb!vm::fdefn-has-static-callers fdefn) 0))) ; Clear static link flag
) ; end PROGN
