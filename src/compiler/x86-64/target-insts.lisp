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

(in-package "SB-X86-64-ASM")

(defun sb-disassem::pre-decode (chunk dstate)
  (let ((byte (ldb (byte 8 0) chunk)))
    (case byte
      ((#x64  ; FS:
        #x65  ; GS:
        #x66  ; operand size modifier
        #x67  ; address size modifier
        #xf0  ; LOCK
        #xf2  ; REPNE or SSE inst
        #xf3) ; REP or SSE inst
       ;; If the next byte is a REX prefix, then strip it out, recording the 'wrxb'
       ;; bits in the dstate, and return the chunk as if the REX byte were absent.
       (let ((next (ldb (byte 8 8) chunk)))
         (when (= (logand next #xf0) #x40)
           (dstate-setprop dstate (logior +rex+ (logand next #b1111)))
           (let ((new (logior byte (ash (ldb (byte 48 16) chunk) 8))))
             (return-from sb-disassem::pre-decode (values new 1))))))))
  (values chunk 0))

(defmethod print-object ((reg reg) stream)
  (if (or *print-escape* *print-readably*)
      ;; cross-compiled DEFMETHOD can't use call-next-method
      (default-structure-print reg stream *current-level-in-print*)
      (write-string (reg-name reg) stream)))

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
  (declare (type (mod 8) value) (type disassem-state dstate))
  ;; size is arbitrary here since the printer determines it
  (get-gpr :qword (if (dstate-getprop dstate +rex-r+) (+ value 8) value)))

;;; A register field that can be extended by REX.B.
(defun prefilter-reg-b (dstate value)
  (declare (type (mod 8) value) (type disassem-state dstate))
  ;; size is arbitrary here since the printer determines it
  (get-gpr :qword (if (dstate-getprop dstate +rex-b+) (+ value 8) value)))

;; This reader extracts the 'imm' operand in "MOV reg,imm" format.
;; KLUDGE: the REG instruction format can not define a reader
;; because it has no field specification and no prefilter.
;; (It's specified directly in the MOV instruction definition)
(defun reg-imm-data (dchunk dstate) dchunk
  (aref (sb-disassem::dstate-filtered-values dstate) 4))

;;; This structure is logically immutable, except for one problem:
;;; the disassembler recycles instances of it (re-uses the same
;;; one for each successive instruction). See DECODE-MOD-R/M.
(defstruct (machine-ea (:copier nil)
                       (:constructor %make-machine-ea))
  ;; possible TODO: base,index,scale could be packed thusly in 13 bits:
  ;;  2 bits for scale
  ;;  1 bit for base register non-NULL
  ;;  4 bits for base register number
  ;;  1 bit for base-register-is-RIP
  ;;  1 bit for index register non-NULL
  ;;  4 bits for index register number
  disp base index scale)
(declaim (freeze-type machine-ea))

(defun reg-num (reg) (reg-id-num (reg-id reg)))

;;; Print to STREAM the name of the general-purpose register encoded by
;;; VALUE and of size WIDTH.
(defun print-reg-with-width (value width stream dstate)
  (declare (type (or null stream) stream)
           (type disassem-state dstate))
  (let* ((num (etypecase value
               ((unsigned-byte 4) value)
               ;; Decode and re-encode, because the size is always
               ;; initially :qword.
               (reg (reg-num value))))
         (reg (get-gpr width
                       (if (and (eq width :byte)
                                (not (dstate-getprop dstate +rex+))
                                (<= 4 num 7))
                           (+ 16 -4 num) ; legacy high-byte register
                           num))))
    (if stream
        (princ (reg-name reg) stream)
        (operand reg dstate)))
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
  (declare (type (member :byte :word :dword :qword) width)
           (type boolean sized-p))
  (if (machine-ea-p value)
      (print-mem-ref (if sized-p :sized-ref :ref) value width stream dstate)
      (print-reg-with-width value width stream dstate)))

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
  (cond ((null stream) (operand value dstate))
        ((typep value 'machine-ea)
         (print-mem-ref :ref value :qword stream dstate)
         #+immobile-space
         (when (and (null (machine-ea-base value))
                    (null (machine-ea-index value)))
           (let* ((v (- (sb-vm::static-call-entrypoint-vector) other-pointer-lowtag))
                  (data (+ v (ash sb-vm:vector-data-offset sb-vm:word-shift)))
                  (end (+ data (ash (length +static-fdefns+) sb-vm:word-shift))))
             (when (<= data (machine-ea-disp value) (1- end))
               (let ((i (ash (- (machine-ea-disp value) data) (- sb-vm:word-shift))))
                 (note (lambda (stream) (prin1 (aref +static-fdefns+ i) stream)) dstate)
                 (return-from print-jmp-ea)))
             (let* ((v sb-fasl::*asm-routine-vector*)
                    (a (logandc2 (get-lisp-obj-address v) sb-vm:lowtag-mask)))
               (when (<= a (machine-ea-disp value) (1- (+ a (primitive-object-size v))))
                 (let ((target (sap-ref-word (int-sap (machine-ea-disp value)) 0)))
                   (maybe-note-assembler-routine target t dstate)))))))
        (t (write value :stream stream :escape nil))))

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
  (let* ((reg (get-fpr :xmm
                           ;; FIXME: why are we seeing a value from the GPR
                           ;; prefilter instead of XMM prefilter here sometimes?
                           (etypecase value
                             ((unsigned-byte 4) value)
                             (reg (reg-num value)))))
         (name (reg-name reg)))
    (if stream
        (write-string name stream)
        (operand name dstate))))

(defun print-xmmreg/mem (value stream dstate)
  (if (machine-ea-p value)
      (print-mem-ref :ref value nil stream dstate)
      (print-xmmreg value stream dstate)))

;;; Guess whether VALUE is an immobile-space symbol or code blob by looking
;;; at all code header constants. If it matches any constant, assume that it
;;; is a use of the constant.  This has false positives of course,
;;; as does MAYBE-NOTE-STATIC-SYMBOL and friends. Any random immediate value
;;; used in an unboxed context, such as an ADD instruction,
;;; can be wrongly construed as an address.
;;; Note that for symbols we can match either the tagged pointer to it
;;; OR the untagged address of the SYMBOL-VALUE slot.
;;;
;;; "static" in this usage implies "at a fixed address" - it could be
;;; in static space or immobile space.
;;;
;;; TODO: probably should take an &OPTIONAL for ALLOW-INTERIOR-PTR to
;;; reject false positives from instructions that don't access an object
;;; except through a tagged pointer.
(defun maybe-note-static-lispobj (value dstate &optional quote)
  (when (maybe-note-static-symbol value dstate)
    ;; Returning T prints VALUE using base 16
    ;; (see the SIGNED-IMM-DATA printer, PRINT-IMM/ASM-ROUTINE)
    ;; This should probably pass through the QUOTE option but it's not critical.
    (return-from maybe-note-static-lispobj t))
  (let ((code (seg-code (dstate-segment dstate)))
        (adjusted-val (logior (- value (ash sb-vm:symbol-value-slot sb-vm:word-shift))
                              sb-vm:other-pointer-lowtag))
        (found-const)
        (slot))
    (when code
      (loop for i downfrom (1- (code-header-words code)) to sb-vm:code-constants-offset
            for const = (code-header-ref code i)
            do (when (symbolp const)
                 (let ((addr (get-lisp-obj-address const)))
                   (cond ((eql addr value)
                          (return (setq found-const const)))
                         ((eql addr adjusted-val)
                          (return (setq found-const const
                                        slot sb-vm:symbol-value-slot)))))))
      (unless found-const ; try static symbol's value slots
        (dovector (symbol sb-vm:+static-symbols+)
          (when (= (get-lisp-obj-address symbol) adjusted-val)
            (return (setq found-const symbol
                          slot sb-vm:symbol-value-slot)))))
      (when found-const
        (note (cond (slot
                     (lambda (s) (format s "(SYMBOL-VALUE '~S)" found-const)))
                    ((and (symbolp found-const) quote)
                     (lambda (s) (write-char #\' s) (prin1 found-const s)))
                    (t
                     (lambda (s) (prin1 found-const s))))
              dstate)
        ;; Returning T prints in base 16 (see PRINT-IMM/ASM-ROUTINE)
        (return-from maybe-note-static-lispobj t))))
    #| This mysterious code seems to have no regression tests.
       Comenting it out until I can figure out why it was in target-disassem
    ;; Kludge: layout of STREAM, FILE-STREAM, and STRING-STREAM can be used
    ;; as immediate operands without a corresponding boxed header constant.
    ;; I think we always elide the boxed constant for builtin layouts,
    ;; but these three have some slightly unusual codegen that causes a PUSH
    ;; instruction to need some help to show its operand as a lisp object.
    (dolist (thing (load-time-value (list (find-layout 'stream)
                                          (find-layout 'file-stream)
                                          (find-layout 'string-stream))
                                    t))
      (when (eql (get-lisp-obj-address thing) address)
        (return-from found thing))))) |#
  (awhen (and (typep value 'word)
              (sb-disassem::find-code-constant-from-interior-pointer value dstate))
    (note (lambda (stream) (princ it stream)) dstate)))

;;; Return an instance of REG or MACHINE-EA.
;;; MOD and R/M are the extracted bits from the instruction's ModRM byte.
;;; Depending on MOD and R/M, a SIB byte and/or displacement may be read.
;;; The REX.B and REX.X from dstate are appropriately consumed.
(defun decode-mod-r/m (dstate mod r/m regclass)
  (declare (type disassem-state dstate)
           (type (unsigned-byte 2) mod)
           (type (unsigned-byte 3) r/m))
  (flet ((make-machine-ea (base &optional disp index scale)
           (let ((ea (the machine-ea
                       (sb-disassem::new-filtered-arg dstate #'%make-machine-ea))))
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
      (cond ((= mod #b11) ; register direct mode
             (case regclass
              (gpr (get-gpr :qword full-reg)) ; size is not really known here
              (fpr (get-fpr :xmm full-reg))))
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

(defun prefilter-reg/mem (dstate mod r/m)
  (decode-mod-r/m dstate mod r/m 'gpr))
(defun prefilter-xmmreg/mem (dstate mod r/m)
  (decode-mod-r/m dstate mod r/m 'fpr))

;;; Return contents of memory if either it refers to an unboxed code constant
;;; or is RIP-relative with a displacement of 0.
(defun unboxed-constant-ref (dstate addr disp)
  (when (and (minusp disp)
             (awhen (seg-code (dstate-segment dstate))
               (sb-disassem::points-to-code-constant-p addr it)))
    (sap-ref-word (int-sap addr) 0)))

(define-load-time-global thread-slot-names
    (let* ((slots (coerce (primitive-object-slots
                           (sb-vm::primitive-object 'sb-vm::thread))
                          'list))
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
(defun print-mem-ref (mode value width stream dstate &key (index-reg-printer #'print-addr-reg))
  ;; :COMPUTE is used for the LEA instruction - it informs this function
  ;; that we're not loading from the address and that the contents should not
  ;; be printed. It'll usually be a reference to code within the disasembly
  ;; segment, as LEA is employed to compute the entry point for local call.
  (declare (type (member :ref :sized-ref :compute) mode)
           (type machine-ea value)
           (type (member nil :byte :word :dword :qword) width)
           (type (or null stream) stream)
           (type disassem-state dstate))
  ;; If disassembling into the dstate, print nothing; just stash the operand.
  (unless stream
    (return-from print-mem-ref (operand (cons value width) dstate)))

  ;; Unpack and print the pieces of the machine EA.
  (let ((base-reg (machine-ea-base value))
        (disp (machine-ea-disp value))
        (index-reg (machine-ea-index value))
        (firstp t))
    (when (and width (eq mode :sized-ref))
      (princ width stream)
      (princ '| PTR | stream))
    (when (dstate-getprop dstate +fs-segment+)
      (princ "FS:" stream))
    (when (dstate-getprop dstate +gs-segment+)
      (princ "GS:" stream))
    (write-char #\[ stream)
    (when base-reg
      (if (eql :rip base-reg)
          (princ base-reg stream)
          (print-addr-reg base-reg stream dstate))
      (setq firstp nil))
    (when index-reg
      (unless firstp (write-char #\+ stream))
      (funcall index-reg-printer index-reg stream dstate)
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
             ;; Avoid the MAYBE-NOTE call if we can.  A negative offset is never an
             ;; absolute address as would be used for asm routines and static symbols.
             ;; FIRSTP implies lack of a base and index register.
             (unless (minusp disp)
               (or (maybe-note-static-symbol (+ disp (- other-pointer-lowtag
                                                        (* n-word-bytes sb-vm:symbol-value-slot)))
                                             dstate)
                   (maybe-note-assembler-routine disp nil dstate))))
            (t
             (princ disp stream))))
    (write-char #\] stream)

    ;; Always try to add an end-of-line comment about the EA.
    ;; Assembler routines were already handled above (not really sure why)
    ;; so now we have to figure out everything else.
    #+sb-safepoint
    (when (and (eql (machine-ea-base value) sb-vm::card-table-reg)
               (eql (machine-ea-disp value) -8))
      (return-from print-mem-ref (note "safepoint" dstate)))

    (when (and (eq (machine-ea-base value) :rip) (neq mode :compute))
      (block nil
       (binding* ((seg (dstate-segment dstate))
                  (code (seg-code seg) :exit-if-null)
                  (offs (sb-disassem::segment-offs-to-code-offs
                         (+ (dstate-next-offs dstate) disp) seg)))
           (when (note-code-constant offs dstate) (return)))
       (let ((addr (+ disp (dstate-next-addr dstate))))
         ;; The origin is zero when disassembling into a trace-file.
         ;; Don't crash on account of it.
         ;; Also, don't try to look up C symbols in immobile space.
         ;; In an elfinated core, the range that is reserved for
         ;; compilation to memory says it is all associated with
         ;; the symbol "lisp_jit_code" which is not useful.
         (when (plusp addr)
           (or (when (<= sb-vm:alien-linkage-table-space-start addr
                         (+ sb-vm:alien-linkage-table-space-start
                            (1- sb-vm:alien-linkage-table-space-size)))
                 (let* ((index (sb-vm::alien-linkage-table-index-from-address addr))
                        (name (sb-impl::alien-linkage-index-to-name index)))
                   (note (lambda (s) (format s "&~A" name)) dstate)))
               (unless (sb-kernel:immobile-space-addr-p addr)
                 (maybe-note-assembler-routine addr nil dstate))
               ;; Show the absolute address and maybe the contents.
               (note (format nil "[#x~x]~@[ = #x~x~]"
                             addr
                             (case width
                              (:qword (unboxed-constant-ref dstate addr disp))))
                     dstate))))))

    ;; Recognize "[Rbase+disp]" as an alien linkage table reference if Rbase was
    ;; just loaded with the base address in the prior instruction.
    (when (and (eql (machine-ea-base value)
                    (car (sb-disassem::dstate-known-register-contents dstate)))
               (eq (cdr (sb-disassem::dstate-known-register-contents dstate))
                   'alien-linkage)
               (not (machine-ea-index value))
               (integerp (machine-ea-disp value)))
      (let ((name (sb-impl::alien-linkage-index-to-name
                   (floor (machine-ea-disp value) sb-vm:alien-linkage-table-entry-size))))
        (note (lambda (s) (format s "&~A" name)) dstate)))
    (setf (sb-disassem::dstate-known-register-contents dstate) nil)

    (flet ((guess-symbol (predicate)
             (binding* ((code-header (seg-code (dstate-segment dstate)) :exit-if-null)
                        (header-n-words (code-header-words code-header)))
               (loop for word-num from code-constants-offset below header-n-words
                     for obj = (code-header-ref code-header word-num)
                     when (and (symbolp obj) (funcall predicate obj))
                     do (return obj)))))
      (when (and (not base-reg) (not index-reg) disp)
        (let ((addr (+ disp ; guess that DISP points to a symbol-value slot
                       (- (ash sb-vm:symbol-value-slot sb-vm:word-shift))
                       sb-vm:other-pointer-lowtag)))
          (awhen (guess-symbol (lambda (s) (= (get-lisp-obj-address s) addr)))
            (note (lambda (stream) (prin1 it stream)) dstate)
            (return-from print-mem-ref))))
      ;; Try to reverse-engineer which thread-local binding this is
      #+sb-thread
      (cond ((and disp ; Test whether disp looks aligned to an object header
                  (not (logtest (- disp 4) sb-vm:lowtag-mask))
                  (not base-reg) (not index-reg))
             (let* ((addr (+ disp (- 4) sb-vm:other-pointer-lowtag))
                    (symbol
                     (guess-symbol (lambda (s) (= (get-lisp-obj-address s) addr)))))
               (when symbol
                 ;; "tls_index:" is access to the half-sized slot within the
                 ;; symbol header that provides an offset into TLS.
                 (note (lambda (stream) (format stream "tls_index: ~S" symbol))
                       dstate))))
            ;; thread slots
            ((and (eql base-reg sb-vm::thread-reg)
                  #+gs-seg (dstate-getprop dstate +gs-segment+)
                  #-gs-seg (not (dstate-getprop dstate +fs-segment+)) ; not system TLS
                  (not index-reg) ; no index
                  (typep disp '(integer -128 *)) ; valid displacement
                  (zerop (logand disp 7))) ; lispword-aligned
             (let* ((index (ash disp -3))
                    (symbol (cond ((minusp index)
                                   (let ((index (1- (- index))))
                                     (when (array-in-bounds-p sb-vm::+thread-header-slot-names+ index)
                                       (aref sb-vm::+thread-header-slot-names+ index))))
                                  ((< index (length thread-slot-names))
                                   (aref thread-slot-names index)))))
               (when symbol
                 (when (and (eq symbol 'sb-vm::alien-linkage-table-base)
                            (eql (logandc2 (sb-disassem::dstate-inst-properties dstate) +rex-r+)
                                 (logior +rex+ +rex-w+ +rex-b+)))
                   (setf (sb-disassem::dstate-known-register-contents dstate)
                         `(,(reg-num (regrm-inst-reg dchunk-zero dstate)) . alien-linkage)))
                 (return-from print-mem-ref
                   (note (lambda (stream) (format stream "thread.~(~A~)" symbol))
                         dstate))))
             (let ((symbol (or (guess-symbol
                                (lambda (s) (= (symbol-tls-index s) disp)))
                               ;; static symbols aren't in the code header
                               (find disp +static-symbols+
                                     :key #'symbol-tls-index))))
               (when symbol
                 (return-from print-mem-ref
                   ;; "tls:" refers to the current value of the symbol in TLS
                   (note (lambda (stream) (format stream "tls: ~S" symbol))
                         dstate)))))
            ))))

(defun lea-compute-label (value dstate)
  ;; If VALUE should be regarded as a label, return the address.
  ;; If not, just return VALUE.
  (if (and (typep value 'machine-ea) (eq (machine-ea-base value) :rip))
      (let ((addr (+ (dstate-next-addr dstate) (machine-ea-disp value))))
        (if (= (logand addr lowtag-mask) fun-pointer-lowtag)
            (- addr fun-pointer-lowtag)
            addr))
      value))

;; Figure out whether LEA should print its EA with just the stuff in brackets,
;; or additionally show the EA as either a label or a hex literal.
(defun lea-print-ea (value stream dstate &aux (width (inst-operand-size dstate)))
  ;; If disassembling into the dstate, print nothing; just stash the operand.
  (unless stream
    (return-from lea-print-ea (operand (cons value width) dstate)))
  (let*
      ((ea)
       (addr
         (etypecase value
           (machine-ea
            ;; Indicate to PRINT-MEM-REF that this is not a memory access.
            (print-mem-ref :compute value width stream dstate)
            (when (eq (machine-ea-base value) :rip)
              (+ (dstate-next-addr dstate) (machine-ea-disp value))))

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
            ;; (the instruction format is known because LEA has exactly one format)
            (print-mem-ref :compute (setf ea (regrm-inst-r/m dchunk-zero dstate))
                           width stream dstate)
            value)

           ;; LEA Rx,Ry is an illegal encoding, but we'll show it as-is.
           ;; When we used integers instead of REG to represent registers, this case
           ;; overlapped with the preceding. It's nice that it no longer does.
           (reg
            (print-reg-with-width value width stream dstate)
            nil))))
    (cond ((stringp addr)             ; label
           (note (lambda (s) (format s "= ~A" addr)) dstate))
          ;; Local function
          ((and ea
                (= (logand (+ (dstate-next-addr dstate) (machine-ea-disp ea))
                           lowtag-mask)
                   fun-pointer-lowtag)
                (let* ((seg (dstate-segment dstate))
                       (code (seg-code seg))
                       (offset (+ (sb-disassem::seg-initial-offset seg)
                                  (dstate-next-offs dstate)
                                  (- (machine-ea-disp ea)
                                     fun-pointer-lowtag))))
                  (loop for n below (code-n-entries code)
                        do (when (= (%code-fun-offset code n) offset)
                             (let ((fun (%code-entry-point code n)))
                               (note (lambda (stream) (prin1-quoted-short fun stream)) dstate))
                             (return t))))))
          (addr
           (note (lambda (s) (format s "= #x~x" addr)) dstate)))))

;;;; interrupt instructions

(defun break-control (chunk inst stream dstate)
  ;; Do not parse bytes following a trap instruction unless it belongs to lisp code.
  ;; C++ compilers will emit ud2 for various reasons.
  (when (sb-disassem::dstate-foreign-code-p dstate)
    (return-from break-control))
  (flet ((nt (x) (if stream (note x dstate))))
    (let ((trap (if (eq (sb-disassem::inst-print-name inst) 'ud2)
                    (word-imm-code chunk dstate)
                    (byte-imm-code chunk dstate))))
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
       (t
        (when (or (and (= trap cerror-trap) (progn (nt "cerror trap") t))
                  (>= trap uninitialized-load-trap))
          (handle-break-args
           (lambda (sap offset trap-number length-only)
             (if (= trap-number uninitialized-load-trap)
                 (let ((reg (ash (sap-ref-8 sap offset) -2)))
                   ;; decode a single byte arg, not an SC+OFFSET
                   (values (error-number-or-lose 'uninitialized-memory-error)
                           1     ; total number of bytes consumed after the trap
                           (list (make-sc+offset unsigned-reg-sc-number reg))
                           '(1)  ; display 1 byte for the register encoding
                           nil)) ; no error number after the trap instruction
                 (snarf-error-junk sap offset trap-number length-only)))
           trap stream dstate)))))))

;;; Disassemble memory of CODE from START-ADDRESS for LENGTH bytes
;;; calling FUNCTION on each instruction that has a PC-relative operand.
;;; If supplied, PREDICATE is used to filter out some function invocations.
(defun scan-relative-operands
    (code start-address length dstate segment function
     &optional (predicate #'constantly-t))
  (declare (type function function))
  (let* ((inst-space (get-inst-space))
         ;; Look for these instruction formats.
         (call-inst (find-inst #xE8 inst-space))
         (jmp-inst (find-inst #xE9 inst-space))
         (cond-jmp-inst (find-inst #x800f inst-space))
         (lea-inst (find-inst #x8D inst-space))
         (mov-inst (find-inst #x8B inst-space))
         (address (get-lisp-obj-address code))
         (text-start (sap-int (code-instructions code)))
         (text-end (+ text-start (%code-text-size code)))
         (sap (int-sap start-address)))
    (setf (seg-virtual-location segment) start-address
          (seg-length segment) length
          (seg-sap-maker segment) (lambda () sap))
    (map-segment-instructions
     (lambda (dchunk inst &aux (opcode (sap-ref-8 sap (dstate-cur-offs dstate))))
       (flet ((includep (target)
                ;; Self-relative (to the code object) operands are ignored.
                (and (or (< target address) (>= target text-end))
                     (funcall predicate target))))
         (cond ((or (eq inst jmp-inst) (eq inst call-inst))
                (let ((operand (+ (near-jump-displacement dchunk dstate)
                                  (dstate-next-addr dstate))))
                  (when (includep operand)
                    (funcall function (+ (dstate-cur-offs dstate) 1)
                             operand inst))))
               ((eq inst cond-jmp-inst)
                ;; jmp CALL-SYMBOL is invoked with a conditional jump
                ;; (but not call CALL-SYMBOL because only JMP can be conditional)
                (let ((operand (+ (near-cond-jump-displacement dchunk dstate)
                                  (dstate-next-addr dstate))))
                  (when (includep operand)
                    (funcall function (+ (dstate-cur-offs dstate) 2)
                             operand inst))))
               ((or (eq inst lea-inst)
                    (and (eq inst mov-inst) (eql opcode #x8B)))
                ;; Computing the address of UNDEFINED-FDEFN is done with LEA.
                ;; Load from the alien linkage table can be done with MOV Rnn,[RIP-k].
                (let ((modrm (sap-ref-8 sap (1+ (dstate-cur-offs dstate)))))
                  (when (= (logand modrm #b11000111) #b00000101) ; RIP-relative mode
                    (let ((operand (+ (signed-sap-ref-32
                                       sap (+ (dstate-cur-offs dstate) 2))
                                      (dstate-next-addr dstate))))
                      (when (includep operand)
                        (aver (eql (logand (sap-ref-8 sap (1- (dstate-cur-offs dstate))) #xF0)
                                   #x40)) ; expect a REX prefix
                        (funcall function (+ (dstate-cur-offs dstate) 2)
                                 operand inst)))))))))
     segment dstate nil)))

;;; A code signature (for purposes of the ICF pass) is a list of function
;;; signatures, each of which is cons of a vector of instruction bytes with some
;;; replaced by 0, plus an opaque set of integers that need to be compared for
;;; equality to test whether the blobs of code are functionally equivalent.
(defun sb-vm::compute-code-signature (code dstate &aux result)
  (dotimes (i (code-n-entries code) result)
    (let* ((f (%code-entry-point code i))
           (len (%simple-fun-text-len f i))
           (buffer (make-array (ceiling len n-word-bytes) :element-type 'word))
           (operand-values))
      (with-pinned-objects (code buffer)
        (let ((fun-sap (simple-fun-entry-sap f)))
          (%byte-blt fun-sap 0 buffer 0 len)
          ;; Smash each PC-relative operand, and collect the effective value of
          ;; the operand and its offset in the buffer.  We needn't compute the
          ;; lisp object referred to by the operand because it can't change.
          ;; (PC-relative values are used only when the EA is not subject to
          ;; movement due to GC, except during defrag). More than 32 bits might
          ;; be needed for the absolute EA, so we don't simply write it back
          ;; to the buffer, though at present 32 bits would in fact suffice.
          (scan-relative-operands
           code (sap-int fun-sap) len dstate
           (make-memory-segment nil 0 0) ; will get start/length reassigned anyway
           (lambda (offset operand inst)
             (declare (ignore inst))
             (setf operand-values (list* operand offset operand-values)
                   (sap-ref-32 (vector-sap buffer) offset) 0)))))
      (push (cons buffer (coerce operand-values 'simple-vector)) result))))

;;; Perform ICF on instructions of CODE
(defun sb-vm::machine-code-icf (code mapper replacements print)
  (declare (ignorable code mapper replacements print))
  #+immobile-space
  (flet ((scan (sap length dstate segment)
           (scan-relative-operands
            code (sap-int sap) length dstate segment
            (lambda (offset operand inst)
              (declare (ignorable inst))
              (let ((lispobj (when (immobile-space-addr-p operand)
                               (sb-vm::find-called-object operand))))
                (when (functionp lispobj)
                  (let ((replacement (funcall mapper lispobj)))
                    (unless (eq replacement lispobj)
                      (when print
                        (format t "ICF: ~S -> ~S~%" lispobj replacement))
                      (let* ((disp (- (get-lisp-obj-address replacement)
                                      (get-lisp-obj-address lispobj)))
                             (old-rel32 (signed-sap-ref-32 sap offset))
                             (new-rel32 (the (signed-byte 32) (+ old-rel32 disp))))
                        (setf (signed-sap-ref-32 sap offset) new-rel32))))))))))
    (if (eq code sb-fasl:*assembler-routines*)
        (multiple-value-bind (start end) (sb-fasl::calc-asm-routine-bounds)
          (scan (sap+ (code-instructions code) start)
                (- end start)
                (make-dstate)
                (make-memory-segment nil 0 0)))
        ;; Pre-scan the code header to determine whether there is
        ;; a reason to scan the instruction bytes.
        (when (loop for i from code-constants-offset below (code-header-words code)
                    thereis (let ((obj (code-header-ref code i)))
                              (typecase obj
                                (fdefn (awhen (fdefn-fun obj)
                                         (gethash (fun-code-header (%fun-fun it))
                                                  replacements)))
                                (simple-fun
                                 (gethash (fun-code-header obj) replacements)))))
          (let ((dstate (make-dstate))
                (seg (make-memory-segment nil 0 0)))
            (with-pinned-objects (code)
              (dotimes (i (code-n-entries code))
                (let ((f (%code-entry-point code i)))
                  (scan (simple-fun-entry-sap f)
                        (%simple-fun-text-len f i)
                        dstate seg)))))))))
