(load (merge-pathnames "editcore.lisp" *load-pathname*))

(in-package "SB-EDITCORE")

(defglobal +noexec-stack-note+ ".section .note.GNU-stack, \"\", @progbits")

;;; Emit .byte or .quad directives dumping memory from SAP for COUNT units
;;; (bytes or qwords) to STREAM.  SIZE specifies which direcive to emit.
;;; EXCEPTIONS specify offsets at which a specific string should be
;;; written to the file in lieu of memory contents, useful for emitting
;;; expressions involving the assembler '.' symbol (the current PC).
(defun emit-asm-directives (size sap count stream &optional exceptions)
  (declare (optimize speed))
  (declare (stream stream))
  (let ((*print-base* 16)
        (string-buffer (make-array 18 :element-type 'base-char))
        (fmt #.(coerce "0x%lx" 'base-string))
        (per-line 0))
    (declare ((integer 0 32) per-line)
             (fixnum count))
    string-buffer fmt
    (ecase size
      (:qword
       (format stream " .quad")
       (dotimes (i count)
         (declare ((unsigned-byte 20) i))
         (declare (simple-vector exceptions))
         (write-char (if (> per-line 0) #\, #\space) stream)
         (acond ((and (< i (length exceptions)) (aref exceptions i))
                 (write-string it stream))
                (t
                 (write-string "0x" stream)
                 (write (sap-ref-word sap (* i n-word-bytes)) :stream stream)))
         (when (and (= (incf per-line) 16) (< (1+ i) count))
           (format stream "~% .quad")
           (setq per-line 0))))
      (:byte
       (aver (not exceptions))
       (format stream " .byte")
       (dotimes (i count)
         (write-char (if (> per-line 0) #\, #\space) stream)
         (write-string "0x" stream)
         (write (sap-ref-8 sap i) :stream stream)
         (when (and (= (incf per-line) 32) (< (1+ i) count))
           (format stream "~% .byte")
           (setq per-line 0))))))
  (terpri stream))

#+arm64
(defun list-textual-instructions (sap length core load-addr emit-cfi)
  (declare (ignore emit-cfi))
  (let ((dstate (core-dstate core))
        (spaces (core-spacemap core))
        (seg (core-seg core))
        (list)
        (inst-ldr-reg (load-time-value (find-inst #xF940002A (get-inst-space))))
        (inst-bl (load-time-value (find-inst #x97EC8AEB (get-inst-space))))
        (inst-b (load-time-value (find-inst #x17FFFFE4 (get-inst-space))))
        (inst-adrp (load-time-value (find-inst #xB0FFA560 (get-inst-space))))
        (inst-add (load-time-value (find-inst #x91003F7C (get-inst-space))))
        (adrp-reg/addr nil))
    (setf (seg-virtual-location seg) load-addr
          (seg-length seg) length
          (seg-sap-maker seg) (lambda () sap))
    (map-segment-instructions
     (lambda (dchunk inst)
       (cond
         ((eq inst inst-adrp)
          (let ((reg (ldb (byte 5 0) dchunk))
                (next-dchunk (sb-arm64-asm::current-instruction dstate 4))
                (current-page (ash (dstate-cur-addr dstate) -12))
                (page-displacement
                  (sign-extend (+ (ldb (byte 2 29) dchunk)
                                  (ash (ldb (byte 19 5) dchunk) 2))
                               21)))
            (cond ((and (eq inst-add
                            (find-inst next-dchunk (load-time-value (get-inst-space))))
                        (= reg (ldb (byte 5 0) next-dchunk))
                        (= reg (ldb (byte 5 5) next-dchunk)))
                   ;; Rewrite any ADRP, ADD sequences which compute addresses
                   ;; into the linkage table into references into the GOT.
                   (let ((target-addr (+ (ash (+ current-page page-displacement) 12)
                                         (ldb (byte 12 10) next-dchunk))))
                     (when (or (in-bounds-p target-addr (core-fixedobj-bounds core))
                               (in-bounds-p target-addr (core-linkage-bounds core)))
                       (push (list (dstate-cur-offs dstate)
                                   4    ; length
                                   :adrp-gotpcrel
                                   target-addr
                                   (format nil "x~d" reg))
                             list)
                       (push (list (+ 4 (dstate-cur-offs dstate))
                                   4    ; length
                                   :ldr-gotpcrel
                                   target-addr
                                   (format nil "x~d" reg))
                             list))))
                  ((and (eq inst-ldr-reg
                            (find-inst next-dchunk (load-time-value (get-inst-space))))
                        (= reg (ldb (byte 5 0) next-dchunk))
                        (= reg (ldb (byte 5 5) next-dchunk)))
                   ;; Rewrite any ADRP, LDR sequences which load
                   ;; foreign-dataref addresses into the linkage table
                   ;; into references into the GOT.
                   (let ((target-addr (+ (ash (+ current-page page-displacement) 12)
                                         (ash (ldb (byte 12 10) next-dchunk) word-shift))))
                     (when (or (in-bounds-p target-addr (core-fixedobj-bounds core))
                               (in-bounds-p target-addr (core-linkage-bounds core)))
                       (push (list (dstate-cur-offs dstate)
                                   4    ; length
                                   :adrp-gotpcrel
                                   target-addr
                                   (format nil "x~d" reg))
                             list)
                       (push (list (+ 4 (dstate-cur-offs dstate))
                                   4    ; length
                                   :ldr-gotpcrel
                                   target-addr
                                   (format nil "x~d" reg))
                             list)))))))
         ((or (eq inst inst-bl) (eq inst inst-b))
          ;; Rewrite any BLs which jump to the trampoline in linkage
          ;; space to instead jump directly to the alien function in
          ;; the text section.
          (let ((target-addr (+ (dstate-cur-addr dstate)
                                (* 4 (sign-extend (ldb (byte 26 0) dchunk) 26)))))
            (when (or (in-bounds-p target-addr (core-fixedobj-bounds core))
                      (in-bounds-p target-addr (core-linkage-bounds core)))
              (push (list* (dstate-cur-offs dstate)
                           4            ; length
                           (if (eq inst inst-bl) :bl :b)
                           target-addr)
                    list))))))
     seg
     dstate
     nil)
    (nreverse list)))

;;; Disassemble the function pointed to by SAP for LENGTH bytes, returning
;;; all instructions that should be emitted using assembly language
;;; instead of .quad and/or .byte directives including:
;;; - instructions that manipulate the call frame
;;; - lisp-linkage table JMP, CALL, or LEA
;;; - alien-linkage table CALL or LEA
;;; At execution time the function will have virtual address LOAD-ADDR.
#+x86-64
(defun list-textual-instructions (sap length core load-addr emit-cfi)
  (let ((insts (simple-collect-inst-model sap length load-addr))
        (alien-linkage-end
         (+ (bounds-low (core-linkage-bounds core)) alien-linkage-space-size))
        (result))
    (flet ((pc-relative-ea-p (x)
             (when (consp x) (setq x (car x)))
             (and (typep x 'machine-ea) (eq (machine-ea-base x) :rip)))
           (ea-disp-of (x)
             (when (consp x) (setq x (car x)))
             (machine-ea-disp x)))
      (when (and emit-cfi
                 (equalp (car insts) '(0 pop (#s(machine-ea :disp 8 :base 5) . :qword))))
        (push (list* 0 3 :pop "8(%rbp)") result))
      (do ((insts insts (cdr insts)))
          ((endp insts))
        (let* ((inst (car insts))
               (inst-len (if (cdr insts) (- (caadr insts) (car inst))))
               (ea (car (last inst))))
          (when (or (member (cadr inst) '(sb-x86-64-asm::jmp sb-x86-64-asm::call))
                    (pc-relative-ea-p ea))
            (cond ((and (integerp ea) (in-bounds-p ea (core-linkage-bounds core)))
                   (aver (eq (cadr inst) 'sb-x86-64-asm::call))
                   (aver (< ea alien-linkage-end)) ; CALL via alien linkage
                   (aver (= inst-len 5))
                   (push (list* (car inst) 5 :call ea) result))
                  ((pc-relative-ea-p ea)
                   (let* ((next-pc (+ load-addr (caadr insts)))
                          (ea (+ next-pc (ea-disp-of ea)))
                          (table-offset (- ea alien-linkage-end)))
                     (when (in-bounds-p ea (core-linkage-bounds core))
                       (cond ((< ea alien-linkage-end) ; alien linkage
                              (aver (= inst-len 7))
                              (let ((op (ecase (cadr inst)
                                          (sb-x86-64-asm::mov :mov-gotpcrel)
                                          (sb-x86-64-asm::lea :lea)))
                                    (args (list ea (string-downcase (princ-to-string (third inst))))))
                                (push (list* (car inst) 7 op args) result)))
                             ((eq (cadr inst) 'sb-x86-64-asm::lea)
                              ;; Get ADDRESS of lisp linkage cell in stepping-enabled code
                              (aver (eq (third inst) (get-gpr :qword 0))) ; %rax
                              (aver (= inst-len 7))
                              (push (list* (car inst) 7 :lea (format nil "(fntbl+~d)(%rip),%rax"
                                                                      table-offset))
                                    result))
                             (t ; lisp CALL or JMP
                              (aver (= inst-len 6))
                              (let ((new-inst
                                     (format nil "~a *(fntbl+~d)(%rip)"
                                             (string-downcase (cadr inst))
                                             table-offset)))
                                (push (list* (car inst) 6 :lispcall new-inst) result))))))))))))
      (nreverse result)))

(defun c-linkage-sym-from-addr (addr core)
  (let ((entry-index (/ (- addr (bounds-low (core-linkage-bounds core)))
                        (core-alien-linkage-entry-size core))))
    (setf (bit (core-alien-linkage-symbol-usedp core) entry-index) 1)
    (let ((symbol (aref (core-alien-linkage-symbols core) entry-index)))
      (if (listp symbol)
          (values (car symbol) entry-index t) ; data linkage
          (values symbol entry-index nil))))) ; code linkage

;;; Using assembler directives and/or real mnemonics, dump COUNT bytes
;;; of memory at PADDR (physical addr) to STREAM.
;;; The function's address as per the core file is VADDR.
;;; (Its eventual address is indeterminate)
;;; If EMIT-CFI is true, then also emit cfi directives.
;;;
;;; Notice that we can use one fewer cfi directive than usual because
;;; Lisp always carries a frame pointer as set up by the caller.
;;;
;;; C convention
;;; ============
;;; pushq %rbp
;;; .cfi_def_cfa_offset 16   # CFA offset from default register (rsp) is +16
;;; .cfi_offset 6, -16       # old rbp was saved in -16(CFA)
;;; movq %rsp, %rbp
;;; .cfi_def_cfa_register 6  # use rbp as CFA register
;;;
;;; Lisp convention
;;; ===============
;;; popq 8(%rbp) # place saved %rip in its ABI-compatible stack slot
;;;              # making RSP = RBP after the pop, and RBP = CFA - 16
;;; .cfi_def_cfa 6, 16
;;; .cfi_offset 6, -16
;;;
;;; Of course there is a flip-side to this: unwinders think that the new frame
;;; is already begun in the caller. Interruption between these two instructions:
;;;   MOV RBP, RSP / CALL #xzzzzz
;;; will show the backtrace as if two invocations of the caller are on stack.
;;; This is tricky to fix because while we can relativize the CFA to the
;;; known frame size, we can't do that based only on a disassembly.
(defun emit-lisp-function (paddr vaddr count stream emit-cfi core &optional labels)
  (when emit-cfi
    (format stream " .cfi_startproc~%"))
  ;; Any byte offset that appears as a key in the INSTRUCTIONS causes the indicated
  ;; bytes to be written as an assembly language instruction rather than opaquely,
  ;; thereby affecting the ELF data (cfi or relocs) produced.
  (let ((instructions
         (merge 'list labels
                (list-textual-instructions (int-sap paddr) count core vaddr emit-cfi)
                #'< :key #'car))
        (ptr paddr))
    (symbol-macrolet ((cur-offset (- ptr paddr)))
      (loop
        (let ((until (if instructions (caar instructions) count)))
          ;; if we're not aligned, then write some number of bytes
          ;; to cause alignment. But do not write past the next offset
          ;; that needs to be written as an instruction.
          (when (logtest ptr #x7) ; unaligned
            (let ((n (min (- (nth-value 1 (ceiling ptr 8)))
                          (- until cur-offset))))
              (aver (<= 0 n 7))
              (emit-asm-directives :byte (int-sap ptr) n stream)
              (incf ptr n)))
          ;; Now we're either aligned to a multiple of 8, or the current
          ;; offset needs to be written as a textual instruction.
          (let ((n (- until cur-offset)))
            (aver (>= n 0))
            (multiple-value-bind (qwords remainder) (floor n 8)
              (when (plusp qwords)
                (emit-asm-directives :qword (int-sap ptr) qwords stream #())
                (incf ptr (* qwords 8)))
              (when (plusp remainder)
                (emit-asm-directives :byte (int-sap ptr) remainder stream)
                (incf ptr remainder))))
          ;; If the current offset is COUNT, we're done.
          (when (= cur-offset count) (return))
          (aver (= cur-offset until))
          ;; A label and a textual instruction could co-occur.
          ;; If so, the label must be emitted first.
          (when (eq (cadar instructions) :label)
            (destructuring-bind (c-symbol globalp) (cddr (pop instructions))
              ;; The C symbol is global only if the Lisp name is a legal function
              ;; designator and not random noise.
              ;; This is a technique to try to avoid appending a uniquifying suffix
              ;; on all the junky internal things like "(lambda # in srcfile.lisp)"
              (when emit-cfi
                (format stream "~:[~; .globl ~a~:*~%~] .type ~a, @function~%"
                        globalp c-symbol))
              (format stream "~a:~%" c-symbol)))
          ;; If both a label and textual instruction occur here, handle the latter.
          ;; [This could could be simpler if all labels were emitted as
          ;; '.set "thing", .+const' together in a single place, but it's more readable
          ;; to see them where they belong in the instruction stream]
          (when (and instructions (= (caar instructions) cur-offset))
            (destructuring-bind (length opcode . operand) (cdr (pop instructions))
              (ecase opcode
                (#+arm64 (:bl :b) #+x86-64 (:call)
                 ;; XXX Should this AVER that it is in-bounds?
                 (when (in-bounds-p operand (core-linkage-bounds core))
                   (setq operand (c-linkage-sym-from-addr operand core)))
                 (format stream " ~A ~:[0x~X~;~a~:[~;@PLT~]~]~%"
                                   opcode (stringp operand) operand
                                   #+x86-64
                                   (core-enable-pie core)
                                   #+arm64 nil ; arm64 doesn't need the extra @PLT
                                   ))
                ((#+x86-64 :mov-gotpcrel) ; = get address of alien code linkage entry
                 (let ((c-symbol (c-linkage-sym-from-addr (car operand) core)))
                   (format stream " mov ~A@GOTPCREL(%rip), %~(~A~)~%" c-symbol (cadr operand))))
                ((#+arm64 :adrp-gotpcrel)
                 (let ((c-symbol (c-linkage-sym-from-addr (car operand) core)))
                   (format stream " adrp ~A,:got:~A~%" (cadr operand) c-symbol)))
                ((#+arm64 :ldr-gotpcrel)
                 (let ((c-symbol (c-linkage-sym-from-addr (car operand) core)))
                   (format stream " ldr ~A, [~A, #:got_lo12:~A]~%"
                           (cadr operand) (cadr operand) c-symbol)))
                ((#+x86-64 :lea)
                 ;; The THEN case is to calculate a linkage-table cell address for stepping,
                 ;; the ELSE case is FOREIGN-SYMBOL-SAP
                 (if (stringp operand)
                     (format stream " lea ~A~%" operand) ; the "operand" is the whole instructon
                     (let ((c-symbol (c-linkage-sym-from-addr (car operand) core)))
                       ;; lea becomes "mov" with gotpcrel as src, which will become lea
                       (format stream " mov ~A@GOTPCREL(%rip), %~(~A~)~%" c-symbol (cadr operand)))))
                ((#+x86-64 :pop)
                 (format stream " ~A ~A~%" opcode operand)
                 (cond ((string= operand "8(%rbp)")
                        (format stream " .cfi_def_cfa 6, 16~% .cfi_offset 6, -16~%"))
                       ((string= operand "%rbp")
                                        ;(format stream " .cfi_def_cfa 7, 8~%")
                        nil)
                       (t)))
                ((#+x86-64 :lispcall)
                           ;; the so-called "operand" is the entire instruction
                           (write-string operand stream)
                           (terpri stream)))
              (incf ptr length)))
          (when (= cur-offset count) (return)))))
    (when emit-cfi
      (format stream " .cfi_endproc~%"))
    nil))

(defun c-symbol-quote (name)
  (concatenate 'string '(#\") name '(#\")))

(defun c-name (lispname core pp-state &optional (prefix ""))
  (when (typep lispname '(string 0))
    (setq lispname "anonymous"))
  ;; Perform backslash escaping on the exploded string
  ;; Strings were stringified without surrounding quotes,
  ;; but there might be quotes embedded anywhere, so escape them,
  ;; and also remove newlines and non-ASCII.
  (let ((characters
         (mapcan (lambda (char)
                   (cond ((not (typep char 'base-char)) (list #\?))
                         ((member char '(#\\ #\")) (list #\\ char))
                         ((eql char #\newline) (list #\_))
                         (t (list char))))
                 (coerce (cond
                           #+darwin
                           ((and (stringp lispname)
                                 ;; L denotes a symbol which can not be global on macOS.
                                 (char= (char lispname 0) #\L))
                            (concatenate 'string "_" lispname))
                           (t
                            (write-to-string lispname
                              ;; Printing is a tad faster without a pretty stream
                              :pretty (not (typep lispname 'core-sym))
                              :pprint-dispatch *editcore-ppd*
                              ;; FIXME: should be :level 1, however see
                              ;; https://bugs.launchpad.net/sbcl/+bug/1733222
                              :escape nil :level 2 :length 5
                              :case :downcase :gensym nil
                              :right-margin 10000)))
                         'list))))
    (let ((string (concatenate 'string prefix characters)))
      ;; If the string appears in the linker symbols, then string-upcase it
      ;; so that it looks like a conventional Lisp symbol.
      (cond ((find-if (lambda (x) (string= string (if (consp x) (car x) x)))
                      (core-alien-linkage-symbols core))
             (setq string (string-upcase string)))
            ((string= string ".") ; can't use the program counter symbol either
             (setq string "|.|")))
      ;; If the symbol is still nonunique, add a random suffix.
      ;; The secondary value is whether the symbol should be a linker global.
      ;; For now, make nothing global, thereby avoiding potential conflicts.
      (let ((occurs (incf (gethash string (car pp-state) 0))))
        (if (> occurs 1)
            (values (concatenate 'string  string "_" (write-to-string occurs))
                    nil)
            (values string
                    nil))))))

(defun emit-symbols (blobs core pp-state output &aux base-symbol)
  (dolist (blob blobs base-symbol)
    (destructuring-bind (name start . end) blob
      (let ((c-name (c-name (or name "anonymous") core pp-state)))
        (unless base-symbol
          (setq base-symbol c-name))
        (format output " lsym \"~a\", 0x~x, 0x~x~%"
                c-name start (- end start))))))

(defun emit-funs (code vaddr core dumpwords output base-symbol emit-cfi)
  (let* ((ranges (get-text-ranges code core))
         (text-sap (code-instructions code))
         (text (sap-int text-sap))
         ;; Like CODE-INSTRUCTIONS, but where the text virtually was
         (text-vaddr (+ vaddr (* (code-header-words code) n-word-bytes)))
         (max-end 0))
    ;; There is *always* at least 1 word of unboxed data now
    (aver (eq (caar ranges) :data))
    (let ((jump-table-size (code-jump-table-words code))
          (total-nwords (cdr (pop ranges))))
      (cond ((> jump-table-size 1)
             (format output "# jump table~%")
             (format output ".quad ~d" (sap-ref-word text-sap 0))
             (dotimes (i (1- jump-table-size))
               (format output ",\"~a\"+0x~x"
                       base-symbol
                       (- (sap-ref-word text-sap (ash (1+ i) word-shift))
                          vaddr)))
             (terpri output)
             (let ((remaining (- total-nwords jump-table-size)))
               (when (plusp remaining)
                 (funcall dumpwords
                          (sap+ text-sap (ash jump-table-size word-shift))
                          remaining output))))
            (t
             (funcall dumpwords text-sap total-nwords output))))
    (loop
      (destructuring-bind (start . end) (pop ranges)
        (setq max-end end)
        ;; FIXME: it seems like this should just be reduced to emitting 2 words
        ;; now that simple-fun headers don't hold any boxed words.
        ;; (generality here is without merit)
        (funcall dumpwords (sap+ text-sap start) simple-fun-insts-offset output
                 #(nil #.(format nil ".+~D" (* (1- simple-fun-insts-offset)
                                             n-word-bytes))))
        (incf start (* simple-fun-insts-offset n-word-bytes))
        ;; Pass the current physical address at which to disassemble,
        ;; the notional core address (which changes after linker relocation),
        ;; and the length.
        (emit-lisp-function (+ text start) (+ text-vaddr start) (- end start)
                            output emit-cfi core)
        (cond ((not ranges) (return))
              ((eq (caar ranges) :pad)
               (format output " .byte ~{0x~x~^,~}~%"
                       (loop for i from 0 below (cdr (pop ranges))
                             collect (sap-ref-8 text-sap (+ end i))))))))
    ;; Emit bytes from the maximum function end to the object end.
    ;; We can't just round up %CODE-CODE-SIZE to a double-lispword
    ;; because the boxed header could end at an odd word, requiring that
    ;; the unboxed bytes have an odd size in words making the total even.
    (format output " .byte ~{0x~x~^,~}~%"
            (loop for i from max-end
                  below (- (code-object-size code)
                           (* (code-header-words code) n-word-bytes))
                  collect (sap-ref-8 text-sap i)))
    nil))

(defconstant +gf-name-slot+ 5)

(defun output-bignum (label bignum stream)
  (let ((nwords (sb-bignum:%bignum-length bignum)))
    (format stream "~@[~a:~] .quad 0x~x"
            label (logior (ash nwords 8) bignum-widetag))
    (dotimes (i nwords)
      (format stream ",0x~x" (sb-bignum:%bignum-ref bignum i)))
    (when (evenp nwords) ; pad
      (format stream ",0"))
    (format stream "~%")))

(defconstant core-align #+x86-64 4096 #+arm64 65536)

(defun write-preamble (output n-lisp-linkage-words)
  #+linkage-space (format output
          "  .local fntbl
 .comm fntbl,~D,8
 .globl lisp_function_linkage_table
 .equiv lisp_function_linkage_table, fntbl~%"
              (* n-lisp-linkage-words n-word-bytes))

  (format output " .text~% .file \"sbcl.core\"
~:[~; .macro .size sym size # ignore
 .endm
 .macro .type sym type # ignore
 .endm~]
 .macro lasmsym name size
 .set \"\\name\", .
 .size \"\\name\", \\size
 .type \"\\name\", function
 .endm
 .macro lsym name start size
 .set \"\\name\", . + \\start
 .size \"\\name\", \\size
 .type \"\\name\", function
 .endm
 .globl ~alisp_code_start, ~alisp_jit_code, ~alisp_code_end
 .balign ~a~%~alisp_code_start:~%CS: # code space~%"
          (member :darwin *features*)
          label-prefix label-prefix label-prefix
          core-align
          label-prefix))

(defun output-lisp-asm-routines (core spacemap code-addr n-lisp-linkage-words output &aux (skip 0))
  (write-preamble output n-lisp-linkage-words)
  (dotimes (i 2)
    (let* ((paddr (int-sap (translate-ptr code-addr spacemap)))
           (word (sap-ref-word paddr 0)))
      ;; After running the converter which moves dynamic-space code to text space,
      ;; the text space starts with an array of uint32 for the offsets to each object
      ;; and an array of uint64 containing some JMP instructions.
      (unless (or (= (%widetag-of word) simple-array-unsigned-byte-32-widetag)
                  (= (%widetag-of word) simple-array-unsigned-byte-64-widetag))
        (return))
      (let* ((array (%make-lisp-obj (logior (sap-int paddr) other-pointer-lowtag)))
             (size (primitive-object-size array))
             (nwords (ash size (- word-shift))))
        (dotimes (i nwords)
          (format output "~A 0x~x"
                  (case (mod i 8)
                    (0 #.(format nil "~% .quad"))
                    (t ","))
                  (sap-ref-word paddr (ash i word-shift))))
        (terpri output)
        (incf skip size)
        (incf code-addr size))))
  (let* ((code-component (make-code-obj code-addr spacemap))
         (obj-sap (int-sap (- (get-lisp-obj-address code-component)
                              other-pointer-lowtag)))
         (header-len (code-header-words code-component))
         (jump-table-count (sap-ref-word (code-instructions code-component) 0)))
    ;; Write the code component header
    (format output "lar: # lisp assembly routines~%")
    (emit-asm-directives :qword obj-sap header-len output #())
    ;; Write the jump table
    (format output " .quad ~D" jump-table-count)
    (dotimes (i (1- jump-table-count))
      (format output ",lar+0x~x"
              (- (sap-ref-word (code-instructions code-component)
                               (ash (1+ i) word-shift))
                 code-addr)))
    (terpri output)
    (let ((name->addr
           ;; the CDR of each alist item is a target cons (needing translation)
           (sort
            (mapcar (lambda (entry &aux (name (translate (undescriptorize (car entry)) spacemap)) ; symbol
                                   ;; VAL is (start end . index)
                                   (val (translate (undescriptorize (cdr entry)) spacemap))
                                   (start (car val))
                                   (end (car (translate (cdr val) spacemap))))
                      (list* (translate (symbol-name name) spacemap) start end))
                    (target-hash-table-alist (%code-debug-info code-component) spacemap))
            #'< :key #'cadr)))
      ;; Possibly unboxed words and/or padding
      (let ((here (ash jump-table-count word-shift))
            (first-entry-point (cadar name->addr)))
        (when (> first-entry-point here)
          (format output " .quad ~{0x~x~^,~}~%"
                  (loop for offs = here then (+ offs 8)
                        while (< offs first-entry-point)
                        collect (sap-ref-word (code-instructions code-component) offs)))))
      ;; Loop over the embedded routines
      (let ((list name->addr)
            (obj-size (code-object-size code-component)))
        (loop
          (destructuring-bind (name start-offs . end-offs) (pop list)
            (let ((nbytes (- (if (endp list)
                                 (- obj-size (* header-len n-word-bytes))
                                 (1+ end-offs))
                             start-offs)))
              (format output " lasmsym ~(\"~a\"~), ~d~%" name nbytes)
              (let ((fixups
                     (emit-lisp-function
                      (+ (sap-int (code-instructions code-component))
                         start-offs)
                      (+ code-addr
                         (ash (code-header-words code-component) word-shift)
                         start-offs)
                      nbytes output nil core)))
                (aver (null fixups)))))
          (when (endp list) (return)))
        (format output "~%# end of lisp asm routines~2%")
        (+ skip obj-size)))))

;;; Return a list of ((NAME START . END) ...)
;;; for each C symbol that should be emitted for this code object.
;;; Start and and are relative to the object's base address,
;;; not the start of its instructions. Hence we add HEADER-BYTES
;;; too all the PC offsets.
(defun code-symbols (code core)
  (let* ((fun-map (extract-fun-map code core))
         (header-bytes (* (code-header-words code) n-word-bytes))
         (start-pc 0)
         (i 1)
         (len (length fun-map))
         (blobs))
    (loop
      (let* ((name (remove-name-junk
                    (sb-c::compiled-debug-fun-name (svref fun-map (1- i)))))
             (end-pc (if (= i len)
                         (code-object-size code)
                         (+ header-bytes (svref fun-map i)))))
        (unless (= end-pc start-pc)
          ;; Collapse adjacent address ranges named the same.
          ;; Use EQUALP instead of EQUAL to compare names
          ;; because instances of CORE-SYMBOL are not interned objects.
          (if (and blobs (equalp (caar blobs) name))
              (setf (cddr (car blobs)) end-pc)
              (push (list* name start-pc end-pc) blobs)))
        (when (= i len)
          (return))
        (setq start-pc end-pc))
      (incf i 2))
    (nreverse blobs)))

;;; Convert immobile text space to an assembly file in OUTPUT.
(defun write-asm-file
    (spacemap linkage-space-info output
     &optional enable-pie (emit-cfi t)
     &aux (code-bounds (space-bounds immobile-text-core-space-id spacemap))
          (fixedobj-bounds (space-bounds immobile-fixedobj-core-space-id spacemap))
          (core (make-core spacemap code-bounds fixedobj-bounds
                           :enable-pie enable-pie :linkage-space-info linkage-space-info))
          (code-addr (bounds-low code-bounds))
          (total-code-size 0)
          (pp-state (cons (make-hash-table :test 'equal) nil))
          (prev-namestring "")
          (n-linker-relocs 0)
          (temp-output (make-string-output-stream :element-type 'base-char))
          end-loc)
  (labels ((dumpwords (sap count stream &optional (exceptions #()) logical-addr)
             (aver (sap>= sap (car spacemap)))
             ;; Add any new "header exceptions" that cause intra-code-space pointers
             ;; to be computed at link time
             (dotimes (i (if logical-addr count 0))
               (unless (and (< i (length exceptions)) (svref exceptions i))
                 (let ((word (sap-ref-word sap (* i n-word-bytes))))
                   (when (and (= (logand word 3) 3) ; is a pointer
                              (in-bounds-p word code-bounds)) ; to code space
                     #+nil
                     (format t "~&~(~x: ~x~)~%" (+ logical-addr  (* i n-word-bytes))
                             word)
                     (incf n-linker-relocs)
                     (setf exceptions (adjust-array exceptions (max (length exceptions) (1+ i))
                                                    :initial-element nil)
                           (svref exceptions i)
                           (format nil "CS+0x~x"
                                   (- word (bounds-low code-bounds))))))))
             (emit-asm-directives :qword sap count stream exceptions)))

    (let ((skip (output-lisp-asm-routines core spacemap code-addr
                                          (linkage-space-count linkage-space-info)
                                          output)))
      (incf code-addr skip)
      (incf total-code-size skip))
    (loop
      (when (>= code-addr (bounds-high code-bounds))
        (setq end-loc code-addr)
        (return))
      (ecase (%widetag-of (sap-ref-word (int-sap (translate-ptr code-addr spacemap)) 0))
        (#.code-header-widetag
         (let* ((code (make-code-obj code-addr spacemap))
                (objsize (code-object-size code)))
           (incf total-code-size objsize)
           (cond
             ((%instancep (%code-debug-info code)) ; assume it's a COMPILED-DEBUG-INFO
              (aver (plusp (code-n-entries code)))
              (let* ((source
                      (sb-c::compiled-debug-info-source
                       (truly-the sb-c::compiled-debug-info
                                  (translate (%code-debug-info code) spacemap))))
                     (namestring
                      (sb-c::debug-source-namestring
                       (truly-the sb-c::debug-source (translate source spacemap)))))
                (setq namestring (if (core-null-p namestring)
                                     "sbcl.core"
                                     (translate namestring spacemap)))
                (unless (string= namestring prev-namestring)
                  (format output " .file \"~a\"~%" namestring)
                  (setq prev-namestring namestring)))
              (let ((code-physaddr (logandc2 (get-lisp-obj-address code) lowtag-mask)))
                (format output "#x~x:~%" code-addr)
                ;; Emit symbols before the code header data, because the symbols
                ;; refer to "." (the current PC) which is the base of the object.
                (let* ((base (emit-symbols (code-symbols code core) core pp-state output))
                       (altered-fixups
                        (emit-funs code code-addr core #'dumpwords temp-output base emit-cfi))
                       (header-exceptions (vector nil nil nil nil)))
                  (aver (null altered-fixups))
                  (dumpwords (int-sap code-physaddr)
                             (code-header-words code) output header-exceptions code-addr)
                  (write-string (get-output-stream-string temp-output) output))))
             (t
              (error "Strange code component: ~S" code)))
           (incf code-addr objsize)))
        (#.filler-widetag
         (let* ((word (sap-ref-word (int-sap (translate-ptr code-addr spacemap)) 0))
                (nwords (ash word -32))
                (nbytes (* nwords n-word-bytes)))
           (format output " .quad 0x~x~% .fill ~d~%" word (- nbytes n-word-bytes))
           (incf code-addr nbytes)))
        ;; This is a trailing array which contains a jump instruction for each
        ;; element of *C-LINKAGE-REDIRECTS* (see "rewrite-asmcalls.lisp").
        (#.simple-array-unsigned-byte-64-widetag
         (let* ((paddr (translate-ptr code-addr spacemap))
                (array (%make-lisp-obj (logior paddr other-pointer-lowtag)))
                (nwords (+ vector-data-offset (align-up (length array) 2))))
           (format output "# alien linkage redirects:~% .quad")
           (dotimes (i nwords (terpri output))
             (format output "~a0x~x" (if (= i 0) " " ",")
                     (sap-ref-word (int-sap paddr) (ash i word-shift))))
           (incf code-addr (ash nwords word-shift))
           (setq end-loc code-addr)
           (return))))))

  ;; coreparse uses the 'lisp_jit_code' symbol to set text_space_highwatermark
  ;; The intent is that compilation to memory can use this reserved area
  ;; (if space remains) so that profilers can associate a C symbol with the
  ;; program counter range. It's better than nothing.
  (format output "~a:~%" (labelize "lisp_jit_code"))

  ;; Pad so that non-lisp code can't be colocated on a GC page.
  ;; (Lack of Lisp object headers in C code is the issue)
  (let ((aligned-end (align-up end-loc core-align)))
    (when (> aligned-end end-loc)
      (multiple-value-bind (nwords remainder)
          (floor (- aligned-end end-loc) n-word-bytes)
        (aver (>= nwords 2))
        (aver (zerop remainder))
        (decf nwords 2)
        (format output " .quad 0x~x, ~d # (simple-array fixnum (~d))~%"
                simple-array-fixnum-widetag
                (ash nwords n-fixnum-tag-bits)
                nwords)
        (when (plusp nwords)
          (format output " .fill ~d~%" (* nwords n-word-bytes))))))
  ;; Extend with 1 MB of filler
  (format output " .fill ~D~%~alisp_code_end:
 .size lisp_jit_code, .-lisp_jit_code~%"
          (* 1024 1024) label-prefix)
  (values core total-code-size n-linker-relocs))

;;;; ELF file I/O

(defconstant +sht-null+     0)
(defconstant +sht-progbits+ 1)
(defconstant +sht-symtab+   2)
(defconstant +sht-strtab+   3)
(defconstant +sht-rela+     4)
(defconstant +sht-rel+      9)

(define-alien-type elf64-ehdr
  (struct elf64-edhr
    (ident     (array unsigned-char 16)) ; 7F 45 4C 46 2 1 1 0 0 0 0 0 0 0 0 0
    (type      (unsigned 16))   ; 1 0
    (machine   (unsigned 16))   ; 3E 0
    (version   (unsigned 32))   ; 1 0 0 0
    (entry     unsigned)        ; 0 0 0 0 0 0 0 0
    (phoff     unsigned)        ; 0 0 0 0 0 0 0 0
    (shoff     unsigned)        ;
    (flags     (unsigned 32))   ; 0 0 0 0
    (ehsize    (unsigned 16))   ; 40 0
    (phentsize (unsigned 16))   ;  0 0
    (phnum     (unsigned 16))   ;  0 0
    (shentsize (unsigned 16))   ; 40 0
    (shnum     (unsigned 16))   ;  n 0
    (shstrndx  (unsigned 16)))) ;  n 0
(defconstant ehdr-size (ceiling (alien-type-bits (parse-alien-type 'elf64-ehdr nil)) 8))
(define-alien-type elf64-shdr
  (struct elf64-shdr
    (name      (unsigned 32))
    (type      (unsigned 32))
    (flags     (unsigned 64))
    (addr      (unsigned 64))
    (off       (unsigned 64))
    (size      (unsigned 64))
    (link      (unsigned 32))
    (info      (unsigned 32))
    (addralign (unsigned 64))
    (entsize   (unsigned 64))))
(defconstant shdr-size (ceiling (alien-type-bits (parse-alien-type 'elf64-shdr nil)) 8))
(define-alien-type elf64-sym
  (struct elf64-sym
    (name  (unsigned 32))
    (info  (unsigned 8))
    (other (unsigned 8))
    (shndx (unsigned 16))
    (value unsigned)
    (size  unsigned)))
(define-alien-type elf64-rela
  (struct elf64-rela
    (offset (unsigned 64))
    (info   (unsigned 64))
    (addend (signed 64))))

(defun make-elf64-sym (name info)
  (let ((a (make-array 24 :element-type '(unsigned-byte 8) :initial-element 0)))
    (with-pinned-objects (a)
      (setf (sap-ref-32 (vector-sap a) 0) name
            (sap-ref-8 (vector-sap a) 4) info))
    a))

;;; Return two values: an octet vector comprising a string table
;;; and an alist which maps string to offset in the table.
(defun string-table (strings)
  (let* ((length (+ (1+ (length strings)) ; one more null than there are strings
                    (reduce #'+ strings :key #'length))) ; data length
         (bytes (make-array length :element-type '(unsigned-byte 8)
                            :initial-element 0))
         (index 1)
         (alist))
    (dolist (string strings)
      (push (cons string index) alist)
      (replace bytes (map 'vector #'char-code string) :start1 index)
      (incf index (1+ (length string))))
    (cons (nreverse alist) bytes)))

(defun write-alien (alien size stream)
  (dotimes (i size)
    (write-byte (sap-ref-8 (alien-value-sap alien) i) stream)))

;;; core header should be an array of words in '.rodata', not a 32K page
(defconstant core-header-size +backend-page-bytes+) ; stupidly large (FIXME)

(defconstant e-machine #+x86-64 #x3E #+arm64 #xB7)

(defun write-elf-header (shdrs-start sections output)
  (let ((shnum (1+ (length sections)))  ; section 0 is implied
        (shstrndx (1+ (position :str sections :key #'car)))
        (ident #.(coerce '(#x7F #x45 #x4C #x46 2 1 1 0 0 0 0 0 0 0 0 0)
                         '(array (unsigned-byte 8) 1))))
    (with-alien ((ehdr elf64-ehdr))
      (dotimes (i (ceiling ehdr-size n-word-bytes))
        (setf (sap-ref-word (alien-value-sap ehdr) (* i n-word-bytes)) 0))
      (with-pinned-objects (ident)
        (%byte-blt (vector-sap ident) 0 (alien-value-sap ehdr) 0 16))
      (setf (slot ehdr 'type)      1
            (slot ehdr 'machine)   e-machine
            (slot ehdr 'version)   1
            (slot ehdr 'shoff)     shdrs-start
            (slot ehdr 'ehsize)    ehdr-size
            (slot ehdr 'shentsize) shdr-size
            (slot ehdr 'shnum)     shnum
            (slot ehdr 'shstrndx)  shstrndx)
      (write-alien ehdr ehdr-size output))))

(defun write-section-headers (placements sections string-table output)
  (with-alien ((shdr elf64-shdr))
    (dotimes (i (ceiling shdr-size n-word-bytes)) ; Zero-fill
      (setf (sap-ref-word (alien-value-sap shdr) (* i n-word-bytes)) 0))
    (dotimes (i (1+ (length sections)))
      (when (plusp i) ; Write the zero-filled header as section 0
        (destructuring-bind (name type flags link info alignment entsize)
            (cdr (aref sections (1- i)))
          (destructuring-bind (offset . size)
              (pop placements)
            (setf (slot shdr 'name)  (cdr (assoc name (car string-table)))
                  (slot shdr 'type)  type
                  (slot shdr 'flags) flags
                  (slot shdr 'off)   offset
                  (slot shdr 'size)  size
                  (slot shdr 'link)  link
                  (slot shdr 'info)  info
                  (slot shdr 'addralign) alignment
                  (slot shdr 'entsize) entsize))))
      (write-alien shdr shdr-size output))))

(defconstant sym-entry-size 24)

;;; Write everything except for the core file itself into OUTPUT-STREAM
;;; and leave the stream padded to a 4K boundary ready to receive data.
(defun prepare-elf (core-size relocs output pie)
  ;; PIE uses coreparse relocs which are 8 bytes each, and no linker relocs.
  ;; Otherwise, linker relocs are 24 bytes each.
  (let* ((reloc-entry-size (if pie 8 24))
         (sections
          ;;        name | type | flags | link | info | alignment | entry size
          `#((:core "lisp.core"       ,+sht-progbits+ 0 0 0 ,core-align 0)
             (:sym  ".symtab"         ,+sht-symtab+   0 3 1 8 ,sym-entry-size)
                          ; section with the strings -- ^ ^ -- 1+ highest local symbol
             (:str  ".strtab"         ,+sht-strtab+   0 0 0 1  0)
             (:rel
              ,@(if pie
              ;; Don't bother with an ELF reloc section; it won't do any good.
              ;; It would apply at executable link time, which is without purpose,
              ;; it just offsets the numbers based on however far the lisp.core
              ;; section is into the physical file. Non-loaded sections don't get
              ;; further relocated on execution, so 'coreparse' has to fix the
              ;; entire dynamic space at execution time anyway.
                  `("lisp.rel"        ,+sht-progbits+ 0 0 0 8 8)
                  `(".relalisp.core"  ,+sht-rela+     0 2 1 8 ,reloc-entry-size)))
                                      ; symbol table -- ^ ^ -- for which section
             (:note ".note.GNU-stack" ,+sht-progbits+ 0 0 0 1  0)))
         (extern-c-symbols
          '("lisp_code_start" #+linkage-space "lisp_function_linkage_table"))
         (string-table
          (string-table (append extern-c-symbols (map 'list #'second sections))))
         (strings (cdr string-table))
         (padded-strings-size (align-up (length strings) 8))
         ;; 0th symbol is always for string table index 0
         (symbols-size (* (1+ (length extern-c-symbols)) sym-entry-size))
         (shdrs-start (+ ehdr-size symbols-size padded-strings-size))
         (shdrs-end (+ shdrs-start (* (1+ (length sections)) shdr-size)))
         (relocs-size (* (length relocs) reloc-entry-size))
         (relocs-end (+ shdrs-end relocs-size))
         (core-start (align-up relocs-end core-align)))

    (write-elf-header shdrs-start sections output)

    ;; Write symbol table
    (aver (eql (file-position output) ehdr-size))
    (write-sequence (make-elf64-sym 0 0) output)
    (dolist (sym extern-c-symbols)
      (let ((string-table-index (cdr (assoc sym (car string-table) :test 'string=))))
        ;; The symbol type is #x10 given:
        ;;      #define STB_GLOBAL 1
        ;;   and: #define ELF32_ST_BIND(val) ((unsigned char) (val)) >> 4)
        ;; which places the binding in the high 4 bits of the low byte.
        (write-sequence (make-elf64-sym string-table-index #x10) output)))

    ;; Write string table
    (aver (eql (file-position output) (+ ehdr-size symbols-size)))
    (write-sequence strings output) ; an octet vector at this point
    (dotimes (i (- padded-strings-size (length strings)))
      (write-byte 0 output))

    ;; Write section headers
    (aver (eql (file-position output) shdrs-start))
    (write-section-headers
     (map 'list
          (lambda (x)
            (ecase (car x)
              (:note '(0 . 0))
              (:sym  (cons ehdr-size symbols-size))
              (:str  (cons (+ ehdr-size symbols-size) (length strings)))
              (:rel  (cons shdrs-end relocs-size))
              (:core (cons core-start core-size))))
          sections)
     sections string-table output)

    ;; Write relocations
    (aver (eql (file-position output) shdrs-end))
    (let ((buf (make-array relocs-size :element-type '(unsigned-byte 8)))
          (ptr 0))
      (if pie
          (dovector (reloc relocs)
            (setf (%vector-raw-bits buf ptr) reloc)
            (incf ptr))
          (with-alien ((rela elf64-rela))
            (dovector (r relocs)
              (declare (type (simple-vector 4) r))
              (let ((kind (aref r 0)) (place (aref r 1)) (symbol (aref r 2)) (addend (aref r 3)))
                (setf (slot rela 'offset) place
                      (slot rela 'info)   (logior (ash symbol 32) kind)
                      (slot rela 'addend) addend))
              (setf (%vector-raw-bits buf (+ ptr 0)) (sap-ref-word (alien-value-sap rela) 0)
                    (%vector-raw-bits buf (+ ptr 1)) (sap-ref-word (alien-value-sap rela) 8)
                    (%vector-raw-bits buf (+ ptr 2)) (sap-ref-word (alien-value-sap rela) 16))
              (incf ptr 3))))
      (write-sequence buf output))

    ;; Write padding
    (dotimes (i (- core-start (file-position output)))
      (write-byte 0 output))
    (aver (eq (file-position output) core-start))))

(defconstant R_ABS64 #+x86-64 1 #+arm64 257) ; /* Direct 64 bit  */
(defconstant R_ABS32 #+x86-64 10 #+arm64 258) ; /* Direct 32 bit zero extended */

;;; Fill in the FIXUPS vector with a list of places to fixup.
;;; For PIE-enabled cores, each place is just a virtual address.
;;; For non-PIE-enabled, the fixup corresponds to an ELF relocation which will be
;;; applied at link time of the excutable.
;;; Note that while this "works" for PIE, it is fairly inefficient because
;;; fundamentally Lisp objects contain absolute pointers, and there may be
;;; millions of words that need fixing at load (execution) time.
;;; Several techniques can mitigate this:
;;; * for funcallable-instances, put a second copy of funcallable-instance-tramp
;;;   in dynamic space so that funcallable-instances can jump to a known address.
;;; * for each closure, create a one-instruction trampoline in dynamic space,
;;;   - embedded in a (simple-array word) perhaps - which jumps to the correct
;;;   place in the text section. Point all closures over the same function
;;;   to the new closure stub. The trampoline, being pseudostatic, is effectively
;;;   immovable. (And you can't re-save from an ELF core)
;;; * for arbitrary pointers to simple-funs, create a proxy simple-fun in dynamic
;;;   space whose entry point is the real function in the ELF text section.
;;;   The GC might have to learn how to handle simple-funs that point externally
;;;   to themselves. Also there's a minor problem of hash-table test functions
;;; The above techniques will reduce by a huge factor the number of fixups
;;; that need to be applied on startup of a position-independent executable.
;;;
(defun collect-relocations (spacemap fixups pie &key (verbose nil) (print nil))
  (let* ((code-bounds (space-bounds immobile-text-core-space-id spacemap))
         (code-start (bounds-low code-bounds))
         (n-abs 0)
         (n-rel 0)
         (affected-pages (make-hash-table)))
    (labels
        ((abs-fixup (vaddr core-offs referent)
           (incf n-abs)
           (when print
             (format t "~x = 0x~(~x~): (a)~%" core-offs vaddr #+nil referent))
           (touch-core-page core-offs)
           ;; PIE relocations are output as a file section that is
           ;; interpreted by 'coreparse'. The addend is implicit.
           (setf (sap-ref-word (car spacemap) core-offs)
                 (if pie
                     (+ (- referent code-start) +code-space-nominal-address+)
                     0))
           (if pie
               (vector-push-extend vaddr fixups)
               (vector-push-extend
                ;; #(kind, where symbol-index addend)
                `#(,R_ABS64 ,(+ core-header-size core-offs) 1 ,(- referent code-start))
                fixups)))
         (touch-core-page (core-offs)
           ;; use the OS page size, not +backend-page-bytes+
           (setf (gethash (floor core-offs core-align) affected-pages) t))
         ;; Given a address which is an offset into the data pages of the target core,
         ;; compute the logical address which that offset would be mapped to.
         ;; For example core address 0 is the virtual address of static space.
         (core-to-logical (core-offs &aux (page (floor core-offs +backend-page-bytes+)))
           (setf (gethash page affected-pages) t)
           (dolist (space (cdr spacemap)
                          (bug "Can't translate core offset ~x using ~x"
                               core-offs spacemap))
             (let* ((page0 (space-data-page space))
                    (nwords (space-nwords space))
                    (id (space-id space))
                    (npages (ceiling nwords (/ +backend-page-bytes+ n-word-bytes))))
               (when (and (<= page0 page (+ page0 (1- npages)))
                          (/= id immobile-text-core-space-id))
                 (return (+ (space-addr space)
                            (* (- page page0) +backend-page-bytes+)
                            (logand core-offs (1- +backend-page-bytes+))))))))
         (scanptrs (vaddr obj wordindex-min wordindex-max &optional force &aux (n-fixups 0))
           (do* ((base-addr (logandc2 (get-lisp-obj-address obj) lowtag-mask))
                 (sap (int-sap base-addr))
                 ;; core-offs is the offset in the lisp.core ELF section.
                 (core-offs (- base-addr (sap-int (car spacemap))))
                 (i wordindex-min (1+ i)))
                ((> i wordindex-max) n-fixups)
             (let* ((byte-offs (ash i word-shift))
                    (ptr (sap-ref-word sap byte-offs)))
               (when (and (or (is-lisp-pointer ptr) force) (in-bounds-p ptr code-bounds))
                 (abs-fixup (+ vaddr byte-offs) (+ core-offs byte-offs) ptr)
                 (incf n-fixups)))))
         (scanptr (vaddr obj wordindex)
           (plusp (scanptrs vaddr obj wordindex wordindex))) ; trivial wrapper
         (scan-obj (vaddr obj widetag size
                    &aux (core-offs (- (logandc2 (get-lisp-obj-address obj) lowtag-mask)
                                       (sap-int (car spacemap))))
                         (nwords (ceiling size n-word-bytes)))
           (when (listp obj)
             (scanptrs vaddr obj 0 1)
             (return-from scan-obj))
           (case widetag
             (#.instance-widetag
              (let ((type (truly-the layout (translate (%instance-layout obj) spacemap))))
                (do-layout-bitmap (i taggedp type (%instance-length obj))
                  (when taggedp
                    (scanptr vaddr obj (1+ i))))))
             (#.simple-vector-widetag
              (let ((len (length (the simple-vector obj))))
                ;; FIXME: VECTOR-ADDR-HASHING-FLAG must be left-shifted by ARRAY-FLAGS-DATA-POSITION
                ;; for this LOGTEST to be correct.  I think it broke when array-rank was placed adjacent
                ;; to the widetag and the flags bits moved over. The fact that it seems to be useless
                ;; suggests that I need to come up with a way to assert that it does anything, which
                ;; is all but impossible. We'd need a correctly-hashed table containing a key in which
                ;; nothing moves in the final S-L-A-D, but does have a function move during elfination.
                ;; I'm leaving this line broken for now because frankly I think it might be safer to
                ;; assert that IF the table is address-sensitive THEN it has the rehash flag already set
                ;; in the k/v vector. That's not quite true either, because it only needs to rehash if
                ;; any key was actually hashed by address.
                (cond ((logtest (get-header-data obj) vector-addr-hashing-flag) ; BUG
                       (do ((i 2 (+ i 2)) (needs-rehash))
                           ;; Refer to the figure at the top of src/code/hash-table.lisp.
                           ;; LEN is an odd number.
                           ((>= i (1- len))
                            (when needs-rehash
                              (setf (svref obj 1) 1)))
                         ;; A weak or EQ-based hash table any of whose keys is a function
                         ;; or code-component might need the 'rehash' flag set.
                         ;; In practice, it is likely already set, because any object that
                         ;; could move in the final GC probably did move.
                         (when (scanptr vaddr obj (+ vector-data-offset i))
                           (setq needs-rehash t))
                         (scanptr vaddr obj (+ vector-data-offset i 1))))
                      (t
                       (scanptrs vaddr obj 1 (+ len 1))))))
             (#.fdefn-widetag
              (scanptrs vaddr obj 1 2)
              (scanptrs vaddr obj 3 3 t))
             ((#.closure-widetag #.funcallable-instance-widetag)
              ;; read the trampoline slot
              (let ((word (sap-ref-word (int-sap (get-lisp-obj-address obj))
                                        (- n-word-bytes fun-pointer-lowtag))))
                (when (in-bounds-p word code-bounds)
                  (abs-fixup (+ vaddr n-word-bytes)
                             (+ core-offs n-word-bytes)
                             word)))
              ;; untaggged pointers are generally not supported in
              ;; funcallable instances, so scan everything.
              (scanptrs vaddr obj 1 (1- nwords)))
             ;; mixed boxed/unboxed objects
             (#.code-header-widetag
              (aver (not pie))
              (dotimes (i (code-n-entries obj))
                ;; I'm being lazy and not computing vaddr, which is wrong,
                ;; but does not matter if non-pie; and if PIE, we can't get here.
                ;; [PIE requires all code in immobile space, and this reloc
                ;; is for a dynamic space object]
                (scanptrs 0 (%code-entry-point obj i) 2 5))
              (scanptrs vaddr obj 1 (1- (code-header-words obj))))
             ;; boxed objects that can reference code/simple-funs
             ((#.value-cell-widetag #.symbol-widetag #.weak-pointer-widetag)
              (scanptrs vaddr obj 1 (1- nwords))))))
      (dolist (space (cdr spacemap))
        (unless (= (space-id space) immobile-text-core-space-id)
          (let* ((logical-addr (space-addr space))
                 (size (space-size space))
                 (physical-addr (space-physaddr space spacemap))
                 (physical-end (sap+ physical-addr size))
                 (vaddr-translation (+ (- (sap-int physical-addr)) logical-addr)))
            (dx-flet ((visit (obj widetag size)
                        ;; Compute the object's intended virtual address
                        (scan-obj (+ (logandc2 (get-lisp-obj-address obj) lowtag-mask)
                                     vaddr-translation)
                                  obj widetag size)))
              (map-objects-in-range
               #'visit
               (ash (sap-int physical-addr) (- n-fixnum-tag-bits))
               (ash (sap-int physical-end) (- n-fixnum-tag-bits))))
            (when (and (plusp (logior n-abs n-rel)) verbose)
              (format t "space @ ~10x: ~6d absolute + ~4d relative fixups~%"
                      logical-addr n-abs n-rel))
            (setq n-abs 0 n-rel 0)))))
    (when verbose
      (format t "total of ~D linker fixups affecting ~D/~D pages~%"
              (length fixups)
              (hash-table-count affected-pages)
              (/ (reduce #'+ (cdr spacemap) :key #'space-nbytes-aligned)
                 core-align))))
  fixups)

(defun force-fntbl-ref-p (spacemap)
  (let ((features (detect-target-features spacemap)))
    (not (find :immobile-code features))))

;;; Given a native SBCL '.core' file, or one attached to the end of an executable,
;;; separate it into pieces.
;;; ASM-PATHNAME is the name of the assembler file that will hold all the Lisp code.
;;; The other two output pathnames are implicit: "x.s" -> "x.core" and "x-core.o"
;;; The ".core" file is a native core file used for starting a binary that
;;; contains the asm code using the "--core" argument.  The "-core.o" file
;;; is for linking in to a binary that needs no "--core" argument.
(defun really-split-core
    (input-pathname asm-pathname
     &key enable-pie (verbose nil) dynamic-space-size
     &aux (elf-core-pathname
           (merge-pathnames
            (make-pathname :name (concatenate 'string (pathname-name asm-pathname) "-core")
                           :type "o")
            asm-pathname))
          (core-header (make-array +backend-page-bytes+ :element-type '(unsigned-byte 8)))
          (original-total-npages 0)
          (core-offset 0)
          (page-adjust 0)
          (linkage-space-info (vector 0 0 0 0 0))
          (code-start-fixup-ofs 0) ; where to fixup the core header
          (space-list)
          (copy-actions)
          (fixedobj-range) ; = (START . SIZE-IN-BYTES)
          (relocs (make-array 100000 :adjustable t :fill-pointer 1)))

  (declare (ignorable fixedobj-range))
  ;; Remove old files
  (ignore-errors (delete-file asm-pathname))
  (ignore-errors (delete-file elf-core-pathname))
  ;; Crappy assumption: we already called redirect-text-space-calls which
  ;; assigned a value to *nil-taggedptr*. Anyway, why doesn't this use
  ;; PARSE-CORE-HEADER like it should?
  (assert (/= *nil-taggedptr* 0))
  ;; Ensure that all files can be opened
  (with-open-file (input input-pathname :element-type '(unsigned-byte 8))
    (with-open-file (asm-file asm-pathname :direction :output :if-exists :supersede)
      ;;(with-open-file (split-core split-core-pathname :direction :output
      ;;                            :element-type '(unsigned-byte 8) :if-exists :supersede)
      (let ((split-core nil))
        (setq core-offset (read-core-header input core-header verbose))
        (do-core-header-entry ((id len ptr) core-header)
          (case id
            (#.build-id-core-entry-type-code
             (when verbose
               (incf ptr 2)
               (let ((string (make-string (%vector-raw-bits core-header ptr)
                                          :element-type 'base-char)))
                 (%byte-blt core-header (* (1+ ptr) n-word-bytes) string 0 (length string))
                 (format t "Build ID [~a]~%" string))))
            (#.directory-core-entry-type-code
             (do-directory-entry ((index ptr len) core-header)
               (incf original-total-npages npages)
               (push (make-space id addr data-page page-adjust nwords) space-list)
               (when verbose
                 (format t "id=~d page=~5x + ~5x addr=~10x words=~8x~:[~; (drop)~]~%"
                         id data-page npages addr nwords
                         (= id immobile-text-core-space-id)))
               (cond ((= id immobile-text-core-space-id)
                      (setq code-start-fixup-ofs (+ index 3))
                      ;; Keep this entry but delete the page count. We need to know
                      ;; where the space was supposed to be mapped and at what size.
                      ;; Subsequent core entries will need to adjust their start page
                      ;; downward (just the PTEs's start page now).
                      (setq page-adjust npages data-page 0 npages 0))
                     (t
                      ;; Keep track of where the fixedobj space wants to be.
                      (when (= id immobile-fixedobj-core-space-id)
                        (setq fixedobj-range (cons addr (ash nwords word-shift))))
                      (when (plusp npages) ; enqueue
                        (push (cons data-page (* npages +backend-page-bytes+)) copy-actions))
                      ;; adjust this entry's start page in the new core
                      (decf data-page page-adjust)))))
            (#.lisp-linkage-space-core-entry-type-code
             (symbol-macrolet ((count (%vector-raw-bits core-header (+ ptr 0)))
                               (data-page (%vector-raw-bits core-header (+ ptr 1))))
               (let ((npages (ceiling (ash count word-shift) +backend-page-bytes+)))
                 (setq linkage-space-info
                       (vector (+ ptr 2) data-page npages count
                               (make-array count :element-type 'word)))
                 (push (cons data-page (* npages +backend-page-bytes+)) copy-actions)
                 (incf original-total-npages npages))))
            (#.page-table-core-entry-type-code
             (aver (= len 4))
             (symbol-macrolet ((n-ptes (%vector-raw-bits core-header (+ ptr 1)))
                               (nbytes (%vector-raw-bits core-header (+ ptr 2)))
                               (data-page (%vector-raw-bits core-header (+ ptr 3))))
               (aver (= data-page original-total-npages))
               (aver (= (ceiling (space-nwords
                                  (find dynamic-core-space-id space-list :key #'space-id))
                                 (/ gencgc-page-bytes n-word-bytes))
                        n-ptes))
               (when verbose
                 (format t "PTE: page=~5x~40tbytes=~8x~%" data-page nbytes))
               (push (cons data-page nbytes) copy-actions)
               (decf data-page page-adjust)))))
        (let ((buffer (make-array +backend-page-bytes+
                                  :element-type '(unsigned-byte 8)))
              (filepos))
          ;; Write the new core file
          (when split-core
            (write-sequence core-header split-core))
          (dolist (action (reverse copy-actions)) ; nondestructive
            ;; page index convention assumes absence of core header.
            ;; i.e. data page 0 is the file page immediately following the core header
            (let ((offset (* (1+ (car action)) +backend-page-bytes+))
                  (nbytes (cdr action)))
              (when verbose
                (format t "File offset ~10x: ~10x bytes~%" offset nbytes))
              (setq filepos (+ core-offset offset))
              (cond (split-core
                     (file-position input filepos)
                     (copy-bytes input split-core nbytes buffer))
                    (t
                     (file-position input (+ filepos nbytes))))))
          ;; Trailer (runtime options and magic number)
          (let ((nbytes (read-sequence buffer input)))
            ;; expect trailing magic number
            (let ((ptr (floor (- nbytes n-word-bytes) n-word-bytes)))
              (aver (= (%vector-raw-bits buffer ptr) core-magic)))
            ;; File position of the core header needs to be set to 0
            ;; regardless of what it was
            (setf (%vector-raw-bits buffer 4) 0)
            (when verbose
              (format t "Trailer words:(~{~X~^ ~})~%"
                      (loop for i below (floor nbytes n-word-bytes)
                            collect (%vector-raw-bits buffer i))))
            (when split-core
              (write-sequence buffer split-core :end nbytes)
              (finish-output split-core)))
          ;; Sanity test
          (when split-core
            (aver (= (+ core-offset
                        (* page-adjust +backend-page-bytes+)
                        (file-length split-core))
                     (file-length input))))
          ;; Seek back to the PTE pages so they can be copied to the '.o' file
          (file-position input filepos)))

      ;; If we're going to write memory size options and they weren't already
      ;; present, then they will be inserted after the core magic,
      ;; and the remainder of the core header moves over by 5 words.
      (when (and dynamic-space-size
                 (/= (%vector-raw-bits core-header 1) runtime-options-magic))
        (let ((added-words 5))
          (incf (linkage-space-header-ptr linkage-space-info) added-words)
          (incf code-start-fixup-ofs added-words)))
      (unless enable-pie
        ;; This fixup sets the 'address' field of the core directory entry
        ;; for code space. If PIE-enabled, we'll figure it out in the C code
        ;; because space relocation is going to happen no matter what.
        (setf (aref relocs 0) `#(,R_ABS64 ,(ash code-start-fixup-ofs word-shift) 1 0)))
      #+linkage-space
      (let ((where (ash (linkage-space-header-ptr linkage-space-info) word-shift)))
        ;; LINKAGE_SPACE core entry gets a linker fixup to the second ELF symbol
        (vector-push-extend `#(,R_ABS64 ,where 2 0) relocs)
        (read-linkage-cells input linkage-space-info core-offset))
      ;; Map the original core file to memory
      (with-mapped-core (sap core-offset original-total-npages input)
        (let* ((data-spaces
                (delete immobile-text-core-space-id (reverse space-list)
                        :key #'space-id))
               (spacemap (cons sap (sort (copy-list space-list) #'> :key #'space-addr)))
               (pte-nbytes (cdar copy-actions)))
          (collect-relocations spacemap relocs enable-pie)
          (with-open-file (output elf-core-pathname
                                  :direction :output :if-exists :supersede
                                  :element-type '(unsigned-byte 8))
            (prepare-elf (+ (apply #'+ (mapcar #'space-nbytes-aligned data-spaces))
                            (* (linkage-space-npages linkage-space-info) +backend-page-bytes+)
                            +backend-page-bytes+ ; core header
                            pte-nbytes)
                         relocs output enable-pie)
            (let ((new-header (change-dynamic-space-size core-header dynamic-space-size)))
              ;; This word will be fixed up by the system linker
              (setf (%vector-raw-bits new-header code-start-fixup-ofs)
                    (if enable-pie +code-space-nominal-address+ 0))
              (write-sequence new-header output))
            (force-output output)
            ;; ELF cores created from #-immobile-space cores use +required-foreign-symbols+.
            ;; But if #+immobile-space the alien-linkage-table values are computed
            ;; by 'ld' and we don't scan +required-foreign-symbols+.
            (when (get-space immobile-fixedobj-core-space-id spacemap)
              (let* ((sym (find-target-symbol (package-id "SB-VM")
                                              "+REQUIRED-FOREIGN-SYMBOLS+" spacemap :physical))
                     (vector (translate (symbol-global-value sym) spacemap)))
                (fill vector 0)
                (setf (%array-fill-pointer vector) 0)))
            ;; Change SB-C::*COMPILE-FILE-TO-MEMORY-SPACE* to :DYNAMIC
            ;; and SB-C::*COMPILE-TO-MEMORY-SPACE* to :AUTO
            ;; in case the resulting executable needs to compile anything.
            ;; (Call frame info will be missing, but at least it's something.)
            (dolist (item '(("*COMPILE-FILE-TO-MEMORY-SPACE*" . "DYNAMIC")
                            ("*COMPILE-TO-MEMORY-SPACE*" . "DYNAMIC")))
              (destructuring-bind (symbol . value) item
                (awhen (%find-target-symbol (package-id "SB-C") symbol spacemap)
                  (%set-symbol-global-value
                   it (find-target-symbol (package-id "KEYWORD") value spacemap :logical)))))
            #+linkage-space
            (let ((start (sb-sys:sap+ (car spacemap)
                                      (* (linkage-space-data-page linkage-space-info)
                                         +backend-page-bytes+)))
                  (size (* (linkage-space-npages linkage-space-info) +backend-page-bytes+))
                  (code-bounds (space-bounds immobile-text-core-space-id spacemap)))
              ;; Words pointing to text space get the space base address subtracted.
              ;; And we toggle the low bit to signify that it needs correction at startup.
              (dotimes (i (linkage-space-count linkage-space-info))
                (let ((val (sap-ref-word start (ash i word-shift))))
                  (when (in-bounds-p val code-bounds)
                    (setf (sap-ref-word start (ash i word-shift))
                          (logior (- val (bounds-low code-bounds)) 1)))))
              (aver (eql (sb-unix:unix-write (sb-sys:fd-stream-fd output) start 0 size)
                         size)))
            (let ((fd (sb-sys:fd-stream-fd output)))
              (dolist (space data-spaces) ; Copy pages from memory
                (let ((start (space-physaddr space spacemap))
                      (size (space-nbytes-aligned space)))
                  (aver (eql (sb-unix:unix-write fd start 0 size) size)))))
            (when verbose
              (format t "Copying ~d bytes (#x~x) from ptes = ~d PTEs~%"
                      pte-nbytes pte-nbytes (floor pte-nbytes 10)))
            (copy-bytes input output pte-nbytes)) ; Copy PTEs from input
          (let ((core (write-asm-file spacemap linkage-space-info asm-file enable-pie)))
            (format asm-file " .section .rodata~% .p2align 4~%lisp_fixups:~%")
            ;; Sort the hash-table in emit order.
            (dolist (x (sort (%hash-table-alist (core-new-fixups core)) #'< :key #'cdr))
              (output-bignum nil (car x) asm-file))
            (cond
              (t ; (get-space immobile-fixedobj-core-space-id spacemap)
               (format asm-file "~% .section .data~%")
               (format asm-file " .globl ~A~%~:*~A:
 .quad ~d~%"
                    (labelize "alien_linkage_values")
                    (length (core-alien-linkage-symbols core)))
               ;; -1 (not a plausible function address) signifies that word
               ;; following it is a data, not text, reference.
               (loop for s across (core-alien-linkage-symbols core)
                     do (format asm-file " .quad ~:[~;-1, ~]~a~%"
                                (consp s)
                                (if (consp s) (car s) s)))
               ;; dark magic to ensure that the elf linkage space is emitted
               (when (force-fntbl-ref-p spacemap)
                 (format asm-file "# this is not an alien symbol~% .quad fntbl~%"))
               )
              (t
               (format asm-file "~% .section .rodata~%")
               (format asm-file " .globl anchor_junk~%")
               (format asm-file "anchor_junk: .quad lseek_largefile, get_timezone, compute_udiv_magic32~%"))))))
      (when (member :linux *features*)
        (format asm-file "~% ~A~%" +noexec-stack-note+)))))

;;; Copy the input core into an ELF section without splitting into code & data.
;;; Also force a linker reference to each C symbol that the Lisp core mentions.
(defun copy-to-elf-obj (input-pathname output-pathname)
  ;; Remove old files
  (ignore-errors (delete-file output-pathname))
  ;; Ensure that all files can be opened
  (with-open-file (input input-pathname :element-type '(unsigned-byte 8))
    (with-open-file (output output-pathname :direction :output
                            :element-type '(unsigned-byte 8) :if-exists :supersede)
      (let* ((core-header (make-array +backend-page-bytes+
                                      :element-type '(unsigned-byte 8)))
             (core-offset (read-core-header input core-header nil))
             (parsed-header (parse-core-header input core-header core-offset))
             (space-list (core-header-space-list parsed-header)))
        ;; Map the core file to memory
        (with-mapped-core (sap core-offset (core-header-total-npages parsed-header) input)
          (let* ((spacemap (cons sap (sort (copy-list space-list) #'> :key #'space-addr)))
                 (core (make-core spacemap
                                  (space-bounds immobile-text-core-space-id spacemap)
                                  (space-bounds immobile-fixedobj-core-space-id spacemap)))
                 (c-symbols (map 'list (lambda (x) (if (consp x) (car x) x))
                                 (core-alien-linkage-symbols core)))
                 (sections `#((:str  ".strtab"         ,+sht-strtab+   0 0 0 1  0)
                              (:sym  ".symtab"         ,+sht-symtab+   0 1 1 8 ,sym-entry-size)
                              ;;             section with the strings -- ^ ^ -- 1+ highest local symbol
                              (:core "lisp.core"       ,+sht-progbits+ 0 0 0 ,core-align 0)
                              (:note ".note.GNU-stack" ,+sht-progbits+ 0 0 0 1  0)))
                 (string-table (string-table (append (map 'list #'second sections)
                                                     c-symbols)))
                 (packed-strings (cdr string-table))
                 (strings-start (+ ehdr-size (* (1+ (length sections)) shdr-size)))
                 (strings-end (+ strings-start (length packed-strings)))
                 (symbols-start (align-up strings-end 8))
                 (symbols-size (* (1+ (length c-symbols)) sym-entry-size))
                 (symbols-end (+ symbols-start symbols-size))
                 (core-start (align-up symbols-end core-align))
                 (core-size (+ (* (1+ (core-header-total-npages parsed-header))
                                  +backend-page-bytes+)
                               (core-header-pte-nbytes parsed-header))))
            (write-elf-header ehdr-size sections output)
            (write-section-headers `((,strings-start . ,(length packed-strings))
                                     (,symbols-start . ,symbols-size)
                                     (,core-start    . ,core-size)
                                     (0 . 0))
                                   sections string-table output)
            (write-sequence packed-strings output)
            ;; Write symbol table
            (file-position output symbols-start)
            (write-sequence (make-elf64-sym 0 0) output)
            (dolist (sym c-symbols)
              (let ((name-ptr (cdr (assoc sym (car string-table)))))
                (write-sequence (make-elf64-sym name-ptr #x10) output)))
            ;; Copy core
            (file-position output core-start)
            (file-position input core-offset)
            (let ((remaining core-size))
              (loop (let ((n (read-sequence core-header input
                                            :end (min +backend-page-bytes+ remaining))))
                      (write-sequence core-header output :end n)
                      (unless (plusp (decf remaining n)) (return))))
              (aver (zerop remaining)))))))))

;; The extra copy of ASM routines, particularly C-calling trampolines, that now reside in text
;; space have to be modified to correctly reference their C functions. They assume that static
;; space is near alien-linkage space, and so they use this form:
;;   xxxx: E8A1F0EFFF  CALL #x50000060 ; alloc
;; which unforuntately means that after relocating to text space, that instruction refers
;; to random garbage, and more unfortunately there is no room to squeeze in an instruction
;; that encodes to 7 bytes.
;; So we have to create an extra jump "somewhere" that indirects through the linkage table
;; but is callable from the text-space code.
;;; I don't feel like programmatically scanning the asm code to determine these.
;;; Hardcoded is good enough (until it isn't)
(defparameter *c-linkage-redirects*
  (mapcar #'list
          '("switch_to_arena"
            "alloc"
            "alloc_list"
            "listify_rest_arg"
            "make_list"
            "alloc_funinstance"
            "allocation_tracker_counted"
            "allocation_tracker_sized")))

(defun patch-asm-codeblob (core &aux (spacemap (core-spacemap core)))
  (binding* ((static-space (get-space static-core-space-id spacemap))
             (text-space (get-space immobile-text-core-space-id spacemap))
             ((new-code-vaddr new-code) (get-text-space-asm-code-replica text-space spacemap))
             ((old-code-vaddr old-code) (get-static-space-asm-code static-space spacemap))
             (code-offsets-vector
              (%make-lisp-obj (logior (sap-int (space-physaddr text-space spacemap))
                                      other-pointer-lowtag)))
             (header-bytes (ash (code-header-words old-code) word-shift))
             (old-insts-vaddr (+ old-code-vaddr header-bytes))
             (new-insts-vaddr (+ new-code-vaddr header-bytes))
             (items *c-linkage-redirects*)
             (inst-buffer (make-array 8 :element-type '(unsigned-byte 8)))
             (code-offsets-vector-size (primitive-object-size code-offsets-vector))
             (c-linkage-vector-vaddr (+ (space-addr text-space) code-offsets-vector-size))
             (c-linkage-vector ; physical
              (%make-lisp-obj (logior (sap-int (sap+ (space-physaddr text-space spacemap)
                                                     code-offsets-vector-size))
                                      other-pointer-lowtag)))
             (target-alien-lss ; linkage-space-start
              (symbol-global-value
               (find-target-symbol (package-id "SB-VM") "ALIEN-LINKAGE-SPACE-START"
                                   spacemap))))
    (aver (<= (length items) (length c-linkage-vector)))
    (dolist (x *c-linkage-redirects*)
      (let* ((index (position (car x) (core-alien-linkage-symbols core)
                              :test (lambda (a b) (and (stringp b) (string= a b)))))
             (addr (+ target-alien-lss (* (core-alien-linkage-entry-size core) index))))
        (rplacd x (int-sap addr))))
    (with-pinned-objects (inst-buffer)
      (do ((sap (vector-sap inst-buffer))
           (item-index 0 (1+ item-index))
           (items items (cdr items)))
          ((null items))
        ;; Each new quasi-linkage-table entry takes 8 bytes to encode.
        ;; The JMP is 7 bytes, followed by a nop.
        ;; FF2425nnnnnnnn = JMP [ea]
        (setf (sap-ref-8 sap 0) #xFF
              (sap-ref-8 sap 1) #x24
              (sap-ref-8 sap 2) #x25
              (sap-ref-32 sap 3) (sap-int (sap+ (cdar items) 8))
              (sap-ref-8 sap 7) #x90) ; nop
        (setf (aref c-linkage-vector item-index) (%vector-raw-bits inst-buffer 0))))
    ;; Produce a model of the instructions. It doesn't really matter whether we scan
    ;; OLD-CODE or NEW-CODE since we're supplying the proper virtual address either way.
    (let ((insts (get-code-instruction-model old-code old-code-vaddr core)))
;;  (dovector (inst insts) (write inst :base 16 :pretty nil :escape nil) (terpri))
      (dovector (inst insts)
        ;; Look for any call to a linkage table entry.
        (when (eq (second inst) 'call)
          (let ((operand (third inst)))
            (when (and (integerp operand)
                       (>= operand target-alien-lss)
                       (< operand (+ target-alien-lss alien-linkage-space-size)))
              (let* ((index (position (int-sap operand) *c-linkage-redirects*
                                      :key #'cdr :test #'sap=))
                     (branch-target (+ c-linkage-vector-vaddr
                                       (ash vector-data-offset word-shift)
                                       ;; each new linkage entry takes up exactly 1 word
                                       (* index n-word-bytes)))
                     (old-next-ip-abs (int-sap (inst-end inst))) ; virtual
                     (next-ip-rel (sap- old-next-ip-abs (int-sap old-insts-vaddr)))
                     (new-next-ip (+ new-insts-vaddr next-ip-rel)))
                (setf (signed-sap-ref-32 (code-instructions new-code) (- next-ip-rel 4))
                      (- branch-target new-next-ip))))))))))

(defconstant smallvec-elts
  (- (ash gencgc-page-bytes (- word-shift)) vector-data-offset))
(defvar *name-map* nil)
(defun linkage-index-to-name (index spacemap)
  (unless *name-map*
    (let ((sym (find-target-symbol (package-id "SB-VM") "*LINKAGE-NAME-MAP*"
                                   spacemap :physical)))
      (setf *name-map* (translate (symbol-global-value sym) spacemap))))
  (multiple-value-bind (hi lo) (floor index smallvec-elts)
    (let ((inner (translate (svref *name-map* hi) spacemap)))
      (translate (weak-vector-ref inner lo) spacemap))))
#+x86-64
(defun use-indirection-p (index spacemap)
  (let ((name (linkage-index-to-name index spacemap)))
    (if (symbolp name)
        (let ((str (translate (symbol-name name) spacemap)))
          (case (symbol-package-id name)
            (#.(sb-impl::package-id (find-package "CL"))
             ;; Users like to encapsulate this apparently
             (string= str "FIND-PACKAGE"))
            (#.(sb-impl::package-id (find-package "SB-VM"))
             ;; These alway get redefined on startup
             (member str '(sb-vm::simd-reverse32 sb-vm::simd-reverse8
                           sb-vm::simd-nreverse32 sb-vm::simd-nreverse8)
                     :test 'string=)))))))
#+x86-64
(defun bypass-indirection-cells
    (code vaddr core
     &optional print
     &aux (insts (get-code-instruction-model code vaddr core))
          (spacemap (core-spacemap core))
          (text-bounds (space-bounds immobile-text-core-space-id spacemap))
          (linkage-bounds (core-linkage-bounds core))
          (alien-linkage-end (+ (bounds-low linkage-bounds) alien-linkage-space-size))
          (indices))
  (declare (simple-vector insts))
  (labels ((linkage-table-load-p (prev-inst)
             (and (eq (second prev-inst) 'mov)
                  (eq (third prev-inst) (get-gpr :qword 0))
                  (let ((src (fourth prev-inst)))
                    (and (typep src '(cons machine-ea (eql :qword)))
                         (let ((ea (car src)))
                           (and (eql (machine-ea-base ea) 13)
                                (eql (machine-ea-disp ea)
                                     (ash sb-vm::thread-linkage-table-slot word-shift))
                                (null (machine-ea-index ea))))))))
           (linkage-index (ea inst i)
             (cond ((eq (machine-ea-base ea) :rip) ; RIP+n format
                    (let* ((next-pc (+ (range-vaddr (car inst)) (range-bytecount (car inst))))
                           (addr (+ next-pc (machine-ea-disp ea))))
                      (when (and (>= addr alien-linkage-end)
                                 (< addr (bounds-high linkage-bounds)))
                        (ash (- addr alien-linkage-end) (- word-shift)))))
                   ((and (eql (machine-ea-base ea) 0) ; RAX+n format
                         (null (machine-ea-index ea))
                         ;; 0 is not a usable linkage cell
                         (typep (machine-ea-disp ea) '(integer 8))
                         (linkage-table-load-p (svref insts (1- i))))
                    (ash (machine-ea-disp ea) (- word-shift))))))
    (do ((i 0 (1+ i)))
        ((>= i (length insts)))
      (let* ((inst (svref insts i))
             (op (second inst))
             (ea (third inst)))
        (when (and (member op '(call jmp)) (typep ea 'machine-ea))
          (binding* ((linkage-index (linkage-index ea inst i) :exit-if-null)
                     (cell (assoc linkage-index indices)))
            (unless cell
              (setq cell (list linkage-index))
              (push cell indices))
            ;; Collect list of insts to replace for the particular linkage-index.
            (push inst (cdr cell)))))))
  ;; Change each linkage table call to instead go directly to the target
  ;; but only if the target uniquely identifies its linkage index within
  ;; this code component for purposes of undoing the optimization.
  ;; TODO: each load of the linkage-table base into RAX should be replaced
  ;; by a 4-byte NOP (#x0f #x1f #x40 #x00) whenever we replace the JMP/CALL.
  (let* ((linkage-cells (linkage-space-cells (core-linkage-space-info core)))
         (indices (coerce indices 'vector))
         (values (map 'vector (lambda (x) (aref linkage-cells (car x)))
                      indices)))
    (dotimes (i (length indices))
      (let ((linkage-index (car (aref indices i))) (value (aref values i)))
        (cond ((not (in-bounds-p value text-bounds))
               (if print (format t "Can't patch ~X: outside of text space~%" value)))
              ((> (count value values) 1)
               (if print (format t "Can't patch ~X: not unique~%" value)))
              ((use-indirection-p linkage-index spacemap)
               (if print (format t "Won't direct call ~D~%" linkage-index)))
              (t
               (dolist (inst (cdr (aref indices i)))
                 (let* ((range (car inst))
                        (sap (sap+ (sap+ (int-sap (get-lisp-obj-address code))
                                         (- other-pointer-lowtag))
                                   (- (range-vaddr range) vaddr)))
                        (next-pc (+ (range-vaddr range) (range-bytecount range)))
                        (disp (the (signed-byte 32) (- value next-pc))))
                   (ecase (cadr inst)
                     (call
                      (setf (sap-ref-8 sap 0) #x40 ; add a do-nothing prefix
                            (sap-ref-8 sap 1) #xe8 ; CALL rel32
                            (signed-sap-ref-32 sap 2) disp))
                     (jmp
                      (setf (sap-ref-8 sap 0) #xe9 ; JMP rel32
                            (signed-sap-ref-32 sap 1) (1+ disp)
                            (sap-ref-8 sap 5) #x90))))))))))) ; followed by NOP

#+x86-64
(defun redirect-text-space-calls (pathname)
  (with-open-file (stream pathname :element-type '(unsigned-byte 8)
                         :direction :io :if-exists :overwrite)
    (let* ((core-header (make-array +backend-page-bytes+ :element-type '(unsigned-byte 8)))
           (core-offset (read-core-header stream core-header))
           (parsed-header (parse-core-header stream core-header core-offset))
           (space-list (core-header-space-list parsed-header)))
      (with-mapped-core (sap core-offset (core-header-total-npages parsed-header) stream)
        (let* ((spacemap (cons sap (sort (copy-list space-list) #'> :key #'space-addr)))
               (core (make-core spacemap (make-bounds 0 0) (make-bounds 0 0)
                                :linkage-space-info (core-header-linkage-space-info parsed-header)))
               (features (detect-target-features spacemap)))
          (cond
            ((not (member :immobile-space features))
             (let* ((text-space (get-space immobile-text-core-space-id spacemap))
                    (offsets-vector (%make-lisp-obj (logior (sap-int (space-physaddr text-space spacemap))
                                                            lowtag-mask))))
               (assert text-space)
               (patch-asm-codeblob core)
               ;; offset 0 is the offset of the ASM routine codeblob which was already processed.
               (loop for j from 1 below (length offsets-vector)
                  do (let ((vaddr (+ (space-addr text-space) (aref offsets-vector j)))
                           (physobj (%make-lisp-obj
                                     (logior (sap-int (sap+ (space-physaddr text-space spacemap)
                                                            (aref offsets-vector j)))
                                             other-pointer-lowtag))))
                       (bypass-indirection-cells physobj vaddr core)))))
            (t
             (let* ((text-space (get-space immobile-text-core-space-id spacemap))
                    (delta (- (translate-ptr (space-addr text-space) spacemap)
                              (space-addr text-space))))
               (walk-target-space (lambda (obj widetag size
                                        &aux (vaddr (- (get-lisp-obj-address obj)
                                                       other-pointer-lowtag delta)))
                                 (declare (ignore widetag size))
                                 (bypass-indirection-cells obj vaddr core))
                               immobile-text-core-space-id spacemap))))
          (persist-to-file spacemap core-offset stream))))))

(defun split-core (input-pathname asm-pathname &rest args)
  (let ((tmp (flet ((try-directory (dir)
                      (when (and dir (string/= "" dir) (sb-unix:unix-access dir sb-unix:w_ok))
                        dir)))
               (format nil "~a/sbcl~D.tmpcore"
                       (or (try-directory (posix-getenv "TMPDIR"))
                           (try-directory "/tmp")
                           (try-directory (user-homedir-pathname))
                           (error "Can't find a writeable directory for our split core."))
                       (sb-unix:unix-getpid)))))
    (with-open-file (stream input-pathname :element-type '(unsigned-byte 8))
      (let* ((core-header (make-array +backend-page-bytes+ :element-type '(unsigned-byte 8)))
             (core-offset (read-core-header stream core-header)))
        (parse-core-header stream core-header core-offset)))
    (unwind-protect
         (progn
           (ecase *heap-arrangement*
             (:gencgc
              ;; input core could be readonly
              (run-program "cp" `("--no-preserve=mode" ,input-pathname ,tmp)
                           :search t))
             (:mark-region-gc
              ;; Assume that the free space in the core hasn't been squashed out yet.
              ;; I'm not sure which of these steps can operate in-place,
              ;; so use an intermediate temp file for the reorg.
              (let ((other-temp (concatenate 'string tmp "0")))
                (reorganize-core input-pathname other-temp)
                (move-dynamic-code-to-text-space other-temp tmp)
                (delete-file other-temp))))
           #+x86-64 (redirect-text-space-calls tmp)
           (apply #'really-split-core tmp asm-pathname args))
      (delete-file tmp))))

(defun cl-user::elfinate (&optional (args (cdr *posix-argv*)))
  (cond ((string= (car args) "split")
         (pop args)
         (let (pie dss)
           (loop (cond ((string= (car args) "--pie")
                        (setq pie t)
                        (pop args))
                       ((string= (car args) "--dynamic-space-size")
                        (pop args)
                        (setq dss (parse-integer (pop args))))
                       (t
                        (return))))
           (destructuring-bind (input asm) args
             (split-core input asm :enable-pie pie :dynamic-space-size dss))))
        ((string= (car args) "copy")
         (apply #'copy-to-elf-obj (cdr args)))
        ((string= (car args) "extract")
         (apply #'move-dynamic-code-to-text-space (cdr args)))
        (t
         (error "Unknown command: ~S" args))))

;; If loaded as a script, do this
(eval-when (:execute)
  (let ((args (cdr *posix-argv*)))
    (when args
      (let ((*print-pretty* nil))
        (format t "Args: ~S~%" args)
        (cl-user::elfinate args)))))
