;;;; that part of the description of the x86-64 instruction set
;;;; which can live on the cross-compilation host

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")
;;; FIXME: SB!DISASSEM: prefixes are used so widely in this file that
;;; I wonder whether the separation of the disassembler from the
;;; virtual machine is valid or adds value.

;;; Note: In CMU CL, this used to be a call to SET-DISASSEM-PARAMS.
(setf sb!disassem:*disassem-inst-alignment-bytes* 1)

;;; This type is used mostly in disassembly and represents legacy
;;; registers only. R8-R15 are handled separately.
(deftype reg () '(unsigned-byte 3))

;;; This includes legacy registers and R8-R15.
(deftype full-reg () '(unsigned-byte 4))

;;; Default word size for the chip: if the operand size /= :dword
;;; we need to output #x66 (or REX) prefix
(def!constant +default-operand-size+ :dword)

;;; The default address size for the chip. It could be overwritten
;;; to :dword with a #x67 prefix, but this is never needed by SBCL
;;; and thus not supported by this assembler/disassembler.
(def!constant +default-address-size+ :qword)

(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)

(defun offset-next (value dstate)
  (declare (type integer value)
	   (type sb!disassem:disassem-state dstate))
  (+ (sb!disassem:dstate-next-addr dstate) value))

(defparameter *byte-reg-names*
  #(al cl dl bl spl bpl sil dil r8b r9b r10b r11b r12b r13b r14b r15b))
(defparameter *word-reg-names*
  #(ax cx dx bx sp bp si di r8w r9w r10w r11w r12w r13w r14w r15w))
(defparameter *dword-reg-names*
  #(eax ecx edx ebx esp ebp esi edi r8d r9d r10d r11d r12d r13d r14d r15d))
(defparameter *qword-reg-names*
  #(rax rcx rdx rbx rsp rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15))

;;; The printers for registers, memory references and immediates need to
;;; take into account the width bit in the instruction, whether a #x66
;;; or a REX prefix was issued, and the contents of the REX prefix.
;;; This is implemented using prefilters to put flags into the slot
;;; INST-PROPERTIES of the DSTATE.  These flags are the following
;;; symbols:
;;;
;;; OPERAND-SIZE-8   The width bit was zero
;;; OPERAND-SIZE-16  The "operand size override" prefix (#x66) was found
;;; REX              A REX prefix was found
;;; REX-W            A REX prefix with the "operand width" bit set was
;;;                  found
;;; REX-R            A REX prefix with the "register" bit set was found
;;; REX-X            A REX prefix with the "index" bit set was found
;;; REX-B            A REX prefix with the "base" bit set was found

;;; Return the operand size depending on the prefixes and width bit as
;;; stored in DSTATE.
(defun inst-operand-size (dstate)
  (declare (type sb!disassem:disassem-state dstate))
  (cond ((sb!disassem:dstate-get-inst-prop dstate 'operand-size-8)
         :byte)
        ((sb!disassem:dstate-get-inst-prop dstate 'rex-w)
         :qword)
        ((sb!disassem:dstate-get-inst-prop dstate 'operand-size-16)
         :word)
        (t
         +default-operand-size+)))

;;; The same as INST-OPERAND-SIZE, but for those instructions (e.g.
;;; PUSH, JMP) that have a default operand size of :qword. It can only
;;; be overwritten to :word.
(defun inst-operand-size-default-qword (dstate)
  (declare (type sb!disassem:disassem-state dstate))
  (if (sb!disassem:dstate-get-inst-prop dstate 'operand-size-16)
      :word
      :qword))

(defun print-reg-with-width (value width stream dstate)
  (declare (type full-reg value)
	   (type stream stream)
           (ignore dstate))
  (princ (aref (ecase width
		 (:byte *byte-reg-names*)
		 (:word *word-reg-names*)
		 (:dword *dword-reg-names*)
		 (:qword *qword-reg-names*))
	       value)
	 stream)
  ;; XXX plus should do some source-var notes
  )

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
    (print-mem-access value (and sized-p width) stream dstate)))

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

(defun print-label (value stream dstate)
  (declare (ignore dstate))
  (sb!disassem:princ16 value stream))

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

;;; This prefilter is used solely for its side effect, namely to put
;;; the property OPERAND-SIZE-8 into the DSTATE if VALUE is 0.
(defun prefilter-width (value dstate)
  (declare (type bit value)
	   (type sb!disassem:disassem-state dstate))
  (when (zerop value)
    (sb!disassem:dstate-put-inst-prop dstate 'operand-size-8))
  value)

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
  (let ((mod (first value))
	(r/m (second value)))
    (declare (type (unsigned-byte 2) mod)
	     (type (unsigned-byte 3) r/m))
    (let ((full-reg (if (sb!disassem:dstate-get-inst-prop dstate 'rex-b)
                        (+ r/m 8)
                        r/m)))
      (declare (type full-reg full-reg))
      (cond ((= mod #b11)
	     ;; registers
	     full-reg)
	    ((= r/m #b100)
	     ;; sib byte
	     (let ((sib (sb!disassem:read-suffix 8 dstate)))
	       (declare (type (unsigned-byte 8) sib))
	       (let ((base-reg (ldb (byte 3 0) sib))
		     (index-reg (ldb (byte 3 3) sib))
		     (index-scale (ldb (byte 2 6) sib)))
		 (declare (type (unsigned-byte 3) base-reg index-reg)
			  (type (unsigned-byte 2) index-scale))
		 (let* ((offset
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
                           (if (sb!disassem:dstate-get-inst-prop dstate 'rex-b)
                               (+ base-reg 8)
                               base-reg))
			 offset
			 (unless (= index-reg #b100)
                           (if (sb!disassem:dstate-get-inst-prop dstate 'rex-x)
                               (+ index-reg 8)
                               index-reg))
			 (ash 1 index-scale))))))
	    ((and (= mod #b00) (= r/m #b101))
	     (list 'rip (sb!disassem:read-signed-suffix 32 dstate)) )
	    ((= mod #b00)
	     (list full-reg))
	    ((= mod #b01)
	   (list full-reg (sb!disassem:read-signed-suffix 8 dstate)))
	  (t				; (= mod #b10)
	   (list full-reg (sb!disassem:read-signed-suffix 32 dstate)))))))

(defun read-address (value dstate)
  (declare (ignore value))		; always nil anyway
  (sb!disassem:read-suffix (width-bits (inst-operand-size dstate)) dstate))

(defun width-bits (width)
  (ecase width
    (:byte 8)
    (:word 16)
    (:dword 32)
    (:qword 64)
    (:float 32)
    (:double 64)))

) ; EVAL-WHEN

;;;; disassembler argument types

;;; Used to capture the lower four bits of the REX prefix.
(sb!disassem:define-arg-type wrxb
  :prefilter #'prefilter-wrxb)

(sb!disassem:define-arg-type width
  :prefilter #'prefilter-width
  :printer (lambda (value stream dstate)
             (declare (ignore value))
             (princ (schar (symbol-name (inst-operand-size dstate)) 0)
                    stream)))

(sb!disassem:define-arg-type displacement
  :sign-extend t
  :use-label #'offset-next
  :printer (lambda (value stream dstate)
	     (sb!disassem:maybe-note-assembler-routine value nil dstate)
	     (print-label value stream dstate)))

(sb!disassem:define-arg-type accum
  :printer (lambda (value stream dstate)
	     (declare (ignore value)
		      (type stream stream)
		      (type sb!disassem:disassem-state dstate))
	     (print-reg 0 stream dstate)))

(sb!disassem:define-arg-type reg
  :prefilter #'prefilter-reg-r
  :printer #'print-reg)

(sb!disassem:define-arg-type reg-b
  :prefilter #'prefilter-reg-b
  :printer #'print-reg)

(sb!disassem:define-arg-type reg-b-default-qword
  :prefilter #'prefilter-reg-b
  :printer #'print-reg-default-qword)

(sb!disassem:define-arg-type imm-addr
  :prefilter #'read-address
  :printer #'print-label)

;;; Normally, immediate values for an operand size of :qword are of size
;;; :dword and are sign-extended to 64 bits. For an exception, see the
;;; argument type definition following this one.
(sb!disassem:define-arg-type signed-imm-data
  :prefilter (lambda (value dstate)
	       (declare (ignore value)) ; always nil anyway
               (let ((width (width-bits (inst-operand-size dstate))))
                 (when (= width 64)
                   (setf width 32))
                 (sb!disassem:read-signed-suffix width dstate))))

;;; Used by the variant of the MOV instruction with opcode B8 which can
;;; move immediates of all sizes (i.e. including :qword) into a
;;; register.
(sb!disassem:define-arg-type signed-imm-data-upto-qword
  :prefilter (lambda (value dstate)
	       (declare (ignore value)) ; always nil anyway
               (sb!disassem:read-signed-suffix
                (width-bits (inst-operand-size dstate))
                dstate)))

;;; Used by those instructions that have a default operand size of
;;; :qword. Nevertheless the immediate is at most of size :dword.
;;; The only instruction of this kind having a variant with an immediate
;;; argument is PUSH.
(sb!disassem:define-arg-type signed-imm-data-default-qword
  :prefilter (lambda (value dstate)
	       (declare (ignore value)) ; always nil anyway
               (let ((width (width-bits
                             (inst-operand-size-default-qword dstate))))
                 (when (= width 64)
                   (setf width 32))
                 (sb!disassem:read-signed-suffix width dstate))))

(sb!disassem:define-arg-type signed-imm-byte
  :prefilter (lambda (value dstate)
	       (declare (ignore value)) ; always nil anyway
	       (sb!disassem:read-signed-suffix 8 dstate)))

(sb!disassem:define-arg-type imm-byte
  :prefilter (lambda (value dstate)
	       (declare (ignore value)) ; always nil anyway
	       (sb!disassem:read-suffix 8 dstate)))

;;; needed for the ret imm16 instruction
(sb!disassem:define-arg-type imm-word-16
  :prefilter (lambda (value dstate)
	       (declare (ignore value)) ; always nil anyway
	       (sb!disassem:read-suffix 16 dstate)))

(sb!disassem:define-arg-type reg/mem
  :prefilter #'prefilter-reg/mem
  :printer #'print-reg/mem)
(sb!disassem:define-arg-type sized-reg/mem
  ;; Same as reg/mem, but prints an explicit size indicator for
  ;; memory references.
  :prefilter #'prefilter-reg/mem
  :printer #'print-sized-reg/mem)

;;; Arguments of type reg/mem with a fixed size.
(sb!disassem:define-arg-type sized-byte-reg/mem
  :prefilter #'prefilter-reg/mem
  :printer #'print-sized-byte-reg/mem)
(sb!disassem:define-arg-type sized-word-reg/mem
  :prefilter #'prefilter-reg/mem
  :printer #'print-sized-word-reg/mem)
(sb!disassem:define-arg-type sized-dword-reg/mem
  :prefilter #'prefilter-reg/mem
  :printer #'print-sized-dword-reg/mem)

;;; Same as sized-reg/mem, but with a default operand size of :qword.
(sb!disassem:define-arg-type sized-reg/mem-default-qword
  :prefilter #'prefilter-reg/mem
  :printer #'print-sized-reg/mem-default-qword)

;;; added by jrd
(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
(defun print-fp-reg (value stream dstate)
  (declare (ignore dstate))
  (format stream "FR~D" value))
(defun prefilter-fp-reg (value dstate)
  ;; just return it
  (declare (ignore dstate))
  value)
) ; EVAL-WHEN
(sb!disassem:define-arg-type fp-reg
			     :prefilter #'prefilter-fp-reg
			     :printer #'print-fp-reg)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defparameter *conditions*
  '((:o . 0)
    (:no . 1)
    (:b . 2) (:nae . 2) (:c . 2)
    (:nb . 3) (:ae . 3) (:nc . 3)
    (:eq . 4) (:e . 4) (:z . 4)
    (:ne . 5) (:nz . 5)
    (:be . 6) (:na . 6)
    (:nbe . 7) (:a . 7)
    (:s . 8)
    (:ns . 9)
    (:p . 10) (:pe . 10)
    (:np . 11) (:po . 11)
    (:l . 12) (:nge . 12)
    (:nl . 13) (:ge . 13)
    (:le . 14) (:ng . 14)
    (:nle . 15) (:g . 15)))
(defparameter *condition-name-vec*
  (let ((vec (make-array 16 :initial-element nil)))
    (dolist (cond *conditions*)
      (when (null (aref vec (cdr cond)))
	(setf (aref vec (cdr cond)) (car cond))))
    vec))
) ; EVAL-WHEN

;;; Set assembler parameters. (In CMU CL, this was done with
;;; a call to a macro DEF-ASSEMBLER-PARAMS.)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf sb!assem:*assem-scheduler-p* nil))

(sb!disassem:define-arg-type condition-code
  :printer *condition-name-vec*)

(defun conditional-opcode (condition)
  (cdr (assoc condition *conditions* :test #'eq)))

;;;; disassembler instruction formats

(eval-when (:compile-toplevel :execute)
  (defun swap-if (direction field1 separator field2)
    `(:if (,direction :constant 0)
	  (,field1 ,separator ,field2)
	  (,field2 ,separator ,field1))))

(sb!disassem:define-instruction-format (byte 8 :default-printer '(:name))
  (op    :field (byte 8 0))
  ;; optional fields
  (accum :type 'accum)
  (imm))

(sb!disassem:define-instruction-format (simple 8)
  (op    :field (byte 7 1))
  (width :field (byte 1 0) :type 'width)
  ;; optional fields
  (accum :type 'accum)
  (imm))

(sb!disassem:define-instruction-format (rex-simple 16)
  (rex     :field (byte 4 4)    :value #b0100)
  (wrxb    :field (byte 4 0)    :type 'wrxb)
  (op    :field (byte 7 9))
  (width :field (byte 1 8) :type 'width)
  ;; optional fields
  (accum :type 'accum)
  (imm))

;;; Same as simple, but with direction bit
(sb!disassem:define-instruction-format (simple-dir 8 :include 'simple)
  (op :field (byte 6 2))
  (dir :field (byte 1 1)))

;;; Same as simple, but with the immediate value occurring by default,
;;; and with an appropiate printer.
(sb!disassem:define-instruction-format (accum-imm 8
				     :include 'simple
				     :default-printer '(:name
							:tab accum ", " imm))
  (imm :type 'signed-imm-data))

(sb!disassem:define-instruction-format (rex-accum-imm 16
				     :include 'rex-simple
				     :default-printer '(:name
							:tab accum ", " imm))
  (imm :type 'signed-imm-data))

(sb!disassem:define-instruction-format (reg-no-width 8
				     :default-printer '(:name :tab reg))
  (op	 :field (byte 5 3))
  (reg   :field (byte 3 0) :type 'reg-b)
  ;; optional fields
  (accum :type 'accum)
  (imm))

(sb!disassem:define-instruction-format (rex-reg-no-width 16
				     :default-printer '(:name :tab reg))
  (rex     :field (byte 4 4)    :value #b0100)
  (wrxb    :field (byte 4 0)    :type 'wrxb)
  (op	   :field (byte 5 11))
  (reg     :field (byte 3 8)    :type 'reg-b)
  ;; optional fields
  (accum :type 'accum)
  (imm))

;;; Same as reg-no-width, but with a default operand size of :qword.
(sb!disassem:define-instruction-format (reg-no-width-default-qword 8
				        :include 'reg-no-width
                                        :default-printer '(:name :tab reg))
  (reg   :type 'reg-b-default-qword))

;;; Same as rex-reg-no-width, but with a default operand size of :qword.
(sb!disassem:define-instruction-format (rex-reg-no-width-default-qword 16
				        :include 'rex-reg-no-width
                                        :default-printer '(:name :tab reg))
  (reg     :type 'reg-b-default-qword))

(sb!disassem:define-instruction-format (modrm-reg-no-width 24
				     :default-printer '(:name :tab reg))
  (rex     :field (byte 4 4)    :value #b0100)
  (wrxb    :field (byte 4 0)    :type 'wrxb)
  (ff   :field (byte 8 8)  :value #b11111111)
  (mod	 :field (byte 2 22))
  (modrm-reg :field (byte 3 19))
  (reg     :field (byte 3 16)   :type 'reg-b)
  ;; optional fields
  (accum :type 'accum)
  (imm))

;;; Adds a width field to reg-no-width. Note that we can't use
;;; :INCLUDE 'REG-NO-WIDTH here to save typing because that would put
;;; the WIDTH field last, but the prefilter for WIDTH must run before
;;; the one for IMM to be able to determine the correct size of IMM.
(sb!disassem:define-instruction-format (reg 8
					:default-printer '(:name :tab reg))
  (op    :field (byte 4 4))
  (width :field (byte 1 3) :type 'width)
  (reg   :field (byte 3 0) :type 'reg-b)
  ;; optional fields
  (accum :type 'accum)
  (imm))

(sb!disassem:define-instruction-format (rex-reg 16
					:default-printer '(:name :tab reg))
  (rex     :field (byte 4 4)    :value #b0100)
  (wrxb    :field (byte 4 0)    :type 'wrxb)
  (width   :field (byte 1 11)   :type 'width)
  (op      :field (byte 4 12))
  (reg     :field (byte 3 8)    :type 'reg-b)
  ;; optional fields
  (accum   :type 'accum)
  (imm))

(sb!disassem:define-instruction-format (two-bytes 16
					:default-printer '(:name))
  (op :fields (list (byte 8 0) (byte 8 8))))

(sb!disassem:define-instruction-format (reg-reg/mem 16
					:default-printer
					`(:name :tab reg ", " reg/mem))
  (op      :field (byte 7 1))
  (width   :field (byte 1 0)	:type 'width)
  (reg/mem :fields (list (byte 2 14) (byte 3 8))
	   			:type 'reg/mem)
  (reg     :field (byte 3 11)	:type 'reg)
  ;; optional fields
  (imm))

(sb!disassem:define-instruction-format (rex-reg-reg/mem 24
					:default-printer
					`(:name :tab reg ", " reg/mem))
  (rex     :field (byte 4 4)    :value #b0100)
  (wrxb    :field (byte 4 0)    :type 'wrxb)
  (width   :field (byte 1 8)	:type 'width)
  (op      :field (byte 7 9))
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
	                        :type 'reg/mem)
  (reg     :field (byte 3 19)	:type 'reg)
  ;; optional fields
  (imm))

;;; same as reg-reg/mem, but with direction bit
(sb!disassem:define-instruction-format (reg-reg/mem-dir 16
					:include 'reg-reg/mem
					:default-printer
					`(:name
					  :tab
					  ,(swap-if 'dir 'reg/mem ", " 'reg)))
  (op  :field (byte 6 2))
  (dir :field (byte 1 1)))

(sb!disassem:define-instruction-format (rex-reg-reg/mem-dir 24
					:include 'rex-reg-reg/mem
					:default-printer
					`(:name
					  :tab
					  ,(swap-if 'dir 'reg/mem ", " 'reg)))
  (rex     :field (byte 4 4)    :value #b0100)
  (wrxb    :field (byte 4 0)    :type 'wrxb)
  (op  :field (byte 6 10))
  (dir :field (byte 1 9)))

;;; Same as reg-reg/mem, but uses the reg field as a second op code.
(sb!disassem:define-instruction-format (reg/mem 16
					:default-printer '(:name :tab reg/mem))
  (op      :fields (list (byte 7 1) (byte 3 11)))
  (width   :field (byte 1 0)	:type 'width)
  (reg/mem :fields (list (byte 2 14) (byte 3 8))
	   			:type 'sized-reg/mem)
  ;; optional fields
  (imm))

(sb!disassem:define-instruction-format (rex-reg/mem 24
					:default-printer '(:name :tab reg/mem))
  (rex     :field (byte 4 4)    :value #b0100)
  (wrxb    :field (byte 4 0)    :type 'wrxb)
  (op      :fields (list (byte 7 9) (byte 3 19)))
  (width   :field (byte 1 8)	:type 'width)
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
                                :type 'sized-reg/mem)
  ;; optional fields
  (imm))

;;; Same as reg/mem, but without a width field and with a default
;;; operand size of :qword.
(sb!disassem:define-instruction-format (reg/mem-default-qword 16
                                        :default-printer '(:name :tab reg/mem))
  (op      :fields (list (byte 8 0) (byte 3 11)))
  (reg/mem :fields (list (byte 2 14) (byte 3 8))
	   			:type 'sized-reg/mem-default-qword))

(sb!disassem:define-instruction-format (rex-reg/mem-default-qword 24
                                        :default-printer '(:name :tab reg/mem))
  (rex     :field (byte 4 4)    :value #b0100)
  (wrxb    :field (byte 4 0)    :type 'wrxb)
  (op      :fields (list (byte 8 8) (byte 3 19)))
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
                                :type 'sized-reg/mem-default-qword))

;;; Same as reg/mem, but with the immediate value occurring by default,
;;; and with an appropiate printer.
(sb!disassem:define-instruction-format (reg/mem-imm 16
					:include 'reg/mem
					:default-printer
					'(:name :tab reg/mem ", " imm))
  (reg/mem :type 'sized-reg/mem)
  (imm     :type 'signed-imm-data))

(sb!disassem:define-instruction-format (rex-reg/mem-imm 24
					:include 'rex-reg/mem
					:default-printer
					'(:name :tab reg/mem ", " imm))
  (reg/mem :type 'sized-reg/mem)
  (imm     :type 'signed-imm-data))

;;; Same as reg/mem, but with using the accumulator in the default printer
(sb!disassem:define-instruction-format
    (accum-reg/mem 16
     :include 'reg/mem :default-printer '(:name :tab accum ", " reg/mem))
  (reg/mem :type 'reg/mem)		; don't need a size
  (accum :type 'accum))

;;; Same as reg-reg/mem, but with a prefix of #b00001111
(sb!disassem:define-instruction-format (ext-reg-reg/mem 24
					:default-printer
					`(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0)	:value #b00001111)
  (op      :field (byte 7 9))
  (width   :field (byte 1 8)	:type 'width)
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
	   			:type 'reg/mem)
  (reg     :field (byte 3 19)	:type 'reg)
  ;; optional fields
  (imm))

(sb!disassem:define-instruction-format (ext-reg-reg/mem-no-width 24
					:default-printer
					`(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0)	:value #b00001111)
  (op      :field (byte 8 8))
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
	   			:type 'reg/mem)
  (reg     :field (byte 3 19)	:type 'reg))

(sb!disassem:define-instruction-format (rex-ext-reg-reg/mem-no-width 32
					:default-printer
					`(:name :tab reg ", " reg/mem))
  (rex     :field (byte 4 4)    :value #b0100)
  (wrxb    :field (byte 4 0)    :type 'wrxb)
  (prefix  :field (byte 8 8)	:value #b00001111)
  (op      :field (byte 8 16))
  (reg/mem :fields (list (byte 2 30) (byte 3 24))
	   			:type 'reg/mem)
  (reg     :field (byte 3 27)	:type 'reg))

;;; Same as reg-reg/mem, but with a prefix of #xf2 0f
(sb!disassem:define-instruction-format (xmm-ext-reg-reg/mem 32
					:default-printer
					`(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0)	:value #xf2)
  (prefix2  :field (byte 8 8)	:value #x0f)
  (op      :field (byte 7 17))
  (width   :field (byte 1 16)	:type 'width)
  (reg/mem :fields (list (byte 2 30) (byte 3 24))
	   			:type 'reg/mem)
  (reg     :field (byte 3 27)	:type 'reg)
  ;; optional fields
  (imm))

;;; reg-no-width with #x0f prefix
(sb!disassem:define-instruction-format (ext-reg-no-width 16
				        :default-printer '(:name :tab reg))
  (prefix  :field (byte 8 0)	:value #b00001111)
  (op	 :field (byte 5 11))
  (reg   :field (byte 3 8) :type 'reg-b))

;;; Same as reg/mem, but with a prefix of #b00001111
(sb!disassem:define-instruction-format (ext-reg/mem 24
					:default-printer '(:name :tab reg/mem))
  (prefix  :field (byte 8 0)	:value #b00001111)
  (op      :fields (list (byte 7 9) (byte 3 19)))
  (width   :field (byte 1 8)	:type 'width)
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
	   			:type 'sized-reg/mem)
  ;; optional fields
  (imm))

(sb!disassem:define-instruction-format (ext-reg/mem-imm 24
                                        :include 'ext-reg/mem
					:default-printer
                                        '(:name :tab reg/mem ", " imm))
  (imm :type 'signed-imm-data))

;;;; This section was added by jrd, for fp instructions.

;;; regular fp inst to/from registers/memory
(sb!disassem:define-instruction-format (floating-point 16
					:default-printer
					`(:name :tab reg/mem))
  (prefix :field (byte 5 3) :value #b11011)
  (op     :fields (list (byte 3 0) (byte 3 11)))
  (reg/mem :fields (list (byte 2 14) (byte 3 8)) :type 'reg/mem))

;;; fp insn to/from fp reg
(sb!disassem:define-instruction-format (floating-point-fp 16
					:default-printer `(:name :tab fp-reg))
  (prefix :field (byte 5 3) :value #b11011)
  (suffix :field (byte 2 14) :value #b11)
  (op     :fields (list (byte 3 0) (byte 3 11)))
  (fp-reg :field (byte 3 8) :type 'fp-reg))

;;; fp insn to/from fp reg, with the reversed source/destination flag.
(sb!disassem:define-instruction-format
 (floating-point-fp-d 16
   :default-printer `(:name :tab ,(swap-if 'd "ST0" ", " 'fp-reg)))
  (prefix :field (byte 5 3) :value #b11011)
  (suffix :field (byte 2 14) :value #b11)
  (op     :fields (list (byte 2 0) (byte 3 11)))
  (d      :field (byte 1 2))
  (fp-reg :field (byte 3 8) :type 'fp-reg))


;;; (added by (?) pfw)
;;; fp no operand isns
(sb!disassem:define-instruction-format (floating-point-no 16
				      :default-printer '(:name))
  (prefix :field (byte 8  0) :value #b11011001)
  (suffix :field (byte 3 13) :value #b111)
  (op     :field (byte 5  8)))

(sb!disassem:define-instruction-format (floating-point-3 16
				      :default-printer '(:name))
  (prefix :field (byte 5 3) :value #b11011)
  (suffix :field (byte 2 14) :value #b11)
  (op     :fields (list (byte 3 0) (byte 6 8))))

(sb!disassem:define-instruction-format (floating-point-5 16
				      :default-printer '(:name))
  (prefix :field (byte 8  0) :value #b11011011)
  (suffix :field (byte 3 13) :value #b111)
  (op     :field (byte 5  8)))

(sb!disassem:define-instruction-format (floating-point-st 16
				      :default-printer '(:name))
  (prefix :field (byte 8  0) :value #b11011111)
  (suffix :field (byte 3 13) :value #b111)
  (op     :field (byte 5  8)))

(sb!disassem:define-instruction-format (string-op 8
				     :include 'simple
				     :default-printer '(:name width)))

(sb!disassem:define-instruction-format (rex-string-op 16
				     :include 'rex-simple
				     :default-printer '(:name width)))

(sb!disassem:define-instruction-format (short-cond-jump 16)
  (op    :field (byte 4 4))
  (cc	 :field (byte 4 0) :type 'condition-code)
  (label :field (byte 8 8) :type 'displacement))

(sb!disassem:define-instruction-format (short-jump 16
				     :default-printer '(:name :tab label))
  (const :field (byte 4 4) :value #b1110)
  (op	 :field (byte 4 0))
  (label :field (byte 8 8) :type 'displacement))

(sb!disassem:define-instruction-format (near-cond-jump 16)
  (op    :fields (list (byte 8 0) (byte 4 12)) :value '(#b00001111 #b1000))
  (cc	 :field (byte 4 8) :type 'condition-code)
  ;; The disassembler currently doesn't let you have an instruction > 32 bits
  ;; long, so we fake it by using a prefilter to read the offset.
  (label :type 'displacement
	 :prefilter (lambda (value dstate)
		      (declare (ignore value)) ; always nil anyway
		      (sb!disassem:read-signed-suffix 32 dstate))))

(sb!disassem:define-instruction-format (near-jump 8
				     :default-printer '(:name :tab label))
  (op    :field (byte 8 0))
  ;; The disassembler currently doesn't let you have an instruction > 32 bits
  ;; long, so we fake it by using a prefilter to read the address.
  (label :type 'displacement
	 :prefilter (lambda (value dstate)
		      (declare (ignore value)) ; always nil anyway
		      (sb!disassem:read-signed-suffix 32 dstate))))


(sb!disassem:define-instruction-format (cond-set 24
				     :default-printer '('set cc :tab reg/mem))
  (prefix :field (byte 8 0) :value #b00001111)
  (op    :field (byte 4 12) :value #b1001)
  (cc	 :field (byte 4 8) :type 'condition-code)
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
	   :type 'sized-byte-reg/mem)
  (reg     :field (byte 3 19)	:value #b000))

(sb!disassem:define-instruction-format (cond-move 24
                                     :default-printer
                                        '('cmov cc :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0)    :value #b00001111)
  (op      :field (byte 4 12)   :value #b0100)
  (cc      :field (byte 4 8)    :type 'condition-code)
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
                                :type 'reg/mem)
  (reg     :field (byte 3 19)   :type 'reg))

(sb!disassem:define-instruction-format (enter-format 32
				     :default-printer '(:name
							:tab disp
							(:unless (:constant 0)
							  ", " level)))
  (op :field (byte 8 0))
  (disp :field (byte 16 8))
  (level :field (byte 8 24)))

;;; Single byte instruction with an immediate byte argument.
(sb!disassem:define-instruction-format (byte-imm 16
				     :default-printer '(:name :tab code))
 (op :field (byte 8 0))
 (code :field (byte 8 8)))

;;;; primitive emitters

(define-bitfield-emitter emit-word 16
  (byte 16 0))

(define-bitfield-emitter emit-dword 32
  (byte 32 0))

(define-bitfield-emitter emit-qword 64
  (byte 64 0))

(define-bitfield-emitter emit-byte-with-reg 8
  (byte 5 3) (byte 3 0))

(define-bitfield-emitter emit-mod-reg-r/m-byte 8
  (byte 2 6) (byte 3 3) (byte 3 0))

(define-bitfield-emitter emit-sib-byte 8
  (byte 2 6) (byte 3 3) (byte 3 0))

(define-bitfield-emitter emit-rex-byte 8
  (byte 4 4) (byte 1 3) (byte 1 2) (byte 1 1) (byte 1 0))



;;;; fixup emitters

(defun emit-absolute-fixup (segment fixup &optional quad-p)
  (note-fixup segment (if quad-p :absolute64 :absolute) fixup)
  (let ((offset (fixup-offset fixup)))
    (if (label-p offset)
	(emit-back-patch segment
			 (if quad-p 8 4)
			 (lambda (segment posn)
			   (declare (ignore posn))
			   (let ((val  (- (+ (component-header-length)
					     (or (label-position offset)
						 0))
					  other-pointer-lowtag)))
			     (if quad-p
				 (emit-qword segment val )
				 (emit-dword segment val )))))
	(if quad-p
	    (emit-qword segment (or offset 0))
	    (emit-dword segment (or offset 0))))))

(defun emit-relative-fixup (segment fixup)
  (note-fixup segment :relative fixup)
  (emit-dword segment (or (fixup-offset fixup) 0)))


;;;; the effective-address (ea) structure

(defun reg-tn-encoding (tn)
  (declare (type tn tn))
  (aver (member  (sb-name (sc-sb (tn-sc tn))) '(registers float-registers)))
  ;; ea only has space for three bits of register number: regs r8
  ;; and up are selected by a REX prefix byte which caller is responsible
  ;; for having emitted where necessary already
  (cond ((fp-reg-tn-p tn)
	 (mod (tn-offset tn) 8))
	(t
	 (let ((offset (mod (tn-offset tn) 16)))
	   (logior (ash (logand offset 1) 2)
		   (ash offset -1))))))
  
(defstruct (ea (:constructor make-ea (size &key base index scale disp))
	       (:copier nil))
  ;; note that we can represent an EA qith a QWORD size, but EMIT-EA
  ;; can't actually emit it on its own: caller also needs to emit REX
  ;; prefix
  (size nil :type (member :byte :word :dword :qword))
  (base nil :type (or tn null))
  (index nil :type (or tn null))
  (scale 1 :type (member 1 2 4 8))
  (disp 0 :type (or (unsigned-byte 32) (signed-byte 32) fixup)))
(def!method print-object ((ea ea) stream)
  (cond ((or *print-escape* *print-readably*)
	 (print-unreadable-object (ea stream :type t)
	   (format stream
		   "~S~@[ base=~S~]~@[ index=~S~]~@[ scale=~S~]~@[ disp=~S~]"
		   (ea-size ea)
		   (ea-base ea)
		   (ea-index ea)
		   (let ((scale (ea-scale ea)))
		     (if (= scale 1) nil scale))
		   (ea-disp ea))))
	(t
	 (format stream "~A PTR [" (symbol-name (ea-size ea)))
	 (when (ea-base ea)
	   (write-string (sb!c::location-print-name (ea-base ea)) stream)
	   (when (ea-index ea)
	     (write-string "+" stream)))
	 (when (ea-index ea)
	   (write-string (sb!c::location-print-name (ea-index ea)) stream))
	 (unless (= (ea-scale ea) 1)
	   (format stream "*~A" (ea-scale ea)))
	 (typecase (ea-disp ea)
	   (null)
	   (integer
	    (format stream "~@D" (ea-disp ea)))
	   (t
	    (format stream "+~A" (ea-disp ea))))
	 (write-char #\] stream))))

(defun emit-constant-tn-rip (segment constant-tn reg)
  ;; AMD64 doesn't currently have a code object register to use as a
  ;; base register for constant access. Instead we use RIP-relative
  ;; addressing. The offset from the SIMPLE-FUN-HEADER to the instruction
  ;; is passed to the backpatch callback. In addition we need the offset
  ;; from the start of the function header to the slot in the CODE-HEADER
  ;; that stores the constant. Since we don't know where the code header
  ;; starts, instead count backwards from the function header.
  (let* ((2comp (component-info *component-being-compiled*))
	 (constants (ir2-component-constants 2comp))
	 (len (length constants))
	 ;; Both CODE-HEADER and SIMPLE-FUN-HEADER are 16-byte aligned.
	 ;; If there are an even amount of constants, there will be
	 ;; an extra qword of padding before the function header, which
	 ;; needs to be adjusted for. XXX: This will break if new slots
	 ;; are added to the code header.
	 (offset (* (- (+ len (if (evenp len)
				  1
				  2))
		       (tn-offset constant-tn))
		    n-word-bytes)))
    ;; RIP-relative addressing
    (emit-mod-reg-r/m-byte segment #b00 reg #b101)
    (emit-back-patch segment
		     4
		     (lambda (segment posn)
		       ;; The addressing is relative to end of instruction,
		       ;; i.e. the end of this dword. Hence the + 4.
		       (emit-dword segment (+ 4 (- (+ offset posn)))))))
  (values))

(defun emit-label-rip (segment fixup reg)
  (let ((label (fixup-offset fixup)))
    ;; RIP-relative addressing
    (emit-mod-reg-r/m-byte segment #b00 reg #b101)
    (emit-back-patch segment
		     4
		     (lambda (segment posn)
		       (emit-dword segment (- (label-position label)
					      (+ posn 4))))))
  (values))

(defun emit-ea (segment thing reg &optional allow-constants)
  (etypecase thing
    (tn
     ;; this would be eleganter if we had a function that would create
     ;; an ea given a tn
     (ecase (sb-name (sc-sb (tn-sc thing)))
       ((registers float-registers)
	(emit-mod-reg-r/m-byte segment #b11 reg (reg-tn-encoding thing)))
       (stack
	;; Convert stack tns into an index off RBP.
	(let ((disp (- (* (1+ (tn-offset thing)) n-word-bytes))))
	  (cond ((< -128 disp 127)
		 (emit-mod-reg-r/m-byte segment #b01 reg #b101)
		 (emit-byte segment disp))
		(t
		 (emit-mod-reg-r/m-byte segment #b10 reg #b101)
		 (emit-dword segment disp)))))
       (constant
	(unless allow-constants
	  ;; Why?
	  (error
	   "Constant TNs can only be directly used in MOV, PUSH, and CMP."))
	(emit-constant-tn-rip segment thing reg))))
    (ea
     (let* ((base (ea-base thing))
	    (index (ea-index thing))
	    (scale (ea-scale thing))
	    (disp (ea-disp thing))
	    (mod (cond ((or (null base)
			    (and (eql disp 0)
				 (not (= (reg-tn-encoding base) #b101))))
			#b00)
		       ((and (fixnump disp) (<= -128 disp 127))
			#b01)
		       (t
			#b10)))
	    (r/m (cond (index #b100)
		       ((null base) #b101)
		       (t (reg-tn-encoding base)))))
       (when (and (= mod 0) (= r/m #b101))
	 ;; this is rip-relative in amd64, so we'll use a sib instead
	 (setf r/m #b100 scale 1))
       (emit-mod-reg-r/m-byte segment mod reg r/m)
       (when (= r/m #b100)
	 (let ((ss (1- (integer-length scale)))
	       (index (if (null index)
			  #b100
			  (let ((index (reg-tn-encoding index)))
			    (if (= index #b100)
				(error "can't index off of ESP")
				index))))
	       (base (if (null base)
			 #b101
			 (reg-tn-encoding base))))
	   (emit-sib-byte segment ss index base)))
       (cond ((= mod #b01)
	      (emit-byte segment disp))
	     ((or (= mod #b10) (null base))
	      (if (fixup-p disp)
		  (emit-absolute-fixup segment disp)
		  (emit-dword segment disp))))))
    (fixup
     (typecase (fixup-offset thing)
       (label
	(emit-label-rip segment thing reg))
       (t
	(emit-mod-reg-r/m-byte segment #b00 reg #b100)
	(emit-sib-byte segment 0 #b100 #b101)
	(emit-absolute-fixup segment thing))))))

(defun fp-reg-tn-p (thing)
  (and (tn-p thing)
       (eq (sb-name (sc-sb (tn-sc thing))) 'float-registers)))

;;; like the above, but for fp-instructions--jrd
(defun emit-fp-op (segment thing op)
  (if (fp-reg-tn-p thing)
      (emit-byte segment (dpb op (byte 3 3) (dpb (tn-offset thing)
						 (byte 3 0)
						 #b11000000)))
    (emit-ea segment thing op)))

(defun byte-reg-p (thing)
  (and (tn-p thing)
       (eq (sb-name (sc-sb (tn-sc thing))) 'registers)
       (member (sc-name (tn-sc thing)) *byte-sc-names*)
       t))

(defun byte-ea-p (thing)
  (typecase thing
    (ea (eq (ea-size thing) :byte))
    (tn
     (and (member (sc-name (tn-sc thing)) *byte-sc-names*) t))
    (t nil)))

(defun word-reg-p (thing)
  (and (tn-p thing)
       (eq (sb-name (sc-sb (tn-sc thing))) 'registers)
       (member (sc-name (tn-sc thing)) *word-sc-names*)
       t))

(defun word-ea-p (thing)
  (typecase thing
    (ea (eq (ea-size thing) :word))
    (tn (and (member (sc-name (tn-sc thing)) *word-sc-names*) t))
    (t nil)))

(defun dword-reg-p (thing)
  (and (tn-p thing)
       (eq (sb-name (sc-sb (tn-sc thing))) 'registers)
       (member (sc-name (tn-sc thing)) *dword-sc-names*)
       t))

(defun dword-ea-p (thing)
  (typecase thing
    (ea (eq (ea-size thing) :dword))
    (tn
     (and (member (sc-name (tn-sc thing)) *dword-sc-names*) t))
    (t nil)))

(defun qword-reg-p (thing)
  (and (tn-p thing)
       (eq (sb-name (sc-sb (tn-sc thing))) 'registers)
       (member (sc-name (tn-sc thing)) *qword-sc-names*)
       t))

(defun qword-ea-p (thing)
  (typecase thing
    (ea (eq (ea-size thing) :qword))
    (tn
     (and (member (sc-name (tn-sc thing)) *qword-sc-names*) t))
    (t nil)))

(defun register-p (thing)
  (and (tn-p thing)
       (eq (sb-name (sc-sb (tn-sc thing))) 'registers)))

(defun accumulator-p (thing)
  (and (register-p thing)
       (= (tn-offset thing) 0)))


;;;; utilities

(def!constant +operand-size-prefix-byte+ #b01100110)

(defun maybe-emit-operand-size-prefix (segment size)
  (unless (or (eq size :byte) 
	      (eq size :qword)		; REX prefix handles this
	      (eq size +default-operand-size+))
    (emit-byte segment +operand-size-prefix-byte+)))

(defun maybe-emit-rex-prefix (segment operand-size r x b)
  (labels ((if-hi (r)
	     (if (and r (> (tn-offset r)
			   ;; offset of r8 is 16, offset of xmm8 is 8
			   (if (fp-reg-tn-p r)
			       7
			       15)))
		 1
		 0)))
    (let ((rex-w (if (eq operand-size :qword) 1 0))
	  (rex-r (if-hi r))
	  (rex-x (if-hi x))
	  (rex-b (if-hi b)))
      (when (or (eq operand-size :byte) ;; REX needed to access SIL/DIL
		(not (zerop (logior rex-w rex-r rex-x rex-b))))
	(emit-rex-byte segment #b0100 rex-w rex-r rex-x rex-b)))))

(defun maybe-emit-rex-for-ea (segment ea reg &key operand-size)
  (let ((ea-p (ea-p ea)))		;emit-ea can also be called with a tn
    (maybe-emit-rex-prefix segment
			   (or operand-size (operand-size ea))
			   reg
			   (and ea-p (ea-index ea))
			   (cond (ea-p (ea-base ea))
				 ((and (tn-p ea)
				       (member (sb-name (sc-sb (tn-sc ea))) 
					       '(float-registers registers)))
				  ea)
				 (t nil)))))

(defun operand-size (thing)
  (typecase thing
    (tn
     ;; FIXME: might as well be COND instead of having to use #. readmacro
     ;; to hack up the code
     (case (sc-name (tn-sc thing))
       (#.*qword-sc-names*
	:qword)
       (#.*dword-sc-names*
	:dword)
       (#.*word-sc-names*
	:word)
       (#.*byte-sc-names*
	:byte)
       ;; added by jrd: float-registers is a separate size (?)
       (#.*float-sc-names*
	:float)
       (#.*double-sc-names*
	:double)
       (t
	(error "can't tell the size of ~S ~S" thing (sc-name (tn-sc thing))))))
    (ea
     (ea-size thing))
    (fixup
     ;; GNA.  Guess who spelt "flavor" correctly first time round?
     ;; There's a strong argument in my mind to change all uses of
     ;; "flavor" to "kind": and similarly with some misguided uses of
     ;; "type" here and there.  -- CSR, 2005-01-06.
     (case (fixup-flavor thing)
       ((:foreign-dataref) :qword)))
    (t
     nil)))

(defun matching-operand-size (dst src)
  (let ((dst-size (operand-size dst))
	(src-size (operand-size src)))
    (if dst-size
	(if src-size
	    (if (eq dst-size src-size)
		dst-size
		(error "size mismatch: ~S is a ~S and ~S is a ~S."
		       dst dst-size src src-size))
	    dst-size)
	(if src-size
	    src-size
	    (error "can't tell the size of either ~S or ~S" dst src)))))

(defun emit-sized-immediate (segment size value &optional quad-p)
  (ecase size
    (:byte
     (emit-byte segment value))
    (:word
     (emit-word segment value))
    ((:dword :qword)
     ;; except in a very few cases (MOV instructions A1,A3,B8) we expect
     ;; dword data bytes even when 64 bit work is being done.  So, mostly
     ;; we treat quad constants as dwords.
     (if (and quad-p (eq size :qword))
	 (emit-qword segment value)
	 (emit-dword segment value)))))

;;;; general data transfer

(define-instruction mov (segment dst src)
  ;; immediate to register
  (:printer reg ((op #b1011) (imm nil :type 'signed-imm-data))
	    '(:name :tab reg ", " imm))
  (:printer rex-reg ((op #b1011) (imm nil :type 'signed-imm-data-upto-qword))
	    '(:name :tab reg ", " imm))
  ;; absolute mem to/from accumulator
  (:printer simple-dir ((op #b101000) (imm nil :type 'imm-addr))
	    `(:name :tab ,(swap-if 'dir 'accum ", " '("[" imm "]"))))
  ;; register to/from register/memory
  (:printer reg-reg/mem-dir ((op #b100010)))
  (:printer rex-reg-reg/mem-dir ((op #b100010)))
  ;; immediate to register/memory
  (:printer reg/mem-imm ((op '(#b1100011 #b000))))
  (:printer rex-reg/mem-imm ((op '(#b1100011 #b000))))

  (:emitter
   (let ((size (matching-operand-size dst src)))
     (maybe-emit-operand-size-prefix segment size)
     (cond ((register-p dst)
	    (cond ((integerp src)
		   (maybe-emit-rex-prefix segment size nil nil dst)
		   (emit-byte-with-reg segment
				       (if (eq size :byte)
					   #b10110
					   #b10111)
				       (reg-tn-encoding dst))
		   (emit-sized-immediate segment size src (eq size :qword)))
		  (t
		   (maybe-emit-rex-for-ea segment src dst)
		   (emit-byte segment
			      (if (eq size :byte)
				  #b10001010
				  #b10001011))
		   (emit-ea segment src (reg-tn-encoding dst) t))))
	   ((integerp src)
	    ;; C7 only deals with 32 bit immediates even if register is 
	    ;; 64 bit: only b8-bf use 64 bit immediates
	    (maybe-emit-rex-for-ea segment dst nil)
 	    (cond ((typep src '(or (signed-byte 32) (unsigned-byte 32)))
		   (emit-byte segment
			      (if (eq size :byte) #b11000110 #b11000111))
		   (emit-ea segment dst #b000)
		   (emit-sized-immediate segment 
					 (case size (:qword :dword) (t size))
					 src))
		  (t
		   (aver nil))))
	   ((register-p src)
	    (maybe-emit-rex-for-ea segment dst src)
	    (emit-byte segment (if (eq size :byte) #b10001000 #b10001001))
	    (emit-ea segment dst (reg-tn-encoding src)))
	   ((fixup-p src)
	    ;; Generally we can't MOV a fixupped value into an EA, since
	    ;; MOV on non-registers can only take a 32-bit immediate arg.
	    ;; Make an exception for :FOREIGN fixups (pretty much just
	    ;; the runtime asm, since other foreign calls go through the
	    ;; the linkage table) and for linkage table references, since
	    ;; these should always end up in low memory.
	    (aver (or (eq (fixup-flavor src) :foreign)
		      (eq (fixup-flavor src) :foreign-dataref)
		      (eq (ea-size dst) :dword)))
	    (maybe-emit-rex-for-ea segment dst nil)
	    (emit-byte segment #b11000111)
	    (emit-ea segment dst #b000)
	    (emit-absolute-fixup segment src))
	   (t
	    (error "bogus arguments to MOV: ~S ~S" dst src))))))

(defun emit-move-with-extension (segment dst src signed-p)
  (aver (register-p dst))
  (let ((dst-size (operand-size dst))
	(src-size (operand-size src))
	(opcode (if signed-p  #b10111110 #b10110110)))
    (ecase dst-size
      (:word
       (aver (eq src-size :byte))
       (maybe-emit-operand-size-prefix segment :word)
       (emit-byte segment #b00001111)
       (emit-byte segment opcode)
       (emit-ea segment src (reg-tn-encoding dst)))
      ((:dword :qword)
       (ecase src-size
	 (:byte
	  (maybe-emit-operand-size-prefix segment :dword)
	  (maybe-emit-rex-for-ea segment src dst
				 :operand-size (operand-size dst))
	  (emit-byte segment #b00001111)
	  (emit-byte segment opcode)
	  (emit-ea segment src (reg-tn-encoding dst)))
	 (:word
 	  (maybe-emit-rex-for-ea segment src dst
				 :operand-size (operand-size dst))
	  (emit-byte segment #b00001111)
	  (emit-byte segment (logior opcode 1))
	  (emit-ea segment src (reg-tn-encoding dst)))
	 (:dword
	  (aver (eq dst-size :qword))
	  ;; dst is in reg, src is in modrm
	  (let ((ea-p (ea-p src)))
	    (maybe-emit-rex-prefix segment (if signed-p :qword :dword) dst 
				   (and ea-p (ea-index src))
				   (cond (ea-p (ea-base src))
					 ((tn-p src) src)
					 (t nil)))
	    (emit-byte segment #x63)	;movsxd	
	    ;;(emit-byte segment opcode)
	    (emit-ea segment src (reg-tn-encoding dst)))))))))

(define-instruction movsx (segment dst src)
  (:printer ext-reg-reg/mem-no-width
            ((op #b10111110) (reg/mem nil :type 'sized-byte-reg/mem)))
  (:printer rex-ext-reg-reg/mem-no-width
            ((op #b10111110) (reg/mem nil :type 'sized-byte-reg/mem)))
  (:printer ext-reg-reg/mem-no-width
            ((op #b10111111) (reg/mem nil :type 'sized-word-reg/mem)))
  (:printer rex-ext-reg-reg/mem-no-width
            ((op #b10111111) (reg/mem nil :type 'sized-word-reg/mem)))
  (:emitter (emit-move-with-extension segment dst src :signed)))

(define-instruction movzx (segment dst src)
  (:printer ext-reg-reg/mem-no-width
            ((op #b10110110) (reg/mem nil :type 'sized-byte-reg/mem)))
  (:printer rex-ext-reg-reg/mem-no-width
            ((op #b10110110) (reg/mem nil :type 'sized-byte-reg/mem)))
  (:printer ext-reg-reg/mem-no-width
            ((op #b10110111) (reg/mem nil :type 'sized-word-reg/mem)))
  (:printer rex-ext-reg-reg/mem-no-width
            ((op #b10110111) (reg/mem nil :type 'sized-word-reg/mem)))
  (:emitter (emit-move-with-extension segment dst src nil)))

(define-instruction movsxd (segment dst src)
  (:printer rex-reg-reg/mem ((op #b0110001) (width 1)
                             (reg/mem nil :type 'sized-dword-reg/mem)))
  (:emitter (emit-move-with-extension segment dst src :signed)))

;;; this is not a real amd64 instruction, of course
(define-instruction movzxd (segment dst src)
  ; (:printer reg-reg/mem ((op #x63) (reg nil :type 'reg)))
  (:emitter (emit-move-with-extension segment dst src nil)))

(define-instruction push (segment src)
  ;; register
  (:printer reg-no-width-default-qword ((op #b01010)))
  (:printer rex-reg-no-width-default-qword ((op #b01010)))
  ;; register/memory
  (:printer reg/mem-default-qword ((op '(#b11111111 #b110))))
  (:printer rex-reg/mem-default-qword ((op '(#b11111111 #b110))))
  ;; immediate
  (:printer byte ((op #b01101010) (imm nil :type 'signed-imm-byte))
	    '(:name :tab imm))
  (:printer byte ((op #b01101000)
                  (imm nil :type 'signed-imm-data-default-qword))
	    '(:name :tab imm))
  ;; ### segment registers?

  (:emitter
   (cond ((integerp src)
	  (cond ((<= -128 src 127)
		 (emit-byte segment #b01101010)
		 (emit-byte segment src))
		(t
		 ;; AMD64 manual says no REX needed but is unclear
		 ;; whether it expects 32 or 64 bit immediate here
		 (emit-byte segment #b01101000)
		 (emit-dword segment src))))
	 (t
	  (let ((size (operand-size src)))
	    (aver (not (eq size :byte)))
	    (maybe-emit-operand-size-prefix segment size)
	    (maybe-emit-rex-for-ea segment src nil)
	    (cond ((register-p src)
		   (emit-byte-with-reg segment #b01010 (reg-tn-encoding src)))
		  (t
		   (emit-byte segment #b11111111)
		   (emit-ea segment src #b110 t))))))))

(define-instruction pusha (segment)
  (:printer byte ((op #b01100000)))
  (:emitter
   (emit-byte segment #b01100000)))

(define-instruction pop (segment dst)
  (:printer reg-no-width-default-qword ((op #b01011)))
  (:printer rex-reg-no-width-default-qword ((op #b01011)))
  (:printer reg/mem-default-qword ((op '(#b10001111 #b000))))
  (:printer rex-reg/mem-default-qword ((op '(#b10001111 #b000))))
  (:emitter
   (let ((size (operand-size dst)))
     (aver (not (eq size :byte)))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-for-ea segment dst nil)     
     (cond ((register-p dst)
	    (emit-byte-with-reg segment #b01011 (reg-tn-encoding dst)))
	   (t
	    (emit-byte segment #b10001111)
	    (emit-ea segment dst #b000))))))

(define-instruction popa (segment)
  (:printer byte ((op #b01100001)))
  (:emitter
   (emit-byte segment #b01100001)))

(define-instruction xchg (segment operand1 operand2)
  ;; Register with accumulator.
  (:printer reg-no-width ((op #b10010)) '(:name :tab accum ", " reg))
  ;; Register/Memory with Register.
  (:printer reg-reg/mem ((op #b1000011)))
  (:printer rex-reg-reg/mem ((op #b1000011)))
  (:emitter
   (let ((size (matching-operand-size operand1 operand2)))
     (maybe-emit-operand-size-prefix segment size)
     (labels ((xchg-acc-with-something (acc something)
		(if (and (not (eq size :byte)) (register-p something))
		    (progn
		      (maybe-emit-rex-for-ea segment acc something)
		      (emit-byte-with-reg segment
					  #b10010
					  (reg-tn-encoding something)))
		    (xchg-reg-with-something acc something)))
	      (xchg-reg-with-something (reg something)
		(maybe-emit-rex-for-ea segment something reg)
		(emit-byte segment (if (eq size :byte) #b10000110 #b10000111))
		(emit-ea segment something (reg-tn-encoding reg))))
       (cond ((accumulator-p operand1)
	      (xchg-acc-with-something operand1 operand2))
	     ((accumulator-p operand2)
	      (xchg-acc-with-something operand2 operand1))
	     ((register-p operand1)
	      (xchg-reg-with-something operand1 operand2))
	     ((register-p operand2)
	      (xchg-reg-with-something operand2 operand1))
	     (t
	      (error "bogus args to XCHG: ~S ~S" operand1 operand2)))))))

(define-instruction lea (segment dst src)
  (:printer rex-reg-reg/mem ((op #b1000110)))
  (:printer reg-reg/mem ((op #b1000110) (width 1)))
  (:emitter
   (aver (or (dword-reg-p dst) (qword-reg-p dst)))
   (maybe-emit-rex-for-ea segment src dst
			  :operand-size :qword)
   (emit-byte segment #b10001101)
   (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction cmpxchg (segment dst src)
  ;; Register/Memory with Register.
  (:printer ext-reg-reg/mem ((op #b1011000)) '(:name :tab reg/mem ", " reg))
  (:emitter
   (aver (register-p src))
   (let ((size (matching-operand-size src dst)))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-for-ea segment dst src)
     (emit-byte segment #b00001111)
     (emit-byte segment (if (eq size :byte) #b10110000 #b10110001))
     (emit-ea segment dst (reg-tn-encoding src)))))



(define-instruction fs-segment-prefix (segment)
  (:emitter
   (emit-byte segment #x64)))

;;;; flag control instructions

;;; CLC -- Clear Carry Flag.
(define-instruction clc (segment)
  (:printer byte ((op #b11111000)))
  (:emitter
   (emit-byte segment #b11111000)))

;;; CLD -- Clear Direction Flag.
(define-instruction cld (segment)
  (:printer byte ((op #b11111100)))
  (:emitter
   (emit-byte segment #b11111100)))

;;; CLI -- Clear Iterrupt Enable Flag.
(define-instruction cli (segment)
  (:printer byte ((op #b11111010)))
  (:emitter
   (emit-byte segment #b11111010)))

;;; CMC -- Complement Carry Flag.
(define-instruction cmc (segment)
  (:printer byte ((op #b11110101)))
  (:emitter
   (emit-byte segment #b11110101)))

;;; LAHF -- Load AH into flags.
(define-instruction lahf (segment)
  (:printer byte ((op #b10011111)))
  (:emitter
   (emit-byte segment #b10011111)))

;;; POPF -- Pop flags.
(define-instruction popf (segment)
  (:printer byte ((op #b10011101)))
  (:emitter
   (emit-byte segment #b10011101)))

;;; PUSHF -- push flags.
(define-instruction pushf (segment)
  (:printer byte ((op #b10011100)))
  (:emitter
   (emit-byte segment #b10011100)))

;;; SAHF -- Store AH into flags.
(define-instruction sahf (segment)
  (:printer byte ((op #b10011110)))
  (:emitter
   (emit-byte segment #b10011110)))

;;; STC -- Set Carry Flag.
(define-instruction stc (segment)
  (:printer byte ((op #b11111001)))
  (:emitter
   (emit-byte segment #b11111001)))

;;; STD -- Set Direction Flag.
(define-instruction std (segment)
  (:printer byte ((op #b11111101)))
  (:emitter
   (emit-byte segment #b11111101)))

;;; STI -- Set Interrupt Enable Flag.
(define-instruction sti (segment)
  (:printer byte ((op #b11111011)))
  (:emitter
   (emit-byte segment #b11111011)))

;;;; arithmetic

(defun emit-random-arith-inst (name segment dst src opcode
				    &optional allow-constants)
  (let ((size (matching-operand-size dst src)))
    (maybe-emit-operand-size-prefix segment size)
    (cond
     ((integerp src)
      (cond ((and (not (eq size :byte)) (<= -128 src 127))
	     (maybe-emit-rex-for-ea segment dst nil)
	     (emit-byte segment #b10000011)
	     (emit-ea segment dst opcode allow-constants)
	     (emit-byte segment src))
	    ((accumulator-p dst)  
	     (maybe-emit-rex-for-ea segment dst nil)
	     (emit-byte segment
			(dpb opcode
			     (byte 3 3)
			     (if (eq size :byte)
				 #b00000100
				 #b00000101)))
	     (emit-sized-immediate segment size src))
	    (t
	     (maybe-emit-rex-for-ea segment dst nil)
	     (emit-byte segment (if (eq size :byte) #b10000000 #b10000001))
	     (emit-ea segment dst opcode allow-constants)
	     (emit-sized-immediate segment size src))))
     ((register-p src)
      (maybe-emit-rex-for-ea segment dst src)
      (emit-byte segment
		 (dpb opcode
		      (byte 3 3)
		      (if (eq size :byte) #b00000000 #b00000001)))
      (emit-ea segment dst (reg-tn-encoding src) allow-constants))
     ((register-p dst)
      (maybe-emit-rex-for-ea segment src dst)
      (emit-byte segment
		 (dpb opcode
		      (byte 3 3)
		      (if (eq size :byte) #b00000010 #b00000011)))
      (emit-ea segment src (reg-tn-encoding dst) allow-constants))
     (t
      (error "bogus operands to ~A" name)))))

(eval-when (:compile-toplevel :execute)
  (defun arith-inst-printer-list (subop)
    `((accum-imm ((op ,(dpb subop (byte 3 2) #b0000010))))
      (rex-accum-imm ((op ,(dpb subop (byte 3 2) #b0000010))))
      (reg/mem-imm ((op (#b1000000 ,subop))))
      (rex-reg/mem-imm ((op (#b1000000 ,subop))))
      ;; The redundant encoding #x82 is invalid in 64-bit mode,
      ;; therefore we force WIDTH to 1.
      (reg/mem-imm ((op (#b1000001 ,subop)) (width 1)
		    (imm nil :type signed-imm-byte)))
      (rex-reg/mem-imm ((op (#b1000001 ,subop))
		    (imm nil :type signed-imm-byte)))
      (reg-reg/mem-dir ((op ,(dpb subop (byte 3 1) #b000000))))
      (rex-reg-reg/mem-dir ((op ,(dpb subop (byte 3 1) #b000000))))))
  )

(define-instruction add (segment dst src)
  (:printer-list (arith-inst-printer-list #b000))
  (:emitter (emit-random-arith-inst "ADD" segment dst src #b000)))

(define-instruction adc (segment dst src)
  (:printer-list (arith-inst-printer-list #b010))
  (:emitter (emit-random-arith-inst "ADC" segment dst src #b010)))

(define-instruction sub (segment dst src)
  (:printer-list (arith-inst-printer-list #b101))
  (:emitter (emit-random-arith-inst "SUB" segment dst src #b101)))

(define-instruction sbb (segment dst src)
  (:printer-list (arith-inst-printer-list #b011))
  (:emitter (emit-random-arith-inst "SBB" segment dst src #b011)))

(define-instruction cmp (segment dst src)
  (:printer-list (arith-inst-printer-list #b111))
  (:emitter (emit-random-arith-inst "CMP" segment dst src #b111 t)))

(define-instruction inc (segment dst)
  ;; Register
  (:printer modrm-reg-no-width ((modrm-reg #b000)))
  ;; Register/Memory
  ;; (:printer rex-reg/mem ((op '(#b11111111 #b001))))
  (:printer reg/mem ((op '(#b1111111 #b000))))
  (:emitter
   (let ((size (operand-size dst)))
     (maybe-emit-operand-size-prefix segment size)
     (cond #+nil ; these opcodes become REX prefixes in x86-64
	   ((and (not (eq size :byte)) (register-p dst))
	    (emit-byte-with-reg segment #b01000 (reg-tn-encoding dst)))
	   (t
	    (maybe-emit-rex-for-ea segment dst nil)
	    (emit-byte segment (if (eq size :byte) #b11111110 #b11111111))
	    (emit-ea segment dst #b000))))))

(define-instruction dec (segment dst)
  ;; Register.
  (:printer modrm-reg-no-width ((modrm-reg #b001)))
  ;; Register/Memory
  (:printer reg/mem ((op '(#b1111111 #b001))))
  (:emitter
   (let ((size (operand-size dst)))
     (maybe-emit-operand-size-prefix segment size)
     (cond #+nil
	   ((and (not (eq size :byte)) (register-p dst))
	    (emit-byte-with-reg segment #b01001 (reg-tn-encoding dst)))
	   (t
	    (maybe-emit-rex-for-ea segment dst nil)
	    (emit-byte segment (if (eq size :byte) #b11111110 #b11111111))
	    (emit-ea segment dst #b001))))))

(define-instruction neg (segment dst)
  (:printer reg/mem ((op '(#b1111011 #b011))))
  (:emitter
   (let ((size (operand-size dst)))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-for-ea segment dst nil)
     (emit-byte segment (if (eq size :byte) #b11110110 #b11110111))
     (emit-ea segment dst #b011))))

(define-instruction mul (segment dst src)
  (:printer accum-reg/mem ((op '(#b1111011 #b100))))
  (:emitter
   (let ((size (matching-operand-size dst src)))
     (aver (accumulator-p dst))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-for-ea segment src nil)
     (emit-byte segment (if (eq size :byte) #b11110110 #b11110111))
     (emit-ea segment src #b100))))

(define-instruction imul (segment dst &optional src1 src2)
  (:printer accum-reg/mem ((op '(#b1111011 #b101))))
  (:printer ext-reg-reg/mem-no-width ((op #b10101111)))
  (:printer rex-ext-reg-reg/mem-no-width ((op #b10101111)))
  (:printer reg-reg/mem ((op #b0110100) (width 1)
                         (imm nil :type 'signed-imm-data))
	    '(:name :tab reg ", " reg/mem ", " imm))
  (:printer rex-reg-reg/mem ((op #b0110100) (width 1)
                             (imm nil :type 'signed-imm-data))
	    '(:name :tab reg ", " reg/mem ", " imm))
  (:printer reg-reg/mem ((op #b0110101) (width 1)
			 (imm nil :type 'signed-imm-byte))
	    '(:name :tab reg ", " reg/mem ", " imm))
  (:printer rex-reg-reg/mem ((op #b0110101) (width 1)
                             (imm nil :type 'signed-imm-byte))
	    '(:name :tab reg ", " reg/mem ", " imm))
  (:emitter
   (flet ((r/m-with-immed-to-reg (reg r/m immed)
	    (let* ((size (matching-operand-size reg r/m))
		   (sx (and (not (eq size :byte)) (<= -128 immed 127))))
	      (maybe-emit-operand-size-prefix segment size)
	      (maybe-emit-rex-for-ea segment r/m reg)
	      (emit-byte segment (if sx #b01101011 #b01101001))
	      (emit-ea segment r/m (reg-tn-encoding reg))
	      (if sx
		  (emit-byte segment immed)
		  (emit-sized-immediate segment size immed)))))
     (cond (src2
	    (r/m-with-immed-to-reg dst src1 src2))
	   (src1
	    (if (integerp src1)
		(r/m-with-immed-to-reg dst dst src1)
		(let ((size (matching-operand-size dst src1)))
		  (maybe-emit-operand-size-prefix segment size)
		  (maybe-emit-rex-for-ea segment src1 dst)
		  (emit-byte segment #b00001111)
		  (emit-byte segment #b10101111)
		  (emit-ea segment src1 (reg-tn-encoding dst)))))
	   (t
	    (let ((size (operand-size dst)))
	      (maybe-emit-operand-size-prefix segment size)
	      (maybe-emit-rex-for-ea segment dst nil)
	      (emit-byte segment (if (eq size :byte) #b11110110 #b11110111))
	      (emit-ea segment dst #b101)))))))

(define-instruction div (segment dst src)
  (:printer accum-reg/mem ((op '(#b1111011 #b110))))
  (:emitter
   (let ((size (matching-operand-size dst src)))
     (aver (accumulator-p dst))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-for-ea segment src nil)
     (emit-byte segment (if (eq size :byte) #b11110110 #b11110111))
     (emit-ea segment src #b110))))

(define-instruction idiv (segment dst src)
  (:printer accum-reg/mem ((op '(#b1111011 #b111))))
  (:emitter
   (let ((size (matching-operand-size dst src)))
     (aver (accumulator-p dst))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-for-ea segment src nil)
     (emit-byte segment (if (eq size :byte) #b11110110 #b11110111))
     (emit-ea segment src #b111))))

(define-instruction bswap (segment dst)
  (:printer ext-reg-no-width ((op #b11001)))
  (:emitter
   (let ((size (operand-size dst)))
     (maybe-emit-rex-prefix segment size nil nil dst)
     (emit-byte segment #x0f)
     (emit-byte-with-reg segment #b11001 (reg-tn-encoding dst)))))

;;; CBW -- Convert Byte to Word. AX <- sign_xtnd(AL)
(define-instruction cbw (segment)
  (:emitter
   (maybe-emit-operand-size-prefix segment :word)
   (emit-byte segment #b10011000)))

;;; CWDE -- Convert Word To Double Word Extened. EAX <- sign_xtnd(AX)
(define-instruction cwde (segment)
  (:emitter
   (maybe-emit-operand-size-prefix segment :dword)
   (emit-byte segment #b10011000)))

;;; CWD -- Convert Word to Double Word. DX:AX <- sign_xtnd(AX)
(define-instruction cwd (segment)
  (:emitter
   (maybe-emit-operand-size-prefix segment :word)
   (emit-byte segment #b10011001)))

;;; CDQ -- Convert Double Word to Quad Word. EDX:EAX <- sign_xtnd(EAX)
(define-instruction cdq (segment)
  (:printer byte ((op #b10011001)))
  (:emitter
   (maybe-emit-operand-size-prefix segment :dword)
   (emit-byte segment #b10011001)))

;;; CQO -- Convert Quad or Octaword. RDX:RAX <- sign_xtnd(RAX)
(define-instruction cqo (segment)
  (:emitter
   (maybe-emit-rex-prefix segment :qword nil nil nil)
   (emit-byte segment #b10011001)))

(define-instruction xadd (segment dst src)
  ;; Register/Memory with Register.
  (:printer ext-reg-reg/mem ((op #b1100000)) '(:name :tab reg/mem ", " reg))
  (:emitter
   (aver (register-p src))
   (let ((size (matching-operand-size src dst)))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-for-ea segment dst src)
     (emit-byte segment #b00001111)
     (emit-byte segment (if (eq size :byte) #b11000000 #b11000001))
     (emit-ea segment dst (reg-tn-encoding src)))))


;;;; logic

(defun emit-shift-inst (segment dst amount opcode)
  (let ((size (operand-size dst)))
    (maybe-emit-operand-size-prefix segment size)
    (multiple-value-bind (major-opcode immed)
	(case amount
	  (:cl (values #b11010010 nil))
	  (1 (values #b11010000 nil))
	  (t (values #b11000000 t)))
      (maybe-emit-rex-for-ea segment dst nil)
      (emit-byte segment
		 (if (eq size :byte) major-opcode (logior major-opcode 1)))
      (emit-ea segment dst opcode)
      (when immed
	(emit-byte segment amount)))))

(eval-when (:compile-toplevel :execute)
  (defun shift-inst-printer-list (subop)
    `((reg/mem ((op (#b1101000 ,subop)))
	       (:name :tab reg/mem ", 1"))
      (rex-reg/mem ((op (#b1101000 ,subop)))
		   (:name :tab reg/mem ", 1"))
      (reg/mem ((op (#b1101001 ,subop)))
	       (:name :tab reg/mem ", " 'cl))
      (rex-reg/mem ((op (#b1101001 ,subop)))
	       (:name :tab reg/mem ", " 'cl))
      (reg/mem-imm ((op (#b1100000 ,subop))
		    (imm nil :type imm-byte)))
      (rex-reg/mem-imm ((op (#b1100000 ,subop))
		    (imm nil :type imm-byte))))))

(define-instruction rol (segment dst amount)
  (:printer-list
   (shift-inst-printer-list #b000))
  (:emitter
   (emit-shift-inst segment dst amount #b000)))

(define-instruction ror (segment dst amount)
  (:printer-list
   (shift-inst-printer-list #b001))
  (:emitter
   (emit-shift-inst segment dst amount #b001)))

(define-instruction rcl (segment dst amount)
  (:printer-list
   (shift-inst-printer-list #b010))
  (:emitter
   (emit-shift-inst segment dst amount #b010)))

(define-instruction rcr (segment dst amount)
  (:printer-list
   (shift-inst-printer-list #b011))
  (:emitter
   (emit-shift-inst segment dst amount #b011)))

(define-instruction shl (segment dst amount)
  (:printer-list
   (shift-inst-printer-list #b100))
  (:emitter
   (emit-shift-inst segment dst amount #b100)))

(define-instruction shr (segment dst amount)
  (:printer-list
   (shift-inst-printer-list #b101))
  (:emitter
   (emit-shift-inst segment dst amount #b101)))

(define-instruction sar (segment dst amount)
  (:printer-list
   (shift-inst-printer-list #b111))
  (:emitter
   (emit-shift-inst segment dst amount #b111)))

(defun emit-double-shift (segment opcode dst src amt)
  (let ((size (matching-operand-size dst src)))
    (when (eq size :byte)
      (error "Double shifts can only be used with words."))
    (maybe-emit-operand-size-prefix segment size)
    (maybe-emit-rex-for-ea segment dst src)
    (emit-byte segment #b00001111)
    (emit-byte segment (dpb opcode (byte 1 3)
			    (if (eq amt :cl) #b10100101 #b10100100)))
    (emit-ea segment dst (reg-tn-encoding src))	
    (unless (eq amt :cl)
      (emit-byte segment amt))))

(eval-when (:compile-toplevel :execute)
  (defun double-shift-inst-printer-list (op)
    `(#+nil
      (ext-reg-reg/mem-imm ((op ,(logior op #b100))
			    (imm nil :type signed-imm-byte)))
      (ext-reg-reg/mem ((op ,(logior op #b101)))
	 (:name :tab reg/mem ", " 'cl)))))

(define-instruction shld (segment dst src amt)
  (:declare (type (or (member :cl) (mod 32)) amt))
  (:printer-list (double-shift-inst-printer-list #b10100000))
  (:emitter
   (emit-double-shift segment #b0 dst src amt)))

(define-instruction shrd (segment dst src amt)
  (:declare (type (or (member :cl) (mod 32)) amt))
  (:printer-list (double-shift-inst-printer-list #b10101000))
  (:emitter
   (emit-double-shift segment #b1 dst src amt)))

(define-instruction and (segment dst src)
  (:printer-list
   (arith-inst-printer-list #b100))
  (:emitter
   (emit-random-arith-inst "AND" segment dst src #b100)))

(define-instruction test (segment this that)
  (:printer accum-imm ((op #b1010100)))
  (:printer rex-accum-imm ((op #b1010100)))
  (:printer reg/mem-imm ((op '(#b1111011 #b000))))
  (:printer rex-reg/mem-imm ((op '(#b1111011 #b000))))
  (:printer reg-reg/mem ((op #b1000010)))
  (:printer rex-reg-reg/mem ((op #b1000010)))
  (:emitter
   (let ((size (matching-operand-size this that)))
     (maybe-emit-operand-size-prefix segment size)
     (flet ((test-immed-and-something (immed something)
	      (cond ((accumulator-p something)
		     (maybe-emit-rex-for-ea segment something nil)
		     (emit-byte segment
				(if (eq size :byte) #b10101000 #b10101001))
		     (emit-sized-immediate segment size immed))
		    (t
		     (maybe-emit-rex-for-ea segment something nil)
		     (emit-byte segment
				(if (eq size :byte) #b11110110 #b11110111))
		     (emit-ea segment something #b000)
		     (emit-sized-immediate segment size immed))))
	    (test-reg-and-something (reg something)
	      (maybe-emit-rex-for-ea segment something reg)
	      (emit-byte segment (if (eq size :byte) #b10000100 #b10000101))
	      (emit-ea segment something (reg-tn-encoding reg))))
       (cond ((integerp that)
	      (test-immed-and-something that this))
	     ((integerp this)
	      (test-immed-and-something this that))
	     ((register-p this)
	      (test-reg-and-something this that))
	     ((register-p that)
	      (test-reg-and-something that this))
	     (t
	      (error "bogus operands for TEST: ~S and ~S" this that)))))))

(define-instruction or (segment dst src)
  (:printer-list
   (arith-inst-printer-list #b001))
  (:emitter
   (emit-random-arith-inst "OR" segment dst src #b001)))

(define-instruction xor (segment dst src)
  (:printer-list
   (arith-inst-printer-list #b110))
  (:emitter
   (emit-random-arith-inst "XOR" segment dst src #b110)))

(define-instruction not (segment dst)
  (:printer reg/mem ((op '(#b1111011 #b010))))
  (:emitter
   (let ((size (operand-size dst)))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-for-ea segment dst nil)
     (emit-byte segment (if (eq size :byte) #b11110110 #b11110111))
     (emit-ea segment dst #b010))))

;;;; string manipulation

(define-instruction cmps (segment size)
  (:printer string-op ((op #b1010011)))
  (:printer rex-string-op ((op #b1010011)))
  (:emitter
   (maybe-emit-operand-size-prefix segment size)
   (maybe-emit-rex-prefix segment size nil nil nil)
   (emit-byte segment (if (eq size :byte) #b10100110 #b10100111))))

(define-instruction ins (segment acc)
  (:printer string-op ((op #b0110110)))
  (:printer rex-string-op ((op #b0110110)))
  (:emitter
   (let ((size (operand-size acc)))
     (aver (accumulator-p acc))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-prefix segment size nil nil nil)
     (emit-byte segment (if (eq size :byte) #b01101100 #b01101101)))))

(define-instruction lods (segment acc)
  (:printer string-op ((op #b1010110)))
  (:printer rex-string-op ((op #b1010110)))
  (:emitter
   (let ((size (operand-size acc)))
     (aver (accumulator-p acc))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-prefix segment size nil nil nil)
     (emit-byte segment (if (eq size :byte) #b10101100 #b10101101)))))

(define-instruction movs (segment size)
  (:printer string-op ((op #b1010010)))
  (:printer rex-string-op ((op #b1010010)))
  (:emitter
   (maybe-emit-operand-size-prefix segment size)
   (maybe-emit-rex-prefix segment size nil nil nil)
   (emit-byte segment (if (eq size :byte) #b10100100 #b10100101))))

(define-instruction outs (segment acc)
  (:printer string-op ((op #b0110111)))
  (:printer rex-string-op ((op #b0110111)))
  (:emitter
   (let ((size (operand-size acc)))
     (aver (accumulator-p acc))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-prefix segment size nil nil nil)
     (emit-byte segment (if (eq size :byte) #b01101110 #b01101111)))))

(define-instruction scas (segment acc)
  (:printer string-op ((op #b1010111)))
  (:printer rex-string-op ((op #b1010111)))
  (:emitter
   (let ((size (operand-size acc)))
     (aver (accumulator-p acc))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-prefix segment size nil nil nil)
     (emit-byte segment (if (eq size :byte) #b10101110 #b10101111)))))

(define-instruction stos (segment acc)
  (:printer string-op ((op #b1010101)))
  (:printer rex-string-op ((op #b1010101)))
  (:emitter
   (let ((size (operand-size acc)))
     (aver (accumulator-p acc))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-prefix segment size nil nil nil)
     (emit-byte segment (if (eq size :byte) #b10101010 #b10101011)))))

(define-instruction xlat (segment)
  (:printer byte ((op #b11010111)))
  (:emitter
   (emit-byte segment #b11010111)))

(define-instruction rep (segment)
  (:emitter
   (emit-byte segment #b11110010)))

(define-instruction repe (segment)
  (:printer byte ((op #b11110011)))
  (:emitter
   (emit-byte segment #b11110011)))

(define-instruction repne (segment)
  (:printer byte ((op #b11110010)))
  (:emitter
   (emit-byte segment #b11110010)))


;;;; bit manipulation

(define-instruction bsf (segment dst src)
  (:printer ext-reg-reg/mem ((op #b1011110) (width 0)))
  (:emitter
   (let ((size (matching-operand-size dst src)))
     (when (eq size :byte)
       (error "can't scan bytes: ~S" src))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-for-ea segment src dst)
     (emit-byte segment #b00001111)
     (emit-byte segment #b10111100)
     (emit-ea segment src (reg-tn-encoding dst)))))

(define-instruction bsr (segment dst src)
  (:printer ext-reg-reg/mem ((op #b1011110) (width 1)))
  (:emitter
   (let ((size (matching-operand-size dst src)))
     (when (eq size :byte)
       (error "can't scan bytes: ~S" src))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-for-ea segment src dst)
     (emit-byte segment #b00001111)
     (emit-byte segment #b10111101)
     (emit-ea segment src (reg-tn-encoding dst)))))

(defun emit-bit-test-and-mumble (segment src index opcode)
  (let ((size (operand-size src)))
    (when (eq size :byte)
      (error "can't scan bytes: ~S" src))
    (maybe-emit-operand-size-prefix segment size)
    (cond ((integerp index)
	   (maybe-emit-rex-for-ea segment src nil)
	   (emit-byte segment #b00001111)
	   (emit-byte segment #b10111010)
	   (emit-ea segment src opcode)
	   (emit-byte segment index))
	  (t
	   (maybe-emit-rex-for-ea segment src index)
	   (emit-byte segment #b00001111)
	   (emit-byte segment (dpb opcode (byte 3 3) #b10000011))
	   (emit-ea segment src (reg-tn-encoding index))))))

(eval-when (:compile-toplevel :execute)
  (defun bit-test-inst-printer-list (subop)
    `((ext-reg/mem-imm ((op (#b1011101 ,subop))
                        (reg/mem nil :type reg/mem)
                        (imm nil :type imm-byte)
                        (width 0)))
      (ext-reg-reg/mem ((op ,(dpb subop (byte 3 2) #b1000001))
                        (width 1))
                       (:name :tab reg/mem ", " reg)))))

(define-instruction bt (segment src index)
  (:printer-list (bit-test-inst-printer-list #b100))
  (:emitter
   (emit-bit-test-and-mumble segment src index #b100)))

(define-instruction btc (segment src index)
  (:printer-list (bit-test-inst-printer-list #b111))
  (:emitter
   (emit-bit-test-and-mumble segment src index #b111)))

(define-instruction btr (segment src index)
  (:printer-list (bit-test-inst-printer-list #b110))
  (:emitter
   (emit-bit-test-and-mumble segment src index #b110)))

(define-instruction bts (segment src index)
  (:printer-list (bit-test-inst-printer-list #b101))
  (:emitter
   (emit-bit-test-and-mumble segment src index #b101)))


;;;; control transfer

(define-instruction call (segment where)
  (:printer near-jump ((op #b11101000)))
  (:printer reg/mem-default-qword ((op '(#b11111111 #b010))))
  (:printer rex-reg/mem-default-qword ((op '(#b11111111 #b010))))
  (:emitter
   (typecase where
     (label
      (maybe-emit-rex-for-ea segment where nil)
      (emit-byte segment #b11101000) ; 32 bit relative
      (emit-back-patch segment
		       4
		       (lambda (segment posn)
			 (emit-dword segment
				     (- (label-position where)
					(+ posn 4))))))
     (fixup
      (maybe-emit-rex-for-ea segment where nil)
      (emit-byte segment #b11101000)
      (emit-relative-fixup segment where))
     (t
      (maybe-emit-rex-for-ea segment where nil)
      (emit-byte segment #b11111111)
      (emit-ea segment where #b010)))))

(defun emit-byte-displacement-backpatch (segment target)
  (emit-back-patch segment
		   1
		   (lambda (segment posn)
		     (let ((disp (- (label-position target) (1+ posn))))
		       (aver (<= -128 disp 127))
		       (emit-byte segment disp)))))

(define-instruction jmp (segment cond &optional where)
  ;; conditional jumps
  (:printer short-cond-jump ((op #b0111)) '('j cc :tab label))
  (:printer near-cond-jump () '('j cc :tab label))
  ;; unconditional jumps
  (:printer short-jump ((op #b1011)))
  (:printer near-jump ((op #b11101001)) )
  (:printer reg/mem-default-qword ((op '(#b11111111 #b100))))
  (:printer rex-reg/mem-default-qword ((op '(#b11111111 #b100))))
  (:emitter
   (cond (where
	  (emit-chooser
	   segment 6 2
	   (lambda (segment posn delta-if-after)
	     (let ((disp (- (label-position where posn delta-if-after)
			    (+ posn 2))))
	       (when (<= -128 disp 127)
		 (emit-byte segment
			    (dpb (conditional-opcode cond)
				 (byte 4 0)
				 #b01110000))
		 (emit-byte-displacement-backpatch segment where)
		 t)))
	   (lambda (segment posn)
	     (let ((disp (- (label-position where) (+ posn 6))))
	       (emit-byte segment #b00001111)
	       (emit-byte segment
			  (dpb (conditional-opcode cond)
			       (byte 4 0)
			       #b10000000))
	       (emit-dword segment disp)))))
	 ((label-p (setq where cond))
	  (emit-chooser
	   segment 5 0
	   (lambda (segment posn delta-if-after)
	     (let ((disp (- (label-position where posn delta-if-after)
			    (+ posn 2))))
	       (when (<= -128 disp 127)
		 (emit-byte segment #b11101011)
		 (emit-byte-displacement-backpatch segment where)
		 t)))
	   (lambda (segment posn)
	     (let ((disp (- (label-position where) (+ posn 5))))
	       (emit-byte segment #b11101001)
	       (emit-dword segment disp)))))
	 ((fixup-p where)
	  (emit-byte segment #b11101001)
	  (emit-relative-fixup segment where))
	 (t
	  (unless (or (ea-p where) (tn-p where))
		  (error "don't know what to do with ~A" where))
	  ;; near jump defaults to 64 bit
	  ;; w-bit in rex prefix is unnecessary 
	  (maybe-emit-rex-for-ea segment where nil :operand-size :dword)
	  (emit-byte segment #b11111111)
	  (emit-ea segment where #b100)))))

(define-instruction jmp-short (segment label)
  (:emitter
   (emit-byte segment #b11101011)
   (emit-byte-displacement-backpatch segment label)))

(define-instruction ret (segment &optional stack-delta)
  (:printer byte ((op #b11000011)))
  (:printer byte ((op #b11000010) (imm nil :type 'imm-word-16))
	    '(:name :tab imm))
  (:emitter
   (cond (stack-delta
	  (emit-byte segment #b11000010)
	  (emit-word segment stack-delta))
	 (t
	  (emit-byte segment #b11000011)))))

(define-instruction jecxz (segment target)
  (:printer short-jump ((op #b0011)))
  (:emitter
   (emit-byte segment #b11100011)
   (emit-byte-displacement-backpatch segment target)))

(define-instruction loop (segment target)
  (:printer short-jump ((op #b0010)))
  (:emitter
   (emit-byte segment #b11100010)	; pfw this was 11100011, or jecxz!!!!
   (emit-byte-displacement-backpatch segment target)))

(define-instruction loopz (segment target)
  (:printer short-jump ((op #b0001)))
  (:emitter
   (emit-byte segment #b11100001)
   (emit-byte-displacement-backpatch segment target)))

(define-instruction loopnz (segment target)
  (:printer short-jump ((op #b0000)))
  (:emitter
   (emit-byte segment #b11100000)
   (emit-byte-displacement-backpatch segment target)))

;;;; conditional move
(define-instruction cmov (segment cond dst src)
  (:printer cond-move ())
  (:emitter
   (aver (register-p dst))
   (let ((size (matching-operand-size dst src)))
     (aver (or (eq size :word) (eq size :dword) (eq size :qword) ))
     (maybe-emit-operand-size-prefix segment size))
   (maybe-emit-rex-for-ea segment src dst)
   (emit-byte segment #b00001111)
   (emit-byte segment (dpb (conditional-opcode cond) (byte 4 0) #b01000000))
   (emit-ea segment src (reg-tn-encoding dst))))

;;;; conditional byte set

(define-instruction set (segment dst cond)
  (:printer cond-set ())
  (:emitter
   (maybe-emit-rex-for-ea segment dst nil)
   (emit-byte segment #b00001111)
   (emit-byte segment (dpb (conditional-opcode cond) (byte 4 0) #b10010000))
   (emit-ea segment dst #b000)))

;;;; enter/leave

(define-instruction enter (segment disp &optional (level 0))
  (:declare (type (unsigned-byte 16) disp)
	    (type (unsigned-byte 8) level))
  (:printer enter-format ((op #b11001000)))
  (:emitter
   (emit-byte segment #b11001000)
   (emit-word segment disp)
   (emit-byte segment level)))

(define-instruction leave (segment)
  (:printer byte ((op #b11001001)))
  (:emitter
   (emit-byte segment #b11001001)))

;;;; interrupt instructions

(defun snarf-error-junk (sap offset &optional length-only)
  (let* ((length (sb!sys:sap-ref-8 sap offset))
	 (vector (make-array length :element-type '(unsigned-byte 8))))
    (declare (type sb!sys:system-area-pointer sap)
	     (type (unsigned-byte 8) length)
	     (type (simple-array (unsigned-byte 8) (*)) vector))
    (cond (length-only
	   (values 0 (1+ length) nil nil))
	  (t
	   (sb!kernel:copy-from-system-area sap (* n-byte-bits (1+ offset))
					    vector (* n-word-bits
						      vector-data-offset)
					    (* length n-byte-bits))
	   (collect ((sc-offsets)
		     (lengths))
	     (lengths 1)		; the length byte
	     (let* ((index 0)
		    (error-number (sb!c:read-var-integer vector index)))
	       (lengths index)
	       (loop
		 (when (>= index length)
		   (return))
		 (let ((old-index index))
		   (sc-offsets (sb!c:read-var-integer vector index))
		   (lengths (- index old-index))))
	       (values error-number
		       (1+ length)
		       (sc-offsets)
		       (lengths))))))))

#|
(defmacro break-cases (breaknum &body cases)
  (let ((bn-temp (gensym)))
    (collect ((clauses))
      (dolist (case cases)
	(clauses `((= ,bn-temp ,(car case)) ,@(cdr case))))
      `(let ((,bn-temp ,breaknum))
	 (cond ,@(clauses))))))
|#

(defun break-control (chunk inst stream dstate)
  (declare (ignore inst))
  (flet ((nt (x) (if stream (sb!disassem:note x dstate))))
    ;; FIXME: Make sure that BYTE-IMM-CODE is defined. The genesis
    ;; map has it undefined; and it should be easier to look in the target
    ;; Lisp (with (DESCRIBE 'BYTE-IMM-CODE)) than to definitively deduce
    ;; from first principles whether it's defined in some way that genesis
    ;; can't grok.
    (case (byte-imm-code chunk dstate)
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
       (nt "function end breakpoint trap")))))

(define-instruction break (segment code)
  (:declare (type (unsigned-byte 8) code))
  (:printer byte-imm ((op #b11001100)) '(:name :tab code)
	    :control #'break-control)
  (:emitter
   (emit-byte segment #b11001100)
   (emit-byte segment code)))

(define-instruction int (segment number)
  (:declare (type (unsigned-byte 8) number))
  (:printer byte-imm ((op #b11001101)))
  (:emitter
   (etypecase number
     ((member 3)
      (emit-byte segment #b11001100))
     ((unsigned-byte 8)
      (emit-byte segment #b11001101)
      (emit-byte segment number)))))

(define-instruction into (segment)
  (:printer byte ((op #b11001110)))
  (:emitter
   (emit-byte segment #b11001110)))

(define-instruction bound (segment reg bounds)
  (:emitter
   (let ((size (matching-operand-size reg bounds)))
     (when (eq size :byte)
       (error "can't bounds-test bytes: ~S" reg))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-for-ea segment bounds reg)
     (emit-byte segment #b01100010)
     (emit-ea segment bounds (reg-tn-encoding reg)))))

(define-instruction iret (segment)
  (:printer byte ((op #b11001111)))
  (:emitter
   (emit-byte segment #b11001111)))

;;;; processor control

(define-instruction hlt (segment)
  (:printer byte ((op #b11110100)))
  (:emitter
   (emit-byte segment #b11110100)))

(define-instruction nop (segment)
  (:printer byte ((op #b10010000)))
  (:emitter
   (emit-byte segment #b10010000)))

(define-instruction wait (segment)
  (:printer byte ((op #b10011011)))
  (:emitter
   (emit-byte segment #b10011011)))

(define-instruction lock (segment)
  (:printer byte ((op #b11110000)))
  (:emitter
   (emit-byte segment #b11110000)))

;;;; miscellaneous hackery

(define-instruction byte (segment byte)
  (:emitter
   (emit-byte segment byte)))

(define-instruction word (segment word)
  (:emitter
   (emit-word segment word)))

(define-instruction dword (segment dword)
  (:emitter
   (emit-dword segment dword)))

(defun emit-header-data (segment type)
  (emit-back-patch segment
		   n-word-bytes
		   (lambda (segment posn)
		     (emit-qword segment
				 (logior type
					 (ash (+ posn
						 (component-header-length))
					      (- n-widetag-bits
						 word-shift)))))))

(define-instruction simple-fun-header-word (segment)
  (:emitter
   (emit-header-data segment simple-fun-header-widetag)))

(define-instruction lra-header-word (segment)
  (:emitter
   (emit-header-data segment return-pc-header-widetag)))

;;;; fp instructions
;;;;
;;;; Note: We treat the single-precision and double-precision variants
;;;; as separate instructions.

;;; Load single to st(0).
(define-instruction fld (segment source)
  (:printer floating-point ((op '(#b001 #b000))))
  (:emitter
    (and (not (fp-reg-tn-p source))
	 (maybe-emit-rex-for-ea segment source nil))
    (emit-byte segment #b11011001)
    (emit-fp-op segment source #b000)))

;;; Load double to st(0).
(define-instruction fldd (segment source)
  (:printer floating-point ((op '(#b101 #b000))))
  (:printer floating-point-fp ((op '(#b001 #b000))))
  (:emitter
   (if (fp-reg-tn-p source)
       (emit-byte segment #b11011001)
       (progn
	 (maybe-emit-rex-for-ea segment source nil)
	 (emit-byte segment #b11011101)))
   (emit-fp-op segment source #b000)))

;;; Load long to st(0).
(define-instruction fldl (segment source)
  (:printer floating-point ((op '(#b011 #b101))))
  (:emitter
    (and (not (fp-reg-tn-p source))
	 (maybe-emit-rex-for-ea segment source nil))
    (emit-byte segment #b11011011)
    (emit-fp-op segment source #b101)))

;;; Store single from st(0).
(define-instruction fst (segment dest)
  (:printer floating-point ((op '(#b001 #b010))))
  (:emitter
    (cond ((fp-reg-tn-p dest)
	   (emit-byte segment #b11011101)
	   (emit-fp-op segment dest #b010))
	  (t
	   (maybe-emit-rex-for-ea segment dest nil)
	   (emit-byte segment #b11011001)
	   (emit-fp-op segment dest #b010)))))

;;; Store double from st(0).
(define-instruction fstd (segment dest)
  (:printer floating-point ((op '(#b101 #b010))))
  (:printer floating-point-fp ((op '(#b101 #b010))))
  (:emitter
   (cond ((fp-reg-tn-p dest)
	  (emit-byte segment #b11011101)
	  (emit-fp-op segment dest #b010))
	 (t
	  (maybe-emit-rex-for-ea segment dest nil)
	  (emit-byte segment #b11011101)
	  (emit-fp-op segment dest #b010)))))

;;; Arithmetic ops are all done with at least one operand at top of
;;; stack. The other operand is is another register or a 32/64 bit
;;; memory loc.

;;; dtc: I've tried to follow the Intel ASM386 conventions, but note
;;; that these conflict with the Gdb conventions for binops. To reduce
;;; the confusion I've added comments showing the mathamatical
;;; operation and the two syntaxes. By the ASM386 convention the
;;; instruction syntax is:
;;;
;;;      Fop Source
;;; or   Fop Destination, Source
;;;
;;; If only one operand is given then it is the source and the
;;; destination is ST(0). There are reversed forms of the fsub and
;;; fdiv instructions inducated by an 'R' suffix.
;;;
;;; The mathematical operation for the non-reverse form is always:
;;;     destination = destination op source
;;;
;;; For the reversed form it is:
;;;     destination = source op destination
;;;
;;; The instructions below only accept one operand at present which is
;;; usually the source. I've hack in extra instructions to implement
;;; the fops with a ST(i) destination, these have a -sti suffix and
;;; the operand is the destination with the source being ST(0).

;;; Add single:
;;;   st(0) = st(0) + memory or st(i).
(define-instruction fadd (segment source)
  (:printer floating-point ((op '(#b000 #b000))))
  (:emitter
    (and (not (fp-reg-tn-p source))
	 (maybe-emit-rex-for-ea segment source nil))
    (emit-byte segment #b11011000)
    (emit-fp-op segment source #b000)))

;;; Add double:
;;;   st(0) = st(0) + memory or st(i).
(define-instruction faddd (segment source)
  (:printer floating-point ((op '(#b100 #b000))))
  (:printer floating-point-fp ((op '(#b000 #b000))))
  (:emitter
   (and (not (fp-reg-tn-p source))
	(maybe-emit-rex-for-ea segment source nil))
   (if (fp-reg-tn-p source)
       (emit-byte segment #b11011000)
     (emit-byte segment #b11011100))
   (emit-fp-op segment source #b000)))

;;; Add double destination st(i):
;;;   st(i) = st(0) + st(i).
(define-instruction fadd-sti (segment destination)
  (:printer floating-point-fp ((op '(#b100 #b000))))
  (:emitter
   (aver (fp-reg-tn-p destination))
   (emit-byte segment #b11011100)
   (emit-fp-op segment destination #b000)))
;;; with pop
(define-instruction faddp-sti (segment destination)
  (:printer floating-point-fp ((op '(#b110 #b000))))
  (:emitter
   (aver (fp-reg-tn-p destination))
   (emit-byte segment #b11011110)
   (emit-fp-op segment destination #b000)))

;;; Subtract single:
;;;   st(0) = st(0) - memory or st(i).
(define-instruction fsub (segment source)
  (:printer floating-point ((op '(#b000 #b100))))
  (:emitter
    (and (not (fp-reg-tn-p source))
	 (maybe-emit-rex-for-ea segment source nil))
    (emit-byte segment #b11011000)
    (emit-fp-op segment source #b100)))

;;; Subtract single, reverse:
;;;   st(0) = memory or st(i) - st(0).
(define-instruction fsubr (segment source)
  (:printer floating-point ((op '(#b000 #b101))))
  (:emitter
    (and (not (fp-reg-tn-p source))
	 (maybe-emit-rex-for-ea segment source nil))
    (emit-byte segment #b11011000)
    (emit-fp-op segment source #b101)))

;;; Subtract double:
;;;   st(0) = st(0) - memory or st(i).
(define-instruction fsubd (segment source)
  (:printer floating-point ((op '(#b100 #b100))))
  (:printer floating-point-fp ((op '(#b000 #b100))))
  (:emitter
   (if (fp-reg-tn-p source)
       (emit-byte segment #b11011000)
       (progn
	 (and (not (fp-reg-tn-p source))
	      (maybe-emit-rex-for-ea segment source nil))
	 (emit-byte segment #b11011100)))
   (emit-fp-op segment source #b100)))

;;; Subtract double, reverse:
;;;   st(0) = memory or st(i) - st(0).
(define-instruction fsubrd (segment source)
  (:printer floating-point ((op '(#b100 #b101))))
  (:printer floating-point-fp ((op '(#b000 #b101))))
  (:emitter
   (if (fp-reg-tn-p source)
       (emit-byte segment #b11011000)
       (progn
	 (and (not (fp-reg-tn-p source))
	      (maybe-emit-rex-for-ea segment source nil))
	 (emit-byte segment #b11011100)))
   (emit-fp-op segment source #b101)))

;;; Subtract double, destination st(i):
;;;   st(i) = st(i) - st(0).
;;;
;;; ASM386 syntax: FSUB ST(i), ST
;;; Gdb    syntax: fsubr %st,%st(i)
(define-instruction fsub-sti (segment destination)
  (:printer floating-point-fp ((op '(#b100 #b101))))
  (:emitter
   (aver (fp-reg-tn-p destination))
   (emit-byte segment #b11011100)
   (emit-fp-op segment destination #b101)))
;;; with a pop
(define-instruction fsubp-sti (segment destination)
  (:printer floating-point-fp ((op '(#b110 #b101))))
  (:emitter
   (aver (fp-reg-tn-p destination))
   (emit-byte segment #b11011110)
   (emit-fp-op segment destination #b101)))

;;; Subtract double, reverse, destination st(i):
;;;   st(i) = st(0) - st(i).
;;;
;;; ASM386 syntax: FSUBR ST(i), ST
;;; Gdb    syntax: fsub %st,%st(i)
(define-instruction fsubr-sti (segment destination)
  (:printer floating-point-fp ((op '(#b100 #b100))))
  (:emitter
   (aver (fp-reg-tn-p destination))
   (emit-byte segment #b11011100)
   (emit-fp-op segment destination #b100)))
;;; with a pop
(define-instruction fsubrp-sti (segment destination)
  (:printer floating-point-fp ((op '(#b110 #b100))))
  (:emitter
   (aver (fp-reg-tn-p destination))
   (emit-byte segment #b11011110)
   (emit-fp-op segment destination #b100)))

;;; Multiply single:
;;;   st(0) = st(0) * memory or st(i).
(define-instruction fmul (segment source)
  (:printer floating-point ((op '(#b000 #b001))))
  (:emitter
   (and (not (fp-reg-tn-p source))
	(maybe-emit-rex-for-ea segment source nil))
   (emit-byte segment #b11011000)
   (emit-fp-op segment source #b001)))

;;; Multiply double:
;;;   st(0) = st(0) * memory or st(i).
(define-instruction fmuld (segment source)
  (:printer floating-point ((op '(#b100 #b001))))
  (:printer floating-point-fp ((op '(#b000 #b001))))
  (:emitter
   (if (fp-reg-tn-p source)
       (emit-byte segment #b11011000)
       (progn
	 (and (not (fp-reg-tn-p source))
	      (maybe-emit-rex-for-ea segment source nil))
	 (emit-byte segment #b11011100)))
   (emit-fp-op segment source #b001)))

;;; Multiply double, destination st(i):
;;;   st(i) = st(i) * st(0).
(define-instruction fmul-sti (segment destination)
  (:printer floating-point-fp ((op '(#b100 #b001))))
  (:emitter
   (aver (fp-reg-tn-p destination))
   (emit-byte segment #b11011100)
   (emit-fp-op segment destination #b001)))

;;; Divide single:
;;;   st(0) = st(0) / memory or st(i).
(define-instruction fdiv (segment source)
  (:printer floating-point ((op '(#b000 #b110))))
  (:emitter
   (and (not (fp-reg-tn-p source))
	(maybe-emit-rex-for-ea segment source nil))
   (emit-byte segment #b11011000)
   (emit-fp-op segment source #b110)))

;;; Divide single, reverse:
;;;   st(0) = memory or st(i) / st(0).
(define-instruction fdivr (segment source)
  (:printer floating-point ((op '(#b000 #b111))))
  (:emitter
   (and (not (fp-reg-tn-p source))
	(maybe-emit-rex-for-ea segment source nil))
   (emit-byte segment #b11011000)
   (emit-fp-op segment source #b111)))

;;; Divide double:
;;;   st(0) = st(0) / memory or st(i).
(define-instruction fdivd (segment source)
  (:printer floating-point ((op '(#b100 #b110))))
  (:printer floating-point-fp ((op '(#b000 #b110))))
  (:emitter
   (if (fp-reg-tn-p source)
       (emit-byte segment #b11011000)
       (progn
	 (and (not (fp-reg-tn-p source))
	      (maybe-emit-rex-for-ea segment source nil))
	 (emit-byte segment #b11011100)))
   (emit-fp-op segment source #b110)))

;;; Divide double, reverse:
;;;   st(0) = memory or st(i) / st(0).
(define-instruction fdivrd (segment source)
  (:printer floating-point ((op '(#b100 #b111))))
  (:printer floating-point-fp ((op '(#b000 #b111))))
  (:emitter
   (if (fp-reg-tn-p source)
       (emit-byte segment #b11011000)
       (progn 
	 (and (not (fp-reg-tn-p source))
	      (maybe-emit-rex-for-ea segment source nil))
	 (emit-byte segment #b11011100)))
   (emit-fp-op segment source #b111)))

;;; Divide double, destination st(i):
;;;   st(i) = st(i) / st(0).
;;;
;;; ASM386 syntax: FDIV ST(i), ST
;;; Gdb    syntax: fdivr %st,%st(i)
(define-instruction fdiv-sti (segment destination)
  (:printer floating-point-fp ((op '(#b100 #b111))))
  (:emitter
   (aver (fp-reg-tn-p destination))
   (emit-byte segment #b11011100)
   (emit-fp-op segment destination #b111)))

;;; Divide double, reverse, destination st(i):
;;;   st(i) = st(0) / st(i).
;;;
;;; ASM386 syntax: FDIVR ST(i), ST
;;; Gdb    syntax: fdiv %st,%st(i)
(define-instruction fdivr-sti (segment destination)
  (:printer floating-point-fp ((op '(#b100 #b110))))
  (:emitter
   (aver (fp-reg-tn-p destination))
   (emit-byte segment #b11011100)
   (emit-fp-op segment destination #b110)))

;;; Exchange fr0 with fr(n). (There is no double precision variant.)
(define-instruction fxch (segment source)
  (:printer floating-point-fp ((op '(#b001 #b001))))
  (:emitter
    (unless (and (tn-p source)
		 (eq (sb-name (sc-sb (tn-sc source))) 'float-registers))
      (cl:break))
    (emit-byte segment #b11011001)
    (emit-fp-op segment source #b001)))

;;; Push 32-bit integer to st0.
(define-instruction fild (segment source)
  (:printer floating-point ((op '(#b011 #b000))))
  (:emitter
    (and (not (fp-reg-tn-p source))
	 (maybe-emit-rex-for-ea segment source nil))
    (emit-byte segment #b11011011)
    (emit-fp-op segment source #b000)))

;;; Push 64-bit integer to st0.
(define-instruction fildl (segment source)
  (:printer floating-point ((op '(#b111 #b101))))
  (:emitter
    (and (not (fp-reg-tn-p source))
	 (maybe-emit-rex-for-ea segment source nil))
    (emit-byte segment #b11011111)
    (emit-fp-op segment source #b101)))

;;; Store 32-bit integer.
(define-instruction fist (segment dest)
  (:printer floating-point ((op '(#b011 #b010))))
  (:emitter
   (and (not (fp-reg-tn-p dest))
	(maybe-emit-rex-for-ea segment dest nil))
   (emit-byte segment #b11011011)
   (emit-fp-op segment dest #b010)))

;;; Store and pop 32-bit integer.
(define-instruction fistp (segment dest)
  (:printer floating-point ((op '(#b011 #b011))))
  (:emitter
   (and (not (fp-reg-tn-p dest))
	(maybe-emit-rex-for-ea segment dest nil))
   (emit-byte segment #b11011011)
   (emit-fp-op segment dest #b011)))

;;; Store and pop 64-bit integer.
(define-instruction fistpl (segment dest)
  (:printer floating-point ((op '(#b111 #b111))))
  (:emitter
   (and (not (fp-reg-tn-p dest))
	(maybe-emit-rex-for-ea segment dest nil))
   (emit-byte segment #b11011111)
   (emit-fp-op segment dest #b111)))

;;; Store single from st(0) and pop.
(define-instruction fstp (segment dest)
  (:printer floating-point ((op '(#b001 #b011))))
  (:emitter
   (cond ((fp-reg-tn-p dest)
	  (emit-byte segment #b11011101)
	  (emit-fp-op segment dest #b011))
	 (t
	  (maybe-emit-rex-for-ea segment dest nil)
	  (emit-byte segment #b11011001)
	  (emit-fp-op segment dest #b011)))))

;;; Store double from st(0) and pop.
(define-instruction fstpd (segment dest)
  (:printer floating-point ((op '(#b101 #b011))))
  (:printer floating-point-fp ((op '(#b101 #b011))))
  (:emitter
   (cond ((fp-reg-tn-p dest)
	  (emit-byte segment #b11011101)
	  (emit-fp-op segment dest #b011))
	 (t
	  (maybe-emit-rex-for-ea segment dest nil)
	  (emit-byte segment #b11011101)
	  (emit-fp-op segment dest #b011)))))

;;; Store long from st(0) and pop.
(define-instruction fstpl (segment dest)
  (:printer floating-point ((op '(#b011 #b111))))
  (:emitter
   (and (not (fp-reg-tn-p dest))
	(maybe-emit-rex-for-ea segment dest nil))
   (emit-byte segment #b11011011)
   (emit-fp-op segment dest #b111)))

;;; Decrement stack-top pointer.
(define-instruction fdecstp (segment)
  (:printer floating-point-no ((op #b10110)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11110110)))

;;; Increment stack-top pointer.
(define-instruction fincstp (segment)
  (:printer floating-point-no ((op #b10111)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11110111)))

;;; Free fp register.
(define-instruction ffree (segment dest)
  (:printer floating-point-fp ((op '(#b101 #b000))))
  (:emitter
   (and (not (fp-reg-tn-p dest))
	(maybe-emit-rex-for-ea segment dest nil))
   (emit-byte segment #b11011101)
   (emit-fp-op segment dest #b000)))

(define-instruction fabs (segment)
  (:printer floating-point-no ((op #b00001)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11100001)))

(define-instruction fchs (segment)
  (:printer floating-point-no ((op #b00000)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11100000)))

(define-instruction frndint(segment)
  (:printer floating-point-no ((op #b11100)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11111100)))

;;; Initialize NPX.
(define-instruction fninit(segment)
  (:printer floating-point-5 ((op #b00011)))
  (:emitter
   (emit-byte segment #b11011011)
   (emit-byte segment #b11100011)))

;;; Store Status Word to AX.
(define-instruction fnstsw(segment)
  (:printer floating-point-st ((op #b00000)))
  (:emitter
   (emit-byte segment #b11011111)
   (emit-byte segment #b11100000)))

;;; Load Control Word.
;;;
;;; src must be a memory location
(define-instruction fldcw(segment src)
  (:printer floating-point ((op '(#b001 #b101))))
  (:emitter
   (and (not (fp-reg-tn-p src))
	(maybe-emit-rex-for-ea segment src nil))
   (emit-byte segment #b11011001)
   (emit-fp-op segment src #b101)))

;;; Store Control Word.
(define-instruction fnstcw(segment dst)
  (:printer floating-point ((op '(#b001 #b111))))
  (:emitter
   (and (not (fp-reg-tn-p dst))
	(maybe-emit-rex-for-ea segment dst nil))
   (emit-byte segment #b11011001)
   (emit-fp-op segment dst #b111)))

;;; Store FP Environment.
(define-instruction fstenv(segment dst)
  (:printer floating-point ((op '(#b001 #b110))))
  (:emitter
   (and (not (fp-reg-tn-p dst))
	(maybe-emit-rex-for-ea segment dst nil))
   (emit-byte segment #b11011001)
   (emit-fp-op segment dst #b110)))

;;; Restore FP Environment.
(define-instruction fldenv(segment src)
  (:printer floating-point ((op '(#b001 #b100))))
  (:emitter
   (and (not (fp-reg-tn-p src))
	(maybe-emit-rex-for-ea segment src nil))
   (emit-byte segment #b11011001)
   (emit-fp-op segment src #b100)))

;;; Save FP State.
(define-instruction fsave(segment dst)
  (:printer floating-point ((op '(#b101 #b110))))
  (:emitter
   (and (not (fp-reg-tn-p dst))
	(maybe-emit-rex-for-ea segment dst nil))
   (emit-byte segment #b11011101)
   (emit-fp-op segment dst #b110)))

;;; Restore FP State.
(define-instruction frstor(segment src)
  (:printer floating-point ((op '(#b101 #b100))))
  (:emitter
   (and (not (fp-reg-tn-p src))
	(maybe-emit-rex-for-ea segment src nil))
   (emit-byte segment #b11011101)
   (emit-fp-op segment src #b100)))

;;; Clear exceptions.
(define-instruction fnclex(segment)
  (:printer floating-point-5 ((op #b00010)))
  (:emitter
   (emit-byte segment #b11011011)
   (emit-byte segment #b11100010)))

;;; comparison
(define-instruction fcom (segment src)
  (:printer floating-point ((op '(#b000 #b010))))
  (:emitter
   (and (not (fp-reg-tn-p src))
	(maybe-emit-rex-for-ea segment src nil))
   (emit-byte segment #b11011000)
   (emit-fp-op segment src #b010)))

(define-instruction fcomd (segment src)
  (:printer floating-point ((op '(#b100 #b010))))
  (:printer floating-point-fp ((op '(#b000 #b010))))
  (:emitter
   (if (fp-reg-tn-p src)
       (emit-byte segment #b11011000)
       (progn
	 (maybe-emit-rex-for-ea segment src nil)
	 (emit-byte segment #b11011100)))
   (emit-fp-op segment src #b010)))

;;; Compare ST1 to ST0, popping the stack twice.
(define-instruction fcompp (segment)
  (:printer floating-point-3 ((op '(#b110 #b011001))))
  (:emitter
   (emit-byte segment #b11011110)
   (emit-byte segment #b11011001)))

;;; unordered comparison
(define-instruction fucom (segment src)
  (:printer floating-point-fp ((op '(#b101 #b100))))
  (:emitter
   (aver (fp-reg-tn-p src))
   (emit-byte segment #b11011101)
   (emit-fp-op segment src #b100)))

(define-instruction ftst (segment)
  (:printer floating-point-no ((op #b00100)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11100100)))

;;;; 80387 specials

(define-instruction fsqrt(segment)
  (:printer floating-point-no ((op #b11010)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11111010)))

(define-instruction fscale(segment)
  (:printer floating-point-no ((op #b11101)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11111101)))

(define-instruction fxtract(segment)
  (:printer floating-point-no ((op #b10100)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11110100)))

(define-instruction fsin(segment)
  (:printer floating-point-no ((op #b11110)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11111110)))

(define-instruction fcos(segment)
  (:printer floating-point-no ((op #b11111)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11111111)))

(define-instruction fprem1(segment)
  (:printer floating-point-no ((op #b10101)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11110101)))

(define-instruction fprem(segment)
  (:printer floating-point-no ((op #b11000)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11111000)))

(define-instruction fxam (segment)
  (:printer floating-point-no ((op #b00101)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11100101)))

;;; These do push/pop to stack and need special handling
;;; in any VOPs that use them. See the book.

;;; st0 <- st1*log2(st0)
(define-instruction fyl2x(segment)	; pops stack
  (:printer floating-point-no ((op #b10001)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11110001)))

(define-instruction fyl2xp1(segment)
  (:printer floating-point-no ((op #b11001)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11111001)))

(define-instruction f2xm1(segment)
  (:printer floating-point-no ((op #b10000)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11110000)))

(define-instruction fptan(segment)	; st(0) <- 1; st(1) <- tan
  (:printer floating-point-no ((op #b10010)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11110010)))

(define-instruction fpatan(segment)	; POPS STACK
  (:printer floating-point-no ((op #b10011)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11110011)))

;;;; loading constants

(define-instruction fldz(segment)
  (:printer floating-point-no ((op #b01110)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11101110)))

(define-instruction fld1(segment)
  (:printer floating-point-no ((op #b01000)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11101000)))

(define-instruction fldpi(segment)
  (:printer floating-point-no ((op #b01011)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11101011)))

(define-instruction fldl2t(segment)
  (:printer floating-point-no ((op #b01001)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11101001)))

(define-instruction fldl2e(segment)
  (:printer floating-point-no ((op #b01010)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11101010)))

(define-instruction fldlg2(segment)
  (:printer floating-point-no ((op #b01100)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11101100)))

(define-instruction fldln2(segment)
  (:printer floating-point-no ((op #b01101)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11101101)))

;; new xmm insns required by sse float 
;; movsd andpd comisd comiss

(define-instruction movsd (segment dst src)
;  (:printer reg-reg/mem ((op #x10) (width 1))) ;wrong
  (:emitter
   (cond ((typep src 'tn) 
	  (emit-byte segment #xf2)
	  (maybe-emit-rex-for-ea segment dst src)
	  (emit-byte segment #x0f)
	  (emit-byte segment #x11)
	  (emit-ea segment dst (reg-tn-encoding src)))
	 (t
	  (emit-byte segment #xf2)
	  (maybe-emit-rex-for-ea segment src dst)
	  (emit-byte segment #x0f)
	  (emit-byte segment #x10)
	  (emit-ea segment src (reg-tn-encoding dst))))))

(define-instruction movss (segment dst src)
;  (:printer reg-reg/mem ((op #x10) (width 1))) ;wrong
  (:emitter
   (cond ((tn-p src)
	  (emit-byte segment #xf3)
	  (maybe-emit-rex-for-ea segment dst src)
	  (emit-byte segment #x0f)
	  (emit-byte segment #x11)
	  (emit-ea segment dst (reg-tn-encoding src)))
	 (t
	  (emit-byte segment #xf3)
	  (maybe-emit-rex-for-ea segment src dst)
	  (emit-byte segment #x0f)
	  (emit-byte segment #x10)
	  (emit-ea segment src (reg-tn-encoding dst))))))

(define-instruction andpd (segment dst src)
;  (:printer reg-reg/mem ((op #x10) (width 1))) ;wrong
  (:emitter
   (emit-byte segment #x66)
   (maybe-emit-rex-for-ea segment src dst)
   (emit-byte segment #x0f)
   (emit-byte segment #x54)
   (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction andps (segment dst src)
  (:emitter
   (maybe-emit-rex-for-ea segment src dst)
   (emit-byte segment #x0f)
   (emit-byte segment #x54)
   (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction comisd (segment dst src)
;  (:printer reg-reg/mem ((op #x10) (width 1))) ;wrong
  (:emitter
   (emit-byte segment #x66)
   (maybe-emit-rex-for-ea segment src dst)
   (emit-byte segment #x0f)
   (emit-byte segment #x2f)
   (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction comiss (segment dst src)
;  (:printer reg-reg/mem ((op #x10) (width 1))) ;wrong
  (:emitter
   (maybe-emit-rex-for-ea segment src dst)
   (emit-byte segment #x0f)
   (emit-byte segment #x2f)
   (emit-ea segment src (reg-tn-encoding dst))))

;;  movd movq xorp xord

;; we only do the xmm version of movd
(define-instruction movd (segment dst src)
;  (:printer reg-reg/mem ((op #x10) (width 1))) ;wrong
  (:emitter
   (cond ((fp-reg-tn-p dst)
	  (emit-byte segment #x66)
	  (maybe-emit-rex-for-ea segment src dst)
	  (emit-byte segment #x0f)
	  (emit-byte segment #x6e)
	  (emit-ea segment src (reg-tn-encoding dst)))
	 (t
	  (aver (fp-reg-tn-p src))
	  (emit-byte segment #x66)
	  (maybe-emit-rex-for-ea segment dst src)
	  (emit-byte segment #x0f)
	  (emit-byte segment #x7e)
	  (emit-ea segment dst (reg-tn-encoding src))))))

(define-instruction movq (segment dst src)
;  (:printer reg-reg/mem ((op #x10) (width 1))) ;wrong
  (:emitter
   (cond ((fp-reg-tn-p dst)
	  (emit-byte segment #xf3)
	  (maybe-emit-rex-for-ea segment src dst)
	  (emit-byte segment #x0f)
	  (emit-byte segment #x7e)
	  (emit-ea segment src (reg-tn-encoding dst)))
	 (t
	  (aver (fp-reg-tn-p src))
	  (emit-byte segment #x66)
	  (maybe-emit-rex-for-ea segment dst src)
	  (emit-byte segment #x0f)
	  (emit-byte segment #xd6)
	  (emit-ea segment dst (reg-tn-encoding src))))))

(define-instruction xorpd (segment dst src)
;  (:printer reg-reg/mem ((op #x10) (width 1))) ;wrong
  (:emitter
   (emit-byte segment #x66)
   (maybe-emit-rex-for-ea segment src dst)
   (emit-byte segment #x0f)
   (emit-byte segment #x57)
   (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction xorps (segment dst src)
;  (:printer reg-reg/mem ((op #x10) (width 1))) ;wrong
  (:emitter
   (maybe-emit-rex-for-ea segment src dst)
   (emit-byte segment #x0f)
   (emit-byte segment #x57)
   (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction cvtsd2si (segment dst src)
;  (:printer reg-reg/mem ((op #x10) (width 1))) ;wrong
  (:emitter
   (emit-byte segment #xf2)
   (maybe-emit-rex-for-ea segment src dst :operand-size :qword)
   (emit-byte segment #x0f)
   (emit-byte segment #x2d)
   (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction cvtsd2ss (segment dst src)
;  (:printer reg-reg/mem ((op #x10) (width 1))) ;wrong
  (:emitter
   (emit-byte segment #xf2)
   (maybe-emit-rex-for-ea segment src dst)
   (emit-byte segment #x0f)
   (emit-byte segment #x5a)
   (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction cvtss2si (segment dst src)
;  (:printer reg-reg/mem ((op #x10) (width 1))) ;wrong
  (:emitter
   (emit-byte segment #xf3)
   (maybe-emit-rex-for-ea segment src dst :operand-size :qword)
   (emit-byte segment #x0f)
   (emit-byte segment #x2d)
   (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction cvtss2sd (segment dst src)
;  (:printer reg-reg/mem ((op #x10) (width 1))) ;wrong
  (:emitter
   (emit-byte segment #xf3)
   (maybe-emit-rex-for-ea segment src dst)
   (emit-byte segment #x0f)
   (emit-byte segment #x5a)
   (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction cvtsi2ss (segment dst src)
;  (:printer reg-reg/mem ((op #x10) (width 1))) ;wrong
  (:emitter
   (emit-byte segment #xf3)
   (maybe-emit-rex-for-ea segment src dst)
   (emit-byte segment #x0f)
   (emit-byte segment #x2a)
   (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction cvtsi2sd (segment dst src)
;  (:printer reg-reg/mem ((op #x10) (width 1))) ;wrong
  (:emitter
   (emit-byte segment #xf2)
   (maybe-emit-rex-for-ea segment src dst)
   (emit-byte segment #x0f)
   (emit-byte segment #x2a)
   (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction cvtdq2pd (segment dst src)
;  (:printer reg-reg/mem ((op #x10) (width 1))) ;wrong
  (:emitter
   (emit-byte segment #xf3)
   (maybe-emit-rex-for-ea segment src dst)
   (emit-byte segment #x0f)
   (emit-byte segment #xe6)
   (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction cvtdq2ps (segment dst src)
;  (:printer reg-reg/mem ((op #x10) (width 1))) ;wrong
  (:emitter
   (maybe-emit-rex-for-ea segment src dst)
   (emit-byte segment #x0f)
   (emit-byte segment #x5b)
   (emit-ea segment src (reg-tn-encoding dst))))

;; CVTTSD2SI CVTTSS2SI

(define-instruction cvttsd2si (segment dst src)
;  (:printer reg-reg/mem ((op #x10) (width 1))) ;wrong
  (:emitter
   (emit-byte segment #xf2)
   (maybe-emit-rex-for-ea segment src dst :operand-size :qword)
   (emit-byte segment #x0f)
   (emit-byte segment #x2c)
   (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction cvttss2si (segment dst src)
;  (:printer reg-reg/mem ((op #x10) (width 1))) ;wrong
  (:emitter
   (emit-byte segment #xf3)
   (maybe-emit-rex-for-ea segment src dst :operand-size :qword)
   (emit-byte segment #x0f)
   (emit-byte segment #x2c)
   (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction addsd (segment dst src)
;  (:printer reg-reg/mem ((op #x10) (width 1))) ;wrong
  (:emitter
   (emit-byte segment #xf2)
   (maybe-emit-rex-for-ea segment src dst)
   (emit-byte segment #x0f)
   (emit-byte segment #x58)
   (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction addss (segment dst src)
;  (:printer reg-reg/mem ((op #x10) (width 1))) ;wrong
  (:emitter
   (emit-byte segment #xf3)
   (maybe-emit-rex-for-ea segment src dst)
   (emit-byte segment #x0f)
   (emit-byte segment #x58)
   (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction divsd (segment dst src)
;  (:printer reg-reg/mem ((op #x10) (width 1))) ;wrong
  (:emitter
   (emit-byte segment #xf2)
   (maybe-emit-rex-for-ea segment src dst)
   (emit-byte segment #x0f)
   (emit-byte segment #x5e)
   (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction divss (segment dst src)
;  (:printer reg-reg/mem ((op #x10) (width 1))) ;wrong
  (:emitter
   (emit-byte segment #xf3)
   (maybe-emit-rex-for-ea segment src dst)
   (emit-byte segment #x0f)
   (emit-byte segment #x5e)
   (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction mulsd (segment dst src)
;  (:printer reg-reg/mem ((op #x10) (width 1))) ;wrong
  (:emitter
   (emit-byte segment #xf2)
   (maybe-emit-rex-for-ea segment src dst)
   (emit-byte segment #x0f)
   (emit-byte segment #x59)
   (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction mulss (segment dst src)
;  (:printer reg-reg/mem ((op #x10) (width 1))) ;wrong
  (:emitter
   (emit-byte segment #xf3)
   (maybe-emit-rex-for-ea segment src dst)
   (emit-byte segment #x0f)
   (emit-byte segment #x59)
   (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction subsd (segment dst src)
;  (:printer reg-reg/mem ((op #x10) (width 1))) ;wrong
  (:emitter
   (emit-byte segment #xf2)
   (maybe-emit-rex-for-ea segment src dst)
   (emit-byte segment #x0f)
   (emit-byte segment #x5c)
   (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction subss (segment dst src)
;  (:printer reg-reg/mem ((op #x10) (width 1))) ;wrong
  (:emitter
   (emit-byte segment #xf3)
   (maybe-emit-rex-for-ea segment src dst)
   (emit-byte segment #x0f)
   (emit-byte segment #x5c)
   (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction ldmxcsr (segment src)
  (:emitter
   (emit-byte segment #x0f)
   (emit-byte segment #xae)
   (emit-ea segment src 2)))
   
(define-instruction stmxcsr (segment dst)
  (:emitter
   (emit-byte segment #x0f)
   (emit-byte segment #xae)
   (emit-ea segment dst 3)))

