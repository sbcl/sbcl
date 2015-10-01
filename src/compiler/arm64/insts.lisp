;;;; that part of the description of the ARM instruction set (for
;;;; ARMv5) which can live on the cross-compilation host

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

(setf sb!disassem:*disassem-inst-alignment-bytes* 4)


(defparameter *conditions*
  '((:eq . 0)
    (:ne . 1)
    (:cs . 2) (:hs . 2)
    (:cc . 3) (:lo . 3)
    (:mi . 4)
    (:pl . 5)
    (:vs . 6)
    (:vc . 7)
    (:hi . 8)
    (:ls . 9)
    (:ge . 10)
    (:lt . 11)
    (:gt . 12)
    (:le . 13)
    (:al . 14)))

(defparameter *condition-name-vec*
  (let ((vec (make-array 16 :initial-element nil)))
    (dolist (cond *conditions*)
      (when (null (aref vec (cdr cond)))
        (setf (aref vec (cdr cond)) (car cond))))
    vec))

;;; Set assembler parameters. (In CMU CL, this was done with
;;; a call to a macro DEF-ASSEMBLER-PARAMS.)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf sb!assem:*assem-scheduler-p* nil))

(defun conditional-opcode (condition)
  (cdr (assoc condition *conditions* :test #'eq)))

(defun invert-condition (condition)
  (aref *condition-name-vec*
        (logxor 1 (conditional-opcode condition))))

;;;; disassembler field definitions

(defun maybe-add-notes (dstate)
  (let* ((inst (sb!disassem::sap-ref-int
                (sb!disassem:dstate-segment-sap dstate)
                (sb!disassem:dstate-cur-offs dstate)
                n-word-bytes
                (sb!disassem::dstate-byte-order dstate)))
         (op (ldb (byte 8 20) inst))
         (offset (ldb (byte 12 0) inst))
         (rn (ldb (byte 4 16) inst)))
    (cond ((and (= rn null-offset))
           (let ((offset (+ nil-value offset)))
             (case op
               ((88 89) ;; LDR/STR
                (sb!disassem:maybe-note-assembler-routine offset nil dstate)
                (sb!disassem::maybe-note-static-symbol
                 (logior offset other-pointer-lowtag) dstate))
               (40 ;; ADD
                (sb!disassem::maybe-note-static-symbol offset dstate)))))
          (t
           (case op
             (89 ;; LDR
              (case rn
                (#.code-offset
                 (sb!disassem:note-code-constant offset dstate))
                ;; (#.pc-offset
                ;;  (let ((value (sb!disassem::sap-ref-int
                ;;                (sb!disassem:dstate-segment-sap dstate)
                ;;                (+ (sb!disassem:dstate-cur-offs dstate)
                ;;                   offset 8)
                ;;                n-word-bytes
                ;;                (sb!disassem::dstate-byte-order dstate))))
                ;;    (sb!disassem:maybe-note-assembler-routine value nil dstate)))
                )))))))
(defun current-instruction (dstate)
  (sb!disassem::sap-ref-int
   (sb!disassem:dstate-segment-sap dstate)
   (sb!disassem:dstate-cur-offs dstate)
   n-word-bytes
   (sb!disassem::dstate-byte-order dstate)))

(defun 32-bit-register-p (dstate)
  (not (logbitp 31 (current-instruction dstate))))

(progn
  (eval-when (:compile-toplevel :load-toplevel :execute)

    (defun print-shift (value stream dstate)
      (declare (ignore dstate))
      (destructuring-bind (kind amount) value
        (when (plusp amount)
          (princ ", ")
          (princ (ecase kind
                   (#b00 "LSL")
                   (#b01 "LSR")
                   (#b10 "ASR")
                   (#b11 "ROR"))
                 stream)
          (format stream " #~d" amount))))

    (defun print-2-bit-shift (value stream dstate)
      (declare (ignore dstate))
      (when (= value 1)
        (princ ", LSL #12" stream)))

    (defun print-extend (value stream dstate)
      (destructuring-bind (kind amount) value
        (let* ((inst (current-instruction dstate))
               (rd (ldb (byte 5 0) inst))
               (rn (ldb (byte 5 5) inst)))
          (princ " " stream)
          (princ (if (and (= kind #b011)
                          (or (= rd nsp-offset)
                              (= rn nsp-offset)))
                     "LSL"
                     (ecase kind
                       (#b00 "UXTB")
                       (#b001 "UXTH")
                       (#b010 "UXTW")
                       (#b011 "UXTX")
                       (#b100 "SXTB")
                       (#b101 "SXTH")
                       (#b110 "SXTW")
                       (#b111 "SXTX")))
                 stream))
        (when (plusp amount)
          (format stream  " #~d" amount))))

    (defun print-immediate (value stream dstate)
      (declare (ignore dstate))
      (format stream "#~D" value))

    (defun print-logical-immediate (value stream dstate)
      (declare (ignore dstate))
      (format stream " #~D" (apply #'decode-logical-immediate value)))

    (defun print-reg (value stream dstate)
      (when (32-bit-register-p dstate)
        (princ "W" stream))
      (princ (aref *register-names* value) stream))

    (defun print-reg-sp (value stream dstate)
      (when (32-bit-register-p dstate)
        (princ "W" stream))
      (if (= value nsp-offset)
          (princ "NSP" stream)
          (princ (aref *register-names* value) stream))))

  (sb!disassem:define-arg-type shift
    :printer #'print-shift)

  (sb!disassem:define-arg-type 2-bit-shift
    :printer #'print-2-bit-shift)

  (sb!disassem:define-arg-type extend
    :printer #'print-extend)

  (sb!disassem:define-arg-type immediate
    :printer #'print-immediate)

  (sb!disassem:define-arg-type logical-immediate
    :printer #'print-logical-immediate)

  (sb!disassem:define-arg-type reg
    :printer #'print-reg)

  (sb!disassem:define-arg-type reg-sp
    :printer #'print-reg-sp)


  (sb!disassem:define-arg-type condition-code
    :printer #'print-condition))

;;;; special magic to support decoding internal-error and related traps

;; snarf-error-junk is basically identical on all platforms that
;; define it (meaning, not Alpha).  Shouldn't it be common somewhere?
(defun snarf-error-junk (sap offset &optional length-only)
  (let* ((length (sb!sys:sap-ref-8 sap offset))
         (vector (make-array length :element-type '(unsigned-byte 8))))
    (declare (type sb!sys:system-area-pointer sap)
             (type (unsigned-byte 8) length)
             (type (simple-array (unsigned-byte 8) (*)) vector))
    (cond (length-only
           (values 0 (1+ length) nil nil))
          (t
           (sb!kernel:copy-ub8-from-system-area sap (1+ offset)
                                                vector 0 length)
           (collect ((sc-offsets)
                     (lengths))
             (lengths 1)                ; the length byte
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

(defun debug-trap-control (chunk inst stream dstate)
  (declare (ignore inst))
  (flet ((nt (x) (if stream (sb!disassem:note x dstate))))
    (case (debug-trap-code chunk dstate)
      (#.halt-trap
       (nt "Halt trap"))
      (#.pending-interrupt-trap
       (nt "Pending interrupt trap"))
      (#.error-trap
       (nt "Error trap")
       (sb!disassem:handle-break-args #'snarf-error-junk stream dstate))
      (#.cerror-trap
       (nt "Cerror trap")
       (sb!disassem:handle-break-args #'snarf-error-junk stream dstate))
      (#.breakpoint-trap
       (nt "Breakpoint trap"))
      (#.fun-end-breakpoint-trap
       (nt "Function end breakpoint trap"))
      (#.single-step-around-trap
       (nt "Single step around trap"))
      (#.single-step-before-trap
       (nt "Single step before trap")))))

;;;; primitive emitters

(define-bitfield-emitter emit-word 32
  (byte 32 0))

(define-bitfield-emitter emit-dword 64
  (byte 64 0))

;;;; miscellaneous hackery

(defun register-p (thing)
  (and (tn-p thing)
       (eq (sb-name (sc-sb (tn-sc thing))) 'registers)))

(defun fp-register-p (thing)
  (and (tn-p thing)
       (eq (sb-name (sc-sb (tn-sc thing))) 'float-registers)))

(defmacro with-condition-defaulted ((argvar arglist) &body body)
  (let ((internal-emitter (gensym)))
    `(flet ((,internal-emitter ,arglist
              ,@body))
       (if (assoc (car ,argvar) *conditions*)
           (apply #',internal-emitter ,argvar)
           (apply #',internal-emitter :al ,argvar)))))

(define-instruction byte (segment byte)
  (:emitter
   (emit-byte segment byte)))

(define-instruction word (segment word)
  (:emitter
   (etypecase word
     (fixup
      (note-fixup segment :absolute word)
      (emit-word segment 0))
     (integer
      (emit-word segment word)))))

(define-instruction dword (segment word)
  (:emitter
   (etypecase word
     (fixup
      (note-fixup segment :absolute word)
      (emit-dword segment 0))
     (integer
      (emit-dword segment word)))))

(defun emit-header-data (segment type)
  (emit-back-patch segment
                   8
                   (lambda (segment posn)
                     (emit-dword segment
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

;;;; Addressing mode 1 support

;;; Addressing mode 1 has some 11 formats.  These are immediate,
;;; register, and nine shift/rotate functions based on one or more
;;; registers.  As the mnemonics used for these functions are not
;;; currently used, we simply define them as constructors for a
;;; shifter-operand structure, similar to the make-ea function in the
;;; x86 backend.

(defstruct shifter-operand
  register
  function-code
  operand)


(defun lsl (register operand)
  (aver (register-p register))
  (aver (or (register-p operand)
            (typep operand '(integer 0 63))))

  (make-shifter-operand :register register :function-code 0 :operand operand))

(defun lsr (register operand)
  (aver (register-p register))
  (aver (or (register-p operand)
            (typep operand '(integer 0 63))))

  (make-shifter-operand :register register :function-code 1 :operand operand))

(defun asr (register operand)
  (aver (register-p register))
  (aver (or (register-p operand)
            (typep operand '(integer 1 63))))

  (make-shifter-operand :register register :function-code 2 :operand operand))

(defun ror (register operand)
  ;; ROR is a special case: the encoding for ROR with an immediate
  ;; shift of 32 (0) is actually RRX.
  (aver (register-p register))
  (aver (or (register-p operand)
            (typep operand '(integer 1 63))))

  (make-shifter-operand :register register :function-code 3 :operand operand))

(defun rrx (register)
  ;; RRX is a special case: it is encoded as ROR with an immediate
  ;; shift of 32 (0), and has no operand.
  (aver (register-p register))
  (make-shifter-operand :register register :function-code 3 :operand 0))

(defstruct (extend
            (:constructor extend (register kind &optional (operand 0))))
  (register nil :type tn)
  kind
  (operand 0 :type (integer 0 63)))

(define-condition cannot-encode-immediate-operand (error)
  ((value :initarg :value)))

(defun encodable-immediate (operand)
  ;; 32-bit immediate data is encoded as an 8-bit immediate data value
  ;; and a 4-bit immediate shift count.  The actual value is the
  ;; immediate data rotated right by a number of bits equal to twice
  ;; the shift count.  Note that this means that there are a limited
  ;; number of valid immediate integers and that some integers have
  ;; multiple possible encodings.  In the case of multiple encodings,
  ;; the correct one to use is the one with the lowest shift count.
  ;;
  ;; XXX: Is it possible to determine the correct encoding in constant
  ;; time, rather than time proportional to the final shift count?  Is
  ;; it possible to determine if a given integer is valid without
  ;; attempting to encode it?  Are such solutions cheaper (either time
  ;; or spacewise) than simply attempting to encode it?
  (labels ((try-immediate-encoding (value shift)
             (unless (<= 0 shift 15)
               (return-from encodable-immediate))
             (if (typep value '(unsigned-byte 8))
                 (dpb shift (byte 4 8) value)
                 (try-immediate-encoding (dpb value (byte 30 2)
                                              (ldb (byte 2 30) value))
                                         (1+ shift)))))
    (try-immediate-encoding operand 0)))

(defun encode-shifter-immediate (operand)
  (or
   (encodable-immediate operand)
   (error 'cannot-encode-immediate-operand :value operand)))

(defun encode-shifted-register (operand)
  (etypecase operand
    (tn
     (values 0 0 (tn-offset operand)))
    (shifter-operand
     (values (shifter-operand-function-code operand)
             (shifter-operand-operand operand)
             (tn-offset (shifter-operand-register operand))))))

(defmacro composite-immediate-instruction (op r x y &key fixnumize neg-op invert-y invert-r single-op-op first-op first-no-source)
  ;; Successively applies 8-bit wide chunks of Y to X using OP storing the result in R.
  ;;
  ;; If FIXNUMIZE is true, Y is fixnumized before being used.
  ;; If NEG-OP is given and Y is negative, NEG-OP is used instead of OP.
  ;; If INVERT-Y is given LOGNOT is applied to Y before it being used (but after possibly
  ;; being fixnumized.
  ;; If INVERT-R is given R is bit wise inverted at the end.
  ;; If SINGLE-OP-OP is given and (possibly fixnumized) Y fits into a single ARM immediate
  ;; it is used for a single operation instead of OP.
  ;; If FIRST-OP is given, it is used in the first iteration instead of OP.
  ;; If FIRST-NO-SOURCE is given, there will be ne source register (X) in the first iteration.
  (let ((bytespec (gensym "bytespec"))
        (value (gensym "value"))
        (transformed (gensym "transformed")))
    (labels ((instruction (source-reg op neg-op &optional no-source)
               `(,@(if neg-op
                        `((if (< ,y 0)
                              (inst ,neg-op ,r ,@(when (not no-source)`(,source-reg))
                                    (mask-field ,bytespec ,value))
                              (inst ,op ,r ,@(when (not no-source) `(,source-reg))
                                    (mask-field ,bytespec ,value))))
                        `((inst ,op ,r ,@(when (not no-source) `(,source-reg))
                                (mask-field ,bytespec ,value))))
                  (setf (ldb ,bytespec ,value) 0)))
             (composite ()
               `((let ((,bytespec (byte 8 (logandc1 1 (lowest-set-bit-index ,value)))))
                    ,@(instruction x (or first-op op) neg-op first-no-source))
                  (do ((,bytespec (byte 8 (logandc1 1 (lowest-set-bit-index ,value)))
                                  (byte 8 (logandc1 1 (lowest-set-bit-index ,value)))))
                      ((zerop ,value))
                    ,@(instruction r op neg-op)
                    ,@(when invert-r
                            `((inst mvn ,r ,r)))))))
      `(let* ((,transformed ,(if fixnumize
                                 `(fixnumize ,y)
                                 `,y))
              (,value (ldb (byte 32 0)
                           ,@(if neg-op
                                 `((if (< ,transformed 0) (- ,transformed) ,transformed))
                                 (if invert-y
                                     `((lognot ,transformed))
                                     `(,transformed))))))
         ,@(if single-op-op
              `((handler-case
                    (progn
                      (inst ,single-op-op ,r ,x ,transformed))
                  (cannot-encode-immediate-operand ()
                    ,@(composite))))
              (composite))))))


;;;; Addressing mode 2 support

;;; Addressing mode 2 ostensibly has 9 formats.  These are formed from
;;; a cross product of three address calculations and three base
;;; register writeback modes.  As one of the address calculations is a
;;; scaled register calculation identical to the mode 1 register shift
;;; by constant, we reuse the shifter-operand structure and its public
;;; constructors.

(defstruct memory-operand
  base
  offset
  mode)

;;; The @ function is used to encode a memory addressing mode.  The
;;; parameters for the base form are a base register, an optional
;;; offset (either an integer, a register tn or a shifter-operand
;;; structure with a constant shift amount, optionally within a unary
;;; - form), and a base register writeback mode (either :offset,
;;; :pre-index, or :post-index).  The alternative form uses a label as
;;; the base register, and accepts only (optionally negated) integers
;;; as offsets, and requires a mode of :offset.
(defun @ (base &optional (offset 0) (mode :offset))
  (when (label-p base)
    (aver (eq mode :offset))
    (aver (integerp offset)))

  (when (shifter-operand-p offset)
    (aver (integerp (shifter-operand-operand offset))))

  (make-memory-operand :base base :offset offset
                       :mode mode))


;;;; Data-processing instructions


(defmacro def-emitter (name &rest specs)
  (sb!int:collect ((arg-names) (arg-types))
    (let* ((total-bits 32)
           (overall-mask (ash -1 total-bits))
           (num-bytes (truncate total-bits sb!assem::assembly-unit-bits))
           (bytes (make-array num-bytes :initial-element nil)))
      (dolist (spec-expr specs)
        (destructuring-bind (arg size pos) spec-expr
          (when (ldb-test (byte size pos) overall-mask)
            (error "The byte spec ~S either overlaps another byte spec, or ~
                    extends past the end."
                   spec-expr))
          (setf (ldb (byte size pos) overall-mask) -1)
          (unless (numberp arg)
            (arg-names arg)
            (arg-types `(type (integer ,(ash -1 (1- size))
                                       ,(1- (ash 1 size)))
                              ,arg)))
          (multiple-value-bind (start-byte offset)
              (floor pos sb!assem::assembly-unit-bits)
            (let ((end-byte (floor (1- (+ pos size))
                                   sb!assem::assembly-unit-bits)))
              (flet ((maybe-ash (expr offset)
                       (if (zerop offset)
                           expr
                           `(ash ,expr ,offset))))
                (declare (inline maybe-ash))
                (cond ((zerop size))
                      ((= start-byte end-byte)
                       (push (maybe-ash `(ldb (byte ,size 0) ,arg)
                                        offset)
                             (svref bytes start-byte)))
                      (t
                       (push (maybe-ash
                              `(ldb (byte ,(- sb!assem::assembly-unit-bits offset) 0)
                                    ,arg)
                              offset)
                             (svref bytes start-byte))
                       (do ((index (1+ start-byte) (1+ index)))
                           ((>= index end-byte))
                         (push
                          `(ldb (byte ,sb!assem::assembly-unit-bits
                                      ,(- (* sb!assem::assembly-unit-bits
                                             (- index start-byte))
                                          offset))
                                ,arg)
                          (svref bytes index)))
                       (let ((len (rem (+ size offset)
                                       sb!assem::assembly-unit-bits)))
                         (push
                          `(ldb (byte ,(if (zerop len)
                                           sb!assem::assembly-unit-bits
                                           len)
                                      ,(- (* sb!assem::assembly-unit-bits
                                             (- end-byte start-byte))
                                          offset))
                                ,arg)
                          (svref bytes end-byte))))))))))
      (unless (= overall-mask -1)
        (error "There are holes. ~v,'0b"
               total-bits
               (ldb (byte total-bits 0) overall-mask)))
      (let ((forms nil))
        (dotimes (i num-bytes)
          (let ((pieces (svref bytes i)))
            (aver pieces)
            (push `(emit-byte segment
                              ,(if (cdr pieces)
                                   `(logior ,@pieces)
                                   (car pieces)))
                  forms)))
        `(defun ,(symbolicate "EMIT-" name) (segment ,@(arg-names))
           (declare (type segment segment) ,@(arg-types))
           ,@(ecase sb!c:*backend-byte-order*
               (:little-endian (nreverse forms))
               (:big-endian forms))
           nil)))))

(defconstant +64-bit-size+ 1)

(def-emitter add-sub-imm
  (size 1 31)
  (op 2 29)
  (#b10001 5 24)
  (shift 2 22)
  (imm 12 10)
  (rn 5 5)
  (rd 5 0))

(sb!disassem:define-instruction-format
    (add-sub 32)
    (op :field (byte 2 29))
    (rn :field (byte 5 5) :type 'reg-sp)
    (rd :field (byte 5 0) :type 'reg-sp))

(sb!disassem:define-instruction-format
    (add-sub-imm 32
     :default-printer '(:name :tab rd ", " rn ", " imm shift)
     :include add-sub)
    (op2 :field (byte 5 24) :value #b10001)
    (shift :field (byte 2 22) :type '2-bit-shift)
    (imm :field (byte 12 10) :type 'immediate))

(sb!disassem:define-instruction-format
    (adds-subs-imm 32
     :include add-sub-imm
     :default-printer '(:name :tab rd ", " rn ", " imm shift))
    (rd :type 'reg))

(sb!disassem:define-instruction-format
    (add-sub-shift-reg 32
     :default-printer '(:name :tab rd ", " rn ", " rm shift)
     :include add-sub)
    (op2 :field (byte 5 24) :value #b01011)
    (op3 :field (byte 1 21) :value #b0)
    (shift :fields (list (byte 2 22) (byte 6 10)) :type 'shift)
    (rm :field (byte 5 16) :type 'reg)
    (rn :type 'reg)
    (rd :type 'reg))

(def-emitter add-sub-shift-reg
  (size 1 31)
  (op 2 29)
  (#b01011 5 24)
  (shift 2 22)
  (#b0 1 21)
  (rm 5 16)
  (imm 6 10)
  (rn 5 5)
  (rd 5 0))

(sb!disassem:define-instruction-format
    (add-sub-ext-reg 32
     :default-printer '(:name :tab rd ", " rn ", " extend)
     :include add-sub)
    (op2 :field (byte 8 21) :value #b01011001)
    (extend :fields (list (byte 3 13) (byte 3 10)) :type 'extend)
    (rm :field (byte 5 16) :type 'reg)
    (rd :type 'reg))

(def-emitter add-sub-ext-reg
  (size 1 31)
  (op 2 29)
  (#b01011001 8 21)
  (rm 5 16)
  (option 3 13)
  (imm 3 10)
  (rn 5 5)
  (rd 5 0))

(defun add-sub-immediate-p (x)
  (or (typep x '(unsigned-byte 12))
      (and (typep x '(unsigned-byte 24))
           (not (ldb-test (byte 12 0) x)))))

(defmacro def-add-sub (name op &rest printer)
  `(define-instruction ,name (segment rd rn rm)
     ,@printer
     (:emitter
      (let ((rd (tn-offset rd)))
        (cond ((or (register-p rm)
                   (shifter-operand-p rm))
               (multiple-value-bind (shift amount source) (encode-shifted-register rm)
                 (emit-add-sub-shift-reg segment +64-bit-size+ ,op shift source amount (tn-offset rn) rd)))
              ((extend-p rm)
               (let* ((shift 0)
                      (extend (ecase (extend-kind rm)
                                (:uxtb #b00)
                                (:uxth #b001)
                                (:uxtw #b010)
                                (:lsl
                                 (aver (or (= (extend-operand rm) 0)
                                           (= (extend-operand rm) 3)))
                                 (setf shift 1)
                                 #b011)
                                (:uxtx #b011)
                                (:sxtb #b100)
                                (:sxth #b101)
                                (:sxtw #b110)
                                (:sxtx #b111))))
                 (emit-add-sub-ext-reg segment +64-bit-size+ ,op
                                       (tn-offset (extend-register rm))
                                       extend shift (tn-offset rn) rd)))
              (t
               (let ((imm rm)
                     (shift 0))
                 (when (and (typep imm '(unsigned-byte 24))
                            (not (zerop imm))
                            (not (ldb-test (byte 12 0) imm)))
                   (setf imm (ash imm -12)
                         shift 1))
                 (emit-add-sub-imm segment +64-bit-size+ ,op shift imm (tn-offset rn) rd))))))))

(def-add-sub add #b00
  (:printer add-sub-imm ((op #b00)))
  (:printer add-sub-ext-reg ((op #b00)))
  (:printer add-sub-shift-reg ((op #b00))))

(def-add-sub adds #b01
  (:printer add-sub-imm ((op #b01) (rd nil :type 'reg)))
  (:printer add-sub-ext-reg ((op #b01) (rd nil :type 'reg)))
  (:printer add-sub-shift-reg ((op #b01) (op #b01)))
  (:printer add-sub-imm ((op #b01) (rd #b11111))
            '('cmn :tab rn ", " imm shift))
  (:printer add-sub-ext-reg ((op #b01) (rd #b11111))
            '('cmn :tab rn ", " rm extend))
  (:printer add-sub-shift-reg ((op #b01) (rd #b11111))
            '('cmn :tab rn ", " rm shift)))

(def-add-sub sub #b10
  (:printer add-sub-imm ((op #b10)))
  (:printer add-sub-ext-reg ((op #b10)))
  (:printer add-sub-shift-reg ((op #b10)))
  (:printer add-sub-shift-reg ((op #b10) (rn #b11111))
            '('neg :tab rd ", " rm shift)))

(def-add-sub subs #b11
  (:printer add-sub-imm ((op #b11)))
  (:printer add-sub-ext-reg ((op #b11)))
  (:printer add-sub-shift-reg ((op #b11)))
  (:printer add-sub-imm ((op #b11) (rd #b11111))
            '('cmp :tab rn ", " imm shift))
  (:printer add-sub-ext-reg ((op #b11) (rd #b11111))
            '('cmp :tab rn ", " extend))
  (:printer add-sub-shift-reg ((op #b11) (rd #b11111))
            '('cmp :tab rn ", " shift))
  (:printer add-sub-shift-reg ((op #b11) (rn #b11111))
            '('negs :tab rd ", " rm shift)))

(define-instruction-macro cmp (rn rm)
  `(inst subs zr-tn ,rn ,rm))

(define-instruction-macro cmn (rn rm)
  `(inst adds zr-tn ,rn ,rm))

(define-instruction-macro neg (rd rm)
  `(inst sub ,rd zr-tn ,rm))

(define-instruction-macro negs (rd rm)
  `(inst subs ,rd zr-tn ,rm))

;;;

(def-emitter add-sub-carry
  (size 1 31)
  (op 2 29)
  (#b11010000 8 21)
  (rm 5 16)
  (#b000000 6 10)
  (rn 5 5)
  (rd 5 0))

(defmacro def-add-sub-carry (name opc)
  `(define-instruction ,name (segment rd rn rm)
     (:emitter
      (emit-add-sub-carry segment +64-bit-size+ ,opc
                          (tn-offset rm) (tn-offset rn) (tn-offset rd)))))

(def-add-sub-carry adc #b00)
(def-add-sub-carry adcs #b01)
(def-add-sub-carry sbc #b10)
(def-add-sub-carry sbcs #b11)



;;;

(sb!disassem:define-instruction-format
    (logical 32)
    (op :field (byte 2 29))
    (rn :field (byte 5 5) :type 'reg)
    (rd :field (byte 5 0) :type 'reg))

(def-emitter logical-reg
  (size 1 31)
  (opc 2 29)
  (#b01010 5 24)
  (shift 2 22)
  (n 1 21)
  (rm 5 16)
  (imm 6 10)
  (rn 5 5)
  (rd 5 0))

(sb!disassem:define-instruction-format
    (logical-reg 32
     :include logical
     :default-printer '(:name :tab rd ", " rn ", " rm shift))
    (op2 :field (byte 5 24) :value #b01010)
    (shift :fields (list (byte 2 22) (byte 6 10)) :type 'shift)
    (n :field (byte 1 21) :value 0)
    (rm :field (byte 5 16) :type 'reg))

(def-emitter logical-imm
  (size 1 31)
  (opc 2 29)
  (#b100100 6 23)
  (n 1 22)
  (imr 6 16)
  (ims 6 10)
  (rn 5 5)
  (rd 5 0))

(sb!disassem:define-instruction-format
    (logical-imm 32
     :include logical
     :default-printer '(:name :tab rd  ", " rn ", " imm))
    (op2 :field (byte 6 23) :value #b100100)
    (imm :fields (list (byte 1 22) (byte 6 16) (byte 6 10))
         :type 'logical-immediate)
    (rd :type 'reg-sp))

(defun sequence-of-ones-p (integer)
  (declare (type (unsigned-byte 64) integer))
  (and (plusp integer)
       (let ((ones (logior (1- integer) integer))) ;; turns zeros on the right into ones
         (not (logtest (1+ ones) ;; Turns #b111 into #b1000
                       ones))))) ;; And when ANDed will produce 0

(defun count-trailing-zeros (integer)
  (declare (type (unsigned-byte 64) integer))
  (loop for i below 64
        until (logbitp 0 integer)
        do (setf integer (ash integer -1))
        finally (return i)))

(defun find-pattern (integer)
  (declare (type (unsigned-byte 64) integer)
           (optimize speed))
  (loop with pattern = integer
        for size of-type (integer 0 32) = 32 then (truncate size 2)
        for try-pattern of-type (unsigned-byte 32) = (ldb (byte size 0) integer)
        while (and (= try-pattern
                      (the (unsigned-byte 32) (ldb (byte size size) integer)))
                   (> size 1))
        do (setf pattern try-pattern)
        finally (return (values (* size 2) pattern))))

(defun encode-logical-immediate (integer)
  (let ((integer (ldb (byte 64 0) integer)))
    (cond ((or (zerop integer)
               (= integer (ldb (byte 64 0) -1)))
           nil)
          (t
           (multiple-value-bind (size pattern) (find-pattern integer)
             (values (ldb (byte 1 6) size) ;; 64-bit patterns need to set the N bit to 1
                     (cond ((sequence-of-ones-p pattern)
                            ;; Simple case of consecutive ones, just needs shifting right
                            (mod (- size (count-trailing-zeros pattern)) size))
                           ;; Invert the pattern and find consecutive ones there
                           ((not (sequence-of-ones-p (ldb (byte size 0)
                                                          (lognot pattern))))
                            (return-from encode-logical-immediate))
                           (t
                            ;; Rotate the bits on the left so that they are all consecutive
                            (- size (integer-length (ldb (byte size 0) (lognot pattern))))))
                     (logior (1- (logcount pattern))
                             ;; The size is calculated based on the highest set bit of IMMS inverted.
                             ;; Set unused bits to 1 so that the size can be calcuted correctly.
                             (ldb (byte 6 0) (ash -1 (integer-length size))))))))))

(defun rotate-byte (count size pos integer)
  ;; Taken from sb-rotate-byte
  (let ((count (nth-value 1 (round count size)))
        (mask (1- (ash 1 size))))
    (logior (logand integer (lognot (ash mask pos)))
            (let ((field (logand (ash mask pos) integer)))
              (logand (ash mask pos)
                      (if (> count 0)
                          (logior (ash field count)
                                  (ash field (- count size)))
                          (logior (ash field count)
                                  (ash field (+ count size)))))))))

(defun decode-logical-immediate (n immr imms)
  ;; DecodeBitMasks() From the ARM manual
  (declare (type bit n)
           (type (unsigned-byte 6) imms imms))
  (let* ((length (if (zerop n)
                     (1- (integer-length (ldb (byte 6 0) (lognot imms))))
                     6))
         (levels (ldb (byte length 0) -1))
         (s (logand imms levels))
         (r (logand immr levels))
         (bits (ldb (byte (1+ s) 0) -1))
         (pattern (rotate-byte (- r) (ash 1 length) 0 bits))
         (result 0))
    (declare (type (unsigned-byte 64) result))
    (loop for i below 64 by (1+ levels)
          do (setf (ldb (byte (1+ levels) i) result)
                   pattern))
    result))

(defun emit-logical-reg-inst (segment opc n rd rn rm)
  (let* ((shift 0)
         (amount 0))
    (when (shifter-operand-p rm)
      (setf shift (shifter-operand-function-code rm)
            amount (shifter-operand-operand rm)))
    (emit-logical-reg segment +64-bit-size+ opc
                      shift n (tn-offset
                               (if (shifter-operand-p rm)
                                   (shifter-operand-register rm)
                                   rm))
                      amount
                      (tn-offset rn) (tn-offset rd))))

(defmacro def-logical-imm-and-reg (name opc &rest printers)
  `(define-instruction ,name (segment rd rn rm)
     ,@printers
     (:emitter
      (if (or (register-p rm)
              (shifter-operand-p rm))
          (emit-logical-reg-inst segment ,opc 0 rd rn rm)
          (multiple-value-bind (n immr imms)
              (encode-logical-immediate rm)
            (unless n
              (error 'cannot-encode-immediate-operand :value rm))
            (emit-logical-imm segment +64-bit-size+ ,opc n immr imms (tn-offset rn) (tn-offset rd)))))))

(def-logical-imm-and-reg and #b00
  (:printer logical-imm ((op #b00) (n 0)))
  (:printer logical-reg ((op #b00) (n 0))))
(def-logical-imm-and-reg orr #b01
  (:printer logical-imm ((op #b01)))
  (:printer logical-reg ((op #b01)))
  (:printer logical-imm ((op #b01) (rn 31))
            '('mov :tab rd  ", " imm))
  (:printer logical-reg ((op #b01) (rn 31))
                        '('mov :tab rd ", " rm shift)))
(def-logical-imm-and-reg eor #b10
  (:printer logical-imm ((op #b10)))
  (:printer logical-reg ((op #b10))))
(def-logical-imm-and-reg ands #b11
  (:printer logical-imm ((op #b11)))
  (:printer logical-reg ((op #b11)))
  (:printer logical-imm ((op #b11) (rd 31))
            '('tst :tab rn  ", " imm))
  (:printer logical-reg ((op #b11) (rd 31))
            '('tst :tab rn ", " rm shift)))

(define-instruction-macro tst (rn rm)
  `(inst ands zr-tn ,rn ,rm))

(defmacro def-logical-reg (name opc &rest printers)
  `(define-instruction ,name (segment rd rn rm)
     ,@printers
     (:emitter
      (emit-logical-reg-inst segment ,opc 1 rd rn rm))))

(defun bic-mask (x)
  (ldb (byte 64 0) (lognot x)))

(def-logical-reg bic #b00
  (:printer logical-reg ((op #b00) (n 1))))
(def-logical-reg orn #b01
  (:printer logical-reg ((op #b01) (n 1)))
  (:printer logical-reg ((op #b01) (n 1) (rn 31))
            '('mvn :tab rd ", " rm shift)))
(def-logical-reg eon #b10
  (:printer logical-reg ((op #b10) (n 1))))
(def-logical-reg bics #b11
  (:printer logical-reg ((op #b11) (n 1))))

(define-instruction-macro mvn (rd rm)
  `(inst orn ,rd zr-tn ,rm))

;;;

(def-emitter bitfield
  (size 1 31)
  (opc 2 29)
  (#b100110 6 23)
  (n 1 22)
  (imr 6 16)
  (ims 6 10)
  (rn 5 5)
  (rd 5 0))

(define-instruction sbfm (segment rd rn immr imms)
  (:emitter
   (emit-bitfield segment +64-bit-size+ 0 +64-bit-size+
                  immr imms (tn-offset rn) (tn-offset rd))))

(define-instruction bfm (segment rd rn immr imms)
  (:emitter
   (emit-bitfield segment +64-bit-size+ 1 +64-bit-size+
                  immr imms (tn-offset rn) (tn-offset rd))))

(define-instruction ubfm (segment rd rn immr imms)
  (:emitter
   (emit-bitfield segment +64-bit-size+ #b10 +64-bit-size+
                  immr imms (tn-offset rn) (tn-offset rd))))

(define-instruction-macro asr (rd rn shift)
  `(let ((rd ,rd)
         (rn ,rn)
         (shift ,shift))
     (if (integerp shift)
         (inst sbfm rd rn shift 63)
         (inst asrv rd rn shift))))

(define-instruction-macro lsr (rd rn shift)
  `(let ((rd ,rd)
         (rn ,rn)
         (shift ,shift))
     (if (integerp shift)
         (inst ubfm rd rn shift 63)
         (inst lsrv rd rn shift))))

(define-instruction-macro lsl (rd rn shift)
  `(let ((rd ,rd)
         (rn ,rn)
         (shift ,shift))
     (if (integerp shift)
         (inst ubfm rd rn
               (mod (- shift) 64)
               (- 63 shift))
         (inst lslv rd rn shift))))

(define-instruction-macro sxtw (rd rn)
  `(inst sbfm ,rd ,rn 0 31))

;;;

(def-emitter move-wide
  (size 1 31)
  (opc 2 29)
  (#b100101 6 23)
  (hw 2 21)
  (imm 16 5)
  (rd 5 0))

(defmacro process-null-sc (reg)
  `(setf ,reg (if (and (tn-p ,reg)
                       (eq 'null (sc-name (tn-sc ,reg))))
                  null-tn
                  ,reg)))

(define-instruction-macro mov-sp (rd rm)
  `(inst add ,rd ,rm 0))

(define-instruction-macro mov (rd rm)
  `(let ((rd ,rd)
         (rm ,rm))
     (process-null-sc rm)
     (if (integerp rm)
         (load-immediate-word rd rm)
         (inst orr rd zr-tn rm))))

(define-instruction movn (segment rd imm &optional (shift 0))
  (:emitter
   (aver (not (ldb-test (byte 4 0) shift)))
   (emit-move-wide segment +64-bit-size+ #b00 (/ shift 16) imm (tn-offset rd))))

(define-instruction movz (segment rd imm &optional (shift 0))
  (:emitter
   (aver (not (ldb-test (byte 4 0) shift)))
   (emit-move-wide segment +64-bit-size+ #b10 (/ shift 16) imm (tn-offset rd))))

(define-instruction movk (segment rd imm &optional (shift 0))
  (:emitter
   (aver (not (ldb-test (byte 4 0) shift)))
   (emit-move-wide segment +64-bit-size+ #b11 (/ shift 16) imm (tn-offset rd))))

;;;

(def-emitter cond-select
  (size 1 31)
  (op 1 30)
  (#b011010100 9 21)
  (rm 5 16)
  (cond 4 12)
  (op2 2 10)
  (rn 5 5)
  (rd 5 0))

(defmacro def-cond-select (name op op2)
  `(define-instruction ,name (segment rd rn rm cond)
     (:emitter
      (emit-cond-select segment +64-bit-size+ ,op (tn-offset rm) (conditional-opcode cond)
                        ,op2 (tn-offset rn) (tn-offset rd)))))

(def-cond-select csel 0 0)
(def-cond-select csinc 0 1)
(def-cond-select csinv 1 0)
(def-cond-select csneg 1 1)

(define-instruction-macro cset (rd cond)
  `(inst csinc ,rd zr-tn zr-tn (invert-condition ,cond)))
;;;

(def-emitter cond-compare
  (size 1 31)
  (op 1 30)
  (#b111010010 9 21)
  (rm-imm 5 16)
  (cond 4 12)
  (imm-p 1 11)
  (#b0 1 10)
  (rn 5 5)
  (0 1 4)
  (nzcv 4 0))

(defmacro def-cond-compare (name op)
  `(define-instruction ,name (segment rn rm-imm cond &optional (nzcv 0))
     (:emitter
      (emit-cond-compare segment +64-bit-size+ ,op
                         (if (integerp rm-imm)
                             rm-imm
                             (tn-offset rm-imm))
                         (conditional-opcode cond)
                         (if (integerp rm-imm)
                             1
                             0)
                         (tn-offset rn) nzcv))))

(def-cond-compare ccmn #b0)
(def-cond-compare ccmp #b1)

;;;

(def-emitter data-processing-1
  (size 1 31)
  (#b101101011000000000 18 13)
  (opcode 3 10)
  (rn 5 5)
  (rd 5 0))

(defmacro def-data-processing-1 (name opc)
  `(define-instruction ,name (segment rd rn)
     (:emitter
      (emit-data-processing-1 segment +64-bit-size+
                              ,opc (tn-offset rn) (tn-offset rd)))))

(def-data-processing-1 rbit #b000)
(def-data-processing-1 rev16 #b001)
(def-data-processing-1 rev32 #b010)
(def-data-processing-1 rev #b011)
(def-data-processing-1 clz #b100)
(def-data-processing-1 cls #b101)

;;;

(def-emitter data-processing-2
  (size 1 31)
  (#b0011010110 10 21)
  (rm 5 16)
  (opcode 6 10)
  (rn 5 5)
  (rd 5 0))

(define-instruction asrv (segment rd rn rm)
  (:emitter
   (emit-data-processing-2 segment +64-bit-size+ (tn-offset rm)
                           #b001010 (tn-offset rn) (tn-offset rd))))

(define-instruction lslv (segment rd rn rm)
  (:emitter
   (emit-data-processing-2 segment +64-bit-size+ (tn-offset rm)
                           #b001000 (tn-offset rn) (tn-offset rd))))

(define-instruction lsrv (segment rd rn rm)
  (:emitter
   (emit-data-processing-2 segment +64-bit-size+ (tn-offset rm)
                           #b001001 (tn-offset rn) (tn-offset rd))))

(define-instruction rorv (segment rd rn rm)
  (:emitter
   (emit-data-processing-2 segment +64-bit-size+ (tn-offset rm)
                           #b001011 (tn-offset rn) (tn-offset rd))))

(defmacro def-data-processing-2 (name opc)
  `(define-instruction ,name (segment rd rn rm)
     (:emitter
      (emit-data-processing-2 segment +64-bit-size+ (tn-offset rm)
                              ,opc (tn-offset rn) (tn-offset rd)))))

(def-data-processing-2 udiv #b00010)
(def-data-processing-2 sdiv #b00011)

;;;

(def-emitter data-processing-3
  (size 1 31)
  (#b0011011 7 24)
  (op31 3 21)
  (rm 5 16)
  (o0 1 15)
  (ra 5 10)
  (rn 5 5)
  (rd 5 0))

(defmacro def-data-processing-3 (name op31 o0)
  `(define-instruction ,name (segment rd rn rm ra)
     (:emitter
      (emit-data-processing-3 segment +64-bit-size+ ,op31
                              (tn-offset rm)
                              ,o0 (tn-offset ra) (tn-offset rn) (tn-offset rd)))))

(def-data-processing-3 madd #b000 0)
(def-data-processing-3 msub #b000 1)
(def-data-processing-3 smaddl #b001 0)
(def-data-processing-3 smsubl #b001 1)
(def-data-processing-3 umaddl #b101 0)
(def-data-processing-3 umsubl #b101 1)

(define-instruction-macro mul (rd rn rm)
  `(inst madd ,rd ,rn ,rm zr-tn))

(define-instruction smulh (segment rd rn rm)
  (:emitter
   (emit-data-processing-3 segment +64-bit-size+ #b010 (tn-offset rm)
                           0 31 (tn-offset rn) (tn-offset rd))))

(define-instruction umulh (segment rd rn rm)
  (:emitter
   (emit-data-processing-3 segment +64-bit-size+ #b110 (tn-offset rm)
                           0 31 (tn-offset rn) (tn-offset rd))))
;;;

(def-emitter ldr-str-unsigned-imm
  (size 2 30)
  (#b111 3 27)
  (v 1 26)
  (#b01 2 24)
  (opc 2 22)
  (imm 12 10)
  (rn 5 5)
  (rt 5 0))

(def-emitter ldr-str-imm-wb
  (size 2 30)
  (#b111 3 27)
  (v 1 26)
  (#b00 2 24)
  (opc 2 22)
  (#b0 1 21)
  (imm 9 12)
  (mode 1 11)
  (#b1 1 10)
  (rn 5 5)
  (rt 5 0))

(def-emitter ldr-str-reg
  (size 2 30)
  (#b111 3 27)
  (v 1 26)
  (#b00 2 24)
  (opc 2 22)
  (#b1 1 21)
  (rm 5 16)
  (option 3 13)
  (s 1 12)
  (#b10 2 10)
  (rn 5 5)
  (rt 5 0))

(def-emitter ldr-literal
  (opc 2 30)
  (#b011 3 27)
  (v 1 26)
  (#b00 2 24)
  (imm 19 5)
  (rt 5 0))

(def-emitter ldr-str-unscaled-imm
  (size 2 30)
  (#b111 3 27)
  (v 1 26)
  (#b00 2 24)
  (opc 2 22)
  (#b0 1 21)
  (imm 9 12)
  (#b00 2 10)
  (rn 5 5)
  (rt 5 0))

(defun ldr-str-offset-encodable (offset)
  (or (and (typep offset '(unsigned-byte 15))
           (not (ldb-test (byte 3 0) offset)))
      (typep offset '(signed-byte 9))))

(defun emit-load-store (size opc segment dst address)
  (process-null-sc dst)
  (let* ((base (memory-operand-base address))
         (offset (memory-operand-offset address))
         (mode (memory-operand-mode address))
         (index-encoding (position mode '(:post-index :pre-index)))
         (fp (fp-register-p dst))
         (v  (if fp
                 1
                 0))
         (size (cond ((not size)
                      (if (sc-is dst 32-bit-reg)
                          #b10
                          #b11))
                     ((not fp)
                      size)
                     ((eq (sc-name (tn-sc dst)) 'complex-double-reg)
                      (setf opc (logior #b10 opc))
                      #b00)
                     (t
                      (logior #b10
                              (fp-reg-type dst)))))
         (dst (tn-offset dst)))
    (cond ((and (typep offset '(unsigned-byte 15))
                (not (ldb-test (byte 3 0) offset))
                (register-p base)
                (eq mode :offset))
           (emit-ldr-str-unsigned-imm segment size
                                      v opc
                                      (/ offset 8)
                                      (tn-offset base)
                                      dst))
          ((and index-encoding
                (typep offset '(signed-byte 9)))
           (emit-ldr-str-imm-wb segment size
                                v opc offset index-encoding
                                (tn-offset base)
                                dst))
          ((and (eq mode :offset)
                (or (register-p offset)
                    (extend-p offset)))
           (let* ((shift 0)
                  (register (if (extend-p offset)
                                (extend-register offset)
                                offset))
                  (extend (if (extend-p offset)
                              (ecase (extend-kind offset)
                                (:uxtw #b010)
                                (:lsl
                                 (aver (or (= (extend-operand offset) 0)
                                           (= (extend-operand offset) 3)))
                                 (setf shift 1)
                                 #b011)
                                (:sxtw #b110)
                                (:sxtx #b111))
                              #b011)))
             (emit-ldr-str-reg segment size
                               v opc
                               (tn-offset register)
                               extend shift
                               (tn-offset base)
                               dst)))
          ((and (typep offset '(signed-byte 9))
                (or (register-p base)
                    (fp-register-p base))
                (eq mode :offset))
           (emit-ldr-str-unscaled-imm segment size v
                                      opc offset
                                      (tn-offset base) dst))
          (t
           (error "Invalid STR/LDR arguments: ~s ~s" dst address)))))

(defmacro def-load-store (name size opc)
  `(define-instruction ,name (segment dst address)
     (:emitter
      (emit-load-store ,size ,opc segment dst address))))

(def-load-store strb 0 #b00)
(def-load-store ldrb 0 #b01)
(def-load-store ldrsb 0 #b10)
(def-load-store strh 1 #b00)
(def-load-store ldrh 1 #b01)
(def-load-store ldrsh 1 #b10)
(def-load-store str nil #b00)

(define-instruction ldr (segment dst address)
  (:emitter
   (if (label-p address)
       (emit-back-patch segment 4
                        (lambda (segment posn)
                          (emit-ldr-literal segment
                                            #b01
                                            (if (fp-register-p dst)
                                                1
                                                0)
                                            (ash (- (label-position address) posn) -2)
                                            (tn-offset dst))))
       (emit-load-store nil 1 segment dst address))))

(def-emitter ldr-str-pair
  (opc 2 30)
  (#b101 3 27)
  (v 1 26)
  (#b0 1 25)
  (op2 2 23)
  (l 1 22)
  (imm 7 15)
  (rt2 5 10)
  (rn 5 5)
  (rt 5 0))

(defun emit-ldr-str-pair-inst (l segment rt1 rt2 address)
  (let* ((base (memory-operand-base address))
         (offset (memory-operand-offset address))
         (mode (memory-operand-mode address))
         (fp (cond ((and (fp-register-p rt1)
                         (fp-register-p rt2))
                    (assert (and (eq (tn-sc rt1)
                                     (tn-sc rt2)))
                            (rt1 rt2)
                            "Arguments should have the same FP storage class: ~s ~s."
                            rt1 rt2)
                    t)
                   ((or (fp-register-p rt1)
                        (fp-register-p rt2))
                    (error "Both registers must have the same storage class: ~s ~s."
                           rt1 rt2))))
         (v  (if fp
                 1
                 0))
         (opc (cond ((not fp)
                     #b10)
                     (t
                      (fp-reg-type rt1)))))
    (assert (not (ldb-test (byte 3 0) offset)))
    (emit-ldr-str-pair segment opc v
                       (ecase mode
                         (:post-index #b01)
                         (:pre-index #b11)
                         (:offset #b10))
                       l
                       (ash offset -3) (tn-offset rt1) (tn-offset base) (tn-offset rt2))))

(define-instruction stp (segment rt1 rt2 address)
  (:emitter
   (emit-ldr-str-pair-inst 0 segment rt1 rt2 address)))

(define-instruction ldp (segment rt1 rt2 address)
  (:emitter
   (emit-ldr-str-pair-inst 1 segment rt1 rt2 address)))

;;;

(def-emitter cond-branch
  (#b01010100 8 24)
  (imm 19 5)
  (#b0 1 4)
  (cond 4 0))

(def-emitter uncond-branch
  (op 1 31)
  (#b00101 5 26)
  (imm 26 0))

(define-instruction b (segment cond-or-label &optional label)
  (:emitter
   (cond ((and (fixup-p cond-or-label)
               (not label))
          (note-fixup segment :uncond-branch cond-or-label)
          (emit-uncond-branch segment 0 0))
         ((and (fixup-p label))
          (note-fixup segment :cond-branch cond-or-label)
          (emit-cond-branch segment 0 (conditional-opcode cond-or-label)))
         (t
          (emit-back-patch segment 4
                           (cond (label
                                  (assert (label-p label))
                                  (lambda (segment posn)
                                    (emit-cond-branch segment
                                                      (ash (- (label-position label) posn) -2)
                                                      (conditional-opcode cond-or-label))))
                                 (t
                                  (assert (label-p cond-or-label))
                                  (lambda (segment posn)
                                    (emit-uncond-branch segment
                                                        0
                                                        (ash (- (label-position cond-or-label) posn) -2))))))))))

(define-instruction bl (segment label)
  (:emitter
   (ecase label
     (fixup
      (note-fixup segment :uncond-branch label)
      (emit-uncond-branch segment 1 0))
     (label
      (emit-back-patch segment 4
                       (lambda (segment posn)
                         (emit-uncond-branch segment
                                             1
                                             (ash (- (label-position label) posn) -2))))))))

(def-emitter uncond-branch-reg
  (#b1101011 7 25)
  (opc 4 21)
  (#b11111000000 11 10)
  (rn 5 5)
  (#b00000 5 0))

(define-instruction br (segment register)
  (:emitter
   (emit-uncond-branch-reg segment 0 (tn-offset register))))

(define-instruction blr (segment register)
  (:emitter
   (emit-uncond-branch-reg segment 1 (tn-offset register))))

(define-instruction ret (segment &optional (register lr-tn))
  (:emitter
   (emit-uncond-branch-reg segment #b10 (tn-offset register))))

;;;

(def-emitter compare-branch-imm
  (size 1 31)
  (#b011010 6 25)
  (op 1 24)
  (imm 19 5)
  (rt 5 0))

(define-instruction cbz (segment rt label)
  (:emitter
   (assert (label-p label))
   (emit-back-patch segment 4
                    (lambda (segment posn)
                      (emit-compare-branch-imm segment
                                               +64-bit-size+
                                               0
                                               (ash (- (label-position label) posn) -2)
                                               (tn-offset rt))))))

(define-instruction cbnz (segment rt label)
  (:emitter
   (assert (label-p label))
   (emit-back-patch segment 4
                    (lambda (segment posn)
                      (emit-compare-branch-imm segment
                                               +64-bit-size+
                                               1
                                               (ash (- (label-position label) posn) -2)
                                               (tn-offset rt))))))

(def-emitter test-branch-imm
  (b5 1 31)
  (#b011011 6 25)
  (op 1 24)
  (b40 5 19)
  (imm 14 5)
  (rt 5 0))

(define-instruction tbz (segment rt bit label)
  (:emitter
   (assert (label-p label))
   (check-type bit (integer 0 63))
   (emit-back-patch segment 4
                    (lambda (segment posn)
                      (emit-test-branch-imm segment
                                            (ldb (byte 1 5) bit)
                                            0
                                            (ldb (byte 5 0) bit)
                                            (ash (- (label-position label) posn) -2)
                                            (tn-offset rt))))))

(define-instruction tbnz (segment rt bit label)
  (:emitter
   (assert (label-p label))
   (check-type bit (integer 0 63))
   (emit-back-patch segment 4
                    (lambda (segment posn)
                      (emit-test-branch-imm segment
                                            (ldb (byte 1 5) bit)
                                            1
                                            (ldb (byte 5 0) bit)
                                            (ash (- (label-position label) posn) -2)
                                            (tn-offset rt))))))
;;;
(def-emitter exception
  (#b11010100 8 24)
  (opc 3 21)
  (imm 16 5)
  (#b000 3 2)
  (ll 2 0))

(defmacro def-exception (name opc ll)
  `(define-instruction ,name (segment imm)
     (:emitter
      (emit-exception segment ,opc imm ,ll))))

(def-exception brk #b001 #b00)
(def-exception hlt #b010 #b00)

(define-instruction-macro debug-trap ()
  `(inst brk 0))

;;;

(def-emitter pc-relative
  (op 1 31)
  (immlo 2 29)
  (#b10000 5 24)
  (immhi 19 5)
  (rd 5 0))

(defun emit-pc-relative-inst (op segment rd label)
  (assert (label-p label))
  (assert (register-p rd))
  (emit-back-patch segment 4
                   (lambda (segment posn)
                     (let ((offset (- (label-position label) posn)))
                       (emit-pc-relative segment
                                         op
                                         (ldb (byte 2 0) offset)
                                         (ldb (byte 19 2) offset)
                                         (tn-offset rd))))))

(define-instruction adr (segment rd label)
  (:emitter
   (emit-pc-relative-inst 0 segment rd label)))

(define-instruction adrp (segment rd label)
  (:emitter
   (emit-pc-relative-inst 1 segment rd label)))

;;;

(def-emitter system
  (#b1101010100 10 22)
  (l 1 21)
  (sys-reg 16 5)
  (rt 5 0))

(defun sys-reg-encoding (reg)
  (ecase reg
    (:nzcv #b1101101000010000)
    (:fpcr #b1101101000100000)
    (:fpsr #b1101101000100001)
    (:ccnt #b1101110011101000)))

(define-instruction msr (segment sys-reg rt)
  (:emitter
   (emit-system segment 0 (sys-reg-encoding sys-reg) (tn-offset rt))))

(define-instruction mrs (segment rt sys-reg)
  (:emitter
   (emit-system segment 1 (sys-reg-encoding sys-reg) (tn-offset rt))))

;;;

(def-emitter hint
  (#b110101010000001100100000 24 8)
  (imm 3 5)
  (#b11111 5 0))

(define-instruction hint (segment imm)
  (:emitter
   (emit-hint segment imm)))

(define-instruction-macro nop ()
  `(inst hint 0))

;;; Floating point

(defun fp-reg-type (reg)
  (ecase (sc-name (tn-sc reg))
    (single-reg
     0)
    ((double-reg complex-single-reg)
     1)
    (complex-double-reg
     #b10)))

(def-emitter fp-compare
  (#b00011110 8 24)
  (type 2 22)
  (#b1 1 21)
  (rm 5 16)
  (#b001000 6 10)
  (rn 5 5)
  (e 1 4)
  (z 1 3)
  (#b000 3 0))

(defmacro def-fp-compare (name op)
  `(define-instruction ,name (segment rn rm)
     (:emitter
      (assert (or (eql rm 0)
                  (eq (tn-sc rn)
                      (tn-sc rm)))
              (rn rm)
              "Arguments should have the same FP storage class: ~s ~s" rn rm)
      (emit-fp-compare segment
                       (fp-reg-type rn)
                       (if (eql rm 0)
                           0
                           (tn-offset rm))
                       (tn-offset rn)
                       ,op
                       (if (eql rm 0)
                           1
                           0)))))

(def-fp-compare fcmp #b0)
(def-fp-compare fcmpe #b1)

(def-emitter fp-data-processing-1
  (#b000111100 9 23)
  (type 1 22)
  (#b100 3 19)
  (opcode 4 15)
  (#b10000 5 10)
  (rn 5 5)
  (rd 5 0))

(def-emitter fp-data-processing-2
  (#b000111100 9 23)
  (type 1 22)
  (#b1 1 21)
  (rm 5 16)
  (opcode 4 12)
  (#b10 2 10)
  (rn 5 5)
  (rd 5 0))

(def-emitter fp-data-processing-3
    (#b000111110 9 23)
  (type 1 22)
  (o1 1 21)
  (rm 5 16)
  (o2 1 15)
  (ra 5 10)
  (rn 5 5)
  (rd 5 0))

(def-emitter fp-conversion
  (size 1 31)
  (#b00111100 8 23)
  (type 1 22)
  (#b1 1 21)
  (opcode 5 16)
  (#b00000 6 10)
  (rn 5 5)
  (rd 5 0))

(defmacro def-fp-data-processing-1 (name op)
  `(define-instruction ,name (segment rd rn)
     (:emitter
      (assert (and (eq (tn-sc rd)
                       (tn-sc rn)))
              (rd rn)
              "Arguments should have the same FP storage class: ~s ~s." rd rn)
      (emit-fp-data-processing-1 segment
                                 (fp-reg-type rn)
                                 ,op
                                 (tn-offset rn)
                                 (tn-offset rd)))))

(def-fp-data-processing-1 fabs #b0001)
(def-fp-data-processing-1 fneg #b0010)
(def-fp-data-processing-1 fsqrt #b0011)
(def-fp-data-processing-1 frintn #b1000)
(def-fp-data-processing-1 frintp #b1001)
(def-fp-data-processing-1 frintm #b1010)
(def-fp-data-processing-1 frintz #b1011)
(def-fp-data-processing-1 frinta #b1100)
(def-fp-data-processing-1 frintx #b1110)
(def-fp-data-processing-1 frinti #b1111)

(define-instruction fcvt (segment rd rn)
  (:emitter
   (emit-fp-data-processing-1 segment
                              (fp-reg-type rn)
                              (logior #b100 (fp-reg-type rd))
                              (tn-offset rn)
                              (tn-offset rd))))

(defmacro def-fp-data-processing-2 (name op)
  `(define-instruction ,name (segment rd rn rm)
     (:emitter
      (assert (and (eq (tn-sc rd)
                       (tn-sc rn))
                   (eq (tn-sc rd)
                       (tn-sc rm)))
              (rd rn rm)
              "Arguments should have the same FP storage class: ~s ~s ~s." rd rn rm)
      (emit-fp-data-processing-2 segment
                                 (fp-reg-type rn)
                                 (tn-offset rm)
                                 ,op
                                 (tn-offset rn)
                                 (tn-offset rd)))))

(def-fp-data-processing-2 fmul #b0000)
(def-fp-data-processing-2 fdiv #b0001)
(def-fp-data-processing-2 fadd #b0010)
(def-fp-data-processing-2 fsub #b0011)
(def-fp-data-processing-2 fmax #b0100)
(def-fp-data-processing-2 fmin #b0101)
(def-fp-data-processing-2 fmaxnm #b0110)
(def-fp-data-processing-2 fminnm #b0111)
(def-fp-data-processing-2 fnmul #b1000)

(defmacro def-fp-data-processing-3 (name o1 o2)
  `(define-instruction ,name (segment rd rn rm ra)
     (:emitter
      (assert (and (eq (tn-sc rd)
                       (tn-sc rn))
                   (eq (tn-sc rd)
                       (tn-sc rm))
                   (eq (tn-sc rd)
                       (tn-sc ra)))
              (rd rn rm ra)
              "Arguments should have the same FP storage class: ~s ~s ~s ~s." rd rn rm ra)
      (emit-fp-data-processing-3 segment
                                 (fp-reg-type rn)
                                 ,o1
                                 (tn-offset rm)
                                 ,o2
                                 (tn-offset ra)
                                 (tn-offset rn)
                                 (tn-offset rd)))))

(def-fp-data-processing-3 fmadd 0 0)
(def-fp-data-processing-3 fmsub 0 1)
(def-fp-data-processing-3 fnmadd 1 0)
(def-fp-data-processing-3 fnmsub 1 1)

;;;

(defmacro def-fp-conversion (name op &optional from-int)
  `(define-instruction ,name (segment rd rn)
     (:emitter
      ,@(if from-int
            `((assert (fp-register-p rd)
                      (rd)
                      "Destination ~d should be an FP register." rd)
              (assert (register-p rn)
                      (rn)
                      "Source ~d should be an integer register." rn))
            `((assert (register-p rd)
                      (rd)
                      "Destination ~d should be an integer register." rn)
              (assert (fp-register-p rn)
                      (rn)
                      "Source ~d should be an FP register." rn)))
      (emit-fp-conversion segment
                         +64-bit-size+
                         (fp-reg-type ,(if from-int
                                           'rd
                                           'rn))
                         ,op
                         (tn-offset rn)
                         (tn-offset rd)))))

(def-fp-conversion fcvtns #b00000)
(def-fp-conversion fcvtnu #b00001)
(def-fp-conversion scvtf #b00010 t)
(def-fp-conversion ucvtf #b00011 t)
(def-fp-conversion fcvtas #b00100)
(def-fp-conversion fcvtau #b00101)
(def-fp-conversion fcvtps #b01000)
(def-fp-conversion fcvtpu #b01001)
(def-fp-conversion fcvtms #b10000)
(def-fp-conversion fcvtmu #b10001)
(def-fp-conversion fcvtzs #b11000)
(def-fp-conversion fcvtzu #b11001)

(define-instruction fmov (segment rd rn)
  (:emitter
   (cond ((or (sc-is rd complex-double-reg complex-single-reg)
              (sc-is rn complex-double-reg complex-single-reg)))
         ((and (fp-register-p rd)
               (fp-register-p rn))
          (assert (and (eq (tn-sc rd) (tn-sc rn))) (rd rn)
                  "Arguments should have the same fp storage class: ~s ~s."
                  rd rn)
          (emit-fp-data-processing-1 segment (fp-reg-type rn) 0
                                     (tn-offset rn) (tn-offset rd)))
         ((and (register-p rd)
               (fp-register-p rn))
          (let* ((type (fp-reg-type rn))
                 (128-p (= type #b10)))
            (emit-fp-conversion segment (if 128-p
                                            1
                                            type)
                                type
                                (if 128-p
                                    #b01111
                                    #b110)
                                (tn-offset rn) (tn-offset rd))))
         ((and (register-p rn)
               (fp-register-p rd))
          (let* ((type (fp-reg-type rd))
                 (128-p (= type #b10)))
            (emit-fp-conversion segment (if 128-p
                                            1
                                            type)
                                type
                                (if 128-p
                                    #b01111
                                    #b111)
                                (tn-offset rn) (tn-offset rd)))))))

;;;; Boxed-object computation instructions (for LRA and CODE)

;;; Compute the address of a CODE object by parsing the header of a
;;; nearby LRA or SIMPLE-FUN.
(define-instruction compute-code (segment code lip object-label temp)
  (:vop-var vop)
  (:emitter
   (emit-back-patch
    segment 16
    (lambda (segment position)
      (assemble (segment vop)
        ;; Calculate the address of the code component.
        (let ((offset (- (+ (label-position object-label) other-pointer-lowtag)
                         position)))
          (emit-pc-relative segment 0
                            (ldb (byte 2 0) offset)
                            (ldb (byte 19 2) offset)
                            (tn-offset lip)))
        ;; Next, we read the function header.
        (inst ldr temp (@ lip (- other-pointer-lowtag)))
        ;; Strip the widetag
        (inst lsr temp temp n-widetag-bits)
        ;; This leaves the size in words, convert it to bytes and
        ;; subtract from LIP
        (inst sub code lip (lsl temp word-shift)))))))

;;; Compute the address of a nearby LRA object by dead reckoning from
;;; the location of the current instruction.
(define-instruction compute-lra (segment dest lip lra-label)
  (:vop-var vop)
  (:emitter
   ;; We can compute the LRA in a single instruction if the overall
   ;; offset puts it to within an 8-bit displacement.  Otherwise, we
   ;; need to load it by parts into LIP until we're down to an 8-bit
   ;; displacement, and load the final 8 bits into DEST.  We may
   ;; safely presume that an overall displacement may be up to 24 bits
   ;; wide (the PPC backend has special provision for branches over 15
   ;; bits, which implies that segments can become large, but a 16
   ;; megabyte segment (24 bits of displacement) is ridiculous), so we
   ;; need to cover a range of up to three octets of displacement.
   (labels ((compute-delta (position &optional magic-value)
              (- (+ (label-position lra-label
                                    (when magic-value position)
                                    magic-value)
                    other-pointer-lowtag)
                 position))

            (three-instruction-emitter (segment position)
              (declare (ignore segment position))
              (error "write compute-lra three-instruction-emitter"))

            (two-instruction-emitter (segment position)
              (declare (ignore segment position))
              (error "write compute-lra three-instruction-emitter"))

            (one-instruction-emitter (segment position)
              (let ((delta (compute-delta position)))
                (emit-pc-relative segment 0
                                  (ldb (byte 2 0) delta)
                                  (ldb (byte 19 2) delta)
                                  (tn-offset dest))))

            (two-instruction-maybe-shrink (segment posn magic-value)
              (let ((delta (compute-delta posn magic-value)))
                (when (typep delta '(signed-byte 19))
                  (emit-back-patch segment 4
                                   #'one-instruction-emitter)
                  t)))

            (three-instruction-maybe-shrink (segment posn magic-value)
              (let ((delta (compute-delta posn magic-value)))
                (when (<= (integer-length delta) 16)
                  (emit-chooser segment 8 2
                                #'two-instruction-maybe-shrink
                                #'two-instruction-emitter)
                  t))))
     (emit-chooser
      ;; We need to emit up to three instructions, which is 12 octets.
      ;; This preserves a mere two bits of alignment.
      segment 12 2
      #'three-instruction-maybe-shrink
      #'three-instruction-emitter))))

;;; Load a register from a "nearby" LABEL by dead reckoning from the
;;; location of the current instruction.
(define-instruction load-from-label (segment dest label)
  (:vop-var vop)
  (:emitter
   (labels ((compute-delta (position &optional magic-value)
              (- (label-position label
                                 (when magic-value position)
                                 magic-value)
                 position))
            (two-instruction-emitter (segment position)
              (declare (ignore segment position))
              (error "implement two instruction load-from-label"))
            (one-instruction-emitter (segment position)
              (emit-ldr-literal segment
                                #b01 0
                                (ash (compute-delta position) -2)
                                (tn-offset dest)))
            (two-instruction-maybe-shrink (segment posn magic-value)
              (let ((delta (compute-delta posn magic-value)))
                (when (typep delta '(signed-byte 19))
                  (emit-back-patch segment 4
                                   #'one-instruction-emitter)
                  t))))
     (emit-chooser
      ;; We need to emit up to two instructions, which is 8 octets,
      ;; but might wish to emit only one.  This preserves a mere two
      ;; bits of alignment.
      segment 8 2
      #'two-instruction-maybe-shrink
      #'two-instruction-emitter))))
