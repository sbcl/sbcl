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

;;; Note: In CMU CL, this used to be a call to SET-DISASSEM-PARAMS.
(setf sb!disassem:*disassem-inst-alignment-bytes* 4)


(eval-when (:compile-toplevel :load-toplevel :execute)
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
) ; EVAL-WHEN

;;; Set assembler parameters. (In CMU CL, this was done with
;;; a call to a macro DEF-ASSEMBLER-PARAMS.)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf sb!assem:*assem-scheduler-p* nil))

(sb!disassem:define-arg-type condition-code
  :printer *condition-name-vec*)

(defun conditional-opcode (condition)
  (cdr (assoc condition *conditions* :test #'eq)))

;;;; primitive emitters

;(define-bitfield-emitter emit-word 16
;  (byte 16 0))

(define-bitfield-emitter emit-word 32
  (byte 32 0))

;;;; fixup emitters
#|
(defun emit-absolute-fixup (segment fixup)
  (note-fixup segment :absolute fixup)
  (let ((offset (fixup-offset fixup)))
    (if (label-p offset)
        (emit-back-patch segment
                         4 ; FIXME: n-word-bytes
                         (lambda (segment posn)
                           (declare (ignore posn))
                           (emit-dword segment
                                       (- (+ (component-header-length)
                                             (or (label-position offset)
                                                 0))
                                          other-pointer-lowtag))))
        (emit-dword segment (or offset 0)))))

(defun emit-relative-fixup (segment fixup)
  (note-fixup segment :relative fixup)
  (emit-dword segment (or (fixup-offset fixup) 0)))
|#

;;;; miscellaneous hackery

(defun register-p (thing)
  (and (tn-p thing)
       (eq (sb-name (sc-sb (tn-sc thing))) 'registers)))

(defmacro with-condition-defaulted ((argvar arglist) &body body)
  (let ((internal-emitter (gensym)))
    `(flet ((,internal-emitter ,arglist
              ,@body))
       (if (keywordp (car ,argvar))
           (apply #',internal-emitter ,argvar)
           (apply #',internal-emitter :al ,argvar)))))

(define-instruction byte (segment byte)
  (:emitter
   (emit-byte segment byte)))

;(define-instruction word (segment word)
;  (:emitter
;   (emit-word segment word)))

(define-instruction word (segment word)
  (:emitter
   (emit-word segment word)))

(defun emit-header-data (segment type)
  (emit-back-patch segment
                   4
                   (lambda (segment posn)
                     (emit-word segment
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
            (typep operand '(integer 0 31))))

  (make-shifter-operand :register register :function-code 0 :operand operand))

(defun lsr (register operand)
  (aver (register-p register))
  (aver (or (register-p operand)
            (typep operand '(integer 1 32))))

  (make-shifter-operand :register register :function-code 1 :operand operand))

(defun asr (register operand)
  (aver (register-p register))
  (aver (or (register-p operand)
            (typep operand '(integer 1 32))))

  (make-shifter-operand :register register :function-code 2 :operand operand))

(defun ror (register operand)
  ;; ROR is a special case: the encoding for ROR with an immediate
  ;; shift of 32 (0) is actually RRX.
  (aver (register-p register))
  (aver (or (register-p operand)
            (typep operand '(integer 1 31))))

  (make-shifter-operand :register register :function-code 3 :operand operand))

(defun rrx (register)
  ;; RRX is a special case: it is encoded as ROR with an immediate
  ;; shift of 32 (0), and has no operand.
  (aver (register-p register))

  (make-shifter-operand :register register :function-code 3 :operand 0))

(defun encode-shifter-immediate (operand)
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
               (error "Unable to encode #x~X as an immediate operand." operand))
             (if (typep value '(unsigned-byte 8))
                 (dpb shift (byte 4 8) value)
                 (try-immediate-encoding (dpb value (byte 30 2)
                                              (ldb (byte 2 30) value))
                                         (1+ shift)))))
    (try-immediate-encoding operand 0)))

(defun encode-shifter-operand (operand)
  (etypecase operand
    (integer
     (dpb 1 (byte 1 25) (encode-shifter-immediate operand)))

    (tn
     (ecase (sb-name (sc-sb (tn-sc operand)))
       (registers
        ;; For those wondering, this is LSL immediate for 0 bits.
        (tn-offset operand))

       ;; FIXME: Do we actually need constant TNs for shifter operands?
       #+(or)
       (constant )))

    (shifter-operand
     (let ((Rm (tn-offset (shifter-operand-register operand)))
           (shift-code (shifter-operand-function-code operand))
           (shift-amount (shifter-operand-operand operand)))
       (etypecase shift-amount
         (integer
          (dpb shift-amount (byte 5 7)
               (dpb shift-code (byte 2 5)
                    Rm)))
         (tn
          (dpb (tn-offset shift-amount) (byte 4 8)
               (dpb shift-code (byte 2 5)
                    (dpb 1 (byte 1 4)
                         Rm)))))))))

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
  direction
  mode)

;;; The @ macro is used to encode a memory addressing mode.  The
;;; parameters for the base form are a base register, an optional
;;; offset (either an integer, a register tn or a shifter-operand
;;; structure with a constant shift amount, optionally within a unary
;;; - form), and a base register writeback mode (either :offset,
;;; :pre-index, or :post-index).  The alternative form uses a label as
;;; the base register, and accepts only (optionally negated) integers
;;; as offsets, and requires a mode of :offset.
(defun %@ (base offset direction mode)
  (when (label-p base)
    (aver (eq mode :offset))
    (aver (integerp offset)))

  (when (shifter-operand-p offset)
    (aver (integerp (shifter-operand-operand offset))))

  ;; Fix up direction with negative offsets.
  (when (and (not (label-p base))
             (integerp offset)
             (< offset 0))
    (setf offset (- offset))
    (setf direction (if (eq direction :up) :down :up)))

  (make-memory-operand :base base :offset offset
                       :direction direction :mode mode))

(defmacro @ (base &optional (offset 0) (mode :offset))
  (let* ((direction (if (and (consp offset)
                             (eq (car offset) '-)
                             (null (cddr offset)))
                        :down
                        :up))
         (offset (if (eq direction :down) (cadr offset) offset)))
    `(%@ ,base ,offset ,direction ,mode)))

;;;; Data-processing instructions

;;; Data processing instructions have a 4-bit opcode field and a 1-bit
;;; "S" field for updating condition bits.  They are adjacent, so we
;;; roll them into one 5-bit field for convenience.

(define-bitfield-emitter emit-dp-instruction 32
  (byte 4 28) (byte 2 26) (byte 1 25) (byte 5 20)
  (byte 4 16) (byte 4 12) (byte 12 0))

;;; There are 16 data processing instructions, with a breakdown as
;;; follows:
;;;
;;;   1.) Two "move" instructions, with no "source" operand (they have
;;;       destination and shifter operands only).
;;;
;;;   2.) Four "test" instructions, with no "destination" operand.
;;;       These instructions always have their "S" bit set, though it
;;;       is not specified in their mnemonics.
;;;
;;;   3.) Ten "normal" instructions, with all three operands.
;;;
;;; Aside from this, the instructions all have a regular encoding, so
;;; we can use a single macro to define them.

(defmacro define-data-processing-instruction (instruction opcode dest-p src-p)
  `(define-instruction ,instruction (segment &rest args)
     (:emitter
      (with-condition-defaulted (args (condition ,@(if dest-p '(dest))
                                                 ,@(if src-p '(src))
                                                 shifter-operand))
        ,(if dest-p '(aver (register-p dest)))
        ,(if src-p '(aver (register-p src)))
        (let ((shifter-operand (encode-shifter-operand shifter-operand)))
          (emit-dp-instruction segment
                               (conditional-opcode condition)
                               0
                               (ldb (byte 1 25) shifter-operand)
                               ,opcode
                               ,(if src-p '(tn-offset src) 0)
                               ,(if dest-p '(tn-offset dest) 0)
                               shifter-operand))))))

(define-data-processing-instruction and  #x00 t t)
(define-data-processing-instruction ands #x01 t t)
(define-data-processing-instruction eor  #x02 t t)
(define-data-processing-instruction eors #x03 t t)
(define-data-processing-instruction sub  #x04 t t)
(define-data-processing-instruction subs #x05 t t)
(define-data-processing-instruction rsb  #x06 t t)
(define-data-processing-instruction rsbs #x07 t t)
(define-data-processing-instruction add  #x08 t t)
(define-data-processing-instruction adds #x09 t t)
(define-data-processing-instruction adc  #x0a t t)
(define-data-processing-instruction adcs #x0b t t)
(define-data-processing-instruction sbc  #x0c t t)
(define-data-processing-instruction sbcs #x0d t t)
(define-data-processing-instruction rsc  #x0e t t)
(define-data-processing-instruction rscs #x0f t t)
(define-data-processing-instruction orr  #x18 t t)
(define-data-processing-instruction orrs #x19 t t)
(define-data-processing-instruction bic  #x1c t t)
(define-data-processing-instruction bics #x1d t t)

(define-data-processing-instruction tst  #x11 nil t)
(define-data-processing-instruction teq  #x13 nil t)
(define-data-processing-instruction cmp  #x15 nil t)
(define-data-processing-instruction cmn  #x17 nil t)

(define-data-processing-instruction mov  #x1a t nil)
(define-data-processing-instruction movs #x1b t nil)
(define-data-processing-instruction mvn  #x1e t nil)
(define-data-processing-instruction mvns #x1f t nil)

;;;; Exception-generating instructions

;;; There are two exception-generating instructions.  One, BKPT, is
;;; ostensibly used as a breakpoint instruction, and to communicate
;;; with debugging hardware.  The other, SWI, is intended for use as a
;;; system call interface.  We need both because, at least on some
;;; platforms, the only breakpoint trap that works properly is a
;;; syscall.

(define-bitfield-emitter emit-swi-instruction 32
  (byte 4 28) (byte 4 24) (byte 24 0))

(define-instruction swi (segment &rest args)
  (:emitter
   (with-condition-defaulted (args (condition code))
     (emit-swi-instruction segment
                           (conditional-opcode condition)
                           #b1111 code))))

(define-bitfield-emitter emit-bkpt-instruction 32
  (byte 4 28) (byte 8 20) (byte 12 8) (byte 4 4) (byte 4 0))

(define-instruction bkpt (segment code)
  (:emitter
   (emit-bkpt-instruction segment #b1110 #b00010010
                          (ldb (byte 12 4) code)
                          #b0111
                          (ldb (byte 4 0) code))))

;;;; Miscellaneous arithmetic instructions

(define-bitfield-emitter emit-clz-instruction 32
  (byte 4 28) (byte 12 16) (byte 4 12) (byte 8 4) (byte 4 0))

(define-instruction clz (segment &rest args)
  (:emitter
   (with-condition-defaulted (args (condition dest src))
     (aver (register-p dest))
     (aver (register-p src))
     (emit-clz-instruction segment (conditional-opcode condition)
                           #b000101101111
                           (tn-offset dest)
                           #b11110001
                           (tn-offset src)))))

;;;; Branch instructions

(define-bitfield-emitter emit-branch-instruction 32
  (byte 4 28) (byte 4 24) (byte 24 0))

(defun emit-branch-back-patch (segment condition opcode dest)
  (emit-back-patch segment 4
                   (lambda (segment posn)
                     (emit-branch-instruction segment
                                              (conditional-opcode condition)
                                              opcode
                                              (ldb (byte 24 2)
                                                   (- (label-position dest)
                                                      (+ posn 8)))))))

(define-instruction b (segment &rest args)
  (:emitter
   (with-condition-defaulted (args (condition dest))
     (aver (label-p dest))
     (emit-branch-back-patch segment condition #b1010 dest))))

(define-instruction bl (segment &rest args)
  (:emitter
   (with-condition-defaulted (args (condition dest))
     (aver (label-p dest))
     (emit-branch-back-patch segment condition #b1011 dest))))

(define-bitfield-emitter emit-branch-exchange-instruction 32
  (byte 4 28) (byte 8 20) (byte 4 16) (byte 4 12)
  (byte 4 8) (byte 4 4) (byte 4 0))

(define-instruction bx (segment &rest args)
  (:emitter
   (with-condition-defaulted (args (condition dest))
     (aver (register-p dest))
     (emit-branch-exchange-instruction segment
                                       (conditional-opcode condition)
                                       #b00010010 #b1111 #b1111
                                       #b1111 #b0001 (tn-offset dest)))))

(define-instruction blx (segment &rest args)
  (:emitter
   (with-condition-defaulted (args (condition dest))
     (aver (register-p dest))
     (emit-branch-exchange-instruction segment
                                       (conditional-opcode condition)
                                       #b00010010 #b1111 #b1111
                                       #b1111 #b0011 (tn-offset dest)))))

;;;; Semaphore instructions

(defun emit-semaphore-instruction (segment opcode condition dest value address)
  (aver (register-p dest))
  (aver (register-p value))
  (aver (memory-operand-p address))
  (aver (zerop (memory-operand-offset address)))
  (aver (eq :offset (memory-operand-mode address)))
  (emit-dp-instruction segment (conditional-opcode condition)
                       #b00 0 opcode (tn-offset (memory-operand-base address))
                       (tn-offset dest)
                       (dpb #b1001 (byte 4 4) (tn-offset value))))

(define-instruction swp (segment &rest args)
  (:emitter
   (with-condition-defaulted (args (condition dest value address))
     (emit-semaphore-instruction segment #b10000
                                 condition dest value address))))

(define-instruction swpb (segment &rest args)
  (:emitter
   (with-condition-defaulted (args (condition dest value address))
     (emit-semaphore-instruction segment #b10100
                                 condition dest value address))))

;;;; Status-register instructions

(define-instruction mrs (segment &rest args)
  (:emitter
   (with-condition-defaulted (args (condition dest reg))
     (aver (register-p dest))
     (aver (member reg '(:cpsr :spsr)))
     (emit-dp-instruction segment (conditional-opcode condition)
                          #b00 0 (if (eq reg :cpsr) #b10000 #b10100)
                          #b1111 (tn-offset dest) 0))))

(defun encode-status-register-fields (fields)
  (let ((fields (string fields)))
    (labels ((frob (mask index)
               (let* ((field (aref fields index))
                      (field-mask (cdr (assoc field
                                              '((#\c . #b0001) (#\x . #b0010)
                                                (#\s . #b0100) (#\f . #b1000))
                                              :test #'char=))))
                 (unless field-mask
                   (error "bad status register field desginator ~S" fields))
                 (if (< (1+ index) (length fields))
                     (frob (logior mask field-mask) (1+ index))
                     (logior mask field-mask)))))
      (frob 0 0))))

(defmacro cpsr (fields)
  (encode-status-register-fields fields))

(defmacro spsr (fields)
  (logior #b10000 (encode-status-register-fields fields)))

(define-instruction msr (segment &rest args)
  (:emitter
   (with-condition-defaulted (args (condition field-mask src))
     (aver (or (register-p src)
               (integerp src)))
     (let ((encoded-src (encode-shifter-operand src)))
       (emit-dp-instruction segment (conditional-opcode condition)
                            #b00 (ldb (byte 1 25) encoded-src)
                            (if (logbitp 4 field-mask) #b10110 #b10010)
                            field-mask #b1111 encoded-src)))))

;;;; Multiply instructions

(define-bitfield-emitter emit-multiply-instruction 32
  (byte 4 28) (byte 8 20) (byte 4 16) (byte 4 12)
  (byte 4 8) (byte 4 4) (byte 4 0))

(macrolet
    ((define-multiply-instruction (name field-mapping opcode1 opcode2)
       (let ((arglist (ecase field-mapping
                        (:dzsm '(dest src multiplicand))
                        (:dnsm '(dest src multiplicand num))
                        (:ddsm '(dest-lo dest src multiplicand)))))
         `(define-instruction ,name (segment &rest args)
            (:emitter
             (with-condition-defaulted (args (condition ,@arglist))
               ,@(loop
                    for arg in arglist
                    collect `(aver (register-p ,arg)))
               (emit-multiply-instruction segment (conditional-opcode condition)
                                          ,opcode1
                                          (tn-offset dest)
                                          ,(ecase field-mapping
                                             (:dzsm 0)
                                             (:dnsm '(tn-offset num))
                                             (:ddsm '(tn-offset dest-lo)))
                                          (tn-offset src)
                                          ,opcode2
                                          (tn-offset multiplicand))))))))

  (define-multiply-instruction mul  :dzsm #b00000000 #b1001)
  (define-multiply-instruction muls :dzsm #b00000001 #b1001)
  (define-multiply-instruction mla  :dnsm #b00000010 #b1001)
  (define-multiply-instruction mlas :dnsm #b00000011 #b1001)

  (define-multiply-instruction umull  :ddsm #b00001000 #b1001)
  (define-multiply-instruction umulls :ddsm #b00001001 #b1001)
  (define-multiply-instruction umlal  :ddsm #b00001010 #b1001)
  (define-multiply-instruction umlals :ddsm #b00001011 #b1001)

  (define-multiply-instruction smull  :ddsm #b00001100 #b1001)
  (define-multiply-instruction smulls :ddsm #b00001101 #b1001)
  (define-multiply-instruction smlal  :ddsm #b00001110 #b1001)
  (define-multiply-instruction smlals :ddsm #b00001111 #b1001)

  (define-multiply-instruction smlabb :dnsm #b00010000 #b1000)
  (define-multiply-instruction smlatb :dnsm #b00010000 #b1010)
  (define-multiply-instruction smlabt :dnsm #b00010000 #b1100)
  (define-multiply-instruction smlatt :dnsm #b00010000 #b1110)

  (define-multiply-instruction smlalbb :ddsm #b00010100 #b1000)
  (define-multiply-instruction smlaltb :ddsm #b00010100 #b1010)
  (define-multiply-instruction smlalbt :ddsm #b00010100 #b1100)
  (define-multiply-instruction smlaltt :ddsm #b00010100 #b1110)

  (define-multiply-instruction smulbb :dzsm #b00010110 #b1000)
  (define-multiply-instruction smultb :dzsm #b00010110 #b1010)
  (define-multiply-instruction smulbt :dzsm #b00010110 #b1100)
  (define-multiply-instruction smultt :dzsm #b00010110 #b1110)

  (define-multiply-instruction smlawb :dnsm #b00010010 #b1000)
  (define-multiply-instruction smlawt :dnsm #b00010010 #b1100)

  (define-multiply-instruction smulwb :dzsm #b00010010 #b1010)
  (define-multiply-instruction smulwt :dzsm #b00010010 #b1110))

;;;; Load/store instructions

;;; Emit a load/store instruction.  CONDITION is a condition code
;;; name, KIND is :load or :store, WIDTH is :word or :byte, DATA is a
;;; register TN and ADDRESS is either a memory-operand structure or a
;;; stack TN.
(defun emit-load/store-instruction (segment condition kind width data address)
  (flet ((compute-opcode (direction mode)
           (let ((opcode-bits '(:load #b00001 :store #b00000
                                :word #b00000 :byte #b00100
                                :up #b01000 :down #b00000
                                :offset #b10000
                                :pre-index #b10010
                                :post-index #b00000)))
             (reduce #'logior (list kind width direction mode)
                     :key (lambda (value) (getf opcode-bits value))))))
    (etypecase address
      (memory-operand
       (let* ((base (memory-operand-base address))
              (offset (memory-operand-offset address))
              (direction (memory-operand-direction address))
              (mode (memory-operand-mode address))
              (cond-bits (conditional-opcode condition)))
         (cond
           ((label-p base)
            (emit-back-patch
             segment 4
             (lambda (segment posn)
               (let* ((label-delta (- (label-position base)
                                      (+ posn 8)))
                      (offset-delta (if (eq direction :up)
                                        offset
                                        (- offset)))
                      (overall-delta (+ label-delta
                                        offset-delta))
                      (absolute-delta (abs overall-delta)))
                 (aver (typep absolute-delta '(unsigned-byte 12)))
                 (emit-dp-instruction segment cond-bits #b01 0
                                      (compute-opcode (if (< overall-delta 0)
                                                          :down
                                                          :up)
                                                      mode)
                                      (tn-offset base) (tn-offset data)
                                      absolute-delta)))))
           ((integerp offset)
            (aver (typep offset '(unsigned-byte 12)))
            (emit-dp-instruction segment cond-bits #b01 0
                                 (compute-opcode direction mode)
                                 (tn-offset base) (tn-offset data)
                                 offset))
           (t
            (emit-dp-instruction segment cond-bits #b01 1
                                 (compute-opcode direction mode)
                                 (tn-offset base) (tn-offset data)
                                 (encode-shifter-operand offset))))))

      #+(or)
      (tn
       ;; FIXME: This is for stack TN references, and needs must be
       ;; implemented.
       ))))

(macrolet
    ((define-load/store-instruction (name kind width)
       `(define-instruction ,name (segment &rest args)
          (:emitter
           (with-condition-defaulted (args (condition reg address))
             (aver (register-p reg))
             (emit-load/store-instruction segment condition
                                          ,kind ,width reg address))))))
  (define-load/store-instruction ldr :load :word)
  (define-load/store-instruction ldrb :load :byte)
  (define-load/store-instruction str :store :word)
  (define-load/store-instruction strb :store :byte))

;;; Emit a miscellaneous load/store instruction.  CONDITION is a
;;; condition code name, OPCODE1 is the low bit of the first opcode
;;; field, OPCODE2 is the second opcode field, DATA is a register TN
;;; and ADDRESS is either a memory-operand structure or a stack TN.
(defun emit-misc-load/store-instruction (segment condition opcode1
                                         opcode2 data address)
  (flet ((compute-opcode (kind direction mode)
           (let ((opcode-bits '(:register #b00000 :immediate #b00100
                                :up #b01000 :down #b00000
                                :offset #b10000
                                :pre-index #b10010
                                :post-index #b00000)))
             (reduce #'logior (list kind direction mode)
                     :key (lambda (value) (getf opcode-bits value))))))
    (etypecase address
      (memory-operand
       (let* ((base (memory-operand-base address))
              (offset (memory-operand-offset address))
              (direction (memory-operand-direction address))
              (mode (memory-operand-mode address))
              (cond-bits (conditional-opcode condition)))
         (cond
           ((label-p base)
            (emit-back-patch
             segment 4
             (lambda (segment posn)
               (let* ((label-delta (- (label-position base)
                                      (+ posn 8)))
                      (offset-delta (if (eq direction :up)
                                        offset
                                        (- offset)))
                      (overall-delta (+ label-delta
                                        offset-delta))
                      (absolute-delta (abs overall-delta)))
                 (aver (typep absolute-delta '(unsigned-byte 8)))
                 (emit-multiply-instruction segment cond-bits
                                            (logior opcode1
                                                    (compute-opcode :immedaite
                                                                    (if (< overall-delta 0)
                                                                        :down
                                                                        :up)
                                                                    mode))
                                            (tn-offset base) (tn-offset data)
                                            (ldb (byte 4 4) absolute-delta)
                                            opcode2 absolute-delta)))))
           ((integerp offset)
            (aver (typep offset '(unsigned-byte 8)))
            (emit-multiply-instruction segment cond-bits
                                       (logior opcode1
                                               (compute-opcode :immediate direction mode))
                                       (tn-offset base) (tn-offset data)
                                       (ldb (byte 4 4) offset)
                                       opcode2 offset))
           ((register-p offset)
            (emit-multiply-instruction segment cond-bits
                                       (logior opcode1
                                               (compute-opcode :register direction mode))
                                       (tn-offset base) (tn-offset data)
                                       0 opcode2 (tn-offset offset)))
           (t
            (error "bad thing for a miscellaneous load/store address ~S"
                   address)))))

      #+(or)
      (tn
       ;; FIXME: This is for stack TN references, and needs must be
       ;; implemented.
       ))))

(macrolet
    ((define-misc-load/store-instruction (name opcode1 opcode2 double-width)
       `(define-instruction ,name (segment &rest args)
          (:emitter
           (with-condition-defaulted (args (condition reg address))
             (aver (register-p reg))
             ,(when double-width '(aver (evenp (tn-offset reg))))
             (emit-misc-load/store-instruction segment condition
                                               ,opcode1 ,opcode2
                                               reg address))))))
  (define-misc-load/store-instruction strh 0 #b1011 nil)
  (define-misc-load/store-instruction ldrd 0 #b1101 t)
  (define-misc-load/store-instruction strd 0 #b1111 t)

  (define-misc-load/store-instruction ldrh 1 #b1011 nil)
  (define-misc-load/store-instruction ldrsb 1 #b1101 nil)
  (define-misc-load/store-instruction ldrsh 1 #b1111 nil))
