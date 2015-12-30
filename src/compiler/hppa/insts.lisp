;;;; the instruction set definition for HPPA

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!HPPA-ASM")

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Imports from this package into SB-VM
  (import '(reg-tn-encoding) 'sb!vm)
  ;; Imports from SB-VM into this package
  (import '(sb!vm::zero sb!vm::registers sb!vm::float-registers
            sb!vm::single-reg sb!vm::double-reg
            sb!vm::complex-single-reg sb!vm::complex-double-reg
            sb!vm::fp-single-zero sb!vm::fp-double-zero
            sb!vm::zero-tn
            sb!vm::null-offset sb!vm::code-offset sb!vm::zero-offset)))

(!begin-instruction-definitions)

; normally assem-scheduler-p is t, and nil if debugging the assembler
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *assem-scheduler-p* nil))
(setf *assem-max-locations* 68) ; see number-location


;;;; Utility functions.

(defun reg-tn-encoding (tn)
  (declare (type tn tn))
  (sc-case tn
    (null null-offset)
    (zero zero-offset)
    (t
     (aver (eq (sb-name (sc-sb (tn-sc tn))) 'registers))
     (tn-offset tn))))

(defun fp-reg-tn-encoding (tn)
  (declare (type tn tn))
  (sc-case tn
    (fp-single-zero (values 0 nil))
    (single-reg (values (tn-offset tn) nil))
    (fp-double-zero (values 0 t))
    (double-reg (values (tn-offset tn) t))
    (complex-single-reg (values (tn-offset tn) nil))
    (complex-double-reg (values (tn-offset tn) t))))

(defconstant-eqx compare-conditions
  '(:never := :< :<= :<< :<<= :sv :od :tr :<> :>= :> :>>= :>> :nsv :ev)
  #'equalp)

(deftype compare-condition ()
  `(member nil ,@compare-conditions))

(defun compare-condition (cond)
  (declare (type compare-condition cond))
  (if cond
      (let ((result (or (position cond compare-conditions :test #'eq)
                        (error "Bogus Compare/Subtract condition: ~S" cond))))
        (values (ldb (byte 3 0) result)
                (logbitp 3 result)))
      (values 0 nil)))

(defconstant-eqx add-conditions
  '(:never := :< :<= :nuv :znv :sv :od :tr :<> :>= :> :uv :vnz :nsv :ev)
  #'equalp)

(deftype add-condition ()
  `(member nil ,@add-conditions))

(defun add-condition (cond)
    (declare (type add-condition cond))
  (if cond
      (let ((result (or (position cond add-conditions :test #'eq)
                        (error "Bogus Add condition: ~S" cond))))
        (values (ldb (byte 3 0) result)
                (logbitp 3 result)))
      (values 0 nil)))

(defconstant-eqx logical-conditions
  '(:never := :< :<= nil nil nil :od :tr :<> :>= :> nil nil nil :ev)
  #'equalp)

(deftype logical-condition ()
  `(member nil ,@(remove nil logical-conditions)))

(defun logical-condition (cond)
    (declare (type logical-condition cond))
  (if cond
      (let ((result (or (position cond logical-conditions :test #'eq)
                        (error "Bogus Logical condition: ~S" cond))))
        (values (ldb (byte 3 0) result)
                (logbitp 3 result)))
      (values 0 nil)))

(defconstant-eqx unit-conditions
  '(:never nil :sbz :shz :sdc :sbc :shc :tr nil :nbz :nhz :ndc :nbc :nhc)
  #'equalp)

(deftype unit-condition ()
  `(member nil ,@(remove nil unit-conditions)))

(defun unit-condition (cond)
  (declare (type unit-condition cond))
  (if cond
      (let ((result (or (position cond unit-conditions :test #'eq)
                        (error "Bogus Unit condition: ~S" cond))))
        (values (ldb (byte 3 0) result)
                (logbitp 3 result)))
      (values 0 nil)))

(defconstant-eqx extract/deposit-conditions
  '(:never := :< :od :tr :<> :>= :ev)
  #'equalp)

(deftype extract/deposit-condition ()
  `(member nil ,@extract/deposit-conditions))

(defun extract/deposit-condition (cond)
  (declare (type extract/deposit-condition cond))
  (if cond
      (or (position cond extract/deposit-conditions :test #'eq)
          (error "Bogus Extract/Deposit condition: ~S" cond))
      0))


(defun space-encoding (space)
  (declare (type (unsigned-byte 3) space))
  (dpb (ldb (byte 2 0) space)
       (byte 2 1)
       (ldb (byte 1 2) space)))


;;;; Initial disassembler setup.

(setf *disassem-inst-alignment-bytes* 4)

(defvar *disassem-use-lisp-reg-names* t)

; In each define-instruction the form (:dependencies ...)
; contains read and write howto that passed as LOC here.
; Example: (:dependencies (reads src) (writes dst) (writes temp))
;  src, dst and temp is passed each in loc, and can be a register
;  immediate or anything else.
; this routine will return an location-number
; this number must be less than *assem-max-locations*
(defun location-number (loc)
  (etypecase loc
    (null)
    (number)
    (label)
    (fixup)
    (tn
      (ecase (sb-name (sc-sb (tn-sc loc)))
        (immediate-constant
          ;; Can happen if $ZERO or $NULL are passed in.
          nil)
        (registers
          (unless (zerop (tn-offset loc))
            (tn-offset loc)))))
    (symbol
      (ecase loc
        (:memory 0)))))

(defparameter reg-symbols
  (map 'vector
       (lambda (name)
         (cond ((null name) nil)
               (t (make-symbol (concatenate 'string "$" name)))))
       sb!vm::*register-names*))

(define-arg-type reg
  :printer (lambda (value stream dstate)
             (declare (stream stream) (fixnum value))
             (let ((regname (aref reg-symbols value)))
               (princ regname stream)
               (maybe-note-associated-storage-ref
                value
                'registers
                regname
                dstate))))

(defparameter float-reg-symbols
  #.(coerce
     (loop for n from 0 to 31 collect (make-symbol (format nil "$F~d" n)))
     'vector))

(define-arg-type fp-reg
  :printer (lambda (value stream dstate)
             (declare (stream stream) (fixnum value))
             (let ((regname (aref float-reg-symbols value)))
               (princ regname stream)
               (maybe-note-associated-storage-ref
                value
                'float-registers
                regname
                dstate))))

(define-arg-type fp-fmt-0c
  :printer (lambda (value stream dstate)
             (declare (ignore dstate) (stream stream) (fixnum value))
             (ecase value
               (0 (format stream "~A" '\,SGL))
               (1 (format stream "~A" '\,DBL))
               (3 (format stream "~A" '\,QUAD)))))

(defun low-sign-extend (x n)
  (let ((normal (dpb x (byte 1 (1- n)) (ldb (byte (1- n) 1) x))))
    (if (logbitp 0 x)
        (logior (ash -1 (1- n)) normal)
        normal)))

(defun assemble-bits (x list)
  (let ((result 0)
        (offset 0))
    (dolist (e (reverse list))
      (setf result (logior result (ash (ldb e x) offset)))
      (incf offset (byte-size e)))
    result))

(macrolet ((define-imx-decode (name bits)
  `(define-arg-type ,name
     :printer (lambda (value stream dstate)
     (declare (ignore dstate) (stream stream) (fixnum value))
     (format stream "~S" (low-sign-extend value ,bits))))))
  (define-imx-decode im5 5)
  (define-imx-decode im11 11)
  (define-imx-decode im14 14))

(define-arg-type im3
  :printer (lambda (value stream dstate)
             (declare (ignore dstate) (stream stream) (fixnum value))
             (format stream "~S" (assemble-bits value `(,(byte 1 0)
                                                          ,(byte 2 1))))))

(define-arg-type im21
  :printer (lambda (value stream dstate)
             (declare (ignore dstate) (stream stream) (fixnum value))
             (format stream "~S"
                     (assemble-bits value `(,(byte 1 0) ,(byte 11 1)
                                            ,(byte 2 14) ,(byte 5 16)
                                            ,(byte 2 12))))))

(define-arg-type cp
  :printer (lambda (value stream dstate)
             (declare (ignore dstate) (stream stream) (fixnum value))
             (format stream "~S" (- 31 value))))

(define-arg-type clen
  :printer (lambda (value stream dstate)
             (declare (ignore dstate) (stream stream) (fixnum value))
             (format stream "~S" (- 32 value))))

(define-arg-type compare-condition
  :printer #("" \,= \,< \,<= \,<< \,<<= \,SV \,OD \,TR \,<> \,>=
             \,> \,>>= \,>> \,NSV \,EV))

(define-arg-type compare-condition-false
  :printer #(\,TR \,<> \,>= \,> \,>>= \,>> \,NSV \,EV
             "" \,= \,< \,<= \,<< \,<<= \,SV \,OD))

(define-arg-type add-condition
  :printer #("" \,= \,< \,<= \,NUV \,ZNV \,SV \,OD \,TR \,<> \,>= \,> \,UV
             \,VNZ \,NSV \,EV))

(define-arg-type add-condition-false
  :printer #(\,TR \,<> \,>= \,> \,UV \,VNZ \,NSV \,EV
             "" \,= \,< \,<= \,NUV \,ZNV \,SV \,OD))

(define-arg-type logical-condition
  :printer #("" \,= \,< \,<= "" "" "" \,OD \,TR \,<> \,>= \,> "" "" "" \,EV))

(define-arg-type unit-condition
  :printer #("" "" \,SBZ \,SHZ \,SDC \,SBC \,SHC \,TR "" \,NBZ \,NHZ \,NDC
             \,NBC \,NHC))

(define-arg-type extract/deposit-condition
  :printer #("" \,= \,< \,OD \,TR \,<> \,>= \,EV))

(define-arg-type extract/deposit-condition-false
  :printer #(\,TR \,<> \,>= \,EV "" \,= \,< \,OD))

(define-arg-type nullify
  :printer #("" \,N))

(define-arg-type fcmp-cond
  :printer #(\FALSE? \FALSE \? \!<=> \= \=T \?= \!<> \!?>= \< \?<
                     \!>= \!?> \<= \?<= \!> \!?<= \> \?>\ \!<= \!?< \>=
                     \?>= \!< \!?= \<> \!= \!=T \!? \<=> \TRUE? \TRUE))

(define-arg-type integer
  :printer (lambda (value stream dstate)
             (declare (ignore dstate) (stream stream) (fixnum value))
             (format stream "~S" value)))

(define-arg-type space
  :printer #("" |1,| |2,| |3,|))

(define-arg-type memory-address-annotation
  :printer (lambda (value stream dstate)
             (declare (ignore stream))
             (destructuring-bind (reg raw-offset) value
               (let ((offset (low-sign-extend raw-offset 14)))
                 (cond
                   ((= reg code-offset)
                    (note-code-constant offset dstate))
                   ((= reg null-offset)
                    (maybe-note-nil-indexed-object offset dstate)))))))


;;;; Define-instruction-formats for disassembler.

(define-instruction-format (load/store 32)
  (op   :field (byte 6 26))
  (b    :field (byte 5 21) :type 'reg)
  (t/r  :field (byte 5 16) :type 'reg)
  (s    :field (byte 2 14) :type 'space)
  (im14 :field (byte 14 0) :type 'im14)
  (memory-address-annotation :fields (list (byte 5 21) (byte 14 0))
                             :type 'memory-address-annotation))

(defconstant-eqx cmplt-index-print '((:cond ((u :constant 1) '\,S))
                                 (:cond ((m :constant 1) '\,M)))
  #'equalp)

(defconstant-eqx cmplt-disp-print '((:cond ((m :constant 1)
                                  (:cond ((s :constant 0) '\,MA)
                                         (t '\,MB)))))
  #'equalp)

(defconstant-eqx cmplt-store-print '((:cond ((s :constant 0) '\,B)
                                         (t '\,E))
                                  (:cond ((m :constant 1) '\,M)))
  #'equalp)

(define-instruction-format (extended-load/store 32)
  (op1     :field (byte 6 26) :value 3)
  (b       :field (byte 5 21) :type 'reg)
  (x/im5/r :field (byte 5 16) :type 'reg)
  (s       :field (byte 2 14) :type 'space)
  (u       :field (byte 1 13))
  (op2     :field (byte 3 10))
  (ext4/c  :field (byte 4 6))
  (m       :field (byte 1 5))
  (t/im5   :field (byte 5 0) :type 'reg))

(define-instruction-format (ldil 32 :default-printer '(:name :tab im21 "," t))
  (op    :field (byte 6 26))
  (t   :field (byte 5 21) :type 'reg)
  (im21 :field (byte 21 0) :type 'im21))

(define-instruction-format (branch17 32)
  (op1 :field (byte 6 26))
  (t   :field (byte 5 21) :type 'reg)
  (w   :fields `(,(byte 5 16) ,(byte 11 2) ,(byte 1 0))
       :use-label
       (lambda (value dstate)
         (declare (type disassem-state dstate) (list value))
         (let ((x (logior (ash (first value) 12) (ash (second value) 1)
                          (third value))))
           (+ (ash (sign-extend
                    (assemble-bits x `(,(byte 1 0) ,(byte 5 12) ,(byte 1 1)
                                       ,(byte 10 2))) 17) 2)
              (dstate-cur-addr dstate) 8))))
  (op2 :field (byte 3 13))
  (n   :field (byte 1 1) :type 'nullify))

(define-instruction-format (branch12 32)
  (op1 :field (byte 6 26))
  (r2  :field (byte 5 21) :type 'reg)
  (r1  :field (byte 5 16) :type 'reg)
  (w   :fields `(,(byte 11 2) ,(byte 1 0))
       :use-label
       (lambda (value dstate)
         (declare (type disassem-state dstate) (list value))
         (let ((x (logior (ash (first value) 1) (second value))))
           (+ (ash (sign-extend
                    (assemble-bits x `(,(byte 1 0) ,(byte 1 1) ,(byte 10 2)))
                    12) 2)
              (dstate-cur-addr dstate) 8))))
  (c   :field (byte 3 13))
  (n   :field (byte 1 1) :type 'nullify))

(define-instruction-format (branch 32)
  (op1 :field (byte 6 26))
  (t   :field (byte 5 21) :type 'reg)
  (x   :field (byte 5 16) :type 'reg)
  (op2 :field (byte 3 13))
  (x1  :field (byte 11 2))
  (n   :field (byte 1 1) :type 'nullify)
  (x2  :field (byte 1 0)))

(define-instruction-format (r3-inst 32
                            :default-printer '(:name c :tab r1 "," r2 "," t))
  (r3 :field (byte 6 26) :value 2)
  (r2 :field (byte 5 21) :type 'reg)
  (r1 :field (byte 5 16) :type 'reg)
  (c  :field (byte 3 13))
  (f  :field (byte 1 12))
  (op :field (byte 7 5))
  (t  :field (byte 5 0) :type 'reg))

(define-instruction-format (imm-inst 32
                            :default-printer '(:name c :tab im11 "," r "," t))
  (op   :field (byte 6 26))
  (r    :field (byte 5 21) :type 'reg)
  (t    :field (byte 5 16) :type 'reg)
  (c    :field (byte 3 13))
  (f    :field (byte 1 12))
  (o    :field (byte 1 11))
  (im11 :field (byte 11 0) :type 'im11))

(define-instruction-format (extract/deposit-inst 32)
  (op1    :field (byte 6 26))
  (r2     :field (byte 5 21) :type 'reg)
  (r1     :field (byte 5 16) :type 'reg)
  (c      :field (byte 3 13) :type 'extract/deposit-condition)
  (op2    :field (byte 3 10))
  (cp     :field (byte 5 5) :type 'cp)
  (t/clen :field (byte 5 0) :type 'clen))

(define-instruction-format (break 32
                            :default-printer '(:name :tab im13 "," im5))
  (op1  :field (byte 6 26) :value 0)
  (im13 :field (byte 13 13))
  (q2   :field (byte 8 5) :value 0)
  (im5  :field (byte 5 0) :reader break-im5))

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
    (case (break-im5 chunk dstate)
      (#.error-trap
       (nt "Error trap")
       (handle-break-args #'snarf-error-junk stream dstate))
      (#.cerror-trap
       (nt "Cerror trap")
       (handle-break-args #'snarf-error-junk stream dstate))
      (#.breakpoint-trap
       (nt "Breakpoint trap"))
      (#.pending-interrupt-trap
       (nt "Pending interrupt trap"))
      (#.halt-trap
       (nt "Halt trap"))
      (#.fun-end-breakpoint-trap
       (nt "Function end breakpoint trap"))
      (#.single-step-around-trap
       (nt "Single step around trap")))))

(define-instruction-format (system-inst 32)
  (op1 :field (byte 6 26) :value 0)
  (r1  :field (byte 5 21) :type 'reg)
  (r2  :field (byte 5 16) :type 'reg)
  (s   :field (byte 3 13))
  (op2 :field (byte 8 5))
  (r3  :field (byte 5 0) :type 'reg))

(define-instruction-format (fp-load/store 32)
  (op :field (byte 6 26))
  (b  :field (byte 5 21) :type 'reg)
  (x  :field (byte 5 16) :type 'reg)
  (s  :field (byte 2 14) :type 'space)
  (u  :field (byte 1 13))
  (x1 :field (byte 1 12))
  (x2 :field (byte 2 10))
  (x3 :field (byte 1 9))
  (x4 :field (byte 3 6))
  (m  :field (byte 1 5))
  (t  :field (byte 5 0) :type 'fp-reg))

(define-instruction-format (fp-class-0-inst 32)
  (op1 :field (byte 6 26))
  (r   :field (byte 5 21) :type 'fp-reg)
  (x1  :field (byte 5 16) :type 'fp-reg)
  (op2 :field (byte 3 13))
  (fmt :field (byte 2 11) :type 'fp-fmt-0c)
  (x2  :field (byte 2 9))
  (x3  :field (byte 3 6))
  (x4  :field (byte 1 5))
  (t   :field (byte 5 0) :type 'fp-reg))

(define-instruction-format (fp-class-1-inst 32)
  (op1 :field (byte 6 26))
  (r   :field (byte 5 21) :type 'fp-reg)
  (x1  :field (byte 4 17) :value 0)
  (x2  :field (byte 2 15))
  (df  :field (byte 2 13) :type 'fp-fmt-0c)
  (sf  :field (byte 2 11) :type 'fp-fmt-0c)
  (x3  :field (byte 2 9) :value 1)
  (x4  :field (byte 3 6) :value 0)
  (x5  :field (byte 1 5) :value 0)
  (t   :field (byte 5 0) :type 'fp-reg))



;;;; Load and Store stuff.

(define-bitfield-emitter emit-load/store 32
  (byte 6 26)
  (byte 5 21)
  (byte 5 16)
  (byte 2 14)
  (byte 14 0))

(defun encode-imm21 (segment value)
  (declare (type (or fixup (signed-byte 32) (unsigned-byte 32)) value))
  (cond ((fixup-p value)
         (note-fixup segment :hi value)
         (aver (or (null (fixup-offset value)) (zerop (fixup-offset value))))
         0)
        (t
         (let ((hi (ldb (byte 21 11) value)))
           (logior (ash (ldb (byte 5 2) hi) 16)
                   (ash (ldb (byte 2 7) hi) 14)
                   (ash (ldb (byte 2 0) hi) 12)
                   (ash (ldb (byte 11 9) hi) 1)
                   (ldb (byte 1 20) hi))))))

(defun encode-imm11 (value)
  (declare (type (signed-byte 11) value))
  (dpb (ldb (byte 10 0) value)
       (byte 10 1)
       (ldb (byte 1 10) value)))

(defun encode-imm11u (value)
  (declare (type (or (signed-byte 32) (unsigned-byte 32)) value))
  (declare (type (unsigned-byte 11) value))
  (dpb (ldb (byte 11 0) value)
       (byte 11 1)
       0))

(defun encode-imm14 (value)
  (declare (type (signed-byte 14) value))
  (dpb (ldb (byte 13 0) value)
       (byte 13 1)
       (ldb (byte 1 13) value)))

(defun encode-disp/fixup (segment disp imm-bits)
  (cond
    ((fixup-p disp)
      (aver (or (null (fixup-offset disp)) (zerop (fixup-offset disp))))
      (if imm-bits
        (note-fixup segment :load11u disp)
        (note-fixup segment :load disp))
      0)
    (t
      (if imm-bits
        (encode-imm11u disp)
        (encode-imm14 disp)))))

; LDO can be used in two ways: to load an 14bit-signed value
; or load an 11bit-unsigned value. The latter is used for
; example in an LDIL/LDO pair. The key :unsigned specifies this.
(macrolet ((define-load-inst (name opcode &optional imm-bits)
             `(define-instruction ,name (segment disp base reg &key unsigned)
                (:declare (type tn reg base)
                          (type (member t nil) unsigned)
                          (type (or fixup (signed-byte 14)) disp))
                (:delay 0)
                (:printer load/store ((op ,opcode) (s 0))
                          '(:name :tab im14 "(" s b ")," t/r memory-address-annotation))
                (:dependencies (reads base) (reads :memory) (writes reg))
                (:emitter
                  (emit-load/store segment ,opcode
                    (reg-tn-encoding base) (reg-tn-encoding reg) 0
                    (if unsigned
                       (encode-disp/fixup segment disp t)
                       (encode-disp/fixup segment disp nil))))))
           (define-store-inst (name opcode &optional imm-bits)
             `(define-instruction ,name (segment reg disp base)
                (:declare (type tn reg base)
                          (type (or fixup (signed-byte 14)) disp))
                (:delay 0)
                (:printer load/store ((op ,opcode) (s 0))
                  '(:name :tab t/r "," im14 "(" s b ")" memory-address-annotation))
                (:dependencies (reads base) (reads reg) (writes :memory))
                (:emitter
                  (emit-load/store segment ,opcode
                    (reg-tn-encoding base) (reg-tn-encoding reg) 0
                    (encode-disp/fixup segment disp ,imm-bits))))))
    (define-load-inst ldw #x12)
    (define-load-inst ldh #x11)
    (define-load-inst ldb #x10)
    (define-load-inst ldwm #x13)
    (define-load-inst ldo #x0D)
    (define-store-inst stw #x1A)
    (define-store-inst sth #x19)
    (define-store-inst stb #x18)
    (define-store-inst stwm #x1B))

(define-bitfield-emitter emit-extended-load/store 32
  (byte 6 26) (byte 5 21) (byte 5 16) (byte 2 14) (byte 1 13)
  (byte 3 10) (byte 4 6) (byte 1 5) (byte 5 0))

(macrolet ((define-load-indexed-inst (name opcode)
              `(define-instruction ,name (segment index base reg &key modify scale)
                (:declare (type tn reg base index)
                 (type (member t nil) modify scale))
                (:delay 0)
                (:dependencies (reads index) (reads base) (writes reg) (reads :memory))
                (:printer extended-load/store ((ext4/c ,opcode) (t/im5 nil :type 'reg)
                                               (op2 0))
                 `(:name ,@cmplt-index-print :tab x/im5/r
                                              "(" s b ")" t/im5))
                (:emitter
                 (emit-extended-load/store
                  segment #x03 (reg-tn-encoding base) (reg-tn-encoding index)
                  0 (if scale 1 0) 0 ,opcode (if modify 1 0)
                  (reg-tn-encoding reg))))))
  (define-load-indexed-inst ldwx 2)
  (define-load-indexed-inst ldhx 1)
  (define-load-indexed-inst ldbx 0)
  (define-load-indexed-inst ldcwx 7))

(defun short-disp-encoding (segment disp)
  (declare (type (or fixup (signed-byte 5)) disp))
  (cond ((fixup-p disp)
         (note-fixup segment :load-short disp)
         (aver (or (null (fixup-offset disp)) (zerop (fixup-offset disp))))
         0)
        (t
         (dpb (ldb (byte 4 0) disp)
              (byte 4 1)
              (ldb (byte 1 4) disp)))))

(macrolet ((define-load-short-inst (name opcode)
               `(define-instruction ,name (segment base disp reg &key modify)
                 (:declare (type tn base reg)
                  (type (or fixup (signed-byte 5)) disp)
                  (type (member :before :after nil) modify))
                 (:delay 0)
                 (:dependencies (reads base) (writes reg) (reads :memory))
                 (:printer extended-load/store ((ext4/c ,opcode) (t/im5 nil :type 'im5)
                                                (op2 4))
                  `(:name ,@cmplt-disp-print :tab x/im5/r
                    "(" s b ")" t/im5))
                 (:emitter
                  (multiple-value-bind
                        (m a)
                      (ecase modify
                        ((nil) (values 0 0))
                        (:after (values 1 0))
                        (:before (values 1 1)))
                    (emit-extended-load/store segment #x03 (reg-tn-encoding base)
                                              (short-disp-encoding segment disp)
                                              0 a 4 ,opcode m
                                              (reg-tn-encoding reg))))))
           (define-store-short-inst (name opcode)
               `(define-instruction ,name (segment reg base disp &key modify)
                 (:declare (type tn reg base)
                  (type (or fixup (signed-byte 5)) disp)
                  (type (member :before :after nil) modify))
                 (:delay 0)
                 (:dependencies (reads base) (reads reg) (writes :memory))
                 (:printer extended-load/store ((ext4/c ,opcode) (t/im5 nil :type 'im5)
                                                (op2 4))
                  `(:name ,@cmplt-disp-print :tab x/im5/r
                    "," t/im5 "(" s b ")"))
                 (:emitter
                  (multiple-value-bind
                        (m a)
                      (ecase modify
                        ((nil) (values 0 0))
                        (:after (values 1 0))
                        (:before (values 1 1)))
                    (emit-extended-load/store segment #x03 (reg-tn-encoding base)
                                              (short-disp-encoding segment disp)
                                              0 a 4 ,opcode m
                                              (reg-tn-encoding reg)))))))
  (define-load-short-inst ldws 2)
  (define-load-short-inst ldhs 1)
  (define-load-short-inst ldbs 0)
  (define-load-short-inst ldcws 7)

  (define-store-short-inst stws 10)
  (define-store-short-inst sths 9)
  (define-store-short-inst stbs 8))

(define-instruction stbys (segment reg base disp where &key modify)
  (:declare (type tn reg base)
            (type (signed-byte 5) disp)
            (type (member :begin :end) where)
            (type (member t nil) modify))
  (:delay 0)
  (:dependencies (reads base) (reads reg) (writes :memory))
  (:printer extended-load/store ((ext4/c #xC) (t/im5 nil :type 'im5) (op2 4))
            `(:name ,@cmplt-store-print :tab x/im5/r "," t/im5 "(" s b ")"))
  (:emitter
   (emit-extended-load/store segment #x03 (reg-tn-encoding base)
                             (reg-tn-encoding reg) 0
                             (ecase where (:begin 0) (:end 1))
                             4 #xC (if modify 1 0)
                             (short-disp-encoding segment disp))))


;;;; Immediate 21-bit Instructions.
;;; Note the heavy scrambling of the immediate value to instruction memory

(define-bitfield-emitter emit-imm21 32
  (byte 6 26)
  (byte 5 21)
  (byte 21 0))

(define-instruction ldil (segment value reg)
  (:declare (type tn reg)
            (type (or (signed-byte 32) (unsigned-byte 32) fixup) value))
  (:delay 0)
  (:dependencies (writes reg))
  (:printer ldil ((op #x08)))
  (:emitter
   (emit-imm21 segment #x08 (reg-tn-encoding reg)
               (encode-imm21 segment value))))

; this one overwrites number stack ?
(define-instruction addil (segment value reg)
  (:declare (type tn reg)
            (type (or (signed-byte 32) (unsigned-byte 32) fixup) value))
  (:delay 0)
  (:dependencies (writes reg))
  (:printer ldil ((op #x0A)))
  (:emitter
   (emit-imm21 segment #x0A (reg-tn-encoding reg)
               (encode-imm21 segment value))))


;;;; Branch instructions.

(define-bitfield-emitter emit-branch 32
  (byte 6 26) (byte 5 21) (byte 5 16) (byte 3 13)
  (byte 11 2) (byte 1 1) (byte 1 0))

(defun label-relative-displacement (label posn &optional delta-if-after)
   (declare (type label label) (type index posn))
  (ash (- (if delta-if-after
              (label-position label posn delta-if-after)
              (label-position label))
          (+ posn 8)) -2))

(defun decompose-branch-disp (segment disp)
  (declare (type (or fixup (signed-byte 17)) disp))
  (cond ((fixup-p disp)
         (note-fixup segment :branch disp)
         (aver (or (null (fixup-offset disp)) (zerop (fixup-offset disp))))
         (values 0 0 0))
        (t
         (values (ldb (byte 5 11) disp)
                 (dpb (ldb (byte 10 0) disp)
                      (byte 10 1)
                      (ldb (byte 1 10) disp))
                 (ldb (byte 1 16) disp)))))

(defun emit-relative-branch (segment opcode link sub-opcode target nullify)
  (declare (type (unsigned-byte 6) opcode)
           (type (unsigned-byte 5) link)
           (type (unsigned-byte 1) sub-opcode)
           (type label target)
           (type (member t nil) nullify))
  (emit-back-patch segment 4
    (lambda (segment posn)
      (let ((disp (label-relative-displacement target posn)))
        (aver (typep disp '(signed-byte 17)))
        (multiple-value-bind
            (w1 w2 w)
            (decompose-branch-disp segment disp)
          (emit-branch segment opcode link w1 sub-opcode w2
                       (if nullify 1 0) w))))))

(define-instruction b (segment target &key nullify)
  (:declare (type label target) (type (member t nil) nullify))
  (:delay 0)
  (:emitter
   (emit-relative-branch segment #x3A 0 0 target nullify)))

(define-instruction bl (segment target reg &key nullify)
  (:declare (type tn reg) (type label target) (type (member t nil) nullify))
  (:printer branch17 ((op1 #x3A) (op2 0)) '(:name n :tab w "," t))
  (:delay 0)
  (:dependencies (writes reg))
  (:emitter
   (emit-relative-branch segment #x3A (reg-tn-encoding reg) 0 target nullify)))

(define-instruction gateway (segment target reg &key nullify)
  (:declare (type tn reg) (type label target) (type (member t nil) nullify))
  (:printer branch17 ((op1 #x3A) (op2 1)) '(:name n :tab w "," t))
  (:delay 0)
  (:dependencies (writes reg))
  (:emitter
   (emit-relative-branch segment #x3A (reg-tn-encoding reg) 1 target nullify)))

;;; BLR is useless because we have no way to generate the offset.

(define-instruction bv (segment base &key nullify offset)
  (:declare (type tn base)
            (type (member t nil) nullify)
            (type (or tn null) offset))
  (:delay 0)
  (:dependencies (reads base))
  (:printer branch ((op1 #x3A) (op2 6)) '(:name n :tab x "(" t ")"))
  (:emitter
   (emit-branch segment #x3A (reg-tn-encoding base)
                (if offset (reg-tn-encoding offset) 0)
                6 0 (if nullify 1 0) 0)))

(define-instruction be (segment disp space base &key nullify)
  (:declare (type (or fixup (signed-byte 17)) disp)
            (type tn base)
            (type (unsigned-byte 3) space)
            (type (member t nil) nullify))
  (:delay 0)
  (:dependencies (reads base))
  (:printer branch17 ((op1 #x38) (op2 nil :type 'im3))
            '(:name n :tab w "(" op2 "," t ")"))
  (:emitter
   (multiple-value-bind
       (w1 w2 w)
       (decompose-branch-disp segment disp)
     (emit-branch segment #x38 (reg-tn-encoding base) w1
                  (space-encoding space) w2 (if nullify 1 0) w))))

(define-instruction ble (segment disp space base &key nullify)
  (:declare (type (or fixup (signed-byte 17)) disp)
            (type tn base)
            (type (unsigned-byte 3) space)
            (type (member t nil) nullify))
  (:delay 0)
  (:dependencies (reads base))
  (:printer branch17 ((op1 #x39) (op2 nil :type 'im3))
            '(:name n :tab w "(" op2 "," t ")"))
  (:dependencies (writes lip-tn))
  (:emitter
   (multiple-value-bind
       (w1 w2 w)
       (decompose-branch-disp segment disp)
     (emit-branch segment #x39 (reg-tn-encoding base) w1
                  (space-encoding space) w2 (if nullify 1 0) w))))

(defun emit-conditional-branch (segment opcode r2 r1 cond target nullify)
  (emit-back-patch segment 4
    (lambda (segment posn)
      (let ((disp (label-relative-displacement target posn)))
        ; emit-conditional-branch is used by instruction emitters: MOVB, COMB, ADDB and BB
        ; which assembles an immediate of total 12 bits (including sign bit).
        (aver (typep disp '(signed-byte 12)))
        (let ((w1 (logior (ash (ldb (byte 10 0) disp) 1)
                          (ldb (byte 1 10) disp)))
              (w (ldb (byte 1 11) disp))) ; take out the sign bit
          (emit-branch segment opcode r2 r1 cond w1 (if nullify 1 0) w))))))

(defun im5-encoding (value)
  (declare (type (signed-byte 5) value)
           #+nil (values (unsigned-byte 5)))
  (dpb (ldb (byte 4 0) value)
       (byte 4 1)
       (ldb (byte 1 4) value)))

(macrolet ((define-branch-inst (r-name r-opcode i-name i-opcode cond-kind
                                writes-reg)
               (let* ((conditional (symbolicate cond-kind "-CONDITION"))
                      (false-conditional (symbolicate conditional "-FALSE")))
                 `(progn
                   (define-instruction ,r-name (segment cond r1 r2 target &key nullify)
                     (:declare (type ,conditional cond)
                               (type tn r1 r2)
                               (type label target)
                               (type (member t nil) nullify))
                     (:delay 0)
                     ,@(ecase writes-reg
                         (:write-reg
                           '((:dependencies (reads r1) (reads r2) (writes r2))))
                         (:pinned
                           '(:pinned))
                         (nil
                           '((:dependencies (reads r1) (reads r2)))))
;                     ,@(if writes-reg
;                         '((:dependencies (reads r1) (reads r2) (writes r2)))
;                         '((:dependencies (reads r1) (reads r2))))
                     (:printer branch12 ((op1 ,r-opcode) (c nil :type ',conditional))
                      '(:name c n :tab r1 "," r2 "," w))
                     ,@(unless (= r-opcode #x32)
                         `((:printer branch12 ((op1 ,(+ 2 r-opcode))
                                               (c nil :type ',false-conditional))
                            '(:name c n :tab r1 "," r2 "," w))))
                     (:emitter
                      (multiple-value-bind
                            (cond-encoding false)
                          (,conditional cond)
                        (emit-conditional-branch
                         segment (if false ,(+ r-opcode 2) ,r-opcode)
                         (reg-tn-encoding r2) (reg-tn-encoding r1)
                         cond-encoding target nullify))))
                   (define-instruction ,i-name (segment cond imm reg target &key nullify)
                     (:declare (type ,conditional cond)
                               (type (signed-byte 5) imm)
                               (type tn reg)
                               (type (member t nil) nullify))
                     (:delay 0)
;                     ,@(if writes-reg
;                         '((:dependencies (reads reg) (writes reg)))
;                         '((:dependencies (reads reg))))
                     ,@(ecase writes-reg
                         (:write-reg
                           '((:dependencies (reads r1) (reads r2) (writes r2))))
                         (:pinned
                           '(:pinned))
                         (nil
                           '((:dependencies (reads r1) (reads r2)))))
                     (:printer branch12 ((op1 ,i-opcode) (r1 nil :type 'im5)
                                         (c nil :type ',conditional))
                      '(:name c n :tab r1 "," r2 "," w))
                     ,@(unless (= r-opcode #x32)
                               `((:printer branch12 ((op1 ,(+ 2 i-opcode)) (r1 nil :type 'im5)
                                                     (c nil :type ',false-conditional))
                                  '(:name c n :tab r1 "," r2 "," w))))
                     (:emitter
                      (multiple-value-bind
                            (cond-encoding false)
                          (,conditional cond)
                        (emit-conditional-branch
                         segment (if false (+ ,i-opcode 2) ,i-opcode)
                         (reg-tn-encoding reg) (im5-encoding imm)
                         cond-encoding target nullify))))))))
  (define-branch-inst movb #x32 movib #x33 extract/deposit :write-reg)
  (define-branch-inst comb #x20 comib #x21 compare :pinned)
  (define-branch-inst addb #x28 addib #x29 add :write-reg))

(define-instruction bb (segment cond reg posn target &key nullify)
  (:declare (type (member t nil) cond nullify)
            (type tn reg)
            (type (or (member :variable) (unsigned-byte 5)) posn))
  (:delay 0)
  (:dependencies (reads reg))
  (:printer branch12 ((op1 30) (c nil :type 'extract/deposit-condition))
                      '('BVB c n :tab r1 "," w))
  (:emitter
   (multiple-value-bind
       (opcode posn-encoding)
       (if (eq posn :variable)
           (values #x30 0)
           (values #x31 posn))
     (emit-conditional-branch segment opcode posn-encoding
                              (reg-tn-encoding reg)
                              (if cond 2 6) target nullify))))


;;;; Computation Instructions

(define-bitfield-emitter emit-r3-inst 32
  (byte 6 26) (byte 5 21) (byte 5 16) (byte 3 13)
  (byte 1 12) (byte 7 5) (byte 5 0))

(macrolet ((define-r3-inst (name cond-kind opcode &optional pinned)
               `(define-instruction ,name (segment r1 r2 res &optional cond)
                 (:declare (type tn res r1 r2))
                 (:delay 0)
                 ,@(if pinned
                     '(:pinned)
                     '((:dependencies (reads r1) (reads r2) (writes res))))
                 (:printer r3-inst ((op ,opcode) (c nil :type ',(symbolicate
                                                                 cond-kind
                                                                 "-CONDITION"))))
                 ,@(when (eq name 'or)
                         `((:printer r3-inst ((op ,opcode) (r2 0)
                                              (c nil :type ',(symbolicate cond-kind
                                                                          "-CONDITION")))
                            `('COPY :tab r1 "," t))))
                 (:emitter
                  (multiple-value-bind
                        (cond false)
                      (,(symbolicate cond-kind "-CONDITION") cond)
                    (emit-r3-inst segment #x02 (reg-tn-encoding r2) (reg-tn-encoding r1)
                                  cond (if false 1 0) ,opcode
                                  (reg-tn-encoding res)))))))
  (define-r3-inst add add #x30)
  (define-r3-inst addl add #x50)
  (define-r3-inst addo add #x70)
  (define-r3-inst addc add #x38)
  (define-r3-inst addco add #x78)
  (define-r3-inst sh1add add #x32)
  (define-r3-inst sh1addl add #x52)
  (define-r3-inst sh1addo add #x72)
  (define-r3-inst sh2add add #x34)
  (define-r3-inst sh2addl add #x54)
  (define-r3-inst sh2addo add #x74)
  (define-r3-inst sh3add add #x36)
  (define-r3-inst sh3addl add #x56)
  (define-r3-inst sh3addo add #x76)
  (define-r3-inst sub compare #x20)
  (define-r3-inst subo compare #x60)
  (define-r3-inst subb compare #x28)
  (define-r3-inst subbo compare #x68)
  (define-r3-inst subt compare #x26)
  (define-r3-inst subto compare #x66)
  (define-r3-inst ds compare #x22)
  (define-r3-inst comclr compare #x44)
  (define-r3-inst or logical #x12 t) ; as a nop it must be pinned
  (define-r3-inst xor logical #x14)
  (define-r3-inst and logical #x10)
  (define-r3-inst andcm logical #x00)
  (define-r3-inst uxor unit #x1C)
  (define-r3-inst uaddcm unit #x4C)
  (define-r3-inst uaddcmt unit #x4E)
  (define-r3-inst dcor unit #x5C)
  (define-r3-inst idcor unit #x5E))

(define-bitfield-emitter emit-imm-inst 32
  (byte 6 26) (byte 5 21) (byte 5 16) (byte 3 13)
  (byte 1 12) (byte 1 11) (byte 11 0))

(macrolet ((define-imm-inst (name cond-kind opcode subcode &optional pinned)
             `(define-instruction ,name (segment imm src dst &optional cond)
                (:declare (type tn dst src)
                  (type (signed-byte 11) imm))
                (:delay 0)
                (:printer imm-inst ((op ,opcode) (o ,subcode)
                                    (c nil :type
                                       ',(symbolicate cond-kind "-CONDITION"))))
                (:dependencies (reads imm) (reads src) (writes dst))
                (:emitter
                  (multiple-value-bind (cond false)
                      (,(symbolicate cond-kind "-CONDITION") cond)
                    (emit-imm-inst segment ,opcode (reg-tn-encoding src)
                                   (reg-tn-encoding dst) cond
                                   (if false 1 0) ,subcode
                                   (encode-imm11 imm)))))))
  (define-imm-inst addi add #x2D 0)
  (define-imm-inst addio add #x2D 1)
  (define-imm-inst addit add #x2C 0)
  (define-imm-inst addito add #x2C 1)
  (define-imm-inst subi compare #x25 0)
  (define-imm-inst subio compare #x25 1)
  (define-imm-inst comiclr compare #x24 0))

(define-bitfield-emitter emit-extract/deposit-inst 32
  (byte 6 26) (byte 5 21) (byte 5 16) (byte 3 13)
  (byte 3 10) (byte 5 5) (byte 5 0))

(define-instruction shd (segment r1 r2 count res &optional cond)
  (:declare (type tn res r1 r2)
            (type (or (member :variable) (integer 0 31)) count))
  (:delay 0)
  :pinned
  (:printer extract/deposit-inst ((op1 #x34) (op2 2) (t/clen nil :type 'reg))
            '(:name c :tab r1 "," r2 "," cp "," t/clen))
  (:printer extract/deposit-inst ((op1 #x34) (op2 0) (t/clen nil :type 'reg))
            '('VSHD c :tab r1 "," r2 "," t/clen))
  (:emitter
   (etypecase count
     ((member :variable)
      (emit-extract/deposit-inst segment #x34
                                 (reg-tn-encoding r2) (reg-tn-encoding r1)
                                 (extract/deposit-condition cond)
                                 0 0 (reg-tn-encoding res)))
     ((integer 0 31)
      (emit-extract/deposit-inst segment #x34
                                 (reg-tn-encoding r2) (reg-tn-encoding r1)
                                 (extract/deposit-condition cond)
                                 2 (- 31 count)
                                 (reg-tn-encoding res))))))

(macrolet ((define-extract-inst (name opcode)
               `(define-instruction ,name (segment src posn len res &optional cond)
                 (:declare (type tn res src)
                  (type (or (member :variable) (integer 0 31)) posn)
                  (type (integer 1 32) len))
                 (:delay 0)
                 (:dependencies (reads src) (writes res))
                 (:printer extract/deposit-inst ((op1 #x34) (cp nil :type 'integer)
                                                 (op2 ,opcode))
                  '(:name c :tab r2 "," cp "," t/clen "," r1))
                 (:printer extract/deposit-inst ((op1 #x34) (op2 ,(- opcode 2)))
                  '('V :name c :tab r2 "," t/clen "," r1))
                 (:emitter
                  (etypecase posn
                    ((member :variable)
                     (emit-extract/deposit-inst segment #x34 (reg-tn-encoding src)
                                                (reg-tn-encoding res)
                                                (extract/deposit-condition cond)
                                                ,(- opcode 2) 0 (- 32 len)))
                    ((integer 0 31)
                     (emit-extract/deposit-inst segment #x34 (reg-tn-encoding src)
                                                (reg-tn-encoding res)
                                                (extract/deposit-condition cond)
                                                ,opcode posn (- 32 len))))))))
  (define-extract-inst extru 6)
  (define-extract-inst extrs 7))

(macrolet ((define-deposit-inst (name opcode)
             `(define-instruction ,name (segment src posn len res &optional cond)
               (:declare (type tn res)
                (type (or tn (signed-byte 5)) src)
                (type (or (member :variable) (integer 0 31)) posn)
                (type (integer 1 32) len))
               (:delay 0)
               (:dependencies (reads src) (writes res))
               (:printer extract/deposit-inst ((op1 #x35) (op2 ,opcode))
                ',(let ((base '('VDEP c :tab r1 "," t/clen "," r2)))
                       (if (= opcode 0) (cons ''Z base) base)))
               (:printer extract/deposit-inst ((op1 #x35) (op2 ,(+ 2 opcode)))
                ',(let ((base '('DEP c :tab r1 "," cp "," t/clen "," r2)))
                       (if (= opcode 0) (cons ''Z base) base)))
               (:printer extract/deposit-inst ((op1 #x35) (r1 nil :type 'im5)
                                               (op2 ,(+ 4 opcode)))
                ',(let ((base '('VDEPI c :tab r1 "," t/clen "," r2)))
                       (if (= opcode 0) (cons ''Z base) base)))
               (:printer extract/deposit-inst ((op1 #x35) (r1 nil :type 'im5)
                                               (op2 ,(+ 6 opcode)))
                ',(let ((base '('DEPI c :tab r1 "," cp "," t/clen "," r2)))
                       (if (= opcode 0) (cons ''Z base) base)))
               (:emitter
                (multiple-value-bind
                      (opcode src-encoding)
                    (etypecase src
                      (tn
                       (values ,opcode (reg-tn-encoding src)))
                      ((signed-byte 5)
                       (values ,(+ opcode 4) (im5-encoding src))))
                  (multiple-value-bind
                        (opcode posn-encoding)
                      (etypecase posn
                        ((member :variable)
                         (values opcode 0))
                        ((integer 0 31)
                         (values (+ opcode 2) (- 31 posn))))
                    (emit-extract/deposit-inst segment #x35 (reg-tn-encoding res)
                                               src-encoding
                                               (extract/deposit-condition cond)
                                               opcode posn-encoding (- 32 len))))))))

  (define-deposit-inst dep 1)
  (define-deposit-inst zdep 0))



;;;; System Control Instructions.

(define-bitfield-emitter emit-break 32
  (byte 6 26) (byte 13 13) (byte 8 5) (byte 5 0))

(define-instruction break (segment &optional (im5 0) (im13 0))
  (:declare (type (unsigned-byte 13) im13)
            (type (unsigned-byte 5) im5))
  (:cost 0)
  (:delay 0)
  :pinned
  (:printer break () :default :control #'break-control)
  (:emitter
   (emit-break segment 0 im13 0 im5)))

(define-bitfield-emitter emit-system-inst 32
  (byte 6 26) (byte 5 21) (byte 5 16) (byte 3 13) (byte 8 5) (byte 5 0))

(define-instruction ldsid (segment res base &optional (space 0))
  (:declare (type tn res base)
            (type (integer 0 3) space))
  (:delay 0)
  :pinned
  (:printer system-inst ((op2 #x85) (c nil :type 'space)
                         (s nil  :printer #(0 0 1 1 2 2 3 3)))
            `(:name :tab "(" s r1 ")," r3))
  (:emitter
   (emit-system-inst segment 0 (reg-tn-encoding base) 0 (ash space 1) #x85
                     (reg-tn-encoding res))))

(define-instruction mtsp (segment reg space)
  (:declare (type tn reg) (type (integer 0 7) space))
  (:delay 0)
  :pinned
  (:printer system-inst ((op2 #xC1)) '(:name :tab r2 "," s))
  (:emitter
   (emit-system-inst segment 0 0 (reg-tn-encoding reg) (space-encoding space)
                     #xC1 0)))

(define-instruction mfsp (segment space reg)
  (:declare (type tn reg) (type (integer 0 7) space))
  (:delay 0)
  :pinned
  (:printer system-inst ((op2 #x25) (c nil :type 'space)) '(:name :tab s r3))
  (:emitter
   (emit-system-inst segment 0 0 0 (space-encoding space) #x25
                     (reg-tn-encoding reg))))

(deftype control-reg ()
  '(or (unsigned-byte 5) (member :sar)))

(defun control-reg (reg)
  (declare (type control-reg reg)
           #+nil (values (unsigned-byte 32)))
  (if (typep reg '(unsigned-byte 5))
      reg
      (ecase reg
        (:sar 11))))

(define-instruction mtctl (segment reg ctrl-reg)
  (:declare (type tn reg) (type control-reg ctrl-reg))
  (:delay 0)
  :pinned
  (:printer system-inst ((op2 #xC2)) '(:name :tab r2 "," r1))
  (:emitter
   (emit-system-inst segment 0 (control-reg ctrl-reg) (reg-tn-encoding reg)
                     0 #xC2 0)))

(define-instruction mfctl (segment ctrl-reg reg)
  (:declare (type tn reg) (type control-reg ctrl-reg))
  (:delay 0)
  :pinned
  (:printer system-inst ((op2 #x45)) '(:name :tab r1 "," r3))
  (:emitter
   (emit-system-inst segment 0 (control-reg ctrl-reg) 0 0 #x45
                     (reg-tn-encoding reg))))



;;;; Floating point instructions.

(define-bitfield-emitter emit-fp-load/store 32
  (byte 6 26) (byte 5 21) (byte 5 16) (byte 2 14) (byte 1 13) (byte 1 12)
  (byte 2 10) (byte 1 9) (byte 3 6) (byte 1 5) (byte 5 0))

(define-instruction fldx (segment index base result &key modify scale side)
  (:declare (type tn index base result)
            (type (member t nil) modify scale)
            (type (member nil 0 1) side))
  (:delay 0)
  :pinned
  (:printer fp-load/store ((op #x0b) (x1 0) (x2 0) (x3 0))
            `('FLDD ,@cmplt-index-print :tab x "(" s b ")" "," t))
  (:printer fp-load/store ((op #x09) (x1 0) (x2 0) (x3 0))
            `('FLDW ,@cmplt-index-print :tab x "(" s b ")" "," t))
  (:emitter
   (multiple-value-bind
       (result-encoding double-p)
       (fp-reg-tn-encoding result)
     (when side
       (aver double-p)
       (setf double-p nil))
     (emit-fp-load/store segment (if double-p #x0B #x09) (reg-tn-encoding base)
                         (reg-tn-encoding index) 0 (if scale 1 0) 0 0 0
                         (or side 0) (if modify 1 0) result-encoding))))

(define-instruction fstx (segment value index base &key modify scale side)
  (:declare (type tn index base value)
            (type (member t nil) modify scale)
            (type (member nil 0 1) side))
  (:delay 0)
  :pinned
  (:printer fp-load/store ((op #x0b) (x1 0) (x2 0) (x3 1))
            `('FSTD ,@cmplt-index-print :tab t "," x "(" s b ")"))
  (:printer fp-load/store ((op #x09) (x1 0) (x2 0) (x3 1))
            `('FSTW ,@cmplt-index-print :tab t "," x "(" s b ")"))
  (:emitter
   (multiple-value-bind
       (value-encoding double-p)
       (fp-reg-tn-encoding value)
     (when side
       (aver double-p)
       (setf double-p nil))
     (emit-fp-load/store segment (if double-p #x0B #x09) (reg-tn-encoding base)
                         (reg-tn-encoding index) 0 (if scale 1 0) 0 0 1
                         (or side 0) (if modify 1 0) value-encoding))))

(define-instruction flds (segment disp base result &key modify side)
  (:declare (type tn base result)
            (type (signed-byte 5) disp)
            (type (member :before :after nil) modify)
            (type (member nil 0 1) side))
  (:delay 0)
  :pinned
  (:printer fp-load/store ((op #x0b) (x nil :type 'im5) (x1 1) (x2 0) (x3 0))
            `('FLDD ,@cmplt-disp-print :tab x "(" s b ")," t))
  (:printer fp-load/store ((op #x09) (x nil :type 'im5) (x1 1) (x2 0) (x3 0))
            `('FLDW ,@cmplt-disp-print :tab x "(" s b ")," t))
  (:emitter
   (multiple-value-bind
       (result-encoding double-p)
       (fp-reg-tn-encoding result)
     (when side
       (aver double-p)
       (setf double-p nil))
     (emit-fp-load/store segment (if double-p #x0B #x09) (reg-tn-encoding base)
                         (short-disp-encoding segment disp) 0
                         (if (eq modify :before) 1 0) 1 0 0
                         (or side 0) (if modify 1 0) result-encoding))))

(define-instruction fsts (segment value disp base &key modify side)
  (:declare (type tn base value)
            (type (signed-byte 5) disp)
            (type (member :before :after nil) modify)
            (type (member nil 0 1) side))
  (:delay 0)
  :pinned
  (:printer fp-load/store ((op #x0b) (x nil :type 'im5) (x1 1) (x2 0) (x3 1))
            `('FSTD ,@cmplt-disp-print :tab t "," x "(" s b ")"))
  (:printer fp-load/store ((op #x09) (x nil :type 'im5) (x1 1) (x2 0) (x3 1))
            `('FSTW ,@cmplt-disp-print :tab t "," x "(" s b ")"))
  (:emitter
   (multiple-value-bind
       (value-encoding double-p)
       (fp-reg-tn-encoding value)
     (when side
       (aver double-p)
       (setf double-p nil))
     (emit-fp-load/store segment (if double-p #x0B #x09) (reg-tn-encoding base)
                         (short-disp-encoding segment disp) 0
                         (if (eq modify :before) 1 0) 1 0 1
                         (or side 0) (if modify 1 0) value-encoding))))


(define-bitfield-emitter emit-fp-class-0-inst 32
  (byte 6 26) (byte 5 21) (byte 5 16) (byte 3 13) (byte 2 11) (byte 2 9)
  (byte 3 6) (byte 1 5) (byte 5 0))

(define-bitfield-emitter emit-fp-class-1-inst 32
  (byte 6 26) (byte 5 21) (byte 4 17) (byte 2 15) (byte 2 13) (byte 2 11)
  (byte 2 9) (byte 3 6) (byte 1 5) (byte 5 0))

;;; Note: classes 2 and 3 are similar enough to class 0 that we don't need
;;; seperate emitters.

(defconstant-eqx funops '(:copy :abs :sqrt :rnd)
  #'equalp)

(deftype funop ()
  `(member ,@funops))

(define-instruction funop (segment op from to)
  (:declare (type funop op)
            (type tn from to))
  (:delay 0)
  :pinned
  (:printer fp-class-0-inst ((op1 #x0C) (op2 2) (x2 0))
            '('FCPY fmt :tab r "," t))
  (:printer fp-class-0-inst ((op1 #x0C) (op2 3) (x2 0))
            '('FABS fmt  :tab r "," t))
  (:printer fp-class-0-inst ((op1 #x0C) (op2 4) (x2 0))
            '('FSQRT fmt :tab r "," t))
  (:printer fp-class-0-inst ((op1 #x0C) (op2 5) (x2 0))
            '('FRND fmt :tab r "," t))
  (:emitter
   (multiple-value-bind
       (from-encoding from-double-p)
       (fp-reg-tn-encoding from)
     (multiple-value-bind
         (to-encoding to-double-p)
         (fp-reg-tn-encoding to)
       (aver (eq from-double-p to-double-p))
       (emit-fp-class-0-inst segment #x0C from-encoding 0
                             (+ 2 (or (position op funops)
                                      (error "Bogus FUNOP: ~S" op)))
                             (if to-double-p 1 0) 0 0 0 to-encoding)))))

(macrolet ((define-class-1-fp-inst (name subcode)
               `(define-instruction ,name (segment from to)
                 (:declare (type tn from to))
                 (:delay 0)
                 (:printer fp-class-1-inst ((op1 #x0C) (x2 ,subcode))
                  '(:name sf df :tab r "," t))
                 (:emitter
                  (multiple-value-bind
                        (from-encoding from-double-p)
                      (fp-reg-tn-encoding from)
                    (multiple-value-bind
                          (to-encoding to-double-p)
                        (fp-reg-tn-encoding to)
                      (emit-fp-class-1-inst segment #x0C from-encoding 0 ,subcode
                                            (if to-double-p 1 0) (if from-double-p 1 0)
                                            1 0 0 to-encoding)))))))

  (define-class-1-fp-inst fcnvff 0)
  (define-class-1-fp-inst fcnvxf 1)
  (define-class-1-fp-inst fcnvfx 2)
  (define-class-1-fp-inst fcnvfxt 3))

(define-instruction fcmp (segment cond r1 r2)
  (:declare (type (unsigned-byte 5) cond)
            (type tn r1 r2))
  (:delay 0)
  :pinned
  (:printer fp-class-0-inst ((op1 #x0C) (op2 0) (x2 2) (t nil :type 'fcmp-cond))
            '(:name fmt t :tab r "," x1))
  (:emitter
   (multiple-value-bind
       (r1-encoding r1-double-p)
       (fp-reg-tn-encoding r1)
     (multiple-value-bind
         (r2-encoding r2-double-p)
         (fp-reg-tn-encoding r2)
       (aver (eq r1-double-p r2-double-p))
       (emit-fp-class-0-inst segment #x0C r1-encoding r2-encoding 0
                             (if r1-double-p 1 0) 2 0 0 cond)))))

(define-instruction ftest (segment)
  (:delay 0)
  :pinned
  (:printer fp-class-0-inst ((op1 #x0c) (op2 1) (x2 2)) '(:name))
  (:emitter
   (emit-fp-class-0-inst segment #x0C 0 0 1 0 2 0 1 0)))

(defconstant-eqx fbinops '(:add :sub :mpy :div)
  #'equalp)

(deftype fbinop ()
  `(member ,@fbinops))

(define-instruction fbinop (segment op r1 r2 result)
  (:declare (type fbinop op)
            (type tn r1 r2 result))
  (:delay 0)
  :pinned
  (:printer fp-class-0-inst ((op1 #x0C) (op2 0) (x2 3))
            '('FADD fmt :tab r "," x1 "," t))
  (:printer fp-class-0-inst ((op1 #x0C) (op2 1) (x2 3))
            '('FSUB fmt :tab r "," x1 "," t))
  (:printer fp-class-0-inst ((op1 #x0C) (op2 2) (x2 3))
            '('FMPY fmt :tab r "," x1 "," t))
  (:printer fp-class-0-inst ((op1 #x0C) (op2 3) (x2 3))
            '('FDIV fmt :tab r "," x1 "," t))
  (:emitter
   (multiple-value-bind
       (r1-encoding r1-double-p)
       (fp-reg-tn-encoding r1)
     (multiple-value-bind
         (r2-encoding r2-double-p)
         (fp-reg-tn-encoding r2)
       (aver (eq r1-double-p r2-double-p))
       (multiple-value-bind
           (result-encoding result-double-p)
           (fp-reg-tn-encoding result)
         (aver (eq r1-double-p result-double-p))
         (emit-fp-class-0-inst segment #x0C r1-encoding r2-encoding
                               (or (position op fbinops)
                                   (error "Bogus FBINOP: ~S" op))
                               (if r1-double-p 1 0) 3 0 0
                               result-encoding))))))



;;;; Instructions built out of other insts.

(define-instruction-macro move (src dst &optional cond)
  `(inst or ,src zero-tn ,dst ,cond))

(define-instruction-macro nop (&optional cond)
  `(inst or zero-tn zero-tn zero-tn ,cond))

(define-instruction li (segment value reg)
  (:declare (type tn reg)
            (type (or fixup (signed-byte 32) (unsigned-byte 32)) value))
  (:delay 0)
  (:dependencies (reads reg))
  (:vop-var vop)
  (:emitter
   (assemble (segment vop)
     (etypecase value
       (fixup
        (inst ldil value reg)
        (inst ldo value reg reg :unsigned t))
       ((signed-byte 14)
        (inst ldo value zero-tn reg))
       ((or (signed-byte 32) (unsigned-byte 32))
        (let ((lo (ldb (byte 11 0) value)))
          (inst ldil value reg)
          (inst ldo lo reg reg :unsigned t)))))))

(define-instruction-macro sll (src count result &optional cond)
  (once-only ((result result) (src src) (count count) (cond cond))
    `(inst zdep ,src (- 31 ,count) (- 32 ,count) ,result ,cond)))

(define-instruction-macro sra (src count result &optional cond)
  (once-only ((result result) (src src) (count count) (cond cond))
    `(inst extrs ,src (- 31 ,count) (- 32 ,count) ,result ,cond)))

(define-instruction-macro srl (src count result &optional cond)
  (once-only ((result result) (src src) (count count) (cond cond))
    `(inst extru ,src (- 31 ,count) (- 32 ,count) ,result ,cond)))

(defun maybe-negate-cond (cond negate)
  (if negate
      (multiple-value-bind
          (value negate)
          (compare-condition cond)
        (if negate
            (nth value compare-conditions)
            (nth (+ value 8) compare-conditions)))
      cond))

(define-instruction bc (segment cond not-p r1 r2 target)
  (:declare (type compare-condition cond)
            (type (member t nil) not-p)
            (type tn r1 r2)
            (type label target))
  (:delay 0)
  (:dependencies (reads r1) (reads r2))
  (:vop-var vop)
  (:emitter
   (emit-chooser segment 8 2
     (lambda (segment posn delta)
       (let ((disp (label-relative-displacement target posn delta)))
         (when (<= 0 disp (1- (ash 1 11)))
           (assemble (segment vop)
             (inst comb (maybe-negate-cond cond not-p) r1 r2 target
                   :nullify t))
           t)))
     (lambda (segment posn)
       (let ((disp (label-relative-displacement target posn)))
         (assemble (segment vop)
           (cond ((typep disp '(signed-byte 12))
                  (inst comb (maybe-negate-cond cond not-p) r1 r2 target)
                  (inst nop)) ; FIXME-lav, cant nullify when backward branch
                 (t
                  (inst comclr r1 r2 zero-tn
                        (maybe-negate-cond cond (not not-p)))
                  (inst b target :nullify t)))))))))

(define-instruction bci (segment cond not-p imm reg target)
  (:declare (type compare-condition cond)
            (type (member t nil) not-p)
            (type (signed-byte 11) imm)
            (type tn reg)
            (type label target))
  (:delay 0)
  (:dependencies (reads reg))
  (:vop-var vop)
  (:emitter
   (emit-chooser segment 8 2
     (lambda (segment posn delta-if-after)
       (let ((disp (label-relative-displacement target posn delta-if-after)))
         (when (and (<= 0 disp (1- (ash 1 11)))
                    (typep imm '(signed-byte 5)))
           (assemble (segment vop)
             (inst comib (maybe-negate-cond cond not-p) imm reg target
                   :nullify t))
           t)))
     (lambda (segment posn)
       (let ((disp (label-relative-displacement target posn)))
         (assemble (segment vop)
           (cond ((and (typep disp '(signed-byte 12))
                       (typep imm '(signed-byte 5)))
                  (inst comib (maybe-negate-cond cond not-p) imm reg target)
                  (inst nop))
                 (t
                  (inst comiclr imm reg zero-tn
                        (maybe-negate-cond cond (not not-p)))
                  (inst b target :nullify t)))))))))


;;;; Instructions to convert between code ptrs, functions, and lras.

(defun emit-header-data (segment type)
  (emit-back-patch
   segment 4
   (lambda (segment posn)
     (emit-word segment
                (logior type
                        (ash (+ posn (component-header-length))
                             (- n-widetag-bits word-shift)))))))

(define-instruction simple-fun-header-word (segment)
  :pinned
  (:cost 0)
  (:delay 0)
  (:emitter
   (emit-header-data segment simple-fun-header-widetag)))

(define-instruction lra-header-word (segment)
  :pinned
  (:cost 0)
  (:delay 0)
  (:emitter
   (emit-header-data segment return-pc-header-widetag)))


(defun emit-compute-inst (segment vop src label temp dst calc)
  (emit-chooser
   ;; We emit either 12 or 4 bytes, so we maintain 3 byte alignments.
   segment 12 3
   ;; This is the best-case that emits one instruction ( 4 bytes )
   (lambda (segment posn delta-if-after)
     (let ((delta (funcall calc label posn delta-if-after)))
       ;; WHEN, Why not AVER ?
       (when (typep delta '(signed-byte 11))
         (emit-back-patch segment 4
                          (lambda (segment posn)
                            (assemble (segment vop)
                              (inst addi (funcall calc label posn 0) src
                                    dst))))
         t)))
   ;; This is the worst-case that emits three instruction ( 12 bytes )
   (lambda (segment posn)
     (let ((delta (funcall calc label posn 0)))
       ;; FIXME-lav: why do we hit below check ?
       ;;  (when (<= (- (ash 1 10)) delta (1- (ash 1 10)))
       ;;   (error "emit-compute-inst selected worst-case, but is shrinkable, delta is ~s" delta))
       ;; Note: if we used addil/ldo to do this in 2 instructions then the
       ;; intermediate value would be tagged but pointing into space.
       ;; Does above note mean that the intermediate value would be
       ;; a bogus pointer that would be GCed wrongly ?
       ;; Also what I can see addil would also overwrite NFP (r1) ???
       (assemble (segment vop)
         ;; Three instructions (4 * 3) this is the reason for 12 bytes
         (inst ldil delta temp)
         (inst ldo (ldb (byte 11 0) delta) temp temp :unsigned t)
         (inst add src temp dst))))))

(macrolet ((compute ((name) &body body)
             `(define-instruction ,name (segment src label temp dst)
               (:declare (type tn src dst temp) (type label label))
               (:attributes variable-length)
               (:dependencies (reads src) (writes dst) (writes temp))
               (:delay 0)
               (:vop-var vop)
               (:emitter
                 (emit-compute-inst segment vop src label temp dst
                                    ,@body)))))
  (compute (compute-code-from-lip)
    (lambda (label posn delta-if-after)
      (- other-pointer-lowtag
         (label-position label posn delta-if-after)
         (component-header-length))))
  (compute (compute-code-from-lra)
    (lambda (label posn delta-if-after)
      (- (+ (label-position label posn delta-if-after)
            (component-header-length)))))
  (compute (compute-lra-from-code)
     (lambda (label posn delta-if-after)
       (+ (label-position label posn delta-if-after)
          (component-header-length)))))

;;;; Data instructions.
(define-bitfield-emitter emit-word 32
  (byte 32 0))

(macrolet ((data (size type)
             `(define-instruction ,size (segment ,size)
                (:declare (type ,type ,size))
                (:cost 0)
                (:delay 0)
                :pinned
                (:emitter
                 (etypecase ,size
                   ,@(when (eq size 'word)
                       '((fixup
                          (note-fixup segment :absolute word)
                          (emit-word segment 0))))
                   (integer
                    (,(symbolicate "EMIT-" size) segment ,size)))))))
  (data byte  (or (unsigned-byte 8)  (signed-byte 8)))
  (data short (or (unsigned-byte 16) (signed-byte 16)))
  (data word  (or (unsigned-byte 32) (signed-byte 32) fixup)))
