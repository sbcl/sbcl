(in-package "SB-ARM64-ASM")

(defun current-instruction (dstate &optional (offset 0))
  (sap-ref-int (dstate-segment-sap dstate)
               (+ (dstate-cur-offs dstate) offset)
               n-word-bytes
               (dstate-byte-order dstate)))

(defun 32-bit-register-p (dstate)
  (not (logbitp 31 (current-instruction dstate))))

(defun print-ubfm-alias-name (value stream dstate)
  (declare (ignore dstate))
  (destructuring-bind (immr imms) value
    (princ (cond ((and (/= imms 63)
                       (= (1+ imms) immr))
                  'lsl)
                 ((< imms immr)
                  'ubfiz)
                 (t
                  'ubfm))
           stream)))

(defun print-ubfm-alias (value stream dstate)
  (declare (ignore dstate))
  (destructuring-bind (immr imms) value
    (cond ((and (/= imms 63)
                (= (1+ imms) immr))
           (format stream "#~d" (- 63 imms)))
          ((< imms immr)
           (format stream "#~d, #~d" (- 64 immr) (1+ imms)))
          (t
           (format stream "#~d, #~d" immr imms)))))

(defun print-mem-bar-kind (value stream dstate)
  (declare (ignore dstate))
  (let ((kind (car (rassoc value **mem-bar-kinds**))))
    (if kind
        (princ kind stream)
        (format stream "#~d" value))))

(defun print-shift (value stream dstate)
  (declare (ignore dstate))
  (destructuring-bind (kind amount) value
    (when (plusp amount)
      (princ ", " stream)
      (princ (ecase kind
               (#b00 "LSL")
               (#b01 "LSR")
               (#b10 "ASR")
               (#b11 "ROR"))
             stream)
      (format stream " #~d" amount))))

(defun print-wide-shift (value stream dstate)
  (declare (ignore dstate))
  (when (plusp value)
    (format stream ", LSL #~d" (* value 16))))

(defun print-2-bit-shift (value stream dstate)
  (declare (ignore dstate))
  (when (= value 1)
    (princ ", LSL #12" stream)))

(defun print-extend (value stream dstate)
  (destructuring-bind (kind amount) value
    (let* ((inst (current-instruction dstate))
           (rd (ldb (byte 5 0) inst))
           (rn (ldb (byte 5 5) inst)))
      (princ ", " stream)
      (princ (if (and (= kind #b011)
                      (or (= rd nsp-offset)
                          (= rn nsp-offset)))
                 "LSL"
                 (ecase kind
                   (#b000 "UXTB")
                   (#b001 "UXTH")
                   (#b010 "UXTW")
                   (#b011 "UXTX")
                   (#b100 "SXTB")
                   (#b101 "SXTH")
                   (#b110 "SXTW")
                   (#b111 "SXTX")))
             stream))
    (when (plusp amount)
      (format stream " #~d" amount))))

(defun print-ldr-str-extend (value stream dstate)
  (declare (ignore dstate))
  (destructuring-bind (kind amount) value
    (unless (and (= kind #b011)
                 (zerop amount))
      (princ ", " stream)
      (princ (ecase kind
               (#b010 "UXTW")
               (#b011 "LSL")
               (#b110 "SXTW")
               (#b111 "SXTX"))
             stream))
    (when (plusp amount)
      (princ " #3" stream))))

(defun print-immediate (value stream dstate)
  (declare (ignore dstate))
  (format stream "#~D" value))

(defun print-test-branch-immediate (value stream dstate)
  (declare (ignore dstate))
  (format stream "#~D"
          (dpb (car value) (byte 1 5) (cadr value))))

(defun decode-scaled-immediate (value)
  (destructuring-bind (size opc value simd) value
    (if (= simd 1)
        (ash value (logior (ash opc 2) size))
        (ash value size))))

(defun print-scaled-immediate (value stream dstate)
  (declare (ignore dstate))
  (format stream "#~D" (if (consp value)
                           (decode-scaled-immediate value)
                           (ash value 3))))

(defun print-logical-immediate (value stream dstate)
  (declare (ignore dstate))
  (format stream "#~D" (apply #'decode-logical-immediate value)))

(defun print-imm-writeback (value stream dstate)
  (declare (ignore dstate))
  (destructuring-bind (imm mode) value
    (let ((imm (sign-extend imm 9)))
      (if (zerop imm)
          (princ "]" stream)
          (ecase mode
            (#b00
             (format stream ", #~D]" imm))
            (#b01
             (format stream "], #~D" imm))
            (#b11
             (format stream ", #~D]!" imm)))))))

(defun decode-pair-scaled-immediate (opc value simd)
  (ash (sign-extend value 7)
       (+ 2 (ash opc (- (logxor 1 simd))))))

(defun print-pair-imm-writeback (value stream dstate)
  (declare (ignore dstate))
  (destructuring-bind (mode &rest imm) value
    (let ((imm (apply #'decode-pair-scaled-immediate imm)))
      (if (zerop imm)
          (princ "]" stream)
          (ecase mode
            (#b01
             (format stream "], #~D" imm))
            (#b10
             (format stream ", #~D]" imm))
            (#b11
             (format stream ", #~D]!" imm)))))))

(defun print-w-reg (value stream dstate)
  (declare (ignore dstate))
  (princ "W" stream)
  (princ (aref *register-names* value) stream))

(defun print-x-reg (value stream dstate)
  (declare (ignore dstate))
  (princ (aref *register-names* value) stream))

(defun print-reg (value stream dstate)
  (when (32-bit-register-p dstate)
    (princ "W" stream))
  (princ (aref *register-names* value) stream))

(defun print-x-reg-sp (value stream dstate)
  (declare (ignore dstate))
  (if (= value nsp-offset)
      (princ "NSP" stream)
      (princ (aref *register-names* value) stream)))

(defun print-reg-sp (value stream dstate)
  (when (32-bit-register-p dstate)
    (princ "W" stream))
  (if (= value nsp-offset)
      (princ "NSP" stream)
      (princ (aref *register-names* value) stream)))

(defun print-sized-reg (value stream dstate)
  (declare (ignore dstate))
  (destructuring-bind (size reg) value
    (when (zerop size)
      (princ "W" stream))
    (princ (svref *register-names* reg) stream)))

(defun print-reg-float-reg (value stream dstate)
  (let* ((inst (current-instruction dstate))
         (v (ldb (byte 1 26) inst)))
    (if (= (length value) 3)
        (destructuring-bind (size opc reg) value
          (cond ((zerop v)
                 (when (= size #b10)
                   (princ "W" stream))
                 (princ (svref *register-names* reg) stream))
                (t
                 (format stream "~a~d"
                         (cond ((and (= size #b10)
                                     (= opc #b0))
                                "S")
                               ((and (= size #b11)
                                     (= opc #b0))
                                "D")
                               ((and (= size #b00)
                                     (= opc #b1))
                                "Q"))
                         reg))))
        (destructuring-bind (size reg) value
          (cond ((zerop v)
                 (when (zerop size)
                   (princ "W" stream))
                 (princ (svref *register-names* reg) stream))
                (t
                 (format stream "~a~d"
                         (case size
                           (#b00 "S")
                           (#b01 "D")
                           (#b10 "Q"))
                         reg)))))))

(defun print-float-reg (value stream dstate)
  (multiple-value-bind (type value)
      (if (consp value)
          (values (car value) (cadr value))
          (values (ldb (byte 1 22) (current-instruction dstate))
                  value))
    (format stream "~a~d"
            (if (= type 1)
                "D"
                "S")
            value)))

(defun print-simd-reg (value stream dstate)
  (declare (ignore dstate))
  (destructuring-bind (size offset) value
    (format stream "V~d.~a" offset
            (if (zerop size)
                "8B"
                "16B"))))

(defun print-vbhs (value stream dstate)
  (declare (ignore dstate))
  (destructuring-bind (size offset) value
    (format stream "~a~d"
            (case size
              (#b00 "B")
              (#b01 "H")
              (#b10 "S"))
            offset)))

(defun print-vhsd (value stream dstate)
  (declare (ignore dstate))
  (destructuring-bind (size offset) value
    (format stream "~a~d"
            (case size
              (#b00 "H")
              (#b01 "S")
              (#b10 "D"))
            offset)))

(defun print-vx.t (value stream dstate)
  (declare (ignore dstate))
  (destructuring-bind (q size offset) value
    (format stream "V~d.~a"
            offset
            (cond ((and (= size 0)
                        (= q 0))
                   "8B")
                  ((and (= size 0)
                        (= q 1))
                   "16B")
                  ((and (= size 1)
                        (= q 0))
                   "4H")
                  ((and (= size 1)
                        (= q 1))
                   "8H")
                  ((and (= size 2)
                        (= q 1))
                   "4S")))))

(defun lowest-set-bit-index (integer-value)
  (max 0 (1- (integer-length (logand integer-value (- integer-value))))))

(defun print-simd-copy-reg (value stream dstate)
  (declare (ignore dstate))
  (destructuring-bind (offset imm5 &optional imm4) value
    (let ((index (lowest-set-bit-index imm5)))
     (format stream "V~d.~a[~a]" offset
             (char "BHSD" index)
             (if imm4
                 (ash imm4 (- index))
                 (ash imm5 (- (1+ index))))))))

(defun print-sys-reg (value stream dstate)
  (declare (ignore dstate))
  (princ (decode-sys-reg value) stream))

(defun print-cond (value stream dstate)
  (declare (ignore dstate))
  (princ (svref +condition-name-vec+ value) stream))

(defun use-label (value dstate)
  (let* ((value (if (consp value)
                    (logior (ldb (byte 2 0) (car value))
                            (ash (cadr value) 2))
                    (ash value 2)))
         (address (+ value (dstate-cur-addr dstate))))
    ;; LRA pointer
    (if (= (logand address lowtag-mask) other-pointer-lowtag)
        (- address (- other-pointer-lowtag n-word-bytes))
        address)))


(defun annotate-ldr-str (register offset dstate)
  (case register
    (#.sb-vm::null-offset
     (let ((offset (+ sb-vm:nil-value offset)))
       (or (maybe-note-static-symbol (logior offset other-pointer-lowtag)
                                     dstate)
           (maybe-note-assembler-routine offset nil dstate))))
    #+sb-thread
    (#.sb-vm::thread-offset
     (let* ((thread-slots
              (load-time-value
               (primitive-object-slots (primitive-object 'sb-vm::thread))
               t))
            (slot (find (ash offset (- word-shift)) thread-slots :key #'slot-offset)))
       (if slot
           (note (lambda (stream)
                   (format stream "~(~A~)" (slot-name slot)))
                 dstate)
           (flet ((guess-symbol (predicate)
                    (binding* ((code-header (seg-code (dstate-segment dstate)) :exit-if-null)
                               (header-n-words (code-header-words code-header)))
                      (loop for word-num from code-constants-offset below header-n-words
                            for obj = (code-header-ref code-header word-num)
                            when (and (symbolp obj) (funcall predicate obj))
                            do (return obj)))))
             (let ((symbol (or (guess-symbol
                                (lambda (s) (= (symbol-tls-index s) offset)))
                               ;; static symbols aren't in the code header
                               (find offset +static-symbols+
                                     :key #'symbol-tls-index))))
               (when symbol
                 (note (lambda (stream) (format stream "tls: ~S" symbol))
                       dstate)))))))))

(defun find-value-from-previos-inst (register dstate)
  ;; Needs to be MOVZ REGISTER, imm, LSL #0
  ;; Should cover most offsets in sane code
  (let ((inst (current-instruction dstate -4)))
    (when (and (= (ldb (byte 9 23) inst) #b110100101) ;; MOVZ
               (= (ldb (byte 5 0) inst) register)
               (= (ldb (byte 2 21) inst) 0)) ;; LSL #0
      (ldb (byte 16 5) inst))))

(defun annotate-ldr-str-reg (value stream dstate)
  (declare (ignore stream))
  (let* ((inst (current-instruction dstate))
         (float (ldb-test (byte 1 26) inst)))
    (unless float
      (let ((value (find-value-from-previos-inst value dstate)))
        (when value
          (annotate-ldr-str (ldb (byte 5 5) inst) value dstate))))))

(defun annotate-ldr-str-imm (value stream dstate)
  (declare (ignore stream))
  (let* ((inst (current-instruction dstate))
         (float-reg (ldb-test (byte 1 26) inst)))
    (unless float-reg
      (annotate-ldr-str (ldb (byte 5 5) inst)
                        (if (consp value)
                            (decode-scaled-immediate value)
                            value)
                        dstate))))

(defun annotate-ldr-str-pair (value stream dstate)
  (declare (ignore stream) (ignorable dstate))
  (destructuring-bind (reg offset) value
    (declare (ignorable offset))
    (case reg
      #+sb-thread
      (#.sb-vm::thread-offset
       (let* ((thread-slots
                (load-time-value
                 (primitive-object-slots (primitive-object 'sb-vm::thread))
                 t))
              (slot1 (find offset thread-slots :key #'slot-offset))
              (slot2 (find (1+ offset) thread-slots :key #'slot-offset)))
         (when slot1
           (note (lambda (stream)
                   (if (memq (slot-name slot1) '(sb-vm::mixed-tlab
                                                 sb-vm::unboxed-tlab))
                       (format stream "~(~a~).{free-pointer, end-addr}" (slot-name slot1))
                       (format stream "~(~A, ~A~)" (slot-name slot1) (slot-name slot2))))
                 dstate)))))))

(defun annotate-ldr-literal (value stream dstate)
  (declare (ignore stream))
  (let* ((value (* 4 value))
         (seg (dstate-segment dstate))
         (code (seg-code seg)))
    (when code
      (or (note-code-constant (sb-disassem::segment-offs-to-code-offs
                               (+ (dstate-cur-offs dstate) value) seg)
                              dstate)
          (let ((addr (+ (dstate-cur-addr dstate) value)))
            (and (sb-disassem::points-to-code-constant-p addr code)
                 (maybe-note-assembler-routine (sap-ref-word (int-sap addr) 0)
                                               nil dstate)))))))

;;;; special magic to support decoding internal-error and related traps
;;; See EMIT-ERROR-BREAK for the scheme
(defun snarf-error-junk (sap offset trap-number &optional length-only)
  (let* ((inst (sap-ref-32 sap (- offset 4)))
         (error-number (cond
                         ((>= trap-number sb-vm:error-trap)
                          (prog1
                              (- trap-number sb-vm:error-trap)
                            (setf trap-number sb-vm:error-trap)))
                         (t
                          (prog1 (sap-ref-8 sap offset)
                            (incf offset)))))
         (first-arg (ldb (byte 8 13) inst))
         (length (sb-kernel::error-length error-number))
         (index offset))
    (declare (type sb-sys:system-area-pointer sap)
             (type (unsigned-byte 8) length))
    (unless (or (= first-arg sb-vm::zr-offset)
                (zerop length))
      (decf length))
    (cond (length-only
           (loop repeat length do (sb-c:sap-read-var-integerf sap index))
           (values 0 (- index offset) nil nil))
          (t
           (collect ((sc+offsets)
                     (lengths))
             (unless (= first-arg sb-vm::zr-offset)
               (sc+offsets (make-sc+offset sb-vm:descriptor-reg-sc-number first-arg))
               (lengths 0))
             (loop repeat length do
                   (let ((old-index index))
                     (sc+offsets (sb-c:sap-read-var-integerf sap index))
                     (lengths (- index old-index))))
             (values error-number
                     (- index offset)
                     (sc+offsets)
                     (lengths)))))))

(defun brk-control (chunk inst stream dstate)
  (declare (ignore inst chunk))
  (let ((code (ldb (byte 8 5) (current-instruction dstate))))
    (flet ((nt (x) (if stream (note x dstate))))
      (case (if (> code error-trap)
                error-trap
                code)
        (#.halt-trap
         (nt "Halt trap"))
        (#.pending-interrupt-trap
         (nt "Pending interrupt trap"))
        (#.error-trap
         (handle-break-args #'snarf-error-junk code stream dstate))
        (#.cerror-trap
         (nt "Cerror trap")
         (handle-break-args #'snarf-error-junk code stream dstate))
        (#.breakpoint-trap
         (nt "Breakpoint trap"))
        (#.fun-end-breakpoint-trap
         (nt "Function end breakpoint trap"))
        (#.single-step-around-trap
         (nt "Single step around trap"))
        (#.single-step-before-trap
         (nt "Single step before trap"))
        (#.invalid-arg-count-trap
         (nt "Invalid argument count trap"))))))
