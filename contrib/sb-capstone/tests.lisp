(defpackage "SB-CAPSTONE-TESTS"
  (:use :common-lisp :sb-capstone :sb-alien :sb-rt))

(in-package "SB-CAPSTONE-TESTS")

(defun copy-c-string (src dest &aux (index 0))
  (loop (let ((b (sb-sys:sap-ref-8 src index)))
          (when (= b 0)
            (setf (fill-pointer dest) index)
            (return))
          (setf (char dest index) (code-char b))
          (incf index))))

(defun disassem-with-capstone (stream handle insn mnemonic operands insn-addr length starting-vaddr)
  (with-alien ((paddr unsigned)
               (vaddr unsigned)
               (remaining unsigned))
    (setq paddr insn-addr
          vaddr starting-vaddr
          remaining length)
    (loop
      (multiple-value-bind (successful new-paddr new-remaining new-vaddr)
          (cs-disasm-iter handle paddr remaining vaddr insn)
        (setf paddr new-paddr)
        (setf remaining new-remaining)
        (setf vaddr new-vaddr)
        (unless successful
          (return))
        (copy-c-string (alien-sap (slot insn 'insn-mnemonic)) mnemonic)
        (copy-c-string (alien-sap (slot insn 'insn-operands)) operands)
        (format stream "~a ~a~%"
                (string-upcase mnemonic)
                (string-upcase operands))))))

(defun disassem-for-target (stream target insn-addr length starting-vaddr)
  (multiple-value-bind (return-code handle) (cs-open-for-target target)
    (declare (ignore return-code))
    (cs-option handle
               cs-opt-detail
               cs-opt-on)
    (let* ((insn (cs-malloc handle)) ; Get an address for a new cs-insn
           (mnemonic (make-array 31 :element-type 'base-char :fill-pointer t))
           (operands (make-array 159 :element-type 'base-char :fill-pointer t)))
      (disassem-with-capstone stream handle insn mnemonic operands insn-addr length starting-vaddr)
      (cs-free insn 1) ; Free up memory from cs-malloc
      (cs-close handle))))

;; FIXME: Mock capstone instead of loading the real Capstone shared object
(defun capstone-check (bytes-untyped target instructions-to-check)
  (let ((bytes (coerce bytes-untyped '(array (unsigned-byte 8) (*)))))
    ;; We need bytes to be pinned as we are going to make use of its memory location
    (sb-sys:with-pinned-objects (bytes)
      (let ((base (sb-sys:vector-sap bytes))
            (stream (make-string-output-stream)))
        (disassem-for-target stream target (sb-sys:sap-int base) (length bytes) 0)
        (let ((instructions (get-output-stream-string stream)))
          ;; Search returns a 0-indexed position, or NIL
          (every #'(lambda (instruction) (search instruction instructions))
                               instructions-to-check))))))

(deftest x86-64 (capstone-check '(#x8F #x45 #x08 #x48 #x8B #xF0)
                                '(:x86-64 :little-endian)
                                '("MOV RSI, RAX"
                                  "POP QWORD PTR [RBP + 8]"))
                t)


(deftest ppc-little-endian (capstone-check '(#x03 #x10 #x40 #x3c #x00 #x71 #x42 #x38)
                                           '(:ppc64 :little-endian)
                                           '("LIS R2, 0X1003"
                                             "ADDI R2, R2, 0X7100"))
                           t)

(deftest ppc-big-endian (capstone-check '(#x7c #x08 #x02 #xa6)
                                        '(:ppc64 :big-endian)
                                        '("MFLR R0"))
                        t)
