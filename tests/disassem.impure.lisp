
(with-test (:name :disassemble-macro)
  (with-output-to-string (s)
    (disassemble 'and :stream s)))

(with-test (:name :disassemble-special-form-fails)
  (assert-error (disassemble 'progn)))

(with-test (:name :disassemble-sap)
  (with-output-to-string (s)
    (sb-c:dis (sb-sys:int-sap (+ (- (sb-kernel:get-lisp-obj-address #'car)
                                    sb-vm:fun-pointer-lowtag)
                                 (* 2 sb-vm:n-word-bytes)))
              12
              s)))

;;; CLISP:
;;;    [1]> (disassemble (defmethod hello ((self cons)) "here i am"))
;;;    Disassembly of function :LAMBDA
;;;    (CONST 0) = "here i am"
;;;    2 required arguments
;;;    0 optional arguments
;;;    No rest parameter
;;;    No keyword parameters
;;;    2 byte-code instructions:
;;;    0     (CONST 0)                           ; "here i am"
;;;    1     (SKIP&RET 3)

;;; ABCL:
;;;    CL-USER(1): (disassemble (defmethod hello ((self cons)) "here i am"))
;;;    /* Java disassembler */
;;;    ...

;;; I'm not sure how to implement CLISP-compatibility and ABCL-compatibility
;;; via the standard interface now, so test the nonstandard interface.
(with-test (:name :disassemble-method)
  (with-output-to-string (s)
    (sb-c:dis (defmethod hello ((self cons)) "here i am") s)))
