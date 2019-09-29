
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

;;; This can be used to verify that all of the instruction printers respond
;;; correctly (or at least, produce no characters on *standard-output*)
;;; when given NIL as a stream.
;;; If there is any junk between "Start" and "End" other than the animation
;;; then something is wrong.
;;; Unfortunately I think is too slow to make part of the test suite,
;;; which speaks to the terrible performance of our disassembler.
#+nil
(defun disassemble-everything (&aux (dstate (make-dstate)) (i 0))
  (declare (inline %make-segment))
  (format t "~&Start ...  ")
  (force-output)
  (dolist (code (funcall 'sb-vm::list-allocated-objects :all :type sb-vm:code-header-widetag))
    (write-char #\backspace)
    (write-char (aref "\\|/-" (mod (incf i) 4)))
    (force-output)
    (dotimes (j (sb-kernel:code-n-entries code))
      (let* ((f (sb-kernel:%code-entry-point code j))
             (sap (sb-vm:simple-fun-entry-sap f))
             (start (sap-int sap))
             (len (sb-kernel:%simple-fun-text-len f j)))
        ;; we won't - but should - dxify (lambda () start) when so declared
        (dx-flet ((sap-maker () sap))
          (dx-let ((seg (%make-segment :sap-maker #'sap-maker
                                       :virtual-location start
                                       :length len)))
            (map-segment-instructions
             (lambda (chunk inst)
               (declare (type dchunk chunk) (type instruction inst))
               (awhen (inst-printer inst)
                 (funcall it chunk inst nil dstate)
                 (setf (dstate-operands dstate) nil)))
             seg dstate nil))))))
  (format t " done~%"))
