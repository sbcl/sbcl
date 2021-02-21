
;;; Assert that save-lisp-and-die didn't accidentally recreate the inst space.
;;; Fails in parallel-exec which uses PURE-RUNNER which performs ENCAPSULATE
;;; which calls REMOVE-STATIC-LINKS which invokes the disassembler
;;; which constructs the inst-space.
(with-test (:name :inst-space-jit-constructed
                  :fails-on :parallel-test-runner)
  (assert (null sb-disassem::*disassem-inst-space*)))

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
(with-test (:name :disassemble-method
            :skipped-on :interpreter)
  (with-output-to-string (s)
    (sb-c:dis (defmethod hello ((self cons)) "here i am") s)))

(with-test (:name :lp-bug-1861418)
  (disassemble '(lambda ()
                 (let* ((*s* 692985202))
                   (declare (special *s*))
                   0))
               :stream (make-string-output-stream)))

;;; This can be used to verify that all of the instruction printers respond
;;; correctly (or at least, produce no characters on *standard-output*)
;;; when given NIL as a stream.
;;; If there is any junk between "Start" and "End" other than the animation
;;; then something is wrong.
;;; Unfortunately I think is too slow to make part of the test suite,
;;; which speaks to the terrible performance of our disassembler.
(in-package "SB-DISASSEM")
(defun list-all-code ()
  (sb-vm::list-allocated-objects :all :type sb-vm:code-header-widetag))

(defun disassemble-everything (objects
                               &optional (show-progress t)
                               &aux (dstate (make-dstate)) (i 0))
  (declare (inline %make-segment))
  (when show-progress
    (format t "~&Start ...  ")
    (force-output))
  (dolist (code objects)
    (when show-progress
      (write-char #\backspace)
      (write-char (aref "\\|/-" (mod (incf i) 4)))
      (force-output))
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
                 (setf (dstate-n-operands dstate) 0)))
             seg dstate nil))))))
  (when show-progress
    (format t " done~%")))
(compile 'disassemble-everything)

(defun install-counting-wrapper (discount)
  (declare (ignorable discount))
  #+x86-64
  (sb-int:encapsulate
   'sb-x86-64-asm::print-mem-ref 'test
   (lambda (realfun &rest args)
     ;; Each mem ref disassembled is one cons cell
     (incf (car discount) (* sb-vm:cons-size sb-vm:n-word-bytes))
     (apply realfun args))))
(compile 'install-counting-wrapper)

;;; Disassembling everything takes around .8 seconds and conses ~14MB for me.
;;; To ensure that things don't get drastically worse, assert that we're within
;;; some fuzz factor of the expected consing. One small mistake in target-disassem
;;; could leave the disassembler totally working, but consing 10x too much.
(test-util:with-test (:name :disassemble-everything
                      ;; Only the x86-64 disassembler is able to disassemble
                      ;; with output into the dstate rather than a stream.
                      ;; The others choke with "NIL is not of type STREAM"
                            :skipped-on (:not :x86-64))
  (let ((code (list-all-code)) ;; Avoid counting bytes consed in the list
        (discount (list 0)))
    (install-counting-wrapper discount)
    ;; Build the inst space outside the test to avoid influencing bytes consed
    (sb-disassem:get-inst-space)
    (multiple-value-bind (before after)
        (sb-sys:without-gcing
            (values (get-bytes-consed)
                    (progn
                      (disassemble-everything code nil)
                      (get-bytes-consed))))
      (let* ((after (- after (car discount)))
             (delta (- after before)))
        (format t "~&Consed ~D bytes discounting ~D bytes~%" delta (car discount))
        ;; Should be less than this amount of overhead
        (assert (< delta 500000))))))
