
;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(in-package sb-vm)

(test-util:with-test (:name :unparse-alien-niladic-function)
  (let* ((type (parse-alien-type '(function long) nil))
         (val (make-alien-value :sap (int-sap #x4000) :type type)))
    (assert (not (search "#'" (write-to-string type :pretty t))))
    (assert (not (search "#'" (write-to-string val :pretty t))))))

;; This test shows (well, sorta) that call_into_lisp didn't read beyond
;; the Nth item in its argument vector with N being the specified argc.
;; As it happens, we zeroize the unused passing registers, so can check for that.
(defun monkeybiz (a1 a2 a3)
  ;; grr. what if a safety policy restriction is in effect?
  (declare (optimize (safety 0)))
  (declare (special monkeybiz-result))
  (setq monkeybiz-result (list a1 a2 a3)))
(compile 'monkeybiz) ; in case somebody runs this test with the interpreter

(defun try-call-into-lisp (c-prog) ; er, assembly program, but whatever
  (flet ((assemble-it (n)
           (let ((segment (sb-assem:make-segment :type :regular)))
             (dolist (instruction (subst n :ARGC c-prog)
                      (sb-assem::segment-buffer segment))
               (apply (sb-assem::inst-emitter-symbol (car instruction))
                      segment nil (cdr instruction))))))
    (dotimes (n-args 4)
      (let ((the-code (assemble-it n-args)))
        ;; in case we change the way the assembler output works ...
        (assert (typep the-code '(simple-array (unsigned-byte 8) 1)))
        (with-pinned-objects (the-code)
          (let ((my-little-alien
                 (make-alien-value :type (parse-alien-type '(function long) nil)
                                   :sap (vector-sap the-code)))
                (expect  (concatenate 'list (subseq '(#\A 311 T) 0 n-args)
                                      (subseq '(0 0 0) n-args 3)))
                (monkeybiz-result))
            (declare (special monkeybiz-result))
            (alien-funcall my-little-alien)
            (format t "Call with ~D arg~:P: ~S~%" n-args monkeybiz-result)
            (assert (equal monkeybiz-result expect))))))))

#+X86-64
(test-util:with-test (:name :call-into-lisp)
  ;; Obviously we need a C function to call the Lisp function, so here's one,
  ;; carefully hand-crafted so as to need no input arguments,
  ;; using only a static Lisp symbol, two non-pointers, and a pinned function.
  (with-pinned-objects (#'monkeybiz)
    (try-call-into-lisp
     ;; Making room for 3 args aligns the stack to a 16-byte boundary
     ;; presuming it was at CALL to me. Darwin requires the alignment, others don't care.
     `((sub ,rsp-tn 24)
       (mov ,(make-ea :qword :base rsp-tn :disp 16) ,(get-lisp-obj-address T))
       (mov ,(make-ea :qword :base rsp-tn :disp 8) ,(fixnumize 311))
       (mov ,(make-ea :qword :base rsp-tn :disp 0) ,(get-lisp-obj-address #\A))
       (mov ,rdi-tn ,(get-lisp-obj-address #'monkeybiz)) ; C arg 0 = Lisp function
       (mov ,rsi-tn ,rsp-tn)                             ; C arg 1 = argv
       (mov ,rdx-tn :ARGC)                               ; C arg 2 = argc
       (mov ,rax-tn ,(sap-int
                      (alien-value-sap
                       (extern-alien "call_into_lisp"
                                     (function long long long long)))))
       (call ,rax-tn)
       (add ,rsp-tn 24)
       (ret)))))
