;;;; tests for assembler/disassembler

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

(load "test-util.lisp")
(use-package :test-util)

;; this is architecture-agnostic
(defun test-assemble (inst expect)
  (let ((segment (sb-assem:make-segment :type :regular)))
    (apply (sb-assem::inst-emitter-symbol (car inst)) segment nil (cdr inst))
    (let* ((buf (sb-assem::segment-buffer segment))
           (string
            (with-output-to-string (stream)
              (with-pinned-objects (buf)
                (let ((sb-disassem:*disassem-location-column-width* 0))
                  (sb-disassem:disassemble-memory
                   (sap-int (vector-sap buf))
                   (sb-assem::segment-current-posn segment)
                   :stream stream))))))
      (assert (string= (subseq string (1+ (position #\newline string))
                               (1- (length string))) ; chop final newline
                       expect)))))

(with-test (:name :assemble-movti-instruction :skipped-on '(not :x86-64))
  (flet ((test-movnti (dst src expect)
           (test-assemble `(movnti ,dst ,src) expect)))
    (test-movnti (make-ea :dword :base rdi-tn :disp 57) eax-tn
                 ";         0FC34739         MOVNTI [RDI+57], EAX")
    (test-movnti (make-ea :qword :base rax-tn) r12-tn
                 ";         4C0FC320         MOVNTI [RAX], R12")))
