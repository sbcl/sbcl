;;;; tests related to Lisp streams

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

(in-package :cl-user)

(load "assertoid.lisp")

;;; type errors for inappropriate stream arguments, fixed in
;;; sbcl-0.7.8.19
(locally
    (declare (optimize (safety 3)))
  (assert (raises-error? (make-two-way-stream (make-string-output-stream)
					      (make-string-output-stream))
			 type-error))
  (assert (raises-error? (make-two-way-stream (make-string-input-stream "foo")
					      (make-string-input-stream "bar"))
			 type-error))
  ;; the following two aren't actually guaranteed, because ANSI, as it
  ;; happens, doesn't say "should signal an error" for
  ;; MAKE-ECHO-STREAM. It's still good to have, but if future
  ;; maintenance work causes this test to fail because of these
  ;; MAKE-ECHO-STREAM clauses, consider simply removing these clauses
  ;; from the test. -- CSR, 2002-10-06
  (assert (raises-error? (make-echo-stream (make-string-output-stream)
					   (make-string-output-stream))
			 type-error))
  (assert (raises-error? (make-echo-stream (make-string-input-stream "foo")
					   (make-string-input-stream "bar"))
			 type-error))
  (assert (raises-error? (make-concatenated-stream
			  (make-string-output-stream)
			  (make-string-input-stream "foo"))
			 type-error)))

;;; success
(quit :unix-status 104)
