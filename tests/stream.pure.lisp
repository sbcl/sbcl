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

;;; Until sbcl-0.6.11.31, we didn't have an N-BIN method for
;;; CONCATENATED-STRING, so stuff like this would fail.
(let ((stream (make-concatenated-stream (make-string-input-stream "Demo")))
      (buffer (make-string 4)))
  (read-sequence buffer stream))
;;; test for the new N-BIN method doing what it's supposed to
(let* ((substrings (list "This " "is " "a " ""
			 "test of concatenated streams behaving "
			 "as ordinary streams do under READ-SEQUENCE. "
			 (make-string 140041 :initial-element #\%)
			 "For any size of read.."
			 (make-string 4123 :initial-element #\.)
			 "they should give the same results."
			 (make-string (expt 2 14) :initial-element #\*)
			 "There should be no differences."))
       (substreams (mapcar #'make-string-input-stream substrings))
       (concatenated-stream (apply #'make-concatenated-stream substreams))
       (concatenated-string (apply #'concatenate 'string substrings))
       (stream (make-string-input-stream concatenated-string))
       (max-n-to-read 24)
       (buffer-1 (make-string max-n-to-read))
       (buffer-2 (make-string max-n-to-read)))
  (loop
   (let* ((n-to-read (random max-n-to-read))
	  (n-actually-read-1 (read-sequence buffer-1
					    concatenated-stream
					    :end n-to-read))
	  (n-actually-read-2 (read-sequence buffer-2
					    stream
					    :end n-to-read)))
;;     (format t "buffer-1=~S~%buffer-2=~S~%" buffer-1 buffer-2)
     (assert (= n-actually-read-1 n-actually-read-2))
     (assert (string= buffer-1 buffer-2
		      :end1 n-actually-read-1
		      :end2 n-actually-read-2))
     (unless (= n-actually-read-1 n-to-read)
       (assert (< n-actually-read-1 n-to-read))
       (return)))))
