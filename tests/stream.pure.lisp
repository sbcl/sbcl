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
;;; CONCATENATED-STREAM, so stuff like this would fail.
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

;;; Entomotomy PEEK-CHAR-WRONGLY-ECHOS-TO-ECHO-STREAM bug, fixed by
;;; by MRD patch sbcl-devel 2002-11-02 merged ca. sbcl-0.7.9.32...
(assert (string=
	 (with-output-to-string (out)
	   (peek-char #\]
		      (make-echo-stream  
		       (make-string-input-stream "ab cd e df s]") out)))
	 ;; (Before the fix, the result had a trailing #\] in it.)
	 "ab cd e df s"))
;;; ...and a missing wrinkle in the original patch, dealing with
;;; PEEK-CHAR/UNREAD-CHAR on ECHO-STREAMs, fixed by MRD patch
;;; sbcl-devel 2002-11-18, merged ca. sbcl-0.7.9.66
(assert (string=
	 (let* ((in-stream (make-string-input-stream "abc"))
		(out-stream (make-string-output-stream))
		(echo-stream (make-echo-stream in-stream out-stream)))  
	   (unread-char (read-char echo-stream) echo-stream)  
	   (peek-char #\a echo-stream)
	   (get-output-stream-string out-stream))
	 ;; (Before the fix, the LET* expression just signalled an error.)
	 "a"))

;; 0.7.12 doesn't advance current stream in concatenated streams
;; correctly when searching a stream for a char to read.
(with-input-from-string (p "")
  (with-input-from-string (q "foo")
    (let* ((r (make-concatenated-stream p q)))
      (peek-char nil r))))

;; 0.7.14 and previous SBCLs don't have a working INTERACTIVE-STREAM-P
;; because it called UNIX-ISATTY, which wasn't defined.
(with-input-from-string (s "a non-interactive stream")
  (assert (not (interactive-stream-p s))))
;;; KLUDGE: Unfortunately it's hard to find a reliably interactive
;;; stream to test, since it's reasonable for these tests to be run
;;; from a script, conceivably even as something like a cron job.
;;; Ideas?
#+nil (assert (eq (interactive-stream-p *terminal-io*) t))
