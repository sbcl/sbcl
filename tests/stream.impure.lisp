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

(load "assertoid.lisp")
(use-package "ASSERTOID")

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

;;; bug 225: STRING-STREAM was not a class
(eval `(defgeneric bug225 (s)
         ,@(mapcar (lambda (class)
                     `(:method :around ((s ,class)) (cons ',class (call-next-method))))
                   '(stream string-stream sb-impl::string-input-stream
                     sb-impl::string-output-stream))
         (:method (class) nil)))

(assert (equal (bug225 (make-string-input-stream "hello"))
               '(sb-impl::string-input-stream string-stream stream)))
(assert (equal (bug225 (make-string-output-stream))
               '(sb-impl::string-output-stream string-stream stream)))


;;; improper buffering on (SIGNED-BYTE 8) streams (fixed by David Lichteblau):
(let ((p "signed-byte-8-test.data"))
  (with-open-file (s p
		     :direction :output
		     :element-type '(unsigned-byte 8)
		     :if-exists :supersede)
    (write-byte 255 s))
  (with-open-file (s p :element-type '(signed-byte 8))
    (assert (= (read-byte s) -1)))
  (delete-file p))

;;; :IF-EXISTS got :ERROR and NIL the wrong way round (reported by
;;; Milan Zamazal)
(let* ((p "this-file-will-exist")
       (stream (open p :direction :output :if-exists :error)))
  (assert (null (with-open-file (s p :direction :output :if-exists nil) s)))
  (assert (raises-error?
	   (with-open-file (s p :direction :output :if-exists :error))))
  (close stream)
  (delete-file p))

(assert (raises-error? (read-byte (make-string-input-stream "abc"))
		       type-error))
(assert (raises-error? (with-open-file (s "/dev/zero")
			 (read-byte s))
		       type-error))
;;; bidirectional streams getting confused about their position
(let ((p "bidirectional-stream-test"))
  (with-open-file (s p :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (format s "~S ~S ~S~%" 'these 'are 'symbols)))
  (with-open-file (s p :direction :io :if-exists :overwrite)
    (read s)
    (with-standard-io-syntax
      (prin1 'insert s)))
  (with-open-file (s p)
    (assert (string= (read-line s) "THESE INSERTMBOLS")))
  (delete-file p))

;;; :DIRECTION :IO didn't work on non-existent pathnames
(let ((p "direction-io-test"))
  (ignore-errors (delete-file p))
  (with-open-file (s p :direction :io)
    (format s "1")
    (finish-output s)
    (file-position s :start)
    (assert (char= (read-char s) #\1)))
  (delete-file p))

;;; FILE-POSITION on broadcast-streams is mostly uncontroversial
(assert (= 0 (file-position (make-broadcast-stream))))
(assert (file-position (make-broadcast-stream) :start))
(assert (file-position (make-broadcast-stream) 0))
(assert (not (file-position (make-broadcast-stream) 1)))
(let ((s (make-broadcast-stream)))
  (write-char #\a s)
  (assert (not (file-position s 1)))
  (assert (= 0 (file-position s))))

(let ((p "broadcast-stream-test"))
  (ignore-errors (delete-file p))
  (with-open-file (f p :direction :output)
    (let ((s (make-broadcast-stream f)))
      (assert (= 0 (file-position s)))
      (assert (file-position s :start))
      (assert (file-position s 0))
      (write-char #\a s)
      (assert (= 1 (file-position s))) ; unicode...
      (assert (file-position s 0))))
  (delete-file p))

;;; CLOSING a non-new streams should not delete them, and superseded
;;; files should be restored.
(let ((test "test-file-for-close-should-not-delete"))
  (macrolet ((test-mode (mode)
	       `(progn
		 (catch :close-test-exit
		   (with-open-file (f test :direction :output :if-exists ,mode)
		     (write-line "test" f)
		     (throw :close-test-exit t)))
		 (assert (and (probe-file test) ,mode)))))
    (unwind-protect
	 (progn
	   (with-open-file (f test :direction :output)
	     (write-line "test" f))
	   (test-mode :append)
	   ;; FIXME: We really should recover supersede files as well, according to
	   ;; CLOSE in CLHS, but at the moment we don't.
	   ;; (test-mode :supersede)
	   (test-mode :rename)
	   (test-mode :rename-and-delete))
      (when (probe-file test)
	(delete-file test)))))

;;; success
(quit :unix-status 104)
