;;;; This file is for compiler tests which have side effects (e.g.
;;;; executing DEFUN) but which don't need any special side-effecting
;;;; environmental stuff (e.g. DECLAIM of particular optimization
;;;; settings). Similar tests which *do* expect special settings may
;;;; be in files compiler-1.impure.lisp, compiler-2.impure.lisp, etc.

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

(cl:in-package :cl-user)

;;;; class precedence tests

(assert (subtypep 'fundamental-stream 'stream))
(assert (subtypep 'fundamental-stream 'standard-object))

(assert (subtypep 'fundamental-input-stream 'fundamental-stream))
(assert (subtypep 'fundamental-output-stream 'fundamental-stream))
(assert (subtypep 'fundamental-character-stream 'fundamental-stream))
(assert (subtypep 'fundamental-binary-stream 'fundamental-stream))

(assert (subtypep 'fundamental-character-input-stream
                  'fundamental-input-stream))
(assert (subtypep 'fundamental-character-input-stream
                  'fundamental-character-stream))
(assert (subtypep 'fundamental-character-output-stream
                  'fundamental-output-stream))
(assert (subtypep 'fundamental-character-output-stream
                  'fundamental-character-stream))

(assert (subtypep 'fundamental-binary-input-stream
                  'fundamental-input-stream))
(assert (subtypep 'fundamental-binary-input-stream
                  'fundamental-binary-stream))
(assert (subtypep 'fundamental-binary-output-stream
                  'fundamental-output-stream))
(assert (subtypep 'fundamental-binary-output-stream
                  'fundamental-binary-stream))

(defvar *fundamental-input-stream-instance*
  (make-instance 'fundamental-input-stream))

(defvar *fundamental-output-stream-instance*
  (make-instance 'fundamental-output-stream))

(defvar *fundamental-character-stream-instance*
  (make-instance 'fundamental-character-stream))

(assert (input-stream-p *fundamental-input-stream-instance*))
(assert (output-stream-p *fundamental-output-stream-instance*))
(assert (eql (stream-element-type
              *fundamental-character-stream-instance*)
             'character))

;;;; example character input and output streams

(defclass character-output-stream (fundamental-character-output-stream)
  ((lisp-stream :initarg :lisp-stream
		:accessor character-output-stream-lisp-stream)))
  
(defclass character-input-stream (fundamental-character-input-stream)
  ((lisp-stream :initarg :lisp-stream
		:accessor character-input-stream-lisp-stream)))
  
;;;; example character output stream encapsulating a lisp-stream

(defun make-character-output-stream (lisp-stream)
  (declare (type sb-kernel:lisp-stream lisp-stream))
  (make-instance 'character-output-stream :lisp-stream lisp-stream))
  
(defmethod open-stream-p ((stream character-output-stream))
  (open-stream-p (character-output-stream-lisp-stream stream)))
  
(defmethod close ((stream character-output-stream) &key abort)
  (close (character-output-stream-lisp-stream stream) :abort abort))
  
(defmethod input-stream-p ((stream character-output-stream))
  (input-stream-p (character-output-stream-lisp-stream stream)))

(defmethod output-stream-p ((stream character-output-stream))
  (output-stream-p (character-output-stream-lisp-stream stream)))

(defmethod stream-write-char ((stream character-output-stream) character)
  (write-char character (character-output-stream-lisp-stream stream)))

(defmethod stream-line-column ((stream character-output-stream))
  (sb-kernel:charpos (character-output-stream-lisp-stream stream)))

(defmethod stream-line-length ((stream character-output-stream))
  (sb-kernel:line-length (character-output-stream-lisp-stream stream)))

(defmethod stream-finish-output ((stream character-output-stream))
  (finish-output (character-output-stream-lisp-stream stream)))

(defmethod stream-force-output ((stream character-output-stream))
  (force-output (character-output-stream-lisp-stream stream)))

(defmethod stream-clear-output ((stream character-output-stream))
  (clear-output (character-output-stream-lisp-stream stream)))

;;;; example character input stream encapsulating a lisp-stream

(defun make-character-input-stream (lisp-stream)
  (declare (type sb-kernel:lisp-stream lisp-stream))
  (make-instance 'character-input-stream :lisp-stream lisp-stream))

(defmethod open-stream-p ((stream character-input-stream))
  (open-stream-p (character-input-stream-lisp-stream stream)))

(defmethod close ((stream character-input-stream) &key abort)
  (close (character-input-stream-lisp-stream stream) :abort abort))

(defmethod input-stream-p ((stream character-input-stream))
  (input-stream-p (character-input-stream-lisp-stream stream)))

(defmethod output-stream-p ((stream character-input-stream))
  (output-stream-p (character-input-stream-lisp-stream stream)))

(defmethod stream-read-char ((stream character-input-stream))
  (read-char (character-input-stream-lisp-stream stream) nil :eof))

(defmethod stream-unread-char ((stream character-input-stream) character)
  (unread-char character (character-input-stream-lisp-stream stream)))

(defmethod stream-read-char-no-hang ((stream character-input-stream))
  (read-char-no-hang (character-input-stream-lisp-stream stream) nil :eof))

#+nil
(defmethod stream-peek-char ((stream character-input-stream))
  (peek-char nil (character-input-stream-lisp-stream stream) nil :eof))

#+nil
(defmethod stream-listen ((stream character-input-stream))
  (listen (character-input-stream-lisp-stream stream)))

(defmethod stream-clear-input ((stream character-input-stream))
  (clear-input (character-input-stream-lisp-stream stream)))

;;;; tests for character i/o, using the above:

(let ((test-string (format nil
                           "~% This is a test.~& This is the second line.~
                             ~% This should be the third and last line.~%")))
  (with-input-from-string (foo test-string)
    (assert (equal
             (with-output-to-string (bar)
               (let ((our-char-input (make-character-input-stream foo))
                     (our-char-output (make-character-output-stream bar)))
                 (assert (open-stream-p our-char-input))
                 (assert (open-stream-p our-char-output))
                 (assert (input-stream-p our-char-input))
                 (assert (output-stream-p our-char-output))
                 (let ((test-char (read-char our-char-input)))
                   (assert (char-equal test-char (char test-string 0)))
                   (unread-char test-char our-char-input))
                 (do ((line #1=(read-line our-char-input nil nil nil) #1#))
                     ((not (listen our-char-input))
                      (format our-char-output "~A~%" line))
                   (format our-char-output "~A~%" line))
                 (assert (null (peek-char nil our-char-input nil nil nil)))))
             test-string))))

(assert
  (equal
   (with-output-to-string (foo)
     (let ((our-char-output (make-character-output-stream foo)))
       (write-char #\a our-char-output)
       (finish-output our-char-output)
       (write-char #\  our-char-output)
       (force-output our-char-output)
       (fresh-line our-char-output)
       (write-char #\b our-char-output)
       (clear-output our-char-output)
       (terpri our-char-output)
       (assert (null (fresh-line our-char-output)))
       (write-char #\c our-char-output)))
   (format nil "a ~%b~%c")))

;;;; example classes for binary output

(defclass binary-to-char-output-stream (fundamental-binary-output-stream)
  ((lisp-stream :initarg :lisp-stream
		:accessor binary-to-char-output-stream-lisp-stream)))
  
(defclass binary-to-char-input-stream (fundamental-binary-input-stream)
  ((lisp-stream :initarg :lisp-stream
		:accessor binary-to-char-input-stream-lisp-stream)))

(defmethod stream-element-type ((stream binary-to-char-output-stream))
  '(unsigned-byte 8))
(defmethod stream-element-type ((stream binary-to-char-input-stream))
  '(unsigned-byte 8))

(defun make-binary-to-char-input-stream (lisp-stream)
  (declare (type sb-kernel:lisp-stream lisp-stream))
  (make-instance 'binary-to-char-input-stream
		 :lisp-stream lisp-stream))

(defun make-binary-to-char-output-stream (lisp-stream)
  (declare (type sb-kernel:lisp-stream lisp-stream))
  (make-instance 'binary-to-char-output-stream
		 :lisp-stream lisp-stream))
  
(defmethod stream-read-byte ((stream binary-to-char-input-stream))
  (let ((char (read-char
	       (binary-to-char-input-stream-lisp-stream stream) nil :eof)))
    (if (eq char :eof)
	char
	(char-code char))))

(defmethod stream-write-byte ((stream binary-to-char-output-stream) integer)
  (let ((char (code-char integer)))
    (write-char char
		(binary-to-char-output-stream-lisp-stream stream))))
      
;;;; tests using binary i/o, using the above

(let ((test-string (format nil
                           "~% This is a test.~& This is the second line.~
                             ~% This should be the third and last line.~%")))
  (with-input-from-string (foo test-string)
    (assert (equal
             (with-output-to-string (bar)
               (let ((our-bin-to-char-input (make-binary-to-char-input-stream
					     foo))
                     (our-bin-to-char-output (make-binary-to-char-output-stream
					      bar)))
                 (assert (open-stream-p our-bin-to-char-input))
                 (assert (open-stream-p our-bin-to-char-output))
                 (assert (input-stream-p our-bin-to-char-input))
                 (assert (output-stream-p our-bin-to-char-output))
                 (do ((byte #1=(read-byte our-bin-to-char-input nil :eof) #1#))
                     ((eq byte :eof))
                   (write-byte byte our-bin-to-char-output))))
             test-string))))

;;;; Voila!

(quit :unix-status 104) ; success
