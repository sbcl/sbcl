;;;; tests related to Gray streams

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

(with-test (:name (:class-precedence))
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
                    'fundamental-binary-stream)))

(defvar *fundamental-input-stream-instance*
  (make-instance 'fundamental-input-stream))

(defvar *fundamental-output-stream-instance*
  (make-instance 'fundamental-output-stream))

(defvar *fundamental-character-stream-instance*
  (make-instance 'fundamental-character-stream))

(with-test (:name (input-stream-p output-stream-p stream-element-type))
  (assert (input-stream-p *fundamental-input-stream-instance*))
  (assert (output-stream-p *fundamental-output-stream-instance*))
  (assert (eql (stream-element-type
                *fundamental-character-stream-instance*)
               'character)))

;;;; example character input and output streams

(defclass character-output-stream (fundamental-character-output-stream)
  ((lisp-stream :initarg :lisp-stream
                :accessor character-output-stream-lisp-stream)
   (position :initform 42 :accessor character-output-stream-position)))

(defclass character-input-stream (fundamental-character-input-stream)
  ((lisp-stream :initarg :lisp-stream
                :accessor character-input-stream-lisp-stream)))

;;;; example character output stream encapsulating a lisp-stream

(defun make-character-output-stream (lisp-stream)
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

(defmethod stream-file-position ((stream character-output-stream) &optional new-value)
  (if new-value
      (setf (character-output-stream-position stream) new-value)
      (character-output-stream-position stream)))

;;;; example character input stream encapsulating a lisp-stream

(defun make-character-input-stream (lisp-stream)
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

(defmethod stream-clear-input ((stream character-input-stream))
  (clear-input (character-input-stream-lisp-stream stream)))

;;;; tests for character i/o, using the above:

(with-test (:name (:character-input-stream :character-output-stream))
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
               test-string)))))

(with-test (:name (:character-output-stream))
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
    (format nil "a ~%b~%c"))))

;;; Patches introduced in sbcl-0.6.11.5 made the pretty-print logic
;;; test not only *PRINT-PRETTY* but also PRETTY-STREAM-P in some
;;; cases. Try to verify that we don't end up doing tests like that on
;;; bare Gray streams and thus bogusly omitting pretty-printing
;;; operations.
(with-test (:name (*print-pretty* sb-pretty:pretty-stream-p))
  (flet ((frob ()
           (with-output-to-string (string)
             (let ((gray-output-stream (make-character-output-stream string)))
               (format gray-output-stream
                       "~@<testing: ~@:_pretty Gray line breaks~:>~%")))))
    (assert (= 1 (count #\newline (let ((*print-pretty* nil)) (frob)))))
    (assert (= 2 (count #\newline (let ((*print-pretty* t)) (frob)))))))

;;; tests for STREAM-READ-SEQUENCE/STREAM-WRITE-SEQUENCE for
;;; subclasses of FUNDAMENTAL-CHARACTER-INPUT-/OUTPUT-STREAM (i.e.,
;;; where the default methods are available)
(with-test (:name (stream-read-sequence stream-write-sequence :default-methods))
  (let* ((test-string (format nil
                              "~% Testing for STREAM-*-SEQUENCE.~
                               ~& This is the second line.~
                               ~% This should be the third and last line.~%"))
         (test-string-len (length test-string))
         (output-test-string (make-string test-string-len)))
    ;; test for READ-/WRITE-SEQUENCE on strings/vectors
    (with-input-from-string (foo test-string)
      (assert (equal
               (with-output-to-string (bar)
                 (let ((our-char-input (make-character-input-stream foo))
                       (our-char-output (make-character-output-stream bar)))
                   (read-sequence output-test-string our-char-input)
                   (assert (typep output-test-string 'string))
                   (write-sequence output-test-string our-char-output)
                   (assert (null (peek-char nil our-char-input nil nil nil)))))
               test-string)))
    ;; test for READ-/WRITE-SEQUENCE on lists
    (let ((output-test-list (make-list test-string-len)))
      (with-input-from-string (foo test-string)
        (assert (equal
                 (with-output-to-string (bar)
                   (let ((our-char-input (make-character-input-stream foo))
                         (our-char-output (make-character-output-stream bar)))
                     (read-sequence output-test-list our-char-input)
                     (assert (typep output-test-list 'list))
                     (write-sequence output-test-list our-char-output)
                     (assert (null (peek-char nil our-char-input nil nil nil)))))
                 test-string))))))

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
  (make-instance 'binary-to-char-input-stream
                 :lisp-stream lisp-stream))

(defun make-binary-to-char-output-stream (lisp-stream)
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

(with-test (:name (fundamental-binary-input-stream
                   fundamental-binary-output-stream))
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
               test-string)))))



;;; Minimal test of file-position
(with-test (:name file-position)
  (let ((stream (make-instance 'character-output-stream)))
    (assert (= (file-position stream) 42))
    (assert (file-position stream 50))
    (assert (= (file-position stream) 50))
    (assert (file-position stream :end))))

;;; Using gray streams as parts of two-way-, concatenate-, and synonym-streams.

(defvar *gray-binary-data*
  (let ((vector (make-array 1024 :element-type '(unsigned-byte 8) :fill-pointer 0)))
    (dotimes (i (length vector))
      (setf (aref vector i) (random 256)))
    vector))

(defun vector-hop-or-eof (vector)
  (let ((pos (fill-pointer vector)))
    (if (< pos (array-total-size vector))
        (prog1
            (aref vector pos)
          (incf (fill-pointer vector)))
        :eof)))

(defclass part-of-composite-stream (fundamental-binary-input-stream)
  ())

(defmethod stream-read-byte ((stream part-of-composite-stream))
  (vector-hop-or-eof *gray-binary-data*))

(defmethod stream-element-type ((stream part-of-composite-stream))
  '(unsigned-byte 8))

(defvar *part-of-composite* (make-instance 'part-of-composite-stream))

(defun test-composite-reads (stream)
  (setf (fill-pointer *gray-binary-data*) 0)
  (let ((binary-buffer (make-array 1024 :element-type '(unsigned-byte 8))))
    (assert (eql 1024 (read-sequence binary-buffer stream)))
    (dotimes (i 1024)
      (unless (eql (aref *gray-binary-data* i)
                   (aref binary-buffer i))
        (error "wanted ~S at ~S, got ~S (~S)"
               (aref *gray-binary-data* i)
               i
               (aref binary-buffer i)
               stream)))))

(with-test (:name (fundamental-binary-input-stream
                   :in two-way-stream))
  (test-composite-reads
   (make-two-way-stream *part-of-composite* *standard-output*)))

(with-test (:name (fundamental-binary-input-stream
                   :in concatenated-stream))
  (test-composite-reads (make-concatenated-stream *part-of-composite*)))

(with-test (:name (fundamental-binary-input-stream
                   :in synonym-stream))
  (test-composite-reads (make-synonym-stream '*part-of-composite*)))

;;; Using STREAM-FILE-POSITION on an ANSI-STREAM
(with-test (:name (stream-file-position sb-kernel:ansi-stream))
  (with-output-to-string (s)
    (assert (zerop (file-position s)))
    (assert (zerop (stream-file-position s)))))

(defclass broken-char-input-stream (fundamental-input-stream) ())
(defmethod stream-read-char ((s broken-char-input-stream))
  :1potato)
(defmethod stream-read-char-no-hang ((s broken-char-input-stream))
  :1potato)
(defmethod stream-peek-char ((s broken-char-input-stream))
  :1potato)
(defclass broken-binary-input-stream (fundamental-input-stream) ())
(defmethod stream-read-byte ((s broken-binary-input-stream))
  :2potato)

(with-test (:name (read-char read-byte :check-types))
  (loop for (class fn . arg) in '((broken-char-input-stream read-char)
                                  (broken-char-input-stream read-char-no-hang)
                                  (broken-char-input-stream peek-char #\z)
                                  (broken-char-input-stream peek-char t)
                                  (broken-char-input-stream peek-char nil)
                                  (broken-binary-input-stream read-byte))
     for stream = (make-instance class)
     do (assert-error (if arg
                          (funcall fn (car arg) stream)
                          (funcall fn stream))
                      type-error)))
