;;;; -*- lisp -*-

(defpackage sb-simple-streams-test
  (:use #:common-lisp #:sb-simple-streams #:sb-rt))


(in-package #:sb-simple-streams-test)

(defparameter *dumb-string*
  "This file created by simple-stream-tests.lisp. Nothing to see here, move along.")

(defparameter *test-path*
  (merge-pathnames (make-pathname :name nil :type nil :version nil)
                   *load-truename*)
  "Directory for temporary test files.")

(eval-when (:load-toplevel) (ensure-directories-exist *test-path*))



(deftest create-file-1
  ;; Create a file-simple-stream, write data.
  (let* ((file (merge-pathnames #p"test-data.txt" *test-path*)))
    (prog1
        (with-open-stream (s (make-instance 'file-simple-stream
                                            :filename file
                                            :direction :output
                                            :if-exists :overwrite))
          (string= (write-string *dumb-string* s) *dumb-string*))
      (delete-file file)))
  t)

(deftest create-file-2
  ;; Create a file-simple-stream via :class argument to open, write data.
  (let ((file (merge-pathnames #p"test-data.txt" *test-path*)))
    (prog1
        (with-open-file (s file
                           :class 'file-simple-stream
                           :direction :output :if-exists :overwrite)
           (string= (write-string *dumb-string* s) *dumb-string*))
      (delete-file file)))
  t)

(deftest create-read-file-1
  ;; Via file-simple-stream objects, write and then re-read data.
  (let ((result t)
        (file (merge-pathnames #p"test-data.txt" *test-path*)))
    (with-open-stream (s (make-instance 'file-simple-stream
                                        :filename file
                                        :direction :output
                                        :if-exists :overwrite))
      (write-line *dumb-string* s)
      (setf result (and result (string= (write-string *dumb-string* s)
                                        *dumb-string*))))
    (with-open-stream (s (make-instance 'file-simple-stream
                                        :filename file
                                        :direction :input
                                        :if-does-not-exist :error))
      ;; Check first line
      (multiple-value-bind (string missing-newline-p)
          (read-line s)
        (setf result (and result (string= string *dumb-string*)
                          (not missing-newline-p))))
      ;; Check second line
      (multiple-value-bind (string missing-newline-p)
          (read-line s)
        (setf result (and result (string= string *dumb-string*)
                          missing-newline-p))))
    (delete-file file)
    result)
  t)

(deftest create-read-mapped-file-1
  ;; Read data via a mapped-file-simple-stream object.
  (let ((result t)
        (file (merge-pathnames #p"test-data.txt" *test-path*)))
    (with-open-file (s file
                       :class 'file-simple-stream
                       :direction :output :if-exists :overwrite)
       (setf result (and result (string= (write-string *dumb-string* s)
                                         *dumb-string*))))
    (with-open-file (s file
                       :class 'mapped-file-simple-stream
                       :direction :input)
       (setf result (and result (string= (read-line s) *dumb-string*))))
    (delete-file file)
    result)
  t)

(deftest write-read-inet
  (handler-case
      (with-open-stream (s (make-instance 'socket-simple-stream
					  :remote-host #(127 0 0 1)
					  :remote-port 7))
	(string= (prog1 (write-line "Got it!" s) (finish-output s))
		 (read-line s)))
    (sb-bsd-sockets::connection-refused-error () t))
  t)

