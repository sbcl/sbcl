;;;; -*- lisp -*-

(defpackage sb-simple-streams-test
  (:use #:common-lisp #:sb-simple-streams #:sb-rt))


(in-package #:sb-simple-streams-test)

(defparameter *dumb-string* "This file created by simple-stream-tests.lisp. Nothing to see here, move along.")

(defparameter *test-path* (merge-pathnames
                           (make-pathname :name nil :type nil :version nil)
                           *load-truename*))

(eval-when (:load-toplevel) (ensure-directories-exist *test-path*))

(deftest create-file-1
  (let* ((file (merge-pathnames #p"test-data.txt" *test-path*))
         (stream-object (make-instance 'file-simple-stream
                                      :filename file
                                      :direction :output
                                      :if-exists :overwrite)))
    (prog1
        (with-open-stream (s stream-object)
           (string= (write-string *dumb-string* s) *dumb-string*))
      (delete-file file)))
  t)

(deftest create-file-2
  (let ((file (merge-pathnames #p"test-data.txt" *test-path*)))
    (prog1
        (with-open-file (s file
                           :class 'file-simple-stream
                           :direction :output :if-exists :overwrite)
           (string= (write-string *dumb-string* s) *dumb-string*))
      (delete-file file)))
  t)

(deftest create-read-file-1
  (let ((result t)
        (file (merge-pathnames #p"test-data.txt" *test-path*)))
    (let ((stream-object (make-instance 'file-simple-stream
                                      :filename file
                                      :direction :output
                                      :if-exists :overwrite)))
      (with-open-stream (s stream-object)
         (setf result (and result (string= (write-string *dumb-string* s)
                                           *dumb-string*)))
         (terpri s)))
    (let ((stream-object (make-instance 'file-simple-stream
                                      :filename file
                                      :direction :input)))
      (with-open-stream (s stream-object)
         (setf result (and result (string= (read-line s) *dumb-string*)))))
    result)
  t)

(deftest create-read-mapped-file-1
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



