;;;; -*- lisp -*-

(defpackage sb-simple-streams-test
  (:use #:common-lisp #:sb-simple-streams #:sb-rt))


(in-package #:sb-simple-streams-test)

(defparameter *dumb-string*
  "This file was created by simple-stream-tests.lisp. Nothing to see here, move along.")

(defparameter *test-path*
  (merge-pathnames (make-pathname :name nil :type nil :version nil)
                   *load-truename*)
  "Directory for temporary test files.")

(eval-when (:load-toplevel) (ensure-directories-exist *test-path*))

(defmacro with-test-file ((stream file &rest open-arguments
                                  &key (delete-afterwards t)
                                  initial-content
                                  &allow-other-keys)
                          &body body)
  (remf open-arguments :delete-afterwards)
  (remf open-arguments :initial-content)
  (if initial-content
      (let ((create-file-stream (gensym)))
        `(progn
           (with-open-file (,create-file-stream ,file :direction :output
                                                :if-exists :supersede
                                                :if-does-not-exist :create)
             (write-sequence ,initial-content ,create-file-stream))
           (unwind-protect
                (with-open-file (,stream ,file ,@open-arguments)
                  (progn ,@body))
             ,(when delete-afterwards `(ignore-errors (delete-file ,file))))))
      `(unwind-protect
            (with-open-file (,stream ,file ,@open-arguments)
              (progn ,@body))
         ,(when delete-afterwards `(ignore-errors (delete-file ,file))))))


(deftest create-file-1
  ;; Create a file-simple-stream, write data.
  (let* ((file (merge-pathnames #p"test-data.txt" *test-path*)))
    (prog1
        (with-open-stream (s (make-instance 'file-simple-stream
                                            :filename file
                                            :direction :output
                                            :if-exists :overwrite
                                            :if-does-not-exist :create))
          (string= (write-string *dumb-string* s) *dumb-string*))
      (delete-file file)))
  t)

(deftest create-file-2
  ;; Create a file-simple-stream via :class argument to open, write data.
  (let ((file (merge-pathnames #p"test-data.txt" *test-path*)))
    (with-test-file (s file :class 'file-simple-stream :direction :output
                       :if-exists :overwrite :if-does-not-exist :create)
      (string= (write-string *dumb-string* s) *dumb-string*)))
  t)

(deftest create-read-file-1
  ;; Via file-simple-stream objects, write and then re-read data.
  (let ((result t)
        (file (merge-pathnames #p"test-data.txt" *test-path*)))
    (with-test-file (s file :class 'file-simple-stream :direction :output
                       :if-exists :overwrite :if-does-not-exist :create
                       :delete-afterwards nil)
      (write-line *dumb-string* s)
      (setf result (and result (string= (write-string *dumb-string* s)
                                        *dumb-string*))))

    (with-test-file (s file :class 'file-simple-stream
                       :direction :input :if-does-not-exist :error)
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
    result)
  t)

;;; FIXME
#-darwin
(deftest create-read-mapped-file-1
  ;; Read data via a mapped-file-simple-stream object.
  (let ((result t)
        (file (merge-pathnames #p"test-data.txt" *test-path*)))
    (with-test-file (s file :class 'mapped-file-simple-stream
                       :direction :input :if-does-not-exist :error
                       :initial-content *dumb-string*)
      (setf result (and result (string= (read-line s) *dumb-string*))))
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

(deftest write-read-large-sc-1
  ;; Do write and read with more data than the buffer will hold
  ;; (single-channel simple-stream)
  (let* ((file (merge-pathnames #p"test-data.txt" *test-path*))
         (stream (make-instance 'file-simple-stream
                                :filename file :direction :output
                                :if-exists :overwrite
                                :if-does-not-exist :create))
         (content (make-string (1+ (device-buffer-length stream))
                               :initial-element #\x)))
    (with-open-stream (s stream)
      (write-string content s))
    (with-test-file (s file :class 'file-simple-stream
                       :direction :input :if-does-not-exist :error)
      (string= content (read-line s))))
  t)

(deftest write-read-large-dc-1
  ;; Do write and read with more data than the buffer will hold
  ;; (dual-channel simple-stream; we only have socket streams atm)
  (handler-case
   (let* ((stream (make-instance 'socket-simple-stream
                                 :remote-host #(127 0 0 1)
                                 :remote-port 7))
          (content (make-string (1+ (device-buffer-length stream))
                                :initial-element #\x)))
     (with-open-stream (s stream)
       (string= (prog1 (write-line content s) (finish-output s))
                (read-line s))))
   (sb-bsd-sockets::connection-refused-error () t))
  t)


(deftest file-position-1
  ;; Test reading of file-position
  (let* ((file (merge-pathnames #p"test-data.txt" *test-path*)))
    (with-test-file (s file :class 'file-simple-stream :direction :input
                       :initial-content *dumb-string*)
      (file-position s)))
  0)

;;; file-position-2 fails ONLY when called with
;;; (asdf:oos 'asdf:test-op :sb-simple-streams)
;;; TODO: Find out why
#+nil
(deftest file-position-2
  ;; Test reading of file-position
  (let* ((file (merge-pathnames #p"test-data.txt" *test-path*)))
    (with-test-file (s file :class 'file-simple-stream :direction :input
                       :initial-content *dumb-string*)
      (read-byte s)
      (file-position s)))
  1)

(deftest file-position-3
  ;; Test reading of file-position in the presence of unsaved data
  (let* ((file (merge-pathnames #p"test-data.txt" *test-path*)))
    (with-test-file (s file :class 'file-simple-stream :direction :output
                       :if-exists :supersede :if-does-not-exist :create)
      (write-byte 50 s)
      (file-position s)))
  1)

(deftest file-position-4
    ;; Test file position when opening with :if-exists :append
    (let ((file (merge-pathnames #p"test-data.txt" *test-path*)))
      (with-test-file (s file :class 'file-simple-stream :direction :io
                         :if-exists :append :if-does-not-exist :create
                         :initial-content "Foo")
        (= (file-length s) (file-position s))))
  T)

(deftest write-read-unflushed-sc-1
  ;; Write something into a single-channel stream and read it back
  ;; without explicitly flushing the buffer in-between
  (let* ((file (merge-pathnames #p"test-data.txt" *test-path*)))
    (with-test-file (s file :class 'file-simple-stream :direction :io
                       :if-does-not-exist :create :if-exists :supersede)
      (write-char #\x s)
      (file-position s :start)
      (read-char s)))
  #\x)

(deftest write-read-unflushed-sc-2
  ;; Write something into a single-channel stream, try to read back too much
  (handler-case
   (let* ((file (merge-pathnames #p"test-data.txt" *test-path*)))
     (with-test-file (s file :class 'file-simple-stream :direction :io
                        :if-does-not-exist :create :if-exists :supersede)
       (write-char #\x s)
       (file-position s :start)
       (read-char s)
       (read-char s))
     nil)
    (end-of-file () t))
  t)

(deftest write-read-unflushed-sc-3
    (let ((file (merge-pathnames #p"test-data.txt" *test-path*))
          (result t))
      (with-test-file (s file :class 'file-simple-stream :direction :io
                         :if-exists :overwrite :if-does-not-exist :create
                         :initial-content *dumb-string*)
        (setq result (and result (char= (read-char s) (char *dumb-string* 0))))
        (setq result (and result (= (file-position s) 1)))
        (let ((pos (file-position s)))
          (write-char #\x s)
          (file-position s pos)
          (setq result (and result (char= (read-char s) #\x)))))
      result)
  t)

(deftest write-read-unflushed-sc-4
    ;; Test flushing of buffers
    (let ((file (merge-pathnames #p"test-data.txt" *test-path*)))
      (with-test-file (s file :class 'file-simple-stream :direction :io
                         :if-exists :overwrite :if-does-not-exist :create
                         :initial-content "Foo"
                         :delete-afterwards nil)
        (read-char s)                   ; Fill the buffer.
        (file-position s :start)        ; Change existing data.
        (write-char #\X s)
        (file-position s :end)          ; Extend file.
        (write-char #\X s))
      (with-test-file (s file :class 'file-simple-stream :direction :input
                         :if-does-not-exist :error)
        (read-line s)))
  "XooX"
  T)

(deftest write-read-append-sc-1
    ;; Test writing in the middle of a stream opened in append mode
    (let ((file (merge-pathnames #p"test-data.txt" *test-path*)))
      (with-test-file (s file :class 'file-simple-stream :direction :io
                         :if-exists :append :if-does-not-exist :create
                         :initial-content "Foo"
                         :delete-afterwards nil)
        (file-position s :start)        ; Jump to beginning.
        (write-char #\X s)
        (file-position s :end)          ; Extend file.
        (write-char #\X s))
      (with-test-file (s file :class 'file-simple-stream :direction :input
                         :if-does-not-exist :error)
        (read-line s)))
  "XooX"
  T)




