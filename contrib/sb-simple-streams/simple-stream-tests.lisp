;;;; -*- lisp -*-

(defpackage sb-simple-streams-test
  (:use #:common-lisp #:sb-simple-streams #:sb-rt))


(in-package #:sb-simple-streams-test)

(defparameter *dumb-string*
  "This file was created by simple-stream-tests.lisp. Nothing to see here, move along.")

(defparameter *test-path*
  (merge-pathnames (make-pathname :name :unspecific :type :unspecific
                                  :version :unspecific)
                   *load-truename*)
  "Directory for temporary test files.")

(defparameter *test-file*
  (merge-pathnames #p"test-data.tmp" *test-path*))

(eval-when (:load-toplevel) (ensure-directories-exist *test-path* :verbose t))

;;; Non-destructive functional analog of REMF
(defun remove-key (key list)
  (loop for (current-key val . rest) on list by #'cddr
        until (eql current-key key)
        collect current-key into result
        collect val into result
        finally (return (nconc result rest))))

(defun create-test-file (&key (filename *test-file*) (content *dumb-string*))
  (with-open-file (s filename :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede)
    (write-sequence content s)))

(defun remove-test-file (&key (filename *test-file*))
  (delete-file filename))

(defmacro with-test-file ((stream file &rest open-arguments
                                  &key (delete-afterwards t)
                                  initial-content
                                  &allow-other-keys)
                          &body body)
  (setq open-arguments (remove-key :delete-afterwards open-arguments))
  (setq open-arguments (remove-key :initial-content open-arguments))
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
    (prog1
        (with-open-stream (s (make-instance 'file-simple-stream
                                            :filename *test-file*
                                            :direction :output
                                            :if-exists :overwrite
                                            :if-does-not-exist :create))
          (string= (write-string *dumb-string* s) *dumb-string*))
      (delete-file *test-file*))
  t)

(deftest create-file-2
    ;; Create a file-simple-stream via :class argument to open, write data.
    (with-test-file (s *test-file* :class 'file-simple-stream
                       :direction :output :if-exists :overwrite
                       :if-does-not-exist :create)
      (string= (write-string *dumb-string* s) *dumb-string*))
  t)

(deftest create-read-file-1
  ;; Via file-simple-stream objects, write and then re-read data.
  (let ((result t))
    (with-test-file (s *test-file* :class 'file-simple-stream
                       :direction :output :if-exists :overwrite
                       :if-does-not-exist :create :delete-afterwards nil)
      (write-line *dumb-string* s)
      (setf result (and result (string= (write-string *dumb-string* s)
                                        *dumb-string*))))

    (with-test-file (s *test-file* :class 'file-simple-stream
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

(deftest create-read-mapped-file-1
  ;; Read data via a mapped-file-simple-stream object.
  (let ((result t))
    (with-test-file (s *test-file* :class 'mapped-file-simple-stream
                       :direction :input :if-does-not-exist :error
                       :initial-content *dumb-string*)
      (setf result (and result (string= (read-line s) *dumb-string*))))
    result)
  t)

(deftest write-read-inet
  (handler-case
      (with-open-stream (s (make-instance 'socket-simple-stream
					  :remote-host #(127 0 0 1)
					  :remote-port 7
                                          :direction :io))
	(string= (prog1 (write-line "Got it!" s) (finish-output s))
		 (read-line s)))
    ;; Fail gracefully if echo isn't activated on the system
    (sb-bsd-sockets::connection-refused-error () t))
  t)

(deftest write-read-large-sc-1
  ;; Do write and read with more data than the buffer will hold
  ;; (single-channel simple-stream)
  (let* ((stream (make-instance 'file-simple-stream
                                :filename *test-file* :direction :output
                                :if-exists :overwrite
                                :if-does-not-exist :create))
         (content (make-string (1+ (device-buffer-length stream))
                               :initial-element #\x)))
    (with-open-stream (s stream)
      (write-string content s))
    (with-test-file (s *test-file* :class 'file-simple-stream
                       :direction :input :if-does-not-exist :error)
      (string= content (read-line s))))
  t)

(deftest write-read-large-sc-2
  (let* ((stream (make-instance 'file-simple-stream
                                :filename *test-file* :direction :output
                                :if-exists :overwrite
                                :if-does-not-exist :create))
         (length (1+ (* 3 (device-buffer-length stream))))
         (content (make-string length)))
    (dotimes (i (length content))
      (setf (aref content i) (code-char (random 256))))
    (with-open-stream (s stream)
      (write-string content s))
    (with-test-file (s *test-file* :class 'file-simple-stream
                       :direction :input :if-does-not-exist :error)
      (let ((seq (make-string length)))
        #+nil (read-sequence seq s)
        #-nil (dotimes (i length)
                (setf (char seq i) (read-char s)))
        (string= content seq))))
  t)

(deftest write-read-large-sc-3
  (let* ((stream (make-instance 'file-simple-stream
                                :filename *test-file* :direction :output
                                :if-exists :overwrite
                                :if-does-not-exist :create))
         (length (1+ (* 3 (device-buffer-length stream))))
         (content (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (i (length content))
      (setf (aref content i) (random 256)))
    (with-open-stream (s stream)
      (write-sequence content s))
    (with-test-file (s *test-file* :class 'file-simple-stream
                       :direction :input :if-does-not-exist :error)
      (let ((seq (make-array length :element-type '(unsigned-byte 8))))
        #+nil (read-sequence seq s)
        #-nil (dotimes (i length)
                (setf (aref seq i) (read-byte s)))
        (equalp content seq))))
  t)

(deftest write-read-large-dc-1
  ;; Do write and read with more data than the buffer will hold
  ;; (dual-channel simple-stream; we only have socket streams atm)
  (handler-case
   (let* ((stream (make-instance 'socket-simple-stream
                                 :remote-host #(127 0 0 1)
                                 :remote-port 7
                                 :direction :io))
          (content (make-string (1+ (device-buffer-length stream))
                                :initial-element #\x)))
     (with-open-stream (s stream)
       (string= (prog1 (write-line content s) (finish-output s))
                (read-line s))))
    ;; Fail gracefully if echo isn't activated on the system
   (sb-bsd-sockets::connection-refused-error () t))
  t)


(deftest file-position-1
    ;; Test reading of file-position
    (with-test-file (s *test-file* :class 'file-simple-stream :direction :input
                       :initial-content *dumb-string*)
      (file-position s))
  0)

(deftest file-position-2
    ;; Test reading of file-position
    (with-test-file (s *test-file* :class 'file-simple-stream :direction :input
                       :initial-content *dumb-string*)
      (read-byte s)
      (file-position s))
  1)

(deftest file-position-3
    ;; Test reading of file-position in the presence of unsaved data
    (with-test-file (s *test-file* :class 'file-simple-stream
                       :direction :output :if-exists :supersede
                       :if-does-not-exist :create)
      (write-byte 50 s)
      (file-position s))
  1)

(deftest file-position-4
    ;; Test reading of file-position in the presence of unsaved data and
    ;; filled buffer
    (with-test-file (s *test-file* :class 'file-simple-stream :direction :io
                       :if-exists :overwrite :if-does-not-exist :create
                       :initial-content *dumb-string*)
      (read-byte s)                     ; fill buffer
      (write-byte 50 s)                 ; advance file-position
      (file-position s))
  2)

(deftest file-position-5
    ;; Test file position when opening with :if-exists :append
    (with-test-file (s *test-file* :class 'file-simple-stream :direction :io
                       :if-exists :append :if-does-not-exist :create
                       :initial-content *dumb-string*)
      (= (file-length s) (file-position s)))
  T)

(deftest write-read-unflushed-sc-1
    ;; Write something into a single-channel stream and read it back
    ;; without explicitly flushing the buffer in-between
    (with-test-file (s *test-file* :class 'file-simple-stream :direction :io
                       :if-does-not-exist :create :if-exists :supersede)
      (write-char #\x s)
      (file-position s :start)
      (read-char s))
  #\x)

(deftest write-read-unflushed-sc-2
    ;; Write something into a single-channel stream, try to read back too much
    (handler-case
        (with-test-file (s *test-file* :class 'file-simple-stream
                           :direction :io :if-does-not-exist :create
                           :if-exists :supersede)
            (write-char #\x s)
            (file-position s :start)
            (read-char s)
            (read-char s)
            nil)
      (end-of-file () t))
  t)

(deftest write-read-unflushed-sc-3
    ;; Test writing in a buffer filled with previous file contents
    (let ((result t))
      (with-test-file (s *test-file* :class 'file-simple-stream :direction :io
                         :if-exists :overwrite :if-does-not-exist :create
                         :initial-content *dumb-string*)
        (setq result (and result (char= (read-char s) (schar *dumb-string* 0))))
        (setq result (and result (= (file-position s) 1)))
        (let ((pos (file-position s)))
          (write-char #\x s)
          (file-position s pos)
          (setq result (and result (char= (read-char s) #\x)))))
      result)
  t)

(deftest write-read-unflushed-sc-4
    ;; Test flushing of buffers
    (progn
      (with-test-file (s *test-file* :class 'file-simple-stream :direction :io
                         :if-exists :overwrite :if-does-not-exist :create
                         :initial-content "Foo"
                         :delete-afterwards nil)
        (read-char s)                   ; Fill the buffer.
        (file-position s :start)        ; Change existing data.
        (write-char #\X s)
        (file-position s :end)          ; Extend file.
        (write-char #\X s))
      (with-test-file (s *test-file* :class 'file-simple-stream
                         :direction :input :if-does-not-exist :error)
        (read-line s)))
  "XooX"
  T)

(deftest write-read-append-sc-1
    ;; Test writing in the middle of a stream opened in append mode
    (progn
      (with-test-file (s *test-file* :class 'file-simple-stream :direction :io
                         :if-exists :append :if-does-not-exist :create
                         :initial-content "Foo"
                         :delete-afterwards nil)
        (file-position s :start)        ; Jump to beginning.
        (write-char #\X s)
        (file-position s :end)          ; Extend file.
        (write-char #\X s))
      (with-test-file (s *test-file* :class 'file-simple-stream
                         :direction :input :if-does-not-exist :error)
        (read-line s)))
  "XooX"
  T)

(deftest write-read-mixed-sc-1
    ;; Test read/write-sequence of types string and (unsigned-byte 8)
    (let ((uvector (make-array '(10) :element-type '(unsigned-byte 8)
                               :initial-element 64))
          (svector (make-array '(10) :element-type '(signed-byte 8)
                               :initial-element -1))
          (result-uvector (make-array '(10) :element-type '(unsigned-byte 8)
                              :initial-element 0))
          (result-svector (make-array '(10) :element-type '(signed-byte 8)
                              :initial-element 0))
          (result-string (make-string (length *dumb-string*)
                                      :initial-element #\Space)))
      (with-test-file (s *test-file* :class 'file-simple-stream :direction :io
                         :if-exists :overwrite :if-does-not-exist :create
                         :delete-afterwards nil)
        (write-sequence svector s)
        (write-sequence uvector s)
        (write-sequence *dumb-string* s))
      (with-test-file (s *test-file* :class 'file-simple-stream
                         :direction :input :if-does-not-exist :error
                         :delete-afterwards nil)
        (read-sequence result-svector s)
        (read-sequence result-uvector s)
        (read-sequence result-string s))
      (and (string= *dumb-string* result-string)
           (equalp uvector result-uvector)
           (equalp svector result-svector)))
  T)
