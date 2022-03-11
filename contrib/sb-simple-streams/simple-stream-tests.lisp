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

#+nil ; fails
(deftest non-existent-class
  (handler-case
      (with-test-file (s *test-file* :class 'non-existent-stream)
        nil)
    ;; find-class will raise a simple-error
    (simple-error (c) (search "There is no class" (simple-condition-format-control c))))
  0)

(deftest non-stream-class
  (handler-case
      (with-test-file (s *test-file* :class 'standard-class)
        nil)
    ;; Will fall through sb-simple-streams:open as it is no stream class.
    (simple-error (c) (search "Don't know how to handle" (simple-condition-format-control c))))
  0)

(deftest create-file-1
    ;; Create a file-simple-stream, write data.
    (prog1
        (with-open-stream (s (make-instance 'file-simple-stream
                                            :filename *test-file*
                                            :direction :output
                                            :if-exists :overwrite
                                            :if-does-not-exist :create))
          (fresh-line s)
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
    (sb-bsd-sockets::connection-refused-error () t)
    ;; Timeout may occur on the restricted systems (e.g. FreeBSD
    ;; with jail(8) or blackhole(4) is used).
    (sb-bsd-sockets::operation-timeout-error () t))
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
   (sb-bsd-sockets::connection-refused-error () t)
   ;; Timeout may occur on the restricted systems (e.g. FreeBSD
   ;; with jail(8) or blackhole(4) is used).
   (sb-bsd-sockets::operation-timeout-error () t))
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

(defparameter *multi-line-string*
  "This file was created by simple-stream-tests.lisp.
Nothing to see here, move along.")

(defmacro with-dc-test-stream ((s &key initial-content) &body body)
  `(with-test-file
       (.ansi-stream.
        *test-file*
        :direction :io
        :if-exists :overwrite
        :initial-content ,(or initial-content '*multi-line-string*))
     (let ((,s (make-instance 'terminal-simple-stream
                 :input-handle (sb-kernel::fd-stream-fd .ansi-stream.)
                 :output-handle (sb-kernel::fd-stream-fd .ansi-stream.))))
       ,@body)))

(defmacro with-sc-test-stream ((s &key initial-content) &body body)
  `(with-test-file
       (,s
        *test-file*
        :class 'file-simple-stream
        :direction :io
        :if-exists :overwrite
        :initial-content ,(or initial-content '*multi-line-string*))
     ,@body))

(deftest listen-dc-1
    ;; LISTEN with filled buffer
    (with-dc-test-stream (s) (read-char s) (listen s))
  T)

(deftest listen-dc-2
    ;; LISTEN with empty buffer
    (with-dc-test-stream (s) (listen s))
  T)

(deftest listen-dc-3
    ;; LISTEN at EOF
    (with-dc-test-stream (s)
      (read-line s)
      (read-line s)
      (listen s))
  NIL)

;;; the following tests are for problems fixed in SBCL 0.8.6.2:

(deftest charpos-1
    ;; check for bug involving the -1 vs. 0 oddity in std-dc-newline-in-handler
    ;;
    ;; Note: It not not clear to me that input should affect the CHARPOS at
    ;; all.  (Except for a terminal stream perhaps, which our test stream
    ;; happens to be.  Hmm.)
    ;;
    ;; But CHARPOS must not be -1, so much is sure, hence this test is right
    ;; in any case.
    (with-dc-test-stream (s)
      (read-line s)
      (sb-simple-streams:charpos s))
  0)

(deftest charpos-2
    ;; FIXME: It not not clear to me that input should affect the CHARPOS at
    ;; all, and indeed it does not.  That is, except for newlines?! (see above)
    ;;
    ;; What this test does is (a) check that the CHARPOS works at all without
    ;; erroring and (b) force anyone changing the CHARPOS behaviour to read
    ;; this comment and start thinking things through better than I can.
    (with-dc-test-stream (s)
      (read-char s)
      (and (eql (sb-kernel:charpos s) 0)
           (eql (sb-simple-streams:charpos s) 0)))
  T)

(deftest reader-1
    ;; does the reader support simple streams?  Note that, say, "123" instead
    ;; of "(1 2)" does not trigger the bugs present in SBCL 0.8.6.
    (with-dc-test-stream (s :initial-content "(1 2)")
      (equal (read s) '(1 2)))
  T)

(deftest line-length-dc-1
    ;; does LINE-LENGTH support simple streams?
    (with-dc-test-stream (s)
      (eql (sb-simple-streams:line-length s)
           (sb-kernel:line-length s)))
  T)

(defvar *synonym*)

;; the biggest change in 0.8.6.2:
;; support composite streams writing to simple streams

;; first, SYNONYM-STREAM:

(deftest synonym-stream-1
    ;; READ-CHAR
    (with-dc-test-stream (*synonym*)
      (read-char (make-synonym-stream '*synonym*)))
  #\T)

(deftest synonym-stream-2
    ;; UNREAD-CHAR (via STREAM-MISC-DISPATCH)
    (with-dc-test-stream (*synonym*)
      (let ((s (make-synonym-stream '*synonym*)))
        (unread-char (read-char s) s)
        (read-char s)))
  #\T)

(deftest synonym-stream-3
    ;; READ-BYTE
    (with-dc-test-stream (*synonym*)
      (read-byte (make-synonym-stream '*synonym*)))
  #.(char-code #\T))

(deftest synonym-stream-4
    ;; WRITE-CHAR
    (with-sc-test-stream (*synonym*)
      (let ((s (make-synonym-stream '*synonym*)))
        (write-char #\A s)
        (file-position s 0)
        (read-char s)))
  #\A)

(deftest synonym-stream-5
    ;; WRITE-BYTE
    (with-sc-test-stream (*synonym*)
      (let ((s (make-synonym-stream '*synonym*)))
        (write-byte 65 s)
        (file-position s 0)
        (read-char s)))
  #\A)

#+nil ; fails
(deftest synonym-stream-6
    ;; WRITE-STRING
    (with-sc-test-stream (*synonym*)
      (let ((s (make-synonym-stream '*synonym*)))
        (write-string "ab" s)
        (file-position s 0)
        (and (char= (read-char s) #\a)
             (char= (read-char s) #\b))))
  T)

(deftest synonym-stream-7
    ;; LISTEN (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (*synonym*)
      (let ((s (make-synonym-stream '*synonym*)))
        (and (listen s) t)))
  T)

(deftest synonym-stream-8
    ;; CLEAR-INPUT (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (*synonym*)
      (let ((s (make-synonym-stream '*synonym*)))
        (clear-input s)))
  NIL)

(deftest synonym-stream-9
    ;; FORCE-OUTPUT (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (*synonym*)
      ;; could test more here
      (force-output (make-synonym-stream '*synonym*)))
  NIL)

(deftest synonym-stream-10
    ;; FINISH-OUTPUT (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (*synonym*)
      ;; could test more here
      (finish-output (make-synonym-stream '*synonym*)))
  NIL)

(deftest synonym-stream-11
    ;; STREAM-ELEMENT-TYPE (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (*synonym*)
      (equal (stream-element-type (make-synonym-stream '*synonym*))
             (stream-element-type *synonym*)))
  T)

(deftest synonym-stream-12
    ;; INTERACTIVE-STREAM-P (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (*synonym*)
      (eql (interactive-stream-p (make-synonym-stream '*synonym*))
           (interactive-stream-p *synonym*)))
  T)

(deftest synonym-stream-13
    ;; LINE-LENGTH (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (*synonym*)
      (eql (sb-kernel:line-length (make-synonym-stream '*synonym*))
           (sb-kernel:line-length *synonym*)))
  T)

(deftest synonym-stream-14
    ;; CHARPOS (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (*synonym*)
      (eql (sb-kernel:charpos (make-synonym-stream '*synonym*))
           (sb-kernel:charpos *synonym*)))
  T)

(deftest synonym-stream-15
    ;; FILE-LENGTH (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (*synonym*)
      (eql (file-length (make-synonym-stream '*synonym*))
           (file-length *synonym*)))
  T)

(deftest synonym-stream-16
    ;; FILE-POSITION (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (*synonym*)
      (eql (file-position (make-synonym-stream '*synonym*))
           (file-position *synonym*)))
  T)

;; SYNONYM-STREAM tests repeated for BROADCAST-STREAM, where applicable

(deftest broadcast-stream-4
    ;; WRITE-CHAR
    (with-sc-test-stream (synonym)
      (let ((s (make-broadcast-stream synonym)))
        (write-char #\A s)
        (force-output s))
      (file-position synonym 0)
      (read-char synonym))
  #\A)

(deftest broadcast-stream-5
    ;; WRITE-BYTE
    (with-sc-test-stream (synonym)
      (let ((s (make-broadcast-stream synonym)))
        (write-byte 65 s)
        (force-output s))
      (file-position synonym 0)
      (read-char synonym))
  #\A)

#+nil ; fails
(deftest broadcast-stream-6
    ;; WRITE-STRING
    (with-sc-test-stream (synonym)
      (let ((s (make-broadcast-stream synonym)))
        (write-string "ab" s)
        (force-output s))
      (file-position synonym 0)
      (and (char= (read-char synonym) #\a)
           (char= (read-char synonym) #\b)))
  T)

(deftest broadcast-stream-9
    ;; FORCE-OUTPUT (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (synonym)
      ;; could test more here
      (force-output (make-broadcast-stream synonym)))
  NIL)

(deftest broadcast-stream-10
    ;; FINISH-OUTPUT (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (synonym)
      ;; could test more here
      (finish-output (make-broadcast-stream synonym)))
  NIL)

(deftest broadcast-stream-11
    ;; STREAM-ELEMENT-TYPE (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (synonym)
      (eql (stream-element-type (make-broadcast-stream synonym))
           (stream-element-type synonym)))
  T)

(deftest broadcast-stream-12
    ;; INTERACTIVE-STREAM-P (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (synonym)
      (eql (interactive-stream-p (make-broadcast-stream synonym))
           (interactive-stream-p synonym)))
  T)

(deftest broadcast-stream-13
    ;; LINE-LENGTH (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (synonym)
      (eql (sb-kernel:line-length (make-broadcast-stream synonym))
           (sb-kernel:line-length synonym)))
  T)

(deftest broadcast-stream-14
    ;; CHARPOS (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (synonym)
      (eql (sb-kernel:charpos (make-broadcast-stream synonym))
           (sb-kernel:charpos synonym)))
  T)

(deftest broadcast-stream-16
    ;; FILE-POSITION (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (synonym)
      (eql (file-position (make-broadcast-stream synonym))
           (file-position synonym)))
  T)

;; SYNONYM-STREAM tests repeated for TWO-WAY-STREAM, where applicable

(deftest two-way-stream-1
    ;; READ-CHAR
    (with-dc-test-stream (synonym)
      (read-char (make-two-way-stream synonym synonym)))
  #\T)

(deftest two-way-stream-2
    ;; UNREAD-CHAR (via STREAM-MISC-DISPATCH)
    (with-dc-test-stream (synonym)
      (let ((s (make-two-way-stream synonym synonym)))
        (unread-char (read-char s) s)
        (read-char s)))
  #\T)

(deftest two-way-stream-3
    ;; READ-BYTE
    (with-dc-test-stream (synonym)
      (read-byte (make-two-way-stream synonym synonym)))
  #.(char-code #\T))

(deftest two-way-stream-4
    ;; WRITE-CHAR
    (with-sc-test-stream (synonym)
      (let ((s (make-two-way-stream synonym synonym)))
        (write-char #\A s)
        (force-output s))
      (file-position synonym 0)
      (read-char synonym))
  #\A)

(deftest two-way-stream-5
    ;; WRITE-BYTE
    (with-sc-test-stream (synonym)
      (let ((s (make-two-way-stream synonym synonym)))
        (write-byte 65 s)
        (force-output s))
      (file-position synonym 0)
      (read-char synonym))
  #\A)

#+nil ; fails
(deftest two-way-stream-6
    ;; WRITE-STRING
    (with-sc-test-stream (synonym)
      (let ((s (make-two-way-stream synonym synonym)))
        (write-string "ab" s)
        (force-output s))
      (file-position synonym 0)
      (and (char= (read-char synonym) #\a)
           (char= (read-char synonym) #\b)))
  T)

(deftest two-way-stream-7
    ;; LISTEN (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (synonym)
      (let ((s (make-two-way-stream synonym synonym)))
        (and (listen s) t)))
  T)

(deftest two-way-stream-8
    ;; CLEAR-INPUT (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (synonym)
      (let ((s (make-two-way-stream synonym synonym)))
        (clear-input s)))
  NIL)

(deftest two-way-stream-9
    ;; FORCE-OUTPUT (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (synonym)
      ;; could test more here
      (force-output (make-two-way-stream synonym synonym)))
  NIL)

(deftest two-way-stream-10
    ;; FINISH-OUTPUT (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (synonym)
      ;; could test more here
      (finish-output (make-two-way-stream synonym synonym)))
  NIL)

(deftest two-way-stream-11
    ;; STREAM-ELEMENT-TYPE (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (synonym)
      (eql (stream-element-type (make-two-way-stream synonym synonym))
           (stream-element-type synonym)))
  T)

(deftest two-way-stream-12
    ;; INTERACTIVE-STREAM-P (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (synonym)
      (eql (interactive-stream-p (make-two-way-stream synonym synonym))
           (interactive-stream-p synonym)))
  T)

(deftest two-way-stream-13
    ;; LINE-LENGTH (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (synonym)
      (eql (sb-kernel:line-length (make-two-way-stream synonym synonym))
           (sb-kernel:line-length synonym)))
  T)

(deftest two-way-stream-14
    ;; CHARPOS (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (synonym)
      (eql (sb-kernel:charpos (make-two-way-stream synonym synonym))
           (sb-kernel:charpos synonym)))
  T)

(deftest two-way-stream-16
    ;; FILE-POSITION (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (synonym)
      (eql (file-position (make-two-way-stream synonym synonym))
           (file-position synonym)))
  T)

;; SYNONYM-STREAM tests repeated for ECHO-STREAM, where applicable

(deftest echo-stream-1
    ;; READ-CHAR
    (with-dc-test-stream (*synonym*)
      (read-char (make-echo-stream *synonym* *synonym*)))
  #\T)

(deftest echo-stream-2
    ;; UNREAD-CHAR (via STREAM-MISC-DISPATCH)
    (with-dc-test-stream (*synonym*)
      (let ((s (make-echo-stream *synonym* *synonym*)))
        (unread-char (read-char s) s)
        (read-char s)))
  #\T)

(deftest echo-stream-3
    ;; READ-BYTE
    (with-dc-test-stream (*synonym*)
      (read-byte (make-echo-stream *synonym* *synonym*)))
  #.(char-code #\T))

(deftest echo-stream-7
    ;; LISTEN (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (*synonym*)
      (let ((s (make-echo-stream *synonym* *synonym*)))
        (and (listen s) t)))
  T)

(deftest echo-stream-8
    ;; CLEAR-INPUT (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (*synonym*)
      (let ((s (make-echo-stream *synonym* *synonym*)))
        (clear-input s)))
  NIL)

(deftest echo-stream-11
    ;; STREAM-ELEMENT-TYPE (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (*synonym*)
      (eql (stream-element-type (make-echo-stream *synonym* *synonym*))
           (stream-element-type *synonym*)))
  T)

(deftest echo-stream-12
    ;; INTERACTIVE-STREAM-P (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (*synonym*)
      (eql (interactive-stream-p (make-echo-stream *synonym* *synonym*))
           (interactive-stream-p *synonym*)))
  T)

(deftest echo-stream-13
    ;; LINE-LENGTH (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (*synonym*)
      (eql (sb-kernel:line-length (make-echo-stream *synonym* *synonym*))
           (sb-kernel:line-length *synonym*)))
  T)

(deftest echo-stream-14
    ;; CHARPOS (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (*synonym*)
      (eql (sb-kernel:charpos (make-echo-stream *synonym* *synonym*))
           (sb-kernel:charpos *synonym*)))
  T)

(deftest echo-stream-16
    ;; FILE-POSITION (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (*synonym*)
      (eql (file-position (make-echo-stream *synonym* *synonym*))
           (file-position *synonym*)))
  T)

;; SYNONYM-STREAM tests repeated for CONCATENATED-STREAM, where applicable

(deftest concatenated-stream-1
    ;; READ-CHAR
    (with-dc-test-stream (*synonym*)
      (read-char (make-concatenated-stream *synonym*)))
  #\T)

(deftest concatenated-stream-2
    ;; UNREAD-CHAR (via STREAM-MISC-DISPATCH)
    (with-dc-test-stream (*synonym*)
      (let ((s (make-concatenated-stream *synonym*)))
        (unread-char (read-char s) s)
        (read-char s)))
  #\T)

(deftest concatenated-stream-3
    ;; READ-BYTE
    (with-dc-test-stream (*synonym*)
      (read-byte (make-concatenated-stream *synonym*)))
  #.(char-code #\T))

(deftest concatenated-stream-7
    ;; LISTEN (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (*synonym*)
      (let ((s (make-concatenated-stream *synonym*)))
        (and (listen s) t)))
  T)

(deftest concatenated-stream-8
    ;; CLEAR-INPUT (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (*synonym*)
      (let ((s (make-concatenated-stream *synonym*)))
        (clear-input s)))
  NIL)

(deftest concatenated-stream-11
    ;; STREAM-ELEMENT-TYPE (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (*synonym*)
      (equal (stream-element-type (make-concatenated-stream *synonym*))
             (stream-element-type *synonym*)))
  T)

(deftest concatenated-stream-12
    ;; INTERACTIVE-STREAM-P (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (*synonym*)
      (eql (interactive-stream-p (make-concatenated-stream *synonym*))
           (interactive-stream-p *synonym*)))
  T)

(deftest concatenated-stream-13
    ;; LINE-LENGTH (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (*synonym*)
      (eql (sb-kernel:line-length (make-concatenated-stream *synonym*))
           (sb-kernel:line-length *synonym*)))
  T)

(deftest concatenated-stream-14
    ;; CHARPOS (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (*synonym*)
      (eql (sb-kernel:charpos (make-concatenated-stream *synonym*))
           (sb-kernel:charpos *synonym*)))
  T)

(deftest concatenated-stream-16
    ;; FILE-POSITION (via STREAM-MISC-DISPATCH)
    (with-sc-test-stream (*synonym*)
      (eql (file-position (make-concatenated-stream *synonym*))
           (file-position *synonym*)))
  T)

;; uncovered by synonym-stream-15

(deftest file-simple-stream-1
    (values (subtypep 'file-simple-stream 'file-stream))
  T)

(deftest string-simple-stream-1
    (values (subtypep 'string-simple-stream 'string-stream))
  T)

;; don't break fd-stream external-format support:

(deftest external-format-1
    (progn
      (with-open-file (s *test-file*
                       :direction :output
                       :if-exists :supersede
                       :element-type '(unsigned-byte 8))
        (write-byte 195 s)
        (write-byte 132 s))
      (with-open-file (s *test-file*
                       :direction :input
                       :external-format :utf-8)
        (char-code (read-char s))))
  196)

;; launchpad bug #491087

#+nil ; fails
(deftest lp491087
    (labels ((read-big-int (stream)
               (let ((b (make-array 1 :element-type '(signed-byte 32)
                                    :initial-element 0)))
                 (declare (dynamic-extent b))
                 (sb-simple-streams::read-vector b stream
                                                 :endian-swap :network-order)
                 (aref b 0))))
      (with-open-file (stream
                       (merge-pathnames #P"lp491087.txt" *test-path*)
                       :class 'file-simple-stream)
        (let* ((start (file-position stream))
               (integer (read-big-int stream))
               (end (file-position stream)))
          (and (= start 0)
               (= integer #x30313233)
               (= end 4)))))
  T)
