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
  (assert-error (make-two-way-stream (make-string-output-stream)
                                     (make-string-output-stream))
                type-error)
  (assert-error (make-two-way-stream (make-string-input-stream "foo")
                                     (make-string-input-stream "bar"))
                type-error)
  ;; the following two aren't actually guaranteed, because ANSI, as it
  ;; happens, doesn't say "should signal an error" for
  ;; MAKE-ECHO-STREAM. It's still good to have, but if future
  ;; maintenance work causes this test to fail because of these
  ;; MAKE-ECHO-STREAM clauses, consider simply removing these clauses
  ;; from the test. -- CSR, 2002-10-06
  (assert-error (make-echo-stream (make-string-output-stream)
                                  (make-string-output-stream))
                type-error)
  (assert-error (make-echo-stream (make-string-input-stream "foo")
                                  (make-string-input-stream "bar"))
                type-error)
  (assert-error (make-concatenated-stream
                 (make-string-output-stream)
                 (make-string-input-stream "foo"))
                type-error))

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
  (assert-error
   (with-open-file (s p :direction :output :if-exists :error)))
  (close stream)
  (delete-file p))

(assert-error (read-byte (make-string-input-stream "abc"))
              type-error)
(assert-error (with-open-file (s "/dev/zero")
                (read-byte s))
              #-win32 type-error
              #+win32 sb-int:simple-file-error)
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
    (let ((line (read-line s))
          (want "THESE INSERTMBOLS"))
      (unless (equal line want)
        (error "wanted ~S, got ~S" want line))))
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
(with-test (:name :test-file-for-close-should-not-delete :fails-on :win32)
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
            (test-mode :overwrite)
            ;; FIXME: We really should recover supersede files as well, according to
            ;; CLOSE in CLHS, but at the moment we don't.
            ;; (test-mode :supersede)
            (test-mode :rename)
            (test-mode :rename-and-delete))
        (when (probe-file test)
          (delete-file test))))))

;;; test for read-write invariance of signed bytes, from Bruno Haible
;;; cmucl-imp 2004-09-06
(defun bin-stream-test (&key (size (integer-length most-positive-fixnum))
                        (type 'unsigned-byte) (file-name "stream-impure.tmp")
                        (num-bytes 10)
                        (bytes (if (eq type 'signed-byte)
                                   (loop :repeat num-bytes :collect
                                         (- (random (ash 1 size))
                                            (ash 1 (1- size))))
                                   (loop :repeat num-bytes :collect
                                         (random (ash 1 size))))))
  (with-open-file (foo file-name :direction :output :if-exists :supersede
                       :element-type (list type size))
    (dolist (byte bytes)
      (write-byte byte foo)))
  (unwind-protect
       (with-open-file (foo file-name :direction :input
                            :element-type (list type size))
         (list (stream-element-type foo) (file-length foo) bytes
               (loop :for byte :in bytes :for nb = (read-byte foo) :collect nb
                     :unless (= nb byte) :do
                     (flet ((by-out (sz by)
                              (format nil "~v,'0,' ,4:b"
                                      (+ sz (floor sz 4)) by)))
                       (error "~& * [(~s ~s)] ~a != ~a~%" type size
                              (by-out size byte) (by-out size nb))))))
    (delete-file file-name)))
(loop for size from 2 to 40 do (bin-stream-test :size size :type 'signed-byte))

;;; Check READ-SEQUENCE signals a TYPE-ERROR when the sequence can't
;;; contain a stream element.
;;;
;;; These tests check READ-SEQUENCE correctness, not whether the fast
;;; or slow paths are being taken for each element type.  To check the
;;; fast or slow paths, trace ANSI-STREAM-READ-BYTE (slow path) and/or
;;; READ-N-BYTES:
;;;
;;; (trace sb-impl::ansi-stream-read-byte sb-impl::read-n-bytes)
;;;
;;; The order should be ANSI-STREAM-READ-BYTE, READ-N-BYTES,
;;; READ-N-BYTES, ANSI-STREAM-READ-BYTE, ANSI-STREAM-READ-BYTE.

(let ((pathname "read-sequence.data"))

  ;; Create the binary data.
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :element-type '(unsigned-byte 8))
    (write-byte 255 stream))

  ;; Check the slow path for generic vectors.
  (let ((sequence (make-array 1)))
    (with-open-file (stream pathname
                            :direction :input
                            :element-type '(unsigned-byte 8))
    (read-sequence sequence stream)
    (assert (equalp sequence #(255)))))

  (let ((sequence (make-array 1)))
    (with-open-file (stream pathname
                            :direction :input
                            :external-format :latin-1
                            :element-type 'character)
      (read-sequence sequence stream)
      (assert (equalp sequence #(#.(code-char 255))))))

  ;; Check the fast path works for (UNSIGNED-BYTE 8) and (SIGNED-BYTE
  ;; 8) vectors.
  (let ((sequence (make-array 1 :element-type '(unsigned-byte 8))))
    (with-open-file (stream pathname
                            :direction :input
                            :element-type '(unsigned-byte 8))
      (read-sequence sequence stream)
      (assert (equalp sequence #(255)))))

  (let ((sequence (make-array 1 :element-type '(signed-byte 8))))
    (with-open-file (stream pathname
                            :direction :input
                            :element-type '(signed-byte 8))
    (read-sequence sequence stream)
    (assert (equalp sequence #(-1)))))

  ;; A bivalent stream can be read to a unsigned-byte vector, a
  ;; string, or a generic vector

  (let ((sequence (make-array 1 :element-type '(unsigned-byte 8))))
    (with-open-file (stream pathname
                            :direction :input
                            :element-type :default)
      (read-sequence sequence stream)
      (assert (equalp sequence #(255)))))

  (let ((sequence (make-array 1 :element-type 'character)))
    (with-open-file (stream pathname
                            :direction :input
                            :external-format :latin-1
                            :element-type :default)
      (read-sequence sequence stream)
      (assert (equalp sequence #(#.(code-char 255))))))

  (let ((sequence (make-array 1)))
    (with-open-file (stream pathname
                            :direction :input
                            :external-format :latin-1
                            :element-type :default)
      (read-sequence sequence stream)
      (assert (equalp sequence #(#.(code-char 255))))))

  ;; Check that a TYPE-ERROR is signalled for incompatible (sequence,
  ;; stream) pairs.

  (let ((sequence (make-array 1 :element-type '(signed-byte 8))))
    (with-open-file (stream pathname
                            :direction :input
                            :element-type '(unsigned-byte 8))
      (handler-case (progn
                      (read-sequence sequence stream)
                      (error "READ-SEQUENCE didn't signal an error"))
        (type-error (condition)
          (assert (= (type-error-datum condition) 255))
          (assert (subtypep (type-error-expected-type condition)
                            '(signed-byte 8)))))))

  (let ((sequence (make-array 1 :element-type '(unsigned-byte 8))))
    (with-open-file (stream pathname
                            :direction :input
                            :element-type '(signed-byte 8))
      (handler-case (progn
                      (read-sequence sequence stream)
                      (error "READ-SEQUENCE didn't signal an error"))
        (type-error (condition)
          (assert (= (type-error-datum condition) -1))
          (assert (subtypep (type-error-expected-type condition)
                            '(unsigned-byte 8)))))))

  ;; Can't read a signed-byte from a bivalent stream

  (let ((sequence (make-array 1 :element-type '(signed-byte 8))))
    (with-open-file (stream pathname
                            :direction :input
                            :external-format :latin1
                            :element-type :default)
      (handler-case (progn
                      (read-sequence sequence stream)
                      (error "READ-SEQUENCE didn't signal an error"))
        (type-error (condition)
          (assert (eql (type-error-datum condition) 255))
          (assert (subtypep (type-error-expected-type condition)
                            '(signed-byte 8)))))))
  (delete-file pathname))

;;; Check WRITE-SEQUENCE signals a TYPE-ERROR when the stream can't
;;; write a sequence element.
;;;
;;; These tests check WRITE-SEQUENCE correctness, not whether the fast
;;; or slow paths are being taken for each element type.  See the
;;; READ-SEQUENCE tests above for more information.
;;;
;;; (trace sb-impl::output-unsigned-byte-full-buffered sb-impl::output-signed-byte-full-buffered sb-impl::output-raw-bytes)

(let ((pathname "write-sequence.data")
      (generic-sequence (make-array 1 :initial-contents '(255)))
      (generic-character-sequence (make-array 1 :initial-element #\a))
      (generic-mixed-sequence (make-array 2 :initial-element #\a))
      (string (make-array 1 :element-type 'character
                          :initial-element (code-char 255)))
      (unsigned-sequence (make-array 1
                                     :element-type '(unsigned-byte 8)
                                     :initial-contents '(255)))
      (signed-sequence (make-array 1
                                   :element-type '(signed-byte 8)
                                   :initial-contents '(-1))))

  (setf (aref generic-mixed-sequence 1) 255)

  ;; Check the slow path for generic vectors.
  (with-open-file (stream pathname
                           :direction :output
                           :if-exists :supersede
                           :element-type '(unsigned-byte 8))
    (write-sequence generic-sequence stream))

  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :element-type 'character)
    (write-sequence generic-character-sequence stream))

  ;; Check the fast path for unsigned and signed vectors.
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :element-type '(unsigned-byte 8))
    (write-sequence unsigned-sequence stream))

  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :element-type '(signed-byte 8))
    (write-sequence signed-sequence stream))

  ;; Bivalent streams on unsigned-byte vectors, strings, and a simple
  ;; vector with mixed characters and bytes

  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :element-type :default)
    (write-sequence unsigned-sequence stream))

  (with-open-file (stream pathname
                          :direction :output
                          :external-format :latin-1
                          :if-exists :supersede
                          :element-type :default)
    (write-sequence string stream))

  (with-open-file (stream pathname
                          :direction :output
                          :external-format :latin-1
                          :if-exists :supersede
                          :element-type :default)
    (write-sequence generic-mixed-sequence stream))

  ;; Check a TYPE-ERROR is signalled for unsigned and signed vectors
  ;; which are incompatible with the stream element type.
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :element-type '(signed-byte 8))
    (handler-case (progn
                    (write-sequence unsigned-sequence stream)
                    (error "WRITE-SEQUENCE didn't signal an error"))
      (type-error (condition)
        (assert (= (type-error-datum condition) 255))
        (assert (subtypep (type-error-expected-type condition)
                          '(signed-byte 8))))))

  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :element-type '(unsigned-byte 8))
    (handler-case (progn
                    (write-sequence signed-sequence stream)
                    (error "WRITE-SEQUENCE didn't signal an error"))
      (type-error (condition)
        (assert (= (type-error-datum condition) -1))
        (assert (subtypep (type-error-expected-type condition)
                          '(unsigned-byte 8))))))

  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :element-type :default)
    (handler-case (progn
                    (write-sequence signed-sequence stream)
                    (error "WRITE-SEQUENCE didn't signal an error"))
      (type-error (condition)
        (assert (= (type-error-datum condition) -1))
        (assert (subtypep (type-error-expected-type condition)
                          '(unsigned-byte 8))))))

  (delete-file pathname))

;;; writing looong lines. takes way too long and way too much space
;;; to test on 64 bit platforms
#-64-bit
(let ((test "long-lines-write-test.tmp"))
    (unwind-protect
         (with-open-file (f test
                            :direction :output
                            :external-format :ascii
                            :element-type 'character
                            :if-does-not-exist :create
                            :if-exists :supersede)
           (let* ((n (truncate most-positive-fixnum 16))
                  (m 18)
                  (p (* n m))
                  (buffer (make-string n)))
             (dotimes (i m)
               (write-char #\.)
               (finish-output)
               (write-sequence buffer f))
             (assert (= p (sb-impl::fd-stream-output-column f)))
             (write-char #\! f)
             (assert (= (+ 1 p) (sb-impl::fd-stream-output-column f)))
             (assert (typep p 'bignum))))
      (when (probe-file test)
        (delete-file test))))

;;; read-sequence misreported the amount read and lost position
(let ((string (make-array (* 3 sb-impl::+ansi-stream-in-buffer-length+)
                          :element-type 'character)))
  (dotimes (i (length string))
    (setf (char string i) (code-char (mod i char-code-limit))))
  (with-open-file (f "read-sequence-character-test-data.tmp"
                     :if-exists :supersede
                     :direction :output
                     :external-format :utf-8)
    (write-sequence string f))
  (let ((copy
         (with-open-file (f "read-sequence-character-test-data.tmp"
                            :if-does-not-exist :error
                            :direction :input
                            :external-format :utf-8)
           (let ((buffer (make-array 128 :element-type 'character))
                 (total 0))
             (with-output-to-string (datum)
               (loop for n-read = (read-sequence buffer f)
                     do (write-sequence buffer datum :start 0 :end n-read)
                        (assert (<= (incf total n-read) (length string)))
                     while (and (= n-read 128))))))))
    (assert (equal copy string)))
  (delete-file "read-sequence-character-test-data.tmp"))

;;; ANSI-STREAM-OUTPUT-STREAM-P used to assume that a SYNONYM-STREAM's
;;; target was an ANSI stream, but it could be a user-defined stream,
;;; e.g., a SLIME stream.
(defclass user-output-stream (fundamental-output-stream)
  ())

(let ((*stream* (make-instance 'user-output-stream)))
  (declare (special *stream*))
  (with-open-stream (stream (make-synonym-stream '*stream*))
    (assert (output-stream-p stream))))

(defclass user-input-stream (fundamental-input-stream)
  ())

(let ((*stream* (make-instance 'user-input-stream)))
  (declare (special *stream*))
  (with-open-stream (stream (make-synonym-stream '*stream*))
    (assert (input-stream-p stream))))

;;; READ-LINE on ANSI-STREAM did not return T for the last line
;;; (reported by Yoshinori Tahara)
(let ((pathname "test-read-line-eol"))
  (with-open-file (out pathname :direction :output :if-exists :supersede)
    (format out "a~%b"))
  (let ((result (with-open-file (in pathname)
                  (list (multiple-value-list (read-line in nil nil))
                        (multiple-value-list (read-line in nil nil))
                        (multiple-value-list (read-line in nil nil))))))
    (delete-file pathname)
    (assert (equal result '(("a" nil) ("b" t) (nil t))))))

;;; READ-LINE used to work on closed streams because input buffers were left in place
(with-test (:name :bug-425)
  ;; Normal close
  (let ((f (open "stream.impure.lisp" :direction :input)))
    (assert (stringp (read-line f)))
    (close f)
    (assert (eq :fii
                (handler-case
                    (read-line f)
                  (sb-int:closed-stream-error () :fii)))))
  ;; Abort
  (let ((f (open "stream.impure.lisp" :direction :input)))
    (assert (stringp (read-line f nil nil)))
    (close f :abort t)
    (assert (eq :faa
                (handler-case
                    (read-line f)
                  (sb-int:closed-stream-error () :faa))))))

(with-test (:name :regression-1.0.12.22)
  (with-open-file (s "stream.impure.lisp" :direction :input)
    (let ((buffer (make-string 20)))
      (assert (= 2 (read-sequence buffer s :start 0 :end 2)))
      (assert (= 3 (read-sequence buffer s :start 2 :end 3)))
      (file-position s :end)
      (assert (= 3 (read-sequence buffer s :start 3))))))

;;; In 1.0.27 (and also 0.9.16; presumably in between, too), binary
;;; input operations on a bivalent stream did something bad after
;;; unread-char: READ-BYTE would return the character, and
;;; READ-SEQUENCE into a byte buffer would lose when attempting to
;;; store the character in the vector.
(let ((pathname "bivalent-stream-unread-char-test.tmp"))
  (with-open-file (s pathname
                     :element-type :default
                     :direction :io :if-exists :rename)
    (write-char #\a s)
    (file-position s :start)
    (unread-char (read-char s) s)
    (assert (integerp (read-byte s))))
  (delete-file pathname))

(let ((pathname "bivalent-stream-unread-char-test.tmp"))
  (with-open-file (s pathname
                     :element-type :default
                     :direction :io :if-exists :rename)
    (write-char #\a s)
    (file-position s :start)
    (unread-char (read-char s) s)
    (assert (let ((buffer (make-array 10 :element-type '(unsigned-byte 8))))
              (read-sequence buffer s))))
  (delete-file pathname))

#+sb-unicode
(let ((pathname "bivalent-stream-unread-char-test.tmp"))
  (with-open-file (s pathname
                     :element-type :default
                     :direction :io :if-exists :rename
                     :external-format :utf8)
    (write-char (code-char 192) s)
    (file-position s :start)
    (unread-char (read-char s) s)
    (assert (integerp (read-byte s))))
  (delete-file pathname))

#+sb-unicode
(let ((pathname "bivalent-stream-unread-char-test.tmp"))
  (with-open-file (s pathname
                     :element-type :default
                     :direction :io :if-exists :rename
                     :external-format :utf8)
    (write-char (code-char 192) s)
    (file-position s :start)
    (unread-char (read-char s) s)
    (assert (let ((buffer (make-array 10 :element-type '(unsigned-byte 8))))
              (read-sequence buffer s))))
  (delete-file pathname))

(with-test (:name :delete-file-on-streams)
  (with-open-file (f "delete-file-on-stream-test.tmp"
                     :direction :io)
    (delete-file f)
    #-win32
    (progn
      (write-line "still open" f)
      (file-position f :start)
      (assert (equal "still open" (read-line f)))))
  (assert (not (probe-file "delete-file-on-stream-test.tmp"))))

;;; READ-CHAR-NO-HANG on bivalent streams (as returned by RUN-PROGRAM)
;;; was wrong.  CSR managed to promote the wrongness to all streams in
;;; the 1.0.32.x series, breaking slime instantly.
(with-test (:name :read-char-no-hang-after-unread-char :skipped-on :win32)
  (let* ((process (run-program "/bin/sh" '("-c" "echo a && sleep 10")
                               :output :stream :wait nil))
         (stream (process-output process))
         (char (read-char stream)))
    (assert (char= char #\a))
    (unread-char char stream)
    (assert (char= (read-char stream) #\a))
    (assert (char= (read-char stream) #\Newline))
    (let ((time (get-universal-time)))
      ;; no input, not yet known to be at EOF: should return
      ;; immediately
      (read-char-no-hang stream)
      (assert (< (- (get-universal-time) time) 2)))))

(require :sb-posix)
#-win32
(with-test (:name :interrupt-open :skipped-on :win32)
  (let ((fifo nil)
        (to 0))
    (unwind-protect
         (progn
           ;; Make a FIFO
           (setf fifo (sb-posix:mktemp "SBCL-fifo.XXXXXXX"))
           (sb-posix:mkfifo fifo (logior sb-posix:s-iwusr sb-posix:s-irusr))
           ;; Try to open it (which hangs), and interrupt ourselves with a timer,
           ;; continue (this used to result in an error due to open(2) returning with
           ;; EINTR, then interupt again and unwind.
           (handler-case
               (with-timeout 2
                 (handler-bind ((timeout (lambda (c)
                                           (when (eql 1 (incf to))
                                             (continue c)))))
                   (with-timeout 1
                     (with-open-file (f fifo :direction :input)
                       :open))))
             (timeout ()
               (if (eql 2 to)
                   :timeout
                   :wtf))
             (error (e)
               e)))
      (when fifo
        (ignore-errors (delete-file fifo))))))

#-win32
(with-test (:name :overeager-character-buffering :skipped-on :win32)
  (let ((fifo nil)
        (proc nil))
    (maphash
     (lambda (format _)
       (declare (ignore _))
       (format t "trying ~A~%" format)
       (finish-output t)
       (unwind-protect
            (progn
              (setf fifo (sb-posix:mktemp "SBCL-fifo-XXXXXXX"))
              (sb-posix:mkfifo fifo (logior sb-posix:s-iwusr sb-posix:s-irusr))
              ;; KLUDGE: because we have both ends in the same process, we would
              ;; need to use O_NONBLOCK, but this works too.
              (setf proc
                    (run-program "/bin/sh"
                                 (list "-c"
                                       (format nil "cat > ~A" (native-namestring fifo)))
                                 :input :stream
                                 :wait nil
                                 :external-format format))
              (write-line "foobar" (process-input proc))
              (finish-output (process-input proc))
              (with-open-file (f fifo :direction :input :external-format format)
                (assert (equal "foobar" (read-line f)))))
         (when proc
           (ignore-errors
             (close (process-input proc) :abort t)
             (process-wait proc))
           (ignore-errors (process-close proc))
           (setf proc nil))
         (when fifo
           (ignore-errors (delete-file fifo))
           (setf fifo nil))))
     sb-impl::*external-formats*)))

(with-test (:name :bug-657183 :skipped-on '(not :sb-unicode))
  #+sb-unicode
  (let ((name (merge-pathnames "stream-impure.temp-test"))
        (text '(#\GREEK_SMALL_LETTER_LAMDA
                #\JAPANESE_BANK_SYMBOL
                #\Space
                #\HEAVY_BLACK_HEART))
        (positions '(2 5 6 9))
        (sb-impl::*default-external-format* :utf-8))
    (unwind-protect
         (progn
           (with-open-file (f name :external-format :default :direction :output
                              :if-exists :supersede)
             (assert (eql 0 (file-position f)))
             (mapc (lambda (char pos)
                     (write-char char f)
                     (assert (eql pos (file-position f))))
                   text
                   positions))
           (with-open-file (f name :external-format :default :direction :input)
             (assert (eql 0 (file-position f)))
             (assert (eql (pop text) (read-char f)))
             (assert (eql (file-position f) 2))
             (assert (eql (pop text) (read-char f)))
             (assert (eql (file-position f) 5))
             (assert (eql (pop text) (read-char f)))
             (assert (eql (file-position f) 6))
             (assert (eql (pop text) (read-char f)))
             (assert (eql (file-position f) 9))
             (assert (eql (file-length f) 9))))
      (ignore-errors (delete-file name)))))

(with-test (:name :bug-561642)
  (let ((p "bug-561642-test.tmp"))
    (unwind-protect
         (progn
           (with-open-file (f p
                              :if-exists :supersede
                              :if-does-not-exist :create
                              :direction :output)
             (write-line "FOOBAR" f))
           (with-open-file (f p
                              :if-exists :append
                              :direction :output)
             (let ((p0 (file-position f))
                   (p1 (progn
                         (write-char #\newline f)
                         (file-position f)))
                   (p2 (progn
                         (write-char #\newline f)
                         (finish-output f)
                         (file-position f))))
               (assert (eql 7 p0))
               (assert (eql 8 p1))
               (assert (eql 9 p2)))))
      (ignore-errors (delete-file p)))))

(defstruct (mock-fd-stream
            (:constructor %make-mock-fd-stream (buffer-chain))
            (:include sb-impl::ansi-stream
                      (in #'mock-fd-stream-in)
                      (n-bin #'mock-fd-stream-n-bin)
                      (cin-buffer
                       (make-array sb-impl::+ansi-stream-in-buffer-length+
                                   :element-type 'character))))
  buffer-chain)

(defun make-mock-fd-stream (buffer-chain)
  ;; For notational convenience, #\| becomes #\Newline.
  (%make-mock-fd-stream
   (mapcar (lambda (x) (substitute #\Newline #\| x)) buffer-chain)))

(defun mock-fd-stream-in (stream eof-err-p eof-val)
  (sb-impl::eof-or-lose stream eof-err-p eof-val))

(defun mock-fd-stream-n-bin (stream char-buf start count eof-err-p)
  (cond ((mock-fd-stream-buffer-chain stream)
         (let* ((chars (pop (mock-fd-stream-buffer-chain stream)))
                (n-chars (length chars)))
           ;; make sure the mock object is being used as expected.
           (assert (>= count (length chars)))
           (replace char-buf chars :start1 start)
           n-chars))
        (t
         (sb-impl::eof-or-lose stream eof-err-p 0))))

(with-test (:name :read-chunk-from-frc-buffer)
  (let ((s (make-mock-fd-stream '("zabc" "d" "efgh" "foo|bar" "hi"))))
    (multiple-value-bind (line eofp)
        (sb-impl::ansi-stream-read-line-from-frc-buffer s nil 'woot)
      (assert (and (string= line "zabcdefghfoo") (not eofp))))
    (multiple-value-bind (line eofp)
        (sb-impl::ansi-stream-read-line-from-frc-buffer s nil 'woot)
      (assert (and (string= line "barhi") eofp)))
    (multiple-value-bind (line eofp)
        (sb-impl::ansi-stream-read-line-from-frc-buffer s nil 'woot)
      (assert (and (eq line 'woot) eofp))))
  (let ((s (make-mock-fd-stream '("zabc" "d" "efgh" "foo*bar" "hi")))
        (string (make-string 100)))
    (let ((endpos
           (sb-impl::ansi-stream-read-string-from-frc-buffer string s 10 nil)))
      (assert (and (= endpos 28)
                   (string= (subseq string 10 endpos) "zabcdefghfoo*barhi"))))
    (let ((endpos
           (sb-impl::ansi-stream-read-string-from-frc-buffer string s 0 nil)))
      (assert (= endpos 0)))))

;;; success
