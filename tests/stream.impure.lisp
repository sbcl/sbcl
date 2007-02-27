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
           (test-mode :overwrite)
           ;; FIXME: We really should recover supersede files as well, according to
           ;; CLOSE in CLHS, but at the moment we don't.
           ;; (test-mode :supersede)
           (test-mode :rename)
           (test-mode :rename-and-delete))
      (when (probe-file test)
        (delete-file test)))))

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
          (assert (eql (type-error-datum condition) (code-char 255)))
          (assert (subtypep (type-error-expected-type condition)
                            '(signed-byte 8))))))))

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
                          '(unsigned-byte 8)))))))

;;; success
