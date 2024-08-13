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

;;; Until sbcl-0.6.11.31, we didn't have an N-BIN method for
;;; CONCATENATED-STREAM, so stuff like this would fail.
(with-test (:name (concatenated-stream read-sequence 1))
  (let ((stream (make-concatenated-stream (make-string-input-stream "Demo")))
        (buffer (make-string 4)))
    (read-sequence buffer stream)))

;;; test for the new N-BIN method doing what it's supposed to
(with-test (:name (concatenated-stream read-sequence 2))
  (let* ((substrings (list "This " "is " "a " ""
                           "test of concatenated streams behaving "
                           "as ordinary streams do under READ-SEQUENCE. "
                           (make-string 140041 :initial-element #\%)
                           "For any size of read.."
                           (make-string 4123 :initial-element #\.)
                           "they should give the same results."
                           (make-string (expt 2 14) :initial-element #\*)
                           "There should be no differences."))
         (substreams (mapcar #'make-string-input-stream substrings))
         (concatenated-stream (apply #'make-concatenated-stream substreams))
         (concatenated-string (apply #'concatenate 'string substrings))
         (stream (make-string-input-stream concatenated-string))
         (max-n-to-read 24)
         (buffer-1 (make-string max-n-to-read))
         (buffer-2 (make-string max-n-to-read)))
    (loop
      (let* ((n-to-read (random max-n-to-read))
             (n-actually-read-1 (read-sequence buffer-1
                                               concatenated-stream
                                               :end n-to-read))
             (n-actually-read-2 (read-sequence buffer-2
                                               stream
                                               :end n-to-read)))
        ;;     (format t "buffer-1=~S~%buffer-2=~S~%" buffer-1 buffer-2)
        (assert (= n-actually-read-1 n-actually-read-2))
        (assert (string= buffer-1 buffer-2
                         :end1 n-actually-read-1
                         :end2 n-actually-read-2))
        (unless (= n-actually-read-1 n-to-read)
          (assert (< n-actually-read-1 n-to-read))
          (return))))))

;;; Entomotomy PEEK-CHAR-WRONGLY-ECHOS-TO-ECHO-STREAM bug, fixed by
;;; MRD patch sbcl-devel 2002-11-02 merged ca. sbcl-0.7.9.32...
(with-test (:name (peek-char :wrongly-echos-to echo-stream))
  (assert (string=
           (with-output-to-string (out)
             (peek-char #\]
                        (make-echo-stream
                         (make-string-input-stream "ab cd e df s]") out)))
           ;; (Before the fix, the result had a trailing #\] in it.)
           "ab cd e df s")))

;;; ...and a missing wrinkle in the original patch, dealing with
;;; PEEK-CHAR/UNREAD-CHAR on ECHO-STREAMs, fixed by MRD patch
;;; sbcl-devel 2002-11-18, merged ca. sbcl-0.7.9.66
(with-test (:name (unread-char peek-char echo-stream))
  (assert (string=
           (let* ((in-stream (make-string-input-stream "abc"))
                  (out-stream (make-string-output-stream))
                  (echo-stream (make-echo-stream in-stream out-stream)))
             (unread-char (read-char echo-stream) echo-stream)
             (peek-char #\a echo-stream)
             (get-output-stream-string out-stream))
           ;; (Before the fix, the LET* expression just signalled an error.)
           "a")))

;;; ... and yet, a little over 6 years on, echo-streams were still
;;; broken when a read-char followed the unread/peek sequence.  Do
;;; people not actually use echo-streams?  RMK, 2009-04-02.
(with-test (:name (unread-char peek-char read-char echo-stream))
  (assert (string=
           (let* ((in-stream (make-string-input-stream "abc"))
                  (out-stream (make-string-output-stream))
                  (echo-stream (make-echo-stream in-stream out-stream)))
             (unread-char (read-char echo-stream) echo-stream)
             (peek-char nil echo-stream)
             (read-char echo-stream)
             (get-output-stream-string out-stream))
           ;; before ca. 1.0.27.18, the LET* returned "aa"
           "a")))

;;; Reported by Fredrik Sandstrom to sbcl-devel 2005-05-17 ("Bug in
;;; peek-char"):
;;; Description: In (peek-char nil s nil foo), if foo happens to be
;;; the same character that peek-char returns, the character is
;;; removed from the input stream, as if read by read-char.
(with-test (:name (peek-char :eof-value))
  (assert (equal (with-input-from-string (s "123")
                   (list (peek-char nil s nil #\1) (read-char s) (read-char s)))
                 '(#\1 #\1 #\2))))

;;; ... and verify that the fix does not break echo streams
(with-test (:name (peek-char :eof-value echo-stream))
  (assert (string= (let ((out (make-string-output-stream)))
                     (with-open-stream (s (make-echo-stream
                                           (make-string-input-stream "123")
                                           out))
                       (format s "=>~{~A~}"
                               (list (peek-char nil s nil #\1)
                                     (read-char s)
                                     (read-char s)))
                       (get-output-stream-string out)))
                   "12=>112")))

;;; 0.7.12 doesn't advance current stream in concatenated streams
;;; correctly when searching a stream for a char to read.
(with-test (:name (concatenated-stream peek-char))
  (with-input-from-string (p "")
    (with-input-from-string (q "foo")
      (let* ((r (make-concatenated-stream p q)))
        (peek-char nil r)))))

;;; 0.7.14 and previous SBCLs don't have a working INTERACTIVE-STREAM-P
;;; because it called UNIX-ISATTY, which wasn't defined.
(with-test (:name (interactive-stream-p))
  (with-input-from-string (s "a non-interactive stream")
    (assert (not (interactive-stream-p s)))))

;;; KLUDGE: Unfortunately it's hard to find a reliably interactive
;;; stream to test, since it's reasonable for these tests to be run
;;; from a script, conceivably even as something like a cron job.
;;; Ideas?
#+nil (assert (eq (interactive-stream-p *terminal-io*) t))

;;; FILE-POSITION should not accept NIL
(with-test (:name :file-position-smoke-test)
  (let ((s (make-broadcast-stream)))
    (assert-error (file-position s (opaque-identity nil)) type-error)))

;;; MAKE-STRING-INPUT-STREAM
;;;
;;; * Observe FILE-POSITION :START and :END, and allow setting of
;;;   FILE-POSITION beyond the end of string, signalling END-OF-FILE only
;;;   on read.
(with-test (:name (make-string-input-stream file-position))
  (let* ((string (copy-seq "abc"))
         (stream (make-string-input-stream string)))
    (assert (char= (read-char stream) #\a))
    (assert (= 1 (file-position stream)))
    (assert (file-position stream :start))
    (assert (= 0 (file-position stream)))
    (assert (file-position stream :end))
    (assert (= (length string) (file-position stream)))
    (assert (file-position stream (1- (file-position stream))))
    (assert (char= (read-char stream) #\c))
    (assert (file-position stream (1- (file-position stream))))
    (assert (char= (read-char stream) #\c))
    (assert (file-position stream :end))
    (let ((eof (cons nil nil)))
      (assert (eq (read-char stream nil eof) eof)))
    (assert (file-position stream 10))
    ;; Avoid type mismatch warning when compiling of this file.
    (let ((fun (checked-compile `(lambda (stream)
                                   (file-position stream -1))
                                :allow-warnings t)))
      (assert-error (funcall fun stream) type-error))
    (assert-error (read-char stream) end-of-file)))

;;; MAKE-STRING-OUTPUT-STREAM
;;;
;;; * Observe FILE-POSITION :START and :END, and allow setting of
;;;   FILE-POSITION to an arbitrary index.
;;;
;;; * END will always refer to the farthest position of stream so-far
;;;   seen, and setting FILE-POSITION beyond the current END will extend
;;;   the string/stream with uninitialized elements.
;;;
;;; * Rewinding the stream works with overwriting semantics.
;;;
(with-test (:name (make-string-output-stream file-position))
  (let ((stream (make-string-output-stream)))
    (princ "abcd" stream)
    (assert (= 4 (file-position stream)))
    (assert (file-position stream :start))
    (assert (= 0 (file-position stream)))
    (princ "0" stream)
    (assert (= 1 (file-position stream)))
    (file-position stream 2)
    (assert (= 2 (file-position stream)))
    (princ "2" stream)
    (assert (file-position stream :end))
    (assert (= 4 (file-position stream)))
    (assert (file-position stream 6))
    (assert (file-position stream 4))
    (assert (file-position stream :end))
    (assert (= 6 (file-position stream)))
    (assert (file-position stream 4))
    ;; Avoid type mismatch warning when compiling of this file.
    (let ((fun (checked-compile `(lambda (stream)
                                   (file-position stream -1))
                                :allow-warnings t)))
      (assert-error (funcall fun stream) type-error))
    (princ "!!" stream)
    (assert (equal "0b2d!!" (get-output-stream-string stream)))))

(with-test (:name (make-string-output-stream file-position :lp-1839040))
  (let ((stream (make-string-output-stream)))
    (dotimes (i 64) (write-char #\a stream))
    (file-position stream 40)
    (write-char #\x stream)
    (file-position stream 39)
    (write-char #\y stream)
    (file-position stream 41)
    (write-char #\z stream)
    (let ((string (get-output-stream-string stream)))
      (assert (equal "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaayxzaaaaaaaaaaaaaaaaaaaaaa" string))))
  (let ((stream (make-string-output-stream)))
    (dotimes (i 64) (write-char #\a stream))
    (dotimes (i 64) (write-char #\b stream))
    (file-position stream 3)
    (file-position stream 4)
    (write-char #\x stream)
    (let ((string (get-output-stream-string stream))
          (expected (concatenate
                     'string
                     (loop for i from 0 below 64 collect (if (= i 4) #\x #\a))
                     (loop for i from 0 below 64 collect #\b))))
      (assert (equal expected string)))))

;;; WITH-OUTPUT-TO-STRING (when provided with a string argument)
;;;
;;; * Observe FILE-POSITION :START and :END, and allow setting of
;;; FILE-POSITION to an arbitrary index. If the new position is beyond
;;; the end of string and the string is adjustable the string will be
;;; implicitly extended, otherwise an error will be signalled. The
;;; latter case is provided for in the code, but not currently
;;; excercised since SBCL fill-pointer arrays are always (currently)
;;; adjustable.
;;;
;;; * END will refer to the ARRAY-TOTAL-SIZE of string, not
;;; FILL-POINTER, since by definition the FILE-POSITION will always be
;;; a FILL-POINTER, so that would be of limited use.
;;;
;;; * Rewinding the stream works with overwriting semantics.
;;;
#+nil (let ((str (make-array 0
                       :element-type 'character
                       :adjustable nil
                       :fill-pointer t)))
  (with-output-to-string (stream str)
    (princ "abcd" stream)
    (assert (= 4 (file-position stream)))
    (assert (file-position stream :start))
    (assert (= 0 (file-position stream)))
    (princ "0" stream)
    (assert (= 1 (file-position stream)))
    (file-position stream 2)
    (assert (= 2 (file-position stream)))
    (princ "2" stream)
    (assert (file-position stream :end))
    (assert (= 4 (file-position stream)))
    (multiple-value-bind (val cond) (ignore-errors (file-position stream -1))
      (assert (null val))
      (assert (typep cond 'error)))
    (multiple-value-bind (val cond) (ignore-errors (file-position stream 6))
      (assert (null val))
      (assert (typep cond 'error)))
    (assert (equal "0b2d" str))))

(with-test (:name (with-output-to-string file-position))
  (let ((str (make-array 0
                         :element-type 'character
                         :adjustable nil
                         :fill-pointer t)))
    (with-output-to-string (stream str)
      (princ "abcd" stream)
      (assert (= 4 (file-position stream)))
      (assert (file-position stream :start))
      (assert (= 0 (file-position stream)))
      (princ "0" stream)
      (assert (= 1 (file-position stream)))
      (file-position stream 2)
      (assert (= 2 (file-position stream)))
      (princ "2" stream)
      (assert (file-position stream :end))
      (assert (= 4 (file-position stream)))
      (assert (file-position stream 6))
      (assert (file-position stream 4))
      (assert (file-position stream :end))
      (assert (= 6 (file-position stream)))
      (assert (file-position stream 4))
      ;; Avoid type mismatch warning when compiling of this file.
      (let ((fun (checked-compile `(lambda (stream)
                                     (file-position stream -1))
                                  :allow-warnings t)))
        (assert-error (funcall fun stream) type-error))
      (princ "!!" stream)
      (assert (equal "0b2d!!" str)))))

;;; MAKE-STRING-OUTPUT-STREAM and WITH-OUTPUT-TO-STRING take an
;;; :ELEMENT-TYPE keyword argument
(with-test (:name (make-string-output-stream with-output-to-string :element-type))
  (macrolet ((frob (element-type-form expect &optional (expect2 expect))
               `(progn
                  (let ((s (with-output-to-string
                               (s nil ,@(when element-type-form
                                          `(:element-type ,element-type-form))))))
                    (assert (typep s '(simple-array ,expect (0)))))
                  (let ((s (get-output-stream-string
                            (make-string-output-stream
                             ,@(when element-type-form
                                 `(:element-type ,element-type-form))))))
                    (assert (typep s '(simple-array ,expect2 (0))))))))
    ;; If you pass NIL as element-type, note that there seems to be no requirement
    ;; to produce a stream that can *accept* only characters of that type.
    ;; We produce a CHARACTER-STRING-OUTPUT-STREAM if you do something so pointless.
    (frob nil character)
    (frob 'character character)
    (frob 'base-char base-char)
    ;; I literally do not care why these results differ.
    (frob 'nil base-char character)))

(with-test (:name (make-string-output-stream :element-type :bogosity))
  (assert-error (make-string-output-stream :element-type 'real)))

(with-test (:name (read-byte :element-type :eof-value))
  (with-open-file (s #-win32 "/dev/null" #+win32 "nul" :element-type '(signed-byte 48))
    (assert (eq :eof (read-byte s nil :eof)))))

(with-test (:name (echo-stream read-sequence 1))
 (let* ((is (make-string-input-stream "foo"))
        (os (make-string-output-stream))
        (s (make-echo-stream is os))
        (sequence (copy-seq "abcdef")))
   (assert (= (read-sequence sequence s) 3))
   (assert (string= sequence "foodef"))
   (assert (string= (get-output-stream-string os) "foo"))))

(with-test (:name (echo-stream read-sequence 2))
 (let* ((is (make-string-input-stream "foo"))
        (os (make-string-output-stream))
        (s (make-echo-stream is os))
        (sequence (copy-seq "abcdef")))
   (assert (char= #\f (read-char s)))
   (assert (= (read-sequence sequence s) 2))
   (assert (string= sequence "oocdef"))
   (assert (string= (get-output-stream-string os) "foo"))))

(with-test (:name (echo-stream read-sequence 3))
 (let* ((is (make-string-input-stream "foo"))
        (os (make-string-output-stream))
        (s (make-echo-stream is os))
        (sequence (copy-seq "abcdef")))
   (assert (char= #\f (read-char s)))
   (unread-char #\f s)
   (assert (= (read-sequence sequence s) 3))
   (assert (string= sequence "foodef"))
   (assert (string= (get-output-stream-string os) "foo"))))

(with-test (:name (with-standard-io-syntax open))
  (with-standard-io-syntax
    (open #-win32 "/dev/null" #+win32 "nul" )))

;;; PEEK-CHAR T uses whitespace[2]
(with-test (:name (peek-char :whitespace[2]))
  (let ((*readtable* (copy-readtable)))
    (assert (char= (peek-char t (make-string-input-stream " a")) #\a))
    (set-syntax-from-char #\Space #\a)
    (assert (char= (peek-char t (make-string-input-stream " a")) #\Space))))

(with-test (:name :whitespace[2]p-is-type-safe)
  (let ((fun (checked-compile `(lambda () (sb-impl::whitespace[2]p :potato *readtable*))
                              :allow-warnings t)))
    (assert-error (funcall fun) type-error)))

;;; It is actually easier to run into the problem exercised by this
;;; test with sockets, due to their delays between availabilities of
;;; data.  However edgy the case may be for normal files, however,
;;; there is still a case to be found in which CL:LISTEN answers
;;; improperly.
;;;
;;; This test assumes that buffering is still done until a buffer of
;;; SB-IMPL::+BYTES-PER-BUFFER+ bytes is filled up, that the buffer may
;;; immediately be completely filled for normal files, and that the
;;; buffer-fill routine is responsible for figuring out when we've
;;; reached EOF.
(with-test (:name (stream :listen-vs-select))
  (let ((listen-testfile-name (scratch-file-name))
        ;; If non-NIL, size (in bytes) of the file that will exercise
        ;; the LISTEN problem.
        (bytes-per-buffer-sometime
         (and (boundp 'sb-impl::+bytes-per-buffer+)
              (symbol-value 'sb-impl::+bytes-per-buffer+))))
    (when bytes-per-buffer-sometime
      (unwind-protect
           (progn
             (with-open-file (stream listen-testfile-name
                                     :direction :output :if-exists :error
                                     :element-type '(unsigned-byte 8))
               (dotimes (n bytes-per-buffer-sometime)
                 (write-byte 113 stream)))
             (with-open-file (stream listen-testfile-name
                                     :direction :input :element-type '(unsigned-byte 8))
               (dotimes (n bytes-per-buffer-sometime)
                 (read-byte stream))
               (assert (not (listen stream)))))
        (ignore-errors (delete-file listen-testfile-name))))))

(with-test (:name :bug-395)
  (let ((v (make-array 5 :fill-pointer 0 :element-type 'standard-char)))
    (format v "foo")
    (assert (equal (coerce "foo" 'base-string) v))))

;;; Circa 1.0.27.18, echo-streams were changed somewhat, so that
;;; unread-char on an echo-stream propagated the character down to the
;;; echo-stream's input stream.  (All other implementations but CMUCL
;;; seemed to do this).  The most useful argument for this behavior
;;; involves cases where an input operation on an echo-stream finishes
;;; up by unreading a delimiter, and the user wants to proceed to use the
;;; underlying stream, e.g.,
(with-test (:name (echo-stream unread-char))
  (assert (equal
           (with-input-from-string (in "foo\"bar\"")
             (with-open-stream (out (make-broadcast-stream))
               (with-open-stream (echo (make-echo-stream in out))
                 (read echo)))
             (read in))
           ;; Before ca 1.0.27.18, the implicit UNREAD-CHAR at the end of
           ;; the first READ wouldn't get back to IN, so the second READ
           ;; returned BAR, not "BAR" (and then subsequent reads would
           ;; lose).
           "bar")))

;; WITH-INPUT-FROM-STRING would multiply evaluate the :END argument,
;; and so previously this returned the symbol A, not ABC.
(with-test (:name (with-input-from-string :end :once-only))
  (assert (eq (let ((s "ABCDEFG")
                    (i 5))
                (symbol-macrolet ((ptr (decf i 2)))
                  (with-input-from-string (stream s :end ptr)
                    (read stream))))
              'abc)))

(flet ((test (form)
         ;; CHECKED-COMPILE avoids the compile-time warning when
         ;; loading this file.
         (let ((fun (checked-compile `(lambda () ,form) :allow-warnings t)))
           (assert-error (funcall fun) type-error))))

  (with-test (:name (read-sequence sequence type-error))
    (test `(read-sequence 1 (make-string-input-stream "foo"))))

  (with-test (:name (write-sequence sequence type-error))
    (test `(write-sequence 1 (make-string-output-stream)))))

(with-test (:name :fill-pointer-stream-charpos)
  (let ((string (make-array 3  :initial-contents (format nil "~%ab")
                               :element-type 'character :fill-pointer 1)))
    (with-output-to-string (stream string)
      (fresh-line stream))
    (assert (equal string (string #\Newline)))))

(with-test (:name (:fill-pointer-stream-charpos :displaced))
  (let* ((displaced (format nil "~%abc"))
         (string (make-array 3 :displaced-to displaced
                               :displaced-index-offset 1
                               :element-type (array-element-type displaced)
                               :fill-pointer 0)))
    (with-output-to-string (stream string)
      (fresh-line stream))
    (assert (equal string ""))))

#+sb-unicode
(with-test (:name (:write-char-base-char-stream-reject-non-base-char))
  (assert-error
   (write-char (code-char 1000)
               (make-string-output-stream :element-type 'base-char))))

#+sb-unicode
(with-test (:name (:write-string-base-char-stream-reject-non-base-char))
  (assert-error
   (write-string (make-string 1 :initial-element (code-char 1000))
                 (make-string-output-stream :element-type 'base-char))))

#+sb-unicode
(with-test (:name (:default-char-stream-resets))
  (sb-impl::%with-output-to-string (s)
    (dotimes (i 2)
      (write-char (code-char 1000) s)
      (assert (equal (type-of (get-output-stream-string s))
                     '(simple-array character (1))))
      (write-char #\a s)
      ;; result type reverts back to simple-base-string after get-output-stream-string
      (assert (equal (type-of (get-output-stream-string s))
                     '(simple-base-string 1))))))

(with-test (:name :with-input-from-string-nowarn)
  (checked-compile '(lambda ()
                     (with-input-from-string (s "muffin")))))

(with-test (:name :with-input-from-string-declarations)
  (checked-compile-and-assert
      ()
      `(lambda (string)
         (with-input-from-string (x string)
           (declare (optimize safety))
           (read-char x)))
    (("a") #\a)))

(defun input-from-dynamic-extent-stream ()
  (handler-case (with-input-from-string (stream "#w") (read stream nil nil))
    (error (condition)
      (format nil "~A" condition))))
(compile 'input-from-dynamic-extent-stream)
(with-test (:name :with-input-from-string-signal-stream-error)
  (assert (search "unavailable" (input-from-dynamic-extent-stream))))

(with-test (:name :closeable-broadcast-stream)
  (let ((b (make-broadcast-stream)))
   (close b)
   (assert (not (open-stream-p b)))
   (assert-error (write-string "test" b))))

(defvar *some-stream*)
(with-test (:name :closeable-synonym-stream)
  (let ((*some-stream* (make-string-input-stream "hola")))
    (let ((syn (make-synonym-stream '*some-stream*)))
      (assert (eql (read-char syn) #\h))
      (close syn)
      (assert (not (open-stream-p syn)))
      (assert-error (read-char syn))
      (close syn) ; no error
      (assert (eql (read-char *some-stream*) #\o)))))

(with-test (:name :read-sequence-displaced-offset
            :skipped-on :win32)
  (let* ((d (make-array 3 :element-type '(unsigned-byte 8)
                          :initial-element 1))
         (x (make-array 1 :element-type '(unsigned-byte 8) :displaced-to d
                          :displaced-index-offset 1)))
    (with-open-file (s "/dev/zero" :element-type '(unsigned-byte 8))
      (assert (= (read-sequence x s) 1))
      (assert (equalp d #(1 0 1)))
      (assert (equalp x #(0))))))
