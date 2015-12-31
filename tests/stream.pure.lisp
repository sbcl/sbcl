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

(in-package :cl-user)

;;; Until sbcl-0.6.11.31, we didn't have an N-BIN method for
;;; CONCATENATED-STREAM, so stuff like this would fail.
(let ((stream (make-concatenated-stream (make-string-input-stream "Demo")))
      (buffer (make-string 4)))
  (read-sequence buffer stream))
;;; test for the new N-BIN method doing what it's supposed to
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
       (return)))))

;;; Entomotomy PEEK-CHAR-WRONGLY-ECHOS-TO-ECHO-STREAM bug, fixed by
;;; MRD patch sbcl-devel 2002-11-02 merged ca. sbcl-0.7.9.32...
(assert (string=
         (with-output-to-string (out)
           (peek-char #\]
                      (make-echo-stream
                       (make-string-input-stream "ab cd e df s]") out)))
         ;; (Before the fix, the result had a trailing #\] in it.)
         "ab cd e df s"))
;;; ...and a missing wrinkle in the original patch, dealing with
;;; PEEK-CHAR/UNREAD-CHAR on ECHO-STREAMs, fixed by MRD patch
;;; sbcl-devel 2002-11-18, merged ca. sbcl-0.7.9.66
(assert (string=
         (let* ((in-stream (make-string-input-stream "abc"))
                (out-stream (make-string-output-stream))
                (echo-stream (make-echo-stream in-stream out-stream)))
           (unread-char (read-char echo-stream) echo-stream)
           (peek-char #\a echo-stream)
           (get-output-stream-string out-stream))
         ;; (Before the fix, the LET* expression just signalled an error.)
         "a"))
;;; ... and yet, a little over 6 years on, echo-streams were still
;;; broken when a read-char followed the unread/peek sequence.  Do
;;; people not actually use echo-streams?  RMK, 2009-04-02.
(assert (string=
         (let* ((in-stream (make-string-input-stream "abc"))
                (out-stream (make-string-output-stream))
                (echo-stream (make-echo-stream in-stream out-stream)))
           (unread-char (read-char echo-stream) echo-stream)
           (peek-char nil echo-stream)
           (read-char echo-stream)
           (get-output-stream-string out-stream))
         ;; before ca. 1.0.27.18, the LET* returned "aa"
         "a"))

;;; Reported by Fredrik Sandstrom to sbcl-devel 2005-05-17 ("Bug in
;;; peek-char"):
;;; Description: In (peek-char nil s nil foo), if foo happens to be
;;; the same character that peek-char returns, the character is
;;; removed from the input stream, as if read by read-char.
(assert (equal (with-input-from-string (s "123")
                 (list (peek-char nil s nil #\1) (read-char s) (read-char s)))
               '(#\1 #\1 #\2)))

;;; ... and verify that the fix does not break echo streams
(assert (string= (let ((out (make-string-output-stream)))
                   (with-open-stream (s (make-echo-stream
                                         (make-string-input-stream "123")
                                         out))
                     (format s "=>~{~A~}"
                             (list (peek-char nil s nil #\1)
                                   (read-char s)
                                   (read-char s)))
                     (get-output-stream-string out)))
                 "12=>112"))

;;; 0.7.12 doesn't advance current stream in concatenated streams
;;; correctly when searching a stream for a char to read.
(with-input-from-string (p "")
  (with-input-from-string (q "foo")
    (let* ((r (make-concatenated-stream p q)))
      (peek-char nil r))))

;;; 0.7.14 and previous SBCLs don't have a working INTERACTIVE-STREAM-P
;;; because it called UNIX-ISATTY, which wasn't defined.
(with-input-from-string (s "a non-interactive stream")
  (assert (not (interactive-stream-p s))))
;;; KLUDGE: Unfortunately it's hard to find a reliably interactive
;;; stream to test, since it's reasonable for these tests to be run
;;; from a script, conceivably even as something like a cron job.
;;; Ideas?
#+nil (assert (eq (interactive-stream-p *terminal-io*) t))

;;; MAKE-STRING-INPUT-STREAM
;;;
;;; * Observe FILE-POSITION :START and :END, and allow setting of
;;;   FILE-POSITION beyond the end of string, signalling END-OF-FILE only
;;;   on read.
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
  (multiple-value-bind (val cond) (ignore-errors (file-position stream -1))
    (assert (null val))
    (assert (typep cond 'error)))
  (multiple-value-bind (val cond) (ignore-errors (read-char stream))
    (assert (null val))
    (assert (typep cond 'end-of-file))))

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
  (multiple-value-bind (val cond) (ignore-errors (file-position stream -1))
    (assert (null val))
    (assert (typep cond 'error)))
  (princ "!!" stream)
  (assert (equal "0b2d!!" (get-output-stream-string stream))))

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
    (multiple-value-bind (val cond) (ignore-errors (file-position stream -1))
      (assert (null val))
      (assert (typep cond 'error)))
    (princ "!!" stream)
    (assert (equal "0b2d!!" str))))

;;; MAKE-STRING-OUTPUT-STREAM and WITH-OUTPUT-TO-STRING take an
;;; :ELEMENT-TYPE keyword argument
(macrolet ((frob (element-type-form)
             `(progn
                (let ((s (with-output-to-string
                           (s nil ,@(when element-type-form
                                      `(:element-type ,element-type-form))))))
                  (assert (typep s '(simple-array ,(if element-type-form
                                                       (eval element-type-form)
                                                       'character)
                                                  (0)))))
                (get-output-stream-string
                 (make-string-output-stream
                  ,@(when element-type-form
                      `(:element-type ,element-type-form)))))))
  (frob nil)
  (frob 'character)
  (frob 'base-char)
  (frob 'nil))

(with-test (:name :make-string-output-stream-et-bogosity)
  (assert-error (make-string-output-stream :element-type 'real)))

(with-open-file (s #-win32 "/dev/null" #+win32 "nul" :element-type '(signed-byte 48))
  (assert (eq :eof (read-byte s nil :eof))))

(let* ((is (make-string-input-stream "foo"))
       (os (make-string-output-stream))
       (s (make-echo-stream is os))
       (sequence (copy-seq "abcdef")))
  (assert (= (read-sequence sequence s) 3))
  (assert (string= sequence "foodef"))
  (assert (string= (get-output-stream-string os) "foo")))

(let* ((is (make-string-input-stream "foo"))
       (os (make-string-output-stream))
       (s (make-echo-stream is os))
       (sequence (copy-seq "abcdef")))
  (assert (char= #\f (read-char s)))
  (assert (= (read-sequence sequence s) 2))
  (assert (string= sequence "oocdef"))
  (assert (string= (get-output-stream-string os) "foo")))

(let* ((is (make-string-input-stream "foo"))
       (os (make-string-output-stream))
       (s (make-echo-stream is os))
       (sequence (copy-seq "abcdef")))
  (assert (char= #\f (read-char s)))
  (unread-char #\f s)
  (assert (= (read-sequence sequence s) 3))
  (assert (string= sequence "foodef"))
  (assert (string= (get-output-stream-string os) "foo")))

(with-standard-io-syntax
  (open #-win32 "/dev/null" #+win32 "nul" ))

;;; PEEK-CHAR T uses whitespace[2]
(let ((*readtable* (copy-readtable)))
  (assert (char= (peek-char t (make-string-input-stream " a")) #\a))
  (set-syntax-from-char #\Space #\a)
  (assert (char= (peek-char t (make-string-input-stream " a")) #\Space)))
(with-test (:name :whitespace[2]p-is-type-safe)
  (assert-error (sb-impl::whitespace[2]p :potato)))

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
(with-test (:name (stream :listen-vs-select) :fails-on :win32)
  (let ((listen-testfile-name "stream.impure.lisp.testqfile")
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
         "bar"))

;; WITH-INPUT-FROM-STRING would multiply evaluate the :END argument,
;; and so previously this returned the symbol A, not ABC.
(with-test (:name :with-input-from-string-end-once-only)
  (assert (eq (let ((s "ABCDEFG")
                    (i 5))
                (symbol-macrolet ((ptr (decf i 2)))
                  (with-input-from-string (stream s :end ptr)
                    (read stream))))
              'abc)))

(with-test (:name (read-sequence sequence type-error))
  (assert-error (read-sequence 1 (make-string-input-stream "foo"))
                type-error))

(with-test (:name (write-sequence sequence type-error))
  (assert-error (write-sequence 1 (make-string-output-stream))
                type-error))
