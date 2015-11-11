;;;; This file is for testing external-format functionality, using
;;;; test machinery which might have side effects (e.g.  executing
;;;; DEFUN, writing files).  Note that the tests here reach into
;;;; unexported functionality, and should not be used as a guide for
;;;; users.

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

(defmacro do-external-formats ((xf) &body body)
  (let ((nxf (gensym)))
    `(loop for ,nxf being the hash-values of sb-impl::*external-formats*
        do (let ((,xf (first (sb-impl::ef-names ,nxf))))
             ,@body))))

(defvar *test-path* "external-format-test.tmp")

(with-test (:name :end-of-file)
  (do-external-formats (xf)
    (with-open-file (s #-win32 "/dev/null" #+win32 "nul" :direction :input :external-format xf)
      (assert (eq (read-char s nil s) s)))))

;;; Test standard character read-write equivalency over all external formats.
(macrolet
    ((frob ()
       (let ((tests nil))
         (do-external-formats (xf)
           (pushnew `(with-test (:name (:standard-character :read-write-equivalency ,xf))
                       (let ((standard-characters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!$\"'(),_-./:;?+<=>#%&*@[\\]{|}`^~"))
                         (with-open-file (s *test-path* :direction :output
                                            :if-exists :supersede :external-format ,xf)
                           (loop for character across standard-characters
                                 do (write-char character s)))
                         (with-open-file (s *test-path* :direction :input
                                            :external-format ,xf)
                           (loop for character across standard-characters
                                 do (let ((got (read-char s)))
                                      (unless (eql character got)
                                        (error "wanted ~S, got ~S" character got)))))))
                    tests :key #'cadr :test #'equal))
         `(progn ,@tests))))
  (frob))

(delete-file *test-path*)
#-sb-unicode
(progn
  (test-util:report-test-status)
  (sb-ext:exit :code 104))

;;; Test UTF-8 writing and reading of 1, 2, 3 and 4 octet characters with
;;; all possible offsets. Tests for buffer edge bugs. fd-stream buffers are
;;; 4096 wide.
(dotimes (width-1 4)
  (let ((character (code-char (elt '(1 #x81 #x801 #x10001) width-1))))
    (dotimes (offset (+ width-1 1))
      (with-open-file (s *test-path* :direction :output
                       :if-exists :supersede :external-format :utf-8)
        (dotimes (n offset)
          (write-char #\a s))
        (dotimes (n (+ 4 sb-impl::+bytes-per-buffer+))
          (write-char character s)))
      (with-open-file (s *test-path* :direction :input
                       :external-format :utf-8)
        (dotimes (n offset)
          (assert (eql (read-char s) #\a)))
        (dotimes (n (+ 4 sb-impl::+bytes-per-buffer+))
          (let ((got (read-char s)))
            (unless (eql got character)
              (error "wanted ~S, got ~S (~S)" character got n))))
        (assert (eql (read-char s nil s) s))))))

;;; Test character decode restarts.
(with-open-file (s *test-path* :direction :output
                 :if-exists :supersede :element-type '(unsigned-byte 8))
  (write-byte 65 s)
  (write-byte 66 s)
  (write-byte #xe0 s)
  (write-byte 67 s))
(with-open-file (s *test-path* :direction :input
                 :external-format :utf-8)
  (let ((count 0))
    (handler-bind
        ((sb-int:character-decoding-error #'(lambda (decoding-error)
                                              (declare (ignore decoding-error))
                                              (when (> (incf count) 1)
                                                (error "too many errors"))
                                              (invoke-restart
                                               'sb-int:attempt-resync))))
      (assert (equal (read-line s nil s) "ABC"))
      (assert (equal (read-line s nil s) s)))))
(with-open-file (s *test-path* :direction :input
                 :external-format :utf-8)
  (let ((count 0))
    (handler-bind
        ((sb-int:character-decoding-error #'(lambda (decoding-error)
                                              (declare (ignore decoding-error))
                                              (when (> (incf count) 1)
                                                (error "too many errors"))
                                              (invoke-restart
                                               'sb-int:force-end-of-file))))
      (assert (equal (read-line s nil s) "AB"))
      (setf count 0)
      (assert (equal (read-line s nil s) s)))))

;;; And again with more data to account for buffering (this was briefly)
;;; broken in early 0.9.6.
(with-open-file (s *test-path* :direction :output
                 :if-exists :supersede :element-type '(unsigned-byte 8))
  (let ((a (make-array 50
                       :element-type '(unsigned-byte 64)
                       :initial-contents (map 'list #'char-code
                                              "1234567890123456789012345678901234567890123456789."))))
    (setf (aref a 49) (char-code #\Newline))
    (dotimes (i 40)
      (write-sequence a s))
    (write-byte #xe0 s)
    (dotimes (i 40)
      (write-sequence a s))))
(with-test (:name (:character-decode-large :attempt-resync))
  (with-open-file (s *test-path* :direction :input
                     :external-format :utf-8)
    (let ((count 0))
      (handler-bind
          ((sb-int:character-decoding-error (lambda (decoding-error)
                                              (declare (ignore decoding-error))
                                              (when (> (incf count) 1)
                                                (error "too many errors"))
                                              (invoke-restart
                                               'sb-int:attempt-resync)))
           ;; The failure mode is an infinite loop, add a timeout to
           ;; detetct it.
           (sb-ext:timeout (lambda () (error "Timeout"))))
        (sb-ext:with-timeout 5
          (dotimes (i 80)
            (assert (equal (read-line s nil s)
                           "1234567890123456789012345678901234567890123456789"))))))))

(with-test (:name (:character-decode-large :force-end-of-file))
  (with-open-file (s *test-path* :direction :input
                     :external-format :utf-8)
    (let ((count 0))
      (handler-bind
          ((sb-int:character-decoding-error (lambda (decoding-error)
                                              (declare (ignore decoding-error))
                                              (when (> (incf count) 1)
                                                (error "too many errors"))
                                              (invoke-restart
                                               'sb-int:force-end-of-file)))
           ;; The failure mode is an infinite loop, add a timeout to detetct it.
           (sb-ext:timeout (lambda () (error "Timeout"))))
        (sb-ext:with-timeout 5
          (dotimes (i 40)
            (assert (equal (read-line s nil s)
                           "1234567890123456789012345678901234567890123456789")))
          (setf count 0)
          (assert (equal (read-line s nil s) s)))))))

;;; Test character encode restarts.
(with-open-file (s *test-path* :direction :output
                 :if-exists :supersede :external-format :latin-1)
  (handler-bind
      ((sb-int:character-encoding-error #'(lambda (encoding-error)
                                            (declare (ignore encoding-error))
                                            (invoke-restart
                                             'sb-impl::output-nothing))))
    (write-char #\A s)
    (write-char #\B s)
    (write-char (code-char 322) s)
    (write-char #\C s)))
(with-open-file (s *test-path* :direction :input
                 :external-format :latin-1)
  (assert (equal (read-line s nil s) "ABC"))
  (assert (equal (read-line s nil s) s)))

(with-open-file (s *test-path* :direction :output
                 :if-exists :supersede :external-format :latin-1)
  (handler-bind
      ((sb-int:character-encoding-error #'(lambda (encoding-error)
                                            (declare (ignore encoding-error))
                                            (invoke-restart
                                             'sb-impl::output-nothing))))
    (let ((string (make-array 4 :element-type 'character
                              :initial-contents `(#\A #\B ,(code-char 322)
                                                      #\C))))
      (write-string string s))))
(with-open-file (s *test-path* :direction :input
                 :external-format :latin-1)
  (assert (equal (read-line s nil s) "ABC"))
  (assert (equal (read-line s nil s) s)))

;;; Test skipping character-decode-errors in comments.
(let ((s (open "external-format-test.lisp" :direction :output
               :if-exists :supersede :external-format :latin-1)))
  (unwind-protect
       (progn
         (write-string ";;; ABCD" s)
         (write-char (code-char 233) s)
         (terpri s)
         (close s)
         (let ((*error-output* (make-broadcast-stream)))
           (compile-file "external-format-test.lisp"
                         :external-format :utf-8 :verbose nil)))
    (delete-file s)
    (let ((p (probe-file (compile-file-pathname "external-format-test.lisp"))))
      (when p
        (delete-file p)))))


;;;; KOI8-R external format
(with-open-file (s *test-path* :direction :output
                 :if-exists :supersede :external-format :koi8-r)
  (write-char (code-char #xB0) s)
  (assert (eq
           (handler-case
               (progn
                 (write-char (code-char #xBAAD) s)
                 :bad)
             (sb-int:character-encoding-error ()
               :good))
           :good)))
(with-open-file (s *test-path* :direction :input
                 :element-type '(unsigned-byte 8))
  (let ((byte (read-byte s)))
    (assert (= (eval byte) #x9C))))
(with-open-file (s *test-path* :direction :input
                 :external-format :koi8-r)
  (let ((char (read-char s)))
    (assert (= (char-code (eval char)) #xB0))))
(delete-file *test-path*)

(let* ((koi8-r-codes (coerce '(240 210 201 215 197 212 33) '(vector (unsigned-byte 8))))
       (uni-codes #(1055 1088 1080 1074 1077 1090 33))

       (string (octets-to-string koi8-r-codes :external-format :koi8-r))
       (uni-decoded (map 'vector #'char-code string)))
  (declare (ignore uni-decoded))
  (assert (equalp (map 'vector #'char-code (octets-to-string koi8-r-codes :external-format :koi8-r))
                  uni-codes))
  (assert (equalp (string-to-octets (map 'string #'code-char uni-codes) :external-format :koi8-r)
                  koi8-r-codes)))

;;; tests of FILE-STRING-LENGTH
(let ((standard-characters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!$\"'(),_-./:;?+<=>#%&*@[\\]{|}`^~"))
  (do-external-formats (xf)
    (with-open-file (s *test-path* :direction :output
                       :external-format xf)
      (loop for x across standard-characters
            for position = (file-position s)
            for char-length = (file-string-length s x)
            do (write-char x s)
            do (assert (= (file-position s) (+ position char-length))))
      (let ((position (file-position s))
            (string-length (file-string-length s standard-characters)))
        (write-string standard-characters s)
        (assert (= (file-position s) (+ position string-length)))))
    (delete-file *test-path*)))

(let ((char-codes '(0 1 255 256 511 512 1023 1024 2047 2048 4095 4096
                    8191 8192 16383 16384 32767 32768 65535 65536 131071
                    131072 262143 262144)))
  (with-open-file (s *test-path* :direction :output
                     :external-format :utf-8)
    (dolist (code char-codes)
      (let* ((char (code-char code))
             (position (file-position s))
             (char-length (file-string-length s char)))
        (write-char char s)
        (assert (= (file-position s) (+ position char-length)))))
    (let* ((string (map 'string #'code-char char-codes))
           (position (file-position s))
           (string-length (file-string-length s string)))
      (write-string string s)
      (assert (= (file-position s) (+ position string-length))))))


;;; See sbcl-devel "Subject: Bug in FILE-POSITION on UTF-8-encoded files"
;;; by Lutz Euler on 2006-03-05 for more details.
(with-test (:name (:file-position :utf-8))
  (let ((path *test-path*))
    (with-open-file (s path
                       :direction :output
                       :if-exists :supersede
                       :element-type '(unsigned-byte 8))
      ;; Write #\*, encoded in UTF-8, to the file.
      (write-byte 42 s)
      ;; Append #\adiaeresis, encoded in UTF-8, to the file.
      (write-sequence '(195 164) s))
    (with-open-file (s path :external-format :utf-8)
      (read-char s)
      (let ((pos (file-position s))
            (char (read-char s)))
        #+nil
        (format t "read character with code ~a successfully from file position ~a~%"
                (char-code char) pos)
        (file-position s pos)
        #+nil
        (format t "set file position back to ~a, trying to read-char again~%" pos)
        (let ((new-char (read-char s)))
          (assert (char= char new-char)))))
    (values)))
(delete-file *test-path*)

;;; We used to call STREAM-EXTERNAL-FORMAT on the stream in the error
;;; when printing a coding error, but that didn't work if the stream
;;; was closed by the time the error was printed.  See sbcl-devel
;;; "Subject: Printing coding errors for closed streams" by Zach Beane
;;; on 2008-10-16 for more info.
(with-test (:name (:character-coding-error-stream-external-format))
  (flet ((first-file-character ()
           (with-open-file (stream *test-path* :external-format :utf-8)
             (read-char stream))))
    (with-open-file (stream *test-path*
                            :direction :output
                            :if-exists :supersede
                            :element-type '(unsigned-byte 8))
      (write-byte 192 stream))
    (princ-to-string (nth-value 1 (ignore-errors (first-file-character))))))
(delete-file *test-path*)

;;; External format support in SB-ALIEN

(with-test (:name (:sb-alien :vanilla))
  (define-alien-routine (#-win32 "strdup" #+win32 "_strdup" strdup)
      c-string
    (str c-string))
  (assert (equal "foo" (strdup "foo"))))

(with-test (:name (:sb-alien :utf-8 :utf-8))
  (define-alien-routine (#-win32 "strdup" #+win32 "_strdup" strdup)
      (c-string :external-format :utf-8)
    (str (c-string :external-format :utf-8)))
  (assert (equal "foo" (strdup "foo"))))

(with-test (:name (:sb-alien :latin-1 :utf-8))
  (define-alien-routine (#-win32 "strdup" #+win32 "_strdup" strdup)
      (c-string :external-format :latin-1)
    (str (c-string :external-format :utf-8)))
  (assert (= (length (strdup (string (code-char 246))))
             2)))

(with-test (:name (:sb-alien :utf-8 :latin-1))
  (define-alien-routine (#-win32 "strdup" #+win32 "_strdup" strdup)
      (c-string :external-format :utf-8)
    (str (c-string :external-format :latin-1)))
  (assert (equal (string (code-char 228))
                 (strdup (concatenate 'string
                                      (list (code-char 195))
                                      (list (code-char 164)))))))

(with-test (:name (:sb-alien :ebcdic :ebcdic))
  (define-alien-routine (#-win32 "strdup" #+win32 "_strdup" strdup)
      (c-string :external-format :ebcdic-us)
    (str (c-string :external-format :ebcdic-us)))
  (assert (equal "foo" (strdup "foo"))))

(with-test (:name (:sb-alien :latin-1 :ebcdic))
  (define-alien-routine (#-win32 "strdup" #+win32 "_strdup" strdup)
      (c-string :external-format :latin-1)
    (str (c-string :external-format :ebcdic-us)))
  (assert (not (equal "foo" (strdup "foo")))))

(with-test (:name (:sb-alien :simple-base-string))
  (define-alien-routine (#-win32 "strdup" #+win32 "_strdup" strdup)
      (c-string :external-format :ebcdic-us
                :element-type base-char)
    (str (c-string :external-format :ebcdic-us)))
  (assert (typep (strdup "foo") 'simple-base-string)))

(with-test (:name (:input-replacement :at-end-of-file))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (handler-bind ((sb-int:character-decoding-error
                    (lambda (c)
                      (declare (ignore c))
                      (invoke-restart 'sb-impl::input-replacement #\?))))
      (with-open-file (s *test-path* :external-format :utf-8)
        (cond
          ((char= (read-char s) #\?)
           (assert (or (= i (char-code #\?)) (> i 127))))
          (t (assert (and (not (= i (char-code #\?))) (< i 128)))))))))

(with-test (:name (:unibyte-invalid-codepoints :cp857))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format :cp857)
      (handler-case (read-char s)
        (error () (assert (member i '(#xd5 #xe7 #xf2))))
        (:no-error (char) char (assert (not (member i '(#xd5 #xe7 #xf2)))))))))
(delete-file *test-path*)

(with-test (:name (:unibyte-input-replacement :cp857))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:cp857 :replacement #\?))
      (let ((char (read-char s)))
        (cond
          ((eq char #\?)
           (assert (member i `(,(char-code #\?) #xd5 #xe7 #xf2))))
          (t (assert (not (member i `(,(char-code #\?) #xd5 #xe7 #xf2))))))))))
(delete-file *test-path*)

(with-test (:name (:unibyte-output-replacement :cp857))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:cp857 :replacement #\?))
    (dotimes (i 256)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:cp857))
    (let ((string (make-string 256)))
      (read-sequence string s)
      (dotimes (i 128)
        (assert (= (char-code (char string i)) i)))
      (assert (= 38 (count #\? string :start 128))))))
(delete-file *test-path*)

(with-test (:name (:unibyte-input-replacement :ascii))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:ascii :replacement #\?))
      (let ((char (read-char s)))
        (cond
          ((eq char #\?)
           (assert (or (= i (char-code #\?)) (> i 127))))
          (t (assert (and (< i 128) (not (= i (char-code #\?)))))))))))
(delete-file *test-path*)

(with-test (:name (:unibyte-output-replacement :ascii))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:ascii :replacement #\?))
    (dotimes (i 256)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:ascii))
    (let ((string (make-string 256)))
      (read-sequence string s)
      (dotimes (i 128)
        (assert (= (char-code (char string i)) i)))
      (assert (= 128 (count #\? string :start 128))))))
(delete-file *test-path*)

(with-test (:name (:unibyte-input-replacement :latin-1))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:latin-1 :replacement #\?))
      (let ((char (read-char s)))
        (assert (= (char-code char) i))))))
(delete-file *test-path*)

(with-test (:name (:unibyte-output-replacement :latin-1))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:latin-1 :replacement #\?))
    (dotimes (i 257)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:latin-1))
    (let ((string (make-string 257)))
      (read-sequence string s)
      (dotimes (i 256)
        (assert (= (char-code (char string i)) i)))
      (assert (char= #\? (char string 256))))))
(delete-file *test-path*)

;;; latin-2 tests
(with-test (:name (:unibyte-input-replacement :latin-2))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:latin-2 :replacement #\?))
      (let ((char (read-char s)))
        (cond
          ((< i #xa1) (assert (= (char-code char) i)))
          ;; FIXME: more tests
          )))))
(delete-file *test-path*)

(with-test (:name (:unibyte-output-replacement :latin-2))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:latin-2 :replacement #\?))
    (dotimes (i 256)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:latin-2))
    (let ((string (make-string 256)))
      (read-sequence string s)
      (dotimes (i #xa1)
        (assert (= (char-code (char string i)) i)))
      (assert (= 57 (count #\? string :start #xa1))))))
(delete-file *test-path*)

;;; latin-3 tests
(with-test (:name (:unibyte-input-replacement :latin-3))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:latin-3 :replacement #\?))
      (let ((char (read-char s)))
        (cond
          ((eq char #\?)
           (assert #1=(or (= i (char-code #\?))
                          (member i '(#xa5 #xae #xbe #xc3 #xd0 #xe3 #xf0)))))
          (t (assert (not #1#))))))))
(delete-file *test-path*)

(with-test (:name (:unibyte-output-replacement :latin-3))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:latin-3 :replacement #\?))
    (dotimes (i 256)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:latin-3))
    (let ((string (make-string 256)))
      (read-sequence string s)
      (dotimes (i #xa1)
        (assert (= (char-code (char string i)) i)))
      (assert (= 35 (count #\? string :start #xa1))))))
(delete-file *test-path*)

;;; latin-4 tests
(with-test (:name (:unibyte-input-replacement :latin-4))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:latin-4 :replacement #\?))
      (let ((char (read-char s)))
        (cond
          ((< i #xa1) (assert (= (char-code char) i)))
          ;; FIXME: more tests
          )))))
(delete-file *test-path*)

(with-test (:name (:unibyte-output-replacement :latin-4))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:latin-4 :replacement #\?))
    (dotimes (i 256)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:latin-4))
    (let ((string (make-string 256)))
      (read-sequence string s)
      (dotimes (i #xa1)
        (assert (= (char-code (char string i)) i)))
      (assert (= 50 (count #\? string :start #xa1))))))
(delete-file *test-path*)

;;; iso-8859-5 tests
(with-test (:name (:unibyte-input-replacement :iso-8859-5))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:iso-8859-5 :replacement #\?))
      (let ((char (read-char s)))
        (cond
          ((= (char-code char) i)
           (assert (or (< i #xa1) (= i #xad))))
          (t (assert (and (>= i #xa1) (/= i #xad)))))))))
(delete-file *test-path*)

(with-test (:name (:unibyte-output-replacement :iso-8859-5))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:iso-8859-5 :replacement #\?))
    (dotimes (i 256)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:iso-8859-5))
    (let ((string (make-string 256)))
      (read-sequence string s)
      (dotimes (i #xa1)
        (assert (= (char-code (char string i)) i)))
      (assert (= 93 (count #\? string :start #xa1))))))
(delete-file *test-path*)

;;; iso-8859-6 tests
(with-test (:name (:unibyte-input-replacement :iso-8859-6))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:iso-8859-6 :replacement #\?))
      (let ((char (read-char s)))
        (cond
          ((eq char #\?)
           (assert #1=(or (= i (char-code #\?))
                          (<= #xa1 i #xa3) (<= #xa5 i #xab) (<= #xae i #xba)
                          (<= #xbc i #xbe) (= i #xc0) (<= #xdb i #xdf)
                          (<= #xf3 i))))
          (t (assert (not #1#))))))))
(delete-file *test-path*)

(with-test (:name (:unibyte-output-replacement :iso-8859-6))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:iso-8859-6 :replacement #\?))
    (dotimes (i 256)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:iso-8859-6))
    (let ((string (make-string 256)))
      (read-sequence string s)
      (dotimes (i #xa1)
        (assert (= (char-code (char string i)) i)))
      (assert (= 93 (count #\? string :start #xa1))))))
(delete-file *test-path*)

;;; iso-8859-7 tests
(with-test (:name (:unibyte-input-replacement :iso-8859-7))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:iso-8859-7 :replacement #\?))
      (let ((char (read-char s)))
        (cond
          ((eq char #\?)
           (assert #1=(or (= i (char-code #\?))
                          (member i '(#xa4 #xa5 #xaa #xae #xd2 #xff)))))
          (t (assert (not #1#))))))))
(delete-file *test-path*)

(with-test (:name (:unibyte-output-replacement :iso-8859-7))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:iso-8859-7 :replacement #\?))
    (dotimes (i 256)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:iso-8859-7))
    (let ((string (make-string 256)))
      (read-sequence string s)
      (dotimes (i #xa1)
        (assert (= (char-code (char string i)) i)))
      (assert (= 80 (count #\? string :start #xa1))))))
(delete-file *test-path*)

;;; iso-8859-8 tests
(with-test (:name (:unibyte-input-replacement :iso-8859-8))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:iso-8859-8 :replacement #\?))
      (let ((char (read-char s)))
        (cond
          ((eq char #\?)
           (assert #1=(or (= i (char-code #\?))
                          (= i #xa1) (<= #xbf i #xde) (>= i #xfb))))
          (t (assert (not  #1#))))))))
(delete-file *test-path*)

(with-test (:name (:unibyte-output-replacement :iso-8859-8))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:iso-8859-8 :replacement #\?))
    (dotimes (i 256)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:iso-8859-8))
    (let ((string (make-string 256)))
      (read-sequence string s)
      (dotimes (i #xa1)
        (assert (= (char-code (char string i)) i)))
      (assert (= 67 (count #\? string :start #xa1))))))
(delete-file *test-path*)

;;; latin-5 tests
(with-test (:name (:unibyte-input-replacement :latin-5))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:latin-5 :replacement #\?))
      (let ((char (read-char s)))
        (assert (or (and (= (char-code char) i)
                         (not (member i '(#xd0 #xdd #xde #xf0 #xfd #xfe))))
                    (and (member i '(#xd0 #xdd #xde #xf0 #xfd #xfe))
                         (not (char= char #\?)))))))))
(delete-file *test-path*)

(with-test (:name (:unibyte-output-replacement :latin-5))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:latin-5 :replacement #\?))
    (dotimes (i 256)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:latin-5))
    (let ((string (make-string 256)))
      (read-sequence string s)
      (dotimes (i #xd0)
        (assert (= (char-code (char string i)) i)))
      (assert (= 6 (count #\? string :start #xd0))))))
(delete-file *test-path*)

;;; latin-6 tests
(with-test (:name (:unibyte-input-replacement :latin-6))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:latin-6 :replacement #\?))
      (let ((char (read-char s)))
        (assert (or (= (char-code char) i)
                    (and (<= #xa1 i #xff)
                         (not (char= char #\?)))))))))
(delete-file *test-path*)

(with-test (:name (:unibyte-output-replacement :latin-6))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:latin-6 :replacement #\?))
    (dotimes (i 256)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:latin-6))
    (let ((string (make-string 256)))
      (read-sequence string s)
      (dotimes (i #xa1)
        (assert (= (char-code (char string i)) i)))
      (assert (= 46 (count #\? string :start #xa1))))))
(delete-file *test-path*)

;;; iso-8859-11 tests
(with-test (:name (:unibyte-input-replacement :iso-8859-11))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:iso-8859-11 :replacement #\?))
      (let ((char (read-char s)))
        (cond
          ((eq char #\?)
           (assert (member i #1=`(,(char-code #\?) #xdb #xdc #xdd #xde #xfc #xfd #xfe #xff))))
          (t (assert (not (member i #1#)))))))))
(delete-file *test-path*)

(with-test (:name (:unibyte-output-replacement :iso-8859-11))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:iso-8859-11 :replacement #\?))
    (dotimes (i 256)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:iso-8859-11))
    (let ((string (make-string 256)))
      (read-sequence string s)
      (dotimes (i #xa1)
        (assert (= (char-code (char string i)) i)))
      (assert (= 95 (count #\? string :start #xa1))))))
(delete-file *test-path*)

;;; latin-7 tests
(with-test (:name (:unibyte-input-replacement :latin-7))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:latin-7 :replacement #\?))
      (let ((char (read-char s)))
        (assert (or (= (char-code char) i)
                    (and (<= #xa1 i #xff)
                         (not (char= char #\?)))))))))
(delete-file *test-path*)

(with-test (:name (:unibyte-output-replacement :latin-7))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:latin-7 :replacement #\?))
    (dotimes (i 256)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:latin-7))
    (let ((string (make-string 256)))
      (read-sequence string s)
      (dotimes (i #xa1)
        (assert (= (char-code (char string i)) i)))
      (dolist (i '(#xd8 #xc6 #xf8 #xe6))
        (assert (char/= (char string i) #\?)))
      (assert (= 52 (count #\? string :start #xa1))))))
(delete-file *test-path*)

;;; latin-8 tests
(with-test (:name (:unibyte-input-replacement :latin-8))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:latin-8 :replacement #\?))
      (let ((char (read-char s)))
        (assert (or (= (char-code char) i)
                    (and (<= #xa1 i #xfe)
                         (not (char= char #\?)))))))))
(delete-file *test-path*)

(with-test (:name (:unibyte-output-replacement :latin-8))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:latin-8 :replacement #\?))
    (dotimes (i 256)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:latin-8))
    (let ((string (make-string 256)))
      (read-sequence string s)
      (dotimes (i #xa1)
        (assert (= (char-code (char string i)) i)))
      (assert (= 31 (count #\? string :start #xa1))))))
(delete-file *test-path*)

;;; latin-9 tests
(with-test (:name (:unibyte-input-replacement :latin-9))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:latin-9 :replacement #\?))
      (let ((char (read-char s)))
        (assert (or (and (= (char-code char) i)
                         (not (member i '(#xa4 #xa6 #xa8 #xb4 #xb8 #xbc #xbd #xbe))))
                    (and (member i '(#xa4 #xa6 #xa8 #xb4 #xb8 #xbc #xbd #xbe))
                         (not (char= char #\?)))))))))
(delete-file *test-path*)

(with-test (:name (:unibyte-output-replacement :latin-9))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:latin-9 :replacement #\?))
    (dotimes (i 256)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:latin-9))
    (let ((string (make-string 256)))
      (read-sequence string s)
      (dotimes (i #xa4)
        (assert (= (char-code (char string i)) i)))
      (assert (= 8 (count #\? string :start #xa4))))))
(delete-file *test-path*)

;;; koi8-r tests
(with-test (:name (:unibyte-input-replacement :koi8-r))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:koi8-r :replacement #\?))
      (let ((char (read-char s)))
        (cond ((= (char-code char) i)
               (assert (< i 128)))
              (t (assert (> i 127))))))))
(delete-file *test-path*)

(with-test (:name (:unibyte-output-replacement :koi8-r))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:koi8-r :replacement #\?))
    (dotimes (i 256)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:koi8-r))
    (let ((string (make-string 256)))
      (read-sequence string s)
      (dotimes (i #x80)
        (assert (= (char-code (char string i)) i)))
      (assert (= 122 (count #\? string :start #x80))))))
(delete-file *test-path*)

;;; koi8-u tests
(with-test (:name (:unibyte-input-replacement :koi8-u))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:koi8-u :replacement #\?))
      (let ((char (read-char s)))
        (cond ((= (char-code char) i)
               (assert (< i 128)))
              (t (assert (> i 127))))))))
(delete-file *test-path*)

(with-test (:name (:unibyte-output-replacement :koi8-u))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:koi8-u :replacement #\?))
    (dotimes (i 256)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:koi8-u))
    (let ((string (make-string 256)))
      (read-sequence string s)
      (dotimes (i #x80)
        (assert (= (char-code (char string i)) i)))
      (assert (= 122 (count #\? string :start #x80))))))
(delete-file *test-path*)

;;; x-mac-cyrillic tests
(with-test (:name (:unibyte-input-replacement :x-mac-cyrillic))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:x-mac-cyrillic :replacement #\?))
      (let ((char (read-char s)))
        (cond ((= (char-code char) i)
               (assert (or (< i 128) (member i '(#xa2 #xa3 #xa9 #xb1 #xb5)))))
              (t (assert (and (> i 127)
                              (not (member i '(#xa2 #xa3 #xa9 #xb1 #xb5)))))))))))
(delete-file *test-path*)

(with-test (:name (:unibyte-output-replacement :x-mac-cyrillic))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:x-mac-cyrillic :replacement #\?))
    (dotimes (i 256)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:x-mac-cyrillic))
    (let ((string (make-string 256)))
      (read-sequence string s)
      (dotimes (i #x80)
        (assert (= (char-code (char string i)) i)))
      (assert (= 113 (count #\? string :start #x80))))))
(delete-file *test-path*)

;;; ucs-2 tests
(with-test (:name (:multibyte :ucs2le))
  (let* ((size 120)
         (array (map-into (make-array size :element-type '(unsigned-byte 16))
                          (lambda () (random #x10000)))))
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (dotimes (i size)
        (write-byte (ldb (byte 8 0) (aref array i)) s)
        (write-byte (ldb (byte 8 8) (aref array i)) s)))
    (with-open-file (s *test-path* :external-format :ucs2le)
      (let ((string (make-string size)))
        (read-sequence string s)
        (dotimes (i size)
          (assert (= (char-code (char string i)) (aref array i))))))))

(with-test (:name (:multibyte :ucs2be))
  (let* ((size 120)
         (array (map-into (make-array size :element-type '(unsigned-byte 16))
                          (lambda () (random #x10000)))))
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (dotimes (i size)
        (write-byte (ldb (byte 8 8) (aref array i)) s)
        (write-byte (ldb (byte 8 0) (aref array i)) s)))
    (with-open-file (s *test-path* :external-format :ucs2be)
      (let ((string (make-string size)))
        (read-sequence string s)
        (dotimes (i size)
          (assert (= (char-code (char string i)) (aref array i))))))))

(with-test (:name (:multibyte :output-replacement :ucs2le))
  (let* ((size 1200)
         (string (map-into (make-string size)
                           (lambda () (code-char (random #x10000))))))
    (setf (char string 0) (code-char #x10001)
          (char string (1- size)) (code-char #x10002))
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:ucs2le :replacement #\replacement_character))
      (write-string string s))
    (with-open-file (s *test-path* :external-format :ucs2le)
      (let ((new (make-string size)))
        (read-sequence new s)
        (assert (char= (char new 0) #\replacement_character))
        (assert (char= (char new (1- size)) #\replacement_character))
        (assert (string= string new :start1 1 :start2 1 :end1 (1- size) :end2 (1- size)))))))

(with-test (:name (:multibyte :output-replacement :ucs2be))
  (let* ((size 1200)
         (string (map-into (make-string size)
                           (lambda () (code-char (random #x10000))))))
    (setf (char string 0) (code-char #x10001)
          (char string (1- size)) (code-char #x10002))
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:ucs2be :replacement #\replacement_character))
      (write-string string s))
    (with-open-file (s *test-path* :external-format :ucs2be)
      (let ((new (make-string size)))
        (read-sequence new s)
        (assert (char= (char new 0) #\replacement_character))
        (assert (char= (char new (1- size)) #\replacement_character))
        (assert (string= string new :start1 1 :start2 1 :end1 (1- size) :end2 (1- size)))))))

(with-test (:name (:multibyte :input-replacement :ucs4le))
  (let ((octets (coerce '(0 1 1 0 1 0 0 1) '(vector (unsigned-byte 8)))))
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-sequence octets s))
    (with-open-file (s *test-path* :external-format '(:ucs4le :replacement #\replacement_character))
      (let ((string (read-line s)))
        (assert (char= (char string 0) (code-char #x10100)))
        (assert (char= (char string 1) #\replacement_character))))))

(with-test (:name (:multibyte :input-replacement :ucs4le))
  (let ((octets (coerce '(0 1 1 0 1 0 0 1) '(vector (unsigned-byte 8)))))
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-sequence octets s))
    (with-open-file (s *test-path* :external-format '(:ucs4be :replacement #\replacement_character))
      (let ((string (read-line s)))
        (assert (char= (char string 0) (code-char #x10100)))
        (assert (char= (char string 1) #\replacement_character))))))

;;; utf tests
(with-test (:name (:utf-16le :roundtrip))
  (let ((string (map 'string 'code-char '(#x20 #x200 #x2000 #xfffd #x10fffd))))
    (with-open-file (s *test-path* :direction :output :if-exists :supersede
                       :external-format :utf-16le)
      (write-string string s))
    (with-open-file (s *test-path* :external-format :utf-16le)
      (assert (string= string (read-line s))))))
(with-test (:name (:utf-16be :roundtrip))
  (let ((string (map 'string 'code-char '(#x20 #x200 #x2000 #xfffd #x10fffd))))
    (with-open-file (s *test-path* :direction :output :if-exists :supersede
                       :external-format :utf-16be)
      (write-string string s))
    (with-open-file (s *test-path* :external-format :utf-16be)
      (assert (string= string (read-line s))))))
(with-test (:name (:utf-16le :encoding-error))
  (let ((string (map 'string 'code-char '(#x20 #xfffe #xdc00 #xd800 #x1fffe #x20))))
    (with-open-file (s *test-path* :direction :output :if-exists :supersede
                       :external-format '(:utf-16le :replacement #\?))
      (write-string string s))
    (with-open-file (s *test-path* :external-format :utf-16le)
      (assert (string= " ???? " (read-line s))))))
(with-test (:name (:utf-16be :encoding-error))
  (let ((string (map 'string 'code-char '(#x20 #xfffe #xdc00 #xd800 #x1fffe #x20))))
    (with-open-file (s *test-path* :direction :output :if-exists :supersede
                       :external-format '(:utf-16be :replacement #\?))
      (write-string string s))
    (with-open-file (s *test-path* :external-format :utf-16be)
      (assert (string= " ???? " (read-line s))))))

(with-test (:name (:utf-32le :roundtrip))
  (let ((string (map 'string 'code-char '(#x20 #x200 #x2000 #xfffd #x10fffd))))
    (with-open-file (s *test-path* :direction :output :if-exists :supersede
                       :external-format :utf-32le)
      (write-string string s))
    (with-open-file (s *test-path* :external-format :utf-32le)
      (assert (string= string (read-line s))))))
(with-test (:name (:utf-32be :roundtrip))
  (let ((string (map 'string 'code-char '(#x20 #x200 #x2000 #xfffd #x10fffd))))
    (with-open-file (s *test-path* :direction :output :if-exists :supersede
                       :external-format :utf-32be)
      (write-string string s))
    (with-open-file (s *test-path* :external-format :utf-32be)
      (assert (string= string (read-line s))))))
(with-test (:name (:utf-32le :encoding-error))
  (let ((string (map 'string 'code-char '(#x20 #xfffe #xdc00 #xd800 #x1fffe #x20))))
    (with-open-file (s *test-path* :direction :output :if-exists :supersede
                       :external-format '(:utf-32le :replacement #\?))
      (write-string string s))
    (with-open-file (s *test-path* :external-format :utf-32le)
      (assert (string= " ???? " (read-line s))))))
(with-test (:name (:utf-32be :encoding-error))
  (let ((string (map 'string 'code-char '(#x20 #xfffe #xdc00 #xd800 #x1fffe #x20))))
    (with-open-file (s *test-path* :direction :output :if-exists :supersede
                       :external-format '(:utf-32be :replacement #\?))
      (write-string string s))
    (with-open-file (s *test-path* :external-format :utf-32be)
      (assert (string= " ???? " (read-line s))))))

(with-test (:name :invalid-external-format :fails-on :win32)
  (labels ((test-error (e)
             (assert (typep e 'error))
             (unless (equal "Undefined external-format: :BAD-FORMAT"
                            (princ-to-string e))
               (error "Bad error:~%  ~A" e)))
           (test (direction)
             (test-error
              (handler-case
                  (open "/dev/null" :direction direction :external-format :bad-format
                        :if-exists :overwrite)
                (error (e) e)))))
    (test :input)
    (test :output)
    (test :io)
    (test-error
     (handler-case
         (run-program "sh" '() :input :stream :external-format :bad-format)
       (error (e) e)))
    (test-error
     (handler-case
         (string-to-octets "foobar" :external-format :bad-format)
       (error (e) e)))
    (test-error
     (let ((octets (string-to-octets "foobar" :external-format :latin1)))
       (handler-case
           (octets-to-string octets :external-format :bad-format)
         (error (e) e))))))

(with-test (:name :lp713063)
  (with-open-file (f *test-path*
                     :direction :output
                     :external-format '(:euc-jp :replacement #\?)
                     :if-exists :supersede)
    (write-string (make-string 3 :initial-element #\horizontal_bar) f))
  (assert (equal "???"
                 (with-open-file (f *test-path*
                                    :direction :input
                                    :external-format :euc-jp)
                   (read-line f))))
  (delete-file *test-path*))

;;;; success
