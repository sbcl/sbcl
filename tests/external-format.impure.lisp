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

(defmacro do-external-formats ((xf &optional result) &body body)
  (let ((nxf (gensym)))
    `(dolist (,nxf sb-impl::*external-formats* ,result)
       (let ((,xf (first (first ,nxf))))
         ,@body))))

(do-external-formats (xf)
  (with-open-file (s #-win32 "/dev/null" #+win32 "nul" :direction :input :external-format xf)
    (assert (eq (read-char s nil s) s))))

;;; Test standard character read-write equivalency over all external formats.
(let ((standard-characters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!$\"'(),_-./:;?+<=>#%&*@[\\]{|}`^~"))
  (do-external-formats (xf)
    (with-open-file (s "external-format-test.txt" :direction :output
                     :if-exists :supersede :external-format xf)
      (loop for character across standard-characters
            do (write-char character s)))
    (with-open-file (s "external-format-test.txt" :direction :input
                     :external-format xf)
      (loop for character across standard-characters
            do (let ((got (read-char s)))
                 (unless (eql character got)
                   (error "wanted ~S, got ~S" character got)))))))

(delete-file "external-format-test.txt")
#-sb-unicode
(progn
  (test-util:report-test-status)
  (sb-ext:quit :unix-status 104))

;;; Test UTF-8 writing and reading of 1, 2, 3 and 4 octet characters with
;;; all possible offsets. Tests for buffer edge bugs. fd-stream buffers are
;;; 4096 wide.
(dotimes (width-1 4)
  (let ((character (code-char (elt '(1 #x81 #x801 #x10001) width-1))))
    (dotimes (offset (+ width-1 1))
      (with-open-file (s "external-format-test.txt" :direction :output
                       :if-exists :supersede :external-format :utf-8)
        (dotimes (n offset)
          (write-char #\a s))
        (dotimes (n (+ 4 sb-impl::+bytes-per-buffer+))
          (write-char character s)))
      (with-open-file (s "external-format-test.txt" :direction :input
                       :external-format :utf-8)
        (dotimes (n offset)
          (assert (eql (read-char s) #\a)))
        (dotimes (n (+ 4 sb-impl::+bytes-per-buffer+))
          (let ((got (read-char s)))
            (unless (eql got character)
              (error "wanted ~S, got ~S (~S)" character got n))))
        (assert (eql (read-char s nil s) s))))))

;;; Test character decode restarts.
(with-open-file (s "external-format-test.txt" :direction :output
                 :if-exists :supersede :element-type '(unsigned-byte 8))
  (write-byte 65 s)
  (write-byte 66 s)
  (write-byte #xe0 s)
  (write-byte 67 s))
(with-open-file (s "external-format-test.txt" :direction :input
                 :external-format :utf-8)
  (handler-bind
      ((sb-int:character-decoding-error #'(lambda (decoding-error)
                                            (declare (ignore decoding-error))
                                            (invoke-restart
                                             'sb-int:attempt-resync))))
    (assert (equal (read-line s nil s) "ABC"))
    (assert (equal (read-line s nil s) s))))
(with-open-file (s "external-format-test.txt" :direction :input
                 :external-format :utf-8)
  (handler-bind
      ((sb-int:character-decoding-error #'(lambda (decoding-error)
                                            (declare (ignore decoding-error))
                                            (invoke-restart
                                             'sb-int:force-end-of-file))))
    (assert (equal (read-line s nil s) "AB"))
    (assert (equal (read-line s nil s) s))))

;;; And again with more data to account for buffering (this was briefly)
;;; broken in early 0.9.6.
(with-open-file (s "external-format-test.txt" :direction :output
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
  (with-open-file (s "external-format-test.txt" :direction :input
                     :external-format :utf-8)
    (handler-bind
        ((sb-int:character-decoding-error #'(lambda (decoding-error)
                                              (declare (ignore decoding-error))
                                              (invoke-restart
                                               'sb-int:attempt-resync)))
         ;; The failure mode is an infinite loop, add a timeout to detetct it.
         (sb-ext:timeout (lambda () (error "Timeout"))))
      (sb-ext:with-timeout 5
        (dotimes (i 80)
          (assert (equal (read-line s nil s)
                         "1234567890123456789012345678901234567890123456789")))))))

(with-test (:name (:character-decode-large :force-end-of-file)
            :fails-on :sbcl)
  (error "We can't reliably test this due to WITH-TIMEOUT race condition")
  ;; This test will currently fail. But sometimes it will fail in
  ;; ungracefully due to the WITH-TIMEOUT race mentioned above. This
  ;; rightfully confuses some people, so we'll skip running the code
  ;; for now. -- JES, 2006-01-27
  #+nil
  (with-open-file (s "external-format-test.txt" :direction :input
                     :external-format :utf-8)
    (handler-bind
        ((sb-int:character-decoding-error #'(lambda (decoding-error)
                                              (declare (ignore decoding-error))
                                              (invoke-restart
                                               'sb-int:force-end-of-file)))
         ;; The failure mode is an infinite loop, add a timeout to detetct it.
         (sb-ext:timeout (lambda () (error "Timeout"))))
      (sb-ext:with-timeout 5
        (dotimes (i 80)
          (assert (equal (read-line s nil s)
                         "1234567890123456789012345678901234567890123456789")))
        (assert (equal (read-line s nil s) s))))))

;;; Test character encode restarts.
(with-open-file (s "external-format-test.txt" :direction :output
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
(with-open-file (s "external-format-test.txt" :direction :input
                 :external-format :latin-1)
  (assert (equal (read-line s nil s) "ABC"))
  (assert (equal (read-line s nil s) s)))

(with-open-file (s "external-format-test.txt" :direction :output
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
(with-open-file (s "external-format-test.txt" :direction :input
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
         (compile-file "external-format-test.lisp" :external-format :utf-8))
    (delete-file s)
    (let ((p (probe-file (compile-file-pathname "external-format-test.lisp"))))
      (when p
        (delete-file p)))))


;;;; KOI8-R external format
(with-open-file (s "external-format-test.txt" :direction :output
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
(with-open-file (s "external-format-test.txt" :direction :input
                 :element-type '(unsigned-byte 8))
  (let ((byte (read-byte s)))
    (assert (= (eval byte) #x9C))))
(with-open-file (s "external-format-test.txt" :direction :input
                 :external-format :koi8-r)
  (let ((char (read-char s)))
    (assert (= (char-code (eval char)) #xB0))))
(delete-file "external-format-test.txt")

(let* ((koi8-r-codes (coerce '(240 210 201 215 197 212 33) '(vector (unsigned-byte 8))))
       (uni-codes #(1055 1088 1080 1074 1077 1090 33))

       (string (octets-to-string koi8-r-codes :external-format :koi8-r))
       (uni-decoded (map 'vector #'char-code string)))
  (assert (equalp (map 'vector #'char-code (octets-to-string koi8-r-codes :external-format :koi8-r))
                  uni-codes))
  (assert (equalp (string-to-octets (map 'string #'code-char uni-codes) :external-format :koi8-r)
                  koi8-r-codes)))

;;; tests of FILE-STRING-LENGTH
(let ((standard-characters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!$\"'(),_-./:;?+<=>#%&*@[\\]{|}`^~"))
  (do-external-formats (xf)
    (with-open-file (s "external-format-test.txt" :direction :output
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
    (delete-file "external-format-test.txt")))

(let ((char-codes '(0 1 255 256 511 512 1023 1024 2047 2048 4095 4096
                    8191 8192 16383 16384 32767 32768 65535 65536 131071
                    131072 262143 262144)))
  (with-open-file (s "external-format-test.txt" :direction :output
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
  (let ((path "external-format-test.txt"))
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
        (format t "read character with code ~a successfully from file position ~a~%"
                (char-code char) pos)
        (file-position s pos)
        (format t "set file position back to ~a, trying to read-char again~%" pos)
        (let ((new-char (read-char s)))
          (assert (char= char new-char)))))
    (values)))

;;; External format support in SB-ALIEN

(with-test (:name (:sb-alien :vanilla))
  (define-alien-routine strdup c-string (str c-string))
  (assert (equal "foo" (strdup "foo"))))

(with-test (:name (:sb-alien :utf-8 :utf-8))
  (define-alien-routine strdup (c-string :external-format :utf-8)
    (str (c-string :external-format :utf-8)))
  (assert (equal "foo" (strdup "foo"))))

(with-test (:name (:sb-alien :latin-1 :utf-8))
  (define-alien-routine strdup (c-string :external-format :latin-1)
    (str (c-string :external-format :utf-8)))
  (assert (= (length (strdup (string (code-char 246))))
             2)))

(with-test (:name (:sb-alien :utf-8 :latin-1))
  (define-alien-routine strdup (c-string :external-format :utf-8)
    (str (c-string :external-format :latin-1)))
  (assert (equal (string (code-char 228))
                 (strdup (concatenate 'string
                                      (list (code-char 195))
                                      (list (code-char 164)))))))

(with-test (:name (:sb-alien :ebcdic :ebcdic))
  (define-alien-routine strdup (c-string :external-format :ebcdic-us)
    (str (c-string :external-format :ebcdic-us)))
  (assert (equal "foo" (strdup "foo"))))

(with-test (:name (:sb-alien :latin-1 :ebcdic))
  (define-alien-routine strdup (c-string :external-format :latin-1)
    (str (c-string :external-format :ebcdic-us)))
  (assert (not (equal "foo" (strdup "foo")))))

(with-test (:name (:sb-alien :simple-base-string))
  (define-alien-routine strdup (c-string :external-format :ebcdic-us
                                         :element-type base-char)
    (str (c-string :external-format :ebcdic-us)))
  (assert (typep (strdup "foo") 'simple-base-string)))

;;;; success
