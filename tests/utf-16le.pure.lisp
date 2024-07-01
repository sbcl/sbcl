;;;; This file is for testing external-format functionality for
;;;; little-endian UTF-16, using test machinery which does not have
;;;; side effects.  Note that the tests here reach into unexported
;;;; functionality, and should not be used as a guide for users.

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

#-sb-unicode (invoke-restart 'run-tests::skip-file)

(defvar *test-path* (scratch-file-name))

(macrolet ((input-test (inxf expected &environment env)
             `(progn
                (with-test (:name (,(macroexpand 'name env) :file ,inxf))
                  (with-open-file (s *test-path* :external-format ',inxf)
                    (handler-bind ((sb-int:character-decoding-error
                                    (lambda (c) (use-value "" c))))
                      (let* ((string (make-string 20))
                             (count (read-sequence string s)))
                        (assert (equal (map 'list 'identity (subseq string 0 count)) ,expected))))))
                (with-test (:name (,(macroexpand 'name env) :octets ,inxf))
                  (handler-bind ((sb-int:character-decoding-error
                                  (lambda (c) (use-value "" c))))
                    (let ((octets (coerce bytes '(simple-array (unsigned-byte 8) 1))))
                      (assert (equal (sb-ext:octets-to-string octets :external-format ',inxf)
                                     (coerce ,expected 'string))))))))
           (with-input-bytes ((id bytes) &body body)
             `(let ((bytes ,bytes))
                (with-open-file (s *test-path* :element-type '(unsigned-byte 8)
                                   :direction :output :if-exists :supersede)
                  (dolist (byte bytes)
                    (write-byte byte s)))
                (symbol-macrolet ((name ,id))
                  (macrolet ((test (inxf expected)
                               `(input-test ,inxf ,expected)))
                    ,@body)))))
  (with-input-bytes ((:input :invalid) (list #x35 #x00 #xff #xff #x37 #x00))
    (test :utf-16le '(#\5 #\7))
    (test (:utf-16le :replacement #\?) '(#\5 #\? #\7)))
  (with-input-bytes ((:input :multiple-invalid) (list #x35 #x00 #xff #xff #xff #xff #x37 #x00))
    (test :utf-16le '(#\5 #\7))
    (test (:utf-16le :replacement #\?) '(#\5 #\? #\? #\7)))
  (with-input-bytes ((:input :invalid-units) (list #x00 #x35 #x00))
    (test :utf-16be '(#\5))
    (test (:utf-16be :replacement #\?) '(#\5 #\?)))
  (with-input-bytes ((:input :invalid-then-invalid-units) (list #xff #xff #x00))
    (test :utf-16be '())
    (test (:utf-16be :replacement #\?) '(#\? #\?))))

(macrolet ((output-test (chars outxf expected &environment env)
             `(progn
                (with-test (:name (,(macroexpand 'name env) file-string-length ,outxf))
                  (let ((string (coerce ,chars 'string)))
                    (with-open-file (s *test-path* :element-type 'character
                                       :external-format ',outxf
                                       :direction :output :if-exists :supersede)
                      (handler-bind ((sb-int:character-encoding-error
                                      (lambda (c) (use-value "" c))))
                        (let ((pos (file-position s))
                              (len (file-string-length s string)))
                          (let ((actual
                                 (loop for index from 0 below (length string)
                                       for char = (char string index)
                                       for thislen = (file-string-length s char)
                                       for thisstringlen = (file-string-length s (subseq string index))
                                       if (null thisstringlen) do (assert (some 'null (subseq ,expected index))) else do (assert (notany 'null (subseq ,expected index)))
                                       collect thislen
                                       if (and (null len) thisstringlen) do (setf len (+ pos thisstringlen))
                                       if thisstringlen do (assert (= (+ pos thisstringlen) len))
                                       do (write-char char s)
                                       if thislen do (assert (= (+ pos thislen) (file-position s)))
                                       do (setf pos (file-position s)))))
                            (assert (equal actual ,expected))))))))))
           (with-output-characters ((id chars) &body body)
             `(let ((chars ,chars))
                (symbol-macrolet ((name ,id))
                  (macrolet ((test (outxf expected)
                               `(output-test chars ,outxf ,expected)))
                    ,@body)))))
  (with-output-characters ((:output :lf) (list #\5 #\LATIN_SMALL_LETTER_E_WITH_ACUTE #\Linefeed #\7))
    (test :utf-16le '(2 2 2 2)))
  (with-output-characters ((:output :invalid :lf) (list #\5 (code-char #xdb00) (code-char #x12345) #\Linefeed #\7))
    ;; A sufficiently-smart streams implementation could statically determine the lengths
    ;; of replacement characters given as part of the external format
    (test :utf-16le '(2 nil 4 2 2))
    (test (:utf-16le :replacement #\?) '(2 nil 4 2 2))))

(delete-file *test-path*)
