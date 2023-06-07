;;;; This file is for testing external-format functionality for
;;;; little-endian UCS-2, using test machinery which does not have
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

(macrolet ((output-test (chars outxf expected &environment env)
             `(progn
                (with-test (:name (,(macroexpand 'name env) file-string-length ,outxf))
                  (let ((string (coerce ,chars 'string)))
                    (with-open-file (s *test-path* :element-type 'character
                                       :external-format ',outxf
                                       :direction :output :if-exists :supersede)
                      (handler-bind ((sb-int:character-encoding-error
                                      (lambda (c)
                                        (let ((restart (find-restart 'sb-impl::output-nothing c)))
                                          (invoke-restart restart)))))
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
    (test :ucs-2le '(2 2 2 2)))
  (with-output-characters ((:output :invalid :lf) (list #\5 #\LATIN_SMALL_LETTER_E_WITH_ACUTE (code-char #x12345) #\Linefeed #\7))
    ;; A sufficiently-smart streams implementation could statically determine the lengths
    ;; of replacement characters given as part of the external format
    (test :ucs-2le '(2 2 nil 2 2))
    (test (:ucs-2le :replacement #\?) '(2 2 nil 2 2))))
