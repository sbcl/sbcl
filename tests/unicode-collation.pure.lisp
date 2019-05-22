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

(use-package :sb-unicode)

(defun parse-codepoints (string &key (singleton-list t))
  (let ((list (mapcar
              (lambda (s) (parse-integer s :radix 16))
              (remove "" (split-string string #\Space) :test #'string=))))
    (if (not (or (cdr list) singleton-list)) (car list) list)))

(defun parse-string (codepoints)
  (coerce (mapcar #'code-char (parse-codepoints codepoints)) 'string))

(defun test-collation ()
  (declare (optimize (debug 2)))
  (with-test (:name (:collation)
                    :skipped-on (not :sb-unicode))
    (with-open-file (s "data/CollationTest_SHIFTED_SHORT.txt" :external-format :utf8)
      (loop with previous-string = ""
         for line = (read-line s nil nil)
         while line
         unless (or (eql 0 (position #\# line)) (string= line ""))
         do (let ((string (parse-string (subseq line 0 (position #\; line)))))
              (assert (unicode<= previous-string string))
              (setf previous-string string))))))

(test-collation)
