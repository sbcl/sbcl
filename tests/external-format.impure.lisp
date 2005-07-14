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

#-sb-unicode
(sb-ext:quit :unix-status 104)

(defmacro do-external-formats ((xf &optional result) &body body)
  (let ((nxf (gensym)))
    `(dolist (,nxf sb-impl::*external-formats* ,result)
       (let ((,xf (first (first ,nxf))))
         ,@body))))

(do-external-formats (xf)
  (with-open-file (s "/dev/null" :direction :input :external-format xf)
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
            do (assert (eql (read-char s) character))))))

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
        (dotimes (n 4097)
          (write-char character s)))
      (with-open-file (s "external-format-test.txt" :direction :input
                       :external-format :utf-8)
        (dotimes (n offset)
          (assert (eql (read-char s) #\a)))
        (dotimes (n 4097)
          (assert (eql (read-char s) character)))
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

(delete-file "external-format-test.txt")

(sb-ext:quit :unix-status 104)
