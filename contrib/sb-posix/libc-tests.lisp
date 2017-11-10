(defpackage #:sb-libc-tests (:use #:cl #:sb-rt))
(in-package "SB-LIBC-TESTS")

(defparameter *tests* ; unicode strings (by default unless #-sb-unicode)
  #("1.20203" "3.4400" "3240.2205" "10088.92" "12.3" "1000000000e-2"
    "-323.5233243" "10001.993344e-8" "3985.1"))

(defparameter *decimal-point-character*
  (loop for candidate in '(#\. #\,)
     when (= (nth-value 1 (sb-posix:strtod "1.234")) 5)
     return candidate))

(defun substitute-decimal-point (string)
  (substitute *decimal-point-character* #\. string))

(deftest sb-libc.strtod.0
    (or (not *decimal-point-character*)
        (loop with *read-default-float-format* = 'double-float
           for string across *tests*
           do (assert (= (sb-posix:strtod (substitute-decimal-point string))
                         (read-from-string string)))
           finally (return t)))
  t)

(deftest sb-libc.strtod.1
    (or (not *decimal-point-character*)
        (loop with *read-default-float-format* = 'double-float
           for string across *tests*
           for simple-base-string = (coerce (substitute-decimal-point string)
                                            'simple-base-string)
           do (assert (= (sb-posix:strtod simple-base-string)
                         (read-from-string string)))
           finally (return t)))
  t)
