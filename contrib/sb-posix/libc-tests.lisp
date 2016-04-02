(defpackage #:sb-libc-tests (:use #:cl #:sb-rt))
(in-package "SB-LIBC-TESTS")

(defparameter *tests* ; unicode strings (by default unless #-sb-unicode)
  #("1.20203" "3.4400" "3240.2205" "10088.92" "12.3" "1000000000e-2"
    "-323.5233243" "10001.993344e-8" "3985.1"))

(deftest sb-libc.strtod.0
  (loop with *read-default-float-format* = 'double-float
        for string across *tests*
        do (assert (= (sb-posix:strtod string) (read-from-string string)))
        finally (return t))
  t)

(deftest sb-libc.strtod.1
  (loop with *read-default-float-format* = 'double-float
        for string across (map 'vector
                               (lambda (x) (coerce x 'simple-base-string))
                               *tests*)
        do (assert (= (sb-posix:strtod string) (read-from-string string)))
        finally (return t))
  t)
