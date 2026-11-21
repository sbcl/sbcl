(in-package "SB-POSIX-TESTS")

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

(defvar *displaced-base-string*
  (make-array 3 :element-type 'base-char
                :displaced-to (make-array 10 :element-type 'base-char
                                             :initial-contents "0123456789")
                                             ;; will be set to #\null ^
                :displaced-index-offset 2))
;; Our strtod() wrapper looked for a null 1 char past where it would be required to treat
;; a non-simple base string as a C string. Consequently the answer was often bogus
;; except when string had an additional null in the right place.
(setf (char (sb-kernel:%array-data *displaced-base-string*) 6) #\null)

(deftest sb-libc.strtod.2
    (or (not *decimal-point-character*)
        (progn (assert (= (sb-posix:strtod *displaced-base-string*) 234.0d0))
               t))
  t)
