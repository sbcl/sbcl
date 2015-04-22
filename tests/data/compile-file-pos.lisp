;; This file is encoded in UTF8 xxx.
;;
(defun cfp-foolz1 ()
  (values "Here is a string: םולש"
          (format nil "Line ~D" (compile-file-line))
          (format nil "Hey hey! ~D" (compile-file-position))))
