;;;; miscellaneous tests of SYMBOL-related stuff

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

(in-package "CL-USER")

;;; Reported by Paul F. Dietz
(with-test (:name (:symbol :non-simple-string-name))
  (let ((sym (make-symbol (make-array '(1) :element-type 'character
                                      :adjustable t :initial-contents "X"))))
    (assert (simple-string-p (symbol-name sym)))
    (print sym (make-broadcast-stream))))

(with-test (:name (gentemp pprinter))
  (let* ((*print-pprint-dispatch* (copy-pprint-dispatch)))
    (set-pprint-dispatch 'string
                         (lambda (stream obj) (write-string "BAR-" stream)))
    (assert (string= "FOO-" (gentemp "FOO-") :end2 4))))
