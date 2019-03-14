(defpackage sb-cover-test-with-nickname
  (:use :cl)
  (:local-nicknames (:foo :cl)))
(in-package sb-cover-test-with-nickname)

;; in order to process package-local nicknames correctly, sb-cover
;; must (heuristically) track (in-package) forms.
(defun sb-cover-test::test4 ()
  (foo:+ 1 2))

