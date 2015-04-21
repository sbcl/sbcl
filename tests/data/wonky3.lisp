"(in form starting at line: 13, column: 1, position: 247)" ; expect this
#|
 More randomness
|#


#+no-such-feature (defun foo
                   (hooey))
    ; yay

;;; This should report EOF at a reasonable place
;;; and not "line 1 column 0"
 (defun complex (realpart &optional (imagpart 0))
  "Return a complex number with the specified real and imaginary components."
  (flet ((%%make-complex (realpart imagpart)
           (cond ((and (typep realpart 'double-float)
                       (typep imagpart 'double-float))
