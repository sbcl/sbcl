(in-package sb-cover-test)

(defun sharp-plus-sharp-dot (x)
  #+#.(cl:if sb-cover-test::*flag* '(and) '(or))
  (1+ x)
  #-#.(cl:if sb-cover-test::*flag* '(and) '(or))
  (1- x))
