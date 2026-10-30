(eval-when (:compile-toplevel :load-toplevel :execute)
  (locally (declare (sb-ext:muffle-conditions sb-int:package-at-variance))
    (handler-bind ((sb-int:package-at-variance #'muffle-warning))
      (defpackage :sb-manual
        (:use :cl :sb-alien)
        (:export #:use-pax)
        #+sb-doc
        (:import-from #:sb-pcl
         #:string-lines #:whitespacep #:indentation #:blankp
         #:reindent-docstring #:*normalize-sbcl-docstrings*
         #:markdown-to-plain-text)))))

#-sb-doc
(defun sb-manual::generate-texinfo ()
  (format t "SBCL was built with --WITHOUT-SB-DOC. ~
            Not generating the manual.~%"))
