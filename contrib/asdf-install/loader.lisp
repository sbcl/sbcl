
(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'asdf)
  (asdf:operate 'asdf:load-op 'asdf-install :verbose nil))

(defun run ()
  (handler-case
      (apply #'asdf-install:install (cdr *posix-argv*))
    (error (c)
      (princ "Install failed due to error:") (terpri)
      (princ c) (terpri)
      (quit :unix-status 1))))

;(quit)