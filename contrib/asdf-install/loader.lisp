(in-package :cl-user)

(eval-when (:load-toplevel)
  (require 'asdf)
  (require 'asdf-install))

(defun run ()
  (handler-case
      (apply #'asdf-install:install (cdr *posix-argv*))
    (error (c)
      (format *error-output* "Install failed due to error:~%  ~A~%" c)
      (sb-ext:quit :unix-status 1))))

