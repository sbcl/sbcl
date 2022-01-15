(with-compilation-unit ()
  (load "test-util")
  (load "assertoid"))

(defpackage :run-tests
  (:use :cl :test-util :sb-ext))

(in-package :cl-user)
(use-package :test-util)
(use-package :assertoid)

(in-package :run-tests)

(defvar *break-on-error*)

(load "test-funs")

(defun run (file test-fun
            break-on-failure break-on-expected-failure break-on-error
            interpret)
  (setf *break-on-failure* break-on-failure
        *break-on-expected-failure* break-on-expected-failure
        *break-on-error* break-on-error)
  (when interpret
    (setf *test-evaluator-mode* :interpret)
    (push :interpreter *features*))
  (setf sb-ext:*evaluator-mode* *test-evaluator-mode*)
  (format t "// Running ~a in ~a evaluator mode~%"
          file *evaluator-mode*)
  (restart-case
      (handler-bind
          ((error (lambda (condition)
                    (push (list :unhandled-error file)
                          *failures*)
                    (cond (*break-on-error*
                           (test-util:really-invoke-debugger condition))
                          (t
                           (format *error-output* "~&Unhandled ~a: ~a~%"
                                   (type-of condition) condition)
                           (sb-debug:print-backtrace)))
                    (invoke-restart 'skip-file))))
        (let ((*package* (find-package :cl-user)))
          (funcall test-fun file)))
    (skip-file ()
      (format t ">>>~a<<<~%"*failures*)))
  (report-test-status)
  (exit :code 104))
