(load "test-util.lisp")
(load "assertoid.lisp")
(load "compiler-test-util.lisp")

(defpackage :run-tests
  (:use :cl :test-util :sb-ext))

(in-package run-tests)

(load "colorize.lisp")

(defvar *all-failures* nil)
(defvar *break-on-error* nil)
(defvar *report-skipped-tests* nil)
(defvar *explicit-test-files* nil)

(load "test-funs")

(defun run-all ()
  (loop :with remainder = (rest *posix-argv*)
     :while remainder
     :for arg = (pop remainder)
     :do (cond
           ((string= arg "--evaluator-mode")
            (let ((mode (pop remainder)))
              (cond
                ((string= mode "interpret")
                 (setf *test-evaluator-mode* :interpret))
                ((string= mode "compile")
                 (setf *test-evaluator-mode* :compile))
                (t
                 (error "~@<Invalid evaluator mode: ~A. Must be one ~
                           of interpret, compile.~@:>"
                        mode)))))
           ((string= arg "--break-on-failure")
            (setf *break-on-error* t)
            (setf test-util:*break-on-failure* t))
           ((string= arg "--break-on-expected-failure")
            (setf test-util:*break-on-expected-failure* t))
           ((string= arg "--report-skipped-tests")
            (setf *report-skipped-tests* t))
           ((string= arg "--no-color"))
           (t
            (push (truename (parse-namestring arg)) *explicit-test-files*))))
  (setf *explicit-test-files* (nreverse *explicit-test-files*))
  (pure-runner (pure-load-files) 'load-test)
  (pure-runner (pure-cload-files) 'cload-test)
  (impure-runner (impure-load-files) 'load-test)
  (impure-runner (impure-cload-files) 'cload-test)
  #-win32 (impure-runner (sh-files) 'sh-test)
  (report)
  (sb-ext:exit :code (if (unexpected-failures)
                         1
                         104)))

(defun report ()
  (terpri)
  (format t "Finished running tests.~%")
  (let ((skipcount 0)
        (*print-pretty* nil))
    (cond (*all-failures*
           (format t "Status:~%")
           (dolist (fail (reverse *all-failures*))
             (cond ((eq (car fail) :unhandled-error)
                    (output-colored-text (car fail)
                                          " Unhandled Error")
                    (format t " ~a~%"
                            (enough-namestring (second fail))))
                   ((eq (car fail) :invalid-exit-status)
                    (output-colored-text (car fail)
                                          " Invalid exit status:")
                    (format t " ~a~%"
                            (enough-namestring (second fail))))
                   ((eq (car fail) :skipped-disabled)
                    (when *report-skipped-tests*
                      (format t " ~20a ~a / ~a~%"
                              "Skipped (irrelevant):"
                              (enough-namestring (second fail))
                              (third fail)))
                    (incf skipcount))
                   (t
                    (output-colored-text
                     (first fail)
                     (ecase (first fail)
                       (:expected-failure " Expected failure:")
                       (:unexpected-failure " Failure:")
                       (:leftover-thread " Leftover thread (broken):")
                       (:unexpected-success " Unexpected success:")
                       (:skipped-broken " Skipped (broken):")
                       (:skipped-disabled " Skipped (irrelevant):")))
                    (format t " ~a / ~a~%"
                            (enough-namestring (second fail))
                            (third fail)))))
           (when (> skipcount 0)
             (format t " (~a tests skipped for this combination of platform and features)~%"
                     skipcount)))
          (t
           (format t "All tests succeeded~%")))))

(defun pure-runner (files test-fun)
  (when files
    (format t "// Running pure tests (~a)~%" test-fun)
    (let ((*package* (find-package :cl-user))
          (*failures* nil))
      (setup-cl-user)
      (dolist (file files)
        (format t "// Running ~a in ~a evaluator mode~%"
                file *test-evaluator-mode*)
        (restart-case
            (handler-bind ((error (make-error-handler file)))
              (let* ((sb-ext:*evaluator-mode* *test-evaluator-mode*)
                     (*features*
                       (if (eq sb-ext:*evaluator-mode* :interpret)
                           (cons :interpreter *features*)
                           *features*)))
                (funcall test-fun file)))
          (skip-file ())))
      (append-failures))))

(defun run-in-child-sbcl (load eval)
  (process-exit-code
   (sb-ext:run-program
    (first *POSIX-ARGV*)
    (list "--core" SB-INT:*CORE-STRING*
           "--noinform"
           "--no-sysinit"
           "--no-userinit"
           "--noprint"
           "--disable-debugger"
           "--load" load
           "--eval" (write-to-string eval
                                     :right-margin 1000))
    :output t
    :input t)))

(defun run-impure-in-child-sbcl (test-file test-fun)
  (clear-test-status)
  (run-in-child-sbcl
   "impure-runner"
   `(run-tests::run
     ,(enough-namestring test-file)
     ',test-fun
     ,*break-on-failure*
     ,*break-on-expected-failure*
     ,*break-on-error*
     ,(eq *test-evaluator-mode* :interpret))))

(defun impure-runner (files test-fun)
  (when files
    (format t "// Running impure tests (~a)~%" test-fun)
    (dolist (file files)
      (force-output)
      (let ((exit-code (run-impure-in-child-sbcl file test-fun)))
        (if (= exit-code 104)
            (with-open-file (stream "test-status.lisp-expr"
                                    :direction :input
                                    :if-does-not-exist :error)
              (append-failures (read stream)))
            (push (list :invalid-exit-status file)
                  *all-failures*))))))

(defun make-error-handler (file)
  (lambda (condition)
    (push (list :unhandled-error file) *failures*)
    (cond (*break-on-error*
           (test-util:really-invoke-debugger condition))
          (t
           (format *error-output* "~&Unhandled ~a: ~a~%"
                   (type-of condition) condition)
           (sb-debug:print-backtrace)))
    (invoke-restart 'skip-file)))

(defun append-failures (&optional (failures *failures*))
  (setf *all-failures* (append failures *all-failures*)))

(defun unexpected-failures ()
  (remove-if (lambda (x)
               (or (eq (car x) :expected-failure)
                   (eq (car x) :unexpected-success)
                   (eq (car x) :skipped-broken)
                   (eq (car x) :skipped-disabled)))
             *all-failures*))

(defun setup-cl-user ()
  (use-package :test-util)
  (use-package :assertoid))

(defun filter-test-files (wild-mask)
  (if *explicit-test-files*
      (loop for file in *explicit-test-files*
            when (pathname-match-p file wild-mask)
            collect file)
      (directory wild-mask)))

(defun pure-load-files ()
  (filter-test-files "*.pure.lisp"))

(defun pure-cload-files ()
  (filter-test-files "*.pure-cload.lisp"))

(defun impure-load-files ()
  (filter-test-files "*.impure.lisp"))

(defun impure-cload-files ()
  (filter-test-files "*.impure-cload.lisp"))

(defun sh-files ()
  (filter-test-files "*.test.sh"))
