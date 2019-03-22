(load "test-util.lisp")
(load "assertoid.lisp")
(load "compiler-test-util.lisp")

(defpackage :run-tests
  (:use :cl :test-util :sb-ext))

(in-package run-tests)

(load "colorize.lisp")

(defvar *all-results* nil)
(defvar *break-on-error* nil)
(defvar *report-skipped-tests* nil)
(defvar *report-style* :describe)
(defvar *report-target* *standard-output*)
(defvar *explicit-test-files* nil)

(load "test-funs")

(defun run-all (&aux (start-time (get-internal-real-time)))
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
           ((string= arg "--report-style")
            (let ((style (intern (string-upcase (pop remainder)) :keyword)))
              (setf *report-style* style)))
           ((string= arg "--report-target")
            (setf *report-target* (pop remainder)))
           ((string= arg "--no-color"))
           (t
            (push (truename (parse-namestring arg)) *explicit-test-files*))))
  (setf *explicit-test-files* (nreverse *explicit-test-files*))
  (with-open-file (log "test.log" :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (pure-runner (pure-load-files) 'load-test log)
    (pure-runner (pure-cload-files) 'cload-test log)
    (impure-runner (impure-load-files) 'load-test log)
    (impure-runner (impure-cload-files) 'cload-test log)
    #-win32 (impure-runner (sh-files) 'sh-test log)
    (log-file-elapsed-time "GRAND TOTAL" start-time log))
  (unless (eq *report-style* :describe)
    (report :describe *standard-output*))
  (report *report-style* *report-target*)
  (sb-ext:exit :code (if (unexpected-failures)
                         1
                         104)))

(defun report (&optional (style :describe) (target *standard-output*))
  (let ((reporter (ecase style
                    (:describe #'report/describe)
                    (:junit    #'report/junit))))
    (etypecase target
      (stream
       (funcall reporter target))
      ((or string pathname)
       (with-open-file (stream target :if-does-not-exist :create
                               :direction :output
                               :if-exists :supersede)
         (funcall reporter stream))))))

(defun report/describe (stream)
  (terpri stream)
  (format stream "Finished running tests.~%")
  (let ((skipcount 0)
        (*print-pretty* nil))
    (cond ((failures)
           (format stream "Status:~%")
           (dolist (failure (reverse (failures)))
             (with-accessors ((status result-status)
                              (file result-file)
                              (name result-name)
                              (condition result-condition))
                 failure
               (case status
                 (:unhandled-error
                  (output-colored-text status
                                       " Unhandled Error")
                  (format stream " ~a~%"
                          (enough-namestring file)))
                 (:invalid-exit-status
                  (output-colored-text status
                                       " Invalid exit status:")
                  (format stream " ~a~%"
                          (enough-namestring file)))
                 (:skipped-disabled
                  (when *report-skipped-tests*
                    (format stream " ~20a ~a / ~a~%"
                            "Skipped (irrelevant):"
                            (enough-namestring file)
                            name))
                  (incf skipcount))
                 (t
                  (format stream "~20a ~a / ~a~%"
                          (ecase status
                            (:expected-failure " Expected failure:")
                            (:unexpected-failure " Failure:")
                            (:leftover-thread " Leftover thread (broken):")
                            (:unexpected-success " Unexpected success:")
                            (:skipped-broken " Skipped (broken):")
                            (:skipped-disabled " Skipped (irrelevant):"))
                          (enough-namestring file)
                          name)))))
           (when (> skipcount 0)
             (format t " (~a tests skipped for this combination of platform and features)~%"
                     skipcount)))
          (t
           (format t "All tests succeeded~%")))))

(defun failure-status-p (status)
  (member status '(:unexpected-failure :leftover-thread
                   :unexpected-success)))

(defun error-status-p (status)
  (member status '(:unhandled-error :invalid-exit-status)))

(defun legalize-string (string)
  (with-output-to-string (stream)
    (loop :for character :across string :do
       (write-string
        (case character
          (#\" "&quot;")
          (#\& "&amp;")
          (#\< "&lt;")
          (#\> "&gt;")
          (t   (string character)))
        stream))))

(defun legalize-component (component)
  (legalize-string (princ-to-string component)))

(defun report/junit (stream)
  (format stream "<testsuite name=~S ~
                             hostname=~S ~
                             timestamp=~S ~
                             time=\"~D\" ~
                             failures=\"~D\" ~
                             errors=\"~D\" ~
                  >~%"
          (format nil "~(~A~)-evaluator-mode" *test-evaluator-mode*)
          (legalize-string (machine-instance))
          "1"
          1
          (count-if #'failure-status-p *all-results* :key #'result-status)
          (count-if #'error-status-p *all-results* :key #'result-status))
  (dolist (result (reverse *all-results*))
    (with-accessors ((pathname result-file) (name result-name)
                     (reason result-status) (condition result-condition)) result
      (format stream "~2@T<testcase name=\"~(~A.~{~A~^_~}~)\" ~
                                    class=~S ~
                                    time=\"~D\" ~
                          >~%"
              (enough-namestring pathname)
              (etypecase name
                (null '("?"))
                (list (mapcar #'legalize-component name))
                ((or string symbol) (list (legalize-component name))))
              "DummyClass"
              1)
      (case reason
        (:unexpected-failure
         (format stream "~4@T<failure type=\"failure\" message=~S/>~%"
                 (legalize-string condition)))
        (:leftover-thread
         (format stream "~4@T<failure type=\"failure\" message=\"Leftover thread (broken)\"/>~%"))
        (:unexpected-success
         (format stream "~4@T<failure type=\"failure\" message=\"Unexpected success\"/>~%"))
        (:unhandled-error
         (format stream "~4@T<failure type=\"error\" message=\"Unhandled Error\"/>~%"))
        (:invalid-exit-status
         (format stream "~4@T<failure type=\"error\" message=\"Invalid exit status\"/>~%")))
      (format stream "~2@T</testcase>~%")))
  (format stream "</testsuite>~%"))

(defun log-file-elapsed-time (source-file begin-time log)
  (let ((end-time (get-internal-real-time)))
    (format log "~6d - ~a~%" (- end-time begin-time) source-file)
    (force-output log)))

(defun pure-runner (files test-fun log)
  (unless files
    (return-from pure-runner))
  (format t "// Running pure tests (~a)~%" test-fun)
  (let ((*failures* nil)
        ;; in case somebody corrupts CL-USER's use list, of course
        (standard-use-list (package-use-list "CL-USER")))
    (dolist (file files)
      (format t "// Running ~a in ~a evaluator mode~%"
              file *test-evaluator-mode*)
      (let ((test-package (make-package
                           (format nil "TEST~36,5,'_R" (random (expt 36 5)))
                           :use (append '("ASSERTOID" "TEST-UTIL")
                                        standard-use-list))))
        (let ((*package* test-package))
          (restart-case
            (handler-bind ((error (make-error-handler file)))
              (let* ((sb-ext:*evaluator-mode* *test-evaluator-mode*)
                     (*features*
                       (if (eq sb-ext:*evaluator-mode* :interpret)
                           (cons :interpreter *features*)
                           *features*)))
                (let ((start (get-internal-real-time)))
                  (funcall test-fun file)
                  (log-file-elapsed-time file start log))))
            (skip-file ())))
        (delete-package test-package)))
    ;; after all the files are done
    (append-results)))

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

(defun impure-runner (files test-fun log)
  (when files
    (format t "// Running impure tests (~a)~%" test-fun)
    (dolist (file files)
      (force-output)
      (let ((start (get-internal-real-time))
            (exit-code (run-impure-in-child-sbcl file test-fun)))
        (log-file-elapsed-time file start log)
        (if (= exit-code 104)
            (with-open-file (stream "test-status.lisp-expr"
                                    :direction :input
                                    :if-does-not-exist :error)
              (append-results (read stream)))
            (push (make-result :file file :status :invalid-exit-status)
                  *all-results*))))))

(defun make-error-handler (file)
  (lambda (condition)
    (push (make-result :file file :status :unhandled-error) *results*)
    (cond (*break-on-error*
           (test-util:really-invoke-debugger condition))
          (t
           (format *error-output* "~&Unhandled ~a: ~a~%"
                   (type-of condition) condition)
           (sb-debug:print-backtrace)))
    (invoke-restart 'skip-file)))

(defun append-results (&optional (results *results*))
  (setf *all-results* (append results *all-results*)))

(defun failures ()
  (remove :success *all-results* :key #'result-status))

(defun unexpected-failures ()
  (remove-if (lambda (x)
               (member (result-status x)
                       '(:success
                         :expected-failure
                         :unexpected-success
                         :skipped-broken
                         :skipped-disabled)))
             *all-results*))

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
