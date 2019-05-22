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
      (let* ((actually-pure
              (not (or (search ".impure" (namestring file))
                       (search ".impure-cload" (namestring file)))))
             (packages-to-use '("ASSERTOID" "TEST-UTIL"))
             (test-package
              (if actually-pure
                  (make-package
                   (format nil "TEST~36,5,'_R" (random (expt 36 5)))
                   :use (append packages-to-use standard-use-list))
                  (find-package "CL-USER"))))
        ;; We want to ensure that pure tests remain as pure as possible.
        ;; DEFSTRUCT, DEFCLASS, DEFGENERIC, DEFMETHOD are certainly impure
        ;; as there is no easy way to eradicate after-effects. Supposing that
        ;; one did (SETF (FIND-CLASS 'x) NIL) for each classoid type defined in
        ;; a test, it does not remove from CLASS-DIRECT-SUBCLASSES of the ancestor.
        ;; We need to disallow all those impure macros by shadowing them
        ;; and providing no definition.
        ;; However, parallel execution uses PURE-RUNNER for impure tests,
        ;; so we need to leave the definitions alone in that case.
        ;; DEF{constant,fun,macro,parameter,setf,type,var} are generally ok
        ;; except when DEFfoo defines something too hairy to hang off a symbol.
        (cond (actually-pure
               (shadow '("DEFSTRUCT" "DEFMETHOD"
                         ;; Hiding IN-PACKAGE is a good preventative measure.
                         ;; There are other ways to do nasty things of course.
                         ;; Deliberately violating a package lock has got to be impure.
                         "IN-PACKAGE" "WITHOUT-PACKAGE-LOCKS")
                       test-package)
               ;; We have pure tests that exercise the DEFCLASS and DEFGENERIC
               ;; macros to generate macroexpansion-time errors.  That's mostly ok.
               ;; We can trap attempts to use SB-KERNEL::%COMPILER-mumble
               ;; functions though.
               (dolist (symbol '(sb-kernel::%compiler-defclass
                                 sb-pcl::compile-or-load-defgeneric))
                 (sb-int:encapsulate symbol 'defblah-guard
                                     (lambda (f &rest args)
                                       (if (eq *package* test-package)
                                           (error "Can't call ~S" f)
                                           (apply f args))))))
              (t
               (use-package packages-to-use test-package)))
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
        (when actually-pure
          (dolist (symbol '(sb-pcl::compile-or-load-defgeneric
                            sb-kernel::%compiler-defclass))
            (sb-int:unencapsulate symbol 'defblah-guard))
          (delete-package test-package))))
    ;; after all the files are done
    (append-failures)))

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
            (with-open-file (stream #.(merge-pathnames "test-status.lisp-expr"
                                                       *load-pathname*)
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
