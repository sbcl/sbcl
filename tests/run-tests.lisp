#+#.(cl:if (cl:find-package "ASDF") '(or) '(and))
(load (merge-pathnames "../contrib/asdf/asdf.fasl"))

#+#.(cl:if (cl:find-package "SB-POSIX") '(or) '(and))
(let ((asdf:*central-registry*
       (cons "../contrib/systems/" asdf:*central-registry*)))
  (asdf:oos 'asdf:load-op 'sb-posix))

(load "test-util.lisp")

(defpackage :run-tests
    (:use :cl :test-util :sb-ext))

(load "assertoid.lisp")

(in-package run-tests)

(defvar *all-failures* nil)
(defvar *break-on-error* nil)
(defvar *accept-files* nil)

(defun run-all ()
  (dolist (arg (cdr *posix-argv*))
    (cond ((string= arg "--break-on-failure")
           (setf *break-on-error* t)
           (setf test-util:*break-on-failure* t))
          ((string= arg "--break-on-expected-failure")
           (setf test-util:*break-on-expected-failure* t))
          (t
           (push (truename (parse-namestring arg)) *accept-files*))))
  (pure-runner (pure-load-files) #'load-test)
  (pure-runner (pure-cload-files) #'cload-test)
  (impure-runner (impure-load-files) #'load-test)
  (impure-runner (impure-cload-files) #'cload-test)
  #-win32 (impure-runner (sh-files) #'sh-test)
  (report)
  (sb-ext:quit :unix-status (if (unexpected-failures)
                                1
                                104)))

(defun report ()
  (terpri)
  (format t "Finished running tests.~%")
  (cond (*all-failures*
         (format t "Status:~%")
         (dolist (fail (reverse *all-failures*))
           (cond ((eq (car fail) :unhandled-error)
                  (format t " ~20a ~a~%"
                          "Unhandled error"
                          (enough-namestring (second fail))))
                 ((eq (car fail) :invalid-exit-status)
                  (format t " ~20a ~a~%"
                          "Invalid exit status:"
                          (enough-namestring (second fail))))
                 (t
                  (format t " ~20a ~a / ~a~%"
                          (ecase (first fail)
                            (:expected-failure "Expected failure:")
                            (:unexpected-failure "Failure:")
                            (:unexpected-success "Unexpected success:"))
                          (enough-namestring (second fail))
                          (third fail))))))
        (t
         (format t "All tests succeeded~%"))))

(defun pure-runner (files test-fun)
  (format t "// Running pure tests (~a)~%" test-fun)
  (let ((*package* (find-package :cl-user))
        (*failures* nil))
    (setup-cl-user)
    (dolist (file files)
      (when (accept-test-file file)
        (format t "// Running ~a~%" file)
        (restart-case
            (handler-bind ((error (make-error-handler file)))
              (eval (funcall test-fun file)))
          (skip-file ()))))
    (append-failures)))

(defun run-in-child-sbcl (load-forms forms)
  ;; We used to fork() for POSIX platforms, and use this for Windows.
  ;; However, it seems better to use the same solution everywhere.
  (process-exit-code
   (#-win32 with-open-file #-win32 (devnull "/dev/null") #+win32 progn
     (sb-ext:run-program
      (first *POSIX-ARGV*)
      (append
       (list "--core" SB-INT:*CORE-STRING*
             "--noinform"
             "--no-sysinit"
             "--no-userinit"
             "--noprint"
             "--disable-debugger")
       (loop for form in (append load-forms forms)
             collect "--eval"
             collect (write-to-string form)))
      :output sb-sys:*stdout*
      :input #-win32 devnull #+win32 sb-sys:*stdin*))))

(defun run-impure-in-child-sbcl (test-file test-code)
  (run-in-child-sbcl
    `((load "test-util")
      (load "assertoid")
      (defpackage :run-tests
        (:use :cl :test-util :sb-ext)))

    `((in-package :cl-user)
      (use-package :test-util)
      (use-package :assertoid)
      (setf test-util:*break-on-failure* ,test-util:*break-on-failure*)
      (setf test-util:*break-on-expected-failure*
            ,test-util:*break-on-expected-failure*)
      (let ((file ,test-file)
            (*break-on-error* ,run-tests::*break-on-error*))
        (format t "// Running ~a~%" file)
        (restart-case
            (handler-bind
                ((error (lambda (condition)
                          (push (list :unhandled-error file)
                                test-util::*failures*)
                          (cond (*break-on-error*
                                 (test-util:really-invoke-debugger condition))
                                (t
                                 (format *error-output* "~&Unhandled ~a: ~a~%"
                                         (type-of condition) condition)
                                 (sb-debug:backtrace)))
                          (invoke-restart 'skip-file))))
              ,test-code)
          (skip-file ()
            (format t ">>>~a<<<~%" test-util::*failures*)))
        (test-util:report-test-status)
        (sb-ext:quit :unix-status 104)))))

(defun impure-runner (files test-fun)
  (format t "// Running impure tests (~a)~%" test-fun)
  (let ((*package* (find-package :cl-user)))
    (setup-cl-user)
    (dolist (file files)
      (when (accept-test-file file)
        (force-output)
        (let ((exit-code (run-impure-in-child-sbcl file
                                                   (funcall test-fun file))))
          (if (= exit-code 104)
              (with-open-file (stream "test-status.lisp-expr"
                                      :direction :input
                                      :if-does-not-exist :error)
                (append-failures (read stream)))
              (push (list :invalid-exit-status file)
                    *all-failures*)))))))

(defun make-error-handler (file)
  (lambda (condition)
    (push (list :unhandled-error file) *failures*)
    (cond (*break-on-error*
           (test-util:really-invoke-debugger condition))
          (t
           (format *error-output* "~&Unhandled ~a: ~a~%"
                   (type-of condition) condition)
           (sb-debug:backtrace)))
    (invoke-restart 'skip-file)))

(defun append-failures (&optional (failures *failures*))
  (setf *all-failures* (append failures *all-failures*)))

(defun unexpected-failures ()
  (remove-if (lambda (x)
               (or (eq (car x) :expected-failure)
                   (eq (car x) :unexpected-success)))
             *all-failures*))

(defun setup-cl-user ()
  (use-package :test-util)
  (use-package :assertoid))

(defun load-test (file)
  `(load ,file))

(defun cload-test (file)
  `(let ((compile-name (compile-file-pathname ,file)))
     (unwind-protect
          (progn
            (compile-file ,file)
            (load compile-name))
       (ignore-errors
         (delete-file compile-name)))))

(defun sh-test (file)
  ;; What? No SB-POSIX:EXECV?
  `(let ((process (sb-ext:run-program "/bin/sh"
                                      (list (native-namestring ,file))
                                      :environment (test-util::test-env)
                                      :output *error-output*)))
     (sb-ext:quit :unix-status (process-exit-code process))))

(defun accept-test-file (file)
  (if *accept-files*
      (find (truename file) *accept-files* :test #'equalp)
      t))

(defun pure-load-files ()
  (directory "*.pure.lisp"))

(defun pure-cload-files ()
  (directory "*.pure-cload.lisp"))

(defun impure-load-files ()
  (directory "*.impure.lisp"))

(defun impure-cload-files ()
  (directory "*.impure-cload.lisp"))

(defun sh-files ()
  (directory "*.test.sh"))
