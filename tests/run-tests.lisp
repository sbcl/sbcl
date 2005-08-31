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
  (impure-runner (sh-files) #'sh-test)
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
        (handler-case
            (funcall test-fun file)
          (error (error)
                 (push (list :unhandled-error file)
                       *all-failures*)
                 (when *break-on-error*
                   (test-util:really-invoke-debugger error))))))
    (append-failures)))

(defun impure-runner (files test-fun)
  (format t "// Running impure tests (~a)~%" test-fun)
  (let ((*package* (find-package :cl-user)))
    (setup-cl-user)
    (dolist (file files)
      (when (accept-test-file file)
        (force-output)
        (let ((pid (sb-posix:fork)))
          (cond ((= pid 0)
                 (format t "// Running ~a~%" file)
                 (handler-case
                     (funcall test-fun file)
                   (error (error)
                          (push (list :unhandled-error file) *failures*)
                          (when *break-on-error*
                            (test-util:really-invoke-debugger error))))
                 (report-test-status)
                 (sb-ext:quit :unix-status 104))
                (t
                 (let ((status (make-array 1 :element-type '(signed-byte 32))))
                   (sb-posix:waitpid pid 0 status)
                   (if (and (sb-posix:wifexited (aref status 0))
                            (= (sb-posix:wexitstatus (aref status 0))
                               104))
                       (with-open-file (stream "test-status.lisp-expr"
                                               :direction :input
                                               :if-does-not-exist :error)
                         (append-failures (read stream)))
                       (push (list :invalid-exit-status file)
                             *all-failures*))))))))))

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
  (load file))

(defun cload-test (file)
  (let ((compile-name (compile-file-pathname file)))
    (unwind-protect
         (progn
           (compile-file file)
           (load compile-name))
      (ignore-errors
        (delete-file compile-name)))))

(defun sh-test (file)
  ;; What? No SB-POSIX:EXECV?
  (let ((process (sb-ext:run-program "/bin/sh"
                                     (list (namestring file))
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
