(defvar *test-evaluator-mode* :compile)

(defun clear-test-status ()
  (with-open-file (stream "test-status.lisp-expr"
                          :direction :output
                          :if-exists :supersede)
    (write-line "NIL" stream)))

;;; Only the pure load-only (vs. compile+load) tests will be run concurrently.
;;; The impure ones won't because we don't know what side-effects they depend on
;;; (they are stateful with regard to order within the file).
;;; Potentially the pure '.cload' tests could work.
(defun load-test (file)
  (let ((test-util::*deferred-test-forms*))
    (declare (special test-util::*deferred-test-forms*))
    (makunbound 'test-util::*deferred-test-forms*)
    (load file :external-format :utf-8)
    (when (boundp 'test-util::*deferred-test-forms*)
      ;; Execute all tests that were wrapped in WITH-TEST
      (let ((holder test-util::*deferred-test-forms*))
        ;; Sort the slow tests in front of non-slow tests
        (setf (elt holder 1) (nconc (nreverse (elt holder 1))
                                    (nreverse (elt holder 2)))
              (elt holder 2) nil)
        (let ((n (elt holder 0))
              (threads))
          (format t "~&// Enabling ~D threads~%" n)
          (dotimes (i n)
            (push (sb-thread:make-thread
                   (lambda ()
                     (loop
                      (let ((test (atomic-pop (svref holder 1))))
                        (unless test (return))
                        (test-util::run-test-concurrently test)))))
                  threads))
          (dolist (thr threads)
            (sb-thread:join-thread thr)))))))

(defun cload-test (file)
  (let ((compile-name (compile-file-pathname file)))
    (unwind-protect
         (progn
           (compile-file file :print nil)
           (load compile-name))
      (ignore-errors
       (delete-file compile-name)))))

(defun sh-test (file)
  (clear-test-status)
  (progn
    (sb-posix:setenv "TEST_SBCL_EVALUATOR_MODE"
                     (string-downcase *test-evaluator-mode*)
                     1)
    (let ((process (sb-ext:run-program "/bin/sh"
                                       (list (native-namestring file))
                                       :output *error-output*)))
      (let ((*failures* nil))
        (test-util:report-test-status))
      (sb-ext:exit :code (process-exit-code process)))))
