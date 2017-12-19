(defvar *test-evaluator-mode* :compile)

(defun clear-test-status ()
  (with-open-file (stream "test-status.lisp-expr"
                          :direction :output
                          :if-exists :supersede)
    (write-line "NIL" stream)))

(defun load-test (file)
  (load file :external-format :utf-8))

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
