#!/bin/sh

mkdir -p /var/tmp/junk /var/tmp/sbcl-test-logs
TEST_DIRECTORY=/var/tmp/junk SBCL_HOME=../obj/sbcl-home exec ../src/runtime/sbcl \
  --noinform --core ../output/sbcl.core --no-userinit --no-sysinit --noprint --disable-debugger << EOF
(require :sb-posix)
(let ((*evaluator-mode* :compile))
  (with-compilation-unit () (load"run-tests")))
(in-package run-tests)
(import '(sb-alien:alien-funcall sb-alien:extern-alien
          sb-alien:int sb-alien:c-string sb-alien:unsigned))
(setq *summarize-test-times* t)
(defun parallel-execute-tests (max-jobs)
  (format t "Using ~D processes~%" max-jobs)
  ;; Interleave the order in which all tests are launched rather than
  ;; starting them in the batches that filtering places them in.
  (let ((files (sort (mapcar #'pathname-name
                             (append (pure-load-files)
                                     (pure-cload-files)
                                     (impure-load-files)
                                     (impure-cload-files)
                                     (sh-files)))
                     #'string<))
        (subprocess-count 0)
        (subprocess-list nil)
        (losing))
    (flet ((wait ()
             (multiple-value-bind (pid status) (sb-posix:wait)
               (decf subprocess-count)
               (let ((process (assoc pid subprocess-list)))
                 (setq subprocess-list (delete process subprocess-list))
                 (let* ((code (ash status -8))
                        (filename (cdr process)))
                   (cond ((eq code 104)
                          (format t "~A: success~%" filename))
                         (t
                          (format t "~A: status ~D~%" filename code)
                          (push filename losing))))))))
      (dolist (file files)
        (when (>= subprocess-count max-jobs)
          (wait))
        (let ((pid (sb-posix:fork)))
          (when (zerop pid)
            (with-open-file (stream (format nil "/var/tmp/sbcl-test-logs/~a" file)
                                    :direction :output :if-exists :supersede)
              (alien-funcall (extern-alien "dup2" (function int int int))
                             (sb-sys:fd-stream-fd stream) 1)
              (alien-funcall (extern-alien "dup2" (function int int int)) 1 2))
            ;; Send this to the log file, not the terminal
            (setq *debug-io* (make-two-way-stream (make-concatenated-stream)
                                                  *error-output*))
            (cond ((string= (pathname-type file) "test")
                   ;; exec /bin/sh with the test and we'll pick up its exit code
                   (alien-funcall (extern-alien "execl" (function int c-string c-string
                                                                  c-string unsigned))
                                  "/bin/sh" "/bin/sh"
                                  (concatenate 'string file ".sh") 0)
                   ;; if exec fails, just exit with a wrong (not 104) status
                   (alien-funcall (extern-alien "_exit" (function (values) int)) 0))
                  (t
                   (pure-runner (list (concatenate 'string file ".lisp"))
                                (if (search "-cload" file) 'cload-test 'load-test)
                                (make-broadcast-stream))
                   (exit :code (if (unexpected-failures) 1 104)))))
          (format t "~A: pid ~d~%" file pid)
          (incf subprocess-count)
          (push (cons pid file) subprocess-list)))
      (loop (if (plusp subprocess-count) (wait) (return)))
      (when losing
        (terpri)
        (format t "Failing files:~%")
        (dolist (filename losing)
          (format t "~A~%" filename))
        (exit :code 1)))))
(parallel-execute-tests $1)
EOF
