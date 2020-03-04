#!/bin/sh

logdir=${SBCL_PAREXEC_TMP:-$HOME}/sbcl-test-logs-$$
echo ==== Writing logs to $logdir ====
junkdir=${SBCL_PAREXEC_TMP:-/tmp}/junk
mkdir -p $junkdir $logdir

TEST_DIRECTORY=$junkdir SBCL_HOME=../obj/sbcl-home exec ../src/runtime/sbcl \
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
        (aggregate-vop-usage (make-hash-table))
        (missing-usage)
        (losing))
    (labels ((wait ()
               ;; Though far from elegant, this is an easy way to figure out
               ;; which tests are getting stuck, if any are.
               #+nil (format t "Runner is waiting on: ~S~%" subprocess-list)
               (multiple-value-bind (pid status) (sb-posix:wait)
                 (decf subprocess-count)
                 (let ((process (assoc pid subprocess-list)))
                   (setq subprocess-list (delete process subprocess-list))
                   (let* ((code (ash status -8))
                          (filename (cdr process)))
                     (unless (sum-vop-usage (format nil "$logdir/~a.vop-usage" filename) t)
                       (when (or (search ".pure" filename) (search ".impure" filename))
                         (push filename missing-usage)))
                     (cond ((eq code 104)
                            (format t "~A: success~%" filename))
                           (t
                            (format t "~A: status ~D~%" filename code)
                            (push filename losing)))))))
             (sum-vop-usage (input deletep)
               (with-open-file (f input :if-does-not-exist nil)
                 ;; No vop coverage file from shell script tests or any test
                 ;; that internally uses (EXIT) for whatever reason.
                 (when f
                   (loop (let ((line (read-line f nil)))
                           (unless line (return))
                           (let ((count (read-from-string line))
                                 (name (read-from-string line t nil :start 8)))
                             (incf (gethash name aggregate-vop-usage 0) count))))
                   (when deletep (delete-file f))))))
      (dolist (file files)
        (when (>= subprocess-count max-jobs)
          (wait))
        (let ((pid (sb-posix:fork)))
          (when (zerop pid)
            (with-open-file (stream (format nil "$logdir/~a" file)
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
                   (setq sb-c::*static-vop-usage-counts* (make-hash-table))
                   (let ((*features* (cons :parallel-test-runner *features*)))
                     (pure-runner (list (concatenate 'string file ".lisp"))
                                  (if (search "-cload" file) 'cload-test 'load-test)
                                  (make-broadcast-stream)))
                   (with-open-file (output (format nil "$logdir/~a.vop-usage" file)
                                           :direction :output)
                     ;; There's an impure test that screws with the default pprint dispatch
                     ;; table such that integers don't print normally (and can't be parsed).
                     (let ((*print-pretty* nil))
                       (sb-int:dohash ((name count) sb-c::*static-vop-usage-counts*)
                         (format output "~7d ~s~%" count name))))
                   (exit :code (if (unexpected-failures) 1 104)))))
          (format t "~A: pid ~d~%" file pid)
          (incf subprocess-count)
          (push (cons pid file) subprocess-list)))
      (loop (if (plusp subprocess-count) (wait) (return)))

      (dolist (result '("vop-usage.txt" "vop-usage-combined.txt"))
        (let (list)
          (sb-int:dohash ((name vop) sb-c::*backend-template-names*)
            (declare (ignore vop))
            (push (cons (gethash name aggregate-vop-usage 0) name) list))
          (with-open-file (output (format nil "$logdir/~a" result)
                                          :direction :output
                                          :if-exists :supersede)
            (dolist (cell (sort list #'> :key #'car))
              (format output "~7d ~s~%" (car cell) (cdr cell)))))
        (sum-vop-usage "../output/warm-vop-usage.txt" nil))

      (when missing-usage
        (format t "~&Missing vop-usage:~{ ~a~}~%" missing-usage))

      (when losing
        (format t "~&Failing files:~%")
        (dolist (filename losing)
          (format t "~A~%" filename))
        (exit :code 1)))))
(parallel-execute-tests $1)
EOF
