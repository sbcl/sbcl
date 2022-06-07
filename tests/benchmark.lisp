
(let ((*evaluator-mode* :compile))
 (with-compilation-unit () (load"run-tests")))

(in-package run-tests)
(pop *posix-argv*)
(defvar *logdir* (pop *posix-argv*))
(defvar *njobs* (parse-integer (pop *posix-argv*)))
(require :sb-posix)

(import '(sb-alien:alien-funcall sb-alien:extern-alien
          sb-alien:int sb-alien:c-string sb-alien:unsigned))

;;; Ordered approximately in descending order by running time
(defvar *slow-tests* '("threads.impure"
                       "seq.impure"
                       "threads.pure"
                       "compiler.pure"
                       "timer.impure"
                       "bug-1180102.impure"
                       "gethash-concurrency.impure"
                       "arith-slow.pure"))

(defvar *filter* nil)
(defun choose-order (tests)
  (when *filter*
    (let (strings)
       (with-open-file (file *filter*)
         (loop (let ((line (read-line file nil)))
                 (if line (push line strings) (return)))))
       (setq tests (remove-if (lambda (x) (not (find x strings :test #'string=)))
                              tests))))
  (sort tests
        (lambda (a b)
          (let ((posn-a (or (position a *slow-tests* :test #'string=)
                            most-positive-fixnum))
                (posn-b (or (position b *slow-tests* :test #'string=)
                            most-positive-fixnum)))
            (cond ((< posn-a posn-b) t)
                  ((> posn-a posn-b) nil)
                  (t (string< a b)))))))

(defun output-metrics (c1 c2 c3 c4 testfile comment metricsfile)
  (let ((summary
         ;; FORMAT NIL produces simple-base-string when it can
         (format nil "(~10d ~10d ~10d ~10d ~s ~s)~%"
                 c1
                 c2
                 c3
                 c4
                 testfile
                 comment)))
    (sb-unix:unix-write metricsfile summary 0 (length summary))))

;; Conversion factor from nanoseconds to internal-time-units
(defconstant divisor (/ 1000000000 internal-time-units-per-second))

(defun sort-metrics (filename)
  (let (original-list)
    (with-open-file (f (make-pathname :type "lisp-expr" :defaults filename))
      (loop (let ((expr (read f nil)))
              (unless expr (return))
              (destructuring-bind (a b c d name pf) expr
                (let ((vt-less-compiler (- d (round (+ a b) divisor))))
                  (push (cons vt-less-compiler expr) original-list))))))
    (with-open-file (f (make-pathname :type "txt" :defaults filename)
                       :direction :output :if-exists :supersede)
      (format f "
; Version: ~A
; Parallelism: ~D
; Files: ~D
;
;  COMPILE-     COMPILE        GC       Total      Less
;      FILE                                      compiler
;  (thread)    (thread)             (process)
;   virtual     virtual      real     virtual     virtual
; ---------  ----------  --------  ----------  ----------
" (lisp-implementation-version) *njobs* (length original-list))

      (let ((c1 0) (c2 0) (c3 0) (c4 0) (c5 0))
        (dolist (line (sort original-list #'> :key #'car))
          (destructuring-bind (net cfile compile gc process filename pf) line
            ;; get-internal-run-time returns microseconds
            ;; but we've collected the compiler time in nanoseconds
            (setq cfile (round cfile divisor)
                  compile (round compile divisor))
            (format f "~11d~12d~10d~12d~12d ~A~A~%"
                    cfile compile gc process net filename (if (eql pf :fail) " FAIL" ""))
            (incf c1 cfile)
            (incf c2 compile)
            (incf c3 gc)
            (incf c4 process)
            (incf c5 net)))
        (format f "~11d~12d~10d~12d~12d TOTAL~%"
                c1 c2 c3 c4 c5)))))

(defun parallel-execute-tests (files &optional (max-jobs *njobs*))
  (format t "Using ~D processes~%" max-jobs)
  (let* ((subprocess-count 0)
         (subprocess-list nil)
         ;; Start timing only after all the DIRECTORY calls are done (above)
         (start-time (get-internal-real-time))
         (missing-usage)
         (losing)
         (metricsfilename (format nil "~A/performance" *logdir*))
         (metricsfile
          (sb-unix:unix-open (namestring (make-pathname :type "lisp-expr"
                                                        :defaults metricsfilename))
                             (logior sb-unix:o_creat sb-unix:o_wronly sb-unix:o_append)
                             #o666)))
    (labels ((wait ()
               ;; Though far from elegant, this is an easy way to figure out
               ;; which tests are getting stuck, if any are.
               #+nil (format t "Runner is waiting on: ~S~%" subprocess-list)
               (multiple-value-bind (pid status) (sb-posix:wait)
                 (decf subprocess-count)
                 (let ((process (assoc pid subprocess-list))
                       (code (ash status -8))
                       (et))
                   (unless process
                     (warn "Whoa! Process ~D is an unexpected child" pid)
                     (return-from wait (wait)))
                   (setq subprocess-list (delete process subprocess-list))
                   (destructuring-bind ((filename . iteration) start-time) (cdr process)
                     (setq et (elapsed-time-from start-time))
                     (cond ((eq code 104)
                            (format t "~A: success (~d µsec)~%" filename et))
                           (t
                            (format t "~A~@[[~d]~]: status ~D (~d µsec)~%"
                                      filename iteration code et)
                            (push (list filename iteration pid) losing)))))))
             (elapsed-time-from (when) ; return value in microseconds
               (round (- (get-internal-real-time) when)
                      (/ internal-time-units-per-second 1000000))))
      (dolist (file files)
        (when (>= subprocess-count max-jobs)
          (wait))
        (let ((pid (sb-posix:fork)))
          (when (zerop pid)
            ;; FILE is (filename . test-iteration)
            (with-open-file (stream (format nil "~A/~a~@[-~d~]"
                                            *logdir* (car file) (cdr file))
                                    :direction :output :if-exists :supersede)
              (alien-funcall (extern-alien "dup2" (function int int int))
                             (sb-sys:fd-stream-fd stream) 1)
              (alien-funcall (extern-alien "dup2" (function int int int)) 1 2))
            (setq file (car file))
            ;; Send this to the log file, not the terminal
            (setq *debug-io* (make-two-way-stream (make-concatenated-stream)
                                                  *error-output*))
            (let ((*features* (cons :parallel-test-runner *features*))
                  (run-time-start (get-internal-run-time)))
              (pure-runner (list (concatenate 'string file ".lisp"))
                           (if (search "-cload" file) 'cload-test 'load-test)
                           (make-broadcast-stream))
              (output-metrics sb-c::*compile-file-elapsed-time*
                              sb-c::*compile-elapsed-time*
                              *gc-run-time*
                              (- (get-internal-run-time) run-time-start)
                              file
                              (if (unexpected-failures) :fail :pass)
                              metricsfile)
              (exit :code (if (unexpected-failures) 1 104))))
          (format t "~A: pid ~d~@[ (trial ~d)~]~%" (car file) pid (cdr file))
          (incf subprocess-count)
          (push (list pid file (get-internal-real-time)) subprocess-list)))
      (loop (if (plusp subprocess-count) (wait) (return)))
      (format t "~&Total realtime: ~,,-6f sec~%" (elapsed-time-from start-time))
      (sb-unix:unix-close metricsfile)
      (sort-metrics metricsfilename)
      (when losing
        (format t "~&Failing files:~%")
        (dolist (filename losing)
          (format t "~A~%" filename))
        (format t "==== Logs are in ~A ====~%" *logdir*)
        (exit :code 1)))))

(parallel-execute-tests
 (mapcar #'list
         (choose-order
          (mapcar #'pathname-name
                  (append (pure-load-files)
                          (pure-cload-files)
                          (impure-load-files)
                          (impure-cload-files))))))
