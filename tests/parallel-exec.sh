#!/bin/sh

logdir=${SBCL_PAREXEC_TMP:-$HOME}/sbcl-test-logs-$$
echo ==== Writing logs to $logdir ====
junkdir=${SBCL_PAREXEC_TMP:-/tmp}/junk
mkdir -p $junkdir $logdir

case `uname` in
    CYGWIN* | WindowsNT | MINGW* | MSYS*)
        if [ $# -ne 1 ]
        then
            echo $0: Need arg
            exit 1
        fi
        echo ";; Using -j$1"
        echo "LOGDIR=$logdir" >$logdir/Makefile
        ../run-sbcl.sh --script genmakefile.lisp >>$logdir/Makefile
        exec $GNUMAKE -k -j $1 -f $logdir/Makefile
        ;;
esac

export TEST_DIRECTORY SBCL_HOME
TEST_DIRECTORY=$junkdir SBCL_HOME=../obj/sbcl-home exec ../src/runtime/sbcl \
  --noinform --core ../output/sbcl.core \
  --no-userinit --no-sysinit --noprint --disable-debugger $* << EOF
(pop *posix-argv*)
(require :sb-posix)
(require :sb-sprof)
(let ((*evaluator-mode* :compile))
  (with-compilation-unit () (load"run-tests")))
#+(and x86-64 linux sb-thread)
  (unless (find :gs-seg sb-impl:+internal-features+)
    (push :test-aprof *features*))
(in-package run-tests)
(import '(sb-alien:alien-funcall sb-alien:extern-alien
          sb-alien:int sb-alien:c-string sb-alien:unsigned))
(setq *summarize-test-times* t)
;;; Ordered approximately in descending order by running time
(defvar *slow-tests* '("threads.impure"
                       "seq.impure"
                       "threads.pure"
                       "compiler.pure"
                       "timer.impure"
                       "bug-1180102.impure"
                       "gethash-concurrency.impure"
                       "arith-slow.pure"))
(defun choose-order (tests)
  (sort tests
        (lambda (a b)
          (let ((posn-a (or (position a *slow-tests* :test #'string=)
                            most-positive-fixnum))
                (posn-b (or (position b *slow-tests* :test #'string=)
                            most-positive-fixnum)))
            (cond ((< posn-a posn-b) t)
                  ((> posn-a posn-b) nil)
                  (t (string< a b)))))))
(defun summarize-gc-times ()
  (let (observations)
    (flet ((parse-triple (string pos)
             (sb-int:binding* (((int1 end) (parse-integer string :start (1+ pos)
                                                                 :junk-allowed t))
                               ((int2 end) (parse-integer string :start (1+ end)
                                                                 :junk-allowed t))
                               ((int3) (parse-integer string :start (1+ end)
                                                             :junk-allowed t)))
               (list int1 int2 int3))))
      (dolist (pn (directory "$logdir/*.*"))
        (with-open-file (f pn)
          (let ((legend "GC: stw_delay"))
            (loop
             (let ((line (read-line f nil)))
               (unless line (return))
               (when (and (> (length line) (length legend))
                          (string= line legend :end1 (length legend)))
                 (let* ((p1 (position #\= line))
                        (p2 (position #\= line :start (1+ p1)))
                        (stw-dur (parse-triple line p1))
                        (gc-dur (parse-triple line p2))
                        (count (parse-integer line :start (+ (search "over " line) 5)
                                              :junk-allowed t)))
                   (let ((name (concatenate 'string (pathname-name pn) "."
                                            (pathname-type pn))))
                     (push (list count stw-dur gc-dur name) observations))))))))))
    (let ((fmt "  ~5d (~{~10d~^ ~}) (~{~7d~^ ~}) ~a~%"))
      (format t "~&Top 15 worst by max time to stop-the-world:~%")
      (let ((list (sort (copy-list observations) #'> :key (lambda (x) (third (second x))))))
        (dotimes (i 15) (apply #'format t fmt (pop list))))
      (format t "~&Top 15 worst by avg GC duration (excluding STW delay):~%")
      (let ((list (sort (copy-list observations) #'> :key (lambda (x) (second (third x))))))
        (dotimes (i 15) (apply #'format t fmt (pop list))))
      (format t "~&Top 15 worst by max GC duration (excluding STW delay):~%")
      (let ((list (sort (copy-list observations) #'> :key (lambda (x) (third (third x))))))
        (dotimes (i 15) (apply #'format t fmt (pop list)))))))
(defun parallel-execute-tests (files max-jobs vop-summary-stats-p)
  (format t "Using ~D processes~%" max-jobs)
  ;; Interleave the order in which all tests are launched rather than
  ;; starting them in the batches that filtering places them in.
  (let ((subprocess-count 0)
        (subprocess-list nil)
        (aggregate-vop-usage (make-hash-table))
        ;; Start timing only after all the DIRECTORY calls are done (above)
        (start-time (get-internal-real-time))
        (missing-usage)
        (losing))
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
                     (when vop-summary-stats-p
                       (unless (sum-vop-usage (format nil "$logdir/~a.vop-usage" filename) t)
                         (when (or (search ".pure" filename) (search ".impure" filename))
                           (push filename missing-usage))))
                     (cond ((eq code 104)
                            (format t "~A: success (~d msec)~%" filename et))
                           (t
                            (format t "~A~@[[~d]~]: status ~D (~d msec)~%"
                                      filename iteration code et)
                            (push (list filename iteration pid) losing)))))))
             (elapsed-time-from (when) ; return value in milliseconds
               (round (- (get-internal-real-time) when)
                      (/ internal-time-units-per-second 1000)))
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
            #+(and linux sb-thread 64-bit)
            (sb-alien:alien-funcall (sb-alien:extern-alien "reset_gc_stats"
                                    (function sb-alien:void)))
            ;; FILE is (filename . test-iteration)
            (with-open-file (stream (format nil "$logdir/~a~@[-~d~]" (car file) (cdr file))
                                    :direction :output :if-exists :supersede)
              (alien-funcall (extern-alien "dup2" (function int int int))
                             (sb-sys:fd-stream-fd stream) 1)
              (alien-funcall (extern-alien "dup2" (function int int int)) 1 2))
            (setq file (car file))
            #+test-aprof
            (unless (search "allocator.pure" file)
              (sb-aprof::aprof-start)
              (proclaim '(optimize sb-c:instrument-consing)))
            ;; Send this to the log file, not the terminal
            (setq *debug-io* (make-two-way-stream (make-concatenated-stream)
                                                  *error-output*))
            (cond ((string= (pathname-type file) "test")
                   (let ((shell (or #+sunos (posix-getenv "SHELL") "/bin/sh")))
                     ;; exec the shell with the test and we'll pick up its exit code
                     (alien-funcall (extern-alien "execl" (function int c-string c-string
                                                                    &optional c-string unsigned))
                                    shell shell
                                    (concatenate 'string file ".sh") 0))
                   ;; if exec fails, just exit with a wrong (not 104) status
                   (alien-funcall (extern-alien "_exit" (function (values) int)) 0))
                  (t
                   (sb-sprof:start-profiling :sample-interval .001)
                   (setq sb-c::*static-vop-usage-counts* (make-hash-table :synchronized t))
                   (let ((*features* (cons :parallel-test-runner *features*)))
                     (pure-runner (list (concatenate 'string file ".lisp"))
                                  (if (search "-cload" file) 'cload-test 'load-test)
                                  (make-broadcast-stream)))
                   (when vop-summary-stats-p
                     (with-open-file (output (format nil "$logdir/~a.vop-usage" file)
                                             :direction :output)
                       ;; There's an impure test that screws with the default pprint dispatch
                       ;; table such that integers don't print normally (and can't be parsed).
                       (let ((*print-pretty* nil))
                         (sb-int:dohash ((name count) sb-c::*static-vop-usage-counts*)
                           (format output "~7d ~s~%" count name)))))
                   (sb-sprof:stop-profiling)
                   #+test-aprof (progn (sb-aprof::aprof-stop) (sb-aprof:aprof-show))
                   (when (member :allocator-metrics sb-impl:+internal-features+)
                     (format t "~2&Allocator histogram:~%")
                     (funcall (intern "PRINT-ALLOCATOR-HISTOGRAM" "SB-THREAD")))
                   (sb-sprof:report :type :flat)
                   (exit :code (if (unexpected-failures) 1 104)))))
          (format t "~A: pid ~d~@[ (trial ~d)~]~%" (car file) pid (cdr file))
          (incf subprocess-count)
          (push (list pid file (get-internal-real-time)) subprocess-list)))
      (loop (if (plusp subprocess-count) (wait) (return)))

      (when vop-summary-stats-p
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
          (sum-vop-usage "../output/warm-vop-usage.txt" nil)))

      (format t "~&Total realtime: ~d msec~%" (elapsed-time-from start-time))
      (when missing-usage
        (format t "~&Missing vop-usage:~{ ~a~}~%" missing-usage))

      (when losing
        (format t "~&Failing files:~%")
        (dolist (filename losing)
          (format t "~A~%" filename))
        (format t "==== Logs are in $logdir ====~%")
        (exit :code 1)))))
(if (= (length *posix-argv*) 1)
    ;; short form - all files, argument N is the number of parallel tasks
    (let ((jobs (parse-integer (car *posix-argv*))))
      (parallel-execute-tests
       (mapcar #'list
               (choose-order
                (mapcar #'pathname-name
                        (append (pure-load-files)
                                (pure-cload-files)
                                (impure-load-files)
                                (impure-cload-files)
                                (sh-files)))))
       jobs
       t)
      #+(and linux sb-thread 64-bit) (summarize-gc-times))
    ;; long form
    (let ((jobs 4)
          (runs-per-test 1)
          (argv *posix-argv*))
      (loop (cond ((string= (car argv) "-j")
                   (setq jobs (parse-integer (cadr argv))
                         argv (cddr argv)))
                  ((string= (car argv) "--runs_per_test")
                   (setq runs-per-test (parse-integer (cadr argv))
                         argv (cddr argv)))
                  (t
                   (return))))
      (setq argv
        (mapcar (lambda (file)
                  (probe-file file) ; for effect
                  (pathname-name file)) argv))
      (parallel-execute-tests
       (loop for trial-number from 1 to runs-per-test
             nconc (mapcar (lambda (file)
                             (cons file
                                   (when (> runs-per-test 1) trial-number)))
                           argv))
       jobs
       nil)))
EOF
