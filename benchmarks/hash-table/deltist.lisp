;;;; TODO
;;;;
;;;; - Even with interleaving, there is some unexplained variance
;;;;   left.
;;;;
;;;; - Outliers? Real/CPU anomalies? It's impossible to detect
;;;;   outliers without modelling the source.
;;;;
;;;; - If the RSE cannot decrease below MAX-RSE (given the current
;;;;   MAX-RUNS), then either stop early or discard the most serious
;;;;   outliers.
;;;;
;;;; - Why is max-rse based on real time?
;;;;
;;;; - Skewed distribution
;;;;
;;;; - Add parallel option (as opposed to interleaved)?
;;;;
;;;; - Track other statistics (e.g. rank (estimate the probability of
;;;;   each rank))
;;;;
;;;; - Rerun a subset of benchmarks based on a previous run (e.g.
;;;;   where the biggest differences were)
;;;;
;;;; - Detach from controlling terminal (so that output to
;;;;   *TERMINAL-IO* is redirected to stdout, and stdin is not /dev/tty)
;;;;   or use the script command?
;;;;
;;;; - Track statistics with outliers too?
;;;;
;;;; - Document
;;;;
;;;; - How to handle failures in benchmarks?
;;;;
;;;; - Timeouts
;;;;
;;;; - Use better clocks (e.g. clock_gettime?)
;;;;
;;;; - Better estimate measurement overhead
;;;;
;;;; - Differential timing? (https://www.youtube.com/watch?v=vrfYLlR8X8k&t=914s)
;;;;
;;;; - DEFTESTlike macro?
;;;;
;;;; - Handle failures (<= 1 exit-code 127)?
;;;;
;;;; - Measurement granularity vs max-rse
;;;;
;;;;
;;;; ---- Generalization -----
;;;;
;;;; - Allow arbitrary statistics
;;;;
;;;; - Multiple measurements in one FUNCALL / RUN-PROGRAM? (To reduce
;;;;   the RUN-PROGRAM overhead) (network protocol?)
;;;;
;;;;
;;;; ----- Output -----
;;;;
;;;; - For each benchmark, print a copy-pasteable shell command to
;;;;   rerun it.
;;;;
;;;; - Machine-readable output
;;;;
;;;; - Select output columns
;;;;
;;;; - Control format (e.g. number of float digits)
;;;;
;;;; - Streaming table output (grow/resize columns as necessary)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :alexandria)
  (require :split-sequence))


;;;; KLUDGE: Redefine SB-SYS::GET-SYSTEM-INFO (used by
;;;; SB-EXT:CALL-WITH-TIMING below) with SB-UNIX:RUSAGE_CHILDREN
;;;; instead of SB-UNIX:RUSAGE_SELF.

(defmacro without-redefinition-warnings (&body body)
  #+sbcl
  `(locally
       (declare (sb-ext:muffle-conditions sb-kernel:redefinition-warning))
     (handler-bind ((sb-kernel:redefinition-warning #'muffle-warning))
       ,@body))
  #-sbcl
  `(progn ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:unlock-package :sb-sys))

(without-redefinition-warnings
  sb-sys::
  (defun get-system-info ()
    (multiple-value-bind
          (err? utime stime maxrss ixrss idrss isrss minflt majflt)
        ;; FIXME: This should sometimes be sb-unix:rusage_self or
        ;; thread.
        (sb-unix:unix-getrusage sb-unix:rusage_children)
      (declare (ignore maxrss ixrss idrss isrss minflt))
      (unless err?       ; FIXME: nonmnemonic (reversed) name for ERR?
        (error "Unix system call getrusage failed: ~A." (strerror utime)))
      (values utime stime majflt))))


;;;; Timing

(defun timing-value (timing key)
  (let ((x (if (functionp key)
               (funcall key timing)
               (getf timing key))))
    (or (if (consp x)
            (car x)
            x)
        0)))

;;; This is the variance of the measurement (0 by default). Currently
;;; only used when multiple timings averaged and their sample variance
;;; is estimated (see ESTIMATE-MEAN).
(defun timing-uncertainty (timing key)
  (let ((x (if (functionp key)
               (funcall key timing)
               (getf timing key))))
    (or (and (consp x)
             (cdr x))
        0)))

(defun make-timing (plist)
  (let ((plist (copy-list plist)))
    (setf (getf plist :run-time-us)
          (cons (+ (timing-value plist :user-run-time-us)
                   (timing-value plist :system-run-time-us))
                (+ (timing-uncertainty plist :user-run-time-us)
                   (timing-uncertainty plist :system-run-time-us))))
    plist))

(defun timing-n-measurements (timing)
  (getf timing :n-measurements 1))

(defun timings-mean (timings key)
  (let ((sum 0))
    (map nil (lambda (timing)
               (incf sum (timing-value timing key)))
         timings)
    (/ sum (length timings))))

(defun timings-variance (timings key)
  (let ((mean (timings-mean timings key))
        (sum 0)
        (sum-variances 0))
    (map nil (lambda (timing)
               (let ((x (timing-value timing key)))
                 (incf sum (expt (- x mean) 2))
                 (incf sum-variances (timing-uncertainty timing key))))
         timings)
    (+ (/ sum (length timings))
       sum-variances)))

(defun timing-rse (timing)
  (/ (sqrt (timing-uncertainty timing :real-time-ms))
     (+ (timing-value timing :real-time-ms) 1e-7)))

;;; Return a timing whose TIMING-VALUEs are the estimated means of
;;; TIMINGS, and whose TIMING-UNCERTAINTYs are the estimated variance
;;; of the mean estimate. TIMINGS must be i.i.d. measurements (FIXME).
(defun estimate-mean (timings)
  (let ((keys (loop for rest on (first timings) by #'cddr
                    collect (first rest)))
        (n-measurements (loop for timing in timings
                              sum (timing-n-measurements timing))))
    (list* :n-measurements n-measurements
           (loop for key in keys
                 unless (eq key :n-measurements)
                 append (list key (cons (timings-mean timings key)
                                        (/ (timings-variance timings key)
                                           n-measurements)))))))

;;; Sum the independent random variables TIMINGS (both their means and
;;; variances are summed). They need not be identically distributed.
(defun sum-timings (timings)
  (let ((keys (loop for rest on (first timings) by #'cddr
                    collect (first rest))))
    (list* :n-measurements 1
           (loop for key in keys
                 unless (eq key :n-measurements)
                   append (list key
                                (cons (loop for timing in timings
                                            sum (timing-value timing key))
                                      (loop for timing in timings
                                            sum (timing-uncertainty timing
                                                                    key))))))))

(defvar *time-unit* 1)

(defun print-timing (timing &key (time-unit *time-unit*))
  (let ((n-measurements (timing-n-measurements timing)))
    (flet ((value (key)
             (timing-value timing key))
           (uncertainty (key)
             (timing-uncertainty timing key))
           (print-real-or-run-time (mean stddev)
             (format t "~7,3F" mean)
             (if (or (/= n-measurements 1) (/= stddev 0))
                 (format t " ~6,2F% ~7,3F"
                         ;; Relative Standard Error (stddev of our
                         ;; estimate of the mean)
                         (if (zerop mean)
                             0
                             (* 100 (/ stddev mean)))
                         ;; Biased sample stddev
                         (* (sqrt n-measurements) stddev))
                 (format t "                "))))
      (print-real-or-run-time (/ (value :real-time-ms) 1000 time-unit)
                              (/ (sqrt (uncertainty :real-time-ms))
                                 1000 time-unit))
      (format t " ")
      (print-real-or-run-time (/ (value :run-time-us) 1000000 time-unit)
                              (/ (sqrt (uncertainty :run-time-us))
                                 1000000 time-unit))
      (format t " (~7,3F + ~7,3F~@[, ~7,3F~])~%"
              (/ (value :user-run-time-us) 1000000 time-unit)
              (/ (value :system-run-time-us) 1000000 time-unit)
              (and (getf timing :gc-run-time-ms)
                   (/ (value :gc-run-time-ms) 1000 time-unit))))))


;;; Return the maximum Relative Standard Deviation (sqrt(var) / abs(mean)).
(defun max-rsd (means variances)
  (loop for m across means
        for v across variances
        maximize (/ (sqrt v) (abs m))))

;;; TIMINGS-VECTOR holds i.i.d. timings.
(defun max-rse-of-timings (timings-vector)
  (let ((means (map 'vector
                    (lambda (timings)
                      (timings-mean timings :real-time-ms))
                    timings-vector))
        (variances (map 'vector
                        (lambda (timings)
                          (timings-variance timings :real-time-ms))
                        timings-vector)))
    (/ (max-rsd means variances)
       (sqrt (length (aref timings-vector 0))))))

(defun print-heading (serial-no measure-gc)
  (if serial-no
      (format t "#~2D" serial-no)
      (format t "   "))
  (format t " cmd    real  +-rse%  stddev     cpu  +-rse%  stddev ~
             (   user +     sys~:[~;,      gc~])~%"
          measure-gc))

(defun timing-filter-gc (timing measure-gc)
  (if measure-gc
      timing
      (let ((timing (copy-list timing)))
        (remf timing :gc-run-time-ms)
        timing)))

(defun run-program* (command)
  (uiop:with-temporary-file (:pathname output-file)
    (multiple-value-bind (output error-output exit-code)
        (uiop:run-program command :ignore-error-status t :output output-file
                                  :error-output output-file)
      (declare (ignore output error-output))
      (unless (zerop exit-code)
        (format t "Exit code ~S from command ~A~%Output:~%~A~%"
                exit-code command
                (alexandria:read-file-into-string output-file))
        (sb-ext:exit :code exit-code :abort t)))))

(defun command-name (command-names i)
  (if (< i (length command-names))
      (format nil "~2@A" (elt command-names i))
      (format nil "~2D" (1+ i))))

(defun commands-to-functions (commands command-names)
  (loop for command in commands
        for i upfrom 0
        do (format t "Command ~A: ~A~%" (command-name command-names i)
                   command)
        collect (if (stringp command)
                    (let ((command command))
                      (lambda ()
                        (run-program* command)))
                    command)))

(defun time-interleaved (commands &key command-names (warmup 0) (runs 10)
                                    max-runs max-rse shuffle
                                    (measure-gc t) (time-unit *time-unit*))
  (let* ((fns (commands-to-functions commands command-names))
         (n-commands (length fns))
         (timings (make-array n-commands :initial-element ()))
         (*time-unit* time-unit))
    (flet ((command-indices ()
             (let ((indices (alexandria:iota n-commands)))
               (if shuffle
                   (alexandria:shuffle indices)
                   indices)))
           (print-command-name (command-index kind)
             (format t "~A ~A " (ecase kind
                                  ((:maybe-shuffled)
                                   (if shuffle "shuf" "    "))
                                  ((:ordered) "    ")
                                  ((:mean) "mean"))
                     (command-name command-names command-index))
             (force-output))
           (no-more-runs-p (run timings)
             (and (or (null max-runs) (< run max-runs))
                  (or (< run runs)
                      (and max-rse (or (zerop run)
                                       (< max-rse (max-rse-of-timings
                                                   timings))))))))
      (format t "~%Warming up~%")
      (loop for run below warmup do
        (terpri)
        (print-heading (1+ run) measure-gc)
        (loop for i in (command-indices)
              do (print-command-name i :maybe-shuffled)
                 (sb-ext:call-with-timing
                  (lambda (&rest timing)
                    (print-timing (timing-filter-gc (make-timing timing)
                                                    measure-gc)))
                  (elt fns i))))
      (format t "~%Benchmarking~%")
      (loop for run upfrom 0
            while (no-more-runs-p run timings)
            do (terpri)
               (print-heading (1+ run) measure-gc)
               (loop for i in (command-indices)
                     do (let ((fn (elt fns i)))
                          (print-command-name i :maybe-shuffled)
                          (sb-ext:call-with-timing
                           (lambda (&rest timing)
                             (let ((timing (timing-filter-gc
                                            (make-timing timing) measure-gc)))
                               (print-timing timing)
                               (push timing (aref timings i))))
                           fn)))
               (when shuffle
                 (loop for i below n-commands
                       do (print-command-name i :ordered)
                          (print-timing (first (aref timings i)))))
               (loop for i below n-commands
                     do (print-command-name i :mean)
                        (print-timing (estimate-mean (aref timings i))))))
    (map 'list #'estimate-mean timings)))

#+nil
(time-interleaved (list (lambda () (sleep (+ 0.075 (random 0.05))))
                        (lambda () (sleep (+ 0.175 (random 0.05))))))


(defun blank-char-p (char)
  (member char '(#\Space #\Tab)))

(defun blank-string-p (string)
  (every #'blank-char-p string))

(defun parse-benchmark-file (stream)
  (let ((benchmarks ()))
    (loop
      (let ((lines (loop for line = (read-line stream nil nil)
                         while (and line (not (blank-string-p line)))
                         collect line)))
        (unless lines
          (return))
        (push (list :commands lines) benchmarks)))
    (reverse benchmarks)))

(defun benchmark-weight (benchmark)
  (getf benchmark :weight 1))

(defun benchmark-commands (benchmark)
  (getf benchmark :commands))

(defun time-sequentially (benchmarks time-it
                          &key command-names shuffle max-rse skip-high-rse
                            (measure-gc t) (time-unit *time-unit*))
  (let* ((benchmarks (if shuffle (alexandria:shuffle benchmarks) benchmarks))
         (n-commands (length (benchmark-commands (first benchmarks))))
         (command-timings (make-array n-commands :initial-element ()))
         (skipp (and skip-high-rse max-rse))
         ;; Contains even skipped benchmarks.
         (command-timings/skip (when skipp
                                 (make-array n-commands :initial-element ())))
         (n-skipped 0))
    (flet ((print-command-totals (command-timings)
             (loop for i below n-commands
                   do (format t "tot. ~A " (command-name command-names i))
                      (print-timing (sum-timings (aref command-timings i))
                                    :time-unit time-unit))))
      (loop for benchmark in benchmarks
            do (assert (= (length (benchmark-commands benchmark)) n-commands)))
      (loop
        for benchmark-index upfrom 0
        for benchmark in benchmarks
        do (format t "~%Benchmark ~S~%" (1+ benchmark-index))
           (let* ((timings (funcall time-it (benchmark-commands benchmark)))
                  (rse (loop for timing in timings
                             maximize (timing-rse timing)))
                  (skip-this-p (and skipp (< max-rse rse))))
             (when skip-this-p
               (format t "~%Final RSE too high. ~
                         Skipping results of benchmark ~D.~%"
                       (1+ benchmark-index))
               (incf n-skipped))
             (loop for i upfrom 0
                   for timing in timings
                   do (unless skip-this-p
                        (push timing (aref command-timings i)))
                      (push timing (aref command-timings/skip i)))
             ;; Print totals without skipped
             (format t "~%Totals after benchmark ~S" (1+ benchmark-index))
             (when (plusp n-skipped)
               (format t " (excluding ~S skipped)"  n-skipped))
             (terpri)
             (print-heading nil measure-gc)
             (print-command-totals command-timings)
             ;; Print totals with skipped if any
             (when (plusp n-skipped)
               (format t "~%Totals after benchmark ~S (including ~S skipped)~%"
                       (1+ benchmark-index) n-skipped)
               (print-heading nil measure-gc)
               (print-command-totals command-timings/skip)))))))
#+nil
(time-sequentially `((:commands ,(list (lambda () (sleep 0.1))
                                       (lambda () (sleep 0.2))))
                     (:commands ,(list (lambda () (sleep 0.01))
                                       (lambda () (sleep 0.02)))))
                   #'time-interleaved)


;;;; Command line

(defun parse-arguments (args)
  (let ((options ()))
    (loop until (endp args) do
      (let ((arg (pop args)))
        (unless (alexandria:starts-with-subseq "-" arg)
          (push arg args)
          (return))
        (when (string= arg "--")
          (return))
        (let ((value (pop args)))
          (push (cons arg value) options))))
    (values (reverse options) args)))

(defun get-option-string-value (options option-name &optional default)
  (let ((value (alexandria:assoc-value options option-name :test #'string=)))
    (or value default)))

(defun get-option-integer-value (options option-name &optional default)
  (let ((value (alexandria:assoc-value options option-name :test #'string=)))
    (if value
        (parse-integer value)
        default)))

(defun get-option-boolean-value (options option-name &optional default)
  (let ((x (get-option-integer-value options option-name nil)))
    (if x
        (not (zerop x))
        default)))

(defun get-option-real-value (options option-name &optional default)
  (let ((value (alexandria:assoc-value options option-name :test #'string=)))
    (if value
        (let ((*read-eval* nil))
          (the real (read-from-string value)))
        default)))

#+nil (parse-arguments '("-w" "1" "-r" "10" "--" "a" "b" "c"))
#+nil (parse-arguments '("a" "b" "c"))

(defun deltist ()
  (handler-case
      (multiple-value-bind (options commands)
          (parse-arguments (rest sb-ext:*posix-argv*))
        (let ((*random-state* (make-random-state t))
              (warmup (get-option-integer-value options "--warmup" 1))
              (runs (get-option-integer-value options "--runs" 10))
              (max-runs (get-option-integer-value options "--max-runs" 40))
              (max-rse (get-option-real-value options "--max-rse" 0.001))
              (shuffle (get-option-boolean-value options "--shuffle" nil))
              (time-unit (get-option-real-value options "--time-unit" 1))
              (command-names
                (split-sequence:split-sequence
                 #\Space (get-option-string-value options "--command-names"
                                                  nil)
                 :remove-empty-subseqs t))
              (benchmark-file (get-option-string-value options
                                                       "--benchmark-file"
                                                       nil))
              (shuffle-benchmarks (get-option-boolean-value
                                   options "--shuffle-benchmarks" nil))
              (skip-high-rse (get-option-boolean-value
                              options "--skip-high-rse" nil)))
          (format t "~%warmup: ~S, runs: ~S, max-runs: ~S, max-rse: ~,1F%, ~
                     shuffle: ~S, time-unit: ~Fs,~%~
                     command-names: ~A, benchmark-file: ~S, ~
                     shuffle-benchmarks: ~S, skip-high-rse: ~S~%"
                  warmup runs max-runs (* 100 max-rse) shuffle time-unit
                  command-names benchmark-file shuffle-benchmarks skip-high-rse)
          (flet ((time-it (commands)
                   (time-interleaved commands
                                     :command-names command-names
                                     :warmup warmup :runs runs
                                     :max-runs max-runs :max-rse max-rse
                                     :shuffle shuffle
                                     :measure-gc nil :time-unit time-unit)))
            (when commands
              (time-it commands))
            (when benchmark-file
              (time-sequentially (if (zerop (length benchmark-file))
                                     (parse-benchmark-file *standard-input*)
                                     (with-open-file (s benchmark-file)
                                       (parse-benchmark-file s)))
                                 #'time-it
                                 :command-names command-names
                                 :shuffle shuffle-benchmarks
                                 :max-rse max-rse
                                 :skip-high-rse skip-high-rse
                                 :measure-gc nil)))
          (format t "~%Done.~%")))
    ((or sb-sys:interactive-interrupt sb-int:broken-pipe) ()
      ;; 130=128+SIGINT
      (sb-ext:exit :code 130 :abort t))))
