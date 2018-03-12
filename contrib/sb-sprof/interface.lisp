;;;; User interface of the statistical profiler
;;;;
;;;; Copyright (C) 2003 Gerd Moellmann <gerd.moellmann@t-online.de>
;;;; All rights reserved.

(in-package #:sb-sprof)

(defvar *sample-interval* 0.01
  "Default number of seconds between samples.")
(declaim (type (real (0)) *sample-interval*))

(defvar *alloc-interval* 4
  "Default number of allocation region openings between samples.")
(declaim (type (integer (0)) *alloc-interval*))

(defvar *max-samples* 50000
  "Default number of traces taken. This variable is somewhat misnamed:
each trace may actually consist of an arbitrary number of samples, depending
on the depth of the call stack.")
(declaim (type sb-int:index *max-samples*))

(defvar *sampling-mode* :cpu
  "Default sampling mode. :CPU for cpu profiling, :ALLOC for allocation
profiling, and :TIME for wallclock profiling.")
(declaim (type sampling-mode *sampling-mode*))

(defmacro with-profiling ((&key (sample-interval '*sample-interval*)
                                (alloc-interval '*alloc-interval*)
                                (max-samples '*max-samples*)
                                (reset nil)
                                (mode '*sampling-mode*)
                                (loop nil)
                                (max-depth most-positive-fixnum)
                                show-progress
                                (threads '(list sb-thread:*current-thread*))
                                (report nil report-p))
                          &body body)
  "Evaluate BODY with statistical profiling turned on. If LOOP is true,
loop around the BODY until a sufficient number of samples has been collected.
Returns the values from the last evaluation of BODY.

In multithreaded operation, only the thread in which WITH-PROFILING was
evaluated will be profiled by default. If you want to profile multiple
threads, invoke the profiler with START-PROFILING.

The following keyword args are recognized:

 :SAMPLE-INTERVAL <n>
   Take a sample every <n> seconds. Default is *SAMPLE-INTERVAL*.

 :ALLOC-INTERVAL <n>
   Take a sample every time <n> allocation regions (approximately
   8kB) have been allocated since the last sample. Default is
   *ALLOC-INTERVAL*.

 :MODE <mode>
   If :CPU, run the profiler in CPU profiling mode. If :ALLOC, run the
   profiler in allocation profiling mode. If :TIME, run the profiler
   in wallclock profiling mode.

 :MAX-SAMPLES <max>
   Repeat evaluating body until <max> samples are taken.
   Default is *MAX-SAMPLES*.

 :MAX-DEPTH <max>
   Maximum call stack depth that the profiler should consider. Only
   has an effect on x86 and x86-64.

 :REPORT <type>
   If specified, call REPORT with :TYPE <type> at the end.

 :RESET <bool>
   It true, call RESET at the beginning.

 :THREADS <list-form>
   Form that evaluates to the list threads to profile, or :ALL to indicate
   that all threads should be profiled. Defaults to the current
   thread. (Note: START-PROFILING defaults to all threads.)

   :THREADS has no effect on call-counting at the moment.

   On some platforms (eg. Darwin) the signals used by the profiler are
   not properly delivered to threads in proportion to their CPU usage
   when doing :CPU profiling. If you see empty call graphs, or are obviously
   missing several samples from certain threads, you may be falling afoul
   of this. In this case using :MODE :TIME is likely to work better.

 :LOOP <bool>
   If false (the default), evaluate BODY only once. If true repeatedly
   evaluate BODY."
  (declare (type report-type report))
  (check-type loop boolean)
  (with-unique-names (values last-index oops)
    `(let* ((*sample-interval* ,sample-interval)
            (*alloc-interval* ,alloc-interval)
            (*sampling* nil)
            (*sampling-mode* ,mode)
            (*max-samples* ,max-samples))
       ,@(when reset '((reset)))
       (flet ((,oops ()
                (warn "~@<No sampling progress; run too short, sampling interval ~
                       too long, inappropriate set of sampled thread, or possibly ~
                       a profiler bug.~:@>")))
         (unwind-protect
              (progn
                (start-profiling :max-depth ,max-depth :threads ,threads)
                ,(if loop
                     `(let (,values)
                        (loop
                          (when (>= (samples-trace-count *samples*)
                                    (samples-max-samples *samples*))
                            (return))
                          ,@(when show-progress
                              `((format t "~&===> ~d of ~d samples taken.~%"
                                        (samples-trace-count *samples*)
                                        (samples-max-samples *samples*))))
                          (let ((,last-index (samples-index *samples*)))
                            (setf ,values (multiple-value-list (progn ,@body)))
                            (when (= ,last-index (samples-index *samples*))
                              (,oops)
                              (return))))
                        (values-list ,values))
                     `(let ((,last-index (samples-index *samples*)))
                        (multiple-value-prog1 (progn ,@body)
                          (when (= ,last-index (samples-index *samples*))
                            (,oops))))))
           (stop-profiling)))
       ,@(when report-p `((report :type ,report))))))

(defvar *timer* nil)

#-win32
(defun start-profiling (&key (max-samples *max-samples*)
                        (mode *sampling-mode*)
                        (sample-interval *sample-interval*)
                        (alloc-interval *alloc-interval*)
                        (max-depth most-positive-fixnum)
                        (threads :all)
                        (sampling t))
  "Start profiling statistically in the current thread if not already profiling.
The following keyword args are recognized:

   :SAMPLE-INTERVAL <n>
     Take a sample every <n> seconds.  Default is *SAMPLE-INTERVAL*.

   :ALLOC-INTERVAL <n>
     Take a sample every time <n> allocation regions (approximately
     8kB) have been allocated since the last sample. Default is
     *ALLOC-INTERVAL*.

   :MODE <mode>
     If :CPU, run the profiler in CPU profiling mode. If :ALLOC, run
     the profiler in allocation profiling mode. If :TIME, run the profiler
     in wallclock profiling mode.

   :MAX-SAMPLES <max>
     Maximum number of samples.  Default is *MAX-SAMPLES*.

   :MAX-DEPTH <max>
     Maximum call stack depth that the profiler should consider. Only
     has an effect on x86 and x86-64.

   :THREADS <list>
     List threads to profile, or :ALL to indicate that all threads should be
     profiled. Defaults to :ALL. (Note: WITH-PROFILING defaults to the current
     thread.)

     :THREADS has no effect on call-counting at the moment.

     On some platforms (eg. Darwin) the signals used by the profiler are
     not properly delivered to threads in proportion to their CPU usage
     when doing :CPU profiling. If you see empty call graphs, or are obviously
     missing several samples from certain threads, you may be falling afoul
     of this.

   :SAMPLING <bool>
     If true, the default, start sampling right away.
     If false, START-SAMPLING can be used to turn sampling on."
  #-gencgc
  (when (eq mode :alloc)
    (error "Allocation profiling is only supported for builds using the generational garbage collector."))
  (unless *profiling*
    (multiple-value-bind (secs usecs)
        (multiple-value-bind (secs rest)
            (truncate sample-interval)
          (values secs (truncate (* rest 1000000))))
      (setf *sampling* sampling
            *samples* (make-samples :start-time (get-internal-real-time)
                                    :max-depth max-depth
                                    :max-samples max-samples
                                    :sample-interval sample-interval
                                    :alloc-interval alloc-interval
                                    :mode mode))
      (enable-call-counting)
      (setf *profiled-threads* threads)
      (sb-sys:enable-interrupt sb-unix:sigprof
                               #'sigprof-handler
                               :synchronous t)
      (ecase mode
        (:alloc
         (let ((alloc-signal (1- alloc-interval)))
           #+sb-thread
           (progn
             (when (eq :all threads)
               ;; Set the value new threads inherit.
               (sb-thread::with-all-threads-lock
                 (setf sb-thread::*default-alloc-signal* alloc-signal)))
             ;; Turn on allocation profiling in existing threads.
             (dolist (thread (profiled-threads))
               (sb-thread::%set-symbol-value-in-thread 'sb-vm::*alloc-signal* thread alloc-signal)))
           #-sb-thread
           (setf sb-vm:*alloc-signal* alloc-signal)))
        (:cpu
         (unix-setitimer :profile secs usecs secs usecs))
        (:time
         #+sb-thread
         (let ((setup (sb-thread:make-semaphore :name "Timer thread setup semaphore")))
           (setf *timer-thread*
                 (sb-thread:make-thread (lambda ()
                                          (sb-thread:wait-on-semaphore setup)
                                          (loop while (eq sb-thread:*current-thread* *timer-thread*)
                                                do (sleep 1.0)))
                                        :name "SB-SPROF wallclock timer thread"))
           (sb-thread:signal-semaphore setup))
         #-sb-thread
         (setf *timer-thread* nil)
         (setf *timer* (make-timer #'thread-distribution-handler :name "SB-PROF wallclock timer"
                                   :thread *timer-thread*))
         (schedule-timer *timer* sample-interval :repeat-interval sample-interval)))
      (setq *profiling* mode)))
  (values))

(defun stop-profiling ()
  "Stop profiling if profiling."
  (let ((profiling *profiling*))
    (when profiling
      ;; Even with the timers shut down we cannot be sure that there is no
      ;; undelivered sigprof. The handler is also responsible for turning the
      ;; *ALLOC-SIGNAL* off in individual threads.
      (ecase profiling
        (:alloc
         #+sb-thread
         (setf sb-thread::*default-alloc-signal* nil)
         #-sb-thread
         (setf sb-vm:*alloc-signal* nil))
        (:cpu
         (unix-setitimer :profile 0 0 0 0))
        (:time
         (unschedule-timer *timer*)
         (setf *timer* nil
               *timer-thread* nil)))
     (disable-call-counting)
     (setf *profiling* nil
           *sampling* nil
           *profiled-threads* nil)
     (setf (samples-end-time *samples*) (get-internal-real-time))))
  (values))

(defun reset ()
  "Reset the profiler."
  (stop-profiling)
  (setq *sampling* nil)
  (setq *samples* nil)
  (values))
