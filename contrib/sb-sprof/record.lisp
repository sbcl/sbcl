;;;; Sample recording and storage functions of the statistical profiler
;;;;
;;;; Copyright (C) 2003 Gerd Moellmann <gerd.moellmann@t-online.de>
;;;; All rights reserved.

(in-package #:sb-sprof)


;;; Append-only sample vector

(deftype sampling-mode ()
  '(member :cpu :alloc :time))

;;; 0 code-component-ish
;;; 1 an offset relative to the start of the code-component or an
;;;   absolute address
(defconstant +elements-per-sample+ 2)

;;; 0 the trace start marker (trace-start . END-INDEX)
;;; 1 the current thread, an SB-THREAD:THREAD instance
;;; 2 the current internal real time
(defconstant +elements-per-trace-start+ 3)

(declaim (inline make-sample-vector))
(defun make-sample-vector (max-samples)
  (make-array (* max-samples
                 ;; Arbitrary guess at how many samples we'll be
                 ;; taking for each trace. The exact amount doesn't
                 ;; matter, this is just to decrease the amount of
                 ;; re-allocation that will need to be done.
                 10
                 ;; Each sample takes two cells in the vector
                 +elements-per-sample+)))

;;; Encapsulate all the information about a sampling run
(defstruct (samples
             (:constructor
              make-samples (&key start-time
                                 mode sample-interval alloc-interval
                                 max-depth max-samples
                            &aux (vector (make-sample-vector max-samples)))))
  ;; When this vector fills up, we allocate a new one and copy over
  ;; the old contents.
  (vector          nil                  :type simple-vector)
  (index           0                    :type sb-int:index)
  ;; Only used while recording a trace. Stores a cell that is
  ;;
  ;;   (start-trace . nil)   (start-trace is the marker symbol
  ;;                          SB-SPROF:START-TRACE)
  ;;
  ;; when starting to record a trace and
  ;;
  ;;   (start-trace . END-INDEX-OF-THE-TRACE)
  ;;
  ;; after finishing recording the trace. This allows MAP-TRACES to
  ;; jump from one trace to the next in O(1) instead of O(N) time.
  (trace-start     '(nil . nil)         :type cons)
  (trace-count     0                    :type sb-int:index)

  (sampled-threads nil                  :type list)

  ;; Metadata
  (start-time      (sb-int:missing-arg) :type sb-kernel:internal-time    :read-only t)
  (end-time        nil                  :type (or null sb-kernel:internal-time))

  (mode            nil                  :type sampling-mode              :read-only t)
  (sample-interval (sb-int:missing-arg) :type (real (0))                 :read-only t)
  (alloc-interval  (sb-int:missing-arg) :type (integer (0))              :read-only t)
  (max-depth       most-positive-fixnum :type (and fixnum (integer (0))) :read-only t)
  (max-samples     (sb-int:missing-arg) :type sb-int:index               :read-only t))

(defmethod print-object ((samples samples) stream)
  (let ((*print-array* nil))
    (call-next-method)))

(defun note-sample-vector-full (samples)
  (format *trace-output* "Profiler sample vector full (~:D trace~:P / ~
                          approximately ~:D sample~:P), doubling the ~
                          size~%"
          (samples-trace-count samples)
          (truncate (samples-index samples) +elements-per-sample+)))

(declaim (inline ensure-samples-vector))
(defun ensure-samples-vector (samples)
  (declare (optimize (speed 3)))
  (let ((vector (samples-vector samples))
        (index (samples-index samples)))
    ;; Allocate a new sample vector if the current one is too small to
    ;; accommodate the largest chunk of elements that we could
    ;; potentially store in one go (currently 3 elements, stored by
    ;; RECORD-TRACE-START).
    (values (if (< (length vector) (+ index (max +elements-per-sample+
                                                 +elements-per-trace-start+)))
                (let ((new-vector (make-array (* 2 index))))
                  (note-sample-vector-full samples)
                  (replace new-vector vector)
                  (setf (samples-vector samples) new-vector))
                vector)
            index)))

(defun record-trace-start (samples)
  ;; Mark the start of the trace.
  (multiple-value-bind (vector index) (ensure-samples-vector samples)
    (let ((trace-start (cons 'trace-start nil)))
      (setf (aref vector index)       trace-start
            (aref vector (+ index 1)) sb-thread:*current-thread*
            (aref vector (+ index 2)) (get-internal-real-time))
      (setf (samples-index samples)       (+ index +elements-per-trace-start+)
            (samples-trace-start samples) trace-start))))

(defun record-trace-end (samples)
  (setf (cdr (samples-trace-start samples)) (samples-index samples)))

(declaim (inline record-sample))
(defun record-sample (samples info pc-or-offset)
  (multiple-value-bind (vector index) (ensure-samples-vector samples)
    ;; For each sample, store the debug-info and the PC/offset into
    ;; adjacent cells.
    (setf (aref vector index) info
          (aref vector (1+ index)) pc-or-offset)
    (setf (samples-index samples) (+ index +elements-per-sample+))))

;;; Trace and sample and access functions

(defun map-traces (function samples)
  "Call FUNCTION on each trace in SAMPLES

The lambda list of FUNCTION has to be compatible to

  (thread time trace)

. FUNCTION is called once for each trace such that THREAD is the
SB-THREAD:TREAD instance that was sampled to produce TRACE, TIME is
the internal real time at which TRACE was produced and TRACE is an
opaque object whose only purpose is being used as the second argument
to MAP-TRACE-SAMPLES.

EXPERIMENTAL: Interface subject to change."
  (let ((function (sb-kernel:%coerce-callable-to-fun function))
        (vector (samples-vector samples))
        (index (samples-index samples))
        (start-time (samples-start-time samples)))
    (when (plusp index)
      (sb-int:aver (typep (aref vector 0) '(cons (eql trace-start) index)))
      (loop for start = 0 then end
            while (< start index)
            for end = (cdr (aref vector start))
            for thread = (aref vector (+ start 1))
            for time = (/ (- (aref vector (+ start 2)) start-time)
                          internal-time-units-per-second)
            do (let ((trace (list vector start end)))
                 (funcall function thread time trace))))))

(defun map-trace-samples (function trace)
  "Call FUNCTION on each sample in TRACE.

The lambda list of FUNCTION has to be compatible to

  (info pc-or-offset)

.

TRACE is an object as received by a function passed to MAP-TRACES.

EXPERIMENTAL: Interface subject to change."
  (let ((function (sb-kernel:%coerce-callable-to-fun function)))
    (destructuring-bind (samples start end) trace
      (loop for i from (- end +elements-per-sample+)
            downto (+ start +elements-per-trace-start+ -1)
            by +elements-per-sample+
            for info = (aref samples i)
            for pc-or-offset = (aref samples (1+ i))
            do (funcall function info pc-or-offset)))))

(declaim (special *samples*))
(defun map-all-samples (function &optional (samples *samples*))
  "Call FUNCTION on each sample in SAMPLES.

The lambda list of FUNCTION has to be compatible to

  (info pc-or-offset)

.

SAMPLES is usually the value of *SAMPLES* after a profiling run.

EXPERIMENTAL: Interface subject to change."
  (sb-int:dx-flet ((do-trace (thread time trace)
                     (declare (ignore thread time))
                     (map-trace-samples function trace)))
    (map-traces #'do-trace samples)))

(defun sample-pc (info pc-or-offset)
  "Extract and return program counter from INFO and PC-OR-OFFSET.

Can be applied to the arguments passed by MAP-TRACE-SAMPLES and
MAP-ALL-SAMPLES.

EXPERIMENTAL: Interface subject to change."
  (etypecase info
    ;; Assembly routines or foreign functions don't move around, so
    ;; we've stored a raw PC
    ((or null sb-kernel:code-component string)
     pc-or-offset)
    ;; Lisp functions might move, so we've stored a offset from the
    ;; start of the code component.
    (sb-di::compiled-debug-fun
     (let* ((component (sb-di::compiled-debug-fun-component info))
            (start-pc (code-start component)))
       (+ start-pc pc-or-offset)))))

;;; Sampling

(defvar *samples* nil)
(declaim (type (or null samples) *samples*))

(defvar *profiling* nil)
(declaim (type (or (eql nil) sampling-mode) *profiling*))
(defvar *sampling* nil)
(declaim (type boolean *sampling*))

(defvar *show-progress* nil)

(defvar *old-sampling* nil)

;; Call count encapsulation information
(defvar *encapsulations* (make-hash-table :test 'equal))

(defun turn-off-sampling ()
  (setq *old-sampling* *sampling*)
  (setq *sampling* nil))

(defun turn-on-sampling ()
  (setq *sampling* *old-sampling*))

(defun show-progress (format-string &rest args)
  (when *show-progress*
    (apply #'format t format-string args)
    (finish-output)))

(defun start-sampling ()
  "Switch on statistical sampling."
  (setq *sampling* t))

(defun stop-sampling ()
  "Switch off statistical sampling."
  (setq *sampling* nil))

(defmacro with-sampling ((&optional (on t)) &body body)
  "Evaluate body with statistical sampling turned on or off."
  `(let ((*sampling* ,on)
         (sb-vm:*alloc-signal* sb-vm:*alloc-signal*))
     ,@body))

;;; Return something serving as debug info for address PC.
(declaim (inline debug-info))
(defun debug-info (pc)
  (declare (type system-area-pointer pc)
           (muffle-conditions compiler-note))
  (let ((code (sb-di::code-header-from-pc pc)))
    (cond ((not code)
           (let ((name (sap-foreign-symbol pc)))
             (if name
                 (values (format nil "foreign function ~a" name)
                         (sap-int pc)
                         :foreign)
                 (values nil (sap-int pc) :foreign))))
          (t
           (let* ((code-header-len (* (sb-kernel:code-header-words code)
                                      sb-vm:n-word-bytes))
                  ;; Give up if we land in the 2 or 3 instructions of a
                  ;; code component sans simple-fun that is not an asm routine.
                  ;; While it's conceivable that this could be improved,
                  ;; the problem will be different or nonexistent after
                  ;; funcallable-instances each contain their own trampoline.
                  #+immobile-code
                  (di (unless (typep (sb-kernel:%code-debug-info code)
                                     'sb-c::compiled-debug-info)
                        (return-from debug-info
                          (values code (sap-int pc)))))
                  (pc-offset (- (sap-int pc)
                                (- (sb-kernel:get-lisp-obj-address code)
                                   sb-vm:other-pointer-lowtag)
                                code-header-len))
                  (df (sb-di::debug-fun-from-pc code pc-offset)))
             #+immobile-code (declare (ignorable di))
             (cond ((typep df 'sb-di::bogus-debug-fun)
                    (values code (sap-int pc) nil))
                   (df
                    ;; The code component might be moved by the GC. Store
                    ;; a PC offset, and reconstruct the data in
                    ;; SAMPLE-PC-FROM-PC-OR-OFFSET.
                    (values df pc-offset nil))
                   (t
                    (values nil 0 nil))))))))

(declaim (inline record))
(defun record (samples pc)
  (declare (type system-area-pointer pc))
  (multiple-value-bind (info pc-or-offset foreign) (debug-info pc)
    (record-sample samples info pc-or-offset)
    foreign))

;;; List of thread currently profiled, or :ALL for all threads.
(defvar *profiled-threads* nil)
(declaim (type (or list (member :all)) *profiled-threads*))

;;; Thread which runs the wallclock timers, if any.
(defvar *timer-thread* nil)

(defun profiled-threads ()
  (let ((profiled-threads *profiled-threads*))
    (remove *timer-thread*
            (if (eq :all profiled-threads)
                (sb-thread:list-all-threads)
                profiled-threads))))

(defun profiled-thread-p (thread)
  (let ((profiled-threads *profiled-threads*))
    (or (and (eq :all profiled-threads)
             (not (eq *timer-thread* thread)))
        (member thread profiled-threads :test #'eq))))

#+(and (or x86 x86-64) (not win32))
(progn
  ;; Ensure that only one thread at a time will be doing profiling stuff.
  (defvar *profiler-lock* (sb-thread:make-mutex :name "Statistical Profiler"))
  (defvar *distribution-lock* (sb-thread:make-mutex :name "Wallclock profiling lock"))

  #+sb-thread
  (declaim (inline pthread-kill))
  #+sb-thread
  (define-alien-routine pthread-kill int (os-thread unsigned-long) (signal int))

  ;;; A random thread will call this in response to either a timer firing,
  ;;; This in turn will distribute the notice to those threads we are
  ;;; interested using SIGPROF.
  (defun thread-distribution-handler ()
    (declare (optimize speed (space 0)))
    #+sb-thread
    (let ((lock *distribution-lock*))
      ;; Don't flood the system with more interrupts if the last
      ;; set is still being delivered.
      (unless (sb-thread:mutex-value lock)
        (sb-thread::with-system-mutex (lock)
          (dolist (thread (profiled-threads))
            ;; This may occasionally fail to deliver the signal, but that
            ;; seems better then using kill_thread_safely with it's 1
            ;; second backoff.
            (let ((os-thread (sb-thread::thread-os-thread thread)))
              (when os-thread
                (pthread-kill os-thread sb-unix:sigprof)))))))
    #-sb-thread
    (unix-kill 0 sb-unix:sigprof))

  (defun sigprof-handler (signal code scp)
    (declare (ignore signal code) (optimize speed (space 0))
             (disable-package-locks sb-di::x86-call-context)
             (muffle-conditions compiler-note)
             (type system-area-pointer scp))
    (let ((self sb-thread:*current-thread*)
          (profiling *profiling*))
      ;; Turn off allocation counter when it is not needed. Doing this in the
      ;; signal handler means we don't have to worry about racing with the runtime
      (unless (eq :alloc profiling)
        (setf sb-vm::*alloc-signal* nil))
      (when (and *sampling*
                 ;; Normal SIGPROF gets practically speaking delivered to threads
                 ;; depending on the run time they use, so we need to filter
                 ;; out those we don't care about. For :ALLOC and :TIME profiling
                 ;; only the interesting threads get SIGPROF in the first place.
                 ;;
                 ;; ...except that Darwin at least doesn't seem to work like we
                 ;; would want it to, which makes multithreaded :CPU profiling pretty
                 ;; pointless there -- though it may be that our mach magic is
                 ;; partially to blame?
                 (or (not (eq :cpu profiling)) (profiled-thread-p self)))
        (sb-thread::with-system-mutex (*profiler-lock* :without-gcing t)
          (let ((samples *samples*)
                ;; Don't touch the circularity hash-table
                *print-circle*)
            (when (and samples
                       (< (samples-trace-count samples)
                          (samples-max-samples samples)))
              (with-alien ((scp (* os-context-t) :local scp))
                (let* ((pc-ptr (sb-vm:context-pc scp))
                       (fp (sb-vm::context-register scp #.sb-vm::ebp-offset)))
                  ;; foreign code might not have a useful frame
                  ;; pointer in ebp/rbp, so make sure it looks
                  ;; reasonable before walking the stack
                  (unless (sb-di::control-stack-pointer-valid-p (sb-sys:int-sap fp))
                    (return-from sigprof-handler nil))
                  (incf (samples-trace-count samples))
                  (pushnew self (samples-sampled-threads samples))
                  (let ((fp (int-sap fp))
                        (ok t))
                    (declare (type system-area-pointer fp pc-ptr))
                    ;; FIXME: How annoying. The XC doesn't store enough
                    ;; type information about SB-DI::X86-CALL-CONTEXT,
                    ;; even if we declaim the ftype explicitly in
                    ;; src/code/debug-int. And for some reason that type
                    ;; information is needed for the inlined version to
                    ;; be compiled without boxing the returned saps. So
                    ;; we declare the correct ftype here manually, even
                    ;; if the compiler should be able to deduce this
                    ;; exact same information.
                    (declare (ftype (function (system-area-pointer)
                                              (values (member nil t)
                                                      system-area-pointer
                                                      system-area-pointer))
                                    sb-di::x86-call-context))
                    (record-trace-start samples)
                    (dotimes (i (samples-max-depth samples))
                      (record samples pc-ptr)
                      (setf (values ok pc-ptr fp)
                            (sb-di::x86-call-context fp))
                      (unless ok
                        ;; If we fail to walk the stack beyond the
                        ;; initial frame, there is likely something
                        ;; wrong. Undo the trace start marker and the
                        ;; one sample we already recorded.
                        (when (zerop i)
                          (decf (samples-index samples)
                                (+ +elements-per-trace-start+
                                   (* +elements-per-sample+ (1+ i)))))
                        (return)))
                    (record-trace-end samples))))
              ;; Reset thread-local allocation counter before interrupts
              ;; are enabled.
              (when (eq t sb-vm::*alloc-signal*)
                (setf sb-vm:*alloc-signal* (1- (samples-alloc-interval samples)))))))))
    nil))

;; FIXME: On non-x86 platforms we don't yet walk the call stack deeper
;; than one level.
#-(or x86 x86-64)
(defun sigprof-handler (signal code scp)
  (declare (ignore signal code))
  (sb-sys:without-interrupts
    (let ((samples *samples*))
      (when (and *sampling*
                 samples
                 (< (samples-trace-count samples)
                    (samples-max-samples samples)))
        (sb-sys:without-gcing
          (with-alien ((scp (* os-context-t) :local scp))
            (locally (declare (optimize (inhibit-warnings 2)))
              (incf (samples-trace-count samples))
              (record-trace-start samples)
              (let ((pc-ptr (sb-vm:context-pc scp))
                    (fp (sb-vm::context-register scp #.sb-vm::cfp-offset)))
                (unless (eq (record samples pc-ptr) :foreign)
                  (record samples (sap-ref-sap
                                   (int-sap fp)
                                   (* sb-vm::lra-save-offset sb-vm::n-word-bytes)))))
              (record-trace-end samples))))))))

;;; Return the start address of CODE.
(defun code-start (code)
  (declare (type sb-kernel:code-component code))
  (sap-int (sb-kernel:code-instructions code)))

;;; Return start and end address of CODE as multiple values.
(defun code-bounds (code)
  (declare (type sb-kernel:code-component code))
  (let* ((start (code-start code))
         (end (+ start (sb-kernel:%code-text-size code))))
    (values start end)))
