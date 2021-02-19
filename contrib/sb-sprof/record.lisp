;;;; Sample recording and storage functions of the statistical profiler
;;;;
;;;; Copyright (C) 2003 Gerd Moellmann <gerd.moellmann@t-online.de>
;;;; All rights reserved.

(in-package #:sb-sprof)

(deftype sampling-mode ()
  '(member :cpu :alloc :time))

;;; 0 code-component-ish
;;; 1 an offset relative to the start of the code-component or an
;;;   absolute address
(defconstant +elements-per-pc-loc+ 2)

;;; 0 the trace start marker (trace-start . END-INDEX)
;;; 1 the current thread, an SB-THREAD:THREAD instance
(defconstant +elements-per-trace-start+ 2)

;;; Encapsulate all the information about a sampling run
(defstruct (samples (:constructor make-samples (mode sample-interval)))
  (vector          #()                  :type simple-vector)
  ;; The trace count is updated only when profiling is stopped.
  (trace-count     0                    :type sb-int:index)
  (unique-trace-count nil)
  (sampled-threads nil                  :type list)
  ;; Metadata
  (mode            nil                  :type sampling-mode              :read-only t)
  (sample-interval (sb-int:missing-arg) :type (real (0))                 :read-only t))

(defun samples-index (x) (length (samples-vector x)))

(define-alien-variable ("gencgc_alloc_profiler" enable-alloc-profiler) (signed 32))
(define-alien-variable ("sb_sprof_trace_ct" trace-count) (signed 32))
(define-alien-variable ("sb_sprof_trace_ct_max" trace-limit) (signed 32))

(defmethod print-object ((samples samples) stream)
  (let ((*print-array* nil))
    (call-next-method)))

;;; Trace and sample and access functions

(defun map-traces (function samples)
  "Call FUNCTION on each trace in SAMPLES

The signature of FUNCTION must be compatible with (thread trace).

FUNCTION is called once for each trace where THREAD is the SB-THREAD:TREAD
instance which was sampled to produce TRACE, and TRACE is an opaque object
to be passed to MAP-TRACE-PC-LOCS.

EXPERIMENTAL: Interface subject to change."
  (let ((function (sb-kernel:%coerce-callable-to-fun function))
        (vector (samples-vector samples))
        (index (samples-index samples)))
    (when (plusp index)
      (sb-int:aver (typep (aref vector 0) '(cons (eql trace-start) index)))
      (loop for start = 0 then end
            while (< start index)
            for end = (cdr (aref vector start))
            for thread = (aref vector (+ start 1))
            do (let ((trace (list vector start end)))
                 (funcall function thread trace))))))

;;; Call FUNCTION on each PC location in TRACE.
;;; The signature of FUNCTION must be compatible with (info pc-or-offset).
;;; TRACE is an object as received by the function passed to MAP-TRACES.
(defun map-trace-pc-locs (function trace)
  (let ((function (sb-kernel:%coerce-callable-to-fun function)))
    (destructuring-bind (samples start end) trace
      (loop for i from (- end +elements-per-pc-loc+)
            downto (+ start +elements-per-trace-start+ -1)
            by +elements-per-pc-loc+
            for info = (aref samples i)
            for pc-or-offset = (aref samples (1+ i))
            do (funcall function info pc-or-offset)))))

;;; One "sample" is a trace, usually. But also it means an instance of SAMPLES,
;;; but also it means a PC location expressed as code + offset.
;;; So when is a sample not a sample? When it's a PC location.
(declaim (type (or null samples) *samples*))
(defglobal *samples* nil)

;;; Call FUNCTION on each PC location in SAMPLES which is usually
;;; the value of *SAMPLES* after a profiling run.
;;; The signature of FUNCTION must be compatible with (info pc-or-offset).
(defun map-all-pc-locs (function &optional (samples *samples*))
  (sb-int:dx-flet ((do-trace (thread trace)
                     (declare (ignore thread))
                     (map-trace-pc-locs function trace)))
    (map-traces #'do-trace samples)))

(defun sample-pc (info pc-or-offset)
  "Extract and return program counter from INFO and PC-OR-OFFSET.

Can be applied to the arguments passed by MAP-TRACE-PC-LOCS and
MAP-ALL-PC-LOCS.

EXPERIMENTAL: Interface subject to change."
  (etypecase info
    ;; Assembly routines or foreign functions don't move around, so
    ;; we've stored a raw PC
    ((or null sb-kernel:code-component string symbol)
     pc-or-offset)
    ;; Lisp functions might move, so we've stored a offset from the
    ;; start of the code component.
    (sb-di::compiled-debug-fun
     (let ((component (sb-di::compiled-debug-fun-component info)))
       (sap-int (sap+ (sb-kernel:code-instructions component) pc-or-offset))))))

;;; Sampling

;;; *PROFILING* is both the global enabling flag, and the operational mode.
(defvar *profiling* nil)
(declaim (type (or (eql nil) sampling-mode) *profiling*))

(defvar *show-progress* nil)

;; Call count encapsulation information
(defvar *encapsulations* (make-hash-table :test 'equal))

(defun show-progress (format-string &rest args)
  (when *show-progress*
    (apply #'format t format-string args)
    (finish-output)))

(define-alien-routine "sb_toggle_sigprof" int (context system-area-pointer) (state int))
(eval-when (:compile-toplevel)
  ;; current-thread-offset-sap has no slot setter, let alone for other threads,
  ;; nor for sub-fields of a word, so ...
  (defmacro sprof-enable-byte () ; see 'thread.h'
    (+ (ash sb-vm:thread-state-word-slot sb-vm:word-shift) 1)))

;;; If a thread wants sampling but had previously blocked SIGPROF,
;;; it will have to unblock the signal. We can use %INTERRUPT-THREAD
;;; to tell it to do that.
(defun start-sampling (&optional (thread sb-thread:*current-thread*))
  "Unblock SIGPROF in the specified thread"
  (if (eq thread sb-thread:*current-thread*)
      (when (zerop (sap-ref-8 (sb-thread:current-thread-sap) (sprof-enable-byte)))
        (setf (sap-ref-8 (sb-thread:current-thread-sap) (sprof-enable-byte)) 1)
        (sb-toggle-sigprof (if (boundp 'sb-kernel:*current-internal-error-context*)
                               sb-kernel:*current-internal-error-context*
                               (sb-sys:int-sap 0))
                           0))
      ;; %INTERRUPT-THREAD requires that the interruptions lock be held by the caller.
      (sb-thread:with-deathlok (thread c-thread)
        (when (and (/= c-thread 0)
                   (zerop (sap-ref-8 (int-sap c-thread) (sprof-enable-byte))))
          (sb-thread::%interrupt-thread thread #'start-sampling))))
  nil)

(defun stop-sampling (&optional (thread sb-thread:*current-thread*))
  "Block SIGPROF in the specified thread"
  (sb-thread:with-deathlok (thread c-thread)
    (when (and (/= c-thread 0)
               (not (zerop (sap-ref-8 (int-sap c-thread) (sprof-enable-byte)))))
      (setf (sap-ref-8 (int-sap c-thread) (sprof-enable-byte)) 0)
      ;; Blocking the signal is done lazily in threads other than the current one.
      (when (eq thread sb-thread:*current-thread*)
        (sb-toggle-sigprof (sb-sys:int-sap 0) 1)))) ; 1 = mask it
  nil)

(defun call-with-sampling (enable thunk)
  (declare (dynamic-extent thunk))
  (if (= (sap-ref-8 (sb-thread:current-thread-sap) (sprof-enable-byte))
         (if enable 1 0))
      ;; Already in the correct state
      (funcall thunk)
      ;; Invert state, call thunk, invert again
      (progn (if enable (start-sampling) (stop-sampling))
             (unwind-protect (funcall thunk)
               (if enable (stop-sampling) (start-sampling))))))

(defmacro with-sampling ((&optional (on t)) &body body)
  "Evaluate body with statistical sampling turned on or off in the current thread."
  `(call-with-sampling (if ,on t nil) (lambda () ,@body)))

;;; Return something serving as debug info for address PC.
(defun debug-info (pc code)
  (declare (type system-area-pointer pc)
           (muffle-conditions compiler-note))
  (let ((pc-int (sap-int pc)))
    (cond ((not code)
           (let ((name (sap-foreign-symbol pc)))
             (if name
                 (values (format nil "foreign function ~a" name)
                         pc-int :foreign)
                 (values nil pc-int :foreign))))
          ((eq code sb-fasl:*assembler-routines*)
           (values (sb-disassem::find-assembler-routine pc-int)
                   pc-int :asm-routine))
          (t
           (let* (;; Give up if we land in the 2 or 3 instructions of a
                  ;; code component sans simple-fun that is not an asm routine.
                  ;; While it's conceivable that this could be improved,
                  ;; the problem will be different or nonexistent after
                  ;; funcallable-instances each contain their own trampoline.
                  #+immobile-code
                  (di (unless (typep (sb-kernel:%code-debug-info code)
                                     'sb-c::compiled-debug-info)
                        (return-from debug-info
                          (values code pc-int nil))))
                  (pc-offset (sap- (int-sap pc-int) (sb-kernel:code-instructions code)))
                  (df (sb-di::debug-fun-from-pc code pc-offset)))
             #+immobile-code (declare (ignorable di))
             (cond ((typep df 'sb-di::bogus-debug-fun)
                    (values code pc-int nil))
                   (df
                    ;; The code component might be moved by the GC. Store
                    ;; a PC offset, and reconstruct the data in SAMPLE-PC
                    (values df pc-offset nil))
                   (t
                    (values nil 0 nil))))))))

(defun sprof-data-header (sap)
  (values (sap-ref-sap sap 0)   ; bucket pointer
          (sap-ref-32 sap 8)    ; free pointer
          (sap-ref-32 sap 12))) ; capacity
(defconstant n-buckets #x10000)
(defconstant element-size 8)
(defun sprof-data-trace (data trace-index) (sap+ data (* trace-index element-size)))
(defun trace-multiplicity (trace) (sap-ref-32 trace 4))
(defun trace-len (trace)
  #+64-bit (logand (sap-ref-64 trace 8) #xffffffff)
  #-64-bit (sap-ref-32 trace 8))

;;; Pseudo-functions for marking questionable parts of the stack trace
(defun unavailable-frames ())
(defun unknown-function ())

(defun build-serialno-to-code-map ()
  ;; Allocate a hash-table for serial# -> code object
  ;; Identifying code-containing pages is quick (except on cheneygc), so
  ;; two passes over the heap is cheaper than sizing up the table repeatedly.
  (let ((ht (make-hash-table
             :size (let ((n 0))
                     (sb-vm:map-code-objects (lambda (x) (declare (ignore x)) (incf n)))
                     n))))
    ;; Collect the objects
    (sb-vm:map-code-objects
     (lambda (x)
       (let ((serial (sb-kernel:%code-serialno x)))
         (unless (eql serial 0)
           (setf (gethash serial ht) x)))))
    ht))

(defun extract-traces (sap serialno-to-code)
  (macrolet ((absolute-pc (pc)
               ;; Foreign function or immovable code
               `(let ((sap (int-sap ,pc)))
                  (debug-info sap (sb-di::code-header-from-pc sap))))
             (relative-pc (serialno offset)
               ;; Convert serial# + base-address-relative pc-offset
               ;; to tagged code object + CODE-INSTRUCTIONS-relative pc-offset.
               ;; This can fail only if the code was GCed in the meantime.
               `(let* ((id ,serialno)
                       (code (gethash id serialno-to-code))
                       (rel-pc ,offset))
                  (when (and (not code) (/= id 0))
                    ;; If the serial# could not be found, something has gone wrong in GC.
                    ;; This really should not happen. If it does, simulate a random code blob
                    ;; named with that serial#.
                    (setf code (sb-kernel:fun-code-header
                                (compile nil `(named-lambda ,(format nil "Unknown fn ~d" id) ())))
                          (gethash id serialno-to-code) code
                          rel-pc 0))
                  (if code
                      (with-pinned-objects (code)
                        (debug-info (sap+ (int-sap (logandc2 (sb-kernel:get-lisp-obj-address code)
                                                             sb-vm:lowtag-mask))
                                          rel-pc)
                                    code))
                      (let ((code (sb-kernel:fun-code-header #'unknown-function)))
                        (debug-info (sb-kernel:code-instructions code) code)))))
             (elision-marker ()
               `(let ((code (sb-kernel:fun-code-header #'unavailable-frames)))
                  (debug-info (sb-kernel:code-instructions code) code))))
    (do ((free-ptr (nth-value 1 (sprof-data-header sap)))
         (trace-ptr 2)
         (result))
        ((>= trace-ptr free-ptr)
         (aver (= trace-ptr free-ptr))
         result)
      (let* ((trace (sprof-data-trace sap trace-ptr))
             (len (trace-len trace))
             ;; byte offset into the trace at which the locs[] array begins
             (element-offset 16)
             (locs))
        (dotimes (i len (push (cons (nreverse (coerce locs 'vector))
                                    (trace-multiplicity trace))
                              result))
          (multiple-value-bind (info pc-or-offset)
              #-64-bit
              (let ((word0 (sap-ref-word trace element-offset))
                    (word1 (sap-ref-word trace (+ element-offset 4))))
                (cond ((not (zerop word1)) (relative-pc word0 word1)) ; serial# + offset
                      ((eql word0 sb-ext:most-positive-word) (elision-marker))
                      (t (absolute-pc word0))))
              #+64-bit
              (let ((bits (sap-ref-word trace element-offset)))
                (cond ((eql bits sb-ext:most-positive-word) (elision-marker))
                      ((logbitp 63 bits)
                       (relative-pc (ldb (byte 32 0) bits) (ldb (byte 31 32) bits)))
                      (t (absolute-pc bits))))
          (setf locs (list* pc-or-offset info locs))
          (incf element-offset element-size)))
        (incf trace-ptr (+ 2 len))))))

;;; Call FUNCTION with each thread's sampled data, and deallocate the data.
(defun call-with-each-profile-buffer (function)
  (with-alien ((acquire-data (function system-area-pointer unsigned) :extern "acquire_sprof_data"))
    (flet ((process (sap thread)
             (multiple-value-bind (buckets free-ptr capacity) (sprof-data-header sap)
               (let ((buckets-used 0))
                 (dotimes (bucket-index n-buckets)
                   ;; bucket values are uint32_t
                   (when (plusp (sap-ref-32 buckets (ash bucket-index 2))) (incf buckets-used)))
                 (funcall function sap thread
                          (list (* free-ptr element-size)
                                (* capacity element-size)
                                buckets-used)))
               (sb-sys:deallocate-system-memory buckets (* n-buckets 4))
               (sb-sys:deallocate-system-memory sap (* capacity element-size)))))
      (let ((all-threads sb-thread::*all-threads*)
            (processed-threads))
        ;; Scan exited threads. Careful not to lose elements if a thread receives a
        ;; profiling signal after the timer gets stopped, exits, and pushes data into
        ;; the list. i.e. concurrent access to sb-thread::*sprof-data* is possible.
        #+sb-thread
        (loop
          (let ((data (atomic-pop sb-thread::*sprof-data*)))
            (unless data (return))
            (destructuring-bind (sap . thread) data
              ;; Avoid double-free of the foreign memory, in case two threads try to
              ;; reset the profiler at the same time (which constitutes user error)
              (when (and sap (eq (cas (car data) sap nil) sap))
                (process sap thread)
                (push thread processed-threads)))))
        ;; Scan running threads
        (sb-thread::avltree-filter
         (lambda (node &aux (thread (sb-thread::avlnode-data node)))
           (unless (memq thread processed-threads)
             ;; Ensure that the thread can't exit (which ensures that it can't free the sprof_sem
             ;; that might be needed by the C routine, depending on things), and also ensure
             ;; mutual exclusivity with other callers of this.
             (sb-thread:with-deathlok (thread c-thread)
               (unless (zerop c-thread)
                 (let ((sap (alien-funcall acquire-data c-thread)))
                   (unless (= (sap-int sap) 0)
                     (process sap thread))))))
           nil)
         all-threads)))))

(defun convert-raw-data ()
  (let ((ht (build-serialno-to-code-map))
        (threads)
        ;; traces are not de-deduplicated across threads,
        ;; so this number is only approximate.
        (n-unique-traces 0)
        (aggregate-data))
    ;; Mask SIGPROF in this thread in case of pending signal
    ;; and funky scheduling by the OS.
    (let ((saved-sigprof-mask (sb-toggle-sigprof (int-sap 0) 1)))
      (call-with-each-profile-buffer
       (lambda (sap thread memusage)
         (push (cons thread memusage) threads)
         (push (cons (extract-traces sap ht) thread) aggregate-data)))
      (setf (extern-alien "sb_sprof_enabled" int) 0)
      (sb-toggle-sigprof (int-sap 0) saved-sigprof-mask))
    ;; Precompute the length of the new SAMPLES-VECTOR
    (let ((vector
           (make-array
            (let ((total-length 0))
              (dolist (subsample aggregate-data total-length)
                (loop for (trace . multiplicity) in (car subsample)
                      do (incf total-length (* (+ (length trace) +elements-per-trace-start+)
                                               multiplicity)))))))
          (index 0))
      ;; Now fill in the vector to match the old data format.
      ;; This is of needlessly inefficient for :ALLOC mode, since the entire point
      ;; of the new format is to speed up callgraph construction.
      (dolist (subsample aggregate-data)
        (loop with thread = (cdr subsample)
              for (trace . multiplicity) in (car subsample)
              do (incf n-unique-traces)
                 (dotimes (i multiplicity)
                   (let* ((len (+ (length trace) +elements-per-trace-start+))
                          (end (+ index len)))
                     (setf (aref vector index) `(trace-start . ,end)
                           (aref vector (1+ index)) thread)
                     (replace vector trace :start1 (+ index 2))
                     (setq index end)))))
      (aver (= index (length vector)))
      (values vector n-unique-traces threads))))

#+nil
(defun dump-hash-buckets (sap-or-thread)
  (binding* ((sap (if (typep sap-or-thread 'sb-thread::thread)
                      (sap-ref-sap (int-sap (sb-thread::thread-primitive-thread sap-or-thread))
                                   (ash sb-vm:thread-sprof-data-slot sb-vm:word-shift))
                      sap-or-thread))
             ((buckets free-ptr) (sprof-data-header sap))
             (buckets-used 0)
             (trace-ptr 2)
             (n-traces 0)
             (n-unique 0))
    (dotimes (bucket-index n-buckets)
      (let ((n (do ((entry (sap-ref-32 buckets (ash bucket-index 2))
                           (sap-ref-32 (sprof-data-trace sap entry) 0))
                    (chainlen 0 (1+ chainlen)))
                   ((= entry 0) chainlen))))
        (when (plusp n)
          (incf buckets-used)
          (format t "~5d ~d~%" bucket-index n))))
    (loop while (< trace-ptr free-ptr)
          do (let ((trace (sprof-data-trace sap trace-ptr)))
               (incf n-unique)
               (incf n-traces (trace-multiplicity trace))
               (incf trace-ptr (+ 2 (trace-len trace)))))
    (aver (= trace-ptr free-ptr))
    (format t "~d buckets in use, ~D unique traces, ~D total~%" buckets-used n-unique n-traces)))

#+nil
(defun code-summary-by-gc-generation ()
  (let ((gens (make-array 7 :initial-element 0)))
    (dolist (code (sb-vm:list-allocated-objects :all :type sb-vm:code-header-widetag))
      (let ((gen (sb-kernel:generation-of code)))
        (when (<= 0 gen 6)
          (incf (aref gens gen)))))
    gens))
