;;; Copyright (C) 2003 Gerd Moellmann <gerd.moellmann@t-online.de>
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. The name of the author may not be used to endorse or promote
;;;    products derived from this software without specific prior written
;;;    permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
;;; OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
;;; USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
;;; DAMAGE.

;;; Statistical profiler.

;;; Overview:
;;;
;;; This profiler arranges for SIGPROF interrupts to interrupt a
;;; running program at regular intervals.  Each time a SIGPROF occurs,
;;; the current program counter and return address is recorded in a
;;; vector, until a configurable maximum number of samples have been
;;; taken.
;;;
;;; A profiling report is generated from the samples array by
;;; determining the Lisp functions corresponding to the recorded
;;; addresses.  Each program counter/return address pair forms one
;;; edge in a call graph.

;;; Problems:
;;;
;;; The code being generated on x86 makes determining callers reliably
;;; something between extremely difficult and impossible.  Example:
;;;
;;; 10979F00:       .entry eval::eval-stack-args(arg-count)
;;;       18:       pop     dword ptr [ebp-8]
;;;       1B:       lea     esp, [ebp-32]
;;;       1E:       mov     edi, edx
;;;
;;;       20:       cmp     ecx, 4
;;;       23:       jne     L4
;;;       29:       mov     [ebp-12], edi
;;;       2C:       mov     dword ptr [ebp-16], #x28F0000B ; nil
;;;                                              ; No-arg-parsing entry point
;;;       33:       mov     dword ptr [ebp-20], 0
;;;       3A:       jmp     L3
;;;       3C: L0:   mov     edx, esp
;;;       3E:       sub     esp, 12
;;;       41:       mov     eax, [#x10979EF8]    ; #<FDEFINITION object for eval::eval-stack-pop>
;;;       47:       xor     ecx, ecx
;;;       49:       mov     [edx-4], ebp
;;;       4C:       mov     ebp, edx
;;;       4E:       call    dword ptr [eax+5]
;;;       51:       mov     esp, ebx
;;;
;;; Suppose this function is interrupted by SIGPROF at 4E.  At that
;;; point, the frame pointer EBP has been modified so that the
;;; original return address of the caller of eval-stack-args is no
;;; longer where it can be found by x86-call-context, and the new
;;; return address, for the call to eval-stack-pop, is not yet on the
;;; stack.  The effect is that x86-call-context returns something
;;; bogus, which leads to wrong edges in the call graph.
;;;
;;; One thing that one might try is filtering cases where the program
;;; is interrupted at a call instruction.  But since the above example
;;; of an interrupt at a call instruction isn't the only case where
;;; the stack is something x86-call-context can't really cope with,
;;; this is not a general solution.
;;;
;;; Random ideas for implementation:
;;;
;;; * Space profiler.  Sample when new pages are allocated instead of
;;; at SIGPROF.
;;;
;;; * Record a configurable number of callers up the stack.  That
;;; could give a more complete graph when there are many small
;;; functions.
;;;
;;; * Print help strings for reports, include hints to the problem
;;; explained above.
;;;
;;; * Make flat report the default since call-graph isn't that
;;; reliable?

(defpackage #:sb-sprof
  (:use #:cl #:sb-ext #:sb-unix #:sb-alien #:sb-sys :sb-int)
  (:export #:*sample-interval* #:*max-samples* #:*alloc-interval*
           #:*report-sort-by* #:*report-sort-order*
           #:start-sampling #:stop-sampling #:with-sampling
           #:with-profiling #:start-profiling #:stop-profiling
           #:profile-call-counts #:unprofile-call-counts
           #:reset #:report))

(in-package #:sb-sprof)


;;;; Graph Utilities

(defstruct (vertex (:constructor make-vertex)
                   (:constructor make-scc (scc-vertices edges)))
  (visited     nil :type boolean)
  (root        nil :type (or null vertex))
  (dfn           0 :type fixnum)
  (edges        () :type list)
  (scc-vertices () :type list))

(defstruct edge
  (vertex (sb-impl::missing-arg) :type vertex))

(defstruct graph
  (vertices () :type list))

(declaim (inline scc-p))
(defun scc-p (vertex)
  (not (null (vertex-scc-vertices vertex))))

(defmacro do-vertices ((vertex graph) &body body)
  `(dolist (,vertex (graph-vertices ,graph))
     ,@body))

(defmacro do-edges ((edge edge-to vertex) &body body)
  `(dolist (,edge (vertex-edges ,vertex))
     (let ((,edge-to (edge-vertex ,edge)))
       ,@body)))

(defun self-cycle-p (vertex)
  (do-edges (e to vertex)
    (when (eq to vertex)
      (return t))))

(defun map-vertices (fn vertices)
  (dolist (v vertices)
    (setf (vertex-visited v) nil))
  (dolist (v vertices)
    (unless (vertex-visited v)
      (funcall fn v))))

;;; Eeko Nuutila, Eljas Soisalon-Soininen, around 1992.  Improves on
;;; Tarjan's original algorithm by not using the stack when processing
;;; trivial components.  Trivial components should appear frequently
;;; in a call-graph such as ours, I think.  Same complexity O(V+E) as
;;; Tarjan.
(defun strong-components (vertices)
  (let ((in-component (make-array (length vertices)
                                  :element-type 'boolean
                                  :initial-element nil))
        (stack ())
        (components ())
        (dfn -1))
    (labels ((min-root (x y)
               (let ((rx (vertex-root x))
                     (ry (vertex-root y)))
                 (if (< (vertex-dfn rx) (vertex-dfn ry))
                     rx
                     ry)))
             (in-component (v)
               (aref in-component (vertex-dfn v)))
             ((setf in-component) (in v)
               (setf (aref in-component (vertex-dfn v)) in))
             (vertex-> (x y)
               (> (vertex-dfn x) (vertex-dfn y)))
             (visit (v)
               (setf (vertex-dfn v) (incf dfn)
                     (in-component v) nil
                     (vertex-root v) v
                     (vertex-visited v) t)
               (do-edges (e w v)
                 (unless (vertex-visited w)
                   (visit w))
                 (unless (in-component w)
                   (setf (vertex-root v) (min-root v w))))
               (if (eq v (vertex-root v))
                   (loop while (and stack (vertex-> (car stack) v))
                         as w = (pop stack)
                         collect w into this-component
                         do (setf (in-component w) t)
                         finally
                           (setf (in-component v) t)
                           (push (cons v this-component) components))
                   (push v stack))))
      (map-vertices #'visit vertices)
      components)))

;;; Given a dag as a list of vertices, return the list sorted
;;; topologically, children first.
(defun topological-sort (dag)
  (let ((sorted ())
        (dfn -1))
    (labels ((rec-sort (v)
               (setf (vertex-visited v) t)
               (setf (vertex-dfn v) (incf dfn))
               (dolist (e (vertex-edges v))
                 (unless (vertex-visited (edge-vertex e))
                   (rec-sort (edge-vertex e))))
               (push v sorted)))
      (map-vertices #'rec-sort dag)
      (nreverse sorted))))

;;; Reduce graph G to a dag by coalescing strongly connected components
;;; into vertices.  Sort the result topologically.
(defun reduce-graph (graph &optional (scc-constructor #'make-scc))
  (sb-int:collect ((sccs) (trivial))
    (dolist (c (strong-components (graph-vertices graph)))
      (if (or (cdr c) (self-cycle-p (car c)))
          (sb-int:collect ((outgoing))
            (dolist (v c)
              (do-edges (e w v)
                (unless (member w c)
                  (outgoing e))))
            (sccs (funcall scc-constructor c (outgoing))))
          (trivial (car c))))
    (dolist (scc (sccs))
      (dolist (v (trivial))
        (do-edges (e w v)
          (when (member w (vertex-scc-vertices scc))
            (setf (edge-vertex e) scc)))))
    (setf (graph-vertices graph)
          (topological-sort (nconc (sccs) (trivial))))))

;;;; The Profiler

(deftype address ()
  "Type used for addresses, for instance, program counters,
   code start/end locations etc."
  '(unsigned-byte #.sb-vm::n-machine-word-bits))

(defconstant +unknown-address+ 0
  "Constant representing an address that cannot be determined.")

;;; A call graph.  Vertices are NODE structures, edges are CALL
;;; structures.
(defstruct (call-graph (:include graph)
                       (:constructor %make-call-graph))
  ;; the value of *SAMPLE-INTERVAL* or *ALLOC-INTERVAL* at the time
  ;; the graph was created (depending on the current allocation mode)
  (sample-interval (sb-impl::missing-arg) :type number)
  ;; the sampling-mode that was used for the profiling run
  (sampling-mode (sb-impl::missing-arg) :type (member :cpu :alloc :time))
  ;; number of samples taken
  (nsamples (sb-impl::missing-arg) :type sb-int:index)
  ;; threads that have been sampled
  (sampled-threads nil :type list)
  ;; sample count for samples not in any function
  (elsewhere-count (sb-impl::missing-arg) :type sb-int:index)
  ;; a flat list of NODEs, sorted by sample count
  (flat-nodes () :type list))

;;; A node in a call graph, representing a function that has been
;;; sampled.  The edges of a node are CALL structures that represent
;;; functions called from a given node.
(defstruct (node (:include vertex)
                 (:constructor %make-node))
  ;; A numeric label for the node.  The most frequently called function
  ;; gets label 1.  This is just for identification purposes in the
  ;; profiling report.
  (index 0 :type fixnum)
  ;; Start and end address of the function's code. Depending on the
  ;; debug-info, this might be either as absolute addresses for things
  ;; that won't move around in memory, or as relative offsets from
  ;; some point for things that might move.
  (start-pc-or-offset 0 :type address)
  (end-pc-or-offset 0 :type address)
  ;; the name of the function
  (name nil :type t)
  ;; sample count for this function
  (count 0 :type fixnum)
  ;; count including time spent in functions called from this one
  (accrued-count 0 :type fixnum)
  ;; the debug-info that this node was created from
  (debug-info nil :type t)
  ;; list of NODEs for functions calling this one
  (callers () :type list)
  ;; the call count for the function that corresponds to this node (or NIL
  ;; if call counting wasn't enabled for this function)
  (call-count nil :type (or null integer)))

;;; A cycle in a call graph.  The functions forming the cycle are
;;; found in the SCC-VERTICES slot of the VERTEX structure.
(defstruct (cycle (:include node)))

;;; An edge in a call graph.  EDGE-VERTEX is the function being
;;; called.
(defstruct (call (:include edge)
                 (:constructor make-call (vertex)))
  ;; number of times the call was sampled
  (count 1 :type sb-int:index))

(defvar *sample-interval* 0.01
  "Default number of seconds between samples.")
(declaim (type number *sample-interval*))

(defvar *alloc-interval* 4
  "Default number of allocation region openings between samples.")
(declaim (type number *alloc-interval*))

(defvar *max-samples* 50000
  "Default number of traces taken. This variable is somewhat misnamed:
each trace may actually consist of an arbitrary number of samples, depending
on the depth of the call stack.")
(declaim (type sb-int:index *max-samples*))

;;; Encapsulate all the information about a sampling run
(defstruct (samples)
  ;; When this vector fills up, we allocate a new one and copy over
  ;; the old contents.
  (vector (make-array (* *max-samples*
                         ;; Arbitrary guess at how many samples we'll be
                         ;; taking for each trace. The exact amount doesn't
                         ;; matter, this is just to decrease the amount of
                         ;; re-allocation that will need to be done.
                         10
                         ;; Each sample takes two cells in the vector
                         2))
          :type simple-vector)
  (trace-count 0 :type sb-int:index)
  (index 0 :type sb-int:index)
  (mode nil :type (member :cpu :alloc :time))
  (sample-interval (sb-int:missing-arg) :type number)
  (alloc-interval (sb-int:missing-arg) :type number)
  (max-depth most-positive-fixnum :type number)
  (max-samples (sb-int:missing-arg) :type sb-int:index)
  (sampled-threads nil :type list))

(defmethod print-object ((samples samples) stream)
  (print-unreadable-object (samples stream :type t :identity t)
    (let ((*print-array* nil))
      (call-next-method))))

(defmethod print-object ((call-graph call-graph) stream)
  (print-unreadable-object (call-graph stream :type t :identity t)
    (format stream "~d samples" (call-graph-nsamples call-graph))))

(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "~s [~d]" (node-name node) (node-index node))))

(defmethod print-object ((call call) stream)
  (print-unreadable-object (call stream :type t :identity t)
    (format stream "~s [~d]" (node-name (call-vertex call))
            (node-index (call-vertex call)))))

(deftype report-type ()
  '(member nil :flat :graph))

(defvar *sampling-mode* :cpu
  "Default sampling mode. :CPU for cpu profiling, :ALLOC for allocation
profiling, and :TIME for wallclock profiling.")
(declaim (type (member :cpu :alloc :time) *sampling-mode*))

(defvar *alloc-region-size*
  #-gencgc
  (get-page-size)
  #+gencgc
  (max sb-vm:gencgc-alloc-granularity sb-vm:gencgc-card-bytes))
(declaim (type number *alloc-region-size*))

(defvar *samples* nil)
(declaim (type (or null samples) *samples*))

(defvar *profiling* nil)
(declaim (type (member nil :alloc :cpu :time) *profiling*))
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
  (let ((ptr (sb-di::component-ptr-from-pc pc)))
    (cond ((sap= ptr (int-sap 0))
           (let ((name (sap-foreign-symbol pc)))
             (if name
                 (values (format nil "foreign function ~a" name)
                         (sap-int pc))
                 (values nil (sap-int pc)))))
          (t
           (let* ((code (sb-di::component-from-component-ptr ptr))
                  (code-header-len (* (sb-kernel:get-header-data code)
                                      sb-vm:n-word-bytes))
                  (pc-offset (- (sap-int pc)
                                (- (sb-kernel:get-lisp-obj-address code)
                                   sb-vm:other-pointer-lowtag)
                                code-header-len))
                  (df (sb-di::debug-fun-from-pc code pc-offset)))
             (cond ((typep df 'sb-di::bogus-debug-fun)
                    (values code (sap-int pc)))
                   (df
                    ;; The code component might be moved by the GC. Store
                    ;; a PC offset, and reconstruct the data in
                    ;; SAMPLE-PC-FROM-PC-OR-OFFSET.
                    (values df pc-offset))
                   (t
                    (values nil 0))))))))

(defun ensure-samples-vector (samples)
  (let ((vector (samples-vector samples))
        (index (samples-index samples)))
    ;; Allocate a new sample vector if the old one is full
    (if (= (length vector) index)
        (let ((new-vector (make-array (* 2 index))))
          (format *trace-output* "Profiler sample vector full (~a traces / ~a samples), doubling the size~%"
                  (samples-trace-count samples)
                  (truncate index 2))
          (replace new-vector vector)
          (setf (samples-vector samples) new-vector))
        vector)))

(declaim (inline record))
(defun record (samples pc)
  (declare (type system-area-pointer pc)
           (muffle-conditions compiler-note))
  (multiple-value-bind (info pc-or-offset)
      (debug-info pc)
    (let ((vector (ensure-samples-vector samples))
          (index (samples-index samples)))
      (declare (type simple-vector vector))
      ;; Allocate a new sample vector if the old one is full
      (when (= (length vector) index)
        (let ((new-vector (make-array (* 2 index))))
          (format *trace-output* "Profiler sample vector full (~a traces / ~a samples), doubling the size~%"
                  (samples-trace-count samples)
                  (truncate index 2))
          (replace new-vector vector)
          (setf vector new-vector
                (samples-vector samples) new-vector)))
      ;; For each sample, store the debug-info and the PC/offset into
      ;; adjacent cells.
      (setf (aref vector index) info
            (aref vector (1+ index)) pc-or-offset)))
  (incf (samples-index samples) 2))

(defun record-trace-start (samples)
  ;; Mark the start of the trace.
  (let ((vector (ensure-samples-vector samples)))
    (declare (type simple-vector vector))
    (setf (aref vector (samples-index samples))
          'trace-start))
  (incf (samples-index samples) 2))

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
          (let ((samples *samples*))
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
                    (record samples pc-ptr)
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
                        (return))))))
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
              (let* ((pc-ptr (sb-vm:context-pc scp))
                     (fp (sb-vm::context-register scp #.sb-vm::cfp-offset))
                     (ra (sap-ref-word
                          (int-sap fp)
                          (* sb-vm::lra-save-offset sb-vm::n-word-bytes))))
                (record samples pc-ptr)
                (record samples (int-sap ra))))))))))

;;; Return the start address of CODE.
(defun code-start (code)
  (declare (type sb-kernel:code-component code))
  (sap-int (sb-kernel:code-instructions code)))

;;; Return start and end address of CODE as multiple values.
(defun code-bounds (code)
  (declare (type sb-kernel:code-component code))
  (let* ((start (code-start code))
         (end (+ start (sb-kernel:%code-code-size code))))
    (values start end)))

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

(defvar *old-alloc-interval* nil)
(defvar *old-sample-interval* nil)

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
            *samples* (make-samples :max-depth max-depth
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
           *profiled-threads* nil)))
  (values))

(defun reset ()
  "Reset the profiler."
  (stop-profiling)
  (setq *sampling* nil)
  (setq *samples* nil)
  (values))

;;; Make a NODE for debug-info INFO.
(defun make-node (info)
  (flet ((clean-name (name)
           (if (and (consp name)
                    (member (first name)
                            '(sb-c::xep sb-c::tl-xep sb-c::&more-processor
                              sb-c::top-level-form
                              sb-c::&optional-processor)))
               (second name)
               name)))
    (typecase info
      (sb-kernel::code-component
       (multiple-value-bind (start end)
           (code-bounds info)
         (values
          (%make-node :name (or (sb-disassem::find-assembler-routine start)
                                (format nil "~a" info))
                      :debug-info info
                      :start-pc-or-offset start
                      :end-pc-or-offset end)
          info)))
      (sb-di::compiled-debug-fun
       (let* ((name (sb-di::debug-fun-name info))
              (cdf (sb-di::compiled-debug-fun-compiler-debug-fun info))
              (start-offset (sb-c::compiled-debug-fun-start-pc cdf))
              (end-offset (sb-c::compiled-debug-fun-elsewhere-pc cdf))
              (component (sb-di::compiled-debug-fun-component info))
              (start-pc (code-start component)))
         ;; Call graphs are mostly useless unless we somehow
         ;; distinguish a gazillion different (LAMBDA ())'s.
         (when (equal name '(lambda ()))
           (setf name (format nil "Unknown component: #x~x" start-pc)))
         (values (%make-node :name (clean-name name)
                             :debug-info info
                             :start-pc-or-offset start-offset
                             :end-pc-or-offset end-offset)
                 component)))
      (sb-di::debug-fun
       (%make-node :name (clean-name (sb-di::debug-fun-name info))
                   :debug-info info))
      (t
       (%make-node :name (coerce info 'string)
                   :debug-info info)))))

;;; One function can have more than one COMPILED-DEBUG-FUNCTION with
;;; the same name.  Reduce the number of calls to Debug-Info by first
;;; looking for a given PC in a red-black tree.  If not found in the
;;; tree, get debug info, and look for a node in a hash-table by
;;; function name.  If not found in the hash-table, make a new node.

(defvar *name->node*)

(defmacro with-lookup-tables (() &body body)
  `(let ((*name->node* (make-hash-table :test 'equal)))
     ,@body))

;;; Find or make a new node for INFO.  Value is the NODE found or
;;; made; NIL if not enough information exists to make a NODE for INFO.
(defun lookup-node (info)
  (when info
    (multiple-value-bind (new key)
        (make-node info)
      (when (eql (node-name new) 'call-counter)
        (return-from lookup-node (values nil nil)))
      (let* ((key (cons (node-name new) key))
             (found (gethash key *name->node*)))
        (cond (found
               (setf (node-start-pc-or-offset found)
                     (min (node-start-pc-or-offset found)
                          (node-start-pc-or-offset new)))
               (setf (node-end-pc-or-offset found)
                     (max (node-end-pc-or-offset found)
                          (node-end-pc-or-offset new)))
               found)
              (t
               (let ((call-count-info (gethash (node-name new)
                                               *encapsulations*)))
                 (when call-count-info
                   (setf (node-call-count new)
                         (car call-count-info))))
               (setf (gethash key *name->node*) new)
               new))))))

;;; Return a list of all nodes created by LOOKUP-NODE.
(defun collect-nodes ()
  (loop for node being the hash-values of *name->node*
        collect node))

;;; Value is a CALL-GRAPH for the current contents of *SAMPLES*.
(defun make-call-graph-1 (max-depth)
  (let ((elsewhere-count 0)
        visited-nodes)
    (with-lookup-tables ()
      (loop for i below (- (samples-index *samples*) 2) by 2
            with depth = 0
            for debug-info = (aref (samples-vector *samples*) i)
            for next-info = (aref (samples-vector *samples*)
                                  (+ i 2))
            do (if (eq debug-info 'trace-start)
                   (setf depth 0)
                   (let ((callee (lookup-node debug-info))
                         (caller (unless (eq next-info 'trace-start)
                                   (lookup-node next-info))))
                     (when (< depth max-depth)
                       (when (zerop depth)
                         (setf visited-nodes nil)
                         (cond (callee
                                (incf (node-accrued-count callee))
                                (incf (node-count callee)))
                               (t
                                (incf elsewhere-count))))
                       (incf depth)
                       (when callee
                         (push callee visited-nodes))
                       (when caller
                         (unless (member caller visited-nodes)
                           (incf (node-accrued-count caller)))
                         (when callee
                           (let ((call (find callee (node-edges caller)
                                             :key #'call-vertex)))
                             (pushnew caller (node-callers callee))
                             (if call
                                 (unless (member caller visited-nodes)
                                   (incf (call-count call)))
                                 (push (make-call callee)
                                       (node-edges caller))))))))))
      (let ((sorted-nodes (sort (collect-nodes) #'> :key #'node-count)))
        (loop for node in sorted-nodes and i from 1 do
              (setf (node-index node) i))
        (%make-call-graph :nsamples (samples-trace-count *samples*)
                          :sample-interval (if (eq (samples-mode *samples*)
                                                   :alloc)
                                               (samples-alloc-interval *samples*)
                                               (samples-sample-interval *samples*))
                          :sampling-mode (samples-mode *samples*)
                          :sampled-threads (samples-sampled-threads *samples*)
                          :elsewhere-count elsewhere-count
                          :vertices sorted-nodes)))))

;;; Reduce CALL-GRAPH to a dag, creating CYCLE structures for call
;;; cycles.
(defun reduce-call-graph (call-graph)
  (let ((cycle-no 0))
    (flet ((make-one-cycle (vertices edges)
             (let* ((name (format nil "<Cycle ~d>" (incf cycle-no)))
                    (count (loop for v in vertices sum (node-count v))))
               (make-cycle :name name
                           :index cycle-no
                           :count count
                           :scc-vertices vertices
                           :edges edges))))
      (reduce-graph call-graph #'make-one-cycle))))

;;; For all nodes in CALL-GRAPH, compute times including the time
;;; spent in functions called from them.  Note that the call-graph
;;; vertices are in reverse topological order, children first, so we
;;; will have computed accrued counts of called functions before they
;;; are used to compute accrued counts for callers.
(defun compute-accrued-counts (call-graph)
  (do-vertices (from call-graph)
    (setf (node-accrued-count from) (node-count from))
    (do-edges (call to from)
      (incf (node-accrued-count from)
            (round (* (/ (call-count call) (node-count to))
                      (node-accrued-count to)))))))

;;; Return a CALL-GRAPH structure for the current contents of
;;; *SAMPLES*.  The result contain a list of nodes sorted by self-time
;;; in the FLAT-NODES slot, and a dag in VERTICES, with call cycles
;;; reduced to CYCLE structures.
(defun make-call-graph (max-depth)
  (stop-profiling)
  (show-progress "~&Computing call graph ")
  (let ((call-graph (without-gcing (make-call-graph-1 max-depth))))
    (setf (call-graph-flat-nodes call-graph)
          (copy-list (graph-vertices call-graph)))
    (show-progress "~&Finding cycles")
    #+nil
    (reduce-call-graph call-graph)
    (show-progress "~&Propagating counts")
    #+nil
    (compute-accrued-counts call-graph)
    call-graph))


;;;; Reporting

(defun print-separator (&key (length 72) (char #\-))
  (format t "~&~V,,,V<~>~%" length char))

(defun samples-percent (call-graph count)
  (if (> count 0)
      (* 100.0 (/ count (call-graph-nsamples call-graph)))
      0))

(defun print-call-graph-header (call-graph)
  (let ((nsamples (call-graph-nsamples call-graph))
        (interval (call-graph-sample-interval call-graph))
        (ncycles (loop for v in (graph-vertices call-graph)
                       count (scc-p v))))
    (if (eq (call-graph-sampling-mode call-graph) :alloc)
        (format t "~2&Number of samples:     ~d~%~
                      Alloc interval:        ~a regions (approximately ~a kB)~%~
                      Total sampling amount: ~a regions (approximately ~a kB)~%~
                      Number of cycles:      ~d~%~
                      Sampled threads:~{~%   ~S~}~2%"
                nsamples
                interval
                (truncate (* interval *alloc-region-size*) 1024)
                (* nsamples interval)
                (truncate (* nsamples interval *alloc-region-size*) 1024)
                ncycles
                (call-graph-sampled-threads call-graph))
        (format t "~2&Number of samples:   ~d~%~
                      Sample interval:     ~f seconds~%~
                      Total sampling time: ~f seconds~%~
                      Number of cycles:    ~d~%~
                      Sampled threads:~{~% ~S~}~2%"
                nsamples
                interval
                (* nsamples interval)
                ncycles
                (call-graph-sampled-threads call-graph)))))

(declaim (type (member :samples :cumulative-samples) *report-sort-by*))
(defvar *report-sort-by* :samples
  "Method for sorting the flat report: either by :SAMPLES or by :CUMULATIVE-SAMPLES.")

(declaim (type (member :descending :ascending) *report-sort-order*))
(defvar *report-sort-order* :descending
  "Order for sorting the flat report: either :DESCENDING or :ASCENDING.")

(defun print-flat (call-graph &key (stream *standard-output*) max
                   min-percent (print-header t)
                   (sort-by *report-sort-by*)
                   (sort-order *report-sort-order*))
  (declare (type (member :descending :ascending) sort-order)
           (type (member :samples :cumulative-samples) sort-by))
  (let ((*standard-output* stream)
        (*print-pretty* nil)
        (total-count 0)
        (total-percent 0)
        (min-count (if min-percent
                       (round (* (/ min-percent 100.0)
                                 (call-graph-nsamples call-graph)))
                       0)))
    (when print-header
      (print-call-graph-header call-graph))
    (format t "~&           Self        Total        Cumul~%")
    (format t "~&  Nr  Count     %  Count     %  Count     %    Calls  Function~%")
    (print-separator)
    (let ((elsewhere-count (call-graph-elsewhere-count call-graph))
          (i 0)
          (nodes (stable-sort (copy-list (call-graph-flat-nodes call-graph))
                              (let ((cmp (if (eq :descending sort-order) #'> #'<)))
                                (multiple-value-bind (primary secondary)
                                    (if (eq :samples sort-by)
                                        (values #'node-count #'node-accrued-count)
                                        (values #'node-accrued-count #'node-count))
                                  (lambda (x y)
                                    (let ((cx (funcall primary x))
                                          (cy (funcall primary y)))
                                      (if (= cx cy)
                                          (funcall cmp (funcall secondary x) (funcall secondary y))
                                          (funcall cmp cx cy)))))))))
      (dolist (node nodes)
        (when (or (and max (> (incf i) max))
                  (< (node-count node) min-count))
          (return))
        (let* ((count (node-count node))
               (percent (samples-percent call-graph count))
               (accrued-count (node-accrued-count node))
               (accrued-percent (samples-percent call-graph accrued-count)))
          (incf total-count count)
          (incf total-percent percent)
          (format t "~&~4d ~6d ~5,1f ~6d ~5,1f ~6d ~5,1f ~8@a  ~s~%"
                  (incf i)
                  count
                  percent
                  accrued-count
                  accrued-percent
                  total-count
                  total-percent
                  (or (node-call-count node) "-")
                  (node-name node))
          (finish-output)))
      (print-separator)
      (format t "~&     ~6d ~5,1f~36a elsewhere~%"
              elsewhere-count
              (samples-percent call-graph elsewhere-count)
              ""))))

(defun print-cycles (call-graph)
  (when (some #'cycle-p (graph-vertices call-graph))
    (format t "~&                            Cycle~%")
    (format t "~& Count     %                   Parts~%")
    (do-vertices (node call-graph)
      (when (cycle-p node)
        (flet ((print-info (indent index count percent name)
                 (format t "~&~6d ~5,1f ~11@t ~V@t  ~s [~d]~%"
                         count percent indent name index)))
          (print-separator)
          (format t "~&~6d ~5,1f                ~a...~%"
                  (node-count node)
                  (samples-percent call-graph (cycle-count node))
                  (node-name node))
          (dolist (v (vertex-scc-vertices node))
            (print-info 4 (node-index v) (node-count v)
                        (samples-percent call-graph (node-count v))
                        (node-name v))))))
    (print-separator)
    (format t "~2%")))

(defun print-graph (call-graph &key (stream *standard-output*)
                    max min-percent)
  (let ((*standard-output* stream)
        (*print-pretty* nil))
    (print-call-graph-header call-graph)
    (print-cycles call-graph)
    (flet ((find-call (from to)
             (find to (node-edges from) :key #'call-vertex))
           (print-info (indent index count percent name)
             (format t "~&~6d ~5,1f ~11@t ~V@t  ~s [~d]~%"
                     count percent indent name index)))
      (format t "~&                               Callers~%")
      (format t "~&                 Total.     Function~%")
      (format t "~& Count     %  Count     %      Callees~%")
      (do-vertices (node call-graph)
        (print-separator)
        ;;
        ;; Print caller information.
        (dolist (caller (node-callers node))
          (let ((call (find-call caller node)))
            (print-info 4 (node-index caller)
                        (call-count call)
                        (samples-percent call-graph (call-count call))
                        (node-name caller))))
        ;; Print the node itself.
        (format t "~&~6d ~5,1f ~6d ~5,1f   ~s [~d]~%"
                (node-count node)
                (samples-percent call-graph (node-count node))
                (node-accrued-count node)
                (samples-percent call-graph (node-accrued-count node))
                (node-name node)
                (node-index node))
        ;; Print callees.
        (do-edges (call called node)
          (print-info 4 (node-index called)
                      (call-count call)
                      (samples-percent call-graph (call-count call))
                      (node-name called))))
      (print-separator)
      (format t "~2%")
      (print-flat call-graph :stream stream :max max
                  :min-percent min-percent :print-header nil))))

(defun report (&key (type :graph) max min-percent call-graph
               ((:sort-by *report-sort-by*) *report-sort-by*)
               ((:sort-order *report-sort-order*) *report-sort-order*)
               (stream *standard-output*) ((:show-progress *show-progress*)))
  "Report statistical profiling results.  The following keyword
   args are recognized:

   :TYPE <type>
      Specifies the type of report to generate.  If :FLAT, show
      flat report, if :GRAPH show a call graph and a flat report.
      If nil, don't print out a report.

   :STREAM <stream>
      Specify a stream to print the report on.  Default is
      *STANDARD-OUTPUT*.

   :MAX <max>
      Don't show more than <max> entries in the flat report.

   :MIN-PERCENT <min-percent>
      Don't show functions taking less than <min-percent> of the
      total time in the flat report.

   :SORT-BY <column>
      If :SAMPLES, sort flat report by number of samples taken.
      If :CUMULATIVE-SAMPLES, sort flat report by cumulative number of samples
      taken (shows how much time each function spent on stack.) Default
      is *REPORT-SORT-BY*.

   :SORT-ORDER <order>
      If :DESCENDING, sort flat report in descending order. If :ASCENDING,
      sort flat report in ascending order. Default is *REPORT-SORT-ORDER*.

   :SHOW-PROGRESS <bool>
     If true, print progress messages while generating the call graph.

   :CALL-GRAPH <graph>
     Print a report from <graph> instead of the latest profiling
     results.

Value of this function is a CALL-GRAPH object representing the
resulting call-graph, or NIL if there are no samples (eg. right after
calling RESET.)

Profiling is stopped before the call graph is generated."
  (cond (*samples*
         (let ((graph (or call-graph (make-call-graph most-positive-fixnum))))
           (ecase type
             (:flat
              (print-flat graph :stream stream :max max :min-percent min-percent))
             (:graph
              (print-graph graph :stream stream :max max :min-percent min-percent))
             ((nil)))
           graph))
        (t
         (format stream "~&; No samples to report.~%")
         nil)))

;;; Interface to DISASSEMBLE

(defun sample-pc-from-pc-or-offset (sample pc-or-offset)
  (etypecase sample
    ;; Assembly routines or foreign functions don't move around, so we've
    ;; stored a raw PC
    ((or sb-kernel:code-component string)
     pc-or-offset)
    ;; Lisp functions might move, so we've stored a offset from the
    ;; start of the code component.
    (sb-di::compiled-debug-fun
     (let* ((component (sb-di::compiled-debug-fun-component sample))
            (start-pc (code-start component)))
       (+ start-pc pc-or-offset)))))

(defun add-disassembly-profile-note (chunk stream dstate)
  (declare (ignore chunk stream))
  (when *samples*
    (let* ((location (+ (sb-disassem::seg-virtual-location
                         (sb-disassem:dstate-segment dstate))
                        (sb-disassem::dstate-cur-offs dstate)))
           (samples (loop with index = (samples-index *samples*)
                          for x from 0 below (- index 2) by 2
                          for last-sample = nil then sample
                          for sample = (aref (samples-vector *samples*) x)
                          for pc-or-offset = (aref (samples-vector *samples*)
                                                   (1+ x))
                          when (and sample (eq last-sample 'trace-start))
                          count (= location
                                   (sample-pc-from-pc-or-offset sample
                                                                pc-or-offset)))))
      (unless (zerop samples)
        (sb-disassem::note (format nil "~A/~A samples"
                                   samples (samples-trace-count *samples*))
                           dstate)))))

(pushnew 'add-disassembly-profile-note sb-disassem::*default-dstate-hooks*)


;;;; Call counting

;;; The following functions tell sb-sprof to do call count profiling
;;; for the named functions in addition to normal statistical
;;; profiling.  The benefit of this over using SB-PROFILE is that this
;;; encapsulation is a lot more lightweight, due to not needing to
;;; track cpu usage / consing. (For example, compiling asdf 20 times
;;; took 13s normally, 15s with call counting for all functions in
;;; SB-C, and 94s with SB-PROFILE profiling SB-C).

(defun profile-call-counts (&rest names)
  "Mark the functions named by NAMES as being subject to call counting
during statistical profiling. If a string is used as a name, it will
be interpreted as a package name. In this case call counting will be
done for all functions with names like X or (SETF X), where X is a symbol
with the package as its home package."
  (dolist (name names)
    (if (stringp name)
        (let ((package (find-package name)))
          (do-symbols (symbol package)
            (when (eql (symbol-package symbol) package)
              (dolist (function-name (list symbol (list 'setf symbol)))
                (profile-call-counts-for-function function-name)))))
        (profile-call-counts-for-function name))))

(defun profile-call-counts-for-function (function-name)
  (unless (gethash function-name *encapsulations*)
    (setf (gethash function-name *encapsulations*) nil)))

(defun unprofile-call-counts ()
  "Clear all call counting information. Call counting will be done for no
functions during statistical profiling."
  (clrhash *encapsulations*))

;;; Called when profiling is started to enable the call counting
;;; encapsulation. Wrap all the call counted functions
(defun enable-call-counting ()
  (maphash (lambda (k v)
             (declare (ignore v))
             (enable-call-counting-for-function k))
           *encapsulations*))

;;; Called when profiling is stopped to disable the encapsulation. Restore
;;; the original functions.
(defun disable-call-counting ()
  (maphash (lambda (k v)
             (when v
               (assert (cdr v))
               (without-package-locks
                 (setf (fdefinition k) (cdr v)))
               (setf (cdr v) nil)))
           *encapsulations*))

(defun enable-call-counting-for-function (function-name)
  (let ((info (gethash function-name *encapsulations*)))
    ;; We should never try to encapsulate an fdefn multiple times.
    (assert (or (null info)
                (null (cdr info))))
    (when (and (fboundp function-name)
               (or (not (symbolp function-name))
                   (and (not (special-operator-p function-name))
                        (not (macro-function function-name)))))
      (let* ((original-fun (fdefinition function-name))
             (info (cons 0 original-fun)))
        (setf (gethash function-name *encapsulations*) info)
        (without-package-locks
          (setf (fdefinition function-name)
                (sb-int:named-lambda call-counter (sb-int:&more more-context more-count)
                  (declare (optimize speed (safety 0)))
                  ;; 2^59 calls should be enough for anybody, and it
                  ;; allows using fixnum arithmetic on x86-64. 2^32
                  ;; isn't enough, so we can't do that on 32 bit platforms.
                  (incf (the (unsigned-byte 59)
                          (car info)))
                  (multiple-value-call original-fun
                    (sb-c:%more-arg-values more-context
                                           0
                                           more-count)))))))))

(provide 'sb-sprof)
