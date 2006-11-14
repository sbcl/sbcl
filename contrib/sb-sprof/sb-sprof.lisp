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
  (:use #:cl #:sb-ext #:sb-unix #:sb-alien #:sb-sys)
  (:export #:*sample-interval* #:*max-samples*
           #:start-sampling #:stop-sampling #:with-sampling
           #:with-profiling #:start-profiling #:stop-profiling
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
  ;; the value of *Sample-Interval* at the time the graph was created
  (sample-interval (sb-impl::missing-arg) :type number)
  ;; number of samples taken
  (nsamples (sb-impl::missing-arg) :type sb-impl::index)
  ;; sample count for samples not in any function
  (elsewhere-count (sb-impl::missing-arg) :type sb-impl::index)
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
  ;; start and end address of the function's code
  (start-pc 0 :type address)
  (end-pc 0 :type address)
  ;; the name of the function
  (name nil :type t)
  ;; sample count for this function
  (count 0 :type fixnum)
  ;; count including time spent in functions called from this one
  (accrued-count 0 :type fixnum)
  ;; list of NODEs for functions calling this one
  (callers () :type list))

;;; A cycle in a call graph.  The functions forming the cycle are
;;; found in the SCC-VERTICES slot of the VERTEX structure.
(defstruct (cycle (:include node)))

;;; An edge in a call graph.  EDGE-VERTEX is the function being
;;; called.
(defstruct (call (:include edge)
                 (:constructor make-call (vertex)))
  ;; number of times the call was sampled
  (count 1 :type sb-impl::index))

;;; Info about a function in dynamic-space.  This is used to track
;;; address changes of functions during GC.
(defstruct (dyninfo (:constructor make-dyninfo (code start end)))
  ;; component this info is for
  (code (sb-impl::missing-arg) :type sb-kernel::code-component)
  ;; current start and end address of the component
  (start (sb-impl::missing-arg) :type address)
  (end (sb-impl::missing-arg) :type address)
  ;; new start address of the component, after GC.
  (new-start 0 :type address))

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

(defvar *sample-interval* 0.01
  "Default number of seconds between samples.")
(declaim (number *sample-interval*))

(defvar *max-samples* 50000
  "Default number of samples taken.")
(declaim (type sb-impl::index *max-samples*))

;; For every profiler event we store this many samples (frames 0-n on
;; the call stack).
(defconstant +sample-depth+
  #+(or x86 x86-64) 8
  #-(or x86 x86-64) 2)

;; We store two elements for each sample. The debug-info of the sample
;; and either its absolute PC or a PC offset, depending on the type of
;; the debug-info.
(defconstant +sample-size+ (* +sample-depth+ 2))

(defvar *samples* nil)
(declaim (type (or null simple-vector) *samples*))

(defvar *samples-index* 0)
(declaim (type sb-impl::index *samples-index*))

(defvar *profiling* nil)
(defvar *sampling* nil)
(declaim (type boolean *profiling* *sampling*))

(defvar *show-progress* nil)

(defvar *old-sampling* nil)

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
  `(let ((*sampling* ,on))
     ,@body))

;;; Return something serving as debug info for address PC.
(declaim (inline debug-info))
(defun debug-info (pc)
  (declare (type system-area-pointer pc))
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

(declaim (inline record))
(defun record (pc)
  (declare (type system-area-pointer pc))
  (multiple-value-bind (info pc-or-offset)
      (debug-info pc)
    ;; For each sample, store the debug-info and the PC/offset into
    ;; adjacent cells.
    (setf (aref *samples* *samples-index*) info
          (aref *samples* (1+ *samples-index*)) pc-or-offset))
  (incf *samples-index* 2))

;;; Ensure that only one thread at a time will be executing sigprof handler.
(defvar *sigprof-handler-lock* (sb-thread:make-mutex :name "SIGPROF handler"))

;;; SIGPROF handler.  Record current PC and return address in
;;; *SAMPLES*.
#+(or x86 x86-64)
(defun sigprof-handler (signal code scp)
  (declare (ignore signal code)
           (optimize speed (space 0))
           (type system-area-pointer scp))
  (sb-sys:without-interrupts
    (when (and *sampling*
               *samples*
               (< *samples-index* (length (the simple-vector *samples*))))
      (sb-sys:without-gcing
        (sb-thread:with-mutex (*sigprof-handler-lock*)
          (with-alien ((scp (* os-context-t) :local scp))
            (let* ((pc-ptr (sb-vm:context-pc scp))
                   (fp (sb-vm::context-register scp #.sb-vm::ebp-offset)))
              ;; For some reason completely bogus small values for the
              ;; frame pointer are returned every now and then, leading
              ;; to segfaults. Try to avoid these cases.
              ;;
              ;; FIXME: Do a more thorough sanity check on ebp, or figure
              ;; out why this is happening.
              ;; -- JES, 2005-01-11
              (when (< fp 4096)
                (dotimes (i +sample-depth+)
                  (record (int-sap 0)))
                (return-from sigprof-handler nil))
              (let ((fp (int-sap fp))
                    (ok t))
                (declare (type system-area-pointer fp pc-ptr))
                (dotimes (i +sample-depth+)
                  (record pc-ptr)
                  (when ok
                    (setf (values ok pc-ptr fp)
                          (sb-di::x86-call-context fp)))))))))))
  nil)

;; FIXME: On non-x86 platforms we don't yet walk the call stack deeper
;; than one level.
#-(or x86 x86-64)
(defun sigprof-handler (signal code scp)
  (declare (ignore signal code))
  (sb-sys:without-interrupts
    (when (and *sampling*
               (< *samples-index* (length *samples*)))
      (sb-sys:without-gcing
        (with-alien ((scp (* os-context-t) :local scp))
          (locally (declare (optimize (inhibit-warnings 2)))
            (let* ((pc-ptr (sb-vm:context-pc scp))
                   (fp (sb-vm::context-register scp #.sb-vm::cfp-offset))
                   (ra (sap-ref-word
                        (int-sap fp)
                        (* sb-vm::lra-save-offset sb-vm::n-word-bytes))))
              (record pc-ptr)
              (record (int-sap ra)))))))))

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
                                (max-samples '*max-samples*)
                                (reset nil)
                                show-progress
                                (report nil report-p))
                          &body body)
  "Repeatedly evaluate Body with statistical profiling turned on.
   The following keyword args are recognized:

   :Sample-Interval <seconds>
     Take a sample every <seconds> seconds.  Default is
     *Sample-Interval*.

   :Max-Samples <max>
     Repeat evaluating body until <max> samples are taken.
     Default is *Max-Samples*.

   :Report <type>
     If specified, call Report with :Type <type> at the end.

   :Reset <bool>
     It true, call Reset at the beginning."
  (declare (type report-type report))
  `(let ((*sample-interval* ,sample-interval)
         (*max-samples* ,max-samples))
     ,@(when reset '((reset)))
     (unwind-protect
          (progn
            (start-profiling)
            (loop
               (when (>= *samples-index* (length *samples*))
                 (return))
               ,@(when show-progress
                       `((format t "~&===> ~d of ~d samples taken.~%"
                                 (/ *samples-index* +sample-size+)
                                 *max-samples*)))
               (let ((.last-index. *samples-index*))
                 ,@body
                 (when (= .last-index. *samples-index*)
                   (warn "No sampling progress; possibly a profiler bug.")
                   (return)))))
       (stop-profiling))
     ,@(when report-p `((report :type ,report)))))

(defun start-profiling (&key (max-samples *max-samples*)
                        (sample-interval *sample-interval*)
                        (sampling t))
  "Start profiling statistically if not already profiling.
   The following keyword args are recognized:

   :Sample-Interval <seconds>
     Take a sample every <seconds> seconds.  Default is
     *Sample-Interval*.

   :Max-Samples <max>
     Maximum number of samples.  Default is *Max-Samples*.

   :Sampling <bool>
     If true, the default, start sampling right away.
     If false, Start-Sampling can be used to turn sampling on."
  (unless *profiling*
    (multiple-value-bind (secs usecs)
        (multiple-value-bind (secs rest)
            (truncate sample-interval)
          (values secs (truncate (* rest 1000000))))
      (setq *samples* (make-array (* max-samples +sample-size+)))
      (setq *samples-index* 0)
      (setq *sampling* sampling)
      (sb-sys:enable-interrupt sb-unix:sigprof #'sigprof-handler)
      (unix-setitimer :profile secs usecs secs usecs)
      (setq *profiling* t)))
  (values))

(defun stop-profiling ()
  "Stop profiling if profiling."
  (when *profiling*
    (unix-setitimer :profile 0 0 0 0)
    ;; Even with the timer shut down we cannot be sure that there is
    ;; no undelivered sigprof. Besides, leaving the signal handler
    ;; installed won't hurt.
    (setq *sampling* nil)
    (setq *profiling* nil))
  (values))

(defun reset ()
  "Reset the profiler."
  (stop-profiling)
  (setq *sampling* nil)
  (setq *samples* nil)
  (setq *samples-index* 0)
  (values))

;;; Make a NODE for debug-info INFO.
(defun make-node (info)
  (flet ((clean-name (name)
           (if (and (consp name)
                    (member (first name)
                            '(sb-c::xep sb-c::tl-xep sb-c::&more-processor
                              sb-c::hairy-arg-processor
                              sb-c::&optional-processor)))
               (second name)
               name)))
    (typecase info
      (sb-kernel::code-component
       (multiple-value-bind (start end)
           (code-bounds info)
         (%make-node :name (or (sb-disassem::find-assembler-routine start)
                               (format nil "~a" info))
                     :start-pc start :end-pc end)))
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
         (%make-node :name (clean-name name)
                     :start-pc (+ start-pc start-offset)
                     :end-pc (+ start-pc end-offset))))
      (sb-di::debug-fun
       (%make-node :name (clean-name (sb-di::debug-fun-name info))))
      (t
       (%make-node :name (coerce info 'string))))))

;;; One function can have more than one COMPILED-DEBUG-FUNCTION with
;;; the same name.  Reduce the number of calls to Debug-Info by first
;;; looking for a given PC in a red-black tree.  If not found in the
;;; tree, get debug info, and look for a node in a hash-table by
;;; function name.  If not found in the hash-table, make a new node.

(defvar *name->node*)

(defmacro with-lookup-tables (() &body body)
  `(let ((*name->node* (make-hash-table :test 'equal)))
     ,@body))

;;; Find or make a new node for address PC.  Value is the NODE found
;;; or made; NIL if not enough information exists to make a NODE for
;;; PC.
(defun lookup-node (info)
  (when info
    (let* ((new (make-node info))
           (key (cons (node-name new)
                      (node-start-pc new)))
           (found (gethash key *name->node*)))
      (cond (found
             (setf (node-start-pc found)
                   (min (node-start-pc found) (node-start-pc new)))
             (setf (node-end-pc found)
                   (max (node-end-pc found) (node-end-pc new)))
             found)
            (t
             (setf (gethash key *name->node*) new)
             new)))))

;;; Return a list of all nodes created by LOOKUP-NODE.
(defun collect-nodes ()
  (loop for node being the hash-values of *name->node*
        collect node))

;;; Value is a CALL-GRAPH for the current contents of *SAMPLES*.
(defun make-call-graph-1 (depth)
  (let ((elsewhere-count 0)
        visited-nodes)
    (with-lookup-tables ()
      (loop for i below (- *samples-index* 2) by 2
            for callee = (lookup-node (aref *samples* i))
            for caller = (lookup-node (aref *samples* (+ i 2)))
            do
            (when (and *show-progress* (plusp i))
              (cond ((zerop (mod i 1000))
                     (show-progress "~d" i))
                    ((zerop (mod i 100))
                     (show-progress "."))))
            (when (< (mod i +sample-size+) depth)
              (when (= (mod i +sample-size+) 0)
                (setf visited-nodes nil)
                (cond (callee
                       (incf (node-accrued-count callee))
                       (incf (node-count callee)))
                      (t
                       (incf elsewhere-count))))
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
                        (push (make-call callee) (node-edges caller))))))))
      (let ((sorted-nodes (sort (collect-nodes) #'> :key #'node-count)))
        (loop for node in sorted-nodes and i from 1 do
                (setf (node-index node) i))
        (%make-call-graph :nsamples (/ *samples-index* +sample-size+)
                          :sample-interval *sample-interval*
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
(defun make-call-graph (depth)
  (stop-profiling)
  (show-progress "~&Computing call graph ")
  (let ((call-graph (without-gcing (make-call-graph-1 depth))))
    (setf (call-graph-flat-nodes call-graph)
          (copy-list (graph-vertices call-graph)))
    (show-progress "~&Finding cycles")
    (reduce-call-graph call-graph)
    (show-progress "~&Propagating counts")
    #+nil (compute-accrued-counts call-graph)
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
    (format t "~2&Number of samples:   ~d~%~
                  Sample interval:     ~f seconds~%~
                  Total sampling time: ~f seconds~%~
                  Number of cycles:    ~d~2%"
            nsamples
            interval
            (* nsamples interval)
            ncycles)))

(defun print-flat (call-graph &key (stream *standard-output*) max
                   min-percent (print-header t))
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
    (format t "~&           Self        Cumul        Total~%")
    (format t "~&  Nr  Count     %  Count     %  Count     % Function~%")
    (print-separator)
    (let ((elsewhere-count (call-graph-elsewhere-count call-graph))
          (i 0))
      (dolist (node (call-graph-flat-nodes call-graph))
        (when (or (and max (> (incf i) max))
                  (< (node-count node) min-count))
          (return))
        (let* ((count (node-count node))
               (percent (samples-percent call-graph count))
               (accrued-count (node-accrued-count node))
               (accrued-percent (samples-percent call-graph accrued-count)))
          (incf total-count count)
          (incf total-percent percent)
          (format t "~&~4d ~6d ~5,1f ~6d ~5,1f ~6d ~5,1f ~s~%"
                  (node-index node)
                  count
                  percent
                  accrued-count
                  accrued-percent
                  total-count
                  total-percent
                  (node-name node))
          (finish-output)))
      (print-separator)
      (format t "~&    ~6d ~5,1f              elsewhere~%"
              elsewhere-count
              (samples-percent call-graph elsewhere-count)))))

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
      (format t "~&                 Cumul.     Function~%")
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
               (stream *standard-output*) ((:show-progress *show-progress*)))
  "Report statistical profiling results.  The following keyword
   args are recognized:

   :Type <type>
      Specifies the type of report to generate.  If :FLAT, show
      flat report, if :GRAPH show a call graph and a flat report.
      If nil, don't print out a report.

   :Stream <stream>
      Specify a stream to print the report on.  Default is
      *Standard-Output*.

   :Max <max>
      Don't show more than <max> entries in the flat report.

   :Min-Percent <min-percent>
      Don't show functions taking less than <min-percent> of the
      total time in the flat report.

   :Show-Progress <bool>
     If true, print progress messages while generating the call graph.

   :Call-Graph <graph>
     Print a report from <graph> instead of the latest profiling
     results.

   Value of this function is a Call-Graph object representing the
   resulting call-graph."
  (let ((graph (or call-graph (make-call-graph (1- +sample-depth+)))))
    (ecase type
      (:flat
       (print-flat graph :stream stream :max max :min-percent min-percent))
      (:graph
       (print-graph graph :stream stream :max max :min-percent min-percent))
      ((nil)))
    graph))

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
  (unless (zerop *samples-index*)
    (let* ((location
            (+ (sb-disassem::seg-virtual-location
                (sb-disassem:dstate-segment dstate))
               (sb-disassem::dstate-cur-offs dstate)))
           (samples (loop for x from 0 below *samples-index* by +sample-size+
                          for sample = (aref *samples* x)
                          for pc-or-offset = (aref *samples* (1+ x))
                          count (= location
                                   (sample-pc-from-pc-or-offset sample
                                                                pc-or-offset)))))
      (unless (zerop samples)
        (sb-disassem::note (format nil "~A/~A samples"
                                   samples (/ *samples-index* +sample-size+))
                           dstate)))))

(pushnew 'add-disassembly-profile-note sb-disassem::*default-dstate-hooks*)

;;; silly examples

(defun test-0 (n &optional (depth 0))
  (declare (optimize (debug 3)))
  (when (< depth n)
    (dotimes (i n)
      (test-0 n (1+ depth))
      (test-0 n (1+ depth)))))

(defun test ()
  (with-profiling (:reset t :max-samples 1000 :report :graph)
    (test-0 7)))


;;; provision
(provide 'sb-sprof)

;;; end of file
