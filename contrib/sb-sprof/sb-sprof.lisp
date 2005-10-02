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


;;;; AA Trees

;;; An AA tree is a red-black tree with the extra condition that left
;;; children may not be red.  This condition simplifies the red-black
;;; algorithm.  It eliminates half of the restructuring cases, and
;;; simplifies the delete algorithm.

(defstruct (aa-node (:conc-name aa-))
  (left  nil :type (or null aa-node))
  (right nil :type (or null aa-node))
  (level   0 :type integer)
  (data  nil :type t))

(defvar *null-node*
  (let ((node (make-aa-node)))
    (setf (aa-left node) node)
    (setf (aa-right node) node)
    node))

(defstruct aa-tree
  (root *null-node* :type aa-node))

(declaim (inline skew split rotate-with-left-child rotate-with-right-child))

(defun rotate-with-left-child (k2)
  (let ((k1 (aa-left k2)))
    (setf (aa-left k2) (aa-right k1))
    (setf (aa-right k1) k2)
    k1))

(defun rotate-with-right-child (k1)
  (let ((k2 (aa-right k1)))
    (setf (aa-right k1) (aa-left k2))
    (setf (aa-left k2) k1)
    k2))

(defun skew (aa)
  (if (= (aa-level (aa-left aa)) (aa-level aa))
      (rotate-with-left-child aa)
      aa))

(defun split (aa)
  (when (= (aa-level (aa-right (aa-right aa)))
           (aa-level aa))
    (setq aa (rotate-with-right-child aa))
    (incf (aa-level aa)))
  aa)

(macrolet ((def (name () &body body)
             (let ((name (sb-int::symbolicate 'aa- name)))
               `(defun ,name (item tree &key
                              (test-< #'<) (test-= #'=)
                              (node-key #'identity) (item-key #'identity))
                  (let ((.item-key. (funcall item-key item)))
                    (flet ((item-< (node)
                             (funcall test-< .item-key.
                                      (funcall node-key (aa-data node))))
                           (item-= (node)
                             (funcall test-= .item-key.
                                      (funcall node-key (aa-data node)))))
                      (declare (inline item-< item-=))
                      ,@body))))))

  (def insert ()
    (labels ((insert-into (aa)
               (cond ((eq aa *null-node*)
                      (setq aa (make-aa-node :data item
                                             :left *null-node*
                                             :right *null-node*)))
                     ((item-= aa)
                      (return-from insert-into aa))
                     ((item-< aa)
                      (setf (aa-left aa) (insert-into (aa-left aa))))
                     (t
                      (setf (aa-right aa) (insert-into (aa-right aa)))))
               (split (skew aa))))
      (setf (aa-tree-root tree)
            (insert-into (aa-tree-root tree)))))

  (def delete ()
    (let ((deleted-node *null-node*)
          (last-node nil))
      (labels ((remove-from (aa)
                 (unless (eq aa *null-node*)
                   (setq last-node aa)
                   (if (item-< aa)
                       (setf (aa-left aa) (remove-from (aa-left aa)))
                       (progn
                         (setq deleted-node aa)
                         (setf (aa-right aa) (remove-from (aa-right aa)))))
                   (cond ((eq aa last-node)
                          ;;
                          ;; If at the bottom of the tree, and item
                          ;; is present, delete it.
                          (when (and (not (eq deleted-node *null-node*))
                                     (item-= deleted-node))
                            (setf (aa-data deleted-node) (aa-data aa))
                            (setq deleted-node *null-node*)
                            (setq aa (aa-right aa))))
                         ;;
                         ;; Otherwise not at bottom of tree; rebalance.
                         ((or (< (aa-level (aa-left aa))
                                 (1- (aa-level aa)))
                              (< (aa-level (aa-right aa))
                                 (1- (aa-level aa))))
                          (decf (aa-level aa))
                          (when (> (aa-level (aa-right aa)) (aa-level aa))
                            (setf (aa-level (aa-right aa)) (aa-level aa)))
                          (setq aa (skew aa))
                          (setf (aa-right aa) (skew (aa-right aa)))
                          (setf (aa-right (aa-right aa))
                                (skew (aa-right (aa-right aa))))
                          (setq aa (split aa))
                          (setf (aa-right aa) (split (aa-right aa))))))
                 aa))
        (setf (aa-tree-root tree)
              (remove-from (aa-tree-root tree))))))

  (def find ()
    (let ((current (aa-tree-root tree)))
      (setf (aa-data *null-node*) item)
      (loop
         (cond ((eq current *null-node*)
                (return (values nil nil)))
               ((item-= current)
                (return (values (aa-data current) t)))
               ((item-< current)
                (setq current (aa-left current)))
               (t
                (setq current (aa-right current))))))))


;;;; Other Utilities

;;; Sort the subsequence of Vec in the interval [From To] using
;;; comparison function Test.  Assume each element to sort consists of
;;; Element-Size array slots, and that the slot Key-Offset contains
;;; the sort key.
(defun qsort (vec &key (element-size 1) (key-offset 0)
              (from 0) (to (- (length vec) element-size)))
  (declare (type fixnum to from element-size key-offset))
  (declare (type (simple-array address) vec))
  (labels ((rotate (i j)
             (declare (fixnum i j))
             (loop repeat element-size
                   for i from i and j from j do
                     (rotatef (aref vec i) (aref vec j))))
           (key (i)
             (aref vec (+ i key-offset)))
           (rec-sort (from to)
             (declare (fixnum to from))
             (when (> to from)
               (let* ((mid (* element-size
                              (round (+ (/ from element-size)
                                        (/ to element-size))
                                     2)))
                      (i from)
                      (j (+ to element-size))
                      (p (key mid)))
                 (declare (fixnum mid i j))
                 (rotate mid from)
                 (loop
                    (loop do (incf i element-size)
                          until (or (> i to)
                                    ;; QSORT used to take a test
                                    ;; parameter which was funcalled
                                    ;; here. This caused some consing,
                                    ;; which is problematic since
                                    ;; QSORT is indirectly called in
                                    ;; an after-gc-hook. So just
                                    ;; hardcode >, which would've been
                                    ;; used for the test anyway.
                                    ;; --JES, 2004-07-09
                                    (> p (key i))))
                    (loop do (decf j element-size)
                          until (or (<= j from)
                                    ;; As above.
                                    (> (key j) p)))
                    (when (< j i) (return))
                    (rotate i j))
                 (rotate from j)
                 (rec-sort from (- j element-size))
                 (rec-sort i to)))))
    (rec-sort from to)
    vec))


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

(defconstant +sample-size+
  #+(or x86 x86-64) 8
  #-(or x86 x86-64) 2)

(defvar *samples* nil)
(declaim (type (or null (vector address)) *samples*))

(defvar *samples-index* 0)
(declaim (type sb-impl::index *samples-index*))

(defvar *profiling* nil)
(defvar *sampling* nil)
(declaim (type boolean *profiling* *sampling*))

(defvar *dynamic-space-code-info* ())
(declaim (type list *dynamic-space-code-info*))

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

(defun sort-samples (key-offset)
  "Sort *Samples* using comparison Test.  Key must be one of
   :Pc or :Return-Pc for sorting by pc or return pc."
  (when (plusp *samples-index*)
    (qsort *samples*
           :from 0
           :to (- *samples-index* +sample-size+)
           :element-size +sample-size+
           :key-offset key-offset)))

(defun record (pc)
  (declare (type address pc))
  (setf (aref *samples* *samples-index*) pc)
  (incf *samples-index*))

;;; SIGPROF handler.  Record current PC and return address in
;;; *SAMPLES*.
#+(or x86 x86-64)
(defun sigprof-handler (signal code scp)
  (declare (ignore signal code) (type system-area-pointer scp))
  (when (and *sampling*
             (< *samples-index* (length *samples*)))
    (sb-sys:without-gcing
        (locally (declare (optimize (inhibit-warnings 2)))
          (with-alien ((scp (* os-context-t) :local scp))
            ;; For some reason completely bogus small values for the
            ;; frame pointer are returned every now and then, leading
            ;; to segfaults. Try to avoid these cases.
            ;;
            ;; FIXME: Do a more thorough sanity check on ebp, or figure
            ;; out why this is happening.
            ;; -- JES, 2005-01-11
            (when (< (sb-vm::context-register scp #.sb-vm::ebp-offset)
                     4096)
              (dotimes (i +sample-size+)
                (record 0))
              (return-from sigprof-handler nil))
            (let* ((pc-ptr (sb-vm:context-pc scp))
                   (fp (sb-vm::context-register scp #.sb-vm::ebp-offset)))
              (record (sap-int pc-ptr))
              (let ((fp (int-sap fp))
                    ra)
                (dotimes (i (1- +sample-size+))
                  (cond (fp
                         (setf (values ra fp)
                               (sb-di::x86-call-context fp :depth i))
                         (record (if ra
                                     (sap-int ra)
                                     0)))
                        (t
                         (record 0)))))))))))

;; FIXME: On non-x86 platforms we don't yet walk the call stack deeper
;; than one level.
#-(or x86 x86-64)
(defun sigprof-handler (signal code scp)
  (declare (ignore signal code))
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
           (record (sap-int pc-ptr))
           (record ra)))))))

;;; Map function FN over code objects in dynamic-space.  FN is called
;;; with two arguments, the object and its size in bytes.
(defun map-dynamic-space-code (fn)
  (flet ((call-if-code (obj obj-type size)
           (declare (ignore obj-type))
           (when (sb-kernel:code-component-p obj)
             (funcall fn obj size))))
    (sb-vm::map-allocated-objects #'call-if-code :dynamic)))

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

;;; Record the addresses of dynamic-space code objects in
;;; *DYNAMIC-SPACE-CODE-INFO*.  Call this with GC disabled.
(defun record-dyninfo ()
  (setf *dynamic-space-code-info* nil)
  (flet ((record-address (code size)
           (declare (ignore size))
           (multiple-value-bind (start end)
               (code-bounds code)
             (push (make-dyninfo code start end)
                   *dynamic-space-code-info*))))
    (map-dynamic-space-code #'record-address)))

(defun adjust-samples (offset)
  (sort-samples offset)
  (let ((sidx 0))
    (declare (type sb-impl::index sidx))
    (dolist (info *dynamic-space-code-info*)
      (unless (= (dyninfo-new-start info) (dyninfo-start info))
        (let ((pos (do ((i sidx (+ i +sample-size+)))
                       ((= i *samples-index*) nil)
                     (declare (type sb-impl::index i))
                     (when (<= (dyninfo-start info)
                               (aref *samples* (+ i offset))
                               (dyninfo-end info))
                       (return i)))))
          (when pos
            (setq sidx pos)
            (loop with delta = (- (dyninfo-new-start info)
                                  (dyninfo-start info))
                  for j from sidx below *samples-index* by +sample-size+
                  as pc = (aref *samples* (+ j offset))
                  while (<= (dyninfo-start info) pc (dyninfo-end info)) do
                    (incf (aref *samples* (+ j offset)) delta)
                    (incf sidx +sample-size+))))))))

;;; This runs from *AFTER-GC-HOOKS*.  Adjust *SAMPLES* for address
;;; changes of dynamic-space code objects.
(defun adjust-samples-for-address-changes ()
  (sb-sys:without-gcing
   (turn-off-sampling)
   (setq *dynamic-space-code-info*
         (sort *dynamic-space-code-info* #'> :key #'dyninfo-start))
   (dolist (info *dynamic-space-code-info*)
     (setf (dyninfo-new-start info)
           (code-start (dyninfo-code info))))
   (progn
     (dotimes (i +sample-size+)
       (adjust-samples i)))
   (dolist (info *dynamic-space-code-info*)
     (let ((size (- (dyninfo-end info) (dyninfo-start info))))
       (setf (dyninfo-start info) (dyninfo-new-start info))
       (setf (dyninfo-end info) (+ (dyninfo-new-start info) size))))
   (turn-on-sampling)))

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
            (return))))
     (stop-profiling)
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
      (setq *samples* (make-array (* max-samples +sample-size+)
                                  :element-type 'address))
      (setq *samples-index* 0)
      (setq *sampling* sampling)
      ;; Disabled for now, since this was causing some problems with the
      ;; sampling getting turned off completely. --JES, 2004-06-19
      ;;
      ;; BEFORE-GC-HOOKS have exceedingly bad interactions with
      ;; threads.  -- CSR, 2004-06-21
      ;;
      ;; (pushnew 'turn-off-sampling *before-gc-hooks*)
      (pushnew 'adjust-samples-for-address-changes *after-gc-hooks*)
      (record-dyninfo)
      (sb-sys:enable-interrupt sb-unix::sigprof #'sigprof-handler)
      (unix-setitimer :profile secs usecs secs usecs)
      (setq *profiling* t)))
  (values))

(defun stop-profiling ()
  "Stop profiling if profiling."
  (when *profiling*
    (setq *after-gc-hooks*
          (delete 'adjust-samples-for-address-changes *after-gc-hooks*))
    (unix-setitimer :profile 0 0 0 0)
    (sb-sys:enable-interrupt sb-unix::sigprof :default)
    (setq *sampling* nil)
    (setq *profiling* nil))
  (values))

(defun reset ()
  "Reset the profiler."
  (stop-profiling)
  (setq *sampling* nil)
  (setq *dynamic-space-code-info* ())
  (setq *samples* nil)
  (setq *samples-index* 0)
  (values))

;;; Make a NODE for debug-info INFO.
(defun make-node (info)
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
       (%make-node :name name
                   :start-pc (+ start-pc start-offset)
                   :end-pc (+ start-pc end-offset))))
    (sb-di::debug-fun
     (%make-node :name (sb-di::debug-fun-name info)))
    (t
     (%make-node :name (coerce info 'string)))))

;;; Return something serving as debug info for address PC.  If we can
;;; get something from SB-DI:DEBUG-FUNCTION-FROM-PC, return that.
;;; Otherwise, if we can determine a code component, return that.
;;; Otherwise return nil.
(defun debug-info (pc)
  (declare (type address pc))
  (let ((ptr (sb-di::component-ptr-from-pc (int-sap pc))))
    (cond ((sap= ptr (int-sap 0))
           (let ((name (sap-foreign-symbol (int-sap pc))))
             (when name
               (format nil "foreign function ~a" name))))
          (t
           (let* ((code (sb-di::component-from-component-ptr ptr))
                  (code-header-len (* (sb-kernel:get-header-data code)
                                      sb-vm:n-word-bytes))
                  (pc-offset (- pc
                                (- (sb-kernel:get-lisp-obj-address code)
                                   sb-vm:other-pointer-lowtag)
                                code-header-len))
                  (df (ignore-errors (sb-di::debug-fun-from-pc code
                                                               pc-offset))))
             (or df
                 code))))))


;;; One function can have more than one COMPILED-DEBUG-FUNCTION with
;;; the same name.  Reduce the number of calls to Debug-Info by first
;;; looking for a given PC in a red-black tree.  If not found in the
;;; tree, get debug info, and look for a node in a hash-table by
;;; function name.  If not found in the hash-table, make a new node.

(defvar *node-tree*)
(defvar *name->node*)

(defmacro with-lookup-tables (() &body body)
  `(let ((*node-tree* (make-aa-tree))
         (*name->node* (make-hash-table :test 'equal)))
     ,@body))

(defun tree-find (item)
  (flet ((pc/node-= (pc node)
           (<= (node-start-pc node) pc (node-end-pc node)))
         (pc/node-< (pc node)
           (< pc (node-start-pc node))))
    (aa-find item *node-tree* :test-= #'pc/node-= :test-< #'pc/node-<)))

(defun tree-insert (item)
  (flet ((node/node-= (x y)
           (<= (node-start-pc y) (node-start-pc x) (node-end-pc y)))
         (node/node-< (x y)
           (< (node-start-pc x) (node-start-pc y))))
    (aa-insert item *node-tree* :test-= #'node/node-= :test-< #'node/node-<)))

;;; Find or make a new node for address PC.  Value is the NODE found
;;; or made; NIL if not enough information exists to make a NODE for
;;; PC.
(defun lookup-node (pc)
  (declare (type address pc))
  (or (tree-find pc)
      (let ((info (debug-info pc)))
        (when info
          (let* ((new (make-node info))
                 (found (gethash (node-name new) *name->node*)))
            (cond (found
                   (setf (node-start-pc found)
                         (min (node-start-pc found) (node-start-pc new)))
                   (setf (node-end-pc found)
                         (max (node-end-pc found) (node-end-pc new)))
                   found)
                  (t
                   (setf (gethash (node-name new) *name->node*) new)
                   (tree-insert new)
                   new)))))))

;;; Return a list of all nodes created by LOOKUP-NODE.
(defun collect-nodes ()
  (loop for node being the hash-values of *name->node*
        collect node))

;;; Value is a CALL-GRAPH for the current contents of *SAMPLES*.
(defun make-call-graph-1 (depth)
  (let ((elsewhere-count 0)
        visited-nodes)
    (with-lookup-tables ()
      (loop for i below (1- *samples-index*) ;; by +sample-size+
            as pc = (aref *samples* i)
            as return-pc = (aref *samples* (1+ i))
            as callee = (lookup-node pc)
            as caller =
              (when (and callee (/= return-pc +unknown-address+))
                (let ((caller (lookup-node return-pc)))
                  (when caller
                    caller)))
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
  (* 100.0 (/ count (call-graph-nsamples call-graph))))

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
  (let ((graph (or call-graph (make-call-graph (1- +sample-size+)))))
    (ecase type
      (:flat
       (print-flat graph :stream stream :max max :min-percent min-percent))
      (:graph
       (print-graph graph :stream stream :max max :min-percent min-percent))
      ((nil)))
    graph))

;;; Interface to DISASSEMBLE

(defun add-disassembly-profile-note (chunk stream dstate)
  (declare (ignore chunk stream))
  (unless (zerop *samples-index*)
    (let* ((location
            (+ (sb-disassem::seg-virtual-location
                (sb-disassem:dstate-segment dstate))
               (sb-disassem::dstate-cur-offs dstate)))
           (samples (loop for x from 0 below *samples-index* by +sample-size+
                          summing (if (= (aref *samples* x) location)
                                      1
                                      0))))
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
