;;;; Graph-based reports for the statistical profiler
;;;;
;;;; Copyright (C) 2003 Gerd Moellmann <gerd.moellmann@t-online.de>
;;;; All rights reserved.

(in-package #:sb-sprof)


;;;; Graph Utilities

(defstruct (vertex (:constructor make-vertex)
                   (:constructor make-scc (scc-vertices edges)))
  (visited     nil :type boolean)
  (root        nil :type (or null vertex))
  (dfn           0 :type fixnum)
  (edges        () :type list)
  (scc-vertices () :type list :read-only t))

(defstruct edge
  (vertex (sb-int:missing-arg) :type vertex))

(defstruct graph
  (vertices () :type list))

(declaim (inline scc-p))
(defun scc-p (vertex)
  (not (null (vertex-scc-vertices vertex))))

(defmacro do-vertices ((vertex graph &optional predicate)
                       &body body)
  `(dolist (,vertex ,(if predicate
                         `(stable-sort (copy-list (graph-vertices ,graph))
                                       ,predicate)
                         `(graph-vertices ,graph)))
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


;;;; Call graph

(deftype address ()
  "Type used for addresses, for instance, program counters,
   code start/end locations etc."
  `(unsigned-byte ,sb-vm:n-machine-word-bits))

;;; A call graph.  Vertices are NODE structures, edges are CALL
;;; structures.
(defstruct (call-graph (:include graph)
                       (:constructor %make-call-graph))
  ;; the value of *SAMPLE-INTERVAL* or *ALLOC-INTERVAL* at the time
  ;; the graph was created (depending on the current allocation mode)
  (sample-interval (sb-int:missing-arg) :type (real (0)) :read-only t)
  ;; the sampling-mode that was used for the profiling run
  (sampling-mode   (sb-int:missing-arg) :type sampling-mode :read-only t)
  ;; number of samples taken
  (nsamples        (sb-int:missing-arg) :type sb-int:index :read-only t)
  (unique-trace-count (sb-int:missing-arg) :type sb-int:index :read-only t)
  ;; threads that have been sampled
  (sampled-threads '()                    :type list :read-only t)
  ;; sample count for samples not in any function
  (elsewhere-count (sb-int:missing-arg) :type sb-int:index :read-only t))

(defmethod print-object ((call-graph call-graph) stream)
  (print-unreadable-object (call-graph stream :type t :identity t)
    (format stream "~d samples" (call-graph-nsamples call-graph))))

;;; Used by SLIME
(defun call-graph-flat-nodes (graph)
  (graph-vertices graph))

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
  (name nil :type t :read-only t)
  ;; sample count for this function
  (count 0 :type fixnum)
  ;; count including time spent in functions called from this one
  (accrued-count 0 :type fixnum)
  ;; the debug-info that this node was created from
  (debug-info nil :type t :read-only t)
  ;; list of NODEs for functions calling this one
  (callers () :type list)
  ;; the call count for the function that corresponds to this node (or NIL
  ;; if call counting wasn't enabled for this function)
  (call-count nil :type (or null integer)))

(defun node-all-callers (node)
  (let ((seen   (make-hash-table :test #'eq))
        (result '()))
    (labels ((rec (current)
               (unless (or (eq current node)
                           (gethash current seen))
                 (setf (gethash current seen) t)
                 (push current result)
                 (map nil #'rec (node-callers current)))))
      (map nil #'rec (node-callers node)))
    result))

(defun in-caller-closure-p (caller called)
  (let ((seen (make-hash-table :test #'eq)))
    (labels ((rec (current)
               (cond ((eq current caller)
                      (return-from in-caller-closure-p t))
                     ((or (eq current called)
                          (gethash current seen)))
                     (t
                      (setf (gethash current seen) t)
                      (map nil #'rec (node-callers current))))))
      (map nil #'rec (node-callers called)))
    nil))

(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "~s [~d] ~:D sample~:P"
            (node-name node)
            (node-index node)
            (node-count node))))

;;; A cycle in a call graph.  The functions forming the cycle are
;;; found in the SCC-VERTICES slot of the VERTEX structure.
(defstruct (cycle (:include node)))

;;; An edge in a call graph.  EDGE-VERTEX is the function being
;;; called.
(defstruct (call (:include edge)
                 (:constructor make-call (vertex)))
  ;; number of times the call was sampled
  (count 1 :type sb-int:index))

(defmethod print-object ((call call) stream)
  (print-unreadable-object (call stream :type t :identity t)
    (format stream "~s [~d]" (node-name (call-vertex call))
            (node-index (call-vertex call)))))


;;; Graph construction

;;; One function can have more than one COMPILED-DEBUG-FUNCTION with
;;; the same name.  Reduce the number of calls to Debug-Info by first
;;; looking for a given PC in a red-black tree.  If not found in the
;;; tree, get debug info, and look for a node in a hash-table by
;;; function name.  If not found in the hash-table, make a new node.

;;; Make a NODE for debug-info INFO.
(defun make-node (info)
  (flet ((code-bounds (code)
           (let* ((start (sb-kernel:code-instructions code))
                  (end (sap+ start (sb-kernel:%code-text-size code))))
             (values (sap-int start) (sap-int end))))
         (clean-name (name)
           (if (and (consp name)
                    (member (first name)
                            '(sb-c::xep sb-c::tl-xep sb-c::&more-processor
                              sb-c::top-level-form
                              sb-c::&optional-processor)))
               (second name)
               name)))
    (typecase info
      (sb-kernel:code-component
       (multiple-value-bind (start end)
           (code-bounds info)
         (values
          (%make-node :name (format nil "~a" info)
                      :debug-info info
                      :start-pc-or-offset start
                      :end-pc-or-offset end)
          info)))
      (sb-di::compiled-debug-fun
       (let* ((name (sb-di::debug-fun-name info))
              (cdf (sb-di::compiled-debug-fun-compiler-debug-fun info))
              (start-offset (sb-c::compiled-debug-fun-start-pc cdf))
              (end-offset (sb-c::compiled-debug-fun-elsewhere-pc cdf))
              (component (sb-di::compiled-debug-fun-component info)))
         ;; Call graphs are mostly useless unless we somehow
         ;; distinguish a gazillion different (LAMBDA ())'s.
         (when (equal name '(lambda ()))
           (setf name (format nil "~a in ~a" name component)))
         (values (%make-node :name (clean-name name)
                             :debug-info info
                             :start-pc-or-offset start-offset
                             :end-pc-or-offset end-offset)
                 component)))
      (sb-di::debug-fun
       (%make-node :name (clean-name (sb-di::debug-fun-name info))
                   :debug-info info))
      (symbol
       (%make-node :name (string info)
                   :debug-info sb-fasl:*assembler-routines*))
      (t
       (%make-node :name (coerce info 'string)
                   :debug-info info)))))

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
(defun make-call-graph-1 (samples max-depth)
  (let ((elsewhere-count 0))
    (with-lookup-tables ()
      (map-traces
       (lambda (thread trace)
         (declare (ignore thread))
         (let ((visited-nodes '())
               (depth 0)
               (caller nil))
           (block calls
             (map-trace-pc-locs
              (lambda (debug-info pc-offset)
                (declare (ignore pc-offset))
                (when (> depth max-depth)
                  (return-from calls))
                (let ((callee (lookup-node debug-info)))
                  (when (and callee caller)
                    (let ((call (find callee (node-edges caller)
                                      :key #'call-vertex)))
                      (pushnew caller (node-callers callee))
                      (if call
                          (unless (member caller visited-nodes)
                            (incf (call-count call)))
                          (push (make-call callee)
                                (node-edges caller)))))
                  (when (and caller (not (member caller visited-nodes
                                                 :test #'eq)))
                    (incf (node-accrued-count caller))
                    (push caller visited-nodes))
                  (incf depth)
                  (setf caller callee)))
              trace))
           (cond
             (caller
              (incf (node-count caller))
              (incf (node-accrued-count caller)))
             (t
              (incf elsewhere-count)))))
       samples)
      (let ((sorted-nodes (sort (collect-nodes) #'> :key #'node-count)))
        (loop for node in sorted-nodes and i from 1 do
             (setf (node-index node) i))
        (%make-call-graph :nsamples (samples-trace-count samples)
                          :unique-trace-count (samples-unique-trace-count samples)
                          :sample-interval (if (eq (samples-mode samples) :alloc)
                                               1
                                               (samples-sample-interval samples))
                          :sampling-mode (samples-mode samples)
                          :sampled-threads (samples-sampled-threads samples)
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
(defun make-call-graph (samples max-depth)
  (stop-profiling)
  (when (zerop (length (samples-vector samples)))
    (show-progress "~&Aggregating raw data")
    (setf (values (samples-vector samples)
                  (samples-unique-trace-count samples)
                  (samples-sampled-threads samples))
          (convert-raw-data)))
  (show-progress "~&Computing call graph")
  ;; I _think_ the reason for pinning all code is that the graph logic
  ;; compares absolute PC locations. Wonderfully commented, it is.
  (let ((call-graph (with-code-pages-pinned (:dynamic)
                      (make-call-graph-1 samples max-depth))))
    (show-progress "~&Finding cycles")
    #+nil
    (reduce-call-graph call-graph)
    (show-progress "~&Propagating counts")
    #+nil
    (compute-accrued-counts call-graph)
    call-graph))
