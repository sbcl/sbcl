;;;; This file contains code for the iterative spilling/coloring
;;;; register allocator

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-REGALLOC")
;;;; Useful references to understand the algorithms and decisions made
;;;; in this allocator.
;;;;
;;;; For more background:
;;;;
;;;; Chaitin, Gregory J. "Register allocation & spilling via graph
;;;; coloring." ACM Sigplan Notices. Vol. 17. No. 6. ACM, 1982.
;;;; (http://web.eecs.umich.edu/~mahlke/courses/583f12/reading/chaitin82.pdf)
;;;;
;;;; Briggs, Preston. "Register allocation via graph coloring."
;;;; Diss. Rice University, 1992.
;;;; (http://www.cs.utexas.edu/~mckinley/380C/lecs/briggs-thesis-1992.pdf)
;;;;
;;;; Shorter or more directly applied articles:
;;;;
;;;; Briggs, Preston, Keith D. Cooper, and Linda Torczon.
;;;; "Improvements to graph coloring register allocation."  ACM
;;;; Transactions on Programming Languages and Systems (TOPLAS) 16.3
;;;; (1994): 428-455.
;;;; (http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.30.2616)
;;;;
;;;; Smith, Michael D., Norman Ramsey, and Glenn Holloway.  "A
;;;; generalized algorithm for graph-coloring register allocation."
;;;; ACM SIGPLAN Notices. Vol. 39. No. 6. ACM, 2004.
;;;; (http://www.cs.tufts.edu/~nr/pubs/gcra-abstract.html)
;;;;
;;;; Cooper, Keith D., Anshuman Dasgupta, and Jason Eckhardt.
;;;; "Revisiting graph coloring register allocation: A study of the
;;;; Chaitin-Briggs and Callahan-Koblenz algorithms." Languages and
;;;; Compilers for Parallel Computing. Springer Berlin Heidelberg,
;;;; 2006. 1-16.
;;;; (http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.107.9598)

;;; Interference graph data structure

;; vertex in an interference graph
(defstruct (vertex
             (:include sset-element)
             (:copier nil)
             (:constructor %make-vertex (tn element-size size-mask pack-type)))
  ;; full incidence set. has to support iteration and efficient
  ;; membership test.
  (full-incidence  (make-sset) :type sset :read-only t)
  ;; A mask of the colors of the neighbors
  (neighbor-colors 0 :type sb-vm:finite-sc-offset-map)
  ;; For each bit of the NEIGHBOR-COLORS mask this maintains the
  ;; number of neighbors that share that color. Needed to recolor the vertex.
  (neighbor-color-counts (load-time-value
                          (make-array 0 :element-type '(unsigned-byte 32)))
   :type (simple-array (unsigned-byte 32) (*)))
  ;; list of potential locations in the TN's preferred SB for the
  ;; vertex, taking into account reserve locations and preallocated
  ;; TNs.
  (initial-domain 0 :type sc-locations)
  (initial-domain-size 0 :type #.`(integer 0 ,sb-vm:finite-sc-offset-limit))
  ;; TN this is a vertex for.
  (tn           nil :type tn :read-only t)
  (element-size nil :type (integer 1 8) :read-only t)
  ;; ELEMENT-SIZE set bits, which can be then just shifted left by some
  ;; color and used for testing NEIGHBOR-COLORS
  (size-mask nil :type (unsigned-byte 8) :read-only t)
  ;; type of packing necessary. We should only have to determine
  ;; colors for :normal TNs/vertices
  (pack-type    nil :type (member :normal :wired :restricted) :read-only t)
  ;; (tn-spill-cost (vertex-tn vertex))
  (spill-cost   0   :type fixnum)
  ;; color offset
  (color nil :type (or fixnum null)))

(defprinter (vertex)
  tn
  element-size
  pack-type
  spill-cost
  color)

(declaim (inline make-vertex))
(defun make-vertex (tn pack-type)
  (let ((size (sc-element-size (tn-sc tn))))
    (%make-vertex tn
                  size
                  (ldb (byte size 0) -1)
                  pack-type)))

(declaim (inline vertex-sc))
(defun vertex-sc (vertex)
  (tn-sc (vertex-tn vertex)))

;; interference graph
(defstruct (interference-graph
             (:copier nil)
             (:constructor %make-interference-graph)
             (:conc-name #:ig-))
  ;; sorted set of yet-uncolored (and not necessarily spilled)
  ;; vertices: vertices with lower spill cost come first.
  (vertices nil :type list)
  ;; unsorted set of precolored vertices.
  (precolored-vertices nil :type list :read-only t))

;;; Interference graph construction
;;;
;;; First, compute conflict edges between vertices that aren't
;;; precolored: precolored vertices have already been handled via
;;; domain initialisation.
;;;
;;; This area is ripe for hard-to-explain bugs. If PACK-COLORED starts
;;; AVERing out, it may be useful to comment out most of
;;; INSERT-CONFLICT-EDGES and test for TNS-CONFLICT in a double loop
;;; over the concatenation of all three vertex lists.

;; Adjoin symmetric edge (A,B) to both A and B. Unless
;; PERHAPS-REDUNDANT, aver that these edges are new.
(defun insert-one-edge (a b &optional perhaps-redundant)
  (declare (type vertex a b))
  (aver (neq a b))
  ;; not even in the same storage base => no conflict;
  ;; or one is pre-allocated => handled via domain.
  (unless (or (neq (sc-sb (vertex-sc a)) (sc-sb (vertex-sc b)))
              (tn-offset (vertex-tn a))
              (tn-offset (vertex-tn b)))
    (aver (or (sset-adjoin b (vertex-full-incidence a))
              perhaps-redundant))
    (aver (or (sset-adjoin a (vertex-full-incidence b))
              perhaps-redundant))))

;; Partition the global TNs that appear in that IR2 block, between
;; those that are LIVE throughout the block and the rest.
(defun block-gtns (block)
  (declare (type ir2-block block))
  (collect ((live-gtns)
            (gtns))
    (do ((conflict (ir2-block-global-tns block)
                   (global-conflicts-next-blockwise
                    conflict)))
        ((null conflict)
         (values (live-gtns) (gtns)))
      (let ((tn (global-conflicts-tn conflict)))
        (awhen (and (not (tn-offset tn))
                    (not (eql :component (tn-kind tn)))
                    (tn-vertex tn))
          (if (eql (global-conflicts-kind conflict) :live)
              (live-gtns it)
              (gtns (cons it conflict))))))))

;; Scan CONFLICTS for conflicts with TNs that come after VERTEX in the
;; local TN order.  Also, add edges with all LIVE-GTNs: they conflict
;; with everything but are absent from conflict bitvectors.
(defun insert-block-local-conflicts-for (vertex number conflicts
                                         local-tns ltn-count
                                         gtn-p live-gtns)
  (declare (type vertex vertex) (type local-tn-number number)
           (type local-tn-bit-vector conflicts)
           (type local-tn-vector local-tns) (type local-tn-count ltn-count)
           (type list live-gtns))
  ;; conflict with all live gtns
  (dolist (b live-gtns)
    (insert-one-edge vertex b gtn-p))
  ;; and add conflicts if LTN number > number
  (loop
    with local = (tn-local (vertex-tn vertex))
    for j from (1+ number) below ltn-count
    when (plusp (sbit conflicts j))
      do (let ((b (aref local-tns j)))
           (when (tn-p b)
             (aver (or gtn-p
                       (tn-global-conflicts b)
                       (eq local (tn-local b))))
             (awhen (tn-vertex b)
               (insert-one-edge vertex it (and gtn-p
                                               (tn-global-conflicts b))))))))

;; Compute all conflicts in a single IR2 block
(defun insert-block-local-conflicts (block)
  (declare (type ir2-block block))
  (let* ((local-tns (ir2-block-local-tns block))
         (n (ir2-block-local-tn-count block)))
    (multiple-value-bind (live-gtns gtns)
        (block-gtns block)
      ;; all live gtns conflict with one another
      (loop for (a . rest) on live-gtns do
        (dolist (b rest)
          (insert-one-edge a b t)))
      ;; normal gtn-* edges
      (loop for (a . conflict) in gtns do
        (let ((number (global-conflicts-number conflict))
              (conflicts (global-conflicts-conflicts conflict)))
          (insert-block-local-conflicts-for a number conflicts
                                            local-tns n
                                            t live-gtns)))
      ;; local-* interference
      (dotimes (i n)
        (binding* ((a (aref local-tns i))
                   (vertex (and (tn-p a)
                                (tn-vertex a)) :exit-if-null)
                   (conflicts (tn-local-conflicts a)))
          (unless (or (tn-offset a)
                      (tn-global-conflicts a))
            (insert-block-local-conflicts-for vertex i conflicts
                                              local-tns n
                                              nil live-gtns)))))))

;; Compute all conflict edges for component
;; COMPONENT-VERTICES is a list of vertices for :component TNs,
;; GLOBAL-VERTICES a list of vertices for TNs with global conflicts,
;; and LOCAL-VERTICES a list of vertices for local TNs.
(defun insert-conflict-edges (component
                              component-vertices global-vertices
                              local-vertices)
  (declare (type list component-vertices global-vertices local-vertices))
  ;; COMPONENT vertices conflict with everything
  (loop for (a . rest) on component-vertices
        do (dolist (b rest)
             (insert-one-edge a b))
           (dolist (b global-vertices)
             (insert-one-edge a b))
           (dolist (b local-vertices)
             (insert-one-edge a b)))
  ;; Find the other edges by enumerating IR2 blocks
  (do-ir2-blocks (block component)
    (insert-block-local-conflicts block)))

;;; Interference graph construction, the rest: annotating vertex
;;; structures, and bundling up the conflict graph.
;;;
;;; Also, permanently removing a vertex from a graph, without
;;; reconstructing it from scratch.

;; Supposing that TN is restricted to its preferred SC, what locations
;; are available?
(declaim (ftype (sfunction (tn) sc-locations) restricted-tn-locations))
(defun restricted-tn-locations (tn)
  (declare (type tn tn))
  (let* ((sc (tn-sc tn))
         (reserve (sc-reserve-locations sc))
         (available-locations (logand (sc-locations sc)
                                      (lognot reserve)))
         (locations 0))
    (declare (type sc-locations locations))
    (do-sc-locations (location available-locations locations
                               (sc-element-size sc))
      (unless (conflicts-in-sc tn sc location)
        (setf (ldb (byte 1 location) locations) 1)))))

;; walk over vertices, precomputing as much information as possible,
;; and partitioning according to their kind.
;; Return the partition
(defun prepare-vertices (vertices)
  (let (component-vertices
        global-vertices
        local-vertices)
    (loop for i upfrom 0
          for vertex in vertices
          do (let* ((tn (vertex-tn vertex))
                    (offset (tn-offset tn))
                    (locs (if offset
                              (sc-offset-to-sc-locations offset)
                              (restricted-tn-locations tn))))
               (aver (not (unbounded-tn-p tn)))
               (setf (vertex-number vertex) i
                     (vertex-initial-domain vertex) locs
                     (vertex-initial-domain-size vertex)
                     (sc-locations-count locs)
                     (vertex-color vertex) offset
                     (vertex-spill-cost vertex) (max (tn-cost tn) 1)
                     (tn-vertex tn) vertex)
               (cond (offset) ; precolored -> no need to track conflict
                     ((eql :component (tn-kind tn))
                      (push vertex component-vertices))
                     ((tn-global-conflicts tn)
                      (push vertex global-vertices))
                     (t
                      (aver (tn-local tn))
                      (push vertex local-vertices)))))
    (values component-vertices global-vertices local-vertices)))

;; Construct the interference graph for these vertices in the component.
;; All TNs types are included in the graph, both with offset and without,
;; but only those requiring coloring appear in the VERTICES slot.
(defun make-interference-graph (vertices component)
  (multiple-value-bind (component-vertices global-vertices local-vertices)
      (prepare-vertices vertices)
    (insert-conflict-edges component
                           component-vertices global-vertices local-vertices)
    ;; Normalize adjacency list ordering, and collect all uncolored
    ;; vertices in the graph.
    (collect ((colored)
              (uncolored))
      (dolist (v vertices)
        (setf (vertex-neighbor-color-counts v)
              (make-array (sb-size (sc-sb (vertex-sc v)))
                          :element-type '(unsigned-byte 32)
                          :initial-element 0))
        (cond ((vertex-color v)
               (aver (tn-offset (vertex-tn v)))
               (colored v))
              (t
               (aver (not (tn-offset (vertex-tn v))))
               (uncolored v))))
      ;; Later passes like having this list sorted; do it in advance.
      (%make-interference-graph
       :vertices (stable-sort (uncolored) #'< :key #'vertex-spill-cost)
       :precolored-vertices (colored)))))

;;; Coloring information is removed from all remaining vertices.
(defun reset-interference-graph-without-vertex (graph vertex)
  (declare (type vertex vertex) (type interference-graph graph))
  (let ((vertices (loop for v in (ig-vertices graph)
                        unless (eql v vertex)
                        do (aver (not (tn-offset (vertex-tn v))))
                           (setf (vertex-color v) nil
                                 (vertex-neighbor-colors v) 0)
                           (fill (vertex-neighbor-color-counts v) 0)
                        and collect v)))
    (setf (ig-vertices graph) vertices)
    (do-sset-elements (neighbor (vertex-full-incidence vertex) graph)
      (sset-delete vertex (vertex-full-incidence neighbor)))))

;;; Support code

;; Return nil if COLOR conflicts with any of NEIGHBOR-COLORS.
;; Take into account element sizes of the respective SCs.
(declaim (inline color-no-conflicts-p))
(defun color-no-conflicts-p (color vertex)
  (declare (type sb-vm:finite-sc-offset color)
           (type vertex vertex)
           (optimize speed (safety 0)))
  (not (logtest (ash (vertex-size-mask vertex) color)
                (vertex-neighbor-colors vertex))))

;; Assumes that VERTEX pack-type is :WIRED.
(defun vertex-color-possible-p (vertex color)
  (declare (type fixnum color)
           (type vertex vertex))
  (and (or (and (neq (vertex-pack-type vertex) :wired)
                (not (tn-offset (vertex-tn vertex))))
           (= color (the fixnum (vertex-color vertex))))
       (sc-locations-member color (vertex-initial-domain vertex))
       (color-no-conflicts-p color vertex)))

(declaim (ftype (sfunction (vertex) sc-locations) vertex-domain)
         (inline vertex-domain))
(defun vertex-domain (vertex)
  (let ((result 0)
        (mask (vertex-size-mask vertex))
        (neighbor-colors (vertex-neighbor-colors vertex)))
    (declare (type sc-locations result))
    (do-sc-locations (color (vertex-initial-domain vertex) result
                      (vertex-element-size vertex))
      (unless (logtest (ash mask color) neighbor-colors)
        (setf (ldb (byte 1 color) result) 1)))))

;; Return a list of vertices that we might want VERTEX to share its
;; location with.
(defun vertex-target-vertices (vertex)
  (declare (type vertex vertex))
  (let ((sb (sc-sb (vertex-sc vertex)))
        (neighbors (vertex-full-incidence vertex))
        (vertices '()))
    (do-target-tns (current (vertex-tn vertex) :limit 20)
      (let ((target (tn-vertex current)))
        (when target
          (let ((offset (vertex-color target)))
            (when (and offset
                       (eq sb (sc-sb (tn-sc current)))
                       (not (sset-member target neighbors)))
              (pushnew target vertices :test #'eq))))))
    vertices))

;;; Choose the "best" color for these vertices: a color is good if as
;;; many of these vertices simultaneously take that color, and those
;;; that can't have a low spill cost.
(declaim (ftype (sfunction (list sc-locations)
                           (values sb-vm:finite-sc-offset list))
                vertices-best-color/single-color))
(defun vertices-best-color/single-color (vertices color)
  (let ((compatible '()))
    (dolist (vertex vertices)
      (when (and (notany (lambda (existing)
                           (sset-member vertex
                                        (vertex-full-incidence existing)))
                         compatible)
                 (vertex-color-possible-p vertex color))
        (push vertex compatible)))
    (values color compatible)))

(declaim (ftype (sfunction (vertex sc-locations)
                           (values sb-vm:finite-sc-offset list))
                vertices-best-color/single-vertex))
(defun vertices-best-color/single-vertex (vertex colors)
  (do-sc-locations (color colors nil (vertex-element-size vertex))
    (when (vertex-color-possible-p vertex color)
      (return-from vertices-best-color/single-vertex
        (values color (list vertex)))))
  (values (sc-locations-first colors) '()))

(declaim (ftype (sfunction (cons sc-locations)
                           (values sb-vm:finite-sc-offset list))
                vertices-best-color/general))
(defun vertices-best-color/general (vertices colors)
  (let* ((best-color      (sc-locations-first colors))
         (best-compatible '())
         (best-cost       0))
    ;; TODO: sort vertices by spill cost, so that high-spill cost ones
    ;; are more likely to be compatible?  We're trying to find a
    ;; maximal 1-colorable subgraph here, ie. a maximum independent
    ;; set :\ Still, a heuristic like first attempting to pack in
    ;; max-cost vertices may be useful
    (do-sc-locations (color colors nil (vertex-element-size (first vertices)))
      (let ((compatible '())
            (cost 0))
        (declare (fixnum cost))
        (dolist (vertex vertices)
          (when (and (vertex-color-possible-p vertex color)
                     (notany (lambda (existing)
                               (sset-member vertex
                                            (vertex-full-incidence existing)))
                             compatible))
            (incf cost (vertex-spill-cost vertex))
            (push vertex compatible)))
        (when (> cost best-cost)
          (setf best-color      color
                best-compatible compatible
                best-cost       cost))))
    (values best-color best-compatible)))

(declaim (inline vertices-best-color))
(defun vertices-best-color (vertices colors)
  (declare (type sc-locations colors))
  (cond
    ((null vertices)
     (values (sc-locations-first colors) '()))
    ((null (rest vertices))
     (vertices-best-color/single-vertex (first vertices) colors))
    ((= 1 (sc-locations-count colors))
     (vertices-best-color/single-color vertices (sc-locations-first colors)))
    (t
     (vertices-best-color/general vertices colors))))

;;; Coloring inner loop

;; Greedily choose the color for this vertex, also moving around any
;; :target vertex to the same color if possible.
(defun find-vertex-color (vertex)
  (let ((domain (vertex-domain vertex)))
    (unless (zerop domain)
      (let* ((targets (vertex-target-vertices vertex))
             (sc (vertex-sc vertex))
             (sb (sc-sb sc)))
        (multiple-value-bind (color recolor-vertices)
            (vertices-best-color targets domain)
          (dolist (target recolor-vertices)
            (aver (vertex-color target))
            (unless (eql color (vertex-color target))
              (aver (eq sb (sc-sb (vertex-sc target))))
              (aver (not (tn-offset (vertex-tn target))))
              #+nil ; this check is slow
              (aver (vertex-color-possible-p target color))
              (recolor-vertex target color)
              (setf (vertex-color target) color)))
          color)))))

(defun recolor-vertex (vertex new-color)
  (declare (type sb-vm:finite-sc-offset new-color)
           (optimize (safety 0)))
  (let* ((size (vertex-element-size vertex))
         (color (vertex-color vertex))
         (new-mask (ash (vertex-size-mask vertex) new-color)))
    ;; Multiple neighbors may share the color, can only zero out the
    ;; mask when the count falls to zero.
    (do-sset-elements (neighbor (vertex-full-incidence vertex) vertex)
      (let ((map (logior (vertex-neighbor-colors neighbor) new-mask))
            (neighbor-color-counts (vertex-neighbor-color-counts neighbor)))
        (declare (type sb-vm:finite-sc-offset-map map))
        (loop for old from color below (+ color size)
              for new from new-color
              do (when (zerop (decf (aref neighbor-color-counts old)))
                   (setf map (logxor map
                                     (truly-the sb-vm:finite-sc-offset-map (ash 1 old)))))
                 (incf (aref neighbor-color-counts new)))
        (setf (vertex-neighbor-colors neighbor) map)))))

;; Partition vertices into those that are likely to be colored and
;; those that are likely to be spilled.  Assumes that the interference
;; graph's vertices are sorted with the least spill cost first, so
;; that the stacks end up with the greatest spill cost vertices first.
(defun partition-and-order-vertices (interference-graph)
  (let* ((precoloring-stack '())
         (prespilling-stack '())
         (vertices (ig-vertices interference-graph)))
    ;; walk the vertices from least important to most important TN wrt
    ;; spill cost.  That way the TNs we really don't want to spill are
    ;; at the head of the colouring lists.
    (loop for vertex in vertices do
         (aver (not (vertex-color vertex))) ; we already took those out above
       ;; FIXME: some interference will be with vertices that don't
       ;;  take the same number of slots. Find a smarter heuristic.
         (cond ((< (sset-count (vertex-full-incidence vertex))
                   (vertex-initial-domain-size vertex))
                (push vertex precoloring-stack))
               (t
                (push vertex prespilling-stack))))
    (values precoloring-stack prespilling-stack)))

(defun color-vertex (vertex color)
  (declare (type vertex vertex)
           (type sb-vm:finite-sc-offset color)
           (optimize speed (safety 0)))
  (setf (vertex-color vertex) color)
  (let* ((size (vertex-element-size vertex))
         (mask (ash (vertex-size-mask vertex) color)))
    (do-sset-elements (neighbor (vertex-full-incidence vertex) vertex)
      (setf (vertex-neighbor-colors neighbor)
            (logior (vertex-neighbor-colors neighbor) mask))
      ;; This is needed to recolor the vertex in RECOLOR-VERTEX
      (loop for i from color below (+ color size)
            do (incf (aref (vertex-neighbor-color-counts neighbor) i))))))

;; Try and color the interference graph once.
(defun color-interference-graph (interference-graph)
  (flet ((color-vertices (vertices)
           (dolist (vertex vertices)
             (awhen (find-vertex-color vertex)
               (color-vertex vertex it)))))
    (multiple-value-bind (probably-colored probably-spilled)
        (partition-and-order-vertices interference-graph)
      (color-vertices probably-colored)
      ;; These might benefit from further ordering... LexBFS?
      (color-vertices probably-spilled)))
  interference-graph)

;;; Iterative spilling logic.

;; maximum number of spill iterations
(defvar *pack-iterations* 500)
(declaim (fixnum *pack-iterations*)
         (always-bound *pack-iterations*))

;; Find the least-spill-cost neighbor in each color.
;; FIXME: this is too slow and isn't the right interface anymore.
;; The code might be fast enough if there were a simple way to detect
;; whether a given vertex is a min-candidate for another uncolored
;; vertex.
;; I'm leaving this around as an idea of what a smart spill choice
;; might be like. -- PK
#+nil
(defun collect-min-spill-candidates (vertex)
  (let ((colors '()))
    (do-oset-elements (neighbor (vertex-full-incidence vertex))
      (when (eql :normal (vertex-pack-type neighbor))
        (let* ((color (vertex-color neighbor))
               (cell (assoc color colors))
               (cost-neighbor (tn-spill-cost (vertex-tn neighbor))))
          (cond (cell
                 (when (< cost-neighbor (tn-spill-cost
                                         (vertex-tn (cdr cell))))
                   (setf (cdr cell) neighbor)))
                (t (push (cons color neighbor) colors))))))
    (remove nil (mapcar #'cdr colors))))

;; Try to color the graph. If some TNs are left uncolored, find a
;; spill candidate, force it on the stack, and try again.
(defun iterate-color (vertices component)
  (let ((graph (make-interference-graph vertices component))
        (spilled 0))
    (declare (type index spilled))
    (labels ((spill-candidates-p (vertex)
               (unless (vertex-color vertex)
                 (aver (eq (vertex-pack-type vertex) :normal))
                 t))
             (try-color ()
               (color-interference-graph graph)
               (find-if #'spill-candidates-p (ig-vertices graph)))
             (spill (vertex)
               (setf (vertex-color vertex) nil)
               (incf spilled)
               (setf graph (reset-interference-graph-without-vertex
                            graph vertex))))
      (loop for uncolored = (try-color)
            repeat *pack-iterations*
            while uncolored
            do (spill uncolored)))
    (let ((colored (ig-vertices graph)))
      (aver (= (length vertices)
               (+ spilled (length colored)
                  (length (ig-precolored-vertices graph)))))
      colored)))

;;; Nice interface

;; Just pack vertices that have been assigned a color.
(defun pack-colored (colored-vertices)
  (dolist (vertex colored-vertices)
    (let* ((color (vertex-color vertex))
           (tn (vertex-tn vertex)))
      (cond ((tn-offset tn))
            (color
             (aver (not (conflicts-in-sc tn (tn-sc tn) color)))
             (setf (tn-offset tn) color)
             (pack-wired-tn (vertex-tn vertex)))
            (t
             ;; we better not have a :restricted TN not packed in its
             ;; finite SC
             (aver (neq (vertex-pack-type vertex) :restricted)))))))

;; Pack pre-allocated TNs, collect vertices, and color.
(defun pack-iterative (component 2comp)
  (declare (type component component) (type ir2-component 2comp))
  (collect ((vertices))
    ;; Pack TNs that *must* be in a certain location, but still
    ;; register them in the interference graph: it's useful to have
    ;; them in the graph for targeting purposes.
    (do ((tn (ir2-component-wired-tns 2comp) (tn-next tn)))
        ((null tn))
      (unless (eq (tn-kind tn) :arg-pass)
        (pack-wired-tn tn)
        (unless (unbounded-tn-p tn)
          (vertices (make-vertex tn :wired)))))

    ;; Preallocate vertices that *must* be in this finite SC.  If
    ;; targeting is improved, giving them a high priority in regular
    ;; regalloc may be a better idea.
    (collect ((component)
              (normal))
      (do ((tn (ir2-component-restricted-tns 2comp) (tn-next tn)))
          ((null tn))
        (unless (or (tn-offset tn) (unbounded-tn-p tn))
          (vertices (make-vertex tn :restricted))
          (if (eq :component (tn-kind tn))
              (component tn)
              (normal tn))))
      ;; First, pack TNs that span the whole component to minimise
      ;; fragmentation.  Also, pack high cost TNs first, so they get
      ;; nice targeting.
      (flet ((pack-tns (tns)
               (dolist (tn (stable-sort tns #'> :key #'tn-cost))
                 (pack-tn tn t))))
        (pack-tns (component))
        (pack-tns (normal))))

    ;; Now that all pre-packed TNs are registered as vertices, work on
    ;; the rest.  Walk through all normal TNs, and determine whether
    ;; we should try to put them in registers or stick them straight
    ;; to the stack.
    (do ((tn (ir2-component-normal-tns 2comp) (tn-next tn)))
        ((null tn))
      ;; Only consider TNs that aren't forced on the stack and for
      ;; which the spill cost is non-negative (i.e. not live across so
      ;; many calls that it's simpler to just leave them on the stack)
      (when (and (not (tn-offset tn))
                 (neq (tn-kind tn) :more)
                 (not (unbounded-tn-p tn))
                 (not (and (sc-save-p (tn-sc tn))   ; SC is caller-save, and
                           (minusp (tn-cost tn))))) ; TN lives in many calls
        ;; otherwise, we'll let the final pass handle them.
        (vertices (make-vertex tn :normal))))
    ;; Iteratively find a coloring/spill partition, and allocate those
    ;; for which we have a location
    (pack-colored (iterate-color (vertices) component)))
  nil)
