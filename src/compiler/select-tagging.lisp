;;;; Select between tagged or untagged VOPs
;;;;
;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMUCL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;; Using a flow graph, separate tagged (source) and untagged (sink)
;;; using min-cut via the Edmonds-Karp algorithm
(defstruct (net (:constructor %make-net ()))
  (table (make-hash-table :test 'eq))
  (head (make-array 16 :element-type '(signed-byte 32) :initial-element -1 :adjustable t :fill-pointer 0)
   :type (vector (signed-byte 32)))
  (to (make-array 32 :element-type '(unsigned-byte 32) :adjustable t :fill-pointer 0)
   :type (vector (unsigned-byte 32)))
  (cap (make-array 32 :element-type '(unsigned-byte 32) :adjustable t :fill-pointer 0)
   :type (vector (unsigned-byte 32)))
  (next (make-array 32 :element-type '(signed-byte 32) :adjustable t :fill-pointer 0)
   :type (vector (signed-byte 32))))

(defun nid-p (net x)
  (gethash x (net-table net)))

(defun nid (net x)
  (or (gethash x (net-table net))
      (setf (gethash x (net-table net))
            (vector-push-extend -1 (net-head net)))))

(defun make-net ()
  (let ((n (%make-net)))
    (nid n :tag)
    (nid n :untag)
    n))

;;; Add an edge from U to V with capacity C, and one back with capacity
;;; BACK.
(defun add-edge (net u v c &optional (back 0))
  (let ((u (nid net u))
        (v (nid net v)))
    (flet ((add (from to cap)
             (vector-push-extend to (net-to net))
             (vector-push-extend cap (net-cap net))
             (vector-push-extend (aref (net-head net) from) (net-next net))
             (setf (aref (net-head net) from) (1- (length (net-to net))))))
      (add u v c)
      (add v u back))))

(defun min-cut (net)
  (declare (optimize speed))
  (macrolet ((data-vector (x)
               #+sb-xc-host x
               #-sb-xc-host
               `(truly-the (simple-array * (*)) (%array-data ,x))))
    (let* ((to (data-vector (net-to net)))
           (cap (data-vector (net-cap net)))
           (next (data-vector (net-next net)))
           (head (net-head net))
           (n (length head))
           (head (data-vector head))
           (path (make-array n :element-type '(signed-byte 32)))
           (queue (make-array n :element-type '(unsigned-byte 32))))
      (loop
       (fill path -1)
       (setf (aref path 0) -2)
       (let ((qh 0) (qt 1))
         (setf (aref queue 0) 0)
         (loop while (and (< qh qt) (= (aref path 1) -1))
               do (let ((u (aref queue qh)))
                    (incf qh)
                    (loop for e = (aref head u) then (aref next e)
                          while (>= e 0)
                          do (let ((v (aref to e)))
                               (when (and (= (aref path v) -1)
                                          (> (aref cap e) 0))
                                 (setf (aref path v) e
                                       (aref queue qt) v)
                                 (incf qt)))))))
       (when (= (aref path 1) -1)
         (return (lambda (x)
                   (/= (aref path (nid-p net x)) -1))))
       (let ((bot (loop for v = 1 then (aref to (logxor e 1))
                        for e = (aref path v)
                        until (= v 0)
                        minimize (aref cap e))))
         (loop for v = 1 then (aref to (logxor e 1))
               for e = (aref path v)
               while (/= v 0)
               do (decf (aref cap e) bot)
                  (incf (aref cap (logxor e 1)) bot)))))))

(defun tagged-template-results-p (vop-info)
  (cond ((vop-info-p vop-info)
         (case (ldb (byte 2 0) (vop-info-result-tags vop-info)) ;; TODO: multiple results
           (1
            (values 1 0))
           (2
            (values 0 1))
           (t
            (values 0 0))))
        ((listp vop-info)
         (values 0 0))
        (t
         (values 1 0))))

(defun combination-arg-template-position (arg node)
  (let ((args (combination-args node)))
    (position arg (if (combination-is node '(%%primitive))
                      (truly-the list (cdr args))
                      args)
              :test #'eq)))

(defun tagged-template-arg-p (lvar n vop-info)
  (declare ((unsigned-byte 8) n))
  (let ((tagging (ldb (byte 2 (* n 2)) (vop-info-arg-tags vop-info))))
    (case tagging
      (1
       (values (boxing-cost lvar) 0))
      (2
       (values 0 1))
      (t
       (values 0 0)))))

(defun boxing-cost (lvar)
  (let ((2lvar  (lvar-info lvar)))
    (if (or (not 2lvar)
            (member (ir2-lvar-primitive-type 2lvar)
                    (list (load-time-value (primitive-type-or-lose 'fixnum))
                          (load-time-value (primitive-type-or-lose 'sb-vm::positive-fixnum)))))
        1
        100000)))

(defun select-tagging (component)
  (let ((net (make-net))
        (pending '()))
    (labels ((reach (x)
               (unless (nid-p net x)
                 (nid net x)
                 (push x pending)))
             (copy (x y)
               (reach y)
               (add-edge net x y 8 8))
             (cost (x tag untag)
               (when (plusp tag)
                 (add-edge net :tag x (* tag 8)))
               (when (plusp untag)
                 (add-edge net x :untag (* untag 8))))
             (read-costs (lvar node)
               (cond ((combination-p node)
                      (let ((template (combination-info node)))
                        (cond ((atom template)
                               (if (vop-info-p template)
                                   (tagged-template-arg-p lvar
                                                          (combination-arg-template-position lvar node)
                                                          template)
                                   (values 1 0)))
                              ((/= (vop-info-related-args (car template)) -1)
                               (let ((pos (combination-arg-template-position lvar node)))
                                 (if (logbitp pos (vop-info-related-args (car template)))
                                     (values 0 0)
                                     (tagged-template-arg-p lvar pos (car template)))))
                              (t
                               ;; Flexible VOP, no costs
                               (values 0 0)))))
                     ((jump-table-p node)
                      (tagged-template-arg-p lvar 0 (template-or-lose 'jump-table)))
                     (t
                      (values 1 0))))
             (local-call-p (node lvar)
               (and (basic-combination-p node)
                    (eq (basic-combination-kind node) :local)
                    (neq lvar (basic-combination-fun node))))
             (walk-lvar (lvar)
               (do-uses (use lvar)
                 (typecase use
                   (cast
                    (let ((value (cast-value use)))
                      (unless (constant-lvar-p value)
                        (copy lvar value))))
                   (ref
                    (let ((leaf (ref-leaf use)))
                      (typecase leaf
                        (lambda-var (reach leaf))
                        (constant)
                        (t (cost lvar 1 0)))))
                   (cset
                    (let ((var (set-var use)))
                      (if (lambda-var-p var)
                          (reach var)
                          (cost lvar 1 0))))
                   (combination
                    (multiple-value-call #'cost lvar
                      (tagged-template-results-p (combination-info use))))
                   (t
                    (cost lvar 1 0))))
               ;; Where it goes.
               (let ((dest (lvar-dest lvar)))
                 (cond ((null dest))
                       ((cast-p dest)
                        (let ((next (node-lvar dest)))
                          (when next
                            (reach next))))
                       ((set-p dest)
                        (let ((var (set-var dest)))
                          (if (lambda-var-p var)
                              (reach var)
                              (cost lvar 1 0))))
                       ((local-call-p dest lvar)
                        (let* ((fun (combination-lambda dest))
                               (vars (lambda-vars fun))
                               (var (if (functional-kind-eq fun mv-let)
                                        (first vars)
                                        (nth (position lvar (basic-combination-args dest))
                                             vars))))
                          (when var
                            (reach var))))
                       ((and (combination-p dest)
                             (lvar-fun-is (combination-fun dest) '(values)))
                        (let* ((n (position lvar (combination-args dest)))
                               (next (node-lvar dest))
                               (next-dest (and next (lvar-dest next))))
                          (cond ((and (local-call-p next-dest next)
                                      (functional-kind-eq (combination-lambda next-dest) mv-let))
                                 (let ((var (nth n (lambda-vars (combination-lambda next-dest)))))
                                   (when var
                                     (reach var))))
                                ((and next (zerop n))
                                 (copy lvar next)))))
                       ((and (basic-combination-p dest)
                             (not (combination-p dest)))
                        (cost lvar 1 0))
                       (t
                        (multiple-value-call #'cost lvar (read-costs lvar dest))))))
             (walk-var (var)
               (let ((home (lambda-var-home var)))
                 (when (functional-kind-eq home external)
                   (cost var 1 0))
                 (dolist (ref (leaf-refs var))
                   (let ((lvar (node-lvar ref)))
                     (when (and lvar (not (constant-lvar-p lvar)))
                       (copy var lvar))))
                 (dolist (set (lambda-var-sets var))
                   (let ((value (set-value set)))
                     (unless (constant-lvar-p value)
                       (copy var value))))
                 ;; The values it is bound to.
                 (cond ((functional-kind-eq home mv-let)
                        (let ((args (basic-combination-args (let-combination home)))
                              (n (position var (lambda-vars home))))
                          (if (and (singleton-p args)
                                   (block values-p
                                     (do-uses (use (car args) t)
                                       (unless (and (combination-p use)
                                                    (lvar-fun-is (combination-fun use) '(values)))
                                         (return-from values-p nil)))))
                              (do-uses (use (car args))
                                (let ((arg (nth n (combination-args use))))
                                  (when (and arg (not (constant-lvar-p arg)))
                                    (copy var arg))))
                              (cost var 1 0))))
                       ((functional-kind-eq home external))
                       (t
                        (let ((n (position var (lambda-vars home))))
                          (dolist (ref (leaf-refs home))
                            (let* ((lvar (node-lvar ref))
                                   (call (and lvar (lvar-dest lvar))))
                              (when (and (combination-p call)
                                         (eq (combination-kind call) :local)
                                         (eq (combination-fun call) lvar))
                                (let ((arg (nth n (combination-args call))))
                                  (when (and arg (not (constant-lvar-p arg)))
                                    (copy var arg))))))))))))
      (do-blocks (block component)
        (do-nodes (node nil block)
          (when (and (combination-p node)
                     (consp (combination-info node)))
            (let ((tagged-template (car (combination-info node)))
                  (result (node-lvar node))
                  (any nil))
              (loop for related fixnum = (vop-info-related-args tagged-template)
                      then (ash related -1)
                    for arg in (combination-args node)
                    for i fixnum from 0
                    do (unless (or (constant-lvar-p arg)
                                   (not (logbitp 0 related)))
                         (setq any t)
                         (reach arg)
                         (add-edge net arg node 8
                                   (if (> (tagged-template-arg-p arg i tagged-template) 0)
                                       (* 8 (boxing-cost arg))
                                       0))))
              (unless (template-conditional-p tagged-template)
                (cond ((zerop (tagged-template-results-p tagged-template))
                       ;; The result is untagged for both vops, e.g. (ash fixnum positive-constant)
                       (when result
                         (reach result)
                         (add-edge net result :untag 8)))
                      (result
                       (setq any t)
                       (reach result)
                       (add-edge net result node 8 8))))
              ;; Somehow all lvars are constant
              (unless any
                (setf (combination-info node)
                      (car (combination-info node))))))))
      (loop while pending
            do (let ((x (pop pending)))
                 (if (lvar-p x)
                     (walk-lvar x)
                     (walk-var x)))))
    (when (> (length (net-to net)) 0)
      (let ((tagged-p (min-cut net)))
        (loop for node being the hash-key of (net-table net)
              do
              (when (combination-p node)
                (let ((info (combination-info node)))
                  (aver (listp info))
                  (let ((new (setf (combination-info node)
                                   (if (funcall tagged-p node)
                                       (first info)
                                       (second info)))))
                    (when (eq (vop-info-tag new) :commutative)
                      (destructuring-bind (a b) (combination-args node)
                        (when (and (nid-p net b)
                                   (funcall tagged-p b)
                                   (not (funcall tagged-p a)))
                          (setf (combination-args node) (list b a)))))))))))))
