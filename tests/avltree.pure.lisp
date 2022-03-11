(load "bbtree-test-util.lisp")
(use-package "SB-INT")

#-sb-thread (invoke-restart 'run-tests::skip-file) ;; some of the symbols below disappear

(import 'sb-thread::(avlnode-key avlnode-data avlnode-left avlnode-right
                     avl-find<= avl-find>=
                     avl-insert avl-delete avl-find
                     avl-balance-factor avl-count))

(defun tree-to-dot (tree output)
  (with-open-file (stream output :direction :output :if-exists :supersede)
    (format stream "digraph G {
node [shape=record];~%")
    (named-let recurse ((node tree))
      (when node
        (let ((key (avlnode-key node)))
          (format stream "node~d [label=\"~d\"];~%" key key)
          (awhen (avlnode-left node)
            (format stream "node~d:sw -> node~d;~%" key (avlnode-key it))
            (recurse it))
          (awhen (avlnode-right node)
            (format stream "node~d:se -> node~d;~%" key (avlnode-key it))
            (recurse it)))))
    (format stream "}~%"))
  tree)

(defun avl-verify-invariants (tree)
  ;; ensure that it's a search tree
  (named-let recurse ((node tree) (min most-negative-fixnum) (max most-positive-fixnum))
    (when node
      (let ((key (avlnode-key node)))
        (assert (<= min key max))
        (recurse (avlnode-left node) min (1- key))
        (recurse (avlnode-right node) (1+ key) max))))
  ;; and as balanced as required
  (named-let recurse ((node tree))
    (when node
      (unless (<= -1 (avl-balance-factor node) +1)
        (error "balance invariant failed on node ~d. b=~d~%"
               (avlnode-key node) (avl-balance-factor node)))
      (recurse (avlnode-left node))
      (recurse (avlnode-right node)))))

(defun random-operations (n-trials n-ops print
                          &aux (keys (make-array 5000 :fill-pointer 0))
                               events)
  (dotimes (iter n-trials)
    (when print
      (format t "Trial ~d " iter)
      (force-output))
    (let (tree (prev-op :delete))
      (setf (fill-pointer keys) 0)
      (loop
       repeat n-ops for event from 0 do
       (assert (= (avl-count tree) (length keys)))
       (let ((op
              (if (< (random 10) 5)
                  prev-op
                  (if (or (null tree)
                          (< (random 10)
                             (let ((ct (length keys)))
                               (cond ((< ct  100) 8)
                                     ((< ct  500) 7)
                                     ((< ct 1000) 6)
                                     (t 1)))))
                      :insert
                      :delete))))
         (when (and (eq op :delete) (zerop (length keys)))
           (setq op :insert))
         (setq prev-op op)
         (when print
           (write-char (if (eq op :insert) #\+ #\-)))
         (ecase op
          (:insert
           (flet ((new-key () (random most-positive-fixnum)))
             (let ((key (new-key)))
               (loop while (avl-find key tree)
                     do (setq key (new-key)))
               (push `(:insert . ,key) events)
               (setq tree (avl-insert tree key (- key)))
               (vector-push-extend key keys))))
          (:delete
           (let* ((i (random (length keys)))
                  (key (aref keys i)))
             (push `(:delete . ,key) events)
             (setq tree (avl-delete key tree))
             (assert (not (avl-find key tree)))
             (setf (aref keys i) (aref keys (1- (length keys))))
             (decf (fill-pointer keys)))))
         (avl-verify-invariants tree)
         (dotimes (i (length keys))
           (let* ((key (aref keys i))
                  (node (avl-find key tree)))
             (unless (and node (= (avlnode-data node) (- key)))
               (error "failed to find ~s~%" key)))))))
    (when print
      (format t " ~d keys~%" (length keys)))))

(test-util:with-test (:name :avltree-random-tester)
  (random-operations 10 10 nil)
  (random-operations 10 200 nil))

(defun test-avlfind-inexact (n-nodes n-iterations)
  (bbtree-test:test-find-inexact-macro avl-insert avl-find<= avl-find>= avlnode-key))

(test-util:with-test (:name :avl-find-inexact)
  (bbtree-test:exercise-find-inexact 'test-avlfind-inexact))
