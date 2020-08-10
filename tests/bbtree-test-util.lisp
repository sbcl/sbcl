
(defpackage "BBTREE-TEST"
  (:use :cl)
  (:export #:test-find-inexact-macro
           #:exercise-find-inexact))

;;;; Tests of two different kinds of balanced binary trees

(defmacro bbtree-test:test-find-inexact-macro (insertion-fun find<=-fun find>=-fun node-key)
  `(let (integers)
     ;; Generate N random integers
     (dotimes (i n-nodes)
       (loop
         (let ((val (+ 5 (random 1000))))
           (unless (member val integers)
             (push val integers)
             (return)))))
     (setq integers (coerce integers 'vector))
     (dotimes (i n-iterations)
       ;; Try many different shuffles of the insertion order because each
       ;; potentially yields a different tree.
       (test-util:shuffle integers)
       ;; Convert a tree
       (let ((tree nil))
         (dotimes (i (length integers))
           (setq tree (,insertion-fun tree (svref integers i) (svref integers i))))
         (setq integers (sort integers #'<))
         (dotimes (i (length integers))
           (let ((this (svref integers i))
                 (pred (if (> i 0) (svref integers (1- i))))
                 (succ (if (< i (1- (length integers))) (svref integers (1+ i)))))
             ;; THIS should be found exactly
             (let ((answer (,find<=-fun this tree)))
               (assert (eql (,node-key answer) this)))
             (let ((answer (,find>=-fun this tree)))
               (assert (eql (,node-key answer) this)))
             ;; find this node by a smaller key using FIND>=
             (unless (eql pred (1- this))
               (assert (eql (,node-key (,find>=-fun (1- this) tree)) this)))
             ;; find this node by a larger key using FIND<=
             (unless (eql succ (1+ this))
               (assert (eql (,node-key (,find<=-fun (1+ this) tree)) this)))
             ;; check the boundary case of FIND<= and/or find the predecessor
             (let ((answer (,find<=-fun (1- this) tree)))
               (if pred
                   (assert (eql (,node-key answer) pred))
                   (assert (not answer))))
             ;; check the boundary case of FIND>= and/or find the successor
             (let ((answer (,find>=-fun (1+ this) tree)))
               (if succ
                   (assert (eql (,node-key answer) succ))
                   (assert (not answer))))))))))

(defun bbtree-test:exercise-find-inexact (fun)
  (loop for n-nodes from 1 to 20
        do (funcall fun  n-nodes
                    (case n-nodes
                      (1 1)
                      (2 4)
                      (3 10)
                      (t 100)))))

