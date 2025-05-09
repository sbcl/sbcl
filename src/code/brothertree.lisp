;;;; Pure functional 1-2 brother tree implementation

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-BROTHERTREE")

;;;; Translated from the Haskell code in
;;;; https://www.cs.ox.ac.uk/ralf.hinze/publications/Brother12.pdf

;(declaim (optimize (sb-c::store-coverage-data 3)))

(sb-xc:deftype keytype () 'sb-vm:word)

(sb-xc:defstruct (binary-node (:copier nil)
                        (:constructor make-binary-node (key %left %right)))
  ;; key must be in slot index 0 because fringe binary nodes have only this slot
  (key 0 :type keytype)
  %left %right)
(sb-xc:defstruct (unary-node (:copier nil)
                       (:conc-name "")
                       (:constructor N1 (child)))
  ;; unary should not have unary child but it seems like the deletion algorithm
  ;; temporarily does that and then eliminates it.
  (child nil :type t))
;;; The remainder of this file is not for the host. This unusual technique suggests
;;; that we ought to have a way to READ and EVAL some TLFs without declaring that the
;;; entire file is to be part of both make-host passes.
#-sb-xc-host
(progn
;;; L2 is a temporary node that gets eliminated by the insertion algorithm.
;;; My guess is that the "2" is supposed to be reminiscent of "binary",
;;; but it's a leaf, a/k/a "external node".
(declaim  (inline L2))
(defun L2 (x) `(leaf . ,x))
;;; Ternary nodes are temporary and eliminated by the insertion algorithm
(defstruct (ternary-node (:copier nil)
                         (:constructor N3 (left key1 middle key2 right)))
  left key1 middle key2 right)

(declaim (freeze-type unary-node binary-node ternary-node))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((dd (sb-kernel:find-defstruct-description 'binary-node)))
    (setf (sb-kernel:dd-flags dd) (logior (sb-kernel:dd-flags dd) sb-kernel:+dd-varylen+))))

;;; Roughly half the binary nodes in a tree are at the fringe, so the left and right
;;; children are both NIL. We have to use binary nodes because only binary nodes
;;; have a key, but a degenerate representation saves a nice amount of space
;;; by implicitly representing the two NILs.
(eval-when (:compile-toplevel :load-toplevel :execute)
(sb-c:defknown fringe-binary-node-p (sb-kernel:instance) boolean ()
               :overwrite-fndb-silently t)
#+x86-64
(sb-c:define-vop ()
  (:translate fringe-binary-node-p)
  (:args (x :scs (sb-vm::descriptor-reg)))
  (:conditional :l)
  (:policy :fast-safe)
  (:generator 1
    (sb-assem:inst cmp :byte (sb-x86-64-asm::ea (- 1 sb-vm:instance-pointer-lowtag) x)
                   (ash (+ 3 sb-vm:instance-data-start)
                        (- sb-vm:instance-length-shift sb-vm:n-widetag-bits))))))
#-x86-64
(progn
(declaim (inline fringe-binary-node-p))
(defun fringe-binary-node-p (x) ; has only 1 data slot
  (= (sb-kernel:%instance-length x) (1+ sb-vm:instance-data-start))))

;;; This is the non-"smart" constructor; it only decides whether or not
;;; to store the left and right children.
;;; |n2| is the so-called "smart" constructor which imposes constraints.
(defun n2 (left key right)
  (declare (inline make-binary-node))
  #+nil (when (and (unary-node-p left) (unary-node-p right)) ; for debugging only
          (error "won't make binary node from ~S ~S" left right))
  (if (or left right)
      (make-binary-node key left right)
      (let ((instance (sb-kernel:%make-instance/mixed
                       (1+ sb-vm:instance-data-start))))
        (sb-kernel:%set-instance-layout instance #.(sb-kernel:find-layout 'binary-node))
        ;; Nodes are immutable, hence no setf'er exists.
        (sb-kernel:%raw-instance-set/word instance sb-vm:instance-data-start key)
        (truly-the binary-node instance))))

(defmacro binary-node-parts (node)
  `(let ((n ,node))
     (if (fringe-binary-node-p n)
         (values nil (binary-node-key n) nil) ; has only one data slot
         ;; has left + right
         (values (binary-node-%left n) (binary-node-key n) (binary-node-%right n)))))

(defmacro ternary-node-parts (n)
  `(values (ternary-node-left ,n) (ternary-node-key1 ,n) (ternary-node-middle ,n)
           (ternary-node-key2 ,n) (ternary-node-right ,n)))

(defmethod print-object ((self binary-node) stream)
  (print-unreadable-object (self stream :identity t :type t)
    (multiple-value-bind (left key right) (binary-node-parts self)
      (format stream "K=~D L=~D R=~D"
              key
              (if left (if (unary-node-p left) "unary" (binary-node-key left)))
              (if right (if (unary-node-p right) "unary" (binary-node-key right)))))))
(defmethod print-object ((self unary-node) stream)
  (print-unreadable-object (self stream :identity t :type t)
    (let ((child (child self)))
      (format stream "child=~D"
              (if child (binary-node-key child))))))

(defun insert (a tree &aux (leaf (L2 a)))
  (declare (keytype a))
  (declare (dynamic-extent leaf))
  ;; The Haskell code uses different definitions of 'root' and 'n2'
  ;; for insert and delete. That's certainly confusing.
  ;; Anyway these have to be internal functions to avoid duplication.
  ;; Were it not for that they would be DEFUNs for traceability.
  (labels
    ((|n1| (x)
       (typecase x
         ((cons (eql leaf)) (N2 nil (cdr x) nil))
         (ternary-node
          (multiple-value-bind (t1 a1 t2 a2 t3) (ternary-node-parts x)
            (N2 (N2 t1 a1 t2) a2 (N1 t3))))
         (t (N1 x))))
     (|n2| (left key right)
       (flet ((leafp (x) (typep x '(cons (eql leaf))))
              (nodep (x) (typep x '(or unary-node binary-node))))
         (declare (inline leafp nodep))
         (cond
           ((leafp left)
            (binding* ((a1 (cdr left)) (a2 key) (t1 right))
              (N3 nil a1 nil a2 (the null t1))))
           ((and (typep left 'ternary-node) (nodep right))
            (binding* (((t1 a1 t2 a2 t3) (ternary-node-parts left))
                       (a3 key))
              (if (typep right 'unary-node)
                  (N2 (N2 t1 a1 t2) a2 (N2 t3 a3 (child right)))
                  (N3 (N2 t1 a1 t2) a2 (N1 t3) a3 right))))
           ((leafp right)
            (binding* ((t1 left) (a1 key) (a2 (cdr right)))
              (N3 (the null t1) a1 nil a2 nil)))
           ((and (typep right 'ternary-node) (nodep left))
            (binding* ((a1 key) ((t2 a2 t3 a3 t4) (ternary-node-parts right)))
              (if (typep left 'unary-node)
                  (N2 (N2 (child left) a1 t2) a2 (N2 t3 a3 t4))
                  (N3 left a1 (N1 t2) a2 (N2 t3 a3 t4)))))
           (t
            (N2 left key right)))))
     (ins (x) ; find insertion point
       (etypecase x
         (binary-node
          (binding* (((l b r) (binary-node-parts x))
                     ((new-left new-key new-right)
                      (if (<= a b)
                          (values (ins l) b r)
                          (values l b (ins r)))))
              (|n2| new-left new-key new-right)))
         (unary-node (|n1| (ins (child x))))
         (null leaf)))
     (root (x) ; bubble up. not sure why 'root' is a good name for this
       (typecase x
         ((cons (eql leaf)) (N2 nil (cdr x) nil))
         (ternary-node
          (multiple-value-bind (t1 a1 t2 a2 t3) (ternary-node-parts x)
            (N2 (N2 t1 a1 t2) a2 (N1 t3))))
         (t x))))
    (root (ins tree))))

;;; I wanted to determine if there is a best order to test the deletion patterns.
;;; But all except the last are roughly equally common, and the last is the most
;;; frequent, so unfortunately we have to try all the others first to make
;;; sure we don't wrongly pick the last.
;;; Hence it doesn't really matter much in what order the others are tried.
(define-load-time-global *cases* (make-array 8))
(defmacro pattern-case ((L R) &rest clauses &aux (n -1))
  (flet ((shape-is (node shape)
           ;; SHAPE has a small number of hardcoded possibilities which are
           ;; essentially just mnemonic devices for the author of this macro.
           (cond ((string= shape "1")      ; Match (N1 _)
                  `(unary-node-p ,node))
                 ((string= shape "1(1)")   ; Match (N1 (N1 _))
                  `(and (unary-node-p ,node) (unary-node-p (child ,node))))
                 ((string= shape "2(1 2)") ; Match (N2 (N1 _) (N2 _ _ _))
                  `(and (binary-node-p ,node)
                        (not (fringe-binary-node-p ,node))
                        (unary-node-p (binary-node-%left ,node))
                        (binary-node-p (binary-node-%right ,node))))
                 ((string= shape "2(2 1)") ; Match (N2 (N2 _ _ _) (N1 _))
                  `(and (binary-node-p ,node)
                        (not (fringe-binary-node-p ,node))
                        (binary-node-p (binary-node-%left ,node))
                        (unary-node-p (binary-node-%right ,node))))
                 ((string= shape "2(2 2)") ; Match (N2 (N2 _ _ _) (N2 _ _ _))
                  `(and (binary-node-p ,node)
                        (not (fringe-binary-node-p ,node))
                        (binary-node-p (binary-node-%left ,node))
                        (binary-node-p (binary-node-%right ,node)))))))
    `(cond ,@(mapcar (lambda (clause)
                       (incf n)
                       (if (eq (car clause) 't)
                           `(t #| (incf (aref *cases* ,n)) |# ,@(rest clause))
                           (let ((test (car clause))
                                 (consequent (cadr clause)))
                             `((and ,(shape-is L (first test))
                                    ,(shape-is R (second test)))
                               ;; (incf (aref *cases* ,n))
                               ,consequent))))
                   clauses))))

(defun delete (a tree)
  (declare (keytype a))
  (labels ((|n2| (left key right)
             ;; this surely isn't as readable as pattern-matching in Haskell,
             ;; but it'll do.
             (pattern-case (left right)
               ;; case 1
               (("1" "1") (N1 (N2 (child left) key (child right))))
               ;; case 2
               (("1(1)" "2(1 2)")
                (binding* ((t1 (child (child left)))
                           (a1 key)
                           ((L a2 t3) (binary-node-parts right))
                           (t2 (child L)))
                  (N1 (N2 (N2 t1 a1 t2) a2 t3))))
               ;; case 3
               (("1(1)" "2(2 1)")
                (binding* ((t1 (child (child left)))
                           (a1 key)
                           ((L a3 R) (binary-node-parts right))
                           ((t2 a2 t3) (binary-node-parts L))
                           (t4 (child R)))
                  (N1 (N2 (N2 t1 a1 t2) a2 (N2 t3 a3 t4)))))
               ;; case 4
               (("1(1)" "2(2 2)")
                (binding* ((t1 (child left))
                           (a1 key)
                           ((t2 a2 t3) (binary-node-parts right)))
                  (N2 (N2 t1 a1 t2) a2 (N1 t3))))
               ;; case 5
               (("2(1 2)" "1(1)")
                (binding* (((L a1 R) (binary-node-parts left))
                           (t1 (child L))
                           ((t2 a2 t3) (binary-node-parts R))
                           (a3 key)
                           (t4 (child (child right))))
                 (N1 (N2 (N2 t1 a1 t2) a2 (N2 t3 a3 t4)))))
               ;; case 6
               (("2(2 1)" "1(1)")
                (binding* (((t1 a1 R) (binary-node-parts left))
                           (t2 (child R))
                           (a2 key)
                           (t3 (child (child right))))
                  (N1 (N2 t1 a1 (N2 t2 a2 t3)))))
               ;; case 7
               (("2(2 2)" "1(1)")
                (binding* (((t1 a1 t2) (binary-node-parts left))
                           (a2 key)
                           (t3 (child right)))
                  (N2 (N1 t1) a1 (N2 t2 a2 t3))))
               ;; case 8
               (t
                (N2 left key right))))
           (del (x)
             (etypecase x
               (binary-node
                (binding* (((l b r) (binary-node-parts x)))
                  (cond ((< a b) (|n2| (del l) b r))
                        ((> a b) (|n2| l b (del r)))
                        (t
                         (binding* (((|a'| |r'|) (split-min r)))
                           (if (not |a'|) (N1 l) (|n2| l |a'| |r'|)))))))
               (unary-node (N1 (del (child x))))
               (null nil)))
           (split-min (x)
             ;; (format t "~&enter split-min with ~s~%" x)
             (etypecase x
               (null
                (values nil nil))
               (unary-node
                (binding* (((a |t'|) (split-min (child x))))
                  (if (not a) (values nil nil) (values a (N1 |t'|)))))
               (binary-node
                (binding* (((t1 a1 t2) (binary-node-parts x))
                           ((a |t1'|) (split-min t1)))
                  ;; (format t "~&binary node split-min got ~s ~s~%" a
                  (if (not a)
                      (values a1 (N1 t2))
                      (values a (|n2| |t1'| a1 t2)))))))
           (root (x)
             (typecase x
               (unary-node (child x))
               (t x))))
  (root (del tree))))

(defun print-tree (tree)
  (named-let recurse ((depth 0) (node tree))
    (etypecase node
      (unary-node
       (format t "~2d: ~v@t unary ~x~%"
               depth (* depth 2) (sb-kernel:get-lisp-obj-address node))
       (recurse (1+ depth) (child node)))
      (binary-node
       (multiple-value-bind (left key right) (binary-node-parts node)
         (format t "~2d: ~v@t Key=~D ~x~%"
                 depth (* depth 2) key (sb-kernel:get-lisp-obj-address node))
         (recurse (1+ depth) left)
         (recurse (1+ depth) right)))
      (null))))

(defun find= (key tree)
  (declare (keytype key))
  (loop
   (typecase tree
     (binary-node
      (multiple-value-bind (left node-key right) (binary-node-parts tree)
        (cond ((< key node-key) (setq tree left))
              ((< node-key key) (setq tree right))
              (t (return tree)))))
     (unary-node (setq tree (child tree)))
     (null (return nil)))))

(defun find<= (key tree)
  (declare (keytype key))
  (named-let recurse ((node tree) (best nil))
    (typecase node
      (binary-node
       (multiple-value-bind (left node-key right) (binary-node-parts node)
        (cond ((< key node-key) (recurse left best))
              ((< node-key key) (recurse right node))
              (t node))))
      (unary-node (recurse (child node) best))
      (t best))))

(defun find>= (key tree)
  (declare (keytype key))
  (named-let recurse ((node tree) (best nil))
    (typecase node
      (binary-node
       (multiple-value-bind (left node-key right) (binary-node-parts node)
        (cond ((< key node-key) (recurse left node))
              ((< node-key key) (recurse right best))
              (t node))))
      (unary-node (recurse (child node) best))
      (t best))))
) ; end PROGN
