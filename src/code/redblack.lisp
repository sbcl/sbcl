;;;; A pure functional red/black tree implementation

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;; For testing
;;; (declaim (optimize sb-c:store-coverage-data))

;;; This file is translated from the Haskell code available at
;;; https://www.seas.upenn.edu/~cis552/13fa/lectures/RedBlack.html
;;;
;;; The insertion algorithm is that of Chris Okasaki
;;; and the deletion algorithm is that of Stefan Kahrs.
;;; An allegedly simpler deletion algorithm is presented in
;;; http://matt.might.net/papers/germane2014deletion.pdf
;;; which I found to be less simple when translated from Haskell.

(defpackage "SB-RBTREE"
  (:use "CL" "SB-INT" "SB-EXT"))
(defpackage "SB-RBTREE.WORD"
  (:use "CL")
  (:shadow "DELETE")
  (:export "INSERT" "DELETE"))
(defpackage "SB-RBTREE.MAP"
  (:use "CL")
  (:shadow "DELETE")
  (:export "INSERT" "DELETE"))

(cl:in-package "SB-RBTREE")

(defmacro get-layout (string)
  (sb-kernel:find-layout (intern string)))
(defmacro childless (node)
  `(= (sb-kernel:%instance-length ,node) ,(1+ sb-vm:instance-data-start)))

;;; This gets a 4.5% consing reduction in the bbtrees benchmark,
;;; but redblack trees are still not as good as brothertrees.
(defconstant +fringe-node-storage-optimization+ t)

(defmacro define-tree-class (&key key-type value-type (lessp '<)
                             &aux (data-type (if value-type 'cons key-type))
                                  (name (intern "RBNODE"))
                                  (red-ctor (intern "RED-NODE"))
                                  (balance (intern "BALANCE"))
                                  (key (if value-type 'car 'identity)))

;; remap accessors/constructors/predicates as written in this package
;; to the corresponding symbols in the specialization package.
`(macrolet ((rb-node (&rest r) `(,(intern "RB-NODE") ,@r))
            (red-node (&rest r) `(,(intern "RED-NODE") ,@r))
            (black-node (&rest r) `(,(intern "BLACK-NODE") ,@r))
            (balance (&rest r) `(,(intern "BALANCE") ,@r))
            (color-of (x) `(,(intern "COLOR-OF") ,x))
            (left (x) `(,(intern "LEFT") ,x))
            (data (x) `(,(intern "DATA") ,x))
            (right (x) `(,(intern "RIGHT") ,x))
            (redp (x) `(,(intern "REDP") ,x))
            (blackp (x) `(,(intern "BLACKP") ,x))
            (redden (x) `(,(intern "REDDEN") ,x))
            (blacken (x) `(,(intern "BLACKEN") ,x)))

;;; This structure is exactly 4 words with #+compact-instance-header
;;; I really wanted one variant of this structure to have a raw slot (SB-VM:WORD)
;;; for DATA, and one not to, but that wasn't working, only because of unintended
;;; consing and not for any real reason.
;;; But that doesn't mean I want to merge these definitions back together -
;;; I still want to figure out why there was excess consing.
(defstruct (,name (:conc-name "") (:constructor nil)
                  (:copier nil) (:predicate nil))
  (data  nil :read-only t :type ,data-type)
  (%left nil :read-only t :type (or ,name null))
  (%right nil :read-only t :type (or ,name null)))

(declaim (inline ,(intern "LEFT") ,(intern "RIGHT")))
(defun ,(intern "LEFT") (x) (if (childless x) nil (,(intern "%LEFT") x)))
(defun ,(intern "RIGHT") (x) (if (childless x) nil (,(intern  "%RIGHT") x)))

(defstruct (,(intern "RED-NODE") (:include ,name) (:copier nil)
            (:predicate ,(intern "REDP"))
            (:constructor ,(intern "%RED-NODE") (%left data %right))
            (:conc-name "")))

(defstruct (,(intern "BLACK-NODE") (:include ,name) (:copier nil)
            (:predicate ,(intern "BLACKP"))
            (:constructor ,(intern "%BLACK-NODE") (%left data %right))
            (:conc-name "")))

(declaim (inline ,(intern "RED-NODE") ,(intern "BLACK-NODE")))
(defun ,(intern "RED-NODE") (left data right)
  (if (or (not +fringe-node-storage-optimization+) left right)
      (,(intern "%RED-NODE") left data right)
      (let ((node (sb-kernel:%make-instance/mixed (1+ sb-vm:instance-data-start))))
        (sb-kernel:%set-instance-layout node (get-layout "RED-NODE"))
        ;; Nodes are immutable, hence no setf'er exists.
        (sb-kernel:%instance-set node sb-vm:instance-data-start data)
        (truly-the ,(intern "RED-NODE") node))))

(defun ,(intern "BLACK-NODE") (left data right)
  (if (or (not +fringe-node-storage-optimization+) left right)
      (,(intern "%BLACK-NODE") left data right)
      (let ((node (sb-kernel:%make-instance/mixed (1+ sb-vm:instance-data-start))))
        (sb-kernel:%set-instance-layout node (get-layout "BLACK-NODE"))
        ;; Nodes are immutable, hence no setf'er exists.
        (sb-kernel:%instance-set node sb-vm:instance-data-start data)
        (truly-the ,(intern "BLACK-NODE") node))))

(defmethod print-object ((self ,name) stream)
  (declare (optimize (speed 1) (safety 1)))
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~s" (data self))))

(defun ,(intern "RB-NODE") (color left data right)
  (ecase color
    (red   (red-node left data right))
    (black (black-node left data right))))

(defun ,(intern "BLACKEN") (tree)
  ;; FIXME: should blackp return T of NIL?
  (cond ((not tree) nil)
        ((blackp tree) tree)
        (t (black-node (left tree) (data tree) (right tree)))))

(defun ,(intern "REDDEN") (tree)
  (if (redp tree)
      tree
      (red-node (left tree) (data tree) (right tree))))

(defun ,(intern "COLOR-OF") (node)
  (etypecase node
    (,(intern "RED-NODE") 'red)
    (,(intern "BLACK-NODE") 'black)))

(declaim (inline ,(intern "RED-NODE") ,(intern "BLACK-NODE")))
(declaim (optimize (sb-c::insert-step-conditions 0)
                   (sb-c::instrument-consing 0)))

;;; This implementation of BALANCE differs from the reference algorithm
;;; only in that by passing 4 arguments rather than an object we can
;;; sometimes avoid consing and immediately discarding one node.
(flet ((,balance (color L data R)
  (when (eq color 'red)
    (return-from ,balance (red-node L data R)))
  (macrolet ((values* (&rest args)
               `(values ,@(mapcan (lambda (arg)
                                    (if (typep arg '(cons (eql node-slots)))
                                        (let ((node (cadr arg)))
                                          `((left ,node) (data ,node) (right ,node)))
                                        (list arg)))
                                  args))))
    (multiple-value-bind (a x b y c z d)
        (cond ((and (redp L) (redp (left L)))
               (let ((LL (left L)))
                 (values* (node-slots LL) (data L) (right L) data R)))
              ((and (redp L) (redp (right L)))
               (let ((RL (right L)))
                 (values* (left L) (data L) (node-slots RL) data R)))
              ((and (redp R) (redp (left R)))
               (let ((LR (left R)))
                 (values* L data (node-slots LR) (data R) (right R))))
              ((and (redp R) (redp (right R)))
               (let ((RR (right R)))
                 (values* L data (left R) (data R) (node-slots RR))))
              (t
               (return-from ,balance (rb-node color L data R))))
      (red-node (black-node a x b) y (black-node c z d))))))

(defun ,(intern "INSERT") (tree key ,@(when value-type '(value)))
  (declare (type ,key-type key))
  (let ((data ,(if value-type '(cons key value) 'key)))
    (labels ((ins (tree)
               (cond (tree
                      (let ((c (color-of tree))
                            (a (left tree))
                            (x (data tree))
                            (b (right tree)))
                        (cond ((,lessp key (the ,key-type (,key x)))
                               (balance c (ins a) x b))
                              ((,lessp (the ,key-type (,key x)) key)
                               (balance c a x (ins b)))
                              (t (rb-node c a data b)))))
                     (t
                      (red-node nil data nil)))))
      (blacken (ins tree)))))

(defun ,(intern "DELETE") (key tree)
  (declare (type ,key-type key))
  (declare (notinline ,(intern "RED-NODE") ,(intern "BLACK-NODE")))
  (labels ((del (tree)
             (when tree
               (binding* (((a y b) (node-slots tree)))
                 (cond ((,lessp key (the ,key-type (,key y))) (delete-left a y b))
                       ((,lessp (the ,key-type (,key y)) key) (delete-right a y b))
                       (t (rb-merge a b))))))
           (delete-left (a y b)
             (funcall (if (blackp a) #'balance-left #',red-ctor) (del a) y b))
           (delete-right (a y b)
             (funcall (if (blackp b) #'balance-right #',red-ctor) a y (del b)))
           (sub1 (tree) ; decrease the black height by 1
             (aver (blackp tree))
             (redden tree))
           (balance-left (L data R)
             (cond ((redp L) (red-node (blacken L) data R))
                   ((blackp R) (balance 'black L data (redden R)))
                   (t
                    (aver (and (redp R) (blackp (left R))))
                    (binding* (((a y b) (node-slots (left R)))
                               (z (data R))
                               (c (right R)))
                      (red-node (black-node L data a) y (balance 'black b z (sub1 c)))))))
           (balance-right (L data R)
             (cond ((redp R) (red-node L data (blacken R)))
                   ((blackp L) (balance 'black (redden L) data R))
                   (t
                    (aver (and (redp L) (blackp (right L))))
                    (binding* ((a (left L))
                               (x (data L))
                               ((b y c) (node-slots (right L))))
                      (red-node (balance 'black (sub1 a) x b) y (black-node c data R))))))
           (rb-merge (L R)
             (cond ((null L) R)
                   ((null R) L)
                   ((eq (color-of L) (color-of R))
                    (binding* (((a x b) (node-slots L))
                               ((c y d) (node-slots R))
                               (bc (rb-merge b c)))
                      (if (redp L)
                          (if (redp bc)
                              (red-node (red-node a x (left bc)) (data bc)
                                        (red-node (right bc) y d))
                              (red-node a x (red-node bc y d)))
                          (if (redp bc)
                              (red-node (black-node a x (left bc)) (data bc)
                                        (black-node (right bc) y d))
                              (balance-left a x (black-node bc y d))))))
                   ((redp R)
                    (red-node (rb-merge L (left R)) (data R) (right R)))
                   (t
                    (aver (redp L))
                    (red-node (left L) (data L) (rb-merge (right L) R)))))
           (node-slots (x) (values (left x) (data x) (right x))))
    (declare (inline node-slots))
    (blacken (del tree))))
) ; end (FLET BALANCE)
) ; end MACROLET
) ; end DEFMACRO

(defmacro define-search-methods (lessp methods)
  `(macrolet ; remap symbols to the specialization package
       ((node-key (x) `(,(intern "NODE-KEY") ,x))
        (left (x) `(,(intern "LEFT") ,x))
        (right (x) `(,(intern "RIGHT") ,x)))

     ,@(when (member '= methods)
         `((defun ,(intern "FIND=") (key tree)
             (loop
              (cond ((null tree) (return nil))
                    ((,lessp key (node-key tree)) (setq tree (left tree)))
                    ((,lessp (node-key tree) key) (setq tree (right tree)))
                    (t (return tree)))))))

     ;; Find a node with key less than or equal to KEY and as near it as possible.
     ,@(when (member '<= methods)
         `((defun ,(intern "FIND<=") (key tree)
             (named-let recurse ((node tree) (best nil))
               (cond ((null node) best)
                     ((,lessp key (node-key node)) (recurse (left node) best))
                     ((,lessp (node-key node) key) (recurse (right node) node))
                     (t node))))))

     ;; Find a node with key greater than or equal to KEY and as near it as possible.
     ,@(when (member '>= methods)
         `((defun ,(intern "FIND>=") (key tree)
             (named-let recurse ((node tree) (best nil))
               (cond ((null node) best)
                     ((,lessp key (node-key node)) (recurse (left node) node))
                     ((,lessp (node-key node) key) (recurse (right node) best))
                     (t node))))))))

;;; Each specialization of the structure is in its own package.
;;; This may not be the best way to do it.
;;; I was hoping that the FLET of BALANCE would eliminate consing when passing
;;; the arg, but it didn't. Block compilation might.
;;; So maybe I will need to rename the two structure types as something like
;;;  SB-RBTREE::|TREE<SET<WORD>>|
;;;  SB-RBTREE::|TREE<TABLE<FIXNUM,T>>|
;;; where the C++-template-like syntax indicates firstly whether the tree
;;; is used as a set versus k,v pairs, and secondly the element types,
;;; and devise a similar naming scheme for the insert/delete operators.
;;; Packages seemed like the natural choice. What are packages for if not this?
;;; (Anyway, I thought we only generate a warning about DEFPACKAGE and not also
;;; when changing the package. Maybe I was holding it wrong)

(in-package "SB-RBTREE.WORD")
;; WORD trees will be used to find a code blob base address from an interior pointer,
;; both in the debugger and in stack scanning.
;; I'd like the type of the DATA slot to be SB-EXT:WORD, but I saw an excessive number
;; of compiler notes about consing from that. FIXNUM is ok as long as we use *untagged*
;; base pointers, and a specified comparator and not just "<" which would do the wrong
;; thing (because "negative" fixnums are large positive adddresses)
(declaim (inline addr<))
(defun addr< (a b)
  (< (sb-kernel:get-lisp-obj-address a) (sb-kernel:get-lisp-obj-address b)))
(sb-rbtree::define-tree-class
    :key-type fixnum :value-type nil
    :lessp addr<)
(declaim (inline node-key node-value))
(defun node-key (node) (data node))
(defun node-value (node) node)
(sb-rbtree::define-search-methods addr< (= <=))
(export '(find= find<=))

(in-package "SB-RBTREE.MAP")
;; MAP trees can have any data associated with the key, which is an ordinary fixnum.
;; It would be conventional to store key and value as two slots,
;; but using a cons is actually better - the table operations
;; create new RBNODEs, but we can share the cons cells and/or update
;; the CDR (the user data) of any node while tree operations are in progress.
(sb-rbtree::define-tree-class :key-type fixnum :value-type t)
(declaim (inline node-key node-value))
(defun node-key (node) (sb-ext:truly-the fixnum (car (data node))))
(defun node-value (node) (cdr (data node)))
(sb-rbtree::define-search-methods < (= <= >=))
(export '(find= find>= find<=))
