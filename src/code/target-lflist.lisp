;;;; Lockfree singly-linked lists
;;;; using the algorithm of https://timharris.uk/papers/2001-disc.pdf.
;;;; The algorithm as described requires being able to change the
;;;; low-order bit of a pointer from 0 to 1 to mark pending deletions.
;;;; Java implementations support this through a wrapper object
;;;; known as AtomicMarkableReference.
;;;; SBCL directly supports the mark bit by using the lowtag.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-LOCKLESS")

;;; The changes to GC to support this code were as follows:
;;;
;;; * If an instance is flagged as being a LFlist node, then the first data slot
;;;   is treated as an instance pointer even if it missing its tag bits.
;;;
;;; * Since you can't pin an object that you don't a-priori have a tagged pointer
;;;   to, pinning a lockfree list node may implicitly pin not only that node but
;;;   also the successor node, since there would otherwise be no way to reconstruct
;;;   (in Lisp) a tagged pointer to the successor of a node pending deletion.
;;;
;;; * Copying a lockfree list node tries to copy the successor nodes into adjacent
;;;   memory just like copying a chain of cons cells. This is inessential but nice.
;;;
;;; * verify_range() knows how to verify the 'next' pointer even when it looks like
;;;   a fixnum. Without this it would have been more difficult to test the above.
;;;
;;; The remaining issue is relatively unimportant: neither traceroot nor
;;; DO-REFERENCED-OBJECT can follow untagged pointers.
;;; This is potentially more of an annoyance than it is a bug.

(defstruct (keyed-node
            (:conc-name nil)
            (:include list-node)
            (:constructor make-node (node-key node-data)))
  (node-key 0 :read-only t)
  (node-data))

(declaim (inline ptr-markedp node-markedp))
(defun ptr-markedp (bits) (fixnump bits))
(defun node-markedp (node) (fixnump (%node-next node)))

;;; Lockless lists should be terminated by *tail-atom*.
;;; The value of %NODE-NEXT of the tail atom is chosen such that we will never
;;; violate the type assertion in GET-NEXT if pointer is inadvertently
;;; followed out of the tail atom. It would mostly work to use NIL as the %NEXT,
;;; but just in terms of whether the %MAKE-LISP-OBJ expression is correct.
;;; (ORing in instance-pointer-lowtag does not change NIL's representation.)
;;; However, using NIL would violates the type assertion.
(define-load-time-global *tail-atom*
  (let ((node (%make-sentinel-node)))
    (setf (%node-next node) node)))

(defmacro endp (node) `(eq ,node (load-time-value *tail-atom*)))

(defconstant-eqx +predefined-key-types+
  #((fixnum  lfl-insert/fixnum  lfl-delete/fixnum  lfl-find/fixnum < =)
    (integer lfl-insert/integer lfl-delete/integer lfl-find/integer < =)
    (real    lfl-insert/real    lfl-delete/real    lfl-find/real < =)
    (string  lfl-insert/string  lfl-delete/string  lfl-find/string
             string< string=))
  #'equalp)

(defun make-ordered-list (&key (constructor '%make-lfl) key-type sort test)
  (multiple-value-bind (insert delete find inequality equality)
      (cond (key-type
             (when (or sort test)
               (error "Must not specify :SORT or :TEST with :KEY-TYPE"))
             (let ((operations (find key-type +predefined-key-types+ :key #'car)))
               (unless operations
                 (error ":TYPE must be one of ~S"
                        '#.(map 'list (lambda (x) (elt x 0)) +predefined-key-types+)))
               (values (symbol-function (elt operations 1))
                       (symbol-function (elt operations 2))
                       (symbol-function (elt operations 3))
                       (symbol-function (elt operations 4))
                       (symbol-function (elt operations 5)))))
            (t
             (if (and sort test)
                 (values #'lfl-insert/t #'lfl-delete/t #'lfl-find/t
                         (coerce sort 'function)
                         (coerce test 'function))
                 (error "Must specify both :SORT and :TEST"))))
    (let ((head (%make-sentinel-node)))
      (setf (%node-next head) *tail-atom*)
      (funcall constructor
               head insert delete find inequality equality))))

;;; "Marked" in the reference algorithm means ORing in a 1 to the low bit.
;;; For us it means *removal* of tag bits.
;;; MAKE-MARKED-REF can only be called in the scope of WITH-PINNED-OBJECTS.
;;; The critical invariant is that once a 'next' pointer has been turned into
;;; a fixnum, it CAN NOT change. Therefore, the object that GC implicitly pins
;;; - along with the explicit pin of NODE within MARKED+NEXT - is definitely the
;;; object whose tagged pointer is reconstructed. This is exactly why we choose
;;; the tagged state as the normal state and the untagged state as deleted.
;;; If that were reversed (so tag bits = deleted, no tag bits = normal) to be like
;;; the reference algorithm, wherein "marked" = "deleted", object pinning
;;; could fail. A competing thread could CAS the untagged bits, invoke GC, while we
;;; try to reconstructed a different object from bits that were read prior to the CAS
;;; and prior to the GC, during which time the object to reconstruct moved.
(declaim (inline make-marked-ref))
(defun make-marked-ref (x)
  (%make-lisp-obj (logandc2 (get-lisp-obj-address x) sb-vm:lowtag-mask)))

(declaim (inline get-next))
(defun get-next (node)
  ;; Storing NODE in *PINNED-OBJECTS* causes its successor to become pinned.
  (#+cheneygc sb-sys:without-gcing #+gencgc progn
   (let* ((sb-vm::*pinned-objects* (cons node sb-vm::*pinned-objects*))
          (%next (%node-next node)))
      (declare (truly-dynamic-extent sb-vm::*pinned-objects*))
      (values (truly-the list-node
               (%make-lisp-obj (logior (get-lisp-obj-address %next)
                                       sb-vm:instance-pointer-lowtag)))
              %next))))

(defmethod print-object ((list linked-list) stream)
  (print-unreadable-object (list stream :type t)
    (write-char #\{ stream)
    (let ((node (%node-next (list-head list))))
      (unless (endp node)
        (loop (multiple-value-bind (next ptr-bits) (get-next node)
                (when (ptr-markedp ptr-bits)
                  (write-char #\* stream))
                (write (node-key node) :stream stream)
                (setq node next)
                (when (endp node) (return))
                (write-char #\space stream)))))
    (write-char #\} stream)))

(defmethod print-object ((node list-node) stream)
  (cond ((typep node 'keyed-node)
         (print-unreadable-object (node stream :type t)
           (format stream "(~:[~;*~]~S ~S)"
                   (node-markedp node) (node-key node) (node-data node))))
        ((eq node *tail-atom*)
         (print-unreadable-object (node stream :type t)
           (write '*tail-atom* :stream stream)))
        (t
         (print-unreadable-object (node stream :type t :identity t)))))

(defmacro do-lockfree-list ((var list &optional result) &body body)
  `(let* ((.list. ,list)
          (,var (%node-next (list-head .list.))))
     (loop
      (when (endp ,var) (return ,result))
      (multiple-value-bind (.next. .ptr-bits.) (get-next (truly-the list-node ,var))
        (unless (ptr-markedp .ptr-bits.)
          (let ((,var ,var))
            (declare (ignorable ,var))
            ,@body))
        (setq ,var .next.)))))

;;; SEARCH returns a pair of nodes satisfying the following constraints:
;;;  - key(left) < search-key and key(right) >= search-key
;;;  - neither left nor right is marked for deletion
;;;  - right is the immediate successor of left
;;; Any logically deleted nodes in between left and right will be removed.
;;; Note that the predicate here is exactly as for #'SORT -
;;; it should return T if and only if strictly less than.
(defmacro lfl-search-macro (compare< type)
  `(prog ((left 0) (left-node-next 0) (right 0))
     search-again
     ;; 1. Find left and right nodes
     (binding* ((this head)
                ((next bits) (get-next this)))
       ;; There *must* be some node to the left of the key you've supplied.
       ;; It's the head node if nothing else. The head can't be marked for deletion.
       ;; So if this node is marked, you're using this function wrongly.
       ;; There ought to have been some unmarked node to the left.
       (aver (not (ptr-markedp bits)))
       (tagbody
        advance
            (setq left this left-node-next next)
        end-test
            (when (endp (setq this next))
              (go out))
            (multiple-value-setq (next bits) (get-next this))
            (when (ptr-markedp bits)
              (go end-test))
            (when (,compare< (truly-the ,type (node-key (truly-the keyed-node this)))
                             key)
              (go advance))
        out)
       (setq right this))
     ;; 2. Check adjacency
     (when (eq left-node-next right)
       ;; the reference algorithm has:
       ;;   "if ((right_node != tail) && is_marked_reference(right_node.next))"
       ;; but the first test is redundant, because right_node.next
       ;; can always be dereferenced. Skipping the test seems better than
       ;; doing it, as skipping it avoids a conditional branch.
       (if (node-markedp right)
           (go search-again)
           (return (values right (truly-the list-node left)))))
     ;; 3. Remove intervening marked nodes
     (when (eq (cas (%node-next (truly-the list-node left)) left-node-next right)
               left-node-next)
       ;; as above, the reference algorithm had "right_node != tail && ..." here
       (unless (node-markedp right)
         (return (values right left))))
     (go search-again)))

;;; This is pretty much the standard CAS-based atomic list insert algorithm.
(defmacro lfl-insert-macro (search compare= type)
  `(let ((new 0))
     (loop
      ;; LEFT and RIGHT are the nodes bracketing the insertion point.
      ;; In the case of a found key, RIGHT is the node with that key.
      (multiple-value-bind (right left) (,search ,@(if (eq type 't) '(list)) head key)
        (when (and (not (endp right))
                   (,compare= key (truly-the ,type (node-key right))))
          (return (values right t))) ; 2nd value = T for "existed"
        (when (eql new 0)
          (setq new (make-node key data)))
        (setf (%node-next new) right)
        (when (eq (cas (%node-next left) right new) right)
          (return (values new nil))))))) ; didn't exist

;;; Deletion
;;; Step 1: find the node to be deleted
;;; Step 2: mark it as pending deletion in the 'next' slot
;;; Step 3: swing the predecessor's next to the successor of deleted node.
;;;
;;; Example: After step 2 of deleting node C we have:
;;;     A --> B --> C --> D
;;;                 ^ (mark)
;;; If swapping node B's 'next' fails, then some operation occurred to the left.
;;; Due to deletion the predecessor of C might become node A:
;;;     A --> C --> D
;;; Due to insertion the predecessor of C might become node X:
;;;     A --> B --> X --> C --> D
(defmacro lfl-delete-macro (search compare= type)
  `(loop
    ;; Step 1: find
    (multiple-value-bind (this predecessor)
        (,search ,@(if (eq type 't) '(list)) (list-head list) key)
      (when (or (endp this)
                (not (,compare= key (truly-the ,type (node-key this)))))
        (return nil))
      (let ((succ (%node-next this)))
        (unless (fixnump succ)
          ;; Pin here because we're taking the address of the successor object.
          ;; Instead we could use bit-test-and-set on the x86 architecture.
          (with-pinned-objects (succ)
            ;; Step 2: logically delete 'this'
            (when (eq (cas (%node-next this) succ (make-marked-ref succ)) succ)
              ;; Step 3: physically delete by swinging the predecessor's successor
              (unless (eq succ (cas (%node-next predecessor) this succ))
                ;; Call SEARCH again which will perform physical deletion.
                (,search ,@(if (eq type 't) '(list)) (list-head list) key))
              (return t))))))))

(defmacro define-variation (type compare< compare=)
  (let ((search (symbolicate "LFL-SEARCH/" type)))
    `(progn
       (declaim (ftype (sfunction (,@(if (eq type 't) '(linked-list)) list-node ,type)
                                  (values list-node list-node))
                       ,search))
       (defun ,search (,@(if (eq type 't) '(list)) head key)
         (declare (optimize (debug 0)))
         (lfl-search-macro ,compare< ,type))

       (defun ,(symbolicate "LFL-INSERT/"type) (list key data)
         (declare (linked-list list) (,type key))
         (let ((head (list-head list)))
           (lfl-insert-macro ,search ,compare= ,type)))

       ;; same as INSERT, but starting from any node
       (defun ,(symbolicate "LFL-INSERT*/"type) (list head key data)
         (declare (linked-list list) (ignorable list) (,type key))
         (lfl-insert-macro ,search ,compare= ,type))

       (defun ,(symbolicate "LFL-DELETE/"type) (list key)
         (declare (linked-list list) (,type key))
         (lfl-delete-macro ,search ,compare= ,type))

       (defun ,(symbolicate "LFL-FIND/"type) (list key)
         (declare (linked-list list) (,type key))
         (let ((node (,search ,@(if (eq type 't) '(list)) (list-head list) key)))
           (when (and (not (endp node))
                      (,compare= key (truly-the ,type (node-key node))))
             node))))))

(define-variation real < =) ; uses general case of math functions
;; TODO: implement an INTEGER< assembly routine perhaps?
(define-variation integer < =) ; comparator= reduces to INTEGER-EQL
(define-variation fixnum < =)
(define-variation string string< string=)
(define-variation t
  (lambda (a b) (funcall (list-inequality list) a b))
  (lambda (a b) (funcall (list-equality list) a b)))

(defun lfl-insert (list key data)
  (funcall (list-inserter list) list key data))
(defun lfl-delete (list key)
  (funcall (list-deleter list) list key))
(defun lfl-find (list key)
  (funcall (list-finder list) list key))

;;; SAVE-LISP-AND-DIE must unlink logically deleted nodes, because coreparse
;;; would not understand how to followed untagged pointers in the event that
;;; heap relocation had to occur on restart. Of course the only way to see
;;; a logically deleted node here is if a deleting thread died a horrible
;;; sudden death.
;;; Each list will be processed exactly once.
(defun finish-incomplete-deletions (list)
  (let* ((pred (list-head list))
         (node (get-next pred)))
    (loop
      (when (endp node)
        (return))
      (multiple-value-bind (next bits) (get-next node)
        (if (ptr-markedp bits)
            (setf node next (%node-next pred) node)
            (setf pred node node next))))))

;;; The following functions are for examining state while debugging.
;;; Not properly concurrent, but no worries.
(defun lfl-keys (list)
  (collect ((copy))
    (do-lockfree-list (item list) (copy (node-key item)))
    (copy)))

(defun lfl-length (list) ; a snapshot at a point in time
  (let ((n 0))
    (do-lockfree-list (x list) (incf n))
    n))

;;; This function is not really part of the API. It preserve the deletion bit,
;;; which doesn't really make sense from an interface perspective.
;;; However, when devising tests of the algorithms, it is useful to capture
;;; a complete snapshot of the list.
(defun copy-lfl (lfl)
  (labels ((copy-chain (node)
             (if (eq node *tail-atom*)
                 node
                 (let ((copy (copy-structure node))
                       (copy-of-next (copy-chain (get-next node))))
                   (with-pinned-objects (copy-of-next)
                     (setf (%node-next copy)
                           (if (fixnump (%node-next node))
                               (make-marked-ref copy-of-next)
                               copy-of-next)))
                   copy))))
    (let ((new (copy-structure lfl)))
      (%instance-set new (get-dsd-index linked-list head)
            (copy-chain (list-head new)))
      new)))
