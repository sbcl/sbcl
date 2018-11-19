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

(defpackage "SB-LFL"
  (:use "CL" "SB-EXT" "SB-INT" "SB-SYS" "SB-KERNEL"))

(in-package "SB-LFL")

;;; The changes to GC to support this code were as follows:
;;; * One bit of the payload length in an instance header is reserved to signify
;;;   that the instance has a special GC scavenge method. This avoids indirecting
;;;   to the layout to see whether all instances of a type have a special method.
;;;
;;; * If an instance has a header bit so indicating, then the first data slot
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

(defstruct (node (:conc-name nil)
                 (:constructor %make-node (node-key node-data)))
  ;; Using either 0 or NIL as the 'next' would make sense for the final cell.
  ;; 0 makes things easier for C, but NIL makes things easier for Lisp.
  ;; Using NIL simplifies the test in MARKEDP+NEXT so that the condition
  ;; for ORing in tag bits is simply whether 'next' is a fixnum.
  ;; Using 0 would require checking for fixnum and non-zero.
  (%node-next nil)
  (node-data)
  (node-key 0 :read-only t))
;;; Change the layout bitmap from -1 to a bitmap with 1s for each tagged slot.
;;; These are essentially equivalent, however -1 indicates that all slots are
;;; tagged *and* that there is no special scavenge method.
;;; A positive number _may_ indicate that all slots are tagged, but also
;;; informs the scavenge that there may be a custom action as well.
(let ((layout (sb-kernel:find-layout 'node)))
  (setf (layout-bitmap layout)
        ;; Round to odd, make any padding slot tagged.
        ;; If we allow subtypes of NODE, then the bitmap will have to
        ;; be fixed up as well.
        (1- (ash 1 (logior (layout-length layout) 1)))))

(defconstant special-gc-strategy-flag #x800000)
(declaim (ftype (sfunction (t t) node) make-node))
(defun make-node (key data)
  (declare (inline %make-node))
  (let ((n (%make-node key data)))
    ;; FIXME: this bit needs to be frobbed with a vop, or better yet,
    ;; just set in the allocator; and should (ideally) be made atomic.
    ;; Note that SET-HEADER-DATA only works on OTHER-POINTER-LOWTAG.
    (with-pinned-objects (n)
      (let ((sap (int-sap (get-lisp-obj-address n))))
        (setf (sap-ref-word sap (- sb-vm:instance-pointer-lowtag))
              (logior special-gc-strategy-flag
                      (sap-ref-word sap (- sb-vm:instance-pointer-lowtag))))))
    n))

(define-load-time-global *tail-atom* (make-node nil :tail))

;;; Specialized list variants will be created for
;;;  fixnum, integer, real, string, generic "comparable"
;;; but the node type and list type is the same regardless of key type.
(defstruct (linked-list (:constructor %make-lfl)
                        (:conc-name list-))
  (head nil :type node)
  (tail nil :type node))

(defun new-lockfree-list ()
  (let ((head (make-node nil :head))
        (tail *tail-atom*))
    (setf (%node-next head) tail)
    (%make-lfl :head head :tail tail)))

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

(declaim (inline markedp markedp+next))
(defun markedp (node) (fixnump (%node-next node)))
(defun markedp+next (node)
  (with-pinned-objects (node) ; pinning a node also pins its 'next'
    (let ((next (%node-next node)))
      (if (fixnump next)
          (values t (truly-the node
                     (%make-lisp-obj (logior (get-lisp-obj-address next)
                                             sb-vm:instance-pointer-lowtag))))
          (values nil (truly-the node next))))))

(defun node-next (node)
  (nth-value 1 (markedp+next node)))

(defmethod print-object ((list linked-list) stream)
  (print-unreadable-object (list stream :type t)
    (write-char #\{ stream)
    (let ((node (%node-next (list-head list))))
      (unless (eq node (list-tail list))
        (loop (multiple-value-bind (deleted next) (markedp+next node)
                (when deleted
                  (write-char #\* stream))
                (write (node-key node) :stream stream)
                (setq node next)
                (when (eq node (list-tail list)) (return))
                (write-char #\space stream)))))
    (write-char #\} stream)))

(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type t)
    (format stream "(~:[~;*~]~D ~S)"
            (markedp node)
            (node-key node)
            (node-data node))))

(defmacro do-lockfree-list ((var list &optional result) &body body)
  `(let* ((.list. ,list)
          (.end. (list-tail .list.))
          (,var (%node-next (list-head .list.))))
     (loop
      (when (eq ,var .end.) (return ,result))
      (multiple-value-bind (.mark. .next.) (markedp+next (truly-the node ,var))
        (unless .mark. (let ((,var ,var)) (declare (ignorable ,var)) ,@body))
        (setq ,var .next.)))))

(defun lfl-length (list) ; a snapshot at a point in time
  (let ((n 0))
    (do-lockfree-list (x list) (incf n))
    n))

;;; SEARCH returns a pair of nodes satisfying the following constraints:
;;;  - key(left) < search-key and key(right) >= search-key
;;;  - neither left nor right is marked for deletion
;;;  - right is the immediate successor of left
;;; Any logically deleted nodes in between left and right will be removed.
(defmacro lfl-search-macro (compare< type)
  `(block search
    (let (left left-node-next right (tail (list-tail list)))
      (tagbody
       again
       ;; 1. Find left and right nodes
       (binding* ((this (list-head list))
                  ((markedp next) (markedp+next this)))
         (loop (unless markedp
                 (setq left this left-node-next next))
               (when (eq (setq this next) tail)
                 (return))
               (multiple-value-setq (markedp next) (markedp+next this))
               (unless (or markedp (,compare< (truly-the ,type (node-key this))
                                              key))
                 (return)))
         (setq right this))
       ;; 2. Check adjacency
       (when (eq left-node-next right)
         (if (and (neq right tail) (markedp right))
             (go again)
             (return-from search (values right left))))
       ;; 3. Remove intervening marked nodes
       (when (eq (cas (%node-next (truly-the node left)) left-node-next right)
                 left-node-next)
         (unless (and (neq right tail) (markedp right))
           (return-from search (values right left))))
       (go again)))))

;;; This is pretty much the standard CAS-based atomic list insert algorithm.
(defmacro lfl-insert-macro (search compare= type)
  `(let ((new (make-node key data)))
     (loop
      ;; LEFT and RIGHT are the nodes bracketing the insertion point.
      (multiple-value-bind (right left) (,search list key)
        (when (and (neq right (list-tail list))
                   (,compare= key (truly-the ,type (node-key right))))
         (return nil))
       (setf (%node-next new) right)
       (when (eq (cas (%node-next left) right new) right)
         (return new))))))

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
    (multiple-value-bind (this predecessor) (,search list key)
      (when (or (eql this (list-tail list))
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
                (,search list key))
              (return t))))))))

(defmacro define-variation (type compare< compare=)
  (let ((search (symbolicate "LFL-SEARCH/" type)))
    `(progn
       (declaim (ftype (sfunction (linked-list ,type) (values node node))
                       ,search))
       (defun ,search (list key) (lfl-search-macro ,compare< ,type))

       (defun ,(symbolicate "LFL-INSERT/"type) (list key data)
         (declare (linked-list list) (,type key))
         (lfl-insert-macro ,search ,compare= ,type))

       (defun ,(symbolicate "LFL-DELETE/"type) (list key)
         (declare (linked-list list) (,type key))
         (lfl-delete-macro ,search ,compare= ,type))

       (defun ,(symbolicate "LFL-FIND/"type) (list key)
         (declare (linked-list list) (,type key))
         (let ((node (,search list key)))
           (when (and (neq node (list-tail list))
                      (,compare= key (truly-the ,type (node-key node))))
             node))))))

(define-variation real < =) ; uses general case of math functions
;; TODO: implement an INTEGER< assembly routine perhaps?
(define-variation integer < =) ; comparator= reduces to INTEGER-EQL
(define-variation fixnum < =)
(define-variation string string< string=)

;;; SAVE-LISP-AND-DIE must unlink logically deleted nodes, because coreparse
;;; would not understand how to followed untagged pointers in the event that
;;; heap relocation had to occur on restart. Of course the only way to see
;;; a logically deleted node here is if a deleting thread died a horrible
;;; sudden death.
;;; Each list will be processed exactly once.
(defun finalize-deletion (list)
  (let* ((pred (list-head list))
         (node (node-next pred)))
    (loop
      (when (eq node (list-tail list))
        (return))
      (multiple-value-bind (markedp next) (markedp+next node)
        (if markedp
            (setf node next (%node-next pred) node)
            (setf pred node node next))))))
