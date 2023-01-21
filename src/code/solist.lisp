;;; Lockfree hash table and hash set based on Shalev & Shavit paper
;;; https://www.cs.tau.ac.il/~afek/SplitOrderListHashSS03.pdf

;;; This table never uses a mutex for any operation, including resizing.
;;; The paper does not describe in detail how to deal with hash collisions,
;;; so this implementation imposes a requirement that either there be no
;;; hash collisions, or there be a partial order on keys such that any keys
;;; whose hashes collide can be ordered. Usually there would be a total order.

;;; Also we change the way a hash implies a bucket. Instead of reversing
;;; the bits of the hash, we use bits from left-to-right, so that as the
;;; bucket vector doubles, one more bit of lesser significance comes into play.

;;; Some possible uses:
;;; * Package name table
;;;   total ordering: name
;;;   This maybe doesn't need to be lockfree because there are invariants
;;;   enforced by the global lock.
;;;   However, it would be nice to remove the global lock.
;;;
;;; * classoid -> layout hash-tables in classoid-subclasses
;;;   will maybe fix some deadlock issues by removing locks?
;;;   total ordering by an opaque globally unique ID.
;;;   (These are currently not created as :synchronized
;;;   because access is guarded by the world lock)
;;;   If all of the table are replaced by split-ordered lists,
;;;   the net memory used decreases, although at larger sizes
;;;   the split-ordered list consumes more memory.
;;;
;;;  * finalizer store. very tricky with movable objects
;;;    might need frequent rebuilds (same as status quo)
;;;    and the rebuild may need to occur from within C.
;;;
;;;  * exact remembered sets for GC in immobile space
;;;

(in-package "SB-LOCKLESS")

(defmethod print-object ((self split-ordered-list) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~d keys, ~d bins" (so-count self)
            (length (car (so-bins self))))))

;;; A split-order dummy node has only a hash. Hashes of dummy nodes are unique.
;;; The least-significant-bit in the hash of a dummy node is 0, and in an ordinary
;;; node it is 1, so that there always exists a dummy node whose hash is less
;;; than that of an ordinary node.
(defstruct (so-node (:conc-name nil)
                    (:include list-node)
                    (:copier nil)
                    (:constructor %make-so-dummy-node (node-hash)))
  (node-hash 0 :type fixnum :read-only t))
;;; An ordinary node has a key and hash. Keys are unique but hashes can be nonunique,
;;; though uniqueness will improve efficiency of the algorithms.
;;; The ordering is primarily based on hash, using key as a tiebreaker,
;;; so there is a total ordering even if there are hash collisions.
(defstruct (so-key-node
            (:conc-name nil)
            (:include so-node)
            (:copier nil)
            (:constructor %make-so-set-node (node-hash so-key)))
  (so-key (missing-arg))) ; should be readonly. DO NOT MUTATE without extreme care
(defstruct (so-data-node
            (:conc-name nil)
            (:include so-key-node)
            (:copier nil)
            (:constructor %make-so-map-node (node-hash so-key so-data)))
  (so-data nil))
(defmethod print-object ((self so-key-node) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "hash=~x key=~a~@[ val=~a~]"
            (node-hash self) (so-key self)
            (if (typep self 'so-data-node) (so-data self)))))

(declaim (inline dummy-node-p))
(defun dummy-node-p (node) (evenp (node-hash node)))

(declaim (ftype (sfunction (fixnum) so-node) make-so-dummy-node))
(defun make-so-dummy-node (hash)
  (declare (inline %make-so-dummy-node))
  ;; CROSS-TYPEP croaks on (THE (SATISFIES EVENP) HASH)
  ;; I think we're OK leaving out the assertion.
  (%make-so-dummy-node hash))

(declaim (ftype (sfunction (fixnum t t t) so-key-node) make-so-node))
(defun make-so-node (hash key datap data)
  (declare (inline %make-so-set-node %make-so-map-node))
  (aver (oddp hash))
  (if datap
      (%make-so-map-node hash key data)
      (%make-so-set-node hash key)))

;; define this as truly-the when fully debugged
;(defmacro |the| (x y) `(the ,x ,y))
(defmacro |the| (x y) `(truly-the ,x ,y))

(defun %so-search/fixnum (head hash key)
  (declare (list-node head) (fixnum hash) (fixnum key))
  (macrolet ((compare (x y)
               (declare (ignore x y))
               `(let ((x (node-hash (|the| so-node this))))
                  (cond ((< x hash) t)
                        ((= x hash)
                         (< (|the| fixnum (so-key (|the| so-key-node this))) key))))))
    (lfl-search-macro compare fixnum)))

(defun %so-search/addr (head hash key)
  ;; KEY is any object, which either has to be immobile, or else GC has to repair the table.
  (declare (list-node head) (fixnum hash))
  (macrolet ((cast-to-word (x) `(get-lisp-obj-address ,x))
             (compare (x y)
               (declare (ignore x y))
               `(let ((x (node-hash (|the| so-node this))))
                  (cond ((< x hash) t)
                        ((= x hash)
                         (< (cast-to-word (so-key (|the| so-key-node this)))
                            (cast-to-word key)))))))
    (lfl-search-macro compare t)))

(defun %so-search/string (head hash key)
  (declare (list-node head) (fixnum hash) (string key))
  (macrolet ((compare (x y)
               (declare (ignore x y))
               `(let ((x (node-hash (|the| so-node this))))
                  (cond ((< x hash) t)
                        ((= x hash)
                         (string< (so-key (|the| so-key-node this)) key))))))
    (lfl-search-macro compare string)))

;;; Ensure existence of dummy node having HASH.
(defun %so-insert/dummy (start hash &aux (new 0))
  (loop
    ;; Use the LFL-SEARCH method, not the SO- search method.
    ;; This hash must be unique amongst the hashes, so if it exists in the list,
    ;; we should stop looking. The SO- method would, upon seeing an equal hash,
    ;; attempt to use the SO-KEY as a tiebreaker. That would be wrong,
    ;; especially as dummy nodes do not have keys.
    ;; Note that a dummy node's hash can not collide with a key node's hash,
    ;; because they will always differ in the least significant bit.
    #+nil (format t "~&Inserting dummy node with hash ~32,'0b~%" hash)
    (multiple-value-bind (right left) (lfl-search/fixnum start hash)
      ;; LEFT and RIGHT are the nodes bracketing the insertion point.
      ;; In the case of a found key, RIGHT is the node with that key.
      (aver (< (node-hash left) hash)) ; CAN BE REMOVED
      (aver (or (endp right) (>= (node-hash right) hash))) ; CAN BE REMOVED
      (when (and (not (endp right)) (= (node-hash right) hash))
        (return right))
      (when (eql new 0)
        (setq new (make-so-dummy-node hash)))
      (setf (%node-next new) right)
      (when (eq (cas (%node-next left) right new) right)
        (return new))))) ; didn't exist

(defun initialize-bin (bins bin-number shift)
  ;; Find any bin to the left to use as the starting point.
  ;; The reference algorithm used the "parent" bin, which involved recursion
  ;; if that bin was itself uninitialized, and so on.
  ;; Backwards linear scan works just as well and is easier to understand.
  (let* ((initialized-bin
          (find-if-not #'unbound-marker-p bins :end bin-number :from-end t))
         (hash (ash bin-number shift))
         (node (%so-insert/dummy initialized-bin hash))
         (old (cas (svref bins bin-number) (make-unbound-marker) node)))
    (if (unbound-marker-p old) node old)))

;;; The internal methods do not reference a bin, just a lockfree-linked-list
;;; headed by START, which is a dummy node.
(macrolet ((guts (type searcher equality-fn)
             `(let ((new 0))
                (aver (dummy-node-p start))
                ;; (format t "enter insert: hash=~x~%" hash)
                (loop
                 ;; LEFT and RIGHT are the nodes bracketing the insertion point.
                 ;; If the key was found, then RIGHT should be the node with that key.
                 ;; Note that the search method can return *any* RIGHT key that is >=
                 ;; than KEY, so we have to test for equality here.
                 (multiple-value-bind (right left) (,searcher start hash key)
                   (when (and (not (endp right))
                              (not (dummy-node-p right))
                              (,equality-fn (|the| ,type (so-key right)) key))
                     (return (values right t))) ; 2nd value = T for "existed"
                   (when (eql new 0)
                     (setq new (make-so-node hash key datap data)))
                   (setf (%node-next new) right)
                   ;; This insert attempt between LEFT and RIGHT might not be the only
                   ;; one in progress. If it works, then we can quit now, but if not,
                   ;; then start from the top just in case KEY was already inserted.
                   (when (eq (cas (%node-next left) right new) right)
                     (return (values new nil)))))))) ; didn't exist
 (defun %so-insert/fixnum (start hash key &optional (data nil datap))
   (declare (fixnum key))
   (guts fixnum %so-search/fixnum =))
 (defun %so-insert/addr (start hash key &optional (data nil datap))
   (guts t %so-search/addr eq))
 (defun %so-insert/string (start hash key &optional (data nil datap))
   (declare (string key))
   (guts string %so-search/string string=))
 )

(declaim (inline masked-hash))
(defun masked-hash (hash) (mask-field (byte (1- +hash-nbits+) 1) hash))

(defmacro with-bin ((table-var ; input
                     hash-var node-var &rest rest) ; output
                    hash-expr &body body)
  `(multiple-value-bind (,hash-var ,node-var ,@rest)
       (let* ((hash (masked-hash ,hash-expr))
              (bins+shift (so-bins ,table-var)))
         (declare (optimize (safety 0))) ; won't pertain to the body, just these bindings
         (let* ((bins (car bins+shift))
                ;; We don't use the declared type- I guess because it's a CONS ?
                (shift (the (integer 1 (,+hash-nbits+)) (cdr bins+shift)))
                (bin-number (ash hash (- shift)))
                (start-node (svref bins bin-number)))
           (values hash (if (unbound-marker-p start-node)
                            (initialize-bin bins bin-number shift)
                            start-node)
                   ;; INSERT needs these but FIND and DELETE don't
                   ,@(if rest '(bins+shift bins shift)))))
     ,@body))

(defun so-insert (table key &optional (value nil valuep))
  (when (and valuep (not (so-valuesp table)))
    (error "~S is a set, not a map" table))
  (with-bin (table hash start-node bins+shift bins shift) (funcall (so-hashfun table) key)
    (multiple-value-bind (node foundp)
        (if (so-valuesp table)
            (funcall (so-inserter table) start-node (logior hash 1) key value)
            (funcall (so-inserter table) start-node (logior hash 1) key))
      (cond (foundp ; must not find a dummy node
             ;; Note that in this case we do not update SO-DATA. The client of the table
             ;; can use the secondary value to notice that the node existed,
             ;; and pick an appropriate way to atomically update the node.
             (aver (typep node 'so-key-node)))
            ((and (> (atomic-incf (so-count table)) (so-threshold table))
                  (> shift 1)) ; can't reduce the shift below 1
             ;; Try to compare-and-swap the threshold first before changing the bins.
             ;; This way, at most one thread should win, and rellocate bins.
             (let* ((cur-n-bins (length bins))
                    (new-n-bins (the index (* 2 cur-n-bins)))
                    (cur-threshold (so-threshold table))
                    (new-threshold (the index (* new-n-bins (so-elts-per-bin table)))))
               ;; These tables don't downsize ever (same as our HASH-TABLE), so just make sure
               ;; we're increasing the threshold.  Due to unusual scheduling of threads, it could
               ;; be that CUR-THRESHOLD is already larger than NEW-THRESHOLD.
               (when (and (> new-threshold cur-threshold)
                          #+(or arm mips sparc) ; no support for raw slot atomic ops, really?
                          (setf (so-threshold table) new-threshold)
                          #-(or arm mips sparc)
                          (eql (cas (so-threshold table) cur-threshold new-threshold)
                               cur-threshold))
                 (let ((new-bins (make-array new-n-bins :initial-element (make-unbound-marker))))
                   (declare (optimize (sb-c::insert-array-bounds-checks 0)))
                   (dotimes (i cur-n-bins) (setf (aref new-bins (* i 2)) (aref bins i)))
                   ;; If the CAS fails, there was already at least another doubling in another thread.
                   (cas (so-bins table) bins+shift (cons new-bins (1- shift))) bins+shift)))))
      (values node foundp))))

;; This is like LFL-DELETE-MACRO but passes both a hash and a key
;; to the search function. The hash is the primary key of the usual LFL search,
;; but the user's "actual" key breaks ties when hashes collide.
(defmacro so-delete-macro (search compare= type)
  `(loop
    ;; Step 1: find
    (multiple-value-bind (this predecessor)
        (,search ,@(if (eq type '*) '(list)) head hash key)
      ;(format t "~&Deletion point: ~S and ~S (~D)~%" predecessor this (dummy-node-p this))
      (when (or (endp this)
                (dummy-node-p this)
                (not (,compare= key (|the| ,type (so-key this)))))
        (return nil))
      (let ((succ (%node-next this)))
        ;; If THIS is already marked for deletion (its successor pointer is a fixnum),
        ;; then we have to search again, which should finish the physical delete
        ;; using the correct object pinning strategy. i.e. we can't just OR in
        ;; some tag bits now and assume that the resulting bit pattern is
        ;; a legal pointer, because THIS was not pinned while reading %NODE-NEXT.
        (unless (fixnump succ)
          ;; Pin here because we're taking the address of the successor object.
          ;; This is the ordinary WITH-PINNED-OBJECTS which manipulates
          ;; *PINNED-OBJECTS* only if precise GC. Compare/contrast with
          ;; GET-NEXT which _always_ binds *PINNED-OBJECTS* except for
          ;; the architectures that provide a vop employing pseudo-atomic.
          (with-pinned-objects (succ)
            ;; Step 2: logically delete THIS by changing its successor pointer
            ;; to a marked reference. If CAS fails, then we're racing to
            ;; insert or delete. There's no other possibility.
            (when (eq (cas (%node-next this) succ (make-marked-ref succ)) succ)
              ;; Step 3: physically delete by swinging the predecessor's successor
              (unless (eq succ (cas (%node-next predecessor) this succ))
                ;; Call SEARCH again which will perform physical deletion.
                (,search ,@(if (eq type '*) '(list)) head hash key))
              (return t)))))))) ; T = successful removal of KEY

(defun %so-delete/fixnum (head hash key)
  (so-delete-macro %so-search/fixnum = fixnum))
(defun %so-delete/addr (head hash key)
  (so-delete-macro %so-search/addr eq t))
(defun %so-delete/string (head hash key)
  (so-delete-macro %so-search/string string= string))

(defun so-delete (table key)
  (with-bin (table hash start-node) (funcall (so-hashfun table) key)
    (let ((deleted (funcall (so-deleter table) start-node (logior hash 1) key)))
      (when deleted
        (atomic-decf (so-count table)))
      deleted)))

(macrolet ((guts (key-hash searcher equality-fn)
             `(with-bin (table hash start-node) ,key-hash
                (let ((node (,searcher start-node (logior hash 1) key)))
                  (when (and (not (endp node))
                             (not (dummy-node-p node))
                             (,equality-fn (so-key node) key))
                    node)))))
  (defun so-find/fixnum (table key)
    (declare (split-ordered-list table))
    (declare (fixnum key))
    (guts (murmur-hash-word/fixnum key) %so-search/fixnum =))
  (defun so-find/addr (table key)
    (declare (split-ordered-list table))
    (guts (funcall (so-hashfun table) key) %so-search/addr eq))
  (defun so-find/string (table key)
    (declare (split-ordered-list table))
    (declare (string key))
    (guts (sxhash key) %so-search/string string=)))

(defun so-find (table key)
  (funcall (so-finder table) table key)) ; returns a NODE or NIL

(defun nofun (&rest args)
  (declare (ignore args))
  (bug "don't call me"))

(defun %make-so-list (&rest args)
  (let ((initial-bin (make-so-dummy-node 0)))
    (setf (%node-next initial-bin) +tail+)
    (let ((so-list (apply #'%%make-split-ordered-list initial-bin args)))
      ;; Start with 2 bins, only the first being initialized.
      ;; Shift out all bits except 1 for the bin number.
      (setf (so-threshold so-list) (* 2 (so-elts-per-bin so-list)) ; 2 bins
            (so-bins so-list) (cons (vector initial-bin (make-unbound-marker))
                                    (- +hash-nbits+ 1)))
      so-list)))

(defun so-maplist (function solist)
  (let ((node (%node-next (so-head solist))))
    (loop
     (when (endp node) (return))
     (if (evenp (node-hash node)) ; dummy node, no KEY, and can't be deleted
         (setq node (%node-next node))
         (multiple-value-bind (next bits) (get-next node)
           (unless (ptr-markedp bits)
             (funcall function node))
           (setq node next))))))

(flet ((make (valuesp)
         (%make-so-list #'murmur-hash-word/fixnum
                        #'%so-insert/fixnum
                        #'%so-delete/fixnum
                        #'so-find/fixnum
                        #'nofun #'nofun valuesp)))
  (defun make-so-set/fixnum () (make nil))
  (defun make-so-map/fixnum () (make t)))

(flet ((make (valuesp)
         (%make-so-list #'sxhash
                        #'%so-insert/string
                        #'%so-delete/string
                        #'so-find/string
                        #'nofun #'nofun valuesp)))
  (defun make-so-set/string () (make nil))
  (defun make-so-map/string () (make t)))

(flet ((make (valuesp)
         (%make-so-list (lambda (x) (murmur-hash-word/+fixnum (get-lisp-obj-address x)))
                        #'%so-insert/addr #'%so-delete/addr #'so-find/addr
                        #'nofun #'nofun valuesp)))
  (defun make-so-set/addr () (make nil))
  (defun make-so-map/addr () (make t)))
