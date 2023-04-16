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
;;; * classoid -> layout hash-tables in classoid-subclasses
;;;   will maybe fix some deadlock issues by removing locks?
;;;   total ordering by an opaque globally unique ID.
;;;   (These are currently not created as :synchronized
;;;   because access is guarded by the world lock)
;;;   If all of the table are replaced by split-ordered lists,
;;;   the net memory used decreases, although at larger sizes
;;;   the split-ordered list consumes more memory.
;;;
;;;  * exact remembered sets for GC in immobile space
;;;

(in-package "SB-LOCKLESS")

(defmethod print-object ((self split-ordered-list) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~d keys, ~d bins" (so-count self)
            (length (car (so-bins self))))))

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
  (let ((head (truly-the list-node head))
        (hash (truly-the fixnum hash)))
    (macrolet ((cast-to-word (x)
                 `(get-lisp-obj-address ,x))
               (compare (x y)
                 (declare (ignore x y))
                 `(let ((x (node-hash (|the| so-node this))))
                    (cond ((< x hash) t)
                          ((= x hash)
                           (< (cast-to-word (so-key (|the| so-key-node this)))
                              (cast-to-word key)))))))
      (lfl-search-macro compare t))))

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

;;; The internal -PUT methods do not reference a bin, just a lockfree-linked-list
;;; headed by START, which is a dummy node.  The desired key may or may not exist
;;; in the list yet. If it exists, the containing node will be returned,
;;; and if it does not exist, a new node will be allocated and inserted.
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
 (defun %so-put/fixnum (start hash key &optional (data nil datap))
   (declare (fixnum key))
   (guts fixnum %so-search/fixnum =))
 (defun %so-put/addr (start hash key &optional (data nil datap))
   (guts t %so-search/addr eq))
 (defun %so-put/string (start hash key &optional (data nil datap))
   (declare (string key))
   (guts string %so-search/string string=))
 )

(defun initialize-bin (vector index shift)
  ;; Find any bin to the left to use as the starting point.
  ;; The reference algorithm used the "parent" bin, which involved recursion
  ;; if that bin was itself uninitialized, and so on.
  ;; Backwards linear scan works just as well and is easier to understand.
  (let* ((initialized-bin (find-if-not #'unbound-marker-p vector :end index :from-end t))
         (hash (ash index shift))
         (node (%so-insert/dummy initialized-bin hash))
         (old (cas (svref vector index) (make-unbound-marker) node)))
    (if (unbound-marker-p old) node old)))

(defmacro bin-shift (x)
  ;; Compiler conservatively can't use the declared slot type because it's a CONS
  `(truly-the (integer 1 (,+hash-nbits+)) (cdr ,x)))

(declaim (inline masked-hash))
;;(defun masked-hash (hash) (mask-field (byte (1- +hash-nbits+) 1) hash))
;; HASH *must* be non-negative. We don't mask out the sign bit any more.
(defun masked-hash (hash) (logior hash 1))
(defmacro with-bin ((table-var ; input
                     hash-var node-var &rest rest) ; output
                    hash-expr &body body)
  `(multiple-value-bind (,hash-var ,node-var ,@rest)
       (let* ((hash (masked-hash ,hash-expr))
              (bins (so-bins ,table-var)))
         (declare (optimize (safety 0))) ; won't pertain to the body, just these bindings
         (let* ((bin-vector (car bins))
                (shift (bin-shift bins))
                (index (ash hash (- shift)))
                (start-node (svref bin-vector index)))
           (values hash (if (unbound-marker-p start-node)
                            (initialize-bin bin-vector index shift)
                            start-node)
                   ;; INSERT needs the bins but FIND and DELETE don't
                   ,@(if rest '(bins)))))
     ,@body))

(defun so-expand-bins (table bins &aux (shift (bin-shift bins)))
  (when (eql shift 1)
    ;; The table would have to double (1- N-POSITIVE-FIXNUM-BITS) times
    ;; or something like that - maybe I'm off by 2x - for this failure to happen.
    (return-from so-expand-bins nil))
  ;; Try to compare-and-swap the threshold first before changing the bins.
  ;; This way, at most one thread should win, and rellocate bins.
  (let* ((bin-vector (car bins))
         (cur-n-bins (length bin-vector))
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
      (let ((new-bin-vector (make-array new-n-bins :initial-element (make-unbound-marker))))
        (declare (optimize (sb-c::insert-array-bounds-checks 0)))
        (dotimes (i cur-n-bins) (setf (svref new-bin-vector (* i 2)) (svref bin-vector i)))
        ;; If the CAS fails, there was already at least another doubling in another thread.
        (cas (so-bins table) bins (cons new-bin-vector (1- shift)))))))

(defun so-insert (table key &optional (value nil valuep))
  (when (and valuep (not (so-valuesp table)))
    (error "~S is a set, not a map" table))
  (with-bin (table hash start-node bins) (funcall (so-hashfun table) key)
    (multiple-value-bind (node foundp)
        (if (so-valuesp table)
            (funcall (so-inserter table) start-node hash key value)
            (funcall (so-inserter table) start-node hash key))
      (cond (foundp ; must not find a dummy node
             ;; Note that in this case we do not update SO-DATA. The client of the table
             ;; can use the secondary value to notice that the node existed,
             ;; and pick an appropriate way to atomically update the node.
             (aver (typep node 'so-key-node)))
            ((> (atomic-incf (so-count table)) (so-threshold table))
             (so-expand-bins table bins)))
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
    (let ((deleted (funcall (so-deleter table) start-node hash key)))
      (when deleted
        (atomic-decf (so-count table)))
      deleted)))

(defmacro multiplicative-hash (x)
  ;; Use Knuth's hash multiplier of 2^32 * (-1 + sqrt(5)) / 2.
  ;; This constant works quite well for our variant of the solist algorithm
  ;; which consumes hash bits from most-significant to least-significant.
  ;; Note that most descriptions of this require a right-shift, but here it's
  ;; exactly the correct answer by itself because the solist algorithm consumes
  ;; bits from left-to-right (most-to-least-significant).
  #-64-bit
  `(ash (logand (* ,x 2654435769) sb-ext:most-positive-word)
        ,(- (1+ sb-vm:n-fixnum-tag-bits))) ; Ensure a positive fixnum result
  ;; Same thing but with 64 bits of precision. I used MPFR to compute this
  ;; (also https://asecuritysite.com/hash/smh_fib gives the same number).
  ;; The number 11400714819323198486 is slightly more correct, but an odd multiplier
  ;; is better than even, otherwise the rightmost result bit would be always 0.
  #+64-bit
  `(ash (logand (* ,x 11400714819323198485) sb-ext:most-positive-word)
        ,(- (1+ sb-vm:n-fixnum-tag-bits))))

(macrolet ((guts (key-hash searcher equality-fn)
             `(with-bin (table hash start-node) ,key-hash
                (let ((node (,searcher start-node hash key)))
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
    (let ((h (multiplicative-hash (get-lisp-obj-address key))))
      (guts h %so-search/addr eq)))
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
         (%make-so-list #'murmur-hash-word/+fixnum
                        #'%so-put/fixnum
                        #'%so-delete/fixnum
                        #'so-find/fixnum
                        #'nofun #'nofun valuesp)))
  (defun make-so-set/fixnum () (make nil))
  (defun make-so-map/fixnum () (make t)))

(flet ((make (valuesp)
         (%make-so-list #'sxhash
                        #'%so-put/string
                        #'%so-delete/string
                        #'so-find/string
                        #'nofun #'nofun valuesp)))
  (defun make-so-set/string () (make nil))
  (defun make-so-map/string () (make t)))

(flet ((make (valuesp)
         (%make-so-list (lambda (x) (multiplicative-hash (get-lisp-obj-address x)))
                        #'%so-put/addr #'%so-delete/addr #'so-find/addr
                        #'nofun #'nofun valuesp)))
  (defun make-so-set/addr () (make nil))
  (defun make-so-map/addr () (make t)))

;;; This special case can be used during allocation of new objects (and usually only then).
;;; The address can not have previously existed in the table since it is fresh.
;;; Additionally, this takes a pre-allocated NODE, does not perform INITIALIZE-BIN,
;;; and does not increment the occupancy count. The latter two steps can be performed later.
;;; This can be run within a pseudo-atomic section, and the next step outside of it.
(defun %so-eq-set-phase1-insert (table node key)
  (let* ((hash (masked-hash (multiplicative-hash (get-lisp-obj-address key))))
         (bins (so-bins (truly-the split-ordered-list table)))
         (shift (bin-shift bins))
         (bin-vector (car bins))
         (index (ash hash (- shift)))
         (start-node (svref bin-vector index)))
    ;; HASH and KEY of a node are not accessible as read/write slots to clients
    ;; of the table, but _can_ be written by this insert function only in as much as
    ;; the caller has to preallocate the node, and we have to fill it in here.
    (setf (%instance-ref (truly-the instance node) (get-dsd-index so-node node-hash)) hash
          (%instance-ref node (get-dsd-index so-key-node so-key)) key)
    (when (unbound-marker-p start-node)
      ;; Pick any nonempty bin to the left of the intended bin.
      ;; It's always OK to pick a suboptimal start bin, because there is no requirement
      ;; to observe the BINS vector in the most up-to-date state anyway.
      (setf start-node (find-if-not #'unbound-marker-p bin-vector
                                    :end index :from-end t)))
    #+sb-devel (aver (dummy-node-p start-node))
    (loop
     (multiple-value-bind (right left) (%so-search/addr start-node hash key)
       #+sb-devel
       (when (and (not (endp right)) (not (dummy-node-p right)))
         ;; The successor had better not be the droid you're looking for.
         (aver (neq key (so-key right))))
       (setf (%node-next node) right)
       (when (eq (cas (%node-next left) right node) right)
         (return t))))))

;;; Complete the insertion of a previously-known-not-to-exist key.
(defun %so-eq-set-phase2-insert (table node)
  (with-bin (table hash start-node bins) (node-hash node)
    (declare (ignore hash start-node))
    (when (> (atomic-incf (so-count table)) (so-threshold table))
      (so-expand-bins table bins)))
  node)

(defun c-so-find/addr (solist key)
  (let ((bins (so-bins solist)))
    ;; Technically this needs to inhibit GC or else use immobile objects
    ;; for all list nodes, but it's good enough for now.
    (sb-sys:with-pinned-objects (solist bins (car bins) key)
      (let ((result
             (sb-alien:alien-funcall
              (sb-alien:extern-alien "split_ordered_list_find"
                                     (function sb-alien:unsigned
                                               sb-alien:unsigned sb-alien:unsigned))
              (logandc2 (get-lisp-obj-address solist) sb-vm:lowtag-mask)
              (get-lisp-obj-address key))))
        (unless (= result 0)
          ;; need to be unsafe here, otherwise might get
          ;; "<integer> is not a valid argument to SB-KERNEL:MAKE-LISP-OBJ"
          ;; because the allocation region wasn't closed.
          ;; Would it better for tests to close the region? Maybe,
          ;; but we can't count on everybody doing that.
          (%make-lisp-obj (logior result sb-vm:instance-pointer-lowtag)))))))
