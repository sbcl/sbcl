;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;;; XSET
;;;;
;;;; A somewhat efficient set implementation that can store arbitrary
;;;; objects. For small sets the data is stored in a list, but when
;;;; the amount of elements grows beyond +XSET-LIST-SIZE-LIMIT+, we
;;;; switch to a hash-table instead.
;;;;
;;;; ALLOC-XSET allocates an empty XSET. ADD-TO-XSET adds an element
;;;; to an XSET: it should be used only on freshly allocated XSETs.
;;;;
;;;; XSET-EMPTY-P, XSET-INTERSECTION, XSET-SUBSET-P, and XSET-MEMBER-P
;;;; do the obvious things. MAP-XSET maps over the element, but
;;;; requires a function as the first argument -- not a function
;;;; designator.
;;;;
;;;; Note: XSET always uses EQL as the equivalence test

(in-package "SB-KERNEL")

(defstruct (xset (:constructor alloc-xset ())
                 (:constructor !new-xset (data extra))
                 (:copier nil)
                 (:predicate nil))
  (data nil :type (or list hash-table))
  ;; EXTRA is a dual-purpose slot: initially it holds the number of items
  ;; in LIST. If the list becomes a hash-table, then EXTRA becomes 0.
  ;; An XSET can be optionally have a vector of stable hashes, 1 per element.
  ;; The hash vector if present goes in EXTRA, and the vector length
  ;; is the same as the list length. After creating a hash vector, it is forbidden
  ;; to add more elements to the set. In this manner we can avoid adding a subtype
  ;; of XSET stably-hashed-xset, or wasting a slot that would almost never be used.
  ;; (99.999% of all XSETs do not need stable hashes)
  (extra 0 :type (or simple-vector index)))
(declaim (freeze-type xset))

(defun xset-count (xset)
  (let ((data (xset-data xset)))
    (if (listp data)
        (let ((extra (xset-extra xset)))
          (if (fixnump extra) extra (length extra)))
        (hash-table-count data))))

(defun map-xset (function xset)
  (declare (function function))
  (declare (dynamic-extent function))
  (let ((data (xset-data xset)))
    (if (listp data)
        (dolist (elt data)
          (funcall function elt))
        (maphash (lambda (k v)
                   (declare (ignore v))
                   (funcall function k))
                 data)))
  nil)

(defconstant +xset-list-size-limit+ 24)

;;; Checks that the element is not in the set yet.
(defun add-to-xset (elt xset)
  (let ((data (xset-data xset)))
    (cond ((not (listp data)) (setf (gethash elt data) t))
          ((member elt data)) ; ignore
          ((< (xset-extra xset) +xset-list-size-limit+)
           (setf (xset-extra xset) (1+ (xset-extra xset))
                 (xset-data xset) (cons elt data)))
          (t
           (let ((table (make-hash-table :size 36))) ; arb
             (setf (gethash elt table) t)
             (dolist (x data)
               (setf (gethash x table) t))
             (setf (xset-extra xset) 0 ; looks nice to clear it
                   (xset-data xset) table)))))
  xset)

(defun xset-member-p (elt xset)
  (let ((data (xset-data xset)))
    (if (if (listp data)
            (member elt data :test #'eql)
            (gethash elt data))
        t
        nil)))

(defun xset-members (xset)
  (let ((data (xset-data xset)))
    (if (listp data)
        data
        (let (members)
          (maphash (lambda (k v)
                     (declare (ignore v))
                     (push k members))
                   data)
          members))))

;;; Possible TODO:
;;; INTERSECTION and UNION could allocate the new xset where the input(s)
;;; were, to avoid an extra copy operation.
;;; The reason for not always forcing to dynamic space is that I'd like
;;; (eventually) the compiler to able to run in an arena, with only its
;;; output forced to the dynamic or immmobile space.
(defun xset-intersection (a b)
  (declare (inline alloc-xset))
  (let ((intersection (alloc-xset)))
    ;; Under the assumption that lookup time is constant in either set,
    ;; you should scan the * smaller * set to see if each item
    ;; is in the larger set, thereby doing fewer constant-time steps.
    ;; SOURCE is the set to scan; LOOKUP is the set to check membership in.
    (multiple-value-bind (source lookup)
        (if (< (xset-count a) (xset-count b))
            (values a b)
            (values b a))
      (let ((data (xset-data lookup)))
        (map-xset (if (listp data)
                      (lambda (elt)
                        (when (member elt data :test #'eql)
                          (add-to-xset elt intersection)))
                      (lambda (elt)
                        (when (gethash elt data)
                          (add-to-xset elt intersection))))
                 source)))
    intersection))

;;; This attempts to return A or B if one is a subset of the other.
(defun xset-union (a b)
  (declare (inline !new-xset))
  (binding* (((small large)
              (if (< (xset-count a) (xset-count b)) (values a b) (values b a)))
             (data (xset-data large)))
  ;; If one of A or B is a hash-table, then surely it's the larger. The other might be too.
  ;; For each key in the smaller, count the elements missing from the larger.
  (if (hash-table-p data)
      (let ((missing 0))
        (declare (type index missing))
        (map-xset (lambda (x) (unless (gethash x data) (incf missing))) small)
        (cond ((= missing 0) large)
              (t
               ;; We have an exact count for the resulting hash-table.
               (let ((new (make-hash-table :size (+ (hash-table-count data) missing))))
                 ;; Ideally we would have a valueless hash-table so we don't store all the Ts
                 (map-xset (lambda (k) (setf (gethash k new) t)) small)
                 (maphash (lambda (k v) (setf (gethash k new) v)) data)
                 (!new-xset new 0)))))
      ;; Both A and B are lists. Share list tails since the lists are immutable.
      ;; Let the resulting XSET dynamically become a hash-table if it wants to.
      (let ((xset (!new-xset data (xset-count large))))
        (dolist (elt (xset-data small)) (add-to-xset elt xset))
        ;; if nothing was actually added, then NEW can be GCed
        (if (eq (xset-data xset) data) large xset)))))

(defun xset-subset-p (xset1 xset2)
  (when (<= (xset-count xset1) (xset-count xset2))
    (let ((data (xset-data xset2)))
      (map-xset
       (if (listp data)
           (lambda (elt)
             (unless (member elt data :test #'eql)
               (return-from xset-subset-p nil)))
           (lambda (elt)
             (unless (gethash elt data)
               (return-from xset-subset-p nil))))
       xset1))
    t))

(defun xset= (xset1 xset2)
  (declare (inline subsetp))
  (when (= (xset-count xset1) (xset-count xset2))
    (let ((data1 (xset-data xset1))
          (data2 (xset-data xset2)))
      (if (listp data1)
          (return-from xset= (subsetp data1 data2))
          (maphash
           (lambda (k v)
             (declare (ignore v))
             (unless (gethash k data2)
               (return-from xset= nil)))
           data1)))
    t))

(declaim (inline xset-empty-p))
(defun xset-empty-p (xset)
  (not (xset-data xset)))

(defun xset-every (predicate xset)
  (map-xset (lambda (elt)
              (unless (funcall predicate elt)
                (return-from xset-every nil)))
            xset)
  t)

(defmacro plus-mod-fixnum (a b)
  `(#+sb-xc-host sb-c::plus-mod-fixnum #-sb-xc-host  sb-vm::+-modfx ,a ,b))

;;; Hash an element ELT of an XSET using EQL-HASH. If ELT is a symbol, we _assume_ it to
;;; have a stable hash under EQL. With #+relocatable-static-space the assumption on EQL-HASH
;;; is violated, so instead wire in an arbitrary value.
;;; Note that EQL-HASH does not try to pick off NIL as symbol (and use its hash slot)
;;; because that's just an extra unnecessary step. EQL hash-tables don't care whether NIL's
;;; hash is address-based, because a rehash is forced only if some key actually moves.
(defmacro xset-elt-hash (elt)
  `(multiple-value-bind (hash bool)
       (locally
           #-sb-xc-host (declare (notinline sb-impl::eql-hash)) ; forward ref
           #+relocatable-static-space (if ,elt (sb-impl::eql-hash ,elt) (values #xababababa nil))
           #-relocatable-static-space (sb-impl::eql-hash ,elt))
     (values (truly-the sb-xc:fixnum hash) bool)))

;;; Produce a hash that helps decide whether two xsets could be considered equivalent
;;; as order-insensitive sets comparing elements by EQL.
(defun xset-elts-hash (xset)
  (let* ((c (xset-count xset))
         (h (mix c c)))
    (declare (type sb-xc:fixnum h))
    ;; Use modular addition as the hash mixer since it is commutative and associative,
    ;; and the low bits come out the same no matter the order of operations.
    (let ((hashes (xset-extra xset)))
      (if (simple-vector-p hashes)
          (dovector (x hashes)
            (setq h (plus-mod-fixnum h (truly-the sb-xc:fixnum (if (listp x) (cdr x) x)))))
          ;; If stable hashes were never assigned, then the set contains
          ;; a restricted type of element, which we AVER below
          (map-xset (lambda (x)
                      (aver (typep x '(or symbol number character)))
                      (setq h (plus-mod-fixnum (xset-elt-hash x) h)))
                    xset)))
    ;; Now mix the bits thoroughly and then mask to a positive fixnum.
    ;; I think this does not need to be compatible between host and target.
    ;; But I'm trying to make it compatible anyway because I'm not 100% sure
    ;; what will happen if it isn't. While the hash installed in a CTYPE instance
    ;; is pseudorandom, the hash used for locating in a hashset is content-based.
    ;; However, all hashsets are rebuilt in PRELOAD-CTYPE-HASHSETS without regard
    ;; for what index an element was in during cross-compilation.
    (let ((word-bits
           #+sb-xc-host (ldb (byte sb-vm:n-word-bits 0) (ash h sb-vm:n-fixnum-tag-bits))
           #-sb-xc-host (get-lisp-obj-address h)))
      (murmur-hash-word/+fixnum word-bits))))

;;; Stably-hashed XSETs that have elements which are not nicely EQL-hashable
;;; rely on a global table that maps any object to a pseudorandom hash.
;;; The table keys are refcounted so that they can be deleted when no XSET
;;; references a particular key. Caller MUST provide synchronization.
(define-load-time-global *xset-stable-hashes* (make-hash-table :test 'eq))

(defun xset-generate-stable-hashes (xset &aux (hashmap *xset-stable-hashes*))
  #-sb-xc-host (declare (sb-c::tlab :system))
  ;; Regarding the (NULL X) in the first COND clause below - EQL-HASH pessimistically reports
  ;; that NIL's hash _does_ depend on its address which is technically true but for purposes here
  ;; is not true. This avoids assigning yet another arbitrary hash value to NIL for each MEMBER
  ;; type in which it participates, if the members did not all have stable hashes already.
  ;; Unusual (not-very-useful) MEMBER types with non-EQL-comparable values are quite common.
  ;; Probably the most common case is the derived type of the iteration var in a DOLIST, e.g.
  ;; in contrib/sb-grovel/def-to-lisp the type (MEMBER "char" "short" "long long" "long" "int")
  ;; occurs, being the type of the iteration variable whose name incidentally is TYPE.
  ;; Also the compiler always calls CTYPE-OF on all constants, and often computes unions of those
  ;; types based on control flow, which can lead to similar MEMBER types as shown above.
  (let ((hashes (make-array (xset-count xset)))
        (index -1))
    (dx-flet ((assign-hash (x)
                (multiple-value-bind (h address-based) (xset-elt-hash x)
                  (setf (aref hashes (incf index))
                        (acond ((or (not address-based) (null x))
                                ;; EQL-HASH does not hash fixnums well enough
                                (murmur-hash-word/fixnum h))
                               ((gethash x hashmap)
                                (incf (car it)) ; bump refcount
                                it)
                               (t ; new entry with refcount = 1
                                (setf (gethash x hashmap) (cons 1 (ctype-random)))))))))
      (map-xset #'assign-hash xset)
      (setf (xset-extra xset) hashes)))
  xset)
(defun xset-delete-stable-hashes (xset &aux (hashmap *xset-stable-hashes*))
  (let ((hashes (the simple-vector (xset-extra xset)))
        (index -1))
    ;; Iteration order will be the same as it was in GENERATE-STABLE-HASHES
    (map-xset (lambda (elt &aux (cell (aref hashes (incf index))))
                (when (and (listp cell)
                           (zerop (decf (car cell)))) ; element is not used in any XSET
                  (remhash elt hashmap)))
              xset)))
