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

(defstruct (xset (:constructor alloc-xset)
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
    (if (listp data)
        (let ((size (xset-extra xset)))
          (if (< size +xset-list-size-limit+)
              (unless (member elt data :test #'eql)
                (setf (xset-extra xset) (1+ size)
                      (xset-data xset) (cons elt data)))
              (let ((table (make-hash-table :size (* 2 size) :test #'eql)))
                (setf (gethash elt table) t)
                (dolist (x data)
                  (setf (gethash x table) t))
                (setf (xset-extra xset) 0 ; looks nice to clear it
                      (xset-data xset) table))))
        (setf (gethash elt data) t))))

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
        (declare (index missing))
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

#-sb-xc-host (defmacro plus-mod-fixnum (a b) `(sb-vm::+-modfx ,a ,b))

;;; Every architecture needs this portable replacement for +-modfx.
;;; Some, but not all, define +-modfx in cross-modular.
;;; We need this available sooner than that because type-classes needs it
;;; to compute a hash of a list of things order-insensitively.
#+sb-xc-host
(progn
(defun plus-mod-fixnum (a b)
  (declare (type sb-xc:fixnum a b))
  (let* ((mask (ldb (byte sb-vm:n-fixnum-bits 0) -1))
         (result (logand (+ (logand a mask) (logand b mask)) mask)))
    (if (logbitp sb-vm:n-positive-fixnum-bits result) ; then it's negative
        (dpb result (byte sb-vm:n-fixnum-bits 0) -1)
        result)))

;; Assume 2 fixnum tag bits:
;;      significant bits      tag
;;   #b01111...11111111111111  00
;; + #b0                    1  00
;;   ------------------------
;; = #b10000...00000000000000  00
(assert (= (plus-mod-fixnum sb-xc:most-positive-fixnum 1)
           sb-xc:most-negative-fixnum))
;; etc
(assert (= (plus-mod-fixnum sb-xc:most-negative-fixnum sb-xc:most-negative-fixnum)
           0))
(assert (= (plus-mod-fixnum -1 most-negative-fixnum)
           sb-xc:most-positive-fixnum))
)

;;; Produce a hash that helps decide whether two xsets could be considered equivalent
;;; as order-insensitive sets comparing elements by EQL. This shouldn't use EQL-HASH
;;; because the intent is that it be useful for both host and target. SXHASH is fine
;;; for SYMBOL, NUMBER, and CHARACTER since EQL and EQUAL are the the same
;;; (SXHASH being the hash function for EQUAL). The target can include STRUCTURE-OBJECT
;;; because we have stable hashes that do not depend on the slots. But it's no good
;;; to mix in STRING, BIT-VECTOR, CONS, or other type where SXHASH reads the contents.
;;; Use modular addition since it is commutative and associative, and the low bits
;;; come out the same no matter the order of operations.
(defun xset-elts-hash (xset)
  (let* ((c (xset-count xset))
         (h (mix c c)))
    (declare (sb-xc:fixnum h))
    ;; Rather than masking each intermediate result to MOST-POSITIVE-FIXNUM,
    ;; allow bits to rollover into the sign bit
    (let ((hashes (xset-extra xset)))
      (if (simple-vector-p hashes)
          (dovector (x hashes)
            (setq h (plus-mod-fixnum h (truly-the sb-xc:fixnum (if (listp x) (cdr x) x)))))
          (flet ((elt-hash (e)
                   ;; SYMBOL-HASH is better than SXHASH for symbols (because SXHASH collides
                   ;; on STRING= symbols - though SYMBOL-HASH does too at the moment).
                   ;; and SXHASH is better than EQL-HASH for (OR CHARACTER NUMBER)
                   ;; though either is technically an acceptable hash function on those types.
                   #-sb-xc-host (if (symbolp e) (symbol-hash e) (sb-xc:sxhash e))
                   #+sb-xc-host (sb-xc:sxhash e)))
            ;; If stable hashes were never assigned, then the set must contain
            ;; only these object types. There are some other objects (INSTANCE e.g.) that
            ;; could have non-address-based EQL-hashes but they don't really appear
            ;; in MEMBER types often enough to matter. (And in fact we'd have to use
            ;; something other than EQL-hash)
            (map-xset (lambda (x)
                        (aver (typep x '(or symbol number character)))
                        (setq h (plus-mod-fixnum (elt-hash x) h)))
                      xset))))
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
  #-sb-xc-host (declare (notinline sb-impl::eql-hash) ; forward ref
                        (sb-c::tlab :system))
  (flet ((get-stable-hash-cell (obj)
           (let ((cell (gethash obj hashmap)))
             (cond (cell
                    (incf (car cell))
                    cell)
                   (t
                    (setf (gethash obj hashmap) (cons 1 (ctype-random))))))))
    (let ((hashes (make-array (xset-count xset)))
          (index -1))
      (map-xset (lambda (elt)
                  (setf (aref hashes (incf index))
                        #-sb-xc-host
                        (multiple-value-bind (hashval addr) (sb-impl::eql-hash elt)
                          ;; EQL-HASH does _nothing_ to fixnums. Hash-tables perturb the hash
                          ;; in PREFUZZ-HASH, so too should we mix bits some more here.
                          (if addr (get-stable-hash-cell elt) (murmur-hash-word/fixnum hashval)))
                        #+sb-xc-host
                        (multiple-value-bind (hashval addr)
                            (if (sb-xc:typep elt '(or symbol character number))
                                (values (sb-xc:sxhash elt) nil)
                                (values 4 ; chosen by algorithm of https://xkcd.com/221/
                                        t)) ; yes, it's address-based
                          (if addr (get-stable-hash-cell elt) hashval))))
                xset)
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
