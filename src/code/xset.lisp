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
;;;; XSET-LIST-SIZE is true only for XSETs whose data is stored into a
;;;; list -- XSET-COUNT returns the real value.
;;;;
;;;; Note: XSET always uses EQL as the equivalence test

(in-package "SB-KERNEL")

(defstruct (xset (:constructor alloc-xset)
                 (:constructor !copy-xset (list-size data))
                 (:copier nil)
                 (:predicate nil))
  (list-size 0 :type index)
  (data nil :type (or list hash-table)))
(declaim (freeze-type xset))

(defun xset-count (xset)
  (let ((data (xset-data xset)))
    (if (listp data)
        (xset-list-size xset)
        (hash-table-count data))))

(defun map-xset (function xset)
  (declare (function function))
  #-sb-xc-host (declare (dynamic-extent function)) ; Avoid "unable" in host
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
  (let ((data (xset-data xset))
        (size (xset-list-size xset)))
    (if (listp data)
        (if (< size +xset-list-size-limit+)
            (unless (member elt data :test #'eql)
              (setf (xset-list-size xset) (1+ size)
                    (xset-data xset) (cons elt data)))
            (let ((table (make-hash-table :size (* 2 size) :test #'eql)))
              (setf (gethash elt table) t)
              (dolist (x data)
                (setf (gethash x table) t))
              (setf (xset-data xset) table)))
        (setf (gethash elt data) t))))

;; items must be canonical - no duplicates - and few in number.
(defun xset-from-list (items)
  (let ((n (length items)))
    (aver (<= n +xset-list-size-limit+))
    (let ((xset (alloc-xset)))
      (setf (xset-list-size xset) n (xset-data xset) items)
      xset)))

(defun xset-union (a b)
  (let ((xset (alloc-xset)))
    (map-xset (lambda (x)
                (add-to-xset x xset))
              a)
    (map-xset (lambda (y)
                (add-to-xset y xset))
              b)
    xset))

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

(defun xset-intersection (a b)
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
  (let ((h 0))
    (declare (fixnum h))
    (map-xset (lambda (x)
                ;; Rather than masking each intermediate result to MOST-POSITIVE-FIXNUM,
                ;; allow bits to rollover into the sign bit
                (when (typep x '(or symbol number character #-sb-xc-host instance))
                  (setq h (plus-mod-fixnum (sb-xc:sxhash x) h))))
              xset)
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
      (logand (sb-impl::murmur3-fmix-word word-bits) most-positive-fixnum))))
