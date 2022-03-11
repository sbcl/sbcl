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

(in-package "SB-KERNEL")

(defstruct (xset (:constructor alloc-xset) (:copier nil) (:predicate nil))
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
    (multiple-value-bind (source lookup)
        (if (< (xset-list-size a) (xset-list-size b))
            (values b a)
            (values a b))
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

(declaim (inline xset-empty-p))
(defun xset-empty-p (xset)
  (not (xset-data xset)))
