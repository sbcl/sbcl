;;;; This file implements a sparse set abstraction, represented as a
;;;; sorted linked list. We don't use bit-vectors to represent sets in
;;;; flow analysis, since the universe may be quite large but the
;;;; average number of elements is small. We keep the list sorted so
;;;; that we can do union and intersection in linear time.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;; Each structure that may be placed in a SSET must include the
;;; SSET-ELEMENT structure. We allow an initial value of NIL to mean
;;; that no ordering has been assigned yet (although an ordering must
;;; be assigned before doing set operations.)
(defstruct (sset-element (:constructor nil)
			 (:copier nil))
  (number nil :type (or index null)))

(defstruct (sset (:copier nil))
  ;; The element at the head of the list here seems always to be
  ;; ignored. I think this idea is that the extra level of indirection
  ;; it provides is handy to allow various destructive operations on
  ;; SSETs to be expressed more easily. -- WHN
  (elements (list nil) :type cons))
(defprinter (sset)
  (elements :prin1 (cdr elements)))

;;; Iterate over the elements in SSET, binding VAR to each element in
;;; turn.
(defmacro do-sset-elements ((var sset &optional result) &body body)
  `(dolist (,var (cdr (sset-elements ,sset)) ,result) ,@body))

;;; Destructively add ELEMENT to SET. If ELEMENT was not in the set,
;;; then we return true, otherwise we return false.
(declaim (ftype (function (sset-element sset) boolean) sset-adjoin))
(defun sset-adjoin (element set)
  (let ((number (sset-element-number element))
	(elements (sset-elements set)))
    (do ((prev elements current)
	 (current (cdr elements) (cdr current)))
	((null current)
	 (setf (cdr prev) (list element))
	 t)
      (let ((el (car current)))
	(when (>= (sset-element-number el) number)
	  (when (eq el element)
	    (return nil))
	  (setf (cdr prev) (cons element current))
	  (return t))))))

;;; Destructively remove ELEMENT from SET. If element was in the set,
;;; then return true, otherwise return false.
(declaim (ftype (function (sset-element sset) boolean) sset-delete))
(defun sset-delete (element set)
  (let ((elements (sset-elements set)))
    (do ((prev elements current)
	 (current (cdr elements) (cdr current)))
	((null current) nil)
      (when (eq (car current) element)
	(setf (cdr prev) (cdr current))
	(return t)))))

;;; Return true if ELEMENT is in SET, false otherwise.
(declaim (ftype (function (sset-element sset) boolean) sset-member))
(defun sset-member (element set)
  (declare (inline member))
  (not (null (member element (cdr (sset-elements set)) :test #'eq))))

;;; Return true if SET contains no elements, false otherwise.
(declaim (ftype (function (sset) boolean) sset-empty))
(defun sset-empty (set)
  (null (cdr (sset-elements set))))

;;; Return a new copy of SET.
(declaim (ftype (function (sset) sset) copy-sset))
(defun copy-sset (set)
  (make-sset :elements (copy-list (sset-elements set))))

;;; Perform the appropriate set operation on SET1 and SET2 by
;;; destructively modifying SET1. We return true if SET1 was modified,
;;; false otherwise.
(declaim (ftype (function (sset sset) boolean) sset-union sset-intersection
		sset-difference))
(defun sset-union (set1 set2)
  (let* ((prev-el1 (sset-elements set1))
	 (el1 (cdr prev-el1))
	 (changed nil))
    (do ((el2 (cdr (sset-elements set2)) (cdr el2)))
	((null el2) changed)
      (let* ((e (car el2))
	     (num2 (sset-element-number e)))
	(loop
	  (when (null el1)
	    (setf (cdr prev-el1) (copy-list el2))
	    (return-from sset-union t))
	  (let ((num1 (sset-element-number (car el1))))
	    (when (>= num1 num2)
	      (if (> num1 num2)
		  (let ((new (cons e el1)))
		    (setf (cdr prev-el1) new)
		    (setq prev-el1 new
			  changed t))
		  (shiftf prev-el1 el1 (cdr el1)))
	      (return))
	    (shiftf prev-el1 el1 (cdr el1))))))))
(defun sset-intersection (set1 set2)
  (let* ((prev-el1 (sset-elements set1))
	 (el1 (cdr prev-el1))
	 (changed nil))
    (do ((el2 (cdr (sset-elements set2)) (cdr el2)))
	((null el2)
	 (cond (el1
		(setf (cdr prev-el1) nil)
		t)
	       (t changed)))
      (let ((num2 (sset-element-number (car el2))))
	(loop
	  (when (null el1)
	    (return-from sset-intersection changed))
	  (let ((num1 (sset-element-number (car el1))))
	    (when (>= num1 num2)
	      (when (= num1 num2)
		(shiftf prev-el1 el1 (cdr el1)))
	      (return))
	    (pop el1)
	    (setf (cdr prev-el1) el1)
	    (setq changed t)))))))
(defun sset-difference (set1 set2)
  (let* ((prev-el1 (sset-elements set1))
	 (el1 (cdr prev-el1))
	 (changed nil))
    (do ((el2 (cdr (sset-elements set2)) (cdr el2)))
	((null el2) changed)
      (let ((num2 (sset-element-number (car el2))))
	(loop
	  (when (null el1)
	    (return-from sset-difference changed))
	  (let ((num1 (sset-element-number (car el1))))
	    (when (>= num1 num2)
	      (when (= num1 num2)
		(pop el1)
		(setf (cdr prev-el1) el1)
		(setq changed t))
	      (return))
	    (shiftf prev-el1 el1 (cdr el1))))))))

;;; Destructively modify SET1 to include its union with the difference
;;; of SET2 and SET3. We return true if Set1 was modified, false
;;; otherwise.
(declaim (ftype (function (sset sset sset) boolean) sset-union-of-difference))
(defun sset-union-of-difference (set1 set2 set3)
  (let* ((prev-el1 (sset-elements set1))
	 (el1 (cdr prev-el1))
	 (el3 (cdr (sset-elements set3)))
	 (changed nil))
    (do ((el2 (cdr (sset-elements set2)) (cdr el2)))
	((null el2) changed)
      (let* ((e (car el2))
	     (num2 (sset-element-number e)))
	(loop
	  (when (null el3)
	    (loop
	      (when (null el1)
		(setf (cdr prev-el1) (copy-list el2))
		(return-from sset-union-of-difference t))
	      (let ((num1 (sset-element-number (car el1))))
		(when (>= num1 num2)
		  (if (> num1 num2)
		      (let ((new (cons e el1)))
			(setf (cdr prev-el1) new)
			(setq prev-el1 new  changed t))
		      (shiftf prev-el1 el1 (cdr el1)))
		  (return))
		(shiftf prev-el1 el1 (cdr el1))))
	    (return))
	  (let ((num3 (sset-element-number (car el3))))
	    (when (<= num2 num3)
	      (unless (= num2 num3)
		(loop
		  (when (null el1)
		    (do ((el2 el2 (cdr el2)))
			((null el2)
			 (return-from sset-union-of-difference changed))
		      (let* ((e (car el2))
			     (num2 (sset-element-number e)))
			(loop
			  (when (null el3)
			    (setf (cdr prev-el1) (copy-list el2))
			    (return-from sset-union-of-difference t))
			  (setq num3 (sset-element-number (car el3)))
			  (when (<= num2 num3)
			    (unless (= num2 num3)
			      (let ((new (cons e el1)))
				(setf (cdr prev-el1) new)
				(setq prev-el1 new  changed t)))
			    (return))
			  (pop el3)))))
		  (let ((num1 (sset-element-number (car el1))))
		    (when (>= num1 num2)
		      (if (> num1 num2)
			  (let ((new (cons e el1)))
			    (setf (cdr prev-el1) new)
			    (setq prev-el1 new  changed t))
			  (shiftf prev-el1 el1 (cdr el1)))
		      (return))
		    (shiftf prev-el1 el1 (cdr el1)))))
	      (return)))
	  (pop el3))))))
