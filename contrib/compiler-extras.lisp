;;;; The files
;;;;   compiler-extras.lisp
;;;;   code-extras.lisp
;;;; hold things that I (WHN) am working on which are sufficiently
;;;; closely tied to the system that they want to be under the same
;;;; revision control, but which aren't yet ready for prime time.
;;;;
;;;; Unless you like living dangerously, you don't want to be running
;;;; these. But there might be some value to looking at these files to
;;;; see whether I'm working on optimizing something whose performance
;;;; you care about, so that you can patch it, or write test cases for
;;;; it, or pester me to release it, or whatever.
;;;;
;;;; Throughout 0.6.x, these were mostly performance fixes. Fixes for
;;;; logical bugs tend to go straight into the system, but fixes for
;;;; performance problems can easily introduce logical bugs, and no
;;;; one's going to thank me for prematurely replacing old slow
;;;; correct code with new fast code that I haven't yet discovered to
;;;; be wrong.

(in-package "SB-C")

(declaim (optimize (speed 1) (space 2)))

;;; TO DO for DEFTRANSFORM FILL:
;;;   ?? This DEFTRANSFORM, and the old DEFTRANSFORMs, should only
;;;      apply when SPEED > SPACE.
;;;   ?? Add test cases.

#+nil ; not tested yet..
(deftransform replace ((seq1 seq2 &key (start1 0) end1 (start2 0) end2)
		       (vector vector &key
			       (:start1 index) (:end1 (or index null))
			       (:start2 index) (:end2 (or index null)))
		       *
		       ;; This is potentially an awfully big transform
		       ;; (if things like (EQ SEQ1 SEQ2) aren't known
		       ;; at runtime). We need to make it available
		       ;; inline, since otherwise there's no way to do
		       ;; it efficiently on all array types, but it
		       ;; probably doesn't belong inline all the time.
		       :policy (> speed (1+ space)))
  "open code"
  (let ((et1 (upgraded-element-type-specifier-or-give-up seq1))
	(et2 (upgraded-element-type-specifier-or-give-up seq2)))
    `(let* ((n-copied (min (- end1 start1) (- end2 start2)))
	    (effective-end1 (+ start1 n-copied)))
       (if (eq seq1 seq2)
	   (with-array-data ((seq seq1)
			     (start (min start1 start2))
			     (end (max end1 end2)))
	     (declare (type (simple-array ,et1 1) seq))
	     (if (<= start1 start2)
		 (let ((index2 start2))
		   (declare (type index index2))
		   (loop for index1 of-type index
			 from start1 below effective-end1 do
			 (setf (aref seq index1)
			       (aref seq index2))
			 (incf index2)))
		 (let ((index2 (1- end2)))
		   (declare (type (integer -2 #.most-positive-fixnum) index2))
		   (loop for index1 of-type index-or-minus-1
			 from (1- effective-end1) downto start1 do
			 (setf (aref seq index1)
			       (aref seq index2))
			 (decf index2)))))
	   (with-array-data ((seq1 seq1) (start1 start1) (end1 end1))
	     (declare (type (simple-array ,et1 1) seq1))
	     (with-array-data ((seq2 seq2) (start2 start2) (end2 end2))
	       (declare (type (simple-array ,et2 1) seq2))
               (let ((index2 start2))
		 (declare (type index index2))
		 (loop for index1 of-type index
		       from start1 below effective-end1 do
		       (setf (aref seq index1)
			     (aref seq index2))
		       (incf index2))))))
       seq1)))
