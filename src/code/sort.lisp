;;;; SORT and friends

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;; Like CMU CL, we use HEAPSORT. However, other than that, this code
;;; isn't really related to the CMU CL code, since instead of trying
;;; to generalize the CMU CL code to allow START and END values, this
;;; code has been written from scratch following Chapter 7 of
;;; _Introduction to Algorithms_ by Corman, Rivest, and Shamir.
(macrolet ((%index (x) `(truly-the index ,x))
	   (%parent (i) `(ash ,i -1))
           (%left (i) `(%index (ash ,i 1)))
           (%right (i) `(%index (1+ (ash ,i 1))))
           (%heapify (i)
	     `(do* ((i ,i)
		    (left (%left i) (%left i)))
		  ((> left current-heap-size))
		(declare (type index i left))
		(let* ((i-elt (%elt i))
		       (i-key (funcall keyfun i-elt))
		       (left-elt (%elt left))
		       (left-key (funcall keyfun left-elt)))
		  (multiple-value-bind (large large-elt large-key)
		      (if (funcall predicate i-key left-key)
			  (values left left-elt left-key)
			  (values i i-elt i-key))
		    (let ((right (%right i)))
		      (multiple-value-bind (largest largest-elt)
			  (if (> right current-heap-size)
			      (values large large-elt)
			      (let* ((right-elt (%elt right))
				     (right-key (funcall keyfun right-elt)))
				(if (funcall predicate large-key right-key)
				    (values right right-elt)
				    (values large large-elt))))
			(cond ((= largest i)
			       (return))
			      (t
			       (setf (%elt i) largest-elt
				     (%elt largest) i-elt
				     i largest)))))))))
           (%sort-vector (keyfun &optional (vtype 'vector))
             `(macrolet (;; KLUDGE: In SBCL ca. 0.6.10, I had trouble getting
			 ;; type inference to propagate all the way
			 ;; through this tangled mess of inlining. The
			 ;; TRULY-THE here works around that. -- WHN
			 (%elt (i)
			   `(aref (truly-the ,',vtype vector)
				  (%index (+ (%index ,i) start-1)))))
		(let ((start-1 (1- start)) ; Heaps prefer 1-based addressing.
		      (current-heap-size (- end start))
		      (keyfun ,keyfun))
		  (declare (type (integer -1 #.(1- most-positive-fixnum))
				 start-1))
		  (declare (type index current-heap-size))
		  (declare (type function keyfun))
		  (loop for i of-type index
			from (ash current-heap-size -1) downto 1 do
			(%heapify i))
		  (loop 
		   (when (< current-heap-size 2)
		     (return))
		   (rotatef (%elt 1) (%elt current-heap-size))
		   (decf current-heap-size)
		   (%heapify 1))))))

  (declaim (inline sort-vector))
  (defun sort-vector (vector start end predicate key)
    (declare (type vector vector))
    (declare (type index start end))
    (declare (type function predicate))
    (declare (type (or function null) key))
    (declare (optimize (speed 3) (safety 3) (debug 1) (space 1)))
    (if (typep vector 'simple-vector)
	;; (VECTOR T) is worth optimizing for, and SIMPLE-VECTOR is
	;; what we get from (VECTOR T) inside WITH-ARRAY-DATA.
	(if (null key)
	    ;; Special-casing the KEY=NIL case lets us avoid some
	    ;; function calls.
	    (%sort-vector #'identity simple-vector)
	    (%sort-vector key simple-vector))
	;; It's hard to anticipate many speed-critical applications for
	;; sorting vector types other than (VECTOR T), so we just lump
	;; them all together in one slow dynamically typed mess.
	(locally
	  (declare (optimize (speed 2) (space 2) (inhibit-warnings 3)))
	  (%sort-vector (or key #'identity))))))

;;; This is MAYBE-INLINE because it's not too hard to have an
;;; application where sorting is a major bottleneck, and inlining it
;;; allows the compiler to make enough optimizations that it might be
;;; worth the (large) cost in space.
(declaim (maybe-inline sort))
(defun sort (sequence predicate &key key)
  #!+sb-doc
  "Destructively sort SEQUENCE. PREDICATE should return non-NIL if
   ARG1 is to precede ARG2."
  (let ((predicate-function (%coerce-callable-to-function predicate))
	(key-function (and key (%coerce-callable-to-function key))))
    (typecase sequence
      (list (sort-list sequence predicate-function key-function))
      (vector
       (with-array-data ((vector (the vector sequence))
			 (start 0)
			 (end (length sequence)))
         (sort-vector vector start end predicate-function key-function))
       sequence)
      (t
       (error 'simple-type-error
	      :datum sequence
	      :expected-type 'sequence
	      :format-control "~S is not a sequence."
	      :format-arguments (list sequence))))))

;;;; stable sorting

(defun stable-sort (sequence predicate &key key)
  #!+sb-doc
  "Destructively sorts sequence. Predicate should return non-Nil if
   Arg1 is to precede Arg2."
  (typecase sequence
    (simple-vector
     (stable-sort-simple-vector sequence predicate key))
    (list
     (sort-list sequence predicate key))
    (vector
     (stable-sort-vector sequence predicate key))
    (t
     (error 'simple-type-error
	    :datum sequence
	    :expected-type 'sequence
	    :format-control "~S is not a sequence."
	    :format-arguments (list sequence)))))

;;; stable sort of lists

;;; SORT-LIST uses a bottom up merge sort. First a pass is made over
;;; the list grabbing one element at a time and merging it with the
;;; next one form pairs of sorted elements. Then n is doubled, and
;;; elements are taken in runs of two, merging one run with the next
;;; to form quadruples of sorted elements. This continues until n is
;;; large enough that the inner loop only runs for one iteration; that
;;; is, there are only two runs that can be merged, the first run
;;; starting at the beginning of the list, and the second being the
;;; remaining elements.

(defun sort-list (list pred key)
  (let ((head (cons :header list))  ; head holds on to everything
	(n 1)		            ; bottom-up size of lists to be merged
	unsorted		    ; unsorted is the remaining list to be
				    ;   broken into n size lists and merged
	list-1			    ; list-1 is one length n list to be merged
	last)			    ; last points to the last visited cell
    (declare (fixnum n))
    (loop
     ;; start collecting runs of n at the first element
     (setf unsorted (cdr head))
     ;; tack on the first merge of two n-runs to the head holder
     (setf last head)
     (let ((n-1 (1- n)))
       (declare (fixnum n-1))
       (loop
	(setf list-1 unsorted)
	(let ((temp (nthcdr n-1 list-1))
	      list-2)
	  (cond (temp
		 ;; there are enough elements for a second run
		 (setf list-2 (cdr temp))
		 (setf (cdr temp) nil)
		 (setf temp (nthcdr n-1 list-2))
		 (cond (temp
			(setf unsorted (cdr temp))
			(setf (cdr temp) nil))
		       ;; the second run goes off the end of the list
		       (t (setf unsorted nil)))
		 (multiple-value-bind (merged-head merged-last)
		     (merge-lists* list-1 list-2 pred key)
		   (setf (cdr last) merged-head)
		   (setf last merged-last))
		 (if (null unsorted) (return)))
		;; if there is only one run, then tack it on to the end
		(t (setf (cdr last) list-1)
		   (return)))))
       (setf n (ash n 1)) ; (+ n n)
       ;; If the inner loop only executed once, then there were only
       ;; enough elements for two runs given n, so all the elements
       ;; have been merged into one list. This may waste one outer
       ;; iteration to realize.
       (if (eq list-1 (cdr head))
	   (return list-1))))))

;;; APPLY-PRED saves us a function call sometimes.
(eval-when (:compile-toplevel :execute)
  (sb!xc:defmacro apply-pred (one two pred key)
    `(if ,key
	 (funcall ,pred (funcall ,key ,one)
		  (funcall ,key  ,two))
	 (funcall ,pred ,one ,two)))
) ; EVAL-WHEN

(defvar *merge-lists-header* (list :header))

;;; MERGE-LISTS*   originally written by Jim Large.
;;; 		   modified to return a pointer to the end of the result
;;; 		      and to not cons header each time its called.
;;; It destructively merges list-1 with list-2. In the resulting
;;; list, elements of list-2 are guaranteed to come after equal elements
;;; of list-1.
(defun merge-lists* (list-1 list-2 pred key)
  (do* ((result *merge-lists-header*)
	(P result))		     ; points to last cell of result
       ((or (null list-1) (null list-2)) ; done when either list used up
	(if (null list-1)	       ; in which case, append the
	    (rplacd p list-2)	   ;   other list
	    (rplacd p list-1))
	(do ((drag p lead)
	     (lead (cdr p) (cdr lead)))
	    ((null lead)
	     (values (prog1 (cdr result) ; Return the result sans header
			    (rplacd result nil)) ; (free memory, be careful)
		     drag))))	   ;   and return pointer to last element.
    (cond ((apply-pred (car list-2) (car list-1) pred key)
	   (rplacd p list-2)	   ; Append the lesser list to last cell of
	   (setq p (cdr p))	    ;   result. Note: test must bo done for
	   (pop list-2))	       ;   LIST-2 < LIST-1 so merge will be
	  (T (rplacd p list-1)	 ;   stable for LIST-1.
	     (setq p (cdr p))
	     (pop list-1)))))

;;; stable sort of vectors

;;; Stable sorting vectors is done with the same algorithm used for
;;; lists, using a temporary vector to merge back and forth between it
;;; and the given vector to sort.

(eval-when (:compile-toplevel :execute)

;;; STABLE-SORT-MERGE-VECTORS* takes a source vector with subsequences,
;;;    start-1 (inclusive) ... end-1 (exclusive) and
;;;    end-1 (inclusive) ... end-2 (exclusive),
;;; and merges them into a target vector starting at index start-1.

(sb!xc:defmacro stable-sort-merge-vectors* (source target start-1 end-1 end-2
						     pred key source-ref
						     target-ref)
  (let ((i (gensym))
	(j (gensym))
	(target-i (gensym)))
    `(let ((,i ,start-1)
	   (,j ,end-1) ; start-2
	   (,target-i ,start-1))
       (declare (fixnum ,i ,j ,target-i))
       (loop
	(cond ((= ,i ,end-1)
	       (loop (if (= ,j ,end-2) (return))
		     (setf (,target-ref ,target ,target-i)
			   (,source-ref ,source ,j))
		     (incf ,target-i)
		     (incf ,j))
	       (return))
	      ((= ,j ,end-2)
	       (loop (if (= ,i ,end-1) (return))
		     (setf (,target-ref ,target ,target-i)
			   (,source-ref ,source ,i))
		     (incf ,target-i)
		     (incf ,i))
	       (return))
	      ((apply-pred (,source-ref ,source ,j)
			   (,source-ref ,source ,i)
			   ,pred ,key)
	       (setf (,target-ref ,target ,target-i)
		     (,source-ref ,source ,j))
	       (incf ,j))
	      (t (setf (,target-ref ,target ,target-i)
		       (,source-ref ,source ,i))
		 (incf ,i)))
	(incf ,target-i)))))

;;; VECTOR-MERGE-SORT is the same algorithm used to stable sort lists,
;;; but it uses a temporary vector. DIRECTION determines whether we
;;; are merging into the temporary (T) or back into the given vector
;;; (NIL).
(sb!xc:defmacro vector-merge-sort (vector pred key vector-ref)
  (let ((vector-len (gensym)) (n (gensym))
	(direction (gensym))  (unsorted (gensym))
	(start-1 (gensym))    (end-1 (gensym))
	(end-2 (gensym))      (temp-len (gensym))
	(i (gensym)))
    `(let ((,vector-len (length (the vector ,vector)))
	   (,n 1)	 ; bottom-up size of contiguous runs to be merged
	   (,direction t) ; t vector --> temp    nil temp --> vector
	   (,temp-len (length (the simple-vector *merge-sort-temp-vector*)))
	   (,unsorted 0)  ; unsorted..vector-len are the elements that need
			  ; to be merged for a given n
	   (,start-1 0))  ; one n-len subsequence to be merged with the next
       (declare (fixnum ,vector-len ,n ,temp-len ,unsorted ,start-1))
       (if (> ,vector-len ,temp-len)
	   (setf *merge-sort-temp-vector*
		 (make-array (max ,vector-len (+ ,temp-len ,temp-len)))))
       (loop
	;; for each n, we start taking n-runs from the start of the vector
	(setf ,unsorted 0)
	(loop
	 (setf ,start-1 ,unsorted)
	 (let ((,end-1 (+ ,start-1 ,n)))
	   (declare (fixnum ,end-1))
	   (cond ((< ,end-1 ,vector-len)
		  ;; there are enough elements for a second run
		  (let ((,end-2 (+ ,end-1 ,n)))
		    (declare (fixnum ,end-2))
		    (if (> ,end-2 ,vector-len) (setf ,end-2 ,vector-len))
		    (setf ,unsorted ,end-2)
		    (if ,direction
			(stable-sort-merge-vectors*
			 ,vector *merge-sort-temp-vector*
			 ,start-1 ,end-1 ,end-2 ,pred ,key ,vector-ref svref)
			(stable-sort-merge-vectors*
			 *merge-sort-temp-vector* ,vector
			 ,start-1 ,end-1 ,end-2 ,pred ,key svref ,vector-ref))
		    (if (= ,unsorted ,vector-len) (return))))
		 ;; if there is only one run, copy those elements to the end
		 (t (if ,direction
			(do ((,i ,start-1 (1+ ,i)))
			    ((= ,i ,vector-len))
			  (declare (fixnum ,i))
			  (setf (svref *merge-sort-temp-vector* ,i)
				(,vector-ref ,vector ,i)))
			(do ((,i ,start-1 (1+ ,i)))
			    ((= ,i ,vector-len))
			  (declare (fixnum ,i))
			  (setf (,vector-ref ,vector ,i)
				(svref *merge-sort-temp-vector* ,i))))
		    (return)))))
	;; If the inner loop only executed once, then there were only enough
	;; elements for two subsequences given n, so all the elements have
	;; been merged into one list. Start-1 will have remained 0 upon exit.
	(when (zerop ,start-1)
	  (if ,direction
	      ;; if we just merged into the temporary, copy it all back
	      ;; to the given vector.
	      (dotimes (,i ,vector-len)
		(setf (,vector-ref ,vector ,i)
		      (svref *merge-sort-temp-vector* ,i))))
	  (return ,vector))
	(setf ,n (ash ,n 1)) ; (* 2 n)
	(setf ,direction (not ,direction))))))

) ; EVAL-when

;;; temporary vector for stable sorting vectors
(defvar *merge-sort-temp-vector*
  (make-array 50))

(declaim (simple-vector *merge-sort-temp-vector*))

(defun stable-sort-simple-vector (vector pred key)
  (declare (simple-vector vector))
  (vector-merge-sort vector pred key svref))

(defun stable-sort-vector (vector pred key)
  (vector-merge-sort vector pred key aref))

;;;; merging

(eval-when (:compile-toplevel :execute)

;;; MERGE-VECTORS returns a new vector which contains an interleaving
;;; of the elements of VECTOR-1 and VECTOR-2. Elements from VECTOR-2
;;; are chosen only if they are strictly less than elements of
;;; VECTOR-1, (PRED ELT-2 ELT-1), as specified in the manual.
(sb!xc:defmacro merge-vectors (vector-1 length-1 vector-2 length-2
			       result-vector pred key access)
  (let ((result-i (gensym))
	(i (gensym))
	(j (gensym)))
    `(let* ((,result-i 0)
	    (,i 0)
	    (,j 0))
       (declare (fixnum ,result-i ,i ,j))
       (loop
	(cond ((= ,i ,length-1)
	       (loop (if (= ,j ,length-2) (return))
		     (setf (,access ,result-vector ,result-i)
			   (,access ,vector-2 ,j))
		     (incf ,result-i)
		     (incf ,j))
	       (return ,result-vector))
	      ((= ,j ,length-2)
	       (loop (if (= ,i ,length-1) (return))
		     (setf (,access ,result-vector ,result-i)
			   (,access ,vector-1 ,i))
		     (incf ,result-i)
		     (incf ,i))
	       (return ,result-vector))
	      ((apply-pred (,access ,vector-2 ,j) (,access ,vector-1 ,i)
			   ,pred ,key)
	       (setf (,access ,result-vector ,result-i)
		     (,access ,vector-2 ,j))
	       (incf ,j))
	      (t (setf (,access ,result-vector ,result-i)
		       (,access ,vector-1 ,i))
		 (incf ,i)))
	(incf ,result-i)))))

) ; EVAL-WHEN

(defun merge (result-type sequence1 sequence2 predicate &key key)
  #!+sb-doc
  "Merge the sequences SEQUENCE1 and SEQUENCE2 destructively into a
   sequence of type RESULT-TYPE using PREDICATE to order the elements."
  (if (eq result-type 'list)
      (let ((result (merge-lists* (coerce sequence1 'list)
				  (coerce sequence2 'list)
				  predicate key)))
	result)
      (let* ((vector-1 (coerce sequence1 'vector))
	     (vector-2 (coerce sequence2 'vector))
	     (length-1 (length vector-1))
	     (length-2 (length vector-2))
	     (result (make-sequence-of-type result-type
					    (+ length-1 length-2))))
	(declare (vector vector-1 vector-2)
		 (fixnum length-1 length-2))

	#!+high-security (aver (typep result result-type))
	(if (and (simple-vector-p result)
		 (simple-vector-p vector-1)
		 (simple-vector-p vector-2))
	    (merge-vectors vector-1 length-1 vector-2 length-2
			   result predicate key svref)
	    (merge-vectors vector-1 length-1 vector-2 length-2
			   result predicate key aref)))))
