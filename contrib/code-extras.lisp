;;;; (See the comments at the head of the file compiler-extras.lisp.)

(in-package "SB-IMPL")

(declaim (optimize (speed 3) (space 1)))

;;; Like CMU CL, we use HEAPSORT. However, instead of trying to
;;; generalize the CMU CL code to allow START and END values, this
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
           (%srt-vector (keyfun &optional (vtype 'vector))
             `(macrolet (;; In SBCL ca. 0.6.10, I had trouble getting
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
		  (/noshow "doing SRT-VECTOR" keyfun)
		  (loop for i of-type index
			from (ash current-heap-size -1) downto 1 do
			(/noshow vector "about to %HEAPIFY" i)
			(%heapify i))
		  (loop 
		   (/noshow current-heap-size vector)
		   (when (< current-heap-size 2)
		     (/noshow "returning")
		     (return))
		   (/noshow "setting" current-heap-size "element to" (%elt 1))
		   (rotatef (%elt 1) (%elt current-heap-size))
		   (decf current-heap-size)
		   (%heapify 1))
		  (/noshow "falling out of %SRT-VECTOR")))))

  (declaim (inline srt-vector))
  (defun srt-vector (vector start end predicate key)
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
	    (%srt-vector #'identity simple-vector)
	    (%srt-vector key simple-vector))
	;; It's hard to imagine many important applications for
	;; sorting vector types other than (VECTOR T), so we just lump
	;; them all together in one slow dynamically typed mess.
	(locally
	  (declare (optimize (speed 2) (space 2) (inhibit-warnings 3)))
	  (error "stub: suppressed to hide notes")
	  #+nil (%srt-vector (or key #'identity))))))

(declaim (maybe-inline sort))
(defun sort (sequence predicate &key key)
  (let ((predicate-function (%coerce-callable-to-function predicate))
	(key-function (and key (%coerce-callable-to-function key))))
    (typecase sequence
      (list (sort-list sequence predicate-function key-function))
      (vector
       (with-array-data ((vector (the vector sequence))
			 (start 0)
			 (end (length sequence)))
         (srt-vector vector start end predicate-function key-function))
       (/noshow "back from SRT-VECTOR" sequence)
       sequence)
      (t
       (error 'simple-type-error
	      :datum sequence
	      :expected-type 'sequence
	      :format-control "~S is not a sequence."
	      :format-arguments (list sequence))))))

(defun vector-push-extend (new-element
			   vector
			   &optional
			   (extension nil extension-p))
  (declare (type vector vector))
  (let ((old-fill-pointer (fill-pointer vector)))
    (declare (type index old-fill-pointer))
    (when (= old-fill-pointer (%array-available-elements vector))
      (adjust-array vector (+ old-fill-pointer
			      (if extension-p
				  (the (integer 1 #.most-positive-fixnum)
				    extension)
				  (1+ old-fill-pointer)))))
    (setf (%array-fill-pointer vector)
	  (1+ old-fill-pointer))
    ;; Wrapping the type test and the AREF in the same WITH-ARRAY-DATA
    ;; saves some time.
    (with-array-data ((v vector) (i old-fill-pointer) (end)
		      :force-inline t)
      (declare (ignore end) (optimize (safety 0)))
      (if (simple-vector-p v) ; if common special case
          (setf (aref v i) new-element)
	  (setf (aref v i) new-element)))
    old-fill-pointer))

;;; FIXME: should DEFUN REPLACE in terms of same expansion as
;;; DEFTRANSFORM
#+nil
(defun replace (..)
  (cond ((and (typep seq1 'simple-vector)
	      (typep seq2 'simple-vector))
	 (%replace-vector-vector ..))
	((and (typep seq1 'simple-string)
	      (typep seq2 'simple-string))
	 (%replace-vector-vector ..))
	(t
	 ..)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; POSITION/FIND stuff

#+sb-xc-host
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; FIXME: Report seq.impure.lisp test failures to cmucl-imp@cons.org.
  ;; FIXME: Add BUGS entry for the way that inline expansions offunctions
  ;; like FIND cause compiler warnings when the system can't prove that
  ;; NIL is never returned; and give (NEED (FIND ..)) workaround.
  (error "need to fix FIXMEs"))
  
;;; logic to unravel :TEST, :TEST-NOT, and :KEY options in FIND,
;;; POSITION-IF, etc.
(declaim (inline effective-find-position-test effective-find-position-key))
(defun effective-find-position-test (test test-not)
  (cond ((and test test-not)
	 (error "can't specify both :TEST and :TEST-NOT"))
	(test (%coerce-callable-to-function test))
	(test-not
	 ;; (Without DYNAMIC-EXTENT, this is potentially horribly
	 ;; inefficient, but since the TEST-NOT option is deprecated
	 ;; anyway, we don't care.)
	 (complement (%coerce-callable-to-function test-not)))
	(t #'eql)))
(defun effective-find-position-key (key)
  (if key
      (%coerce-callable-to-function key)
      #'identity))

;;; shared guts of out-of-line FIND, POSITION, FIND-IF, and POSITION-IF
(macrolet (;; shared logic for defining %FIND-POSITION and
	   ;; %FIND-POSITION-IF in terms of various inlineable cases
	   ;; of the expression defined in FROB and VECTOR*-FROB
	   (frobs ()
	     `(etypecase sequence-arg
		(list (frob sequence-arg from-end))
		(vector 
		 (with-array-data ((sequence sequence-arg :offset-var offset)
				   (start start)
				   (end (or end (length sequence-arg))))
		   (multiple-value-bind (f p)
		       (macrolet ((frob2 () '(if from-end
						 (frob sequence t)
						 (frob sequence nil))))
			 (typecase sequence
			   (simple-vector (frob2))
			   (simple-string (frob2))
			   (t (vector*-frob sequence))))
		     (declare (type (or index null) p))
		     (values f (and p (the index (+ p offset))))))))))
  (defun %find-position (item sequence-arg from-end start end key test)
    (macrolet ((frob (sequence from-end)
		 `(%find-position item ,sequence
				  ,from-end start end key test))
	       (vector*-frob (sequence)
		 `(%find-position-vector-macro item ,sequence
					       from-end start end key test)))
      (frobs)))
  (defun %find-position-if (predicate sequence-arg from-end start end key)
    (macrolet ((frob (sequence from-end)
		 `(%find-position-if predicate ,sequence
				     ,from-end start end key))
	       (vector*-frob (sequence)
		 `(%find-position-if-vector-macro predicate ,sequence
						  from-end start end key)))
      (frobs))))

;;; the user interface to FIND and POSITION: Get all our ducks in a row,
;;; then call %FIND-POSITION
(declaim (inline find position))
(macrolet ((def-find-position (fun-name values-index)
	     `(defun ,fun-name (item
				sequence
				&key
				from-end
				(start 0)
				end
				key
				test
				test-not)
		(nth-value
		 ,values-index
		 (%find-position item
				 sequence
				 from-end
				 start
				 end
				 (effective-find-position-key key)
				 (effective-find-position-test test
							       test-not))))))
  (def-find-position find 0)
  (def-find-position position 1))

;;; the user interface to FIND-IF and POSITION-IF, entirely analogous
;;; to the interface to FIND and POSITION
(declaim (inline find-if position-if))
(macrolet ((def-find-position-if (fun-name values-index)
	     `(defun ,fun-name (predicate sequence
				&key from-end (start 0) end key)
		(nth-value
		 ,values-index
		 (%find-position-if (%coerce-callable-to-function predicate)
				    sequence
				    from-end
				    start
				    end
				    (effective-find-position-key key))))))
  
  (def-find-position-if find-if 0)
  (def-find-position-if position-if 1))

;;; the deprecated functions FIND-IF-NOT and POSITION-IF-NOT
(macrolet ((def-find-position-if-not (fun-name values-index)
	     `(defun ,fun-name (predicate sequence
				&key from-end (start 0) end key)
		(nth-value
		 ,values-index
		 (%find-position-if (complement (%coerce-callable-to-function
						 predicate))
				    sequence
				    from-end
				    start
				    end
				    (effective-find-position-key key))))))
  (def-find-position-if-not find-if-not 0)
  (def-find-position-if-not position-if-not 1))
;;; FIXME: Remove uses of these deprecated functions, and of :TEST-NOT too.

