;;;; (See the comments at the head of the file compiler-extras.lisp.)

(in-package "SB-IMPL")

(declaim (optimize (speed 3) (space 1)))

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

