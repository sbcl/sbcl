;;;; The files
;;;;   compiler-extras.lisp
;;;;   code-extras.lisp
;;;; hold things that I (WHN) am working on which are sufficiently
;;;; closely tied to the system that they want to be under the same
;;;; revision control, but which aren't yet ready for prime time.
;;;;
;;;; As of around sbcl-0.6.10, these are mostly performance fixes.
;;;; Fixes for logical bugs tend to go straight into the system, but
;;;; fixes for performance problems can easily introduce logical bugs,
;;;; and no one's going to thank me for replacing old slow correct
;;;; code with new fast wrong code.
;;;;
;;;; Unless you want to live *very* dangerously, you don't want to be
;;;; running these. There might be some small value to looking at
;;;; these files to see whether I'm working on optimizing something
;;;; whose performance you care about, so that you can patch it, or
;;;; write test cases for it, or pester me to release it, or whatever.

(in-package "SB-KERNEL")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(%with-array-data-macro
	    index-or-minus-1
            %find-position %find-position-vector-macro
	    %find-position-if %find-position-if-vector-macro)))

(in-package "SB-C")

(deftype index-or-minus-1 () `(integer -1 ,(1- most-positive-fixnum)))

(declaim (optimize (speed 1) (space 2)))

;;; This checks to see whether the array is simple and the start and
;;; end are in bounds. If so, it proceeds with those values.
;;; Otherwise, it calls %WITH-ARRAY-DATA. Note that %WITH-ARRAY-DATA
;;; may be further optimized.
;;;
;;; Given any ARRAY, bind DATA-VAR to the array's data vector and
;;; START-VAR and END-VAR to the start and end of the designated
;;; portion of the data vector. SVALUE and EVALUE are any start and
;;; end specified to the original operation, and are factored into the
;;; bindings of START-VAR and END-VAR. OFFSET-VAR is the cumulative
;;; offset of all displacements encountered, and does not include
;;; SVALUE.
;;;
;;; When FORCE-INLINE is set, the underlying %WITH-ARRAY-DATA form is
;;; forced to be inline, overriding the ordinary judgment of the
;;; %WITH-ARRAY-DATA DEFTRANSFORMs. Ordinarily the DEFTRANSFORMs are
;;; fairly picky about their arguments, figuring that if you haven't
;;; bothered to get all your ducks in a row, you probably don't care
;;; that much about speed anyway! But in some cases it makes sense to
;;; do type testing inside %WITH-ARRAY-DATA instead of outside, and
;;; the DEFTRANSFORM can't tell that that's going on, so it can make
;;; sense to use FORCE-INLINE option in that case.
(defmacro with-array-data (((data-var array &key offset-var)
			    (start-var &optional (svalue 0))
			    (end-var &optional (evalue nil))
			    &key force-inline)
			   &body forms)
  (once-only ((n-array array)
	      (n-svalue `(the index ,svalue))
	      (n-evalue `(the (or index null) ,evalue)))
    `(multiple-value-bind (,data-var
			   ,start-var
			   ,end-var
			   ,@(when offset-var `(,offset-var)))
	 (if (not (array-header-p ,n-array))
	     (let ((,n-array ,n-array))
	       (declare (type (simple-array * (*)) ,n-array))
	       ,(once-only ((n-len `(length ,n-array))
			    (n-end `(or ,n-evalue ,n-len)))
		  `(if (<= ,n-svalue ,n-end ,n-len)
		       ;; success
		       (values ,n-array ,n-svalue ,n-end 0)
		       ;; failure: Make a NOTINLINE call to
		       ;; %WITH-ARRAY-DATA with our bad data
		       ;; to cause the error to be signalled.
		       (locally
			 (declare (notinline %with-array-data))
			 (%with-array-data ,n-array ,n-svalue ,n-evalue)))))
	     (,(if force-inline '%with-array-data-macro '%with-array-data)
	      ,n-array ,n-svalue ,n-evalue))
       ,@forms)))

;;; This is the fundamental definition of %WITH-ARRAY-DATA, for use in
;;; DEFTRANSFORMs and DEFUNs.
(defmacro %with-array-data-macro (array
				  start
				  end
				  &key
				  (element-type '*)
				  unsafe?
				  fail-inline?)
  (let ((size (gensym "SIZE-"))
	(data (gensym "DATA-"))
	(cumulative-offset (gensym "CUMULATIVE-OFFSET-")))
    `(let* ((,size (array-total-size ,array))
	    (,end (cond (,end
			 (unless (or ,unsafe? (<= ,end ,size))
			   ,(if fail-inline?
				`(error "End ~D is greater than total size ~D."
					,end ,size)
				`(failed-%with-array-data ,array ,start ,end)))
			 ,end)
			(t ,size))))
       (unless (or ,unsafe? (<= ,start ,end))
	 ,(if fail-inline?
	      `(error "Start ~D is greater than end ~D." ,start ,end)
	      `(failed-%with-array-data ,array ,start ,end)))
       (do ((,data ,array (%array-data-vector ,data))
	    (,cumulative-offset 0
				(+ ,cumulative-offset
				   (%array-displacement ,data))))
	   ((not (array-header-p ,data))
	    (values (the (simple-array ,element-type 1) ,data)
		    (the index (+ ,cumulative-offset ,start))
		    (the index (+ ,cumulative-offset ,end))
		    (the index ,cumulative-offset)))
	 (declare (type index ,cumulative-offset))))))

(defun upgraded-element-type-specifier-or-give-up (continuation)
  (let* ((element-ctype (extract-upgraded-element-type continuation))
	 (element-type-specifier (type-specifier element-ctype)))
    (if (eq element-type-specifier '*)
	(give-up-ir1-transform
	 "upgraded array element type not known at compile time")
	element-type-specifier)))

(deftransform %with-array-data ((array start end)
				;; Note: This transform is limited to
				;; VECTOR only because I happened to
				;; create it in order to get sequence
				;; function operations to be more
				;; efficient. It might very well be
				;; reasonable to allow general ARRAY
				;; here, I just haven't tried to
				;; understand the performance issues
				;; involved. -- WHN
				(vector index (or index null))
				*
				:important t
				:node node
				:policy (> speed space))
  "inline non-SIMPLE-vector-handling logic"
  (let ((element-type (upgraded-element-type-specifier-or-give-up array)))
    `(%with-array-data-macro array start end
			     :unsafe? ,(policy node (= safety 0))
			     :element-type ,element-type)))

;;; It'd waste space to expand copies of error handling in every
;;; inline %WITH-ARRAY-DATA, so we have them call this function
;;; instead. This is just a wrapper which is known never to return.
(defknown failed-%with-array-data (t t t) nil)
(defun failed-%with-array-data (array start end)
  (declare (notinline %with-array-data))
  (%with-array-data array start end)
  (error "internal error: shouldn't be here with valid parameters"))

(deftransform fill ((seq item &key (start 0) (end (length seq)))
		    (vector t &key (:start t) (:end index))
		    *
		    :policy (> speed space))
  "open code"
  (let ((element-type (upgraded-element-type-specifier-or-give-up seq)))
    `(with-array-data ((data seq)
		       (start start)
		       (end end))
       (declare (type (simple-array ,element-type 1) data))
       (do ((i start (1+ i)))
	   ((= i end) seq)
	 (declare (type index i))
	 ;; WITH-ARRAY-DATA does our range checks once and for all, so
	 ;; it'd be wasteful to check again on every AREF.
	 (declare (optimize (safety 0))) 
	 (setf (aref data i) item)))))
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

(setf (function-info-transforms (info :function :info 'coerce)) nil)
(deftransform coerce ((x type) (* *) * :when :both)
  (unless (constant-continuation-p type)
    (give-up-ir1-transform))
  (let ((tspec (specifier-type (continuation-value type))))
    (if (csubtypep (continuation-type x) tspec)
	'x
	;; Note: The THE here makes sure that specifiers like
	;; (SINGLE-FLOAT 0.0 1.0) can raise a TYPE-ERROR.
	`(the ,(continuation-value type)
	   ,(cond
	     ((csubtypep tspec (specifier-type 'double-float))
	      '(%double-float x))	
	     ;; FIXME: #!+long-float (t ,(error "LONG-FLOAT case needed"))
	     ((csubtypep tspec (specifier-type 'float))
	      '(%single-float x))
	     ((csubtypep tspec (specifier-type 'simple-vector))
	      '(coerce-to-simple-vector x)) ; FIXME: needs DEFKNOWN return type
	     (t
	      (give-up-ir1-transform)))))))
(defun coerce-to-simple-vector (x)
  (if (simple-vector-p x)
      x
      (replace (make-array (length x)) x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; setting up for POSITION/FIND stuff

(defknown %find-position
  (t sequence t index sequence-end function function)
  (values t (or index null))
  (flushable call))
(defknown %find-position-if 
  (function sequence t index sequence-end function)
  (values t (or index null))
  (call))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; POSITION, POSITION-IF, FIND, and FIND-IF proper

;;; FIXME: Blow away old CMU CL implementation:
;;;  * the section of seq.lisp with VECTOR-LOCATER-MACRO and LOCATER-TEST-NOT
;;;  * matches to 'find' and 'position' in seq.lisp

;;; We want to make sure that %FIND-POSITION is inline-expanded into
;;; %FIND-POSITION-IF only when %FIND-POSITION-IF has an inline
;;; expansion, so we factor out the condition into this function.
(defun check-inlineability-of-find-position-if (sequence from-end)
  (let ((ctype (continuation-type sequence)))
    (cond ((csubtypep ctype (specifier-type 'vector))
	   ;; It's not worth trying to inline vector code unless we know
	   ;; a fair amount about it at compile time.
	   (upgraded-element-type-specifier-or-give-up sequence)
	   (unless (constant-continuation-p from-end)
	     (give-up-ir1-transform
	      "FROM-END argument value not known at compile time")))
	  ((csubtypep ctype (specifier-type 'list))
	   ;; Inlining on lists is generally worthwhile.
	   ) 
	  (t
	   (give-up-ir1-transform
	    "sequence type not known at compile time")))))

;;; %FIND-POSITION-IF for LIST data
(deftransform %find-position-if ((predicate sequence from-end start end key)
				 (function list t t t function)
				 *
				 :policy (> speed space)
				 :important t)
  "expand inline"
  '(let ((index 0)
	 (find nil)
	 (position nil))
     (declare (type index index))
     (dolist (i sequence (values find position))
       (let ((key-i (funcall key i)))
	 (when (and end (>= index end))
	   (return (values find position)))
	 (when (>= index start)
	   (when (funcall predicate key-i)
	     ;; This hack of dealing with non-NIL FROM-END for list data
	     ;; by iterating forward through the list and keeping track of
	     ;; the last time we found a match might be more screwy than
	     ;; what the user expects, but it seems to be allowed by the
	     ;; ANSI standard. (And if the user is screwy enough to ask
	     ;; for FROM-END behavior on list data, turnabout is fair play.)
	     ;;
	     ;; It's also not enormously efficient, calling PREDICATE and
	     ;; KEY more often than necessary; but all the alternatives
	     ;; seem to have their own efficiency problems.
	     (if from-end
		 (setf find i
		       position index)
		 (return (values i index))))))
       (incf index))))

;;; %FIND-POSITION for LIST data can be expanded into %FIND-POSITION-IF
;;; without loss of efficiency. (I.e., the optimizer should be able
;;; to straighten everything out.)
(deftransform %find-position ((item sequence from-end start end key test)
			      (t list t t t t t)
			      *
			      :policy (> speed space)
			      :important t)
  "expand inline"
  '(%find-position-if (let ((test-fun (%coerce-callable-to-function test)))
			(lambda (i)
			  (funcall test-fun i item)))
		      sequence
		      from-end
		      start
		      end
		      (%coerce-callable-to-function key)))

;;; The inline expansions for the VECTOR case are saved as macros so
;;; that we can share them between the DEFTRANSFORMs and the default
;;; cases in the DEFUNs. (This isn't needed for the LIST case, because
;;; the DEFTRANSFORMs for LIST are less choosy about when to expand.)
(defun %find-position-or-find-position-if-vector-expansion (sequence-arg
							    from-end
							    start
							    end-arg
							    element
							    done-p-expr)
  (let ((offset (gensym "OFFSET"))
	(block (gensym "BLOCK"))
	(index (gensym "INDEX"))
	(n-sequence (gensym "N-SEQUENCE-"))
	(sequence (gensym "SEQUENCE"))
	(n-end (gensym "N-END-"))
	(end (gensym "END-")))
    `(let ((,n-sequence ,sequence-arg)
	   (,n-end ,end-arg))
       (with-array-data ((,sequence ,n-sequence :offset-var ,offset)
			 (,start ,start)
			 (,end (or ,n-end (length ,n-sequence))))
         (block ,block
	   (macrolet ((maybe-return ()
			'(let ((,element (aref ,sequence ,index)))
			   (when ,done-p-expr
			     (return-from ,block
			       (values ,element
				       (- ,index ,offset)))))))
	     (if ,from-end
		 (loop for ,index
		       ;; (If we aren't fastidious about declaring that 
		       ;; INDEX might be -1, then (FIND 1 #() :FROM-END T)
		       ;; can send us off into never-never land, since
		       ;; INDEX is initialized to -1.)
		       of-type index-or-minus-1
		       from (1- ,end) downto ,start do
		       (maybe-return))
		 (loop for ,index of-type index from ,start below ,end do
		       (maybe-return))))
	   (values nil nil))))))
(defmacro %find-position-vector-macro (item sequence
					    from-end start end key test)
  (let ((element (gensym "ELEMENT")))
    (%find-position-or-find-position-if-vector-expansion
     sequence
     from-end
     start
     end
     element
     `(funcall ,test ,item (funcall ,key ,element)))))
(defmacro %find-position-if-vector-macro (predicate sequence
						    from-end start end key)
  (let ((element (gensym "ELEMENT")))
    (%find-position-or-find-position-if-vector-expansion
     sequence
     from-end
     start
     end
     element
     `(funcall ,predicate (funcall ,key ,element)))))

;;; %FIND-POSITION and %FIND-POSITION-IF for VECTOR data
(deftransform %find-position-if ((predicate sequence from-end start end key)
				 (function vector t t t function)
				 *
				 :policy (> speed space)
				 :important t)
  "expand inline"
  (check-inlineability-of-find-position-if sequence from-end)
  '(%find-position-if-vector-macro predicate sequence
				   from-end start end key))
(deftransform %find-position ((item sequence from-end start end key test)
			      (t vector t t t function function)
			      *
			      :policy (> speed space)
			      :important t)
  "expand inline"
  (check-inlineability-of-find-position-if sequence from-end)
  '(%find-position-vector-macro item sequence
				from-end start end key test))

;;;; optimizations for floating point FLOOR, CEILING, TRUNCATE, and
;;;; ROUND, lifted from CMU CL 18c
;;;;
;;;; (Without these optimizations, these functions cons!)

;;; Convert (TRUNCATE x y) to the obvious implementation.  We only want
;;; this when under certain conditions and let the generic TRUNCATE
;;; handle the rest.  (Note: if Y = 1, the divide and multiply by Y
;;; should be removed by other DEFTRANSFORMs.)
(deftransform truncate ((x &optional y)
                        (float &optional (or float integer)))
  '(let ((res (%unary-truncate (/ x y))))
     (values res (- x (* y res)))))

(deftransform floor ((number &optional divisor)
                     (float &optional (or integer float)))
  '(multiple-value-bind (tru rem) (truncate number divisor)
    (if (and (not (zerop rem))
             (if (minusp divisor)
                 (plusp number)
                 (minusp number)))
        (values (1- tru) (+ rem divisor))
        (values tru rem))))

(deftransform ceiling ((number &optional divisor)
                       (float &optional (or integer float)))
  '(multiple-value-bind (tru rem) (truncate number divisor)
    (if (and (not (zerop rem))
             (if (minusp divisor)
                 (minusp number)
                 (plusp number)))
        (values (1+ tru) (- rem divisor))
        (values tru rem))))
