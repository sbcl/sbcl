;;;; optimizers for list and sequence functions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;;; mapping onto lists: the MAPFOO functions

(defun mapfoo-transform (fn arglists accumulate take-car)
  (collect ((do-clauses)
	    (args-to-fn)
	    (tests))
    (let ((n-first (gensym)))
      (dolist (a (if accumulate
		     arglists
		     `(,n-first ,@(rest arglists))))
	(let ((v (gensym)))
	  (do-clauses `(,v ,a (cdr ,v)))
	  (tests `(endp ,v))
	  (args-to-fn (if take-car `(car ,v) v))))

      (let ((call `(funcall ,fn . ,(args-to-fn)))
	    (endtest `(or ,@(tests))))
	(ecase accumulate
	  (:nconc
	   (let ((temp (gensym))
		 (map-result (gensym)))
	     `(let ((,map-result (list nil)))
		(do-anonymous ((,temp ,map-result) . ,(do-clauses))
			      (,endtest (cdr ,map-result))
		  (setq ,temp (last (nconc ,temp ,call)))))))
	  (:list
	   (let ((temp (gensym))
		 (map-result (gensym)))
	     `(let ((,map-result (list nil)))
		(do-anonymous ((,temp ,map-result) . ,(do-clauses))
			      (,endtest (cdr ,map-result))
		  (rplacd ,temp (setq ,temp (list ,call)))))))
	  ((nil)
	   `(let ((,n-first ,(first arglists)))
	      (do-anonymous ,(do-clauses)
			    (,endtest ,n-first) ,call))))))))

(define-source-transform mapc (function list &rest more-lists)
  (mapfoo-transform function (cons list more-lists) nil t))

(define-source-transform mapcar (function list &rest more-lists)
  (mapfoo-transform function (cons list more-lists) :list t))

(define-source-transform mapcan (function list &rest more-lists)
  (mapfoo-transform function (cons list more-lists) :nconc t))

(define-source-transform mapl (function list &rest more-lists)
  (mapfoo-transform function (cons list more-lists) nil nil))

(define-source-transform maplist (function list &rest more-lists)
  (mapfoo-transform function (cons list more-lists) :list nil))

(define-source-transform mapcon (function list &rest more-lists)
  (mapfoo-transform function (cons list more-lists) :nconc nil))

;;;; mapping onto sequences: the MAP function

;;; MAP is %MAP plus a check to make sure that any length specified in
;;; the result type matches the actual result. We also wrap it in a
;;; TRULY-THE for the most specific type we can determine.
(deftransform map ((result-type-arg fun &rest seqs) * * :node node)
  (let* ((seq-names (make-gensym-list (length seqs)))
	 (bare `(%map result-type-arg fun ,@seq-names))
	 (constant-result-type-arg-p (constant-continuation-p result-type-arg))
	 ;; what we know about the type of the result. (Note that the
	 ;; "result type" argument is not necessarily the type of the
	 ;; result, since NIL means the result has NULL type.)
	 (result-type (if (not constant-result-type-arg-p)
			  'consed-sequence
			  (let ((result-type-arg-value
				 (continuation-value result-type-arg)))
			    (if (null result-type-arg-value)
				'null
				result-type-arg-value)))))
    `(lambda (result-type-arg fun ,@seq-names)
       (truly-the ,result-type
	 ,(cond ((policy node (> speed safety))
		 bare)
		((not constant-result-type-arg-p)
		 `(sequence-of-checked-length-given-type ,bare
							 result-type-arg))
		(t
		 (let ((result-ctype (specifier-type result-type)))
		   (if (array-type-p result-ctype)
		       (let* ((dims (array-type-dimensions result-ctype))
			      (dim (first dims)))
			 (if (eq dim '*)
			     bare
			     `(vector-of-checked-length-given-length ,bare
								     ,dim)))
		       bare))))))))

;;; Try to compile %MAP efficiently when we can determine sequence
;;; argument types at compile time.
;;;
;;; Note: This transform was written to allow open coding of
;;; quantifiers by expressing them in terms of (MAP NIL ..). For
;;; non-NIL values of RESULT-TYPE, it's still useful, but not
;;; necessarily as efficient as possible. In particular, it will be
;;; inefficient when RESULT-TYPE is a SIMPLE-ARRAY with specialized
;;; numeric element types. It should be straightforward to make it
;;; handle that case more efficiently, but it's left as an exercise to
;;; the reader, because the code is complicated enough already and I
;;; don't happen to need that functionality right now. -- WHN 20000410
(deftransform %map ((result-type fun &rest seqs) * * :policy (>= speed space))
  "open code"
  (unless seqs (abort-ir1-transform "no sequence args"))
  (unless (constant-continuation-p result-type)
    (give-up-ir1-transform "RESULT-TYPE argument not constant"))
  (labels (;; 1-valued SUBTYPEP, fails unless second value of SUBTYPEP is true
	   (fn-1subtypep (fn x y)
	     (multiple-value-bind (subtype-p valid-p) (funcall fn x y)
	       (if valid-p
		   subtype-p
		   (give-up-ir1-transform
		    "can't analyze sequence type relationship"))))
	   (1subtypep (x y) (fn-1subtypep #'sb!xc:subtypep x y))
	   (1csubtypep (x y) (fn-1subtypep #'csubtypep x y))
	   (seq-supertype (seq)
	     (let ((ctype (continuation-type seq)))
	       (cond ((1csubtypep ctype (specifier-type 'vector)) 'vector)
		     ((1csubtypep ctype (specifier-type 'list)) 'list)
		     (t
		      (give-up-ir1-transform
		       "can't determine sequence argument type"))))))
    (let* ((result-type-value (continuation-value result-type))
	   (result-supertype (cond ((null result-type-value) 'null)
				   ((1subtypep result-type-value 'vector)
				    'vector)
				   ((1subtypep result-type-value 'list)
				    'list)
				   (t
				    (give-up-ir1-transform
				     "can't determine result type"))))
	   (seq-supertypes (mapcar #'seq-supertype seqs)))
      (cond ((and result-type-value (= 1 (length seqs)))
	     ;; The consing arity-1 cases can be implemented
	     ;; reasonably efficiently as function calls, and the cost
	     ;; of consing should be significantly larger than
	     ;; function call overhead, so we always compile these
	     ;; cases as full calls regardless of speed-versus-space
	     ;; optimization policy.
	     (cond ((subtypep 'list result-type-value)
		    '(apply #'%map-to-list-arity-1 fun seqs))
		   (;; (This one can be inefficient due to COERCE, but
		    ;; the current open-coded implementation has the
		    ;; same problem.)
		    (subtypep result-type-value 'vector)
		    `(coerce (apply #'%map-to-simple-vector-arity-1 fun seqs)
			     ',result-type-value))
		   (t (give-up-ir1-transform
		       "internal error: unexpected sequence type"))))
	    (t
	     (let* ((seq-args (make-gensym-list (length seqs)))
		    (index-bindingoids
		     (mapcar (lambda (seq-arg seq-supertype)
			       (let ((i (gensym "I"))) 
				 (ecase seq-supertype
				   (vector `(,i 0 (1+ ,i)))
				   (list `(,i ,seq-arg (rest ,i))))))
			     seq-args seq-supertypes))
		    (indices (mapcar #'first index-bindingoids))
		    (index-decls (mapcar (lambda (index seq-supertype)
					   `(type ,(ecase seq-supertype
						     (vector 'index)
						     (list 'list))
						  ,index))
					 indices seq-supertypes))
		    (tests (mapcar (lambda (seq-arg seq-supertype index)
				     (ecase seq-supertype
				       (vector `(>= ,index (length ,seq-arg)))
				       (list `(endp ,index))))
				   seq-args seq-supertypes indices))
		    (values (mapcar (lambda (seq-arg seq-supertype index)
				      (ecase seq-supertype
					(vector `(aref ,seq-arg ,index))
					(list `(first ,index))))
				    seq-args seq-supertypes indices)))
	       (multiple-value-bind (push-dacc final-result)
		   (ecase result-supertype
		     (null (values nil nil))
		     (list (values `(push dacc acc) `(nreverse acc)))
		     (vector (values `(push dacc acc)
				     `(coerce (nreverse acc)
					      ',result-type-value))))
		 ;; (We use the same idiom, of returning a LAMBDA from
		 ;; DEFTRANSFORM, as is used in the DEFTRANSFORMs for
		 ;; FUNCALL and ALIEN-FUNCALL, and for the same
		 ;; reason: we need to get the runtime values of each
		 ;; of the &REST vars.)
		 `(lambda (result-type fun ,@seq-args)
		    (declare (ignore result-type))
		    (do ((really-fun (%coerce-callable-to-fun fun))
			 ,@index-bindingoids
			 (acc nil))
		    ((or ,@tests)
		     ,final-result)
		    (declare ,@index-decls)
		    (declare (type list acc))
		    (declare (ignorable acc))
		    (let ((dacc (funcall really-fun ,@values)))
		      (declare (ignorable dacc))
		      ,push-dacc))))))))))

(deftransform elt ((s i) ((simple-array * (*)) *) * :when :both)
  '(aref s i))

(deftransform elt ((s i) (list *) * :when :both)
  '(nth i s))

(deftransform %setelt ((s i v) ((simple-array * (*)) * *) * :when :both)
  '(%aset s i v))

(deftransform %setelt ((s i v) (list * *))
  '(setf (car (nthcdr i s)) v))

(macrolet ((def-frob (name)
             `(deftransform ,name ((e l &key (test #'eql)) * * :node node :when :both)
                (unless (constant-continuation-p l)
                  (give-up-ir1-transform))

                (let ((val (continuation-value l)))
                  (unless (policy node
                                  (or (= speed 3)
                                      (and (>= speed space)
                                           (<= (length val) 5))))
                    (give-up-ir1-transform))

                  (labels ((frob (els)
                             (if els
                                 `(if (funcall test e ',(car els))
                                      ',els
                                      ,(frob (cdr els)))
                                 nil)))
                    (frob val))))))
  (def-frob member)
  (def-frob memq))

;;; FIXME: We have rewritten the original code that used DOLIST to this
;;; more natural MACROLET.  However, the original code suggested that when
;;; this was done, a few bytes could be saved by a call to a shared
;;; function.  This remains to be done.
(macrolet ((def-frob (fun eq-fun)
             `(deftransform ,fun ((item list &key test) (t list &rest t) *)
                "convert to EQ test"
                ;; FIXME: The scope of this transformation could be
                ;; widened somewhat, letting it work whenever the test is
                ;; 'EQL and we know from the type of ITEM that it #'EQ
                ;; works like #'EQL on it. (E.g. types FIXNUM, CHARACTER,
                ;; and SYMBOL.)
                ;;   If TEST is EQ, apply transform, else
                ;;   if test is not EQL, then give up on transform, else
                ;;   if ITEM is not a NUMBER or is a FIXNUM, apply
                ;;   transform, else give up on transform.
                (cond (test
                       (unless (continuation-function-is test '(eq))
                         (give-up-ir1-transform)))
                      ((types-equal-or-intersect (continuation-type item)
                                                 (specifier-type 'number))
                       (give-up-ir1-transform "Item might be a number.")))
                `(,',eq-fun item list))))
  (def-frob delete delq)
  (def-frob assoc assq)
  (def-frob member memq))

(deftransform delete-if ((pred list) (t list))
  "open code"
  '(do ((x list (cdr x))
	(splice '()))
       ((endp x) list)
     (cond ((funcall pred (car x))
	    (if (null splice)
		(setq list (cdr x))
		(rplacd splice (cdr x))))
	   (T (setq splice x)))))

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
	 ;; WITH-ARRAY-DATA did our range checks once and for all, so
	 ;; it'd be wasteful to check again on every AREF.
	 (declare (optimize (safety 0))) 
	 (setf (aref data i) item)))))

;;;; utilities

;;; Return true if CONT's only use is a non-notinline reference to a
;;; global function with one of the specified NAMES.
(defun continuation-function-is (cont names)
  (declare (type continuation cont) (list names))
  (let ((use (continuation-use cont)))
    (and (ref-p use)
	 (let ((leaf (ref-leaf use)))
	   (and (global-var-p leaf)
		(eq (global-var-kind leaf) :global-function)
		(not (null (member (leaf-source-name leaf) names
				   :test #'equal))))))))

;;; If CONT is a constant continuation, the return the constant value.
;;; If it is null, then return default, otherwise quietly give up the
;;; IR1 transform.
;;;
;;; ### Probably should take an ARG and flame using the NAME.
(defun constant-value-or-lose (cont &optional default)
  (declare (type (or continuation null) cont))
  (cond ((not cont) default)
	((constant-continuation-p cont)
	 (continuation-value cont))
	(t
	 (give-up-ir1-transform))))

#|
;;; This is a frob whose job it is to make it easier to pass around
;;; the arguments to IR1 transforms. It bundles together the name of
;;; the argument (which should be referenced in any expansion), and
;;; the continuation for that argument (or NIL if unsupplied.)
(defstruct (arg (:constructor %make-arg (name cont))
		(:copier nil))
  (name nil :type symbol)
  (cont nil :type (or continuation null)))
(defmacro make-arg (name)
  `(%make-arg ',name ,name))

;;; If Arg is null or its CONT is null, then return Default, otherwise
;;; return Arg's NAME.
(defun default-arg (arg default)
  (declare (type (or arg null) arg))
  (if (and arg (arg-cont arg))
      (arg-name arg)
      default))

;;; If Arg is null or has no CONT, return the default. Otherwise, Arg's
;;; CONT must be a constant continuation whose value we return. If not, we
;;; give up.
(defun arg-constant-value (arg default)
  (declare (type (or arg null) arg))
  (if (and arg (arg-cont arg))
      (let ((cont (arg-cont arg)))
	(unless (constant-continuation-p cont)
	  (give-up-ir1-transform "Argument is not constant: ~S."
				 (arg-name arg)))
	(continuation-value from-end))
      default))

;;; If Arg is a constant and is EQL to X, then return T, otherwise NIL. If
;;; Arg is NIL or its CONT is NIL, then compare to the default.
(defun arg-eql (arg default x)
  (declare (type (or arg null) x))
  (if (and arg (arg-cont arg))
      (let ((cont (arg-cont arg)))
	(and (constant-continuation-p cont)
	     (eql (continuation-value cont) x)))
      (eql default x)))

(defstruct (iterator (:copier nil))
  ;; The kind of iterator.
  (kind nil (member :normal :result))
  ;; A list of LET* bindings to create the initial state.
  (binds nil :type list)
  ;; A list of declarations for Binds.
  (decls nil :type list)
  ;; A form that returns the current value. This may be set with SETF to set
  ;; the current value.
  (current (error "Must specify CURRENT."))
  ;; In a :Normal iterator, a form that tests whether there is a current value.
  (done nil)
  ;; In a :Result iterator, a form that truncates the result at the current
  ;; position and returns it.
  (result nil)
  ;; A form that returns the initial total number of values. The result is
  ;; undefined after NEXT has been evaluated.
  (length (error "Must specify LENGTH."))
  ;; A form that advances the state to the next value. It is an error to call
  ;; this when the iterator is Done.
  (next (error "Must specify NEXT.")))

;;; Type of an index var that can go negative (in the from-end case.)
(deftype neg-index ()
  `(integer -1 ,most-positive-fixnum))

;;; Return an ITERATOR structure describing how to iterate over an arbitrary
;;; sequence. Sequence is a variable bound to the sequence, and Type is the
;;; type of the sequence. If true, INDEX is a variable that should be bound to
;;; the index of the current element in the sequence.
;;;
;;; If we can't tell whether the sequence is a list or a vector, or whether
;;; the iteration is forward or backward, then GIVE-UP.
(defun make-sequence-iterator (sequence type &key start end from-end index)
  (declare (symbol sequence) (type ctype type)
	   (type (or arg null) start end from-end)
	   (type (or symbol null) index))
  (let ((from-end (arg-constant-value from-end nil)))
    (cond ((csubtypep type (specifier-type 'vector))
	   (let* ((n-stop (gensym))
		  (n-idx (or index (gensym)))
		  (start (default-arg 0 start))
		  (end (default-arg `(length ,sequence) end)))
	     (make-iterator
	      :kind :normal
	      :binds `((,n-idx ,(if from-end `(1- ,end) ,start))
		       (,n-stop ,(if from-end `(1- ,start) ,end)))
	      :decls `((type neg-index ,n-idx ,n-stop))
	      :current `(aref ,sequence ,n-idx)
 	      :done `(,(if from-end '<= '>=) ,n-idx ,n-stop)
	      :next `(setq ,n-idx
			   ,(if from-end `(1- ,n-idx) `(1+ ,n-idx)))
	      :length (if from-end
			  `(- ,n-idx ,n-stop)
			  `(- ,n-stop ,n-idx)))))
	  ((csubtypep type (specifier-type 'list))
	   (let* ((n-stop (if (and end (not from-end)) (gensym) nil))
		  (n-current (gensym))
		  (start-p (not (arg-eql start 0 0)))
		  (end-p (not (arg-eql end nil nil)))
		  (start (default-arg start 0))
		  (end (default-arg end nil)))
	     (make-iterator
	      :binds `((,n-current
			,(if from-end
			     (if (or start-p end-p)
				 `(nreverse (subseq ,sequence ,start
						    ,@(when end `(,end))))
				 `(reverse ,sequence))
			     (if start-p
				 `(nthcdr ,start ,sequence)
				 sequence)))
		       ,@(when n-stop
			   `((,n-stop (nthcdr (the index
						   (- ,end ,start))
					      ,n-current))))
		       ,@(when index
			   `((,index ,(if from-end `(1- ,end) start)))))
	      :kind :normal
	      :decls `((list ,n-current ,n-end)
		       ,@(when index `((type neg-index ,index))))
	      :current `(car ,n-current)
	      :done `(eq ,n-current ,n-stop)
	      :length `(- ,(or end `(length ,sequence)) ,start)
	      :next `(progn
		       (setq ,n-current (cdr ,n-current))
		       ,@(when index
			   `((setq ,n-idx
				   ,(if from-end
					`(1- ,index)
					`(1+ ,index)))))))))
	  (t
	   (give-up-ir1-transform
	    "can't tell whether sequence is a list or a vector")))))

;;; Make an iterator used for constructing result sequences. Name is a
;;; variable to be bound to the result sequence. Type is the type of result
;;; sequence to make. Length is an expression to be evaluated to get the
;;; maximum length of the result (not evaluated in list case.)
(defun make-result-sequence-iterator (name type length)
  (declare (symbol name) (type ctype type))

;;; Defines each Name as a local macro that will call the value of the
;;; Fun-Arg with the given arguments. If the argument isn't known to be a
;;; function, give them an efficiency note and reference a coerced version.
(defmacro coerce-functions (specs &body body)
  #!+sb-doc
  "COERCE-FUNCTIONS ({(Name Fun-Arg Default)}*) Form*"
  (collect ((binds)
	    (defs))
    (dolist (spec specs)
      `(let ((body (progn ,@body))
	     (n-fun (arg-name ,(second spec)))
	     (fun-cont (arg-cont ,(second spec))))
	 (cond ((not fun-cont)
		`(macrolet ((,',(first spec) (&rest args)
			     `(,',',(third spec) ,@args)))
		   ,body))
	       ((not (csubtypep (continuation-type fun-cont)
				(specifier-type 'function)))
		(when (policy *compiler-error-context*
			      (> speed inhibit-warnings))
		  (compiler-note
		   "~S may not be a function, so must coerce at run-time."
		   n-fun))
		(once-only ((n-fun `(if (functionp ,n-fun)
					,n-fun
					(symbol-function ,n-fun))))
		  `(macrolet ((,',(first spec) (&rest args)
			       `(funcall ,',n-fun ,@args)))
		     ,body)))
	       (t
		`(macrolet ((,',(first spec) (&rest args)
			      `(funcall ,',n-fun ,@args)))
		   ,body)))))))

;;; Wrap code around the result of the body to define Name as a local macro
;;; that returns true when its arguments satisfy the test according to the Args
;;; Test and Test-Not. If both Test and Test-Not are supplied, abort the
;;; transform.
(defmacro with-sequence-test ((name test test-not) &body body)
  `(let ((not-p (arg-cont ,test-not)))
     (when (and (arg-cont ,test) not-p)
       (abort-ir1-transform "Both ~S and ~S were supplied."
			    (arg-name ,test)
			    (arg-name ,test-not)))
     (coerce-functions ((,name (if not-p ,test-not ,test) eql))
       ,@body)))
|#

;;;; hairy sequence transforms

;;; FIXME: no hairy sequence transforms in SBCL?

;;;; string operations

;;; We transform the case-sensitive string predicates into a non-keyword
;;; version. This is an IR1 transform so that we don't have to worry about
;;; changing the order of evaluation.
(macrolet ((def-frob (fun pred*)
             `(deftransform ,fun ((string1 string2 &key (start1 0) end1
                                                         (start2 0) end2)
                                   * *)
                `(,',pred* string1 string2 start1 end1 start2 end2))))
  (def-frob string< string<*)
  (def-frob string> string>*)
  (def-frob string<= string<=*)
  (def-frob string>= string>=*)
  (def-frob string= string=*)
  (def-frob string/= string/=*))

;;; Return a form that tests the free variables STRING1 and STRING2
;;; for the ordering relationship specified by LESSP and EQUALP. The
;;; start and end are also gotten from the environment. Both strings
;;; must be SIMPLE-STRINGs.
(macrolet ((def-frob (name lessp equalp)
             `(deftransform ,name ((string1 string2 start1 end1 start2 end2)
                                    (simple-string simple-string t t t t) *)
                `(let* ((end1 (if (not end1) (length string1) end1))
                        (end2 (if (not end2) (length string2) end2))
                        (index (sb!impl::%sp-string-compare
                                string1 start1 end1 string2 start2 end2)))
                  (if index
                      (cond ((= index ,(if ',lessp 'end1 'end2)) index)
                            ((= index ,(if ',lessp 'end2 'end1)) nil)
                            ((,(if ',lessp 'char< 'char>)
                               (schar string1 index)
                               (schar string2
                                      (truly-the index
                                                 (+ index
                                                    (truly-the fixnum
                                                               (- start2 start1))))))
                             index)
                            (t nil))
                      ,(if ',equalp 'end1 nil))))))
  (def-frob string<* t nil)
  (def-frob string<=* t t)
  (def-frob string>* nil nil)
  (def-frob string>=* nil t))

(macrolet ((def-frob (name result-fun)
             `(deftransform ,name ((string1 string2 start1 end1 start2 end2)
                                   (simple-string simple-string t t t t) *)
                `(,',result-fun
                  (sb!impl::%sp-string-compare
                   string1 start1 (or end1 (length string1))
                   string2 start2 (or end2 (length string2)))))))
  (def-frob string=* not)
  (def-frob string/=* identity))


;;;; string-only transforms for sequence functions
;;;;
;;;; Note: CMU CL had more of these, including transforms for
;;;; functions which cons. In SBCL, we've gotten rid of most of the
;;;; transforms for functions which cons, since our GC overhead is
;;;; sufficiently large that it doesn't seem worth it to try to
;;;; economize on function call overhead or on the overhead of runtime
;;;; type dispatch in AREF. The exception is CONCATENATE, since
;;;; a full call to CONCATENATE would have to look up the sequence
;;;; type, which can be really slow.
;;;;
;;;; FIXME: It would be nicer for these transforms to work for any
;;;; calls when all arguments are vectors with the same element type,
;;;; rather than restricting them to STRINGs only.

;;; FIXME: Shouldn't we be testing for legality of
;;;   * START1, START2, END1, and END2 indices?
;;;   * size of copied string relative to destination string?
;;; (Either there should be tests conditional on SAFETY>=SPEED, or
;;; the transform should be conditional on SPEED>SAFETY.)
;;;
;;; FIXME: Also, the transform should probably be dependent on
;;; SPEED>SPACE.
(deftransform replace ((string1 string2 &key (start1 0) (start2 0)
				end1 end2)
		       (simple-string simple-string &rest t))
  `(locally
     (declare (optimize (safety 0)))
     (bit-bash-copy string2
		    (the index
			 (+ (the index (* start2 sb!vm:n-byte-bits))
			    ,vector-data-bit-offset))
		    string1
		    (the index
			 (+ (the index (* start1 sb!vm:n-byte-bits))
			    ,vector-data-bit-offset))
		    (the index
			 (* (min (the index (- (or end1 (length string1))
					       start1))
				 (the index (- (or end2 (length string2))
					       start2)))
			    sb!vm:n-byte-bits)))
     string1))

;;; FIXME: It seems as though it should be possible to make a DEFUN
;;; %CONCATENATE (with a DEFTRANSFORM to translate constant RTYPE to
;;; CTYPE before calling %CONCATENATE) which is comparably efficient,
;;; at least once DYNAMIC-EXTENT works.
(deftransform concatenate ((rtype &rest sequences)
			   (t &rest simple-string)
			   simple-string)
  (collect ((lets)
	    (forms)
	    (all-lengths)
	    (args))
    (dolist (seq sequences)
      (declare (ignore seq))
      (let ((n-seq (gensym))
	    (n-length (gensym)))
	(args n-seq)
	(lets `(,n-length (the index (* (length ,n-seq) sb!vm:n-byte-bits))))
	(all-lengths n-length)
	(forms `(bit-bash-copy ,n-seq ,vector-data-bit-offset
			       res start
			       ,n-length))
	(forms `(setq start (+ start ,n-length)))))
    `(lambda (rtype ,@(args))
       (declare (ignore rtype))
       (let* (,@(lets)
	      (res (make-string (truncate (the index (+ ,@(all-lengths)))
					  sb!vm:n-byte-bits)))
	      (start ,vector-data-bit-offset))
	 (declare (type index start ,@(all-lengths)))
	 ,@(forms)
	 res))))

;;;; CONS accessor DERIVE-TYPE optimizers

(defoptimizer (car derive-type) ((cons))
  (let ((type (continuation-type cons))
	(null-type (specifier-type 'null)))
    (cond ((eq type null-type)
	   null-type)
	  ((cons-type-p type)
	   (cons-type-car-type type)))))

(defoptimizer (cdr derive-type) ((cons))
  (let ((type (continuation-type cons))
	(null-type (specifier-type 'null)))
    (cond ((eq type null-type)
	   null-type)
	  ((cons-type-p type)
	   (cons-type-cdr-type type)))))

;;;; FIND, POSITION, and their -IF and -IF-NOT variants

;;; We want to make sure that %FIND-POSITION is inline-expanded into
;;; %FIND-POSITION-IF only when %FIND-POSITION-IF has an inline
;;; expansion, so we factor out the condition into this function.
(defun check-inlineability-of-find-position-if (sequence from-end)
  (let ((ctype (continuation-type sequence)))
    (cond ((csubtypep ctype (specifier-type 'vector))
	   ;; It's not worth trying to inline vector code unless we
	   ;; know a fair amount about it at compile time.
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
	     ;; This hack of dealing with non-NIL FROM-END for list
	     ;; data by iterating forward through the list and keeping
	     ;; track of the last time we found a match might be more
	     ;; screwy than what the user expects, but it seems to be
	     ;; allowed by the ANSI standard. (And if the user is
	     ;; screwy enough to ask for FROM-END behavior on list
	     ;; data, turnabout is fair play.)
	     ;;
	     ;; It's also not enormously efficient, calling PREDICATE
	     ;; and KEY more often than necessary; but all the
	     ;; alternatives seem to have their own efficiency
	     ;; problems.
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
  '(%find-position-if (let ((test-fun (%coerce-callable-to-fun test)))
			;; I'm having difficulty believing I'm
			;; reading it right, but as far as I can see,
			;; the only guidance that ANSI gives for the
			;; order of arguments to asymmetric tests is
			;; the character-set dependent example from
			;; the definition of FIND,
			;;   (find #\d "here are some.." :test #'char>)
			;;     => #\Space
			;; (In ASCII, we have (CHAR> #\d #\SPACE)=>T.)
			;; (Neither the POSITION definition page nor
			;; section 17.2 ("Rules about Test Functions")
			;; seem to consider the possibility of
			;; asymmetry.)
			;;
			;; So, judging from the example, we want to
			;; do (FUNCALL TEST-FUN ITEM I), because
			;; (FUNCALL #'CHAR> #\d #\SPACE)=>T.
			;;
			;; -- WHN (whose attention was drawn to it by
			;;         Alexey Dejneka's bug report/fix)
			(lambda (i)
			  (funcall test-fun item i)))
		      sequence
		      from-end
		      start
		      end
		      (%coerce-callable-to-fun key)))

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

(def!macro %find-position-vector-macro (item sequence
					     from-end start end key test)
  (let ((element (gensym "ELEMENT")))
    (%find-position-or-find-position-if-vector-expansion
     sequence
     from-end
     start
     end
     element
     ;; (See the LIST transform for a discussion of the correct
     ;; argument order, i.e. whether the searched-for ,ITEM goes before
     ;; or after the checked sequence element.)
     `(funcall ,test ,item (funcall ,key ,element)))))

(def!macro %find-position-if-vector-macro (predicate sequence
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
