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

      (let* ((fn-sym (gensym))  ; for ONCE-ONLY-ish purposes
	     (call `(funcall ,fn-sym . ,(args-to-fn)))
	     (endtest `(or ,@(tests))))
	(ecase accumulate
	  (:nconc
	   (let ((temp (gensym))
		 (map-result (gensym)))
	     `(let ((,fn-sym ,fn)
		    (,map-result (list nil)))
		(do-anonymous ((,temp ,map-result) . ,(do-clauses))
			      (,endtest (cdr ,map-result))
		  (setq ,temp (last (nconc ,temp ,call)))))))
	  (:list
	   (let ((temp (gensym))
		 (map-result (gensym)))
	     `(let ((,fn-sym ,fn)
		    (,map-result (list nil)))
		(do-anonymous ((,temp ,map-result) . ,(do-clauses))
			      (,endtest (truly-the list (cdr ,map-result)))
		  (rplacd ,temp (setq ,temp (list ,call)))))))
	  ((nil)
	   `(let ((,fn-sym ,fn)
		  (,n-first ,(first arglists)))
	      (do-anonymous ,(do-clauses)
			    (,endtest (truly-the list ,n-first))
                            ,call))))))))

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
(deftransform map ((result-type-arg fun seq &rest seqs) * * :node node)
  (let* ((seq-names (make-gensym-list (1+ (length seqs))))
	 (bare `(%map result-type-arg fun ,@seq-names))
	 (constant-result-type-arg-p (constant-lvar-p result-type-arg))
	 ;; what we know about the type of the result. (Note that the
	 ;; "result type" argument is not necessarily the type of the
	 ;; result, since NIL means the result has NULL type.)
	 (result-type (if (not constant-result-type-arg-p)
			  'consed-sequence
			  (let ((result-type-arg-value
				 (lvar-value result-type-arg)))
			    (if (null result-type-arg-value)
				'null
				result-type-arg-value)))))
    `(lambda (result-type-arg fun ,@seq-names)
       (truly-the ,result-type
	 ,(cond ((policy node (< safety 3))
		 ;; ANSI requires the length-related type check only
		 ;; when the SAFETY quality is 3... in other cases, we
		 ;; skip it, because it could be expensive.
		 bare)
		((not constant-result-type-arg-p)
		 `(sequence-of-checked-length-given-type ,bare
							 result-type-arg))
		(t
		 (let ((result-ctype (ir1-transform-specifier-type
				      result-type)))
		   (if (array-type-p result-ctype)
		       (let ((dims (array-type-dimensions result-ctype)))
			 (unless (and (listp dims) (= (length dims) 1))
			   (give-up-ir1-transform "invalid sequence type"))
			 (let ((dim (first dims)))
			   (if (eq dim '*)
			       bare
			       `(vector-of-checked-length-given-length ,bare
				                                       ,dim))))
		       ;; FIXME: this is wrong, as not all subtypes of
		       ;; VECTOR are ARRAY-TYPEs [consider, for
		       ;; example, (OR (VECTOR T 3) (VECTOR T
		       ;; 4))]. However, it's difficult to see what we
		       ;; should put here... maybe we should
		       ;; GIVE-UP-IR1-TRANSFORM if the type is a
		       ;; subtype of VECTOR but not an ARRAY-TYPE?
		       bare))))))))

;;; Return a DO loop, mapping a function FUN to elements of
;;; sequences. SEQS is a list of lvars, SEQ-NAMES - list of variables,
;;; bound to sequences, INTO - a variable, which is used in
;;; MAP-INTO. RESULT and BODY are forms, which can use variables
;;; FUNCALL-RESULT, containing the result of application of FUN, and
;;; INDEX, containing the current position in sequences.
(defun build-sequence-iterator (seqs seq-names &key result into body)
  (declare (type list seqs seq-names)
           (type symbol into))
  (collect ((bindings)
	    (declarations)
            (vector-lengths)
            (tests)
            (places))
    (let ((found-vector-p nil))
      (flet ((process-vector (length)
               (unless found-vector-p
                 (setq found-vector-p t)
                 (bindings `(index 0 (1+ index)))
                 (declarations `(type index index)))
               (vector-lengths length)))
        (loop for seq of-type lvar in seqs
           for seq-name in seq-names
           for type = (lvar-type seq)
           do (cond ((csubtypep type (specifier-type 'list))
		     (with-unique-names (index)
                       (bindings `(,index ,seq-name (cdr ,index)))
                       (declarations `(type list ,index))
                       (places `(car ,index))
                       (tests `(endp ,index))))
                    ((csubtypep type (specifier-type 'vector))
                     (process-vector `(length ,seq-name))
                     (places `(aref ,seq-name index)))
                    (t
                     (give-up-ir1-transform
                      "can't determine sequence argument type"))))
        (when into
          (process-vector `(array-dimension ,into 0))))
      (when found-vector-p
        (bindings `(length (min ,@(vector-lengths))))
        (tests `(= index length)))
      `(do (,@(bindings))
           ((or ,@(tests)) ,result)
         (declare ,@(declarations))
         (let ((funcall-result (funcall fun ,@(places))))
           (declare (ignorable funcall-result))
           ,body)))))

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
(deftransform %map ((result-type fun seq &rest seqs) * *
		    :policy (>= speed space))
  "open code"
  (unless (constant-lvar-p result-type)
    (give-up-ir1-transform "RESULT-TYPE argument not constant"))
  (labels ( ;; 1-valued SUBTYPEP, fails unless second value of SUBTYPEP is true
	   (fn-1subtypep (fn x y)
	     (multiple-value-bind (subtype-p valid-p) (funcall fn x y)
	       (if valid-p
		   subtype-p
		   (give-up-ir1-transform
		    "can't analyze sequence type relationship"))))
	   (1subtypep (x y) (fn-1subtypep #'sb!xc:subtypep x y)))
    (let* ((result-type-value (lvar-value result-type))
	   (result-supertype (cond ((null result-type-value) 'null)
				   ((1subtypep result-type-value 'vector)
				    'vector)
				   ((1subtypep result-type-value 'list)
				    'list)
				   (t
				    (give-up-ir1-transform
				     "can't determine result type")))))
      (cond ((and result-type-value (null seqs))
	     ;; The consing arity-1 cases can be implemented
	     ;; reasonably efficiently as function calls, and the cost
	     ;; of consing should be significantly larger than
	     ;; function call overhead, so we always compile these
	     ;; cases as full calls regardless of speed-versus-space
	     ;; optimization policy.
	     (cond ((subtypep result-type-value 'list)
		    '(%map-to-list-arity-1 fun seq))
		   ( ;; (This one can be inefficient due to COERCE, but
		    ;; the current open-coded implementation has the
		    ;; same problem.)
		    (subtypep result-type-value 'vector)
		    `(coerce (%map-to-simple-vector-arity-1 fun seq)
			     ',result-type-value))
		   (t (bug "impossible (?) sequence type"))))
	    (t
	     (let* ((seqs (cons seq seqs))
		    (seq-args (make-gensym-list (length seqs))))
	       (multiple-value-bind (push-dacc result)
		   (ecase result-supertype
		     (null (values nil nil))
		     (list (values `(push funcall-result acc)
                                   `(nreverse acc)))
		     (vector (values `(push funcall-result acc)
				     `(coerce (nreverse acc)
					      ',result-type-value))))
		 ;; (We use the same idiom, of returning a LAMBDA from
		 ;; DEFTRANSFORM, as is used in the DEFTRANSFORMs for
		 ;; FUNCALL and ALIEN-FUNCALL, and for the same
		 ;; reason: we need to get the runtime values of each
		 ;; of the &REST vars.)
		 `(lambda (result-type fun ,@seq-args)
		    (declare (ignore result-type))
                    (let ((fun (%coerce-callable-to-fun fun))
                          (acc nil))
                      (declare (type list acc))
                      (declare (ignorable acc))
                      ,(build-sequence-iterator
                        seqs seq-args
                        :result result
                        :body push-dacc))))))))))

;;; MAP-INTO
(deftransform map-into ((result fun &rest seqs)
                        (vector * &rest *)
                        *)
  "open code"
  (let ((seqs-names (mapcar (lambda (x)
                              (declare (ignore x))
                              (gensym))
                            seqs)))
    `(lambda (result fun ,@seqs-names)
       ,(build-sequence-iterator
         seqs seqs-names
         :result '(when (array-has-fill-pointer-p result)
                   (setf (fill-pointer result) index))
         :into 'result
         :body '(setf (aref result index) funcall-result))
       result)))


;;; FIXME: once the confusion over doing transforms with known-complex
;;; arrays is over, we should also transform the calls to (AND (ARRAY
;;; * (*)) (NOT (SIMPLE-ARRAY * (*)))) objects.
(deftransform elt ((s i) ((simple-array * (*)) *) *)
  '(aref s i))

(deftransform elt ((s i) (list *) * :policy (< safety 3))
  '(nth i s))

(deftransform %setelt ((s i v) ((simple-array * (*)) * *) *)
  '(%aset s i v))

(deftransform %setelt ((s i v) (list * *) * :policy (< safety 3))
  '(setf (car (nthcdr i s)) v))

(deftransform %check-vector-sequence-bounds ((vector start end)
					     (vector * *) *
					     :node node)
  (if (policy node (< safety speed))
      '(or end (length vector))
      '(let ((length (length vector)))
	(if (<= 0 start (or end length) length)
	    (or end length)
	    (sb!impl::signal-bounding-indices-bad-error vector start end)))))

(macrolet ((def (name)
             `(deftransform ,name ((e l &key (test #'eql)) * *
				   :node node)
                (unless (constant-lvar-p l)
                  (give-up-ir1-transform))

                (let ((val (lvar-value l)))
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
  (def member)
  (def memq))

;;; FIXME: We have rewritten the original code that used DOLIST to this
;;; more natural MACROLET.  However, the original code suggested that when
;;; this was done, a few bytes could be saved by a call to a shared
;;; function.  This remains to be done.
(macrolet ((def (fun eq-fun)
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
                       (unless (lvar-fun-is test '(eq))
                         (give-up-ir1-transform)))
                      ((types-equal-or-intersect (lvar-type item)
                                                 (specifier-type 'number))
                       (give-up-ir1-transform "Item might be a number.")))
                `(,',eq-fun item list))))
  (def delete delq)
  (def assoc assq)
  (def member memq))

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
    (values 
     `(with-array-data ((data seq)
			(start start)
			(end end))
       (declare (type (simple-array ,element-type 1) data))
       (declare (type fixnum start end))
       (do ((i start (1+ i)))
	   ((= i end) seq)
	 (declare (type index i))
	 ;; WITH-ARRAY-DATA did our range checks once and for all, so
	 ;; it'd be wasteful to check again on every AREF...
	 (declare (optimize (safety 0))) 
	 (setf (aref data i) item)))
     ;; ... though we still need to check that the new element can fit
     ;; into the vector in safe code. -- CSR, 2002-07-05
     `((declare (type ,element-type item))))))

;;;; utilities

;;; Return true if LVAR's only use is a non-NOTINLINE reference to a
;;; global function with one of the specified NAMES.
(defun lvar-fun-is (lvar names)
  (declare (type lvar lvar) (list names))
  (let ((use (lvar-uses lvar)))
    (and (ref-p use)
	 (let ((leaf (ref-leaf use)))
	   (and (global-var-p leaf)
		(eq (global-var-kind leaf) :global-function)
		(not (null (member (leaf-source-name leaf) names
				   :test #'equal))))))))

;;; If LVAR is a constant lvar, the return the constant value. If it
;;; is null, then return default, otherwise quietly give up the IR1
;;; transform.
;;;
;;; ### Probably should take an ARG and flame using the NAME.
(defun constant-value-or-lose (lvar &optional default)
  (declare (type (or lvar null) lvar))
  (cond ((not lvar) default)
	((constant-lvar-p lvar)
	 (lvar-value lvar))
	(t
	 (give-up-ir1-transform))))

;;; FIXME: Why is this code commented out? (Why *was* it commented
;;; out? We inherited this situation from cmucl-2.4.8, with no
;;; explanation.) Should we just delete this code?
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
  ;; In a :NORMAL iterator, a form that tests whether there is a current value.
  (done nil)
  ;; In a :RESULT iterator, a form that truncates the result at the current
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

;;; Define each NAME as a local macro that will call the value of the
;;; function arg with the given arguments. If the argument isn't known to be a
;;; function, give them an efficiency note and reference a coerced version.
(defmacro coerce-funs (specs &body body)
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
		  (compiler-notify
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
     (coerce-funs ((,name (if not-p ,test-not ,test) eql))
       ,@body)))
|#

;;;; hairy sequence transforms

;;; FIXME: no hairy sequence transforms in SBCL?

;;;; string operations

;;; We transform the case-sensitive string predicates into a non-keyword
;;; version. This is an IR1 transform so that we don't have to worry about
;;; changing the order of evaluation.
(macrolet ((def (fun pred*)
             `(deftransform ,fun ((string1 string2 &key (start1 0) end1
                                                         (start2 0) end2)
                                   * *)
                `(,',pred* string1 string2 start1 end1 start2 end2))))
  (def string< string<*)
  (def string> string>*)
  (def string<= string<=*)
  (def string>= string>=*)
  (def string= string=*)
  (def string/= string/=*))

;;; Return a form that tests the free variables STRING1 and STRING2
;;; for the ordering relationship specified by LESSP and EQUALP. The
;;; start and end are also gotten from the environment. Both strings
;;; must be SIMPLE-BASE-STRINGs.
(macrolet ((def (name lessp equalp)
             `(deftransform ,name ((string1 string2 start1 end1 start2 end2)
                                    (simple-base-string simple-base-string t t t t) *)
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
                                                               (- start2
								  start1))))))
                             index)
                            (t nil))
                      ,(if ',equalp 'end1 nil))))))
  (def string<* t nil)
  (def string<=* t t)
  (def string>* nil nil)
  (def string>=* nil t))

(macrolet ((def (name result-fun)
             `(deftransform ,name ((string1 string2 start1 end1 start2 end2)
                                   (simple-base-string simple-base-string t t t t) *)
                `(,',result-fun
                  (sb!impl::%sp-string-compare
                   string1 start1 (or end1 (length string1))
                   string2 start2 (or end2 (length string2)))))))
  (def string=* not)
  (def string/=* identity))


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

;;; Moved here from generic/vm-tran.lisp to satisfy clisp
;;;
;;; FIXME: Add a comment telling whether this holds for all vectors
;;; or only for vectors based on simple arrays (non-adjustable, etc.).
(def!constant vector-data-bit-offset
  (* sb!vm:vector-data-offset sb!vm:n-word-bits))

(deftransform replace ((string1 string2 &key (start1 0) (start2 0)
				end1 end2)
		       (simple-base-string simple-base-string &rest t)
		       *
		       ;; FIXME: consider replacing this policy test
		       ;; with some tests for the STARTx and ENDx
		       ;; indices being valid, conditional on high
		       ;; SAFETY code.
		       ;;
		       ;; FIXME: It turns out that this transform is
		       ;; critical for the performance of string
		       ;; streams.  Make this more explicit.
		       :policy (< (max safety space) 3))
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

#+nil
(deftransform replace ((string1 string2 &key (start1 0) (start2 0)
				end1 end2)
		       ((simple-array character (*))
			(simple-array character (*))
			&rest t)
		       *
		       ;; FIXME: consider replacing this policy test
		       ;; with some tests for the STARTx and ENDx
		       ;; indices being valid, conditional on high
		       ;; SAFETY code.
		       ;;
		       ;; FIXME: It turns out that this transform is
		       ;; critical for the performance of string
		       ;; streams.  Make this more explicit.
		       :policy (< (max safety space) 3))
  `(locally
     (declare (optimize (safety 0)))
     (bit-bash-copy string2
		    (the index
			 (+ (the index (* start2 sb!vm:n-word-bits))
			    ,vector-data-bit-offset))
		    string1
		    (the index
			 (+ (the index (* start1 sb!vm:n-word-bits))
			    ,vector-data-bit-offset))
		    (the index
			 (* (min (the index (- (or end1 (length string1))
					       start1))
				 (the index (- (or end2 (length string2))
					       start2)))
			    sb!vm:n-word-bits)))
     string1))

;;; FIXME: this would be a valid transform for certain excluded cases:
;;;   * :TEST 'CHAR= or :TEST #'CHAR=
;;;   * :TEST 'EQL   or :TEST #'EQL
;;;   * :FROM-END NIL (or :FROM-END non-NIL, with a little ingenuity)
(deftransform search ((pattern text &key (start1 0) (start2 0) end1 end2)
		      (simple-string simple-string &rest t)
		      *
		      :policy (> speed (max space safety)))
  `(block search
    (let ((end1 (or end1 (length pattern)))
	  (end2 (or end2 (length text))))
      (do ((index2 start2 (1+ index2)))
	  ((>= index2 end2) nil)
	(when (do ((index1 start1 (1+ index1))
		   (index2 index2 (1+ index2)))
		  ((>= index1 end1) t)
		(when (= index2 end2)
		  (return-from search nil))
		(when (char/= (char pattern index1) (char text index2))
		  (return nil)))
	  (return index2))))))

;;; FIXME: It seems as though it should be possible to make a DEFUN
;;; %CONCATENATE (with a DEFTRANSFORM to translate constant RTYPE to
;;; CTYPE before calling %CONCATENATE) which is comparably efficient,
;;; at least once DYNAMIC-EXTENT works.
;;;
;;; FIXME: currently KLUDGEed because of bug 188
(deftransform concatenate ((rtype &rest sequences)
			   (t &rest (or simple-base-string
					(simple-array nil (*))))
			   (simple-array character (*))
			   :policy (< safety 3))
  (loop for rest-seqs on sequences
        for n-seq = (gensym "N-SEQ")
        for n-length = (gensym "N-LENGTH")
        for start = vector-data-bit-offset then next-start
        for next-start = (gensym "NEXT-START")
        collect n-seq into args
        collect `(,n-length (* (length ,n-seq) sb!vm:n-byte-bits)) into lets
        collect n-length into all-lengths
        collect next-start into starts
        collect `(if (and (typep ,n-seq '(simple-array nil (*)))
		          (> ,n-length 0))
		     (error 'nil-array-accessed-error)
		     (bit-bash-copy ,n-seq ,vector-data-bit-offset
		                    res ,start ,n-length))
                into forms
        collect `(setq ,next-start (+ ,start ,n-length)) into forms
        finally
        (return
          `(lambda (rtype ,@args)
             (declare (ignore rtype))
             (let* (,@lets
                      (res (make-string (truncate (the index (+ ,@all-lengths))
                                                  sb!vm:n-byte-bits))))
               (declare (type index ,@all-lengths))
               (let (,@(mapcar (lambda (name) `(,name 0)) starts))
                 (declare (type index ,@starts))
                 ,@forms)
               res)))))

;;;; CONS accessor DERIVE-TYPE optimizers

(defoptimizer (car derive-type) ((cons))
  (let ((type (lvar-type cons))
	(null-type (specifier-type 'null)))
    (cond ((eq type null-type)
	   null-type)
	  ((cons-type-p type)
	   (cons-type-car-type type)))))

(defoptimizer (cdr derive-type) ((cons))
  (let ((type (lvar-type cons))
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
  (let ((ctype (lvar-type sequence)))
    (cond ((csubtypep ctype (specifier-type 'vector))
	   ;; It's not worth trying to inline vector code unless we
	   ;; know a fair amount about it at compile time.
	   (upgraded-element-type-specifier-or-give-up sequence)
	   (unless (constant-lvar-p from-end)
	     (give-up-ir1-transform
	      "FROM-END argument value not known at compile time")))
	  ((csubtypep ctype (specifier-type 'list))
	   ;; Inlining on lists is generally worthwhile.
	   ) 
	  (t
	   (give-up-ir1-transform
	    "sequence type not known at compile time")))))

;;; %FIND-POSITION-IF and %FIND-POSITION-IF-NOT for LIST data
(macrolet ((def (name condition)
	     `(deftransform ,name ((predicate sequence from-end start end key)
				   (function list t t t function)
				   *
				   :policy (> speed space))
		"expand inline"
		`(let ((index 0)
		       (find nil)
		       (position nil))
		   (declare (type index index))
		   (dolist (i sequence
			    (if (and end (> end index))
				(sb!impl::signal-bounding-indices-bad-error
				 sequence start end)
				(values find position)))
		     (let ((key-i (funcall key i)))
		       (when (and end (>= index end))
			 (return (values find position)))
		       (when (>= index start)
			 (,',condition (funcall predicate key-i)
			  ;; This hack of dealing with non-NIL
			  ;; FROM-END for list data by iterating
			  ;; forward through the list and keeping
			  ;; track of the last time we found a match
			  ;; might be more screwy than what the user
			  ;; expects, but it seems to be allowed by
			  ;; the ANSI standard. (And if the user is
			  ;; screwy enough to ask for FROM-END
			  ;; behavior on list data, turnabout is
			  ;; fair play.)
			  ;;
			  ;; It's also not enormously efficient,
			  ;; calling PREDICATE and KEY more often
			  ;; than necessary; but all the
			  ;; alternatives seem to have their own
			  ;; efficiency problems.
			  (if from-end
			      (setf find i
				    position index)
			      (return (values i index))))))
		     (incf index))))))
  (def %find-position-if when)
  (def %find-position-if-not unless))

;;; %FIND-POSITION for LIST data can be expanded into %FIND-POSITION-IF
;;; without loss of efficiency. (I.e., the optimizer should be able
;;; to straighten everything out.)
(deftransform %find-position ((item sequence from-end start end key test)
			      (t list t t t t t)
			      *
			      :policy (> speed space))
  "expand inline"
  '(%find-position-if (let ((test-fun (%coerce-callable-to-fun test)))
			;; The order of arguments for asymmetric tests
			;; (e.g. #'<, as opposed to order-independent
			;; tests like #'=) is specified in the spec
			;; section 17.2.1 -- the O/Zi stuff there.
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
  (with-unique-names (offset block index n-sequence sequence n-end end)
    `(let ((,n-sequence ,sequence-arg)
	   (,n-end ,end-arg))
       (with-array-data ((,sequence ,n-sequence :offset-var ,offset)
			 (,start ,start)
			 (,end (%check-vector-sequence-bounds
				,n-sequence ,start ,n-end)))
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
  (with-unique-names (element)
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
  (with-unique-names (element)
    (%find-position-or-find-position-if-vector-expansion
     sequence
     from-end
     start
     end
     element
     `(funcall ,predicate (funcall ,key ,element)))))

(def!macro %find-position-if-not-vector-macro (predicate sequence
							 from-end start end key)
  (with-unique-names (element)
    (%find-position-or-find-position-if-vector-expansion
     sequence
     from-end
     start
     end
     element
     `(not (funcall ,predicate (funcall ,key ,element))))))

;;; %FIND-POSITION, %FIND-POSITION-IF and %FIND-POSITION-IF-NOT for
;;; VECTOR data
(deftransform %find-position-if ((predicate sequence from-end start end key)
				 (function vector t t t function)
				 *
				 :policy (> speed space))
  "expand inline"
  (check-inlineability-of-find-position-if sequence from-end)
  '(%find-position-if-vector-macro predicate sequence
				   from-end start end key))

(deftransform %find-position-if-not ((predicate sequence from-end start end key)
				     (function vector t t t function)
				     *
				     :policy (> speed space))
  "expand inline"
  (check-inlineability-of-find-position-if sequence from-end)
  '(%find-position-if-not-vector-macro predicate sequence
                                       from-end start end key))

(deftransform %find-position ((item sequence from-end start end key test)
			      (t vector t t t function function)
			      *
			      :policy (> speed space))
  "expand inline"
  (check-inlineability-of-find-position-if sequence from-end)
  '(%find-position-vector-macro item sequence
				from-end start end key test))

;;; logic to unravel :TEST, :TEST-NOT, and :KEY options in FIND,
;;; POSITION-IF, etc.
(define-source-transform effective-find-position-test (test test-not)
  (once-only ((test test)
	      (test-not test-not))
    `(cond
      ((and ,test ,test-not)
       (error "can't specify both :TEST and :TEST-NOT"))
      (,test (%coerce-callable-to-fun ,test))
      (,test-not
       ;; (Without DYNAMIC-EXTENT, this is potentially horribly
       ;; inefficient, but since the TEST-NOT option is deprecated
       ;; anyway, we don't care.)
       (complement (%coerce-callable-to-fun ,test-not)))
      (t #'eql))))
(define-source-transform effective-find-position-key (key)
  (once-only ((key key))
    `(if ,key
	 (%coerce-callable-to-fun ,key)
	 #'identity)))

(macrolet ((define-find-position (fun-name values-index)
	     `(deftransform ,fun-name ((item sequence &key
					     from-end (start 0) end
					     key test test-not))
		'(nth-value ,values-index
			    (%find-position item sequence
					    from-end start
					    end
					    (effective-find-position-key key)
					    (effective-find-position-test
					     test test-not))))))
  (define-find-position find 0)
  (define-find-position position 1))

(macrolet ((define-find-position-if (fun-name values-index)
	     `(deftransform ,fun-name ((predicate sequence &key
						  from-end (start 0)
						  end key))
		'(nth-value
		  ,values-index
		  (%find-position-if (%coerce-callable-to-fun predicate)
				     sequence from-end
				     start end
				     (effective-find-position-key key))))))
  (define-find-position-if find-if 0)
  (define-find-position-if position-if 1))

;;; the deprecated functions FIND-IF-NOT and POSITION-IF-NOT. We
;;; didn't bother to worry about optimizing them, except note that on
;;; Sat, Oct 06, 2001 at 04:22:38PM +0100, Christophe Rhodes wrote on
;;; sbcl-devel
;;;
;;;     My understanding is that while the :test-not argument is
;;;     deprecated in favour of :test (complement #'foo) because of
;;;     semantic difficulties (what happens if both :test and :test-not
;;;     are supplied, etc) the -if-not variants, while officially
;;;     deprecated, would be undeprecated were X3J13 actually to produce
;;;     a revised standard, as there are perfectly legitimate idiomatic
;;;     reasons for allowing the -if-not versions equal status,
;;;     particularly remove-if-not (== filter).
;;;
;;;     This is only an informal understanding, I grant you, but
;;;     perhaps it's worth optimizing the -if-not versions in the same
;;;     way as the others?
;;;
;;; FIXME: Maybe remove uses of these deprecated functions within the
;;; implementation of SBCL.
(macrolet ((define-find-position-if-not (fun-name values-index)
	       `(deftransform ,fun-name ((predicate sequence &key
					  from-end (start 0)
					  end key))
		 '(nth-value
		   ,values-index
		   (%find-position-if-not (%coerce-callable-to-fun predicate)
		    sequence from-end
		    start end
		    (effective-find-position-key key))))))
  (define-find-position-if-not find-if-not 0)
  (define-find-position-if-not position-if-not 1))
