;;;; functions to implement lists

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;;; KLUDGE: comment from CMU CL, what does it mean?
;;;;   NSUBLIS, things at the beginning broken.
;;;; -- WHN 20000127

(declaim (maybe-inline
	  tree-equal list-length nth %setnth nthcdr last make-list append
	  copy-list copy-alist copy-tree revappend nconc nreconc butlast
	  nbutlast ldiff member member-if member-if-not tailp adjoin union
	  nunion intersection nintersection set-difference nset-difference
	  set-exclusive-or nset-exclusive-or subsetp acons pairlis assoc
	  assoc-if assoc-if-not rassoc rassoc-if rassoc-if-not subst subst-if
	  subst-if-not nsubst nsubst-if nsubst-if-not sublis nsublis))

;;; These functions perform basic list operations:
(defun car (list) #!+sb-doc "Returns the 1st object in a list." (car list))
(defun cdr (list)
  #!+sb-doc "Returns all but the first object in a list."
  (cdr list))
(defun cadr (list) #!+sb-doc "Returns the 2nd object in a list." (cadr list))
(defun cdar (list) #!+sb-doc "Returns the cdr of the 1st sublist." (cdar list))
(defun caar (list) #!+sb-doc "Returns the car of the 1st sublist." (caar list))
(defun cddr (list)
  #!+sb-doc "Returns all but the 1st two objects of a list."
  (cddr list))
(defun caddr (list)
  #!+sb-doc "Returns the 1st object in the cddr of a list."
  (caddr list))
(defun caadr (list)
  #!+sb-doc "Returns the 1st object in the cadr of a list."
  (caadr list))
(defun caaar (list)
  #!+sb-doc "Returns the 1st object in the caar of a list."
  (caaar list))
(defun cdaar (list)
  #!+sb-doc "Returns the cdr of the caar of a list."
  (cdaar list))
(defun cddar (list)
  #!+sb-doc "Returns the cdr of the cdar of a list."
  (cddar list))
(defun cdddr (list)
  #!+sb-doc "Returns the cdr of the cddr of a list."
  (cdddr list))
(defun cadar (list)
  #!+sb-doc "Returns the car of the cdar of a list."
  (cadar list))
(defun cdadr (list)
  #!+sb-doc "Returns the cdr of the cadr of a list."
  (cdadr list))
(defun caaaar (list)
  #!+sb-doc "Returns the car of the caaar of a list."
  (caaaar list))
(defun caaadr (list)
  #!+sb-doc "Returns the car of the caadr of a list."
  (caaadr list))
(defun caaddr (list)
  #!+sb-doc "Returns the car of the caddr of a list."
  (caaddr list))
(defun cadddr (list)
  #!+sb-doc "Returns the car of the cdddr of a list."
  (cadddr list))
(defun cddddr (list)
  #!+sb-doc "Returns the cdr of the cdddr of a list."
  (cddddr list))
(defun cdaaar (list)
  #!+sb-doc "Returns the cdr of the caaar of a list."
  (cdaaar list))
(defun cddaar (list)
  #!+sb-doc "Returns the cdr of the cdaar of a list."
  (cddaar list))
(defun cdddar (list)
  #!+sb-doc "Returns the cdr of the cddar of a list."
  (cdddar list))
(defun caadar (list)
  #!+sb-doc "Returns the car of the cadar of a list."
  (caadar list))
(defun cadaar (list)
  #!+sb-doc "Returns the car of the cdaar of a list."
  (cadaar list))
(defun cadadr (list)
  #!+sb-doc "Returns the car of the cdadr of a list."
  (cadadr list))
(defun caddar (list)
  #!+sb-doc "Returns the car of the cddar of a list."
  (caddar list))
(defun cdaadr (list)
  #!+sb-doc "Returns the cdr of the caadr of a list."
  (cdaadr list))
(defun cdadar (list)
  #!+sb-doc "Returns the cdr of the cadar of a list."
  (cdadar list))
(defun cdaddr (list)
  #!+sb-doc "Returns the cdr of the caddr of a list."
  (cdaddr list))
(defun cddadr (list)
  #!+sb-doc "Returns the cdr of the cdadr of a list."
  (cddadr list))
(defun cons (se1 se2)
  #!+sb-doc "Returns a list with se1 as the car and se2 as the cdr."
  (cons se1 se2))

(declaim (maybe-inline tree-equal-test tree-equal-test-not))

(defun tree-equal-test-not (x y test-not)
  (cond ((consp x)
	 (and (consp y)
	      (tree-equal-test-not (car x) (car y) test-not)
	      (tree-equal-test-not (cdr x) (cdr y) test-not)))
	((consp y) nil)
	((not (funcall test-not x y)) t)
	(t ())))

(defun tree-equal-test (x y test)
  (cond	((consp x)
	 (and (consp y)
	      (tree-equal-test (car x) (car y) test)
	      (tree-equal-test (cdr x) (cdr y) test)))
	((consp y) nil)
	((funcall test x y) t)
	(t ())))

(defun tree-equal (x y &key (test #'eql) test-not)
  #!+sb-doc
  "Returns T if X and Y are isomorphic trees with identical leaves."
  (if test-not
      (tree-equal-test-not x y test-not)
      (tree-equal-test x y test)))

(defun endp (object)
  #!+sb-doc
  "The recommended way to test for the end of a list. True if Object is nil,
   false if Object is a cons, and an error for any other types of arguments."
  (endp object))

(defun list-length (list)
  #!+sb-doc
  "Returns the length of the given List, or Nil if the List is circular."
  (do ((n 0 (+ n 2))
       (y list (cddr y))
       (z list (cdr z)))
      (())
    (declare (fixnum n) (list y z))
    (when (endp y) (return n))
    (when (endp (cdr y)) (return (+ n 1)))
    (when (and (eq y z) (> n 0)) (return nil))))

(defun nth (n list)
  #!+sb-doc
  "Returns the nth object in a list where the car is the zero-th element."
  (car (nthcdr n list)))

(defun first (list)
  #!+sb-doc
  "Returns the 1st object in a list or NIL if the list is empty."
  (car list))
(defun second (list)
  "Returns the 2nd object in a list or NIL if there is no 2nd object."
  (cadr list))
(defun third (list)
  #!+sb-doc
  "Returns the 3rd object in a list or NIL if there is no 3rd object."
  (caddr list))
(defun fourth (list)
  #!+sb-doc
  "Returns the 4th object in a list or NIL if there is no 4th object."
  (cadddr list))
(defun fifth (list)
  #!+sb-doc
  "Returns the 5th object in a list or NIL if there is no 5th object."
  (car (cddddr list)))
(defun sixth (list)
  #!+sb-doc
  "Returns the 6th object in a list or NIL if there is no 6th object."
  (cadr (cddddr list)))
(defun seventh (list)
  #!+sb-doc
  "Returns the 7th object in a list or NIL if there is no 7th object."
  (caddr (cddddr list)))
(defun eighth (list)
  #!+sb-doc
  "Returns the 8th object in a list or NIL if there is no 8th object."
  (cadddr (cddddr list)))
(defun ninth (list)
  #!+sb-doc
  "Returns the 9th object in a list or NIL if there is no 9th object."
  (car (cddddr (cddddr list))))
(defun tenth (list)
  #!+sb-doc
  "Returns the 10th object in a list or NIL if there is no 10th object."
  (cadr (cddddr (cddddr list))))
(defun rest (list)
  #!+sb-doc
  "Means the same as the cdr of a list."
  (cdr list))

(defun nthcdr (n list)
  (declare (type index n))
  #!+sb-doc
  "Performs the cdr function n times on a list."
  (do ((i n (1- i))
       (result list (cdr result)))
      ((not (plusp i)) result)
      (declare (type index i))))

(defun last (list &optional (n 1))
  #!+sb-doc
  "Returns the last N conses (not the last element!) of a list."
  (declare (type index n))
  (do ((checked-list list (cdr checked-list))
       (returned-list list)
       (index 0 (1+ index)))
      ((atom checked-list) returned-list)
    (declare (type index index))
    (if (>= index n)
	(pop returned-list))))

(defun list (&rest args)
  #!+sb-doc
  "Returns constructs and returns a list of its arguments."
  args)

;;; List* is done the same as list, except that the last cons is made a
;;; dotted pair

(defun list* (arg &rest others)
  #!+sb-doc
  "Returns a list of the arguments with last cons a dotted pair"
  (cond ((atom others) arg)
	((atom (cdr others)) (cons arg (car others)))
	(t (do ((x others (cdr x)))
	       ((null (cddr x)) (rplacd x (cadr x))))
	   (cons arg others))))

(defun make-list (size &key initial-element)
  #!+sb-doc
  "Constructs a list with size elements each set to value"
  (declare (type index size))
  (do ((count size (1- count))
       (result '() (cons initial-element result)))
      ((zerop count) result)
    (declare (type index count))))

;;; The outer loop finds the first non-null list and the result is started.
;;; The remaining lists in the arguments are tacked to the end of the result
;;; using splice which cdr's down the end of the new list

(defun append (&rest lists)
  #!+sb-doc
  "Construct a new list by concatenating the list arguments"
  (do ((top lists (cdr top)))	 ;;Cdr to first non-null list.
      ((atom top) '())
    (cond ((null (car top)))				; Nil -> Keep looping
	  ((not (consp (car top)))			; Non cons
	   (if (cdr top)
	       (error "~S is not a list." (car top))
	       (return (car top))))
	  (t						; Start appending
	   (return
	     (if (atom (cdr top))
		 (car top)    ;;Special case.
		 (let* ((result (cons (caar top) '()))
			(splice result))
		   (do ((x (cdar top) (cdr x)))  ;;Copy first list
		       ((atom x))
		     (setq splice
			   (cdr (rplacd splice (cons (car x) ()) ))) )
		   (do ((y (cdr top) (cdr y)))	 ;;Copy rest of lists.
		       ((atom (cdr y))
			(setq splice (rplacd splice (car y)))
			result)
		     (if (listp (car y))
			 (do ((x (car y) (cdr x)))   ;;Inner copy loop.
			     ((atom x))
			   (setq
			    splice
			    (cdr (rplacd splice (cons (car x) ())))))
			 (error "~S is not a list." (car y)))))))))))

;;; list copying functions

(defun copy-list (list)
  #!+sb-doc
  "Returns a new list which is EQUAL to LIST."
  ;; The list is copied correctly even if the list is not terminated
  ;; by NIL. The new list is built by CDR'ing SPLICE which is always
  ;; at the tail of the new list.
  (if (atom list)
      list
      (let ((result (list (car list))))
	(do ((x (cdr list) (cdr x))
	     (splice result
		     (cdr (rplacd splice (cons (car x) '() ))) ))
	    ((atom x)
	     (unless (null x)
	       (rplacd splice x))))
	result)))

(defun copy-alist (alist)
  #!+sb-doc
  "Returns a new association list which is EQUAL to ALIST."
  (if (atom alist)
      alist
      (let ((result
	     (cons (if (atom (car alist))
		       (car alist)
		       (cons (caar alist) (cdar alist)) )
		   nil)))
	(do ((x (cdr alist) (cdr x))
	     (splice result
		     (cdr (rplacd splice
				  (cons
				   (if (atom (car x))
				       (car x)
				       (cons (caar x) (cdar x)))
				   nil)))))
	    ;; Non-null terminated alist done here.
	    ((atom x)
	     (unless (null x)
	       (rplacd splice x))))
	result)))

(defun copy-tree (object)
  #!+sb-doc
  "Recursively copy trees of conses."
  (if (consp object)
      (cons (copy-tree (car object)) (copy-tree (cdr object)))
      object))

;;; more commonly-used list functions

(defun revappend (x y)
  #!+sb-doc
  "Returns (append (reverse x) y)"
  (do ((top x (cdr top))
       (result y (cons (car top) result)))
      ((endp top) result)))

;;; NCONC finds the first non-null list, so it can make splice point
;;; to a cons. After finding the first cons element, it holds it in a
;;; result variable while running down successive elements tacking
;;; them together. While tacking lists together, if we encounter a
;;; null list, we set the previous list's last cdr to nil just in case
;;; it wasn't already nil, and it could have been dotted while the
;;; null list was the last argument to NCONC. The manipulation of
;;; splice (that is starting it out on a first cons, setting LAST of
;;; splice, and setting splice to ele) inherently handles (nconc x x),
;;; and it avoids running down the last argument to NCONC which allows
;;; the last argument to be circular.
(defun nconc (&rest lists)
  #!+sb-doc
  "Concatenates the lists given as arguments (by changing them)"
  (do ((top lists (cdr top)))
      ((null top) nil)
    (let ((top-of-top (car top)))
      (typecase top-of-top
	(cons
	 (let* ((result top-of-top)
		(splice result))
	   (do ((elements (cdr top) (cdr elements)))
	       ((endp elements))
	     (let ((ele (car elements)))
	       (typecase ele
		 (cons (rplacd (last splice) ele)
		       (setf splice ele))
		 (null (rplacd (last splice) nil))
		 (atom (if (cdr elements)
			   (error "Argument is not a list -- ~S." ele)
			   (rplacd (last splice) ele)))
		 (t (error "Argument is not a list -- ~S." ele)))))
	   (return result)))
	(null)
	(atom
	 (if (cdr top)
	     (error "Argument is not a list -- ~S." top-of-top)
	     (return top-of-top)))
	(t (error "Argument is not a list -- ~S." top-of-top))))))

(defun nreconc (x y)
  #!+sb-doc
  "Returns (nconc (nreverse x) y)"
  (do ((1st (cdr x) (if (atom 1st) 1st (cdr 1st)))
       (2nd x 1st)		;2nd follows first down the list.
       (3rd y 2nd))		;3rd follows 2nd down the list.
      ((atom 2nd) 3rd)
    (rplacd 2nd 3rd)))

(flet (;; Return the number of conses at the head of the
       ;; possibly-improper list LIST. (Or if LIST is circular, you
       ;; lose.)
       (count-conses (list)
	 (do ((in-list list (cdr in-list))
	      (result 0 (1+ result)))
	     ((atom in-list)
	      result)
	   (declare (type index result)))))
  (declare (ftype (function (t) index) count-conses))
  (defun butlast (list &optional (n 1))
    (let* ((n-conses-in-list (count-conses list))
	   (n-remaining-to-copy (- n-conses-in-list n)))
      (declare (type fixnum n-remaining-to-copy))
      (when (plusp n-remaining-to-copy)
	(do* ((result (list (first list)))
	      (rest (rest list) (rest rest))
	      (splice result))
	    ((zerop (decf n-remaining-to-copy))
	     result)
	  (setf splice
		(setf (cdr splice)
		      (list (first rest))))))))
  (defun nbutlast (list &optional (n 1))
    (let ((n-conses-in-list (count-conses list)))
      (unless (< n-conses-in-list n)
	(setf (cdr (nthcdr (- n-conses-in-list n 1) list))
	      nil)
	list))))

(defun ldiff (list object)
  "Return a new list, whose elements are those of LIST that appear before
   OBJECT. If OBJECT is not a tail of LIST, a copy of LIST is returned.
   LIST must be a proper list or a dotted list."
  (do* ((list list (cdr list))
	(result (list ()))
	(splice result))
       ((atom list)
	(if (eql list object)
	    (cdr result)
	    (progn (rplacd splice list) (cdr result))))
    (if (eql list object)
	(return (cdr result))
	(setq splice (cdr (rplacd splice (list (car list))))))))

;;;; functions to alter list structure

(defun rplaca (x y)
  #!+sb-doc
  "Change the CAR of X to Y and return the new X."
  (rplaca x y))

(defun rplacd (x y)
  #!+sb-doc
  "Change the CDR of X to Y and return the new X."
  (rplacd x y))

;;; The following are for use by SETF.

(defun %rplaca (x val) (rplaca x val) val)

(defun %rplacd (x val) (rplacd x val) val)

;;; Set the Nth element of LIST to NEWVAL.
(defun %setnth (n list newval)
  (declare (type index n))
  (do ((count n (1- count))
       (list list (cdr list)))
      ((endp list)
       (error "~S is too large an index for SETF of NTH." n))
    (declare (fixnum count))
    (when (<= count 0)
      (rplaca list newval)
      (return newval))))

;;;; :KEY arg optimization to save funcall of IDENTITY

;;; APPLY-KEY saves us a function call sometimes.
;;;    This is not wrapped in an (EVAL-WHEN (COMPILE EVAL) ..)
;;;    because this is used in seq.lisp and sort.lisp.
(defmacro apply-key (key element)
  `(if ,key
       (funcall ,key ,element)
       ,element))

(defun identity (thing)
  #!+sb-doc
  "This function simply returns what was passed to it."
  thing)

(defun complement (function)
  #!+sb-doc
  "Return a new function that returns T whenever FUNCTION returns NIL and
   NIL whenever FUNCTION returns non-NIL."
  (lambda (&optional (arg0 nil arg0-p) (arg1 nil arg1-p) (arg2 nil arg2-p)
		     &rest more-args)
    (not (cond (more-args (apply function arg0 arg1 arg2 more-args))
	       (arg2-p (funcall function arg0 arg1 arg2))
	       (arg1-p (funcall function arg0 arg1))
	       (arg0-p (funcall function arg0))
	       (t (funcall function))))))

(defun constantly (value)
  #!+sb-doc
  "Return a function that always returns VALUE."
  (lambda ()
    ;; KLUDGE: This declaration is a hack to make the closure ignore
    ;; all its arguments without consing a &REST list or anything.
    ;; Perhaps once DYNAMIC-EXTENT is implemented we won't need to
    ;; screw around with this kind of thing. -- WHN 2001-04-06
    (declare (optimize (speed 3) (safety 0)))
    value))

;;;; macros for (&KEY (KEY #'IDENTITY) (TEST #'EQL TESTP) (TEST-NOT NIL NOTP))

;;; Use these with the following &KEY args:
(defmacro with-set-keys (funcall)
  `(cond ((and testp notp) (error ":TEST and :TEST-NOT were both supplied."))
	 (notp ,(append funcall '(:key key :test-not test-not)))
	 (t ,(append funcall '(:key key :test test)))))

(defmacro satisfies-the-test (item elt)
  (let ((key-tmp (gensym)))
    `(let ((,key-tmp (apply-key key ,elt)))
      (cond (testp (funcall test ,item ,key-tmp))
	    (notp (not (funcall test-not ,item ,key-tmp)))
	    (t (funcall test ,item ,key-tmp))))))

;;;; substitution of expressions

(defun subst (new old tree &key key (test #'eql testp) (test-not nil notp))
  #!+sb-doc
  "Substitutes new for subtrees matching old."
  (labels ((s (subtree)
	      (cond ((satisfies-the-test old subtree) new)
		    ((atom subtree) subtree)
		    (t (let ((car (s (car subtree)))
			     (cdr (s (cdr subtree))))
			 (if (and (eq car (car subtree))
				  (eq cdr (cdr subtree)))
			     subtree
			     (cons car cdr)))))))
    (s tree)))

(defun subst-if (new test tree &key key)
  #!+sb-doc
  "Substitutes new for subtrees for which test is true."
  (labels ((s (subtree)
	      (cond ((funcall test (apply-key key subtree)) new)
		    ((atom subtree) subtree)
		    (t (let ((car (s (car subtree)))
			     (cdr (s (cdr subtree))))
			 (if (and (eq car (car subtree))
				  (eq cdr (cdr subtree)))
			     subtree
			     (cons car cdr)))))))
    (s tree)))

(defun subst-if-not (new test tree &key key)
  #!+sb-doc
  "Substitutes new for subtrees for which test is false."
  (labels ((s (subtree)
	      (cond ((not (funcall test (apply-key key subtree))) new)
		    ((atom subtree) subtree)
		    (t (let ((car (s (car subtree)))
			     (cdr (s (cdr subtree))))
			 (if (and (eq car (car subtree))
				  (eq cdr (cdr subtree)))
			     subtree
			     (cons car cdr)))))))
    (s tree)))

(defun nsubst (new old tree &key key (test #'eql testp) (test-not nil notp))
  #!+sb-doc
  "Substitutes new for subtrees matching old."
  (labels ((s (subtree)
	      (cond ((satisfies-the-test old subtree) new)
		    ((atom subtree) subtree)
		    (t (do* ((last nil subtree)
			     (subtree subtree (Cdr subtree)))
			    ((atom subtree)
			     (if (satisfies-the-test old subtree)
				 (setf (cdr last) new)))
			 (if (satisfies-the-test old subtree)
			     (return (setf (cdr last) new))
			     (setf (car subtree) (s (car subtree)))))
		       subtree))))
    (s tree)))

(defun nsubst-if (new test tree &key key)
  #!+sb-doc
  "Substitutes new for subtrees of tree for which test is true."
  (labels ((s (subtree)
	      (cond ((funcall test (apply-key key subtree)) new)
		    ((atom subtree) subtree)
		    (t (do* ((last nil subtree)
			     (subtree subtree (Cdr subtree)))
			    ((atom subtree)
			     (if (funcall test (apply-key key subtree))
				 (setf (cdr last) new)))
			 (if (funcall test (apply-key key subtree))
			     (return (setf (cdr last) new))
			     (setf (car subtree) (s (car subtree)))))
		       subtree))))
    (s tree)))

(defun nsubst-if-not (new test tree &key key)
  #!+sb-doc
  "Substitutes new for subtrees of tree for which test is false."
  (labels ((s (subtree)
	      (cond ((not (funcall test (apply-key key subtree))) new)
		    ((atom subtree) subtree)
		    (t (do* ((last nil subtree)
			     (subtree subtree (Cdr subtree)))
			    ((atom subtree)
			     (if (not (funcall test (apply-key key subtree)))
				 (setf (cdr last) new)))
			 (if (not (funcall test (apply-key key subtree)))
			     (return (setf (cdr last) new))
			     (setf (car subtree) (s (car subtree)))))
		       subtree))))
    (s tree)))

(defun sublis (alist tree &key key (test #'eql) (test-not nil notp))
  #!+sb-doc
  "Substitutes from alist into tree nondestructively."
  (declare (inline assoc))
  (labels ((s (subtree)
	     (let* ((key-val (apply-key key subtree))
		    (assoc (if notp
			       (assoc key-val alist :test-not test-not)
			       (assoc key-val alist :test test))))
	       (cond (assoc (cdr assoc))
		     ((atom subtree) subtree)
		     (t (let ((car (s (car subtree)))
			      (cdr (s (cdr subtree))))
			  (if (and (eq car (car subtreE))
				   (eq cdr (cdr subtree)))
			      subtree
			      (cons car cdr))))))))
    (s tree)))

;;; These are in run-time env (i.e. not wrapped in EVAL-WHEN (COMPILE EVAL))
;;; because they can be referenced in inline expansions.
(defmacro nsublis-macro ()
  (let ((key-tmp (gensym)))
    `(let ((,key-tmp (apply-key key subtree)))
      (if notp
	  (assoc ,key-tmp alist :test-not test-not)
	  (assoc ,key-tmp alist :test test)))))

(defun nsublis (alist tree &key key (test #'eql) (test-not nil notp))
  #!+sb-doc
  "Substitutes new for subtrees matching old."
  (declare (inline assoc))
  (let (temp)
    (labels ((s (subtree)
		(cond ((Setq temp (nsublis-macro))
		       (cdr temp))
		      ((atom subtree) subtree)
		      (t (do* ((last nil subtree)
			       (subtree subtree (Cdr subtree)))
			      ((atom subtree)
			       (if (setq temp (nsublis-macro))
				   (setf (cdr last) (cdr temp))))
			   (if (setq temp (nsublis-macro))
			       (return (setf (Cdr last) (Cdr temp)))
			       (setf (car subtree) (s (car subtree)))))
			 subtree))))
      (s tree))))

;;;; functions for using lists as sets

(defun member (item list &key key (test #'eql testp) (test-not nil notp))
  #!+sb-doc
  "Returns tail of list beginning with first element satisfying EQLity,
   :TEST, or :TEST-NOT with a given item."
  (do ((list list (cdr list)))
      ((null list) nil)
    (let ((car (car list)))
      (if (satisfies-the-test item car)
	  (return list)))))

(defun member-if (test list &key key)
  #!+sb-doc
  "Return tail of LIST beginning with first element satisfying TEST."
  (do ((list list (Cdr list)))
      ((endp list) nil)
    (if (funcall test (apply-key key (car list)))
	(return list))))

(defun member-if-not (test list &key key)
  #!+sb-doc
  "Return tail of LIST beginning with first element not satisfying TEST."
  (do ((list list (cdr list)))
      ((endp list) ())
    (if (not (funcall test (apply-key key (car list))))
	(return list))))

(defun tailp (object list)
  #!+sb-doc
  "Return true if OBJECT is the same as some tail of LIST, otherwise
   returns false. LIST must be a proper list or a dotted list."
  (do ((list list (cdr list)))
      ((atom list) (eql list object))
    (if (eql object list)
	(return t))))

(defun adjoin (item list &key key (test #'eql) (test-not nil notp))
  #!+sb-doc
  "Add ITEM to LIST unless it is already a member"
  (declare (inline member))
  (if (let ((key-val (apply-key key item)))
	(if notp
	    (member key-val list :test-not test-not :key key)
	    (member key-val list :test test :key key)))
      list
      (cons item list)))

;;; This function assumes list2 is the result, adding to it from list1 as
;;; necessary. List2 must initialize the result value, so the call to MEMBER
;;; will apply the test to the elements from list1 and list2 in the correct
;;; order.
(defun union (list1 list2 &key key (test #'eql testp) (test-not nil notp))
  #!+sb-doc
  "Return the union of LIST1 and LIST2."
  (declare (inline member))
  (when (and testp notp) (error "Test and test-not both supplied."))
  (let ((res list2))
    (dolist (elt list1)
      (unless (with-set-keys (member (apply-key key elt) list2))
	(push elt res)))
    res))

;;; Destination and source are SETF-able and many-evaluable. Set the
;;; SOURCE to the CDR, and "cons" the 1st elt of source to DESTINATION.
;;;
;;; FIXME: needs a more mnemonic name
(defmacro steve-splice (source destination)
  `(let ((temp ,source))
     (setf ,source (cdr ,source)
	   (cdr temp) ,destination
	   ,destination temp)))

(defun nunion (list1 list2 &key key (test #'eql testp) (test-not nil notp))
  #!+sb-doc
  "Destructively return the union of LIST1 and LIST2."
  (declare (inline member))
  (if (and testp notp)
      (error ":TEST and :TEST-NOT were both supplied."))
  (let ((res list2)
	(list1 list1))
    (do ()
	((endp list1))
      (if (not (with-set-keys (member (apply-key key (car list1)) list2)))
	  (steve-splice list1 res)
	  (setf list1 (cdr list1))))
    res))

(defun intersection (list1 list2 &key key
			   (test #'eql testp) (test-not nil notp))
  #!+sb-doc
  "Return the intersection of LIST1 and LIST2."
  (declare (inline member))
  (if (and testp notp)
      (error "Test and test-not both supplied."))
  (let ((res nil))
    (dolist (elt list1)
      (if (with-set-keys (member (apply-key key elt) list2))
	  (push elt res)))
    res))

(defun nintersection (list1 list2 &key key
			    (test #'eql testp) (test-not nil notp))
  #!+sb-doc
  "Destructively return the intersection of LIST1 and LIST2."
  (declare (inline member))
  (if (and testp notp)
      (error "Test and test-not both supplied."))
  (let ((res nil)
	(list1 list1))
    (do () ((endp list1))
      (if (with-set-keys (member (apply-key key (car list1)) list2))
	  (steve-splice list1 res)
	  (setq list1 (Cdr list1))))
    res))

(defun set-difference (list1 list2 &key key
			     (test #'eql testp) (test-not nil notp))
  #!+sb-doc
  "Return the elements of LIST1 which are not in LIST2."
  (declare (inline member))
  (if (and testp notp)
      (error "Test and test-not both supplied."))
  (if (null list2)
      list1
      (let ((res nil))
	(dolist (elt list1)
	  (if (not (with-set-keys (member (apply-key key elt) list2)))
	      (push elt res)))
	res)))

(defun nset-difference (list1 list2 &key key
			      (test #'eql testp) (test-not nil notp))
  #!+sb-doc
  "Destructively return the elements of LIST1 which are not in LIST2."
  (declare (inline member))
  (if (and testp notp)
      (error "Test and test-not both supplied."))
  (let ((res nil)
	(list1 list1))
    (do () ((endp list1))
      (if (not (with-set-keys (member (apply-key key (car list1)) list2)))
	  (steve-splice list1 res)
	  (setq list1 (cdr list1))))
    res))

(defun set-exclusive-or (list1 list2 &key key
			       (test #'eql testp) (test-not nil notp))
  #!+sb-doc
  "Return new list of elements appearing exactly once in LIST1 and LIST2."
  (declare (inline member))
  (let ((result nil))
    (dolist (elt list1)
      (unless (with-set-keys (member (apply-key key elt) list2))
	(setq result (cons elt result))))
    (dolist (elt list2)
      (unless (with-set-keys (member (apply-key key elt) list1))
	(setq result (cons elt result))))
    result))

;;; The outer loop examines list1 while the inner loop examines list2.
;;; If an element is found in list2 "equal" to the element in list1,
;;; both are spliced out. When the end of list1 is reached, what is
;;; left of list2 is tacked onto what is left of list1. The splicing
;;; operation ensures that the correct operation is performed
;;; depending on whether splice is at the top of the list or not
(defun nset-exclusive-or (list1 list2 &key (test #'eql) (test-not nil notp)
				key)
  #!+sb-doc
  "Destructively return a list with elements which appear but once in LIST1
   and LIST2."
  (do ((list1 list1)
       (list2 list2)
       (x list1 (cdr x))
       (splicex ()))
      ((endp x)
       (if (null splicex)
	   (setq list1 list2)
	   (rplacd splicex list2))
       list1)
    (do ((y list2 (cdr y))
	 (splicey ()))
	((endp y) (setq splicex x))
      (cond ((let ((key-val-x (apply-key key (car x)))
		   (key-val-y (apply-key key (Car y))))
	       (if notp
		   (not (funcall test-not key-val-x key-val-y))
		   (funcall test key-val-x key-val-y)))
	     (if (null splicex)
		 (setq list1 (cdr x))
		 (rplacd splicex (cdr x)))
	     (if (null splicey)
		 (setq list2 (cdr y))
		 (rplacd splicey (cdr y)))
	     (return ()))			; assume lists are really sets
	    (t (setq splicey y))))))

(defun subsetp (list1 list2 &key key (test #'eql testp) (test-not nil notp))
  #!+sb-doc
  "Return T if every element in LIST1 is also in LIST2."
  (declare (inline member))
  (dolist (elt list1)
    (unless (with-set-keys (member (apply-key key elt) list2))
      (return-from subsetp nil)))
  T)

;;; functions that operate on association lists

(defun acons (key datum alist)
  #!+sb-doc
  "Construct a new alist by adding the pair (KEY . DATUM) to ALIST."
  (cons (cons key datum) alist))

(defun pairlis (keys data &optional (alist '()))
  #!+sb-doc
  "Construct an association list from KEYS and DATA (adding to ALIST)."
  (do ((x keys (cdr x))
       (y data (cdr y)))
      ((and (endp x) (endp y)) alist)
    (if (or (endp x) (endp y))
	(error "The lists of keys and data are of unequal length."))
    (setq alist (acons (car x) (car y) alist))))

;;; This is in the run-time environment (i.e. not wrapped in
;;; EVAL-WHEN (COMPILE EVAL)) because these guys can be inline
;;; expanded.
(defmacro assoc-guts (test-guy)
  `(do ((alist alist (cdr alist)))
       ((endp alist))
     ;; FIXME: would be clearer as (WHEN (AND ..) ..)
     (if (car alist)
	 (if ,test-guy (return (car alist))))))

(defun assoc (item alist &key key test test-not)
  #!+sb-doc
  "Returns the cons in ALIST whose car is equal (by a given test or EQL) to
   the ITEM."
  ;; FIXME: Shouldn't there be a check for existence of both TEST and TEST-NOT?
  (cond (test
	 (if key
	     (assoc-guts (funcall test item (funcall key (caar alist))))
	     (assoc-guts (funcall test item (caar alist)))))
	(test-not
	 (if key
	     (assoc-guts (not (funcall test-not item
				       (funcall key (caar alist)))))
	     (assoc-guts (not (funcall test-not item (caar alist))))))
	(t
	 (if key
	     (assoc-guts (eql item (funcall key (caar alist))))
	     (assoc-guts (eql item (caar alist)))))))

(defun assoc-if (predicate alist &key key)
  #!+sb-doc
  "Returns the first cons in alist whose car satisfies the Predicate. If
   key is supplied, apply it to the car of each cons before testing."
  (if key
      (assoc-guts (funcall predicate (funcall key (caar alist))))
      (assoc-guts (funcall predicate (caar alist)))))

(defun assoc-if-not (predicate alist &key key)
  #!+sb-doc
  "Returns the first cons in ALIST whose car does not satisfy the PREDICATE.
  If KEY is supplied, apply it to the car of each cons before testing."
  (if key
      (assoc-guts (not (funcall predicate (funcall key (caar alist)))))
      (assoc-guts (not (funcall predicate (caar alist))))))

(defun rassoc (item alist &key key test test-not)
  (declare (list alist))
  #!+sb-doc
  "Returns the cons in ALIST whose cdr is equal (by a given test or EQL) to
   the ITEM."
  (cond (test
	 (if key
	     (assoc-guts (funcall test item (funcall key (cdar alist))))
	     (assoc-guts (funcall test item (cdar alist)))))
	(test-not
	 (if key
	     (assoc-guts (not (funcall test-not item
				       (funcall key (cdar alist)))))
	     (assoc-guts (not (funcall test-not item (cdar alist))))))
	(t
	 (if key
	     (assoc-guts (eql item (funcall key (cdar alist))))
	     (assoc-guts (eql item (cdar alist)))))))

(defun rassoc-if (predicate alist &key key)
  #!+sb-doc
  "Returns the first cons in alist whose cdr satisfies the Predicate. If key
  is supplied, apply it to the cdr of each cons before testing."
  (if key
      (assoc-guts (funcall predicate (funcall key (cdar alist))))
      (assoc-guts (funcall predicate (cdar alist)))))

(defun rassoc-if-not (predicate alist &key key)
  #!+sb-doc
  "Returns the first cons in alist whose cdr does not satisfy the Predicate.
  If key is supplied, apply it to the cdr of each cons before testing."
  (if key
      (assoc-guts (not (funcall predicate (funcall key (cdar alist)))))
      (assoc-guts (not (funcall predicate (cdar alist))))))

;;;; mapping functions

(defun map1 (function original-arglists accumulate take-car)
  #!+sb-doc
  "This function is called by mapc, mapcar, mapcan, mapl, maplist, and mapcon.
  It Maps function over the arglists in the appropriate way. It is done when any
  of the arglists runs out. Until then, it CDRs down the arglists calling the
  function and accumulating results as desired."

  (let* ((arglists (copy-list original-arglists))
	 (ret-list (list nil))
	 (temp ret-list))
    (do ((res nil)
	 (args '() '()))
	((dolist (x arglists nil) (if (null x) (return t)))
	 (if accumulate
	     (cdr ret-list)
	     (car original-arglists)))
      (do ((l arglists (cdr l)))
	  ((null l))
	(push (if take-car (caar l) (car l)) args)
	(setf (car l) (cdar l)))
      (setq res (apply function (nreverse args)))
      (case accumulate
	(:nconc (setq temp (last (nconc temp res))))
	(:list (rplacd temp (list res))
	       (setq temp (cdr temp)))))))

(defun mapc (function list &rest more-lists)
  #!+sb-doc
  "Applies fn to successive elements of lists, returns its second argument."
  (map1 function (cons list more-lists) nil t))

(defun mapcar (function list &rest more-lists)
  #!+sb-doc
  "Applies fn to successive elements of list, returns list of results."
  (map1 function (cons list more-lists) :list t))

(defun mapcan (function list &rest more-lists)
  #!+sb-doc
  "Applies fn to successive elements of list, returns NCONC of results."
  (map1 function (cons list more-lists) :nconc t))

(defun mapl (function list &rest more-lists)
  #!+sb-doc
  "Applies fn to successive CDRs of list, returns ()."
  (map1 function (cons list more-lists) nil nil))

(defun maplist (function list &rest more-lists)
  #!+sb-doc
  "Applies fn to successive CDRs of list, returns list of results."
  (map1 function (cons list more-lists) :list nil))

(defun mapcon (function list &rest more-lists)
  #!+sb-doc
  "Applies fn to successive CDRs of lists, returns NCONC of results."
  (map1 function (cons list more-lists) :nconc nil))
