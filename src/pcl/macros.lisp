;;;; macros, global variable definitions, and other miscellaneous support stuff
;;;; used by the rest of the PCL subsystem

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is derived from software originally released by Xerox
;;;; Corporation. Copyright and release statements follow. Later modifications
;;;; to the software are in the public domain and are provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for more
;;;; information.

;;;; copyright information from original PCL sources:
;;;;
;;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;;; All rights reserved.
;;;;
;;;; Use and copying of this software and preparation of derivative works based
;;;; upon this software are permitted. Any distribution of this software or
;;;; derivative works must comply with all applicable United States export
;;;; control laws.
;;;;
;;;; This software is made available AS IS, and Xerox Corporation makes no
;;;; warranty about the software, its performance or its conformity to any
;;;; specification.

(in-package "SB-PCL")

(declaim (declaration
	  ;; FIXME: Since none of these are supported in SBCL, the
	  ;; declarations using them are just noise now that this is
	  ;; not a portable package any more, and could be deleted.
	  values			; I use this so that Zwei can remind
					; me what values a function returns.
	  arglist			; Tells me what the pretty arglist
					; of something (which probably takes
					; &REST args) is.
	  indentation			; Tells ZWEI how to indent things
					; like DEFCLASS.
	  class
	  variable-rebinding
	  pcl-fast-call
	  method-name
	  method-lambda-list))

;;; These are age-old functions which CommonLisp cleaned-up away. They probably
;;; exist in other packages in all CommonLisp implementations, but I will leave
;;; it to the compiler to optimize into calls to them.
;;;
;;; FIXME: MEMQ, ASSQ, and DELQ are already defined in SBCL, and we should
;;; use those. POSQ and NEQ aren't defined in SBCL, and are used too often
;;; in PCL to make it appealing to hand expand all uses and then delete
;;; the macros, so they should be boosted up to SBCL to stand by MEMQ,
;;; ASSQ, and DELQ.
(defmacro memq (item list) `(member ,item ,list :test #'eq))
(defmacro assq (item list) `(assoc ,item ,list :test #'eq))
(defmacro delq (item list) `(delete ,item ,list :test #'eq))
(defmacro posq (item list) `(position ,item ,list :test #'eq))
(defmacro neq (x y) `(not (eq ,x ,y)))

;;; Rename these to CONSTANTLY-T, CONSTANTLY-NIL, and CONSTANTLY-0
;;; and boost them up to SB-INT.
(defun true (&rest ignore) (declare (ignore ignore)) t)
(defun false (&rest ignore) (declare (ignore ignore)) nil)
(defun zero (&rest ignore) (declare (ignore ignore)) 0)

;;; ONCE-ONLY does the same thing as it does in zetalisp. I should have just
;;; lifted it from there but I am honest. Not only that but this one is
;;; written in Common Lisp. I feel a lot like bootstrapping, or maybe more
;;; like rebuilding Rome.
;;;
;;; FIXME: We should only need one ONCE-ONLY in CMU CL, and there's one
;;; in SB-EXT already (presently to go in SB-INT). Can we use
;;; only one of these in both places?
(defmacro once-only (vars &body body)
  (let ((gensym-var (gensym))
	(run-time-vars (gensym))
	(run-time-vals (gensym))
	(expand-time-val-forms ()))
    (dolist (var vars)
      (push `(if (or (symbolp ,var)
		     (numberp ,var)
		     (and (listp ,var)
			  (member (car ,var) '(quote function))))
		 ,var
		 (let ((,gensym-var (gensym)))
		   (push ,gensym-var ,run-time-vars)
		   (push ,var ,run-time-vals)
		   ,gensym-var))
	    expand-time-val-forms))
    `(let* (,run-time-vars
	    ,run-time-vals
	    (wrapped-body
	      (let ,(mapcar #'list vars (reverse expand-time-val-forms))
		,@body)))
       `(let ,(mapcar #'list (reverse ,run-time-vars)
			     (reverse ,run-time-vals))
	  ,wrapped-body))))

;;; FIXME: This looks like SBCL's PARSE-BODY, and should be shared.
(eval-when (:compile-toplevel :load-toplevel :execute)
(defun extract-declarations (body &optional environment)
  ;;(declare (values documentation declarations body))
  (let (documentation
	declarations
	form)
    (when (and (stringp (car body))
	       (cdr body))
      (setq documentation (pop body)))
    (block outer
      (loop
	(when (null body) (return-from outer nil))
	(setq form (car body))
	(when (block inner
		(loop (cond ((not (listp form))
			     (return-from outer nil))
			    ((eq (car form) 'declare)
			     (return-from inner 't))
			    (t
			     (multiple-value-bind (newform macrop)
				  (macroexpand-1 form environment)
			       (if (or (not (eq newform form)) macrop)
				   (setq form newform)
				 (return-from outer nil)))))))
	  (pop body)
	  (dolist (declaration (cdr form))
	    (push declaration declarations)))))
    (values documentation
	    (and declarations `((declare ,.(nreverse declarations))))
	    body)))
) ; EVAL-WHEN

;;; FIXME: This seems to only be used to get 'METHOD-NAME and
;;; METHOD-LAMBDA-LIST declarations. They aren't ANSI. Are they important?
(defun get-declaration (name declarations &optional default)
  (dolist (d declarations default)
    (dolist (form (cdr d))
      (when (and (consp form) (eq (car form) name))
	(return-from get-declaration (cdr form))))))

(defmacro collecting-once (&key initial-value)
   `(let* ((head ,initial-value)
	   (tail ,(and initial-value `(last head))))
	  (values #'(lambda (value)
			   (if (null head)
			       (setq head (setq tail (list value)))
			       (unless (memq value head)
				 (setq tail
				       (cdr (rplacd tail (list value)))))))
		  #'(lambda nil head))))

(defmacro doplist ((key val) plist &body body &environment env)
  (multiple-value-bind (doc decls bod)
      (extract-declarations body env)
    (declare (ignore doc))
    `(let ((.plist-tail. ,plist) ,key ,val)
       ,@decls
       (loop (when (null .plist-tail.) (return nil))
	     (setq ,key (pop .plist-tail.))
	     (when (null .plist-tail.)
	       (error "malformed plist, odd number of elements"))
	     (setq ,val (pop .plist-tail.))
	     (progn ,@bod)))))

(defmacro dolist-carefully ((var list improper-list-handler) &body body)
  `(let ((,var nil)
	 (.dolist-carefully. ,list))
     (loop (when (null .dolist-carefully.) (return nil))
	   (if (consp .dolist-carefully.)
	       (progn
		 (setq ,var (pop .dolist-carefully.))
		 ,@body)
	       (,improper-list-handler)))))

;;; FIXME: Do we really need this? It seems to be used only
;;; for class names. Why not just the default ALL-CAPS?
(defun capitalize-words (string &optional (dashes-p t))
  (let ((string (copy-seq (string string))))
    (declare (string string))
    (do* ((flag t flag)
	  (length (length string) length)
	  (char nil char)
	  (i 0 (+ i 1)))
	 ((= i length) string)
      (setq char (elt string i))
      (cond ((both-case-p char)
	     (if flag
		 (and (setq flag (lower-case-p char))
		      (setf (elt string i) (char-upcase char)))
		 (and (not flag) (setf (elt string i) (char-downcase char))))
	     (setq flag nil))
	    ((char-equal char #\-)
	     (setq flag t)
	     (unless dashes-p (setf (elt string i) #\space)))
	    (t (setq flag nil))))))

;;;; FIND-CLASS
;;;;
;;;; This is documented in the CLOS specification.
;;;; KLUDGE: Except that SBCL deviates from the spec by having CL:FIND-CLASS
;;;; distinct from PCL:FIND-CLASS, alas. -- WHN 19991203

(defvar *find-class* (make-hash-table :test 'eq))

(defun make-constant-function (value)
  #'(lambda (object)
      (declare (ignore object))
      value))

(defun function-returning-nil (x)
  (declare (ignore x))
  nil)

(defun function-returning-t (x)
  (declare (ignore x))
  t)

(defmacro find-class-cell-class (cell)
  `(car ,cell))

(defmacro find-class-cell-predicate (cell)
  `(cadr ,cell))

(defmacro find-class-cell-make-instance-function-keys (cell)
  `(cddr ,cell))

(defmacro make-find-class-cell (class-name)
  (declare (ignore class-name))
  '(list* nil #'function-returning-nil nil))

(defun find-class-cell (symbol &optional dont-create-p)
  (or (gethash symbol *find-class*)
      (unless dont-create-p
	(unless (legal-class-name-p symbol)
	  (error "~S is not a legal class name." symbol))
	(setf (gethash symbol *find-class*) (make-find-class-cell symbol)))))

(defvar *create-classes-from-internal-structure-definitions-p* t)

(defun find-class-from-cell (symbol cell &optional (errorp t))
  (or (find-class-cell-class cell)
      (and *create-classes-from-internal-structure-definitions-p*
	   (structure-type-p symbol)
	   (find-structure-class symbol))
      (cond ((null errorp) nil)
	    ((legal-class-name-p symbol)
	     (error "There is no class named ~S." symbol))
	    (t
	     (error "~S is not a legal class name." symbol)))))

(defun find-class-predicate-from-cell (symbol cell &optional (errorp t))
  (unless (find-class-cell-class cell)
    (find-class-from-cell symbol cell errorp))
  (find-class-cell-predicate cell))

(defun legal-class-name-p (x)
  (and (symbolp x)
       (not (keywordp x))))

(defun find-class (symbol &optional (errorp t) environment)
  (declare (ignore environment))
  (find-class-from-cell symbol
			(find-class-cell symbol errorp)
			errorp))

(defun find-class-predicate (symbol &optional (errorp t) environment)
  (declare (ignore environment))
  (find-class-predicate-from-cell symbol
				  (find-class-cell symbol errorp)
				  errorp))

;;; This DEFVAR was originally in defs.lisp, now moved here.
;;;
;;; Possible values are NIL, EARLY, BRAID, or COMPLETE.
;;;
;;; KLUDGE: This should probably become
;;;   (DECLAIM (TYPE (MEMBER NIL :EARLY :BRAID :COMPLETE) *BOOT-STATE*))
(defvar *boot-state* nil)

;;; Note that in SBCL as in CMU CL,
;;;   COMMON-LISP:FIND-CLASS /= SB-PCL:FIND-CLASS.
;;; (Yes, this is a KLUDGE!)
(define-compiler-macro find-class (&whole form
				   symbol &optional (errorp t) environment)
  (declare (ignore environment))
  (if (and (constantp symbol)
	   (legal-class-name-p (eval symbol))
	   (constantp errorp)
	   (member *boot-state* '(braid complete)))
      (let ((symbol (eval symbol))
	    (errorp (not (null (eval errorp))))
	    (class-cell (make-symbol "CLASS-CELL")))	
	`(let ((,class-cell (load-time-value (find-class-cell ',symbol))))
	   (or (find-class-cell-class ,class-cell)
	       ,(if errorp
		    `(find-class-from-cell ',symbol ,class-cell t)
		    `(and (sb-kernel:class-cell-class
			   ',(sb-kernel:find-class-cell symbol))
			  (find-class-from-cell ',symbol ,class-cell nil))))))
      form))

;;; FIXME: These #-SETF forms are pretty ugly. Could they please go away?
#-setf
(defsetf find-class (symbol &optional (errorp t) environment) (new-value)
  (declare (ignore errorp environment))
  `(SETF\ SB-PCL\ FIND-CLASS ,new-value ,symbol))

(defun #-setf SETF\ SB-PCL\ FIND-CLASS #+setf (setf find-class) (new-value
							       symbol)
  (if (legal-class-name-p symbol)
      (let ((cell (find-class-cell symbol)))
	(setf (find-class-cell-class cell) new-value)
	(when (or (eq *boot-state* 'complete)
		  (eq *boot-state* 'braid))
	  (when (and new-value (class-wrapper new-value))
	    (setf (find-class-cell-predicate cell)
		  (symbol-function (class-predicate-name new-value))))
	  (when (and new-value (not (forward-referenced-class-p new-value)))

	    (dolist (keys+aok (find-class-cell-make-instance-function-keys cell))
	      (update-initialize-info-internal
	       (initialize-info new-value (car keys+aok) nil (cdr keys+aok))
	       'make-instance-function))))
	new-value)
      (error "~S is not a legal class name." symbol)))

#-setf
(defsetf find-class-predicate (symbol &optional (errorp t) environment) (new-value)
  (declare (ignore errorp environment))
  `(SETF\ SB-PCL\ FIND-CLASS-PREDICATE ,new-value ,symbol))

(defun #-setf SETF\ SB-PCL\ FIND-CLASS-PREDICATE
       #+setf (setf find-class-predicate)
    (new-value symbol)
  (if (legal-class-name-p symbol)
      (setf (find-class-cell-predicate (find-class-cell symbol)) new-value)
      (error "~S is not a legal class name." symbol)))

(defun find-wrapper (symbol)
  (class-wrapper (find-class symbol)))

(defmacro gathering1 (gatherer &body body)
  `(gathering ((.gathering1. ,gatherer))
     (macrolet ((gather1 (x) `(gather ,x .gathering1.)))
       ,@body)))

(defmacro vectorizing (&key (size 0))
  `(let* ((limit ,size)
	  (result (make-array limit))
	  (index 0))
     (values #'(lambda (value)
		 (if (= index limit)
		     (error "vectorizing more elements than promised")
		     (progn
		       (setf (svref result index) value)
		       (incf index)
		       value)))
	     #'(lambda () result))))

;;; These are augmented definitions of list-elements and list-tails from
;;; iterate.lisp. These versions provide the extra :by keyword which can
;;; be used to specify the step function through the list.
(defmacro *list-elements (list &key (by #'cdr))
  `(let ((tail ,list))
     #'(lambda (finish)
	 (if (endp tail)
	     (funcall finish)
	     (prog1 (car tail)
		    (setq tail (funcall ,by tail)))))))

(defmacro *list-tails (list &key (by #'cdr))
   `(let ((tail ,list))
      #'(lambda (finish)
	  (prog1 (if (endp tail)
		     (funcall finish)
		     tail)
		 (setq tail (funcall ,by tail))))))

(defmacro function-funcall (form &rest args)
  `(funcall (the function ,form) ,@args))

(defmacro function-apply (form &rest args)
  `(apply (the function ,form) ,@args))

;;;; various nastiness to work around nonstandardness of SETF when PCL
;;;; was written

;;; Convert a function name to its standard SETF function name. We
;;; have to do this hack because not all Common Lisps have yet
;;; converted to having SETF function specs.
;;;
;;; KLUDGE: We probably don't have to do this any more. But in Debian
;;; cmucl 2.4.8 the :SETF feature isn't set (?). Perhaps it's because of
;;; the comment ca. 10 lines down about how the built-in setf mechanism
;;; takes a hash table lookup each time? It would be nice to go one
;;; way or another on this, perhaps some benchmarking would be in order..
;;; (Oh, more info: In debian src/pcl/notes.text, which looks like stale
;;; documentation from 1992, it says TO DO: When CMU CL improves its
;;; SETF handling, remove the comment in macros.lisp beginning the line
;;; #+CMU (PUSHNEW :SETF *FEATURES*). So since CMU CL's (and now SBCL's)
;;; SETF handling seems OK to me these days, there's a fairly decent chance
;;; this would work.) -- WHN 19991203
;;;
;;; In a port that does have SETF function specs you can use those just by
;;; making the obvious simple changes to these functions. The rest of PCL
;;; believes that there are function names like (SETF <foo>), this is the
;;; only place that knows about this hack.
(eval-when (:compile-toplevel :load-toplevel :execute)
; In 15e (and also 16c), using the built-in SETF mechanism costs
; a hash table lookup every time a SETF function is called.
; Uncomment the next line to use the built in SETF mechanism.
;#+cmu (pushnew :setf *features*)
) ; EVAL-WHEN

(eval-when (:compile-toplevel :load-toplevel :execute)

#-setf
(defvar *setf-function-names* (make-hash-table :size 200 :test 'eq))

(defun get-setf-function-name (name)
  #+setf `(setf ,name)
  #-setf
  (or (gethash name *setf-function-names*)
      (setf (gethash name *setf-function-names*)
	    (let ((pkg (symbol-package name)))
	      (if pkg
		  (intern (format nil
				  "SETF ~A ~A"
				  (package-name pkg)
				  (symbol-name name))
			  *pcl-package*)
		  (make-symbol (format nil "SETF ~A" (symbol-name name))))))))

;;; Call this to define a setf macro for a function with the same behavior as
;;; specified by the SETF function cleanup proposal. Specifically, this will
;;; cause: (SETF (FOO a b) x) to expand to (|SETF FOO| x a b).
;;;
;;; do-standard-defsetf		  A macro interface for use at top level
;;;				      in files. Unfortunately, users may
;;;				      have to use this for a while.
;;;
;;; do-standard-defsetfs-for-defclass    A special version called by defclass.
;;;
;;; do-standard-defsetf-1		A functional interface called by the
;;;				      above, defmethod and defgeneric.
;;;				      Since this is all a crock anyways,
;;;				      users are free to call this as well.
;;;
;;; FIXME: Once we fix up SETF, a lot of stuff around here should evaporate.
(defmacro do-standard-defsetf (&rest function-names)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (dolist (fn-name ',function-names) (do-standard-defsetf-1 fn-name))))

(defun do-standard-defsetfs-for-defclass (accessors)
  (dolist (name accessors) (do-standard-defsetf-1 name)))

(defun do-standard-defsetf-1 (function-name)
  #+setf
  (declare (ignore function-name))
  #+setf nil
  #-setf
  (unless (and (setfboundp function-name)
	       (get function-name 'standard-setf))
    (setf (get function-name 'standard-setf) t)
    (let* ((setf-function-name (get-setf-function-name function-name)))
      (eval `(defsetf ,function-name (&rest accessor-args) (new-value)
	       (let* ((bindings (mapcar #'(lambda (x) `(,(gensym) ,x)) accessor-args))
		      (vars (mapcar #'car bindings)))
		  `(let ,bindings
		      (,',setf-function-name ,new-value ,@vars))))))))

(defun setfboundp (symbol)
  (fboundp `(setf ,symbol)))

) ; EVAL-WHEN

;;; PCL, like user code, must endure the fact that we don't have a
;;; properly working SETF. Many things work because they get mentioned
;;; by a DEFCLASS or DEFMETHOD before they are used, but others have
;;; to be done by hand.
;;;
;;; FIXME: We don't have to do this stuff any more, do we?
(do-standard-defsetf
  class-wrapper				 ;***
  generic-function-name
  method-function-plist
  method-function-get
  plist-value
  object-plist
  gdefinition
  slot-value-using-class)

(defsetf slot-value set-slot-value)
