;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(declaim (special *read-suppress* *standard-readtable* *bq-vector-flag*))

;;; FIXME: Is it standard to ignore numeric args instead of raising errors?
(defun ignore-numarg (sub-char numarg)
  (when numarg
    (warn "A numeric argument was ignored in #~W~A." numarg sub-char)))

;;;; reading arrays and vectors: the #(, #*, and #A readmacros

(defun sharp-left-paren (stream ignore length)
  (declare (ignore ignore) (special *backquote-count*))
  (let* ((list (read-list stream nil))
	 (listlength (length list)))
    (declare (list list)
	     (fixnum listlength))
    (cond (*read-suppress* nil)
	  ((zerop *backquote-count*)
	   (if length
	       (cond ((> listlength (the fixnum length))
		      (%reader-error
		       stream
		       "vector longer than specified length: #~S~S"
		       length list))
		     (t
		      (fill (the simple-vector
				 (replace (the simple-vector
					       (make-array length))
					  list))
			    (car (last list))
			    :start listlength)))
	       (coerce list 'vector)))
	  (t (cons *bq-vector-flag* list)))))

(defun sharp-star (stream ignore numarg)
  (declare (ignore ignore))
  (multiple-value-bind (bstring escape-appearedp) (read-extended-token stream)
    (declare (simple-string bstring))
    (cond (*read-suppress* nil)
	  (escape-appearedp
	   (%reader-error stream "An escape character appeared after #*"))
	  ((and numarg (zerop (length bstring)) (not (zerop numarg)))
	   (%reader-error
	    stream
	    "You have to give a little bit for non-zero #* bit-vectors."))
	  ((or (null numarg) (>= (the fixnum numarg) (length bstring)))
	   (let* ((len1 (length bstring))
		  (last1 (1- len1))
		  (len2 (or numarg len1))
		  (bvec (make-array len2 :element-type 'bit
				    :initial-element 0)))
	     (declare (fixnum len1 last1 len2))
	     (do ((i 0 (1+ i))
		  (char ()))
		 ((= i len2))
	       (declare (fixnum i))
	       (setq char (elt bstring (if (< i len1) i last1)))
	       (setf (elt bvec i)
		     (cond ((char= char #\0) 0)
			   ((char= char #\1) 1)
			   (t
			    (%reader-error
			     stream
			     "illegal element given for bit-vector: ~S"
			     char)))))
	     bvec))
	  (t
	   (%reader-error stream
			 "Bit vector is longer than specified length #~A*~A"
			 numarg bstring)))))

(defun sharp-A (stream ignore dimensions)
  (declare (ignore ignore))
  (when *read-suppress*
    (read stream t nil t)
    (return-from sharp-A nil))
  (unless dimensions (%reader-error stream "no dimensions argument to #A"))
  (collect ((dims))
    (let* ((contents (read stream t nil t))
	   (seq contents))
      (dotimes (axis dimensions
		     (make-array (dims) :initial-contents contents))
	(unless (typep seq 'sequence)
	  (%reader-error stream
			 "#~WA axis ~W is not a sequence:~%  ~S"
			 dimensions axis seq))
	(let ((len (length seq)))
	  (dims len)
	  (unless (or (= axis (1- dimensions))
		      ;; ANSI: "If some dimension of the array whose
		      ;; representation is being parsed is found to be
		      ;; 0, all dimensions to the right (i.e., the
		      ;; higher numbered dimensions) are also
		      ;; considered to be 0."
		      (= len 0))
	    (setq seq (elt seq 0))))))))

;;;; reading structure instances: the #S readmacro

(defun sharp-S (stream sub-char numarg)
  (ignore-numarg sub-char numarg)
  (when *read-suppress*
    (read stream t nil t)
    (return-from sharp-S nil))
  (let ((body (if (char= (read-char stream t) #\( )
		  (read-list stream nil)
		  (%reader-error stream "non-list following #S"))))
    (unless (listp body)
      (%reader-error stream "non-list following #S: ~S" body))
    (unless (symbolp (car body))
      (%reader-error stream "Structure type is not a symbol: ~S" (car body)))
    (let ((classoid (find-classoid (car body) nil)))
      (unless (typep classoid 'structure-classoid)
	(%reader-error stream "~S is not a defined structure type."
		       (car body)))
      (let ((def-con (dd-default-constructor
		      (layout-info
		       (classoid-layout classoid)))))
	(unless def-con
	  (%reader-error
	   stream "The ~S structure does not have a default constructor."
	   (car body)))
	(when (and (atom (rest body))
		   (not (null (rest body))))
	  (%reader-error
	   stream "improper list for #S: ~S." body))
	(apply (fdefinition def-con)
	       (loop for tail on (rest body) by #'cddr
		     with slot-name = (and (consp tail) (car tail))
		     do (progn
			  (when (null (cdr tail))
			    (%reader-error
			     stream
			     "the arglist for the ~S constructor in #S ~
                              has an odd length: ~S."
			     (car body) (rest body)))
			  (when (or (atom (cdr tail))
				    (and (atom (cddr tail))
					 (not (null (cddr tail)))))
			    (%reader-error
			     stream
			     "the arglist for the ~S constructor in #S ~
                              is improper: ~S."
			     (car body) (rest body)))
			  (when (not (typep (car tail) 'string-designator))
			    (%reader-error
			     stream
			     "a slot name in #S is not a string ~
                              designator: ~S."
			     slot-name))
			  (when (not (keywordp slot-name))
			    (style-warn "in #S ~S, the use of non-keywords ~
                                         as slot specifiers is deprecated: ~S."
					(car body) slot-name)))
		     collect (intern (string (car tail)) *keyword-package*)
		     collect (cadr tail)))))))

;;;; reading numbers: the #B, #C, #O, #R, and #X readmacros

(defun sharp-B (stream sub-char numarg)
  (ignore-numarg sub-char numarg)
  (sharp-r stream sub-char 2))

(defun sharp-C (stream sub-char numarg)
  (ignore-numarg sub-char numarg)
  ;; The next thing had better be a list of two numbers.
  (let ((cnum (read stream t nil t)))
    (when *read-suppress* (return-from sharp-c nil))
    (if (and (listp cnum) (= (length cnum) 2))
	(complex (car cnum) (cadr cnum))
	(%reader-error stream "illegal complex number format: #C~S" cnum))))

(defun sharp-O (stream sub-char numarg)
  (ignore-numarg sub-char numarg)
  (sharp-r stream sub-char 8))

(defun sharp-R (stream sub-char radix)
  (cond (*read-suppress*
	 (read-extended-token stream)
	 nil)
	((not radix)
	 (%reader-error stream "radix missing in #R"))
	((not (<= 2 radix 36))
	 (%reader-error stream "illegal radix for #R: ~D." radix))
	(t
	 (let ((res (let ((*read-base* radix))
		      (read stream t nil t))))
	   (unless (typep res 'rational)
	     (%reader-error stream
			    "#~A (base ~D.) value is not a rational: ~S."
			    sub-char
			    radix
			    res))
	   res))))

(defun sharp-X (stream sub-char numarg)
  (ignore-numarg sub-char numarg)
  (sharp-r stream sub-char 16))

;;;; reading circular data: the #= and ## readmacros

;;; objects already seen by CIRCLE-SUBST
(defvar *sharp-equal-circle-table*)
(declaim (type hash-table *sharp-equal-circle-table*))

;; This function is kind of like NSUBLIS, but checks for circularities and
;; substitutes in arrays and structures as well as lists. The first arg is an
;; alist of the things to be replaced assoc'd with the things to replace them.
(defun circle-subst (old-new-alist tree)
  (cond ((not (typep tree
		     '(or cons (array t) structure-object standard-object)))
	 (let ((entry (find tree old-new-alist :key #'second)))
	   (if entry (third entry) tree)))
	((null (gethash tree *sharp-equal-circle-table*))
	 (setf (gethash tree *sharp-equal-circle-table*) t)
	 (cond ((typep tree '(or structure-object standard-object))
		(do ((i 1 (1+ i))
		     (end (%instance-length tree)))
		    ((= i end))
		  (let* ((old (%instance-ref tree i))
			 (new (circle-subst old-new-alist old)))
		    (unless (eq old new)
		      (setf (%instance-ref tree i) new)))))
	       ((arrayp tree)
		(with-array-data ((data tree) (start) (end))
		  (declare (fixnum start end))
		  (do ((i start (1+ i)))
		      ((>= i end))
		    (let* ((old (aref data i))
			   (new (circle-subst old-new-alist old)))
		      (unless (eq old new)
			(setf (aref data i) new))))))
	       (t
		(let ((a (circle-subst old-new-alist (car tree)))
		      (d (circle-subst old-new-alist (cdr tree))))
		  (unless (eq a (car tree))
		    (rplaca tree a))
		  (unless (eq d (cdr tree))
		    (rplacd tree d)))))
	 tree)
	(t tree)))

;;; Sharp-equal works as follows. When a label is assigned (i.e. when
;;; #= is called) we GENSYM a symbol is which is used as an
;;; unforgeable tag. *SHARP-SHARP-ALIST* maps the integer tag to this
;;; gensym.
;;;
;;; When SHARP-SHARP encounters a reference to a label, it returns the
;;; symbol assoc'd with the label. Resolution of the reference is
;;; deferred until the read done by #= finishes. Any already resolved
;;; tags (in *SHARP-EQUAL-ALIST*) are simply returned.
;;;
;;; After reading of the #= form is completed, we add an entry to
;;; *SHARP-EQUAL-ALIST* that maps the gensym tag to the resolved
;;; object. Then for each entry in the *SHARP-SHARP-ALIST, the current
;;; object is searched and any uses of the gensysm token are replaced
;;; with the actual value.
(defvar *sharp-sharp-alist* ())

(defun sharp-equal (stream ignore label)
  (declare (ignore ignore))
  (when *read-suppress* (return-from sharp-equal (values)))
  (unless label
    (%reader-error stream "missing label for #=" label))
  (when (or (assoc label *sharp-sharp-alist*)
	    (assoc label *sharp-equal-alist*))
    (%reader-error stream "multiply defined label: #~D=" label))
  (let* ((tag (gensym))
	 (*sharp-sharp-alist* (acons label tag *sharp-sharp-alist*))
	 (obj (read stream t nil t)))
    (when (eq obj tag)
      (%reader-error stream
		     "must tag something more than just #~D#"
		     label))
    (push (list label tag obj) *sharp-equal-alist*)
    (let ((*sharp-equal-circle-table* (make-hash-table :test 'eq :size 20)))
      (circle-subst *sharp-equal-alist* obj))))

(defun sharp-sharp (stream ignore label)
  (declare (ignore ignore))
  (when *read-suppress* (return-from sharp-sharp nil))
  (unless label
    (%reader-error stream "missing label for ##" label))

  (let ((entry (assoc label *sharp-equal-alist*)))
    (if entry
	(third entry)
	(let ((pair (assoc label *sharp-sharp-alist*)))
	  (unless pair
	    (%reader-error stream "object is not labelled #~S#" label))
	  (cdr pair)))))

;;;; conditional compilation: the #+ and #- readmacros

(flet ((guts (stream not-p)
	 (unless (if (handler-case
			 (let ((*package* *keyword-package*)
			       (*read-suppress* nil))
			   (featurep (read stream t nil t)))
		       (reader-package-error
			(condition)
			(declare (ignore condition))
			nil))
		     (not not-p)
		     not-p)
	   (let ((*read-suppress* t))
	     (read stream t nil t)))
	 (values)))

  (defun sharp-plus (stream sub-char numarg)
    (ignore-numarg sub-char numarg)
    (guts stream nil))

  (defun sharp-minus (stream sub-char numarg)
    (ignore-numarg sub-char numarg)
    (guts stream t)))

;;;; reading miscellaneous objects: the #P, #\, and #| readmacros

(defun sharp-P (stream sub-char numarg)
  (ignore-numarg sub-char numarg)
  (let ((namestring (read stream t nil t)))
    (unless *read-suppress*
      (parse-namestring namestring))))

(defun sharp-backslash (stream backslash numarg)
  (ignore-numarg backslash numarg)
  (let ((charstring (read-extended-token-escaped stream)))
    (declare (simple-string charstring))
    (cond (*read-suppress* nil)
	  ((= (the fixnum (length charstring)) 1)
	   (char charstring 0))
	  ((name-char charstring))
	  (t
	   (%reader-error stream "unrecognized character name: ~S"
			  charstring)))))

(defun sharp-vertical-bar (stream sub-char numarg)
  (ignore-numarg sub-char numarg)
  (handler-bind
      ((character-decoding-error
	#'(lambda (decoding-error)
	    (declare (ignorable decoding-error))
	    (style-warn "Character decoding error in a #|-comment at position ~A reading source file ~A, resyncing." (file-position stream) stream)
	    (invoke-restart 'attempt-resync))))
    (let ((stream (in-synonym-of stream)))
      (if (ansi-stream-p stream)
	  (prepare-for-fast-read-char stream
	    (do ((level 1)
		 (prev (fast-read-char) char)
		 (char (fast-read-char) (fast-read-char)))
		(())
	      (cond ((and (char= prev #\|) (char= char #\#))
		     (setq level (1- level))
		     (when (zerop level)
		       (done-with-fast-read-char)
		       (return (values)))
		     (setq char (fast-read-char)))
		    ((and (char= prev #\#) (char= char #\|))
		     (setq char (fast-read-char))
		     (setq level (1+ level))))))
	  ;; fundamental-stream
	  (do ((level 1)
	       (prev (read-char stream t) char)
	       (char (read-char stream t) (read-char stream t)))
	      (())
	    (cond ((and (char= prev #\|) (char= char #\#))
		   (setq level (1- level))
		   (when (zerop level)
		     (return (values)))
		   (setq char (read-char stream t)))
		  ((and (char= prev #\#) (char= char #\|))
		   (setq char (read-char stream t))
		   (setq level (1+ level)))))))))

;;;; a grab bag of other sharp readmacros: #', #:, and #.

(defun sharp-quote (stream sub-char numarg)
  (ignore-numarg sub-char numarg)
  ;; The fourth arg tells READ that this is a recursive call.
  `(function ,(read stream t nil t)))

(defun sharp-colon (stream sub-char numarg)
  (ignore-numarg sub-char numarg)
  (multiple-value-bind (token escapep colon) (read-extended-token stream)
    (declare (simple-string token) (ignore escapep))
    (cond
     (*read-suppress* nil)
     (colon
      (%reader-error stream
		     "The symbol following #: contains a package marker: ~S"
		     token))
     (t
      (make-symbol token)))))

(defvar *read-eval* t
  #!+sb-doc
  "If false, then the #. read macro is disabled.")

(defun sharp-dot (stream sub-char numarg)
  (ignore-numarg sub-char numarg)
  (let ((token (read stream t nil t)))
    (unless *read-suppress*
      (unless *read-eval*
	(%reader-error stream "can't read #. while *READ-EVAL* is NIL"))
      (eval token))))

(defun sharp-illegal (stream sub-char ignore)
  (declare (ignore ignore))
  (%reader-error stream "illegal sharp macro character: ~S" sub-char))

;;; for cold init: Install SHARPM stuff in the current *READTABLE*.
(defun !sharpm-cold-init ()
  (make-dispatch-macro-character #\# t)
  (set-dispatch-macro-character #\# #\\ #'sharp-backslash)
  (set-dispatch-macro-character #\# #\' #'sharp-quote)
  (set-dispatch-macro-character #\# #\( #'sharp-left-paren)
  (set-dispatch-macro-character #\# #\* #'sharp-star)
  (set-dispatch-macro-character #\# #\: #'sharp-colon)
  (set-dispatch-macro-character #\# #\. #'sharp-dot)
  (set-dispatch-macro-character #\# #\R #'sharp-R)
  (set-dispatch-macro-character #\# #\r #'sharp-R)
  (set-dispatch-macro-character #\# #\B #'sharp-B)
  (set-dispatch-macro-character #\# #\b #'sharp-B)
  (set-dispatch-macro-character #\# #\O #'sharp-O)
  (set-dispatch-macro-character #\# #\o #'sharp-O)
  (set-dispatch-macro-character #\# #\X #'sharp-X)
  (set-dispatch-macro-character #\# #\x #'sharp-X)
  (set-dispatch-macro-character #\# #\A #'sharp-A)
  (set-dispatch-macro-character #\# #\a #'sharp-A)
  (set-dispatch-macro-character #\# #\S #'sharp-S)
  (set-dispatch-macro-character #\# #\s #'sharp-S)
  (set-dispatch-macro-character #\# #\= #'sharp-equal)
  (set-dispatch-macro-character #\# #\# #'sharp-sharp)
  (set-dispatch-macro-character #\# #\+ #'sharp-plus)
  (set-dispatch-macro-character #\# #\- #'sharp-minus)
  (set-dispatch-macro-character #\# #\C #'sharp-C)
  (set-dispatch-macro-character #\# #\c #'sharp-C)
  (set-dispatch-macro-character #\# #\| #'sharp-vertical-bar)
  (set-dispatch-macro-character #\# #\p #'sharp-p)
  (set-dispatch-macro-character #\# #\P #'sharp-p)
  (set-dispatch-macro-character #\# #\) #'sharp-illegal)
  (set-dispatch-macro-character #\# #\< #'sharp-illegal)
  (set-dispatch-macro-character #\# #\Space #'sharp-illegal)
  (dolist (cc '#.(list tab-char-code form-feed-char-code return-char-code
                       line-feed-char-code backspace-char-code))
    (set-dispatch-macro-character #\# (code-char cc) #'sharp-illegal)))
