;;;; This file contains the definition of the CTYPE (Compiler TYPE)
;;;; structure and related macros used for manipulating it. This is
;;;; sort of a mini object system with rather odd dispatching rules.
;;;; Other compile-time definitions needed by multiple files are also
;;;; here.
;;;;
;;;; FIXME: The comment above about what's in this file is no longer so
;;;; true now that I've split off type-class.lisp. Perhaps we should
;;;; split off CTYPE into the same file as type-class.lisp, rename that
;;;; file to ctype.lisp, move the current comment to the head of that file,
;;;; and write a new comment for this file saying how this file holds
;;;; concrete types.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

(file-comment
  "$Header$")

(!begin-collecting-cold-init-forms)

;;; Define the translation from a type-specifier to a type structure for
;;; some particular type. Syntax is identical to DEFTYPE.
(defmacro def-type-translator (name arglist &body body)
  (check-type name symbol)
  ;; FIXME: Now that the T%CL hack is ancient history and we just use CL
  ;; instead, we can probably return to using PARSE-DEFMACRO here.
  ;;
  ;; was:
  ;;   This song and dance more or less emulates PARSE-DEFMACRO. The reason for
  ;;   doing this emulation instead of just calling PARSE-DEFMACRO is just that
  ;;   at cross-compile time PARSE-DEFMACRO expects lambda-list keywords in the
  ;;   T%CL package, which is not what we have here. Maybe there's a tidier
  ;;   solution.. (Other than wishing that ANSI had used symbols in the KEYWORD
  ;;   package as lambda list keywords, rather than using symbols in the LISP
  ;;   package!)
  (multiple-value-bind (whole wholeless-arglist)
      (if (eq '&whole (car arglist))
	(values (cadr arglist) (cddr arglist))
	(values (gensym) arglist))
    (multiple-value-bind (forms decls) (parse-body body nil)
      `(progn
	 (!cold-init-forms
	  (setf (info :type :translator ',name)
		(lambda (,whole)
		  (block ,name
		    (destructuring-bind ,wholeless-arglist
			(rest ,whole) ; discarding NAME
		      ,@decls
		      ,@forms)))))
	 ',name))))

;;; DEFVARs for these come later, after we have enough stuff defined.
(declaim (special *wild-type* *universal-type* *empty-type*))

;;; The XXX-Type structures include the CTYPE structure for some slots that
;;; apply to all types.
(def!struct (ctype (:conc-name type-)
		   (:constructor nil)
		   (:make-load-form-fun make-type-load-form)
		   #-sb-xc-host (:pure t))
  ;; The class of this type.
  ;;
  ;; FIXME: It's unnecessarily confusing to have a structure accessor
  ;; named TYPE-CLASS-INFO which is an accessor for the CTYPE structure
  ;; even though the TYPE-CLASS structure also exists in the system.
  ;; Rename this slot: TYPE-CLASS or ASSOCIATED-TYPE-CLASS or something.
  (class-info (required-argument) :type type-class)
  ;; True if this type has a fixed number of members, and as such could
  ;; possibly be completely specified in a MEMBER type. This is used by the
  ;; MEMBER type methods.
  (enumerable nil :type (member t nil) :read-only t)
  ;; an arbitrary hash code used in EQ-style hashing of identity (since EQ
  ;; hashing can't be done portably)
  (hash-value (random (1+ most-positive-fixnum))
	      :type (and fixnum unsigned-byte)
	      :read-only t))
(def!method print-object ((ctype ctype) stream)
  (print-unreadable-object (ctype stream :type t)
    (prin1 (type-specifier ctype) stream)))

;;; Just dump it as a specifier. (We'll convert it back upon loading.)
(defun make-type-load-form (type)
  (declare (type ctype type))
  `(specifier-type ',(type-specifier type)))

;;;; utilities

;;; Like ANY and EVERY, except that we handle two-arg uncertain predicates.
;;; If the result is uncertain, then we return Default from the block PUNT.
;;; If LIST-FIRST is true, then the list element is the first arg, otherwise
;;; the second.
(defmacro any-type-op (op thing list &key (default '(values nil nil))
			  list-first)
  (let ((n-this (gensym))
	(n-thing (gensym))
	(n-val (gensym))
	(n-win (gensym))
	(n-uncertain (gensym)))
    `(let ((,n-thing ,thing)
	   (,n-uncertain nil))
       (dolist (,n-this ,list
			(if ,n-uncertain
			    (return-from PUNT ,default)
			    nil))
	 (multiple-value-bind (,n-val ,n-win)
	     ,(if list-first
		  `(,op ,n-this ,n-thing)
		`(,op ,n-thing ,n-this))
	   (unless ,n-win (setq ,n-uncertain t))
	   (when ,n-val (return t)))))))
(defmacro every-type-op (op thing list &key (default '(values nil nil))
			    list-first)
  (let ((n-this (gensym))
	(n-thing (gensym))
	(n-val (gensym))
	(n-win (gensym)))
    `(let ((,n-thing ,thing))
       (dolist (,n-this ,list t)
	 (multiple-value-bind (,n-val ,n-win)
	     ,(if list-first
		  `(,op ,n-this ,n-thing)
		`(,op ,n-thing ,n-this))
	   (unless ,n-win (return-from PUNT ,default))
	   (unless ,n-val (return nil)))))))

;;; Compute the intersection for types that intersect only when one is a
;;; hierarchical subtype of the other.
(defun vanilla-intersection (type1 type2)
  (multiple-value-bind (stp1 win1) (csubtypep type1 type2)
    (multiple-value-bind (stp2 win2) (csubtypep type2 type1)
      (cond (stp1 (values type1 t))
	    (stp2 (values type2 t))
	    ((and win1 win2) (values *empty-type* t))
	    (t
	     (values type1 nil))))))

(defun vanilla-union (type1 type2)
  (cond ((csubtypep type1 type2) type2)
	((csubtypep type2 type1) type1)
	(t nil)))

;;; Hash two things (types) down to 8 bits. In CMU CL this was an EQ hash, but
;;; since it now needs to run in vanilla ANSI Common Lisp at cross-compile
;;; time, it's now based on the CTYPE-HASH-VALUE field instead.
;;;
;;; FIXME: This was a macro in CMU CL, and is now an INLINE function. Is
;;; it important for it to be INLINE, or could be become an ordinary
;;; function without significant loss? -- WHN 19990413
#!-sb-fluid (declaim (inline type-cache-hash))
(declaim (ftype (function (ctype ctype) (unsigned-byte 8)) type-cache-hash))
(defun type-cache-hash (type1 type2)
  (logand (logxor (ash (type-hash-value type1) -3)
		  (type-hash-value type2))
	  #xFF))

;;;; cold loading initializations

(!defun-from-collected-cold-init-forms !typedefs-cold-init)
