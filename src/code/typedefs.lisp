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

(!begin-collecting-cold-init-forms)

;;; Define the translation from a type-specifier to a type structure for
;;; some particular type. Syntax is identical to DEFTYPE.
(defmacro !def-type-translator (name arglist &body body)
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

;;; the base class for the internal representation of types
(def!struct (ctype (:conc-name type-)
		   (:constructor nil)
		   (:make-load-form-fun make-type-load-form)
		   #-sb-xc-host (:pure t))
  ;; the class of this type
  ;;
  ;; FIXME: It's unnecessarily confusing to have a structure accessor
  ;; named TYPE-CLASS-INFO which is an accessor for the CTYPE structure
  ;; even though the TYPE-CLASS structure also exists in the system.
  ;; Rename this slot: TYPE-CLASS or ASSOCIATED-TYPE-CLASS or something.
  (class-info (required-argument) :type type-class)
  ;; True if this type has a fixed number of members, and as such
  ;; could possibly be completely specified in a MEMBER type. This is
  ;; used by the MEMBER type methods.
  (enumerable nil :read-only t)
  ;; an arbitrary hash code used in EQ-style hashing of identity
  ;; (since EQ hashing can't be done portably)
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

;;; sort of like ANY and EVERY, except:
;;;   * We handle two-VALUES predicate functions like SUBTYPEP. (And
;;;     if the result is uncertain, then we return (VALUES NIL NIL).)
;;;   * THING is just an atom, and we apply OP (an arity-2 function)
;;;     successively to THING and each element of LIST.
(defun any/type (op thing list)
  (declare (type function op))
  (let ((certain? t))
    (dolist (i list (values nil certain?))
      (multiple-value-bind (sub-value sub-certain?)
	  (funcall op thing i)
	(unless sub-certain? (setf certain? nil))
	(when sub-value (return (values t t)))))))
(defun every/type (op thing list)
  (declare (type function op))
  (dolist (i list (values t t))
    (multiple-value-bind (sub-value sub-certain?)
	(funcall op thing i)
      (unless sub-certain? (return (values nil nil)))
      (unless sub-value (return (values nil t))))))

;;; Return a function like FUN, but expecting its (two) arguments in
;;; the opposite order that FUN does.
;;;
;;; (This looks like a sort of general utility, but currently it's
;;; used only in the implementation of the type system, so it's
;;; internal to SB-KERNEL. -- WHN 2001-02-13)
(declaim (inline swapped-args-fun))
(defun swapped-args-fun (fun)
  (declare (type function fun))
  (lambda (x y)
    (funcall fun y x)))

;;; Look for a nice intersection for types that intersect only when
;;; one is a hierarchical subtype of the other.
(defun hierarchical-intersection2 (type1 type2)
  (multiple-value-bind (subtypep1 win1) (csubtypep type1 type2)
    (multiple-value-bind (subtypep2 win2) (csubtypep type2 type1)
      (cond (subtypep1 type1)
	    (subtypep2 type2)
	    ((and win1 win2) *empty-type*)
	    (t nil)))))

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
