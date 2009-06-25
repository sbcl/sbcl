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
  (declare (type symbol name))
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
          (values (sb!xc:gensym) arglist))
    (multiple-value-bind (forms decls)
        (parse-body body :doc-string-allowed nil)
      `(progn
         (!cold-init-forms
          (let ((fun (lambda (,whole)
                       (block ,name
                         (destructuring-bind ,wholeless-arglist
                             (rest ,whole)  ; discarding NAME
                           ,@decls
                       ,@forms)))))
            #-sb-xc-host
            (setf (%simple-fun-arglist (the simple-fun fun)) ',wholeless-arglist)
            (setf (info :type :translator ',name) fun)))
         ',name))))

;;; DEFVARs for these come later, after we have enough stuff defined.
(declaim (special *wild-type* *universal-type* *empty-type*))

(defvar *type-random-state*)

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
  (class-info (missing-arg) :type type-class)
  ;; True if this type has a fixed number of members, and as such
  ;; could possibly be completely specified in a MEMBER type. This is
  ;; used by the MEMBER type methods.
  (enumerable nil :read-only t)
  ;; an arbitrary hash code used in EQ-style hashing of identity
  ;; (since EQ hashing can't be done portably)
  (hash-value (random #.(ash 1 15)
                      (if (boundp '*type-random-state*)
                          *type-random-state*
                          (setf *type-random-state*
                                (make-random-state))))
              :type (and fixnum unsigned-byte)
              :read-only t)
  ;; Can this object contain other types? A global property of our
  ;; implementation (which unfortunately seems impossible to enforce
  ;; with assertions or other in-the-code checks and constraints) is
  ;; that subclasses which don't contain other types correspond to
  ;; disjoint subsets (except of course for the NAMED-TYPE T, which
  ;; covers everything). So NUMBER-TYPE is disjoint from CONS-TYPE is
  ;; is disjoint from MEMBER-TYPE and so forth. But types which can
  ;; contain other types, like HAIRY-TYPE and INTERSECTION-TYPE, can
  ;; violate this rule.
  (might-contain-other-types-p nil :read-only t))
(def!method print-object ((ctype ctype) stream)
  (print-unreadable-object (ctype stream :type t)
    (prin1 (type-specifier ctype) stream)))

;;; Just dump it as a specifier. (We'll convert it back upon loading.)
(defun make-type-load-form (type)
  (declare (type ctype type))
  `(specifier-type ',(type-specifier type)))

;;;; miscellany

;;; Look for nice relationships for types that have nice relationships
;;; only when one is a hierarchical subtype of the other.
(defun hierarchical-intersection2 (type1 type2)
  (multiple-value-bind (subtypep1 win1) (csubtypep type1 type2)
    (multiple-value-bind (subtypep2 win2) (csubtypep type2 type1)
      (cond (subtypep1 type1)
            (subtypep2 type2)
            ((and win1 win2) *empty-type*)
            (t nil)))))
(defun hierarchical-union2 (type1 type2)
  (cond ((csubtypep type1 type2) type2)
        ((csubtypep type2 type1) type1)
        (t nil)))

;;; Hash two things (types) down to 8 bits. In CMU CL this was an EQ
;;; hash, but since it now needs to run in vanilla ANSI Common Lisp at
;;; cross-compile time, it's now based on the CTYPE-HASH-VALUE field
;;; instead.
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
#!-sb-fluid (declaim (inline type-list-cache-hash))
(declaim (ftype (function (list) (unsigned-byte 8)) type-list-cache-hash))
(defun type-list-cache-hash (types)
  (logand (loop with res = 0
             for type in types
             for hash = (type-hash-value type)
             do (setq res (logxor res hash))
             finally (return res))
          #xFF))

;;;; cold loading initializations

(!defun-from-collected-cold-init-forms !typedefs-cold-init)
