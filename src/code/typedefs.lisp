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
          (let ((fun (named-lambda ,(format nil "~A-TYPE-PARSE" name) (,whole)
                       (block ,name
                         (destructuring-bind ,wholeless-arglist
                             (rest ,whole)  ; discarding NAME
                           ,@decls
                       ,@forms)))))
            #-sb-xc-host
            (setf (%simple-fun-arglist (the simple-fun fun)) ',wholeless-arglist)
            (setf (info :type :translator ',name) fun)))
         ',name))))


#+sb-xc-host
(defun ctype-random (mask)
  (logand (setq *ctype-lcg-state*
                (logand #x8fffff (+ (* 1103515245 *ctype-lcg-state*) 12345)))
          mask))

;;; the base class for the internal representation of types

;; Each CTYPE instance (incl. subtypes thereof) has a random opaque hash value.
;; Hashes are mixed together to form a lookup key in the memoization wrappers
;; for most operations in CTYPES. This works because CTYPEs are immutable.
;; But 2 bits are "stolen" from the hash to use as flag bits.
;; The sign bit indicates that the object is the *only* object representing
;; its type-specifier - it is an "interned" object.
;; The next highest bit indicates that the object, if compared for TYPE=
;; against an interned object can quickly return false when not EQ.
;; Complicated types don't admit the quick failure check.
;; At any rate, the totally opaque pseudo-random bits are under this mask.
(defconstant +ctype-hash-mask+
  (ldb (byte (1- sb!vm:n-positive-fixnum-bits) 0) -1))

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
  ;; [or TYPE-VTABLE or TYPE-METHODS either of which basically equates
  ;;  a type-class with the set of things it can do, while avoiding
  ;;  ambiguity to whether it is a 'CLASS-INFO' slot in a 'TYPE'
  ;;  or an 'INFO' slot in a 'TYPE-CLASS']
  (class-info (missing-arg) :type type-class)
  ;; an arbitrary hash code used in EQ-style hashing of identity
  ;; (since EQ hashing can't be done portably)
  ;; - in the host lisp, generate a hash value using a known, simple
  ;;   random number generator (rather than the host lisp's
  ;;   implementation of RANDOM)
  ;; - in the target, use scrambled bits from the allocation pointer
  ;;   instead.
  (hash-value
   #+sb-xc-host (ctype-random +ctype-hash-mask+)
   #-sb-xc-host (sb!impl::quasi-random-address-based-hash
                 *ctype-hash-state* +ctype-hash-mask+)
              :type (signed-byte #.sb!vm:n-fixnum-bits)
              ;; FIXME: is there a better way to initialize the hash value
              ;; and its flag bit simultaneously rather than have it
              ;; be a read/write slot?
              :read-only nil))
(def!method print-object ((ctype ctype) stream)
  (print-unreadable-object (ctype stream :type t)
    (prin1 (type-specifier ctype) stream)))

;; Set the sign bit (the "interned" bit) of the hash-value of OBJ to 1.
;; This is an indicator that the object is the unique internal representation
;; of any ctype that is TYPE= to this object.
;; Everything starts out assumed non-unique.
;; The hash-cache logic (a/k/a memoization) tends to ignore high bits when
;; creating cache keys because the mixing function is XOR and the caches
;; are power-of-2 sizes. Lkewise making the low bits non-random is bad
;; for cache distribution.
(defconstant +type-admits-type=-optimization+
  (ash 1 (- sb!vm:n-positive-fixnum-bits 1))) ; highest bit in fixnum
(defun mark-ctype-interned (obj)
  (setf (type-hash-value obj)
        (logior sb!xc:most-negative-fixnum
                (if (eq (type-class-name (type-class-info obj)) 'array)
                    0
                    +type-admits-type=-optimization+)
                (type-hash-value obj)))
  obj)

(declaim (inline type-might-contain-other-types-p))
(defun type-might-contain-other-types-p (ctype)
  (type-class-might-contain-other-types-p (type-class-info ctype)))

(declaim (inline type-enumerable))
(defun type-enumerable (ctype)
  (let ((answer (type-class-enumerable-p (type-class-info ctype))))
    (if (functionp answer)
        (funcall answer ctype)
        answer)))

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

;;; Hash two things (types) down to a fixnum. In CMU CL this was an EQ
;;; hash, but since it now needs to run in vanilla ANSI Common Lisp at
;;; cross-compile time, it's now based on the CTYPE-HASH-VALUE field
;;; instead.
;;;
;;; FIXME: This was a macro in CMU CL, and is now an INLINE function. Is
;;; it important for it to be INLINE, or could be become an ordinary
;;; function without significant loss? -- WHN 19990413
#!-sb-fluid (declaim (inline type-cache-hash))
(declaim (ftype (function (ctype ctype) fixnum) type-cache-hash))
(defun type-cache-hash (type1 type2)
  (logxor (ash (type-hash-value type1) -3) (type-hash-value type2)))

#!-sb-fluid (declaim (inline type-list-cache-hash))
(declaim (ftype (function (list) fixnum) type-list-cache-hash))
(defun type-list-cache-hash (types)
  (loop with res fixnum = 0
        for type in types
        do (setq res (logxor (ash res -1) (type-hash-value type)))
        finally (return res)))

;;;; cold loading initializations

(!defun-from-collected-cold-init-forms !typedefs-cold-init)
