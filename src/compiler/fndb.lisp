;;;; This file defines all the standard functions to be known
;;;; functions. Each function has type and side-effect information,
;;;; and may also have IR1 optimizers.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

(file-comment
  "$Header$")

;;;; information for known functions:

(defknown coerce (t type-specifier) t
  ;; Note:
  ;; (1) This is not FLUSHABLE because it's defined to signal errors.
  ;; (2) It's not worth trying to make this FOLDABLE in the
  ;;     cross-compiler,because
  ;;       (a) it would probably be really hard to make all the 
  ;;           tricky issues (e.g. which specialized array types are
  ;;           supported) match between cross-compiler and target
  ;;           compiler, and besides
  ;;       (b) leaving it not FOLDABLE lets us use the idiom
  ;;	           (COERCE FOO 'SOME-SPECIALIZED-ARRAY-TYPE-OR-ANOTHER)
  ;;	       as a way of delaying the generation of specialized
  ;;	       array types until runtime, which helps us keep the
  ;;	       cross-compiler's dumper relatively simple and which
  ;;	       lets us preserve distinctions which might not even exist
  ;;           on the cross-compilation host (because ANSI doesn't
  ;;	       guarantee that specialized array types exist there).
  (movable #-sb-xc-host foldable)
  :derive-type (result-type-specifier-nth-arg 2))
(defknown list-to-simple-string* (list) simple-string)
(defknown list-to-bit-vector* (list) bit-vector)
(defknown list-to-vector* (list type) vector)
(defknown list-to-simple-vector* (list) simple-vector)
(defknown vector-to-vector* (vector type) vector)
(defknown vector-to-simple-string* (vector) vector)

(defknown type-of (t) t (foldable flushable))

;;; These can be affected by type definitions, so they're not FOLDABLE.
(defknown (upgraded-complex-part-type upgraded-array-element-type)
	  (type-specifier) type-specifier
  (flushable))

;;;; from the "Predicates" chapter:

;;; FIXME: Is it right to have TYPEP (and TYPE-OF, elsewhere; and
;;; perhaps SPECIAL-OPERATOR-P and others) be FOLDABLE in the
;;; cross-compilation host? After all, some type relationships (e.g.
;;; FIXNUMness) might be different between host and target. Perhaps
;;; this property should be protected by #-SB-XC-HOST? Perhaps we need
;;; 3-stage bootstrapping after all? (Ugh! It's *so* slow already!)
(defknown typep (t type-specifier) boolean
  (flushable
   ;; Unlike SUBTYPEP or UPGRADED-ARRAY-ELEMENT-TYPE and friends, this
   ;; seems to be FOLDABLE. Like SUBTYPEP, it's affected by type
   ;; definitions, but unlike SUBTYPEP, there should be no way to make
   ;; a TYPEP expression with constant arguments which doesn't return
   ;; an error before the type declaration (because of undefined
   ;; type). E.g. you can do
   ;;   (SUBTYPEP 'INTEGER 'FOO) => NIL, NIL
   ;;   (DEFTYPE FOO () T)
   ;;   (SUBTYPEP 'INTEGER 'FOO) => T, T
   ;; but the analogous
   ;;   (TYPEP 12 'FOO)
   ;;   (DEFTYPE FOO () T)
   ;;   (TYPEP 12 'FOO)
   ;; doesn't work because the first call is an error.
   ;;
   ;; (UPGRADED-ARRAY-ELEMENT-TYPE and UPGRADED-COMPLEX-PART-TYPE have
   ;; behavior like SUBTYPEP in this respect, not like TYPEP.)
   foldable))
(defknown subtypep (type-specifier type-specifier) (values boolean boolean) 
  ;; This is not FOLDABLE because its value is affected by type
  ;; definitions.
  ;;
  ;; FIXME: Is it OK to fold this when the types have already been
  ;; defined? Does the code inherited from CMU CL already do this?
  (flushable)) 

(defknown (null symbolp atom consp listp numberp integerp rationalp floatp
		complexp characterp stringp bit-vector-p vectorp
		simple-vector-p simple-string-p simple-bit-vector-p arrayp
		sb!xc:packagep functionp compiled-function-p not)
  (t) boolean (movable foldable flushable))

(defknown (eq eql) (t t) boolean (movable foldable flushable))
(defknown (equal equalp) (t t) boolean (foldable flushable recursive))

;;;; classes

(sb!xc:deftype name-for-class () 't)
(defknown class-name (sb!xc:class) name-for-class (flushable))
(defknown find-class (name-for-class &optional t lexenv)
  (or sb!xc:class null) ())
(defknown class-of (t) sb!xc:class (flushable))
(defknown layout-of (t) layout (flushable))
(defknown copy-structure (structure-object) structure-object
  (flushable unsafe))

;;;; from the "Control Structure" chapter:

;;; This is not FLUSHABLE, since it's required to signal an error if
;;; unbound.
(defknown (symbol-value symbol-function) (symbol) t ())

(defknown boundp (symbol) boolean (flushable))
(defknown fboundp ((or symbol cons)) boolean (flushable explicit-check))
(defknown special-operator-p (symbol) t
  ;; The set of special operators never changes.
  (movable foldable flushable)) 
(defknown set (symbol t) t (unsafe)
  :derive-type #'result-type-last-arg)
(defknown fdefinition ((or symbol cons)) function (unsafe explicit-check))
(defknown %set-fdefinition ((or symbol cons) function) function
  (unsafe explicit-check))
(defknown makunbound (symbol) symbol)
(defknown fmakunbound ((or symbol cons)) (or symbol cons)
  (unsafe explicit-check))
(defknown (get-setf-method get-setf-method-multiple-value)
  ((or list symbol) &optional lexenv)
  (values list list list form form)
  (flushable))
(defknown apply (callable t &rest t) *) ; ### Last arg must be List...
(defknown funcall (callable &rest t) *)

(defknown (mapcar maplist mapcan mapcon) (callable list &rest list) list
  (call))

(defknown (mapc mapl) (callable list &rest list) list (foldable call))

;;; We let VALUES-LIST be foldable, since constant-folding will turn
;;; it into VALUES. VALUES is not foldable, since MV constants are
;;; represented by a call to VALUES.
(defknown values (&rest t) * (movable flushable unsafe))
(defknown values-list (list) * (movable foldable flushable))

;;;; from the "Macros" chapter:

(defknown macro-function (symbol &optional lexenv)
  (or function null)
  (flushable))
(defknown (macroexpand macroexpand-1) (t &optional lexenv)
  (values form &optional boolean))

(defknown compiler-macro-function (t &optional lexenv)
  (or function null)
  (flushable))

;;;; from the "Declarations" chapter:

(defknown proclaim (list) (values) (recursive))

;;;; from the "Symbols" chapter:

(defknown get (symbol t &optional t) t (flushable))
(defknown remprop (symbol t) t)
(defknown symbol-plist (symbol) list (flushable))
(defknown getf (list t &optional t) t (foldable flushable))
(defknown get-properties (list list) (values t t list) (foldable flushable))
(defknown symbol-name (symbol) simple-string (movable foldable flushable))
(defknown make-symbol (string) symbol (flushable))
(defknown copy-symbol (symbol &optional t) symbol (flushable))
(defknown gensym (&optional (or string unsigned-byte)) symbol ())
(defknown symbol-package (symbol) (or sb!xc:package null) (flushable))
(defknown keywordp (t) boolean (flushable))	  ; If someone uninterns it...

;;;; from the "Packages" chapter:

(sb!xc:deftype package-designator () '(or stringable sb!xc:package))
(sb!xc:deftype symbols () '(or list symbol))

;;; Should allow a package name, I think, tho CLtL II doesn't say so...
(defknown gentemp (&optional string package-designator) symbol)

(defknown make-package (stringable &key
				   (:use list)
				   (:nicknames list)
				   ;; ### Extensions...
				   (:internal-symbols index)
				   (:external-symbols index))
  sb!xc:package)
(defknown find-package (package-designator) (or sb!xc:package null)
  (flushable))
(defknown package-name (package-designator) (or simple-string null)
  (flushable))
(defknown package-nicknames (package-designator) list (flushable))
(defknown rename-package (package-designator package-designator &optional list)
  sb!xc:package)
(defknown package-use-list (package-designator) list (flushable))
(defknown package-used-by-list (package-designator) list (flushable))
(defknown package-shadowing-symbols (package-designator) list (flushable))
(defknown list-all-packages () list (flushable))
(defknown intern (string &optional package-designator)
  (values symbol (member :internal :external :inherited nil))
  ())
(defknown find-symbol (string &optional package-designator)
  (values symbol (member :internal :external :inherited nil))
  (flushable))
(defknown (export import) (symbols &optional package-designator) (eql t))
(defknown unintern (symbol &optional package-designator) boolean)
(defknown unexport (symbols &optional package-designator) (eql t))
(defknown shadowing-import (symbols &optional package-designator) (eql t))
(defknown shadow ((or symbol string list) &optional package-designator) (eql t))
(defknown (use-package unuse-package) ((or list package-designator) &optional package-designator) (eql t))
(defknown find-all-symbols (stringable) list (flushable))

;;;; from the "Numbers" chapter:

(defknown zerop (number) boolean (movable foldable flushable explicit-check))
(defknown (plusp minusp) (real) boolean
  (movable foldable flushable explicit-check))
(defknown (oddp evenp) (integer) boolean
  (movable foldable flushable explicit-check))
(defknown (= /=) (number &rest number) boolean
  (movable foldable flushable explicit-check))
(defknown (< > <= >=) (real &rest real) boolean
  (movable foldable flushable explicit-check))
(defknown (max min) (real &rest real) real
  (movable foldable flushable explicit-check))

(defknown + (&rest number) number
  (movable foldable flushable explicit-check))
(defknown - (number &rest number) number
  (movable foldable flushable explicit-check))
(defknown * (&rest number) number
  (movable foldable flushable explicit-check))
(defknown / (number &rest number) number
  (movable foldable flushable explicit-check))
(defknown (1+ 1-) (number) number
  (movable foldable flushable explicit-check))

(defknown conjugate (number) number
  (movable foldable flushable explicit-check))

(defknown gcd (&rest integer) unsigned-byte
  (movable foldable flushable explicit-check)
  #|:derive-type 'boolean-result-type|#)
(defknown lcm (&rest integer) unsigned-byte
  (movable foldable flushable explicit-check))

#!-propagate-fun-type
(defknown exp (number) irrational
  (movable foldable flushable explicit-check recursive)
  :derive-type #'result-type-float-contagion)

#!+propagate-fun-type
(defknown exp (number) irrational
  (movable foldable flushable explicit-check recursive))

(defknown expt (number number) number
  (movable foldable flushable explicit-check recursive))
(defknown log (number &optional real) irrational
  (movable foldable flushable explicit-check))
(defknown sqrt (number) irrational
  (movable foldable flushable explicit-check))
(defknown isqrt (unsigned-byte) unsigned-byte
  (movable foldable flushable explicit-check recursive))

(defknown (abs phase signum) (number) number
  (movable foldable flushable explicit-check))
(defknown cis (real) (complex float)
  (movable foldable flushable explicit-check))

#!-propagate-fun-type
(progn
(defknown (sin cos) (number)
  (or (float -1.0 1.0) (complex float))
  (movable foldable flushable explicit-check recursive)
  :derive-type #'result-type-float-contagion)

(defknown atan
  (number &optional real) irrational
  (movable foldable flushable explicit-check recursive)
  :derive-type #'result-type-float-contagion)

(defknown (tan sinh cosh tanh asinh)
  (number) irrational (movable foldable flushable explicit-check recursive)
  :derive-type #'result-type-float-contagion)
) ; PROGN

#!+propagate-fun-type
(progn
(defknown (sin cos) (number)
  (or (float -1.0 1.0) (complex float))
  (movable foldable flushable explicit-check recursive))

(defknown atan
  (number &optional real) irrational
  (movable foldable flushable explicit-check recursive))

(defknown (tan sinh cosh tanh asinh)
  (number) irrational (movable foldable flushable explicit-check recursive))
) ; PROGN

(defknown (asin acos acosh atanh)
  (number) irrational
  (movable foldable flushable explicit-check recursive))

(defknown float (real &optional float) float
  (movable foldable flushable explicit-check))

(defknown (rational) (real) rational
  (movable foldable flushable explicit-check))

(defknown (rationalize) (real) rational
  (movable foldable flushable explicit-check recursive))

(defknown (numerator denominator) (rational) integer
  (movable foldable flushable))

(defknown (floor ceiling truncate round)
  (real &optional real) (values integer real)
  (movable foldable flushable explicit-check))

(defknown (mod rem) (real real) real
  (movable foldable flushable explicit-check))

(defknown (ffloor fceiling fround ftruncate)
  (real &optional real) (values float float)
  (movable foldable flushable explicit-check))

(defknown decode-float (float) (values float float-exponent float)
  (movable foldable flushable explicit-check))
(defknown scale-float (float float-exponent) float
  (movable foldable flushable explicit-check))
(defknown float-radix (float) float-radix
  (movable foldable flushable explicit-check))
(defknown float-sign (float &optional float) float
  (movable foldable flushable explicit-check))
(defknown (float-digits float-precision) (float) float-digits
  (movable foldable flushable explicit-check))
(defknown integer-decode-float (float)
	  (values integer float-exponent (member -1 1))
	  (movable foldable flushable explicit-check))

(defknown complex (real &optional real) number
  (movable foldable flushable explicit-check))

(defknown (realpart imagpart) (number) real (movable foldable flushable))

(defknown (logior logxor logand logeqv) (&rest integer) integer
  (movable foldable flushable explicit-check))

(defknown (lognand lognor logandc1 logandc2 logorc1 logorc2)
	  (integer integer) integer
  (movable foldable flushable explicit-check))

(defknown boole (boole-code integer integer) integer
  (movable foldable flushable))

(defknown lognot (integer) integer (movable foldable flushable explicit-check))
(defknown logtest (integer integer) boolean (movable foldable flushable))
(defknown logbitp (bit-index integer) boolean (movable foldable flushable))
(defknown ash (integer integer) integer (movable foldable flushable explicit-check))
(defknown (logcount integer-length) (integer) bit-index
  (movable foldable flushable explicit-check))
;;; FIXME: According to the ANSI spec, it's legal to use any
;;; nonnegative indices for BYTE arguments, not just BIT-INDEX. It's
;;; hard to come up with useful ways to do this, but it is possible to
;;; come up with *legal* ways to do this, so it would be nice
;;; to fix this so we comply with the spec.
(defknown byte (bit-index bit-index) byte-specifier
  (movable foldable flushable))
(defknown (byte-size byte-position) (byte-specifier) bit-index
  (movable foldable flushable))
(defknown ldb (byte-specifier integer) integer (movable foldable flushable))
(defknown ldb-test (byte-specifier integer) boolean
  (movable foldable flushable))
(defknown mask-field (byte-specifier integer) integer
  (movable foldable flushable))
(defknown dpb (integer byte-specifier integer) integer
  (movable foldable flushable))
(defknown deposit-field (integer byte-specifier integer) integer
  (movable foldable flushable))
(defknown random ((real (0)) &optional random-state) (real 0) ())
(defknown make-random-state (&optional (or (member nil t) random-state))
  random-state (flushable))
(defknown random-state-p (t) boolean (movable foldable flushable))

;;;; from the "Characters" chapter:
(defknown (standard-char-p graphic-char-p alpha-char-p
			   upper-case-p lower-case-p both-case-p alphanumericp)
  (character) boolean (movable foldable flushable))

(defknown digit-char-p (character &optional unsigned-byte)
  (or (integer 0 35) null) (movable foldable flushable))

(defknown (char= char/= char< char> char<= char>= char-equal char-not-equal
		 char-lessp char-greaterp char-not-greaterp char-not-lessp)
  (character &rest character) boolean (movable foldable flushable))

(defknown character (t) character (movable foldable flushable))
(defknown char-code (character) char-code (movable foldable flushable))
(defknown (char-upcase char-downcase) (character) character
  (movable foldable flushable))
(defknown digit-char (integer &optional integer)
  (or character null) (movable foldable flushable))
(defknown char-int (character) char-code (movable foldable flushable))
(defknown char-name (character) (or simple-string null)
  (movable foldable flushable))
(defknown name-char (stringable) (or character null)
  (movable foldable flushable))
(defknown code-char (char-code) base-char
  ;; By suppressing constant folding on CODE-CHAR when the
  ;; cross-compiler is running in the cross-compilation host vanilla
  ;; ANSI Common Lisp, we can use CODE-CHAR expressions to delay until
  ;; target Lisp run time the generation of CHARACTERs which aren't
  ;; STANDARD-CHARACTERs. That way, we don't need to rely on the host
  ;; Common Lisp being able to handle any characters other than those
  ;; guaranteed by the ANSI spec.
  (movable #-sb-xc-host foldable flushable))

;;;; from the "Sequences" chapter:

(defknown elt (sequence index) t (foldable flushable))

(defknown subseq (sequence index &optional sequence-end) consed-sequence
  (flushable)
  :derive-type (sequence-result-nth-arg 1))

(defknown copy-seq (sequence) consed-sequence (flushable)
  :derive-type #'result-type-first-arg)

(defknown length (sequence) index (foldable flushable))

(defknown reverse (sequence) consed-sequence (flushable)
  :derive-type #'result-type-first-arg)

(defknown nreverse (sequence) sequence ()
  :derive-type #'result-type-first-arg)

(defknown make-sequence (type-specifier index
					&key
					(:initial-element t))
  consed-sequence
  (movable flushable unsafe)
  :derive-type (result-type-specifier-nth-arg 1))

(defknown concatenate (type-specifier &rest sequence) consed-sequence
  (flushable)
  :derive-type (result-type-specifier-nth-arg 1))

(defknown (map %map) (type-specifier callable sequence &rest sequence) consed-sequence
  (flushable call)
; :DERIVE-TYPE 'TYPE-SPEC-ARG1 ? Nope... (MAP NIL ...) returns NULL, not NIL.
  )
(defknown %map-to-list-arity-1 (callable sequence) list (flushable call))
(defknown %map-to-simple-vector-arity-1 (callable sequence) simple-vector
  (flushable call))
(defknown %map-to-nil-on-simple-vector (callable simple-vector) null
  (flushable call))
(defknown %map-to-nil-on-vector (callable vector) null (flushable call))
(defknown %map-to-nil-on-sequence (callable sequence) null (flushable call))

;;; returns the result from the predicate...
(defknown some (callable sequence &rest sequence) t
  (foldable flushable call))

(defknown (every notany notevery) (callable sequence &rest sequence) boolean
  (foldable flushable call))

;;; unsafe for :INITIAL-VALUE...
(defknown reduce (callable
		  sequence
		  &key
		  (:from-end t)
		  (:start index)
		  (:end sequence-end)
		  (:initial-value t)
		  (:key callable))
  t
  (foldable flushable call unsafe))

(defknown fill (sequence t &key (:start index) (:end sequence-end)) sequence
  (unsafe)
  :derive-type #'result-type-first-arg)

(defknown replace (sequence
		   sequence
		   &key
		   (:start1 index)
		   (:end1 sequence-end)
		   (:start2 index)
		   (:end2 sequence-end))
  sequence ()
  :derive-type #'result-type-first-arg)

(defknown remove
  (t sequence &key (:from-end t) (:test callable)
     (:test-not callable) (:start index) (:end sequence-end)
     (:count sequence-end) (:key callable))
  consed-sequence
  (flushable call)
  :derive-type (sequence-result-nth-arg 2))

(defknown substitute
  (t t sequence &key (:from-end t) (:test callable)
     (:test-not callable) (:start index) (:end sequence-end)
     (:count sequence-end) (:key callable))
  consed-sequence
  (flushable call)
  :derive-type (sequence-result-nth-arg 3))

(defknown (remove-if remove-if-not)
  (callable sequence &key (:from-end t) (:start index) (:end sequence-end)
	    (:count sequence-end) (:key callable))
  consed-sequence
  (flushable call)
  :derive-type (sequence-result-nth-arg 2))

(defknown (substitute-if substitute-if-not)
  (t callable sequence &key (:from-end t) (:start index) (:end sequence-end)
     (:count sequence-end) (:key callable))
  consed-sequence
  (flushable call)
  :derive-type (sequence-result-nth-arg 3))

(defknown delete
  (t sequence &key (:from-end t) (:test callable)
     (:test-not callable) (:start index) (:end sequence-end)
     (:count sequence-end) (:key callable))
  sequence
  (flushable call)
  :derive-type (sequence-result-nth-arg 2))

(defknown nsubstitute
  (t t sequence &key (:from-end t) (:test callable)
     (:test-not callable) (:start index) (:end sequence-end)
     (:count sequence-end) (:key callable))
  sequence
  (flushable call)
  :derive-type (sequence-result-nth-arg 3))

(defknown (delete-if delete-if-not)
  (callable sequence &key (:from-end t) (:start index) (:end sequence-end)
	    (:count sequence-end) (:key callable))
  sequence
  (flushable call)
  :derive-type (sequence-result-nth-arg 2))

(defknown (nsubstitute-if nsubstitute-if-not)
  (t callable sequence &key (:from-end t) (:start index) (:end sequence-end)
     (:count sequence-end) (:key callable))
  sequence
  (flushable call)
  :derive-type (sequence-result-nth-arg 3))

(defknown remove-duplicates
  (sequence &key (:test callable) (:test-not callable) (:start index) (:from-end t)
	    (:end sequence-end) (:key callable))
  consed-sequence
  (flushable call)
  :derive-type (sequence-result-nth-arg 1))

(defknown delete-duplicates
  (sequence &key (:test callable) (:test-not callable) (:start index) (:from-end t)
	    (:end sequence-end) (:key callable))
  sequence
  (flushable call)
  :derive-type (sequence-result-nth-arg 1))

(defknown find (t sequence &key (:test callable) (:test-not callable)
		  (:start index) (:from-end t) (:end sequence-end) (:key callable))
  t
  (foldable flushable call))

(defknown (find-if find-if-not)
  (callable sequence &key (:from-end t) (:start index) (:end sequence-end)
	    (:key callable))
  t
  (foldable flushable call))

(defknown position (t sequence &key (:test callable) (:test-not callable)
		      (:start index) (:from-end t) (:end sequence-end)
		      (:key callable))
  (or index null)
  (foldable flushable call))

(defknown (position-if position-if-not)
  (callable sequence &key (:from-end t) (:start index) (:end sequence-end)
	    (:key callable))
  (or index null)
  (foldable flushable call))

(defknown count (t sequence &key (:test callable) (:test-not callable)
		      (:start index) (:from-end t) (:end sequence-end)
		      (:key callable))
  index
  (foldable flushable call))

(defknown (count-if count-if-not)
  (callable sequence &key (:from-end t) (:start index) (:end sequence-end)
	    (:key callable))
  index
  (foldable flushable call))

(defknown (mismatch search)
  (sequence sequence &key (:from-end t) (:test callable) (:test-not callable)
	    (:start1 index) (:end1 sequence-end) (:start2 index) (:end2 sequence-end)
	    (:key callable))
  (or index null)
  (foldable flushable call))

;;; not FLUSHABLE, since vector sort guaranteed in-place...
(defknown (stable-sort sort) (sequence callable &key (:key callable)) sequence
  (call)
  :derive-type (sequence-result-nth-arg 1))

(defknown merge (type-specifier sequence sequence callable
				&key (:key callable))
  sequence
  (flushable call)
  :derive-type (result-type-specifier-nth-arg 1))

;;; not FLUSHABLE, despite what CMU CL's DEFKNOWN said..
(defknown read-sequence (sequence stream
				  &key
				  (:start index)
				  (:end sequence-end))
  (index)
  ())

(defknown write-sequence (sequence stream
				   &key
				   (:start index)
				   (:end sequence-end))
  sequence
  ()
  :derive-type (sequence-result-nth-arg 1))

;;;; from the "Manipulating List Structure" chapter:
(defknown (car cdr caar cadr cdar cddr
	       caaar caadr cadar caddr cdaar cdadr cddar cdddr
	       caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
	       cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
	       first second third fourth fifth sixth seventh eighth ninth tenth
	       rest)
  (list)
  t
  (foldable flushable))

(defknown cons (t t) cons (movable flushable unsafe))

(defknown tree-equal (t t &key (:test callable) (:test-not callable)) boolean
  (foldable flushable call))
(defknown endp (t) boolean (foldable flushable movable))
(defknown list-length (list) (or index null) (foldable flushable))
(defknown (nth nthcdr) (index list) t (foldable flushable))
(defknown last (list &optional index) list (foldable flushable))
(defknown list (&rest t) list (movable flushable unsafe))
(defknown list* (t &rest t) t (movable flushable unsafe))
(defknown make-list (index &key (:initial-element t)) list
  (movable flushable unsafe))

;;; All but last must be list...
(defknown append (&rest t) t (flushable))

(defknown copy-list (list) list (flushable))
(defknown copy-alist (list) list (flushable))
(defknown copy-tree (t) t (flushable recursive))
(defknown revappend (list t) t (flushable))
(defknown nconc (&rest list) list ())
(defknown nreconc (list t) list ())
(defknown butlast (list &optional index) list (flushable))
(defknown nbutlast (list &optional index) list ())
(defknown ldiff (list t) list (flushable))
(defknown (rplaca rplacd) (cons t) list (unsafe))

(defknown (nsubst subst) (t t t &key (:key callable) (:test callable)
			    (:test-not callable))
  list (flushable unsafe call))

(defknown (subst-if subst-if-not nsubst-if nsubst-if-not)
	  (t t t &key (:key callable))
  list (flushable unsafe call))

(defknown (sublis nsublis) (list t &key (:key callable) (:test callable)
				 (:test-not callable))
  list (flushable unsafe call))

(defknown member (t list &key (:key callable) (:test callable)
		    (:test-not callable))
  list (foldable flushable call))
(defknown (member-if member-if-not) (callable list &key (:key callable))
  list (foldable flushable call))

(defknown tailp (t list) boolean (foldable flushable))

(defknown adjoin (t list &key (:key callable) (:test callable)
		    (:test-not callable))
  list (foldable flushable unsafe call))

(defknown (union intersection set-difference set-exclusive-or)
	  (list list &key (:key callable) (:test callable) (:test-not callable))
  list
  (foldable flushable call))

(defknown (nunion nintersection nset-difference nset-exclusive-or)
	  (list list &key (:key callable) (:test callable) (:test-not callable))
  list
  (foldable flushable call))

(defknown subsetp
	  (list list &key (:key callable) (:test callable) (:test-not callable))
  boolean
  (foldable flushable call))

(defknown acons (t t t) list (movable flushable unsafe))
(defknown pairlis (t t &optional t) list (flushable unsafe))

(defknown (rassoc assoc)
	  (t list &key (:key callable) (:test callable) (:test-not callable))
  list (foldable flushable call))
(defknown (assoc-if-not assoc-if rassoc-if rassoc-if-not)
	  (callable list &key (:key callable)) list (foldable flushable call))

(defknown (memq assq) (t list) list (foldable flushable unsafe))
(defknown delq (t list) list (flushable unsafe))

;;;; from the "Hash Tables" chapter:

(defknown make-hash-table
  (&key (:test callable) (:size unsigned-byte)
        (:rehash-size (or (integer 1) (float (1.0))))
        (:rehash-threshold (real 0 1))
        (:weak-p t))
  hash-table
  (flushable unsafe))
(defknown hash-table-p (t) boolean (movable foldable flushable))
(defknown gethash (t hash-table &optional t) (values t boolean)
  (foldable flushable unsafe))
(defknown %puthash (t hash-table t) t (unsafe))
(defknown remhash (t hash-table) boolean ())
(defknown maphash (callable hash-table) null (foldable flushable call))
(defknown clrhash (hash-table) hash-table ())
(defknown hash-table-count (hash-table) index (foldable flushable))
(defknown hash-table-rehash-size (hash-table) (or (integer 1) (float (1.0)))
  (foldable flushable))
(defknown hash-table-rehash-threshold (hash-table) (real 0 1)
  (foldable flushable))
(defknown hash-table-size (hash-table) index (foldable flushable))
(defknown hash-table-test (hash-table) symbol (foldable flushable))
(defknown sxhash (t) (integer 0 #.sb!vm:*target-most-positive-fixnum*)
  (foldable flushable))

;;;; from the "Arrays" chapter

(defknown make-array ((or index list)
		      &key
		      (:element-type type-specifier)
		      (:initial-element t)
		      (:initial-contents t)
		      (:adjustable t)
		      (:fill-pointer t)
		      (:displaced-to (or array null))
		      (:displaced-index-offset index))
  array (flushable unsafe))

(defknown vector (&rest t) simple-vector (flushable unsafe))

(defknown aref (array &rest index) t (foldable flushable))
(defknown row-major-aref (array index) t (foldable flushable))

(defknown array-element-type (array)
  type-specifier
  (foldable flushable recursive))
(defknown array-rank (array) array-rank (foldable flushable))
(defknown array-dimension (array array-rank) index (foldable flushable))
(defknown array-dimensions (array) list (foldable flushable))
(defknown array-in-bounds-p (array &rest index) boolean (foldable flushable))
(defknown array-row-major-index (array &rest index) array-total-size
  (foldable flushable))
(defknown array-total-size (array) array-total-size (foldable flushable))
(defknown adjustable-array-p (array) boolean (movable foldable flushable))

(defknown svref (simple-vector index) t (foldable flushable))
(defknown bit ((array bit) &rest index) bit (foldable flushable))
(defknown sbit ((simple-array bit) &rest index) bit (foldable flushable))

(defknown (bit-and bit-ior bit-xor bit-eqv bit-nand bit-nor bit-andc1 bit-andc2
		   bit-orc1 bit-orc2)
  ((array bit) (array bit) &optional (or (array bit) (member t)))
  (array bit)
  (foldable)
  #|:derive-type #'result-type-last-arg|#)

(defknown bit-not ((array bit) &optional (or (array bit) (member t)))
  (array bit)
  (foldable)
  #|:derive-type #'result-type-last-arg|#)

(defknown array-has-fill-pointer-p (array) boolean (movable foldable flushable))
(defknown fill-pointer (vector) index (foldable flushable))
(defknown vector-push (t vector) (or index null) ())
(defknown vector-push-extend (t vector &optional index) index ())
(defknown vector-pop (vector) t ())

(defknown adjust-array
  (array (or index list) &key (:element-type type-specifier)
	 (:initial-element t) (:initial-contents list)
	 (:fill-pointer t) (:displaced-to (or array null))
	 (:displaced-index-offset index))
  array (unsafe))
;  :derive-type 'result-type-arg1) Not even close...

;;;; from the "Strings" chapter:

(defknown char (string index) character (foldable flushable))
(defknown schar (simple-string index) character (foldable flushable))

(sb!xc:deftype stringable () '(or character string symbol))

(defknown (string= string-equal)
  (stringable stringable &key (:start1 index) (:end1 sequence-end)
	      (:start2 index) (:end2 sequence-end))
  boolean
  (foldable flushable))

(defknown (string< string> string<= string>= string/= string-lessp
		   string-greaterp string-not-lessp string-not-greaterp
		   string-not-equal)
  (stringable stringable &key (:start1 index) (:end1 sequence-end)
	      (:start2 index) (:end2 sequence-end))
  (or index null)
  (foldable flushable))

(defknown make-string (index &key (:element-type type-specifier)
		       (:initial-element character))
  simple-string (flushable))

(defknown (string-trim string-left-trim string-right-trim)
  (sequence stringable) simple-string (flushable))

(defknown (string-upcase string-downcase string-capitalize)
  (stringable &key (:start index) (:end sequence-end))
  simple-string (flushable))

(defknown (nstring-upcase nstring-downcase nstring-capitalize)
  (string &key (:start index) (:end sequence-end))
  string ())

(defknown string (stringable) string
  (flushable explicit-check))

;;;; internal non-keyword versions of string predicates:

(defknown (string<* string>* string<=* string>=* string/=*)
  (stringable stringable index sequence-end index sequence-end)
  (or index null)
  (foldable flushable))

(defknown string=*
  (stringable stringable index sequence-end index sequence-end)
  boolean
  (foldable flushable))

;;;; from the "Eval" chapter:

(defknown eval (t) * (recursive))
(defknown constantp (t &optional lexenv) boolean
  (foldable flushable))

;;;; from the "Streams" chapter:

(defknown make-synonym-stream (symbol) stream (flushable))
(defknown make-broadcast-stream (&rest stream) stream (flushable))
(defknown make-concatenated-stream (&rest stream) stream (flushable))
(defknown make-two-way-stream (stream stream) stream (flushable))
(defknown make-echo-stream (stream stream) stream (flushable))
(defknown make-string-input-stream (string &optional index index) stream (flushable unsafe))
(defknown make-string-output-stream () stream (flushable))
(defknown get-output-stream-string (stream) simple-string ())
(defknown streamp (t) boolean (movable foldable flushable))
(defknown stream-element-type (stream) type-specifier (movable foldable flushable))
(defknown (output-stream-p input-stream-p) (stream) boolean (movable foldable
								     flushable))
(defknown close (stream &key (:abort t)) stream ())

;;;; from the "Input/Output" chapter:

;;; The I/O functions are currently given effects ANY under the theory
;;; that code motion over I/O operations is particularly confusing and
;;; not very important for efficency.

(defknown copy-readtable (&optional (or readtable null) readtable) readtable
  ())
(defknown readtablep (t) boolean (movable foldable flushable))

(defknown set-syntax-from-char
  (character character &optional (or readtable null) readtable) (eql t)
  ())

(defknown set-macro-character (character callable &optional t readtable) (eql t)
  (unsafe))
(defknown get-macro-character (character &optional readtable)
  (values callable boolean) (flushable))

(defknown make-dispatch-macro-character (character &optional t readtable)
  (eql t) ())
(defknown set-dispatch-macro-character
  (character character callable &optional readtable) (eql t)
  (unsafe))
(defknown get-dispatch-macro-character
  (character character &optional readtable) callable
  (flushable))

;;; may return any type due to eof-value...
(defknown (read read-preserving-whitespace read-char-no-hang read-char)
  (&optional streamlike t t t) t  (explicit-check))

(defknown read-delimited-list (character &optional streamlike t) t
  (explicit-check))
(defknown read-line (&optional streamlike t t t) (values t boolean)
  (explicit-check))
(defknown unread-char (character &optional streamlike) t
  (explicit-check))
(defknown peek-char (&optional (or character (member nil t)) streamlike t t t)
  t
  (explicit-check))
(defknown listen (&optional streamlike) boolean (flushable explicit-check))

(defknown clear-input (&optional stream) null (explicit-check))

(defknown read-from-string
  (string &optional t t
	  &key
	  (:start index)
	  (:end sequence-end)
	  (:preserve-whitespace t))
  (values t index))
(defknown parse-integer
  (string &key
	  (:start index)
	  (:end sequence-end)
	  (:radix (integer 2 36))
	  (:junk-allowed t))
  (values (or integer null ()) index))

(defknown read-byte (stream &optional t t) t (explicit-check))

(defknown write
  (t &key
     (:stream streamlike)
     (:escape t)
     (:radix t)
     (:base (integer 2 36))
     (:circle t)
     (:pretty t)
     (:level (or unsigned-byte null))
     (:readably t)
     (:length (or unsigned-byte null))
     (:case t)
     (:array t)
     (:gensym t)
     (:lines (or unsigned-byte null))
     (:right-margin (or unsigned-byte null))
     (:miser-width (or unsigned-byte null))
     (:pprint-dispatch t))
  t
  (any explicit-check)
  :derive-type #'result-type-first-arg)

(defknown (prin1 print princ) (t &optional streamlike) t (any explicit-check)
  :derive-type #'result-type-first-arg)

;;; xxx-TO-STRING functions are not foldable because they depend on
;;; the dynamic environment.
(defknown write-to-string
  (t &key (:escape t) (:radix t) (:base (integer 2 36)) (:readably t)
     (:circle t) (:pretty t) (:level (or unsigned-byte null))
     (:length (or unsigned-byte null)) (:case t) (:array t) (:gensym t)
     (:lines (or unsigned-byte null)) (:right-margin (or unsigned-byte null))
     (:miser-width (or unsigned-byte null)) (:pprint-dispatch t))
  simple-string
  (foldable flushable explicit-check))

(defknown (prin1-to-string princ-to-string) (t) simple-string (flushable))

(defknown write-char (character &optional streamlike) character
  (explicit-check))
(defknown (write-string write-line)
  (string &optional streamlike &key (:start index) (:end sequence-end))
  string
  (explicit-check))

(defknown (terpri finish-output force-output clear-output)
  (&optional streamlike) null
  (explicit-check))

(defknown fresh-line (&optional streamlike) boolean
  (explicit-check))

(defknown write-byte (integer stream) integer
  (explicit-check))

(defknown format ((or streamlike string) (or string function) &rest t)
  (or string null)
  (explicit-check))

(defknown (y-or-n-p yes-or-no-p) (&optional string &rest t) boolean
  (explicit-check))

;;;; from the "File System Interface" chapter:

;;; No pathname functions are foldable because they all potentially
;;; depend on *DEFAULT-PATHNAME-DEFAULTS*, e.g. to provide a default
;;; host when parsing a namestring.

(defknown wild-pathname-p (pathname-designator
			   &optional
			   (member nil :host :device
				   :directory :name
				   :type :version))
  boolean
  (flushable))
(defknown pathname-match-p (pathname-designator pathname-designator) boolean
  (flushable))
(defknown translate-pathname (pathname-designator
			      pathname-designator
			      pathname-designator &key)
  pathname
  (flushable))

;;; KLUDGE: There was a comment from CMU CL here, "We need to add the
;;; logical pathname stuff here." -- WHN 19991213

(defknown pathname (pathname-designator) pathname (flushable))
(defknown truename (pathname-designator) pathname ())

(defknown parse-namestring
  (pathname-designator &optional pathname-host pathname-designator
		       &key
		       (:start index)
		       (:end sequence-end)
		       (:junk-allowed t))
  (values (or pathname null) index)
  ())

(defknown merge-pathnames
  (pathname-designator &optional pathname-designator pathname-version)
  pathname
  (flushable))

(defknown make-pathname
 (&key (:defaults pathname-designator)
       (:host (or string pathname-host))
       (:device (or string pathname-device))
       (:directory (or pathname-directory string (member :wild)))
       (:name (or pathname-name string (member :wild)))
       (:type (or pathname-type string (member :wild)))
       (:version pathname-version) (:case (member :local :common)))
  pathname (flushable))

(defknown pathnamep (t) boolean (movable flushable))

(defknown pathname-host (pathname-designator
			 &key (:case (member :local :common)))
  pathname-host (flushable))
(defknown pathname-device (pathname-designator
			   &key (:case (member :local :common)))
  pathname-device (flushable))
(defknown pathname-directory (pathname-designator
			      &key (:case (member :local :common)))
  pathname-directory (flushable))
(defknown pathname-name (pathname-designator
			 &key (:case (member :local :common)))
  pathname-name (flushable))
(defknown pathname-type (pathname-designator
			 &key (:case (member :local :common)))
  pathname-type (flushable))
(defknown pathname-version (pathname-designator)
  pathname-version (flushable))

(defknown (namestring file-namestring directory-namestring host-namestring)
  (pathname-designator) simple-string
  (flushable))

(defknown enough-namestring (pathname-designator &optional pathname-designator)
  simple-string
  (flushable))

(defknown user-homedir-pathname (&optional t) pathname (flushable))

(defknown open
  (pathname-designator &key
		       (:direction (member :input :output :io :probe))
		       (:element-type type-specifier)
		       (:if-exists (member :error :new-version :rename
					   :rename-and-delete :overwrite
					   :append :supersede nil))
		       (:if-does-not-exist (member :error :create nil))
		       (:external-format (member :default)))
  (or stream null))

(defknown rename-file (pathname-designator filename)
  (values pathname pathname pathname))
(defknown delete-file (pathname-designator) t)
(defknown probe-file (pathname-designator) (or pathname null) (flushable))
(defknown file-write-date (pathname-designator) (or unsigned-byte null)
  (flushable))
(defknown file-author (pathname-designator) (or simple-string null)
  (flushable))

(defknown file-position (stream &optional
				(or unsigned-byte (member :start :end)))
  (or unsigned-byte (member t nil)))
(defknown file-length (stream) (or unsigned-byte null) (flushable))

(defknown load
  ((or filename stream)
   &key
   (:verbose t)
   (:print t)
   (:if-does-not-exist (member :error :create nil))
   ;; FIXME: ANSI specifies an :EXTERNAL-FORMAT keyword too.
   )
  t)

(defknown directory (pathname-designator &key
					 (:check-for-subdirs t)
					 (:all t)
					 (:follow-links t))
  list (flushable))

;;;; from the "Errors" chapter:

(defknown error (t &rest t) nil) ; never returns...
(defknown cerror (string t &rest t) null)
(defknown warn (t &rest t) null)
(defknown break (&optional t &rest t) null)

;;;; from the "Miscellaneous" Chapter:

(defknown compile ((or symbol cons) &optional (or list function null))
  (values (or function symbol cons) boolean boolean))

(defknown compile-file
  (filename
   &key
   (:output-file (or filename
		     null
		     ;; FIXME: This last case is a non-ANSI hack.
		     (member t)))
   (:verbose t)
   (:print t)
   (:external-format t)
   (:block-compile t)
   (:entry-points list)
   (:byte-compile (member t nil :maybe)))
  (values (or pathname null) boolean boolean))

(defknown disassemble (callable &key
				(:stream stream)
				(:use-labels t))
  null)

(defknown fdocumentation (t symbol)
  (or string null)
  (flushable))

(defknown describe (t &optional (or stream (member t nil))) (values))
(defknown inspect (t) (values))

(defknown room (&optional (member t nil :default)) (values))
(defknown ed (&optional (or symbol cons filename) &key (:init t) (:display t))
  t)
(defknown dribble (&optional filename &key (:if-exists t)) t)

(defknown apropos      (stringable &optional package-designator t) (values))
(defknown apropos-list (stringable &optional package-designator t) list
  (flushable))

(defknown get-decoded-time ()
  (values (integer 0 59) (integer 0 59) (integer 0 23) (integer 1 31)
	  (integer 1 12) unsigned-byte (integer 0 6) boolean (rational -24 24))
  (flushable))

(defknown get-universal-time () unsigned-byte (flushable))

(defknown decode-universal-time
	  (unsigned-byte &optional (or null (rational -24 24)))
  (values (integer 0 59) (integer 0 59) (integer 0 23) (integer 1 31)
	  (integer 1 12) unsigned-byte (integer 0 6) boolean (rational -24 24))
  (flushable))

(defknown encode-universal-time
  ((integer 0 59) (integer 0 59) (integer 0 23) (integer 1 31)
   (integer 1 12) unsigned-byte &optional (or null (rational -24 24)))
  unsigned-byte
  (flushable))

(defknown (get-internal-run-time get-internal-real-time)
  () internal-time (flushable))

(defknown sleep ((or (rational 0) (float 0.0))) null)

;;; Even though ANSI defines LISP-IMPLEMENTATION-TYPE and
;;; LISP-IMPLEMENTATION-VERSION to possibly punt and return NIL, we
;;; know that there's no valid reason for our implementations to ever
;;; do so, so we can safely guarantee that they'll return strings.
(defknown (lisp-implementation-type lisp-implementation-version)
  () simple-string (flushable))

;;; For any of these functions, meaningful information might not be
;;; available, so -- unlike the related LISP-IMPLEMENTATION-FOO
;;; functions -- these really can return NIL.
(defknown (machine-type machine-version machine-instance
	   software-type software-version
	   short-site-name long-site-name)
  () (or simple-string null) (flushable))

(defknown identity (t) t (movable foldable flushable unsafe)
  :derive-type #'result-type-first-arg)

;;; &OPTIONAL is to agree with the optimization in the interpreter stub.
(defknown constantly (t &optional t t &rest t) function (movable flushable))
(defknown complement (function) function (movable flushable))

;;;; magical compiler frobs

;;; We can't fold this in general because of SATISFIES. There is a
;;; special optimizer anyway.
(defknown %typep (t (or type-specifier ctype)) boolean
  (movable flushable explicit-check))
(defknown %instance-typep (t (or type-specifier ctype)) boolean
  (movable flushable explicit-check))

(defknown %cleanup-point () t)
(defknown %special-bind (t t) t)
(defknown %special-unbind (t) t)
(defknown %listify-rest-args (t index) list (flushable))
(defknown %more-arg-context (t t) (values t index) (flushable))
(defknown %more-arg (t index) t)
(defknown %more-arg-values (t index index) * (flushable))
(defknown %verify-argument-count (index index) (values))
(defknown %argument-count-error (t) nil)
(defknown %unknown-values () *)
(defknown %catch (t t) t)
(defknown %unwind-protect (t t) t)
(defknown (%catch-breakup %unwind-protect-breakup) () t)
(defknown %lexical-exit-breakup (t) t)
(defknown %continue-unwind (t t t) nil)
(defknown %throw (t &rest t) nil) ; This is MV-called.
(defknown %nlx-entry (t) *)
(defknown %%primitive (t t &rest t) *)
(defknown %pop-values (t) t)
(defknown %type-check-error (t t) nil)
(defknown %odd-keyword-arguments-error () nil)
(defknown %unknown-keyword-argument-error (t) nil)
(defknown (%ldb %mask-field) (bit-index bit-index integer) unsigned-byte
  (movable foldable flushable explicit-check))
(defknown (%dpb %deposit-field) (integer bit-index bit-index integer) integer
  (movable foldable flushable explicit-check))
(defknown %negate (number) number (movable foldable flushable explicit-check))
(defknown %check-bound (array index fixnum) index (movable foldable flushable))
(defknown data-vector-ref (simple-array index) t (foldable flushable explicit-check))
(defknown data-vector-set (array index t) t (unsafe explicit-check))
(defknown hairy-data-vector-ref (array index) t (foldable flushable explicit-check))
(defknown hairy-data-vector-set (array index t) t (unsafe explicit-check))
(defknown sb!kernel:%caller-frame-and-pc () (values t t) (flushable))
(defknown sb!kernel:%with-array-data (array index (or index null))
  (values (simple-array * (*)) index index index)
  (foldable flushable))
(defknown %set-symbol-package (symbol t) t (unsafe))
(defknown %coerce-name-to-function ((or symbol cons)) function (flushable))
(defknown %coerce-callable-to-function (callable) function (flushable))

;;; Structure slot accessors or setters are magically "known" to be
;;; these functions, although the var remains the Slot-Accessor
;;; describing the actual function called.
;;;
;;; FIXME: It would be nice to make structure slot accessors be
;;; ordinary functions (proclaimed as SB-EXT:CONSTANT-FUNCTION, but
;;; otherwise ordinary).
(defknown %slot-accessor (t) t (flushable))
(defknown %slot-setter (t t) t (unsafe))

;;;; SETF inverses

(defknown %aset (array &rest t) t (unsafe))
(defknown %set-row-major-aref (array index t) t (unsafe))
(defknown %rplaca (cons t) t (unsafe))
(defknown %rplacd (cons t) t (unsafe))
(defknown %put (symbol t t) t (unsafe))
(defknown %setelt (sequence index t) t (unsafe))
(defknown %svset (simple-vector index t) t (unsafe))
(defknown %bitset (bit-vector &rest index) bit (unsafe))
(defknown %sbitset (simple-bit-vector &rest index) bit (unsafe))
(defknown %charset (string index character) character (unsafe))
(defknown %scharset (simple-string index character) character (unsafe))
(defknown %set-symbol-value (symbol t) t (unsafe))
(defknown fset (symbol function) function (unsafe))
(defknown %set-symbol-plist (symbol t) t (unsafe))
(defknown (setf fdocumentation) ((or string null) t symbol)
  (or string null)
  ())
(defknown %setnth (index list t) t (unsafe))
(defknown %set-fill-pointer (vector index) index (unsafe))

;;;; internal type predicates

;;; Simple TYPEP uses that don't have any standard predicate are
;;; translated into non-standard unary predicates.
(defknown (fixnump bignump ratiop short-float-p single-float-p double-float-p
	   long-float-p base-char-p %standard-char-p %instancep
	   array-header-p)
  (t) boolean (movable foldable flushable))

;;;; miscellaneous "sub-primitives"

(defknown %sp-string-compare
  (simple-string index index simple-string index index)
  (or index null)
  (foldable flushable))
