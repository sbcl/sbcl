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

;;;; information for known functions:

(defknown coerce (t type-specifier) t
    ;; Note:
    ;; This is not FLUSHABLE because it's defined to signal errors.
    (movable)
  ;; :DERIVE-TYPE RESULT-TYPE-SPEC-NTH-ARG 2 ? Nope... (COERCE 1 'COMPLEX)
  ;; returns REAL/INTEGER, not COMPLEX.
  )
(defknown list-to-vector* (list type-specifier) vector)
(defknown vector-to-vector* (vector type-specifier) vector)

(defknown type-of (t) t (foldable flushable))

;;; These can be affected by type definitions, so they're not FOLDABLE.
(defknown (sb!xc:upgraded-complex-part-type sb!xc:upgraded-array-element-type)
    (type-specifier &optional lexenv-designator) type-specifier
    (unsafely-flushable))

;;;; from the "Predicates" chapter:

;;; FIXME: Is it right to have TYPEP (and TYPE-OF, elsewhere; and
;;; perhaps SPECIAL-OPERATOR-P and others) be FOLDABLE in the
;;; cross-compilation host? After all, some type relationships (e.g.
;;; FIXNUMness) might be different between host and target. Perhaps
;;; this property should be protected by #-SB-XC-HOST? Perhaps we need
;;; 3-stage bootstrapping after all? (Ugh! It's *so* slow already!)
(defknown typep (t type-specifier &optional lexenv-designator) t
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
   (foldable))
(defknown subtypep (type-specifier type-specifier &optional lexenv-designator)
  (values boolean boolean)
  ;; This is not FOLDABLE because its value is affected by type
  ;; definitions.
  ;;
  ;; FIXME: Is it OK to fold this when the types have already been
  ;; defined? Does the code inherited from CMU CL already do this?
  (unsafely-flushable))

(defknown (null symbolp atom consp listp numberp integerp rationalp floatp
                complexp characterp stringp bit-vector-p vectorp
                simple-vector-p simple-string-p simple-bit-vector-p arrayp
                sb!xc:packagep functionp compiled-function-p not)
  (t) boolean (movable foldable flushable))

(defknown (eq eql) (t t) boolean (movable foldable flushable))
(defknown (equal equalp) (t t) boolean (foldable flushable recursive))

;;;; classes

(sb!xc:deftype name-for-class () t)
(defknown classoid-name (classoid) name-for-class (flushable))
(defknown find-classoid (name-for-class &optional t)
  (or classoid null) ())
(defknown classoid-of (t) classoid (flushable))
(defknown layout-of (t) layout (flushable))
(defknown copy-structure (structure-object) structure-object
  (flushable unsafe))

;;;; from the "Control Structure" chapter:

;;; This is not FLUSHABLE, since it's required to signal an error if
;;; unbound.
(defknown (symbol-value) (symbol) t ())
;;; From CLHS, "If the symbol is globally defined as a macro or a
;;; special operator, an object of implementation-dependent nature and
;;; identity is returned. If the symbol is not globally defined as
;;; either a macro or a special operator, and if the symbol is fbound,
;;; a function object is returned".  Our objects of
;;; implementation-dependent nature happen to be functions.
(defknown (symbol-function) (symbol) function ())

(defknown boundp (symbol) boolean (flushable))
(defknown fboundp ((or symbol cons)) boolean (unsafely-flushable explicit-check))
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
  ((or list symbol) &optional lexenv-designator)
  (values list list list form form)
  (flushable))
(defknown apply (callable t &rest t) *) ; ### Last arg must be List...
(defknown funcall (callable &rest t) *)

(defknown (mapcar maplist) (callable list &rest list) list
  (call))

;;; According to CLHS the result must be a LIST, but we do not check
;;; it.
(defknown (mapcan mapcon) (callable list &rest list) t
  (call))

(defknown (mapc mapl) (callable list &rest list) list (foldable call))

;;; We let VALUES-LIST be foldable, since constant-folding will turn
;;; it into VALUES. VALUES is not foldable, since MV constants are
;;; represented by a call to VALUES.
(defknown values (&rest t) * (movable flushable unsafe))
(defknown values-list (list) * (movable foldable unsafely-flushable))

;;;; from the "Macros" chapter:

(defknown macro-function (symbol &optional lexenv-designator)
  (or function null)
  (flushable))
(defknown (macroexpand macroexpand-1) (t &optional lexenv-designator)
  (values form &optional boolean))

(defknown compiler-macro-function (t &optional lexenv-designator)
  (or function null)
  (flushable))

;;;; from the "Declarations" chapter:

(defknown proclaim (list) (values) (recursive))

;;;; from the "Symbols" chapter:

(defknown get (symbol t &optional t) t (flushable))
(defknown sb!impl::get2 (symbol t) t (flushable))
(defknown sb!impl::get3 (symbol t t) t (flushable))
(defknown remprop (symbol t) t)
(defknown symbol-plist (symbol) list (flushable))
(defknown getf (list t &optional t) t (foldable flushable))
(defknown get-properties (list list) (values t t list) (foldable flushable))
(defknown symbol-name (symbol) simple-string (movable foldable flushable))
(defknown make-symbol (string) symbol (flushable))
(defknown %make-symbol (simple-string) symbol (flushable))
(defknown copy-symbol (symbol &optional t) symbol (flushable))
(defknown gensym (&optional (or string unsigned-byte)) symbol ())
(defknown symbol-package (symbol) (or sb!xc:package null) (flushable))
(defknown keywordp (t) boolean (flushable))       ; If someone uninterns it...

;;;; from the "Packages" chapter:

(defknown gentemp (&optional string package-designator) symbol)

(defknown make-package (string-designator &key
                                          (:use list)
                                          (:nicknames list)
                                          ;; ### extensions...
                                          (:internal-symbols index)
                                          (:external-symbols index))
  sb!xc:package)
(defknown find-package (package-designator) (or sb!xc:package null)
  (flushable))
(defknown package-name (package-designator) (or simple-string null)
  (unsafely-flushable))
(defknown package-nicknames (package-designator) list (unsafely-flushable))
(defknown rename-package (package-designator package-designator &optional list)
  sb!xc:package)
(defknown package-use-list (package-designator) list (unsafely-flushable))
(defknown package-used-by-list (package-designator) list (unsafely-flushable))
(defknown package-shadowing-symbols (package-designator) list (unsafely-flushable))
(defknown list-all-packages () list (flushable))
(defknown intern (string &optional package-designator)
  (values symbol (member :internal :external :inherited nil))
  ())
(defknown find-symbol (string &optional package-designator)
  (values symbol (member :internal :external :inherited nil))
  (flushable))
(defknown (export import) (symbols-designator &optional package-designator)
  (eql t))
(defknown unintern (symbol &optional package-designator) boolean)
(defknown unexport (symbols-designator &optional package-designator) (eql t))
(defknown shadowing-import (symbols-designator &optional package-designator)
  (eql t))
(defknown shadow ((or symbol character string list) &optional package-designator)
  (eql t))
(defknown (use-package unuse-package)
  ((or list package-designator) &optional package-designator) (eql t))
(defknown find-all-symbols (string-designator) list (flushable))

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
  (movable foldable unsafely-flushable explicit-check))
(defknown (1+ 1-) (number) number
  (movable foldable flushable explicit-check))

(defknown conjugate (number) number
  (movable foldable flushable explicit-check))

(defknown gcd (&rest integer) unsigned-byte
  (movable foldable flushable explicit-check)
  #|:derive-type 'boolean-result-type|#)
(defknown lcm (&rest integer) unsigned-byte
  (movable foldable flushable explicit-check))

#+sb-xc-host ; (See CROSS-FLOAT-INFINITY-KLUDGE.)
(defknown exp (number) irrational
  (movable foldable flushable explicit-check recursive)
  :derive-type #'result-type-float-contagion)

#-sb-xc-host ; (See CROSS-FLOAT-INFINITY-KLUDGE.)
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

#+sb-xc-host ; (See CROSS-FLOAT-INFINITY-KLUDGE.)
(progn
(defknown (sin cos) (number)
  (or (float -1.0 1.0) (complex float))
  (movable foldable flushable explicit-check recursive)
  :derive-type #'result-type-float-contagion)

(defknown atan
  (number &optional real) irrational
  (movable foldable unsafely-flushable explicit-check recursive)
  :derive-type #'result-type-float-contagion)

(defknown (tan sinh cosh tanh asinh)
  (number) irrational (movable foldable flushable explicit-check recursive)
  :derive-type #'result-type-float-contagion)
) ; PROGN

#-sb-xc-host ; (See CROSS-FLOAT-INFINITY-KLUDGE.)
(progn
(defknown (sin cos) (number)
  (or (float -1.0 1.0) (complex float))
  (movable foldable flushable explicit-check recursive))

(defknown atan
  (number &optional real) irrational
  (movable foldable unsafely-flushable explicit-check recursive))

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
  (real &optional real) (values float real)
  (movable foldable flushable explicit-check))

(defknown decode-float (float) (values float float-exponent float)
  (movable foldable unsafely-flushable explicit-check))
(defknown scale-float (float integer) float
  (movable foldable unsafely-flushable explicit-check))
(defknown float-radix (float) float-radix
  (movable foldable unsafely-flushable))
(defknown float-sign (float &optional float) float
  (movable foldable unsafely-flushable explicit-check))
(defknown (float-digits float-precision) (float) float-digits
  (movable foldable unsafely-flushable explicit-check))
(defknown integer-decode-float (float)
    (values integer float-int-exponent (member -1 1))
    (movable foldable unsafely-flushable explicit-check))

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
(defknown logbitp (unsigned-byte integer) boolean (movable foldable flushable))
(defknown ash (integer integer) integer
  (movable foldable flushable explicit-check))
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
(defknown random ((or (float (0.0)) (integer 1)) &optional random-state)
  (or (float 0.0) (integer 0))
  (explicit-check))
(defknown make-random-state (&optional
                             (or (member nil t) random-state unsigned-byte
                                 (simple-array (unsigned-byte 8) (*))
                                 (simple-array (unsigned-byte 32) (*))))
  random-state (flushable))
(defknown random-state-p (t) boolean (movable foldable flushable))

;;;; from the "Characters" chapter:
(defknown (standard-char-p graphic-char-p alpha-char-p
                           upper-case-p lower-case-p both-case-p alphanumericp)
  (character) boolean (movable foldable flushable))

(defknown digit-char-p (character &optional (integer 2 36))
  (or (integer 0 35) null) (movable foldable flushable))

(defknown (char= char/= char< char> char<= char>= char-equal char-not-equal
                 char-lessp char-greaterp char-not-greaterp char-not-lessp)
  (character &rest character) boolean (movable foldable flushable))

(defknown character (t) character (movable foldable unsafely-flushable))
(defknown char-code (character) char-code (movable foldable flushable))
(defknown (char-upcase char-downcase) (character) character
  (movable foldable flushable))
(defknown digit-char (unsigned-byte &optional (integer 2 36))
  (or character null) (movable foldable flushable))
(defknown char-int (character) char-code (movable foldable flushable))
(defknown char-name (character) (or simple-string null)
  (movable foldable flushable))
(defknown name-char (string-designator) (or character null)
  (movable foldable flushable))
(defknown code-char (char-code) character
  ;; By suppressing constant folding on CODE-CHAR when the
  ;; cross-compiler is running in the cross-compilation host vanilla
  ;; ANSI Common Lisp, we can use CODE-CHAR expressions to delay until
  ;; target Lisp run time the generation of CHARACTERs which aren't
  ;; STANDARD-CHARACTERs. That way, we don't need to rely on the host
  ;; Common Lisp being able to handle any characters other than those
  ;; guaranteed by the ANSI spec.
  (movable #-sb-xc-host foldable flushable))

;;;; from the "Sequences" chapter:

(defknown elt (sequence index) t (foldable unsafely-flushable))

(defknown subseq (sequence index &optional sequence-end) consed-sequence
  (flushable)
  :derive-type (sequence-result-nth-arg 1))

(defknown copy-seq (sequence) consed-sequence (flushable)
  :derive-type (sequence-result-nth-arg 1))

(defknown length (sequence) index (foldable flushable))

(defknown reverse (sequence) consed-sequence (flushable)
  :derive-type (sequence-result-nth-arg 1))

(defknown nreverse (sequence) sequence (important-result)
  :derive-type #'result-type-first-arg
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 1))

(defknown make-sequence (type-specifier index
                                        &key
                                        (:initial-element t))
  consed-sequence
  (movable unsafe)
  :derive-type (creation-result-type-specifier-nth-arg 1))

(defknown concatenate (type-specifier &rest sequence) consed-sequence
  ()
  :derive-type (creation-result-type-specifier-nth-arg 1))

(defknown (map %map) (type-specifier callable sequence &rest sequence)
  consed-sequence
  (call)
; :DERIVE-TYPE 'TYPE-SPEC-ARG1 ? Nope... (MAP NIL ...) returns NULL, not NIL.
  )
(defknown %map-to-list-arity-1 (callable sequence) list (flushable call))
(defknown %map-to-simple-vector-arity-1 (callable sequence) simple-vector
  (flushable call))
(defknown %map-to-nil-on-simple-vector (callable simple-vector) null
  (flushable call))
(defknown %map-to-nil-on-vector (callable vector) null (flushable call))
(defknown %map-to-nil-on-sequence (callable sequence) null (flushable call))

(defknown map-into (sequence callable &rest sequence)
  sequence
  (call)
  :derive-type #'result-type-first-arg
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 1))

;;; returns the result from the predicate...
(defknown some (callable sequence &rest sequence) t
  (foldable unsafely-flushable call))

(defknown (every notany notevery) (callable sequence &rest sequence) boolean
  (foldable unsafely-flushable call))

;;; unsafe for :INITIAL-VALUE...
(defknown reduce (callable sequence &rest t &key (:from-end t) (:start index)
                  (:end sequence-end) (:initial-value t) (:key callable))
  t
  (foldable flushable call unsafe))

(defknown fill (sequence t &rest t &key
                         (:start index) (:end sequence-end)) sequence
  (unsafe)
  :derive-type #'result-type-first-arg
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 1)
  :result-arg 0)

(defknown replace (sequence sequence &rest t &key (:start1 index)
                   (:end1 sequence-end) (:start2 index) (:end2 sequence-end))
  sequence ()
  :derive-type #'result-type-first-arg
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 1)
  :result-arg 0)

(defknown remove
  (t sequence &rest t &key (:from-end t) (:test callable)
     (:test-not callable) (:start index) (:end sequence-end)
     (:count sequence-count) (:key callable))
  consed-sequence
  (flushable call)
  :derive-type (sequence-result-nth-arg 2))

(defknown substitute
  (t t sequence &rest t &key (:from-end t) (:test callable)
     (:test-not callable) (:start index) (:end sequence-end)
     (:count sequence-count) (:key callable))
  consed-sequence
  (flushable call)
  :derive-type (sequence-result-nth-arg 3))

(defknown (remove-if remove-if-not)
  (callable sequence &rest t &key (:from-end t) (:start index)
            (:end sequence-end) (:count sequence-count) (:key callable))
  consed-sequence
  (flushable call)
  :derive-type (sequence-result-nth-arg 2))

(defknown (substitute-if substitute-if-not)
  (t callable sequence &rest t &key (:from-end t) (:start index)
     (:end sequence-end) (:count sequence-count) (:key callable))
  consed-sequence
  (flushable call)
  :derive-type (sequence-result-nth-arg 3))

(defknown delete
  (t sequence &rest t &key (:from-end t) (:test callable)
     (:test-not callable) (:start index) (:end sequence-end)
     (:count sequence-count) (:key callable))
  sequence
  (flushable call important-result)
  :derive-type (sequence-result-nth-arg 2)
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 2))

(defknown nsubstitute
  (t t sequence &rest t &key (:from-end t) (:test callable)
     (:test-not callable) (:start index) (:end sequence-end)
     (:count sequence-count) (:key callable))
  sequence
  (flushable call)
  :derive-type (sequence-result-nth-arg 3)
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 3))

(defknown (delete-if delete-if-not)
  (callable sequence &rest t &key (:from-end t) (:start index)
            (:end sequence-end) (:count sequence-count) (:key callable))
  sequence
  (flushable call important-result)
  :derive-type (sequence-result-nth-arg 2)
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 2))

(defknown (nsubstitute-if nsubstitute-if-not)
  (t callable sequence &rest t &key (:from-end t) (:start index)
     (:end sequence-end) (:count sequence-count) (:key callable))
  sequence
  (flushable call)
  :derive-type (sequence-result-nth-arg 3)
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 3))

(defknown remove-duplicates
  (sequence &rest t &key (:test callable) (:test-not callable) (:start index)
            (:from-end t) (:end sequence-end) (:key callable))
  consed-sequence
  (unsafely-flushable call)
  :derive-type (sequence-result-nth-arg 1))

(defknown delete-duplicates
  (sequence &rest t &key (:test callable) (:test-not callable) (:start index)
            (:from-end t) (:end sequence-end) (:key callable))
  sequence
  (unsafely-flushable call important-result)
  :derive-type (sequence-result-nth-arg 1)
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 1))

(defknown find (t sequence &rest t &key (:test callable)
                (:test-not callable) (:start index) (:from-end t)
                (:end sequence-end) (:key callable))
  t
  (foldable flushable call))

(defknown (find-if find-if-not)
  (callable sequence &rest t &key (:from-end t) (:start index)
   (:end sequence-end) (:key callable))
  t
  (foldable flushable call))

(defknown position (t sequence &rest t &key (:test callable)
                    (:test-not callable) (:start index) (:from-end t)
                    (:end sequence-end) (:key callable))
  (or index null)
  (foldable flushable call))

(defknown (position-if position-if-not)
  (callable sequence &rest t &key (:from-end t) (:start index)
   (:end sequence-end) (:key callable))
  (or index null)
  (foldable flushable call))

(defknown count (t sequence &rest t &key
                   (:test callable) (:test-not callable) (:start index)
                   (:from-end t) (:end sequence-end) (:key callable))
  index
  (foldable flushable call))

(defknown (count-if count-if-not)
  (callable sequence &rest t &key
            (:from-end t) (:start index) (:end sequence-end) (:key callable))
  index
  (foldable flushable call))

(defknown (mismatch search)
  (sequence sequence &rest t &key (:from-end t) (:test callable)
   (:test-not callable) (:start1 index) (:end1 sequence-end)
   (:start2 index) (:end2 sequence-end) (:key callable))
  (or index null)
  (foldable flushable call))

;;; not FLUSHABLE, since vector sort guaranteed in-place...
(defknown (stable-sort sort) (sequence callable &rest t &key (:key callable))
  sequence
  (call)
  :derive-type (sequence-result-nth-arg 1)
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 1))
(defknown sb!impl::stable-sort-list (list function function) list
  (call important-result)
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 1))
(defknown sb!impl::sort-vector (vector index index function (or function null))
  * ; SORT-VECTOR works through side-effect
  (call)
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 1))

(defknown merge (type-specifier sequence sequence callable
                                &key (:key callable))
  sequence
  (call important-result)
  :derive-type (creation-result-type-specifier-nth-arg 1)
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 2 3))

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
(defknown (car cdr first rest)
  (list)
  t
  (foldable flushable))

;; Correct argument type restrictions for these functions are
;; complicated, so we just declare them to accept LISTs and suppress
;; flushing is safe code.
(defknown (caar cadr cdar cddr
                caaar caadr cadar caddr cdaar cdadr cddar cdddr
                caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
                cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
                second third fourth fifth sixth seventh eighth ninth tenth)
  (list)
  t
  (foldable unsafely-flushable))

(defknown cons (t t) cons (movable flushable unsafe))

(defknown tree-equal (t t &key (:test callable) (:test-not callable)) boolean
  (foldable flushable call))
(defknown endp (list) boolean (foldable flushable movable))
(defknown list-length (list) (or index null) (foldable unsafely-flushable))
(defknown nth (unsigned-byte list) t (foldable flushable))
(defknown nthcdr (unsigned-byte list) t (foldable unsafely-flushable))

(defknown last (list &optional unsigned-byte) t (foldable flushable))
(defknown %last0 (list) t (foldable flushable))
(defknown %last1 (list) t (foldable flushable))
(defknown %lastn/fixnum (list (and unsigned-byte fixnum)) t (foldable flushable))
(defknown %lastn/bignum (list (and unsigned-byte bignum)) t (foldable flushable))

(defknown list (&rest t) list (movable flushable unsafe))
(defknown list* (t &rest t) t (movable flushable unsafe))
(defknown make-list (index &key (:initial-element t)) list
  (movable flushable unsafe))

;;; All but last must be of type LIST, but there seems to be no way to
;;; express that in this syntax.
(defknown append (&rest t) t (flushable))

(defknown copy-list (list) list (flushable))
(defknown copy-alist (list) list (flushable))
(defknown copy-tree (t) t (flushable recursive))
(defknown revappend (list t) t (flushable))

;;; All but last must be of type LIST, but there seems to be no way to
;;; express that in this syntax. The result must be LIST, but we do
;;; not check it now :-).
(defknown nconc (&rest t) t ()
  :destroyed-constant-args (remove-non-constants-and-nils #'butlast))
(defknown sb!impl::nconc2 (list t) t ()
  :destroyed-constant-args (remove-non-constants-and-nils #'butlast))

(defknown nreconc (list t) t (important-result)
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 1))
(defknown butlast (list &optional unsigned-byte) list (flushable))
(defknown nbutlast (list &optional unsigned-byte) list ()
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 1))

(defknown ldiff (list t) list (flushable))
(defknown (rplaca rplacd) (cons t) list (unsafe)
  :destroyed-constant-args (nth-constant-args 1))

(defknown subst (t t t &key (:key callable) (:test callable)
                   (:test-not callable))
  t (flushable unsafe call))
(defknown nsubst (t t t &key (:key callable) (:test callable)
                    (:test-not callable))
  t (unsafe call)
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 3))

(defknown (subst-if subst-if-not)
          (t callable t &key (:key callable))
  t (flushable unsafe call))
(defknown (nsubst-if nsubst-if-not)
          (t callable t &key (:key callable))
  t (unsafe call)
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 3))

(defknown sublis (list t &key (:key callable) (:test callable)
                       (:test-not callable))
  t (flushable unsafe call))
(defknown nsublis (list t &key (:key callable) (:test callable)
                        (:test-not callable))
  t (flushable unsafe call)
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 2))

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
  (foldable flushable call important-result)
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 1 2))

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
(defknown delq (t list) list (flushable unsafe)
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 2))

;;;; from the "Hash Tables" chapter:

(defknown make-hash-table
  (&key (:test callable) (:size unsigned-byte)
        (:rehash-size (or (integer 1) (float (1.0))))
        (:rehash-threshold (real 0 1))
        (:hash-function (or null callable))
        (:weakness (member nil :key :value :key-and-value :key-or-value))
        (:synchronized t))
  hash-table
  (flushable unsafe))
(defknown hash-table-p (t) boolean (movable foldable flushable))
(defknown gethash (t hash-table &optional t) (values t boolean)
  (flushable unsafe)) ; not FOLDABLE, since hash table contents can change
(defknown sb!impl::gethash2 (t hash-table) (values t boolean)
  (flushable unsafe)) ; not FOLDABLE, since hash table contents can change
(defknown sb!impl::gethash3 (t hash-table t) (values t boolean)
  (flushable unsafe)) ; not FOLDABLE, since hash table contents can change
(defknown %puthash (t hash-table t) t (unsafe)
  :destroyed-constant-args (nth-constant-args 2))
(defknown remhash (t hash-table) boolean ()
  :destroyed-constant-args (nth-constant-args 2))
(defknown maphash (callable hash-table) null (flushable call))
(defknown clrhash (hash-table) hash-table ()
  :destroyed-constant-args (nth-constant-args 2))
(defknown hash-table-count (hash-table) index (flushable))
(defknown hash-table-rehash-size (hash-table) (or index (single-float (1.0)))
  (foldable flushable))
(defknown hash-table-rehash-threshold (hash-table) (single-float (0.0) 1.0)
  (foldable flushable))
(defknown hash-table-size (hash-table) index (flushable))
(defknown hash-table-test (hash-table) symbol (foldable flushable))
(defknown sxhash (t) hash (#-sb-xc-host foldable flushable))
(defknown psxhash (t &optional t) hash (#-sb-xc-host foldable flushable))

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

(defknown aref (array &rest index) t (foldable))
(defknown row-major-aref (array index) t (foldable))

(defknown array-element-type (array)
  type-specifier
  (foldable flushable recursive))
(defknown array-rank (array) array-rank (foldable flushable))
(defknown array-dimension (array array-rank) index (foldable flushable))
(defknown array-dimensions (array) list (foldable flushable))
(defknown array-in-bounds-p (array &rest integer) boolean (foldable flushable))
(defknown array-row-major-index (array &rest index) array-total-size
  (foldable flushable))
(defknown array-total-size (array) array-total-size (foldable flushable))
(defknown adjustable-array-p (array) boolean (movable foldable flushable))

(defknown svref (simple-vector index) t (foldable flushable))
(defknown bit ((array bit) &rest index) bit (foldable flushable))
(defknown sbit ((simple-array bit) &rest index) bit (foldable flushable))

;;; FIXME: :DESTROYED-CONSTANT-ARGS for these is complicated.
(defknown (bit-and bit-ior bit-xor bit-eqv bit-nand bit-nor bit-andc1 bit-andc2
                   bit-orc1 bit-orc2)
  ((array bit) (array bit) &optional (or (array bit) (member t nil)))
  (array bit)
  ()
  #|:derive-type #'result-type-last-arg|#)

(defknown bit-not ((array bit) &optional (or (array bit) (member t nil)))
  (array bit)
  ()
  #|:derive-type #'result-type-last-arg|#)

(defknown bit-vector-= (bit-vector bit-vector) boolean
  (movable foldable flushable))

(defknown array-has-fill-pointer-p (array) boolean
  (movable foldable flushable))
(defknown fill-pointer (complex-vector) index
    (unsafely-flushable explicit-check))
(defknown vector-push (t complex-vector) (or index null)
    (explicit-check)
  :destroyed-constant-args (nth-constant-args 2))
(defknown vector-push-extend (t complex-vector &optional (and index (integer 1))) index
    (explicit-check)
  :destroyed-constant-args (nth-constant-args 2))
(defknown vector-pop (complex-vector) t
    (explicit-check)
  :destroyed-constant-args (nth-constant-args 1))

;;; FIXME: complicated :DESTROYED-CONSTANT-ARGS
;;; Also, an important-result warning could be provided if the array
;;; is known to be not expressly adjustable.
(defknown adjust-array
  (array (or index list) &key (:element-type type-specifier)
         (:initial-element t) (:initial-contents t)
         (:fill-pointer t) (:displaced-to (or array null))
         (:displaced-index-offset index))
  array (unsafe))
;  :derive-type 'result-type-arg1) Not even close...

;;;; from the "Strings" chapter:

(defknown char (string index) character (foldable flushable))
(defknown schar (simple-string index) character (foldable flushable))

(defknown (string= string-equal)
  (string-designator string-designator &key (:start1 index) (:end1 sequence-end)
              (:start2 index) (:end2 sequence-end))
  boolean
  (foldable flushable))

(defknown (string< string> string<= string>= string/= string-lessp
                   string-greaterp string-not-lessp string-not-greaterp
                   string-not-equal)
  (string-designator string-designator &key (:start1 index) (:end1 sequence-end)
              (:start2 index) (:end2 sequence-end))
  (or index null)
  (foldable flushable))

(defknown make-string (index &key (:element-type type-specifier)
                       (:initial-element character))
  simple-string (flushable))

(defknown (string-trim string-left-trim string-right-trim)
  (sequence string-designator) string (flushable))

(defknown (string-upcase string-downcase string-capitalize)
  (string-designator &key (:start index) (:end sequence-end))
  simple-string (flushable))

(defknown (nstring-upcase nstring-downcase nstring-capitalize)
  (string &key (:start index) (:end sequence-end))
  string ()
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 1))

(defknown string (string-designator) string
  (flushable explicit-check))

;;;; internal non-keyword versions of string predicates:

(defknown (string<* string>* string<=* string>=* string/=*)
  (string-designator string-designator index sequence-end index sequence-end)
  (or index null)
  (foldable flushable))

(defknown string=*
  (string-designator string-designator index sequence-end index sequence-end)
  boolean
  (foldable flushable))

;;;; from the "Eval" chapter:

(defknown eval (t) * (recursive))
(defknown constantp (t &optional lexenv-designator) boolean
  (foldable flushable))

;;;; from the "Streams" chapter:

(defknown make-synonym-stream (symbol) stream (flushable))
(defknown make-broadcast-stream (&rest stream) stream (unsafely-flushable))
(defknown make-concatenated-stream (&rest stream) stream (unsafely-flushable))
(defknown make-two-way-stream (stream stream) stream (unsafely-flushable))
(defknown make-echo-stream (stream stream) stream (flushable))
(defknown make-string-input-stream (string &optional index sequence-end)
  stream
  (flushable unsafe))
(defknown make-string-output-stream
    (&key (:element-type type-specifier))
    stream
  (flushable))
(defknown get-output-stream-string (stream) simple-string ())
(defknown streamp (t) boolean (movable foldable flushable))
(defknown stream-element-type (stream) type-specifier
  (movable foldable flushable))
(defknown (output-stream-p input-stream-p) (stream) boolean
  (movable foldable flushable))
(defknown close (stream &key (:abort t)) (eql t) ())

;;;; from the "Input/Output" chapter:

;;; (The I/O functions are given effects ANY under the theory that
;;; code motion over I/O operations is particularly confusing and not
;;; very important for efficiency.)

(defknown copy-readtable (&optional (or readtable null) (or readtable null))
  readtable
  ())
(defknown readtablep (t) boolean (movable foldable flushable))

(defknown set-syntax-from-char
  (character character &optional readtable (or readtable null)) (eql t)
  ())

(defknown set-macro-character (character callable &optional t (or readtable null))
  (eql t)
  (unsafe))
(defknown get-macro-character (character &optional (or readtable null))
  (values callable boolean) (flushable))

(defknown make-dispatch-macro-character (character &optional t readtable)
  (eql t) ())
(defknown set-dispatch-macro-character
  (character character callable &optional (or readtable null)) (eql t)
  (unsafe))
(defknown get-dispatch-macro-character
  (character character &optional (or readtable null)) (or callable null)
  ())

(defknown copy-pprint-dispatch
  (&optional (or sb!pretty:pprint-dispatch-table null))
  sb!pretty:pprint-dispatch-table
  ())
(defknown pprint-dispatch
  (t &optional (or sb!pretty:pprint-dispatch-table null))
  (values callable boolean)
  ())
(defknown (pprint-fill pprint-linear)
  (stream-designator t &optional t t)
  null
  ())
(defknown pprint-tabular
  (stream-designator t &optional t t unsigned-byte)
  null
  ())
(defknown pprint-indent
  ((member :block :current) real &optional stream-designator)
  null
  ())
(defknown pprint-newline
  ((member :linear :fill :miser :mandatory) &optional stream-designator)
  null
  ())
(defknown pprint-tab
  ((member :line :section :line-relative :section-relative)
   unsigned-byte unsigned-byte &optional stream-designator)
  null
  ())
(defknown set-pprint-dispatch
  (type-specifier (or null callable)
   &optional real sb!pretty:pprint-dispatch-table)
  null
  ())

;;; may return any type due to eof-value...
(defknown (read read-preserving-whitespace read-char-no-hang read-char)
  (&optional stream-designator t t t) t (explicit-check))

(defknown read-delimited-list (character &optional stream-designator t) list
  (explicit-check))
(defknown read-line (&optional stream-designator t t t) (values t boolean)
  (explicit-check))
(defknown unread-char (character &optional stream-designator) t
  (explicit-check))
(defknown peek-char (&optional (or character (member nil t))
                               stream-designator t t t)
  t
  (explicit-check))
(defknown listen (&optional stream-designator) boolean (flushable explicit-check))

(defknown clear-input (&optional stream-designator) null (explicit-check))

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
     (:stream stream-designator)
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

(defknown (prin1 print princ) (t &optional stream-designator)
  t
  (any explicit-check)
  :derive-type #'result-type-first-arg)

;;; xxx-TO-STRING functions are not foldable because they depend on
;;; the dynamic environment, the state of the pretty printer dispatch
;;; table, and probably other run-time factors.
(defknown write-to-string
  (t &key (:escape t) (:radix t) (:base (integer 2 36)) (:readably t)
     (:circle t) (:pretty t) (:level (or unsigned-byte null))
     (:length (or unsigned-byte null)) (:case t) (:array t) (:gensym t)
     (:lines (or unsigned-byte null)) (:right-margin (or unsigned-byte null))
     (:miser-width (or unsigned-byte null)) (:pprint-dispatch t))
  simple-string
  (flushable explicit-check))

(defknown (prin1-to-string princ-to-string) (t) simple-string (flushable))

(defknown write-char (character &optional stream-designator) character
  (explicit-check))
(defknown (write-string write-line)
  (string &optional stream-designator &key (:start index) (:end sequence-end))
  string
  (explicit-check))

(defknown (terpri finish-output force-output clear-output)
  (&optional stream-designator) null
  (explicit-check))

(defknown fresh-line (&optional stream-designator) boolean
  (explicit-check))

(defknown write-byte (integer stream) integer
  (explicit-check))

;;; FIXME: complicated :DESTROYED-CONSTANT-ARGS
(defknown format ((or (member nil t) stream string)
                  (or string function) &rest t)
  (or string null)
  (explicit-check))

(defknown (y-or-n-p yes-or-no-p) (&optional string &rest t) boolean
  (explicit-check))

;;;; from the "File System Interface" chapter:

;;; (No pathname functions are FOLDABLE because they all potentially
;;; depend on *DEFAULT-PATHNAME-DEFAULTS*, e.g. to provide a default
;;; host when parsing a namestring. They are not FLUSHABLE because
;;; parsing of a PATHNAME-DESIGNATOR might signal an error.)

(defknown wild-pathname-p (pathname-designator
                           &optional
                           (member nil :host :device
                                   :directory :name
                                   :type :version))
  generalized-boolean
  ())
(defknown pathname-match-p (pathname-designator pathname-designator)
  generalized-boolean
  ())
(defknown translate-pathname (pathname-designator
                              pathname-designator
                              pathname-designator &key)
  pathname
  ())

(defknown logical-pathname (pathname-designator) logical-pathname ())
(defknown translate-logical-pathname (pathname-designator &key) pathname
  (recursive))
(defknown load-logical-pathname-translations (string) t ())
(defknown logical-pathname-translations (logical-host-designator) list ())

(defknown pathname (pathname-designator) pathname ())
(defknown truename (pathname-designator) pathname ())

(defknown parse-namestring
  (pathname-designator &optional
                       (or list host string (member :unspecific))
                       pathname-designator
                       &key
                       (:start index)
                       (:end sequence-end)
                       (:junk-allowed t))
  (values (or pathname null) sequence-end)
  ())

(defknown merge-pathnames
  (pathname-designator &optional pathname-designator pathname-version)
  pathname
  ())

(defknown make-pathname
 (&key (:defaults pathname-designator)
       (:host (or string pathname-host))
       (:device (or string pathname-device))
       (:directory (or pathname-directory string (member :wild)))
       (:name (or pathname-name string (member :wild)))
       (:type (or pathname-type string (member :wild)))
       (:version pathname-version) (:case (member :local :common)))
  pathname (unsafely-flushable))

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
  (pathname-designator) (or simple-string null)
  (unsafely-flushable))

(defknown enough-namestring (pathname-designator &optional pathname-designator)
  simple-string
  (unsafely-flushable))

(defknown user-homedir-pathname (&optional t) pathname (flushable))

(defknown open
  (pathname-designator &key
                       (:direction (member :input :output :io :probe))
                       (:element-type type-specifier)
                       (:if-exists (member :error :new-version :rename
                                           :rename-and-delete :overwrite
                                           :append :supersede nil))
                       (:if-does-not-exist (member :error :create nil))
                       (:external-format external-format-designator))
  (or stream null))

(defknown rename-file (pathname-designator filename)
  (values pathname pathname pathname))
(defknown delete-file (pathname-designator) t)
(defknown probe-file (pathname-designator) (or pathname null) ())
(defknown file-write-date (pathname-designator) (or unsigned-byte null)
  ())
(defknown file-author (pathname-designator) (or simple-string null)
  ())

(defknown file-position (stream &optional
                                (or unsigned-byte (member :start :end)))
  (or unsigned-byte (member t nil)))
(defknown file-length (stream) (or unsigned-byte null) (unsafely-flushable))

(defknown load
  ((or filename stream)
   &key
   (:verbose t)
   (:print t)
   (:if-does-not-exist t)
   (:external-format external-format-designator))
  t)

(defknown directory (pathname-designator &key (:resolve-symlinks t))
  list ())

;;;; from the "Conditions" chapter:

(defknown cell-error-name (cell-error) t)
(defknown error (t &rest t) nil)
(defknown cerror (format-control t &rest t) null)
(defknown invalid-method-error (t format-control &rest t) *) ; FIXME: first arg is METHOD
(defknown method-combination-error (format-control &rest t) *)
(defknown signal (t &rest t) null)
(defknown simple-condition-format-control (condition)
  format-control)
(defknown simple-condition-format-arguments (condition)
  list)
(defknown warn (t &rest t) null)
(defknown invoke-debugger (condition) nil)
(defknown break (&optional format-control &rest t) null)
(defknown make-condition (type-specifier &rest t) condition)
(defknown compute-restarts (&optional (or condition null)) list)
(defknown find-restart (restart-designator &optional (or condition null))
  (or restart null))
(defknown invoke-restart (restart-designator &rest t) *)
(defknown invoke-restart-interactively (restart-designator) *)
(defknown restart-name (restart) symbol)
(defknown (abort muffle-warning) (&optional (or condition null)) nil)
(defknown continue (&optional (or condition null)) null)
(defknown (store-value use-value) (t &optional (or condition null))
  null)

;;; and analogous SBCL extension:
(defknown sb!impl::%failed-aver (t) nil)
(defknown bug (t &rest t) nil) ; never returns


;;;; from the "Miscellaneous" Chapter:

(defknown compile ((or symbol cons) &optional (or list function null))
  (values (or function symbol cons) boolean boolean))

(defknown compile-file
  (pathname-designator
   &key

   ;; ANSI options
   (:output-file (or pathname-designator
                     null
                     ;; FIXME: This last case is a non-ANSI hack.
                     (member t)))
   (:verbose t)
   (:print t)
   (:external-format external-format-designator)

   ;; extensions
   (:trace-file t)
   (:block-compile t)
   (:emit-cfasl t))
  (values (or pathname null) boolean boolean))

;; FIXME: consider making (OR CALLABLE CONS) something like
;; EXTENDED-FUNCTION-DESIGNATOR
(defknown disassemble ((or callable cons) &key
                       (:stream stream) (:use-labels t))
  null)

(defknown describe (t &optional (or stream (member t nil))) (values))
(defknown inspect (t) (values))
(defknown room (&optional (member t nil :default)) (values))
(defknown ed (&optional (or symbol cons filename))
  t)
(defknown dribble (&optional filename &key (:if-exists t)) (values))

(defknown apropos      (string-designator &optional package-designator t) (values))
(defknown apropos-list (string-designator &optional package-designator t) list
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

(defknown constantly (t) function (movable flushable))
(defknown complement (function) function (movable flushable))

;;;; miscellaneous extensions

(defknown symbol-global-value (symbol) t ())
(defknown set-symbol-global-value (symbol t) t ())

(defknown get-bytes-consed () unsigned-byte (flushable))
(defknown mask-signed-field ((integer 0 *) integer) integer
          (movable flushable foldable))

(defknown array-storage-vector (array) (simple-array * (*))
    (any))

;;;; magical compiler frobs

(defknown %unary-truncate/single-float (single-float) integer (movable foldable flushable))
(defknown %unary-truncate/double-float (double-float) integer (movable foldable flushable))

;;; We can't fold this in general because of SATISFIES. There is a
;;; special optimizer anyway.
(defknown %typep (t (or type-specifier ctype)) boolean
  (movable flushable explicit-check))
(defknown %instance-typep (t (or type-specifier ctype)) boolean
  (movable flushable explicit-check always-translatable))

(defknown %cleanup-point () t)
(defknown %special-bind (t t) t)
(defknown %special-unbind (t) t)
(defknown %listify-rest-args (t index) list (flushable))
(defknown %more-arg-context (t t) (values t index) (flushable))
(defknown %more-arg (t index) t)
#!+stack-grows-downward-not-upward
;;; FIXME: The second argument here should really be NEGATIVE-INDEX, but doing that
;;; breaks the build, and I cannot seem to figure out why. --NS 2006-06-29
(defknown %more-kw-arg (t fixnum) (values t t))
(defknown %more-arg-values (t index index) * (flushable))
(defknown %verify-arg-count (index index) (values))
(defknown %arg-count-error (t) nil)
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
(defknown %nip-values (t t &rest t) (values))
(defknown %allocate-closures (t) *)
(defknown %type-check-error (t t) nil)

;; FIXME: This function does not return, but due to the implementation
;; of FILTER-LVAR we cannot write it here.
(defknown %compile-time-type-error (t t t) *)
(defknown sb!kernel::case-failure (t t t) nil)

(defknown %odd-key-args-error () nil)
(defknown %unknown-key-arg-error (t) nil)
(defknown (%ldb %mask-field) (bit-index bit-index integer) unsigned-byte
  (movable foldable flushable explicit-check))
(defknown (%dpb %deposit-field) (integer bit-index bit-index integer) integer
  (movable foldable flushable explicit-check))
(defknown %negate (number) number (movable foldable flushable explicit-check))
(defknown %check-bound (array index fixnum) index (movable foldable flushable))
(defknown data-vector-ref (simple-array index) t
  (foldable explicit-check always-translatable))
(defknown data-vector-ref-with-offset (simple-array index fixnum) t
  (foldable explicit-check always-translatable))
(defknown data-vector-set (array index t) t
  (unsafe explicit-check always-translatable))
(defknown data-vector-set-with-offset (array index fixnum t) t
  (unsafe explicit-check always-translatable))
(defknown hairy-data-vector-ref (array index) t
  (foldable explicit-check))
(defknown hairy-data-vector-set (array index t) t (unsafe explicit-check))
(defknown hairy-data-vector-ref/check-bounds (array index) t
  (foldable explicit-check))
(defknown hairy-data-vector-set/check-bounds (array index t)
  t
  (unsafe explicit-check))
(defknown %caller-frame () t (flushable))
(defknown %caller-pc () system-area-pointer (flushable))
(defknown %with-array-data (array index (or index null))
  (values (simple-array * (*)) index index index)
  (foldable flushable))
(defknown %with-array-data/fp (array index (or index null))
  (values (simple-array * (*)) index index index)
  (foldable flushable))
(defknown %set-symbol-package (symbol t) t (unsafe))
(defknown %coerce-name-to-fun ((or symbol cons)) function (flushable))
(defknown %coerce-callable-to-fun (callable) function (flushable))
(defknown array-bounding-indices-bad-error (t t t) nil)
(defknown sequence-bounding-indices-bad-error (t t t) nil)
(defknown %find-position
  (t sequence t index sequence-end function function)
  (values t (or index null))
  (flushable call))
(defknown (%find-position-if %find-position-if-not)
  (function sequence t index sequence-end function)
  (values t (or index null))
  (call))
(defknown effective-find-position-test (callable callable)
  function
  (flushable foldable))
(defknown effective-find-position-key (callable)
  function
  (flushable foldable))

(defknown %adjoin     (t list)          list (explicit-check foldable flushable))
(defknown %adjoin-key (t list function) list (explicit-check foldable flushable call))
(defknown %assoc      (t list)          list (explicit-check foldable flushable))
(defknown %assoc-key  (t list function) list (explicit-check foldable flushable call))
(defknown %member     (t list)          list (explicit-check foldable flushable))
(defknown %member-key (t list function) list (explicit-check foldable flushable call))
(defknown %rassoc     (t list)          list (explicit-check foldable flushable))
(defknown %rassoc-key (t list function) list (explicit-check foldable flushable call))

(defknown %check-vector-sequence-bounds (vector index sequence-end)
  index
  (unwind))
;;; FIXME: including this information here is probably necessary to
;;; get efficient compilation of the inline expansion of
;;; %FIND-POSITION-IF, so it should maybe be in a more
;;; compiler-friendly package (SB-INT?)
(defknown sb!impl::signal-bounding-indices-bad-error
    (sequence index sequence-end)
  nil) ; never returns


(defknown arg-count-error (t t t t t t) nil (unsafe))

;;;; SETF inverses

(defknown %aset (array &rest t) t (unsafe)
  :destroyed-constant-args (nth-constant-args 1))
(defknown %set-row-major-aref (array index t) t (unsafe)
  :destroyed-constant-args (nth-constant-args 1))
(defknown (%rplaca %rplacd) (cons t) t (unsafe)
  :destroyed-constant-args (nth-constant-args 1))
(defknown %put (symbol t t) t (unsafe))
(defknown %setelt (sequence index t) t (unsafe)
  :destroyed-constant-args (nth-constant-args 1))
(defknown %svset (simple-vector index t) t (unsafe)
  :destroyed-constant-args (nth-constant-args 1))
(defknown %bitset ((array bit) &rest index) bit (unsafe)
  :destroyed-constant-args (nth-constant-args 1))
(defknown %sbitset ((simple-array bit) &rest index) bit (unsafe)
  :destroyed-constant-args (nth-constant-args 1))
(defknown %charset (string index character) character (unsafe)
  :destroyed-constant-args (nth-constant-args 1))
(defknown %scharset (simple-string index character) character (unsafe)
  :destroyed-constant-args (nth-constant-args 1))
(defknown %set-symbol-value (symbol t) t (unsafe))
(defknown (setf symbol-function) (function symbol) function (unsafe))
(defknown %set-symbol-plist (symbol list) list (unsafe))
(defknown %setnth (unsigned-byte list t) t (unsafe)
  :destroyed-constant-args (nth-constant-args 2))
(defknown %set-fill-pointer (complex-vector index) index
    (unsafe explicit-check)
  :destroyed-constant-args (nth-constant-args 1))

;;;; ALIEN and call-out-to-C stuff

;; Used by WITH-PINNED-OBJECTS
#!+(or x86 x86-64)
(defknown sb!vm::touch-object (t) (values)
    (unsafe always-translatable))

#!+linkage-table
(defknown foreign-symbol-dataref-sap (simple-string)
  system-area-pointer
  (movable flushable))

(defknown foreign-symbol-sap (simple-string &optional boolean)
  system-area-pointer
  (movable flushable))

(defknown foreign-symbol-address (simple-string &optional boolean)
  (values integer boolean)
  (movable flushable))

;;;; miscellaneous internal utilities

(defknown %fun-name (function) t (flushable))
(defknown (setf %fun-name) (t function) t (unsafe))

(defknown policy-quality (policy symbol) policy-quality
          (flushable))

(defknown compiler-error (t &rest t) nil ())
(defknown (compiler-warn compiler-style-warn) (t &rest t) (values) ())
(defknown (compiler-notify maybe-compiler-notify) ((or string symbol) &rest t)
  (values)
  ())
(defknown style-warn (t &rest t) null ())


;;;; memory barriers

(defknown sb!vm:%compiler-barrier () (values) ())
(defknown sb!vm:%memory-barrier () (values) ())
(defknown sb!vm:%read-barrier () (values) ())
(defknown sb!vm:%write-barrier () (values) ())
(defknown sb!vm:%data-dependency-barrier () (values) ())


;;;; atomic ops
(defknown %compare-and-swap-svref (simple-vector index t t) t
    (unsafe))
(defknown %compare-and-swap-instance-ref (instance index t t) t
    (unsafe))
(defknown %compare-and-swap-symbol-value (symbol t t) t
    (unsafe unwind))
