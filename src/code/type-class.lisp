;;;; This file contains the definition of the CTYPE (Compiler TYPE)
;;;; structure, as well as the TYPE-CLASS structure which is a metaobject
;;;; that factors out commonality amongst the subtypes of CTYPE.
;;;; Together they form a sort of mini object system with slightly
;;;; odd dispatching rules. The TYPE-CLASS is a vtable, essentially.
;;;; Various macros related to manipulating those things are here too.

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

;; We can't make an instance of any CTYPE descendant until its type-class
;; exists in *TYPE-CLASSES* and the quasi-random state has been made.
;; By initializing the state and type-class storage vector at once,
;; it is obvious that either both have been made or neither one has been.
#-sb-xc
(progn (defvar *ctype-lcg-state* 1)
       (defvar *ctype-hash-state* (make-random-state))
       (defvar *type-classes* (make-array 20 :fill-pointer 0)))
#+sb-xc
(macrolet ((def ()
             (let* ((state-type `(unsigned-byte ,sb!vm:n-positive-fixnum-bits))
                    (initform `(make-array 1 :element-type ',state-type))
                    (n (length *type-classes*)))
             `(progn
                (declaim (type (simple-array ,state-type (1))
                               *ctype-hash-state*)
                         (type (simple-vector ,n) *type-classes*))
                ;; The value forms are for type-correctness only.
                ;; COLD-INIT-FORMS will already have been run.
                (defglobal *ctype-hash-state* ,initform)
                (defglobal *type-classes* (make-array ,n))
                (!cold-init-forms (setq *ctype-hash-state* ,initform))))))
  (def))

(defun type-class-or-lose (name)
  (or (find name *type-classes* :key #'type-class-name)
      (error "~S is not a defined type class." name)))

#-sb-xc-host
(define-compiler-macro type-class-or-lose (&whole form name)
  ;; If NAME is a quoted constant, the resultant form should be
  ;; a fixed index into *TYPE-CLASSES* except that during the building
  ;; of the cross-compiler the array hasn't been populated yet.
  ;; One solution to that, which I favored, is that DEFINE-TYPE-CLASS
  ;; appear before the structure definition that uses the corresponding
  ;; type-class in its slot initializer. That posed a problem for
  ;; the :INHERITS option, because the constructor of a descendant
  ;; grabs all the methods [sic] from its ancestor at the time the
  ;; descendant is defined, which means the methods of the ancestor
  ;; should have been filled in, which means at least one DEFINE-TYPE-CLASS
  ;; wants to appear _after_ a structure definition that uses it.
  (if (constantp name)
      (let ((name (constant-form-value name)))
        `(aref *type-classes*
               ,(or (position name *type-classes* :key #'type-class-name)
                    (error "~S is not a defined type class." name))))
      form))

(defun must-supply-this (&rest foo)
  (/show0 "failing in MUST-SUPPLY-THIS")
  (error "missing type method for ~S" foo))

;;; A TYPE-CLASS object represents the "kind" of a type. It mainly
;;; contains functions which are methods on that kind of type, but is
;;; also used in EQ comparisons to determined if two types have the
;;; "same kind".
(def!struct (type-class
             #-no-ansi-print-object
             (:print-object (lambda (x stream)
                              (print-unreadable-object (x stream :type t)
                                (prin1 (type-class-name x) stream)))))
  ;; the name of this type class (used to resolve references at load time)
  (name (missing-arg) :type symbol :read-only t)
  ;; Dyadic type methods. If the classes of the two types are EQ, then
  ;; we call the SIMPLE-xxx method. If the classes are not EQ, and
  ;; either type's class has a COMPLEX-xxx method, then we call it.
  ;;
  ;; Although it is undefined which method will get precedence when
  ;; both types have a complex method, the complex method can assume
  ;; that the second arg always is in its class, and the first always
  ;; is not. The arguments to commutative operations will be swapped
  ;; if the first argument has a complex method.
  ;;
  ;; Since SUBTYPEP is not commutative, we have two complex methods.
  ;; The ARG1 method is only called when the first argument is in its
  ;; class, and the ARG2 method is only called when called when the
  ;; second type is. If either is specified, both must be.
  ;; FIXME: "both must be" is false of CLASSOID type-class.
  ;;        Figure out if this is a comment bug or a logic bug.
  ;; * (type-class-complex-subtypep-arg1 (type-class-or-lose 'classoid)) => NIL
  ;; * (type-class-complex-subtypep-arg2 (type-class-or-lose 'classoid))
  ;;   => #<FUNCTION CLASSOID-COMPLEX-SUBTYPEP-ARG2-TYPE-METHOD>
  (simple-subtypep #'must-supply-this :type function)
  (complex-subtypep-arg1 nil :type (or function null))
  (complex-subtypep-arg2 nil :type (or function null))
  ;; SIMPLE-UNION2, COMPLEX-UNION2, SIMPLE-INTERSECTION2, and
  ;; COMPLEX-INTERSECTION2 methods take pairs of types and try to find
  ;; a new type which expresses the result nicely, better than could
  ;; be done by just stuffing the two component types into an
  ;; UNION-TYPE or INTERSECTION-TYPE object. They return NIL on
  ;; failure, or a CTYPE for success.
  ;;
  ;; Note: These methods are similar to CMU CL's SIMPLE-UNION,
  ;; COMPLEX-UNION, SIMPLE-INTERSECTION, and COMPLEX-UNION methods.
  ;; They were reworked in SBCL because SBCL has INTERSECTION-TYPE
  ;; objects (where CMU CL just punted to HAIRY-TYPE) and because SBCL
  ;; wants to simplify unions and intersections by considering all
  ;; possible pairwise simplifications (where the CMU CL code only
  ;; considered simplifications between types which happened to appear
  ;; next to each other the argument sequence).
  ;;
  ;; Differences in detail from old CMU CL methods:
  ;;   * SBCL's methods are more parallel between union and
  ;;     intersection forms. Each returns one values, (OR NULL CTYPE).
  ;;   * SBCL doesn't use type methods to deal with unions or
  ;;     intersections of the COMPOUND-TYPE of the corresponding form.
  ;;     Instead the wrapper functions TYPE-UNION2, TYPE-INTERSECTION2,
  ;;     TYPE-UNION, and TYPE-INTERSECTION handle those cases specially
  ;;     (and deal with canonicalization/simplification issues at the
  ;;     same time).
  (simple-union2 #'hierarchical-union2 :type function)
  (complex-union2 nil :type (or function null))
  (simple-intersection2 #'hierarchical-intersection2 :type function)
  (complex-intersection2 nil :type (or function null))
  (simple-= #'must-supply-this :type function)
  (complex-= nil :type (or function null))
  ;; monadic functions
  (negate #'must-supply-this :type function)
  ;; a function which returns a Common Lisp type specifier
  ;; representing this type
  (unparse #'must-supply-this :type function)

  ;; Can types of this type-class contain other types?
  ;; A global property of our
  ;; implementation (which unfortunately seems impossible to enforce
  ;; with assertions or other in-the-code checks and constraints) is
  ;; that subclasses which don't contain other types correspond to
  ;; disjoint subsets (except of course for the NAMED-TYPE T, which
  ;; covers everything). So NUMBER-TYPE is disjoint from CONS-TYPE is
  ;; is disjoint from MEMBER-TYPE and so forth. But types which can
  ;; contain other types, like HAIRY-TYPE and INTERSECTION-TYPE, can
  ;; violate this rule.
  (might-contain-other-types-p nil :type boolean :read-only t)
  ;; a function which returns T if the CTYPE could possibly be
  ;; equivalent to a MEMBER type. If not a function, then it's
  ;; a constant T or NIL for all instances of this type class.
  ;; Note that the old comment for this slot was
  ;;   "True if this type has a fixed number of members, and as such
  ;;    could possibly be completely specified in a MEMBER type."
  ;; The second half of that is right because of the "possibly,"
  ;; but "has a fixed number" is too strong a claim, because we
  ;; set enumerable=T for NEGATION and HAIRY and some other things.
  ;; Conceptually the choices are really {yes, no, unknown}, but
  ;; whereas "no" means "definitely not", T means "yes or maybe".
  (enumerable-p nil :type (or function boolean) :read-only t)
  ;; a function which returns T if the CTYPE is inhabited by a single
  ;; object and, as a value, the object.  Otherwise, returns NIL, NIL.
  ;; The default case (NIL) is interpreted as a function that always
  ;; returns NIL, NIL.
  (singleton-p nil :type (or function null))

  #|
  Not used, and not really right. Probably we want a TYPE= alist for the
  unary operations, since there are lots of interesting unary predicates that
  aren't equivalent to an entire class
  ;; Names of functions used for testing the type of objects in this type
  ;; class. UNARY-PREDICATE takes just the object, whereas PREDICATE gets
  ;; passed both the object and the CTYPE. Normally one or the other will be
  ;; supplied for any type that can be passed to TYPEP; there is no point in
  ;; supplying both.
  (unary-typep nil :type (or symbol null))
  (typep nil :type (or symbol null))
  ;; These are like TYPEP and UNARY-TYPEP except they coerce objects to
  ;; the type.
  (unary-coerce nil :type (or symbol null))
  (coerce :type (or symbol null))
  |#
  )
#!-sb-fluid (declaim (freeze-type type-class))

#+sb-xc-host
(defun ctype-random (mask)
  (logand (setq *ctype-lcg-state*
                (logand #x8fffff (+ (* 1103515245 *ctype-lcg-state*) 12345)))
          mask))

;;; the base class for the internal representation of types

;; Each CTYPE instance (incl. subtypes thereof) has a random opaque hash value.
;; Hashes are mixed together to form a lookup key in the memoization wrappers
;; for most operations in CTYPES. This works because CTYPEs are immutable.
;; But some bits are "stolen" from the hash as flag bits.
;; The sign bit indicates that the object is the *only* object representing
;; its type-specifier - it is an "interned" object.
;; The next highest bit indicates that the object, if compared for TYPE=
;; against an interned object can quickly return false when not EQ.
;; Complicated types don't admit the quick failure check.
;; At any rate, the totally opaque pseudo-random bits are under this mask.
(defconstant +ctype-hash-mask+
  (ldb (byte (1- sb!vm:n-positive-fixnum-bits) 0) -1))

;;; When comparing two ctypes, if this bit is 1 in each and they are not EQ,
;;; and at least one is interned, then they are not TYPE=.
(defconstant +type-admits-type=-optimization+
  (ash 1 (- sb!vm:n-positive-fixnum-bits 1)))

;;; Represent an index into *SPECIALIZED-ARRAY-ELEMENT-TYPE-PROPERTIES*
;;; if applicable. For types which are not array specializations,
;;; the bits are arbitrary.
(defmacro !ctype-saetp-index (x)
  `(ldb (byte 5 ,(- sb!vm:n-positive-fixnum-bits 6)) (type-hash-value ,x)))

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

;;; The "interned" bit indicates uniqueness of the internal representation of
;;; any specifier that parses to this object.
;;; Not all interned types admit TYPE= optimization. As one example:
;;; (type= (specifier-type '(array (unsigned-byte 6) (*)))
;;;        (specifier-type '(array (unsigned-byte 7) (*)))) => T and T
;;; because we preserve the difference in spelling of the two types.
(defun mark-ctype-interned (obj)
  (setf (type-hash-value obj)
        (logior sb!xc:most-negative-fixnum
                (if (eq (type-class-name (type-class-info obj)) 'array)
                    0
                    +type-admits-type=-optimization+)
                (type-hash-value obj)))
  obj)

;; For cold-init: improve the randomness of the hash.
;; (The host uses at most 21 bits of randomness. See CTYPE-RANDOM)
#+sb-xc
(defun !fix-ctype-hash (obj)
  (let ((saetp-index (!ctype-saetp-index obj)))
    ;; Preserve the interned-p and type=-optimization bits
    ;; by flipping only the bits under the hash-mask.
    (setf (type-hash-value obj)
          (logxor (logand (sb!impl::quasi-random-address-based-hash
                           *ctype-hash-state* +ctype-hash-mask+))
                   (type-hash-value obj)))
    ;; Except that some of those "non-intelligent" bits contain
    ;; critical information, if this type is an array specialization.
    (setf (!ctype-saetp-index obj) saetp-index))
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

#+sb-xc
(eval-when (:compile-toplevel)
  (assert (= (length (dd-slots (find-defstruct-description 'type-class)))
             ;; there exist two boolean slots, plus NAME
             (+ (length !type-class-fun-slots) 3))))

;; Unfortunately redundant with the slots in the DEF!STRUCT,
;; but allows asserting about correctness of the constructor
;; without relying on introspection in host Lisp.
(defconstant-eqx !type-class-fun-slots
    '(simple-subtypep
      complex-subtypep-arg1
      complex-subtypep-arg2
      simple-union2
      complex-union2
      simple-intersection2
      complex-intersection2
      simple-=
      complex-=
      negate
      unparse
      singleton-p)
  #'equal)

(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
  (defun !type-class-fun-slot (name)
    (unless (member name !type-class-fun-slots
                    :key (if (keywordp name) 'keywordicate 'identity))
      (warn "Undefined type-class method ~S" name))
    (symbolicate "TYPE-CLASS-" name)))

(defmacro !define-type-method ((class method &rest more-methods)
                               lambda-list &body body)
  (let ((name (symbolicate class "-" method "-TYPE-METHOD")))
    `(progn
       (defun ,name ,lambda-list
         ,@body)
       (!cold-init-forms
        ,@(mapcar (lambda (method)
                    `(setf (,(!type-class-fun-slot method)
                            (type-class-or-lose ',class))
                           #',name))
                  (cons method more-methods)))
       ',name)))

(defmacro !define-type-class (name &key inherits
                                     (enumerable (unless inherits (must-supply-this))
                                                 enumerable-supplied-p)
                                     (might-contain-other-types
                                      (unless inherits (must-supply-this))
                                      might-contain-other-types-supplied-p))
  (let ((make-it
         `(let ,(if inherits `((parent (type-class-or-lose ',inherits))))
            (make-type-class
             :name ',name
             :enumerable-p ,(if enumerable-supplied-p
                                enumerable
                                `(type-class-enumerable-p parent))
             :might-contain-other-types-p
             ,(if might-contain-other-types-supplied-p
                  might-contain-other-types
                  `(type-class-might-contain-other-types-p parent))
             ,@(when inherits
                 (loop for name in !type-class-fun-slots
                       append `(,(keywordicate name)
                                (,(!type-class-fun-slot name) parent))))))))
    #-sb-xc
    `(if (find ',name *type-classes* :key #'type-class-name)
         ;; Careful: type-classes are very complicated things to redefine.
         ;; For the sake of parallelized make-host-1 we have to allow it
         ;; not to be an error to get here, but we can't overwrite anything.
         (style-warn "Not redefining type-class ~S" ',name)
         (vector-push-extend ,make-it *type-classes*))
    ;; The Nth entry in the array of classes contain a list of instances
    ;; of the type-class created by genesis that need patching.
    ;; Types are dumped into the cold core without pointing to their class
    ;; which avoids a bootstrap problem: it's tricky to dump a type-class.
    #+sb-xc
    (let ((type-class-index
           (position name *type-classes* :key #'type-class-name))
          (slot-index
           ;; KLUDGE: silence bogus warning that FIND "certainly" returns NIL
           (locally (declare (notinline find))
             (dsd-index (find 'class-info
                              (dd-slots (find-defstruct-description 'ctype))
                              :key #'dsd-name)))))
      `(!cold-init-forms
        (let* ((backpatch-list (svref *type-classes* ,type-class-index))
               (type-class ,make-it))
          (setf (svref *type-classes* ,type-class-index) type-class)
          #+nil
          (progn
            (princ ,(format nil "Patching type-class ~A into instances: " name))
            (princ (length backpatch-list))
            (terpri))
          (dolist (instance backpatch-list)
            ;; Fixup the class first, in case fixing the hash needs the class.
            ;; (It doesn't currently, but just in case it does)
            (setf (%instance-ref instance ,slot-index) type-class)
            (!fix-ctype-hash instance)))))))

;;; Define the translation from a type-specifier to a type structure for
;;; some particular type. Syntax is identical to DEFTYPE.
;;; Semantics are slightly different though: DEFTYPE causes the default
;;; for missing &OPTIONAL arguments to be '* but a translator requires
;;; an explicit default of '*, or else it assumes a default of NIL.
(defmacro !def-type-translator (name &rest stuff)
  (declare (type symbol name))
  (let* ((allow-atom (if (eq (car stuff) :list) (progn (pop stuff) nil) t))
         (lambda-list (pop stuff))
         (context-var-p (typep (car lambda-list) '(cons (eql :context))))
         (context
          (if context-var-p (cadr (pop lambda-list)) (make-symbol "CONTEXT")))
         ;; If atoms are allowed, then the internal destructuring-bind receives
         ;; NIL when the spec is an atom; it should not take CDR of its input.
         ;; (Note that a &WHOLE argument gets NIL, not the atom in that case)
         ;; If atoms are disallowed, it's basically like a regular macro.
         (lexpr (make-macro-lambda nil lambda-list stuff nil nil
                                   :accessor (if allow-atom 'identity 'cdr)
                                   :environment nil))
         (ll-decl (third lexpr))
         (defun-name (symbolicate "PARSE-<" name ">")))
    (aver (and (eq (car ll-decl) 'declare) (caadr ll-decl) 'sb!c::lambda-list))
    `(progn
       (defun ,defun-name (,context spec)
         ,ll-decl
         ,@(unless context-var-p `((declare (ignore ,context))))
         ,(if allow-atom
              `(,lexpr (and (listp spec) (cdr spec)))
              `(if (listp spec) (,lexpr spec))))
       (!cold-init-forms
        (setf (info :type :expander ',name) (list #',defun-name))))))

;;; Invoke a type method on TYPE1 and TYPE2. If the two types have the
;;; same class, invoke the simple method. Otherwise, invoke any
;;; complex method. If there isn't a distinct COMPLEX-ARG1 method,
;;; then swap the arguments when calling TYPE1's method. If no
;;; applicable method, return DEFAULT.
;;;
;;; KLUDGE: It might be a lot easier to understand this and the rest
;;; of the type system code if we used CLOS to express it instead of
;;; trying to maintain this squirrely hand-crufted object system.
;;; Unfortunately that'd require reworking PCL bootstrapping so that
;;; all the compilation can get done by the cross-compiler, which I
;;; suspect is hard, so we'll bear with the old system for the time
;;; being. -- WHN 2001-03-11
(defmacro !invoke-type-method (simple complex-arg2 type1 type2 &key
                                      (default '(values nil t))
                                        ; assume complex fn is symmetric
                                        ; unless told otherwise.
                                      (complex-arg1 complex-arg2 complex-arg1-p))
  (declare (type keyword simple complex-arg1 complex-arg2))
  (once-only ((left type1)
              (right type2))
    (once-only ((class1 `(type-class-info ,left))
                (class2 `(type-class-info ,right)))
      `(if (eq ,class1 ,class2)
           (funcall (,(!type-class-fun-slot simple) ,class1) ,left ,right)
           (acond ((,(!type-class-fun-slot complex-arg2) ,class2)
                   (funcall it ,left ,right))
                  ((,(!type-class-fun-slot complex-arg1) ,class1)
                   ;; if COMPLEX-ARG1 method was provided, the method accepts
                   ;; the arguments exactly as given. Otherwise, flip them.
                   (funcall it ,@(if complex-arg1-p
                                     `(,left ,right) `(,right ,left))))
                  (t ,default))))))

;;; This is a very specialized implementation of CLOS-style
;;; CALL-NEXT-METHOD within our twisty little type class object
;;; system, which works given that it's called from within a
;;; COMPLEX-SUBTYPEP-ARG2 method. (We're particularly motivated to
;;; implement CALL-NEXT-METHOD in that case, because ANSI imposes some
;;; strict limits on when SUBTYPEP is allowed to return (VALUES NIL NIL),
;;; so instead of just complacently returning (VALUES NIL NIL) from a
;;; COMPLEX-SUBTYPEP-ARG2 method we usually need to CALL-NEXT-METHOD.)
;;;
;;; KLUDGE: In CLOS, this could just be CALL-NEXT-METHOD and
;;; everything would Just Work without us having to think about it. In
;;; our goofy type dispatch system, it's messier to express. It's also
;;; more fragile, since (0) there's no check that it's called from
;;; within a COMPLEX-SUBTYPEP-ARG2 method as it should be, and (1) we
;;; rely on our global knowledge that the next (and only) relevant
;;; method is COMPLEX-SUBTYPEP-ARG1, and (2) we rely on our global
;;; knowledge of the appropriate default for the CSUBTYPEP function
;;; when no next method exists. -- WHN 2002-04-07
;;;
;;; (We miss CLOS! -- CSR and WHN)
(defun invoke-complex-subtypep-arg1-method (type1 type2 &optional subtypep win)
  (let* ((type-class (type-class-info type1))
         (method-fun (type-class-complex-subtypep-arg1 type-class)))
    (if method-fun
        (funcall (the function method-fun) type1 type2)
        (values subtypep win))))

;;; KLUDGE: This function is dangerous, as its overuse could easily
;;; cause stack exhaustion through unbounded recursion.  We only use
;;; it in one place; maybe it ought not to be a function at all?
(defun invoke-complex-=-other-method (type1 type2)
  (let* ((type-class (type-class-info type1))
         (method-fun (type-class-complex-= type-class)))
    (if method-fun
        (funcall (the function method-fun) type2 type1)
        (values nil t))))

;;;; miscellany

;;; Hash two things (types) down to a target fixnum. In CMU CL this was an EQ
;;; hash, but since it now needs to run in vanilla ANSI Common Lisp at
;;; cross-compile time, it's now based on the CTYPE-HASH-VALUE field
;;; instead.
;;;
;;; FIXME: This was a macro in CMU CL, and is now an INLINE function. Is
;;; it important for it to be INLINE, or could be become an ordinary
;;; function without significant loss? -- WHN 19990413
#!-sb-fluid (declaim (inline type-cache-hash))
(declaim (ftype (function (ctype ctype) (signed-byte #.sb!vm:n-fixnum-bits))
                type-cache-hash))
(defun type-cache-hash (type1 type2)
  (logxor (ash (type-hash-value type1) -3) (type-hash-value type2)))

#!-sb-fluid (declaim (inline type-list-cache-hash))
(declaim (ftype (function (list) (signed-byte #.sb!vm:n-fixnum-bits))
                type-list-cache-hash))
(defun type-list-cache-hash (types)
  (loop with res of-type (signed-byte #.sb!vm:n-fixnum-bits) = 0
        for type in types
        do (setq res (logxor (ash res -1) (type-hash-value type)))
        finally (return res)))

(!defun-from-collected-cold-init-forms !type-class-cold-init)

;;; A few type representations need to be defined slightly earlier than
;;; 'early-type' is compiled, so they're defined here.

;;; The NAMED-TYPE is used to represent *, T and NIL, the standard
;;; special cases, as well as other special cases needed to
;;; interpolate between regions of the type hierarchy, such as
;;; INSTANCE (which corresponds to all those classes with slots which
;;; are not funcallable), FUNCALLABLE-INSTANCE (those classes with
;;; slots which are funcallable) and EXTENDED-SEQUUENCE (non-LIST
;;; non-VECTOR classes which are also sequences).  These special cases
;;; are the ones that aren't really discussed by Baker in his
;;; "Decision Procedure for SUBTYPEP" paper.
(defstruct (named-type (:include ctype
                                 (class-info (type-class-or-lose 'named)))
                       (:copier nil))
  (name nil :type symbol :read-only t))

;;; A MEMBER-TYPE represent a use of the MEMBER type specifier. We
;;; bother with this at this level because MEMBER types are fairly
;;; important and union and intersection are well defined.
(defstruct (member-type (:include ctype
                                  (class-info (type-class-or-lose 'member)))
                        (:copier nil)
                        (:constructor %make-member-type (xset fp-zeroes))
                        #-sb-xc-host (:pure nil))
  (xset nil :type xset :read-only t)
  (fp-zeroes nil :type list :read-only t))

;;; An ARRAY-TYPE is used to represent any array type, including
;;; things such as SIMPLE-BASE-STRING.
(defstruct (array-type (:include ctype
                                 (class-info (type-class-or-lose 'array)))
                       (:constructor %make-array-type
                        (dimensions complexp element-type
                                    specialized-element-type))
                       (:copier nil))
  ;; the dimensions of the array, or * if unspecified. If a dimension
  ;; is unspecified, it is *.
  (dimensions '* :type (or list (member *)) :read-only t)
  ;; Is this not a simple array type? (:MAYBE means that we don't know.)
  (complexp :maybe :type (member t nil :maybe) :read-only t)
  ;; the element type as originally specified
  (element-type nil :type ctype :read-only t)
  ;; the element type as it is specialized in this implementation
  (specialized-element-type nil :type ctype :read-only t))

(defstruct (character-set-type
            (:include ctype
                      (class-info (type-class-or-lose 'character-set)))
            (:constructor %make-character-set-type (pairs))
            (:copier nil))
  (pairs (missing-arg) :type list :read-only t))

;;; A COMPOUND-TYPE is a type defined out of a set of types, the
;;; common parent of UNION-TYPE and INTERSECTION-TYPE.
(defstruct (compound-type (:include ctype)
                          (:constructor nil)
                          (:copier nil))
  ;; Formerly defined in every CTYPE, but now just in the ones
  ;; for which enumerability is variable.
  (enumerable nil :read-only t)
  (types nil :type list :read-only t))

;;; A UNION-TYPE represents a use of the OR type specifier which we
;;; couldn't canonicalize to something simpler. Canonical form:
;;;   1. All possible pairwise simplifications (using the UNION2 type
;;;      methods) have been performed. Thus e.g. there is never more
;;;      than one MEMBER-TYPE component. FIXME: As of sbcl-0.6.11.13,
;;;      this hadn't been fully implemented yet.
;;;   2. There are never any UNION-TYPE components.
;;;
;;; TODO: As STRING is an especially important union type,
;;; it could be interned by canonicalizing its subparts into
;;; ARRAY of {CHARACTER,BASE-CHAR,NIL} in that exact order always.
;;; It will therefore admit quick TYPE=, but not quick failure, since
;;;   (type= (specifier-type '(or (simple-array (member #\a) (*))
;;;                               (simple-array character (*))
;;;                               (simple-array nil (*))))
;;;          (specifier-type 'simple-string)) => T and T
;;; even though (MEMBER #\A) is not TYPE= to BASE-CHAR.
;;;
(defstruct (union-type (:include compound-type
                                 (class-info (type-class-or-lose 'union)))
                       (:constructor make-union-type (enumerable types))
                       (:copier nil)))

;;; An INTERSECTION-TYPE represents a use of the AND type specifier
;;; which we couldn't canonicalize to something simpler. Canonical form:
;;;   1. All possible pairwise simplifications (using the INTERSECTION2
;;;      type methods) have been performed. Thus e.g. there is never more
;;;      than one MEMBER-TYPE component.
;;;   2. There are never any INTERSECTION-TYPE components: we've
;;;      flattened everything into a single INTERSECTION-TYPE object.
;;;   3. There are never any UNION-TYPE components. Either we should
;;;      use the distributive rule to rearrange things so that
;;;      unions contain intersections and not vice versa, or we
;;;      should just punt to using a HAIRY-TYPE.
(defstruct (intersection-type (:include compound-type
                                        (class-info (type-class-or-lose
                                                     'intersection)))
                              (:constructor %make-intersection-type
                                            (enumerable types))
                              (:copier nil)))

;;; a list of all the float "formats" (i.e. internal representations;
;;; nothing to do with #'FORMAT), in order of decreasing precision
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *float-formats*
    '(long-float double-float single-float short-float)))

;;; The type of a float format.
(deftype float-format () `(member ,@*float-formats*))

;;; A NUMERIC-TYPE represents any numeric type, including things
;;; such as FIXNUM.
(defstruct (numeric-type (:include ctype
                                   (class-info (type-class-or-lose 'number)))
                         (:constructor %make-numeric-type)
                         (:copier nil))
  ;; Formerly defined in every CTYPE, but now just in the ones
  ;; for which enumerability is variable.
  (enumerable nil :type boolean :read-only t)
  ;; the kind of numeric type we have, or NIL if not specified (just
  ;; NUMBER or COMPLEX)
  ;;
  ;; KLUDGE: A slot named CLASS for a non-CLASS value is bad.
  ;; Especially when a CLASS value *is* stored in another slot (called
  ;; CLASS-INFO:-). Perhaps this should be called CLASS-NAME? Also
  ;; weird that comment above says "Numeric-Type is used to represent
  ;; all numeric types" but this slot doesn't allow COMPLEX as an
  ;; option.. how does this fall into "not specified" NIL case above?
  ;; Perhaps someday we can switch to CLOS and make NUMERIC-TYPE
  ;; be an abstract base class and INTEGER-TYPE, RATIONAL-TYPE, and
  ;; whatnot be concrete subclasses..
  (class nil :type (member integer rational float nil) :read-only t)
  ;; "format" for a float type (i.e. type specifier for a CPU
  ;; representation of floating point, e.g. 'SINGLE-FLOAT -- nothing
  ;; to do with #'FORMAT), or NIL if not specified or not a float.
  ;; Formats which don't exist in a given implementation don't appear
  ;; here.
  (format nil :type (or float-format null) :read-only t)
  ;; Is this a complex numeric type?  Null if unknown (only in NUMBER).
  ;;
  ;; FIXME: I'm bewildered by FOO-P names for things not intended to
  ;; interpreted as truth values. Perhaps rename this COMPLEXNESS?
  (complexp :real :type (member :real :complex nil) :read-only t)
  ;; The upper and lower bounds on the value, or NIL if there is no
  ;; bound. If a list of a number, the bound is exclusive. Integer
  ;; types never have exclusive bounds, i.e. they may have them on
  ;; input, but they're canonicalized to inclusive bounds before we
  ;; store them here.
  (low nil :type (or number cons null) :read-only t)
  (high nil :type (or number cons null) :read-only t))

;;; A CONS-TYPE is used to represent a CONS type.
(defstruct (cons-type (:include ctype (class-info (type-class-or-lose 'cons)))
                      (:constructor
                       %make-cons-type (car-type
                                        cdr-type))
                      (:copier nil))
  ;; the CAR and CDR element types (to support ANSI (CONS FOO BAR) types)
  (car-type (missing-arg) :type ctype :read-only t)
  (cdr-type (missing-arg) :type ctype :read-only t))

(in-package "SB!ALIEN")
(def!struct (alien-type
             (:make-load-form-fun sb!kernel:just-dump-it-normally)
             (:constructor make-alien-type
                           (&key class bits alignment
                            &aux (alignment
                                  (or alignment (guess-alignment bits))))))
  (class 'root :type symbol :read-only t)
  (bits nil :type (or null unsigned-byte))
  (alignment nil :type (or null unsigned-byte)))

(in-package "SB!KERNEL")
(defstruct (alien-type-type
            (:include ctype
                      (class-info (type-class-or-lose 'alien)))
            (:constructor %make-alien-type-type (alien-type))
            (:copier nil))
  (alien-type nil :type alien-type :read-only t))

;;; the description of a &KEY argument
(defstruct (key-info #-sb-xc-host (:pure t)
                     (:copier nil))
  ;; the key (not necessarily a keyword in ANSI Common Lisp)
  (name (missing-arg) :type symbol :read-only t)
  ;; the type of the argument value
  (type (missing-arg) :type ctype :read-only t))

;;; ARGS-TYPE objects are used both to represent VALUES types and
;;; to represent FUNCTION types.
(defstruct (args-type (:include ctype)
                      (:constructor nil)
                      (:copier nil))
  ;; Lists of the type for each required and optional argument.
  (required nil :type list :read-only t)
  (optional nil :type list :read-only t)
  ;; The type for the rest arg. NIL if there is no &REST arg.
  (rest nil :type (or ctype null) :read-only t)
  ;; true if &KEY arguments are specified
  (keyp nil :type boolean :read-only t)
  ;; list of KEY-INFO structures describing the &KEY arguments
  (keywords nil :type list :read-only t)
  ;; true if other &KEY arguments are allowed
  (allowp nil :type boolean :read-only t))

;;; (SPECIFIER-TYPE 'FUNCTION) and its subtypes
(defstruct (fun-type (:include args-type
                               (class-info (type-class-or-lose 'function)))
                     (:constructor
                      %make-fun-type (required optional rest
                                      keyp keywords allowp wild-args returns)))
  ;; true if the arguments are unrestrictive, i.e. *
  (wild-args nil :type boolean :read-only t)
  ;; type describing the return values. This is a values type
  ;; when multiple values were specified for the return.
  (returns (missing-arg) :type ctype :read-only t))

(declaim (ftype (sfunction (ctype ctype) (values t t)) csubtypep))
;;; Look for nice relationships for types that have nice relationships
;;; only when one is a hierarchical subtype of the other.
(defun hierarchical-intersection2 (type1 type2)
  ;; *EMPTY-TYPE* is involved in a dependency cycle: It wants to be a constant
  ;; instance of NAMED-TYPE. To construct an instance of a type, you need a
  ;; type-class. A type-class needs to refer to this function, which refers
  ;; to *EMPTY-TYPE*, which .... etc.
  ;; In the cross-compiler, it is actually a constant.
  #+sb-xc-host (declare (special *empty-type*))
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
