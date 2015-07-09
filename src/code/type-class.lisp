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
                (!cold-init-forms
                 (setq *ctype-hash-state* ,initform
                       *type-classes* (make-array ,n)))))))
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

(declaim (ftype (sfunction (ctype ctype) (values t t)) csubtypep))
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
  (might-contain-other-types-p nil)
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
  (enumerable-p nil :type (or function null t))
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
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (unless (find ',name *type-classes* :key #'type-class-name)
           (vector-push-extend ,make-it *type-classes*))))
    #+sb-xc
    `(!cold-init-forms
      (setf (svref *type-classes*
                   ,(position name *type-classes* :key #'type-class-name))
            ,make-it))))

;;; Define the translation from a type-specifier to a type structure for
;;; some particular type. Syntax is identical to DEFTYPE.
;;; Semantics are slightly different though: DEFTYPE causes the default
;;; for missing &OPTIONAL arguments to be '* but a translator requires
;;; an explicit default of '*, or else it assumes a default of NIL.
(defmacro !def-type-translator (name arglist &body body)
  (declare (type symbol name))
  (multiple-value-bind (fun #-sb-xc-host arglist)
      (make-macro-lambda (format nil "~A-TYPE-PARSE" name)
                         arglist body nil nil :environment nil)
    `(!cold-init-forms
      (let ((fun ,fun))
        #-sb-xc-host
        (setf (%simple-fun-arglist (the simple-fun fun)) ',arglist)
        (setf (info :type :translator ',name) fun)))))

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
