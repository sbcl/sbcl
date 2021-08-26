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

(in-package "SB-KERNEL")

(!begin-collecting-cold-init-forms)

;; We can't make an instance of any CTYPE descendant until its type-class
;; exists in *TYPE-CLASSES* and the quasi-random state has been made.
;; By initializing the state and type-class storage vector at once,
;; it is obvious that either both have been made or neither one has been.
#-sb-xc
(progn (defvar *ctype-lcg-state* 1)
       (defvar *ctype-hash-state* (make-random-state))
       ;; There are 5 bits in a type-class index, so at most 32 type-classes
       ;; of which about 17 are currently defined.
       (defvar *type-classes* (make-array 32 :fill-pointer 0))
       ;; We track for each type-class whether it has any descendant class.
       ;; Inheritance is implemented by copying the vtable from an ancestor
       ;; to the descendant at the time the descendant is defined.
       ;; So the following minimal example might not do what you expect:
       ;;  (DEFINE-TYPE-CLASS ROOT)
       ;;  (DEFINE-TYPE-CLASS CHILD :INHERITS ROOT)
       ;;  (DEFINE-TYPE-METHOD (ROOT :SOME-METHOD) ...)
       ;; CHILD fails to copy a pointer to SOME-METHOD.
       ;; This is subtle and perhaps unintuitive. As such, we guard against
       ;; it by preventing DEFINE-TYPE-METHOD after use of :INHERITS.
       (defvar *type-class-was-inherited*
         (make-array 32 :element-type 'bit :initial-element 0)))

#+sb-xc
(macrolet ((def (name init-form)
             `(progn
                (define-load-time-global ,name ,init-form)
                (!cold-init-forms (setq ,name ,init-form)))))
  (declaim (type (simple-array (and fixnum unsigned-byte) (1))
                 *ctype-hash-state*)
           (type (simple-vector 32) *type-classes*)
           (type fixnum *type-cache-nonce*))
  (def *ctype-hash-state* (make-array 1 :element-type '(and fixnum unsigned-byte)
                                        :initial-element 0))
  (def *type-classes* (make-array 32 :initial-element nil))
  ;; This is for "observers" who want to know if type names have been added.
  ;; Rather than registering listeners, they can detect changes by comparing
  ;; their stored nonce to the current nonce. Additionally the observers
  ;; can detect whether function definitions have occurred.
  (def *type-cache-nonce* 0))

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
  (declare (notinline position)) ; out-of-order use of #'type-class-name
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
(defstruct (type-class
             (:copier nil)
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
(declaim (freeze-type type-class))

(defun type-class-or-lose (name)
  (or (find name *type-classes* :key #'type-class-name)
      (error "~S is not a defined type class." name)))

(defun type-class-name->id (name)
  (or (position name *type-classes* :key #'type-class-name)
      (error "~S is not a defined type class." name)))

;;; DEFINE-TYPE-METHOD, DEFINE-TYPE-CLASS, and DEFSTRUCT forms for each
;;; type class can appear in a random order. To allow this, we have to delay
;;; lookup of the index into *TYPE-CLASSES*. It would be inefficient to scan
;;; *TYPE-CLASSES* on every call of a constructor of an instance though,
;;; so we cache the index in a cons cell.
;;; make-host-2 has an easier time at this - it just looks at the vector
;;; of *TYPE-CLASSES* built up in make-host-1. No caching necessary.
(defmacro memoized-type-class-name->id (class-name)
  #+sb-xc-host
  `(let* ((cell (load-time-value (list ',class-name)))
          (value (car cell)))
     (if (integerp value)
         value
         (setf (car cell) (type-class-name->id ',class-name))))
  #-sb-xc-host
  (position class-name *type-classes* :key #'type-class-name))

(defun ctype-random ()
  #+sb-xc-host
  (setq *ctype-lcg-state*
             (logand #x8fffff (+ (* 1103515245 *ctype-lcg-state*) 12345)))
  #-sb-xc-host
  (sb-impl::quasi-random-address-based-hash *ctype-hash-state* #xfffffff))

;;; the base class for the internal representation of types

;; Each CTYPE instance (incl. subtypes thereof) has a random opaque hash value.
;; Hashes are mixed together to form a lookup key in the memoization wrappers
;; for most operations in CTYPES. This works because CTYPEs are immutable.
(def!struct (ctype (:conc-name type-)
                   (:constructor nil)
                   (:copier nil)
                   (:pure t))
  ;; bits  0..19: 20 bits for opaque hash
  ;; bit      20: 1 if interned: specifier -> object is guaranteed unique
  ;; bit      21: 1 if admits type= optimization: NEQ implies (NOT TYPE=)
  ;; bits 22..26: 5 bits for specialized-array-element-type-properties index.
  ;;   also usable as hash bits if this ctype is not a character-set
  ;;   or numeric type or named-type T or NIL
  ;; bits 27..31: 5 bits for type-class index
  ;; We'll never return the upper 5 bits from a hash mixer, so it's fine
  ;; that this uses all 32 bits for a 32-bit word.
  ;; No more than 32 bits are used, even for 64-bit words.
  ;; But it's consistent for genesis to treat it always as a raw slot.
  (%bits (missing-arg) :type sb-vm:word :read-only t))

(defmethod print-object ((ctype ctype) stream)
  (print-unreadable-object (ctype stream :type t)
    (prin1 (type-specifier ctype) stream)))

;;; take 27 low bits but exclude bits 20 and 21
;;; [Our MASK-FIELD can't be folded, and I didn't feel like fixing that.]
(defconstant +type-hash-mask+
  #.(cl:logandc2 (cl:ldb (cl:byte 27 0) -1) (cl:mask-field (cl:byte 2 20) -1)))

(defmacro type-class-id (ctype) `(ldb (byte 5 27) (type-%bits ,ctype)))
(defmacro type-class (ctype)
  `(truly-the type-class (aref *type-classes* (type-class-id ,ctype))))

(declaim (inline type-hash-value))
(defun type-hash-value (ctype) (ldb (byte 27 0) (type-%bits ctype)))

;;; Represent an index into *SPECIALIZED-ARRAY-ELEMENT-TYPE-PROPERTIES*
;;; if applicable. For types which are not array specializations,
;;; the bits are arbitrary.
(defmacro type-saetp-index (ctype) `(ldb (byte 5 22) (type-%bits ,ctype)))

(defconstant +type-internedp+ (ash 1 20))
(defconstant +type-admits-type=-optimization+ (ash 1 21))
(defmacro type-bits-internedp (bits) `(logbitp 20 ,bits))
(defmacro type-bits-admit-type=-optimization (bits) `(logbitp 21 ,bits))

;;; For system build-time only
(defun pack-interned-ctype-bits (type-class &optional hash saetp-index)
  (let ((hash (or hash (ctype-random))))
    (logior (ash (type-class-name->id type-class) 27)
            (if saetp-index
                (logior (ash saetp-index 22) (ldb (byte 20 0) hash))
                (logand hash +type-hash-mask+))
            ;; type= optimization is valid if not an array-type
            (if (eq type-class 'array) 0 +type-admits-type=-optimization+)
            +type-internedp+)))

;;; For runtime
;;; CLASSOIDs have a deterministic hash based on the name,
;;; utilizing the same hash calculation as with LAYOUT instances.
;;; NUMBER types could compute a stable hash too (but they don't currently)
;;; which would mean that HASH-not-equal implies TYPE-not-equal.
;;; Most other things would not benefit.
(defmacro pack-ctype-bits (type-class &optional name)
 ;;; TYPE-CLASS is an unevaluated argument
  (when (eq type-class 'classoid)
    (aver name))
  (let ((hash (if name `(hash-layout-name ,name) '(ctype-random))))
    `(logior (ash (memoized-type-class-name->id ,type-class) 27)
             (logand ,hash +type-hash-mask+)
             ;; NUMBER, MEMBER, and CLASSOID admit TYPE= optimization.
             ;; Other type classes might, but this is the conservative assumption.
             ,@(when (member type-class '(number member classoid))
                 '(+type-admits-type=-optimization+))
             ;; The mapping from name to a CLASSOID type is unique,
             ;; therefore all CLASSOIDs have the "interned" bit on.
             ,@(when (eq type-class 'classoid)
                 '(+type-internedp+)))))

(declaim (inline type-might-contain-other-types-p))
(defun type-might-contain-other-types-p (ctype)
  (type-class-might-contain-other-types-p (type-class ctype)))

(declaim (inline type-enumerable))
(defun type-enumerable (ctype)
  (let ((answer (type-class-enumerable-p (type-class ctype))))
    (if (functionp answer)
        (funcall answer ctype)
        answer)))

#+sb-xc
(eval-when (:compile-toplevel)
  (assert (= (length (dd-slots (find-defstruct-description 'type-class)))
             ;; there exist two boolean slots, plus NAME
             (+ (length type-class-fun-slots) 3))))

;; Unfortunately redundant with the slots in the DEF!STRUCT,
;; but allows asserting about correctness of the constructor
;; without relying on introspection in host Lisp.
(defconstant-eqx type-class-fun-slots
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
  (defun type-class-fun-slot (name)
    (unless (member name type-class-fun-slots
                    :key (if (keywordp name) 'keywordicate 'identity))
      (warn "Undefined type-class method ~S" name))
    (package-symbolicate "SB-KERNEL" "TYPE-CLASS-" name)))

(defmacro define-type-method ((class method &rest more-methods)
                               lambda-list &body body)
  (let* ((name (symbolicate class "-" method "-TYPE-METHOD"))
         (arg-type
          (case class
            (classoid 'classoid)
            (number 'numeric-type)
            (function 'fun-type)
            (alien 'alien-type-type)
            (t (symbolicate class "-TYPE"))))
         (first (car lambda-list))
         (second (cadr lambda-list))
         ;; make-host-1 verifies that type methods are invoked correctly,
         ;; but afterwards we assume that they are
         (operator #+sb-xc-host 'the #-sb-xc-host 'truly-the)
         (rebind
          (unless more-methods
            (case method
              ((:complex-subtypep-arg1 :unparse :negate :singleton-p)
               `((,first (,operator ,arg-type ,first))))
              ((:complex-subtypep-arg2)
               `((,first ,first) ; because there might be a DECLARE IGNORE on it
                 (,second (,operator ,arg-type ,second))))
              ((:simple-intersection2 :simple-union2 :simple-subtypep :simple-=)
               `((,first (,operator ,arg-type ,first))
                 (,second (,operator ,arg-type ,second))))))))
    `(progn
       #+sb-xc-host
       (when (plusp (bit *type-class-was-inherited* (type-class-name->id ',class)))
         ;; This disallows one case that would be ok - a method definition for
         ;; both an ancestor and its descendants on some method.
         ;; Too bad for you- this throws the baby out with the bathwater.
         (error "Can't define-type-method for class ~s: already inherited" ',class))
       (defun ,name ,lambda-list ,@(if rebind `((let ,rebind ,@body)) body))
       (!cold-init-forms
        ,@(mapcar (lambda (method)
                    `(setf (,(type-class-fun-slot method)
                            (type-class-or-lose ',class))
                           #',name))
                  (cons method more-methods)))
       ',name)))

(defmacro define-type-class (name &key inherits
                                     (enumerable (unless inherits (missing-arg))
                                                 enumerable-supplied-p)
                                     (might-contain-other-types
                                      (unless inherits (missing-arg))
                                      might-contain-other-types-supplied-p))
  (let ((make-it
         `(let* ,(if inherits `((parent-index (type-class-name->id ',inherits))
                                (parent (aref *type-classes* parent-index))))
            #+sb-xc-host
            ,@(when inherits
                `((setf (bit *type-class-was-inherited* parent-index) 1)))
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
                 (loop for name in type-class-fun-slots
                       append `(,(keywordicate name)
                                (,(type-class-fun-slot name) parent))))))))
    #+sb-xc-host
    `(progn
       ;; Careful: type-classes are very complicated things to redefine.
       ;; For the sake of parallelized make-host-1 we have to allow
       ;; redefinition, but it has to be a no-op.
       (unless (find ',name *type-classes* :key #'type-class-name)
         (vector-push ,make-it *type-classes*))
       ;; I have no idea what compiler bug could be worked around by adding a form here,
       ;; but this certainly achieves something, somehow.
       #+host-quirks-cmu (print (aref *type-classes* (1- (length *type-classes*)))))

    #+sb-xc
    (let ((type-class-index
           (position name *type-classes* :key #'type-class-name)))
      `(!cold-init-forms
        (setf (svref *type-classes* ,type-class-index) ,make-it)))))

;;; Define the translation from a type-specifier to a type structure for
;;; some particular type. Syntax is identical to DEFTYPE.
;;; Semantics are slightly different though: DEFTYPE causes the default
;;; for missing &OPTIONAL arguments to be '* but a translator requires
;;; an explicit default of '*, or else it assumes a default of NIL.
(defmacro def-type-translator (name &rest stuff)
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
    (aver (and (eq (car ll-decl) 'declare) (caadr ll-decl) 'sb-c::lambda-list))
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
    (once-only ((class1 `(type-class ,left))
                (class2 `(type-class ,right)))
      `(if (eq ,class1 ,class2)
           (funcall (,(type-class-fun-slot simple) ,class1) ,left ,right)
           (acond ((,(type-class-fun-slot complex-arg2) ,class2)
                   (funcall it ,left ,right))
                  ((,(type-class-fun-slot complex-arg1) ,class1)
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
  (let* ((type-class (type-class type1))
         (method-fun (type-class-complex-subtypep-arg1 type-class)))
    (if method-fun
        (funcall (the function method-fun) type1 type2)
        (values subtypep win))))

(defvar *invoked-complex-=-other-method* nil)
(defun invoke-complex-=-other-method (type1 type2)
  (let* ((type-class (type-class type1))
         (method-fun (type-class-complex-= type-class)))
    (if (and method-fun (not *invoked-complex-=-other-method*))
        (let ((*invoked-complex-=-other-method* t))
          (funcall (the function method-fun) type2 type1))
        (values nil t))))

;; The following macros expand into either constructor calls,
;; if building the cross-compiler, or forms which reference
;; previously constructed objects, if running the cross-compiler.
#+sb-xc-host
(progn
  (defmacro literal-ctype (constructor &optional specifier)
    (declare (ignore specifier))
    `(load-time-value ,constructor))

  (defmacro literal-ctype-vector (var)
    `(load-time-value ,var nil)))

;; Omitting the specifier works only if the unparser method has been
;; defined in time to use it, and you're sure that constructor's result
;; can be unparsed - some unparsers may be confused if called on a
;; non-canonical object, such as an instance of (CONS T T) that is
;; not EQ to the interned instance.
#-sb-xc-host
(progn
  (defmacro literal-ctype (constructor &optional (specifier nil specifier-p))
    (if specifier-p (specifier-type specifier) (symbol-value constructor)))

  (defmacro literal-ctype-vector (var)
    (symbol-value var)))

;;;; miscellany

;;; Hash two things (types) down to a target fixnum. In CMU CL this was an EQ
;;; hash, but since it now needs to run in vanilla ANSI Common Lisp at
;;; cross-compile time, it's now based on the CTYPE-HASH-VALUE field
;;; instead.
;;;
;;; FIXME: This was a macro in CMU CL, and is now an INLINE function. Is
;;; it important for it to be INLINE, or could be become an ordinary
;;; function without significant loss? -- WHN 19990413
(declaim (inline type-cache-hash))
(declaim (ftype (function (ctype ctype) (signed-byte #.sb-vm:n-fixnum-bits))
                type-cache-hash))
(defun type-cache-hash (type1 type2)
  (logxor (ash (type-hash-value type1) -3) (type-hash-value type2)))

(declaim (inline type-list-cache-hash))
(declaim (ftype (function (list) (signed-byte #.sb-vm:n-fixnum-bits))
                type-list-cache-hash))
(defun type-list-cache-hash (types)
  (loop with res of-type (signed-byte #.sb-vm:n-fixnum-bits) = 0
        for type in types
        do (setq res (logxor (ash res -1) (type-hash-value type)))
        finally (return res)))

;;;; representations of types

;;; The NAMED-TYPE is used to represent *, T and NIL, the standard
;;; special cases, as well as other special cases needed to
;;; interpolate between regions of the type hierarchy, such as
;;; INSTANCE (which corresponds to all those classes with slots which
;;; are not funcallable), FUNCALLABLE-INSTANCE (those classes with
;;; slots which are funcallable) and EXTENDED-SEQUENCE (non-LIST
;;; non-VECTOR classes which are also sequences).  These special cases
;;; are the ones that aren't really discussed by Baker in his
;;; "Decision Procedure for SUBTYPEP" paper.
(defstruct (named-type (:include ctype)
                       (:constructor !make-named-type (%bits name))
                       (:copier nil))
  (name nil :type symbol :read-only t))

;;; A HAIRY-TYPE represents anything too weird to be described
;;; reasonably or to be useful, such as NOT, SATISFIES, unknown types,
;;; and unreasonably complicated types involving AND. We just remember
;;; the original type spec.
;;; A possible improvement would be for HAIRY-TYPE to have a subtype
;;; named SATISFIES-TYPE for the hairy types which are specifically
;;; of the form (SATISFIES pred) so that we don't have to examine
;;; the sexpr repeatedly to decide whether it takes that form.
;;; And as a further improvement, we might want a table that maps
;;; predicates to their exactly recognized type when possible.
;;; We have such a table in fact - *BACKEND-PREDICATE-TYPES*
;;; as a starting point. But something like PLUSP isn't in there.
;;; On the other hand, either of these points may not be sources of
;;; inefficiency, and the latter if implemented might have undesirable
;;; user-visible ramifications, though it seems unlikely.
(defstruct (hairy-type (:include ctype)
                       (:constructor %make-hairy-type
                           (specifier &aux (%bits (pack-ctype-bits hairy))))
                       (:constructor !make-interned-hairy-type
                           (specifier &aux (%bits (pack-interned-ctype-bits 'hairy))))
                       (:copier nil))
  ;; the Common Lisp type-specifier of the type we represent.
  ;; For other than an unknown type, this must be a (SATISFIES f) expression.
  (specifier nil :type t :read-only t))

;;; A MEMBER-TYPE represent a use of the MEMBER type specifier. We
;;; bother with this at this level because MEMBER types are fairly
;;; important and union and intersection are well defined.
(defstruct (member-type (:include ctype (%bits (pack-ctype-bits member)))
                        (:copier nil)
                        (:constructor %make-member-type (xset fp-zeroes))
                        (:constructor !make-interned-member-type (%bits xset fp-zeroes))
                        #-sb-xc-host (:pure nil))
  (xset nil :type xset :read-only t)
  (fp-zeroes nil :type list :read-only t))

;;; An ARRAY-TYPE is used to represent any array type, including
;;; things such as SIMPLE-BASE-STRING.
(defstruct (array-type (:include ctype (%bits (pack-ctype-bits array)))
                       (:constructor %make-array-type
                        (dimensions complexp element-type
                                    specialized-element-type))
                       (:constructor !make-interned-array-type
                        (%bits dimensions complexp element-type
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
            (:include ctype (%bits (pack-ctype-bits character-set)))
            (:constructor %make-character-set-type (pairs))
            (:constructor !make-interned-character-set-type (%bits pairs))
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
(defstruct (union-type (:include compound-type (%bits (pack-ctype-bits union)))
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
                               (%bits (pack-ctype-bits intersection)))
                              (:constructor %make-intersection-type
                                            (enumerable types))
                              (:copier nil)))

(defstruct (alien-type-type
            (:include ctype (%bits (pack-ctype-bits alien)))
            (:constructor %make-alien-type-type (alien-type))
            (:copier nil))
  (alien-type nil :type alien-type :read-only t))

(defstruct (negation-type (:include ctype (%bits (pack-ctype-bits negation)))
                          (:copier nil)
                          (:constructor make-negation-type (type)))
  (type (missing-arg) :type ctype :read-only t))

;;; An UNKNOWN-TYPE is a type not known to the type system (not yet
;;; defined). We make this distinction since we don't want to complain
;;; about types that are hairy but defined.
(defstruct (unknown-type (:include hairy-type (%bits (pack-ctype-bits hairy)))
                         (:copier nil)))

;;; a list of all the float "formats" (i.e. internal representations;
;;; nothing to do with #'FORMAT), in order of decreasing precision
(defglobal *float-formats*
    '(long-float double-float single-float short-float))

;;; The type of a float format.
(deftype float-format () `(member ,@*float-formats*))

;;; A NUMERIC-TYPE represents any numeric type, including things
;;; such as FIXNUM.
(defstruct (numeric-type (:include ctype (%bits (pack-ctype-bits number)))
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
  (low nil :type (or real (cons real null) null) :read-only t)
  (high nil :type (or real (cons real null) null) :read-only t))

;;; A CONS-TYPE is used to represent a CONS type.
(defstruct (cons-type (:include ctype (%bits (pack-ctype-bits cons)))
                      (:constructor %make-cons-type (car-type cdr-type))
                      (:constructor !make-interned-cons-type (%bits car-type cdr-type))
                      (:copier nil))
  ;; the CAR and CDR element types (to support ANSI (CONS FOO BAR) types)
  (car-type (missing-arg) :type ctype :read-only t)
  (cdr-type (missing-arg) :type ctype :read-only t))

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

;;; the description of a &KEY argument
(defstruct (key-info #-sb-xc-host (:pure t)
                     (:copier nil))
  ;; the key (not necessarily a keyword in ANSI Common Lisp)
  (name (missing-arg) :type symbol :read-only t)
  ;; the type of the argument value
  (type (missing-arg) :type ctype :read-only t))
(declaim (freeze-type key-info))

(defstruct (values-type
            (:include args-type (%bits (pack-ctype-bits values)))
            (:constructor %make-values-type)
            (:copier nil)))

(declaim (freeze-type values-type))

;;; (SPECIFIER-TYPE 'FUNCTION) and its subtypes
(defstruct (fun-type (:include args-type (%bits (pack-ctype-bits function)))
                     (:copier nil)
                     (:constructor
                      %make-fun-type (required optional rest
                                      keyp keywords allowp wild-args returns))
                     (:constructor !make-interned-fun-type
                         (%bits required optional rest keyp keywords
                          allowp wild-args returns)))
  ;; true if the arguments are unrestrictive, i.e. *
  (wild-args nil :type boolean :read-only t)
  ;; type describing the return values. This is a values type
  ;; when multiple values were specified for the return.
  (returns (missing-arg) :type ctype :read-only t))

(defstruct (fun-designator-type
            (:include fun-type)
            (:copier nil)
            (:conc-name fun-type-)
            (:constructor make-fun-designator-type
                (required optional rest
                 keyp keywords allowp wild-args returns))))

;;; The CONSTANT-TYPE structure represents a use of the CONSTANT-ARG
;;; "type specifier", which is only meaningful in function argument
;;; type specifiers used within the compiler. (It represents something
;;; that the compiler knows to be a constant.)
(defstruct (constant-type
            (:include ctype (%bits (pack-ctype-bits constant)))
            (:copier nil))
  ;; The type which the argument must be a constant instance of for this type
  ;; specifier to win.
  (type (missing-arg) :type ctype :read-only t))


;;; A SIMD-PACK-TYPE is used to represent a SIMD-PACK type.
#+sb-simd-pack
(defstruct (simd-pack-type
            (:include ctype (%bits (pack-ctype-bits simd-pack)))
            (:constructor %make-simd-pack-type (element-type))
            (:copier nil))
  (element-type (missing-arg)
   :type (cons #||(member #.*simd-pack-element-types*) ||#)
   :read-only t))

#+sb-simd-pack-256
(defstruct (simd-pack-256-type
            (:include ctype (%bits (pack-ctype-bits simd-pack-256)))
            (:constructor %make-simd-pack-256-type (element-type))
            (:copier nil))
  (element-type (missing-arg)
   :type (cons #||(member #.*simd-pack-element-types*) ||#)
   :read-only t))

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

(!defun-from-collected-cold-init-forms !type-class-cold-init)

;;; CAUTION: unhygienic macro specifically designed to expand into body code
;;; for TYPEP, CTYPEP (compiler-typep), or CROSS-TYPEP (cross-compiler-[c]typep)
(defmacro typep-impl-macro ((thing &key (defaults t)) &rest more-clauses &aux seen)
  (labels ((convert-clause (clause)
             (let ((metatype (car clause)))
               `(,(if (and (consp metatype) (eq (car metatype) 'or))
                      (mapcar #'metatype-name->class-id (cdr metatype))
                      (list (metatype-name->class-id metatype)))
                 (let ((type (truly-the ,metatype type))) ,@(cdr clause)))))
           (metatype-name->class-id (name)
             ;; See also DEFINE-TYPE-METHOD which needs the inverse mapping.
             ;; Maybe it should be stored globally in an alist?
             (let* ((type-class-name
                      (case name
                        ((values-type constant-type)
                         (bug "Unexpected type ~S in CTYPEP-MACRO" name))
                        (classoid 'classoid)
                        (numeric-type 'number)
                        (fun-type 'function)
                        (alien-type-type 'alien)
                        ;; remove "-TYPE" suffix from name of type's type to get
                        ;; name of type-class.
                        (t (intern (subseq (string name) 0 (- (length (string name)) 5))
                                   "SB-KERNEL"))))
                    (id (type-class-name->id type-class-name)))
               (when (member type-class-name seen)
                 (bug "Duplicated type-class: ~S" name))
               (push type-class-name seen)
               id)))
    (let ((clauses
            (append
             (when defaults
               `(;; Standard AND, NOT, OR combinators
                 (union-type
                  (any/type #'recurse ,thing (union-type-types type)))
                 (intersection-type
                  (every/type #'recurse ,thing (intersection-type-types type)))
                 (negation-type
                  (multiple-value-bind (result certain)
                      (recurse ,thing (negation-type-type type))
                    (if certain
                        (values (not result) t)
                        (values nil nil))))
                 ;; CONS is basically an AND type and can be handled generically here.
                 ;; This is correct in the cross-compiler so long as there are no atoms
                 ;; in the host that represent target conses or vice-versa.
                 (cons-type
                  (if (atom ,thing)
                      (values nil t)
                      (multiple-value-bind (result certain)
                          (recurse (car ,thing) (cons-type-car-type type))
                        (if result
                            (recurse (cdr ,thing) (cons-type-cdr-type type))
                            (values nil certain)))))))
             more-clauses)))
      `(named-let recurse ((,thing ,thing) (type type))
         (flet ((test-keywordp ()
                  ;; answer with certainty sometimes
                  (cond ((or (not (symbolp ,thing))
                             (let ((pkg (sb-xc:symbol-package ,thing)))
                               (or (eq pkg *cl-package*)
                                   ;; The user can't re-home our symbols in KEYWORD.
                                   (and pkg (system-package-p pkg)))))
                         (values nil t)) ; certainly no
                        ((eq (sb-xc:symbol-package ,thing) *keyword-package*)
                         (values t t)) ; certainly yes
                        (t
                         (values nil nil)))) ; can't decide
                (test-character-type (type)
                  (when (characterp ,thing)
                    (let ((code (char-code ,thing)))
                      (dolist (pair (character-set-type-pairs type) nil)
                        (destructuring-bind (low . high) pair
                          (when (<= low code high)
                            (return t))))))))
           ;; It should always work to dispatch by class-id, but ALIEN-TYPE-TYPE
           ;; is a problem in the cross-compiler due to not having a type-class-id
           ;; when 'src/code/cross-type' is compiled. I briefly tried moving
           ;; it later, but then type-init failed to compile.
           #+sb-xc-host
           (etypecase type ,@clauses)
           #-sb-xc-host
           (case (truly-the (mod ,(length *type-classes*)) (type-class-id type))
             ,@(let ((clauses (mapcar #'convert-clause clauses)))
                 (let ((absent (loop for class across *type-classes*
                                     unless (or (member (type-class-name class)
                                                        '(values constant))
                                                (member (type-class-name class) seen))
                                     collect class)))
                   (when absent
                     (error "Unhandled type-classes: ~S" absent)))
                 clauses)))))))

;;; Common logic for %%TYPEP and CROSS-TYPEP to test numeric types
(defmacro number-typep (object type)
  `(let ((object ,object) (type ,type))
     (and (numberp object)
          (let ((num (if (complexp object) (realpart object) object)))
            (ecase (numeric-type-class type)
              (integer (and (integerp num)
                            ;; If the type is (COMPLEX INTEGER), it can
                            ;; only match the object if both real and imag
                            ;; parts are integers.
                            (or (not (complexp object))
                                (integerp (imagpart object)))))
              (rational (rationalp num))
              (float
               (ecase (numeric-type-format type)
                 ;; (short-float (typep num 'short-float))
                 (single-float (typep num 'single-float))
                 (double-float (typep num 'double-float))
                 ;; (long-float (typep num 'long-float))
                 ((nil) (floatp num))))
              ((nil) t)))
          (flet ((bound-test (val)
                   (and (let ((low (numeric-type-low type)))
                          (cond ((null low) t)
                                ((listp low) (sb-xc:> val (car low)))
                                (t (sb-xc:>= val low))))
                        (let ((high (numeric-type-high type)))
                          (cond ((null high) t)
                                ((listp high) (sb-xc:< val (car high)))
                                (t (sb-xc:<= val high)))))))
            (ecase (numeric-type-complexp type)
              ((nil) t)
              (:complex
               (and (complexp object)
                    (bound-test (realpart object))
                    (bound-test (imagpart object))))
              (:real
               (and (not (complexp object))
                    (bound-test object))))))))
