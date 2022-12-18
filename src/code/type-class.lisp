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

(in-package "SB-ALIEN")

(defstruct (alien-type
             (:copier nil)
             (:constructor make-alien-type
                           (&key hash bits alignment
                            &aux (alignment
                                  (or alignment (guess-alignment bits))))))
  ;; HASH is a derived from the contents, not just a pseudo-random number.
  ;; The highest 5 bits of it imply the alien-type-class.
  ;; (There are curretly 16 type-classes with room for expansion)
  ;; These slots should be read-only, but alas they get modified by
  ;; the parsers for ENUM and RECORD types.
  ;; Maybe the sign bit could be used to indicate hash-consed versus non-hash-consed
  ;; and so we can know whether it is a safe operation to mutate the thing?
  (hash 0 :type (and sb-xc:fixnum unsigned-byte))
  ;; It's quasi-bogus that these can be NULL - it occurs when and only when parsing
  ;; a structure type that involves self-recursion I think. The :type option is inadequate
  ;; to enforce that atoms like {SINGLE,DOUBLE-}FLOAT always have an integer here.
  (bits nil :type (or null unsigned-byte))
  (alignment nil :type (or null unsigned-byte)))
(!set-load-form-method alien-type (:xc :target))

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
       (defvar *type-classes* (make-array 32 :initial-element nil))
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

(defun !type-class-or-lose (name)
  ;; Careful about NIL elements since they aren't populated strictly in order
  (or (find-if (lambda (x) (and x (eq (type-class-name x) name)))
               *type-classes*)
      (error "~S is not a defined type class." name)))

#-sb-xc-host
(progn
;; Return a number that increments by 1 for each word-pair allocation,
;; barring complications such as exhaustion of the current page.
;; The result is guaranteed to be a positive fixnum.
(declaim (inline address-based-counter-val quasi-random-address-based-hash))
(defun address-based-counter-val ()
  (let ((word
         ;; threads imply gencgc. use the per-thread alloc region pointer
         #+sb-thread
         (sap-int (sb-vm::current-thread-offset-sap sb-vm::thread-mixed-tlab-slot))
         ;; dynamic-space-free-pointer increments only when a page is full.
         ;; Using mixed_region directly is finer-grained.
         #+(and (not sb-thread) gencgc)
         (sb-sys:sap-ref-word (sb-sys:int-sap sb-vm::mixed-region) 0)))
    ;; counter should increase by 1 for each cons cell allocated
    (ash word (- (1+ sb-vm:word-shift)))))
;;; Return some bits that are dependent on the next address that will be
;;; allocated, mixed with the previous state (in case addresses get recycled).
;;; This algorithm, used for stuffing a hash-code into instances of CTYPE
;;; subtypes and generic functions, is simpler than RANDOM.
;;; I don't know whether it is more random or less random than a PRNG,
;;; but it's faster.
(defun quasi-random-address-based-hash (state mask)
  (declare (type (simple-array (and fixnum unsigned-byte) (1)) state))
  ;; Ok with multiple threads - No harm, no foul.
  (logand (setf (aref state 0) (mix (address-based-counter-val) (aref state 0)))
          mask)))

(defun ctype-random ()
  #+sb-xc-host
  (setq *ctype-lcg-state*
             (logand #x8fffff (+ (* 1103515245 *ctype-lcg-state*) 12345)))
  #-sb-xc-host
  (quasi-random-address-based-hash *ctype-hash-state* #xfffffff))

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
  ;; bits 22..26: more bits of hash
  ;; bits 27..31: 5 bits for type-class index
  ;; We'll never return the upper 5 bits from a hash mixer, so it's fine
  ;; that this uses all 32 bits for a 32-bit word.
  ;; No more than 32 bits are used, even for 64-bit words.
  ;; But it's consistent for genesis to treat it always as a raw slot.
  (%bits (missing-arg) :type sb-vm:word :read-only t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant ctype-hash-nbits 27))
;;; take 27 low bits but exclude bits 20 and 21
;;; [Our MASK-FIELD can't be folded, and I didn't feel like fixing that.]
(defconstant +type-hash-mask+
  #.(cl:logandc2 (cl:ldb (cl:byte 27 0) -1) (cl:mask-field (cl:byte 2 20) -1)))

(defmacro type-class-id (ctype) `(ldb (byte 5 ,ctype-hash-nbits) (type-%bits ,ctype)))
(defmacro type-id->type-class (id) `(truly-the type-class (aref *type-classes* ,id)))
(defmacro type-class (ctype) `(type-id->type-class (type-class-id ,ctype)))

(declaim (inline type-hash-value))
(defun type-hash-value (ctype) (ldb (byte ctype-hash-nbits 0) (type-%bits ctype)))

;;; TODO: remove the "interned" bit. No two internal representations can exist
;;; for the same structures that are EQUALP. Except for ALIEN-TYPE-TYPE (FIXME)
(defmacro type-bits-internedp (bits) `(logbitp 20 ,bits))
(defmacro type-bits-admit-type=-optimization (bits) `(logbitp 21 ,bits))

(defvar *ctype-hashsets* nil)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +type-internedp+ (ash 1 20))
  (defconstant +type-admits-type=-optimization+ (ash 1 21))
  (defun ctype-class-bits (type-class)
    (logior (ash (type-class-name->id type-class) ctype-hash-nbits)
            ;; NUMBER, MEMBER, and CLASSOID admit TYPE= optimization.
            ;; Other type classes might, but this is the conservative assumption.
            (if (member type-class '(number member classoid))
                +type-admits-type=-optimization+ 0)
            ;; The mapping from name to a CLASSOID type is unique,
            ;; therefore all CLASSOIDs have the "interned" bit on.
            (if (eq type-class 'classoid) +type-internedp+ 0)))
  (defvar *type-class-list*
    ;; type-class and ctype instance types in that class
    ;; The instance types MUST be list in descending order of DEPTHOID.
    ;; See CTYPE->HASHSET-NAME for the rationale for this constraint.
    '((named         named-type)
      (classoid      classoid)
      (values        values-type)
      (function      fun-designator-type fun-type)
      (constant      constant-type)
      (hairy         unknown-type hairy-type)
      (intersection  intersection-type)
      (union         union-type)
      (negation      negation-type)
      (number        numeric-type)
      (array         array-type)
      (character-set character-set-type)
      (member        member-type)
      (cons          cons-type)
      #+sb-simd-pack
      (simd-pack     simd-pack-type)
      #+sb-simd-pack-256
      (simd-pack-256 simd-pack-256-type)
      ;; clearly alien-type-type is not consistent with the (FOO FOO-TYPE) theme
      (alien         alien-type-type)))
  (defun ctype-instance->type-class (name)
    (car (the (not null)
              (find name *type-class-list* :key #'cdr :test #'member)))))
(eval-when (#+sb-xc-host :compile-toplevel :load-toplevel :execute)
  (defun type-class-name->id (name)
    (or #+sb-xc-host (position name *type-class-list* :key #'car)
        #-sb-xc-host (position name *type-classes* :key #'type-class-name)
        (error "~S is not a defined type class." name))))

;;; For system build-time only
(defun pack-interned-ctype-bits (type-class &optional hash)
  (let ((hash (or hash (ctype-random))))
    (logior (ash (type-class-name->id type-class) ctype-hash-nbits)
            (logand hash +type-hash-mask+)
            ;; type= optimization is valid if not an array-type
            (if (eq type-class 'array) 0 +type-admits-type=-optimization+)
            +type-internedp+)))

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
              ((:complex-subtypep-arg1 :negate :singleton-p)
               `((,first (,operator ,arg-type ,first))))
              ((:unparse)
               `((,second (,operator ,arg-type ,second))))
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
       (defun ,name ,lambda-list
         ,@(if (eq method :unparse) `((declare (ignorable ,(first lambda-list)))))
         ,@(if rebind `((let ,rebind ,@body)) body))
       (!cold-init-forms
        ,@(mapcar (lambda (method)
                    `(setf (,(type-class-fun-slot method)
                            (svref *type-classes* ,(type-class-name->id class)))
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
       (let ((index ,(type-class-name->id name)))
         (unless (aref *type-classes* index)
           (setf (aref *type-classes* index) ,make-it)))
       ;; I have no idea what compiler bug could be worked around by adding a form here,
       ;; but this certainly achieves something, somehow.
       #+host-quirks-cmu (print (aref *type-classes* (1- (length *type-classes*)))))

    #+sb-xc
    `(!cold-init-forms (setf (svref *type-classes* ,(type-class-name->id name))
                             ,make-it))))

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
(defmacro invoke-type-method (simple complex-arg2 type1 type2 &key
                                      (default '(values nil t))
                                        ; assume complex fn is symmetric
                                        ; unless told otherwise.
                                      (complex-arg1 complex-arg2 complex-arg1-p))
  (declare (type keyword simple complex-arg1 complex-arg2))
   `(let* ((.L ,type1) (id1 (type-class-id .L))
           (.R ,type2) (id2 (type-class-id .R)))
      (if (= id1 id2)
          (funcall (,(type-class-fun-slot simple) (type-id->type-class id1)) .L .R)
          (acond ((,(type-class-fun-slot complex-arg2) (type-id->type-class id2))
                  (funcall it .L .R))
                 ((,(type-class-fun-slot complex-arg1) (type-id->type-class id1))
                   ;; if COMPLEX-ARG1 method was provided, the method accepts
                   ;; the arguments exactly as given. Otherwise, flip them.
                  (funcall it ,@(if complex-arg1-p `(.L .R) `(.R .L))))
                 (t ,default)))))

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
(defmacro literal-ctype (constructor &optional specifier)
  (declare (ignore specifier))
  `(load-time-value ,constructor))

;; Omitting the specifier works only if the unparser method has been
;; defined in time to use it, and you're sure that constructor's result
;; can be unparsed - some unparsers may be confused if called on a
;; non-canonical object, such as an instance of (CONS T T) that is
;; not EQ to the interned instance.
#-sb-xc-host
(defmacro literal-ctype (constructor &optional (specifier nil specifier-p))
  (if specifier-p (specifier-type specifier) (symbol-value constructor)))

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

(declaim (inline hash-ctype-list))
(declaim (ftype (function (list) (signed-byte #.sb-vm:n-fixnum-bits))
                hash-ctype-list))
(defun hash-ctype-list (types)
  (loop with res of-type (signed-byte #.sb-vm:n-fixnum-bits) = 0
        for type in types
        do (setq res (logxor (ash res -1) (type-hash-value type)))
        finally (return res)))

;;;; representations of types

(defun hash-ctype-set (types) ; ctype list hashed order-insensitively
  (let ((hash (type-hash-value (car types)))
        (n 1))
    (dolist (type (cdr types) (mix (the (integer 2) n) hash))
      (incf n)
      (setq hash (logxor (type-hash-value type) hash)))))
(defun ctype-set= (a b)
  ;; We could order ctype sets deterministically by the TYPE-HASH-VALUE of
  ;; the items, but there would also have to be a provision made for hash
  ;; collisions, which entails the comparator doing one thing or another
  ;; depending on whether the set had collisions.
  ;; However, it might also be nice to canonicalize putting any type containing
  ;; SATISFIES to the right of any type that does not. This order would tend to have
  ;; a beneficial effect of making TYPEP tests pass or fail "sooner" without calling
  ;; a random predicate. It's just as well to compare sets elementwise for now
  ;; rather than sorting them by some complicated set of criteria.
  (and (= (length a) (length b)) (every (lambda (x) (memq x b)) a)))

(define-load-time-global *ctype-list-hashset*
  (make-hashset 32 #'list-elts-eq #'hash-ctype-list :weakness t :synchronized t))
(define-load-time-global *ctype-set-hashset*
  (make-hashset 32 #'ctype-set= #'hash-ctype-set :weakness t :synchronized t))

(defun intern-ctype-list (list)
  (when list
    (hashset-insert-if-absent *ctype-list-hashset* list #'ensure-heap-list)))
(defun intern-ctype-set (set)
  (aver set) ; Sets of ctypes (as used by COMPOUND-TYPE) are nonempty lists
  (hashset-insert-if-absent *ctype-set-hashset* set #'ensure-heap-list))

;;; DEF-TYPE-MODEL is like DEFSTRUCT, with the following differences:
;;;  1. it inserts (:INCLUDE CTYPE) unless otherwise expressed
;;;  2. it inserts (:COPIER NIL)
;;;  3. it adds :READ-ONLY T to all slots
;;;  4. it has slot options to help with hash-consing
(defmacro def-type-model ((name &rest options) &rest direct-slots)
  ;; :CONSTRUCTOR* reminds you that it's not a direct translation to defstruct.
  (aver (<= (count :constructor* options :key #'car) 1))
  ;; The private constructor is always positional.
  ;; (:CONSTRUCTOR* NAME (arg1 arg2)) has a private constructor
  ;;   and a public constructor. The latter will eventually
  ;;   become a caching constructor when that change is completed.
  ;; (:CONSTRUCTOR* NIL (arg1 arg2)) specifies the argument order
  ;;   but asks for no automatically-defined public constructor.
  ;;   You might hand-define one which takes &KEY args if desired.
  (let* ((public-ctor (assoc :constructor* options))
         (public-ctor-args (third public-ctor))
         (private-ctor (unless (member name '(compound-type args-type))
                          (symbolicate "!ALLOC-" name)))
         (private-ctor-args (cons '%bits public-ctor-args))
         (conc-name (symbolicate name "-"))
         (include (awhen (assoc :include options) (cadr it)))
         ;; list of triples #(SLOT-NAME HASHER COMPARATOR) of direct slots only.
         ;; Inherited slots are mixed in by calling the supertype's hash function.
         (hashed-slots
          (mapcan (lambda (slot &aux (type (getf (cddr slot) :type))
                                     (hasher (getf (cddr slot) :hasher 'clipped-sxhash))
                                     (comparator (getf (cddr slot) :test)))
                    (cond ((or comparator (null hasher))) ; ok
                          ((eq type 'ctype)
                           (setq comparator 'eq hasher 'type-hash-value))
                          ((or (and (typep type '(cons (eql member)))
                                    (every #'symbolp (cdr type)))
                               (eq type 'boolean))
                           (setq comparator 'eq))
                          (t (bug "Underspecified slot: ~S.~S type ~S~%"
                                  name (car slot) type)))
                    (when hasher (list (vector (car slot) hasher comparator))))
                  direct-slots)))
    `(progn
       ,@(when private-ctor `((declaim (inline ,private-ctor))))
       (defstruct (,name ,@(unless include '((:include ctype)))
                         ,@(remove-if (lambda (x)
                                        (member (car x) '(:constructor* :extra-mix-step)))
                                      options)
                         ,(if private-ctor
                              `(:constructor ,private-ctor ,private-ctor-args)
                              '(:constructor nil))
                         (:copier nil))
         ,@(mapcar (lambda (slot &aux (copy (copy-list slot)))
                     (remf copy :hasher)
                     (remf copy :test)
                     (append copy '(:read-only t)))
                   direct-slots))
       ;; Always define hash-consing functions if any new slots, even if abstract.
       ,@(when direct-slots
           (let ((hashfn (symbolicate "CALC-" name "-HASH"))
                 (compare (symbolicate name "-EQUIV")))
             `((defun ,hashfn (x)
                 #-sb-xc-host (declare (optimize (safety 0)))
                 (,(if (assoc :extra-mix-step options) 'type-hash-final-mix 'progn)
                  (type-hash-mix
                   ,@(when include `((,(symbolicate "CALC-" include "-HASH") x)))
                   ,@(mapcar (lambda (slot &aux (reader
                                                 (symbolicate conc-name (svref slot 0))))
                               `(,(svref slot 1) (,reader x)))
                             hashed-slots))))
               (defun ,compare (a b)
                 #-sb-xc-host (declare (optimize (safety 0)))
                 (and ,@(mapcar (lambda (slot &aux (reader
                                                    (symbolicate conc-name (svref slot 0))))
                                  `(,(elt slot 2) (,reader a) (,reader b)))
                                hashed-slots)
                      ,@(when include `((,(symbolicate include "-EQUIV") a b))))))))
       ;; Define a hashset unless this is an abstract type
       ,@(unless (member name '(compound-type args-type))
           (let* ((stem (if direct-slots name include))
                  (hashfn (symbolicate "CALC-" stem "-HASH"))
                  (test (symbolicate stem "-EQUIV"))
                  (hashset (symbolicate "*" name "-HASHSET*")))
             `((pushnew ',hashset *ctype-hashsets*)
               (define-load-time-global ,hashset
                   (make-hashset 32 #',test #',hashfn :synchronized t :weakness t)))))
       ;; If the internal constructor is wrapped in a hand-written constructor, then
       ;; that other constructor invokes the cachine lookup macro. Don't do it here.
       ;; See e.g. MAKE-CONS-TYPE which picks off 2 cases and then uses the cache.
       ,@(when (second public-ctor)
           `((declaim (ftype (sfunction * ,name) ,(second public-ctor)))
             (defun ,(second public-ctor) ,public-ctor-args
               (new-ctype ,name ,@(cdr private-ctor-args))))))))

;;; The "clipped hash" is just some stable hash that may rely on the host's SXHASH
;;; but always ensuring that the result is an unsigned fixnum for the target,
;;; so that we can call our MIX on the value. It's needlessly tedious to fully
;;; replicate our SXHASH on BIGNUM and RATIO which can appear in numeric bounds.
#+sb-xc-host
(defun clipped-sxhash (x)
  (typecase x
    (rational ; numeric-type high,low bound; array dimensions, etc
     ;; All integers can fall through to the host because it would be silly to restrict
     ;; this case to exactly (OR (AND INTEGER (NOT SB-XC:FIXNUM)) RATIO).
     (logand (cl:sxhash x) sb-xc:most-positive-fixnum))
    (cons
     (if (eq (car x) 'satisfies)
         (sb-xc:sxhash (cadr x)) ; it's good enough
         (error "please no: ~S" x)))
    (t
     (sb-xc:sxhash x)))) ; FLOAT representation as struct, or SYMBOL
#-sb-xc-host (defmacro clipped-sxhash (x) `(sxhash ,x))

(defmacro type-hash-mix (&rest args) (reduce (lambda (a b) `(mix ,a ,b)) args))
;;; The final mix ensures that all bits affect the masked hash.
;;; Since it takes non-zero time, only do it for NUMERIC where it seems to make
;;; a large difference in the maximum probe sequence
(defmacro type-hash-final-mix (val)
  `(ldb (byte ctype-hash-nbits 0) (sb-impl::murmur3-fmix-word ,val)))

;; Singleton MEMBER types are best dealt with via a weak-value hash-table because:
;; * (MEMBER THING) might lack an address-insensitive hash for THING
;;   but src/code/hashset can not use address-based hashes. This limitation is unique
;;   to MEMBER types because other CTYPE instances are compositions of CTYPES
;;   where all subparts have assigned hashes.
;; * Symbols have slightly bad SXHASH values (by language requirement):
;;    "For any two objects, x and y which are symbols and which are similar
;;    (sxhash x) and (sxhash y) yield the same mathematical value even if x and y exist
;;    in different Lisp images of the same implementation."
;;   This seems to imply that pseudorandom hashes are disallowed for symbols,
;;   and that any two gensyms spelled the same hash the same.
;;   Consequently, a thousand occurrences of (MEMBER #:DUMMY) for different gensyms,
;;   will cause the hashset to exceed its probe sequence length limit.
;;   This isn't to say we couldn't assign some bits of SYMBOL-HASH pseudorandomly,
;;   and mask them out in the value returned by CL:SXHASH.
;;
;; Also: XSETs containing arbitrary objects such as strings and lists don't have
;; a good hash at all. There is not really a way to compute a mixed hash
;; other than by pinning all objects in the XSET and taking their addresses.
;; Then we'd need to figure out that GC happened, and it becomes a pain.
;; This seems more complicated than the situation warrants.
;; So we'll just give up on hash-consing which should fix lp#1999687
;;
;; An outline of a better design would be as follows:
;; - create a global hash-table of objects which were placed in an XSET
;;   other than the nicely hashable object types. Call this xset-key->hash-mapping.
;;   This mapping does not need to be weak, because it will have a way of purging it.
;;   Each value in the table is a cons of a random hash and the number of XSETs
;;   using the key. Increment the refcount when making a new XSET with that key.
;; - when hashing the XSET, look up its keys (other than EQL hashable) in the global table
;; - attach a finalizer to the XSET. The finalizer's job is to decrement the refcount
;;   on each key in the global mappping. When the count reaches zero there are no
;;   XSETs that refer to the key, and the random hash can be removed.
;; Why make a refcounted table? Because otherwise there is no way to remove mapping
;; entries for objects that outlive the MEMBER-TYPE's use of the object but where the
;; MEMBER type itself is dead. Worst-case, every object in the lisp image could at some
;; point appear in a MEMBER type but then never be needed again with regard to type
;; system operations. So you'd have created a permanent mapping of every object to
;; a random hash for no good reason.

;; Why the singleton table is so important is that any time the compiler asks itself
;; the ctype-of a constant leaf, it might yield `(MEMBER ,the-constant).
;; So then you end up with an assortment of random objects that don't hash
;; nicely in a ctype hashset, but are OK in a hash-table.
(define-load-time-global *eql-type-cache* ; like EQL-SPECIALIZER-TABLE in PCL
    (sb-impl::make-system-hash-table :test 'eql :weakness :value :synchronized nil))

(defmacro safe-member-type-elt-p (obj)
  `(or (not (sb-vm:is-lisp-pointer (get-lisp-obj-address ,obj)))
       (heap-allocated-p ,obj)))

(defvar *hashsets-preloaded* nil)
(defmacro new-ctype (pseudonym &rest initargs)
  (let* ((name (if (eq pseudonym 'eql) 'member-type pseudonym))
         (allocator (package-symbolicate "SB-KERNEL" "!ALLOC-" name))
         (hashset (package-symbolicate "SB-KERNEL" "*" name "-HASHSET*"))
         (bits (ctype-class-bits (ctype-instance->type-class name))))
    #+sb-xc-host ; allocate permanent data, and insert into cache if not found
    `(let ((temp (,allocator (logior (logand (ctype-random) +type-hash-mask+) ,bits)
                             ,@initargs)))
       (hashset-insert-if-absent ,hashset temp #'identity))
    ;; allocate temporary key, copy it if and only if not found.
    ;; COPY-CTYPE can copy subparts like the numeric bound if arena-allocated
    #-sb-xc-host
    `(let ((temp (,allocator ,bits ,@initargs)))
       ;; Too many "can't stack-allocate" warnings for most
       #+c-stack-is-control-stack (declare (truly-dynamic-extent temp))
       #+nil ; or #+sb-devel as you see fit
       (unless *hashsets-preloaded*
         (write-string "CTYPE hashset preload failure")
         (sb-vm:ldb-monitor))
       ,(case pseudonym
          (eql ; as per above remarks: use hash-table, not hashset
            `(let* ((xset ,(first initargs))
                    (zeros ,(second initargs))
                    (key (first (or zeros (xset-data xset))))
                    (table *eql-type-cache*))
               (with-system-mutex ((hash-table-lock table))
                ;; This is like ENSURE-GETHASH but it potentially copies the key
                (or (gethash key table)
                    ;; hope no off-heap pointers buried within KEY
                    (let ((key (cond ((numberp key) (sb-vm:copy-number-to-heap key))
                                     ((safe-member-type-elt-p key) key)
                                     (t
                                      (warn "Off-heap hash-table key @ ~X"
                                            (get-lisp-obj-address key))
                                      key))))
                      (setf (gethash key table) (copy-ctype temp)))))))
          (member-type ; problem case: don't always know how to hash well
           `(let ((xset ,(first initargs)))
              (flet ((hashable (x) (typep x '(or symbol number character instance))))
                (if (xset-every #'hashable xset)
                    (hashset-insert-if-absent ,hashset temp #'copy-ctype)
                    ;; otherwise just copy it always (for now)
                    (copy-ctype temp)))))
          (t
            `(hashset-insert-if-absent ,hashset temp #'copy-ctype))))))

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

;;; A HAIRY-TYPE represents a SATISFIES type or UNKNOWN type.
;;; FIXME: those should be two distinct things (in HAIRY type-class)
;;; so that we don't have to examine the sexpr repeatedly to decide its form.
;;; And as a further improvement, we might want a table that maps
;;; predicates to their exactly recognized type when possible.
;;; We have such a table in fact - *BACKEND-PREDICATE-TYPES*
;;; as a starting point. But something like PLUSP isn't in there.
;;; On the other hand, either of these points may not be sources of
;;; inefficiency, and the latter if implemented might have undesirable
;;; user-visible ramifications, though it seems unlikely.
(def-type-model (hairy-type (:constructor* %make-hairy-type (specifier)))
  ;; the Common Lisp type-specifier of the type we represent.
  ;; In UNKNOWN types this can only be a symbol.
  ;; For other than an unknown type, this must be a (SATISFIES f) expression.
  ;; The reason we can't constrain this to
  ;;    (OR SYMBOL (CONS (EQL SATISFIES) (CONS SYMBOL NULL)))
  ;; is that apparently we'll store _illegal_ type specifiers in a hairy-type.
  ;; There's an example in the regression test named
  ;;  :single-warning-for-single-undefined-type
  (specifier nil :type t :test equal))

(macrolet ((hash-fp-zeros (x) ; order-insensitive
             `(let ((h 0))
                (dolist (x ,x h) (setq h (logxor (sb-xc:sxhash x) h)))))
           (fp-zeros= (a b)
             `(let ((a ,a) (b ,b))
                (and (= (length a) (length b))
                     (every (lambda (x) (member x b)) a)))))
;;; A MEMBER-TYPE represent a use of the MEMBER type specifier. We
;;; bother with this at this level because MEMBER types are fairly
;;; important and union and intersection are well defined.
(def-type-model (member-type (:constructor* nil (xset fp-zeroes)))
  (xset nil :type xset :hasher xset-elts-hash :test xset=)
  (fp-zeroes nil :type list :hasher hash-fp-zeros :test fp-zeros=)))

;;; An ARRAY-TYPE is used to represent any array type, including
;;; things such as SIMPLE-BASE-STRING.
(macrolet ((hash-dims (list)
;; We should not use our SXHASH on ARRAY-TYPE-DIMENSIONS because it cuts off at 5 items:
;; * (loop for i from 4 to 7 do (format t "~d ~x~%" i (sxhash (make-list i))))
;; 4 75FA4FC28C64CC
;; 5 75FA4A37B3E5CD
;; 6 75FA4A37B3E5CD
;; 7 75FA4A37B3E5CD
             `(if (eql ,list '*)
                  #x1980B71D ; = (ldb (byte 29 0) (sxhash '*)) not that it matters
                  (let ((h 0))
                    (dolist (dim ,list h)
                      (setq h (mix (sb-xc:sxhash dim) h)))))))
(def-type-model (array-type
                 (:extra-mix-step)
                 (:constructor* %make-array-type
                                (dimensions complexp element-type
                                            specialized-element-type)))
  ;; the dimensions of the array, or * if unspecified. If a dimension
  ;; is unspecified, it is *.
  (dimensions '* :type (or list (eql *)) :test equal :hasher hash-dims)
  ;; Is this not a simple array type? (:MAYBE means that we don't know.)
  (complexp :maybe :type (member t nil :maybe))
  ;; the element type as originally specified
  (element-type nil :type ctype)
  ;; the element type as it is specialized in this implementation
  ;; Strangely, this is *NOT* a pure function of ELEMENT-TYPE.
  ;;
  ;; The :unparse-safely test in 'type.pure' produces the following result:
  ;; (describe (type-intersection (specifier-type '(vector (or bit character)))
  ;;                              (specifier-type `(vector (or bit symbol)))))
  ;;
  ;; #<ARRAY-TYPE (VECTOR T)>
  ;;   [structure-object]
  ;; Slots with :INSTANCE allocation:
  ;;   %BITS                          = 1443812512
  ;;   DIMENSIONS                     = (*)
  ;;   COMPLEXP                       = :MAYBE
  ;;   ELEMENT-TYPE                   = #<NUMERIC-TYPE BIT>
  ;;   SPECIALIZED-ELEMENT-TYPE       = #<NAMED-TYPE T>
  ;;
  ;; Frankly I'm somewhat disinclined to believe this result
  ;; because intuitively the specialization is what you would get if you
  ;; asked the question "how would an array of <x> be specialized?"
  (specialized-element-type nil :type ctype)))

(macrolet ((hash-ranges (list)
             `(let ((h 0))
                (dolist (pair ,list h)
                  (setq h (mix (sb-xc:sxhash (cdr pair))
                               (mix h (sb-xc:sxhash (car pair)))))))))
(def-type-model (character-set-type (:constructor* nil (pairs)))
  ;; these get canonically ordered by the parser
  (pairs (missing-arg) :type list :test equal :hasher hash-ranges)))

;;; A COMPOUND-TYPE is a type defined out of a set of types, the
;;; common parent of UNION-TYPE and INTERSECTION-TYPE.
(def-type-model (compound-type) ; no direct instances
  ;; Formerly defined in every CTYPE, but now just in the ones
  ;; for which enumerability is variable.
  ;; This is a pure function of TYPES and need not be part of the hash
  (enumerable nil :type boolean :hasher nil)
  ;; This list must have at least 2 items in it.
  ;; A singleton would not be a compound type.
  ;; An empty OR is the type NIL, and an empty AND is type T.
  (types nil :type (cons t cons) :hasher hash-ctype-set :test eq)) ; list is hash-consed

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
;;; ARRAY of {CHARACTER,BASE-CHAR} in that exact order always.
;;; It will therefore admit quick TYPE=, but not quick failure, since
;;;   (type= (specifier-type '(or (simple-array (member #\a) (*))
;;;                               (simple-array character (*))))
;;;          (specifier-type 'simple-string)) => T and T
;;; even though (MEMBER #\A) is not TYPE= to BASE-CHAR.
;;;
(def-type-model (union-type
                 (:constructor* nil (enumerable types))
                 (:include compound-type)))

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
(def-type-model (intersection-type
                 (:constructor* nil (enumerable types))
                 (:include compound-type)))

(def-type-model (alien-type-type (:constructor* %make-alien-type-type (alien-type)))
  (alien-type nil :type alien-type :hasher sb-alien::alien-type-hash :test eq))

(def-type-model (negation-type (:constructor* make-negation-type (type)))
  (type (missing-arg) :type ctype))

;;; An UNKNOWN-TYPE is a type not known to the type system (not yet
;;; defined). We make this distinction since we don't want to complain
;;; about types that are hairy but defined.
(def-type-model (unknown-type (:constructor* make-unknown-type (specifier))
                              (:include hairy-type)))

;;; a list of all the float "formats" (i.e. internal representations;
;;; nothing to do with #'FORMAT), in order of decreasing precision
(defglobal *float-formats*
    '(long-float double-float single-float short-float))

;;; The type of a float format.
(deftype float-format () `(member ,@*float-formats*))

;;; Using 3 separate fields to represent information with fewer than 4 bits
;;; of entropy is a terribly wasteful representation choice.
;;; (4 bits = 16 possibilities but in fact there are only 9 valid combinations)
(defstruct (numtype-aspects
             (:constructor !make-numeric-aspects (complexp class precision id))
             (:predicate nil)
             (:copier nil))
  ;; Is this a complex numeric type?  Null if unknown (only in NUMBER).
  (complexp :real :type (member :real :complex nil))
  ;; the kind of numeric type we have, or NIL if the type is NUMBER.
  ;; The types corresponding to all of REAL or all of COMPLEX are UNION types,
  ;; and no constituent type thereof will have a NIL here.
  (class nil :type (member integer rational float nil))
  ;; "precision" for a float type (i.e. type specifier for a CPU
  ;; representation of floating point, e.g. 'SINGLE-FLOAT).
  ;; NIL if and only if CLASS is not FLOAT
  (precision nil :type (member single-float double-float nil))
  ;; a value that uniquely identifies this triple of <complexp,class,precision>
  (id 0 :type (unsigned-byte 8)))

;;; There legal combinations of (COMPLEXP CLASS PRECISION) are as follows:
;;; 0. (NIL NIL NIL)
;;; 1. (:REAL FLOAT SINGLE-FLOAT) and 2. (:REAL FLOAT DOUBLE-FLOAT)
;;; 3. (:COMPLEX FLOAT SINGLE-FLOAT) and 4. (:COMPLEX FLOAT DOUBLE-FLOAT)
;;; 5. (:REAL INTEGER) 6. (:COMPLEX INTEGER)
;;; 7. (:REAL RATIONAL) 8. (:COMPLEX RATIONAL)
;;; any other combination that would attempt to carve out a subset
;;; of the numeric type space will instead be a UNION type.
(declaim (inline !compute-numtype-aspect-id))
(defun !compute-numtype-aspect-id (complexp class precision)
  (declare (type (member :real :complex nil) complexp)
           (type (member integer rational float nil) class)
           (type (member single-float double-float nil) precision))
  (unless (eq class 'float) (aver (not precision)))
  (case class
    (float (+ (if (eq complexp :real) 1 3)
              (if (eq precision 'single-float) 0 1)))
    (integer (if (eq complexp :real) 5 6))
    (rational (if (eq complexp :real) 7 8))
    (t (aver (not class))
       (aver (not complexp))
       0)))
(declaim (notinline !compute-numtype-aspect-id))

;;; force the SBCL-default initial value, because genesis also 0-fills it
(defglobal *numeric-aspects-v* (make-array 9 :initial-element 0))
(declaim (type (simple-vector 9) *numeric-aspects-v*))
(loop for (complexp class precision)
      in '((nil nil nil)
           (:real float single-float) (:real float double-float)
           (:complex float single-float) (:complex float double-float)
           (:real integer nil) (:complex integer nil)
           (:real rational nil) (:complex rational nil))
      do (let ((index (!compute-numtype-aspect-id complexp class precision)))
           (when (eql (aref *numeric-aspects-v* index) 0)
             (setf (aref *numeric-aspects-v* index)
                   (!make-numeric-aspects complexp class precision index)))))

(defmacro get-numtype-aspects (&rest rest)
  `(the (not null)
        (aref *numeric-aspects-v* (!compute-numtype-aspect-id ,@rest))))

(macrolet ((numbound-hash (b)
             ;; It doesn't matter what the hash of a number is, as long as it's stable.
             ;; Use the host's SXHASH for convenience.
             ;; We aren't obliged to fully emulate own behavior on numbers,
             ;; but we can't trust the host to do the right thing on our proxy floats.
             `(let ((x ,b))
                (block nil
                  (multiple-value-bind (h v)
                      (if (listp x)
                          (if x (values #x55AA55 (car x)) (return 0))
                          (values 0 x))
                    (logxor h (#+sb-xc-host clipped-sxhash
                               #-sb-xc-host sb-impl::number-sxhash v))))))
           (numbound-eql (a b)
             ;; Determine whether the 'low' and 'high' slots of two NUMERIC-TYPE instances
             ;; are "the same". It is a stricter test than in the SIMPLE-= method, because
             ;; the cache preserves distinctions that the type algebra does not,
             ;; specifically in regard to signed zeros.
             `(let ((a ,a) (b ,b))
                (if (listp a)
                    (and (listp b) (eql (car a) (car b)))
                    (eql a b)))))
;;; A NUMERIC-TYPE represents any numeric type, including things
;;; such as FIXNUM.
(def-type-model (numeric-type
                 (:extra-mix-step)
                 (:constructor* nil (aspects low high)))
  (aspects (missing-arg) :type numtype-aspects :hasher numtype-aspects-id :test eq)
  (low nil :type (or real (cons real null) null)
       :hasher numbound-hash :test numbound-eql)
  (high nil :type (or real (cons real null) null)
        :hasher numbound-hash :test numbound-eql)))
(declaim (inline numeric-type-complexp numeric-type-class numeric-type-format))
(defun numeric-type-complexp (x) (numtype-aspects-complexp (numeric-type-aspects x)))
(defun numeric-type-class (x) (numtype-aspects-class (numeric-type-aspects x)))
(defun numeric-type-format (x) (numtype-aspects-precision (numeric-type-aspects x)))

;;; A CONS-TYPE is used to represent a CONS type.
(def-type-model (cons-type (:constructor* nil (car-type cdr-type)))
  ;; the CAR and CDR element types (to support ANSI (CONS FOO BAR) types)
  (car-type (missing-arg) :type ctype)
  (cdr-type (missing-arg) :type ctype))

;;; ARGS-TYPE objects are used both to represent VALUES types and
;;; to represent FUNCTION types.
;;; This used to contain slots for KEYP,KEYWORDS,ALLOWP which could never
;;; be useful in a VALUES-TYPE.
;;; CMUCL rev 2e8488e0ace2d21a3d7af217037bcb445cc93496 said
;;; "(values) <type translator>: Disallow &key and &allow-other-keys"
;;; but they kept all the slots in ARGS-TYPE.
(macrolet ((hash-ctype-or-null (x)
             `(let ((x ,x)) (if x (type-hash-value x) 0))))
(def-type-model (args-type (:constructor* nil (required optional rest)))
  ;; Lists of the type for each required and optional argument.
  (required nil :type list :hasher hash-ctype-list :test eq) ; hash-consed list
  (optional nil :type list :hasher hash-ctype-list :test eq) ; hash-consed list
  ;; The type for the rest arg. NIL if there is no &REST arg.
  (rest nil :type (or ctype null) :hasher hash-ctype-or-null :test eq)))

;;; the description of a &KEY argument
(declaim (inline !make-key-info))
(defstruct (key-info #-sb-xc-host (:pure t)
                     (:constructor !make-key-info (name type))
                     (:copier nil))
  ;; the key (not necessarily a keyword in ANSI Common Lisp)
  (name (missing-arg) :type symbol :read-only t)
  ;; the type of the argument value
  (type (missing-arg) :type ctype :read-only t))
(declaim (freeze-type key-info))

(defun key-info= (a b)
  (declare (optimize (safety 0)))
  (and (eq (key-info-name a) (key-info-name b))
       ;; This will work better once I hash-cons all ctypes
       (eq (key-info-type a) (key-info-type b))))
(defun key-info-hash (x)
  (declare (optimize (safety 0)))
  (mix (#+sb-xc-host sb-impl::symbol-name-hash #-sb-xc-host sxhash (key-info-name x))
       (type-hash-value (key-info-type x))))

(defun key-info-list-hash (list)
  (declare (optimize (safety 0)))
  (let ((h 0))
  (dolist (elt list h) (setf h (mix (key-info-hash elt) h)))))

(define-load-time-global *key-info-hashset*
    (make-hashset 32 #'key-info= #'key-info-hash :weakness t :synchronized t))
(define-load-time-global *key-info-list-hashset*
    (make-hashset 32 #'list-elts-eq #'key-info-list-hash :weakness t :synchronized t))

(defun make-key-info (key type)
  (dx-let ((x (!make-key-info key type)))
     (hashset-insert-if-absent *key-info-hashset* x
      (named-lambda "MAKE-KEY-INFO" (x)
        (!make-key-info (key-info-name x) (key-info-type x))))))
(defun intern-key-infos (list)
  (when list
    (hashset-insert-if-absent *key-info-list-hashset* list #'identity)))

(def-type-model (values-type (:constructor* nil (required optional rest))
                             (:include args-type)))
(declaim (freeze-type values-type))

;;; (SPECIFIER-TYPE 'FUNCTION) and its subtypes
(def-type-model (fun-type
                 (:constructor* nil (required optional rest keyp keywords allowp
                                     wild-args returns))
                 (:include args-type))
  ;; true if &KEY arguments are specified
  (keyp nil :type boolean)
  ;; list of KEY-INFO structures describing the &KEY arguments
  (keywords nil :type list :hasher key-info-list-hash :test eq) ; hash-consed already
  ;; true if other &KEY arguments are allowed
  (allowp nil :type boolean)
  ;; true if the arguments are unrestrictive, i.e. *
  (wild-args nil :type boolean)
  ;; type describing the return values. This is a values type
  ;; when multiple values were specified for the return.
  (returns (missing-arg) :type ctype))

(declaim (inline args-type-keyp args-type-keywords args-type-allowp))
(defun args-type-keyp (type) (and (fun-type-p type) (fun-type-keyp type)))
(defun args-type-keywords (type) (and (fun-type-p type) (fun-type-keywords type)))
(defun args-type-allowp (type) (and (fun-type-p type) (fun-type-allowp type)))

(def-type-model (fun-designator-type
                 (:constructor* nil (required optional rest keyp keywords allowp
                                     wild-args returns))
                 (:include fun-type)))

;;; The CONSTANT-TYPE structure represents a use of the CONSTANT-ARG
;;; "type specifier", which is only meaningful in function argument
;;; type specifiers used within the compiler. (It represents something
;;; that the compiler knows to be a constant.)
(def-type-model (constant-type (:constructor* nil (type)))
  ;; The type which the argument must be a constant instance of for this type
  ;; specifier to win.
  (type (missing-arg) :type ctype))


;;; A SIMD-PACK-TYPE is used to represent a SIMD-PACK type.
#+sb-simd-pack
(def-type-model (simd-pack-type
                 (:constructor* %make-simd-pack-type (tag-mask)))
  (tag-mask (missing-arg) ; bitmask over possible simd-pack-tag values
   :test =
   :type (and (unsigned-byte #.(length +simd-pack-element-types+))
              (not (eql 0)))))

#+sb-simd-pack-256
(def-type-model (simd-pack-256-type
                 (:constructor* %make-simd-pack-256-type (tag-mask)))
  (tag-mask (missing-arg)
   :test =
   :type (and (unsigned-byte #.(length +simd-pack-element-types+))
              (not (eql 0)))))

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

;;; Return the name of the global hashset that OBJ (a CTYPE instance)
;;; would be stored in, if it were stored in one.
(defun ctype->hashset-sym (obj)
  (macrolet ((generate  ()
               (collect ((clauses))
                 (dolist (type-class *type-class-list*
                                     `(etypecase obj ,@(clauses)))
                   (dolist (instance-type (cdr type-class))
                     (clauses
                      (list instance-type
                            (unless (member instance-type '(classoid named-type))
                              `',(symbolicate "*" instance-type "-HASHSET*")))))))))
    (generate)))

(export 'show-ctype-ctor-cache-metrics)
;;; The minimum hashset storage size is 64 elements, so a bunch of the caches
;;; start out with too-low load-factor, being somewhat oversized.
;;; The EQL table never sizes down (because our hash-tables don't)
;;; so it may operate at a fairly low load factor.
;;; Other than that we should expect load factors between 50% and 75%.
;;; So it's extremely unexpected that List starts out with a load-factor of 12%.
;;; Probably should investigate, though it's harmless.
(defun show-ctype-ctor-cache-metrics ()
  (let (caches (total 0))
    (push (list "List" *ctype-list-hashset*) caches)
    (push (list "Set" *ctype-set-hashset*) caches)
    (push (list "Key-Info" *key-info-hashset*) caches)
    (push (list "Key-Info-List" *key-info-list-hashset*) caches)
    (push (list "EQL" *eql-type-cache*) caches)
    (dolist (symbol *ctype-hashsets*)
      (push (list (subseq (string symbol) 1
                          (- (length (string symbol)) (length "-TYPE-HASHSET*")))
                  (symbol-value symbol))
            caches))
    (flet ((tablecount (x)
             (if (hash-table-p x) (hash-table-count x) (sb-impl::hashset-count x))))
      (format t "~&ctype cache metrics:  Count     LF     Seek    Hit maxPSL  Mask~%")
      (dolist (cache (sort caches #'> ; decreasing cout
                           :key (lambda (x) (tablecount (second x)))))
        (binding*
            ((name (first cache))
             (table (second cache))
             (count (tablecount table))
             ((load seeks hit psl mask)
              (if (hash-table-p table)
                  (values #+sb-xc-host nil
                          #-sb-xc-host
                          (/ count
                             (ash (length (sb-impl::hash-table-pairs table)) -1))
                          nil nil nil nil nil) ; FIXME: compute PSL and mask
                  (let* ((cells (sb-impl::hss-cells (sb-impl::hashset-storage table)))
                         (psl (sb-impl::hs-cells-max-psl cells))
                         (mask (sb-impl::hs-cells-mask cells))
                         (lf (/ count (1+ mask))))
                    #-hashset-metrics (values lf nil nil psl mask)
                    #+hashset-metrics
                    (let ((seeks (sb-impl::hashset-count-finds table)))
                      (values lf seeks
                              (when (plusp seeks)
                                (/ (sb-impl::hashset-count-find-hits table) seeks))
                              psl mask))))))
          (incf total count)
          (apply #'format t
                 "  ~16a: ~7D ~5,1,2F%~#[~:; ~:[        ~;~:*~8D~]  ~:[     ~;~:*~4,1,2f%~]~
 ~6D ~6X~]~%" name count load
              (unless (hash-table-p table) (list seeks hit psl mask))))))
    (format t "  ~16A: ~7D~%" "Total" total)))

#-sb-xc-host
(progn
(defglobal *!initial-ctypes* nil)
(defun preload-ctype-hashsets (&aux permtypes)
  (declare (ignorable permtypes))
  (dolist (pair (nreverse *!initial-ctypes*))
    (destructuring-bind (instance . hashset-symbol) pair
      (cond ((not hashset-symbol)
             ;; There are very few which aren't in a hashset:
             ;; - (6) NAMED-TYPEs
             ;; - (1) MEMBER-TYPE NULL
             ;; - (3) BASE-CHAR, EXTENDED-CHAR, CHARACTER
             ;; - (1) CONS
             (push instance permtypes))
            ;; Mandatory special-case for singleton MEMBER types
            ((and (member-type-p instance) (not (cdr (member-type-members instance))))
             (setf (gethash (car (member-type-members instance)) *eql-type-cache*)
                   instance))
            (t
             (let ((hashset (symbol-value hashset-symbol)))
               (aver (not (hashset-find hashset instance))) ; instances are dumped bottom-up
               (hashset-insert hashset instance))))
      (labels ((ensure-interned-list (list hashset)
                 (let ((found (hashset-find hashset list)))
                   (when (and found (neq found list))
                     (bug "genesis failed to uniquify list-of-ctype in ~X"
                          (get-lisp-obj-address instance)))
                   (when (and list (not found))
                     (hashset-insert hashset list)))
                 (mapc #'check list))
               ;; Assert that looking for SUBPART finds nothing or finds itself
               (check (subpart &aux (hashset-symbol (ctype->hashset-sym subpart)))
                 (when hashset-symbol
                   (let* ((hashset (symbol-value hashset-symbol))
                          (found (hashset-find hashset subpart)))
                     (when (and found (neq found subpart))
                       (bug "genesis dumped bad instance within ~X"
                            (get-lisp-obj-address instance)))))))
        (etypecase instance
          ((or named-type numeric-type member-type character-set-type ; nothing extra to do
           #+sb-simd-pack simd-pack-type #+sb-simd-pack-256 simd-pack-256-type
           hairy-type))
          (args-type
           (ensure-interned-list (args-type-required instance) *ctype-list-hashset*)
           (ensure-interned-list (args-type-optional instance) *ctype-list-hashset*)
           (awhen (args-type-rest instance) (check it))
           (when (fun-type-p instance)
             (aver (null (fun-type-keywords instance)))
             (check (fun-type-returns instance))))
          (cons-type
           (check (cons-type-car-type instance))
           (check (cons-type-cdr-type instance)))
          (array-type
           (check (array-type-element-type instance))
           (check (array-type-specialized-element-type instance)))
          (compound-type
           (ensure-interned-list (compound-type-types instance) *ctype-set-hashset*))
          (negation-type
           (check (negation-type-type instance)))))))
  (aver (= (length permtypes) (+ 11 #-sb-unicode -2)))
  #+sb-devel (setq *hashsets-preloaded* t))
(preload-ctype-hashsets))

;;; *TYPE-CLASS-LIST* is defined only in the host. When the cross-compiler
;;; expands TYPEP-IMPL-MACRO it get the value of this symbol from the host's
;;; value. This function avoids a warning about a missing symbol.
(defun type-class-name-list () (mapcar 'car (symbol-value '*type-class-list*)))

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
                 (let ((absent (loop for class in (type-class-name-list)
                                     unless (or (member class '(values constant))
                                                (member class seen))
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

;;; Copy X to the heap, give it a random hash, and if it is a MEMBER type
;;; then assert that all members are cacheable.
#-sb-xc-host
(defun copy-ctype (x)
  (declare (type ctype x))
  (declare (sb-c::tlab :system) (inline !copy-xset))
  #+c-stack-is-control-stack (aver (stack-allocated-p x))
  (labels ((copy (x)
             ;; Return a heap copy of X if X was arena or stack-allocated.
             ;; I suspect it's quicker to copy always rather than conditionally.
             ;; The use for this is that supposing the user constructs a type specifier
             ;; like (DOUBLE-FLOAT (2.0) 4.0) where those numbers and the inner list
             ;; were constructed on an arena, they need to be copied.
             (etypecase x
               (number (sb-vm:copy-number-to-heap x))
               (cons (cons (copy (car x)) (copy (cdr x))))
               (symbol x)))
           (copy-xset (xset &aux (data (xset-data xset)))
             ;; MEMBER-TYPE is a problem because the members could be arena-allocated.
             ;; It would be easy enough to avoid entering some instances in a hashset, though
             ;; the larger issue is that it may be inserted into any number of other caches.
             ;; CLHS never says whether DX objects are or aren't legal in type specifiers.
             ;; I consider this "user error" as it seems to push the boundary of what should
             ;; be permissible, but we can do better than to cache data that are on the stack.
             ;; If the XSET is represented as a hash-table, we may have another issue
             ;; which is not dealt with here (hash-table in the arena)
             (cond ((listp data)
                    ;; the XSET can be empty if a MEMBER type contains only FP zeros.
                    ;; While we could use (load-time-value) to referece a constant empty xset
                    ;; there's really no point to doing that.
                    (collect ((elts))
                      (dolist (x data (!copy-xset (xset-list-size xset) (elts)))
                        (elts (cond ((numberp x) (sb-vm:copy-number-to-heap x))
                                    ((safe-member-type-elt-p x) x)
                                    ;; surely things will go haywire if this occurs
                                    (t (error "Off-heap MEMBER type member @ ~X"
                                              (get-lisp-obj-address x))))))))
                   ;; Huge MEMBER types are rare so I'm not going to worry too much,
                   ;; just check whether it's OK or not
                   ((and (loop for k being each hash-key of data
                               always (safe-member-type-elt-p k))
                         (heap-allocated-p data))
                    xset)
                   (t ; This could certainly be improved
                    (error "Off-heap MEMBER type members")))))
    (let ((bits (logior (logand (ctype-random) +type-hash-mask+)
                        (type-%bits x))))
      ;; These cases are in descending order of frequency of seek in the hashsets.
      ;; It matters only for backends that don't convert TYPECASE to a jump table.
      (etypecase x
        (values-type
         (!alloc-values-type bits (values-type-required x) (values-type-optional x)
                             (values-type-rest x)))
        (fun-type ; or FUN-DESIGNATOR-TYPE
         (let ((copy (!alloc-fun-type
                      bits (fun-type-required x) (fun-type-optional x) (fun-type-rest x)
                      (fun-type-keyp x) (fun-type-keywords x) (fun-type-allowp x)
                      (fun-type-wild-args x) (fun-type-returns x))))
           (%set-instance-layout copy (%instance-layout x))
           copy))
        (numeric-type
         (!alloc-numeric-type bits (numeric-type-aspects x)
                              (copy (numeric-type-low x)) (copy (numeric-type-high x))))
        (compound-type ; UNION or INTERSECTION
         (let ((copy (!alloc-union-type bits (compound-type-enumerable x)
                                        (compound-type-types x))))
           (%set-instance-layout copy (%instance-layout x))
           copy))
        (member-type
         (!alloc-member-type bits (copy-xset (member-type-xset x))
          (mapcar 'sb-vm:copy-number-to-heap (member-type-fp-zeroes x))))
        (array-type
         (!alloc-array-type bits (copy (array-type-dimensions x))
                            (array-type-complexp x) (array-type-element-type x)
                            (array-type-specialized-element-type x)))
        (hairy-type ; SATISFIES or UNKNOWN
         (let ((copy (!alloc-hairy-type bits (copy (hairy-type-specifier x)))))
           (%set-instance-layout copy (%instance-layout x))
           copy))
        (negation-type (!alloc-negation-type bits (negation-type-type x)))
        (constant-type (!alloc-constant-type bits (constant-type-type x)))
        (cons-type (!alloc-cons-type bits (cons-type-car-type x) (cons-type-cdr-type x)))
        (character-set-type
         (!alloc-character-set-type bits (copy (character-set-type-pairs x))))
        #+sb-simd-pack
        (simd-pack-type (!alloc-simd-pack-type bits (simd-pack-type-tag-mask x)))
        #+sb-simd-pack-256
        (simd-pack-256-type (!alloc-simd-pack-256-type bits (simd-pack-256-type-tag-mask x)))
        (alien-type-type (!alloc-alien-type-type bits (alien-type-type-alien-type x)))))))

;;; Drop NILs, possibly reducing the storage vector length
(defun rebuild-ctype-hashsets ()
  (dolist (sym (list* '*key-info-hashset* '*key-info-list-hashset*
                      *ctype-hashsets*))
    (sb-impl::hashset-rehash (symbol-value sym) nil)))

;;; a flag that causes TYPE-SPECIFIER to represent UNKNOWN-TYPE
;;; as itself rather than the symbol naming the type so that the printed
;;; representation is not confusable for a good type of the same name.
(defconstant +ctype-unparse-disambiguate+ 1)
;;; a flag that causes all function types to unparse as FUNCTION.
;;; This is useful when we want a specifier that we can pass to TYPEP.
(defconstant +unparse-fun-type-simplify+  2)

(defmethod print-object ((ctype ctype) stream)
  (let ((expr
         (if (unknown-type-p ctype)
             ;; Don't call the unparse method - it returns the instance itself
             ;; which would infinitely recurse back into print-object
             (unknown-type-specifier ctype)
             (funcall (type-class-unparse (type-class ctype))
                      +ctype-unparse-disambiguate+
                      ctype))))
    (print-unreadable-object (ctype stream :type t)
      (prin1 expr stream))))
