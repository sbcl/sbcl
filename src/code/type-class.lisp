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
         ;; Use the per-thread alloc region pointer when possible
         #+(or x86-64 sb-thread)
         (sap-int (sb-vm::current-thread-offset-sap sb-vm::thread-mixed-tlab-slot))
         ;; Otherwise mixed_region in static space
         #-(or x86-64 sb-thread)
         (sb-sys:sap-ref-word (sb-sys:int-sap (+ sb-vm::static-space-start
                                                 sb-vm::mixed-region-offset))
                              0)))
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

;;; Each CTYPE instance (all subtypes thereof) has a random opaque hash value.
;;; Hashes are mixed together to form a lookup key in the memoization wrappers
;;; for most operations on CTYPES. This works because CTYPEs are immutable.
;;; No more than N-FIXNUM-BITS for 32-bit machines are used, even for 64-bit words.
;;; It's easiest this way. It could be host-fixnum-sized for the host, and then
;;; target-fixnum-sized for the target, but that's not easy to do with DEF!STRUCT.
;;; (In fact I think it's probably infeasible but I'm not certain of it)
;;; You could always make it SB-XC:FIXNUM at the risk of forcing the host to
;;; deal in bignums. Why cause it undue slowness when we don't need so many bits?
;;; NOTE: we _do_ use the sign bit, leaving us 25 pseudorandom bits, but
;;; the 2 bits of least significance are NOT pseudorandom, so it's best
;;; not to use them directly in the hash index.
(defconstant ctype-hash-size  30)  ; all significant bits, for the slot type specifier
(defconstant ctype-PRNG-nbits 25)  ; from pseudorandom number generator
(defconstant ctype-contains-unknown #b01)
(defconstant ctype-contains-hairy   #b10) ; any hairy type, including UNKNOWN
(defconstant +ctype-flag-mask+ #b11)
(defconstant +ctype-hash-mask+ (logandc2 (1- (ash 1 ctype-PRNG-nbits)) #b11))

(defstruct (ctype (:conc-name type-)
                   (:constructor nil)
                   (:copier nil)
                   #-sb-xc-host (:pure t))
  ;; bits  0..24: pseudorandom hash
  ;; bits 25..29: 5 bits for type-class index
  (%bits (missing-arg) :type (signed-byte #.ctype-hash-size) :read-only t))

;;; Apparently the old CONTAINS-UNKNOWN-TYPE-P function could accept NIL
;;; and return NIL. This seems kinda sloppy. Can we get rid of that "feature"?
(declaim (inline contains-unknown-type-p contains-hairy-type-p))
(defun contains-unknown-type-p (ctype)
  (if ctype (oddp (type-%bits ctype)) nil))
(defun contains-hairy-type-p (ctype)
  (logbitp 1 (type-%bits ctype)))

(defun ok-to-memoize-p (arg)
  (etypecase arg
    (ctype (evenp (type-%bits arg))) ; i.e. not CTYPE-CONTAINS-UNKNOWN
    (list  (dolist (elt arg t)
             (when (oddp (type-%bits elt)) (return nil))))))

(defmacro type-class-id (ctype) `(ldb (byte 5 ,ctype-PRNG-nbits) (type-%bits ,ctype)))
(defmacro type-id->type-class (id) `(truly-the type-class (aref *type-classes* ,id)))
(defmacro type-class (ctype) `(type-id->type-class (type-class-id ,ctype)))

(declaim (inline type-hash-value))
(defun type-hash-value (ctype) (logand (type-%bits ctype) sb-xc:most-positive-fixnum))

(defmacro type-flags (ctype) `(logand (type-%bits ,ctype) +ctype-flag-mask+))
;;; Hash caches can in general accept any signed fixnum as the hash.
;;; Hashsets probably can as well, but if the mixing function entails MIX,
;;; the inputs have to be positive fixnums.
;;; I can't remember if our hash-tables that use arbitrary user-supplied
;;; hash functions can accept negative fixnums.
(defun type-list-flags (list)
  (let ((bits 0)) ; LOGIOR together and then mask once when done
    (dolist (ctype list (logand bits +ctype-flag-mask+))
      (setq bits (logior bits (type-%bits ctype))))))

(defglobal *ctype-hashsets* nil)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ctype-class-bits (type-class)
    (let* ((index (type-class-name->id type-class))
           (shifted
            (dpb index
                 (byte 5 ctype-PRNG-nbits)
                 ;; ensure that the result is a (SIGNED-BYTE 30) by depositing
                 ;; into a -1 if the high bit of the class ID is on.
                 (if (logbitp 4 index) (ash -1 ctype-PRNG-nbits) 0))))
      (the (signed-byte #.ctype-hash-size) shifted)))
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
(defun make-ctype-bits (type-class &optional (hash (ctype-random)))
  (logior (ctype-class-bits type-class) (logand hash +ctype-hash-mask+)))

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

;;; Sure we could infer this list by seeing what gets defined,
;;; but doing that would introduce a requirement that TYPE= be defined
;;; only after all the methods are.
;;; No need to make this more complicated.
(defglobal classes-having-complex-=-method
  '(named intersection union negation member hairy))
(eval-when (#-sb-xc-host :compile-toplevel)
  (dolist (name classes-having-complex-=-method)
    ;; Assert that NAME is valid and defines a COMPLEX-= method
    (assert (functionp (type-class-complex-= (!type-class-or-lose name))))))

(defmacro define-type-method ((class method &rest more-methods)
                               lambda-list &body body)
  (when (and (eq method :complex-=)
             (not (member class classes-having-complex-=-method)))
    (error "Didn't expect to see a ~S method for type-class ~S"
           method class))
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
         #-sb-xc-host (declare (optimize (sb-c::verify-arg-count 0)))
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
                                      ;; This default is counterintuitive.
                                      ;; You'd think the most general default
                                      ;; would be "don't know" (i.e. NIL NIL)
                                      ;; instead of "Certainly no"
                                      (default '(values nil t))
                                        ; assume complex fn is symmetric
                                        ; unless told otherwise.
                                      (complex-arg1 complex-arg2 complex-arg1-p))
  (declare (type keyword simple complex-arg1 complex-arg2))
   `(let* ((.L ,type1) (id1 (type-class-id .L))
           (.R ,type2) (id2 (type-class-id .R))
           (c2 (type-id->type-class id2)))
      (if (/= id1 id2)
          (acond ((,(type-class-fun-slot complex-arg2) c2)
                  (funcall it .L .R))
                 ((,(type-class-fun-slot complex-arg1) (type-id->type-class id1))
                   ;; if COMPLEX-ARG1 method was provided, the method accepts
                   ;; the arguments exactly as given. Otherwise, flip them.
                  (funcall it ,@(if complex-arg1-p `(.L .R) `(.R .L))))
                 (t ,default))
          ,(if (eq simple :none)
               '(bug "nope")
               `(funcall (,(type-class-fun-slot simple) c2) .L .R)))))

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

;;;; miscellany

;;; Various hash mixing functions.
(declaim (inline hash-ctype-pair))
(declaim (ftype (function (ctype ctype) (signed-byte #.ctype-hash-size)) hash-ctype-pair))
(defun hash-ctype-pair (type1 type2)
  (logxor (ash (type-%bits type1) -3) (type-%bits type2)))

(declaim (inline hash-ctype-list))
(declaim (ftype (function (list) (signed-byte #.ctype-hash-size)) hash-ctype-list))
(defun hash-ctype-list (types)
  (loop with res of-type (signed-byte #.ctype-hash-size) = 0
        for type in types
        do (setq res (logxor (ash res -1) (type-%bits type)))
        ;; This returns a positive number so that it can be passed to MIX
        finally (return (ldb (byte (1- ctype-hash-size) 0) res))))

(defun hash-ctype-set (types) ; ctype list hashed order-insensitively
  (let ((hash (type-%bits (car types)))
        (n 1))
    (declare (type sb-xc:fixnum hash))
    (dolist (type (cdr types) (mix (logand hash sb-xc:most-positive-fixnum)
                                   (the (integer 2) n)))
      (incf n)
      (setq hash (plus-mod-fixnum (type-%bits type) hash)))))
;;; NOTE: despite the name, this does not operate only on lists of CTYPE.
;;; Maybe pick a better name.
(defun ctype-set= (a b)
  ;; However, it might also be nice to canonicalize sets by putting any type containing
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
                                     (hasher (getf (cddr slot) :hasher :fail))
                                     (comparator (getf (cddr slot) :test)))
                    (cond ((or comparator (null hasher))) ; ok
                          ((eq type 'ctype)
                           (setq comparator 'eq hasher 'type-hash-value))
                          ((or (and (typep type '(cons (eql member)))
                                    (every #'symbolp (cdr type)))
                               (eq type 'boolean))
                           (setq comparator 'eq)
                           (when (eq type 'boolean)
                             (setq hasher '(lambda (x) (if x #xaa55aa #x55aa55)))))
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
           (let
            ((functions
             `((defun ,(symbolicate "CALC-" name "-HASH") (x)
                 #-sb-xc-host (declare (optimize (safety 0)))
                 (,(if (assoc :extra-mix-step options) 'type-hash-final-mix 'progn)
                  (type-hash-mix
                   ,@(when include `((,(symbolicate "CALC-" include "-HASH") x)))
                   ,@(mapcar (lambda (slot &aux (reader
                                                 (symbolicate conc-name (svref slot 0))))
                               `(,(svref slot 1) (,reader x)))
                             hashed-slots))))
               (defun ,(symbolicate name "-EQUIV") (a b)
                 #-sb-xc-host (declare (optimize (safety 0)))
                 (and ,@(mapcar (lambda (slot &aux (reader
                                                    (symbolicate conc-name (svref slot 0))))
                                  `(,(elt slot 2) (,reader a) (,reader b)))
                                hashed-slots)
                      ,@(when include `((,(symbolicate include "-EQUIV") a b))))))))
             functions))
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
               (new-ctype ,name
                          ,(ecase name ; Compute or propagate the flag bits
                             (hairy-type ctype-contains-hairy)
                             (unknown-type (logior ctype-contains-unknown ctype-contains-hairy))
                             ((simd-pack-type simd-pack-256-type alien-type-type) 0)
                             (negation-type '(type-flags type))
                             (array-type '(type-flags element-type)))
                          ,@(cdr private-ctor-args))))))))

(defmacro type-hash-mix (&rest args) (reduce (lambda (a b) `(mix ,a ,b)) args))
;;; The final mix ensures that all bits affect the masked hash.
;;; Since it takes non-zero time, only do it for NUMERIC and ARRAY, where it seems
;;; to make a large difference in the maximum probe sequence length.
(defmacro type-hash-final-mix (val) `(murmur-hash-word/+fixnum ,val))

#-sb-xc-host
(progn
  (defmacro sb-c::number-hash (x) `(sb-impl::number-sxhash ,x))
  ;; This is used on a HAIRY specifier which could be an UNKNOWN (just a symbol), or a SATISFIES.
  ;; There is no reason at all that two distinct symbols should hash the same when their
  ;; names are STRING= so really we want something better than SXHASH, but it does noeed to
  ;; recurse on lists.
  (defmacro sb-c::fallback-hash (x) `(sxhash ,x)))

;; Singleton MEMBER types are best dealt with via a weak-value hash-table because:
;; * (MEMBER THING) might lack an address-insensitive hash for THING
;;   but src/code/hashset goes through a lot of rigmarole to handle address-bashed
;;   hashing, and the end result for a single key would laboriously emulate an EQL table.
;;   This is especially important for the compiler because each time it asks itself the
;;   CTYPE-OF a constant leaf, the answer might be a singleton MEMBER type.
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
(define-load-time-global *eql-type-cache* ; like EQL-SPECIALIZER-TABLE in PCL
    (sb-impl::make-system-hash-table :test 'eql :weakness :value :synchronized nil))

#-sb-xc-host
(defun ctype-hashset-insert-if-absent (hashset key function)
  (or (hashset-find hashset key)
      (let ((flags (funcall function key)))
        (with-system-mutex ((hashset-mutex hashset))
          (or (hashset-find hashset key)
              (hashset-insert hashset (copy-ctype key flags)))))))

(defvar *hashsets-preloaded* nil)
(defmacro new-ctype (metatype flags-expr &rest initargs)
  (let* ((hashset (package-symbolicate "SB-KERNEL" "*" metatype "-HASHSET*"))
         (allocator (package-symbolicate "SB-KERNEL" "!ALLOC-" metatype))
         (defer-flags (typep flags-expr '(cons (member lambda function))))
         (flag-bits (if defer-flags 0 flags-expr))
         (class-bits (ctype-class-bits (ctype-instance->type-class metatype))))

    #+sb-xc-host
    (let ((gensyms (make-gensym-list (length initargs))))
      `(multiple-value-bind ,gensyms (values ,@initargs)
         (let ((temp (,allocator
                      (logior ,@(unless defer-flags
                                  '((logand (ctype-random) +ctype-hash-mask+)))
                              ,flag-bits ,class-bits)
                      ,@gensyms)))
           ;; If lazily computing flags, might have to make a second instance
           ;; since the %BITS slot is immutable, so try to stack-allocate this.
           ,@(if defer-flags
                 `((declare (dynamic-extent temp))
                   (or (hashset-find ,hashset temp)
                       (hashset-insert
                        ,hashset
                        (,allocator (logior (logand (ctype-random) +ctype-hash-mask+)
                                            (funcall ,flags-expr temp) ,class-bits)
                                    ,@gensyms))))
                 `((hashset-insert-if-absent ,hashset temp #'identity))))))

    ;; allocate temporary key, copy it if and only if not found.
    ;; COPY-CTYPE can copy subparts like the numeric bound if arena-allocated
    #-sb-xc-host
    `(let ((temp (,allocator (logior ,flag-bits ,class-bits) ,@initargs)))
       (declare (dynamic-extent temp))
       #+nil ; or #+sb-devel as you see fit
       (unless *hashsets-preloaded*
         (write-string "CTYPE hashset preload failure")
         (sb-vm:ldb-monitor))
       (truly-the (values ,metatype &optional)
                  ,(if defer-flags
                       `(ctype-hashset-insert-if-absent ,hashset temp ,flags-expr)
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
  (specifier nil :type t :test equal :hasher sb-c::fallback-hash))

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
(define-load-time-global *xset-mutex* (or #-sb-xc-host (sb-thread:make-mutex :name "xset")))
;;; This hashset is guarded by *XSET-MUTEX*. It is _not_ declared as synchronized
;;; so that HASHSET-INSERT-IF-ABSENT should not acquire a mutex inside a mutex
;;; (stable hashes have to be assigned while holding the lock)
(define-load-time-global *member/eq-type-hashset*
    (make-hashset 32 #'member-type-equiv #'calc-member-type-hash
                  :weakness t :synchronized nil))
(pushnew '*member/eq-type-hashset* *ctype-hashsets*)

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
  (complexp :maybe :type (member t nil :maybe)
            :hasher (lambda (s) (case s (:maybe #36rMAYBE) ((nil) #xffff) (t 1))))
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
(defun compound-type-flags (type) (type-list-flags (compound-type-types type)))

;;; A UNION-TYPE represents a use of the OR type specifier which we
;;; couldn't canonicalize to something simpler. Canonical form:
;;;   1. All possible pairwise simplifications (using the UNION2 type
;;;      methods) have been performed. Thus e.g. there is never more
;;;      than one MEMBER-TYPE component. FIXME: As of sbcl-0.6.11.13,
;;;      this hadn't been fully implemented yet.
;;;   2. There are never any UNION-TYPE components.
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
  (complexp :real :read-only t :type (member :real :complex nil))
  ;; the kind of numeric type we have, or NIL if the type is NUMBER.
  ;; The types corresponding to all of REAL or all of COMPLEX are UNION types,
  ;; and no constituent type thereof will have a NIL here.
  (class nil :read-only t :type (member integer rational float nil))
  ;; "precision" for a float type (i.e. type specifier for a CPU
  ;; representation of floating point, e.g. 'SINGLE-FLOAT).
  ;; NIL if and only if CLASS is not FLOAT
  (precision nil :read-only t :type (member single-float double-float nil))
  ;; a value that uniquely identifies this triple of <complexp,class,precision>
  (id 0 :read-only t :type (unsigned-byte 8)))

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
             `(let ((x ,b))
                (block nil
                  (multiple-value-bind (h v)
                      (if (listp x)
                          (if x (values #x55AA55 (car x)) (return 0))
                          (values 0 x))
                    (logxor h (sb-c::number-hash v))))))
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

(defmethod print-object ((self key-info) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "(~S ~S)"
            (key-info-name self)
            (type-specifier (key-info-type self)))))
(defun key-info= (a b)
  (declare (optimize (safety 0)))
  (and (eq (key-info-name a) (key-info-name b))
       (eq (key-info-type a) (key-info-type b))))
(defun key-info-hash (x)
  (declare (optimize (safety 0)))
  (murmur-hash-word/+fixnum
   (mix (type-hash-value (key-info-type x)) (symbol-hash (key-info-name x)))))

(defun hash-key-info-set (set) ; order-insensitive
  (declare (optimize (safety 0)))
  (let ((h 0))
    (declare (type sb-xc:fixnum h))
    ;; Don't need the answer to be positive for key-info-set-hashset,
    ;; but do need it to be positive when hashing ARGS-TYPE which uses MIX.
    (dolist (elt set (logand h sb-xc:most-positive-fixnum))
      (setf h (plus-mod-fixnum (truly-the sb-xc:fixnum (key-info-hash elt)) h)))))

(defun key-info-list-flags (list)
  (let ((bits 0))
    (dolist (elt list (logand bits +ctype-flag-mask+))
      (setq bits (logior bits (type-%bits (key-info-type elt)))))))

(define-load-time-global *key-info-hashset*
    (make-hashset 32 #'key-info= #'key-info-hash :weakness t :synchronized t))
(define-load-time-global *key-info-set-hashset*
    (make-hashset 32 #'ctype-set= #'hash-key-info-set :weakness t :synchronized t))

(defun make-key-info (key type)
  (dx-let ((x (!make-key-info key type)))
     (hashset-insert-if-absent *key-info-hashset* x
      (named-lambda "MAKE-KEY-INFO" (x)
        (!make-key-info (key-info-name x) (key-info-type x))))))
(defun intern-key-infos (list)
  (when list
    ;; I suppose we don't have to COPY-LIST to insert.
    (hashset-insert-if-absent *key-info-set-hashset* list #'identity)))

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
  (keywords nil :type list :hasher hash-key-info-set :test eq) ; hash-consed already
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
   :test = :hasher identity ; the tag-mask is its own hash
   :type (and (unsigned-byte #.(length +simd-pack-element-types+))
              (not (eql 0)))))

#+sb-simd-pack-256
(def-type-model (simd-pack-256-type
                 (:constructor* %make-simd-pack-256-type (tag-mask)))
  (tag-mask (missing-arg)
   :test = :hasher identity ; the tag-mask is its own hash
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

(defglobal *alien-type-hashsets* nil)
(export 'show-ctype-ctor-cache-metrics)
;;; The minimum hashset storage size is 64 elements, so a bunch of the caches
;;; start out with too-low load-factor, being somewhat oversized.
;;; The EQL table never sizes down (because our hash-tables don't)
;;; so it may operate at a fairly low load factor.
;;; Other than that we should expect load factors between 50% and 75%.
;;; So it's extremely unexpected that List starts out with a load-factor of 12%.
;;; Probably should investigate, though it's harmless.
(defun show-ctype-ctor-cache-metrics ()
  (labels
      ((tablecount (x)
         (if (hash-table-p x) (hash-table-count x) (sb-impl::hashset-count x)))
       (display (caches &aux (total 0))
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
 ~6D ~6X~]~%"
                      name count load
                      (unless (hash-table-p table) (list seeks hit psl mask)))))
         (format t "  ~16A: ~7D~%" "Total" total)))
    (let (caches)
      (push (list "List" *ctype-list-hashset*) caches)
      (push (list "Set" *ctype-set-hashset*) caches)
      (push (list "Key-Info" *key-info-hashset*) caches)
      (push (list "Key-Info-Set" *key-info-set-hashset*) caches)
      (push (list "EQL" *eql-type-cache*) caches)
      (dolist (symbol *ctype-hashsets*)
        (push (list (subseq (string symbol) 1
                            (- (length (string symbol)) (length "-TYPE-HASHSET*")))
                    (symbol-value symbol))
              caches))
      (format t "~&ctype cache metrics:  Count     LF     Seek    Hit maxPSL  Mask~%")
      (display caches))
    (let (caches)
      (format t "~&Alien:~%")
      (dolist (symbol *alien-type-hashsets*)
        (let ((name (subseq (string symbol) 1
                            (- (length (string symbol)) (length "-TYPE-CACHE*")))))
          (push (list (if (char= (char name 0) #\A) (subseq name 6) name)
                      (symbol-value symbol))
              caches)))
      (display caches))))

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

;;; Drop NILs, possibly reducing the storage vector length
(defun rebuild-ctype-hashsets ()
  (dolist (sym (list* '*key-info-hashset* '*key-info-set-hashset*
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
