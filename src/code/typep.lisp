;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-KERNEL")

;;; (Note that when cross-compiling, SB-XC:TYPEP is interpreted as a
;;; test that the host Lisp object OBJECT translates to a target SBCL
;;; type TYPE. This behavior is needed e.g. to test for the validity
;;; of numeric subtype bounds read when cross-compiling.)
(defun typep (object type &optional environment)
  "Is OBJECT of type TYPE?"
  (declare (type lexenv-designator environment) (ignore environment))
  (declare (explicit-check))
  ;; Actually interpreting types at runtime is done by %TYPEP. The
  ;; cost of the extra function call here should be negligible
  ;; compared to the cost of interpreting types. (And the compiler
  ;; tries hard to optimize away the interpretation of types at
  ;; runtime, and when it succeeds, we never get here anyway.)
  (%%typep object (specifier-type type)))

;;; the actual TYPEP engine. The compiler only generates calls to this
;;; function when it can't figure out anything more intelligent to do.
(defun %typep (object specifier)
  ;; Checking CTYPE-P on the specifier, as used to be done, is not right.
  ;; If the specifier were a CTYPE we shouldn't have gotten here.
  (declare (explicit-check))
  (%%typep object (specifier-type specifier)))

;;; probably not the right place for this declamation. The benefits
;;; should be more widespread.
(declaim (freeze-type ctype))
(defun %%typep (object type &optional (strict t))
 (declare (type ctype type))
 (typep-impl-macro (object :defaults nil)
    (named-type
     (ecase (named-type-name type)
       ((* t) t)
       ((instance) (%instancep object))
       ((funcallable-instance) (funcallable-instance-p object))
       ((extended-sequence) (extended-sequence-p object))
       ((nil) nil)))
    (numeric-type (number-typep object type))
    (array-type
     (and (arrayp object)
          (or (eq (array-type-complexp type) :maybe)
              (eq (not (simple-array-p object))
                  (array-type-complexp type)))
          (let ((want (array-type-dimensions type)))
            (or (eq want '*)
                (if (array-header-p object)
                    (do ((rank (array-rank object))
                         (axis 0 (1+ axis))
                         (want want (cdr want)))
                        ((= axis rank) (null want))
                     (let ((dim (car want)))
                       (unless (or (eq dim '*)
                                   (eq dim (%array-dimension object axis)))
                         (return nil))))
                    (let ((dim (car want)))
                      (and (or (eq dim '*) (eq dim (length object)))
                           (not (cdr want)))))))
          (if (unknown-type-p (array-type-element-type type))
              ;; better to fail this way than to get bogosities like
              ;;   (TYPEP (MAKE-ARRAY 11) '(ARRAY SOME-UNDEFINED-TYPE)) => T
              (error "~@<unknown element type in array type: ~2I~_~S~:>"
                     (type-specifier type))
              t)
          (or (eq (array-type-specialized-element-type type) *wild-type*)
              (values (type= (array-type-specialized-element-type type)
                             ;; FIXME: not the most efficient.
                             (specifier-type (array-element-type
                                              object)))))))
    (member-type
     (when (member-type-member-p object type)
       t))
    (classoid
     ;; It might be more efficient to check that OBJECT is either INSTANCEP
     ;; or FUNCALLABLE-INSTANCE-P before making this call.
     ;; But doing that would change the behavior if %%TYPEP were ever called
     ;; with a built-in classoid whose members are not instances.
     ;; e.g. (%%typep (find-fdefn 'car) (specifier-type 'fdefn))
     ;; I'm not sure if that can happen.
     (classoid-typep (layout-of object) type object))
    (union-type
     (some (lambda (union-type-type) (recurse object union-type-type))
           (union-type-types type)))
    (intersection-type
     (every (lambda (intersection-type-type) (recurse object intersection-type-type))
            (intersection-type-types type)))
    (cons-type
     (and (consp object)
          (recurse (car object) (cons-type-car-type type))
          (recurse (cdr object) (cons-type-cdr-type type))))
    #+sb-simd-pack
    (simd-pack-type
     (and (simd-pack-p object)
          (let* ((tag (%simd-pack-tag object))
                 (name (nth tag *simd-pack-element-types*)))
            (not (not (member name (simd-pack-type-element-type type)))))))
    #+sb-simd-pack-256
    (simd-pack-256-type
     (and (simd-pack-256-p object)
          (let* ((tag (%simd-pack-256-tag object))
                 (name (nth tag *simd-pack-element-types*)))
            (not (not (member name (simd-pack-256-type-element-type type)))))))
    (character-set-type
     (test-character-type type))
    (negation-type
     (not (recurse object (negation-type-type type))))
    (hairy-type
     (if (unknown-type-p type)
         ;; Parse it again to make sure it's really undefined.
         (let ((reparse (specifier-type (unknown-type-specifier type))))
           (if (typep reparse 'unknown-type)
               (error "unknown type specifier: ~S" (unknown-type-specifier reparse))
               (recurse object reparse)))
         ;; Must be a SATISFIES type
         (when (funcall (symbol-function (cadr (hairy-type-specifier type)))
                        object)
           t)))
    (alien-type-type
     (sb-alien-internals:alien-typep object (alien-type-type-alien-type type)))
    (fun-type
     (if (fun-designator-type-p type)
         (bug "%%TYPEP got ~S" type)
         (case strict
           ((functionp) (functionp object)) ; least strict
           ((nil) ; medium strict
            (and (functionp object)
                 (csubtypep (specifier-type (sb-impl::%fun-type object)) type)))
           (t ; strict
            (error "Function types are not a legal argument to TYPEP:~%  ~S"
                   (type-specifier type))))))))

(defun cached-typep (cache object)
  (let* ((type (cdr cache))
         (ctype (if (ctype-p type)
                    type
                    (specifier-type type))))
    (if (unknown-type-p ctype)
        (%%typep object ctype)
        ;; Most of the time an undefined type becomes defined is
        ;; through structure or class definition, optimize that case
        (let ((fun
                (if (classoid-p ctype)
                    (lambda (cache object)
                      ;; TODO: structures can be optimized even further
                      (block nil
                        (classoid-typep
                         (typecase object
                           (instance (%instance-layout object))
                           (funcallable-instance
                            (%funcallable-instance-layout object))
                           (t (return)))
                         (cdr (truly-the cons cache))
                         object)))
                    (lambda (cache object)
                      (%%typep object (cdr (truly-the cons cache)))))))
          (setf (cdr cache) ctype)
          (sb-thread:barrier (:write))
          (setf (car cache) fun)
          (funcall fun cache object)))))

;;; Do a type test from a class cell, allowing forward reference and
;;; redefinition.
(defun classoid-cell-typep (cell object)
  (let ((layout (typecase object
                  (instance (%instance-layout object))
                  (funcallable-instance (%funcallable-instance-layout object))
                  (t (return-from classoid-cell-typep))))
        (classoid (classoid-cell-classoid cell)))
    (unless classoid
      (error "The class ~S has not yet been defined."
             (classoid-cell-name cell)))
    (classoid-typep layout classoid object)))

;;; Test whether OBJ-LAYOUT is from an instance of CLASSOID.

;;; IMPORTANT: If none of the classes involved (directly or indirectly)
;;; in a call to CLASSOID-TYPEP gets redefined during execution of the predicate,
;;; the layout update loop should require at most 2 iterations.
;;; Theoretically, ensuring validity of the classoid layout and the object layout
;;; could be done in either order, * HOWEVER * it is less racy to perform
;;; them in this exact order. Consider the case that OBJ-LAYOUT is T
;;; for a class that satisfies CLASS-FINALIZED-P and suppose these operations were
;;; reversed from the order below. UPDATE-OBJECT-LAYOUT-OR-INVALID is going to make
;;; a new layout, registering it and installing into the classoid.
;;; Then %ENSURE-CLASSOID-VALID is going to call %FORCE-CACHE-FLUSHES which is going
;;; to make yet another new layout. The "transitivity of wrapper updates" usually
;;; causes the first new layout to automatically update to the second new layout,
;;; except that the other thread has already fetched the old layout.
;;; But by using the order below, there will not be two new layouts made, only one,
;;; because UPDATE-OBJECT-LAYOUT-OR-INVALID is able to use the layout
;;; that was updated into the classoid by %ENSURE-CLASSOID-VALID.
;;; All other things being equal, one new layout is better than two.
;;; At least I think that's what happens.
;;; So consider what happens if two threads are both doing this -
;;; with the opposite order, there could have been as many as 5 new layouts
;;; created (empirically observed via instrumentation of MAKE-LAYOUT) -
;;; two per thread; plus one more, which is the failing one.
;;; It was even possible to have *BOTH* threads fail the AVER, although more often it
;;; was just one of them that fails.
;;; With the order of operations below, I observed no failures in hundreds
;;; of thousands of iterations of 'classoid-typep.impure.lisp'

(defun classoid-typep (obj-layout classoid object)
  ;; FIXME & KLUDGE: We could like to grab the *WORLD-LOCK* here (to ensure that
  ;; class graph doesn't change while we're doing the typep test), but in
  ;; practice that causes trouble -- deadlocking against the compiler
  ;; if compiler output (or macro, or compiler-macro expansion) causes
  ;; another thread to do stuff. Not locking is a shoddy bandaid as it is remains
  ;; easy to trigger the same problem using a different code path -- but in practice
  ;; locking here makes Slime unusable with :SPAWN in post *WORLD-LOCK* world. So...
  ;; -- NS 2008-12-16
  (multiple-value-bind (obj-layout layout)
      (do ((layout (classoid-layout classoid) (classoid-layout classoid))
           (i 0 (+ i 1))
           (obj-layout obj-layout))
          ((and (not (layout-invalid obj-layout))
                (not (layout-invalid layout)))
           (values obj-layout layout))
        (aver (< i 2))
        (%ensure-classoid-valid classoid layout "typep")
        (when (layout-invalid obj-layout)
          (setq obj-layout (update-object-layout-or-invalid object layout))))
    (or (eq obj-layout layout)
        (let ((obj-inherits (layout-inherits obj-layout)))
          (dotimes (i (length obj-inherits) nil)
            (when (eq (svref obj-inherits i) layout)
              (return t)))))))

;;; If TYPE is a type that we can do a compile-time test on, then
;;; return whether the object is of that type as the first value and
;;; second value true. Otherwise return NIL, NIL.
;;;
;;; We give up on unknown types and pick off FUNCTION- and COMPOUND-
;;; types. For STRUCTURE- types, we require that the type be defined
;;; in both the current and compiler environments, and that the
;;; INCLUDES be the same.
;;;
(defun ctypep (obj type)
  (declare (type ctype type))
  (typep-impl-macro (obj)
    ((or numeric-type
         named-type
         member-type
         character-set-type
         #+sb-simd-pack simd-pack-type
         #+sb-simd-pack-256 simd-pack-256-type)
     (values (%%typep obj type)
             t))
    (array-type
     (if (contains-unknown-type-p type)
         (values nil (not (arrayp obj)))
         (values (%%typep obj type) t)))
    (classoid
     (if (built-in-classoid-p type)
         (values (%%typep obj type) t)
         (if (if (csubtypep type (specifier-type 'function))
                 (funcallable-instance-p obj)
                 (%instancep obj))
             (if (eq (classoid-layout type)
                     (info :type :compiler-layout (classoid-name type)))
                 (values (sb-xc:typep obj type) t)
                 (values nil nil))
             (values nil t))))
    (fun-type
     (cond ((and (symbolp obj) (fun-designator-type-p type))
            (values nil nil))
           ((functionp obj)
            (csubtypep (specifier-type (%simple-fun-type (%fun-fun obj))) type))
           (t (values nil t))))
    (alien-type-type
     (values (alien-typep obj (alien-type-type-alien-type type)) t))
    (hairy-type
     (if (unknown-type-p type)
         (values nil nil)
         ;; Now the tricky stuff.
         (let ((predicate (cadr (hairy-type-specifier type))))
           (case predicate
             (keywordp
              (test-keywordp))
             (t
              ;; If the SATISFIES function is not foldable, we cannot answer!
              (dx-let ((form `(,predicate ',obj)))
                (multiple-value-bind (ok result)
                    (sb-c::constant-function-call-p form nil nil)
                  (values (not (null result)) ok))))))))))

;;; :SB-XREF-FOR-INTERNALS hangs on to more symbols. It is not also the intent
;;; to retain all toplevel definitions whether subsequently needed or not.
;;; That's an unfortunate side-effect; this macro is done being used now.
(fmakunbound 'typep-impl-macro)

;;;; miscellaneous interfaces

;;; Clear memoization of all type system operations that can be
;;; altered by type definition/redefinition.
;;;
(defun clear-type-caches ()
  ;; FIXME: We would like to differentiate between different cache
  ;; kinds, but at the moment all our caches pretty much are type
  ;; caches.
  (drop-all-hash-caches)
  (values))

;;; This is like TYPE-OF, only we return a CTYPE structure instead of
;;; a type specifier, and we try to return the type most useful for
;;; type checking, rather than trying to come up with the one that the
;;; user might find most informative.
;;;
;;; To avoid inadvertent memory retention we avoid using arrays
;;; and functions as keys.
;;; During cross-compilation, the CTYPE-OF function is not memoized.
;;; Constants get their type stored in their LEAF, so it's ok.

(defun-cached (ctype-of :hash-bits 7 :hash-function #'sxhash
                        :memoizer memoize)
;; an unfortunate aspect of using EQ is that several appearances
;; of the = double-float can be in the cache, but it's
;; probably more efficient overall to use object identity.
    ((x eq))
  (flet ((try-cache (x)
           (memoize
            ;; For functions, the input is a type specifier
            ;; of the form (FUNCTION (...) ...)
            (cond ((listp x) (specifier-type x)) ; NIL can't occur
                  ((symbolp x) (make-eql-type x))
                  (t (ctype-of-number x))))))
    (typecase x
      (function
       (if (funcallable-instance-p x)
           (classoid-of x)
           (let ((type (sb-impl::%fun-type x)))
             (if (typep type '(cons (eql function))) ; sanity test
                 (try-cache type)
                 (classoid-of x)))))
      (symbol (if x (try-cache x) (specifier-type 'null)))
      (number (try-cache x))
      (array (ctype-of-array x))
      (cons (specifier-type 'cons))
      (character
       (typecase x
         (standard-char (specifier-type 'standard-char))
         (base-char (specifier-type 'base-char))
         ;; If the last case were expressed as EXTENDED-CHAR,
         ;; we wrongly get "this is not a (VALUES CTYPE): NIL"
         ;; because the compiler is too naive to see that
         ;; the last 2 cases partition CHARACTER.
         (t (specifier-type 'extended-char))))
      #+sb-simd-pack
      (simd-pack
       (let ((tag (%simd-pack-tag x)))
         (svref (load-time-value
                 (coerce (cons (specifier-type 'simd-pack)
                               (mapcar (lambda (x) (specifier-type `(simd-pack ,x)))
                                       *simd-pack-element-types*))
                         'vector)
                 t)
                (if (<= 0 tag #.(1- (length *simd-pack-element-types*)))
                    (1+ tag)
                    0))))
      #+sb-simd-pack-256
      (simd-pack-256
       (let ((tag (%simd-pack-256-tag x)))
         (svref (load-time-value
                 (coerce (cons (specifier-type 'simd-pack-256)
                               (mapcar (lambda (x) (specifier-type `(simd-pack-256 ,x)))
                                       *simd-pack-element-types*))
                         'vector)
                 t)
                (if (<= 0 tag #.(1- (length *simd-pack-element-types*)))
                    (1+ tag)
                    0))))
      (t
       (classoid-of x)))))

;; Helper function that implements (CTYPE-OF x) when X is an array.
(defun-cached (ctype-of-array
               :values (ctype) ; Bind putative output to this when probing.
               :hash-bits 7
               :hash-function (lambda (a &aux (hash cookie))
                                (if header-p
                                    (dotimes (axis rank hash)
                                      (mixf hash (%array-dimension a axis)))
                                    (mixf hash (length a)))))
    ;; "type-key" is a perfect hash of rank + widetag + simple-p.
    ;; If it matches, then compare dims, which are read from the output.
    ;; The hash of the type-key + dims can have collisions.
    ((array (lambda (array type-key)
              (and (eq type-key cookie)
                   (let ((dims (array-type-dimensions ctype)))
                     (if header-p
                         (dotimes (axis rank t)
                           (unless (eq (pop (truly-the list dims))
                                       (%array-dimension array axis))
                             (return nil)))
                         (eq (length array) (car dims))))))
            cookie) ; Store COOKIE as the single key.
     &aux (rank (array-rank array))
          (simple-p (if (simple-array-p array) 1 0))
          (header-p (array-header-p array)) ; non-simple or rank <> 1 or both
          (cookie (the fixnum (logior (ash (logior (ash rank 1) simple-p)
                                           sb-vm:n-widetag-bits)
                                      (array-underlying-widetag array)))))
  ;; The value computed on cache miss.
  (let ((etype (specifier-type (array-element-type array))))
    (make-array-type (array-dimensions array)
                     :complexp (not (simple-array-p array))
                     :element-type etype
                     :specialized-element-type etype)))


;;;; Some functions for examining the type system
;;;; which are not needed during self-build.

(defun typexpand-all (type-specifier &optional env)
  "Takes and expands a type specifier recursively like MACROEXPAND-ALL."
  ;; TYPE-SPECIFIER is of type TYPE-SPECIFIER, but it is preferable to
  ;; defer to VALUES-SPECIFIER-TYPE for the check.
  (declare (type lexenv-designator env) (ignore env))
  ;; I first thought this would not be a good implementation because
  ;; it signals an error on e.g. (CONS 1 2) until I realized that
  ;; walking and calling TYPEXPAND would also result in errors, and
  ;; it actually makes sense.
  ;;
  ;; There's still a small problem in that
  ;;   (TYPEXPAND-ALL '(CONS * FIXNUM)) => (CONS T FIXNUM)
  ;; whereas walking+typexpand would result in (CONS * FIXNUM).
  ;;
  ;; Similiarly, (TYPEXPAND-ALL '(FUNCTION (&REST T) *)) => FUNCTION.
  (type-specifier (values-specifier-type type-specifier)))

(defun defined-type-name-p (name &optional env)
  "Returns T if NAME is known to name a type specifier, otherwise NIL."
  (declare (symbol name))
  (declare (ignore env))
  (and (info :type :kind name) t))

(defun valid-type-specifier-p (type-specifier &optional env)
  "Returns T if TYPE-SPECIFIER is a valid type specifier, otherwise NIL.

There may be different metrics on what constitutes a \"valid type
specifier\" depending on context. If this function does not suit your
exact need, you may be able to craft a particular solution using a
combination of DEFINED-TYPE-NAME-P and the TYPEXPAND functions.

The definition of \"valid type specifier\" employed by this function
is based on the following mnemonic:

          \"Would TYPEP accept it as second argument?\"

Except that unlike TYPEP, this function fully supports compound
FUNCTION type specifiers, and the VALUES type specifier, too.

In particular, VALID-TYPE-SPECIFIER-P will return NIL if
TYPE-SPECIFIER is not a class, not a symbol that is known to name a
type specifier, and not a cons that represents a known compound type
specifier in a syntactically and recursively correct way.

Examples:

  (valid-type-specifier-p '(cons * *))     => T
  (valid-type-specifier-p '#:foo)          => NIL
  (valid-type-specifier-p '(cons * #:foo)) => NIL
  (valid-type-specifier-p '(cons 1 *)      => NIL

Experimental."
  (declare (ignore env))
  ;; We don't even care if the spec is parseable -
  ;; just deem it invalid.
  (not (null (ignore-errors
               (type-or-nil-if-unknown type-specifier t)))))
