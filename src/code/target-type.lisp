;;;; type-related stuff which exists only in the target SBCL runtime

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

;;; If TYPE is a type that we can do a compile-time test on, then
;;; return whether the object is of that type as the first value and
;;; second value true. Otherwise return NIL, NIL.
;;;
;;; We give up on unknown types and pick off FUNCTION- and COMPOUND-
;;; types. For STRUCTURE- types, we require that the type be defined
;;; in both the current and compiler environments, and that the
;;; INCLUDES be the same.
;;;
;;; KLUDGE: This should probably be a type method instead of a big
;;; ETYPECASE. But then the type method system should probably be CLOS
;;; too, and until that happens wedging more stuff into it might be
;;; messy. So I've left it a big ETYPECASE. -- 2001-03-16
(defun ctypep (obj type)
  (declare (type ctype type))
  (etypecase type
    ((or numeric-type
         named-type
         member-type
         array-type
         character-set-type
         built-in-classoid
         cons-type
         #!+sb-simd-pack simd-pack-type)
     (values (%%typep obj type) t))
    (classoid
     (if (if (csubtypep type (specifier-type 'function))
             (funcallable-instance-p obj)
             (%instancep obj))
         (if (eq (classoid-layout type)
                 (info :type :compiler-layout (classoid-name type)))
             (values (sb!xc:typep obj type) t)
             (values nil nil))
         (values nil t)))
    (compound-type
     (funcall (etypecase type
                (intersection-type #'every/type)
                (union-type #'any/type))
              #'ctypep
              obj
              (compound-type-types type)))
    (fun-type
     (values (functionp obj) t))
    (unknown-type
     (values nil nil))
    (alien-type-type
     (values (alien-typep obj (alien-type-type-alien-type type)) t))
    (negation-type
     (multiple-value-bind (res win)
         (ctypep obj (negation-type-type type))
       (if win
           (values (not res) t)
           (values nil nil))))
    (hairy-type
     ;; Now the tricky stuff.
     (let* ((hairy-spec (hairy-type-specifier type))
            (symbol (if (consp hairy-spec) (car hairy-spec) hairy-spec)))
       (ecase symbol
         (and
          (if (atom hairy-spec)
              (values t t)
              (dolist (spec (cdr hairy-spec) (values t t))
                (multiple-value-bind (res win)
                    (ctypep obj (specifier-type spec))
                  (unless win (return (values nil nil)))
                  (unless res (return (values nil t)))))))
         (not
          (multiple-value-bind (res win)
              (ctypep obj (specifier-type (cadr hairy-spec)))
            (if win
                (values (not res) t)
                (values nil nil))))
         (satisfies
          ;; If the SATISFIES function is not foldable, we cannot answer!
          (let* ((form `(,(second hairy-spec) ',obj)))
            (multiple-value-bind (ok result)
                (sb!c::constant-function-call-p form nil nil)
              (values (not (null result)) ok)))))))))

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
           (let ((type (sb!impl::%fun-type x)))
             (if (typep type '(cons (eql function))) ; sanity test
                 (try-cache type)
                 (classoid-of x)))))
      (symbol (if x (try-cache x) *null-type*))
      (number (try-cache x))
      (array (ctype-of-array x))
      (cons *cons-t-t-type*)
      ;; This makes no distinction for BASE/EXTENDED-CHAR. Should it?
      (character *character-type*)
      #!+sb-simd-pack
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
                                           sb!vm:n-widetag-bits)
                                      (array-underlying-widetag array)))))
  ;; The value computed on cache miss.
  (let ((etype (specifier-type (array-element-type array))))
    (make-array-type (array-dimensions array)
                     :complexp (not (simple-array-p array))
                     :element-type etype
                     :specialized-element-type etype)))

(!defun-from-collected-cold-init-forms !target-type-cold-init)

;;;; Some functions for examining the type system
;;;; which are not needed during self-build.

(defun typexpand-all (type-specifier &optional env)
  #!+sb-doc
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
  #!+sb-doc
  "Returns T if NAME is known to name a type specifier, otherwise NIL."
  (declare (symbol name))
  (declare (ignore env))
  (and (info :type :kind name) t))

(defun valid-type-specifier-p (type-specifier &optional env)
  #!+sb-doc
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
  (handler-case (prog1 t (values-specifier-type type-specifier))
    (parse-unknown-type () nil)
    (error () nil)))
