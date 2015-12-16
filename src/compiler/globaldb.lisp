;;;; This file provides a functional interface to global information
;;;; about named things in the system. Information is considered to be
;;;; global if it must persist between invocations of the compiler. The
;;;; use of a functional interface eliminates the need for the compiler
;;;; to worry about the actual representation. This is important, since
;;;; the information may well have several representations.
;;;;
;;;; The database contains arbitrary Lisp values, addressed by a
;;;; <Name,Info-Number> pair, where Info-Number is identified by
;;;; a <Category,Kind> pair, each being a keyword. The Name is a thing
;;;; which we are recording information about. [Names are compared by EQUAL.]
;;;; Category and Kind create a taxonomy of the data values for a thing.
;;;; For example, '+ names both a function and a variable, so has (at least)
;;;; two categories of information. Within each category, we have several
;;;; pieces of info, and in fact some of these have the same-named :Kind
;;;; such as <:FUNCTION,:TYPE> and <:VARIABLE,:TYPE>.
;;;; (And sometimes the Kind is literally :KIND, as a consequence of
;;;; how users of the database desire to name their keys.)

;;;; The relation between this file and 'info-vectors' is that the
;;;; latter provides a fundamental mechanism to create property-list-like
;;;; things whose "indicators" are restricted to small integers.
;;;; The globaldb abstraction is layered on top of that and is responsible
;;;; for translating <Category,Kind> to a small integer.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

#-no-ansi-print-object
(defmethod print-object ((x meta-info) stream)
  (print-unreadable-object (x stream)
    (format stream "~S ~S, ~D" (meta-info-category x) (meta-info-kind x)
            (meta-info-number x))))

(!begin-collecting-cold-init-forms)
#!+sb-show (!cold-init-forms (/show0 "early in globaldb.lisp cold init"))

;;; This is sorta semantically equivalent to SXHASH, but better-behaved for
;;; legal function names. It performs more work by not cutting off as soon
;;; in the CDR direction, thereby improving the distribution of method names.
;;; More work here equates to less work in the global hashtable.
;;; To wit: (eq (sxhash '(foo a b c bar)) (sxhash '(foo a b c d))) => T
;;; but the corresponding globaldb-sxhashoids differ.
;;; This is no longer inline because for the cases where it is needed -
;;; names which are not just symbols or (SETF F) - an extra call has no impact.
(defun globaldb-sxhashoid (name)
  ;; we can't use MIX because it's in 'target-sxhash',
  ;; so use the host's sxhash, but ensure that the result is a target fixnum.
  #+sb-xc-host (logand (sxhash name) sb!xc:most-positive-fixnum)
  #-sb-xc-host
  (locally
      (declare (optimize (safety 0))) ; after the argc check
    ;; TRAVERSE will walk across more cons cells than RECURSE will descend.
    ;; That's why this isn't just one self-recursive function.
    (labels ((traverse (accumulator x length-limit)
               (declare (fixnum length-limit))
               (cond ((atom x) (sb!int:mix (sxhash x) accumulator))
                     ((zerop length-limit) accumulator)
                     (t (traverse (sb!int:mix (recurse (car x) 4) accumulator)
                                  (cdr x) (1- length-limit)))))
             (recurse (x depthoid) ; depthoid = a blend of level and length
               (declare (fixnum depthoid))
               (cond ((atom x) (sxhash x))
                     ((zerop depthoid)
                      #.(logand sb!xc:most-positive-fixnum #36Rglobaldbsxhashoid))
                     (t (sb!int:mix (recurse (car x) (1- depthoid))
                                    (recurse (cdr x) (1- depthoid)))))))
      (traverse 0 name 10))))

;;; Given any non-negative integer, return a prime number >= to it.
;;;
;;; FIXME: This logic should be shared with ALMOST-PRIMIFY in
;;; hash-table.lisp. Perhaps the merged logic should be
;;; PRIMIFY-HASH-TABLE-SIZE, implemented as a lookup table of primes
;;; after integral powers of two:
;;;    #(17 37 67 131 ..)
;;; (Or, if that's too coarse, after half-integral powers of two.) By
;;; thus getting rid of any need for primality testing at runtime, we
;;; could punt POSITIVE-PRIMEP, too.
(defun primify (x)
  (declare (type unsigned-byte x))
  (do ((n (logior x 1) (+ n 2)))
      ((positive-primep n) n)))

;;; Why do we suppress the :COMPILE-TOPLEVEL situation here when we're
;;; running the cross-compiler? The cross-compiler (which was built
;;; from these sources) has its version of these data and functions
;;; defined in the same places we'd be defining into. We're happy with
;;; its version, since it was compiled from the same sources, so
;;; there's no point in overwriting its nice compiled version of this
;;; stuff with our interpreted version. (And any time we're *not*
;;; happy with its version, perhaps because we've been editing the
;;; sources partway through bootstrapping, tch tch, overwriting its
;;; version with our version would be unlikely to help, because that
;;; would make the cross-compiler very confused.)
(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
(defun !register-meta-info (metainfo)
  (let* ((name (meta-info-kind metainfo))
         (list (!get-meta-infos name)))
    (set-info-value name +info-metainfo-type-num+
                    (cond ((not list) metainfo) ; unique, just store it
                          ((listp list) (cons metainfo list)) ; prepend to the list
                          (t (list metainfo list)))))) ; convert atom to a list

(defun !%define-info-type (category kind type-spec type-checker
                           validate-function default &optional id)
  (awhen (meta-info category kind nil) ; if found
    (when id
      (aver (= (meta-info-number it) id)))
    (return-from !%define-info-type it)) ; do nothing
  (let ((id (or id (position nil *info-types* :start 1)
                   (error "no more INFO type numbers available"))))
    (!register-meta-info
     (setf (aref *info-types* id)
           (!make-meta-info id category kind type-spec type-checker
                            validate-function default)))))

) ; EVAL-WHEN

#-sb-xc
(setf (get '!%define-info-type :sb-cold-funcall-handler/for-effect)
      (lambda (category kind type-spec checker validator default id)
        ;; The SB!FASL: symbols are poor style, but the lesser evil.
        ;; If exported, then they'll stick around in the target image.
        ;; Perhaps SB-COLD should re-export some of these.
        (declare (special sb!fasl::*dynamic* sb!fasl::*cold-layouts*))
        (let ((layout (gethash 'meta-info sb!fasl::*cold-layouts*)))
          (sb!fasl::cold-svset
           (sb!fasl::cold-symbol-value '*info-types*)
           id
           (sb!fasl::write-slots
            (sb!fasl::allocate-struct sb!fasl::*dynamic* layout)
            'meta-info ; give the type name in lieu of layout
            :category category :kind kind :type-spec type-spec
            :type-checker checker :validate-function validator
            :default default :number id)))))

(!cold-init-forms
 (dovector (x (the simple-vector *info-types*))
   ;; Genesis writes the *INFO-TYPES* array, but setting up the mapping
   ;; from keyword-pair to object is deferred until cold-init.
   (when x (!register-meta-info x))))

;;;; info types, and type numbers, part II: what's
;;;; needed only at compile time, not at run time

;;; Define a new type of global information.
;;; CATEGORY/KIND form a two-part name for the piece of information,
;;; DEFAULT is a defaulting expression, and TYPE-SPEC
;;; is a type specifier which data values must satisfy.
;;; Roughly speaking there is a hierarchy to the two-piece names
;;; but this is a fiction that is not maintained anywhere in the internals.
;;;
;;; If the defaulting expression's value is a function, it is called with
;;; the name for which the information is being looked up; otherwise it is
;;; taken as the default value. The defaulting expression is used each time
;;; a value is needed when one hasn't been previously set. (The result
;;; does not automatically become the new value for the piece of info.)
;;; Should a default value be itself a function, this must be expressed as
;;;  :DEFAULT (CONSTANTLY #'<a-function-name>) to adhere to the convention
;;; that default objects satisfying FUNCTIONP will always be funcalled.
;;;
(eval-when (:compile-toplevel :execute)
;; This convoluted idiom creates a macro that disappears from the target,
;; kind of an alternative to the "!" name convention.
(#+sb-xc-host defmacro
 #-sb-xc-host sb!xc:defmacro
    define-info-type ((category kind)
                       &key (type-spec (missing-arg))
                            (validate-function)
                            default)
  (declare (type keyword category kind))
  ;; There was formerly a remark that (COPY-TREE TYPE-SPEC) ensures repeatable
  ;; fasls. That's not true now, probably never was. A compiler is permitted to
  ;; coalesce EQUAL quoted lists and there's no defense against it, so why try?
  (let ((form
         `(!%define-info-type
           ,category ,kind ',type-spec
           ,(cond ((eq type-spec 't) '#'identity)
                  ;; evil KLUDGE to avoid "undefined type" warnings
                  ;; when building the cross-compiler.
                  #+sb-xc-host
                  ((member type-spec
                           '((or fdefn null)
                             (or alien-type null) (or heap-alien-info null))
                           :test 'equal)
                   `(lambda (x)
                      (declare (notinline typep))
                      (if (typep x ',type-spec)
                          x
                          (error "~S is not a ~S" x ',type-spec))))
                  (t
                   `(named-lambda "check-type" (x) (the ,type-spec x))))
           ,validate-function ,default
           ;; Rationale for hardcoding here is explained at INFO-VECTOR-FDEFN.
           ,(or (and (eq category :function) (eq kind :definition)
                     +fdefn-info-num+)
                #+sb-xc (meta-info-number (meta-info category kind))))))
    `(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute) ,form))))


(macrolet ((meta-info-or-lose (category kind)
             ;; don't need to type-check META-INFO's result, since it
             ;; defaults to signaling an error if no meta-info found.
             `(truly-the meta-info (meta-info ,category ,kind))))
;;; INFO is the standard way to access the database. It's settable.
;;;
;;; Return the information of the specified CATEGORY and KIND for NAME.
;;; The second value returned is true if there is any such information
;;; recorded. If there is no information, the first value returned is
;;; the default and the second value returned is NIL.
  (defun info (category kind name)
    (let ((info (meta-info category kind)))
      (get-info-value name (meta-info-number info))))

  (defun (setf info) (new-value category kind name)
    (let ((info (meta-info category kind)))
      (funcall (meta-info-type-checker info) new-value)
      (awhen (meta-info-validate-function info)
        (funcall it name new-value))
      (set-info-value name (meta-info-number info) new-value)))

  ;; Clear the information of the specified CATEGORY and KIND for NAME in
  ;; the current environment. Return true if there was any info.
  (defun clear-info (category kind name)
    (let* ((info (meta-info category kind))
           (info-number-list (list (meta-info-number info))))
      (declare (dynamic-extent info-number-list))
      (clear-info-values name info-number-list))))

(defun clear-info-values (name info-numbers)
  (dolist (type info-numbers)
    (aver (and (typep type 'info-number) (svref *info-types* type))))
  ;; A call to UNCROSS was suspiciously absent, so I added this ERROR
  ;; to be certain that it's not supposed to happen when building the xc.
  #+sb-xc-xhost (error "Strange CLEAR-INFO building the xc: ~S ~S"
                       name info-numbers)
  (let (new)
    (with-globaldb-name (key1 key2) name
      :simple
      ;; If PACKED-INFO-REMOVE has nothing to do, it returns NIL,
      ;; corresponding to the input that UPDATE-SYMBOL-INFO expects.
      (dx-flet ((clear-simple (old)
                  (setq new (packed-info-remove old key2 info-numbers))))
        (update-symbol-info key1 #'clear-simple))
      :hairy
      ;; The global hashtable is not imbued with knowledge of the convention
      ;; for PACKED-INFO-REMOVE because that would render it less useful
      ;; as a general-purpose global hashtable for other kinds of stuff
      ;; that I might want it to store aside from packed infos.
      ;; So here UPDATE might receive NIL but must not return NIL if
      ;; there was a non-nil input. NIL doesn't mean "do nothing".
      (dx-flet ((clear-hairy (old)
                  (if old
                      ;; if -REMOVE => nil, then update NEW but return OLD
                      (or (setq new (packed-info-remove
                                     old +no-auxilliary-key+ info-numbers))
                          old))))
        (info-puthash *info-environment* name #'clear-hairy)))
    (not (null new))))

;;;; *INFO-ENVIRONMENT*

(!cold-init-forms
  (setq *info-environment* (make-info-hashtable))
  (/show0 "done setting *INFO-ENVIRONMENT*"))

;;;; GET-INFO-VALUE

;;; If non-nil, *GLOBALDB-OBSERVER*'s CAR is a bitmask over info numbers
;;; for which you'd like to call the function in the CDR whenever info
;;; of that number is queried.
(!defvar *globaldb-observer* nil)
(declaim (type (or (cons (unsigned-byte #.(ash 1 info-number-bits)) function)
                   null) *globaldb-observer*))
#-sb-xc-host (declaim (always-bound *globaldb-observer*))

;;; Return the value of NAME / INFO-NUMBER from the global environment,
;;; or return the default if there is no global info.
;;; The secondary value indicates whether info was found vs defaulted.
(declaim (ftype (sfunction (t info-number) (values t boolean))
                get-info-value))
(defun get-info-value (name info-number)
  (let* ((hook *globaldb-observer*)
         (hookp (and (and hook
                          (not (eql 0 (car hook)))
                          (logbitp info-number (car hook))))))
    (multiple-value-bind (vector aux-key)
        (let ((name (uncross name)))
          (with-globaldb-name (key1 key2) name
           ;; In the :simple branch, KEY1 is no doubt a symbol,
           ;; but constraint propagation isn't informing the compiler here.
           :simple (values (symbol-info-vector (truly-the symbol key1)) key2)
           :hairy (values (info-gethash name *info-environment*)
                          +no-auxilliary-key+)))
      (when vector
        (let ((index (packed-info-value-index vector aux-key info-number)))
          (when index
            (let ((answer (svref vector index)))
              (when hookp
                (funcall (truly-the function (cdr hook))
                         name info-number answer t))
              (return-from get-info-value (values answer t)))))))
    (let* ((def (meta-info-default (aref *info-types* info-number)))
           (answer (if (functionp def) (funcall def name) def)))
      (when hookp
        (funcall (truly-the function (cdr hook)) name info-number answer nil))
      (values answer nil))))

;; interface to %ATOMIC-SET-INFO-VALUE
;; GET-INFO-VALUE-INITIALIZING is a restricted case of this,
;; and perhaps could be implemented as such.
;; Atomic update will be important for making the fasloader threadsafe
;; using a predominantly lock-free design, and other nice things.
(def!macro atomic-set-info-value (category kind name lambda)
  (with-unique-names (info-number proc)
    `(let ((,info-number
            ,(if (and (keywordp category) (keywordp kind))
                 (meta-info-number (meta-info category kind))
                 `(meta-info-number (meta-info ,category ,kind)))))
       ,(if (and (listp lambda) (eq (car lambda) 'lambda))
            ;; rewrite as FLET because the compiler is unable to dxify
            ;;   (DX-LET ((x (LAMBDA <whatever>))) (F x))
            (destructuring-bind (lambda-list . body) (cdr lambda)
              `(dx-flet ((,proc ,lambda-list ,@body))
                 (%atomic-set-info-value ,name ,info-number #',proc)))
            `(%atomic-set-info-value ,name ,info-number ,lambda)))))

;; Call FUNCTION once for each Name in globaldb that has information associated
;; with it, passing the function the Name as its only argument.
;;
(defun call-with-each-globaldb-name (fun-designator)
  (let ((function (coerce fun-designator 'function)))
    (with-package-iterator (iter (list-all-packages) :internal :external)
      (loop (multiple-value-bind (winp symbol access package) (iter)
              (declare (ignore access))
              (if (not winp) (return))
              ;; Try to process each symbol at most once by associating it with
              ;; a single package. If a symbol is apparently uninterned,
              ;; always keep it since we can't know if it has been seen once.
              (when (or (not (symbol-package symbol))
                        (eq package (symbol-package symbol)))
                (dolist (name (info-vector-name-list symbol))
                  (funcall function name))))))
    (info-maphash (lambda (name data)
                    (declare (ignore data))
                    (funcall function name))
                  *info-environment*)))

;;;; ":FUNCTION" subsection - Data pertaining to globally known functions.

(define-info-type (:function :definition) :type-spec (or fdefn null))

;;; the kind of functional object being described. If null, NAME isn't
;;; a known functional object.
(define-info-type (:function :kind)
  :type-spec (member nil :function :macro :special-form)
  ;; I'm a little confused what the correct behavior of this default
  ;; is. It's not clear how to generalize the FBOUNDP expression to
  ;; the cross-compiler. As far as I can tell, NIL is a safe default
  ;; -- it might keep the compiler from making some valid
  ;; optimization, but it shouldn't produce incorrect code. -- WHN
  ;; 19990330
  :default
  #+sb-xc-host nil
  #-sb-xc-host (lambda (name) (if (fboundp name) :function nil)))

;;; Indicates whether the function is deprecated.
(define-info-type (:function :deprecated)
  :type-spec (or null deprecation-info))

(declaim (ftype (sfunction (t) ctype)
                specifier-type ctype-of sb!kernel::ctype-of-array))

(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
(defun ftype-from-fdefn (name)
  (declare (ignorable name))
  ;; Again [as in (DEFINE-INFO-TYPE (:FUNCTION :TYPE) ...)] it's
  ;; not clear how to generalize the FBOUNDP expression to the
  ;; cross-compiler. -- WHN 19990330
  #+sb-xc-host
  (specifier-type 'function)
  #-sb-xc-host
  (let* ((fdefn (sb!kernel::find-fdefn name))
         (fun (and fdefn (fdefn-fun fdefn))))
    (if fun
        (handler-bind ((style-warning #'muffle-warning))
          (specifier-type (sb!impl::%fun-type fun)))
        (specifier-type 'function)))))

;;; The type specifier for this function, or a DEFSTRUCT-DESCRIPTION
;;; or the symbol :GENERIC-FUNTION.
;;; If a DD, it must contain a constructor whose name is
;;; the one being sought in globaldb, which is used to derive the type.
;;; If :GENERIC-FUNCTION, the info is recomputed from existing methods
;;; and stored back into globaldb.
(define-info-type (:function :type)
  :type-spec (or ctype defstruct-description (member :generic-function))
  :default #'ftype-from-fdefn)

;;; the ASSUMED-TYPE for this function, if we have to infer the type
;;; due to not having a declaration or definition
(define-info-type (:function :assumed-type)
  ;; FIXME: The type-spec really should be
  ;;   (or approximate-fun-type null)).
  ;; It was changed to T as a hopefully-temporary hack while getting
  ;; cold init problems untangled.
  :type-spec t)

;;; where this information came from:
;;;    :ASSUMED  = from uses of the object
;;;    :DEFINED  = from examination of the definition
;;;    :DEFINED-METHOD = implicit, incremental declaration by CLOS.
;;;    :DECLARED = from a declaration
;;; :DEFINED trumps :ASSUMED, :DEFINED-METHOD trumps :DEFINED,
;;; and :DECLARED trumps :DEFINED-METHOD.
;;; :DEFINED and :ASSUMED are useful for issuing compile-time warnings,
;;; :DEFINED-METHOD and :DECLARED are useful for ANSIly specializing
;;; code which implements the function, or which uses the function's
;;; return values.
(define-info-type (:function :where-from)
  :type-spec (member :declared :defined-method :assumed :defined)
  :default
  ;; Again [as in (DEFINE-INFO-TYPE (:FUNCTION :KIND) ...)] it's
  ;; not clear how to generalize the FBOUNDP expression to the
  ;; cross-compiler. -- WHN 19990606
  #+sb-xc-host :assumed
  #-sb-xc-host (lambda (name) (if (fboundp name) :defined :assumed)))

;;; something which can be decoded into the inline expansion of the
;;; function, or NIL if there is none
;;;
;;; To inline a function, we want a lambda expression, e.g.
;;; '(LAMBDA (X) (+ X 1)).
(define-info-type (:function :inline-expansion-designator)
  :type-spec list)

;;; This specifies whether this function may be expanded inline. If
;;; null, we don't care.
(define-info-type (:function :inlinep) :type-spec inlinep)

;;; Track how many times IR2 converted a call to this function as a full call
;;; that was not in the scope of a local or global notinline declaration.
;;; Useful for finding functions that were supposed to have been converted
;;; through some kind of transformation but were not.
(define-info-type (:function :emitted-full-calls) :type-spec list)

;;; a macro-like function which transforms a call to this function
;;; into some other Lisp form. This expansion is inhibited if inline
;;; expansion is inhibited.
;;; As an exception, a cons of two atoms represents structure metadata
;;; which is recognized and transformed in a stylized way.
;;;
;;; This item is almost mutually exclusive with an inline expansion,
;;; but both are possible in the rare case of a system-defined transform
;;; that may decline to expand. If it does, an inline expansion could win.
;;; We don't actually have anything like that any more though.
;;; For user-defined functions, the invariant is maintained that at most
;;; one of :source-transform and an inline-expansion exist.
(define-info-type (:function :source-transform)
  :type-spec (or function null (cons atom atom)))

;;; the macroexpansion function for this macro
(define-info-type (:function :macro-function) :type-spec (or function null))

;;; the compiler-macroexpansion function for this function or macro
(define-info-type (:function :compiler-macro-function)
  :type-spec (or function null))

;;; a function which converts this special form into IR1
(define-info-type (:function :ir1-convert) :type-spec (or function null))

;;; If a function is "known" to the compiler, then this is a FUN-INFO
;;; structure containing the info used to special-case compilation.
(define-info-type (:function :info) :type-spec (or fun-info null))

;;; This is a type specifier <t> such that if an argument X to the function
;;; does not satisfy (TYPEP x <t>) then the function definitely returns NIL.
;;; When the named function is a predicate that appears in (SATISFIES p)
;;; specifiers, it is possible for type operations to see into the predicate
;;; just enough to determine that something like
;;;   (AND (SATISFIES UNINTERESTING-METHOD-REDEFINITION-P) RATIONAL)
;;; is *empty-type*, which in turn avoids type cache pollution.
(define-info-type (:function :predicate-truth-constraint) :type-spec t)

;;;; ":VARIABLE" subsection - Data pertaining to globally known variables.

;;; the kind of variable-like thing described
(define-info-type (:variable :kind)
  :type-spec (member :special :constant :macro :global :alien :unknown)
  :default (lambda (name)
             (if (typep name '(or boolean keyword))
                 :constant
                 :unknown)))

(define-info-type (:variable :always-bound)
  :type-spec (member nil :eventually :always-bound))

(define-info-type (:variable :deprecated)
  :type-spec (or null deprecation-info))

;;; the declared type for this variable
(define-info-type (:variable :type)
  :type-spec ctype
  ;; This gets set to *UNIVERSAL-TYPE* in 'late-type'
  :default (lambda (x) (declare (ignore x)) (error "Too early for INFO")))

;;; where this type and kind information came from
(define-info-type (:variable :where-from)
  :type-spec (member :declared :assumed :defined) :default :assumed)

;;; the macro-expansion for symbol-macros
(define-info-type (:variable :macro-expansion) :type-spec t)

(define-info-type (:variable :alien-info)
  :type-spec (or heap-alien-info null))

(define-info-type (:variable :documentation) :type-spec (or string null))

;; :WIRED-TLS describes how SYMBOL-VALUE (implicit or not) should be compiled.
;;  - :ALWAYS-HAS-TLS means that calls to SYMBOL-VALUE should access the TLS
;;     with a fixed offset. The index is assigned no later than load-time of
;;     the file containing code thus compiled. Presence of an index in the
;;     image that performed compilation is irrelevant (for now).
;;  - :ALWAYS-THREAD-LOCAL implies a fixed offset, *and* that the check for
;;     no-tls-value may be elided. There is currently no way to set this.
;;     Note that this does not affect elision of the check for unbound-marker
;;     which is under control of the :ALWAYS-BOUND info.
;;  - an integer is a permanent index, and also implies :ALWAYS-THREAD-LOCAL.
;; Specials in the CL package (notably reader/printer controls) use a wired-tls,
;; whether or not we bind per-thread [if we don't, that's a bug!]
;; We don't assume wired TLS more generally, because user code often defines
;; thousands of DEFVARs, possibly due to poor style, or due to ANSI's stance
;; that DEFCONSTANT is only for EQL-comparable objects. In such cases with
;; more symbols than can be bound per-thread, the compiler won't exacerbate
;; things by making the loader eagerly assign a TLS index to every symbol
;; ever referenced by SYMBOL-VALUE or SET. Depletion should occur lazily.
;;
(define-info-type (:variable :wired-tls)
    :type-spec (or (member nil :always-has-tls :always-thread-local)
                   fixnum) ; the actual index, for thread slots (to be done)
    :default
    (lambda (symbol)
      (declare (symbol symbol))
      (and (eq (info :variable :kind symbol) :special)
           #-sb-xc-host
           (eq (symbol-package symbol) *cl-package*)
           #+sb-xc-host
           (flet ((external-in-package-p (pkg)
                    (and (string= (package-name (symbol-package symbol)) pkg)
                         (eq (nth-value 1 (find-symbol (string symbol) pkg))
                             :external))))
             ;; I'm not worried about random extra externals in some bizarro
             ;; host lisp. TLS assignment has no bearing on semantics at all.
             (or (external-in-package-p "COMMON-LISP")
                 (external-in-package-p "SB-XC")))
           :always-has-tls)))

;;;; ":TYPE" subsection - Data pertaining to globally known types.

;;; the kind of type described. We return :INSTANCE for standard types
;;; that are implemented as structures. For PCL classes, that have
;;; only been compiled, but not loaded yet, we return
;;; :FORTHCOMING-DEFCLASS-TYPE.
;;; The only major distinction between :PRIMITIVE and :DEFINED
;;; is how badly the system complains about attempted redefinition.
(define-info-type (:type :kind)
  :type-spec (member :primitive :defined :instance
                     :forthcoming-defclass-type nil)
  :validate-function (lambda (name new-value)
                       (declare (ignore new-value))
                       ;; The compiler-macro signals an error
                       ;; on forward-referenced info-types.
                       #+sb-xc-host (declare (notinline info))
                       (when (info :declaration :recognized name)
                         (error 'declaration-type-conflict-error
                                :format-arguments (list name)))))

(define-info-type (:type :documentation) :type-spec (or string null))

;;; The expander function for a defined type,
;;; or a cons whose CAR is a function which is a builtin type translator.
(define-info-type (:type :expander) :type-spec (or function list))

;;; If non-nil, then the type coresponding to this name. Note that if
;;; this is a built-in class with a translation, then this is the
;;; translation, not the class object. This info type keeps track of
;;; various atomic types (NIL etc.) and also serves as a means to
;;; ensure that common standard types are only consed once.
(define-info-type (:type :builtin) :type-spec (or ctype null))

;;; The classoid-cell for this type
(define-info-type (:type :classoid-cell) :type-spec t)

(defun find-classoid-cell (name &key create)
  (let ((real-name (uncross name)))
    (cond ((info :type :classoid-cell real-name))
          (create
           (get-info-value-initializing
            :type :classoid-cell real-name
            (sb!kernel::make-classoid-cell real-name))))))

;;; Return the classoid with the specified NAME. If ERRORP is false,
;;; then NIL is returned when no such class exists.
(defun find-classoid (name &optional (errorp t))
  (declare (type symbol name))
  (let ((cell (find-classoid-cell name)))
    (cond ((and cell (classoid-cell-classoid cell)))
          (errorp
           (error 'simple-type-error
                  :datum nil
                  :expected-type 'class
                  :format-control "Class not yet defined: ~S"
                  :format-arguments (list name))))))

;;; layout for this type being used by the compiler
(define-info-type (:type :compiler-layout)
  :type-spec (or layout null)
  :default (lambda (name)
             (let ((class (find-classoid name nil)))
               (when class (classoid-layout class)))))

;;; DEFTYPE lambda-list
;; FIXME: remove this after making swank-fancy-inspector not use it.
(define-info-type (:type :lambda-list) :type-spec t)

(define-info-type (:type :source-location) :type-spec t)

;;; Indicates whether the type is deprecated.
(define-info-type (:type :deprecated)
  :type-spec (or null deprecation-info))

;;;; ":TYPED-STRUCTURE" subsection.
;;;; Data pertaining to structures that used DEFSTRUCT's :TYPE option.
(define-info-type (:typed-structure :info) :type-spec t)
(define-info-type (:typed-structure :documentation) :type-spec (or string null))

;;;; ":DECLARATION" subsection - Data pertaining to user-defined declarations.
;; CLTL2 offers an API to provide a list of known declarations, but it is
;; inefficient to iterate over all symbols to find ones which have the
;; (:DECLARATION :RECOGNIZED) info.
;; Therefore maintain a list of recognized declarations. This list makes the
;; globaldb storage of same redundant, but oh well.
(defglobal *recognized-declarations* nil)
(define-info-type (:declaration :recognized)
  :type-spec boolean
  ;; There's no portable way to unproclaim that a symbol is a declaration,
  ;; but at the low-level permit new-value to be NIL.
  :validate-function (lambda (name new-value)
                       (declare (symbol name))
                       (cond (new-value
                              (when (info :type :kind name)
                                (error 'declaration-type-conflict-error
                                       :format-arguments (list name)))
                              (pushnew name *recognized-declarations*))
                             (t
                              (setq *recognized-declarations*
                                    (delete name *recognized-declarations*))))))

(define-info-type (:declaration :handler) :type-spec (or function null))

;;;; ":ALIEN-TYPE" subsection - Data pertaining to globally known alien-types.
(define-info-type (:alien-type :kind)
  :type-spec (member :primitive :defined :unknown)
  :default :unknown)
(define-info-type (:alien-type :translator) :type-spec (or function null))
(define-info-type (:alien-type :definition) :type-spec (or alien-type null))
(define-info-type (:alien-type :struct) :type-spec (or alien-type null))
(define-info-type (:alien-type :union) :type-spec (or alien-type null))
(define-info-type (:alien-type :enum) :type-spec (or alien-type null))

;;;; ":SETF" subsection - Data pertaining to expansion of the omnipotent macro.
(define-info-type (:setf :inverse) :type-spec (or symbol null))
(define-info-type (:setf :documentation) :type-spec (or string null))
(define-info-type (:setf :expander)
    :type-spec (or function (cons integer function) null))

;;;; ":CAS" subsection - Like SETF but there are no "inverses", just expanders
(define-info-type (:cas :expander) :type-spec (or function null))

;;;; ":RANDOM-DOCUMENTATION" subsection.
;;; This is used for storing miscellaneous documentation types. The
;;; stuff is an alist translating documentation kinds to values.
(define-info-type (:random-documentation :stuff) :type-spec list)

;;;; ":SOURCE-LOCATION" subsection.
;;; This is kind of the opposite of what I'd have thought more logical,
;;; where each of the above categories has one of its kinds of information
;;; being :SOURCE-LOCATION.
;;; And in fact that *is* how :TYPE was handled. However, many global entities
;;; store their source-location hanging off some other hook, avoiding the
;;; globaldb entirely, such as functions using a #<code-component>.
;;; So either way is basically a hodgepodge.

(define-info-type (:source-location :variable) :type-spec t)
(define-info-type (:source-location :constant) :type-spec t)
(define-info-type (:source-location :typed-structure) :type-spec t)
(define-info-type (:source-location :symbol-macro) :type-spec t)
(define-info-type (:source-location :vop) :type-spec t)
(define-info-type (:source-location :declaration) :type-spec t)
(define-info-type (:source-location :alien-type) :type-spec t)

;; This is for the SB-INTROSPECT contrib module, and debugging.
(defun call-with-each-info (function symbol)
  (awhen (symbol-info-vector symbol)
    (%call-with-each-info function it symbol)))

;; This is for debugging at the REPL.
(defun show-info (sym)
  (let ((prev 0))
    (call-with-each-info
     (lambda (name type-num val)
       (unless (eq name prev)
         (format t "~&~S" (setq prev name)))
       (let ((type (svref *info-types* type-num)))
         (format t "~&  ~@[type ~D~]~@[~{~S ~S~}~] = "
                 (if (not type) type-num)
                 (if type
                     (list (meta-info-category type) (meta-info-kind type))))
         (write val :level 1)))
     sym)))

(!defun-from-collected-cold-init-forms !globaldb-cold-init)
