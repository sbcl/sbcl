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

(in-package "SB-IMPL")

(defmethod print-object ((x meta-info) stream)
  (print-unreadable-object (x stream)
    (format stream "~S ~S, ~D" (meta-info-category x) (meta-info-kind x)
            (meta-info-number x))))


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
(defun register-meta-info (metainfo)
  (let* ((name (meta-info-kind metainfo))
         (list (get-meta-infos name)))
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
    (register-meta-info
     (setf (aref *info-types* id)
           (!make-meta-info id category kind type-spec type-checker
                            validate-function default)))))


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
(defmacro define-info-type ((category kind)
                            &key (type-spec (missing-arg))
                                 (validate-function)
                                 default)
  (declare (type keyword category kind))
  ;; There was formerly a remark that (COPY-TREE TYPE-SPEC) ensures repeatable
  ;; fasls. That's not true now, probably never was. A compiler is permitted to
  ;; coalesce EQUAL quoted lists and there's no defense against it, so why try?
  `(!cold-init-forms
    (!%define-info-type
     ,category ,kind ',type-spec
     ,(if (eq type-spec 't)
          '#'identity
          `(named-lambda "check-type" (x) (the ,type-spec x)))
     ,validate-function ,default
     ;; Rationale for hardcoding here is explained at PACKED-INFO-FDEFN.
     ,(or (and (eq category :function) (eq kind :definition)
               +fdefn-info-num+)
          #+sb-xc (meta-info-number (meta-info category kind))))))
;; It's an external symbol of SB-INT so wouldn't be removed automatically
(push '("SB-INT" define-info-type) *!removable-symbols*)


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
  (when (pcl-methodfn-name-p name)
    (error "Can't SET-INFO-VALUE on PCL-internal function"))
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
                                     old +no-auxiliary-key+ info-numbers))
                          old))))
        (info-puthash *info-environment* name #'clear-hairy)))
    (not (null new))))

;;;; GET-INFO-VALUE

;;; If non-nil, *GLOBALDB-OBSERVER*'s CAR is a bitmask over info numbers
;;; for which you'd like to call the function in the CDR whenever info
;;; of that number is queried.
(defvar *globaldb-observer* nil)
(declaim (type (or (cons (unsigned-byte #.(ash 1 info-number-bits)) function)
                   null) *globaldb-observer*))
#-sb-xc-host (declaim (always-bound *globaldb-observer*))

#+sb-xc-host
(progn
  (defun info-gethash (key table) (gethash key table))
  (defun info-puthash (table key augmenter)
    (let ((old (gethash key table)))
      (setf (gethash key table) (funcall augmenter old)))))

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
    (multiple-value-bind (packed-info aux-key)
        (let ((name (uncross name)))
          (with-globaldb-name (key1 key2) name
           ;; In the :simple branch, KEY1 is no doubt a symbol,
           ;; but constraint propagation isn't informing the compiler here.
           :simple (values (symbol-dbinfo (truly-the symbol key1)) key2)
           :hairy (values (info-gethash name *info-environment*)
                          +no-auxiliary-key+)))
      (when packed-info
        (let ((index (packed-info-value-index packed-info aux-key info-number)))
          (when index
            (let ((answer (%info-ref packed-info index)))
              (when hookp
                (funcall (truly-the function (cdr hook))
                         name info-number answer t))
              (return-from get-info-value (values answer t)))))))
    (let* ((def (meta-info-default (aref *info-types* info-number)))
           (answer (if (functionp def) (funcall def name) def)))
      (when hookp
        (funcall (truly-the function (cdr hook)) name info-number answer nil))
      (values answer nil))))

(!begin-collecting-cold-init-forms)
;;;; ":FUNCTION" subsection - Data pertaining to globally known functions.
(define-info-type (:function :definition) :type-spec #-sb-xc-host (or fdefn null) #+sb-xc-host t)

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
  #-sb-xc-host (lambda (name)
                 (if (or (fboundp name) (pcl-methodfn-name-p name))
                     :function
                     nil)))

;;; The deferred mode processor for fasteval special operators.
;;; Immediate processors are hung directly off symbols in a dedicated slot.
#+sb-fasteval
(define-info-type (:function :interpreter) :type-spec (or function null))

;;; Indicates whether the function is deprecated.
(define-info-type (:function :deprecated)
  :type-spec (or null deprecation-info))

;;; FIXME: Why are these here? It seems like the wrong place.
(declaim (ftype (sfunction (t &optional t symbol) ctype) specifier-type)
         (ftype (sfunction (t) ctype) ctype-of sb-kernel::ctype-of-array))

;;; The parsed or unparsed type for this function, or the symbol :GENERIC-FUNCTION.
;;; Ordinarily a parsed type is stored. Only if the parsed type contains
;;; an unknown type will the original specifier be stored; we attempt to reparse
;;; on each lookup, in the hope that the type becomes known at some point.
;;; If :GENERIC-FUNCTION, the info is recomputed from methods at the time of lookup
;;; and stored back. Method redefinition resets the value to :GENERIC-FUNCTION.
(define-info-type (:function :type)
  :type-spec (or ctype (cons (eql function)) (member :generic-function))
  :default (lambda (name)
             (declare (ignorable name))
             #+sb-xc-host (specifier-type 'function)
             #-sb-xc-host (sb-impl::ftype-from-fdefn name)))

;;; the ASSUMED-TYPE for this function, if we have to infer the type
;;; due to not having a declaration or definition
;;; FIXME: It may be better to have this and/or :TYPE stored in a single
;;; property as either (:known . #<ctype>) or (:assumed . #<ctype>)
;;; rather than using two different properties. Do we ever use *both* ?
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

;;; Two kinds of hints for compiling calls as efficiently as possible:
;;; (A) Inline expansion: To inline a function, we want a lambda
;;; expression, e.g. '(LAMBDA (X) (+ X 1)) or a lambda-with-lexenv.
;;; (B) List of arguments which could be dynamic-extent closures, and which
;;; we could, under suitable compilation policy, DXify in the caller
;;; especially when open-coding a call to this function.
;;; If only (A) is stored, then this value is a list (the lambda expression).
;;; If only (B) is stored, then this is a DXABLE-ARGS.
;;; If both, this is an INLINING-DATA.
(define-info-type (:function :inlining-data)
  :type-spec (or list sb-c::dxable-args sb-c::inlining-data))

;;; This specifies whether this function may be expanded inline. If
;;; null, we don't care.
(define-info-type (:function :inlinep) :type-spec sb-c::inlinep)

;;; Track how many times IR2 converted a call to this function as a full call
;;; that was not in the scope of a local or global notinline declaration.
;;; Useful for finding functions that were supposed to have been converted
;;; through some kind of transformation but were not.
(define-info-type (:function :emitted-full-calls) :type-spec list)

;; Return the number of calls to NAME that IR2 emitted as full calls,
;; not counting calls via #'F that went untracked.
;; Return 0 if the answer is nonzero but a warning was already signaled
;; about any full calls were emitted. This return convention satisfies the
;; intended use of this statistic - to decide whether to generate a warning
;; about failure to inline NAME, which is shown at most once per name
;; to avoid unleashing a flood of identical warnings.
(defun emitted-full-call-count (name)
  (let ((status (car (info :function :emitted-full-calls name))))
     (and (integerp status)
          ;; Bit 0 tells whether any call was NOT in the presence of
          ;; a 'notinline' declaration, thus eligible to be inline.
          ;; Bit 1 tells whether any warning was emitted yet.
          (= (logand status 3) #b01)
          (ash status -2)))) ; the call count as tracked by IR2

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
;;; However, there is one exception: a structure constructor can have an
;;; inline expansion and also store (#<dd> . :constructor) here.
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
(define-info-type (:function :info) :type-spec (or sb-c::fun-info null))

;;; For PCL code walker
(define-info-type (:function :walker-template) :type-spec (or list symbol))

;;; The exact type for which this function is the predicate.
;;; Applicable only for symbols in the CL package.
(define-info-type (:function :predicate-for) :type-spec t)

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
  :default #+sb-xc-host (lambda (x)
                          (declare (special *universal-type*) (ignore x))
                          *universal-type*)
           #-sb-xc-host *universal-type*)

;;; where this type and kind information came from
(define-info-type (:variable :where-from)
  :type-spec (member :declared :assumed :defined) :default :assumed)

;;; the macro-expansion for symbol-macros
(define-info-type (:variable :macro-expansion) :type-spec t)

(define-info-type (:variable :alien-info)
  :type-spec (or null sb-alien-internals:heap-alien-info))

(define-info-type (:variable :documentation) :type-spec (or string null))

;; :WIRED-TLS describes how SYMBOL-VALUE (implicit or not) should be compiled.
;;  - T means that SYMBOL-VALUE should access the TLS with a fixed offset,
;;     resolved at load-time. Presence of an index in the image that performed
;;     compilation is irrelevant (for now).
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
    :type-spec (or (member nil t :always-thread-local)
                   fixnum) ; the actual index, for thread slots
    :default nil)

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
                       (when (info :declaration :known name)
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

;;; wrapper for this type being used by the compiler
(define-info-type (:type :compiler-layout)
  :type-spec (or wrapper null)
  :default (lambda (name)
             (let ((class (find-classoid name nil)))
               (and class (classoid-wrapper class)))))

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
(define-info-type (:declaration :known)
  :type-spec (or function boolean)
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

;;;; ":ALIEN-TYPE" subsection - Data pertaining to globally known alien-types.
(define-info-type (:alien-type :kind)
  :type-spec (member :primitive :defined :unknown)
  :default :unknown)
(define-info-type (:alien-type :translator) :type-spec (or function null))
(define-info-type (:alien-type :definition)
  :type-spec (or null sb-alien-internals:alien-type))
(define-info-type (:alien-type :struct)
  :type-spec (or null sb-alien-internals:alien-type))
(define-info-type (:alien-type :union)
  :type-spec (or null sb-alien-internals:alien-type))
(define-info-type (:alien-type :enum)
  :type-spec (or null sb-alien-internals:alien-type))

;;;; ":SETF" subsection - Data pertaining to expansion of the omnipotent macro.
(define-info-type (:setf :expander) :type-spec (or function list))

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
(define-info-type (:source-location :declaration) :type-spec t)
(define-info-type (:source-location :alien-type) :type-spec t)

;;; If we used the maximum number of IDs available, a package gets no ID.
;;; Any symbols in that package must use SYMBOL-DBINFO for their package.
;;; Technically we can't store NIL, because that would be package ID 0,
;;; i.e. directly represented in the symbol, but this type spec has to be
;;; correct for what INFO can return, not what it may store.
(define-info-type (:symbol :package) :type-spec (or package null))

(!defun-from-collected-cold-init-forms !info-type-cold-init)

#-sb-xc-host
(defun !globaldb-cold-init ()
  (let ((h (make-info-hashtable)))
    (setf (sb-thread:mutex-name (info-env-mutex h)) "globaldb")
    (setq *info-environment* h))
  (setq *globaldb-observer* nil)
  (setq *info-types* (make-array (ash 1 info-number-bits) :initial-element nil))
  (!info-type-cold-init))

;; This is for the SB-INTROSPECT contrib module, and debugging.
(defun call-with-each-info (function symbol)
  (awhen (symbol-dbinfo symbol)
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
         (write val :level 2)))
     sym)))
