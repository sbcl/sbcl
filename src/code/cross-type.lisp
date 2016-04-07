;;;; cross-compiler-only versions of TYPEP, TYPE-OF, and related functions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;; (This was a useful warning when trying to get bootstrapping
;;; to work, but it's mostly irrelevant noise now that the system
;;; works.)
(define-condition cross-type-style-warning (style-warning)
  ((call :initarg :call
         :reader cross-type-style-warning-call)
   (message :reader cross-type-style-warning-message
            #+cmu :initarg #+cmu :message ; (to stop bogus non-STYLE WARNING)
            ))
  (:report (lambda (c s)
             (format
              s
              "cross-compilation-time type ambiguity (should be OK) in ~S:~%~A"
              (cross-type-style-warning-call c)
              (cross-type-style-warning-message c)))))

;;; This warning is issued when giving up on a type calculation where a
;;; conservative answer is acceptable. Since a conservative answer is
;;; acceptable, the only downside is lost optimization opportunities.
(define-condition cross-type-giving-up-conservatively
    (cross-type-style-warning)
  ((message :initform "giving up conservatively"
            #+cmu :reader #+cmu #.(gensym) ; (to stop bogus non-STYLE WARNING)
            )))

;;; This warning refers to the flexibility in the ANSI spec with
;;; regard to run-time distinctions between floating point types.
;;; (E.g. the cross-compilation host might not even distinguish
;;; between SINGLE-FLOAT and DOUBLE-FLOAT, so a DOUBLE-FLOAT number
;;; would test positive as SINGLE-FLOAT.) If the target SBCL does make
;;; this distinction, then information is lost. It's not too hard to
;;; contrive situations where this would be a problem. In practice we
;;; don't tend to run into them because all widely used Common Lisp
;;; environments do recognize the distinction between SINGLE-FLOAT and
;;; DOUBLE-FLOAT, and we don't really need the other distinctions
;;; (e.g. between SHORT-FLOAT and SINGLE-FLOAT), so we call
;;; WARN-POSSIBLE-CROSS-TYPE-FLOAT-INFO-LOSS to test at runtime
;;; whether we need to worry about this at all, and not warn unless we
;;; do. If we *do* have to worry about this at runtime, my (WHN
;;; 19990808) guess is that the system will break in multiple places,
;;; so this is a real WARNING, not just a STYLE-WARNING.
;;;
;;; KLUDGE: If we ever try to support LONG-FLOAT or SHORT-FLOAT, this
;;; situation will get a lot more complicated.
(defun warn-possible-cross-type-float-info-loss (call)
  (when (or (subtypep 'single-float 'double-float)
            (subtypep 'double-float 'single-float))
    (warn "possible floating point information loss in ~S" call)))

(defun sb!xc:type-of (object)
  (let ((raw-result (type-of object)))
    (cond ((or (subtypep raw-result 'float)
               (subtypep raw-result 'complex))
           (warn-possible-cross-type-float-info-loss
            `(sb!xc:type-of ,object))
           raw-result)
          ((subtypep raw-result 'integer)
           (cond ((<= 0 object 1)
                  'bit)
                 (;; We can't rely on the host's opinion of whether
                  ;; it's a FIXNUM, but instead test against target
                  ;; MOST-fooITIVE-FIXNUM limits.
                  (fixnump object)
                  'fixnum)
                 (t
                  'integer)))
          ((subtypep raw-result 'simple-string)
           `(simple-base-string ,(length object)))
          ((subtypep raw-result 'string) 'base-string)
          ((some (lambda (type) (subtypep raw-result type))
                 '(array character list symbol))
           raw-result)
          (t
           (error "can't handle TYPE-OF ~S in cross-compilation" object)))))

;;; Is SYMBOL in the CL package? Note that we're testing this on the
;;; cross-compilation host, which could do things any old way. In
;;; particular, it might be in the CL package even though
;;; SYMBOL-PACKAGE is not (FIND-PACKAGE :CL). So we test things
;;; another way.
(defun in-cl-package-p (symbol)
  (eql (find-symbol (symbol-name symbol) :cl)
       symbol))

;; Return T if SYMBOL is a predicate acceptable for use in a SATISFIES type
;; specifier. We assume that anything in CL: is allowed (see explanation at
;; call point), and beyond that, anything we define has to be expressly listed
;; here, for fear of later unexpected confusion.
(defun acceptable-cross-typep-pred (symbol)
  (and (fboundp symbol)
       (or (in-cl-package-p symbol)
           ;; KLUDGE: rather than extensible list of predicates that match
           ;; in behavior between the host and target lisp, hardcode a few.
           (memq symbol '(sb!vm:static-symbol-p
                          sb!vm::wired-tls-symbol-p)))))

;;; This is like TYPEP, except that it asks whether HOST-OBJECT would
;;; be of TARGET-TYPE when instantiated on the target SBCL. Since this
;;; is hard to determine in some cases, and since in other cases we
;;; just haven't bothered to try, it needs to return two values, just
;;; like SUBTYPEP: the first value for its conservative opinion (never
;;; T unless it's certain) and the second value to tell whether it's
;;; certain.
(defun cross-typep (host-object raw-target-type)
  (let ((target-type (typexpand raw-target-type)))
    (flet ((warn-and-give-up ()
           ;; We don't have to keep track of this as long as system
           ;; performance is acceptable, since giving up
           ;; conservatively is a safe way out.
           #+nil
           (warn 'cross-type-giving-up-conservatively
                 :call `(cross-typep ,host-object ,raw-target-type))
           (values nil nil))
           (warn-about-possible-float-info-loss ()
             (warn-possible-cross-type-float-info-loss
               `(cross-typep ,host-object ,raw-target-type)))
           ;; a convenient idiom for making more matches to special cases:
           ;; Test both forms of target type for membership in LIST.
           ;;
           ;; (In order to avoid having to use too much deep knowledge
           ;; of types, it's sometimes convenient to test RAW-TARGET-TYPE
           ;; as well as the expanded type, since we can get matches with
           ;; just EQL. E.g. SIMPLE-STRING can be matched with EQL, while
           ;; safely matching its expansion,
           ;;  (OR (SIMPLE-ARRAY CHARACTER (*)) (SIMPLE-BASE-STRING *))
           ;; would require logic clever enough to know that, e.g., OR is
           ;; commutative.)
           (target-type-is-in (list)
             (or (member raw-target-type list)
                 (member target-type list))))
      (cond (;; Handle various SBCL-specific types which can't exist on
             ;; the ANSI cross-compilation host. KLUDGE: This code will
             ;; need to be tweaked by hand if the names of these types
             ;; ever change, ugh!
             (if (consp target-type)
                 (member (car target-type)
                         '(alien))
                 (member target-type
                         '(system-area-pointer
                           sb!alien-internals:alien-value)))
             (values nil t))
            (;; special case when TARGET-TYPE isn't a type spec, but
             ;; instead a CLASS object.
             (typep target-type 'class)
             (bug "We don't support CROSS-TYPEP of CLASS type specifiers"))
            ((and (symbolp target-type)
                  (find-classoid target-type nil)
                  (sb!xc:subtypep target-type 'cl:structure-object)
                  (typep host-object '(or symbol number list character)))
             (values nil t))
            ((and (symbolp target-type)
                  (find-class target-type nil)
                  (subtypep target-type 'structure!object))
             (values (typep host-object target-type) t))
            (;; easy cases of arrays and vectors
             (target-type-is-in
              '(array simple-string simple-vector string vector))
             (values (typep host-object target-type) t))
            (;; sequence is not guaranteed to be an exhaustive
             ;; partition, but it includes at least lists and vectors.
             (target-type-is-in '(sequence))
             (if (or (vectorp host-object) (listp host-object))
                 (values t t)
                 (if (typep host-object target-type)
                     (warn-and-give-up)
                     (values nil t))))
            (;; general cases of vectors
             (and (not (hairy-type-p (values-specifier-type target-type)))
                  (sb!xc:subtypep target-type 'cl:vector))
             (if (vectorp host-object)
                 (warn-and-give-up) ; general-case vectors being way too hard
                 (values nil t))) ; but "obviously not a vector" being easy
            (;; general cases of arrays
             (and (not (hairy-type-p (values-specifier-type target-type)))
                  (sb!xc:subtypep target-type 'cl:array))
             (if (arrayp host-object)
                 (warn-and-give-up) ; general-case arrays being way too hard
                 (values nil t))) ; but "obviously not an array" being easy
            ((target-type-is-in '(*))
             ;; KLUDGE: SBCL has * as an explicit wild type. While
             ;; this is sort of logical (because (e.g. (ARRAY * 1)) is
             ;; a valid type) it's not ANSI: looking at the ANSI
             ;; definitions of complex types like like ARRAY shows
             ;; that they consider * different from other type names.
             ;; Someday we should probably get rid of this non-ANSIism
             ;; in base SBCL, but until we do, we might as well here
             ;; in the cross compiler. And in order to make sure that
             ;; we don't continue doing it after we someday patch
             ;; SBCL's type system so that * is no longer a type, we
             ;; make this assertion. -- WHN 2001-08-08
             (aver (typep (values-specifier-type '*) 'named-type))
             (values t t))
            (;; Many simple types are guaranteed to correspond exactly
             ;; between any host ANSI Common Lisp and the target
             ;; Common Lisp. (Some array types are too, but they
             ;; were picked off earlier.)
             (target-type-is-in
              '(atom bit character complex cons float function integer keyword
                list nil null number rational real signed-byte symbol t
                unsigned-byte))
             (values (typep host-object target-type) t))
            (;; Floating point types are guaranteed to correspond,
             ;; too, but less exactly.
             (target-type-is-in
              '(single-float double-float))
             (cond ((floatp host-object)
                    (warn-about-possible-float-info-loss)
                    (values (typep host-object target-type) t))
                   (t
                    (values nil t))))
            (;; Complexes suffer the same kind of problems as arrays.
             ;; Our dumping logic is based on contents, however, so
             ;; reasoning about them should be safe
             (and (not (hairy-type-p (values-specifier-type target-type)))
                  (sb!xc:subtypep target-type 'cl:complex))
             (if (complexp host-object)
                 (let ((re (realpart host-object))
                       (im (imagpart host-object)))
                   (if (or (and (eq target-type 'complex)
                                (typep re 'rational) (typep im 'rational))
                           (and (equal target-type '(cl:complex single-float))
                                (typep re 'single-float) (typep im 'single-float))
                           (and (equal target-type '(cl:complex double-float))
                                (typep re 'double-float) (typep im 'double-float)))
                       (values t t)
                       (progn
                         ;; We won't know how to dump it either.
                         (warn "Host complex too complex: ~S" host-object)
                         (warn-and-give-up))))
                 (values nil t)))
            ;; Some types require translation between the cross-compilation
            ;; host Common Lisp and the target SBCL.
            ((target-type-is-in '(classoid))
             (values (typep host-object 'classoid) t))
            ((target-type-is-in '(fixnum))
             (values (fixnump host-object) t))
            ((target-type-is-in '(bignum))
             (values (and (integerp host-object) (not (fixnump host-object)))
                     t))
            ;; Some types are too hard to handle in the positive
            ;; case, but at least we can be confident in a large
            ;; fraction of the negative cases..
            ((target-type-is-in
              '(base-string simple-base-string simple-string))
             (if (stringp host-object)
                 (warn-and-give-up)
                 (values nil t)))
            ((target-type-is-in '(character base-char standard-char))
             (cond ((typep host-object 'standard-char)
                    (values t t))
                   ((not (characterp host-object))
                    (values nil t))
                   (t
                    (warn-and-give-up))))
            ((target-type-is-in '(stream instance))
             ;; Neither target CL:STREAM nor target SB!KERNEL:INSTANCE
             ;; is implemented as a STRUCTURE-OBJECT, so they'll fall
             ;; through the tests above. We don't want to assume too
             ;; much about them here, but at least we know enough
             ;; about them to say that neither T nor NIL nor indeed
             ;; any other symbol in the cross-compilation host is one.
             ;; That knowledge suffices to answer so many of the
             ;; questions that the cross-compiler asks that it's well
             ;; worth special-casing it here.
             (if (symbolp host-object)
                 (values nil t)
                 (warn-and-give-up)))
            ;; various hacks for composite types..
            ((consp target-type)
             (let ((first (first target-type))
                   (rest (rest target-type)))
               (case first
                 ;; Many complex types are guaranteed to correspond exactly
                 ;; between any host ANSI Common Lisp and the target SBCL.
                 ((integer member mod rational real signed-byte unsigned-byte)
                  (values (typep host-object target-type) t))
                 ;; Floating point types are guaranteed to correspond,
                 ;; too, but less exactly.
                 ((single-float double-float)
                  (cond ((floatp host-object)
                         (warn-about-possible-float-info-loss)
                         (values (typep host-object target-type) t))
                        (t
                         (values nil t))))
                 ;; Some complex types have translations that are less
                 ;; trivial.
                 (and (every/type #'cross-typep host-object rest))
                 (or  (any/type   #'cross-typep host-object rest))
                 (not
                  (multiple-value-bind (value surep)
                      (cross-typep host-object (car rest))
                    (if surep
                        (values (not value) t)
                        (warn-and-give-up))))
                 ;; If we want to work with the KEYWORD type, we need
                 ;; to grok (SATISFIES KEYWORDP).
                 (satisfies
                  (destructuring-bind (predicate-name) rest
                    (if (acceptable-cross-typep-pred predicate-name)
                        ;; Many predicates like KEYWORDP, ODDP, PACKAGEP,
                        ;; and NULL correspond between host and target.
                        ;; But we still need to handle errors, because
                        ;; the code which calls us may not understand
                        ;; that a type is unreachable. (E.g. when compiling
                        ;; (AND STRING (SATISFIES ARRAY-HAS-FILL-POINTER-P))
                        ;; CTYPEP may be called on the SATISFIES expression
                        ;; even for non-STRINGs.)
                        (multiple-value-bind (result error?)
                            (ignore-errors (funcall predicate-name
                                                    host-object))
                          (if error?
                              (values nil nil)
                              (values result t)))
                        ;; For symbols not in the CL package, it's not
                        ;; in general clear how things correspond
                        ;; between host and target, so we punt.
                        (warn-and-give-up))))
                 ;; Some complex types are too hard to handle in the
                 ;; positive case, but at least we can be confident in
                 ;; a large fraction of the negative cases..
                 ((base-string simple-base-string simple-string)
                  (if (stringp host-object)
                      (warn-and-give-up)
                      (values nil t)))
                 ((vector simple-vector)
                  (if (vectorp host-object)
                      (warn-and-give-up)
                      (values nil t)))
                 ((array simple-array)
                  (if (arrayp host-object)
                      (warn-and-give-up)
                      (values nil t)))
                 (function
                  (if (functionp host-object)
                      (warn-and-give-up)
                      (values nil t)))
                 ;; And the Common Lisp type system is complicated,
                 ;; and we don't try to implement everything.
                 (otherwise (warn-and-give-up)))))
            ;; And the Common Lisp type system is complicated, and
            ;; we don't try to implement everything.
            (t
             (warn-and-give-up))))))

;;; This is an incomplete TYPEP which runs at cross-compile time to
;;; tell whether OBJECT is the host Lisp representation of a target
;;; SBCL type specified by TARGET-TYPE-SPEC. It need make no pretense
;;; to completeness, since it need only handle the cases which arise
;;; when building SBCL itself, e.g. testing that range limits FOO and
;;; BAR in (INTEGER FOO BAR) are INTEGERs.
(defun sb!xc:typep (host-object target-type-spec &optional (env nil env-p))
  (declare (ignore env))
  (declare (optimize (debug 0))) ; workaround for lp# 1498644
  (aver (null env-p)) ; 'cause we're too lazy to think about it
  (multiple-value-bind (opinion certain-p)
      (cross-typep host-object target-type-spec)
    ;; A program that calls TYPEP doesn't want uncertainty and
    ;; probably can't handle it.
    (if certain-p
        opinion
        (error "uncertain in SB!XC:TYPEP ~S ~S"
               host-object
               target-type-spec))))

;;; This is an incomplete, portable implementation for use at
;;; cross-compile time only.
(defun ctypep (obj ctype)
  (check-type ctype ctype)
  ;; There is at least one possible endless recursion in the
  ;; cross-compiler type system: (SUBTYPEP NULL (OR UNKOWN0 UNKNOWN1)
  ;; runs out of stack. The right way would probably be to not
  ;; implement CTYPEP in terms of TYPE-SPECIFIER (:UNPARSE, that may
  ;; call TYPE=, that in turn may call CTYPEP). Until then, pick a few
  ;; cherries off.
  (cond ((member-type-p ctype)
         (if (member-type-member-p obj ctype)
             (values t t)
             (values nil t)))
        ((union-type-p ctype)
         (any/type #'ctypep obj (union-type-types ctype)))
        ((array-type-p ctype)
         ;; This is essentially just the ARRAY-TYPE case of %%TYPEP
         ;; using !SPECIALIZED-ARRAY-ELEMENT-TYPE, not ARRAY-ELEMENT-TYPE.
         (if (and (arrayp obj)
                  (case (array-type-complexp ctype)
                    ((t) (not (typep obj 'simple-array)))
                    ((nil) (typep obj 'simple-array)))
                  (or (eq (array-type-element-type ctype) *wild-type*)
                      (type= (specifier-type
                              (!specialized-array-element-type obj))
                             (array-type-specialized-element-type ctype)))
                  (or (eq (array-type-dimensions ctype) '*)
                      (and (= (length (array-type-dimensions ctype))
                              (array-rank obj)))
                      (every (lambda (required actual)
                               (or (eq required '*) (eql required actual)))
                             (array-type-dimensions ctype)
                             (array-dimensions obj))))
               (values t t)
               (values nil t)))
        ((and (structure-classoid-p ctype) (symbolp obj))
         (values nil t))
        (t
         (let ( ;; the Common Lisp type specifier corresponding to CTYPE
               (type (type-specifier ctype)))
           (check-type type (or symbol cons))
           (cross-typep obj type)))))

(defun ctype-of (x)
  (typecase x
    (function
     (if (typep x 'generic-function)
         ;; Since at cross-compile time we build a CLOS-free bootstrap
         ;; version of SBCL, it's unclear how to explain to it what a
         ;; generic function is.
         (error "not implemented: cross CTYPE-OF generic function")
         ;; There's no ANSI way to find out what the function is
         ;; declared to be, so we just return the CTYPE for the
         ;; most-general function.
         *universal-fun-type*))
    (symbol
     (make-eql-type x))
    (number
     (ctype-of-number x))
    (array
     ;; It is critical not to inquire of the host for the array's element type.
     (let ((etype (specifier-type (!specialized-array-element-type x))))
       (make-array-type (array-dimensions x)
                        ;; complexp relies on the host implementation,
                        ;; but in practice any array for which we need to
                        ;; call ctype-of will be a simple-array.
                        :complexp (not (typep x 'simple-array))
                        :element-type etype
                        :specialized-element-type etype)))
    (cons (specifier-type 'cons))
    (character
     (cond ((typep x 'standard-char)
            (specifier-type 'base-char))
           ((not (characterp x))
            nil)
           (t
            ;; Beyond this, there seems to be no portable correspondence.
            (error "can't map host Lisp CHARACTER ~S to target Lisp" x))))
    (structure!object
     (find-classoid (uncross (class-name (class-of x))))) ; FIXME: TYPE-OF?
    (t
     ;; There might be more cases which we could handle with
     ;; sufficient effort; since all we *need* to handle are enough
     ;; cases for bootstrapping, we don't try to be complete here,. If
     ;; future maintainers make the bootstrap code more complicated,
     ;; they can also add new cases here to handle it. -- WHN 2000-11-11
     (error "can't handle ~S in cross CTYPE-OF" x))))

(defun sb!pcl::class-has-a-forward-referenced-superclass-p (x)
  (bug "CLASS-HAS-A-FORWARD-REFERENCED-SUPERCLASS-P reached: ~S" x))
