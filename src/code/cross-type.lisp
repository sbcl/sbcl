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

(define-condition cross-type-warning (warning)
  ((call :initarg :call :reader cross-type-warning-call)
   (message :reader cross-type-warning-message
            #+cmu :initarg #+cmu :message ; (to stop bogus non-STYLE WARNING)
            ))
  (:report (lambda (c s)
             (format
              s
              "cross-compiler type ambiguity in ~S:~%~A"
              (cross-type-warning-call c)
              (cross-type-warning-message c)))))

;;; This warning is signaled when giving up on a type calculation
;;; during cross-compilation.
(define-condition cross-type-giving-up (cross-type-warning)
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

;;; These hints needs to be tweaked by hand from time to time.

;; Return T if SYMBOL is a predicate acceptable for use in a SATISFIES type
;; specifier. We assume that anything in CL: is allowed (see explanation at
;; call point), and beyond that, anything we define has to be expressly listed
;; here, for fear of later unexpected confusion.
(defun acceptable-cross-typep-pred (symbol)
  (and (fboundp symbol)
       (or (in-cl-package-p symbol)
           ;; KLUDGE: rather than extensible list of predicates that match
           ;; in behavior between the host and target lisp, hardcode a few.
           (memq symbol '(sb!vm::symbol-always-has-tls-index-p
                          sb!vm:static-symbol-p)))))

;;; The set of types of which no object can be a member during cross-compilation.
(dolist (symbol '(alien system-area-pointer sb!alien-internals:alien-value
                  code-component fdefn lra extended-sequence
                  sb!pcl::%method-function sb!pcl::standard-instance
                  funcallable-instance generic-function
                  #!+sb-eval sb!eval:interpreted-function
                  #!+sb-fasteval sb!interpreter:interpreted-function
                  weak-pointer simd-pack))
  (setf (get symbol :cross-typep-hint) :certainly-nil))

;;; The set of objects for which it's accurate to use the host's type system
;;; as long as the type specifier appeared in its non-list form.
(dolist (symbol '(atom bit character complex cons float function integer
                  #| keyword |# ; nope!
                  list nil null number ratio rational real signed-byte symbol t
                  unsigned-byte
                  ;; easy cases of arrays and vectors
                  ;; But actually only partly right: we might disagree
                  ;; with the host as to whether an array is simple.
                  array simple-string simple-vector string vector))
  (setf (get symbol :cross-typep-hint) :use-host-typep))

;;; Instance types that will be forward-referenced
(dolist (symbol '(lexenv sb!c::abstract-lexenv
                  sb!assem:label
                  condition restart style-warning step-condition
                  class sb!pcl::condition-class error
                  hash-table sb!thread:mutex
                  sb!pretty:pprint-dispatch-table
                  compiler-note
                  deprecation-condition
                  early-deprecation-warning
                  late-deprecation-warning
                  final-deprecation-warning))
  (setf (get symbol :cross-typep-hint) :instance))

;;; This is like TYPEP, except that it asks whether HOST-OBJECT would
;;; be of TARGET-TYPE when instantiated on the target SBCL. Since this
;;; is hard to determine in some cases, and since in other cases we
;;; just haven't bothered to try, it needs to return two values, just
;;; like SUBTYPEP: the first value for its conservative opinion (never
;;; T unless it's certain) and the second value to tell whether it's
;;; certain.
(defun %cross-typep (mode host-object raw-target-type)
  (declare (type (member typep ctypep) mode))
  (let* ((target-type (typexpand raw-target-type))
         (type-atom (if (consp target-type) (car target-type) target-type))
         (hint (get type-atom :cross-typep-hint)))
    (flet ((warn-and-give-up ()
             ;; Giving up on SATISFIES is sane; don't warn.
             (unless (typep raw-target-type '(cons (eql satisfies)))
               (warn 'cross-type-giving-up
                     :call `(,mode ,host-object ,raw-target-type)))
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
           ;; just EQL. E.g. SIMPLE-STRING can be matched with EQL
           (target-type-is-in (list)
             (or (member raw-target-type list)
                 (member target-type list))))
      (cond (;; Handle various SBCL-specific types which can't exist on
             ;; the ANSI cross-compilation host.
             (eq hint :certainly-nil)
             (values nil t))
            ((and (symbolp target-type)
                  (or (eq hint :instance)
                      (and (find-classoid target-type nil)
                           (sb!xc:subtypep target-type 'cl:structure-object)))
                  (typep host-object '(or symbol number list character)))
             (values nil t))
            ((and (symbolp target-type)
                  ;; See if the host knows our DEF!STRUCT yet
                  (find-class target-type nil)
                  (subtypep target-type 'structure!object))
             (values (typep host-object target-type) t))

            ;; Many simple types are guaranteed to correspond exactly
            ;; between any host ANSI Common Lisp and the target SBCL.
            ((and (symbolp target-type) (eq hint :use-host-typep))
             (values (typep host-object target-type) t))

            ;; Translate KEYWORD into (SATISFIES KEYWORDP)
            ((eq target-type 'keyword)
             (cross-typep mode host-object '(satisfies keywordp)))

            ;; Pick off OR before calling anything that properly parses.
            ;; Not sure why this isn't always handled by (UNION-TYPE-P CTYPE)
            ;; in definition of CTYPEP itself, but it isn't.
            ((typep target-type '(cons (eql or)))
             (any/type (lambda (x y) (cross-typep mode x y))
                       host-object (cdr target-type)))

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
             (if (not (vectorp host-object))
                 (values nil t) ; certainly not a vector
                 ;; If our type machinery determined that TARGET-TYPE is a
                 ;; vector type, and the host object is a vector,
                 ;; we can (usually) perform this typep test with confidence.
                 (let ((physical-element-type ; what we recorded for it
                        (!specialized-array-element-type host-object)))
                   ;; Warn if no specialization recorded, or specialized on T.
                   ;; If we actually need to do something with SIMPLE-VECTOR,
                   ;; then !SPECIALIZED-ARRAY-ELEMENT-TYPE will have to return
                   ;; a second value indicating that T was not merely
                   ;; the assumed answer.
                   (if (eq physical-element-type 't)
                       (warn-and-give-up)
                       (values (equal (type-specifier
                                       (array-type-specialized-element-type
                                        (specifier-type target-type)))
                                      physical-element-type)
                               t)))))
            (;; general cases of arrays
             (and (not (hairy-type-p (values-specifier-type target-type)))
                  (sb!xc:subtypep target-type 'cl:array))
             (if (arrayp host-object)
                 (warn-and-give-up) ; general-case arrays being way too hard
                 (values nil t))) ; but "obviously not an array" being easy
            (;; Floating point types are guaranteed to correspond,
             ;; too, but less exactly.
             (target-type-is-in '(single-float double-float))
             (cond ((floatp host-object)
                    (warn-about-possible-float-info-loss)
                    (values (typep host-object target-type) t))
                   (t
                    (values nil t))))

            ((member type-atom '(character-set extended-char))
             (if (characterp host-object)
                 (warn-and-give-up)
                 (values nil t)))

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
            ((target-type-is-in '(base-string simple-base-string))
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
            ((target-type-is-in '(stream instance sb!impl::string-output-stream
                                  broadcast-stream file-stream))
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
                 (function-designator ; exactly the same as CTYPEP here
                  (values (typep host-object '(or symbol function)) t))
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
                 (and (every/type (lambda (x y) (cross-typep mode x y))
                                  host-object rest))
                 ;; OR was handled earlier above
                 (not
                  (multiple-value-bind (value surep)
                      (cross-typep mode host-object (car rest))
                    (cond (surep
                           (values (not value) t))
                          ((typep (car rest) '(cons (eql satisfies)))
                           (values nil nil)) ; Don't WARN-AND-...
                          (t
                           (warn-and-give-up)))))
                 ;; If we want to work with the KEYWORD type, we need
                 ;; to grok (SATISFIES KEYWORDP).
                 (satisfies
                  (destructuring-bind (predicate-name) rest
                    (cond
                      ((not (acceptable-cross-typep-pred predicate-name))
                       (warn-and-give-up))
                      ((eq mode 'ctypep)
                        ;; Do exactly as (DEFUN CTYPEP) in 'target-type'.
                        ;; Many predicates like KEYWORDP, ODDP, PACKAGEP,
                        ;; and NULL correspond between host and target.
                        ;; But we still need to handle errors, because
                        ;; the code which calls us may not understand
                        ;; that a type is unreachable. (E.g. when compiling
                        ;; (AND STRING (SATISFIES ARRAY-HAS-FILL-POINTER-P))
                        ;; CTYPEP may be called on the SATISFIES expression
                        ;; even for non-STRINGs.)
                       (let ((form `(,predicate-name ',host-object)))
                         (multiple-value-bind (ok result)
                             (sb!c::constant-function-call-p form nil nil)
                           (values (not (null result)) ok))))
                      (t
                       ;; Do as (DEFUN %%TYPEP) in 'typep' does, except for the
                       ;; check of well-formedness (we can trust our code).
                       (values (funcall predicate-name host-object) t)))))
                 ;; Some complex types are too hard to handle in the
                 ;; positive case, but at least we can be confident in
                 ;; a large fraction of the negative cases..
                 (cons
                  (if (consp host-object)
                      (warn-and-give-up)
                      (values nil t)))
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

(defvar *cross-typep-calls* nil)
(defvar *cross-typep-logfile*)
(defun cross-typep (mode host-object raw-target-type)
  #-debug-cross-typep
  (%cross-typep mode host-object raw-target-type)
  #+debug-cross-typep
  (multiple-value-bind (v1 v2)
      (%cross-typep mode host-object raw-target-type)
    ;; Record all calls so the can be replayed in the target SBCL
    (unless (or (eq raw-target-type 't)
                (member (cons host-object raw-target-type) *cross-typep-calls*
                        :test #'equal))
      (push (cons host-object raw-target-type) *cross-typep-calls*)
      (unless (boundp '*cross-typep-logfile*)
        (setq *cross-typep-logfile*
              (open (do ((suffix 1 (1+ suffix))) (nil)
                      (let ((pathname (format nil "output/cross-typep-~D.log" suffix)))
                        (unless (probe-file pathname)
                          (format t "~&Opening ~S~%" pathname)
                          (return pathname))))
                    :direction :output :if-exists :supersede)))
      (with-standard-io-syntax
        (let ((*package* (find-package "COMMON-LISP"))
              (stream *cross-typep-logfile*))
          (write-char #\( stream)
          (typecase host-object
            (ctype
             (if (eq host-object *wild-type*)
                 (write-string "#.*WILD-TYPE*" stream)
                 (format stream "#.(SPECIFIER-TYPE '~S)"
                         (type-specifier host-object))))
            (alien-type
             (format stream "#.(PARSE-ALIEN-TYPE '~S NIL)"
                     (unparse-alien-type host-object)))
            (layout
             (format stream "#.(FIND-LAYOUT '~S)"
                     (classoid-name (layout-classoid host-object))))
            (defstruct-description
             (format stream "#.(FIND-DEFSTRUCT-DESCRIPTION '~S)"
                     (dd-name host-object)))
            (heap-alien-info
             (format stream
                     "#.(SB-ALIEN::MAKE-HEAP-ALIEN-INFO :TYPE ~S :ALIEN-NAME ~S :DATAP ~S)"
                     `(parse-alien-type ',(unparse-alien-type
                                           (heap-alien-info-type host-object))
                                        nil)
                     (sb!alien::heap-alien-info-alien-name host-object)
                     (sb!alien::heap-alien-info-datap host-object)))
            (classoid-cell
             (format stream "#.(FIND-CLASSOID-CELL '~S)"
                     (classoid-cell-name host-object)))
            (t
             (format stream "~S" host-object)))
          (format stream " ~S ~S ~S)~%" raw-target-type v1 v2)
          (force-output stream))))
    (values v1 v2)))

;;; This is an incomplete TYPEP which runs at cross-compile time to
;;; tell whether OBJECT is the host Lisp representation of a target
;;; SBCL type specified by TARGET-TYPE-SPEC. It need make no pretense
;;; to completeness, since it need only handle the cases which arise
;;; when building SBCL itself, e.g. testing that range limits FOO and
;;; BAR in (INTEGER FOO BAR) are INTEGERs.
(defun sb!xc:typep (host-object target-type-spec &optional (env nil env-p))
  (declare (ignore env))
  (aver (null env-p)) ; 'cause we're too lazy to think about it
  (multiple-value-bind (opinion certain-p)
      (cross-typep 'typep host-object target-type-spec)
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
           (cross-typep 'ctypep obj type)))))

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
