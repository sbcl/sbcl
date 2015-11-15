;;;; This file contains the definition of non-CLASS types (e.g.
;;;; subtypes of interesting BUILT-IN-CLASSes) and the interfaces to
;;;; the type system. Common Lisp type specifiers are parsed into a
;;;; somewhat canonical internal type representation that supports
;;;; type union, intersection, etc. (Except that ALIEN types have
;;;; moved out..)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

(/show0 "late-type.lisp 19")

(!begin-collecting-cold-init-forms)

;;; ### Remaining incorrectnesses:
;;;
;;; There are all sorts of nasty problems with open bounds on FLOAT
;;; types (and probably FLOAT types in general.)

;;; This condition is signalled whenever we make a UNKNOWN-TYPE so that
;;; compiler warnings can be emitted as appropriate.
(define-condition parse-unknown-type (condition)
  ((specifier :reader parse-unknown-type-specifier :initarg :specifier))
  (:default-initargs
   :specifier (missing-arg)))

;;; This condition is signalled whenever we encounter a type (DEFTYPE,
;;; structure, condition, class) that has been marked as deprecated.
(define-condition parse-deprecated-type (condition)
  ((specifier :reader parse-deprecated-type-specifier :initarg :specifier))
  (:default-initargs
   :specifier (missing-arg)))

;;; These functions are used as method for types which need a complex
;;; subtypep method to handle some superclasses, but cover a subtree
;;; of the type graph (i.e. there is no simple way for any other type
;;; class to be a subtype.) There are always still complex ways,
;;; namely UNION and MEMBER types, so we must give TYPE1's method a
;;; chance to run, instead of immediately returning NIL, T.
(defun delegate-complex-subtypep-arg2 (type1 type2)
  (let ((subtypep-arg1
         (type-class-complex-subtypep-arg1 (type-class-info type1))))
    (if subtypep-arg1
        (funcall subtypep-arg1 type1 type2)
        (values nil t))))
(defun delegate-complex-intersection2 (type1 type2)
  (let ((method (type-class-complex-intersection2 (type-class-info type1))))
    (if (and method (not (eq method #'delegate-complex-intersection2)))
        (funcall method type2 type1)
        (hierarchical-intersection2 type1 type2))))

(defun contains-unknown-type-p (ctype)
  (typecase ctype
   (unknown-type t)
   (compound-type (some #'contains-unknown-type-p (compound-type-types ctype)))
   (negation-type (contains-unknown-type-p (negation-type-type ctype)))
   (cons-type (or (contains-unknown-type-p (cons-type-car-type ctype))
                  (contains-unknown-type-p (cons-type-cdr-type ctype))))
   (array-type (contains-unknown-type-p (array-type-element-type ctype)))))

;; Similar to (NOT CONTAINS-UNKNOWN-TYPE-P), but report that (SATISFIES F)
;; is not a testable type unless F is currently bound.
(defun testable-type-p (ctype)
  (typecase ctype
    (unknown-type nil) ; must precede HAIRY because an unknown is HAIRY
    (hairy-type
     (let ((spec (hairy-type-specifier ctype)))
       ;; Anything other than (SATISFIES ...) is testable
       ;; because there's no reason to suppose that it isn't.
       (or (neq (car spec) 'satisfies) (fboundp (cadr spec)))))
    (compound-type (every #'testable-type-p (compound-type-types ctype)))
    (negation-type (testable-type-p (negation-type-type ctype)))
    (cons-type (and (testable-type-p (cons-type-car-type ctype))
                    (testable-type-p (cons-type-cdr-type ctype))))
    ;; This case could be too strict. I think an array type is testable
    ;; if the upgraded type is testable. Probably nobody cares though.
    (array-type (testable-type-p (array-type-element-type ctype)))
    (t t)))

;;; This is used by !DEFINE-SUPERCLASSES to define the SUBTYPE-ARG1
;;; method. INFO is a list of conses
;;;   (SUPERCLASS-CLASS . {GUARD-TYPE-SPECIFIER | NIL}).
(defun has-superclasses-complex-subtypep-arg1 (type1 type2 info)
  ;; If TYPE2 might be concealing something related to our class
  ;; hierarchy
  (if (type-might-contain-other-types-p type2)
      ;; too confusing, gotta punt
      (values nil nil)
      ;; ordinary case expected by old CMU CL code, where the taxonomy
      ;; of TYPE2's representation accurately reflects the taxonomy of
      ;; the underlying set
      (values
       ;; FIXME: This old CMU CL code probably deserves a comment
       ;; explaining to us mere mortals how it works...
       (and (sb!xc:typep type2 'classoid)
            (dolist (x info nil)
              (when (or (not (cdr x))
                        (csubtypep type1 (specifier-type (cdr x))))
                (return
                 (or (eq type2 (car x))
                     (let ((inherits (layout-inherits
                                      (classoid-layout (car x)))))
                       (dotimes (i (length inherits) nil)
                         (when (eq type2 (layout-classoid (svref inherits i)))
                           (return t)))))))))
       t)))

;;; This function takes a list of specs, each of the form
;;;    (SUPERCLASS-NAME &OPTIONAL GUARD).
;;; Consider one spec (with no guard): any instance of the named
;;; TYPE-CLASS is also a subtype of the named superclass and of any of
;;; its superclasses. If there are multiple specs, then some will have
;;; guards. We choose the first spec whose guard is a supertype of
;;; TYPE1 and use its superclass. In effect, a sequence of guards
;;;    G0, G1, G2
;;; is actually
;;;    G0,(and G1 (not G0)), (and G2 (not (or G0 G1))).
;;;
;;; WHEN controls when the forms are executed.
(defmacro !define-superclasses (type-class-name specs when)
  (with-unique-names (type-class info)
    `(,when
       (let ((,type-class (type-class-or-lose ',type-class-name))
             (,info (mapcar (lambda (spec)
                              (destructuring-bind
                                  (super &optional guard)
                                  spec
                                (cons (find-classoid super) guard)))
                            ',specs)))
         (setf (type-class-complex-subtypep-arg1 ,type-class)
               (lambda (type1 type2)
                 (has-superclasses-complex-subtypep-arg1 type1 type2 ,info)))
         (setf (type-class-complex-subtypep-arg2 ,type-class)
               #'delegate-complex-subtypep-arg2)
         (setf (type-class-complex-intersection2 ,type-class)
               #'delegate-complex-intersection2)))))

;;;; FUNCTION and VALUES types
;;;;
;;;; Pretty much all of the general type operations are illegal on
;;;; VALUES types, since we can't discriminate using them, do
;;;; SUBTYPEP, etc. FUNCTION types are acceptable to the normal type
;;;; operations, but are generally considered to be equivalent to
;;;; FUNCTION. These really aren't true types in any type theoretic
;;;; sense, but we still parse them into CTYPE structures for two
;;;; reasons:

;;;; -- Parsing and unparsing work the same way, and indeed we can't
;;;;    tell whether a type is a function or values type without
;;;;    parsing it.
;;;; -- Many of the places that can be annotated with real types can
;;;;    also be annotated with function or values types.

(!define-type-method (values :simple-subtypep :complex-subtypep-arg1)
                     (type1 type2)
  (declare (ignore type2))
  ;; FIXME: should be TYPE-ERROR, here and in next method
  (error "SUBTYPEP is illegal on this type:~%  ~S" (type-specifier type1)))

(!define-type-method (values :complex-subtypep-arg2)
                     (type1 type2)
  (declare (ignore type1))
  (error "SUBTYPEP is illegal on this type:~%  ~S" (type-specifier type2)))

(!define-type-method (values :negate) (type)
  (error "NOT VALUES too confusing on ~S" (type-specifier type)))

(!define-type-method (values :unparse) (type)
  (cons 'values
        (let ((unparsed (unparse-args-types type)))
          (if (or (values-type-optional type)
                  (values-type-rest type)
                  (values-type-allowp type))
              unparsed
              (nconc unparsed '(&optional))))))

;;; Return true if LIST1 and LIST2 have the same elements in the same
;;; positions according to TYPE=. We return NIL, NIL if there is an
;;; uncertain comparison.
(defun type=-list (list1 list2)
  (declare (list list1 list2))
  (do ((types1 list1 (cdr types1))
       (types2 list2 (cdr types2)))
      ((or (null types1) (null types2))
       (if (or types1 types2)
           (values nil t)
           (values t t)))
    (multiple-value-bind (val win)
        (type= (first types1) (first types2))
      (unless win
        (return (values nil nil)))
      (unless val
        (return (values nil t))))))

(!define-type-method (values :simple-=) (type1 type2)
  (type=-args type1 type2))

(!define-type-class function :enumerable nil
                    :might-contain-other-types nil)

;;; a flag that we can bind to cause complex function types to be
;;; unparsed as FUNCTION. This is useful when we want a type that we
;;; can pass to TYPEP.
(!defvar *unparse-fun-type-simplify* nil)
;;; A flag to prevent TYPE-OF calls by user applications from returning
;;; (NOT x). TYPE-SPECIFIER usually allows it to preserve information.
(!defvar *unparse-allow-negation* t)

(!define-type-method (function :negate) (type) (make-negation-type type))

(!define-type-method (function :unparse) (type)
  (if *unparse-fun-type-simplify*
      'function
      (list 'function
            (if (fun-type-wild-args type)
                '*
                (unparse-args-types type))
            (type-specifier
             (fun-type-returns type)))))

;;; The meaning of this is a little confused. On the one hand, all
;;; function objects are represented the same way regardless of the
;;; arglists and return values, and apps don't get to ask things like
;;; (TYPEP #'FOO (FUNCTION (FIXNUM) *)) in any meaningful way. On the
;;; other hand, Python wants to reason about function types. So...
(!define-type-method (function :simple-subtypep) (type1 type2)
 (flet ((fun-type-simple-p (type)
          (not (or (fun-type-rest type)
                   (fun-type-keyp type))))
        (every-csubtypep (types1 types2)
          (loop
             for a1 in types1
             for a2 in types2
             do (multiple-value-bind (res sure-p)
                    (csubtypep a1 a2)
                  (unless res (return (values res sure-p))))
             finally (return (values t t)))))
   (and/type (values-subtypep (fun-type-returns type1)
                              (fun-type-returns type2))
             (cond ((fun-type-wild-args type2) (values t t))
                   ((fun-type-wild-args type1)
                    (cond ((fun-type-keyp type2) (values nil nil))
                          ((not (fun-type-rest type2)) (values nil t))
                          ((not (null (fun-type-required type2)))
                           (values nil t))
                          (t (and/type (type= *universal-type*
                                              (fun-type-rest type2))
                                       (every/type #'type=
                                                   *universal-type*
                                                   (fun-type-optional
                                                    type2))))))
                   ((not (and (fun-type-simple-p type1)
                              (fun-type-simple-p type2)))
                    (values nil nil))
                   (t (multiple-value-bind (min1 max1) (fun-type-nargs type1)
                        (multiple-value-bind (min2 max2) (fun-type-nargs type2)
                          (cond ((or (> max1 max2) (< min1 min2))
                                 (values nil t))
                                ((and (= min1 min2) (= max1 max2))
                                 (and/type (every-csubtypep
                                            (fun-type-required type1)
                                            (fun-type-required type2))
                                           (every-csubtypep
                                            (fun-type-optional type1)
                                            (fun-type-optional type2))))
                                (t (every-csubtypep
                                    (concatenate 'list
                                                 (fun-type-required type1)
                                                 (fun-type-optional type1))
                                    (concatenate 'list
                                                 (fun-type-required type2)
                                                 (fun-type-optional type2))))))))))))

(!define-superclasses function ((function)) !cold-init-forms)

;;; The union or intersection of two FUNCTION types is FUNCTION.
(!define-type-method (function :simple-union2) (type1 type2)
  (declare (ignore type1 type2))
  (specifier-type 'function))
(!define-type-method (function :simple-intersection2) (type1 type2)
  (let ((ftype (specifier-type 'function)))
    (cond ((eq type1 ftype) type2)
          ((eq type2 ftype) type1)
          (t (let ((rtype (values-type-intersection (fun-type-returns type1)
                                                    (fun-type-returns type2))))
               (flet ((change-returns (ftype rtype)
                        (declare (type fun-type ftype) (type ctype rtype))
                        (make-fun-type :required (fun-type-required ftype)
                                       :optional (fun-type-optional ftype)
                                       :keyp (fun-type-keyp ftype)
                                       :keywords (fun-type-keywords ftype)
                                       :allowp (fun-type-allowp ftype)
                                       :returns rtype)))
               (cond
                 ((fun-type-wild-args type1)
                  (if (fun-type-wild-args type2)
                      (make-fun-type :wild-args t
                                     :returns rtype)
                      (change-returns type2 rtype)))
                 ((fun-type-wild-args type2)
                  (change-returns type1 rtype))
                 (t (multiple-value-bind (req opt rest)
                        (args-type-op type1 type2 #'type-intersection #'max)
                      (make-fun-type :required req
                                     :optional opt
                                     :rest rest
                                     ;; FIXME: :keys
                                     :allowp (and (fun-type-allowp type1)
                                                  (fun-type-allowp type2))
                                     :returns rtype))))))))))

;;; The union or intersection of a subclass of FUNCTION with a
;;; FUNCTION type is somewhat complicated.
(!define-type-method (function :complex-intersection2) (type1 type2)
  (cond
    ((type= type1 (specifier-type 'function)) type2)
    ((csubtypep type1 (specifier-type 'function)) nil)
    (t :call-other-method)))
(!define-type-method (function :complex-union2) (type1 type2)
  (declare (ignore type2))
  ;; TYPE2 is a FUNCTION type.  If TYPE1 is a classoid type naming
  ;; FUNCTION, then it is the union of the two; otherwise, there is no
  ;; special union.
  (cond
    ((type= type1 (specifier-type 'function)) type1)
    (t nil)))

(!define-type-method (function :simple-=) (type1 type2)
  (macrolet ((compare (comparator field)
               (let ((reader (symbolicate '#:fun-type- field)))
                 `(,comparator (,reader type1) (,reader type2)))))
    (and/type (compare type= returns)
              (cond ((neq (fun-type-wild-args type1) (fun-type-wild-args type2))
                     (values nil t))
                    ((eq (fun-type-wild-args type1) t)
                     (values t t))
                    (t (type=-args type1 type2))))))

(!define-type-class constant :inherits values)

(!define-type-method (constant :negate) (type)
  (error "NOT CONSTANT too confusing on ~S" (type-specifier type)))

(!define-type-method (constant :unparse) (type)
  `(constant-arg ,(type-specifier (constant-type-type type))))

(!define-type-method (constant :simple-=) (type1 type2)
  (type= (constant-type-type type1) (constant-type-type type2)))

(!def-type-translator constant-arg ((:context context) type)
  (make-constant-type :type (single-value-specifier-type-r context type)))

;;; Return the lambda-list-like type specification corresponding
;;; to an ARGS-TYPE.
(declaim (ftype (function (args-type) list) unparse-args-types))
(defun unparse-args-types (type)
  (collect ((result))

    (dolist (arg (args-type-required type))
      (result (type-specifier arg)))

    (when (args-type-optional type)
      (result '&optional)
      (dolist (arg (args-type-optional type))
        (result (type-specifier arg))))

    (when (args-type-rest type)
      (result '&rest)
      (result (type-specifier (args-type-rest type))))

    (when (args-type-keyp type)
      (result '&key)
      (dolist (key (args-type-keywords type))
        (result (list (key-info-name key)
                      (type-specifier (key-info-type key))))))

    (when (args-type-allowp type)
      (result '&allow-other-keys))

    (result)))

(!def-type-translator function ((:context context)
                                &optional (args '*) (result '*))
  (let ((result (coerce-to-values (values-specifier-type-r context result))))
    (if (eq args '*)
        (if (eq result *wild-type*)
            (specifier-type 'function)
            (make-fun-type :wild-args t :returns result))
        (multiple-value-bind (llks required optional rest keywords)
            (parse-args-types context args :function-type)
          (if (and (null required)
                   (null optional)
                   (eq rest *universal-type*)
                   (not (ll-kwds-keyp llks)))
              (if (eq result *wild-type*)
                  (specifier-type 'function)
                  (make-fun-type :wild-args t :returns result))
              (make-fun-type :required required
                             :optional optional
                             :rest rest
                             :keyp (ll-kwds-keyp llks)
                             :keywords keywords
                             :allowp (ll-kwds-allowp llks)
                             :returns result))))))

(!def-type-translator values :list ((:context context) &rest values)
  (if (eq values '*)
      *wild-type*
      (multiple-value-bind (llks required optional rest)
          (parse-args-types context values :values-type)
        (if (plusp llks)
            (make-values-type :required required :optional optional :rest rest)
            (make-short-values-type required)))))

;;;; VALUES types interfaces
;;;;
;;;; We provide a few special operations that can be meaningfully used
;;;; on VALUES types (as well as on any other type).

;;; Return the minimum number of values possibly matching VALUES type
;;; TYPE.
(defun values-type-min-value-count (type)
  (etypecase type
    (named-type
     (ecase (named-type-name type)
       ((t *) 0)
       ((nil) 0)))
    (values-type
     (length (values-type-required type)))))

;;; Return the maximum number of values possibly matching VALUES type
;;; TYPE.
(defun values-type-max-value-count (type)
  (etypecase type
    (named-type
     (ecase (named-type-name type)
       ((t *) call-arguments-limit)
       ((nil) 0)))
    (values-type
     (if (values-type-rest type)
         call-arguments-limit
         (+ (length (values-type-optional type))
            (length (values-type-required type)))))))

(defun values-type-may-be-single-value-p (type)
  (<= (values-type-min-value-count type)
      1
      (values-type-max-value-count type)))

;;; VALUES type with a single value.
(defun type-single-value-p (type)
  (and (%values-type-p type)
       (not (values-type-rest type))
       (null (values-type-optional type))
       (singleton-p (values-type-required type))))

;;; Return the type of the first value indicated by TYPE. This is used
;;; by people who don't want to have to deal with VALUES types.
#!-sb-fluid (declaim (freeze-type values-type))
; (inline single-value-type))
(defun single-value-type (type)
  (declare (type ctype type))
  (cond ((eq type *wild-type*)
         *universal-type*)
        ((eq type *empty-type*)
         *empty-type*)
        ((not (values-type-p type))
         type)
        ((car (args-type-required type)))
        (t (type-union (specifier-type 'null)
                       (or (car (args-type-optional type))
                           (args-type-rest type)
                           (specifier-type 'null))))))

;;; Return the minimum number of arguments that a function can be
;;; called with, and the maximum number or NIL. If not a function
;;; type, return NIL, NIL.
(defun fun-type-nargs (type)
  (declare (type ctype type))
  (if (and (fun-type-p type) (not (fun-type-wild-args type)))
      (let ((fixed (length (args-type-required type))))
        (if (or (args-type-rest type)
                (args-type-keyp type)
                (args-type-allowp type))
            (values fixed nil)
            (values fixed (+ fixed (length (args-type-optional type))))))
      (values nil nil)))

;;; Determine whether TYPE corresponds to a definite number of values.
;;; The first value is a list of the types for each value, and the
;;; second value is the number of values. If the number of values is
;;; not fixed, then return NIL and :UNKNOWN.
(defun values-types (type)
  (declare (type ctype type))
  (cond ((or (eq type *wild-type*) (eq type *empty-type*))
         (values nil :unknown))
        ((or (args-type-optional type)
             (args-type-rest type))
         (values nil :unknown))
        (t
         (let ((req (args-type-required type)))
           (values req (length req))))))

;;; Return two values:
;;; 1. A list of all the positional (fixed and optional) types.
;;; 2. The &REST type (if any). If no &REST, then the DEFAULT-TYPE.
(defun values-type-types (type &optional (default-type *empty-type*))
  (declare (type ctype type))
  (if (eq type *wild-type*)
      (values nil *universal-type*)
      (values (append (args-type-required type)
                      (args-type-optional type))
              (cond ((args-type-rest type))
                    (t default-type)))))

;;; types of values in (the <type> (values o_1 ... o_n))
(defun values-type-out (type count)
  (declare (type ctype type) (type unsigned-byte count))
  (if (eq type *wild-type*)
      (make-list count :initial-element *universal-type*)
      (collect ((res))
        (flet ((process-types (types)
                 (loop for type in types
                       while (plusp count)
                       do (decf count)
                       do (res type))))
          (process-types (values-type-required type))
          (process-types (values-type-optional type))
          (when (plusp count)
            (loop with rest = (the ctype (values-type-rest type))
                  repeat count
                  do (res rest))))
        (res))))

;;; types of variable in (m-v-bind (v_1 ... v_n) (the <type> ...
(defun values-type-in (type count)
  (declare (type ctype type) (type unsigned-byte count))
  (if (eq type *wild-type*)
      (make-list count :initial-element *universal-type*)
      (collect ((res))
        (let ((null-type (specifier-type 'null)))
          (loop for type in (values-type-required type)
             while (plusp count)
             do (decf count)
             do (res type))
          (loop for type in (values-type-optional type)
             while (plusp count)
             do (decf count)
             do (res (type-union type null-type)))
          (when (plusp count)
            (loop with rest = (acond ((values-type-rest type)
                                      (type-union it null-type))
                                     (t null-type))
               repeat count
               do (res rest))))
        (res))))

;;; Return a list of OPERATION applied to the types in TYPES1 and
;;; TYPES2, padding with REST2 as needed. TYPES1 must not be shorter
;;; than TYPES2. The second value is T if OPERATION always returned a
;;; true second value.
(defun fixed-values-op (types1 types2 rest2 operation)
  (declare (list types1 types2) (type ctype rest2) (type function operation))
  (let ((exact t))
    (values (mapcar (lambda (t1 t2)
                      (multiple-value-bind (res win)
                          (funcall operation t1 t2)
                        (unless win
                          (setq exact nil))
                        res))
                    types1
                    (append types2
                            (make-list (- (length types1) (length types2))
                                       :initial-element rest2)))
            exact)))

;;; If TYPE isn't a values type, then make it into one.
(defun-cached (%coerce-to-values :hash-bits 8 :hash-function #'type-hash-value)
    ((type eq))
  (cond ((multiple-value-bind (res sure)
             (csubtypep (specifier-type 'null) type)
           (and (not res) sure))
         ;; FIXME: What should we do with (NOT SURE)?
         (make-values-type :required (list type) :rest *universal-type*))
        (t
         (make-values-type :optional (list type) :rest *universal-type*))))

(defun coerce-to-values (type)
  (declare (type ctype type))
  (cond ((or (eq type *universal-type*)
             (eq type *wild-type*))
         *wild-type*)
        ((values-type-p type)
         type)
        (t (%coerce-to-values type))))

;;; Return type, corresponding to ANSI short form of VALUES type
;;; specifier.
(defun make-short-values-type (types)
  (declare (list types))
  (let ((last-required (position-if
                        (lambda (type)
                          (not/type (csubtypep (specifier-type 'null) type)))
                        types
                        :from-end t)))
    (if last-required
        (make-values-type :required (subseq types 0 (1+ last-required))
                          :optional (subseq types (1+ last-required))
                          :rest *universal-type*)
        (make-values-type :optional types :rest *universal-type*))))

(defun make-single-value-type (type)
  (make-values-type :required (list type)))

;;; Do the specified OPERATION on TYPE1 and TYPE2, which may be any
;;; type, including VALUES types. With VALUES types such as:
;;;    (VALUES a0 a1)
;;;    (VALUES b0 b1)
;;; we compute the more useful result
;;;    (VALUES (<operation> a0 b0) (<operation> a1 b1))
;;; rather than the precise result
;;;    (<operation> (values a0 a1) (values b0 b1))
;;; This has the virtue of always keeping the VALUES type specifier
;;; outermost, and retains all of the information that is really
;;; useful for static type analysis. We want to know what is always
;;; true of each value independently. It is worthless to know that if
;;; the first value is B0 then the second will be B1.
;;;
;;; If the VALUES count signatures differ, then we produce a result with
;;; the required VALUE count chosen by NREQ when applied to the number
;;; of required values in TYPE1 and TYPE2. Any &KEY values become
;;; &REST T (anyone who uses keyword values deserves to lose.)
;;;
;;; The second value is true if the result is definitely empty or if
;;; OPERATION returned true as its second value each time we called
;;; it. Since we approximate the intersection of VALUES types, the
;;; second value being true doesn't mean the result is exact.
(defun args-type-op (type1 type2 operation nreq)
  (declare (type ctype type1 type2)
           (type function operation nreq))
  (when (eq type1 type2)
    (values type1 t))
  (multiple-value-bind (types1 rest1)
      (values-type-types type1)
    (multiple-value-bind (types2 rest2)
        (values-type-types type2)
      (multiple-value-bind (rest rest-exact)
          (funcall operation rest1 rest2)
        (multiple-value-bind (res res-exact)
            (if (< (length types1) (length types2))
                (fixed-values-op types2 types1 rest1 operation)
                (fixed-values-op types1 types2 rest2 operation))
          (let* ((req (funcall nreq
                               (length (args-type-required type1))
                               (length (args-type-required type2))))
                 (required (subseq res 0 req))
                 (opt (subseq res req)))
            (values required opt rest
                    (and rest-exact res-exact))))))))

(defun values-type-op (type1 type2 operation nreq)
  (multiple-value-bind (required optional rest exactp)
      (args-type-op type1 type2 operation nreq)
    (values (make-values-type :required required
                              :optional optional
                              :rest rest)
            exactp)))

(defun compare-key-args (type1 type2)
  (let ((keys1 (args-type-keywords type1))
        (keys2 (args-type-keywords type2)))
    (and (= (length keys1) (length keys2))
         (eq (args-type-allowp type1)
             (args-type-allowp type2))
         (loop for key1 in keys1
               for match = (find (key-info-name key1)
                                 keys2 :key #'key-info-name)
               always (and match
                           (type= (key-info-type key1)
                                  (key-info-type match)))))))

(defun type=-args (type1 type2)
  (macrolet ((compare (comparator field)
               (let ((reader (symbolicate '#:args-type- field)))
                 `(,comparator (,reader type1) (,reader type2)))))
    (and/type
     (cond ((null (args-type-rest type1))
            (values (null (args-type-rest type2)) t))
           ((null (args-type-rest type2))
            (values nil t))
           (t
            (compare type= rest)))
     (and/type (and/type (compare type=-list required)
                         (compare type=-list optional))
               (if (or (args-type-keyp type1) (args-type-keyp type2))
                   (values (compare-key-args type1 type2) t)
                   (values t t))))))

;;; Do a union or intersection operation on types that might be values
;;; types. The result is optimized for utility rather than exactness,
;;; but it is guaranteed that it will be no smaller (more restrictive)
;;; than the precise result.
;;;
;;; The return convention seems to be analogous to
;;; TYPES-EQUAL-OR-INTERSECT. -- WHN 19990910.
(defun-cached (values-type-union :hash-function #'type-cache-hash
                                 :hash-bits 8)
    ((type1 eq) (type2 eq))
  (declare (type ctype type1 type2))
  (cond ((or (eq type1 *wild-type*) (eq type2 *wild-type*)) *wild-type*)
        ((eq type1 *empty-type*) type2)
        ((eq type2 *empty-type*) type1)
        (t
         (values (values-type-op type1 type2 #'type-union #'min)))))

(defun-cached (values-type-intersection :hash-function #'type-cache-hash
                                        :hash-bits 8)
    ((type1 eq) (type2 eq))
  (declare (type ctype type1 type2))
  (cond ((eq type1 *wild-type*)
         (coerce-to-values type2))
        ((or (eq type2 *wild-type*) (eq type2 *universal-type*))
         type1)
        ((or (eq type1 *empty-type*) (eq type2 *empty-type*))
         *empty-type*)
        ((and (not (values-type-p type2))
              (values-type-required type1))
         (let ((req1 (values-type-required type1)))
           (make-values-type :required (cons (type-intersection (first req1) type2)
                                             (rest req1))
                             :optional (values-type-optional type1)
                             :rest (values-type-rest type1)
                             :allowp (values-type-allowp type1))))
        (t
         (values (values-type-op type1 (coerce-to-values type2)
                                 #'type-intersection
                                 #'max)))))

;;; This is like TYPES-EQUAL-OR-INTERSECT, except that it sort of
;;; works on VALUES types. Note that due to the semantics of
;;; VALUES-TYPE-INTERSECTION, this might return (VALUES T T) when
;;; there isn't really any intersection.
(defun values-types-equal-or-intersect (type1 type2)
  (cond ((or (eq type1 *empty-type*) (eq type2 *empty-type*))
         (values t t))
        ((or (eq type1 *wild-type*) (eq type2 *wild-type*))
         (values t t))
        (t
         (let ((res (values-type-intersection type1 type2)))
           (values (not (eq res *empty-type*))
                   t)))))

;;; a SUBTYPEP-like operation that can be used on any types, including
;;; VALUES types
(defun-cached (values-subtypep :hash-function #'type-cache-hash
                               :hash-bits 8
                               :values 2)
    ((type1 eq) (type2 eq))
  (declare (type ctype type1 type2))
  (cond ((or (eq type2 *wild-type*) (eq type2 *universal-type*)
             (eq type1 *empty-type*))
         (values t t))
        ((eq type1 *wild-type*)
         (values (eq type2 *wild-type*) t))
        ((or (eq type2 *empty-type*)
             (not (values-types-equal-or-intersect type1 type2)))
         (values nil t))
        ((and (not (values-type-p type2))
              (values-type-required type1))
         (csubtypep (first (values-type-required type1))
                    type2))
        (t (setq type2 (coerce-to-values type2))
           (multiple-value-bind (types1 rest1) (values-type-types type1)
             (multiple-value-bind (types2 rest2) (values-type-types type2)
               (cond ((< (length (values-type-required type1))
                         (length (values-type-required type2)))
                      (values nil t))
                     ((< (length types1) (length types2))
                      (values nil nil))
                     (t
                      (do ((t1 types1 (rest t1))
                           (t2 types2 (rest t2)))
                          ((null t2)
                           (csubtypep rest1 rest2))
                        (multiple-value-bind (res win-p)
                            (csubtypep (first t1) (first t2))
                          (unless win-p
                            (return (values nil nil)))
                          (unless res
                            (return (values nil t))))))))))))

;;;; type method interfaces

;;; like SUBTYPEP, only works on CTYPE structures
(defun-cached (csubtypep :hash-function #'type-cache-hash
                         :hash-bits 10
                         :memoizer memoize
                         :values 2)
              ((type1 eq) (type2 eq))
  (declare (type ctype type1 type2))
  (cond ((or (eq type1 type2)
             (eq type1 *empty-type*)
             (eq type2 *universal-type*))
         (values t t))
        #+nil
        ((eq type1 *universal-type*)
         (values nil t))
        (t
         (memoize
          (!invoke-type-method :simple-subtypep :complex-subtypep-arg2
                               type1 type2
                               :complex-arg1 :complex-subtypep-arg1)))))

;;; Just parse the type specifiers and call CSUBTYPE.
(defun sb!xc:subtypep (type1 type2 &optional environment)
  #!+sb-doc
  "Return two values indicating the relationship between type1 and type2.
  If values are T and T, type1 definitely is a subtype of type2.
  If values are NIL and T, type1 definitely is not a subtype of type2.
  If values are NIL and NIL, it couldn't be determined."
  (declare (type lexenv-designator environment) (ignore environment))
  (declare (explicit-check))
  (csubtypep (specifier-type type1) (specifier-type type2)))

;;; If two types are definitely equivalent, return true. The second
;;; value indicates whether the first value is definitely correct.
;;; This should only fail in the presence of HAIRY types.
(defun-cached (type= :hash-function #'type-cache-hash
                     :hash-bits 11
                     :memoizer memoize
                     :values 2)
              ((type1 eq) (type2 eq))
  (declare (type ctype type1 type2))
  (cond ((eq type1 type2)
         (values t t))
        ;; If args are not EQ, but both allow TYPE= optimization,
        ;; and at least one is interned, then return no and certainty.
        ;; Most of the interned CTYPEs admit this optimization,
        ;; NUMERIC and MEMBER types do as well.
        ((and (minusp (logior (type-hash-value type1) (type-hash-value type2)))
              (logtest (logand (type-hash-value type1) (type-hash-value type2))
                       +type-admits-type=-optimization+))
         (values nil t))
        (t
         (memoize (!invoke-type-method :simple-= :complex-= type1 type2)))))

;;; Not exactly the negation of TYPE=, since when the relationship is
;;; uncertain, we still return NIL, NIL. This is useful in cases where
;;; the conservative assumption is =.
(defun type/= (type1 type2)
  (declare (type ctype type1 type2))
  (multiple-value-bind (res win) (type= type1 type2)
    (if win
        (values (not res) t)
        (values nil nil))))

;;; the type method dispatch case of TYPE-UNION2
(defun %type-union2 (type1 type2)
  ;; As in %TYPE-INTERSECTION2, it seems to be a good idea to give
  ;; both argument orders a chance at COMPLEX-INTERSECTION2. Unlike
  ;; %TYPE-INTERSECTION2, though, I don't have a specific case which
  ;; demonstrates this is actually necessary. Also unlike
  ;; %TYPE-INTERSECTION2, there seems to be no need to distinguish
  ;; between not finding a method and having a method return NIL.
  (flet ((1way (x y)
           (!invoke-type-method :simple-union2 :complex-union2
                                x y
                                :default nil)))
    (declare (inline 1way))
    (or (1way type1 type2)
        (1way type2 type1))))

;;; Find a type which includes both types. Any inexactness is
;;; represented by the fuzzy element types; we return a single value
;;; that is precise to the best of our knowledge. This result is
;;; simplified into the canonical form, thus is not a UNION-TYPE
;;; unless we find no other way to represent the result.
(defun-cached (type-union2 :hash-function #'type-cache-hash
                           :hash-bits 11
                           :memoizer memoize)
              ((type1 eq) (type2 eq))
  ;; KLUDGE: This was generated from TYPE-INTERSECTION2 by Ye Olde Cut And
  ;; Paste technique of programming. If it stays around (as opposed to
  ;; e.g. fading away in favor of some CLOS solution) the shared logic
  ;; should probably become shared code. -- WHN 2001-03-16
  (declare (type ctype type1 type2))
  (let ((t2 nil))
    (if (eq type1 type2)
        type1
        (memoize
         (cond
          ;; CSUBTYPEP for array-types answers questions about the
          ;; specialized type, yet for union we want to take the
          ;; expressed type in account too.
          ((and (not (and (array-type-p type1) (array-type-p type2)))
                (or (setf t2 (csubtypep type1 type2))
                    (csubtypep type2 type1)))
           (if t2 type2 type1))
         ((or (union-type-p type1)
              (union-type-p type2))
          ;; Unions of UNION-TYPE should have the UNION-TYPE-TYPES
          ;; values broken out and united separately. The full TYPE-UNION
          ;; function knows how to do this, so let it handle it.
          (type-union type1 type2))
         (t
          ;; the ordinary case: we dispatch to type methods
          (%type-union2 type1 type2)))))))

;;; the type method dispatch case of TYPE-INTERSECTION2
(defun %type-intersection2 (type1 type2)
  ;; We want to give both argument orders a chance at
  ;; COMPLEX-INTERSECTION2. Without that, the old CMU CL type
  ;; methods could give noncommutative results, e.g.
  ;;   (TYPE-INTERSECTION2 *EMPTY-TYPE* SOME-HAIRY-TYPE)
  ;;     => NIL, NIL
  ;;   (TYPE-INTERSECTION2 SOME-HAIRY-TYPE *EMPTY-TYPE*)
  ;;     => #<NAMED-TYPE NIL>, T
  ;; We also need to distinguish between the case where we found a
  ;; type method, and it returned NIL, and the case where we fell
  ;; through without finding any type method. An example of the first
  ;; case is the intersection of a HAIRY-TYPE with some ordinary type.
  ;; An example of the second case is the intersection of two
  ;; completely-unrelated types, e.g. CONS and NUMBER, or SYMBOL and
  ;; ARRAY.
  ;;
  ;; (Why yes, CLOS probably *would* be nicer..)
  (flet ((1way (x y)
           (!invoke-type-method :simple-intersection2 :complex-intersection2
                                x y
                                :default :call-other-method)))
    (declare (inline 1way))
    (let ((xy (1way type1 type2)))
      (or (and (not (eql xy :call-other-method)) xy)
          (let ((yx (1way type2 type1)))
            (or (and (not (eql yx :call-other-method)) yx)
                (cond ((and (eql xy :call-other-method)
                            (eql yx :call-other-method))
                       *empty-type*)
                      (t
                       nil))))))))

(defun-cached (type-intersection2 :hash-function #'type-cache-hash
                                  :hash-bits 11
                                  :memoizer memoize
                                  :values 1)
              ((type1 eq) (type2 eq))
  (declare (type ctype type1 type2))
  (if (eq type1 type2)
         ;; FIXME: For some reason, this doesn't catch e.g. type1 =
         ;; type2 = (SPECIFIER-TYPE
         ;; 'SOME-UNKNOWN-TYPE). Investigate. - CSR, 2002-04-10
      type1
      (memoize
       (cond
        ((or (intersection-type-p type1)
             (intersection-type-p type2))
         ;; Intersections of INTERSECTION-TYPE should have the
         ;; INTERSECTION-TYPE-TYPES values broken out and intersected
         ;; separately. The full TYPE-INTERSECTION function knows how
         ;; to do that, so let it handle it.
         (type-intersection type1 type2))
        (t
         ;; the ordinary case: we dispatch to type methods
         (%type-intersection2 type1 type2))))))

;;; Return as restrictive and simple a type as we can discover that is
;;; no more restrictive than the intersection of TYPE1 and TYPE2. At
;;; worst, we arbitrarily return one of the arguments as the first
;;; value (trying not to return a hairy type).
(defun type-approx-intersection2 (type1 type2)
  (cond ((type-intersection2 type1 type2))
        ((hairy-type-p type1) type2)
        (t type1)))

;;; a test useful for checking whether a derived type matches a
;;; declared type
;;;
;;; The first value is true unless the types don't intersect and
;;; aren't equal. The second value is true if the first value is
;;; definitely correct. NIL is considered to intersect with any type.
;;; If T is a subtype of either type, then we also return T, T. This
;;; way we recognize that hairy types might intersect with T.
;;;
;;; Well now given the statement above that this is "useful for ..."
;;; a particular thing, I see how treating *empty-type* magically could
;;; be useful, however given all the _other_ calls to this function within
;;; this file, it seems suboptimal, because logically it is wrong.
(defun types-equal-or-intersect (type1 type2)
  (declare (type ctype type1 type2))
  (if (or (eq type1 *empty-type*) (eq type2 *empty-type*))
      (values t t)
      (let ((intersection2 (type-intersection2 type1 type2)))
        (cond ((not intersection2)
               (if (or (csubtypep *universal-type* type1)
                       (csubtypep *universal-type* type2))
                   (values t t)
                   (values t nil)))
              ((eq intersection2 *empty-type*) (values nil t))
              (t (values t t))))))

;;; Return a Common Lisp type specifier corresponding to the TYPE
;;; object.
(defun type-specifier (type)
  (declare (type ctype type))
  (funcall (type-class-unparse (type-class-info type)) type))

;;; Don't try to define a print method until it's actually gonna work!
;;; (Otherwise this would be near the DEFSTRUCT)
(def!method print-object ((ctype ctype) stream)
  (print-unreadable-object (ctype stream :type t)
    (prin1 (type-specifier ctype) stream)))

;;; Same here.
;;; Just dump it as a specifier. (We'll convert it back upon loading.)
(defun make-type-load-form (type)
  (declare (type ctype type))
  `(specifier-type ',(type-specifier type)))

(defun-cached (type-negation :hash-function #'type-hash-value
                             :hash-bits 8
                             :values 1)
              ((type eq))
  (declare (type ctype type))
  (funcall (type-class-negate (type-class-info type)) type))

(defun-cached (type-singleton-p :hash-function #'type-hash-value
                             :hash-bits 8
                             :values 2)
              ((type eq))
  (declare (type ctype type))
  (let ((function (type-class-singleton-p (type-class-info type))))
    (if function
        (funcall function type)
        (values nil nil))))

;;; (VALUES-SPECIFIER-TYPE and SPECIFIER-TYPE moved from here to
;;; early-type.lisp by WHN ca. 19990201.)

;;; Take a list of type specifiers, computing the translation of each
;;; specifier and defining it as a builtin type.
;;; Seee the comments in 'type-init' for why this is a slightly
;;; screwy way to go about it.
(declaim (ftype (function (list) (values)) !precompute-types))
(defun !precompute-types (specs)
  (dolist (spec specs)
    (let ((res (handler-bind
                   ((parse-unknown-type
                     (lambda (c)
                       (declare (ignore c))
                       ;; We can handle conditions at this point,
                       ;; but win32 can not perform i/o here because
                       ;; !MAKE-COLD-STDERR-STREAM has no implementation.
                       #!-win32
                       (progn (write-string "//caught: parse-unknown ")
                              (write spec)
                              (terpri)))))
             (specifier-type spec))))
      (unless (unknown-type-p res)
        (setf (info :type :builtin spec) res)
        (setf (info :type :kind spec) :primitive))))
  (values))

;;;; general TYPE-UNION and TYPE-INTERSECTION operations
;;;;
;;;; These are fully general operations on CTYPEs: they'll always
;;;; return a CTYPE representing the result.

;;; shared logic for unions and intersections: Return a list of
;;; types representing the same types as INPUT-TYPES, but with
;;; COMPOUND-TYPEs satisfying %COMPOUND-TYPE-P broken up into their
;;; component types, and with any SIMPLY2 simplifications applied.
(macrolet
    ((def (name compound-type-p simplify2)
         `(defun ,name (types)
            (when types
              (multiple-value-bind (first rest)
                  (if (,compound-type-p (car types))
                      (values (car (compound-type-types (car types)))
                              (append (cdr (compound-type-types (car types)))
                                      (cdr types)))
                      (values (car types) (cdr types)))
                (let ((rest (,name rest)) u)
                  (dolist (r rest (cons first rest))
                    (when (setq u (,simplify2 first r))
                      (return (,name (nsubstitute u r rest)))))))))))
  (def simplify-intersections intersection-type-p type-intersection2)
  (def simplify-unions union-type-p type-union2))

(defun maybe-distribute-one-union (union-type types)
  (let* ((intersection (apply #'type-intersection types))
         (union (mapcar (lambda (x) (type-intersection x intersection))
                        (union-type-types union-type))))
    (if (notany (lambda (x) (or (hairy-type-p x)
                                (intersection-type-p x)))
                union)
        union
        nil)))

(defun type-intersection (&rest input-types)
  (%type-intersection input-types))
(defun-cached (%type-intersection :hash-bits 10 :hash-function #'type-list-cache-hash)
    ((input-types equal))
  (let ((simplified-types (simplify-intersections input-types)))
    (declare (type list simplified-types))
    ;; We want to have a canonical representation of types (or failing
    ;; that, punt to HAIRY-TYPE). Canonical representation would have
    ;; intersections inside unions but not vice versa, since you can
    ;; always achieve that by the distributive rule. But we don't want
    ;; to just apply the distributive rule, since it would be too easy
    ;; to end up with unreasonably huge type expressions. So instead
    ;; we try to generate a simple type by distributing the union; if
    ;; the type can't be made simple, we punt to HAIRY-TYPE.
    (if (and (cdr simplified-types) (some #'union-type-p simplified-types))
        (let* ((first-union (find-if #'union-type-p simplified-types))
               (other-types (coerce (remove first-union simplified-types)
                                    'list))
               (distributed (maybe-distribute-one-union first-union
                                                        other-types)))
          (if distributed
              (apply #'type-union distributed)
              (%make-hairy-type `(and ,@(map 'list #'type-specifier
                                             simplified-types)))))
        (cond
          ((null simplified-types) *universal-type*)
          ((null (cdr simplified-types)) (car simplified-types))
          (t (%make-intersection-type
              (some #'type-enumerable simplified-types)
              simplified-types))))))

(defun type-union (&rest input-types)
  (%type-union input-types))
(defun-cached (%type-union :hash-bits 8 :hash-function #'type-list-cache-hash)
    ((input-types equal))
  (let ((simplified-types (simplify-unions input-types)))
    (cond
      ((null simplified-types) *empty-type*)
      ((null (cdr simplified-types)) (car simplified-types))
      (t (make-union-type
          (every #'type-enumerable simplified-types)
          simplified-types)))))

;;;; built-in types

(!define-type-class named :enumerable nil :might-contain-other-types nil)

;; This is used when parsing (SATISFIES KEYWORDP)
;; so that simplifications can be made when computing intersections,
;; without which we would see this kind of "empty-type in disguise"
;;   (AND (SATISFIES KEYWORDP) CONS)
;; This isn't *keyword-type* because KEYWORD is implemented
;; as the intersection of SYMBOL and (SATISFIES KEYWORDP)
;; We could also intern the KEYWORD type but that would require
;; hacking the INTERSECTION logic.
(defglobal *satisfies-keywordp-type* -1)

;; Here too I discovered more than 1000 instances in a particular
;; Lisp image, when really this is *EMPTY-TYPE*.
;;  (AND (SATISFIES LEGAL-FUN-NAME-P) (SIMPLE-ARRAY CHARACTER (*)))
(defglobal *fun-name-type* -1)

;; !LATE-TYPE-COLD-INIT can't be GCd - there are lambdas in the toplevel code
;; component that leak out and persist - but everything below is GCable.
;; This leads to about 20KB of extra code being retained on x86-64.
;; An educated guess is that DEFINE-SUPERCLASSES is responsible for the problem.
(defun !late-type-cold-init2 ()
 (macrolet ((frob (name var)
              `(progn
                 (setq ,var
                       (mark-ctype-interned (make-named-type :name ',name)))
                 (setf (info :type :kind ',name) :primitive)
                 (setf (info :type :builtin ',name) ,var))))
   ;; KLUDGE: In ANSI, * isn't really the name of a type, it's just a
   ;; special symbol which can be stuck in some places where an
   ;; ordinary type can go, e.g. (ARRAY * 1) instead of (ARRAY T 1).
   ;; In SBCL it also used to denote universal VALUES type.
   (frob * *wild-type*)
   (frob nil *empty-type*)
   (frob t *universal-type*)
   (setf (sb!c::meta-info-default (sb!c::meta-info :variable :type))
         *universal-type*)
   ;; new in sbcl-0.9.5: these used to be CLASSOID types, but that
   ;; view of them was incompatible with requirements on the MOP
   ;; metaobject class hierarchy: the INSTANCE and
   ;; FUNCALLABLE-INSTANCE types are disjoint (instances have
   ;; instance-pointer-lowtag; funcallable-instances have
   ;; fun-pointer-lowtag), while FUNCALLABLE-STANDARD-OBJECT is
   ;; required to be a subclass of STANDARD-OBJECT.  -- CSR,
   ;; 2005-09-09
   (frob instance *instance-type*)
   (frob funcallable-instance *funcallable-instance-type*)
   ;; new in sbcl-1.0.3.3: necessary to act as a join point for the
   ;; extended sequence hierarchy.  (Might be removed later if we use
   ;; a dedicated FUNDAMENTAL-SEQUENCE class for this.)
   (frob extended-sequence *extended-sequence-type*))
 (!intern-important-fun-type-instances)
 (!intern-important-member-type-instances)
 (!intern-important-cons-type-instances)
 (!intern-important-numeric-type-instances)
 (!intern-important-character-set-type-instances)
 (!intern-important-array-type-instances) ; must be after numeric and char
 (setf *satisfies-keywordp-type*
       (mark-ctype-interned (%make-hairy-type '(satisfies keywordp))))
 (setf *fun-name-type*
       (mark-ctype-interned (%make-hairy-type '(satisfies legal-fun-name-p))))
 ;; This is not an important type- no attempt is made to return exactly this
 ;; object when parsing FUNCTION. In fact we return the classoid instead
 (setf *universal-fun-type*
       (make-fun-type :wild-args t :returns *wild-type*)))

(!define-type-method (named :simple-=) (type1 type2)
  ;;(aver (not (eq type1 *wild-type*))) ; * isn't really a type.
  (values (eq type1 type2) t))

(defun cons-type-might-be-empty-type (type)
  (declare (type cons-type type))
  (let ((car-type (cons-type-car-type type))
        (cdr-type (cons-type-cdr-type type)))
    (or
     (if (cons-type-p car-type)
         (cons-type-might-be-empty-type car-type)
         (multiple-value-bind (yes surep)
             (type= car-type *empty-type*)
           (aver (not yes))
           (not surep)))
     (if (cons-type-p cdr-type)
         (cons-type-might-be-empty-type cdr-type)
         (multiple-value-bind (yes surep)
             (type= cdr-type *empty-type*)
           (aver (not yes))
           (not surep))))))

(defun cons-type-length-info (type)
  (declare (type cons-type type))
  (do ((min 1 (1+ min))
       (cdr (cons-type-cdr-type type) (cons-type-cdr-type cdr)))
      ((not (cons-type-p cdr))
       (cond
         ((csubtypep cdr (specifier-type 'null))
          (values min t))
         ((csubtypep *universal-type* cdr)
          (values min nil))
         ((type/= (type-intersection (specifier-type 'cons) cdr) *empty-type*)
          (values min nil))
         ((type/= (type-intersection (specifier-type 'null) cdr) *empty-type*)
          (values min t))
         (t (values min :maybe))))
    ()))

(!define-type-method (named :complex-=) (type1 type2)
  (cond
    ((and (eq type2 *empty-type*)
          (or (and (intersection-type-p type1)
                   ;; not allowed to be unsure on these... FIXME: keep
                   ;; the list of CL types that are intersection types
                   ;; once and only once.
                   (not (or (type= type1 (specifier-type 'ratio))
                            (type= type1 (specifier-type 'keyword)))))
              (and (cons-type-p type1)
                   (cons-type-might-be-empty-type type1))))
     ;; things like (AND (EQL 0) (SATISFIES ODDP)) or (AND FUNCTION
     ;; STREAM) can get here.  In general, we can't really tell
     ;; whether these are equal to NIL or not, so
     (values nil nil))
    ((type-might-contain-other-types-p type1)
     (invoke-complex-=-other-method type1 type2))
    (t (values nil t))))

(!define-type-method (named :simple-subtypep) (type1 type2)
  (aver (not (eq type1 *wild-type*))) ; * isn't really a type.
  (aver (not (eq type1 type2)))
  (values (or (eq type1 *empty-type*)
              (eq type2 *wild-type*)
              (eq type2 *universal-type*)) t))

(!define-type-method (named :complex-subtypep-arg1) (type1 type2)
  ;; This AVER causes problems if we write accurate methods for the
  ;; union (and possibly intersection) types which then delegate to
  ;; us; while a user shouldn't get here, because of the odd status of
  ;; *wild-type* a type-intersection executed by the compiler can. -
  ;; CSR, 2002-04-10
  ;;
  ;; (aver (not (eq type1 *wild-type*))) ; * isn't really a type.
  (cond ((eq type1 *empty-type*)
         t)
        (;; When TYPE2 might be the universal type in disguise
         (type-might-contain-other-types-p type2)
         ;; Now that the UNION and HAIRY COMPLEX-SUBTYPEP-ARG2 methods
         ;; can delegate to us (more or less as CALL-NEXT-METHOD) when
         ;; they're uncertain, we can't just barf on COMPOUND-TYPE and
         ;; HAIRY-TYPEs as we used to. Instead we deal with the
         ;; problem (where at least part of the problem is cases like
         ;;   (SUBTYPEP T '(SATISFIES FOO))
         ;; or
         ;;   (SUBTYPEP T '(AND (SATISFIES FOO) (SATISFIES BAR)))
         ;; where the second type is a hairy type like SATISFIES, or
         ;; is a compound type which might contain a hairy type) by
         ;; returning uncertainty.
         (values nil nil))
        ((eq type1 *funcallable-instance-type*)
         (values (eq type2 (specifier-type 'function)) t))
        (t
         ;; This case would have been picked off by the SIMPLE-SUBTYPEP
         ;; method, and so shouldn't appear here.
         (aver (not (named-type-p type2)))
         ;; Since TYPE2 is not EQ *UNIVERSAL-TYPE* and is not another
         ;; named type in disguise, TYPE2 is not a superset of TYPE1.
         (values nil t))))

(!define-type-method (named :complex-subtypep-arg2) (type1 type2)
  (aver (not (eq type2 *wild-type*))) ; * isn't really a type.
  (cond ((eq type2 *universal-type*)
         (values t t))
        ;; some CONS types can conceal danger
        ((and (cons-type-p type1) (cons-type-might-be-empty-type type1))
         (values nil nil))
        ((type-might-contain-other-types-p type1)
         ;; those types can be other types in disguise.  So we'd
         ;; better delegate.
         (invoke-complex-subtypep-arg1-method type1 type2))
        ((and (or (eq type2 *instance-type*)
                  (eq type2 *funcallable-instance-type*))
              (member-type-p type1))
         ;; member types can be subtypep INSTANCE and
         ;; FUNCALLABLE-INSTANCE in surprising ways.
         (invoke-complex-subtypep-arg1-method type1 type2))
        ((and (eq type2 *extended-sequence-type*) (classoid-p type1))
         (let* ((layout (classoid-layout type1))
                (inherits (layout-inherits layout))
                (sequencep (find (classoid-layout (find-classoid 'sequence))
                                 inherits)))
           (values (if sequencep t nil) t)))
        ((and (eq type2 *instance-type*) (classoid-p type1))
         (if (member type1 *non-instance-classoid-types* :key #'find-classoid)
             (values nil t)
             (let* ((layout (classoid-layout type1))
                    (inherits (layout-inherits layout))
                    (functionp (find (classoid-layout (find-classoid 'function))
                                     inherits)))
               (cond
                 (functionp
                  (values nil t))
                 ((eq type1 (find-classoid 'function))
                  (values nil t))
                 ((or (structure-classoid-p type1)
                      #+nil
                      (condition-classoid-p type1))
                  (values t t))
                 (t (values nil nil))))))
        ((and (eq type2 *funcallable-instance-type*) (classoid-p type1))
         (if (member type1 *non-instance-classoid-types* :key #'find-classoid)
             (values nil t)
             (let* ((layout (classoid-layout type1))
                    (inherits (layout-inherits layout))
                    (functionp (find (classoid-layout (find-classoid 'function))
                                     inherits)))
               (values (if functionp t nil) t))))
        (t
         ;; FIXME: This seems to rely on there only being 4 or 5
         ;; NAMED-TYPE values, and the exclusion of various
         ;; possibilities above. It would be good to explain it and/or
         ;; rewrite it so that it's clearer.
         (values nil t))))

(!define-type-method (named :complex-intersection2) (type1 type2)
  ;; FIXME: This assertion failed when I added it in sbcl-0.6.11.13.
  ;; Perhaps when bug 85 is fixed it can be reenabled.
  ;;(aver (not (eq type2 *wild-type*))) ; * isn't really a type.
  (cond
    ((eq type2 *extended-sequence-type*)
     (typecase type1
       (structure-classoid *empty-type*)
       (classoid
        (if (member type1 *non-instance-classoid-types* :key #'find-classoid)
            *empty-type*
            (if (find (classoid-layout (find-classoid 'sequence))
                      (layout-inherits (classoid-layout type1)))
                type1
                nil)))
       (t
        (if (or (type-might-contain-other-types-p type1)
                (member-type-p type1))
            nil
            *empty-type*))))
    ((eq type2 *instance-type*)
     (typecase type1
       (structure-classoid type1)
       (classoid
        (if (and (not (member type1 *non-instance-classoid-types*
                              :key #'find-classoid))
                 (not (eq type1 (find-classoid 'function)))
                 (not (find (classoid-layout (find-classoid 'function))
                            (layout-inherits (classoid-layout type1)))))
            nil
            *empty-type*))
       (t
        (if (or (type-might-contain-other-types-p type1)
                (member-type-p type1))
            nil
            *empty-type*))))
    ((eq type2 *funcallable-instance-type*)
     (typecase type1
       (structure-classoid *empty-type*)
       (classoid
        (if (member type1 *non-instance-classoid-types* :key #'find-classoid)
            *empty-type*
            (if (find (classoid-layout (find-classoid 'function))
                      (layout-inherits (classoid-layout type1)))
                type1
                (if (type= type1 (find-classoid 'function))
                    type2
                    nil))))
       (fun-type nil)
       (t
        (if (or (type-might-contain-other-types-p type1)
                (member-type-p type1))
            nil
            *empty-type*))))
    (t (hierarchical-intersection2 type1 type2))))

(!define-type-method (named :complex-union2) (type1 type2)
  ;; Perhaps when bug 85 is fixed this can be reenabled.
  ;;(aver (not (eq type2 *wild-type*))) ; * isn't really a type.
  (cond
    ((eq type2 *extended-sequence-type*)
     (if (classoid-p type1)
         (if (or (member type1 *non-instance-classoid-types*
                         :key #'find-classoid)
                 (not (find (classoid-layout (find-classoid 'sequence))
                            (layout-inherits (classoid-layout type1)))))
             nil
             type2)
         nil))
    ((eq type2 *instance-type*)
     (if (classoid-p type1)
         (if (or (member type1 *non-instance-classoid-types*
                         :key #'find-classoid)
                 (find (classoid-layout (find-classoid 'function))
                       (layout-inherits (classoid-layout type1))))
             nil
             type2)
         nil))
    ((eq type2 *funcallable-instance-type*)
     (if (classoid-p type1)
         (if (or (member type1 *non-instance-classoid-types*
                         :key #'find-classoid)
                 (not (find (classoid-layout (find-classoid 'function))
                            (layout-inherits (classoid-layout type1)))))
             nil
             (if (eq type1 (specifier-type 'function))
                 type1
                 type2))
         nil))
    (t (hierarchical-union2 type1 type2))))

(!define-type-method (named :negate) (x)
  (aver (not (eq x *wild-type*)))
  (cond
    ((eq x *universal-type*) *empty-type*)
    ((eq x *empty-type*) *universal-type*)
    ((or (eq x *instance-type*)
         (eq x *funcallable-instance-type*)
         (eq x *extended-sequence-type*))
     (make-negation-type x))
    (t (bug "NAMED type unexpected: ~S" x))))

(!define-type-method (named :unparse) (x)
  (named-type-name x))

;;;; hairy and unknown types
;;;; DEFINE-TYPE-CLASS HAIRY is in 'early-type'

(!define-type-method (hairy :negate) (x) (make-negation-type x))

(!define-type-method (hairy :unparse) (x)
  (hairy-type-specifier x))

(!define-type-method (hairy :simple-subtypep) (type1 type2)
  (let ((hairy-spec1 (hairy-type-specifier type1))
        (hairy-spec2 (hairy-type-specifier type2)))
    (cond ((equal-but-no-car-recursion hairy-spec1 hairy-spec2)
           (values t t))
          ((maybe-reparse-specifier! type1)
           (csubtypep type1 type2))
          ((maybe-reparse-specifier! type2)
           (csubtypep type1 type2))
          (t
           (values nil nil)))))

(!define-type-method (hairy :complex-subtypep-arg2) (type1 type2)
  (if (maybe-reparse-specifier! type2)
      (csubtypep type1 type2)
      (let ((specifier (hairy-type-specifier type2)))
        (cond ((and (consp specifier) (eql (car specifier) 'satisfies))
               (case (cadr specifier)
                 ((keywordp) (if (type= type1 (specifier-type 'symbol))
                                 (values nil t)
                                 (invoke-complex-subtypep-arg1-method type1 type2)))
                 (t (invoke-complex-subtypep-arg1-method type1 type2))))
              (t
               (invoke-complex-subtypep-arg1-method type1 type2))))))

(!define-type-method (hairy :complex-subtypep-arg1) (type1 type2)
  (if (maybe-reparse-specifier! type1)
      (csubtypep type1 type2)
      (values nil nil)))

(!define-type-method (hairy :complex-=) (type1 type2)
  (if (maybe-reparse-specifier! type2)
      (type= type1 type2)
      (values nil nil)))

(!define-type-method (hairy :simple-intersection2 :complex-intersection2)
                     (type1 type2)
 (acond ((type= type1 type2)
         type1)
        ((eq type2 *satisfies-keywordp-type*)
         ;; (AND (MEMBER A) (SATISFIES KEYWORDP)) is possibly non-empty
         ;; if A is re-homed as :A. However as a special case that really
         ;; does occur, (AND (MEMBER NIL) (SATISFIES KEYWORDP))
         ;; is empty because of the illegality of changing NIL's package.
         (if (eq type1 *null-type*)
             *empty-type*
             (multiple-value-bind (answer certain)
                 (types-equal-or-intersect type1 (specifier-type 'symbol))
               (and (not answer) certain *empty-type*))))
        ((eq type2 *fun-name-type*)
         (multiple-value-bind (answer certain)
             (types-equal-or-intersect type1 (specifier-type 'symbol))
           (and (not answer)
                certain
                (multiple-value-bind (answer certain)
                    (types-equal-or-intersect type1 (specifier-type 'cons))
                  (and (not answer) certain *empty-type*)))))
        ((and (typep (hairy-type-specifier type2) '(cons (eql satisfies)))
              (info :function :predicate-truth-constraint
                    (cadr (hairy-type-specifier type2))))
         (multiple-value-bind (answer certain)
             (types-equal-or-intersect type1 (specifier-type it))
           (and (not answer) certain *empty-type*)))))

(!define-type-method (hairy :simple-union2)
                     (type1 type2)
  (if (type= type1 type2)
      type1
      nil))

(!define-type-method (hairy :simple-=) (type1 type2)
  (if (equal-but-no-car-recursion (hairy-type-specifier type1)
                                  (hairy-type-specifier type2))
      (values t t)
      (values nil nil)))

(!def-type-translator satisfies :list (&whole whole predicate-name)
  (unless (symbolp predicate-name)
    (error 'simple-type-error
           :datum predicate-name
           :expected-type 'symbol
           :format-control "The SATISFIES predicate name is not a symbol: ~S"
           :format-arguments (list predicate-name)))
  (case predicate-name
   (keywordp *satisfies-keywordp-type*)
   (legal-fun-name-p *fun-name-type*)
   (t (%make-hairy-type whole))))

;;;; negation types

(!define-type-method (negation :negate) (x)
  (negation-type-type x))

(!define-type-method (negation :unparse) (x)
  (if (type= (negation-type-type x) (specifier-type 'cons))
      'atom
      `(not ,(type-specifier (negation-type-type x)))))

(!define-type-method (negation :simple-subtypep) (type1 type2)
  (csubtypep (negation-type-type type2) (negation-type-type type1)))

(!define-type-method (negation :complex-subtypep-arg2) (type1 type2)
  (let* ((complement-type2 (negation-type-type type2))
         (intersection2 (type-intersection2 type1
                                            complement-type2)))
    (if intersection2
        ;; FIXME: if uncertain, maybe try arg1?
        (type= intersection2 *empty-type*)
        (invoke-complex-subtypep-arg1-method type1 type2))))

(!define-type-method (negation :complex-subtypep-arg1) (type1 type2)
  ;; "Incrementally extended heuristic algorithms tend inexorably toward the
  ;; incomprehensible." -- http://www.unlambda.com/~james/lambda/lambda.txt
  ;;
  ;; You may not believe this. I couldn't either. But then I sat down
  ;; and drew lots of Venn diagrams. Comments involving a and b refer
  ;; to the call (subtypep '(not a) 'b) -- CSR, 2002-02-27.
  (block nil
    ;; (Several logical truths in this block are true as long as
    ;; b/=T. As of sbcl-0.7.1.28, it seems impossible to construct a
    ;; case with b=T where we actually reach this type method, but
    ;; we'll test for and exclude this case anyway, since future
    ;; maintenance might make it possible for it to end up in this
    ;; code.)
    (multiple-value-bind (equal certain)
        (type= type2 *universal-type*)
      (unless certain
        (return (values nil nil)))
      (when equal
        (return (values t t))))
    (let ((complement-type1 (negation-type-type type1)))
      ;; Do the special cases first, in order to give us a chance if
      ;; subtype/supertype relationships are hairy.
      (multiple-value-bind (equal certain)
          (type= complement-type1 type2)
        ;; If a = b, ~a is not a subtype of b (unless b=T, which was
        ;; excluded above).
        (unless certain
          (return (values nil nil)))
        (when equal
          (return (values nil t))))
      ;; KLUDGE: ANSI requires that the SUBTYPEP result between any
      ;; two built-in atomic type specifiers never be uncertain. This
      ;; is hard to do cleanly for the built-in types whose
      ;; definitions include (NOT FOO), i.e. CONS and RATIO. However,
      ;; we can do it with this hack, which uses our global knowledge
      ;; that our implementation of the type system uses disjoint
      ;; implementation types to represent disjoint sets (except when
      ;; types are contained in other types).  (This is a KLUDGE
      ;; because it's fragile. Various changes in internal
      ;; representation in the type system could make it start
      ;; confidently returning incorrect results.) -- WHN 2002-03-08
      (unless (or (type-might-contain-other-types-p complement-type1)
                  (type-might-contain-other-types-p type2))
        ;; Because of the way our types which don't contain other
        ;; types are disjoint subsets of the space of possible values,
        ;; (SUBTYPEP '(NOT AA) 'B)=NIL when AA and B are simple (and B
        ;; is not T, as checked above).
        (return (values nil t)))
      ;; The old (TYPE= TYPE1 TYPE2) branch would never be taken, as
      ;; TYPE1 and TYPE2 will only be equal if they're both NOT types,
      ;; and then the :SIMPLE-SUBTYPEP method would be used instead.
      ;; But a CSUBTYPEP relationship might still hold:
      (multiple-value-bind (equal certain)
          (csubtypep complement-type1 type2)
        ;; If a is a subtype of b, ~a is not a subtype of b (unless
        ;; b=T, which was excluded above).
        (unless certain
          (return (values nil nil)))
        (when equal
          (return (values nil t))))
      (multiple-value-bind (equal certain)
          (csubtypep type2 complement-type1)
        ;; If b is a subtype of a, ~a is not a subtype of b.  (FIXME:
        ;; That's not true if a=T. Do we know at this point that a is
        ;; not T?)
        (unless certain
          (return (values nil nil)))
        (when equal
          (return (values nil t))))
      ;; old CSR comment ca. 0.7.2, now obsoleted by the SIMPLE-CTYPE?
      ;; KLUDGE case above: Other cases here would rely on being able
      ;; to catch all possible cases, which the fragility of this type
      ;; system doesn't inspire me; for instance, if a is type= to ~b,
      ;; then we want T, T; if this is not the case and the types are
      ;; disjoint (have an intersection of *empty-type*) then we want
      ;; NIL, T; else if the union of a and b is the *universal-type*
      ;; then we want T, T. So currently we still claim to be unsure
      ;; about e.g. (subtypep '(not fixnum) 'single-float).
      ;;
      ;; OTOH we might still get here:
      (values nil nil))))

(!define-type-method (negation :complex-=) (type1 type2)
  ;; (NOT FOO) isn't equivalent to anything that's not a negation
  ;; type, except possibly a type that might contain it in disguise.
  (declare (ignore type2))
  (if (type-might-contain-other-types-p type1)
      (values nil nil)
      (values nil t)))

(!define-type-method (negation :simple-intersection2) (type1 type2)
  (let ((not1 (negation-type-type type1))
        (not2 (negation-type-type type2)))
    (cond
      ((csubtypep not1 not2) type2)
      ((csubtypep not2 not1) type1)
      ;; Why no analagous clause to the disjoint in the SIMPLE-UNION2
      ;; method, below?  The clause would read
      ;;
      ;; ((EQ (TYPE-UNION NOT1 NOT2) *UNIVERSAL-TYPE*) *EMPTY-TYPE*)
      ;;
      ;; but with proper canonicalization of negation types, there's
      ;; no way of constructing two negation types with union of their
      ;; negations being the universal type.
      (t
       (aver (not (eq (type-union not1 not2) *universal-type*)))
       nil))))

(defun maybe-complex-array-refinement (type1 type2)
  (let* ((ntype (negation-type-type type2))
         (ndims (array-type-dimensions ntype))
         (ncomplexp (array-type-complexp ntype))
         (nseltype (array-type-specialized-element-type ntype))
         (neltype (array-type-element-type ntype)))
    (if (and (eql ndims '*) (null ncomplexp)
             (eq neltype *wild-type*) (eq nseltype *wild-type*))
        (make-array-type (array-type-dimensions type1)
                         :complexp t
                         :element-type (array-type-element-type type1)
                         :specialized-element-type (array-type-specialized-element-type type1)))))

(!define-type-method (negation :complex-intersection2) (type1 type2)
  (cond
    ((csubtypep type1 (negation-type-type type2)) *empty-type*)
    ((eq (type-intersection type1 (negation-type-type type2)) *empty-type*)
     type1)
    ((and (array-type-p type1) (array-type-p (negation-type-type type2)))
     (maybe-complex-array-refinement type1 type2))
    (t nil)))

(!define-type-method (negation :simple-union2) (type1 type2)
  (let ((not1 (negation-type-type type1))
        (not2 (negation-type-type type2)))
    (cond
      ((csubtypep not1 not2) type1)
      ((csubtypep not2 not1) type2)
      ((eq (type-intersection not1 not2) *empty-type*)
       *universal-type*)
      (t nil))))

(!define-type-method (negation :complex-union2) (type1 type2)
  (cond
    ((csubtypep (negation-type-type type2) type1) *universal-type*)
    ((eq (type-intersection type1 (negation-type-type type2)) *empty-type*)
     type2)
    (t nil)))

(!define-type-method (negation :simple-=) (type1 type2)
  (type= (negation-type-type type1) (negation-type-type type2)))

(!def-type-translator not :list ((:context context) typespec)
  (type-negation (specifier-type-r context typespec)))

;;;; numeric types

(!define-type-class number :enumerable #'numeric-type-enumerable
                    :might-contain-other-types nil)

(declaim (inline numeric-type-equal))
(defun numeric-type-equal (type1 type2)
  (and (eq (numeric-type-class type1) (numeric-type-class type2))
       (eq (numeric-type-format type1) (numeric-type-format type2))
       (eq (numeric-type-complexp type1) (numeric-type-complexp type2))))

(!define-type-method (number :simple-=) (type1 type2)
  (values
   (and (numeric-type-equal type1 type2)
        (equalp (numeric-type-low type1) (numeric-type-low type2))
        (equalp (numeric-type-high type1) (numeric-type-high type2)))
   t))

(!define-type-method (number :negate) (type)
  (if (and (null (numeric-type-low type)) (null (numeric-type-high type)))
      (make-negation-type type)
      (type-union
       (make-negation-type (modified-numeric-type type :low nil :high nil))
       (cond
         ((null (numeric-type-low type))
          (modified-numeric-type
           type
           :low (let ((h (numeric-type-high type)))
                  (if (consp h) (car h) (list h)))
           :high nil))
         ((null (numeric-type-high type))
          (modified-numeric-type
           type
           :low nil
           :high (let ((l (numeric-type-low type)))
                   (if (consp l) (car l) (list l)))))
         (t (type-union
             (modified-numeric-type
              type
              :low nil
              :high (let ((l (numeric-type-low type)))
                      (if (consp l) (car l) (list l))))
             (modified-numeric-type
              type
              :low (let ((h (numeric-type-high type)))
                     (if (consp h) (car h) (list h)))
              :high nil)))))))

(!define-type-method (number :unparse) (type)
  (let* ((complexp (numeric-type-complexp type))
         (low (numeric-type-low type))
         (high (numeric-type-high type))
         (base (case (numeric-type-class type)
                 (integer 'integer)
                 (rational 'rational)
                 (float (or (numeric-type-format type) 'float))
                 (t 'real))))
    (let ((base+bounds
           (cond ((and (eq base 'integer) high low)
                  (let ((high-count (logcount high))
                        (high-length (integer-length high)))
                    (cond ((= low 0)
                           (cond ((= high 0) '(integer 0 0))
                                 ((= high 1) 'bit)
                                 ((and (= high-count high-length)
                                       (plusp high-length))
                                  `(unsigned-byte ,high-length))
                                 (t
                                  `(mod ,(1+ high)))))
                          ((and (= low sb!xc:most-negative-fixnum)
                                (= high sb!xc:most-positive-fixnum))
                           'fixnum)
                          ((and (= low (lognot high))
                                (= high-count high-length)
                                (> high-count 0))
                           `(signed-byte ,(1+ high-length)))
                          (t
                           `(integer ,low ,high)))))
                 (high `(,base ,(or low '*) ,high))
                 (low
                  (if (and (eq base 'integer) (= low 0))
                      'unsigned-byte
                      `(,base ,low)))
                 (t base))))
      (ecase complexp
        (:real
         base+bounds)
        (:complex
         (aver (neq base+bounds 'real))
         `(complex ,base+bounds))
        ((nil)
         (aver (eq base+bounds 'real))
         'number)))))

(!define-type-method (number :singleton-p) (type)
  (let ((low  (numeric-type-low  type))
        (high (numeric-type-high type)))
    (if (and low
             (eql low high)
             (eql (numeric-type-complexp type) :real)
             (member (numeric-type-class type) '(integer rational
                                                 #-sb-xc-host float)))
        (values t (numeric-type-low type))
        (values nil nil))))

;;; Return true if X is "less than or equal" to Y, taking open bounds
;;; into consideration. CLOSED is the predicate used to test the bound
;;; on a closed interval (e.g. <=), and OPEN is the predicate used on
;;; open bounds (e.g. <). Y is considered to be the outside bound, in
;;; the sense that if it is infinite (NIL), then the test succeeds,
;;; whereas if X is infinite, then the test fails (unless Y is also
;;; infinite).
;;;
;;; This is for comparing bounds of the same kind, e.g. upper and
;;; upper. Use NUMERIC-BOUND-TEST* for different kinds of bounds.
(defmacro numeric-bound-test (x y closed open)
  `(cond ((not ,y) t)
         ((not ,x) nil)
         ((consp ,x)
          (if (consp ,y)
              (,closed (car ,x) (car ,y))
              (,closed (car ,x) ,y)))
         (t
          (if (consp ,y)
              (,open ,x (car ,y))
              (,closed ,x ,y)))))

;;; This is used to compare upper and lower bounds. This is different
;;; from the same-bound case:
;;; -- Since X = NIL is -infinity, whereas y = NIL is +infinity, we
;;;    return true if *either* arg is NIL.
;;; -- an open inner bound is "greater" and also squeezes the interval,
;;;    causing us to use the OPEN test for those cases as well.
(defmacro numeric-bound-test* (x y closed open)
  `(cond ((not ,y) t)
         ((not ,x) t)
         ((consp ,x)
          (if (consp ,y)
              (,open (car ,x) (car ,y))
              (,open (car ,x) ,y)))
         (t
          (if (consp ,y)
              (,open ,x (car ,y))
              (,closed ,x ,y)))))

;;; Return whichever of the numeric bounds X and Y is "maximal"
;;; according to the predicates CLOSED (e.g. >=) and OPEN (e.g. >).
;;; This is only meaningful for maximizing like bounds, i.e. upper and
;;; upper. If MAX-P is true, then we return NIL if X or Y is NIL,
;;; otherwise we return the other arg.
(defmacro numeric-bound-max (x y closed open max-p)
  (once-only ((n-x x)
              (n-y y))
    `(cond ((not ,n-x) ,(if max-p nil n-y))
           ((not ,n-y) ,(if max-p nil n-x))
           ((consp ,n-x)
            (if (consp ,n-y)
                (if (,closed (car ,n-x) (car ,n-y)) ,n-x ,n-y)
                (if (,open (car ,n-x) ,n-y) ,n-x ,n-y)))
           (t
            (if (consp ,n-y)
                (if (,open (car ,n-y) ,n-x) ,n-y ,n-x)
                (if (,closed ,n-y ,n-x) ,n-y ,n-x))))))

(!define-type-method (number :simple-subtypep) (type1 type2)
  (let ((class1 (numeric-type-class type1))
        (class2 (numeric-type-class type2))
        (complexp2 (numeric-type-complexp type2))
        (format2 (numeric-type-format type2))
        (low1 (numeric-type-low type1))
        (high1 (numeric-type-high type1))
        (low2 (numeric-type-low type2))
        (high2 (numeric-type-high type2)))
    ;; If one is complex and the other isn't, they are disjoint.
    (cond ((not (or (eq (numeric-type-complexp type1) complexp2)
                    (null complexp2)))
           (values nil t))
          ;; If the classes are specified and different, the types are
          ;; disjoint unless type2 is RATIONAL and type1 is INTEGER.
          ;; [ or type1 is INTEGER and type2 is of the form (RATIONAL
          ;; X X) for integral X, but this is dealt with in the
          ;; canonicalization inside MAKE-NUMERIC-TYPE ]
          ((not (or (eq class1 class2)
                    (null class2)
                    (and (eq class1 'integer) (eq class2 'rational))))
           (values nil t))
          ;; If the float formats are specified and different, the types
          ;; are disjoint.
          ((not (or (eq (numeric-type-format type1) format2)
                    (null format2)))
           (values nil t))
          ;; Check the bounds.
          ((and (numeric-bound-test low1 low2 >= >)
                (numeric-bound-test high1 high2 <= <))
           (values t t))
          (t
           (values nil t)))))

(!define-superclasses number ((number)) !cold-init-forms)

;;; If the high bound of LOW is adjacent to the low bound of HIGH,
;;; then return true, otherwise NIL.
(defun numeric-types-adjacent (low high)
  (let ((low-bound (numeric-type-high low))
        (high-bound (numeric-type-low high)))
    (cond ((not (and low-bound high-bound)) nil)
          ((and (consp low-bound) (consp high-bound)) nil)
          ((consp low-bound)
           (let ((low-value (car low-bound)))
             (or (eql low-value high-bound)
                 (and (eql low-value
                           (load-time-value (make-unportable-float
                                             :single-float-negative-zero)))
                      (eql high-bound 0f0))
                 (and (eql low-value 0f0)
                      (eql high-bound
                           (load-time-value (make-unportable-float
                                             :single-float-negative-zero))))
                 (and (eql low-value
                           (load-time-value (make-unportable-float
                                             :double-float-negative-zero)))
                      (eql high-bound 0d0))
                 (and (eql low-value 0d0)
                      (eql high-bound
                           (load-time-value (make-unportable-float
                                             :double-float-negative-zero)))))))
          ((consp high-bound)
           (let ((high-value (car high-bound)))
             (or (eql high-value low-bound)
                 (and (eql high-value
                           (load-time-value (make-unportable-float
                                             :single-float-negative-zero)))
                      (eql low-bound 0f0))
                 (and (eql high-value 0f0)
                      (eql low-bound
                           (load-time-value (make-unportable-float
                                             :single-float-negative-zero))))
                 (and (eql high-value
                           (load-time-value (make-unportable-float
                                             :double-float-negative-zero)))
                      (eql low-bound 0d0))
                 (and (eql high-value 0d0)
                      (eql low-bound
                           (load-time-value (make-unportable-float
                                             :double-float-negative-zero)))))))
          ((and (eq (numeric-type-class low) 'integer)
                (eq (numeric-type-class high) 'integer))
           (eql (1+ low-bound) high-bound))
          (t
           nil))))

;;; Return a numeric type that is a supertype for both TYPE1 and TYPE2.
;;;
;;; Binding *APPROXIMATE-NUMERIC-UNIONS* to T allows merging non-adjacent
;;; numeric types, eg (OR (INTEGER 0 12) (INTEGER 20 128)) => (INTEGER 0 128),
;;; the compiler does this occasionally during type-derivation to avoid
;;; creating absurdly complex unions of numeric types.
(defvar *approximate-numeric-unions* nil)

(!define-type-method (number :simple-union2) (type1 type2)
  (declare (type numeric-type type1 type2))
  (cond ((csubtypep type1 type2) type2)
        ((csubtypep type2 type1) type1)
        (t
         (let ((class1 (numeric-type-class type1))
               (format1 (numeric-type-format type1))
               (complexp1 (numeric-type-complexp type1))
               (class2 (numeric-type-class type2))
               (format2 (numeric-type-format type2))
               (complexp2 (numeric-type-complexp type2)))
           (cond
             ((and (eq class1 class2)
                   (eq format1 format2)
                   (eq complexp1 complexp2)
                   (or *approximate-numeric-unions*
                       (numeric-types-intersect type1 type2)
                       (numeric-types-adjacent type1 type2)
                       (numeric-types-adjacent type2 type1)))
              (make-numeric-type
               :class class1
               :format format1
               :complexp complexp1
               :low (numeric-bound-max (numeric-type-low type1)
                                       (numeric-type-low type2)
                                       <= < t)
               :high (numeric-bound-max (numeric-type-high type1)
                                        (numeric-type-high type2)
                                        >= > t)))
             ;; FIXME: These two clauses are almost identical, and the
             ;; consequents are in fact identical in every respect.
             ((and (eq class1 'rational)
                   (eq class2 'integer)
                   (eq format1 format2)
                   (eq complexp1 complexp2)
                   (integerp (numeric-type-low type2))
                   (integerp (numeric-type-high type2))
                   (= (numeric-type-low type2) (numeric-type-high type2))
                   (or *approximate-numeric-unions*
                       (numeric-types-adjacent type1 type2)
                       (numeric-types-adjacent type2 type1)))
              (make-numeric-type
               :class 'rational
               :format format1
               :complexp complexp1
               :low (numeric-bound-max (numeric-type-low type1)
                                       (numeric-type-low type2)
                                       <= < t)
               :high (numeric-bound-max (numeric-type-high type1)
                                        (numeric-type-high type2)
                                        >= > t)))
             ((and (eq class1 'integer)
                   (eq class2 'rational)
                   (eq format1 format2)
                   (eq complexp1 complexp2)
                   (integerp (numeric-type-low type1))
                   (integerp (numeric-type-high type1))
                   (= (numeric-type-low type1) (numeric-type-high type1))
                   (or *approximate-numeric-unions*
                       (numeric-types-adjacent type1 type2)
                       (numeric-types-adjacent type2 type1)))
              (make-numeric-type
               :class 'rational
               :format format1
               :complexp complexp1
               :low (numeric-bound-max (numeric-type-low type1)
                                       (numeric-type-low type2)
                                       <= < t)
               :high (numeric-bound-max (numeric-type-high type1)
                                        (numeric-type-high type2)
                                        >= > t)))
             (t nil))))))


(!cold-init-forms ;; is !PRECOMPUTE-TYPES not doing the right thing?
  (setf (info :type :kind 'number) :primitive)
  (setf (info :type :builtin 'number)
        (make-numeric-type :complexp nil)))

(!def-type-translator complex ((:context context) &optional (typespec '*))
  (if (eq typespec '*)
      (specifier-type '(complex real))
      (labels ((not-numeric ()
                 (error "The component type for COMPLEX is not numeric: ~S"
                        typespec))
               (not-real ()
                 (error "The component type for COMPLEX is not a subtype of REAL: ~S"
                        typespec))
               (complex1 (component-type)
                 (unless (numeric-type-p component-type)
                   (not-numeric))
                 (when (eq (numeric-type-complexp component-type) :complex)
                   (not-real))
                 (if (csubtypep component-type (specifier-type '(eql 0)))
                     *empty-type*
                     (modified-numeric-type component-type
                                            :complexp :complex)))
               (do-complex (ctype)
                 (cond
                   ((eq ctype *empty-type*) *empty-type*)
                   ((eq ctype *universal-type*) (not-real))
                   ((typep ctype 'numeric-type) (complex1 ctype))
                   ((typep ctype 'union-type)
                    (apply #'type-union
                           (mapcar #'do-complex (union-type-types ctype))))
                   ((typep ctype 'member-type)
                    (apply #'type-union
                           (mapcar-member-type-members
                            (lambda (x) (do-complex (ctype-of x)))
                            ctype)))
                   ((and (typep ctype 'intersection-type)
                         ;; FIXME: This is very much a
                         ;; not-quite-worst-effort, but we are required to do
                         ;; something here because of our representation of
                         ;; RATIO as (AND RATIONAL (NOT INTEGER)): we must
                         ;; allow users to ask about (COMPLEX RATIO).  This
                         ;; will of course fail to work right on such types
                         ;; as (AND INTEGER (SATISFIES ZEROP))...
                         (let ((numbers (remove-if-not
                                         #'numeric-type-p
                                         (intersection-type-types ctype))))
                           (and (car numbers)
                                (null (cdr numbers))
                                (eq (numeric-type-complexp (car numbers)) :real)
                                (complex1 (car numbers))))))
                   (t
                    (multiple-value-bind (subtypep certainly)
                        (csubtypep ctype (specifier-type 'real))
                      (if (and (not subtypep) certainly)
                          (not-real)
                          ;; ANSI just says that TYPESPEC is any subtype of
                          ;; type REAL, not necessarily a NUMERIC-TYPE. In
                          ;; particular, at this point TYPESPEC could legally
                          ;; be a hairy type like (AND NUMBER (SATISFIES
                          ;; REALP) (SATISFIES ZEROP)), in which case we fall
                          ;; through the logic above and end up here,
                          ;; stumped.
                          ;; FIXME: (COMPLEX NUMBER) is not rejected but should
                          ;; be, as NUMBER is clearly not a subtype of real.
                          (bug "~@<(known bug #145): The type ~S is too hairy to be ~
used for a COMPLEX component.~:@>"
                               typespec)))))))
        (let ((ctype (specifier-type-r context typespec)))
          (do-complex ctype)))))

;;; If X is *, return NIL, otherwise return the bound, which must be a
;;; member of TYPE or a one-element list of a member of TYPE.
#!-sb-fluid (declaim (inline canonicalized-bound))
(defun canonicalized-bound (bound type)
  (cond ((eq bound '*) nil)
        ((or (sb!xc:typep bound type)
             (and (consp bound)
                  (sb!xc:typep (car bound) type)
                  (null (cdr bound))))
          bound)
        (t
         (error "Bound is not ~S, a ~S or a list of a ~S: ~S"
                '*
                type
                type
                bound))))

(!def-type-translator integer (&optional (low '*) (high '*))
  (let* ((l (canonicalized-bound low 'integer))
         (lb (if (consp l) (1+ (car l)) l))
         (h (canonicalized-bound high 'integer))
         (hb (if (consp h) (1- (car h)) h)))
    (if (and hb lb (< hb lb))
        *empty-type*
      (make-numeric-type :class 'integer
                         :complexp :real
                         :enumerable (not (null (and l h)))
                         :low lb
                         :high hb))))

(defmacro !def-bounded-type (type class format)
  `(!def-type-translator ,type (&optional (low '*) (high '*))
     (let ((lb (canonicalized-bound low ',type))
           (hb (canonicalized-bound high ',type)))
       (if (not (numeric-bound-test* lb hb <= <))
           *empty-type*
         (make-numeric-type :class ',class
                            :format ',format
                            :low lb
                            :high hb)))))

(!def-bounded-type rational rational nil)

;;; Unlike CMU CL, we represent the types FLOAT and REAL as
;;; UNION-TYPEs of more primitive types, in order to make
;;; type representation more unique, avoiding problems in the
;;; simplification of things like
;;;   (subtypep '(or (single-float -1.0 1.0) (single-float 0.1))
;;;             '(or (real -1 7) (single-float 0.1) (single-float -1.0 1.0)))
;;; When we allowed REAL to remain as a separate NUMERIC-TYPE,
;;; it was too easy for the first argument to be simplified to
;;; '(SINGLE-FLOAT -1.0), and for the second argument to be simplified
;;; to '(OR (REAL -1 7) (SINGLE-FLOAT 0.1)) and then for the
;;; SUBTYPEP to fail (returning NIL,T instead of T,T) because
;;; the first argument can't be seen to be a subtype of any of the
;;; terms in the second argument.
;;;
;;; The old CMU CL way was:
;;;   (!def-bounded-type float float nil)
;;;   (!def-bounded-type real nil nil)
;;;
;;; FIXME: If this new way works for a while with no weird new
;;; problems, we can go back and rip out support for separate FLOAT
;;; and REAL flavors of NUMERIC-TYPE. The new way was added in
;;; sbcl-0.6.11.22, 2001-03-21.
;;;
;;; FIXME: It's probably necessary to do something to fix the
;;; analogous problem with INTEGER and RATIONAL types. Perhaps
;;; bounded RATIONAL types should be represented as (OR RATIO INTEGER).
(defun coerce-bound (bound type upperp inner-coerce-bound-fun)
  (declare (type function inner-coerce-bound-fun))
  (if (eql bound '*)
      bound
      (funcall inner-coerce-bound-fun bound type upperp)))
(defun inner-coerce-real-bound (bound type upperp)
  #+sb-xc-host (declare (ignore upperp))
  (let #+sb-xc-host ()
       #-sb-xc-host
       ((nl (load-time-value (symbol-value 'sb!xc:most-negative-long-float)))
        (pl (load-time-value (symbol-value 'sb!xc:most-positive-long-float))))
    (let ((nbound (if (consp bound) (car bound) bound))
          (consp (consp bound)))
      (ecase type
        (rational
         (if consp
             (list (rational nbound))
             (rational nbound)))
        (float
         (cond
           ((floatp nbound) bound)
           (t
            ;; Coerce to the widest float format available, to avoid
            ;; unnecessary loss of precision, but don't coerce
            ;; unrepresentable numbers, except on the host where we
            ;; shouldn't be making these types (but KLUDGE: can't even
            ;; assert portably that we're not).
            #-sb-xc-host
            (ecase upperp
              ((nil)
               (when (< nbound nl) (return-from inner-coerce-real-bound nl)))
              ((t)
               (when (> nbound pl) (return-from inner-coerce-real-bound pl))))
            (let ((result (coerce nbound 'long-float)))
              (if consp (list result) result)))))))))
(defun inner-coerce-float-bound (bound type upperp)
  #+sb-xc-host (declare (ignore upperp))
  (let #+sb-xc-host ()
       #-sb-xc-host
       ((nd (load-time-value (symbol-value 'sb!xc:most-negative-double-float)))
        (pd (load-time-value (symbol-value 'sb!xc:most-positive-double-float)))
        (ns (load-time-value (symbol-value 'sb!xc:most-negative-single-float)))
        (ps (load-time-value
             (symbol-value 'sb!xc:most-positive-single-float))))
    (let ((nbound (if (consp bound) (car bound) bound))
          (consp (consp bound)))
      (ecase type
        (single-float
         (cond
           ((typep nbound 'single-float) bound)
           (t
            #-sb-xc-host
            (ecase upperp
              ((nil)
               (when (< nbound ns) (return-from inner-coerce-float-bound ns)))
              ((t)
               (when (> nbound ps) (return-from inner-coerce-float-bound ps))))
            (let ((result (coerce nbound 'single-float)))
              (if consp (list result) result)))))
        (double-float
         (cond
           ((typep nbound 'double-float) bound)
           (t
            #-sb-xc-host
            (ecase upperp
              ((nil)
               (when (< nbound nd) (return-from inner-coerce-float-bound nd)))
              ((t)
               (when (> nbound pd) (return-from inner-coerce-float-bound pd))))
            (let ((result (coerce nbound 'double-float)))
              (if consp (list result) result)))))))))
(defun coerced-real-bound (bound type upperp)
  (coerce-bound bound type upperp #'inner-coerce-real-bound))
(defun coerced-float-bound (bound type upperp)
  (coerce-bound bound type upperp #'inner-coerce-float-bound))
(!def-type-translator real (&optional (low '*) (high '*))
  (specifier-type `(or (float ,(coerced-real-bound  low 'float nil)
                              ,(coerced-real-bound high 'float t))
                       (rational ,(coerced-real-bound  low 'rational nil)
                                 ,(coerced-real-bound high 'rational t)))))
(!def-type-translator float (&optional (low '*) (high '*))
  (specifier-type
   `(or (single-float ,(coerced-float-bound  low 'single-float nil)
                      ,(coerced-float-bound high 'single-float t))
        (double-float ,(coerced-float-bound  low 'double-float nil)
                      ,(coerced-float-bound high 'double-float t))
        #!+long-float ,(error "stub: no long float support yet"))))

(defmacro !define-float-format (f)
  `(!def-bounded-type ,f float ,f))

;; (!define-float-format short-float) ; it's a DEFTYPE
(!define-float-format single-float)
(!define-float-format double-float)
;; long-float support is dead.
;; (!define-float-format long-float) ; also a DEFTYPE

(defun numeric-types-intersect (type1 type2)
  (declare (type numeric-type type1 type2))
  (let* ((class1 (numeric-type-class type1))
         (class2 (numeric-type-class type2))
         (complexp1 (numeric-type-complexp type1))
         (complexp2 (numeric-type-complexp type2))
         (format1 (numeric-type-format type1))
         (format2 (numeric-type-format type2))
         (low1 (numeric-type-low type1))
         (high1 (numeric-type-high type1))
         (low2 (numeric-type-low type2))
         (high2 (numeric-type-high type2)))
    ;; If one is complex and the other isn't, then they are disjoint.
    (cond ((not (or (eq complexp1 complexp2)
                    (null complexp1) (null complexp2)))
           nil)
          ;; If either type is a float, then the other must either be
          ;; specified to be a float or unspecified. Otherwise, they
          ;; are disjoint.
          ((and (eq class1 'float)
                (not (member class2 '(float nil)))) nil)
          ((and (eq class2 'float)
                (not (member class1 '(float nil)))) nil)
          ;; If the float formats are specified and different, the
          ;; types are disjoint.
          ((not (or (eq format1 format2) (null format1) (null format2)))
           nil)
          (t
           ;; Check the bounds. This is a bit odd because we must
           ;; always have the outer bound of the interval as the
           ;; second arg.
           (if (numeric-bound-test high1 high2 <= <)
               (or (and (numeric-bound-test low1 low2 >= >)
                        (numeric-bound-test* low1 high2 <= <))
                   (and (numeric-bound-test low2 low1 >= >)
                        (numeric-bound-test* low2 high1 <= <)))
               (or (and (numeric-bound-test* low2 high1 <= <)
                        (numeric-bound-test low2 low1 >= >))
                   (and (numeric-bound-test high2 high1 <= <)
                        (numeric-bound-test* high2 low1 >= >))))))))

;;; Take the numeric bound X and convert it into something that can be
;;; used as a bound in a numeric type with the specified CLASS and
;;; FORMAT. If UP-P is true, then we round up as needed, otherwise we
;;; round down. UP-P true implies that X is a lower bound, i.e. (N) > N.
;;;
;;; This is used by NUMERIC-TYPE-INTERSECTION to mash the bound into
;;; the appropriate type number. X may only be a float when CLASS is
;;; FLOAT.
;;;
;;; ### Note: it is possible for the coercion to a float to overflow
;;; or underflow. This happens when the bound doesn't fit in the
;;; specified format. In this case, we should really return the
;;; appropriate {Most | Least}-{Positive | Negative}-XXX-Float float
;;; of desired format. But these conditions aren't currently signalled
;;; in any useful way.
;;;
;;; Also, when converting an open rational bound into a float we
;;; should probably convert it to a closed bound of the closest float
;;; in the specified format. KLUDGE: In general, open float bounds are
;;; screwed up. -- (comment from original CMU CL)
(defun round-numeric-bound (x class format up-p)
  (if x
      (let ((cx (if (consp x) (car x) x)))
        (ecase class
          ((nil rational) x)
          (integer
           (if (and (consp x) (integerp cx))
               (if up-p (1+ cx) (1- cx))
               (if up-p (ceiling cx) (floor cx))))
          (float
           (let ((res
                  (cond
                    ((and format (subtypep format 'double-float))
                     (if (<= most-negative-double-float cx most-positive-double-float)
                         (coerce cx format)
                         nil))
                    (t
                     (if (<= most-negative-single-float cx most-positive-single-float)
                         ;; FIXME: bug #389
                         (coerce cx (or format 'single-float))
                         nil)))))
             (if (consp x) (list res) res)))))
      nil))

;;; Handle the case of type intersection on two numeric types. We use
;;; TYPES-EQUAL-OR-INTERSECT to throw out the case of types with no
;;; intersection. If an attribute in TYPE1 is unspecified, then we use
;;; TYPE2's attribute, which must be at least as restrictive. If the
;;; types intersect, then the only attributes that can be specified
;;; and different are the class and the bounds.
;;;
;;; When the class differs, we use the more restrictive class. The
;;; only interesting case is RATIONAL/INTEGER, since RATIONAL includes
;;; INTEGER.
;;;
;;; We make the result lower (upper) bound the maximum (minimum) of
;;; the argument lower (upper) bounds. We convert the bounds into the
;;; appropriate numeric type before maximizing. This avoids possible
;;; confusion due to mixed-type comparisons (but I think the result is
;;; the same).
(!define-type-method (number :simple-intersection2) (type1 type2)
  (declare (type numeric-type type1 type2))
  (if (numeric-types-intersect type1 type2)
      (let* ((class1 (numeric-type-class type1))
             (class2 (numeric-type-class type2))
             (class (ecase class1
                      ((nil) class2)
                      ((integer float) class1)
                      (rational (if (eq class2 'integer)
                                       'integer
                                       'rational))))
             (format (or (numeric-type-format type1)
                         (numeric-type-format type2))))
        (make-numeric-type
         :class class
         :format format
         :complexp (or (numeric-type-complexp type1)
                       (numeric-type-complexp type2))
         :low (numeric-bound-max
               (round-numeric-bound (numeric-type-low type1)
                                    class format t)
               (round-numeric-bound (numeric-type-low type2)
                                    class format t)
               > >= nil)
         :high (numeric-bound-max
                (round-numeric-bound (numeric-type-high type1)
                                     class format nil)
                (round-numeric-bound (numeric-type-high type2)
                                     class format nil)
                < <= nil)))
      *empty-type*))

;;; Given two float formats, return the one with more precision. If
;;; either one is null, return NIL.
(defun float-format-max (f1 f2)
  (when (and f1 f2)
    (dolist (f *float-formats* (error "bad float format: ~S" f1))
      (when (or (eq f f1) (eq f f2))
        (return f)))))

;;; Return the result of an operation on TYPE1 and TYPE2 according to
;;; the rules of numeric contagion. This is always NUMBER, some float
;;; format (possibly complex) or RATIONAL. Due to rational
;;; canonicalization, there isn't much we can do here with integers or
;;; rational complex numbers.
;;;
;;; If either argument is not a NUMERIC-TYPE, then return NUMBER. This
;;; is useful mainly for allowing types that are technically numbers,
;;; but not a NUMERIC-TYPE.
(defun numeric-contagion (type1 type2)
  (if (and (numeric-type-p type1) (numeric-type-p type2))
      (let ((class1 (numeric-type-class type1))
            (class2 (numeric-type-class type2))
            (format1 (numeric-type-format type1))
            (format2 (numeric-type-format type2))
            (complexp1 (numeric-type-complexp type1))
            (complexp2 (numeric-type-complexp type2)))
        (cond ((or (null complexp1)
                   (null complexp2))
               (specifier-type 'number))
              ((eq class1 'float)
               (make-numeric-type
                :class 'float
                :format (ecase class2
                          (float (float-format-max format1 format2))
                          ((integer rational) format1)
                          ((nil)
                           ;; A double-float with any real number is a
                           ;; double-float.
                           #!-long-float
                           (if (eq format1 'double-float)
                             'double-float
                             nil)
                           ;; A long-float with any real number is a
                           ;; long-float.
                           #!+long-float
                           (if (eq format1 'long-float)
                             'long-float
                             nil)))
                :complexp (if (or (eq complexp1 :complex)
                                  (eq complexp2 :complex))
                              :complex
                              :real)))
              ((eq class2 'float) (numeric-contagion type2 type1))
              ((and (eq complexp1 :real) (eq complexp2 :real))
               (make-numeric-type
                :class (and class1 class2 'rational)
                :complexp :real))
              (t
               (specifier-type 'number))))
      (specifier-type 'number)))

;;;; array types

(!define-type-class array :enumerable nil
                    :might-contain-other-types nil)

(!define-type-method (array :simple-=) (type1 type2)
  (cond ((not (and (equal (array-type-dimensions type1)
                          (array-type-dimensions type2))
                   (eq (array-type-complexp type1)
                       (array-type-complexp type2))))
         (values nil t))
        ((or (unknown-type-p (array-type-element-type type1))
             (unknown-type-p (array-type-element-type type2)))
         (type= (array-type-element-type type1)
                (array-type-element-type type2)))
        (t
         (values (type= (array-type-specialized-element-type type1)
                        (array-type-specialized-element-type type2))
                 t))))

(!define-type-method (array :negate) (type)
  ;; FIXME (and hint to PFD): we're vulnerable here to attacks of the
  ;; form "are (AND ARRAY (NOT (ARRAY T))) and (OR (ARRAY BIT) (ARRAY
  ;; NIL) (ARRAY CHAR) ...) equivalent?" -- CSR, 2003-12-10
  ;; A symptom of the aforementioned is that the following are not TYPE=
  ;;   (AND (VECTOR T) (NOT SIMPLE-ARRAY)) ; an ARRAY-TYPE
  ;;   (AND (VECTOR T) (NOT SIMPLE-VECTOR)) ; an INTERSECTION-TYPE
  ;; even though (VECTOR T) makes it so that the (NOT) clause in each can
  ;; only provide one additional bit of information: that the vector
  ;; is complex as opposed to simple. The rank and element-type are fixed.
  (if (and (eq (array-type-dimensions type) '*)
           (eq (array-type-complexp type) 't)
           (eq (array-type-element-type type) *wild-type*))
      ;; (NOT <hairy-array>) = either SIMPLE-ARRAY or (NOT ARRAY).
      ;; This is deliberately asymmetric - trying to say that NOT simple-array
      ;; equals hairy-array leads to infinite recursion.
      (type-union (make-array-type '* :complexp nil
                                   :element-type *wild-type*)
                  (make-negation-type
                         (make-array-type '* :element-type *wild-type*)))
      (make-negation-type type)))

(!define-type-method (array :unparse) (type)
  (let* ((dims (array-type-dimensions type))
         ;; Compare the specialised element type and the
         ;; derived element type.  If the derived type
         ;; is so small that it jumps to a smaller upgraded
         ;; element type, use the specialised element type.
         ;;
         ;; This protects from unparsing
         ;;   (and (vector (or bit symbol))
         ;;        (vector (or bit character)))
         ;; i.e., the intersection of two T array types,
         ;; as a bit vector.
         (stype (array-type-specialized-element-type type))
         (dtype (array-type-element-type type))
         (utype (%upgraded-array-element-type dtype))
         (eltype (type-specifier (if (type= stype utype)
                                     dtype
                                     stype)))
         (complexp (array-type-complexp type)))
    (if (and (eq complexp t) (not *unparse-allow-negation*))
        (setq complexp :maybe))
    (cond ((eq dims '*)
           (if (eq eltype '*)
               (ecase complexp
                 ((t) '(and array (not simple-array)))
                 ((:maybe) 'array)
                 ((nil) 'simple-array))
               (ecase complexp
                 ((t) `(and (array ,eltype) (not simple-array)))
                 ((:maybe) `(array ,eltype))
                 ((nil) `(simple-array ,eltype)))))
          ((= (length dims) 1)
           (if complexp
               (let ((answer
                      (if (eq (car dims) '*)
                          (case eltype
                            (bit 'bit-vector)
                            ((base-char #!-sb-unicode character) 'base-string)
                            (* 'vector)
                            (t `(vector ,eltype)))
                          (case eltype
                            (bit `(bit-vector ,(car dims)))
                            ((base-char #!-sb-unicode character)
                             `(base-string ,(car dims)))
                            (t `(vector ,eltype ,(car dims)))))))
                 (if (eql complexp :maybe)
                     answer
                     `(and ,answer (not simple-array))))
               (if (eq (car dims) '*)
                   (case eltype
                     (bit 'simple-bit-vector)
                     ((base-char #!-sb-unicode character) 'simple-base-string)
                     ((t) 'simple-vector)
                     (t `(simple-array ,eltype (*))))
                   (case eltype
                     (bit `(simple-bit-vector ,(car dims)))
                     ((base-char #!-sb-unicode character)
                      `(simple-base-string ,(car dims)))
                     ((t) `(simple-vector ,(car dims)))
                     (t `(simple-array ,eltype ,dims))))))
          (t
           (ecase complexp
             ((t) `(and (array ,eltype ,dims) (not simple-array)))
             ((:maybe) `(array ,eltype ,dims))
             ((nil) `(simple-array ,eltype ,dims)))))))

(!define-type-method (array :simple-subtypep) (type1 type2)
  (let ((dims1 (array-type-dimensions type1))
        (dims2 (array-type-dimensions type2))
        (complexp2 (array-type-complexp type2)))
    (cond (;; not subtypep unless dimensions are compatible
           (not (or (eq dims2 '*)
                    (and (not (eq dims1 '*))
                         ;; (sbcl-0.6.4 has trouble figuring out that
                         ;; DIMS1 and DIMS2 must be lists at this
                         ;; point, and knowing that is important to
                         ;; compiling EVERY efficiently.)
                         (= (length (the list dims1))
                            (length (the list dims2)))
                         (every (lambda (x y)
                                  (or (eq y '*) (eql x y)))
                                (the list dims1)
                                (the list dims2)))))
           (values nil t))
          ;; not subtypep unless complexness is compatible
          ((not (or (eq complexp2 :maybe)
                    (eq (array-type-complexp type1) complexp2)))
           (values nil t))
          ;; Since we didn't fail any of the tests above, we win
          ;; if the TYPE2 element type is wild.
          ((eq (array-type-element-type type2) *wild-type*)
           (values t t))
          (;; Since we didn't match any of the special cases above, if
           ;; either element type is unknown we can only give a good
           ;; answer if they are the same.
           (or (unknown-type-p (array-type-element-type type1))
               (unknown-type-p (array-type-element-type type2)))
           (if (type= (array-type-element-type type1)
                      (array-type-element-type type2))
               (values t t)
               (values nil nil)))
          (;; Otherwise, the subtype relationship holds iff the
           ;; types are equal, and they're equal iff the specialized
           ;; element types are identical.
           t
           (values (type= (array-type-specialized-element-type type1)
                          (array-type-specialized-element-type type2))
                   t)))))

(!define-superclasses array
  ((vector vector) (array))
  !cold-init-forms)

(defun array-types-intersect (type1 type2)
  (declare (type array-type type1 type2))
  (let ((dims1 (array-type-dimensions type1))
        (dims2 (array-type-dimensions type2))
        (complexp1 (array-type-complexp type1))
        (complexp2 (array-type-complexp type2)))
    ;; See whether dimensions are compatible.
    (cond ((not (or (eq dims1 '*) (eq dims2 '*)
                    (and (= (length dims1) (length dims2))
                         (every (lambda (x y)
                                  (or (eq x '*) (eq y '*) (= x y)))
                                dims1 dims2))))
           (values nil t))
          ;; See whether complexpness is compatible.
          ((not (or (eq complexp1 :maybe)
                    (eq complexp2 :maybe)
                    (eq complexp1 complexp2)))
           (values nil t))
          ;; Old comment:
          ;;
          ;;   If either element type is wild, then they intersect.
          ;;   Otherwise, the types must be identical.
          ;;
          ;; FIXME: There seems to have been a fair amount of
          ;; confusion about the distinction between requested element
          ;; type and specialized element type; here is one of
          ;; them. If we request an array to hold objects of an
          ;; unknown type, we can do no better than represent that
          ;; type as an array specialized on wild-type.  We keep the
          ;; requested element-type in the -ELEMENT-TYPE slot, and
          ;; *WILD-TYPE* in the -SPECIALIZED-ELEMENT-TYPE.  So, here,
          ;; we must test for the SPECIALIZED slot being *WILD-TYPE*,
          ;; not just the ELEMENT-TYPE slot.  Maybe the return value
          ;; in that specific case should be T, NIL?  Or maybe this
          ;; function should really be called
          ;; ARRAY-TYPES-COULD-POSSIBLY-INTERSECT?  In any case, this
          ;; was responsible for bug #123, and this whole issue could
          ;; do with a rethink and/or a rewrite.  -- CSR, 2002-08-21
          ((or (eq (array-type-specialized-element-type type1) *wild-type*)
               (eq (array-type-specialized-element-type type2) *wild-type*)
               (type= (array-type-specialized-element-type type1)
                      (array-type-specialized-element-type type2)))

           (values t t))
          (t
           (values nil t)))))

(defun unite-array-types-complexp (type1 type2)
  (let ((complexp1 (array-type-complexp type1))
        (complexp2 (array-type-complexp type2)))
    (cond
      ((eq complexp1 complexp2)
       ;; both types are the same complexp-ity
       (values complexp1 t))
      ((eq complexp1 :maybe)
       ;; type1 is wild-complexp
       (values :maybe type1))
      ((eq complexp2 :maybe)
       ;; type2 is wild-complexp
       (values :maybe type2))
      (t
       ;; both types partition the complexp-space
       (values :maybe nil)))))

(defun unite-array-types-dimensions (type1 type2)
  (let ((dims1 (array-type-dimensions type1))
        (dims2 (array-type-dimensions type2)))
    (cond ((equal dims1 dims2)
           ;; both types are same dimensionality
           (values dims1 t))
          ((eq dims1 '*)
           ;; type1 is wild-dimensions
           (values '* type1))
          ((eq dims2 '*)
           ;; type2 is wild-dimensions
           (values '* type2))
          ((not (= (length dims1) (length dims2)))
           ;; types have different number of dimensions
           (values :incompatible nil))
          (t
           ;; we need to check on a per-dimension basis
           (let* ((supertype1 t)
                  (supertype2 t)
                  (compatible t)
                  (result (mapcar (lambda (dim1 dim2)
                                    (cond
                                      ((equal dim1 dim2)
                                       dim1)
                                      ((eq dim1 '*)
                                       (setf supertype2 nil)
                                       '*)
                                      ((eq dim2 '*)
                                       (setf supertype1 nil)
                                       '*)
                                      (t
                                       (setf compatible nil))))
                                  dims1 dims2)))
             (cond
               ((or (not compatible)
                    (and (not supertype1)
                         (not supertype2)))
                (values :incompatible nil))
               ((and supertype1 supertype2)
                (values result supertype1))
               (t
                (values result (if supertype1 type1 type2)))))))))

(defun unite-array-types-element-types (type1 type2)
  ;; FIXME: We'd love to be able to unite the full set of specialized
  ;; array element types up to *wild-type*, but :simple-union2 is
  ;; performed pairwise, so we don't have a good hook for it and our
  ;; representation doesn't allow us to easily detect the situation
  ;; anyway.
  ;; But see SIMPLIFY-ARRAY-UNIONS which is able to do something like that.
  (let* ((eltype1 (array-type-element-type type1))
         (eltype2 (array-type-element-type type2))
         (stype1 (array-type-specialized-element-type type1))
         (stype2 (array-type-specialized-element-type type2))
         (wild1 (eq eltype1 *wild-type*))
         (wild2 (eq eltype2 *wild-type*)))
    (cond
      ((type= eltype1 eltype2)
       (values eltype1 stype1 t))
      (wild1
       (values eltype1 stype1 type1))
      (wild2
       (values eltype2 stype2 type2))
      ((not (type= stype1 stype2))
       ;; non-wild types that don't share UAET don't unite
       (values :incompatible nil nil))
      ((csubtypep eltype1 eltype2)
       (values eltype2 stype2 type2))
      ((csubtypep eltype2 eltype1)
       (values eltype1 stype1 type1))
      (t
       (values :incompatible nil nil)))))

(defun unite-array-types-supertypes-compatible-p (&rest supertypes)
  ;; supertypes are compatible if they are all T, if there is a single
  ;; NIL and all the rest are T, or if all non-T supertypes are the
  ;; same and not NIL.
  (let ((interesting-supertypes
         (remove t supertypes)))
    (or (not interesting-supertypes)
        (equal interesting-supertypes '(nil))
        ;; supertypes are (OR BOOLEAN ARRAY-TYPE), so...
        (typep (remove-duplicates interesting-supertypes)
               '(cons array-type null)))))

(!define-type-method (array :simple-union2) (type1 type2)
  (multiple-value-bind
        (result-eltype result-stype eltype-supertype)
      (unite-array-types-element-types type1 type2)
    (multiple-value-bind
          (result-complexp complexp-supertype)
        (unite-array-types-complexp type1 type2)
      (multiple-value-bind
            (result-dimensions dimensions-supertype)
          (unite-array-types-dimensions type1 type2)
        (when (and (not (eq result-dimensions :incompatible))
                   (not (eq result-eltype :incompatible))
                   (unite-array-types-supertypes-compatible-p
                    eltype-supertype complexp-supertype dimensions-supertype))
          (make-array-type result-dimensions
           :complexp result-complexp
           :element-type result-eltype
           :specialized-element-type result-stype))))))

(!define-type-method (array :simple-intersection2) (type1 type2)
  (declare (type array-type type1 type2))
  (if (array-types-intersect type1 type2)
      (let ((dims1 (array-type-dimensions type1))
            (dims2 (array-type-dimensions type2))
            (complexp1 (array-type-complexp type1))
            (complexp2 (array-type-complexp type2))
            (eltype1 (array-type-element-type type1))
            (eltype2 (array-type-element-type type2))
            (stype1 (array-type-specialized-element-type type1))
            (stype2 (array-type-specialized-element-type type2)))
        (make-array-type (cond ((eq dims1 '*) dims2)
                               ((eq dims2 '*) dims1)
                               (t
                                (mapcar (lambda (x y) (if (eq x '*) y x))
                                        dims1 dims2)))
         :complexp (if (eq complexp1 :maybe) complexp2 complexp1)
         :element-type (cond
                         ((eq eltype1 *wild-type*) eltype2)
                         ((eq eltype2 *wild-type*) eltype1)
                         (t (type-intersection eltype1 eltype2)))
         :specialized-element-type (cond
                                     ((eq stype1 *wild-type*) stype2)
                                     ((eq stype2 *wild-type*) stype1)
                                     (t
                                      (aver (type= stype1 stype2))
                                      stype1))))
      *empty-type*))

;;; Check a supplied dimension list to determine whether it is legal,
;;; and return it in canonical form (as either '* or a list).
(defun canonical-array-dimensions (dims)
  (typecase dims
    ((member *) dims)
    (integer
     (when (minusp dims)
       (error "Arrays can't have a negative number of dimensions: ~S" dims))
     (when (>= dims sb!xc:array-rank-limit)
       (error "array type with too many dimensions: ~S" dims))
     (make-list dims :initial-element '*))
    (list
     (when (>= (length dims) sb!xc:array-rank-limit)
       (error "array type with too many dimensions: ~S" dims))
     (dolist (dim dims)
       (unless (eq dim '*)
         (unless (and (integerp dim)
                      (>= dim 0)
                      (< dim sb!xc:array-dimension-limit))
           (error "bad dimension in array type: ~S" dim))))
     dims)
    (t
     (error "Array dimensions is not a list, integer or *:~%  ~S" dims))))

;;;; MEMBER types

(!define-type-class member :enumerable t
                    :might-contain-other-types nil)

(!define-type-method (member :negate) (type)
  (let ((xset (member-type-xset type))
        (fp-zeroes (member-type-fp-zeroes type)))
    (if fp-zeroes
        ;; Hairy case, which needs to do a bit of float type
        ;; canonicalization.
        (apply #'type-intersection
               (if (xset-empty-p xset)
                   *universal-type*
                   (make-negation-type (make-member-type xset nil)))
               (mapcar
                (lambda (x)
                  (let* ((opposite (neg-fp-zero x))
                         (type (ctype-of opposite)))
                    (type-union
                     (make-negation-type
                      (modified-numeric-type type :low nil :high nil))
                     (modified-numeric-type type :low nil :high (list opposite))
                     (make-eql-type opposite)
                     (modified-numeric-type type :low (list opposite) :high nil))))
                fp-zeroes))
        ;; Easy case
        (make-negation-type type))))

(!define-type-method (member :unparse) (type)
  (let ((members (member-type-members type)))
    (cond ((equal members '(nil)) 'null)
          (t `(member ,@members)))))

(!define-type-method (member :singleton-p) (type)
  (if (eql 1 (member-type-size type))
      (values t (first (member-type-members type)))
      (values nil nil)))

(!define-type-method (member :simple-subtypep) (type1 type2)
   (values (and (xset-subset-p (member-type-xset type1)
                                 (member-type-xset type2))
                (subsetp (member-type-fp-zeroes type1)
                         (member-type-fp-zeroes type2)))
           t))

(!define-type-method (member :complex-subtypep-arg1) (type1 type2)
  (block punt
    (mapc-member-type-members
     (lambda (elt)
       (multiple-value-bind (ok surep) (ctypep elt type2)
         (unless surep
           (return-from punt (values nil nil)))
         (unless ok
           (return-from punt (values nil t)))))
     type1)
    (values t t)))

;;; We punt if the odd type is enumerable and intersects with the
;;; MEMBER type. If not enumerable, then it is definitely not a
;;; subtype of the MEMBER type.
(!define-type-method (member :complex-subtypep-arg2) (type1 type2)
  (cond ((not (type-enumerable type1)) (values nil t))
        ((types-equal-or-intersect type1 type2)
         (invoke-complex-subtypep-arg1-method type1 type2))
        (t (values nil t))))

(!define-type-method (member :simple-intersection2) (type1 type2)
  (make-member-type (xset-intersection (member-type-xset type1)
                                       (member-type-xset type2))
                    (intersection (member-type-fp-zeroes type1)
                                  (member-type-fp-zeroes type2))))

(!define-type-method (member :complex-intersection2) (type1 type2)
  (block punt
    (let ((xset (alloc-xset))
          (fp-zeroes nil))
      (mapc-member-type-members
       (lambda (member)
         (multiple-value-bind (ok sure) (ctypep member type1)
           (unless sure
             (return-from punt nil))
           (when ok
             (if (fp-zero-p member)
                 (pushnew member fp-zeroes)
                 (add-to-xset member xset)))))
       type2)
      (if (and (xset-empty-p xset) (not fp-zeroes))
          *empty-type*
          (make-member-type xset fp-zeroes)))))

;;; We don't need a :COMPLEX-UNION2, since the only interesting case is
;;; a union type, and the member/union interaction is handled by the
;;; union type method.
(!define-type-method (member :simple-union2) (type1 type2)
  (make-member-type (xset-union (member-type-xset type1)
                                (member-type-xset type2))
                    (union (member-type-fp-zeroes type1)
                           (member-type-fp-zeroes type2))))

(!define-type-method (member :simple-=) (type1 type2)
  (let ((xset1 (member-type-xset type1))
        (xset2 (member-type-xset type2))
        (l1 (member-type-fp-zeroes type1))
        (l2 (member-type-fp-zeroes type2)))
    (values (and (eql (xset-count xset1) (xset-count xset2))
                 (xset-subset-p xset1 xset2)
                 (xset-subset-p xset2 xset1)
                 (subsetp l1 l2)
                 (subsetp l2 l1))
            t)))

(!define-type-method (member :complex-=) (type1 type2)
  (if (type-enumerable type1)
      (multiple-value-bind (val win) (csubtypep type2 type1)
        (if (or val (not win))
            (values nil nil)
            (values nil t)))
      (values nil t)))

(!def-type-translator member :list (&rest members)
  (if members
      (let (ms numbers char-codes)
        (dolist (m (remove-duplicates members))
          (typecase m
            (character (push (sb!xc:char-code m) char-codes))
            (real (if (and (floatp m) (zerop m))
                      (push m ms)
                      (push (ctype-of m) numbers)))
            (t (push m ms))))
        (apply #'type-union
               (member-type-from-list ms)
               (make-character-set-type (mapcar (lambda (x) (cons x x))
                                                (sort char-codes #'<)))
               (nreverse numbers)))
      *empty-type*))

;;;; intersection types
;;;;
;;;; Until version 0.6.10.6, SBCL followed the original CMU CL approach
;;;; of punting on all AND types, not just the unreasonably complicated
;;;; ones. The change was motivated by trying to get the KEYWORD type
;;;; to behave sensibly:
;;;;    ;; reasonable definition
;;;;    (DEFTYPE KEYWORD () '(AND SYMBOL (SATISFIES KEYWORDP)))
;;;;    ;; reasonable behavior
;;;;    (AVER (SUBTYPEP 'KEYWORD 'SYMBOL))
;;;; Without understanding a little about the semantics of AND, we'd
;;;; get (SUBTYPEP 'KEYWORD 'SYMBOL)=>NIL,NIL and, for entirely
;;;; parallel reasons, (SUBTYPEP 'RATIO 'NUMBER)=>NIL,NIL. That's
;;;; not so good..)
;;;;
;;;; We still follow the example of CMU CL to some extent, by punting
;;;; (to the opaque HAIRY-TYPE) on sufficiently complicated types
;;;; involving AND.

(!define-type-class intersection
                    :enumerable #'compound-type-enumerable
                    :might-contain-other-types t)

(!define-type-method (intersection :negate) (type)
  (apply #'type-union
         (mapcar #'type-negation (intersection-type-types type))))

;;; A few intersection types have special names. The others just get
;;; mechanically unparsed.
(!define-type-method (intersection :unparse) (type)
  (declare (type ctype type))
  (or (find type '(ratio keyword compiled-function) :key #'specifier-type :test #'type=)
      `(and ,@(mapcar #'type-specifier (intersection-type-types type)))))

;;; shared machinery for type equality: true if every type in the set
;;; TYPES1 matches a type in the set TYPES2 and vice versa
(defun type=-set (types1 types2)
  (flet ((type<=-set (x y)
           (declare (type list x y))
           (every/type (lambda (x y-element)
                         (any/type #'type= y-element x))
                       x y)))
    (and/type (type<=-set types1 types2)
              (type<=-set types2 types1))))

;;; Two intersection types are equal if their subtypes are equal sets.
;;;
;;; FIXME: Might it be better to use
;;;   (AND (SUBTYPEP X Y) (SUBTYPEP Y X))
;;; instead, since SUBTYPEP is the usual relationship that we care
;;; most about, so it would be good to leverage any ingenuity there
;;; in this more obscure method?
(!define-type-method (intersection :simple-=) (type1 type2)
  (type=-set (intersection-type-types type1)
             (intersection-type-types type2)))

(defun %intersection-complex-subtypep-arg1 (type1 type2)
  (type= type1 (type-intersection type1 type2)))

(defun %intersection-simple-subtypep (type1 type2)
  (every/type #'%intersection-complex-subtypep-arg1
              type1
              (intersection-type-types type2)))

(!define-type-method (intersection :simple-subtypep) (type1 type2)
  (%intersection-simple-subtypep type1 type2))

(!define-type-method (intersection :complex-subtypep-arg1) (type1 type2)
  (%intersection-complex-subtypep-arg1 type1 type2))

(defun %intersection-complex-subtypep-arg2 (type1 type2)
  (every/type #'csubtypep type1 (intersection-type-types type2)))

(!define-type-method (intersection :complex-subtypep-arg2) (type1 type2)
  (%intersection-complex-subtypep-arg2 type1 type2))

;;; FIXME: This will look eeriely familiar to readers of the UNION
;;; :SIMPLE-INTERSECTION2 :COMPLEX-INTERSECTION2 method.  That's
;;; because it was generated by cut'n'paste methods.  Given that
;;; intersections and unions have all sorts of symmetries known to
;;; mathematics, it shouldn't be beyond the ken of some programmers to
;;; reflect those symmetries in code in a way that ties them together
;;; more strongly than having two independent near-copies :-/
(!define-type-method (intersection :simple-union2 :complex-union2)
                     (type1 type2)
  ;; Within this method, type2 is guaranteed to be an intersection
  ;; type:
  (aver (intersection-type-p type2))
  ;; Make sure to call only the applicable methods...
  (cond ((and (intersection-type-p type1)
              (%intersection-simple-subtypep type1 type2)) type2)
        ((and (intersection-type-p type1)
              (%intersection-simple-subtypep type2 type1)) type1)
        ((and (not (intersection-type-p type1))
              (%intersection-complex-subtypep-arg2 type1 type2))
         type2)
        ((and (not (intersection-type-p type1))
              (%intersection-complex-subtypep-arg1 type2 type1))
         type1)
        ;; KLUDGE: This special (and somewhat hairy) magic is required
        ;; to deal with the RATIONAL/INTEGER special case.  The UNION
        ;; of (INTEGER * -1) and (AND (RATIONAL * -1/2) (NOT INTEGER))
        ;; should be (RATIONAL * -1/2) -- CSR, 2003-02-28
        ((and (csubtypep type2 (specifier-type 'ratio))
              (numeric-type-p type1)
              (csubtypep type1 (specifier-type 'integer))
              (csubtypep type2
                         (make-numeric-type
                          :class 'rational
                          :complexp nil
                          :low (if (null (numeric-type-low type1))
                                   nil
                                   (list (1- (numeric-type-low type1))))
                          :high (if (null (numeric-type-high type1))
                                    nil
                                    (list (1+ (numeric-type-high type1)))))))
         (let* ((intersected (intersection-type-types type2))
                (remaining   (remove (specifier-type '(not integer))
                                     intersected
                                     :test #'type=)))
           (and (not (equal intersected remaining))
                (type-union type1 (apply #'type-intersection remaining)))))
        (t
         (let ((accumulator *universal-type*))
           (do ((t2s (intersection-type-types type2) (cdr t2s)))
               ((null t2s) accumulator)
             (let ((union (type-union type1 (car t2s))))
               (when (union-type-p union)
                 ;; we have to give up here -- there are all sorts of
                 ;; ordering worries, but it's better than before.
                 ;; Doing exactly the same as in the UNION
                 ;; :SIMPLE/:COMPLEX-INTERSECTION2 method causes stack
                 ;; overflow with the mutual recursion never bottoming
                 ;; out.
                 (if (and (eq accumulator *universal-type*)
                          (null (cdr t2s)))
                     ;; KLUDGE: if we get here, we have a partially
                     ;; simplified result.  While this isn't by any
                     ;; means a universal simplification, including
                     ;; this logic here means that we can get (OR
                     ;; KEYWORD (NOT KEYWORD)) canonicalized to T.
                     (return union)
                     (return nil)))
               (setf accumulator
                     (type-intersection accumulator union))))))))

(!def-type-translator and :list ((:context context) &rest type-specifiers)
  (apply #'type-intersection
         (mapcar (lambda (x) (specifier-type-r context x))
                 type-specifiers)))

;;;; union types

(!define-type-class union
                    :enumerable #'compound-type-enumerable
                    :might-contain-other-types t)

(!define-type-method (union :negate) (type)
  (declare (type ctype type))
  (apply #'type-intersection
         (mapcar #'type-negation (union-type-types type))))

;;; The LIST, FLOAT and REAL types have special names.  Other union
;;; types just get mechanically unparsed.
(!define-type-method (union :unparse) (type)
  (declare (type ctype type))
  (cond
    ((type= type (specifier-type 'list)) 'list)
    ((type= type (specifier-type 'float)) 'float)
    ((type= type (specifier-type 'real)) 'real)
    ((type= type (specifier-type 'sequence)) 'sequence)
    ((type= type (specifier-type 'bignum)) 'bignum)
    ((type= type (specifier-type 'simple-string)) 'simple-string)
    ((type= type (specifier-type 'string)) 'string)
    ((type= type (specifier-type 'complex)) 'complex)
    (t `(or ,@(mapcar #'type-specifier (union-type-types type))))))

;;; Two union types are equal if they are each subtypes of each
;;; other. We need to be this clever because our complex subtypep
;;; methods are now more accurate; we don't get infinite recursion
;;; because the simple-subtypep method delegates to complex-subtypep
;;; of the individual types of type1. - CSR, 2002-04-09
;;;
;;; Previous comment, now obsolete, but worth keeping around because
;;; it is true, though too strong a condition:
;;;
;;; Two union types are equal if their subtypes are equal sets.
(!define-type-method (union :simple-=) (type1 type2)
  (multiple-value-bind (subtype certain?)
      (csubtypep type1 type2)
    (if subtype
        (csubtypep type2 type1)
        ;; we might as well become as certain as possible.
        (if certain?
            (values nil t)
            (multiple-value-bind (subtype certain?)
                (csubtypep type2 type1)
              (declare (ignore subtype))
              (values nil certain?))))))

(!define-type-method (union :complex-=) (type1 type2)
  (declare (ignore type1))
  (if (some #'type-might-contain-other-types-p
            (union-type-types type2))
      (values nil nil)
      (values nil t)))

;;; Similarly, a union type is a subtype of another if and only if
;;; every element of TYPE1 is a subtype of TYPE2.
(defun union-simple-subtypep (type1 type2)
  (every/type (swapped-args-fun #'union-complex-subtypep-arg2)
              type2
              (union-type-types type1)))

(!define-type-method (union :simple-subtypep) (type1 type2)
  (union-simple-subtypep type1 type2))

(defun union-complex-subtypep-arg1 (type1 type2)
  (every/type (swapped-args-fun #'csubtypep)
              type2
              (union-type-types type1)))

(!define-type-method (union :complex-subtypep-arg1) (type1 type2)
  (union-complex-subtypep-arg1 type1 type2))

(defun union-complex-subtypep-arg2 (type1 type2)
  ;; At this stage, we know that type2 is a union type and type1
  ;; isn't. We might as well check this, though:
  (aver (union-type-p type2))
  (aver (not (union-type-p type1)))
  ;; was: (any/type #'csubtypep type1 (union-type-types type2)), which
  ;; turns out to be too restrictive, causing bug 91.
  ;;
  ;; the following reimplementation might look dodgy. It is dodgy. It
  ;; depends on the union :complex-= method not doing very much work
  ;; -- certainly, not using subtypep. Reasoning:
  ;;
  ;;     A is a subset of (B1 u B2)
  ;; <=> A n (B1 u B2) = A
  ;; <=> (A n B1) u (A n B2) = A
  ;;
  ;; But, we have to be careful not to delegate this type= to
  ;; something that could invoke subtypep, which might get us back
  ;; here -> stack explosion. We therefore ensure that the second type
  ;; (which is the one that's dispatched on) is either a union type
  ;; (where we've ensured that the complex-= method will not call
  ;; subtypep) or something with no union types involved, in which
  ;; case we'll never come back here.
  ;;
  ;; If we don't do this, then e.g.
  ;; (SUBTYPEP '(MEMBER 3) '(OR (SATISFIES FOO) (SATISFIES BAR)))
  ;; would loop infinitely, as the member :complex-= method is
  ;; implemented in terms of subtypep.
  ;;
  ;; Ouch. - CSR, 2002-04-10
  (multiple-value-bind (sub-value sub-certain?)
      (type= type1
             (apply #'type-union
                    (mapcar (lambda (x) (type-intersection type1 x))
                            (union-type-types type2))))
    (if sub-certain?
        (values sub-value sub-certain?)
        ;; The ANY/TYPE expression above is a sufficient condition for
        ;; subsetness, but not a necessary one, so we might get a more
        ;; certain answer by this CALL-NEXT-METHOD-ish step when the
        ;; ANY/TYPE expression is uncertain.
        (invoke-complex-subtypep-arg1-method type1 type2))))

(!define-type-method (union :complex-subtypep-arg2) (type1 type2)
  (union-complex-subtypep-arg2 type1 type2))

(!define-type-method (union :simple-intersection2 :complex-intersection2)
                     (type1 type2)
  ;; The CSUBTYPEP clauses here let us simplify e.g.
  ;;   (TYPE-INTERSECTION2 (SPECIFIER-TYPE 'LIST)
  ;;                       (SPECIFIER-TYPE '(OR LIST VECTOR)))
  ;; (where LIST is (OR CONS NULL)).
  ;;
  ;; The tests are more or less (CSUBTYPEP TYPE1 TYPE2) and vice
  ;; versa, but it's important that we pre-expand them into
  ;; specialized operations on individual elements of
  ;; UNION-TYPE-TYPES, instead of using the ordinary call to
  ;; CSUBTYPEP, in order to avoid possibly invoking any methods which
  ;; might in turn invoke (TYPE-INTERSECTION2 TYPE1 TYPE2) and thus
  ;; cause infinite recursion.
  ;;
  ;; Within this method, type2 is guaranteed to be a union type:
  (aver (union-type-p type2))
  ;; Make sure to call only the applicable methods...
  (cond ((and (union-type-p type1)
              (union-simple-subtypep type1 type2)) type1)
        ((and (union-type-p type1)
              (union-simple-subtypep type2 type1)) type2)
        ((and (not (union-type-p type1))
              (union-complex-subtypep-arg2 type1 type2))
         type1)
        ((and (not (union-type-p type1))
              (union-complex-subtypep-arg1 type2 type1))
         type2)
        (t
         ;; KLUDGE: This code accumulates a sequence of TYPE-UNION2
         ;; operations in a particular order, and gives up if any of
         ;; the sub-unions turn out not to be simple. In other cases
         ;; ca. sbcl-0.6.11.15, that approach to taking a union was a
         ;; bad idea, since it can overlook simplifications which
         ;; might occur if the terms were accumulated in a different
         ;; order. It's possible that that will be a problem here too.
         ;; However, I can't think of a good example to demonstrate
         ;; it, and without an example to demonstrate it I can't write
         ;; test cases, and without test cases I don't want to
         ;; complicate the code to address what's still a hypothetical
         ;; problem. So I punted. -- WHN 2001-03-20
         (let ((accumulator *empty-type*))
           (dolist (t2 (union-type-types type2) accumulator)
             (setf accumulator
                   (type-union accumulator
                               (type-intersection type1 t2))))))))

(!def-type-translator or :list ((:context context) &rest type-specifiers)
  (let ((type (apply #'type-union
                     (mapcar (lambda (x) (specifier-type-r context x))
                             type-specifiers))))
    (if (union-type-p type)
        (sb!kernel::simplify-array-unions type)
        type)))

;;;; CONS types

(!define-type-class cons :enumerable nil :might-contain-other-types nil)

(!def-type-translator cons ((:context context)
                            &optional (car-type-spec '*) (cdr-type-spec '*))
  (let ((car-type (single-value-specifier-type-r context car-type-spec))
        (cdr-type (single-value-specifier-type-r context cdr-type-spec)))
    (make-cons-type car-type cdr-type)))

(!define-type-method (cons :negate) (type)
  (if (and (eq (cons-type-car-type type) *universal-type*)
           (eq (cons-type-cdr-type type) *universal-type*))
      (make-negation-type type)
      (type-union
       (make-negation-type (specifier-type 'cons))
       (cond
         ((and (not (eq (cons-type-car-type type) *universal-type*))
               (not (eq (cons-type-cdr-type type) *universal-type*)))
          (type-union
           (make-cons-type
            (type-negation (cons-type-car-type type))
            *universal-type*)
           (make-cons-type
            *universal-type*
            (type-negation (cons-type-cdr-type type)))))
         ((not (eq (cons-type-car-type type) *universal-type*))
          (make-cons-type
           (type-negation (cons-type-car-type type))
           *universal-type*))
         ((not (eq (cons-type-cdr-type type) *universal-type*))
          (make-cons-type
           *universal-type*
           (type-negation (cons-type-cdr-type type))))
         (t (bug "Weird CONS type ~S" type))))))

(!define-type-method (cons :unparse) (type)
  (let ((car-eltype (type-specifier (cons-type-car-type type)))
        (cdr-eltype (type-specifier (cons-type-cdr-type type))))
    (if (and (member car-eltype '(t *))
             (member cdr-eltype '(t *)))
        'cons
        `(cons ,car-eltype ,cdr-eltype))))

(!define-type-method (cons :simple-=) (type1 type2)
  (declare (type cons-type type1 type2))
  (multiple-value-bind (car-match car-win)
      (type= (cons-type-car-type type1) (cons-type-car-type type2))
    (multiple-value-bind (cdr-match cdr-win)
        (type= (cons-type-cdr-type type1) (cons-type-cdr-type type2))
      (cond ((and car-match cdr-match)
             (aver (and car-win cdr-win))
             (values t t))
            (t
             (values nil
                     ;; FIXME: Ideally we would like to detect and handle
                     ;;  (CONS UNKNOWN INTEGER) (CONS UNKNOWN SYMBOL) => NIL, T
                     ;; but just returning a secondary true on (and car-win cdr-win)
                     ;; unfortunately breaks other things. --NS 2006-08-16
                     (and (or (and (not car-match) car-win)
                              (and (not cdr-match) cdr-win))
                          (not (and (cons-type-might-be-empty-type type1)
                                    (cons-type-might-be-empty-type type2))))))))))

(!define-type-method (cons :simple-subtypep) (type1 type2)
  (declare (type cons-type type1 type2))
  (multiple-value-bind (val-car win-car)
      (csubtypep (cons-type-car-type type1) (cons-type-car-type type2))
    (multiple-value-bind (val-cdr win-cdr)
        (csubtypep (cons-type-cdr-type type1) (cons-type-cdr-type type2))
      (if (and val-car val-cdr)
          (values t (and win-car win-cdr))
          (values nil (or (and (not val-car) win-car)
                          (and (not val-cdr) win-cdr)))))))

;;; Give up if a precise type is not possible, to avoid returning
;;; overly general types.
(!define-type-method (cons :simple-union2) (type1 type2)
  (declare (type cons-type type1 type2))
  (let ((car-type1 (cons-type-car-type type1))
        (car-type2 (cons-type-car-type type2))
        (cdr-type1 (cons-type-cdr-type type1))
        (cdr-type2 (cons-type-cdr-type type2))
        car-not1
        car-not2)
    ;; UGH.  -- CSR, 2003-02-24
    (macrolet ((frob-car (car1 car2 cdr1 cdr2
                          &optional (not1 nil not1p))
                 `(type-union
                   (make-cons-type ,car1 (type-union ,cdr1 ,cdr2))
                   (make-cons-type
                    (type-intersection ,car2
                     ,(if not1p
                          not1
                          `(type-negation ,car1)))
                    ,cdr2))))
      (cond ((type= car-type1 car-type2)
             (make-cons-type car-type1
                             (type-union cdr-type1 cdr-type2)))
            ((type= cdr-type1 cdr-type2)
             (make-cons-type (type-union car-type1 car-type2)
                             cdr-type1))
            ((csubtypep car-type1 car-type2)
             (frob-car car-type1 car-type2 cdr-type1 cdr-type2))
            ((csubtypep car-type2 car-type1)
             (frob-car car-type2 car-type1 cdr-type2 cdr-type1))
            ;; more general case of the above, but harder to compute
            ((progn
               (setf car-not1 (type-negation car-type1))
               (multiple-value-bind (yes win)
                   (csubtypep car-type2 car-not1)
                 (and (not yes) win)))
             (frob-car car-type1 car-type2 cdr-type1 cdr-type2 car-not1))
            ((progn
               (setf car-not2 (type-negation car-type2))
               (multiple-value-bind (yes win)
                   (csubtypep car-type1 car-not2)
                 (and (not yes) win)))
             (frob-car car-type2 car-type1 cdr-type2 cdr-type1 car-not2))
            ;; Don't put these in -- consider the effect of taking the
            ;; union of (CONS (INTEGER 0 2) (INTEGER 5 7)) and
            ;; (CONS (INTEGER 0 3) (INTEGER 5 6)).
            #+nil
            ((csubtypep cdr-type1 cdr-type2)
             (frob-cdr car-type1 car-type2 cdr-type1 cdr-type2))
            #+nil
            ((csubtypep cdr-type2 cdr-type1)
             (frob-cdr car-type2 car-type1 cdr-type2 cdr-type1))))))

(!define-type-method (cons :simple-intersection2) (type1 type2)
  (declare (type cons-type type1 type2))
  (let ((car-int2 (type-intersection2 (cons-type-car-type type1)
                                      (cons-type-car-type type2)))
        (cdr-int2 (type-intersection2 (cons-type-cdr-type type1)
                                      (cons-type-cdr-type type2))))
    (cond
      ((and car-int2 cdr-int2) (make-cons-type car-int2 cdr-int2))
      (car-int2 (make-cons-type car-int2
                                (type-intersection
                                 (cons-type-cdr-type type1)
                                 (cons-type-cdr-type type2))))
      (cdr-int2 (make-cons-type
                 (type-intersection (cons-type-car-type type1)
                                    (cons-type-car-type type2))
                 cdr-int2)))))

(!define-superclasses cons ((cons)) !cold-init-forms)

;;;; CHARACTER-SET types

;; all character-set types are enumerable, but it's not possible
;; for one to be TYPE= to a MEMBER type because (MEMBER #\x)
;; is not internally represented as a MEMBER type.
;; So in case it wasn't clear already ENUMERABLE-P does not mean
;;  "possibly a MEMBER type in the Lisp-theoretic sense",
;; but means "could be implemented in SBCL as a MEMBER type".
(!define-type-class character-set :enumerable nil
                    :might-contain-other-types nil)

(!def-type-translator character-set
    (&optional (pairs '((0 . #.(1- sb!xc:char-code-limit)))))
  (make-character-set-type pairs))

(!define-type-method (character-set :negate) (type)
  (let ((pairs (character-set-type-pairs type)))
    (if (and (= (length pairs) 1)
             (= (caar pairs) 0)
             (= (cdar pairs) (1- sb!xc:char-code-limit)))
        (make-negation-type type)
        (let ((not-character
               (make-negation-type
                (make-character-set-type
                 '((0 . #.(1- sb!xc:char-code-limit)))))))
          (type-union
           not-character
           (make-character-set-type
                   (let (not-pairs)
                     (when (> (caar pairs) 0)
                       (push (cons 0 (1- (caar pairs))) not-pairs))
                     (do* ((tail pairs (cdr tail))
                           (high1 (cdar tail) (cdar tail))
                           (low2 (caadr tail) (caadr tail)))
                          ((null (cdr tail))
                           (when (< (cdar tail) (1- sb!xc:char-code-limit))
                             (push (cons (1+ (cdar tail))
                                         (1- sb!xc:char-code-limit))
                                   not-pairs))
                           (nreverse not-pairs))
                       (push (cons (1+ high1) (1- low2)) not-pairs)))))))))

(!define-type-method (character-set :unparse) (type)
  (cond
    ((type= type (specifier-type 'character)) 'character)
    ((type= type (specifier-type 'base-char)) 'base-char)
    ((type= type (specifier-type 'extended-char)) 'extended-char)
    ((type= type (specifier-type 'standard-char)) 'standard-char)
    (t
     ;; Unparse into either MEMBER or CHARACTER-SET. We use MEMBER if there
     ;; are at most as many characters as there are character code ranges.
     ;; (basically saying to use MEMBER if each range is one character)
     (let* ((pairs (character-set-type-pairs type))
            (count (length pairs))
            (chars (loop named outer
                         for (low . high) in pairs
                         nconc (loop for code from low upto high
                                     collect (sb!xc:code-char code)
                                     when (minusp (decf count))
                                     do (return-from outer t)))))
       (if (eq chars t)
           `(character-set ,pairs)
           `(member ,@chars))))))

(!define-type-method (character-set :singleton-p) (type)
  (let* ((pairs (character-set-type-pairs type))
         (pair  (first pairs)))
    (if (and (typep pairs '(cons t null))
             (eql (car pair) (cdr pair)))
        (values t (code-char (car pair)))
        (values nil nil))))

(!define-type-method (character-set :simple-=) (type1 type2)
  (let ((pairs1 (character-set-type-pairs type1))
       (pairs2 (character-set-type-pairs type2)))
    (values (equal pairs1 pairs2) t)))

(!define-type-method (character-set :simple-subtypep) (type1 type2)
  (values
   (dolist (pair (character-set-type-pairs type1) t)
     (unless (position pair (character-set-type-pairs type2)
                      :test (lambda (x y) (and (>= (car x) (car y))
                                               (<= (cdr x) (cdr y)))))
       (return nil)))
   t))

(!define-type-method (character-set :simple-union2) (type1 type2)
  ;; KLUDGE: the canonizing in the MAKE-CHARACTER-SET-TYPE function
  ;; actually does the union for us.  It might be a little fragile to
  ;; rely on it.
  (make-character-set-type
          (merge 'list
                (copy-alist (character-set-type-pairs type1))
                (copy-alist (character-set-type-pairs type2))
                #'< :key #'car)))

(!define-type-method (character-set :simple-intersection2) (type1 type2)
  ;; KLUDGE: brute force.
#|
  (let (pairs)
    (dolist (pair1 (character-set-type-pairs type1)
            (make-character-set-type
                    (sort pairs #'< :key #'car)))
      (dolist (pair2 (character-set-type-pairs type2))
       (cond
         ((<= (car pair1) (car pair2) (cdr pair1))
          (push (cons (car pair2) (min (cdr pair1) (cdr pair2))) pairs))
         ((<= (car pair2) (car pair1) (cdr pair2))
          (push (cons (car pair1) (min (cdr pair1) (cdr pair2))) pairs))))))
|#
  (make-character-set-type
          (intersect-type-pairs
           (character-set-type-pairs type1)
           (character-set-type-pairs type2))))

;;;
;;; Intersect two ordered lists of pairs
;;; Each list is of the form ((start1 . end1) ... (startn . endn)),
;;; where start1 <= end1 < start2 <= end2 < ... < startn <= endn.
;;; Each pair represents the integer interval start..end.
;;;
(defun intersect-type-pairs (alist1 alist2)
  (if (and alist1 alist2)
      (let ((res nil)
            (pair1 (pop alist1))
            (pair2 (pop alist2)))
        (loop
         (when (> (car pair1) (car pair2))
           (rotatef pair1 pair2)
           (rotatef alist1 alist2))
         (let ((pair1-cdr (cdr pair1)))
           (cond
            ((> (car pair2) pair1-cdr)
             ;; No over lap -- discard pair1
             (unless alist1 (return))
             (setq pair1 (pop alist1)))
            ((<= (cdr pair2) pair1-cdr)
             (push (cons (car pair2) (cdr pair2)) res)
             (cond
              ((= (cdr pair2) pair1-cdr)
               (unless alist1 (return))
               (unless alist2 (return))
               (setq pair1 (pop alist1)
                     pair2 (pop alist2)))
              (t ;; (< (cdr pair2) pair1-cdr)
               (unless alist2 (return))
               (setq pair1 (cons (1+ (cdr pair2)) pair1-cdr))
               (setq pair2 (pop alist2)))))
            (t ;; (> (cdr pair2) (cdr pair1))
             (push (cons (car pair2) pair1-cdr) res)
             (unless alist1 (return))
             (setq pair2 (cons (1+ pair1-cdr) (cdr pair2)))
             (setq pair1 (pop alist1))))))
        (nreverse res))
    nil))


;;; Return the type that describes all objects that are in X but not
;;; in Y. If we can't determine this type, then return NIL.
;;;
;;; For now, we only are clever dealing with union and member types.
;;; If either type is not a union type, then we pretend that it is a
;;; union of just one type. What we do is remove from X all the types
;;; that are a subtype any type in Y. If any type in X intersects with
;;; a type in Y but is not a subtype, then we give up.
;;;
;;; We must also special-case any member type that appears in the
;;; union. We remove from X's members all objects that are TYPEP to Y.
;;; If Y has any members, we must be careful that none of those
;;; members are CTYPEP to any of Y's non-member types. We give up in
;;; this case, since to compute that difference we would have to break
;;; the type from X into some collection of types that represents the
;;; type without that particular element. This seems too hairy to be
;;; worthwhile, given its low utility.
(defun type-difference (x y)
  (if (and (numeric-type-p x) (numeric-type-p y))
      ;; Numeric types are easy. Are there any others we should handle like this?
      (type-intersection x (type-negation y))
      (let ((x-types (if (union-type-p x) (union-type-types x) (list x)))
            (y-types (if (union-type-p y) (union-type-types y) (list y))))
        (collect ((res))
          (dolist (x-type x-types)
            (if (member-type-p x-type)
                (let ((xset (alloc-xset))
                      (fp-zeroes nil))
                  (mapc-member-type-members
                   (lambda (elt)
                     (multiple-value-bind (ok sure) (ctypep elt y)
                       (unless sure
                         (return-from type-difference nil))
                       (unless ok
                         (if (fp-zero-p elt)
                             (pushnew elt fp-zeroes)
                             (add-to-xset elt xset)))))
                   x-type)
                  (unless (and (xset-empty-p xset) (not fp-zeroes))
                    (res (make-member-type xset fp-zeroes))))
                (dolist (y-type y-types (res x-type))
                  (multiple-value-bind (val win) (csubtypep x-type y-type)
                    (unless win (return-from type-difference nil))
                    (when val (return))
                    (when (types-equal-or-intersect x-type y-type)
                      (return-from type-difference nil))))))
          (let ((y-mem (find-if #'member-type-p y-types)))
            (when y-mem
              (dolist (x-type x-types)
                (unless (member-type-p x-type)
                  (mapc-member-type-members
                   (lambda (member)
                     (multiple-value-bind (ok sure) (ctypep member x-type)
                       (when (or (not sure) ok)
                         (return-from type-difference nil))))
                   y-mem)))))
          (apply #'type-union (res))))))

(!def-type-translator array ((:context context)
                             &optional (element-type '*)
                                       (dimensions '*))
  (let ((eltype (if (eq element-type '*)
                    *wild-type*
                    (specifier-type-r context element-type))))
    (make-array-type (canonical-array-dimensions dimensions)
                     :complexp :maybe
                     :element-type eltype
                     :specialized-element-type (%upgraded-array-element-type
                                                eltype))))

(!def-type-translator simple-array ((:context context)
                                    &optional (element-type '*)
                                              (dimensions '*))
  (let ((eltype (if (eq element-type '*)
                    *wild-type*
                    (specifier-type-r context element-type))))
   (make-array-type (canonical-array-dimensions dimensions)
                    :complexp nil
                    :element-type eltype
                    :specialized-element-type (%upgraded-array-element-type
                                               eltype))))

;;;; SIMD-PACK types
#!+sb-simd-pack
(progn
  (!define-type-class simd-pack :enumerable nil
                      :might-contain-other-types nil)

  ;; Though this involves a recursive call to parser, parsing context need not
  ;; be passed down, because an unknown-type condition is an immediate failure.
  (!def-type-translator simd-pack (&optional (element-type-spec '*))
     (if (eql element-type-spec '*)
         (%make-simd-pack-type *simd-pack-element-types*)
         (make-simd-pack-type (single-value-specifier-type element-type-spec))))

  (!define-type-method (simd-pack :negate) (type)
     (let ((remaining (set-difference *simd-pack-element-types*
                                      (simd-pack-type-element-type type)))
           (not-simd-pack (make-negation-type (specifier-type 'simd-pack))))
       (if remaining
           (type-union not-simd-pack (%make-simd-pack-type remaining))
           not-simd-pack)))

  (!define-type-method (simd-pack :unparse) (type)
     (let ((eltypes (simd-pack-type-element-type type)))
       (cond ((equal eltypes *simd-pack-element-types*)
              'simd-pack)
             ((= 1 (length eltypes))
              `(simd-pack ,(first eltypes)))
             (t
              `(or ,@(mapcar (lambda (eltype)
                               `(simd-pack ,eltype))
                             eltypes))))))

  (!define-type-method (simd-pack :simple-=) (type1 type2)
     (declare (type simd-pack-type type1 type2))
     (null (set-exclusive-or (simd-pack-type-element-type type1)
                             (simd-pack-type-element-type type2))))

  (!define-type-method (simd-pack :simple-subtypep) (type1 type2)
     (declare (type simd-pack-type type1 type2))
     (subsetp (simd-pack-type-element-type type1)
              (simd-pack-type-element-type type2)))

  (!define-type-method (simd-pack :simple-union2) (type1 type2)
     (declare (type simd-pack-type type1 type2))
     (%make-simd-pack-type (union (simd-pack-type-element-type type1)
                                  (simd-pack-type-element-type type2))))

  (!define-type-method (simd-pack :simple-intersection2) (type1 type2)
     (declare (type simd-pack-type type1 type2))
     (let ((intersection (intersection (simd-pack-type-element-type type1)
                                       (simd-pack-type-element-type type2))))
       (if intersection
           (%make-simd-pack-type intersection)
           *empty-type*)))

  (!define-superclasses simd-pack ((simd-pack)) !cold-init-forms))

;;;; utilities shared between cross-compiler and target system

;;; Does the type derived from compilation of an actual function
;;; definition satisfy declarations of a function's type?
(defun defined-ftype-matches-declared-ftype-p (defined-ftype declared-ftype)
  (declare (type ctype defined-ftype declared-ftype))
  (flet ((is-built-in-class-function-p (ctype)
           (and (built-in-classoid-p ctype)
                (eq (built-in-classoid-name ctype) 'function))))
    (cond (;; DECLARED-FTYPE could certainly be #<BUILT-IN-CLASS FUNCTION>;
           ;; that's what happens when we (DECLAIM (FTYPE FUNCTION FOO)).
           (is-built-in-class-function-p declared-ftype)
           ;; In that case, any definition satisfies the declaration.
           t)
          (;; It's not clear whether or how DEFINED-FTYPE might be
           ;; #<BUILT-IN-CLASS FUNCTION>, but it's not obviously
           ;; invalid, so let's handle that case too, just in case.
           (is-built-in-class-function-p defined-ftype)
           ;; No matter what DECLARED-FTYPE might be, we can't prove
           ;; that an object of type FUNCTION doesn't satisfy it, so
           ;; we return success no matter what.
           t)
          (;; Otherwise both of them must be FUN-TYPE objects.
           t
           ;; FIXME: For now we only check compatibility of the return
           ;; type, not argument types, and we don't even check the
           ;; return type very precisely (as per bug 94a). It would be
           ;; good to do a better job. Perhaps to check the
           ;; compatibility of the arguments, we should (1) redo
           ;; VALUES-TYPES-EQUAL-OR-INTERSECT as
           ;; ARGS-TYPES-EQUAL-OR-INTERSECT, and then (2) apply it to
           ;; the ARGS-TYPE slices of the FUN-TYPEs. (ARGS-TYPE
           ;; is a base class both of VALUES-TYPE and of FUN-TYPE.)
           (values-types-equal-or-intersect
            (fun-type-returns defined-ftype)
            (fun-type-returns declared-ftype))))))

;;; This messy case of CTYPE for NUMBER is shared between the
;;; cross-compiler and the target system.
(defun ctype-of-number (x)
  (let ((num (if (complexp x) (realpart x) x)))
    (multiple-value-bind (complexp low high)
        (if (complexp x)
            (let ((imag (imagpart x)))
              (values :complex (min num imag) (max num imag)))
            (values :real num num))
      (make-numeric-type :class (etypecase num
                                  (integer (if (complexp x)
                                               (if (integerp (imagpart x))
                                                   'integer
                                                   'rational)
                                               'integer))
                                  (rational 'rational)
                                  (float 'float))
                         :format (and (floatp num) (float-format-name num))
                         :complexp complexp
                         :low low
                         :high high))))

;;; The following function is a generic driver for approximating
;;; set-valued functions over types.  Putting this here because it'll
;;; probably be useful for a lot of type analyses.
;;;
;;; Let f be a function from values of type X to Y, e.g., ARRAY-RANK.
;;;
;;; We compute an over or under-approximation of the set
;;;
;;;  F(TYPE) = { f(x) : x in TYPE /\ x in X } \subseteq Y
;;;
;;; via set-valued approximations of f, OVER and UNDER.
;;;
;;; These functions must have the property that
;;;   Forall TYPE, OVER(TYPE) \superseteq F(TYPE) and
;;;   Forall TYPE, UNDER(TYPE) \subseteq F(TYPE)
;;;
;;; The driver is also parameterised over the finite set
;;; representation.
;;;
;;; Union, intersection and difference are binary functions to compute
;;; set union, intersection and difference.  Top and bottom are the
;;; concrete representations for the universe and empty sets; we never
;;; call the set functions on top or bottom, so it's safe to use
;;; special values there.
;;;
;;; Arguments:
;;;
;;;  TYPE: the ctype for which we wish to approximate F(TYPE)
;;;  OVERAPPROXIMATE: true if we wish to overapproximate, nil otherwise.
;;;     You usually want T.
;;;  UNION/INTERSECTION/DIFFERENCE: implementations of finite set operations.
;;;     Conform to cl::(union/intersection/set-difference).  Passing NIL will
;;;     disable some cleverness and result in quicker computation of coarser
;;;     approximations.  However, passing difference without union and intersection
;;;     will probably not end well.
;;;  TOP/BOTTOM: concrete representation of the universe and empty set.  Finite
;;;     set operations are never called on TOP/BOTTOM, so it's safe to use special
;;;     values there.
;;;  OVER/UNDER: the set-valued approximations of F.
;;;
;;; Implementation details.
;;;
;;; It's a straightforward walk down the type.
;;; Union types -> take the union of children, intersection ->
;;; intersect.  There is some complication for negation types: we must
;;; not only negate the result, but also flip from overapproximating
;;; to underapproximating in the children (or vice versa).
;;;
;;; We represent sets as a pair of (negate-p finite-set) in order to
;;; support negation types.

(declaim (inline generic-abstract-type-function))
(defun generic-abstract-type-function
    (type overapproximate
     union intersection difference
     top bottom
     over under)
  (labels ((union* (x y)
             ;; wrappers to avoid calling union/intersection on
             ;; top/bottom.
             (cond ((or (eql x top)
                        (eql y top))
                    top)
                   ((eql x bottom) y)
                   ((eql y bottom) x)
                   (t
                    (funcall union x y))))
           (intersection* (x y)
             (cond ((or (eql x bottom)
                        (eql y bottom))
                    bottom)
                   ((eql x top) y)
                   ((eql y top) x)
                   (t
                    (funcall intersection x y))))
           (unite (not-x-p x not-y-p y)
             ;; if we only have one negated set, it's x.
             (when not-y-p
               (rotatef not-x-p not-y-p)
               (rotatef x y))
             (cond ((and not-x-p not-y-p)
                    ;; -x \/ -y = -(x /\ y)
                    (normalize t (intersection* x y)))
                   (not-x-p
                    ;; -x \/ y = -(x \ y)
                    (cond ((eql x top)
                           (values nil y))
                          ((or (eql y top)
                               (eql x bottom))
                           (values nil top))
                          ((eql y bottom)
                           (values t x))
                          (t
                           (normalize t
                                      (funcall difference x y)))))
                   (t
                    (values nil (union* x y)))))
           (intersect (not-x-p x not-y-p y)
             (when not-y-p
               (rotatef not-x-p not-y-p)
               (rotatef x y))
             (cond ((and not-x-p not-y-p)
                    ;; -x /\ -y = -(x \/ y)
                    (normalize t (union* x y)))
                   (not-x-p
                    ;; -x /\ y = y \ x
                    (cond ((or (eql x top) (eql y bottom))
                           (values nil bottom))
                          ((eql x bottom)
                           (values nil y))
                          ((eql y top)
                           (values t x))
                          (t
                           (values nil (funcall difference y x)))))
                   (t
                    (values nil (intersection* x y)))))
           (normalize (not-x-p x)
             ;; catch some easy cases of redundant negation.
             (cond ((not not-x-p)
                    (values nil x))
                   ((eql x top)
                    bottom)
                   ((eql x bottom)
                    top)
                   (t
                    (values t x))))
           (default (overapproximate)
             ;; default value
             (if overapproximate top bottom))
           (walk-union (types overapproximate)
             ;; Only do this if union is provided.
             (unless union
               (return-from walk-union (default overapproximate)))
             ;; Reduce/union from bottom.
             (let ((not-acc-p nil)
                   (acc bottom))
               (dolist (type types (values not-acc-p acc))
                 (multiple-value-bind (not x)
                     (walk type overapproximate)
                   (setf (values not-acc-p acc)
                         (unite not-acc-p acc not x)))
                 ;; Early exit on top set.
                 (when (and (eql acc top)
                            (not not-acc-p))
                   (return (values nil top))))))
           (walk-intersection (types overapproximate)
             ;; Skip if we don't know how to intersect sets
             (unless intersection
               (return-from walk-intersection (default overapproximate)))
             ;; Reduce/intersection from top
             (let ((not-acc-p nil)
                   (acc top))
               (dolist (type types (values not-acc-p acc))
                 (multiple-value-bind (not x)
                     (walk type overapproximate)
                   (setf (values not-acc-p acc)
                         (intersect not-acc-p acc not x)))
                 (when (and (eql acc bottom)
                            (not not-acc-p))
                   (return (values nil bottom))))))
           (walk-negate (type overapproximate)
             ;; Don't introduce negated types if we don't know how to
             ;; subtract sets.
             (unless difference
               (return-from walk-negate (default overapproximate)))
             (multiple-value-bind (not x)
                 (walk type (not overapproximate))
               (normalize (not not) x)))
           (walk (type overapproximate)
             (typecase type
               (union-type
                (walk-union (union-type-types type) overapproximate))
               ((cons (member or union))
                (walk-union (rest type) overapproximate))
               (intersection-type
                (walk-intersection (intersection-type-types type) overapproximate))
               ((cons (member and intersection))
                (walk-intersection (rest type) overapproximate))
               (negation-type
                (walk-negate (negation-type-type type) overapproximate))
               ((cons (eql not))
                (walk-negate (second type) overapproximate))
               (t
                (values nil
                        (if overapproximate
                            (if over
                                (funcall over type)
                                (default t))
                            (if under
                                (funcall under type)
                                (default nil))))))))
    (multiple-value-call #'normalize (walk type overapproximate))))
(declaim (notinline generic-abstract-type-function))

;;; Standard list representation of sets. Use CL:* for the universe.
(defun list-abstract-type-function (type over &key under (overapproximate t))
  (declare (inline generic-abstract-type-function))
  (generic-abstract-type-function
   type overapproximate
   #'union #'intersection #'set-difference
   '* nil
   over under))

(!defun-from-collected-cold-init-forms !late-type-cold-init)

#-sb-xc (!late-type-cold-init2)

(/show0 "late-type.lisp end of file")
