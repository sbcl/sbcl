;;;; This file contains stuff that implements the portable IR1
;;;; semantics of type tests and coercion. The main thing we do is
;;;; convert complex type operations into simpler code that can be
;;;; compiled inline.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;;; type predicate translation
;;;;
;;;; We maintain a bidirectional association between type predicates
;;;; and the tested type. The presence of a predicate in this
;;;; association implies that it is desirable to implement tests of
;;;; this type using the predicate. These are either predicates that
;;;; the back end is likely to have special knowledge about, or
;;;; predicates so complex that the only reasonable implentation is
;;;; via function call.
;;;;
;;;; Some standard types (such as ATOM) are best tested by letting the
;;;; TYPEP source transform do its thing with the expansion. These
;;;; types (and corresponding predicates) are not maintained in this
;;;; association. In this case, there need not be any predicate
;;;; function unless it is required by the Common Lisp specification.
;;;;
;;;; The mapping between predicates and type structures is considered
;;;; part of the backend; different backends can support different
;;;; sets of predicates.

;;; Establish an association between the type predicate NAME and the
;;; corresponding TYPE. This causes the type predicate to be
;;; recognized for purposes of optimization.
(defmacro define-type-predicate (name type)
  `(%define-type-predicate ',name ',type))
(defun %define-type-predicate (name specifier)
  (let ((type (specifier-type specifier)))
    (setf (gethash name *backend-predicate-types*) type)
    (setf *backend-type-predicates*
          (cons (cons type name)
                (remove name *backend-type-predicates*
                        :key #'cdr)))
    (%deftransform name '(function (t) *) #'fold-type-predicate)
    name))

;;;; IR1 transforms

;;; If we discover the type argument is constant during IR1
;;; optimization, then give the source transform another chance. The
;;; source transform can't pass, since we give it an explicit
;;; constant. At worst, it will convert to %TYPEP, which will prevent
;;; spurious attempts at transformation (and possible repeated
;;; warnings.)
(deftransform typep ((object type &optional env) * * :node node)
  (unless (constant-lvar-p type)
    (give-up-ir1-transform "can't open-code test of non-constant type"))
  (unless (or (null env)
              (and (constant-lvar-p env) (null (lvar-value env))))
    (give-up-ir1-transform "environment argument present and not null"))
  (multiple-value-bind (expansion fail-p)
      (source-transform-typep 'object (lvar-value type))
    (if fail-p
        (abort-ir1-transform)
        expansion)))

;;; If the lvar OBJECT definitely is or isn't of the specified
;;; type, then return T or NIL as appropriate. Otherwise quietly
;;; GIVE-UP-IR1-TRANSFORM.
(defun ir1-transform-type-predicate (object type node)
  (declare (type lvar object) (type ctype type))
  (let ((otype (lvar-type object)))
    (flet ((tricky ()
             (cond ((typep type 'alien-type-type)
                    ;; We don't transform alien type tests until here, because
                    ;; once we do that the rest of the type system can no longer
                    ;; reason about them properly -- so we'd miss out on type
                    ;; derivation, etc.
                    (delay-ir1-transform node :optimize)
                    (let ((alien-type (alien-type-type-alien-type type)))
                      ;; If it's a lisp-rep-type, the CTYPE should be one already.
                      (aver (not (compute-lisp-rep-type alien-type)))
                      `(sb!alien::alien-value-typep object ',alien-type)))
                   (t
                    (give-up-ir1-transform)))))
      (cond ((not (types-equal-or-intersect otype type))
            nil)
           ((csubtypep otype type)
            t)
           ((eq type *empty-type*)
            nil)
           (t
            (let ((intersect (type-intersection2 type otype)))
              (unless intersect
                (tricky))
              (multiple-value-bind (constantp value)
                  (type-singleton-p intersect)
                (if constantp
                    `(eql object ',value)
                    (tricky)))))))))

;;; Flush %TYPEP tests whose result is known at compile time.
(deftransform %typep ((object type) * * :node node)
  (unless (constant-lvar-p type)
    (give-up-ir1-transform))
  (ir1-transform-type-predicate
   object
   (ir1-transform-specifier-type (lvar-value type))
   node))

;;; This is the IR1 transform for simple type predicates. It checks
;;; whether the single argument is known to (not) be of the
;;; appropriate type, expanding to T or NIL as appropriate.
(deftransform fold-type-predicate ((object) * * :node node :defun-only t)
  (let ((ctype (gethash (leaf-source-name
                         (ref-leaf
                          (lvar-uses
                           (basic-combination-fun node))))
                        *backend-predicate-types*)))
    (aver ctype)
    (ir1-transform-type-predicate object ctype node)))

;;; If FIND-CLASSOID is called on a constant class, locate the
;;; CLASSOID-CELL at load time.
(deftransform find-classoid ((name) ((constant-arg symbol)) *)
  (let* ((name (lvar-value name))
         (cell (find-classoid-cell name :create t)))
    `(or (classoid-cell-classoid ',cell)
         (error "class not yet defined: ~S" name))))

(defoptimizer (%typep-wrapper constraint-propagate-if)
    ((test-value variable type) node gen)
  (declare (ignore test-value gen))
  (aver (constant-lvar-p type))
  (let ((type (lvar-value type)))
    (values variable (if (ctype-p type)
                         type
                         (handler-case (careful-specifier-type type)
                           (t () nil))))))

(deftransform %typep-wrapper ((test-value variable type) * * :node node)
  (aver (constant-lvar-p type))
  (if (constant-lvar-p test-value)
      `',(lvar-value test-value)
      (let* ((type (lvar-value type))
             (type (if (ctype-p type)
                       type
                       (handler-case (careful-specifier-type type)
                         (t () nil))))
             (value-type (lvar-type variable)))
        (cond ((not type)
               'test-value)
              ((csubtypep value-type type)
               t)
              ((not (types-equal-or-intersect value-type type))
               nil)
              (t
               (delay-ir1-transform node :constraint)
               'test-value)))))

;;;; standard type predicates, i.e. those defined in package COMMON-LISP,
;;;; plus at least one oddball (%INSTANCEP)
;;;;
;;;; Various other type predicates (e.g. low-level representation
;;;; stuff like SIMPLE-ARRAY-SINGLE-FLOAT-P) are defined elsewhere.

;;; FIXME: This function is only called once, at top level. Why not
;;; just expand all its operations into toplevel code?
(defun !define-standard-type-predicates ()
  (define-type-predicate arrayp array)
  ; (The ATOM predicate is handled separately as (NOT CONS).)
  (define-type-predicate bit-vector-p bit-vector)
  (define-type-predicate characterp character)
  (define-type-predicate compiled-function-p compiled-function)
  (define-type-predicate complexp complex)
  (define-type-predicate complex-rational-p (complex rational))
  (define-type-predicate complex-float-p (complex float))
  (define-type-predicate consp cons)
  (define-type-predicate floatp float)
  (define-type-predicate functionp function)
  (define-type-predicate integerp integer)
  (define-type-predicate keywordp keyword)
  (define-type-predicate listp list)
  (define-type-predicate null null)
  (define-type-predicate numberp number)
  (define-type-predicate rationalp rational)
  (define-type-predicate realp real)
  (define-type-predicate sequencep sequence)
  (define-type-predicate extended-sequence-p extended-sequence)
  (define-type-predicate simple-bit-vector-p simple-bit-vector)
  (define-type-predicate simple-string-p simple-string)
  (define-type-predicate simple-vector-p simple-vector)
  (define-type-predicate stringp string)
  (define-type-predicate %instancep instance)
  (define-type-predicate simple-fun-p simple-fun)
  (define-type-predicate closurep closure)
  (define-type-predicate funcallable-instance-p funcallable-instance)
  (define-type-predicate symbolp symbol)
  (define-type-predicate vectorp vector))
(!define-standard-type-predicates)

;;;; transforms for type predicates not implemented primitively
;;;;
;;;; See also VM dependent transforms.

(define-source-transform atom (x)
  `(not (consp ,x)))
#!+sb-unicode
(define-source-transform base-char-p (x)
  `(typep ,x 'base-char))
;; CONS is implemented as (and list (not (eql nil))) where the 'and' is
;; built-in to the consp vop. Reduce to just LISTP if possible.
(deftransform consp ((x) ((not null)) * :important nil) '(listp x))

;;;; TYPEP source transform

;;; Return a form that tests the variable N-OBJECT for being in the
;;; binds specified by TYPE. BASE is the name of the base type, for
;;; declaration. We make SAFETY locally 0 to inhibit any checking of
;;; this assertion.
(defun transform-numeric-bound-test (n-object type base)
  (declare (type numeric-type type))
  (let ((low (numeric-type-low type))
        (high (numeric-type-high type)))
    `(locally
       (declare (optimize (safety 0)))
       (and ,@(when low
                (if (consp low)
                    `((> (truly-the ,base ,n-object) ,(car low)))
                    `((>= (truly-the ,base ,n-object) ,low))))
            ,@(when high
                (if (consp high)
                    `((< (truly-the ,base ,n-object) ,(car high)))
                    `((<= (truly-the ,base ,n-object) ,high))))))))

;;; Do source transformation of a test of a known numeric type. We can
;;; assume that the type doesn't have a corresponding predicate, since
;;; those types have already been picked off. In particular, CLASS
;;; must be specified, since it is unspecified only in NUMBER and
;;; COMPLEX. Similarly, we assume that COMPLEXP is always specified.
;;;
;;; For non-complex types, we just test that the number belongs to the
;;; base type, and then test that it is in bounds. When CLASS is
;;; INTEGER, we check to see whether the range is no bigger than
;;; FIXNUM. If so, we check for FIXNUM instead of INTEGER. This allows
;;; us to use fixnum comparison to test the bounds.
;;;
;;; For complex types, we must test for complex, then do the above on
;;; both the real and imaginary parts. When CLASS is float, we need
;;; only check the type of the realpart, since the format of the
;;; realpart and the imagpart must be the same.
(defun source-transform-numeric-typep (object type)
  (let* ((class (numeric-type-class type))
         (base (ecase class
                 (integer (containing-integer-type
                           (if (numeric-type-complexp type)
                               (modified-numeric-type type
                                                      :complexp :real)
                               type)))
                 (rational 'rational)
                 (float (or (numeric-type-format type) 'float))
                 ((nil) 'real))))
    (once-only ((n-object object))
      (ecase (numeric-type-complexp type)
        (:real
         (if (and #!-(or x86 x86-64 arm arm64) ;; Not implemented elsewhere yet
                  nil
                  (eql (numeric-type-class type) 'integer)
                  (eql (numeric-type-low type) 0)
                  (fixnump (numeric-type-high type)))
             `(fixnum-mod-p ,n-object ,(numeric-type-high type))
             `(and (typep ,n-object ',base)
                   ,(transform-numeric-bound-test n-object type base))))
        (:complex
         `(and (complexp ,n-object)
               ,(once-only ((n-real `(realpart (truly-the complex ,n-object)))
                            (n-imag `(imagpart (truly-the complex ,n-object))))
                  `(progn
                     ,n-imag ; ignorable
                     (and (typep ,n-real ',base)
                          ,@(when (eq class 'integer)
                              `((typep ,n-imag ',base)))
                          ,(transform-numeric-bound-test n-real type base)
                          ,(transform-numeric-bound-test n-imag type
                                                         base))))))))))

;;; Do the source transformation for a test of a hairy type. AND,
;;; SATISFIES and NOT are converted into the obvious code. We convert
;;; unknown types to %TYPEP, emitting an efficiency note if
;;; appropriate.
(defun source-transform-hairy-typep (object type)
  (declare (type hairy-type type))
  (let ((spec (hairy-type-specifier type)))
    (cond ((unknown-type-p type)
           #+sb-xc-host
           (warn "can't open-code test of unknown type ~S"
                 (type-specifier type))
           #-sb-xc-host
           (when (policy *lexenv* (> speed inhibit-warnings))
             (compiler-notify "can't open-code test of unknown type ~S"
                              (type-specifier type)))
           `(%typep ,object ',spec))
          (t
           (ecase (first spec)
             (satisfies
              (let* ((name (second spec))
                     (expansion (fun-name-inline-expansion name)))
                ;; Lambda without lexenv can easily be handled here.
                ;; This fixes the issue that LEGAL-FUN-NAME-P which is
                ;; just a renaming of VALID-FUNCTION-NAME-P would not
                ;; be inlined when testing the FUNCTION-NAME type.
                `(if ,(if (and (typep expansion '(cons (eql lambda)))
                               (not (fun-lexically-notinline-p name)))
                          `(,expansion ,object)
                          `(funcall (global-function ,name) ,object))
                     t nil)))
             ((not and)
              (once-only ((n-obj object))
                `(,(first spec) ,@(mapcar (lambda (x)
                                            `(typep ,n-obj ',x))
                                          (rest spec))))))))))

(defun source-transform-negation-typep (object type)
  (declare (type negation-type type))
  (let ((spec (type-specifier (negation-type-type type))))
    `(not (typep ,object ',spec))))

;;; Do source transformation for TYPEP of a known union type. If a
;;; union type contains LIST, then we pull that out and make it into a
;;; single LISTP call.  Note that if SYMBOL is in the union, then LIST
;;; will be a subtype even without there being any (member NIL).  We
;;; currently just drop through to the general code in this case,
;;; rather than trying to optimize it (but FIXME CSR 2004-04-05: it
;;; wouldn't be hard to optimize it after all).
;;; FIXME: if the CONSP|NIL -> LISTP optimization kicks in,
;;;        we forgo the array optimizations.
(defun source-transform-union-typep (object type)
  (let* ((types (union-type-types type))
         (type-cons (specifier-type 'cons))
         (mtype (find-if #'member-type-p types))
         (members (when mtype (member-type-members mtype))))
    (once-only ((n-obj object))
      (if (and mtype
               (memq nil members)
               (memq type-cons types))
          `(or (listp ,n-obj)
               (typep ,n-obj
                      '(or ,@(mapcar #'type-specifier
                                     (remove type-cons
                                             (remove mtype types)))
                        (member ,@(remove nil members)))))
          (multiple-value-bind (widetags more-types)
              (sb!kernel::widetags-from-union-type types)
            `(or ,@(if widetags
                       `((%other-pointer-subtype-p ,n-obj ',widetags)))
                 ,@(mapcar (lambda (x)
                             `(typep ,n-obj ',(type-specifier x)))
                           more-types)))))))

;;; Do source transformation for TYPEP of a known intersection type.
(defun source-transform-intersection-typep (object type)
  (once-only ((n-obj object))
    `(and ,@(mapcar (lambda (x)
                      `(typep ,n-obj ',(type-specifier x)))
                    (intersection-type-types type)))))

;;; If necessary recurse to check the cons type.
(defun source-transform-cons-typep (object type)
  (let* ((car-type (cons-type-car-type type))
         (cdr-type (cons-type-cdr-type type))
         (car-test-p (not (type= car-type *universal-type*)))
         (cdr-test-p (not (type= cdr-type *universal-type*))))
    (if (and (not car-test-p) (not cdr-test-p))
        `(consp ,object)
        ;; CONSP can be safely weakened to LISTP if either of the CAR
        ;; or CDR test (or both) can distinguish LIST from CONS
        ;; by never returning T when given an input of NIL.
        (labels ((safely-weakened (ctype)
                   (typecase ctype
                     (member-type
                      (not (member nil (member-type-members ctype))))
                     (classoid
                      ;; can't weaken if the specifier is (CONS SYMBOL)
                      (not (ctypep nil ctype)))
                     ;; these are disjoint from NIL
                     ((or cons-type numeric-type array-type character-set-type)
                      t)
                     (intersection-type
                      ;; at least one of them must not spuriously return T
                      (some #'safely-weakened (compound-type-types ctype)))
                     (union-type
                      ;; require that none spuriously return T
                      (every #'safely-weakened (compound-type-types ctype)))
                     (hairy-type
                      ;; hack - (CONS KEYWORD) is weakenable
                      ;; because NIL is not a keyword.
                      (equal (hairy-type-specifier ctype)
                             '(satisfies keywordp))))))
          (let* ((n-obj (sb!xc:gensym))
                 (car-test
                  (and car-test-p
                       `((typep (car ,n-obj) ',(type-specifier car-type)))))
                 (cdr-test
                  (and cdr-test-p
                       `((typep (cdr ,n-obj) ',(type-specifier cdr-type))))))
            `(let ((,n-obj ,object))
               ;; Being paranoid, perform the safely weakenable test first
               ;; so that the other part doesn't execute on an object that
               ;; it would not have gotten, were the CONSP test not weakened.
               ,(cond ((and car-test-p (safely-weakened car-type))
                       `(and (listp ,n-obj) ,@car-test ,@cdr-test))
                      ((and cdr-test-p (safely-weakened cdr-type))
                       `(and (listp ,n-obj) ,@cdr-test ,@car-test))
                      (t
                       `(and (consp ,n-obj) ,@car-test ,@cdr-test)))))))))

(defun source-transform-character-set-typep (object type)
  (let ((pairs (character-set-type-pairs type)))
    (if (and (= (length pairs) 1)
            (= (caar pairs) 0)
            (= (cdar pairs) (1- sb!xc:char-code-limit)))
       `(characterp ,object)
       (once-only ((n-obj object))
         (let ((n-code (gensym "CODE")))
           `(and (characterp ,n-obj)
                 (let ((,n-code (sb!xc:char-code ,n-obj)))
                   (or
                    ,@(loop for pair in pairs
                            collect
                            `(<= ,(car pair) ,n-code ,(cdr pair)))))))))))

#!+sb-simd-pack
(defun source-transform-simd-pack-typep (object type)
  (if (type= type (specifier-type 'simd-pack))
      `(simd-pack-p ,object)
      (once-only ((n-obj object))
        (let ((n-tag (gensym "TAG")))
          `(and
            (simd-pack-p ,n-obj)
            (let ((,n-tag (%simd-pack-tag ,n-obj)))
              (or ,@(loop
                      for type in (simd-pack-type-element-type type)
                      for index = (position type *simd-pack-element-types*)
                      collect `(eql ,n-tag ,index)))))))))

;;; Return the predicate and type from the most specific entry in
;;; *TYPE-PREDICATES* that is a supertype of TYPE.
(defun find-supertype-predicate (type)
  (declare (type ctype type))
  (let ((res nil)
        (res-type nil))
    (dolist (x *backend-type-predicates*)
      (let ((stype (car x)))
        (when (and (csubtypep type stype)
                   (or (not res-type)
                       (csubtypep stype res-type)))
          (setq res-type stype)
          (setq res (cdr x)))))
    (values res res-type)))

;;; Return forms to test that OBJ has the rank and dimensions
;;; specified by TYPE, where STYPE is the type we have checked against
;;; (which is the same but for dimensions and element type).
;;;
;;; Secondary return value is true if passing the generated tests implies that
;;; the array has a header.
(defun test-array-dimensions (obj type stype)
  (declare (type array-type type stype))
  (let ((obj `(truly-the ,(type-specifier stype) ,obj))
        (dims (array-type-dimensions type)))
    (unless (or (eq dims '*)
                (equal dims (array-type-dimensions stype)))
      (cond ((cdr dims)
             (values `((array-header-p ,obj)
                       ,@(when (eq (array-type-dimensions stype) '*)
                               `((= (%array-rank ,obj) ,(length dims))))
                       ,@(loop for d in dims
                               for i from 0
                               unless (eq '* d)
                               collect `(= (%array-dimension ,obj ,i) ,d)))
                     t))
            ((not dims)
             (values `((array-header-p ,obj)
                       (= (%array-rank ,obj) 0))
                     t))
            ((not (array-type-complexp type))
             (if (csubtypep stype (specifier-type 'vector))
                 (values (unless (eq '* (car dims))
                           `((= (vector-length ,obj) ,@dims)))
                         nil)
                 (values (if (eq '* (car dims))
                             `((not (array-header-p ,obj)))
                             `((not (array-header-p ,obj))
                               (= (vector-length ,obj) ,@dims)))
                         nil)))
            (t
             (values (unless (eq '* (car dims))
                       `((if (array-header-p ,obj)
                             (= (%array-dimension ,obj 0) ,@dims)
                             (= (vector-length ,obj) ,@dims))))
                     nil))))))

;;; Return forms to test that OBJ has the element-type specified by type
;;; specified by TYPE, where STYPE is the type we have checked against (which
;;; is the same but for dimensions and element type). If HEADERP is true, OBJ
;;; is guaranteed to be an array-header.
(defun test-array-element-type (obj type stype headerp)
  (declare (type array-type type stype))
  (let ((obj `(truly-the ,(type-specifier stype) ,obj))
        (eltype (array-type-specialized-element-type type)))
    (unless (or (type= eltype (array-type-specialized-element-type stype))
                (eq eltype *wild-type*))
      (let ((typecode (sb!vm:saetp-typecode (find-saetp-by-ctype eltype))))
        (with-unique-names (data)
         (if (and headerp (not (array-type-complexp stype)))
             ;; If we know OBJ is an array header, and that the array is
             ;; simple, we also know there is exactly one indirection to
             ;; follow.
             `((eq (%other-pointer-widetag (%array-data-vector ,obj)) ,typecode))
             `((do ((,data ,(if headerp `(%array-data-vector ,obj) obj)
                           (%array-data-vector ,data)))
                   ((not (array-header-p ,data))
                    (eq (%other-pointer-widetag ,data) ,typecode))))))))))

;;; If we can find a type predicate that tests for the type without
;;; dimensions, then use that predicate and test for dimensions.
;;; Otherwise, just do %TYPEP.
(defun source-transform-array-typep (obj type)
  ;; Intercept (SIMPLE-ARRAY * (*)) because otherwise it tests
  ;; (AND SIMPLE-ARRAY (NOT ARRAY-HEADER)) to weed out rank 0 and >1.
  ;; By design the simple arrays of of rank 1 occupy a contiguous
  ;; range of widetags, and unlike the arbitrary-widetags code for unions,
  ;; this nonstandard predicate can be generically defined for all backends.
  (if (and (not (array-type-complexp type))
           (eq (array-type-element-type type) *wild-type*)
           (equal (array-type-dimensions type) '(*)))
      (return-from source-transform-array-typep
        `(simple-rank-1-array-*-p ,obj)))
  (multiple-value-bind (pred stype) (find-supertype-predicate type)
    (if (and (array-type-p stype)
             ;; (If the element type hasn't been defined yet, it's
             ;; not safe to assume here that it will eventually
             ;; have (UPGRADED-ARRAY-ELEMENT-TYPE type)=T, so punt.)
             (not (unknown-type-p (array-type-element-type type)))
             (or (eq (array-type-complexp stype) (array-type-complexp type))
                 (and (eql (array-type-complexp stype) :maybe)
                      (eql (array-type-complexp type) t))))
        (once-only ((n-obj obj))
          (multiple-value-bind (tests headerp)
              (test-array-dimensions n-obj type stype)
            `(and (,pred ,n-obj)
                  ,@(when (and (eql (array-type-complexp stype) :maybe)
                               (eql (array-type-complexp type) t))
                      ;; KLUDGE: this is a bit lame; if we get here,
                      ;; we already know that N-OBJ is an array, but
                      ;; (NOT SIMPLE-ARRAY) doesn't know that.  On the
                      ;; other hand, this should get compiled down to
                      ;; two widetag tests, so it's only a bit lame.
                      `((typep ,n-obj '(not simple-array))))
                  ,@tests
                  ,@(test-array-element-type n-obj type stype headerp))))
        `(%typep ,obj ',(type-specifier type)))))

;;; Transform a type test against some instance type. The type test is
;;; flushed if the result is known at compile time. If not properly
;;; named, error. If sealed and has no subclasses, just test for
;;; layout-EQ. If a structure then test for layout-EQ and then a
;;; general test based on layout-inherits. If safety is important,
;;; then we also check whether the layout for the object is invalid
;;; and signal an error if so. Otherwise, look up the indirect
;;; class-cell and call CLASS-CELL-TYPEP at runtime.
(deftransform %instance-typep ((object spec) (* *) * :node node)
  (aver (constant-lvar-p spec))
  (let* ((spec (lvar-value spec))
         (class (specifier-type spec))
         (name (classoid-name class))
         (otype (lvar-type object))
         (layout (let ((res (info :type :compiler-layout name)))
                   (if (and res (not (layout-invalid res)))
                       res
                       nil))))
    (cond
      ;; Flush tests whose result is known at compile time.
      ((not (types-equal-or-intersect otype class))
       nil)
      ((csubtypep otype class)
       t)
      ;; If not properly named, error.
      ((not (and name (eq (find-classoid name) class)))
       (compiler-error "can't compile TYPEP of anonymous or undefined ~
                        class:~%  ~S"
                       class))
      (t
       ;; Delay the type transform to give type propagation a chance.
       (delay-ir1-transform node :constraint)

       ;; FIXME: (TYPEP X 'ERROR) - or any condition - checks whether X
       ;; has the lowtag of either an ordinary or funcallable instance.
       ;; But you can not define a class that is both CONDITION and FUNCTION
       ;; because CONDITION-CLASS and FUNCALLABLE-STANDARD-CLASS are
       ;; incompatible metaclasses. Thus the type test is less efficient than
       ;; could be, since fun-pointer-lowtag can not occur in the "true" case.

       ;; Otherwise transform the type test.
       (binding* (((pred get-layout)
                   (cond ((csubtypep class (specifier-type 'funcallable-instance))
                          (values '(funcallable-instance-p object)
                                  '(%funcallable-instance-layout object)))
                         ((csubtypep class (specifier-type 'instance))
                          (values '(%instancep object)
                                  '(%instance-layout object)))))
                  (get-layout-or-return-false
                   (if pred
                       ;; Test just one of %INSTANCEP or %FUNCALLABLE-INSTANCE-P
                       `(if ,pred ,get-layout (return-from typep nil))
                       ;; But if we don't know which is will be, try both.
                       ;; This is less general than LAYOUT-OF,and therefore
                       ;; a little quicker to fail, because objects with
                       ;; {LIST|OTHER}-POINTER-LOWTAG can't possibly pass.
                       `(cond ((%instancep object)
                               (%instance-layout object))
                              ((funcallable-instance-p object)
                               (%funcallable-instance-layout object))
                              (t (return-from typep nil)))))
                  (n-layout (gensym)))
         (cond
           ;; It's possible to seal a STANDARD-CLASS, not just a STRUCTURE-CLASS,
           ;; though probably extremely weird. Also the PRED should be set in
           ;; that event, but it isn't.
           ((and (eq (classoid-state class) :sealed) layout
                 (not (classoid-subclasses class)))
            ;; Sealed and has no subclasses.
            ;; The crummy dual expressions for the same result are because
            ;; (BLOCK (RETURN ...)) seems to emit a forward branch in the
            ;; passing case, but AND emits a forward branch in the failing
            ;; case which I believe is the better choice.
            (if pred
                `(and ,pred (eq ,get-layout ',layout))
                `(block typep (eq ,get-layout-or-return-false ',layout))))

           ((and (typep class 'structure-classoid) layout)
            ;; structure type tests; hierarchical layout depths
            (let* ((depthoid (layout-depthoid layout))
                   ;; If a structure is apparently an abstract base type,
                   ;; having no constructor, then no instance layout should
                   ;; be EQ to the classoid's layout. It is a slight win
                   ;; to use the depth-based check first, then do the EQ check.
                   ;; There is no loss in the case where both fail, and there
                   ;; is a benefit in a passing case. Always try both though,
                   ;; because (MAKE-INSTANCE 'x) works on any structure class.
                   (abstract-base-p (awhen (layout-info layout)
                                      (not (dd-constructors it))))
                   (get-ancestor
                    ;; Use DATA-VECTOR-REF directly, since that's what SVREF in
                    ;; a SAFETY 0 lexenv will eventually be transformed to.
                    ;; This can give a large compilation speedup, since
                    ;; %INSTANCE-TYPEPs are frequently created during
                    ;; GENERATE-TYPE-CHECKS, and the normal aref transformation
                    ;; path is pretty heavy.
                    `(locally (declare (optimize (safety 0)))
                       (data-vector-ref (layout-inherits ,n-layout) ,depthoid)))
                   (deeper-p `(> (layout-depthoid ,n-layout) ,depthoid)))
              (aver (equal pred '(%instancep object)))
              `(and ,pred
                    (let ((,n-layout ,get-layout))
                      ;; we used to check for invalid layouts here,
                      ;; but in fact that's both unnecessary and
                      ;; wrong; it's unnecessary because structure
                      ;; classes can't be redefined, and it's wrong
                      ;; because it is quite legitimate to pass an
                      ;; object with an invalid layout to a structure
                      ;; type test.
                      ,(if abstract-base-p
                           `(eq (if ,deeper-p ,get-ancestor ,n-layout) ,layout)
                           `(cond ((eq ,n-layout ,layout) t)
                                  (,deeper-p (eq ,get-ancestor ,layout))))))))
           ((and layout (>= (layout-depthoid layout) 0))
            ;; hierarchical layout depths for other things (e.g.
            ;; CONDITION, STREAM)
            ;; The quasi-hierarchical types are abstract base types,
            ;; so perform inheritance check first, and EQ second.
            ;; Actually, since you can't make an abstract STREAM,
            ;; maybe we should skip the EQ test? But you *can* make
            ;; an instance of CONDITION for what it's worth.
            ;; SEQUENCE is special-cased, but could be handled here.
            (let* ((depthoid (layout-depthoid layout))
                   (n-inherits (gensym))
                   (guts
                    `((when (layout-invalid ,n-layout)
                        (setq ,n-layout (update-object-layout-or-invalid
                                         object ',layout)))
                      (let ((,n-inherits (layout-inherits
                                          (truly-the layout ,n-layout))))
                        (declare (optimize (safety 0)))
                        (eq (if (> (vector-length ,n-inherits) ,depthoid)
                                (data-vector-ref ,n-inherits ,depthoid)
                                ,n-layout)
                            ,layout)))))
              (if pred
                  `(and ,pred (let ((,n-layout ,get-layout)) ,@guts))
                  `(block typep
                     (let ((,n-layout ,get-layout-or-return-false)) ,@guts)))))

           (t
            (/noshow "default case -- ,PRED and CLASS-CELL-TYPEP")
            `(block typep
               (classoid-cell-typep ,get-layout-or-return-false
                                       ',(find-classoid-cell name :create t)
                                       object)))))))))

;;; If the specifier argument is a quoted constant, then we consider
;;; converting into a simple predicate or other stuff. If the type is
;;; constant, but we can't transform the call, then we convert to
;;; %TYPEP. We only pass when the type is non-constant. This allows us
;;; to recognize between calls that might later be transformed
;;; successfully when a constant type is discovered. We don't give an
;;; efficiency note when we pass, since the IR1 transform will give
;;; one if necessary and appropriate.
;;;
;;; If the type is TYPE= to a type that has a predicate, then expand
;;; to that predicate. Otherwise, we dispatch off of the type's type.
;;; These transformations can increase space, but it is hard to tell
;;; when, so we ignore policy and always do them.
(defun %source-transform-typep (object type)
  (let ((ctype (careful-specifier-type type)))
    (or (when (not ctype)
          (compiler-warn "illegal type specifier for TYPEP: ~S" type)
          (return-from %source-transform-typep (values nil t)))
        (multiple-value-bind (constantp value) (type-singleton-p ctype)
          (and constantp
               `(eql ,object ',value)))
        (let ((pred (cdr (assoc ctype *backend-type-predicates*
                                :test #'type=))))
          (when pred `(,pred ,object)))
        (typecase ctype
          (hairy-type
           (source-transform-hairy-typep object ctype))
          (negation-type
           (source-transform-negation-typep object ctype))
          (union-type
           (source-transform-union-typep object ctype))
          (intersection-type
           (source-transform-intersection-typep object ctype))
          (member-type
           `(if (member ,object ',(member-type-members ctype)) t))
          (args-type
           (compiler-warn "illegal type specifier for TYPEP: ~S" type)
           (return-from %source-transform-typep (values nil t)))
          (t nil))
        (typecase ctype
          (numeric-type
           (source-transform-numeric-typep object ctype))
          (classoid
           `(%instance-typep ,object ',type))
          (array-type
           (source-transform-array-typep object ctype))
          (cons-type
           (source-transform-cons-typep object ctype))
          (character-set-type
           (source-transform-character-set-typep object ctype))
          #!+sb-simd-pack
          (simd-pack-type
           (source-transform-simd-pack-typep object ctype))
          (t nil))
        `(%typep ,object ',type))))

(defun source-transform-typep (object type)
  (when (typep type 'type-specifier)
    (check-deprecated-type type))
  (let ((name (gensym "OBJECT")))
    (multiple-value-bind (transform error)
        (%source-transform-typep name type)
      (if error
          (values nil t)
          (values `(let ((,name ,object))
                     (%typep-wrapper ,transform ,name ',type)))))))

(define-source-transform typep (object spec &optional env)
  ;; KLUDGE: It looks bad to only do this on explicitly quoted forms,
  ;; since that would overlook other kinds of constants. But it turns
  ;; out that the DEFTRANSFORM for TYPEP detects any constant
  ;; lvar, transforms it into a quoted form, and gives this
  ;; source transform another chance, so it all works out OK, in a
  ;; weird roundabout way. -- WHN 2001-03-18
  (if (and (not env)
           (consp spec)
           (eq (car spec) 'quote))
      (source-transform-typep object (cadr spec))
      (values nil t)))

;;;; coercion

;;; Constant-folding.
;;;
#-sb-xc-host
(defoptimizer (coerce optimizer) ((x type) node)
  (when (and (constant-lvar-p x) (constant-lvar-p type))
    (let ((value (lvar-value x)))
      (when (or (numberp value) (characterp value))
        (constant-fold-call node)
        t))))

;;; Drops dimension information from vector types.
(defun simplify-vector-type (type)
  (aver (csubtypep type (specifier-type '(array * (*)))))
  (let* ((array-type
          (if (csubtypep type (specifier-type 'simple-array))
              'simple-array
              'array))
         (complexp
          (not
           (or (eq 'simple-array array-type)
               (neq *empty-type*
                    (type-intersection type (specifier-type 'simple-array)))))))
    (dolist (etype
              #+sb-xc-host '(t bit character)
              #-sb-xc-host sb!kernel::*specialized-array-element-types*
              #+sb-xc-host (values nil nil nil)
              #-sb-xc-host (values `(,array-type * (*)) t complexp))
      (when etype
        (let ((simplified (specifier-type `(,array-type ,etype (*)))))
          (when (csubtypep type simplified)
            (return (values (type-specifier simplified)
                            etype
                            complexp))))))))

(deftransform coerce ((x type) (* *) * :node node)
  (unless (constant-lvar-p type)
    (give-up-ir1-transform))
  (let* ((tval (lvar-value type))
         (tspec (ir1-transform-specifier-type tval)))
    (if (csubtypep (lvar-type x) tspec)
        'x
        ;; Note: The THE forms we use to wrap the results make sure that
        ;; specifiers like (SINGLE-FLOAT 0.0 1.0) can raise a TYPE-ERROR.
        (cond
          ((csubtypep tspec (specifier-type 'double-float))
           `(the ,tval (%double-float x)))
          ;; FIXME: #!+long-float (t ,(error "LONG-FLOAT case needed"))
          ((csubtypep tspec (specifier-type 'float))
           `(the ,tval (%single-float x)))
          ((csubtypep tspec (specifier-type 'complex))
           (multiple-value-bind (part-type result-type)
               (cond ((and (numeric-type-p tspec)
                           (numeric-type-format tspec))) ; specific FLOAT type
                     ((csubtypep tspec (specifier-type '(complex float)))
                      ;; unspecific FLOAT type
                      'float)
                     ((csubtypep tspec (specifier-type '(complex rational)))
                      (values 'rational `(or ,tval rational)))
                     (t
                      (values t `(or ,tval rational))))
             (let ((result-type (or result-type tval)))
               `(cond
                  ((not (typep x 'complex))
                   (the ,result-type (complex (coerce x ',part-type))))
                  ((typep x ',tval)
                   x)
                  (t     ; X is COMPLEX, but not of the requested type
                   (the ,result-type
                        (complex (coerce (realpart x) ',part-type)
                                 (coerce (imagpart x) ',part-type))))))))
           ;; Special case STRING and SIMPLE-STRING as they are union types
           ;; in SBCL.
          ((member tval '(string simple-string))
           `(the ,tval
                 (if (typep x ',tval)
                     x
                     (replace (make-array (length x) :element-type 'character) x))))
          ((eq tval 'character)
           `(character x))
           ;; Special case VECTOR
           ((eq tval 'vector)
            `(the ,tval
               (if (vectorp x)
                   x
                   (replace (make-array (length x)) x))))
           ;; Handle specialized element types for 1D arrays.
           ((csubtypep tspec (specifier-type '(array * (*))))
            ;; Can we avoid checking for dimension issues like (COERCE FOO
            ;; '(SIMPLE-VECTOR 5)) returning a vector of length 6?
            ;;
            ;; CLHS actually allows this for all code with SAFETY < 3,
            ;; but we're a conservative bunch.
            (if (or (policy node (zerop safety)) ; no need in unsafe code
                    (and (array-type-p tspec)    ; no need when no dimensions
                         (equal (array-type-dimensions tspec) '(*))))
                ;; We can!
                (multiple-value-bind (vtype etype complexp) (simplify-vector-type tspec)
                  (unless vtype
                    (give-up-ir1-transform))
                  `(the ,vtype
                     (if (typep x ',vtype)
                         x
                         (replace
                          (make-array (length x) :element-type ',etype
                                      ,@(when complexp
                                              (list :fill-pointer t
                                                    :adjustable t)))
                          x))))
                ;; No, duh. Dimension checking required.
                (give-up-ir1-transform
                 "~@<~S specifies dimensions other than (*) in safe code.~:@>"
                 tval)))
           ((type= tspec (specifier-type 'list))
            `(coerce-to-list x))
           ((csubtypep tspec (specifier-type 'function))
            (if (csubtypep (lvar-type x) (specifier-type 'symbol))
                `(coerce-symbol-to-fun x)
                ;; if X can later be derived as FUNCTION then we don't want
                ;; to call COERCE-TO-FUN, because there's no smartness
                ;; that can undo that and see that it's really (IDENTITY X).
                (progn (delay-ir1-transform node :constraint)
                       `(coerce-to-fun x))))
           (t
            (give-up-ir1-transform
             "~@<open coding coercion to ~S not implemented.~:@>"
             tval))))))
