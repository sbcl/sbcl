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

(in-package "SB-C")

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
    (%deftransform name nil '(function (t) *) #'fold-type-predicate)))

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
  (unless (unsupplied-or-nil env)
    (give-up-ir1-transform "environment argument present and not null"))
  (multiple-value-bind (expansion fail-p)
      (source-transform-typep 'object (lvar-value type))
    (if fail-p
        (abort-ir1-transform)
        expansion)))

(sb-xc:deftype other-pointer ()
  '(or array
    (and number (not (or fixnum #+64-bit single-float)))
    fdefn (and symbol (not null))
    weak-pointer system-area-pointer code-component))

(defun type-other-pointer-p (type)
  (csubtypep type (specifier-type 'other-pointer)))

(defun type-not-other-pointer-p (type)
  (csubtypep type (specifier-type '(not other-pointer))))

;;; If the lvar OBJECT definitely is or isn't of the specified
;;; type, then return T or NIL as appropriate. Otherwise quietly
;;; GIVE-UP-IR1-TRANSFORM.
(defun ir1-transform-type-predicate (object type node)
  (declare (type lvar object) (type ctype type))
  (let ((otype (lvar-type object)))
    (cond ((not (types-equal-or-intersect otype type))
           (return-from ir1-transform-type-predicate nil))
          ((csubtypep otype type)
           (return-from ir1-transform-type-predicate t))
          ((eq type *empty-type*)
           (return-from ir1-transform-type-predicate nil)))
    (let ((intersect (type-intersection type otype))
          (current-predicate (combination-fun-source-name node)))
      ;; If the object type is known to be (OR NULL <type>),
      ;; it is almost always cheaper to test for not EQ to NIL.
      ;; There is one exception:
      ;;  - FIXNUMP is possibly cheaper than comparison to NIL, or definitely
      ;;    not worse. For x86, NIL is a 4-byte immediate operand,
      ;;    for lack of a null-tn register. FIXNUM-TAG-MASK is only 1 byte.
      (when (type= otype (type-union (specifier-type 'null) type))
        (let ((difference (type-difference type (specifier-type 'null))))
          (unless (type= difference (specifier-type 'fixnum))
            (return-from  ir1-transform-type-predicate `(not (null object))))))
      (flet ((memory-type-test-p (type)
               (and (types-equal-or-intersect
                     type
                     (specifier-type
                      '(not (or fixnum #+64-bit single-float
                                       boolean character
                             function list))))
                    (not (type= type (specifier-type 'instance))))))
        (cond ((typep type 'alien-type-type)
               ;; We don't transform alien type tests until here, because
               ;; once we do that the rest of the type system can no longer
               ;; reason about them properly -- so we'd miss out on type
               ;; derivation, etc.
               (delay-ir1-transform node :ir1-phases)
               (let ((alien-type (alien-type-type-alien-type type)))
                 ;; If it's a lisp-rep-type, the CTYPE should be one already.
                 (aver (not (compute-lisp-rep-type alien-type)))
                 `(sb-alien::alien-value-typep object ',alien-type)))
              ((and (neq current-predicate 'arrayp)
                    (csubtypep intersect (specifier-type 'array))
                    (not (types-equal-or-intersect (type-difference otype type)
                                                   (specifier-type 'array))))
               `(arrayp object))
              ((and (neq current-predicate 'simple-array-p)
                    (neq current-predicate 'arrayp)
                    (csubtypep intersect (specifier-type 'simple-array))
                    (not (types-equal-or-intersect (type-difference otype type)
                                                   (specifier-type 'simple-array))))
               `(simple-array-p object))
              ((and (eq current-predicate 'vectorp)
                    (vop-existsp :translate %array-rank=)
                    (and (csubtypep otype (specifier-type 'array))))
               `(%array-rank= object 1))
              ;; (typep (the (or list fixnum) x) 'integer) =>
              ;; (typep x 'fixnum)
              ((let ((new-predicate
                       (or
                        (backend-type-predicate intersect)
                        ;; Remove bounds from numeric types
                        (and (csubtypep intersect (specifier-type 'real))
                             (macrolet ((up (&rest types)
                                          `(cond ,@(loop for (type predicate) on types by #'cddr
                                                         collect
                                                         `((and (csubtypep intersect (specifier-type ',type))
                                                                (csubtypep (specifier-type ',type) type))
                                                           ',predicate)))))
                               (up fixnum fixnump
                                   integer integerp
                                   rational rationalp
                                   single-float single-float-p
                                   double-float double-float-p
                                   float floatp
                                   real realp))))))
                 (when (and new-predicate
                            (neq new-predicate current-predicate)
                            ;; Some subtypes are more expensive to check
                            (not (and (eq current-predicate 'listp)
                                      (eq new-predicate 'consp)))
                            (not (and (eq current-predicate 'functionp)
                                      (eq new-predicate 'compiled-function-p)))
                            (not (eq current-predicate 'characterp))
                            (not (eq current-predicate 'arrayp))
                            (not (eq current-predicate 'simple-array-p))
                            (not (and (eq current-predicate 'non-null-symbol-p)
                                      (eq new-predicate 'keywordp)))
                            (not (eq new-predicate #+64-bit 'signed-byte-64-p
                                                   #-64-bit 'signed-byte-32-p))
                            (not (eq new-predicate #+64-bit 'unsigned-byte-64-p
                                                   #-64-bit 'unsigned-byte-32-p)))
                   `(,new-predicate object))))
              ;; (typep (the float x) 'double-float) =>
              ;; (typep x 'single-float)
              ((let* ((diff (type-difference otype type))
                      (pred (and (or (eq current-predicate 'sequencep) ;; always expensive
                                     (not (memory-type-test-p diff)))
                                 (or (backend-type-predicate diff)
                                     ;; Remove bounds from numeric types
                                     (and (csubtypep diff (specifier-type 'real))
                                          (macrolet ((up (&rest types)
                                                       `(cond ,@(loop for (type predicate) on types by #'cddr
                                                                      collect
                                                                      `((and (csubtypep diff (specifier-type ',type))
                                                                             (not (types-equal-or-intersect type (specifier-type ',type))))
                                                                        ',predicate)))))
                                            (up fixnum fixnump
                                                integer integerp
                                                rational rationalp
                                                single-float single-float-p
                                                double-float double-float-p
                                                float floatp
                                                real realp)))))))
                 (cond ((and pred
                             ;; Testing for fixnum is usually the cheapest
                             (or (eq pred 'fixnump)
                                 (memory-type-test-p type)))
                        `(not (,pred object)))
                       ((and (memory-type-test-p type)
                             (cond ((and (type-not-other-pointer-p diff)
                                         (type-other-pointer-p type))
                                    `(%other-pointer-p object))
                                   ((and (type-other-pointer-p diff)
                                         (type-not-other-pointer-p type))
                                    `(not (%other-pointer-p object)))))))))
              (t
               (give-up-ir1-transform)))))))

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
         (error "Class not yet defined: ~S" name))))

(defoptimizer (%typep-wrapper constraint-propagate-if) ((test-value variable type) node)
  (aver (constant-lvar-p type))
  (let* ((type (lvar-value type))
         (ctype (if (ctype-p type)
                    type
                    (handler-case (careful-specifier-type type)
                      (t () nil)))))
    (if (and ctype (type-for-constraints-p ctype))
        (values variable ctype))))

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

(deftransform %type-constraint ((x type) * * :node node)
  (delay-ir1-transform node :constraint)
  nil)

(defoptimizer (%type-constraint constraint-propagate) ((x type) node gen)
  (let ((var (ok-lvar-lambda-var x gen)))
    (when var
      (let ((type (lvar-value type)))
        (list (list 'typep var
                    (if (ctype-p type)
                        type
                        (handler-case (careful-specifier-type type)
                          (t () nil)))
                    nil))))))

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
  #+(and sb-unicode (or x86-64 arm64)) ;; others have a source-transform
  (define-type-predicate base-char-p base-char)
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

#+(and sb-unicode (not (or x86-64 arm64)))
(define-source-transform base-char-p (x)
  `(typep ,x 'base-char))
;; CONS is implemented as (and list (not (eql nil))) where the 'and' is
;; built-in to the consp vop. Reduce to just LISTP if possible.
(deftransform consp ((x) ((not null)) * :important nil)
  '(listp x))

;;; If X is known non-nil, then testing SYMBOLP can skip the "= NIL" part.
(deftransform symbolp ((x) ((not null)) * :important nil)
  '(non-null-symbol-p x))
(deftransform non-null-symbol-p ((object) (symbol) * :important nil)
  `(not (eq object nil)))
;;; CLHS: http://www.lispworks.com/documentation/HyperSpec/Body/t_symbol.htm#symbol
;;;   "The consequences are undefined if an attempt is made to alter the home package
;;;    of a symbol external in the COMMON-LISP package or the KEYWORD package."
;;; Therefore, we can constant-fold if the symbol-package is one of those two.
;;; Interestingly, we don't need any transform for (NOT SYMBOL)
;;; because IR1-TRANSFORM-TYPE-PREDICATE knows that the intersection of the type
;;; implied by KEYWORDP with any type that does not intersect SYMBOL is NIL.
(deftransform keywordp ((x) ((constant-arg symbol)))
  (let ((pkg (sb-xc:symbol-package (lvar-value x))))
    (cond ((eq pkg *cl-package*) 'nil)
          ((eq pkg *keyword-package*) 't)
          (t (give-up-ir1-transform)))))

;;;; TYPEP source transform

;;; Return a form that tests the variable N-OBJECT for being in the
;;; binds specified by TYPE. BASE is the name of the base type, for
;;; declaration.
(defun transform-numeric-bound-test (n-object type base)
  (declare (type numeric-type type))
  (let ((low (numeric-type-low type))
        (high (numeric-type-high type)))
    `(and ,@(when low
              (if (consp low)
                  `((> (truly-the ,base ,n-object) ,(car low)))
                  `((>= (truly-the ,base ,n-object) ,low))))
          ,@(when high
              (if (consp high)
                  `((< (truly-the ,base ,n-object) ,(car high)))
                  `((<= (truly-the ,base ,n-object) ,high)))))))

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
                 ((nil) 'real)))
         (low (numeric-type-low type))
         (high (numeric-type-high type)))
    (ecase (numeric-type-complexp type)
      (:real
       (cond ((and (vop-existsp :translate check-range<=)
                   (eql (numeric-type-class type) 'integer)
                   (fixnump low)
                   (fixnump high))
              `(check-range<= ,low ,object ,high))
             ((type= type (specifier-type '(or word sb-vm:signed-word)))
              `(or (typep ,object 'sb-vm:signed-word)
                   (typep ,object 'word)))
             ((and (vop-existsp :translate unsigned-byte-x-p)
                   (eql (numeric-type-class type) 'integer)
                   (eql low 0)
                   (integerp high)
                   (= (logcount (1+ high)) 1)
                   (zerop (rem (integer-length high) sb-vm:n-word-bits)))
              `(unsigned-byte-x-p ,object ,(integer-length high)))
             (t
              `(and (typep ,object ',base)
                    ,(transform-numeric-bound-test object type base)))))
      (:complex
       (let ((part-type (second (type-specifier type))))
         `(and (typep ,object '(complex ,(case base
                                           ((double-float single-float rational) base)
                                           (t (if (eq class 'integer)
                                                  'rational
                                                  '*)))))
               (typep (realpart ,object) ',part-type)
               (typep (imagpart ,object) ',part-type)))))))

;;; Do the source transformation for a test of a hairy type.
;;; SATISFIES is converted into the obvious. Otherwise, we convert
;;; to CACHED-TYPEP an possibly print an efficiency note.
(defun source-transform-hairy-typep (object type)
  (declare (type hairy-type type))
  (let ((spec (hairy-type-specifier type)))
    (cond ((and (unknown-type-p type)
                (symbolp spec)
                (eq (info :type :kind spec) :forthcoming-defclass-type))
           ;; Knowing that it was DEFCLASSed is enough to emit a CLASSOID-CELL-TYPEP test.
           ;; Combinators involving this - e.g. (OR A-NEW-CLASS OTHER-CLASS) -
           ;; are handled correctly, because we don't punt on everything in the expression
           ;; as soon as any unknown is present.
           `(classoid-cell-typep ,(find-classoid-cell spec :create t) ,object))
          ((unknown-type-p type)
           `(let ((object ,object)
                  (cache (load-time-value (cons #'sb-kernel::cached-typep ',spec)
                                          t)))
              (truly-the (values t &optional)
                         (funcall (truly-the function (car (truly-the cons cache)))
                                  cache object))))
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
                     t nil))))))))

(defun source-transform-negation-typep (object type)
  (declare (type negation-type type))
  (let ((spec (type-specifier (negation-type-type type))))
    `(not (typep ,object ',spec))))

;;; Check the type of a group of equally specialized but of
;;; different length simple arrays once
(defun group-vector-type-length-tests (object types)
  (let (groups
        any-grouped)
    (loop for type in types
          do
          (if (and (array-type-p type)
                   (not (array-type-complexp type))
                   (typep (array-type-dimensions type) '(cons integer null))
                   (or (eq (array-type-element-type type) *wild-type*)
                       (neq (array-type-specialized-element-type type) *wild-type*)))
              (push type
                    (getf groups
                          (array-type-specialized-element-type type)))
              (push type (getf groups :other))))
    (loop for (el-type types) on groups by #'cddr
          do
          (cond ((eq el-type :other))
                ((> (length types) 1)
                 (setf any-grouped t))
                (t
                 (push (car types)
                       (getf groups :other)))))
    (when any-grouped
      (let ((other (getf groups :other)))
        `(or
          ,@(loop for (el-type types) on groups by #'cddr
                  when (and (neq el-type :other)
                            (> (length types) 1))
                  collect `(and (typep ,object
                                       '(simple-array ,(type-specifier el-type) (*)))
                                (typep (vector-length
                                        (truly-the (simple-array * (*)) ,object))
                                       '(member ,@(loop for type in types
                                                        collect (car (array-type-dimensions type)))))))
          ,@(and
             other
             `((typep ,object '(or ,@(mapcar #'type-specifier other))))))))))

;;; Test the length of multiple arrays types once
(defun group-vector-length-type-tests (object types)
  (let (groups
        any-grouped)
    (loop for type in types
          do
          (if (and (array-type-p type)
                   (typep (array-type-dimensions type) '(cons integer null)))
              (push type (getf groups (car (array-type-dimensions type))))
              (push type (getf groups :other))))
    (loop for (length types) on groups by #'cddr
          do
          (cond ((eq length :other))
                ((> (length types) 1)
                 (setf any-grouped t))
                (t
                 (push (car types)
                       (getf groups :other)))))
    (when any-grouped
      (let ((other (getf groups :other)))
        `(or
          ,@(loop for (length types) on groups by #'cddr
                  for any-complex = nil
                  for any-simple = nil
                  when (and (neq length :other)
                            (> (length types) 1))
                  collect `(and (typep ,object
                                       '(or
                                         ,@(loop for type in types
                                                 for complex = (array-type-complexp type)
                                                 do (cond (complex
                                                           (setf any-complex t)
                                                           (when (eq complex :maybe)
                                                             (setf any-simple t)))
                                                          (t
                                                           (setf any-simple t)))

                                                 collect
                                                 (type-specifier
                                                  (make-array-type '(*)
                                                                   :complexp complex
                                                                   :element-type
                                                                   (array-type-element-type type)
                                                                   :specialized-element-type
                                                                   (array-type-specialized-element-type type))))))
                                ,(cond
                                   ((not any-complex)
                                    `(= (vector-length (truly-the (simple-array * (*)) ,object))
                                        ,length))
                                   ((not any-simple)
                                    `(= (%array-dimension (truly-the vector ,object) 0)
                                        ,length))
                                   (t
                                    `(if (array-header-p (truly-the vector ,object))
                                         (= (%array-dimension (truly-the vector ,object) 0)
                                            ,length)
                                         (= (vector-length (truly-the vector ,object))
                                            ,length))))))
          ,@(and
             other
             `((typep ,object '(or ,@(mapcar #'type-specifier other))))))))))

(defun source-transform-union-numeric-typep (object types)
  (cond ((and (= (length types) 2)
              ;; Transform (or (double-float * (0d0)) (eql -0d0))
              (destructuring-bind (a b) types
                (multiple-value-bind (member numeric) (cond ((member-type-p a)
                                                             (values a b))
                                                            ((member-type-p b)
                                                             (values b a)))
                  (let ((double))
                    (and (numeric-type-p numeric)
                         (= (member-type-size member) 1)
                         (or (setf double (sb-kernel::member-type-member-p -0d0 member))
                             (sb-kernel::member-type-member-p -0f0 member))
                         (let ((low (numeric-type-low numeric))
                               (high (numeric-type-high numeric))
                               (type (if double
                                         'double-float
                                         'single-float)))
                           (when (and (eq (numeric-type-class numeric) 'float)
                                      (eq (numeric-type-complexp numeric) :real)
                                      (equal high (if double
                                                      '(0d0)
                                                      '(0f0))))
                             `(and ,(if double
                                        `(double-float-p ,object)
                                        `(single-float-p ,object))
                                   ,(if low
                                        `(and (float-sign-bit-set-p (truly-the ,type ,object))
                                              (,@(if (consp low)
                                                     `(< ,(car low))
                                                     `(<= ,low))
                                               (truly-the ,type ,object)))
                                        `(float-sign-bit-set-p (truly-the ,type ,object))))))))))))
        ((not (every #'numeric-type-p types))
         nil)
        ((and (= (length types) 2)
              ;; (and subtype-of-integer (not (eql x)))
              ;; don't test a range.
              ;; (and subtype-of-integer (not (integer x y)))
              (destructuring-bind (b a) types
                (and (integer-type-p a)
                     (integer-type-p b)
                     (flet ((check (a b)
                              (let* ((a-hi (numeric-type-high a))
                                     (a-lo (numeric-type-low a))
                                     (b-hi (numeric-type-high b))
                                     (b-lo (numeric-type-low b)))
                                (when (and a-hi b-lo
                                           (not (eql a-lo a-hi))
                                           (not (eql b-lo b-hi))
                                           (> b-lo a-hi))
                                  (let* (typecheck
                                         (a
                                           (%source-transform-typep object
                                                                    `(integer ,(or a-lo '*) ,(or b-hi '*))))
                                         (b `(not
                                              ,(cond ((= (1+ a-hi)
                                                         (1- b-lo))
                                                      `(eql ,object ,(1+ a-hi)))
                                                     (t
                                                      (setf typecheck t)
                                                      (%source-transform-typep object
                                                                               `(integer (,a-hi) (,b-lo))))))))
                                    (if typecheck
                                        `(and ,a ,b)
                                        `(and ,b ,a)))))))
                       (or (check a b)
                           (check b a)))))))
        ((and (= (length types) 2)
              ;; (or (integer * fixnum-x) (integer fixnum-y))
              ;; only check for bignump and not its value.
              (destructuring-bind (b a) types
                (and (integer-type-p a)
                     (integer-type-p b)
                     (flet ((check (a b)
                              (let* ((a-hi (numeric-type-high a))
                                     (a-lo (numeric-type-low a))
                                     (b-hi (numeric-type-high b))
                                     (b-lo (numeric-type-low b)))
                                (when (and (fixnump a-hi)
                                           (fixnump b-lo)
                                           (not a-lo)
                                           (not b-hi))
                                  `(or (and (fixnump ,object)
                                            (or (>= ,object ,b-lo)
                                                (<= ,object ,a-hi)))
                                       (bignump ,object))))))
                       (or (check a b)
                           (check b a)))))))))

;;; Do source transformation for TYPEP of a known union type. If a
;;; union type contains LIST, then we pull that out and make it into a
;;; single LISTP call.
(defun source-transform-union-typep (object type)
  (let* ((types (union-type-types type))
         (type-cons (specifier-type 'cons))
         (type-symbol (specifier-type 'symbol))
         (mtype (find-if #'member-type-p types))
         (members (when mtype (member-type-members mtype))))
    (cond ((and mtype
                (memq nil members)
                (memq type-cons types))
           `(or (listp ,object)
                (typep ,object
                       '(or ,@(mapcar #'type-specifier
                               (remove type-cons
                                (remove mtype types)))
                         (member ,@(remove nil members))))))
          ((and (memq type-cons types)
                (memq type-symbol types))
           `(or (listp ,object)
                (non-null-symbol-p ,object)
                (typep ,object
                       '(or ,@(mapcar #'type-specifier
                               (remove type-cons
                                (remove type-symbol types)))))))
          ((group-vector-type-length-tests object types))
          ((group-vector-length-type-tests object types))
          ((source-transform-union-numeric-typep object types))
          (t
           (multiple-value-bind (widetags more-types)
               (sb-kernel::widetags-from-union-type types)
             (multiple-value-bind (predicate more-union-types)
                 (split-union-type-tests type)
               (cond ((and predicate
                           (< (length more-union-types)
                              (length more-types)))
                      `(or (,predicate ,object)
                           (typep ,object '(or ,@(mapcar #'type-specifier more-union-types)))))
                     (widetags
                      `(or (%other-pointer-subtype-p ,object ',widetags)
                           (typep ,object '(or ,@(mapcar #'type-specifier more-types)))))
                     ((and (cdr more-types)
                           (every #'intersection-type-p more-types)
                           (let ((common (intersection-type-types (car more-types))))
                             (loop for type in (cdr more-types)
                                   for types = (intersection-type-types type)
                                   for int = (intersection common types :test #'type=)
                                   always int
                                   do (setf common int)
                                   finally
                                   (return `(and
                                             (typep ,object '(and ,@(mapcar #'type-specifier common)))
                                             (or ,@(loop for type in more-types
                                                         for types = (intersection-type-types type)
                                                         collect
                                                         `(typep ,object '(and ,@(mapcar #'type-specifier
                                                                                  (set-difference types common))))))))))))
                     (t
                      `(or
                        ,@(mapcar (lambda (x)
                                    `(typep ,object ',(type-specifier x)))
                                  more-types))))))))))

(defun source-transform-intersection-typep (object type)
  (let (types
        negated)
    ;; Group negated types into a union type which might be better
    ;; handled by source-transform-union-typep above.
    (loop for type in (intersection-type-types type)
          do
          (cond ((hairy-type-p type)
                 ;; These might impose some sort of an order.
                 (setf negated nil)
                 (return))
                ((typep type 'negation-type)
                 (push (negation-type-type type) negated))
                (t
                 (push type types))))
    (cond (negated
           `(and ,@(and types
                        `((typep ,object
                                 '(and ,@(mapcar #'type-specifier types)))))
                 (not
                  (typep ,object
                         '(or ,@(mapcar #'type-specifier negated))))))
          (t
           `(and ,@(mapcar (lambda (x)
                             `(typep ,object ',(type-specifier x)))
                           (intersection-type-types type)))))))

;;; If necessary recurse to check the cons type.
(defun source-transform-cons-typep
    (object type &aux (car-type (cons-type-car-type type))
                      (cdr-type (cons-type-cdr-type type))
                      (car-test-p (not (type= car-type *universal-type*)))
                      (cdr-test-p (not (type= cdr-type *universal-type*))))
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
     (cond
       ((and (not car-test-p) (not cdr-test-p))
        `(consp ,object))
       ((and (not cdr-test-p)
             (member-type-p car-type)
             (vop-existsp :translate car-eq-if-listp)
             (type-singleton-p car-type)
             (typep (first (member-type-members car-type)) '(and symbol (not null))))
        `(car-eq-if-listp ,object ',(first (member-type-members car-type))))
       (t
        (let ((car-test
               (and car-test-p
                    `((typep (car ,object) ',(type-specifier car-type)))))
              (cdr-test
               (and cdr-test-p
                    `((typep (cdr ,object) ',(type-specifier cdr-type))))))
          ;; Being paranoid, perform the safely weakenable test first
          ;; so that the other part doesn't execute on an object that
          ;; it would not have gotten, were the CONSP test not weakened.
          (cond ((and car-test-p (safely-weakened car-type))
                 `(and (listp ,object) ,@car-test ,@cdr-test))
                ((and cdr-test-p (safely-weakened cdr-type))
                 `(and (listp ,object) ,@cdr-test ,@car-test))
                (t
                 `(and (consp ,object) ,@car-test ,@cdr-test))))))))

(defun source-transform-character-set-typep (object type)
  (let ((pairs (character-set-type-pairs type)))
    (or (and (= (length pairs) 1)
             (= (caar pairs) 0)
             (cond
               #+(and sb-unicode (or x86-64 arm64))
               ((= (cdar pairs) (1- base-char-code-limit))
                `(base-char-p ,object))
               ((= (cdar pairs) (1- char-code-limit))
                `(characterp ,object))))
        (let ((n-code (gensym "CODE")))
          `(and (characterp ,object)
                (let ((,n-code (char-code ,object)))
                  (or
                   ,@(loop for pair in pairs
                           collect
                           `(<= ,(car pair) ,n-code ,(cdr pair))))))))))

#+sb-simd-pack
(defun source-transform-simd-pack-typep (object type)
  (let ((mask (simd-pack-type-tag-mask type)))
    (if (= mask sb-kernel::+simd-pack-wild+)
      `(simd-pack-p ,object)
      `(and (simd-pack-p ,object)
            ,(if (= (logcount mask) 1)
                 `(eql (%simd-pack-tag ,object) ,(sb-vm::simd-pack-mask->tag mask))
                 `(logbitp (%simd-pack-tag ,object) ,mask))))))

#+sb-simd-pack-256
(defun source-transform-simd-pack-256-typep (object type)
  (let ((mask (simd-pack-256-type-tag-mask type)))
    (if (= mask sb-kernel::+simd-pack-wild+)
        `(simd-pack-256-p ,object)
        `(and (simd-pack-256-p ,object)
              ,(if (= (logcount mask) 1)
                   `(eql (%simd-pack-256-tag ,object) ,(sb-vm::simd-pack-mask->tag mask))
                   `(logbitp (%simd-pack-256-tag ,object) ,mask))))))

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
(defun test-array-dimensions (original-obj type stype
                              simple-array-header-p)
  (declare (type array-type type stype))
  (let ((obj `(truly-the ,(type-specifier stype) ,original-obj))
        (dims (array-type-dimensions type))
        (header-test (if simple-array-header-p
                         `(simple-array-header-p ,original-obj)
                         `(array-header-p ,original-obj))))
    (unless (or (eq dims '*)
                (equal dims (array-type-dimensions stype)))
      (cond ((cdr dims)
             (values `(,@(if (and simple-array-header-p
                                  (vop-existsp :translate simple-array-header-of-rank-p)
                                  (eq (array-type-dimensions stype) '*))
                             `((simple-array-header-of-rank-p ,original-obj ,(length dims)))
                             `(,header-test
                               ,@(when (eq (array-type-dimensions stype) '*)
                                   (if (vop-existsp :translate %array-rank=)
                                       `((%array-rank= ,obj ,(length dims)))
                                       `((= (%array-rank ,obj) ,(length dims)))))))
                       ,@(loop for d in dims
                               for i from 0
                               unless (eq '* d)
                               collect `(= (%array-dimension ,obj ,i) ,d)))
                     t))
            ((not dims)
             (values `(,header-test
                       (= (%array-rank ,obj) 0))
                     t))
            ((not (array-type-complexp type))
             (if (csubtypep stype (specifier-type 'vector))
                 (values (unless (eq '* (car dims))
                           `((= (vector-length ,obj) ,@dims)))
                         nil)
                 (values (if (eq '* (car dims))
                             `((not ,header-test))
                             `((not ,header-test)
                               (= (vector-length ,obj) ,@dims)))
                         nil)))
            (t
             (values (unless (eq '* (car dims))
                       `((if ,header-test
                             (= (%array-dimension ,obj 0) ,@dims)
                             (= (vector-length ,obj) ,@dims))))
                     nil
                     (car dims)))))))

;;; Return forms to test that OBJ has the element-type specified by type
;;; specified by TYPE, where STYPE is the type we have checked against (which
;;; is the same but for dimensions and element type). If HEADERP is true, OBJ
;;; is guaranteed to be an array-header.
(defun test-array-element-type (obj type stype headerp pred length)
  (declare (type array-type type stype))
  (let ((eltype (array-type-specialized-element-type type)))
    (unless (or (type= eltype (array-type-specialized-element-type stype))
                (eq eltype *wild-type*))
      (let* ((typecode (sb-vm:saetp-typecode (find-saetp-by-ctype eltype))))
        (cond ((and headerp (not (array-type-complexp stype)))
               (let ((obj `(truly-the ,(type-specifier stype) ,obj)))
                 ;; If we know OBJ is an array header, and that the array is
                 ;; simple, we also know there is exactly one indirection to
                 ;; follow.
                 `(#-x86-64
                   (eq (%other-pointer-widetag (%array-data ,obj)) ,typecode)
                   #+x86-64
                   (widetag= (%array-data ,obj) ,typecode))))
              ((not (array-type-complexp stype))
               (values
                `((and (%other-pointer-p ,obj)
                       (let ((widetag (%other-pointer-widetag ,obj)))
                         (or (eq widetag ,typecode)
                             (and (eq widetag sb-vm:simple-array-widetag)
                                  (eq (%other-pointer-widetag (%array-data ,obj)) ,typecode))))))
                ;; skip checking for array.
                t))
              (t
               (ecase pred
                 (arrayp
                  (values `((and (%other-pointer-p ,obj)
                                 (let ((data ,obj))
                                   (loop
                                    (let ((widetag (%other-pointer-widetag data)))
                                      (if (eq widetag ,typecode)
                                          (return t)
                                          (if (or (eq widetag sb-vm:simple-array-widetag)
                                                  (>= widetag sb-vm:complex-base-string-widetag))
                                              (setf data (%array-data data))
                                              (return nil))))))))
                          t))
                 (vectorp
                  (if length
                      (values `((and (%other-pointer-p ,obj)
                                     (let ((widetag (%other-pointer-widetag ,obj)))
                                       (if (eq widetag ,typecode)
                                           (= (vector-length (truly-the (simple-array * (*)) ,obj)) ,length)
                                           (and (= widetag sb-vm:complex-vector-widetag)
                                                (= (%array-dimension (truly-the (and (array * (*))
                                                                                     (not simple-array)) ,obj) 0)
                                                   ,length)
                                                (let ((data ,obj))
                                                  (loop
                                                   (setf data (%array-data data))
                                                   (let ((widetag (%other-pointer-widetag data)))
                                                     (if (eq widetag ,typecode)
                                                         (return t)
                                                         (unless (or (eq widetag sb-vm:simple-array-widetag)
                                                                     (>= widetag sb-vm:complex-vector-widetag))
                                                           (return nil)))))))))))
                              t
                              t)
                      (values `((and (%other-pointer-p ,obj)
                                     (let ((widetag (%other-pointer-widetag ,obj)))
                                       (if (eq widetag ,typecode)
                                           t
                                           (and (= widetag sb-vm:complex-vector-widetag)
                                                (let ((data ,obj))
                                                  (loop
                                                   (setf data (%array-data data))
                                                   (let ((widetag (%other-pointer-widetag data)))
                                                     (if (eq widetag ,typecode)
                                                         (return t)
                                                         (unless (or
                                                                  (eq widetag sb-vm:simple-array-widetag)
                                                                  (>= widetag sb-vm:complex-vector-widetag))
                                                           (return nil)))))))))))
                              t))))))))))

;;; If we can find a type predicate that tests for the type without
;;; dimensions, then use that predicate and test for dimensions.
;;; Otherwise, just do %TYPEP.
(defun source-transform-array-typep (object type)
  ;; Intercept (SIMPLE-ARRAY * (*)) because otherwise it tests
  ;; (AND SIMPLE-ARRAY (NOT ARRAY-HEADER)) to weed out rank 0 and >1.
  ;; By design the simple arrays of of rank 1 occupy a contiguous
  ;; range of widetags, and unlike the arbitrary-widetags code for unions,
  ;; this nonstandard predicate can be generically defined for all backends.
  (let ((dims (array-type-dimensions type))
        (et (array-type-element-type type)))
    (if (and (not (array-type-complexp type))
             (eq et *wild-type*)
             (equal dims '(*)))
        `(simple-rank-1-array-*-p ,object)
        (multiple-value-bind (pred stype) (find-supertype-predicate type)
          (if (and (array-type-p stype)
                   ;; (If the element type hasn't been defined yet, it's
                   ;; not safe to assume here that it will eventually
                   ;; have (UPGRADED-ARRAY-ELEMENT-TYPE type)=T, so punt.)
                   (not (unknown-type-p (array-type-element-type type)))
                   (or (eq (array-type-complexp stype) (array-type-complexp type))
                       (and (eql (array-type-complexp stype) :maybe)
                            (eql (array-type-complexp type) t))))
              (let ((complex-tag (and
                                  (eql (array-type-complexp type) t)
                                  (singleton-p dims)
                                  (and (neq et *wild-type*)
                                       (sb-vm:saetp-complex-typecode
                                        (find-saetp-by-ctype (array-type-element-type type))))))
                    (simple-array-header-p
                      (and (null (array-type-complexp stype))
                           (listp dims)
                           (cdr dims)))
                    (complexp (and (eql (array-type-complexp stype) :maybe)
                                   (eql (array-type-complexp type) t))))
                (if complex-tag
                    `(and (%other-pointer-p ,object)
                          (eq (%other-pointer-widetag ,object) ,complex-tag)
                          ,@(unless (eq (car dims) '*)
                              `((= (%array-dimension ,object 0) ,(car dims)))))
                    (multiple-value-bind (dim-tests headerp length)
                        (test-array-dimensions object type stype
                                               simple-array-header-p)
                      (multiple-value-bind (type-test no-check-for-array length-checked)
                          (test-array-element-type object type stype headerp pred length)
                        (if no-check-for-array
                            `(and ,@type-test
                                  ,@(unless length-checked
                                      dim-tests))
                            `(and
                              ,@(cond ((and (eql pred 'vectorp)
                                            complexp)
                                       `((%other-pointer-subtype-p ,object
                                                                   ',(list sb-vm:complex-base-string-widetag
                                                                           #+sb-unicode sb-vm:complex-character-string-widetag
                                                                           sb-vm:complex-bit-vector-widetag
                                                                           sb-vm:complex-vector-widetag))))
                                      ((and (eql pred 'arrayp)
                                            complexp)
                                       `((%other-pointer-subtype-p ,object
                                                                   ',(list sb-vm:complex-base-string-widetag
                                                                           #+sb-unicode sb-vm:complex-character-string-widetag
                                                                           sb-vm:complex-bit-vector-widetag
                                                                           sb-vm:complex-vector-widetag
                                                                           sb-vm:complex-array-widetag))))
                                      (t
                                       `(,@(unless (or (and headerp (eql pred 'arrayp))
                                                       simple-array-header-p)
                                             ;; ARRAY-HEADER-P from DIM-TESTS will test for that
                                             `((,pred ,object)))
                                         ,@(when complexp
                                             `((typep ,object '(not simple-array)))))))
                              ,@dim-tests
                              ,@type-test))))))
              `(%typep ,object ',(type-specifier type)))))))

;;; Transform a type test against some instance type. The type test is
;;; flushed if the result is known at compile time. If not properly
;;; named, error. If sealed and has no subclasses, just test for
;;; layout-EQ. If a structure then test for layout-EQ and then a
;;; general test based on layout-inherits. Otherwise, look up the indirect
;;; class-cell and call CLASS-CELL-TYPEP at runtime.
(deftransform %instance-typep ((object spec) * * :node node)
  (aver (constant-lvar-p spec))
  (let* ((spec (lvar-value spec))
         (class (specifier-type spec))
         (name (classoid-name class))
         (otype (lvar-type object)))
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
       (transform-instance-typep class)))))

;;; Notice that there are some instance types for which it is almost impossible
;;; to create. One such is SEQUENCE, viz: (make-instance 'sequence) =>
;;;   "Cannot allocate an instance of #<BUILT-IN-CLASS SEQUENCE>."
;;; We should not need to check for that, just the 'inherits' vector.
;;; However, bootstrap code does a sleazy thing, making an instance of
;;; the abstract base type which is impossible for user code to do.
;;;
;;; Preferably the prototype instance for SEQUENCE would be one that could
;;; exist, so it would be a STANDARD-OBJECT and SEQUENCE. But it's not.
;;; Hence we would have to check for a layout that no code using the documented
;;; sequence API would ever see, just to get the boundary case right.
;;; The for STREAM and FILE-STREAM.
;;; But there was precedent for builtin class prototype instances
;;; failing their type predicate, i.e. (TYPEP (CLASS-PROTOTYPE X) X) => NIL
;;; which was fixed in git rev d60a6d30.
;;; Also for what it's worth, some builtins use a prototype object that is strictly
;;; deeper than layout of the named class because it is indeed the case that no
;;; object's layout can ever be EQ to that of the ancestor.
;;; e.g. a fixnum as representative of class REAL.
;;; So in actual practice, you can't make something that is a pure STREAM, etc.

;;; TODOs:
;;; 1. There is an additional tweak that can potentially return false in one fewer
;;;    conditional branch if the layout being tested has depthoid 8 (or 9 if #+64-bit).
;;;    In that scenario, if the ID word of the candidate structure's layout does not
;;;    exist, then it's the 0th bitmap word and safe to read always. Therefore
;;;    STRUCTURE-IS-A and depthoid can be tested in that order. If there is no ID match,
;;;    there's no depthoid test. If there is an ID match, it's the same as before.
;;; 2. Since all backends implement STRUCTURE-IS-A, is there any reason that the
;;;    depthoid test is in the transform's expansion and not baked into that vop?
;;;    Putting it in the vop could be better for some backends,
;;;    and would eliminate the ad-hoc LAYOUT-DEPTHOID-GE vop.

#-(or x86 x86-64) ; vop-translated for these 2
(defmacro layout-depthoid-ge (layout depthoid)
  `(>= (layout-depthoid ,layout) ,depthoid))
(symbol-macrolet ((get-hash 'layout-clos-hash)
                  (get-flags 'layout-flags))
(defun transform-instance-typep (classoid)
  (binding*
      ((name (classoid-name classoid))
       (layout (let ((res (info :type :compiler-layout name)))
                 (when (and res (not (layout-invalid res))) res)))
       ((lowtag lowtag-test slot-reader)
        (cond ((csubtypep classoid (specifier-type 'funcallable-instance))
               (values sb-vm:fun-pointer-lowtag
                       '(function-with-layout-p object) '(%fun-layout object)))
              ((csubtypep classoid (specifier-type 'instance))
               (values sb-vm:instance-pointer-lowtag
                       '(%instancep object) '(%instance-layout object)))))
       (depthoid (if layout (layout-depthoid layout) -1))
       (type (make-symbol "TYPE")))
    (declare (ignorable layout))

    ;; Easiest case first: single bit test.
    (cond ((member name '(condition pathname structure-object))
           (let ((flag (case name
                         (condition +condition-layout-flag+)
                         (pathname  +pathname-layout-flag+)
                         (t         +structure-layout-flag+))))
            (if (vop-existsp :translate structure-typep)
                `(structure-typep object ,flag)
                `(and (%instancep object)
                      (logtest (,get-flags (%instance-layout object)) ,flag)))))

          ;; Next easiest: Sealed and no subtypes. Typically for DEFSTRUCT only.
          ;; Even if you don't seal a DEFCLASS, we're allowed to assume that things
          ;; won't change, as per CLHS 3.2.2.3 on Semantic Constraints:
          ;;  "Classes defined by defclass in the compilation environment must be defined
          ;;  at run time to have the same superclasses and same metaclass."
          ;; I think that means we should know the lowtag always. Nonetheless, this isn't
          ;; an important scenario, and only if you _do_ seal a class could this case be
          ;; reached; users rarely seal their classes since the standard doesn't say how.
          ((and layout
                (eq (classoid-state classoid) :sealed)
                (not (classoid-subclasses classoid)))
           (cond ((and (eq lowtag sb-vm:instance-pointer-lowtag)
                       (vop-existsp :translate structure-typep))
                  `(structure-typep object ,layout))
                 (lowtag-test
                  `(and ,lowtag-test
                        ,(if (vop-existsp :translate layout-eq)
                             `(layout-eq object ,layout ,lowtag)
                             `(eq ,slot-reader ,layout))))
                 (t
                  ;; `(eq ,layout
                  ;;      (if-vop-existsp (:translate %instanceoid-layout)
                  ;;        (%instanceoid-layout object)
                  ;;        ;; Slightly quicker than LAYOUT-OF. See also %PCL-INSTANCE-P
                  ;;        (cond ((%instancep object) (%instance-layout object))
                  ;;              ((funcallable-instance-p object) (%fun-layout object))
                  ;;              (t ,(find-layout 't)))))
                  (bug "Unexpected metatype for ~S" layout))))

          ;; All other structure types
          ((and (typep classoid 'structure-classoid) layout)
            ;; structure type tests; hierarchical layout depths
            (aver (eql lowtag sb-vm:instance-pointer-lowtag))
            ;; we used to check for invalid layouts here, but in fact that's both unnecessary and
            ;; wrong; it's unnecessary because structure classes can't be redefined, and it's wrong
            ;; because it is quite legitimate to pass an object with an invalid layout
            ;; to a structure type test.
           (if (vop-existsp :translate structure-typep)
               ;; A single VOP is easier to optimize later
               `(structure-typep object ,layout)
               `(and (%instancep object)
                     ,(if (<= depthoid sb-kernel::layout-id-vector-fixed-capacity)
                          `(%structure-is-a (%instance-layout object) ,layout)
                          `(let ((,type (%instance-layout object)))
                             (and (layout-depthoid-ge ,type ,depthoid)
                                  (%structure-is-a ,type ,layout)))))))

          ((> depthoid 0)
           ;; fixed-depth ancestors of non-structure types:
           ;; STREAM, FILE-STREAM, STRING-STREAM, and SEQUENCE.
            #+sb-xc-host (when (typep classoid 'static-classoid)
                           ;; should have use :SEALED code above
                           (bug "Non-frozen static classoids ~S" name))
            (let ((guts `((when (zerop (,get-hash ,type))
                            (setq ,type (update-object-layout object)))
                          ,(ecase name
                            (stream
                             `(logtest (,get-flags ,type) ,+stream-layout-flag+))
                            (file-stream
                             `(logtest (,get-flags ,type) ,+file-stream-layout-flag+))
                            (string-stream
                             `(logtest (,get-flags ,type) ,+string-stream-layout-flag+))
                            ;; Testing the type EXTENDED-SEQUENCE tests for #<LAYOUT of SEQUENCE>.
                            ;; It can only arise from a direct invocation of TRANSFORM-INSTANCE-TYPEP,
                            ;; because the lisp type is not a classoid. It's done this way to define
                            ;; the logic once only, instead of both here and src/code/pred.lisp.
                            (sequence
                             `(logtest (,get-flags ,type) ,+sequence-layout-flag+))))))
              (if lowtag-test
                  `(and ,lowtag-test (let ((,type ,slot-reader)) ,@guts))
                  (if-vop-existsp (:translate %instanceoid-layout)
                    `(let ((,type (%instanceoid-layout object))) ,@guts)
                    `(block typep
                       (let ((,type (cond ((%instancep object) (%instance-layout object))
                                          ((funcallable-instance-p object) (%fun-layout object))
                                          (t (return-from typep nil)))))
                         ,@guts))))))

          (t
            `(classoid-cell-typep ',(find-classoid-cell name :create t)
                                  object))))))

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
    (if ctype
        (or
         ;; It's purely a waste of compiler resources to wait for IR1 to
         ;; see these 2 edge cases that can be decided right now.
         (cond ((eq ctype *universal-type*) t)
               ((eq ctype *empty-type*) nil))
         (and (not (intersection-type-p ctype))
              (multiple-value-bind (constantp value) (type-singleton-p ctype)
                (and constantp
                     `(eql ,object ',value))))
         (handler-case
             (or
              (let ((pred (backend-type-predicate ctype)))
                (when pred `(,pred ,object)))
              (let* ((negated (type-negation ctype))
                     (pred (backend-type-predicate negated)))
                (cond (pred
                       `(not (,pred ,object)))
                      ((numeric-type-p negated)
                       `(not ,(%source-transform-typep object (type-specifier negated)))))))
           #+sb-xc-host
           (sb-kernel::cross-type-warning
             nil))
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
           #+sb-simd-pack
           (simd-pack-type
            (source-transform-simd-pack-typep object ctype))
           #+sb-simd-pack-256
           (simd-pack-256-type
            (source-transform-simd-pack-256-typep object ctype))
           (t nil))
         `(%typep ,object ',type))
        (values nil t))))

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

;;; These things will be removed by the tree shaker, so no #+ needed.
(defvar *interesting-types* nil)
(defun involves-alien-p (ctype)
  (sb-kernel::map-type
   (lambda (type)
     (when (alien-type-type-p type) (return-from involves-alien-p t)))
   ctype))
(defun dump/restore-interesting-types (op)
  (declare (ignorable op))
  #+collect-typep-regression-dataset
  (ecase op
   (write
    (when *interesting-types*
      (let ((list (sort (loop for k being each hash-key of *interesting-types* collect k)
                        #'string< :key #'write-to-string)))
        (with-open-file (f "interesting-types.lisp-expr" :direction :output
                         :if-exists :supersede  :if-does-not-exist :create)
          (let ((*package* #+sb-xc-host (find-package "XC-STRICT-CL")
                           #-sb-xc-host #.(find-package "SB-KERNEL"))
                (*print-pretty* nil)
                (*print-length* nil)
                (*print-level* nil)
                (*print-readably* t))
            (dolist (item list)
              (write (uncross item) :stream f)
              (terpri f)))))))
   (read
    (unless (hash-table-p *interesting-types*)
      (setq *interesting-types* (make-hash-table :test 'equal :synchronized t)))
    (with-open-file (f "interesting-types.lisp-expr" :if-does-not-exist nil)
      (when f
        (let ((*package* (find-package "SB-KERNEL")))
          (loop (let ((expr (read f nil f)))
                  (when (eq expr f) (return))
                  (format t "Read ~a~%" expr)
                  (setf (gethash expr *interesting-types*) t))))))
    *interesting-types*)))

(define-source-transform typep (object spec &optional env)
  ;; KLUDGE: It looks bad to only do this on explicitly quoted forms,
  ;; since that would overlook other kinds of constants. But it turns
  ;; out that the DEFTRANSFORM for TYPEP detects any constant
  ;; lvar, transforms it into a quoted form, and gives this
  ;; source transform another chance, so it all works out OK, in a
  ;; weird roundabout way. -- WHN 2001-03-18
  (if (and (not env)
           (typep spec '(cons (eql quote) (cons t null))))
      (with-current-source-form (spec)
        ;; Decline to do the source transform when seeing an unknown
        ;; type immediately while block converting, since it may be
        ;; defined later. By waiting for the deftransform to fire
        ;; during block compilation, we give ourselves a better chance
        ;; at open-coding the type test.
        (let ((type (cadr spec)))
          ;;
          #+collect-typep-regression-dataset
          (let ((parse (specifier-type type)))
            ;; alien types aren't externalizable as trees of symbols,
            ;; and some classoid types aren't defined at the start of warm build,
            ;; making it impossible to re-parse a dump produced late in the build.
            ;; Luckily there are no cases involving compund types and classoids.
            (unless (or (involves-alien-p parse)
                        (or (classoid-p parse)
                            (and (cons-type-p parse)
                                 (classoid-p (cons-type-car-type parse)))))
              (let ((table *interesting-types*))
                (unless (hash-table-p table)
                  (setq table (dump/restore-interesting-types 'read)))
                (setf (gethash type table) t))))
          ;;
          (if (and (block-compile *compilation*)
                   (contains-unknown-type-p (careful-specifier-type type)))
              (values nil t)
              (source-transform-typep object type))))
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
;;; Returns four values
;;; * vector ctype
;;; * upgraded-element ctype or requsted element
;;; * T if the upgraded-element is upgraded, i.e. it
;;;   does not contain any unknown types.
;;; * T if there were any dimensions
(defun simplify-vector-type (type)
  (labels ((process-compound-type (types)
             (let (array-types
                   element-types
                   (upgraded t)
                   dimensions-removed)
               (dolist (type types)
                 (unless (or (hairy-type-p type)
                             (sb-kernel::negation-type-p type))
                   (multiple-value-bind (type et upgraded dimensions) (simplify type)
                     (push type array-types)
                     (push et element-types)
                     (when dimensions
                       (setf dimensions-removed t))
                     (unless upgraded
                       (setf upgraded nil)))))
               (values (apply #'type-union array-types)
                       (if (member *wild-type* element-types)
                           *wild-type*
                           (apply #'type-union element-types))
                       upgraded
                       dimensions-removed)))
           (simplify (type)
             (cond ((and (array-type-p type)
                         (singleton-p (array-type-dimensions type)))
                    (let* ((upgraded t)
                           (et (array-type-specialized-element-type type))
                           (et (cond ((neq et *wild-type*)
                                      et)
                                     ((eq (array-type-element-type type) *wild-type*)
                                      et)
                                     (t
                                      (setf upgraded nil)
                                      (array-type-element-type type)))))
                      (values (specifier-type
                               (list (if (array-type-complexp type)
                                         'array
                                         'simple-array)
                                     (type-specifier et)
                                     '(*)))
                              et
                              upgraded
                              (not (eq (car (array-type-dimensions type)) '*)))))
                   ((union-type-p type)
                    (process-compound-type (union-type-types type)))
                   ((intersection-type-p type)
                    (process-compound-type (intersection-type-types type)))
                   ((member-type-p type)
                    (process-compound-type
                     (mapcar #'ctype-of (member-type-members type))))
                   (t
                    (error "~a is not a subtype of VECTOR." type)))))
    (simplify type)))

(defun strip-array-dimensions-and-complexity (type &optional simple)
  (labels ((process-compound-type (types)
             (let (array-types)
               (dolist (type types)
                 (unless (or (hairy-type-p type)
                             (sb-kernel::negation-type-p type))
                   (push (strip type) array-types)))
               (apply #'type-union array-types)))
           (strip (type)
             (cond ((array-type-p type)
                    (let ((dim (array-type-dimensions type)))
                      (make-array-type
                       (if (eq dim '*)
                           dim
                           (make-list (length dim)
                                      :initial-element '*))
                       :complexp (if simple
                                     nil
                                     :maybe)
                       :element-type (array-type-element-type type)
                       :specialized-element-type (array-type-specialized-element-type type))))
                   ((union-type-p type)
                    (process-compound-type (union-type-types type)))
                   ((intersection-type-p type)
                    (process-compound-type (intersection-type-types type)))
                   ((member-type-p type)
                    (process-compound-type
                     (mapcar #'ctype-of (member-type-members type))))
                   (t
                    (error "~a is not a subtype of ARRAY." type)))))
    (strip type)))

(defun check-coerce (value-type to-type type-specifier node)
  (flet ((fail ()
           (compiler-warn "Cannot coerce ~s to ~s"
                          (type-specifier value-type)
                          (type-specifier to-type))
           (setf (combination-kind node) :error)
           (give-up-ir1-transform)))
    (cond ((eq to-type *empty-type*)
           (fail))
          ((types-equal-or-intersect value-type to-type))
          ((csubtypep to-type (specifier-type 'sequence))
           (unless (csubtypep to-type (specifier-type 'sequence))
             (fail)))
          ((eql type-specifier 'character)
           (unless (types-equal-or-intersect value-type
                                             (specifier-type 'string))
             (fail)))
          ((csubtypep to-type (specifier-type 'complex))
           (unless (types-equal-or-intersect value-type
                                             (specifier-type 'number))
             (fail)))

          ((csubtypep to-type (specifier-type 'float))
           (unless (types-equal-or-intersect value-type
                                             (specifier-type 'real))
             (fail)))
          ((eq type-specifier 'function)
           (unless (types-equal-or-intersect value-type
                                             (specifier-type '(or symbol cons)))
             (fail)))
          (t
           (fail)))))

(deftransform coerce ((x type) * * :node node)
  (unless (constant-lvar-p type)
    (give-up-ir1-transform))
  (let* ((tval (lvar-value type))
         (tspec (ir1-transform-specifier-type tval))
         (value-type (lvar-type x)))
    (check-coerce value-type tspec tval node)
    ;; Note: The THE forms we use to wrap the results make sure that
    ;; specifiers like (SINGLE-FLOAT 0.0 1.0) can raise a TYPE-ERROR.
    (cond
      ((csubtypep value-type tspec)
       'x)
      ((csubtypep tspec (specifier-type 'double-float))
       `(the ,tval (%double-float x)))
      ((csubtypep tspec (specifier-type 'single-float))
       `(the ,tval (%single-float x)))
      ;; FIXME: #+long-float (t ,(error "LONG-FLOAT case needed"))
      ((csubtypep tspec (specifier-type 'float))
       (if (types-equal-or-intersect value-type (specifier-type 'float))
           `(the ,tval (if (floatp x)
                           x
                           (let ((r (the* (real :silent-conflict t) x)))
                             (declare (muffle-conditions code-deletion-note))
                             (sb-kernel:%single-float r))))
           `(the ,tval (%single-float x))))
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
              (t         ; X is COMPLEX, but not of the requested type
               ,(if (eq part-type 'rational)
                    ;; Can't coerce non-rational to a rational and
                    ;; CHECK-COERCE will warn, so just full call
                    ;; COERCE and let it signal an error.
                    `(locally (declare (notinline coerce))
                       (coerce x ',tval))
                    `(the ,result-type
                          (complex (coerce (realpart x) ',part-type)
                                   (coerce (imagpart x) ',part-type)))))))))
      ((eq tval 'character)
       `(character x))
      ;; Handle specialized element types for 1D arrays.
      ((multiple-value-bind (result already-type-p dimension specialization)
           (cond ((and (array-type-p tspec)
                       (neq (array-type-complexp tspec) t) ; :MAYBE and NIL are good
                       (not (contains-unknown-type-p (array-type-element-type tspec)))
                       ;; just for requesting (array nil (*)), you lose
                       (neq (array-type-specialized-element-type tspec) *empty-type*)
                       (consp (array-type-dimensions tspec)))
                  (values tspec
                          (source-transform-array-typep 'x tspec)
                          (car (array-type-dimensions tspec))
                          (let ((et (array-type-specialized-element-type tspec)))
                            (unless (or (eq et *universal-type*) ; don't need
                                        ;; * is illegal as :element-type; in this context
                                        ;; it means to produce a SIMPLE-VECTOR
                                        (eq et *wild-type*))
                              `(:element-type ',(type-specifier et))))))
                 ;; Check for string types.  This loses on (STRING 1) and such.
                 #+sb-unicode
                 ((type= tspec (specifier-type 'simple-string))
                  (values 'simple-string '(simple-string-p x) '* '(:element-type 'character)))
                 #+sb-unicode
                 ((type= tspec (specifier-type 'string))
                  (values 'string '(stringp x) '* '(:element-type 'character))))
         (when result
           ;; If the dimension is in the type, we check the input length if safety > 0,
           ;; though technically CLHS would allow not checking in safety < 3.
           ;; And if mismatch occurs in unsafe code, the results accords with the
           ;; specifier, NOT the dimension of the input. This is a rational choice
           ;; because one could not argue that incorrect code should have taken the
           ;; bad input's length when COERCE was asked for an exact type of output.
           `(truly-the ,result
               (if ,already-type-p
                   x
                   ,(cond ((eq dimension '*)
                           #+ubsan
                           ;; Passing :INITIAL-CONTENTS avoids allocating ubsan shadow bits,
                           ;; but redundantly checks the length of the input in MAKE-ARRAY's
                           ;; transform because we don't or can't infer that LENGTH gives the
                           ;; same answer each time it is called on X. There may be a way to
                           ;; extract more efficiency - at least eliminate the unreachable
                           ;; error-signaling code on mismatch - but I don't care to try.
                           `(make-array (length x) ,@specialization :initial-contents x)
                           #-ubsan ; better: do not generate a redundant LENGTH check
                           `(replace (make-array (length x) ,@specialization) x))
                          ((policy node (= safety 0)) ; Disregard the input length
                           `(replace (make-array ,dimension ,@specialization) x))
                          (t
                           `(make-array ,dimension ,@specialization :initial-contents x))))))))
      ((type= tspec (specifier-type 'list))
       `(coerce-to-list x))
      ((csubtypep tspec (specifier-type 'extended-sequence))
       (let ((class (and (symbolp tval) (find-class tval nil))))
         (if (null class)
             (give-up-ir1-transform)
             `(coerce-to-extended-sequence x (load-time-value (find-class ',tval) t)))))
      ((type= tspec (specifier-type 'function))
       (if (csubtypep (lvar-type x) (specifier-type 'symbol))
           `(coerce-symbol-to-fun x)
           ;; if X can later be derived as FUNCTION then we don't want
           ;; to call COERCE-TO-FUN, because there's no smartness
           ;; that can undo that and see that it's really (IDENTITY X).
           (progn (delay-ir1-transform node :constraint)
                  `(coerce-to-fun x))))
      ((multiple-value-bind (p really)
           (csubtypep tspec
                      (specifier-type '(or sequence character complex float function)))
         (and really
              (not p)))
       `(the* (,tspec :context coerce-context) x))
      (t
       (give-up-ir1-transform
        "~@<open coding coercion to ~S not implemented.~:@>"
        tval)))))

(deftransform #+64-bit unsigned-byte-64-p #-64-bit unsigned-byte-32-p
  ((value) (sb-vm:signed-word) * :important nil)
  `(>= value 0))

(when-vop-existsp (:translate unsigned-byte-x-p)
  (deftransform unsigned-byte-x-p
      ((value x) (t t) * :important nil :node node)
    (ir1-transform-type-predicate value (specifier-type `(unsigned-byte ,(lvar-value x))) node))

  (deftransform unsigned-byte-x-p
      ((value x) ((integer * #.most-positive-word) t) * :important nil)
    `(#+64-bit unsigned-byte-64-p #-64-bit unsigned-byte-32-p x)))

(deftransform %other-pointer-p ((object))
  (let ((type (lvar-type object)))
    (cond ((not (types-equal-or-intersect type (specifier-type 'other-pointer)))
           nil)
          ((or (csubtypep type (specifier-type 'other-pointer))
               ;; It doesn't negate to this type, so check both
               (csubtypep type (specifier-type '(not (or fixnum #+64-bit single-float
                                                                list function instance character)))))
           t)
          ((give-up-ir1-transform)))))

;;; BIGNUMP is simpler than INTEGERP, so if we can rule out FIXNUM then ...
(deftransform integerp ((x) ((not fixnum)) * :important nil) '(bignump x))

(deftransform structure-typep ((object type) (t t) * :node node)
  (if (types-equal-or-intersect (lvar-type object) (specifier-type 'instance))
      (give-up-ir1-transform)
      nil))

(deftransform structure-typep ((object type) (t (constant-arg t)))
  (let* ((layout (lvar-value type))
         (type (case layout
                 (#.+condition-layout-flag+ (specifier-type 'condition))
                 (#.+pathname-layout-flag+  (specifier-type 'pathname))
                 (#.+structure-layout-flag+ (specifier-type 'structure-object))
                 (t
                  (layout-classoid layout))))
         (diff (type-difference (lvar-type object) type))
         (pred (backend-type-predicate diff)))
    (cond ((not (types-equal-or-intersect (lvar-type object) type))
           nil)
          ((csubtypep (lvar-type object) type)
           t)
          (pred
           `(not (,pred object)))
          (t
           (give-up-ir1-transform)))))

(deftransform classoid-cell-typep ((cell object) ((constant-arg t) t))
  (let* ((type (specifier-type (classoid-cell-name (lvar-value cell))))
         (diff (type-difference (lvar-type object) type))
         (pred (backend-type-predicate diff)))
    (if pred
        `(not (,pred object))
        (give-up-ir1-transform))))

(when-vop-existsp (:translate signed-byte-8-p)
  (macrolet ((def (bits)
               `(deftransform ,(symbolicate "SIGNED-BYTE-" (princ-to-string bits) "-P")
                    ((x) (unsigned-byte) * :important nil)
                  '(typep x '(unsigned-byte ,(1- bits))))))
    (def 8)
    (def 16)
    #+64-bit
    (def 32)))

;;; source-transform-union-typep would generate the same thing but
;;; it's too complicated to be optimized later, hence the delay.
(deftransform string-designator-p ((x) * * :node node)
  (delay-ir1-transform node :constraint)
  `(or (%other-pointer-subtype-p x '(,sb-vm:symbol-widetag ,@sb-vm::+string-widetags+))
       (null (truly-the (not (or (and symbol (not null)) string)) x))
       (characterp (truly-the (not (or symbol string)) x))))
