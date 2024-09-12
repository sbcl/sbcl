;;;; array-specific optimizers and transforms

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;;; utilities for optimizing array operations

;;; Return UPGRADED-ARRAY-ELEMENT-TYPE for LVAR, or do
;;; GIVE-UP-IR1-TRANSFORM if the upgraded element type can't be
;;; determined.
(defun upgraded-element-type-specifier-or-give-up (lvar)
  (let ((element-type-specifier (upgraded-element-type-specifier lvar)))
    (if (eq element-type-specifier '*)
        (give-up-ir1-transform
         "upgraded array element type not known at compile time")
        element-type-specifier)))

(defun upgraded-element-type-specifier (lvar)
  (type-specifier (array-type-upgraded-element-type (lvar-type lvar))))

;;; Array access functions return an object from the array, hence its type is
;;; going to be the array upgraded element type. Secondary return value is the
;;; known supertype of the upgraded-array-element-type, if if the exact
;;; U-A-E-T is not known. (If it is NIL, the primary return value is as good
;;; as it gets.)
;;; FIXME: poorly named, it sounds like an accessor on an instance of ARRAY-TYPE,
;;; but unfortunately UPGRADED-ARRAY-ELEMENT-TYPE is a CL: symbol
;;; and %UPGRADED-ARRAY-ELEMENT-TYPE is already a thing as well.
;;; Perhaps ARRAY-IMPLIED-ELEMENT-TYPE would be less misleading?
(defun array-type-upgraded-element-type (type)
  (typecase type
    ;; Note that this IF mightn't be satisfied even if the runtime
    ;; value is known to be a subtype of some specialized ARRAY, because
    ;; we can have values declared e.g. (AND SIMPLE-VECTOR UNKNOWN-TYPE),
    ;; which are represented in the compiler as INTERSECTION-TYPE, not
    ;; array type.
    (array-type
     (values (array-type-specialized-element-type type) nil))
    ;; Deal with intersection types (bug #316078)
    (intersection-type
     (let ((intersection-types (intersection-type-types type))
           (element-type *wild-type*)
           (element-supertypes nil))
       (dolist (intersection-type intersection-types)
         (multiple-value-bind (cur-type cur-supertype)
             (array-type-upgraded-element-type intersection-type)
           ;; According to ANSI, an array may have only one specialized
           ;; element type - e.g. '(and (array foo) (array bar))
           ;; is not a valid type unless foo and bar upgrade to the
           ;; same element type.
           (cond
             ((eq cur-type *wild-type*)
              nil)
             ((eq element-type *wild-type*)
              (setf element-type cur-type))
             ((or (not (csubtypep cur-type element-type))
                  (not (csubtypep element-type cur-type)))
              ;; At least two different element types where given, the array
              ;; is valid iff they represent the same type.
              ;;
              ;; FIXME: TYPE-INTERSECTION already takes care of disjoint array
              ;; types, so I believe this code should be unreachable. Maybe
              ;; signal a warning / error instead?
              (setf element-type *empty-type*)))
           (push (or cur-supertype (type-*-to-t cur-type))
                 element-supertypes)))
       (values element-type
               (when (and (eq *wild-type* element-type) element-supertypes)
                 (apply #'type-intersection element-supertypes)))))
    (union-type
     (let ((union-types (union-type-types type))
           (element-type nil)
           (element-supertypes nil))
       (dolist (union-type union-types)
         (multiple-value-bind (cur-type cur-supertype)
             (array-type-upgraded-element-type union-type)
           (cond
             ((eq element-type *wild-type*)
              nil)
             ((eq element-type nil)
              (setf element-type cur-type))
             ((or (eq cur-type *wild-type*)
                  ;; If each of the two following tests fail, it is not
                  ;; possible to determine the element-type of the array
                  ;; because more than one kind of element-type was provided
                  ;; like in '(or (array foo) (array bar)) although a
                  ;; supertype (or foo bar) may be provided as the second
                  ;; returned value returned. See also the KLUDGE below.
                  (not (csubtypep cur-type element-type))
                  (not (csubtypep element-type cur-type)))
              (setf element-type *wild-type*)))
           (push (or cur-supertype (type-*-to-t cur-type))
                 element-supertypes)))
       (values element-type
               (when (eq *wild-type* element-type)
                 (apply #'type-union element-supertypes)))))
    (member-type
     ;; Convert member-type to an union-type.
     (array-type-upgraded-element-type
      (apply #'type-union (mapcar #'ctype-of (member-type-members type)))))
    (t
     ;; KLUDGE: there is no good answer here, but at least
     ;; *wild-type* won't cause HAIRY-DATA-VECTOR-{REF,SET} to be
     ;; erroneously optimized (see generic/vm-tran.lisp) -- CSR,
     ;; 2002-08-21
     (values *wild-type* nil))))

(defun type-array-element-type (type)
  (if (csubtypep type (specifier-type 'array))
      (multiple-value-bind (upgraded other)
          (array-type-upgraded-element-type type)
        (or other upgraded))
      *wild-type*))

(defun declared-array-element-type (type)
  (if (array-type-p type)
      (array-type-element-type type)
      *wild-type*))

;;; The ``new-value'' for array setters must fit in the array, and the
;;; return type is going to be the same as the new-value for SETF
;;; functions.
(defun assert-new-value-type (new-value array)
  (let ((type (lvar-type array)))
    (when (array-type-p type)
      (assert-lvar-type
       new-value
       (array-type-specialized-element-type type)
       (lexenv-policy (node-lexenv (lvar-dest new-value)))
       'aref-context)))
  (lvar-type new-value))

(defun supplied-and-true (arg)
  (and arg
       (constant-lvar-p arg)
       (lvar-value arg)
       t))

;;;; DERIVE-TYPE optimizers

(defun sequence-elements-type (sequence &optional key)
  (let ((constant (lvar-constant sequence))
        min
        max
        union)
    (or (when constant
          (if (and (arrayp (constant-value constant))
                   (not key))
              (derive-aref-type sequence)
              (or (getf (leaf-info constant) key)
                  (setf (getf (leaf-info constant) key)
                        (let ((sequence (constant-value constant)))
                          (when (proper-sequence-p sequence)
                            (loop for i below (length sequence)
                                  for elt* = (elt sequence i)
                                  for elt = (if key
                                                (handler-case (funcall key elt*)
                                                  (error ()
                                                    (return *universal-type*)))
                                                elt*)
                                  for type = (typecase elt ;; ctype-of gives too much detail
                                               (integer
                                                (if min
                                                    (setf min (min min elt)
                                                          max (max max elt))
                                                    (setf min elt
                                                          max elt))
                                                nil)
                                               (cons
                                                (specifier-type 'cons))
                                               (vector
                                                (specifier-type 'vector))
                                               (array
                                                (specifier-type 'array))
                                               (character
                                                (specifier-type 'character))
                                               (symbol
                                                (specifier-type 'symbol))
                                               (double-float
                                                (specifier-type 'double-float))
                                               (single-float
                                                (specifier-type 'single-float))
                                               (t (return)))
                                  do (when type
                                       (setf union
                                             (if union
                                                 (type-union union type)
                                                 type)))
                                  finally (return (if min
                                                      (let ((int (make-numeric-type :class 'integer :low min :high max)))
                                                        (if union
                                                            (type-union union int)
                                                            int))
                                                      union)))))))))
        (type-array-element-type (lvar-type sequence)))))

(defun derive-aref-type (array)
  (or (let ((constant (lvar-constant array))
            min
            max
            symbols
            union
            (conses t)
            any-conses
            (car-type *empty-type*)
            car-min car-max car-symbols
            (cdr-type *empty-type*)
            cdr-min cdr-max cdr-symbols)
        (block nil
          (when constant
            (or (getf (leaf-info constant) nil)
                (setf (getf (leaf-info constant) nil)
                      (let ((array (constant-value constant)))
                        (or
                         (and (zerop (array-total-size array))
                              *empty-type*)
                         #-sb-xc-host
                         (flet ((int-min-max (array min max)
                                  (declare (optimize (insert-array-bounds-checks 0)))
                                  (with-array-data ((array array) (start) (end))
                                    (let ((min min)
                                          (max max))
                                      (loop for i from start below end
                                            do
                                            (let ((elt (aref array i)))
                                              (when (> elt max)
                                                (setf max elt))
                                              (when (< elt min)
                                                (setf min elt))))
                                      (make-numeric-type :class 'integer :low min :high max)))))
                           (declare (inline int-min-max))
                           (macrolet ((test (type)
                                        (let ((ctype (specifier-type type)))
                                          `(and (typep array '(array ,type))
                                                (int-min-max (the (array ,type) array)
                                                             ,(numeric-type-high ctype)
                                                             ,(numeric-type-low ctype))))))
                             (cond
                               ((test word))
                               ((test sb-vm:signed-word))
                               ((test (unsigned-byte 8)))
                               ((test (signed-byte 8)))
                               ((test (unsigned-byte 16)))
                               ((test (signed-byte 16)))
                               #+64-bit
                               ((test (unsigned-byte 32)))
                               #+64-bit
                               ((test (signed-byte 32)))
                               ((test fixnum))
                               ((test bit))
                               ((csubtypep (array-type-specialized-element-type (leaf-type constant))
                                           (specifier-type '(or float complex base-char)))
                                (return)))))
                         (flet ((lower-type (elt min max set-min set-max symbols set-symbols
                                             give-up)
                                  (declare (ignorable symbols set-symbols))
                                  ;; ctype-of gives too much detail
                                  (typecase elt
                                    (integer
                                     (funcall set-min
                                              (if min
                                                  (min min elt)
                                                  elt))
                                     (funcall set-max
                                              (if max
                                                  (max max elt)
                                                  elt))
                                     nil)
                                    #+sb-xc-host
                                    (symbol
                                     (specifier-type 'symbol))
                                    #-sb-xc-host
                                    (symbol
                                     (unless symbols
                                       (setf symbols (alloc-xset)))
                                     (add-to-xset elt symbols)
                                     (funcall set-symbols symbols)
                                     nil)
                                    (cons
                                     (specifier-type 'cons))
                                    (vector
                                     (specifier-type 'vector))
                                    (array
                                     (specifier-type 'array))
                                    #+sb-unicode
                                    (base-char
                                     (specifier-type 'base-char))
                                    (character
                                     (specifier-type 'character))
                                    (double-float
                                     (specifier-type 'double-float))
                                    (single-float
                                     (specifier-type 'single-float))
                                    (t (funcall give-up)))))
                           (loop for i below (array-total-size array)
                                 for elt = (row-major-aref array i)
                                 for type = (cond ((and conses
                                                        (consp elt))
                                                   (block nil
                                                     (let ((type (lower-type (car elt) car-min car-max
                                                                             (lambda (new)
                                                                               (setf car-min new))
                                                                             (lambda (new)
                                                                               (setf car-max new))
                                                                             car-symbols
                                                                             (lambda (new)
                                                                               (setf car-symbols new))
                                                                             (lambda ()
                                                                               (setf conses nil)
                                                                               (return (specifier-type 'cons))))))
                                                       (when type
                                                         (setf car-type (type-union type car-type))))
                                                     (let ((type (lower-type (cdr elt) cdr-min cdr-max
                                                                             (lambda (new)
                                                                               (setf cdr-min new))
                                                                             (lambda (new)
                                                                               (setf cdr-max new))
                                                                             cdr-symbols
                                                                             (lambda (new)
                                                                               (setf cdr-symbols new))
                                                                             (lambda ()
                                                                               (setf conses nil)
                                                                               (return (specifier-type 'cons))))))
                                                       (when type
                                                         (setf cdr-type (type-union type cdr-type))))
                                                     (setf any-conses t)
                                                     nil))
                                                  (t
                                                   (lower-type elt min max
                                                               (lambda (new)
                                                                 (setf min new))
                                                               (lambda (new)
                                                                 (setf max new))
                                                               symbols
                                                               (lambda (new)
                                                                 (setf symbols new))
                                                               (lambda ()
                                                                 (return)))))
                                 do (when type
                                      (setf union
                                            (if union
                                                (type-union union type)
                                                type)))
                                 finally
                                 (flet ((result (union symbols min max)
                                          (when symbols
                                            (let ((symbols (make-member-type symbols nil)))
                                              (setf union (if union
                                                              (type-union union symbols)
                                                              symbols))))
                                          (if min
                                              (let ((int (make-numeric-type :class 'integer :low min :high max)))
                                                (if union
                                                    (type-union union int)
                                                    int))
                                              union)))
                                   (let ((union (result union symbols min max)))
                                     (return
                                       (if (and conses
                                                any-conses)
                                           (type-union (or union *empty-type*)
                                                       (sb-c::make-cons-type (result car-type car-symbols car-min car-max)
                                                                             (result cdr-type cdr-symbols cdr-min cdr-max)))
                                           union)))))))))))))
      (type-array-element-type (lvar-type array))))

(deftransform array-in-bounds-p ((array &rest subscripts))
  (block nil
    (flet ((give-up (&optional reason)
             (cond ((= (length subscripts) 1)
                    (return
                      `(lambda (array arg)
                         (and (typep arg '(and fixnum unsigned-byte))
                              (< arg (array-dimension array 0))))))
                   (t
                    (give-up-ir1-transform
                     (or reason
                         "~@<lower array bounds unknown or negative and upper bounds not ~
                         negative~:@>")))))
           (bound-known-p (x)
             (integerp x)))             ; might be NIL or *
      (let ((dimensions (catch-give-up-ir1-transform
                            ((array-type-dimensions-or-give-up
                              (lvar-conservative-type array))
                             args)
                          (give-up (car args)))))
        (when (eq '* dimensions)
          (give-up "array bounds unknown"))
        ;; shortcut for zero dimensions
        (when (some (lambda (dim)
                      (and (bound-known-p dim) (zerop dim)))
                    dimensions)
          (return nil))
        ;; we first collect the subscripts LVARs' bounds and see whether
        ;; we can already decide on the result of the optimization without
        ;; even taking a look at the dimensions.
        (flet ((subscript-bounds (subscript)
                 (let* ((type1 (lvar-type subscript))
                        (type2 (if (csubtypep type1 (specifier-type 'integer))
                                   (weaken-integer-type type1 :range-only t)
                                   (give-up)))
                        (low (if (integer-type-p type2)
                                 (numeric-type-low type2)
                                 (give-up)))
                        (high (numeric-type-high type2)))
                   (cond
                     ((and (or (not (bound-known-p low)) (minusp low))
                           (or (not (bound-known-p high)) (not (minusp high))))
                      ;; can't be sure about the lower bound and the upper bound
                      ;; does not give us a definite clue either.
                      (give-up))
                     ((and (bound-known-p high) (minusp high))
                      (return nil)) ; definitely below lower bound (zero).
                     (t
                      (cons low high))))))
          (let* ((subscripts-bounds (mapcar #'subscript-bounds subscripts))
                 (subscripts-lower-bound (mapcar #'car subscripts-bounds))
                 (subscripts-upper-bound (mapcar #'cdr subscripts-bounds))
                 (in-bounds 0))
            (mapcar (lambda (low high dim)
                      (cond
                        ;; first deal with infinite bounds
                        ((some (complement #'bound-known-p) (list low high dim))
                         (when (and (bound-known-p dim) (bound-known-p low) (<= dim low))
                           (return nil)))
                        ;; now we know all bounds
                        ((>= low dim)
                         (return nil))
                        ((< high dim)
                         (aver (not (minusp low)))
                         (incf in-bounds))
                        (t
                         (give-up))))
                    subscripts-lower-bound
                    subscripts-upper-bound
                    dimensions)
            (if (eql in-bounds (length dimensions))
                t
                (give-up))))))))

(defoptimizer (aref derive-type) ((array &rest subscripts))
  (derive-aref-type array))

(defoptimizer ((setf aref) derive-type) ((new-value array &rest subscripts))
  (assert-new-value-type new-value array))

(defoptimizers derive-type
    (hairy-data-vector-ref hairy-data-vector-ref/check-bounds
     data-vector-ref)
    ((array index))
  (derive-aref-type array))

#+(or x86 x86-64)
(defoptimizer (data-vector-ref-with-offset derive-type) ((array index offset))
  (derive-aref-type array))

(defoptimizer (vector-pop derive-type) ((array))
  (derive-aref-type array))

(defoptimizers derive-type
    (hairy-data-vector-set
     hairy-data-vector-set/check-bounds)
    ;; DATA-VECTOR-SET is never used for value, so it doesn't need a type deriver.
    ((array index new-value))
  (assert-new-value-type new-value array))

;;; Figure out the type of the data vector if we know the argument
;;; element type.
(defun derive-%with-array-data/mumble-type (array)
  (let ((atype (lvar-type array)))
    (cond ((array-type-p atype)
           (specifier-type
            `(simple-array ,(type-specifier
                             (array-type-specialized-element-type atype))
                           (*))))
          ((csubtypep atype (specifier-type 'string))
           (specifier-type 'simple-string)))))
(defoptimizer (%with-array-data derive-type) ((array start end))
  (derive-%with-array-data/mumble-type array))
(defoptimizer (%with-array-data/fp derive-type) ((array start end))
  (derive-%with-array-data/mumble-type array))

(defoptimizer (row-major-aref derive-type) ((array index))
  (derive-aref-type array))

(defoptimizer (%set-row-major-aref derive-type) ((array index new-value))
  (assert-new-value-type new-value array))

(defun check-array-dimensions (dims node)
  (or (typep dims 'index)
      (and (proper-list-p dims)
           (every (lambda (x)
                    (typep x 'index))
                  dims))
      (let ((*compiler-error-context* node))
        (setf (combination-kind node) :error)
        (compiler-warn "Bad array dimensions: ~s" dims))))

(defun derive-make-array-type (dims element-type adjustable
                               fill-pointer displaced-to
                               node)
  (let* ((simple (and (unsupplied-or-nil adjustable)
                      (unsupplied-or-nil displaced-to)
                      (unsupplied-or-nil fill-pointer)))
         (spec
           `(,(if simple 'simple-array 'array)
             ;; element-type is usually an LVAR or nil,
             ;; but MAKE-WEAK-VECTOR derive-type passes in 'T.
             ,(cond ((or (not element-type) (eq element-type 't))
                     t)
                    ((ctype-p element-type)
                     (type-specifier element-type))
                    ((constant-lvar-p element-type)
                     (let ((ctype (careful-specifier-type
                                   (lvar-value element-type))))
                       (cond
                         ((or (null ctype) (contains-unknown-type-p ctype)) '*)
                         (t (upgraded-array-element-type
                             (lvar-value element-type))))))
                    (t
                     '*))
             ,(cond ((constant-lvar-p dims)
                     (let* ((val (lvar-value dims))
                            (cdims (ensure-list val)))
                       (unless (check-array-dimensions val node)
                         (return-from derive-make-array-type))
                       (if simple
                           cdims
                           (length cdims))))
                    ((or (csubtypep (lvar-type dims)
                                    (specifier-type 'integer))
                         (supplied-and-true fill-pointer))
                     '(*))
                    (t
                     '*)))))
    (if (and (not simple)
             (or (supplied-and-true adjustable)
                 (supplied-and-true displaced-to)
                 (supplied-and-true fill-pointer)))
        (careful-specifier-type `(and ,spec (not simple-array)))
        (careful-specifier-type spec))))

(defoptimizer (make-array derive-type)
    ((dims &key element-type adjustable fill-pointer displaced-to
           &allow-other-keys)
     node)
  (derive-make-array-type dims element-type adjustable
                          fill-pointer displaced-to
                          node))

(defoptimizer (make-array-header* derive-type) ((&rest inits))
  (let* ((data-position
          #.(sb-vm:slot-offset
             (sb-vm::primitive-object-slot (sb-vm::primitive-object 'array)
                                           'sb-vm::data)))
         (data (nth data-position inits))
         (type (lvar-type data)))
    (when (array-type-p type)
      (make-array-type '* :element-type (array-type-element-type type)
                          :specialized-element-type (array-type-specialized-element-type type)))))

(defoptimizer (%make-array derive-type)
    ((dims widetag n-bits &key adjustable fill-pointer displaced-to
           &allow-other-keys)
     node)
  (let ((saetp (and (constant-lvar-p widetag)
                    (find (lvar-value widetag)
                          sb-vm:*specialized-array-element-type-properties*
                          :key #'sb-vm:saetp-typecode))))
    (derive-make-array-type dims (if saetp
                                     (sb-vm:saetp-ctype saetp)
                                     *wild-type*)
                            adjustable fill-pointer displaced-to
                            node)))


;;;; constructors

;;; Convert VECTOR into a MAKE-ARRAY.
(define-source-transform vector (&rest elements)
  `(make-array ,(length elements) :initial-contents (list ,@elements)))

;;; Convert it into a MAKE-ARRAY if the element-type is known at compile-time.
;;; Otherwise, don't. This prevents allocating memory for a million element
;;; array of things that are not characters, and then signaling an error.
(deftransform make-string ((length &key element-type initial-element))
  (let ((elt-ctype
         (cond ((not element-type) (specifier-type 'character))
               ((constant-lvar-p element-type)
                (ir1-transform-specifier-type (lvar-value element-type))))))
    (when (or (not elt-ctype)
              (eq elt-ctype *empty-type*) ; silly, don't do it
              (contains-unknown-type-p elt-ctype))
      (give-up-ir1-transform))
    (multiple-value-bind (subtypep certainp)
        (csubtypep elt-ctype (specifier-type 'character))
      (if (not certainp) (give-up-ir1-transform)) ; could be valid, don't know
      (if (not subtypep)
          (abort-ir1-transform "~S is not a valid :ELEMENT-TYPE for MAKE-STRING"
                               (lvar-value element-type))))
    `(the simple-string (make-array (the index length)
                         ,@(when initial-element '(:initial-element initial-element))
                         :element-type ',(type-specifier elt-ctype)))))

;; Traverse the :INTIAL-CONTENTS argument to an array constructor call,
;; changing the skeleton of the data to be constructed by calls to LIST
;; and wrapping some declarations around each array cell's constructor.
;; In general, if we fail to optimize out the materialization
;; of initial-contents as distinct from the array itself, we prefer VECTOR
;; over LIST due to the smaller overhead (except for <= 1 item).
;; If a macro is involved, expand it before traversing.
;; Known limitations:
;; - inline functions whose behavior is merely to call LIST don't work
;;   e.g. :INITIAL-CONTENTS (MY-LIST a b) ; where MY-LIST is inline
;;                                        ; and effectively just (LIST ...)
(defun rewrite-initial-contents (rank initial-contents env)
  ;; If FORM is constant to begin with, we don't want to pessimize it
  ;; by turning it into a non-literal. That would happen because when
  ;; optimizing `#(#(foo bar) #(,x ,y)) we convert the whole expression
  ;; into (VECTOR 'FOO 'BAR X Y), whereas in the unidimensional case
  ;; it never makes sense to turn #(FOO BAR) into (VECTOR 'FOO 'BAR).
  (when (or (and (= rank 1) (constantp initial-contents env))
            ;; If you inhibit inlining these - game over.
            (fun-lexically-notinline-p 'vector env)
            (fun-lexically-notinline-p 'list env)
            (fun-lexically-notinline-p 'list* env))
    (return-from rewrite-initial-contents (values nil nil)))
  (let ((dimensions (make-array rank :initial-element nil))
        (output))
    (named-let recurse ((form (handler-case (macroexpand initial-contents env)
                                (error ()
                                  (return-from rewrite-initial-contents))))
                        (axis 0))
      (flet ((make-list-ctor (tail &optional (prefix nil prefixp) &aux val)
               (when (and (constantp tail)
                          (or (proper-list-p (setq val (constant-form-value tail env)))
                              (and (vectorp val) (not prefixp))))
                 (setq form
                       (cons 'list
                             (append (butlast prefix)
                                     (map 'list (lambda (x) (list 'quote x)) val)))))))
        ;; Express quasiquotation using only LIST, not LIST*.
        ;; e.g. `(,A ,B X Y) -> (LIST* A B '(X Y)) -> (LIST A B 'X 'Y)
        (if (typep form '(cons (eql list*) list))
            (let* ((cdr (cdr form)) (last (last cdr)))
              (when (null (cdr last))
                (make-list-ctor (car last) cdr)))
            (make-list-ctor form)))
      (unless (and (typep form '(cons (member list vector)))
                   (do ((items (cdr form))
                        (length 0 (1+ length))
                        (fun (let ((axis (the (mod #.array-rank-limit) (1+ axis))))
                               (if (= axis rank)
                                   (lambda (item) (push item output))
                                   (lambda (item) (recurse item axis))))))
                       ;; FIXME: warn if the nesting is indisputably wrong
                       ;; such as `((,x ,x) (,x ,x ,x)).
                       ((atom items)
                        (and (null items)
                             (if (aref dimensions axis)
                                 (eql length (aref dimensions axis))
                                 (setf (aref dimensions axis) length))))
                     (declare (type index length))
                     (funcall fun (pop items))))
        (return-from rewrite-initial-contents (values nil nil))))
    (when (some #'null dimensions)
      ;; Unless it is the rightmost axis, a 0-length subsequence
      ;; causes a NIL dimension. Give up if that happens.
      (return-from rewrite-initial-contents (values nil nil)))
    (setq output (nreverse output))
    (values
     ;; If the unaltered INITIAL-CONTENTS were constant, then the flattened
     ;; form must be too. Turning it back to a self-evaluating object
     ;; is essential to avoid compile-time blow-up on huge vectors.
     (if (constantp initial-contents env)
         (map 'vector (lambda (x) (constant-form-value x env)) output)
         (let ((f (if (singleton-p output) 'list 'vector)))
           `(locally (declare (notinline ,f))
             (,f ,@(mapcar (lambda (x)
                             (cond ((and (symbolp x)
                                         (not (nth-value
                                               1 (macroexpand-1 x env))))
                                    x)
                                   ((constantp x env)
                                    `',(constant-form-value x env))
                                   (t
                                    `(locally (declare (inline ,f)) ,x))))
                           output)))))
     (coerce dimensions 'list))))

;;; Prevent open coding :INITIAL-CONTENTS arguments, so that we
;;; can pick them apart in the DEFTRANSFORMS.
;;; (MAKE-ARRAY (LIST dim ...)) for rank != 1 is transformed now.
;;; Waiting around to see if IR1 can deduce that the dims are of type LIST
;;; is ineffective, because by then it's too late to flatten the initial
;;; contents using the correct array rank.
;;; We explicitly avoid handling non-simple arrays (uni- or multi-dimensional)
;;; in this path, mainly due to complications in picking the right widetag.
(define-source-transform make-array (dims-form &rest rest &environment env
                                               &aux dims dims-constp)
  (cond ((and (constantp dims-form env)
              (proper-list-p (setq dims (constant-form-value dims-form env)))
              (not (singleton-p dims))
              (every (lambda (x) (typep x 'index)) dims))
         (setq dims-constp t))
        ((and (cond ((typep (setq dims (handler-case
                                           (macroexpand dims-form env)
                                         (error ()
                                           (return-from make-array (values nil t)))))
                            '(cons (eql list)))
                     (setq dims (cdr dims))
                     t)
                    ;; `(,X 2 1) -> (LIST* X '(2 1)) for example
                    ((typep dims '(cons (eql list*) cons))
                     (let ((last (car (last dims))))
                       (when (constantp last env)
                         (let ((lastval (constant-form-value last env)))
                           (when (listp lastval)
                             (setq dims (append (butlast (cdr dims))
                                                (loop for v in  lastval
                                                      collect `(quote ,v))))
                             t))))))
              (proper-list-p dims)
              (not (singleton-p dims)))
         ;; If you spell '(2 2) as (LIST 2 2), it is constant for purposes of MAKE-ARRAY.
         (when (every (lambda (x) (constantp x env)) dims)
           (let ((values (mapcar (lambda (x) (constant-form-value x env)) dims)))
             (when (every (lambda (x) (typep x 'index)) values)
               (setq dims values dims-constp t)))))
        (t
         ;; Regardless of dimension, it is always good to flatten :INITIAL-CONTENTS
         ;; if we can, ensuring that we convert `(,X :A :B) = (LIST* X '(:A :B))
         ;; into (VECTOR X :A :B) which makes it cons less if not optimized,
         ;; or cons not at all (not counting the destination array) if optimized.
         ;; There is no need to transform dimensions of '(<N>) to the integer N.
         ;; The IR1 transform for list-shaped dims will figure it out.
         (binding* ((contents (and (evenp (length rest)) (getf rest :initial-contents))
                              :exit-if-null)
                    ;; N-DIMS = 1 can be "technically" wrong, but it doesn't matter.
                    (data (rewrite-initial-contents 1 contents env) :exit-if-null))
           (setf rest (copy-list rest) (getf rest :initial-contents) data)
           (return-from make-array `(make-array ,dims-form ,@rest)))
         (return-from make-array (values nil t))))
  ;; So now we know that this is a multi-dimensional (or 0-dimensional) array.
  ;; Parse keywords conservatively, rejecting anything that makes it non-simple,
  ;; and accepting only a pattern that is likely to occur in practice.
  ;; e.g we give up on a duplicate keywords rather than bind ignored temps.
  (let* ((unsupplied '#:unsupplied) (et unsupplied) et-constp et-binding
         contents element adjustable keys data-dims)
    (unless (loop (if (null rest) (return t))
                  (if (or (atom rest) (atom (cdr rest))) (return nil))
                  (let ((k (pop rest))
                        (v rest))
                    (pop rest)
                    (case k
                      (:element-type
                       (unless (eq et unsupplied) (return nil))
                       (setq et (car v) et-constp (constantp et env)))
                      (:initial-element
                       (when (or contents element) (return nil))
                       (setq element v))
                      (:initial-contents
                       (when (or contents element) (return nil))
                       (if (not dims) ; If 0-dimensional, use :INITIAL-ELEMENT instead
                           (setq k :initial-element element v)
                           (setq contents v)))
                      (:adjustable ; reject if anything other than literal NIL
                       (when (or adjustable (car v)) (return nil))
                       (setq adjustable v))
                      (t
                       ;; Reject :FILL-POINTER, :DISPLACED-{TO,INDEX-OFFSET},
                       ;; and non-literal keywords.
                       (return nil)))
                    (unless (member k '(:adjustable))
                      (setq keys (nconc keys (list k (car v)))))))
      (return-from make-array (values nil t)))
    (when contents
      (multiple-value-bind (data shape)
          (rewrite-initial-contents (length dims) (car contents) env)
        (cond (shape ; initial-contents will be part of the vector allocation
               ;; and we aren't messing up keyword arg order.
               (when (and dims-constp (not (equal shape dims)))
                 ;; This will become a runtime error if the code is executed.
                 (warn "array dimensions are ~A but :INITIAL-CONTENTS dimensions are ~A"
                       dims shape))
               (setf data-dims shape (getf keys :initial-contents) data))
              (t ; contents could not be flattened
               ;; Preserve eval order. The only keyword arg to worry about
               ;; is :ELEMENT-TYPE. See also the remark at DEFKNOWN FILL-ARRAY.
               (when (and (eq (car keys) :element-type) (not et-constp))
                 (let ((et-temp (make-symbol "ET")))
                   (setf et-binding `((,et-temp ,et)) (cadr keys) et-temp)))
               (remf keys :initial-contents)))))
    (let* ((axis-bindings
            (unless dims-constp
              (loop for d in dims for i from 0
                    collect (list (make-symbol (format nil "D~D" i))
                                  `(the index ,d)))))
           (dims (if axis-bindings (mapcar #'car axis-bindings) dims))
           (size (make-symbol "SIZE"))
           (alloc-form
            `(truly-the (simple-array
                         ,(cond ((eq et unsupplied) t)
                                (et-constp (constant-form-value et env))
                                (t '*))
                         ,(if dims-constp dims (length dims)))
              (make-array-header*
               sb-vm:simple-array-widetag
               ,@(sb-vm::make-array-header-inits
                  `(make-array ,size ,@keys) size dims)))))
      `(let* (,@axis-bindings ,@et-binding (,size (the index (* ,@dims))))
         ,(cond ((or (not contents) (and dims-constp (equal dims data-dims)))
                 ;; If no :initial-contents, or definitely correct shape,
                 ;; then just call the constructor.
                 alloc-form)
                (data-dims ; data are flattened
                 ;; original shape must be asserted to be correct
                 ;; Arguably if the contents have a constant shape,
                 ;; we could cast each individual dimension in its binding form,
                 ;; i.e. (LET* ((#:D0 (THE (EQL <n>) dimension0)) ...)
                 ;; but it seems preferable to imply that the initial contents
                 ;; are wrongly shaped rather than that the array is.
                 `(sb-kernel::check-array-shape ,alloc-form ',data-dims))
                (t ; could not parse the data
                 `(fill-array ,(car contents) ,alloc-form)))))))

(define-source-transform coerce (x type &environment env)
  (if (and (constantp type env)
           (proper-list-p x)
           (memq (car x) '(sb-impl::|List| list
                           sb-impl::|Vector| vector)))
      (let* ((type (constant-form-value type env))
             (length (1- (length x)))
             (ctype (careful-specifier-type type)))
        (if (and ctype
                 (neq ctype *empty-type*)
                 (csubtypep ctype (specifier-type '(array * (*)))))
            (multiple-value-bind (type element-type upgraded had-dimensions)
                (simplify-vector-type ctype)
              (declare (ignore type upgraded))
              (if had-dimensions
                  (values nil t)
                  `(make-array ,length
                               :initial-contents ,x
                               ,@(and (not (eq element-type *universal-type*))
                                      (not (eq element-type *wild-type*))
                                      `(:element-type ',(type-specifier element-type))))))
            (values nil t)))
      (values nil t)))

(defun proper-sequence-p (sequence)
  (if (consp sequence)
      (proper-list-p sequence)
      (typep sequence 'sequence)))

;;; Numeric sizes which are smaller than a word, or an even multiple of a byte,
;;; do not need zero-fill because you can't produce a bogus object by reading
;;; an element. But odd sizes such as (UNSIGNED-BYTE 7) should be zero-filled.
;;; CHARACTER too because it's it's 21 bits taking up the space of 32 bits.
;;; Technically the floating-point types should probably be zero-filled because
;;; there may otherwise be trapping NaNs.
(defun should-zerofill-p (saetp &aux (spec (sb-vm:saetp-specifier saetp))
                                     (ctype (sb-vm:saetp-ctype saetp)))
  (or (eq spec 't)
      (and (numeric-type-p ctype)
           (> (sb-vm:saetp-n-bits saetp) 1)
           (consp spec)
           (not (eql (second spec) (sb-vm:saetp-n-bits saetp))))
      ;; Actually, nothing bad seems to happen by seeing char codes over CHAR-CODE-LIMIT
      ;; (and (eq spec 'character) (= bits 32))
      ))

(declaim (inline calc-nwords-form))
(defun calc-nwords-form (saetp const-length
                         &aux (n-bits (sb-vm:saetp-n-bits saetp))
                              (n-pad-elements (sb-vm:saetp-n-pad-elements saetp)))
  (when const-length
    (return-from calc-nwords-form
      (if (typep const-length 'index)
          (ceiling (* (+ const-length n-pad-elements) n-bits) sb-vm:n-word-bits))))
  (let ((padded-length-form (if (zerop n-pad-elements)
                                '%length
                                `(+ %length ,n-pad-elements))))
    (cond ((= n-bits 0) 0)
          ((= n-bits sb-vm:n-word-bits) padded-length-form)
          ((> n-bits sb-vm:n-word-bits) ; e.g. double-float on 32-bit
           (let ((n-words-per-element
                  (the fixnum (/ n-bits sb-vm:n-word-bits)))) ; i.e., not RATIO
             #+64-bit `(* ,padded-length-form ,n-words-per-element)
             #-64-bit `(the fixnum (* ,padded-length-form ,n-words-per-element))))
          (t
           ;; This would have to change if we ever implement Unicode strings
           ;; using 3 bytes per char (as has been suggested by xof) which makes
           ;; the number of elements per word a fraction.
           (let ((n-elements-per-word
                  (the fixnum (/ sb-vm:n-word-bits n-bits))))  ; i.e., not RATIO
             ;; Use the standard algorithm for integer division rounding up,
             ;; but with right-shift as the divide operator, i.e.
             ;; (NTH-VALUE 0 (CEILING length n-elements-per-word)) without going
             ;; through the ceiling transform, because that needs a few extra
             ;; machinations to eliminate the un-needed second return value.
             `(ash (+ (truly-the index ,padded-length-form) ,(1- n-elements-per-word))
                   ,(- (1- (integer-length n-elements-per-word)))))))))

;;; TODO: "initial-element #\space" for strings would be nice to handle.
(declaim (inline splat-value-p))
(defun splat-value-p (elt-ctype initial-element default-initial-element)
  (declare (ignorable elt-ctype))
  ;; If the initial-element is specified and equivalent to 0-fill
  ;; then use SPLAT.
  (if (constant-lvar-p initial-element)
      (cond ((eql (lvar-value initial-element) default-initial-element)
             0)
            ;; If SPLAT is not always a no-op - which it is for everything
            ;; but x86-64 - then also use it to store NIL or unbound-marker,
            ;; which is better than QUICKFILL on small arrays. Arguably it is
            ;; a defect of the FILL transforms that they can't do as well.
            #+x86-64
            ((eq (lvar-value initial-element) nil)
             sb-vm:nil-value))
      ;; This case should not be architecture-dependent, and it isn't,
      ;; except that the other architectures lack the ability
      ;; to convert SPLAT via a vop.
      ;; I feel that it would be a lot easier if unbound-marker manifested
      ;; itself as a compile-time literal, but there's no lisp type for it
      ;; and I guess we don't like that.
      ;; So why not just return T from ctype-of for that object?
      #+x86-64
      (when (eq elt-ctype *universal-type*)
        (let ((node (lvar-uses initial-element)))
          (when (and (combination-p node)
                     (lvar-fun-is (combination-fun node) '(%%primitive))
                     (let* ((args (combination-args node))
                            (arg (car args))
                            (leaf (when (ref-p (lvar-uses arg))
                                    (ref-leaf (lvar-uses arg)))))
                       (and (constant-p leaf) ; (I think it has to be)
                            (eq (constant-value leaf) 'make-unbound-marker))))
            ;; don't need to look at the other codegen arg which is
            ;; surely NIL.
            :unbound)))))

;;; This baby is a bit of a monster, but it takes care of any MAKE-ARRAY
;;; call which creates a vector with a known element type -- and tries
;;; to do a good job with all the different ways it can happen.
(defun transform-make-array-vector (length element-type initial-element
                                    initial-contents call
                                    &key adjustable fill-pointer
                                    &aux c-length)
  (when (and initial-contents initial-element)
    (abort-ir1-transform "Both ~S and ~S specified."
                         :initial-contents :initial-element))
  (setq c-length (if (lvar-p length) ; some callers pass an integer per se
                     (if (constant-lvar-p length) (lvar-value length))
                     length))
  (when (and (integerp c-length) ; 'bad-code.pure' tries to pass ((("foo"))) e.g.
             fill-pointer
             (csubtypep (lvar-type fill-pointer) (specifier-type 'index))
             (not (types-equal-or-intersect (lvar-type fill-pointer)
                                            (specifier-type `(integer 0 ,c-length)))))
    (abort-ir1-transform "Invalid fill-pointer ~s for a vector of length ~s."
                         (type-specifier (lvar-type fill-pointer))
                         c-length))
  (let* ((expressly-adjustable (cond ((not adjustable) nil)
                                     ((not (constant-lvar-p adjustable)) :maybe)
                                     (t (and (lvar-value adjustable) t))))
         (has-fill-pointer (cond ((not fill-pointer) nil)
                                 ((numeric-type-p (lvar-type fill-pointer)) t)
                                 ((constant-lvar-p fill-pointer)
                                  (not (null (lvar-value fill-pointer))))
                                 (t :maybe)))
         (array-header-p (cond ((or (eq expressly-adjustable t) (eq has-fill-pointer t)) t)
                               ((or (eq expressly-adjustable :maybe) (eq has-fill-pointer :maybe))
                                ;; Picking between simple and nonsimple at runtime is hard
                                (give-up-ir1-transform))))
         (elt-spec (if element-type
                       (lvar-value element-type) ; enforces const-ness.
                       t))
         (elt-ctype (ir1-transform-specifier-type elt-spec))
         (saetp (cond ((unknown-type-p elt-ctype)
                       (give-up-ir1-transform "~S is an unknown type: ~S"
                                              :element-type elt-spec))
                      ((eq elt-ctype *empty-type*)
                       (give-up-ir1-transform))
                      (t
                       (find-saetp-by-ctype elt-ctype))))
         (n-words-form (or (calc-nwords-form saetp c-length) (give-up-ir1-transform)))
         (default-initial-element (sb-vm:saetp-initial-element-default saetp))
         (data-alloc-form
          `(truly-the
            (simple-array ,(sb-vm:saetp-specifier saetp) (,(or c-length '*)))
            (allocate-vector #+ubsan ,(not (or initial-contents initial-element))
                             ,(sb-vm:saetp-typecode saetp) %length nwords))))

    (flet ((wrap (underlying)
             `(let* ((%length ,(or c-length '(the index dims)))
                     (nwords ,n-words-form))
                (declare (flushable sb-vm::splat))
                ,(if (not array-header-p)
                     underlying     ; was already cast using TRULY-THE
                     (let* ((constant-fill-pointer-p (and fill-pointer
                                                          (constant-lvar-p fill-pointer)))
                            (fill-pointer-value (and constant-fill-pointer-p
                                                     (lvar-value fill-pointer)))
                            (length-expr
                              (cond ((eq fill-pointer-value t) '%length)
                                    (fill-pointer-value)
                                    ((and fill-pointer (not constant-fill-pointer-p))
                                     `(cond ((or (eq fill-pointer t) (null fill-pointer))
                                             %length)
                                            ((> fill-pointer %length)
                                             (error "Invalid fill-pointer ~a" fill-pointer))
                                            (t fill-pointer)))
                                    (t '%length)))
                            ;; MAKE-ARRAY-HEADER* demands a constant, not an expression
                            ;; for the the header word.
                            (header-bits
                              (logior (if (eq has-fill-pointer t) ; (i.e. can't handle :maybe)
                                          (ash sb-vm:+array-fill-pointer-p+ sb-vm:array-flags-position)
                                          0)
                                      (or (sb-vm:saetp-complex-typecode saetp)
                                          sb-vm:complex-vector-widetag)))
                            (array-header
                              `(truly-the
                                ;; A constant length must not be part of the result type.
                                (and (array ,(sb-vm:saetp-specifier saetp) (*))
                                     (not simple-array))
                                (make-array-header* ,header-bits
                                                    ,length-expr ; fill-pointer
                                                    %length ; total number of elements
                                                    ,underlying
                                                    0 ; displacement
                                                    nil ; displaced-p
                                                    nil ; displaced-from
                                                    %length)))) ; dimensions
                       (if (eq has-fill-pointer :maybe)
                           `(let ((%array ,array-header))
                              (when fill-pointer
                                (logior-array-flags %array sb-vm:+array-fill-pointer-p+))
                              %array)
                           array-header))))))
      (cond ;; Case (1) - :INITIAL-ELEMENT
            (initial-element
             ;; If the specified initial element is equivalent to zero-filling,
             ;; then use SPLAT, which is elidable for heap allocations.
             ;; Also pick off (at least 2) other common cases for SPLAT: NIL and
             ;; unbound-marker. The latter helps PCL ctors not to call FILL.
             (let ((splat (splat-value-p elt-ctype initial-element
                                         default-initial-element))
                   (init (if (constant-lvar-p initial-element)
                             (list 'quote (lvar-value initial-element))
                             'initial-element)))
               (wrap (cond ((not splat)
                            `(quickfill ,data-alloc-form
                                        ,(if (eq elt-spec t) init
                                             `(the ,elt-spec ,init))))
                           ((or (eq splat :unbound)
                                (and (constant-lvar-p initial-element)
                                     (testable-type-p elt-ctype)
                                     (ctypep (lvar-value initial-element) elt-ctype)))
                            ;; all good
                            `(sb-vm::splat ,data-alloc-form nwords ,splat))
                           (t
                            ;; uncertain if initial-element is type-correct
                            `(progn (the ,elt-spec ,init) ; check en passant
                                    (sb-vm::splat ,data-alloc-form nwords
                                                  ,splat)))))))

            ;; Case (2) - neither element nor contents specified.
            ((not initial-contents)
             ;; The implicit default is to zero-fill, but DX arrays could be initialized
             ;; with the unbound-marker. Either way it's worth a style-warning
             ;; if it looks wrong for the specified element type.
             ;; This situation arises e.g. in (MAKE-ARRAY 4 :ELEMENT-TYPE '(INTEGER 1 5))
             ;; ANSI's definition of MAKE-ARRAY says "If INITIAL-ELEMENT is not supplied,
             ;; the consequences of later reading an uninitialized element of new-array
             ;; are undefined," so this could be legal code as long as the user plans to
             ;; write before he reads, and if he doesn't we're free to do anything we like.
             ;; But in case the user doesn't know to write elements before he reads elements
             ;; (or to read manuals before he writes code:-), we'll signal a STYLE-WARNING
             ;; in case he didn't realize this.
             #-sb-xc-host
             (when (and ;; Warn only if any array elements are initialized using the default.
                        (not (eql c-length 0))
                        ;; If it's coming from the source transform,
                        ;; then fill-array means it was supplied initial-contents
                        (not (lvar-matches-calls (combination-lvar call)
                                                 '(make-array-header* fill-array)))
                        (testable-type-p elt-ctype)
                        ;; I really don't want to style-warn about
                        ;; (MAKE-ARRAY 1 :ELEMENT-TYPE 'STANDARD-CHAR) even though technically
                        ;; the default fill of #\nul is wrong because it must match the specified
                        ;; element type, not the upgraded array type, and #\nul isn't standard.
                        (not (ctypep default-initial-element
                                     (if (and (eq elt-spec 'standard-char) (not initial-element))
                                         (sb-vm:saetp-ctype saetp)
                                         elt-ctype))))
               (compiler-style-warn 'initial-element-mismatch-style-warning
                                    :format-control "The default initial element ~S is not a ~S."
                                    :format-arguments (list default-initial-element elt-spec)))
             (wrap (cond ((eql (sb-vm:saetp-typecode saetp) sb-vm:simple-vector-widetag)
                          `(sb-vm::splat ,data-alloc-form nwords
                                         ;; uninitialized reads are trapped regardless of safety
                                         ;; if #+ubsan
                                         #+ubsan :trap
                                         #-ubsan 0))
                         (t
                          ;; otherwise, reading an element can't cause an invalid bit pattern
                          ;; to be observed, but the bits could be random.
                          data-alloc-form))))

            ;; Case (3) - constant :INITIAL-CONTENTS and LENGTH
            ((and c-length
                  (constant-lvar-p initial-contents)
                  ;; As a practical matter, the initial-contents should not be
                  ;; too long, otherwise the compiler seems to spend forever
                  ;; compiling the lambda with one parameter per item.
                  ;; To make matters worse, the time grows superlinearly,
                  ;; and it's not entirely obvious that passing a constant array
                  ;; of 100x100 things is responsible for such an explosion.
                  (let ((initial-contents (lvar-value initial-contents)))
                    (and (proper-sequence-p initial-contents)
                         (<= (length initial-contents) 1000))))
             (let ((contents (lvar-value initial-contents)))
               (unless (= c-length (length contents))
                 (abort-ir1-transform "~S has ~S elements, vector length is ~S."
                                      :initial-contents (length contents) c-length))
               (wrap `(initialize-vector
                       ,data-alloc-form
                       ,@(map 'list
                              (if (eq elt-spec t) ; THE would be pure noise
                                  (lambda (elt) `',elt)
                                  (lambda (elt) `(the ,elt-spec ',elt)))
                              contents)))))

            ;; Case (4)
            ;; :INITIAL-CONTENTS (LIST ...), (VECTOR ...) and `(1 1 ,x) with constant LENGTH.
            ((and c-length
                  (lvar-matches initial-contents
                                ;; FIXME: probably don't need all 4 of these now?
                                :fun-names '(list vector
                                             sb-impl::|List| sb-impl::|Vector|)
                                :arg-count c-length
                                :notinline nil))
             (let ((parameters (eliminate-keyword-args
                                call 1
                                '((:element-type element-type)
                                  (:initial-contents initial-contents)
                                  (:initial-element initial-element)
                                  (:adjustable adjustable)
                                  (:fill-pointer fill-pointer))))
                   (elt-vars (make-gensym-list c-length))
                   (lambda-list '(length)))
               (splice-fun-args initial-contents :any c-length)
               (dolist (p parameters)
                 (setf lambda-list
                       (append lambda-list
                               (if (eq p 'initial-contents)
                                   elt-vars
                                   (list p)))))
               `(lambda ,lambda-list
                  (declare ,@(unless (eq elt-spec t) `((type ,elt-spec ,@elt-vars)))
                           (ignorable ,@lambda-list))
                  ,(wrap `(initialize-vector ,data-alloc-form ,@elt-vars)))))

            ;; Case (5) - :INITIAL-CONTENTS and indeterminate length
            (t
             `(let ((content-length (length initial-contents)))
                (unless (= content-length ,(or c-length 'dims))
                  (sb-vm::initial-contents-error content-length  ,(or c-length 'dims)))
                ,(wrap
                  (if (and (lvar-matches initial-contents :fun-names '(reverse sb-impl::list-reverse
                                                                       sb-impl::vector-reverse))
                           ;; Nothing should be modifying the original sequence
                           (almost-immediately-used-p initial-contents (lvar-use initial-contents)
                                                      :flushable t))
                      (let* ((reverse (lvar-use initial-contents))
                             (initial-contents-type (lvar-type (car (combination-args reverse)))))
                        (splice-fun-args initial-contents :any 1)
                        (cond ((csubtypep initial-contents-type (specifier-type 'list))
                               `(let ((data ,data-alloc-form))
                                  (loop for i from (1- ,(or c-length 'dims)) downto 0
                                        for elt in initial-contents
                                        do (setf (aref data i) elt))
                                  data))
                              ((csubtypep initial-contents-type (specifier-type 'simple-vector))
                               `(let ((data ,data-alloc-form))
                                  (loop for i from (1- ,(or c-length 'dims)) downto 0
                                        for j from 0
                                        do (setf (aref data i) (aref initial-contents j)))
                                  data))
                              (t
                               `(nreverse (replace ,data-alloc-form initial-contents)))))

                      `(replace ,data-alloc-form initial-contents)))))))))

;;; IMPORTANT: The order of these three MAKE-ARRAY forms matters: the least
;;; specific must come first, otherwise suboptimal transforms will result for
;;; some forms.

;;; 3rd choice
(deftransform make-array ((dims &key initial-element initial-contents
                                     element-type
                                     adjustable fill-pointer
                                     displaced-to
                                     displaced-index-offset)
                          (t &rest t) *
                          :node node)
  (delay-ir1-transform node :constraint)
  (when (and initial-contents initial-element)
    (abort-ir1-transform "Can't specify both :INITIAL-ELEMENT and :INITIAL-CONTENTS"))
  (when (and displaced-index-offset
             (not displaced-to))
    (abort-ir1-transform "Can't specify :DISPLACED-INDEX-OFFSET without :DISPLACED-TO"))
  (when (and displaced-to
             (or initial-element initial-contents))
    (if (types-equal-or-intersect (lvar-type displaced-to)
                                  (specifier-type 'null))
        (give-up-ir1-transform ":DISPLACED-TO potentially used with ~S"
                               (if initial-element
                                   :initial-element
                                   :initial-contents))
        (abort-ir1-transform "Can't specify :DISPLACED-TO and ~S"
                             (if initial-element
                                 :initial-element
                                 :initial-contents))))
  (let ((fp-type (and fill-pointer
                      (lvar-type fill-pointer))))
    (when (and fp-type
               (csubtypep fp-type (specifier-type '(or index (eql t)))))
      (let* ((dims (and (constant-lvar-p dims)
                        (lvar-value dims)))
             (length (cond ((integerp dims)
                            dims)
                           ((singleton-p dims)
                            (car dims)))))
        (cond ((not dims))
              ((not length)
               (compiler-warn "Only vectors can have fill pointers."))
              ((and (csubtypep fp-type (specifier-type 'index))
                    (not (types-equal-or-intersect fp-type
                                                   (specifier-type `(integer 0 ,length)))))
               (compiler-warn "Invalid fill-pointer ~s for a vector of length ~s."
                              (type-specifier fp-type)
                              length))))))
  (macrolet ((maybe-arg (arg)
               `(and ,arg `(,,(keywordicate arg) ,',arg))))
    (block nil
      (let* ((eltype (cond ((not element-type) t)
                           ((not (constant-lvar-p element-type))
                            (let ((uses (lvar-uses element-type)))
                              (when (splice-fun-args element-type 'array-element-type 1)
                                (return
                                  `(multiple-value-bind (widetag shift)
                                       (with-source-path ,(node-source-path uses)
                                         (sb-vm::array-underlying-widetag-and-shift element-type))
                                     (%make-array
                                      dims
                                      widetag
                                      shift
                                      ,@(maybe-arg initial-element)
                                      ,@(maybe-arg initial-contents)
                                      ,@(maybe-arg adjustable)
                                      ,@(maybe-arg fill-pointer)
                                      ,@(maybe-arg displaced-to)
                                      ,@(maybe-arg displaced-index-offset))))))
                            (give-up-ir1-transform
                             "ELEMENT-TYPE is not constant."))
                           (t
                            (lvar-value element-type))))
             (eltype-type (ir1-transform-specifier-type eltype))
             (saetp (if (unknown-type-p eltype-type)
                        (give-up-ir1-transform
                         "ELEMENT-TYPE ~s is not a known type"
                         eltype-type)
                        (find eltype-type
                              sb-vm:*specialized-array-element-type-properties*
                              :key #'sb-vm:saetp-ctype
                              :test #'csubtypep)))
             (creation-form `(%make-array
                              dims
                              ,(if saetp
                                   (sb-vm:saetp-typecode saetp)
                                   (give-up-ir1-transform))
                              ,(sb-vm:saetp-n-bits-shift saetp)
                              ,@(maybe-arg initial-contents)
                              ,@(maybe-arg adjustable)
                              ,@(maybe-arg fill-pointer)
                              ,@(maybe-arg displaced-to)
                              ,@(maybe-arg displaced-index-offset))))
        (cond ((not initial-element) creation-form)
              ;; with ubsan the call to %MAKE-ARRAY needs to see the :INITIAL-ELEMENT
              ;; even if it looks like the default, otherwise %MAKE-ARRAY reserves the right
              ;; to scribble on the array. Same for allocators that don't prezero
              #-ubsan
              ((and (constant-lvar-p initial-element)
                    (eql (lvar-value initial-element)
                         (sb-vm:saetp-initial-element-default saetp)))
               creation-form)
              (t
               ;; error checking for target, disabled on the host because
               ;; (CTYPE-OF #\Null) is not possible.
               #-sb-xc-host
               (when (constant-lvar-p initial-element)
                 (let ((value (lvar-value initial-element)))
                   (cond
                     ((not (ctypep value (sb-vm:saetp-ctype saetp)))
                      ;; this case will cause an error at runtime, so we'd
                      ;; better WARN about it now.
                      (warn 'array-initial-element-mismatch
                            :format-control "~@<~S is not a ~S (which is the ~
                                         ~S of ~S).~@:>"
                            :format-arguments
                            (list
                             value
                             (type-specifier (sb-vm:saetp-ctype saetp))
                             'upgraded-array-element-type
                             eltype)))
                     ((multiple-value-bind (typep surep)
                          (ctypep value eltype-type)
                        (and (not typep) surep))
                      ;; This case will not cause an error at runtime, but
                      ;; it's still worth STYLE-WARNing about.
                      (compiler-style-warn 'initial-element-mismatch-style-warning
                                           :format-control "~S is not a ~S."
                                           :format-arguments (list value eltype))))))
               `(let ((array ,creation-form))
                  (multiple-value-bind (vector)
                      (%data-vector-and-index array 0)
                    (fill vector (the ,(sb-vm:saetp-specifier saetp) initial-element)))
                  array)))))))

;;; The list type restriction does not ensure that the result will be a
;;; multi-dimensional array. But the lack of adjustable, fill-pointer,
;;; and displaced-to keywords ensures that it will be simple.
;;; 2nd choice
(deftransform make-array ((dims &key
                                element-type initial-element initial-contents
                                adjustable fill-pointer)
                          (list &key
                                (:element-type (constant-arg *))
                                (:initial-element *)
                                (:initial-contents *)
                                (:adjustable *)
                                (:fill-pointer *))
                          *
                          :node call)
  (block make-array
    ;; Recognize vector construction where the length is spelled as (LIST n)
    ;; or (LIST* n nil). Don't care if FUN-LEXICALLY-NOTINLINE-P on those because
    ;; you can't portably observe whether they're called (tracing them isn't allowed).
    ;; XXX: minor OAOO problem, see similar logic in (VALUES-LIST OPTIMIZER).
    (awhen (cond ((and (lvar-matches dims :fun-names '(list) :arg-count 1 :notinline nil))
                  (car (splice-fun-args dims :any 1)))
                 ((and (lvar-matches dims :fun-names '(list*) :arg-count 2 :notinline nil)
                       (lvar-value-is (second (combination-args (lvar-uses dims))) nil))
                  (let* ((args (splice-fun-args dims :any 2)) ; the args to LIST*
                         (dummy (cadr args)))
                    (flush-dest dummy)
                    (setf (combination-args call) (delete dummy (combination-args call)))
                    (car args))))
        ;; Don't want (list (list x)) to become a valid dimension specifier.
        (assert-lvar-type it (specifier-type 'index) (%coerce-to-policy call))
        (return-from make-array
          (transform-make-array-vector it
                                       element-type
                                       initial-element
                                       initial-contents
                                       call
                                       :adjustable adjustable
                                       :fill-pointer fill-pointer)))
    (unless (constant-lvar-p dims)
      (give-up-ir1-transform
       "The dimension list is not constant; cannot open code array creation."))
    (let ((dims (lvar-value dims))
          (element-type-ctype (and element-type
                                   (constant-lvar-p element-type)
                                   (ir1-transform-specifier-type
                                    (lvar-value element-type)))))
      (when (or (contains-unknown-type-p element-type-ctype)
                (not (proper-list-p dims)))
        (give-up-ir1-transform))
      (unless (check-array-dimensions dims call)
        (give-up-ir1-transform))
      (cond ((singleton-p dims)
             (transform-make-array-vector (car dims) element-type
                                          initial-element initial-contents call
                                          :adjustable adjustable
                                          :fill-pointer fill-pointer))
            ((and fill-pointer (not (lvar-value-is fill-pointer nil)))
             (give-up-ir1-transform))
            (t
             (let* ((total-size (reduce #'* dims))
                    (rank (length dims))
                    (complex (cond ((not adjustable) nil)
                                   ((not (constant-lvar-p adjustable))
                                    (give-up-ir1-transform))
                                   ((lvar-value adjustable))))
                    (spec `(,(if complex
                                 'array
                                 'simple-array)
                            ,(cond ((null element-type) t)
                                   (element-type-ctype
                                    (upgraded-array-element-type
                                     (lvar-value element-type)))
                                   (t '*))
                            ,(make-list rank :initial-element '*))))
               `(truly-the ,spec
                           (make-array-header* ,(if complex
                                                    sb-vm:complex-array-widetag
                                                    sb-vm:simple-array-widetag)
                                               ;; fill-pointer
                                               ,total-size
                                               ;; elements
                                               ,total-size
                                               ;; data
                                               (let ((data (make-array ,total-size
                                                                       ,@(when element-type
                                                                           '(:element-type element-type))
                                                                       ,@(when initial-element
                                                                           '(:initial-element initial-element)))))
                                                 ,(if initial-contents
                                                      ;; FIXME: This is could be open coded at least a bit too
                                                      `(fill-data-vector data ',dims initial-contents)
                                                      'data))
                                               ;; displacement
                                               0
                                               ;; displaced-p
                                               nil
                                               ;; displaced-from
                                               nil
                                               ;; dimensions
                                               ,@dims))))))))

;;; 1st choice
(deftransform make-array ((dims &key element-type initial-element initial-contents
                                     adjustable fill-pointer)
                          (integer &key
                                   (:element-type (constant-arg *))
                                   (:initial-element *)
                                   (:initial-contents *)
                                   (:adjustable *)
                                   (:fill-pointer *))
                          *
                          :node call)
  (transform-make-array-vector dims
                               element-type
                               initial-element
                               initial-contents
                               call
                               :adjustable adjustable
                               :fill-pointer fill-pointer))

;;;; ADJUST-ARRAY
(deftransform adjust-array ((array dims &key displaced-to displaced-index-offset)
                            (array integer &key
                                   (:displaced-to array)
                                   (:displaced-index-offset *)))
  (unless displaced-to
    (give-up-ir1-transform))
  `(progn
     (when (invalid-array-p array)
       (invalid-array-error array))
     (unless (= 1 (array-rank array))
       (error "The number of dimensions is not equal to the rank of the array"))
     (unless (eql (array-element-type array) (array-element-type displaced-to))
       (error "Can't displace an array of type ~S to another of type ~S"
              (array-element-type array) (array-element-type displaced-to)))
     (let ((displacement (or displaced-index-offset 0)))
       (when (< (array-total-size displaced-to) (+ displacement dims))
         (error "The :DISPLACED-TO array is too small"))
       (if (adjustable-array-p array)
           (let ((nfp (when (array-has-fill-pointer-p array)
                        (when (> (%array-fill-pointer array) dims)
                          (error "Cannot ADJUST-ARRAY an array to a size smaller than its fill pointer"))
                        (%array-fill-pointer array))))
             (set-array-header array displaced-to dims nfp
                               displacement dims t nil))
           (make-array dims :element-type (array-element-type array)
                            :displaced-to displaced-to
                            ,@(and displaced-index-offset
                                   '(:displaced-index-offset displacement)))))))

(defoptimizer (adjust-array derive-type) ((array dims &key
                                                 fill-pointer
                                                 displaced-to
                                                 displaced-index-offset
                                                 &allow-other-keys)
                                          node)
  (let* ((array-type (lvar-type array))
         (complex (conservative-array-type-complexp array-type))
         (simple (null complex))
         (complex (eq complex t))
         (dims (if (constant-lvar-p dims)
                   (let ((value (lvar-value dims)))
                     (if (check-array-dimensions value node)
                         value
                         (return-from adjust-array-derive-type-optimizer)))
                   '*)))
    (unless complex
      (let ((null (specifier-type 'null)))
        (flet ((simple (lvar)
                 (when lvar
                   (cond ((not (type= (lvar-type lvar) null))
                          (setf simple nil))
                         ((not (types-equal-or-intersect (lvar-type lvar) null))
                          (setf simple nil
                                complex t))))))
          (simple fill-pointer)
          (simple displaced-to)
          (simple displaced-index-offset))))
    (let ((int (type-intersection (strip-array-dimensions-and-complexity array-type)
                                  (make-array-type (if (integerp dims)
                                                       (list dims)
                                                       dims)
                                                   :complexp (cond ((eq complex t))
                                                                   ((not simple) :maybe))
                                                   :element-type *wild-type*))))
      (if (eq int *empty-type*)
          (let ((*compiler-error-context* node))
            (setf (combination-kind node) :error)
            (compiler-warn "New dimensions ~s do not match the rank of ~a"
                           dims
                           (type-specifier array-type)))
          int))))

;;;; miscellaneous properties of arrays

;;; Transforms for various array properties. If the property is known
;;; at compile time because of a type spec, use that constant value.

;;; Most of this logic may end up belonging in code/late-type.lisp;
;;; however, here we also need the -OR-GIVE-UP for the transforms, and
;;; maybe this is just too sloppy for actual type logic.  -- CSR,
;;; 2004-02-18
(defun array-type-dimensions-or-give-up (type)
  (let ((no-dimension '#:no))
    (labels ((maybe-array-type-dimensions (type)
               (typecase type
                 (array-type
                  (array-type-dimensions type))
                 (union-type
                  (let* ((types (loop for type in (union-type-types type)
                                      for dimensions = (maybe-array-type-dimensions type)
                                      when (eq dimensions no-dimension)
                                      do (return-from maybe-array-type-dimensions no-dimension)
                                      when (eq dimensions '*)
                                      do
                                      (return-from maybe-array-type-dimensions '*)
                                      unless (eq dimensions no-dimension)
                                      collect dimensions))
                         (result (car types))
                         (length (length result))
                         (complete-match t))
                    (dolist (other (cdr types))
                      (when (/= length (length other))
                        (give-up-ir1-transform
                         "~@<dimensions of arrays in union type ~S do not match~:@>"
                         (type-specifier type)))
                      (unless (equal result other)
                        (setf complete-match nil)))
                    (if complete-match
                        result
                        (make-list length :initial-element '*))))
                 (intersection-type
                  (let* ((types (remove no-dimension
                                        (mapcar #'maybe-array-type-dimensions
                                                (intersection-type-types type))))
                         (result (car types)))
                    (dolist (other (cdr types))
                      (unless (equal result other)
                        (abort-ir1-transform
                         "~@<dimensions of arrays in intersection type ~S do not match~:@>"
                         (type-specifier type))))
                    (if types
                        result
                        no-dimension)))
                 (t
                  no-dimension))))
      (let ((dim (maybe-array-type-dimensions type)))
        (if (eq dim no-dimension)
            (give-up-ir1-transform
             "~@<don't know how to extract array dimensions from type ~S~:@>"
             (type-specifier type))
            dim)))))

(defun conservative-array-type-complexp (type)
  (typecase type
    (array-type (array-type-complexp type))
    (union-type
     (let ((types (union-type-types type)))
       (aver (> (length types) 1))
       (let ((result (conservative-array-type-complexp (car types))))
         (dolist (type (cdr types) result)
           (unless (eq (conservative-array-type-complexp type) result)
             (return-from conservative-array-type-complexp :maybe))))))
    (intersection-type
     (loop for type in (intersection-type-types type)
           do (case (conservative-array-type-complexp type)
                ((t) (return t))
                ((nil) (return nil)))
           finally (return :maybe)))
    (t :maybe)))

;; Let type derivation handle constant cases. We only do easy strength
;; reduction.
(deftransform array-rank ((array) (array) * :node node)
  (let ((array-type (lvar-type array)))
    (cond ((and (array-type-p array-type)
                (listp (array-type-dimensions array-type)))
           (length (array-type-dimensions array-type)))
          ;; We need an extra case in here to best handle a known vector, because
          ;; if we try to add a transform on ARRAY-HEADER-P (returning false for
          ;; known simple-array of rank 1), that would fail to optimize where we know
          ;; the thing is a vector but possibly non-simple. So then the IF below
          ;; would still have both if its consequents considered plausible.
          ((csubtypep array-type (specifier-type 'vector))
           1)
          ((and (array-type-p array-type)
                (eq (array-type-complexp array-type) t))
           '(%array-rank array))
          (t
           (delay-ir1-transform node :constraint) ; Why?
           `(%array-rank array)))))

(defun derive-array-rank (ctype)
  (let ((array (specifier-type 'array)))
    (flet ((over (x)
             (cond ((not (types-equal-or-intersect x array))
                    '()) ; Definitely not an array!
                   ((array-type-p x)
                    (let ((dims (array-type-dimensions x)))
                      (if (eql dims '*)
                          '*
                          (list (length dims)))))
                   (t '*)))
           (under (x)
             ;; Might as well catch some easy negation cases.
             (typecase x
               (array-type
                (let ((dims (array-type-dimensions x))
                      (et (array-type-element-type x)))
                  ;; Need to check if the whole type has the same specialization and simplicity,
                  ;; otherwise it's not clear which part of the type is negated.
                  (cond ((not
                          (case (array-type-complexp x)
                            ((t)
                             (csubtypep ctype (specifier-type '(not simple-array))))
                            ((nil)
                             (csubtypep ctype (specifier-type 'simple-array)))
                            (t t)))
                         nil)
                        ((not (or (eq et *wild-type*)
                                  (csubtypep ctype
                                             (specifier-type `(array ,(type-specifier et))))))
                         nil)
                        ((eq dims '*)
                         '*)
                        ((not (every (lambda (dim)
                                       (eq dim '*))
                                     dims))
                         nil)
                        (t
                         (list (length dims))))))
               (t '()))))
      (declare (dynamic-extent #'over #'under))
      (multiple-value-bind (not-p ranks)
          (list-abstract-type-function ctype #'over :under #'under)
        (cond ((eql ranks '*)
               (aver (not not-p))
               nil)
              (not-p
               (specifier-type `(not (member ,@ranks))))
              (t
               (specifier-type `(member ,@ranks))))))))

(defoptimizer (array-rank derive-type) ((array))
  (derive-array-rank (lvar-type array)))

(defoptimizer (%array-rank derive-type) ((array))
  (derive-array-rank (lvar-type array)))

;;; If we know the dimensions at compile time, just use it. Otherwise,
;;; if we can tell that the axis is in bounds, convert to
;;; %ARRAY-DIMENSION (which just indirects the array header) or length
;;; (if it's simple and a vector).
(deftransform array-dimension ((array axis)
                               (array index))
  (unless (constant-lvar-p axis)
    (give-up-ir1-transform "The axis is not constant."))
  ;; Dimensions may change thanks to ADJUST-ARRAY, so we need the
  ;; conservative type.
  (let* ((array-type (lvar-conservative-type array))
         (axis (lvar-value axis))
         (dims (array-type-dimensions-or-give-up array-type)))
    (unless (listp dims)
      (give-up-ir1-transform
       "The array dimensions are unknown; must call ARRAY-DIMENSION at runtime."))
    (unless (> (length dims) axis)
      (abort-ir1-transform "The array has dimensions ~S, ~W is too large."
                           dims
                           axis))
    (let ((dim (nth axis dims)))
      (cond ((integerp dim)
             dim)
            ((= (length dims) 1)
             (ecase (conservative-array-type-complexp array-type)
               ((t)
                '(%array-dimension array 0))
               ((nil)
                '(vector-length array))
               ((:maybe)
                `(if (array-header-p array)
                     (%array-dimension array axis)
                     (vector-length array)))))
            (t
             '(%array-dimension array axis))))))

(deftransform %array-dimension ((array axis)
                                (array (constant-arg index)))
  (let* ((array-type (lvar-conservative-type array))
         (dims (array-type-dimensions-or-give-up array-type))
         (axis (lvar-value axis))
         (dim (and (listp dims)
                   (nth axis dims))))
    (if (integerp dim)
        dim
        (give-up-ir1-transform))))

;;; All vectors can get their length by using VECTOR-LENGTH. If it's
;;; simple, it will extract the length slot from the vector. It it's
;;; complex, it will extract the fill pointer slot from the array
;;; header.
(deftransform length ((vector) (vector))
  '(vector-length vector))

(deftransform length ((vector) ((or null vector)) * :important nil)
  (unless (and (types-equal-or-intersect (lvar-type vector) (specifier-type 'null))
               (types-equal-or-intersect (lvar-type vector) (specifier-type 'vector)))
    (give-up-ir1-transform))
  `(if vector
       (vector-length vector)
       0))

;;; If a simple array with known dimensions, then VECTOR-LENGTH is a
;;; compile-time constant.
(defun vector-length-type (array-type)
  (block nil
    (catch 'give-up-ir1-transform
      (return
        (let ((dim (array-type-dimensions-or-give-up array-type)))
          (if (and (typep dim '(cons integer))
                   (not (conservative-array-type-complexp array-type)))
              (specifier-type `(eql ,(first dim)))
              (let (min-length
                    max-length)
                (when (and (union-type-p array-type)
                           (loop for type in (union-type-types array-type)
                                 for dim = (array-type-dimensions-or-give-up type)
                                 always (typep dim '(cons integer null))
                                 do (let ((length (car dim)))
                                      (cond ((conservative-array-type-complexp type)
                                             ;; fill-pointer can start from 0
                                             (setf min-length 0))
                                            ((or (not min-length)
                                                 (< length min-length))
                                             (setf min-length length)))
                                      (when (or (not max-length)
                                                (> length max-length))
                                        (setf max-length length)))))
                  (specifier-type `(integer ,(or min-length 0)
                                            ,max-length))))))))
    nil))

(defoptimizer (vector-length derive-type) ((vector))
  (vector-length-type (lvar-conservative-type vector)))

;;; Again, if we can tell the results from the type, just use it.
;;; Otherwise, if we know the rank, convert into a computation based
;;; on array-dimension or %array-available-elements
(deftransform array-total-size ((array) (array))
  (let* ((array-type (lvar-type array))
         (dims (array-type-dimensions-or-give-up array-type)))
    (unless (listp dims)
      (give-up-ir1-transform "can't tell the rank at compile time"))
    (cond ((not (memq '* dims))
           (reduce #'* dims))
          ((not (cdr dims))
           ;; A vector, can't use LENGTH since this ignores the fill-pointer
           `(truly-the index (array-dimension array 0)))
          (t
           `(%array-available-elements array)))))

(unless-vop-existsp (:translate test-header-data-bit)
  (define-source-transform test-header-data-bit (array mask)
    `(logtest (get-header-data ,array) ,mask)))

;;; Any array can be tested for a fill-pointer now, using the header bit.
;;; Only a non-simple vector could possibly return true.
;;; If the input is known simple, we have to avoid doing the logtest because there's
;;; no constraint that says that the logtest will return false, and style-warnings
;;; will result from MAP-INTO and other things which have
;;    (if (array-has-fill-pointer-p a) (setf (fill-pointer a) ...))
;; where the compiler knows that the input is simple.
(deftransform array-has-fill-pointer-p ((array) * * :node node)
  (let* ((array-type (lvar-type array))
         (dims (array-type-dimensions-or-give-up array-type))
         complexp)
    ;; If a vector and possibly non-simple, then perform the bit test,
    ;; otherwise the answer is definitely NIL.
    (cond ((and (listp dims) (/= (length dims) 1)) nil) ; dims = * is possibly a vector
          ((eq (setf complexp (conservative-array-type-complexp array-type)) nil) nil)
          (t
           (when (eq complexp :maybe)
             ;; Delay as much as possible so that this transform and
             ;; the CONSTRAINT-PROPAGATE-IF optimizer have the most
             ;; chances to run.
             (delay-ir1-transform node :ir1-phases))
           `(test-header-data-bit array
                                  (ash sb-vm:+array-fill-pointer-p+ sb-vm:array-flags-data-position))))))

(define-source-transform fill-pointer (vector)
  (let ((vector-sym (gensym "VECTOR")))
    `(let ((,vector-sym ,vector))
       (if (and (arrayp ,vector-sym)
                (array-has-fill-pointer-p ,vector-sym))
           (%array-fill-pointer ,vector-sym)
           (sb-vm::fill-pointer-error ,vector-sym)))))

(defun check-bound-code (array dimension index-var index)
  ;; %CHECK-BOUND will perform both bound and type checking when
  ;; necessary, delete the cast so that it doesn't get confused by
  ;; its derived type.
  (let ((use (principal-lvar-ref-use index)))
    (when (array-index-cast-p use)
      (delete-cast use)))
  `(progn (%check-bound ,array ,dimension ,index-var)
          ,index-var))

(deftransform check-bound ((array dimension index))
  (check-bound-code 'array (if (constant-lvar-p dimension)
                               (lvar-value dimension)
                               'dimension)
                    'index index))

(defun check-bound-empty-p (bound index)
  (let* ((bound-type (lvar-type bound))
         (bound-type
           (specifier-type `(integer 0
                                     (,(cond ((constant-lvar-p bound)
                                              (lvar-value bound))
                                             ((and (integer-type-p bound-type)
                                                   (nth-value 1 (integer-type-numeric-bounds bound-type))))
                                             (array-dimension-limit))))))
         (index-type (lvar-type index)))
    (eq (type-intersection bound-type index-type)
        *empty-type*)))

(defoptimizer (%check-bound derive-type) ((array bound index))
  (when (check-bound-empty-p bound index)
    *empty-type*))

;;;; WITH-ARRAY-DATA

;;; This checks to see whether the array is simple and the start and
;;; end are in bounds. If so, it proceeds with those values.
;;; Otherwise, it calls %WITH-ARRAY-DATA. Note that %WITH-ARRAY-DATA
;;; may be further optimized.
;;;
;;; Given any ARRAY, bind DATA-VAR to the array's data vector and
;;; START-VAR and END-VAR to the start and end of the designated
;;; portion of the data vector. SVALUE and EVALUE are any start and
;;; end specified to the original operation, and are factored into the
;;; bindings of START-VAR and END-VAR. OFFSET-VAR is the cumulative
;;; offset of all displacements encountered, and does not include
;;; SVALUE.
;;;
;;; When FORCE-INLINE is set, the underlying %WITH-ARRAY-DATA form is
;;; forced to be inline, overriding the ordinary judgment of the
;;; %WITH-ARRAY-DATA DEFTRANSFORMs. Ordinarily the DEFTRANSFORMs are
;;; fairly picky about their arguments, figuring that if you haven't
;;; bothered to get all your ducks in a row, you probably don't care
;;; that much about speed anyway! But in some cases it makes sense to
;;; do type testing inside %WITH-ARRAY-DATA instead of outside, and
;;; the DEFTRANSFORM can't tell that that's going on, so it can make
;;; sense to use FORCE-INLINE option in that case.
(sb-xc:defmacro with-array-data (((data-var array &key offset-var)
                                  (start-var &optional (svalue 0))
                                  (end-var &optional (evalue nil))
                                  &key force-inline check-fill-pointer
                                       array-header-p)
                                 &body forms
                                 &environment env)
  (once-only ((n-array array)
              (n-svalue `(the index ,svalue))
              (n-evalue `(the (or index null) ,evalue)))
    (multiple-value-bind (forms declarations) (parse-body forms nil)
      (let ((check-bounds (policy env (plusp insert-array-bounds-checks))))
        `(multiple-value-bind (,data-var
                               ,start-var
                               ,end-var
                               ,@ (when offset-var `(,offset-var)))
             (cond ,@(and (not array-header-p)
                          `(((not (array-header-p ,n-array))
                             (let ((,n-array ,n-array))
                               (declare (type vector ,n-array))
                               ,(once-only ((n-len `(length ,n-array))
                                            (n-end `(or ,n-evalue ,n-len)))
                                  (if check-bounds
                                      `(if (<= 0 ,n-svalue ,n-end ,n-len)
                                           (values (truly-the simple-array ,n-array)
                                                   ,n-svalue ,n-end 0)
                                           ,(if check-fill-pointer
                                                `(sequence-bounding-indices-bad-error ,n-array ,n-svalue ,n-evalue)
                                                `(array-bounding-indices-bad-error ,n-array ,n-svalue ,n-evalue)))
                                      `(values (truly-the simple-array ,n-array)
                                               ,n-svalue ,n-end 0)))))))
                   (t
                    ,(cond (force-inline
                            `(%with-array-data-macro ,n-array ,n-svalue ,n-evalue
                                                     :check-bounds ,check-bounds
                                                     :check-fill-pointer ,check-fill-pointer
                                                     :array-header-p t))
                           (check-fill-pointer
                            `(%with-array-data/fp ,n-array ,n-svalue ,n-evalue))
                           (t
                            `(%with-array-data ,n-array ,n-svalue ,n-evalue)))))
           ,@declarations
           ,@(and (and check-bounds
                       (compiling-p env)
                       (loop for (nil . declare) in declarations
                             never (loop for declaration in declare
                                         thereis
                                         (and (typep declaration '(cons (eql ignore)))
                                              (member end-var (cdr declaration))))))
                  `((%in-bounds-constraint ,data-var ,end-var)))
           ,@forms)))))

;;; This is the fundamental definition of %WITH-ARRAY-DATA, for use in
;;; DEFTRANSFORMs and DEFUNs.
(sb-xc:defmacro %with-array-data-macro
    (array start end &key (element-type '*) check-bounds check-fill-pointer
                          array-header-p)
  (with-unique-names (size defaulted-end data cumulative-offset)
    `(let* ((,size ,(cond (check-fill-pointer
                           `(length (the vector ,array)))
                          (array-header-p
                           `(%array-available-elements ,array))
                          (t
                           `(array-total-size ,array))))
            (,defaulted-end (or ,end ,size)))
       ,@ (when check-bounds
            `((unless (<= ,start ,defaulted-end ,size)
                ,(if check-fill-pointer
                     `(sequence-bounding-indices-bad-error ,array ,start ,end)
                     `(array-bounding-indices-bad-error ,array ,start ,end)))))
       (do ((,data ,(if array-header-p
                        `(%array-data ,array)
                        array)
                   (%array-data ,data))
            (,cumulative-offset ,(if array-header-p
                                     `(%array-displacement ,array)
                                     0)
                                (truly-the index
                                           (+ ,cumulative-offset
                                              (%array-displacement ,data)))))
           ((not (array-header-p ,data))
            (values (truly-the (simple-array ,element-type 1) ,data)
                    (truly-the index (+ ,cumulative-offset ,start))
                    (truly-the index (+ ,cumulative-offset ,defaulted-end))
                    ,cumulative-offset))))))

(defun transform-%with-array-data/mumble (array node check-fill-pointer)
  (let ((element-type (upgraded-element-type-specifier-or-give-up array))
        (type (lvar-type array))
        (check-bounds (policy node (plusp insert-array-bounds-checks))))
    (if (and (array-type-p type)
             (not (array-type-complexp type))
             (listp (array-type-dimensions type))
             (not (null (cdr (array-type-dimensions type)))))
        ;; If it's a simple multidimensional array, then just return
        ;; its data vector directly rather than going through
        ;; %WITH-ARRAY-DATA-MACRO. SBCL doesn't generally generate
        ;; code that would use this currently, but we have encouraged
        ;; users to use WITH-ARRAY-DATA and we may use it ourselves at
        ;; some point in the future for optimized libraries or
        ;; similar.
        (if check-bounds
            `(let* ((data (truly-the (simple-array ,element-type (*))
                                     (%array-data array)))
                    (len (length data))
                    (real-end (or end len)))
               (unless (<= 0 start data-end lend)
                 (sequence-bounding-indices-bad-error array start end))
               (values data 0 real-end 0))
            `(let ((data (truly-the (simple-array ,element-type (*))
                                    (%array-data array))))
               (values data 0 (or end (length data)) 0)))
        `(%with-array-data-macro array start end
                                 :check-fill-pointer ,check-fill-pointer
                                 :check-bounds ,check-bounds
                                 :element-type ,element-type))))

;; It might very well be reasonable to allow general ARRAY here, I
;; just haven't tried to understand the performance issues involved.
;; -- WHN, and also CSR 2002-05-26
(deftransform %with-array-data ((array start end)
                                ((or vector simple-array) index (or index null))
                                *
                                :node node
                                :policy (> speed space))
  "inline non-SIMPLE-vector-handling logic"
  (transform-%with-array-data/mumble array node nil))
(deftransform %with-array-data/fp ((array start end)
                                ((or vector simple-array) index (or index null))
                                *
                                :node node
                                :policy (> speed space))
  "inline non-SIMPLE-vector-handling logic"
  (transform-%with-array-data/mumble array node t))

;;;; array accessors

;;; We convert all typed array accessors into AREF and (SETF AREF) with type
;;; assertions on the array.
(macrolet ((define-bit-frob (reffer simplep)
             `(progn
                (define-source-transform ,reffer (a &rest i)
                  `(aref (the (,',(if simplep 'simple-array 'array)
                                  bit
                                  ,(mapcar (constantly '*) i))
                           ,a) ,@i))
                (define-source-transform (setf ,reffer) (value a &rest i)
                  `(setf (aref (the (,',(if simplep 'simple-array 'array)
                                     bit
                                     ,(mapcar (constantly '*) i))
                                    ,a) ,@i)
                         ,value)))))
  (define-bit-frob sbit t)
  (define-bit-frob bit nil))

(macrolet ((define-frob (reffer setter type)
             `(progn
                (define-source-transform ,reffer (a i)
                  `(aref (the ,',type ,a) ,i))
                (define-source-transform ,setter (a i v)
                  `(setf (aref (the ,',type ,a) ,i) ,v)))))
  (define-frob schar %scharset simple-string)
  (define-frob char %charset string)
  (define-frob svref %svset simple-vector))

(defun the-unwild (type expr)
  (if (or (null type) (eq type *wild-type*)) expr `(the ,type ,expr)))
(defun truly-the-unwild (type expr)
  (if (or (null type) (eq type *wild-type*)) expr `(truly-the ,type ,expr)))

(macrolet (;; This is a handy macro for computing the row-major index
           ;; given a set of indices. We wrap each index with a call
           ;; to CHECK-BOUND to ensure that everything works out
           ;; correctly. We can wrap all the interior arithmetic with
           ;; TRULY-THE INDEX because we know the resultant
           ;; row-major index must be an index.
           (with-row-major-index ((array indices index &optional new-value)
                                  &rest body)
             `(let (n-indices dims)
                (dotimes (i (length ,indices))
                  (push (make-symbol (format nil "INDEX-~D" i)) n-indices)
                  (push (make-symbol (format nil "DIM-~D" i)) dims))
                (setf n-indices (nreverse n-indices))
                (setf dims (nreverse dims))
                `(lambda (,@',(when new-value (list new-value))
                          ,',array ,@n-indices)
                   (declare (ignorable ,',array))
                   (let* (,@(let ((,index -1))
                              (mapcar (lambda (name)
                                        `(,name (array-dimension
                                                 ,',array
                                                 ,(incf ,index))))
                                      dims))
                            (,',index
                             ,(if (null dims)
                                  0
                                (do* ((dims dims (cdr dims))
                                      (indices n-indices (cdr indices))
                                      (last-dim nil (car dims))
                                      (form `(check-bound ,',array
                                                          ,(car dims)
                                                          ,(car indices))
                                            `(truly-the
                                              index
                                              (+ (truly-the index
                                                            (* ,form
                                                               ,last-dim))
                                                 (check-bound
                                                  ,',array
                                                  ,(car dims)
                                                  ,(car indices))))))
                                    ((null (cdr dims)) form)))))
                     ,',@body)))))

  ;; Just return the index after computing it.
  (deftransform array-row-major-index ((array &rest indices))
    (with-row-major-index (array indices index)
      index))

  ;; Convert AREF and (SETF AREF) into a HAIRY-DATA-VECTOR-REF (or
  ;; HAIRY-DATA-VECTOR-SET) with the set of indices replaced with the an
  ;; expression for the row major index.
  (deftransform aref ((array &rest indices))
    (with-row-major-index (array indices index)
      (hairy-data-vector-ref array index)))

  (deftransform (setf aref) ((new-value array &rest subscripts))
    (with-row-major-index (array subscripts index new-value)
      (hairy-data-vector-set array index new-value))))

;; For AREF of vectors we do the bounds checking in the callee. This
;; lets us do a significantly more efficient check for simple-arrays
;; without bloating the code. If we already know the type of the array
;; with sufficient precision, skip directly to DATA-VECTOR-REF.
(deftransform aref ((array index) (t t) * :node node)
  (let* ((type (lvar-type array))
         (element-ctype (array-type-upgraded-element-type type))
         (declared-element-ctype (declared-array-element-type type)))
    (cond
      ((eq element-ctype *empty-type*)
       `(data-nil-vector-ref array index))
      ((and (array-type-p type)
            (null (array-type-complexp type))
            (neq element-ctype *wild-type*)
            (eql (length (array-type-dimensions type)) 1))
       (let* ((index (if (policy node (zerop insert-array-bounds-checks))
                         `index
                         (check-bound-code 'array '(vector-length array) 'index index)))
              (bare-form
                `(data-vector-ref array ,index)))
         (if (type= declared-element-ctype element-ctype)
             bare-form
             `(the ,declared-element-ctype ,bare-form))))
      ((policy node (zerop insert-array-bounds-checks))
       (the-unwild declared-element-ctype `(hairy-data-vector-ref array index)))
      (t
       (the-unwild declared-element-ctype `(hairy-data-vector-ref/check-bounds array index))))))

(deftransform (setf aref) ((new-value array index) (t t t) * :node node)
  (let* ((type (lvar-type array))
         (declared-element-ctype (declared-array-element-type type))
         (element-ctype (array-type-upgraded-element-type type))
         (no-check (policy node (zerop insert-array-bounds-checks))))
    (truly-the-unwild
     declared-element-ctype
     (cond
       ((and (array-type-p type)
             (null (array-type-complexp type))
             (neq element-ctype *wild-type*)
             (eql (length (array-type-dimensions type)) 1))
        (let ((element-type-specifier (type-specifier element-ctype))
              (index (if no-check
                         `index
                         (check-bound-code 'array '(vector-length array) 'index index))))
          `(locally
               (declare (type ,element-type-specifier new-value))
             ,(if (type= element-ctype declared-element-ctype)
                  `(progn (data-vector-set array ,index new-value)
                          new-value)
                  `(progn (data-vector-set array ,index
                                           ,(the-unwild declared-element-ctype 'new-value))
                          ,(truly-the-unwild declared-element-ctype 'new-value))))))
       (no-check
        `(hairy-data-vector-set array index ,(the-unwild declared-element-ctype 'new-value)))
       (t
        `(hairy-data-vector-set/check-bounds array index ,(the-unwild declared-element-ctype 'new-value)))))))

;;; But if we find out later that there's some useful type information
;;; available, switch back to the normal one to give other transforms
;;; a stab at it.

(deftransform hairy-data-vector-ref/check-bounds ((array index))
  (let* ((type (lvar-type array))
         (element-type (array-type-upgraded-element-type type)))
    (when (or (and (eq element-type *wild-type*)
                   ;; This type logic corresponds to the special
                   ;; case for strings in HAIRY-DATA-VECTOR-REF
                   ;; (generic/vm-tran.lisp)
                   (not (csubtypep type (specifier-type 'simple-string))))
              (not (null (conservative-array-type-complexp type))))
      (give-up-ir1-transform "Upgraded element type of array is not known at compile time."))
    `(hairy-data-vector-ref array ,(check-bound-code 'array '(array-dimension array 0) 'index index))))

(deftransform hairy-data-vector-set/check-bounds ((array index new-value))
  (let* ((type (lvar-type array))
         (element-type (array-type-upgraded-element-type type))
         (simple (null (conservative-array-type-complexp type))))
    (if (or (and (eq element-type *wild-type*)
                 (not (csubtypep type (specifier-type 'simple-string))))
            (not simple))
        ;; The new value is only suitable for a simple-vector
        (if (and simple
                 (csubtypep (lvar-type new-value) (specifier-type '(not (or number character)))))
            `(hairy-data-vector-set/check-bounds (the simple-vector array) index new-value)
            (give-up-ir1-transform "Upgraded element type of array is not known at compile time."))
        `(hairy-data-vector-set array
                                ,(check-bound-code 'array '(array-dimension array 0) 'index index)
                                new-value))))


;;; Just convert into a HAIRY-DATA-VECTOR-REF (or
;;; HAIRY-DATA-VECTOR-SET) after checking that the index is inside the
;;; array total size.
(deftransform row-major-aref ((array index))
  `(hairy-data-vector-ref array
                          ,(check-bound-code 'array '(array-total-size array) 'index index)))
(deftransform %set-row-major-aref ((array index new-value))
  `(hairy-data-vector-set array
                          ,(check-bound-code 'array '(array-total-size array) 'index index)
                          new-value))

;;;; bit-vector array operation canonicalization
;;;;
;;;; We convert all bit-vector operations to have the result array
;;;; specified. This allows any result allocation to be open-coded,
;;;; and eliminates the need for any VM-dependent transforms to handle
;;;; these cases.

(macrolet ((def (fun)
             `(progn
               (deftransform ,fun ((bit-array-1 bit-array-2
                                                &optional result-bit-array)
                                   (bit-vector bit-vector &optional null) *
                                   :policy (>= speed space))
                 `(,',fun bit-array-1 bit-array-2
                   (make-array (array-dimension bit-array-1 0) :element-type 'bit)))
               ;; If result is T, make it the first arg.
               (deftransform ,fun ((bit-array-1 bit-array-2 result-bit-array)
                                   (bit-vector bit-vector (eql t)) *)
                 `(,',fun bit-array-1 bit-array-2 bit-array-1)))))
  (def bit-and)
  (def bit-ior)
  (def bit-xor)
  (def bit-eqv)
  (def bit-nand)
  (def bit-nor)
  (def bit-andc1)
  (def bit-andc2)
  (def bit-orc1)
  (def bit-orc2))

;;; Similar for BIT-NOT, but there is only one arg...
(deftransform bit-not ((bit-array-1 &optional result-bit-array)
                       (bit-vector &optional null) *
                       :policy (>= speed space))
  '(bit-not bit-array-1
            (make-array (array-dimension bit-array-1 0) :element-type 'bit)))
(deftransform bit-not ((bit-array-1 result-bit-array)
                       (bit-vector (eql t)))
  '(bit-not bit-array-1 bit-array-1))

;;; Pick off some constant cases.
(defoptimizer (array-header-p derive-type) ((array))
  (let ((type (lvar-type array))
        (array-type (specifier-type 'array)))
    (cond ((or (not (types-equal-or-intersect type array-type))
               (csubtypep type (specifier-type '(simple-array * (*)))))
           (specifier-type 'null))
          ((and (csubtypep type array-type)
                (not (types-equal-or-intersect type (specifier-type 'simple-array))))
           (specifier-type '(eql t)))
          ((not (array-type-p type))
           ;; FIXME: use analogue of ARRAY-TYPE-DIMENSIONS-OR-GIVE-UP
           nil)
          (t
           (let ((dims (array-type-dimensions type)))
             (cond ((and (listp dims) (/= (length dims) 1))
                    ;; multi-dimensional array, will have a header
                    (specifier-type '(eql t)))
                   ((eql (array-type-complexp type) t)
                    (specifier-type '(eql t)))
                   (t
                    nil)))))))

(defoptimizer (array-header-p constraint-propagate-if)
    ((array))
  (values array (specifier-type '(and array (not (simple-array * (*)))))))

;;; For the code generated by TEST-ARRAY-ELEMENT-TYPE
(defoptimizer (%other-pointer-widetag derive-type) ((object))
  (let ((object (lvar-type object)))
    (cond ((types-equal-or-intersect object (specifier-type 'simple-array))
           (when (csubtypep object (specifier-type 'simple-array))
             (let ((eltype (array-type-upgraded-element-type object)))
               (if (and (csubtypep object (specifier-type 'vector))
                        (neq eltype *wild-type*))
                   (specifier-type `(eql ,(sb-vm:saetp-typecode (find-saetp-by-ctype eltype))))
                   (specifier-type `(integer ,sb-vm:simple-array-widetag (,sb-vm:complex-base-string-widetag)))))))
          ((types-equal-or-intersect object (specifier-type 'array))
           (specifier-type `(and (not (integer ,sb-vm:simple-array-widetag
                                               (,sb-vm:complex-base-string-widetag)))
                                 (unsigned-byte ,sb-vm:n-widetag-bits))))
          (t
           (specifier-type `(integer 0 (,sb-vm:simple-array-widetag)))))))

;;; If ARRAY-HAS-FILL-POINTER-P returns true, then ARRAY
;;; is of the specified type.
(defoptimizer (array-has-fill-pointer-p constraint-propagate-if)
    ((array))
  (values array (specifier-type '(and vector (not simple-array)))
          nil nil
          ;; Do not add a complementary type, not all non-simple
          ;; vectors have fill-pointers.
          t))

;;; I am highly reluctant to add a transform on MAKE-WEAK-VECTOR which allows it to inline,
;;; because frankly we may need to cease supporting weak-vectors as they currently exist.
;;; Instead it would be just a vector of weak pointers. The problem stems from allowing
;;; multiple objects to refer weakly to a given object X in relation to on-the-fly GC.
;;; It is inefficient or dangerous (or both) to allow multiple weak referers to X to
;;; simultaneously exist unless you can ensure that they are smashed simultaneously too.
;;; If you don't ensure that, then there is a window in which thread1 observes
;;; weak-pointer-value = NIL while thread2 still has an access path to X simply by
;;; dereferencing the weak pointer. So weak vectors compound that problem because users can
;;; iterate over the vector and enliven everything.
;;; A possible solution: weak pointers may need to become interned so any object has at
;;; most 1 weak referer. Also weak hash-tables need a good amount of thought.
;;; Moreover, there needs to be a read barrier on any weak object to eliminate a race between
;;; GC clearing it (supposing that GC decided the referent was otherwise unreachable) and any
;;; mutator seeing it. So SVREF is out of the question because to implement the read barrier
;;; in SVREF would pessimize every piece of code that uses SIMPLE-VECTOR for performance.

;;; But we need this macro in order for some internal code such as a FIND-PACKAGE
;;; inline cache (from the optimizer) to inline the vector allocation without it
;;; having to know how to call ALLOCATE-VECTOR.
(sb-xc:defmacro allocate-weak-vector (n)
  ;; The "new" weak vector is incompatible with SIMPLE-VECTOR.
  ;; Developers working on new algorithms involving weakness will need to enable this feature.
  #+weak-vector-readbarrier
  `(truly-the weak-pointer
              ;; the defknown for ALLOCATE-VECTOR says it returns a vector
              ;; but we can use its translator regardless of that!
              (%primitive sb-vm::allocate-vector-on-heap
                          #+ubsan nil ,sb-vm:weak-pointer-widetag ,n ,n))
  #-weak-vector-readbarrier
  ;; Explicitly compute a widetag with the weakness bit ORed in.
  (let ((type (logior (ash sb-vm:vector-weak-flag sb-vm:array-flags-position)
                      sb-vm:simple-vector-widetag)))
    `(truly-the simple-vector (allocate-vector #+ubsan nil ,type ,n ,n))))

#-weak-vector-readbarrier
(progn
  (sb-xc:defmacro weak-vector-ref (vector index) `(svref ,vector ,index))
  (define-source-transform weak-vector-len (thing)
    `(length (the simple-vector ,thing))))

(defoptimizer (allocate-vector derive-type) ((widetag length words))
  (when (constant-lvar-p length)
    (make-array-type (list (lvar-value length))
                     :complexp nil :element-type *wild-type*)))
