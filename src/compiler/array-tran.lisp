;;;; array-specific optimizers and transforms

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

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

(defun array-type-declared-element-type (type)
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
       (lexenv-policy (node-lexenv (lvar-dest new-value))))))
  (lvar-type new-value))

;;; Return true if ARG is NIL, or is a constant-lvar whose
;;; value is NIL, false otherwise.
(defun unsupplied-or-nil (arg)
  (declare (type (or lvar null) arg))
  (or (not arg)
      (and (constant-lvar-p arg)
           (not (lvar-value arg)))))

(defun supplied-and-true (arg)
  (and arg
       (constant-lvar-p arg)
       (lvar-value arg)
       t))

;;;; DERIVE-TYPE optimizers

;;; Array operations that use a specific number of indices implicitly
;;; assert that the array is of that rank.
(defun assert-array-rank (array rank)
  (assert-lvar-type
   array
   (specifier-type `(array * ,(make-list rank :initial-element '*)))
   (lexenv-policy (node-lexenv (lvar-dest array)))))

(defun derive-aref-type (array)
  (multiple-value-bind (uaet other)
      (array-type-upgraded-element-type (lvar-type array))
    (or other uaet)))

(defoptimizer (array-in-bounds-p derive-type) ((array &rest indices))
  (assert-array-rank array (length indices))
  *universal-type*)

(deftransform array-in-bounds-p ((array &rest subscripts))
  (block nil
    (flet ((give-up (&optional reason)
             (cond ((= (length subscripts) 1)
                    (let ((arg (sb!xc:gensym)))
                      `(lambda (array ,arg)
                         (and (typep ,arg '(and fixnum unsigned-byte))
                              (< ,arg (array-dimension array 0))))))
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
        ;; Might be *. (Note: currently this is never true, because the type
        ;; derivation infers the rank from the call to ARRAY-IN-BOUNDS-P, but
        ;; let's keep this future proof.)
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

(defoptimizer (aref derive-type) ((array &rest indices))
  (assert-array-rank array (length indices))
  (derive-aref-type array))

(defoptimizer ((setf aref) derive-type) ((new-value array &rest subscripts))
  (assert-array-rank array (length subscripts))
  (assert-new-value-type new-value array))

(macrolet ((define (name)
             `(defoptimizer (,name derive-type) ((array index))
                (declare (ignore index))
                (derive-aref-type array))))
  (define hairy-data-vector-ref)
  (define hairy-data-vector-ref/check-bounds)
  (define data-vector-ref))

#!+(or x86 x86-64)
(defoptimizer (data-vector-ref-with-offset derive-type) ((array index offset))
  (declare (ignore index offset))
  (derive-aref-type array))

(macrolet ((define (name)
             `(defoptimizer (,name derive-type) ((array index new-value))
                (declare (ignore index))
                (assert-new-value-type new-value array))))
  (define hairy-data-vector-set)
  (define hairy-data-vector-set/check-bounds)
  (define data-vector-set))

#!+(or x86 x86-64)
(defoptimizer (data-vector-set-with-offset derive-type) ((array index offset new-value))
  (declare (ignore index offset))
  (assert-new-value-type new-value array))

;;; Figure out the type of the data vector if we know the argument
;;; element type.
(defun derive-%with-array-data/mumble-type (array)
  (let ((atype (lvar-type array)))
    (when (array-type-p atype)
      (specifier-type
       `(simple-array ,(type-specifier
                        (array-type-specialized-element-type atype))
                      (*))))))
(defoptimizer (%with-array-data derive-type) ((array start end))
  (declare (ignore start end))
  (derive-%with-array-data/mumble-type array))
(defoptimizer (%with-array-data/fp derive-type) ((array start end))
  (declare (ignore start end))
  (derive-%with-array-data/mumble-type array))

(defoptimizer (array-row-major-index derive-type) ((array &rest indices))
  (assert-array-rank array (length indices))
  *universal-type*)

(defoptimizer (row-major-aref derive-type) ((array index))
  (declare (ignore index))
  (derive-aref-type array))

(defoptimizer (%set-row-major-aref derive-type) ((array index new-value))
  (declare (ignore index))
  (assert-new-value-type new-value array))

(defun derive-make-array-type (dims element-type adjustable
                               fill-pointer displaced-to)
  (let* ((simple (and (unsupplied-or-nil adjustable)
                      (unsupplied-or-nil displaced-to)
                      (unsupplied-or-nil fill-pointer)))
         (spec
           (or `(,(if simple 'simple-array 'array)
                 ,(cond ((not element-type) t)
                        ((ctype-p element-type)
                         (type-specifier element-type))
                        ((constant-lvar-p element-type)
                         (let ((ctype (careful-specifier-type
                                       (lvar-value element-type))))
                           (cond
                             ((or (null ctype) (contains-unknown-type-p ctype)) '*)
                             (t (sb!xc:upgraded-array-element-type
                                 (lvar-value element-type))))))
                        (t
                         '*))
                 ,(cond ((constant-lvar-p dims)
                         (let* ((val (lvar-value dims))
                                (cdims (ensure-list val)))
                           (if simple
                               cdims
                               (length cdims))))
                        ((csubtypep (lvar-type dims)
                                    (specifier-type 'integer))
                         '(*))
                        (t
                         '*)))
               'array)))
    (if (and (not simple)
             (or (supplied-and-true adjustable)
                 (supplied-and-true displaced-to)
                 (supplied-and-true fill-pointer)))
        (careful-specifier-type `(and ,spec (not simple-array)))
        (careful-specifier-type spec))))

(defoptimizer (make-array derive-type)
    ((dims &key element-type adjustable fill-pointer displaced-to))
  (derive-make-array-type dims element-type adjustable
                          fill-pointer displaced-to))

(defoptimizer (%make-array derive-type)
    ((dims widetag n-bits &key adjustable fill-pointer displaced-to))
  (declare (ignore n-bits))
  (let ((saetp (and (constant-lvar-p widetag)
                    (find (lvar-value widetag)
                          sb!vm:*specialized-array-element-type-properties*
                          :key #'sb!vm:saetp-typecode))))
    (derive-make-array-type dims (if saetp
                                     (sb!vm:saetp-ctype saetp)
                                     *wild-type*)
                            adjustable fill-pointer displaced-to)))


;;;; constructors

;;; Convert VECTOR into a MAKE-ARRAY.
(define-source-transform vector (&rest elements)
  `(make-array ,(length elements) :initial-contents (list ,@elements)))

;;; Just convert it into a MAKE-ARRAY.
(deftransform make-string ((length &key
                                   (element-type 'character)
                                   (initial-element
                                    #.*default-init-char-form*)))
  `(the simple-string (make-array (the index length)
                       :element-type element-type
                       ,@(when initial-element
                           '(:initial-element initial-element)))))

;; Traverse the :INTIAL-CONTENTS argument to an array constructor call,
;; changing the skeleton of the data to be constructed by calls to LIST
;; and wrapping some declarations around each array cell's constructor.
;; If a macro is involved, expand it before traversing.
;; Known bugs:
;; - Despite the effort to handle multidimensional arrays here,
;;   an array-header will not be stack-allocated, so the data won't be either.
;; - inline functions whose behavior is merely to call LIST don't work
;;   e.g. :INITIAL-CONTENTS (MY-LIST a b) ; where MY-LIST is inline
;;                                        ; and effectively just (LIST ...)
(defun rewrite-initial-contents (rank initial-contents env)
  (named-let recurse ((rank rank) (data initial-contents))
    (declare (type index rank))
    (if (plusp rank)
        (flet ((sequence-constructor-p (form)
                 (member (car form) '(sb!impl::|List| list
                                      sb!impl::|Vector| vector))))
          (let (expanded)
            (cond ((not (listp data)) data)
                  ((sequence-constructor-p data)
                   `(list ,@(mapcar (lambda (dim) (recurse (1- rank) dim))
                                    (cdr data))))
                  ((and (sb!xc:macro-function (car data) env)
                        (listp (setq expanded (sb!xc:macroexpand data env)))
                        (sequence-constructor-p expanded))
                   (recurse rank expanded))
                  (t data))))
      ;; This is the important bit: once we are past the level of
      ;; :INITIAL-CONTENTS that relates to the array structure, reinline LIST
      ;; and VECTOR so that nested DX isn't screwed up.
        `(locally (declare (inline list vector)) ,data))))

;;; Prevent open coding DIMENSION and :INITIAL-CONTENTS arguments, so that we
;;; can pick them apart in the DEFTRANSFORMS, and transform '(3) style
;;; dimensions to integer args directly.
(define-source-transform make-array (dimensions &rest keyargs &environment env)
  (if (or (and (fun-lexically-notinline-p 'list)
               (fun-lexically-notinline-p 'vector))
          (oddp (length keyargs)))
      (values nil t)
      (multiple-value-bind (new-dimensions rank)
          (flet ((constant-dims (dimensions)
                   (let* ((dims (constant-form-value dimensions env))
                          (canon (ensure-list dims))
                          (rank (length canon)))
                     (values (if (= rank 1)
                                 (list 'quote (car canon))
                                 (list 'quote canon))
                             rank))))
            (cond ((sb!xc:constantp dimensions env)
                   (constant-dims dimensions))
                  ((and (consp dimensions) (eq 'list dimensions))
                   (values dimensions (length (cdr dimensions))))
                  (t
                   (values dimensions nil))))
        (let ((initial-contents (getf keyargs :initial-contents)))
          (when (and initial-contents rank)
            (setf keyargs (copy-list keyargs)
                  (getf keyargs :initial-contents)
                  (rewrite-initial-contents rank initial-contents env))))
        `(locally (declare (notinline list vector))
           (make-array ,new-dimensions ,@keyargs)))))

(define-source-transform coerce (x type &environment env)
  (if (and (sb!xc:constantp type env)
           (proper-list-p x)
           (memq (car x) '(sb!impl::|List| list
                           sb!impl::|Vector| vector)))
      (let* ((type (constant-form-value type env))
             (length (1- (length x)))
             ;; Special case, since strings are unions
             (string-p (member type '(string simple-string)))
             (ctype (or string-p
                        (careful-values-specifier-type type))))
        (if (or string-p
                (and (array-type-p ctype)
                     (csubtypep ctype (specifier-type '(array * (*))))
                     (proper-list-of-length-p (array-type-dimensions ctype) 1)
                     (or (eq (car (array-type-dimensions ctype)) '*)
                         (eq (car (array-type-dimensions ctype)) length))))
            `(make-array ,length
                         :element-type ',(if string-p
                                             'character
                                             (nth-value 1 (simplify-vector-type ctype)))
                         :initial-contents ,x)
            (values nil t)))
      (values nil t)))

;;; This baby is a bit of a monster, but it takes care of any MAKE-ARRAY
;;; call which creates a vector with a known element type -- and tries
;;; to do a good job with all the different ways it can happen.
(defun transform-make-array-vector (length element-type initial-element
                                    initial-contents call)
  (aver (or (not element-type) (constant-lvar-p element-type)))
  (let* ((c-length (when (constant-lvar-p length)
                     (lvar-value length)))
         (elt-spec (if element-type
                       (lvar-value element-type)
                       t))
         (elt-ctype (ir1-transform-specifier-type elt-spec))
         (saetp (if (unknown-type-p elt-ctype)
                    (give-up-ir1-transform "~S is an unknown type: ~S"
                                           :element-type elt-spec)
                    (find-saetp-by-ctype elt-ctype)))
         (default-initial-element (sb!vm:saetp-initial-element-default saetp))
         (n-bits (sb!vm:saetp-n-bits saetp))
         (typecode (sb!vm:saetp-typecode saetp))
         (n-pad-elements (sb!vm:saetp-n-pad-elements saetp))
         (n-words-form
          (if c-length
              (ceiling (* (+ c-length n-pad-elements) n-bits)
                       sb!vm:n-word-bits)
              (let ((padded-length-form (if (zerop n-pad-elements)
                                            'length
                                            `(+ length ,n-pad-elements))))
                (cond
                  ((= n-bits 0) 0)
                  ((>= n-bits sb!vm:n-word-bits)
                   `(* ,padded-length-form
                       ;; i.e., not RATIO
                       ,(the fixnum (/ n-bits sb!vm:n-word-bits))))
                  (t
                   (let ((n-elements-per-word (/ sb!vm:n-word-bits n-bits)))
                     (declare (type index n-elements-per-word)) ; i.e., not RATIO
                     `(ceiling (truly-the index ,padded-length-form)
                               ,n-elements-per-word)))))))
         (result-spec
          `(simple-array ,(sb!vm:saetp-specifier saetp) (,(or c-length '*))))
         (alloc-form
           `(truly-the ,result-spec
                       (allocate-vector ,typecode (the index length) ,n-words-form))))
    (cond ((and initial-element initial-contents)
           (abort-ir1-transform "Both ~S and ~S specified."
                                :initial-contents :initial-element))
          ;; :INITIAL-CONTENTS (LIST ...), (VECTOR ...) and `(1 1 ,x) with a
          ;; constant LENGTH.
          ((and initial-contents c-length
                (lvar-matches initial-contents
                              :fun-names '(list vector
                                           sb!impl::|List| sb!impl::|Vector|)
                              :arg-count c-length))
           (let ((parameters (eliminate-keyword-args
                              call 1 '((:element-type element-type)
                                       (:initial-contents initial-contents))))
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
                (declare (type ,elt-spec ,@elt-vars)
                         (ignorable ,@lambda-list))
                (truly-the ,result-spec
                 (initialize-vector ,alloc-form ,@elt-vars)))))
          ;; constant :INITIAL-CONTENTS and LENGTH
          ((and initial-contents c-length (constant-lvar-p initial-contents))
           (let ((contents (lvar-value initial-contents)))
             (unless (= c-length (length contents))
               (abort-ir1-transform "~S has ~S elements, vector length is ~S."
                                    :initial-contents (length contents) c-length))
             (let ((parameters (eliminate-keyword-args
                                call 1 '((:element-type element-type)
                                         (:initial-contents initial-contents)))))
               `(lambda (length ,@parameters)
                  (declare (ignorable ,@parameters))
                  (truly-the ,result-spec
                   (initialize-vector ,alloc-form
                                      ,@(map 'list (lambda (elt)
                                                     `(the ,elt-spec ',elt))
                                             contents)))))))
          ;; any other :INITIAL-CONTENTS
          (initial-contents
           (let ((parameters (eliminate-keyword-args
                              call 1 '((:element-type element-type)
                                       (:initial-contents initial-contents)))))
             `(lambda (length ,@parameters)
                (declare (ignorable ,@parameters))
                (unless (= length (length initial-contents))
                  (error "~S has ~S elements, vector length is ~S."
                         :initial-contents (length initial-contents) length))
                (truly-the ,result-spec
                           (replace ,alloc-form initial-contents)))))
          ;; :INITIAL-ELEMENT, not EQL to the default
          ((and initial-element
                (or (not (constant-lvar-p initial-element))
                    (not (eql default-initial-element (lvar-value initial-element)))))
           (let ((parameters (eliminate-keyword-args
                              call 1 '((:element-type element-type)
                                       (:initial-element initial-element))))
                 (init (if (constant-lvar-p initial-element)
                           (list 'quote (lvar-value initial-element))
                           'initial-element)))
             `(lambda (length ,@parameters)
                (declare (ignorable ,@parameters))
                (truly-the ,result-spec
                           (fill ,alloc-form (the ,elt-spec ,init))))))
          ;; just :ELEMENT-TYPE, or maybe with :INITIAL-ELEMENT EQL to the
          ;; default
          (t
           #-sb-xc-host
           (and (and (testable-type-p elt-ctype)
                     (neq elt-ctype *empty-type*)
                     (not (ctypep default-initial-element elt-ctype)))
             ;; This situation arises e.g. in (MAKE-ARRAY 4 :ELEMENT-TYPE
             ;; '(INTEGER 1 5)) ANSI's definition of MAKE-ARRAY says "If
             ;; INITIAL-ELEMENT is not supplied, the consequences of later
             ;; reading an uninitialized element of new-array are undefined,"
             ;; so this could be legal code as long as the user plans to
             ;; write before he reads, and if he doesn't we're free to do
             ;; anything we like. But in case the user doesn't know to write
             ;; elements before he reads elements (or to read manuals before
             ;; he writes code:-), we'll signal a STYLE-WARNING in case he
             ;; didn't realize this.
             (if initial-element
                 (compiler-warn "~S ~S is not a ~S"
                                :initial-element default-initial-element
                                elt-spec)
                 (compiler-style-warn "The default initial element ~S is not a ~S."
                                      default-initial-element
                                      elt-spec)))
           (let ((parameters (eliminate-keyword-args
                              call 1 '((:element-type element-type)
                                       (:initial-element initial-element)))))
             `(lambda (length ,@parameters)
                (declare (ignorable ,@parameters))
                ,alloc-form))))))

;;; IMPORTANT: The order of these three MAKE-ARRAY forms matters: the least
;;; specific must come first, otherwise suboptimal transforms will result for
;;; some forms.

(deftransform make-array ((dims &key initial-element element-type
                                     adjustable fill-pointer)
                          (t &rest *) *
                          :node node)
  (delay-ir1-transform node :constraint)
  (let* ((eltype (cond ((not element-type) t)
                       ((not (constant-lvar-p element-type))
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
                          sb!vm:*specialized-array-element-type-properties*
                          :key #'sb!vm:saetp-ctype
                          :test #'csubtypep)))
         (creation-form `(%make-array
                          dims
                          ,(if saetp
                               (sb!vm:saetp-typecode saetp)
                               (give-up-ir1-transform))
                          ,(sb!vm:saetp-n-bits saetp)
                          ,@(when fill-pointer
                              '(:fill-pointer fill-pointer))
                          ,@(when adjustable
                              '(:adjustable adjustable)))))
    (cond ((or (not initial-element)
               (and (constant-lvar-p initial-element)
                    (eql (lvar-value initial-element)
                         (sb!vm:saetp-initial-element-default saetp))))
           creation-form)
          (t
           ;; error checking for target, disabled on the host because
           ;; (CTYPE-OF #\Null) is not possible.
           #-sb-xc-host
           (when (constant-lvar-p initial-element)
             (let ((value (lvar-value initial-element)))
               (cond
                 ((not (ctypep value (sb!vm:saetp-ctype saetp)))
                  ;; this case will cause an error at runtime, so we'd
                  ;; better WARN about it now.
                  (warn 'array-initial-element-mismatch
                        :format-control "~@<~S is not a ~S (which is the ~
                                         ~S of ~S).~@:>"
                        :format-arguments
                        (list
                         value
                         (type-specifier (sb!vm:saetp-ctype saetp))
                         'upgraded-array-element-type
                         eltype)))
                 ((not (ctypep value eltype-type))
                  ;; this case will not cause an error at runtime, but
                  ;; it's still worth STYLE-WARNing about.
                  (compiler-style-warn "~S is not a ~S."
                                       value eltype)))))
           `(let ((array ,creation-form))
              (multiple-value-bind (vector)
                  (%data-vector-and-index array 0)
                (fill vector (the ,(sb!vm:saetp-specifier saetp) initial-element)))
              array)))))

;;; The list type restriction does not ensure that the result will be a
;;; multi-dimensional array. But the lack of adjustable, fill-pointer,
;;; and displaced-to keywords ensures that it will be simple.
;;;
;;; FIXME: should we generalize this transform to non-simple (though
;;; non-displaced-to) arrays, given that we have %WITH-ARRAY-DATA to
;;; deal with those? Maybe when the DEFTRANSFORM
;;; %DATA-VECTOR-AND-INDEX in the VECTOR case problem is solved? --
;;; CSR, 2002-07-01
(deftransform make-array ((dims &key
                                element-type initial-element initial-contents)
                          (list &key
                                (:element-type (constant-arg *))
                                (:initial-element *)
                                (:initial-contents *))
                          *
                          :node call)
  (block make-array
    (when (lvar-matches dims :fun-names '(list) :arg-count 1)
      (let ((length (car (splice-fun-args dims :any 1))))
        (return-from make-array
          (transform-make-array-vector length
                                       element-type
                                       initial-element
                                       initial-contents
                                       call))))
    (unless (constant-lvar-p dims)
      (give-up-ir1-transform
       "The dimension list is not constant; cannot open code array creation."))
    (let ((dims (lvar-value dims))
          (element-type-ctype (and (constant-lvar-p element-type)
                                   (ir1-transform-specifier-type
                                    (lvar-value element-type)))))
      (when (contains-unknown-type-p element-type-ctype)
        (give-up-ir1-transform))
      (unless (every (lambda (x) (typep x '(integer 0))) dims)
        (give-up-ir1-transform
         "The dimension list contains something other than an integer: ~S"
         dims))
      (if (= (length dims) 1)
          `(make-array ',(car dims)
                       ,@(when element-type
                               '(:element-type element-type))
                       ,@(when initial-element
                               '(:initial-element initial-element))
                       ,@(when initial-contents
                               '(:initial-contents initial-contents)))
          (let* ((total-size (reduce #'* dims))
                 (rank (length dims))
                 (spec `(simple-array
                         ,(cond ((null element-type) t)
                                (element-type-ctype
                                 (sb!xc:upgraded-array-element-type
                                  (lvar-value element-type)))
                                (t '*))
                         ,(make-list rank :initial-element '*))))
            `(let ((header (make-array-header sb!vm:simple-array-widetag ,rank))
                   (data (make-array ,total-size
                                     ,@(when element-type
                                             '(:element-type element-type))
                                     ,@(when initial-element
                                             '(:initial-element initial-element)))))
               ,@(when initial-contents
                       ;; FIXME: This is could be open coded at least a bit too
                       `((sb!impl::fill-data-vector data ',dims initial-contents)))
               (setf (%array-fill-pointer header) ,total-size)
               (setf (%array-fill-pointer-p header) nil)
               (setf (%array-available-elements header) ,total-size)
               (setf (%array-data-vector header) data)
               (setf (%array-displaced-p header) nil)
               (setf (%array-displaced-from header) nil)
               ,@(let ((axis -1))
                      (mapcar (lambda (dim)
                                `(setf (%array-dimension header ,(incf axis))
                                       ,dim))
                              dims))
               (truly-the ,spec header)))))))

(deftransform make-array ((dims &key element-type initial-element initial-contents)
                          (integer &key
                                   (:element-type (constant-arg *))
                                   (:initial-element *)
                                   (:initial-contents *))
                          *
                          :node call)
  (transform-make-array-vector dims
                               element-type
                               initial-element
                               initial-contents
                               call))

;;;; miscellaneous properties of arrays

;;; Transforms for various array properties. If the property is know
;;; at compile time because of a type spec, use that constant value.

;;; Most of this logic may end up belonging in code/late-type.lisp;
;;; however, here we also need the -OR-GIVE-UP for the transforms, and
;;; maybe this is just too sloppy for actual type logic.  -- CSR,
;;; 2004-02-18
(defun array-type-dimensions-or-give-up (type)
  (labels ((maybe-array-type-dimensions (type)
             (typecase type
               (array-type
                (array-type-dimensions type))
               (union-type
                (let* ((types (loop for type in (union-type-types type)
                                    for dimensions = (maybe-array-type-dimensions type)
                                    when (eq dimensions '*)
                                    do
                                    (return-from maybe-array-type-dimensions '*)
                                    when dimensions
                                    collect it))
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
                (let* ((types (remove nil (mapcar #'maybe-array-type-dimensions
                                                  (intersection-type-types type))))
                       (result (car types)))
                  (dolist (other (cdr types) result)
                    (unless (equal result other)
                      (abort-ir1-transform
                       "~@<dimensions of arrays in intersection type ~S do not match~:@>"
                       (type-specifier type)))))))))
    (or (maybe-array-type-dimensions type)
        (give-up-ir1-transform
         "~@<don't know how to extract array dimensions from type ~S~:@>"
         (type-specifier type)))))

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
    ;; FIXME: intersection type
    (t :maybe)))

;; Let type derivation handle constant cases. We only do easy strength
;; reduction.
(deftransform array-rank ((array) (array) * :node node)
  (let ((array-type (lvar-type array)))
    (cond ((eq t (and (array-type-p array-type)
                      (array-type-complexp array-type)))
           '(%array-rank array))
          (t
           (delay-ir1-transform node :constraint)
           `(if (array-header-p array)
                (%array-rank array)
                1)))))

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
                (let ((dims (array-type-dimensions x)))
                  (cond ((eql dims '*)
                         '*)
                        ((every (lambda (dim)
                                  (eql dim '*))
                                dims)
                         (list (length dims)))
                        (t
                         '()))))
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
  (let ((array-type (lvar-conservative-type array))
        (axis (lvar-value axis)))
    (let ((dims (array-type-dimensions-or-give-up array-type)))
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
               '(%array-dimension array axis)))))))

;;; If the length has been declared and it's simple, just return it.
(deftransform length ((vector)
                      ((simple-array * (*))))
  (let ((type (lvar-type vector)))
    (let ((dims (array-type-dimensions-or-give-up type)))
      (unless (and (listp dims) (integerp (car dims)))
        (give-up-ir1-transform
         "Vector length is unknown, must call LENGTH at runtime."))
      (car dims))))

;;; All vectors can get their length by using VECTOR-LENGTH. If it's
;;; simple, it will extract the length slot from the vector. It it's
;;; complex, it will extract the fill pointer slot from the array
;;; header.
(deftransform length ((vector) (vector))
  '(vector-length vector))

;;; If a simple array with known dimensions, then VECTOR-LENGTH is a
;;; compile-time constant.
(deftransform vector-length ((vector))
  (let ((vtype (lvar-type vector)))
    (let ((dim (first (array-type-dimensions-or-give-up vtype))))
      (when (eq dim '*)
        (give-up-ir1-transform))
      (when (conservative-array-type-complexp vtype)
        (give-up-ir1-transform))
      dim)))

;;; Again, if we can tell the results from the type, just use it.
;;; Otherwise, if we know the rank, convert into a computation based
;;; on array-dimension. We can wrap a TRULY-THE INDEX around the
;;; multiplications because we know that the total size must be an
;;; INDEX.
(deftransform array-total-size ((array)
                                (array))
  (let ((array-type (lvar-type array)))
    (let ((dims (array-type-dimensions-or-give-up array-type)))
      (unless (listp dims)
        (give-up-ir1-transform "can't tell the rank at compile time"))
      (if (member '* dims)
          (do ((form 1 `(truly-the index
                                   (* (array-dimension array ,i) ,form)))
               (i 0 (1+ i)))
              ((= i (length dims)) form))
          (reduce #'* dims)))))

;;; Only complex vectors have fill pointers.
(deftransform array-has-fill-pointer-p ((array))
  (let ((array-type (lvar-type array)))
    (let ((dims (array-type-dimensions-or-give-up array-type)))
      (if (and (listp dims) (not (= (length dims) 1)))
          nil
          (ecase (conservative-array-type-complexp array-type)
            ((t)
             t)
            ((nil)
             nil)
            ((:maybe)
             (give-up-ir1-transform
              "The array type is ambiguous; must call ~
               ARRAY-HAS-FILL-POINTER-P at runtime.")))))))

(deftransform check-bound ((array dimension index) * * :node node)
  ;; This is simply to avoid multiple evaluation of INDEX by the
  ;; translator, it's easier to wrap it in a lambda from DEFTRANSFORM
  `(bound-cast array ,(if (constant-lvar-p dimension)
                          (lvar-value dimension)
                          'dimension)
               index))

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
(sb!xc:defmacro with-array-data (((data-var array &key offset-var)
                                  (start-var &optional (svalue 0))
                                  (end-var &optional (evalue nil))
                                  &key force-inline check-fill-pointer)
                                 &body forms
                                 &environment env)
  (once-only ((n-array array)
              (n-svalue `(the index ,svalue))
              (n-evalue `(the (or index null) ,evalue)))
    (let ((check-bounds (policy env (plusp insert-array-bounds-checks))))
      `(multiple-value-bind (,data-var
                             ,start-var
                             ,end-var
                             ,@(when offset-var `(,offset-var)))
           (if (not (array-header-p ,n-array))
               (let ((,n-array ,n-array))
                 (declare (type (simple-array * (*)) ,n-array))
                 ,(once-only ((n-len (if check-fill-pointer
                                         `(length ,n-array)
                                         `(array-total-size ,n-array)))
                              (n-end `(or ,n-evalue ,n-len)))
                             (if check-bounds
                                 `(if (<= 0 ,n-svalue ,n-end ,n-len)
                                      (values ,n-array ,n-svalue ,n-end 0)
                                      ,(if check-fill-pointer
                                           `(sequence-bounding-indices-bad-error ,n-array ,n-svalue ,n-evalue)
                                           `(array-bounding-indices-bad-error ,n-array ,n-svalue ,n-evalue)))
                                 `(values ,n-array ,n-svalue ,n-end 0))))
               ,(if force-inline
                    `(%with-array-data-macro ,n-array ,n-svalue ,n-evalue
                                             :check-bounds ,check-bounds
                                             :check-fill-pointer ,check-fill-pointer)
                    (if check-fill-pointer
                        `(%with-array-data/fp ,n-array ,n-svalue ,n-evalue)
                        `(%with-array-data ,n-array ,n-svalue ,n-evalue))))
         ,@forms))))

;;; This is the fundamental definition of %WITH-ARRAY-DATA, for use in
;;; DEFTRANSFORMs and DEFUNs.
(def!macro %with-array-data-macro (array
                                   start
                                   end
                                   &key
                                   (element-type '*)
                                   check-bounds
                                   check-fill-pointer)
  (with-unique-names (size defaulted-end data cumulative-offset)
    `(let* ((,size ,(if check-fill-pointer
                        `(length ,array)
                        `(array-total-size ,array)))
            (,defaulted-end (or ,end ,size)))
       ,@(when check-bounds
               `((unless (<= ,start ,defaulted-end ,size)
                   ,(if check-fill-pointer
                        `(sequence-bounding-indices-bad-error ,array ,start ,end)
                        `(array-bounding-indices-bad-error ,array ,start ,end)))))
       (do ((,data ,array (%array-data-vector ,data))
            (,cumulative-offset 0
                                (+ ,cumulative-offset
                                   (%array-displacement ,data))))
           ((not (array-header-p ,data))
            (values (the (simple-array ,element-type 1) ,data)
                    (the index (+ ,cumulative-offset ,start))
                    (the index (+ ,cumulative-offset ,defaulted-end))
                    (the index ,cumulative-offset)))
         (declare (type index ,cumulative-offset))))))

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
                                     (%array-data-vector array)))
                    (len (length data))
                    (real-end (or end len)))
               (unless (<= 0 start data-end lend)
                 (sequence-bounding-indices-bad-error array start end))
               (values data 0 real-end 0))
            `(let ((data (truly-the (simple-array ,element-type (*))
                                    (%array-data-vector array))))
               (values data 0 (or end (length data)) 0)))
        `(%with-array-data-macro array start end
                                 :check-fill-pointer ,check-fill-pointer
                                 :check-bounds ,check-bounds
                                 :element-type ,element-type))))

;; It might very well be reasonable to allow general ARRAY here, I
;; just haven't tried to understand the performance issues involved.
;; -- WHN, and also CSR 2002-05-26
(deftransform %with-array-data ((array start end)
                                ((or vector simple-array) index (or index null) t)
                                *
                                :node node
                                :policy (> speed space))
  "inline non-SIMPLE-vector-handling logic"
  (transform-%with-array-data/mumble array node nil))
(deftransform %with-array-data/fp ((array start end)
                                ((or vector simple-array) index (or index null) t)
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
  (define-frob char %charset string))

;;; We transform SVREF and %SVSET directly into DATA-VECTOR-REF/SET: this is
;;; around 100 times faster than going through the general-purpose AREF
;;; transform which ends up doing a lot of work -- and introducing many
;;; intermediate lambdas, each meaning a new trip through the compiler -- to
;;; get the same result.
;;;
;;; FIXME: [S]CHAR, and [S]BIT above would almost certainly benefit from a similar
;;; treatment.
(define-source-transform svref (vector index)
  (let ((elt-type (or (when (symbolp vector)
                        (let ((var (lexenv-find vector vars)))
                          (when (lambda-var-p var)
                            (type-specifier
                             (array-type-declared-element-type (lambda-var-type var))))))
                      t)))
    (with-unique-names (n-vector)
      `(let ((,n-vector ,vector))
         (the ,elt-type (data-vector-ref
                         (the simple-vector ,n-vector)
                         (check-bound ,n-vector (length ,n-vector) ,index)))))))

(define-source-transform %svset (vector index value)
  (let ((elt-type (or (when (symbolp vector)
                        (let ((var (lexenv-find vector vars)))
                          (when (lambda-var-p var)
                            (type-specifier
                             (array-type-declared-element-type (lambda-var-type var))))))
                      t)))
    (with-unique-names (n-vector)
      `(let ((,n-vector ,vector))
         (truly-the ,elt-type (data-vector-set
                               (the simple-vector ,n-vector)
                               (check-bound ,n-vector (length ,n-vector) ,index)
                               (the ,elt-type ,value)))))))

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
         (element-ctype (array-type-upgraded-element-type type)))
    (cond
      ((eq element-ctype *empty-type*)
       `(data-nil-vector-ref array index))
      ((and (array-type-p type)
            (null (array-type-complexp type))
            (neq element-ctype *wild-type*)
            (eql (length (array-type-dimensions type)) 1))
       (let* ((declared-element-ctype (array-type-declared-element-type type))
              (bare-form
                `(data-vector-ref array
                                  (check-bound array (array-dimension array 0) index))))
         (if (type= declared-element-ctype element-ctype)
             bare-form
             `(the ,(type-specifier declared-element-ctype) ,bare-form))))
      ((policy node (zerop insert-array-bounds-checks))
       `(hairy-data-vector-ref array index))
      (t `(hairy-data-vector-ref/check-bounds array index)))))

(deftransform (setf aref) ((new-value array index) (t t t) * :node node)
  (if (policy node (zerop insert-array-bounds-checks))
      `(hairy-data-vector-set array index new-value)
      `(hairy-data-vector-set/check-bounds array index new-value)))

;;; But if we find out later that there's some useful type information
;;; available, switch back to the normal one to give other transforms
;;; a stab at it.
(macrolet ((define (name transform-to extra extra-type)
             (declare (ignore extra-type))
             `(deftransform ,name ((array index ,@extra))
                (let* ((type (lvar-type array))
                       (element-type (array-type-upgraded-element-type type))
                       (declared-type (type-specifier
                                       (array-type-declared-element-type type))))
                  ;; If an element type has been declared, we want to
                  ;; use that information it for type checking (even
                  ;; if the access can't be optimized due to the array
                  ;; not being simple).
                  (when (and (eq element-type *wild-type*)
                             ;; This type logic corresponds to the special
                             ;; case for strings in HAIRY-DATA-VECTOR-REF
                             ;; (generic/vm-tran.lisp)
                             (not (csubtypep type (specifier-type 'simple-string))))
                    (when (or (not (array-type-p type))
                              ;; If it's a simple array, we might be able
                              ;; to inline the access completely.
                              (not (null (array-type-complexp type))))
                      (give-up-ir1-transform
                       "Upgraded element type of array is not known at compile time.")))
                  ,(if extra
                       ``(truly-the ,declared-type
                                    (,',transform-to array
                                                     (check-bound array
                                                                  (array-dimension array 0)
                                                                  index)
                                                     (the ,declared-type ,@',extra)))
                       ``(the ,declared-type
                           (,',transform-to array
                                            (check-bound array
                                                         (array-dimension array 0)
                                                         index))))))))
  (define hairy-data-vector-ref/check-bounds
      hairy-data-vector-ref nil nil)
  (define hairy-data-vector-set/check-bounds
      hairy-data-vector-set (new-value) (*)))

;;; Just convert into a HAIRY-DATA-VECTOR-REF (or
;;; HAIRY-DATA-VECTOR-SET) after checking that the index is inside the
;;; array total size.
(deftransform row-major-aref ((array index))
  `(hairy-data-vector-ref array
                          (check-bound array (array-total-size array) index)))
(deftransform %set-row-major-aref ((array index new-value))
  `(hairy-data-vector-set array
                          (check-bound array (array-total-size array) index)
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
  (let ((type (lvar-type array)))
    (cond ((not (array-type-p type))
           ;; FIXME: use analogue of ARRAY-TYPE-DIMENSIONS-OR-GIVE-UP
           nil)
          (t
           (let ((dims (array-type-dimensions type)))
             (cond ((csubtypep type (specifier-type '(simple-array * (*))))
                    ;; no array header
                    (specifier-type 'null))
                   ((and (listp dims) (/= (length dims) 1))
                    ;; multi-dimensional array, will have a header
                    (specifier-type '(eql t)))
                   ((eql (array-type-complexp type) t)
                    (specifier-type '(eql t)))
                   (t
                    nil)))))))
