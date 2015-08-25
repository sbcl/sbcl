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

;;; the description of a &KEY argument
(defstruct (key-info #-sb-xc-host (:pure t)
                     (:copier nil))
  ;; the key (not necessarily a keyword in ANSI Common Lisp)
  (name (missing-arg) :type symbol :read-only t)
  ;; the type of the argument value
  (type (missing-arg) :type ctype :read-only t))

;;;; representations of types

;;; A HAIRY-TYPE represents anything too weird to be described
;;; reasonably or to be useful, such as NOT, SATISFIES, unknown types,
;;; and unreasonably complicated types involving AND. We just remember
;;; the original type spec.
;;; A possible improvement would be for HAIRY-TYPE to have a subtype
;;; named SATISFIES-TYPE for the hairy types which are specifically
;;; of the form (SATISFIES pred) so that we don't have to examine
;;; the sexpr repeatedly to decide whether it takes that form.
;;; And as a further improvement, we might want a table that maps
;;; predicates to their exactly recognized type when possible.
;;; We have such a table in fact - *BACKEND-PREDICATE-TYPES*
;;; as a starting point. But something like PLUSP isn't in there.
;;; On the other hand, either of these points may not be sources of
;;; inefficiency, and the latter if implemented might have undesirable
;;; user-visible ramifications, though it seems unlikely.
(defstruct (hairy-type (:include ctype
                                 (class-info (type-class-or-lose 'hairy)))
                       (:constructor %make-hairy-type (specifier))
                       (:copier nil)
                       #!+cmu (:pure nil))
  ;; the Common Lisp type-specifier of the type we represent
  (specifier nil :type t :read-only t))

;; ENUMERABLE-P is T because a hairy type could be equivalent to a MEMBER type.
;; e.g. any SATISFIES with a predicate returning T over a finite domain.
;; But in practice there's nothing that can be done with this information,
;; because we don't call random predicates when performing operations on types
;; as objects, only when checking for inclusion of something in the type.
(!define-type-class hairy :enumerable t :might-contain-other-types t)

;;; An UNKNOWN-TYPE is a type not known to the type system (not yet
;;; defined). We make this distinction since we don't want to complain
;;; about types that are hairy but defined.
(defstruct (unknown-type (:include hairy-type)
                         (:copier nil)))

(defun maybe-reparse-specifier (type)
  (when (unknown-type-p type)
    (let* ((spec (unknown-type-specifier type))
           (name (if (consp spec)
                     (car spec)
                     spec)))
      (when (info :type :kind name)
        (let ((new-type (specifier-type spec)))
          (unless (unknown-type-p new-type)
            new-type))))))

;;; Evil macro.
(defmacro maybe-reparse-specifier! (type)
  (assert (symbolp type))
  (with-unique-names (new-type)
    `(let ((,new-type (maybe-reparse-specifier ,type)))
       (when ,new-type
         (setf ,type ,new-type)
         t))))

(defstruct (negation-type (:include ctype
                                    (class-info (type-class-or-lose 'negation)))
                          (:copier nil)
                          #!+cmu (:pure nil))
  (type (missing-arg) :type ctype :read-only t))

;; Former comment was:
;;   FIXME: is this right?  It's what they had before, anyway
;; But I think the reason it's right is that "enumerable :t" is equivalent
;; to "maybe" which is actually the conservative assumption, same as HAIRY.
(!define-type-class negation :enumerable t :might-contain-other-types t)

;;; ARGS-TYPE objects are used both to represent VALUES types and
;;; to represent FUNCTION types.
(defstruct (args-type (:include ctype)
                      (:constructor nil)
                      (:copier nil))
  ;; Lists of the type for each required and optional argument.
  (required nil :type list :read-only t)
  (optional nil :type list :read-only t)
  ;; The type for the rest arg. NIL if there is no &REST arg.
  (rest nil :type (or ctype null) :read-only t)
  ;; true if &KEY arguments are specified
  (keyp nil :type boolean :read-only t)
  ;; list of KEY-INFO structures describing the &KEY arguments
  (keywords nil :type list :read-only t)
  ;; true if other &KEY arguments are allowed
  (allowp nil :type boolean :read-only t))

(defun canonicalize-args-type-args (required optional rest &optional keyp)
  (when (eq rest *empty-type*)
    ;; or vice-versa?
    (setq rest nil))
  (loop with last-not-rest = nil
        for i from 0
        for opt in optional
        do (cond ((eq opt *empty-type*)
                  (return (values required (subseq optional i) rest)))
                 ((and (not keyp) (neq opt rest))
                  (setq last-not-rest i)))
        finally (return (values required
                                (cond (keyp
                                       optional)
                                      (last-not-rest
                                       (subseq optional 0 (1+ last-not-rest))))
                                rest))))

(defun parse-args-types (lambda-listy-thing context)
  (multiple-value-bind (llks required optional rest keys)
      (parse-lambda-list
       lambda-listy-thing
       :context context
       :accept (ecase context
                 (:values-type (lambda-list-keyword-mask '(&optional &rest)))
                 (:function-type (lambda-list-keyword-mask
                                  '(&optional &rest &key &allow-other-keys))))
       :silent t)
    (let ((required (mapcar #'single-value-specifier-type required))
          (optional (mapcar #'single-value-specifier-type optional))
          (rest (when rest (single-value-specifier-type (car rest))))
          (keywords
           (collect ((key-info))
             (dolist (key keys)
               (unless (proper-list-of-length-p key 2)
                 (error "Keyword type description is not a two-list: ~S." key))
               (let ((kwd (first key)))
                 (when (find kwd (key-info) :key #'key-info-name)
                   (error "~@<repeated keyword ~S in lambda list: ~2I~_~S~:>"
                          kwd lambda-listy-thing))
                 (key-info
                  (make-key-info
                   :name kwd
                   :type (single-value-specifier-type (second key))))))
             (key-info))))
      (multiple-value-bind (required optional rest)
          (canonicalize-args-type-args required optional rest
                                       (ll-kwds-keyp llks))
        (values llks required optional rest keywords)))))

(defstruct (values-type
            (:include args-type
                      (class-info (type-class-or-lose 'values)))
            (:constructor %make-values-type)
            (:predicate %values-type-p)
            (:copier nil)))

(declaim (inline values-type-p))
(defun values-type-p (x)
  (or (eq x *wild-type*)
      (%values-type-p x)))

(defun-cached (make-values-type-cached
               :hash-bits 8
               :hash-function
               (lambda (req opt rest allowp)
                 (logxor (type-list-cache-hash req)
                         (type-list-cache-hash opt)
                          (if rest
                              (type-hash-value rest)
                              42)
                          ;; Results (logand #xFF (sxhash t/nil))
                          ;; hardcoded to avoid relying on the xc host.
                          ;; [but (logand (sxhash nil) #xff) => 2
                          ;;  for me, so the code and comment disagree,
                          ;;  but not in a way that matters.]
                          (if allowp
                              194
                              11))))
    ((required equal-but-no-car-recursion)
     (optional equal-but-no-car-recursion)
     (rest eq)
     (allowp eq))
  (%make-values-type :required required
                     :optional optional
                     :rest rest
                     :allowp allowp))

(defun make-values-type (&key required optional rest allowp)
  (multiple-value-bind (required optional rest)
      (canonicalize-args-type-args required optional rest)
    (cond ((and (null required)
                (null optional)
                (eq rest *universal-type*))
           *wild-type*)
          ((memq *empty-type* required)
           *empty-type*)
          (t (make-values-type-cached required optional
                                      rest allowp)))))

(!define-type-class values :enumerable nil
                    :might-contain-other-types nil)

;;; (SPECIFIER-TYPE 'FUNCTION) and its subtypes
(defstruct (fun-type (:include args-type
                               (class-info (type-class-or-lose 'function)))
                     (:constructor
                      %make-fun-type (required optional rest
                                      keyp keywords allowp wild-args returns)))
  ;; true if the arguments are unrestrictive, i.e. *
  (wild-args nil :type boolean :read-only t)
  ;; type describing the return values. This is a values type
  ;; when multiple values were specified for the return.
  (returns (missing-arg) :type ctype :read-only t))

;; Without this canonicalization step, I found >350 different
;; (FUNCTION (T) *) representations in a sample build.
(declaim (type (simple-vector 4) *interned-fun-type-instances*))
(defglobal *interned-fun-types* (make-array 4))
(defun !intern-important-fun-type-instances ()
  (setq *interned-fun-types* (make-array 4))
  (let (required)
    (dotimes (i 4)
      (when (plusp i)
        (push *universal-type* required))
      (setf (svref *interned-fun-types* i)
            (mark-ctype-interned
             (%make-fun-type required nil nil nil nil nil nil *wild-type*))))))

(defun make-fun-type (&key required optional rest
                           keyp keywords allowp
                           wild-args returns)
  (let ((rest (if (eq rest *empty-type*) nil rest))
        (n (length required)))
    (if (and (<= n 3)
             (not optional) (not rest) (not keyp)
             (not keywords) (not allowp) (not wild-args)
             (eq returns *wild-type*)
             (every (lambda (x) (eq x *universal-type*)) required))
        (svref *interned-fun-types* n)
        (%make-fun-type required optional rest keyp keywords
                        allowp wild-args returns))))

;;; The CONSTANT-TYPE structure represents a use of the CONSTANT-ARG
;;; "type specifier", which is only meaningful in function argument
;;; type specifiers used within the compiler. (It represents something
;;; that the compiler knows to be a constant.)
(defstruct (constant-type
            (:include ctype
                      (class-info (type-class-or-lose 'constant)))
            (:copier nil))
  ;; The type which the argument must be a constant instance of for this type
  ;; specifier to win.
  (type (missing-arg) :type ctype :read-only t))

;;; The NAMED-TYPE is used to represent *, T and NIL, the standard
;;; special cases, as well as other special cases needed to
;;; interpolate between regions of the type hierarchy, such as
;;; INSTANCE (which corresponds to all those classes with slots which
;;; are not funcallable), FUNCALLABLE-INSTANCE (those classes with
;;; slots which are funcallable) and EXTENDED-SEQUUENCE (non-LIST
;;; non-VECTOR classes which are also sequences).  These special cases
;;; are the ones that aren't really discussed by Baker in his
;;; "Decision Procedure for SUBTYPEP" paper.
(defstruct (named-type (:include ctype
                                 (class-info (type-class-or-lose 'named)))
                       (:copier nil))
  (name nil :type symbol :read-only t))

;;; a list of all the float "formats" (i.e. internal representations;
;;; nothing to do with #'FORMAT), in order of decreasing precision
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *float-formats*
    '(long-float double-float single-float short-float)))

;;; The type of a float format.
(deftype float-format () `(member ,@*float-formats*))

;;; A NUMERIC-TYPE represents any numeric type, including things
;;; such as FIXNUM.
(defstruct (numeric-type (:include ctype
                                   (class-info (type-class-or-lose 'number)))
                         (:constructor %make-numeric-type)
                         (:copier nil))
  ;; Formerly defined in every CTYPE, but now just in the ones
  ;; for which enumerability is variable.
  (enumerable nil :read-only t)
  ;; the kind of numeric type we have, or NIL if not specified (just
  ;; NUMBER or COMPLEX)
  ;;
  ;; KLUDGE: A slot named CLASS for a non-CLASS value is bad.
  ;; Especially when a CLASS value *is* stored in another slot (called
  ;; CLASS-INFO:-). Perhaps this should be called CLASS-NAME? Also
  ;; weird that comment above says "Numeric-Type is used to represent
  ;; all numeric types" but this slot doesn't allow COMPLEX as an
  ;; option.. how does this fall into "not specified" NIL case above?
  ;; Perhaps someday we can switch to CLOS and make NUMERIC-TYPE
  ;; be an abstract base class and INTEGER-TYPE, RATIONAL-TYPE, and
  ;; whatnot be concrete subclasses..
  (class nil :type (member integer rational float nil) :read-only t)
  ;; "format" for a float type (i.e. type specifier for a CPU
  ;; representation of floating point, e.g. 'SINGLE-FLOAT -- nothing
  ;; to do with #'FORMAT), or NIL if not specified or not a float.
  ;; Formats which don't exist in a given implementation don't appear
  ;; here.
  (format nil :type (or float-format null) :read-only t)
  ;; Is this a complex numeric type?  Null if unknown (only in NUMBER).
  ;;
  ;; FIXME: I'm bewildered by FOO-P names for things not intended to
  ;; interpreted as truth values. Perhaps rename this COMPLEXNESS?
  (complexp :real :type (member :real :complex nil) :read-only t)
  ;; The upper and lower bounds on the value, or NIL if there is no
  ;; bound. If a list of a number, the bound is exclusive. Integer
  ;; types never have exclusive bounds, i.e. they may have them on
  ;; input, but they're canonicalized to inclusive bounds before we
  ;; store them here.
  (low nil :type (or number cons null) :read-only t)
  (high nil :type (or number cons null) :read-only t))

;; For some numeric subtypes, uniqueness of the object representation
;; is enforced. These encompass all array specializations and more.
(defglobal *unsigned-byte-type* -1)
(defglobal *integer-type* -1)
(defglobal *index-type* -1)
;; BIGNUM is not an interned type because union types aren't interned,
;; though some of the important ones probably ought to be.
(defglobal *positive-bignum-type* -1)
(defglobal *negative-bignum-type* -1)
(defglobal *rational-type* -1)
(defglobal *unsigned-byte-n-types* -1)
(defglobal *signed-byte-n-types* -1)
(defglobal *real-ffloat-type* -1)
(defglobal *real-dfloat-type* -1)
(defglobal *complex-ffloat-type* -1)
(defglobal *complex-dfloat-type* -1)
#-sb-xc-host
(declaim (type (simple-vector #.(1+ sb!vm:n-word-bits)) *unsigned-byte-n-types*)
         (type (simple-vector #.sb!vm:n-word-bits) *signed-byte-n-types*))

;; Called after NUMBER-TYPE type-class has been made.
(defun !intern-important-numeric-type-instances ()
  (flet ((float-type (format complexp)
           (mark-ctype-interned
            (%make-numeric-type :class 'float :complexp complexp
                                :format format :enumerable nil)))
         (int-type (enumerable low high)
           (mark-ctype-interned
            (%make-numeric-type :class 'integer :complexp :real
                                :enumerable enumerable
                                :low low :high high))))
    (setq *real-ffloat-type*      (float-type 'single-float :real)
          *real-dfloat-type*      (float-type 'double-float :real)
          *complex-ffloat-type*   (float-type 'single-float :complex)
          *complex-dfloat-type*   (float-type 'double-float :complex)
          *rational-type*         (mark-ctype-interned
                                   (%make-numeric-type :class 'rational))
          *unsigned-byte-type*    (int-type nil 0 nil)
          *integer-type*          (int-type nil nil nil)
          *index-type*            (int-type nil 0 (1- sb!xc:array-dimension-limit))
          *negative-bignum-type*  (int-type nil nil (1- sb!xc:most-negative-fixnum))
          *positive-bignum-type*  (int-type nil (1+ sb!xc:most-positive-fixnum) nil)
          *unsigned-byte-n-types* (make-array (1+ sb!vm:n-word-bits))
          *signed-byte-n-types*   (make-array sb!vm:n-word-bits))
    (dotimes (j (1+ sb!vm:n-word-bits))
      (setf (svref *unsigned-byte-n-types* j) (int-type t 0 (1- (ash 1 j)))))
    (dotimes (j sb!vm:n-word-bits)
      (setf (svref *signed-byte-n-types* j)
            (let ((high (1- (ash 1 j)))) (int-type t (- (1+ high)) high))))))

;;; Impose canonicalization rules for NUMERIC-TYPE. Note that in some
;;; cases, despite the name, we return *EMPTY-TYPE* instead of a
;;; NUMERIC-TYPE.
;;; FIXME: The ENUMERABLE flag is unexpectedly NIL for types that
;;; come from parsing MEMBER. But bounded integer ranges,
;;; however large, are enumerable:
;;;  (TYPE-ENUMERABLE (SPECIFIER-TYPE '(SIGNED-BYTE 99))) => T
;;;  (TYPE-ENUMERABLE (SPECIFIER-TYPE '(COMPLEX (SIGNED-BYTE 99)))) => T
;;; but, in contrast,
;;;  (TYPE-ENUMERABLE (SPECIFIER-TYPE '(EQL 5))) => NIL.
;;; I can't figure out whether this is supposed to matter.
;;; Moreover, it seems like this function should be responsible
;;; for figuring out the right value so that callers don't have to.
(defun make-numeric-type (&key class format (complexp :real) low high
                               enumerable)
  ;; if interval is empty
  (if (and low
           high
           (if (or (consp low) (consp high)) ; if either bound is exclusive
               (>= (type-bound-number low) (type-bound-number high))
               (> low high)))
      *empty-type*
      (multiple-value-bind (low high)
          (case class
            (integer
             ;; INTEGER types always have their LOW and HIGH bounds
             ;; represented as inclusive, not exclusive values.
             (values (if (consp low) (1+ (type-bound-number low)) low)
                     (if (consp high) (1- (type-bound-number high)) high)))
            (t
             ;; no canonicalization necessary
             (values low high)))
        (when (and (eq class 'rational)
                   (integerp low)
                   (integerp high)
                   (= low high))
          (setf class 'integer))

        ;; Either lookup the canonical interned object for
        ;; a point in the type lattice, or construct a new one.
        (or (cond ((eq class 'float)
                   (when (and (null low) (null high))
                     (case format
                       (single-float
                        (case complexp
                          (:real    *real-ffloat-type*)
                          (:complex *complex-ffloat-type*)))
                       (double-float
                        (case complexp
                          (:real    *real-dfloat-type*)
                          (:complex *complex-dfloat-type*))))))
                  ((and (eq class 'integer) (eq complexp :real))
                   (flet ((n-bits () (integer-length (truly-the word high))))
                     (declare (inline n-bits))
                     (cond ((null high)
                            (cond ((eql low 0) *unsigned-byte-type*)
                                  ((not low) *integer-type*)
                                  ((eql low (1+ sb!xc:most-positive-fixnum))
                                   *positive-bignum-type*)))
                           ((or (= high most-positive-word)
                                (and (typep high 'word)
                                     ;; is (1+ high) a power-of-2 ?
                                     (zerop (logand (1+ high) high))))
                            (cond ((eql low 0)
                                   (svref *unsigned-byte-n-types* (n-bits)))
                                  ((and (< high most-positive-word)
                                        (eql low (lognot high)))
                                   (svref *signed-byte-n-types* (n-bits)))))
                           ((and (eql low 0)
                                 (eql high (1- sb!xc:array-dimension-limit)))
                            *index-type*)
                           ((and (not low)
                                 (eql high (1- sb!xc:most-negative-fixnum)))
                            *negative-bignum-type*))))
                  ((and (eq class 'rational) (eq complexp :real)
                        (null low) (eq high low))
                   *rational-type*))
            (let ((result
                   (%make-numeric-type :class class
                                       :format format
                                       :complexp complexp
                                       :low low
                                       :high high
                                       :enumerable enumerable)))
              (setf (type-hash-value result)
                    (logior (type-hash-value result)
                            +type-admits-type=-optimization+))
              result)))))

(defun modified-numeric-type (base
                              &key
                              (class      (numeric-type-class      base))
                              (format     (numeric-type-format     base))
                              (complexp   (numeric-type-complexp   base))
                              (low        (numeric-type-low        base))
                              (high       (numeric-type-high       base))
                              (enumerable (type-enumerable         base)))
  (make-numeric-type :class class
                     :format format
                     :complexp complexp
                     :low low
                     :high high
                     :enumerable enumerable))

(defstruct (character-set-type
            (:include ctype
                      (class-info (type-class-or-lose 'character-set)))
            (:constructor %make-character-set-type (pairs))
            (:copier nil))
  (pairs (missing-arg) :type list :read-only t))

;; Interned character-set types.
(defglobal *character-type* -1)
#!+sb-unicode
(progn (defglobal *base-char-type* -1)
       (defglobal *extended-char-type* -1))
#+sb-xc (declaim (type ctype *character-type*
                       #!+sb-unicode *base-char-type*
                       #!+sb-unicode *extended-char-type*))

(defun !intern-important-character-set-type-instances ()
  (flet ((range (low high)
           (mark-ctype-interned
            (%make-character-set-type (list (cons low high))))))
    (setq *character-type* (range 0 (1- sb!xc:char-code-limit)))
    #!+sb-unicode
    (setq *base-char-type* (range 0 127)
          *extended-char-type* (range 128 (1- sb!xc:char-code-limit)))))

(defun make-character-set-type (&key pairs)
  ; (aver (equal (mapcar #'car pairs)
  ;              (sort (mapcar #'car pairs) #'<)))
  ;; aver that the cars of the list elements are sorted into increasing order
  (aver (or (null pairs)
            (do ((p pairs (cdr p)))
                ((null (cdr p)) t)
              (when (> (caar p) (caadr p)) (return nil)))))
  (let ((pairs (let (result)
                (do ((pairs pairs (cdr pairs)))
                    ((null pairs) (nreverse result))
                  (destructuring-bind (low . high) (car pairs)
                    (loop for (low1 . high1) in (cdr pairs)
                          if (<= low1 (1+ high))
                          do (progn (setf high (max high high1))
                                    (setf pairs (cdr pairs)))
                          else do (return nil))
                    (cond
                      ((>= low sb!xc:char-code-limit))
                      ((< high 0))
                      (t (push (cons (max 0 low)
                                     (min high (1- sb!xc:char-code-limit)))
                               result))))))))
    (if (null pairs)
        *empty-type*
        (or (and (singleton-p pairs)
                 (let* ((pair (car pairs))
                        (low (car pair)))
                   (case (cdr pair) ; high
                     (#.(1- sb!xc:char-code-limit)
                      (case low
                        (0 *character-type*)
                        #!+sb-unicode (128 *extended-char-type*)))
                     #!+sb-unicode
                     (127 (if (eql low 0) *base-char-type*)))))
            (%make-character-set-type pairs)))))

;;; An ARRAY-TYPE is used to represent any array type, including
;;; things such as SIMPLE-BASE-STRING.
(defstruct (array-type (:include ctype
                                 (class-info (type-class-or-lose 'array)))
                       (:constructor %make-array-type
                        (dimensions complexp element-type
                                    specialized-element-type))
                       (:copier nil))
  ;; the dimensions of the array, or * if unspecified. If a dimension
  ;; is unspecified, it is *.
  (dimensions '* :type (or list (member *)) :read-only t)
  ;; Is this not a simple array type? (:MAYBE means that we don't know.)
  (complexp :maybe :type (member t nil :maybe) :read-only t)
  ;; the element type as originally specified
  (element-type (missing-arg) :type ctype :read-only t)
  ;; the element type as it is specialized in this implementation
  (specialized-element-type *wild-type* :type ctype :read-only t))

;; For all ctypes which are the element types of specialized arrays,
;; 3 ctype objects are stored for the rank-1 arrays of that specialization,
;; one for each of simple, maybe-simple, and non-simple (in that order),
;; and 2 ctype objects for unknown-rank arrays, one each for simple
;; and maybe-simple. (Unknown rank, known-non-simple isn't important)
(defglobal *canonical-array-ctypes* -1)
(defconstant +canon-array-ctype-hash-divisor+ 37) ; arbitrary-ish
(defun !intern-important-array-type-instances ()
  ;; Having made the canonical numeric and character ctypes
  ;; representing the points in the type lattice for which there
  ;; are array specializations, we can make the canonical array types.
  (let* ((element-types
          (list*
           *universal-type* *wild-type* *empty-type*
           *character-type*
           #!+sb-unicode *base-char-type*
           ;; FIXME: This one is can't be used by MAKE-ARRAY-TYPE?
           #!+sb-unicode *extended-char-type*
           *real-ffloat-type* *complex-ffloat-type*
           *real-dfloat-type* *complex-dfloat-type*
           (delete
            nil
            ;; Possibly could use the SAETP-IMPORTANCE as sort criterion
            ;; so that collisions in a bucket place the more important
            ;; array type first.
            (mapcar
             (lambda (x)
               (cond ((typep x '(cons (eql unsigned-byte)))
                      (aref *unsigned-byte-n-types* (cadr x)))
                     ((eq x 'bit)
                      (aref *unsigned-byte-n-types* 1))
                     ((typep x '(cons (eql signed-byte)))
                      ;; 1- because there is no such thing as (signed-byte 0)
                      (aref *signed-byte-n-types* (1- (cadr x))))
                     ;; FIXNUM is its own thing, why? See comment in vm-array
                     ;; saying to "See the comment in PRIMITIVE-TYPE-AUX"
                     ((eq x 'fixnum) ; One good kludge deserves another.
                      (aref *signed-byte-n-types* (1- sb!vm:n-fixnum-bits)))))
             '#.*specialized-array-element-types*))))
         (n (length element-types))
         (data-vector (make-array (* 5 n)))
         (index 0)
         (hashtable (make-array +canon-array-ctype-hash-divisor+
                                :initial-element nil)))
    ;; This is a compact binned table. A full-blown hashtable is unneeded.
    #-sb-xc (aver (< (/ n (length hashtable)) 80/100)) ; assert reasonable load
    (flet ((make-it (dims complexp type)
             (setf (aref data-vector (prog1 index (incf index)))
                   (mark-ctype-interned
                    (%make-array-type dims complexp type type)))))
      (dolist (element-type element-types)
        (let ((bin (mod (type-hash-value element-type)
                        +canon-array-ctype-hash-divisor+)))
          (setf (aref hashtable bin)
                (nconc (aref hashtable bin) (list (cons element-type index))))
          (make-it '(*) nil    element-type)
          (make-it '(*) :maybe element-type)
          (make-it '(*) t      element-type)
          (make-it '*   nil    element-type)
          (make-it '*   :maybe element-type))))
    (setq *canonical-array-ctypes* (cons data-vector hashtable))))

(declaim (ftype (sfunction (t &key (:complexp t)
                                   (:element-type t)
                                   (:specialized-element-type t))
                           ctype) make-array-type))
(defun make-array-type (dimensions &key (complexp :maybe) element-type
                                        (specialized-element-type *wild-type*))
  (or (and (eq element-type specialized-element-type)
           (or (and (eq dimensions '*) (neq complexp t))
               (typep dimensions '(cons (eql *) null)))
           (let ((table *canonical-array-ctypes*))
             (dolist (cell (svref (cdr table)
                                  (mod (type-hash-value element-type)
                                       +canon-array-ctype-hash-divisor+)))
               (when (eq (car cell) element-type)
                 (return
                  (truly-the ctype
                   (svref (car table)
                          (+ (cdr cell)
                             (if (listp dimensions) 0 3)
                             (ecase complexp
                              ((nil) 0) ((:maybe) 1) ((t) 2))))))))))
      (%make-array-type dimensions
                        complexp element-type specialized-element-type)))

;;; A MEMBER-TYPE represent a use of the MEMBER type specifier. We
;;; bother with this at this level because MEMBER types are fairly
;;; important and union and intersection are well defined.
(defstruct (member-type (:include ctype
                                  (class-info (type-class-or-lose 'member)))
                        (:copier nil)
                        (:constructor %make-member-type (xset fp-zeroes))
                        #-sb-xc-host (:pure nil))
  (xset (missing-arg) :type xset :read-only t)
  (fp-zeroes (missing-arg) :type list :read-only t))

(defglobal *null-type* -1)    ; = (MEMBER NIL)
(defglobal *eql-t-type* -1)   ; = (MEMBER T)
(defglobal *boolean-type* -1) ; = (MEMBER T NIL)
#+sb-xc (declaim (type ctype *null-type*))

(defun !intern-important-member-type-instances ()
  (flet ((make-it (list)
           (mark-ctype-interned
            (%make-member-type (xset-from-list list) nil))))
    (setf *null-type* (make-it '(nil))
          *eql-t-type* (make-it '(t))
          *boolean-type* (make-it '(t nil)))))

(declaim (ftype (sfunction (xset list) ctype) make-member-type))
(defun member-type-from-list (members)
  (let ((xset (alloc-xset))
        (fp-zeroes))
    (dolist (elt members (make-member-type xset fp-zeroes))
      (if (fp-zero-p elt)
          (pushnew elt fp-zeroes)
          (add-to-xset elt xset)))))
(defun make-eql-type (elt) (member-type-from-list (list elt)))
;; Return possibly a union of a MEMBER type and a NUMERIC type,
;; or just one or the other, or *EMPTY-TYPE* depending on what's in the XSET
;; and the FP-ZEROES. XSET should not contains characters or real numbers.
(defun make-member-type (xset fp-zeroes)
  ;; if we have a pair of zeros (e.g. 0.0d0 and -0.0d0), then we can
  ;; canonicalize to (DOUBLE-FLOAT 0.0d0 0.0d0), because numeric
  ;; ranges are compared by arithmetic operators (while MEMBERship is
  ;; compared by EQL).  -- CSR, 2003-04-23
  (let ((presence 0)
        (unpaired nil)
        (float-types nil))
    (when fp-zeroes ; avoid doing two passes of nothing
      (dotimes (pass 2)
        (dolist (z fp-zeroes)
          (let ((sign (if (minusp (nth-value 2 (integer-decode-float z))) 1 0))
                (pair-idx
                  (etypecase z
                    (single-float 0)
                    (double-float 2
                    #!+long-float (long-float 4)))))
            (if (= pass 0)
                (setf (ldb (byte 1 (+ pair-idx sign)) presence) 1)
                (if (= (ldb (byte 2 pair-idx) presence) #b11)
                    (when (= sign 0)
                      (push (ctype-of z) float-types))
                    (push z unpaired)))))))
    (let ((member-type
           (block nil
             (unless unpaired
               (when (singleton-p (xset-data xset))
                 (case (first (xset-data xset))
                   ((nil) (return *null-type*))
                   ((t) (return *eql-t-type*))))
               ;; Semantically this is fine - XSETs
               ;; are not order-preserving except by accident
               ;; (when not represented as a hash-table).
               (when (or (equal (xset-data xset) '(t nil))
                         (equal (xset-data xset) '(nil t)))
                 (return *boolean-type*)))
             (when (or unpaired (not (xset-empty-p xset)))
               (let ((result (%make-member-type xset unpaired)))
                 (setf (type-hash-value result)
                       (logior (type-hash-value result)
                               +type-admits-type=-optimization+))
                 result)))))
      ;; The actual member-type contains the XSET (with no FP zeroes),
      ;; and a list of unpaired zeroes.
      (if float-types
          (make-union-type t (if member-type
                                 (cons member-type float-types)
                                 float-types))
          (or member-type *empty-type*)))))

(defun member-type-size (type)
  (+ (length (member-type-fp-zeroes type))
     (xset-count (member-type-xset type))))

(defun member-type-member-p (x type)
  (if (fp-zero-p x)
      (and (member x (member-type-fp-zeroes type)) t)
      (xset-member-p x (member-type-xset type))))

(defun mapcar-member-type-members (function type)
  (declare (function function))
  (collect ((results))
    (map-xset (lambda (x)
                (results (funcall function x)))
              (member-type-xset type))
    (dolist (zero (member-type-fp-zeroes type))
      (results (funcall function zero)))
    (results)))

(defun mapc-member-type-members (function type)
  (declare (function function))
  (map-xset function (member-type-xset type))
  (dolist (zero (member-type-fp-zeroes type))
    (funcall function zero)))

(defun member-type-members (type)
  (append (member-type-fp-zeroes type)
          (xset-members (member-type-xset type))))

;;; A COMPOUND-TYPE is a type defined out of a set of types, the
;;; common parent of UNION-TYPE and INTERSECTION-TYPE.
(defstruct (compound-type (:include ctype)
                          (:constructor nil)
                          (:copier nil))
  ;; Formerly defined in every CTYPE, but now just in the ones
  ;; for which enumerability is variable.
  (enumerable nil :read-only t)
  (types nil :type list :read-only t))

;;; A UNION-TYPE represents a use of the OR type specifier which we
;;; couldn't canonicalize to something simpler. Canonical form:
;;;   1. All possible pairwise simplifications (using the UNION2 type
;;;      methods) have been performed. Thus e.g. there is never more
;;;      than one MEMBER-TYPE component. FIXME: As of sbcl-0.6.11.13,
;;;      this hadn't been fully implemented yet.
;;;   2. There are never any UNION-TYPE components.
;;;
;;; TODO: As STRING is an especially important union type,
;;; it could be interned by canonicalizing its subparts into
;;; ARRAY of {CHARACTER,BASE-CHAR,NIL} in that exact order always.
;;; It will therefore admit quick TYPE=, but not quick failure, since
;;;   (type= (specifier-type '(or (simple-array (member #\a) (*))
;;;                               (simple-array character (*))
;;;                               (simple-array nil (*))))
;;;          (specifier-type 'simple-string)) => T and T
;;; even though (MEMBER #\A) is not TYPE= to BASE-CHAR.
;;;
(defstruct (union-type (:include compound-type
                                 (class-info (type-class-or-lose 'union)))
                       (:constructor make-union-type (enumerable types))
                       (:copier nil)))

;;; An INTERSECTION-TYPE represents a use of the AND type specifier
;;; which we couldn't canonicalize to something simpler. Canonical form:
;;;   1. All possible pairwise simplifications (using the INTERSECTION2
;;;      type methods) have been performed. Thus e.g. there is never more
;;;      than one MEMBER-TYPE component.
;;;   2. There are never any INTERSECTION-TYPE components: we've
;;;      flattened everything into a single INTERSECTION-TYPE object.
;;;   3. There are never any UNION-TYPE components. Either we should
;;;      use the distributive rule to rearrange things so that
;;;      unions contain intersections and not vice versa, or we
;;;      should just punt to using a HAIRY-TYPE.
(defstruct (intersection-type (:include compound-type
                                        (class-info (type-class-or-lose
                                                     'intersection)))
                              (:constructor %make-intersection-type
                                            (enumerable types))
                              (:copier nil)))

;;; Return TYPE converted to canonical form for a situation where the
;;; "type" '* (which SBCL still represents as a type even though ANSI
;;; CL defines it as a related but different kind of placeholder) is
;;; equivalent to type T.
(defun type-*-to-t (type)
  (if (type= type *wild-type*)
      *universal-type*
      type))

;;; A CONS-TYPE is used to represent a CONS type.
(defstruct (cons-type (:include ctype (class-info (type-class-or-lose 'cons)))
                      (:constructor
                       %make-cons-type (car-type
                                        cdr-type))
                      (:copier nil))
  ;; the CAR and CDR element types (to support ANSI (CONS FOO BAR) types)
  (car-type (missing-arg) :type ctype :read-only t)
  (cdr-type (missing-arg) :type ctype :read-only t))

;; The function caches work significantly better when there
;; is a unique object that stands for the specifier (CONS T T).
(defglobal *cons-t-t-type* -1)
#+sb-xc (declaim (type ctype *cons-t-t-type*))

(defun !intern-important-cons-type-instances ()
  (setf *cons-t-t-type*
        (mark-ctype-interned
         (%make-cons-type *universal-type* *universal-type*))))

#+sb-xc-host
(declaim (ftype (sfunction (ctype ctype) (values t t)) type=))
(defun make-cons-type (car-type cdr-type)
  (aver (not (or (eq car-type *wild-type*)
                 (eq cdr-type *wild-type*))))
  (cond ((or (eq car-type *empty-type*)
             (eq cdr-type *empty-type*))
         *empty-type*)
        ;; It's not a requirement that (CONS T T) be interned,
        ;; but it improves the hit rate in the function caches.
        ((and (type= car-type *universal-type*)
              (type= cdr-type *universal-type*))
         *cons-t-t-type*)
        (t
         (%make-cons-type car-type cdr-type))))

;;; A SIMD-PACK-TYPE is used to represent a SIMD-PACK type.
#!+sb-simd-pack
(defstruct (simd-pack-type
            (:include ctype (class-info (type-class-or-lose 'simd-pack)))
            (:constructor %make-simd-pack-type (element-type))
            (:copier nil))
  (element-type (missing-arg)
   :type (cons #||(member #.*simd-pack-element-types*) ||#)
   :read-only t))

#!+sb-simd-pack
(defun make-simd-pack-type (element-type)
  (aver (neq element-type *wild-type*))
  (if (eq element-type *empty-type*)
      *empty-type*
      (%make-simd-pack-type
       (dolist (pack-type *simd-pack-element-types*
                          (error "~S element type must be a subtype of ~
                                     ~{~S~#[~;, or ~:;, ~]~}."
                                 'simd-pack *simd-pack-element-types*))
         (when (csubtypep element-type (specifier-type pack-type))
           (return (list pack-type)))))))


;;;; type utilities

;;; Return the type structure corresponding to a type specifier. We
;;; pick off structure types as a special case.
;;;
;;; Note: VALUES-SPECIFIER-TYPE-CACHE-CLEAR must be called whenever a
;;; type is defined (or redefined).
;;; This cache is sized extremely generously, which has payoff
;;; elsewhere: it improves the TYPE= and CSUBTYPEP functions,
;;; since EQ types are an immediate win.
;;;
;;; KLUDGE: why isn't this a MACROLET?  "lexical environment too
;;; hairy"
(defmacro !values-specifier-type-body (arg)
  `(let* ((u (uncross ,arg))
          (cachep t)
          (result (or (info :type :builtin u)
                      (let ((spec (typexpand u)))
                        (when (and (symbolp u) (deprecated-thing-p 'type u))
                          (setf cachep nil)
                          (signal 'parse-deprecated-type :specifier u))
                        (cond
                          ((and (not (eq spec u))
                                (info :type :builtin spec)))
                          ((and (consp spec) (symbolp (car spec))
                                (info :type :builtin (car spec))
                                (let ((expander (info :type :expander (car spec))))
                                  (and expander (values-specifier-type (funcall expander spec))))))
                          ((eq (info :type :kind spec) :instance)
                           (find-classoid spec))
                          ((typep spec 'classoid)
                           (if (typep spec 'built-in-classoid)
                               (or (built-in-classoid-translation spec) spec)
                               spec))
                          (t
                           (when (and (atom spec)
                                      (member spec '(and or not member eql satisfies values)))
                             (error "The symbol ~S is not valid as a type specifier." spec))
                           (let ((fun-or-ctype
                                  (info :type :translator (if (consp spec) (car spec) spec))))
                             (cond ((functionp fun-or-ctype)
                                    (funcall fun-or-ctype (ensure-list spec)))
                                   (fun-or-ctype)
                                   ((or (and (consp spec) (symbolp (car spec))
                                             (not (info :type :builtin (car spec))))
                                        (and (symbolp spec) (not (info :type :builtin spec))))
                                    (when (and *type-system-initialized*
                                               (not (eq (info :type :kind spec)
                                                        :forthcoming-defclass-type)))
                                      (signal 'parse-unknown-type :specifier spec))
                                    (setf cachep nil)
                                    (make-unknown-type :specifier spec))
                                   (t
                                    (error "bad thing to be a type specifier: ~S"
                                           spec))))))))))
     (if cachep
         result
         ;; (The RETURN-FROM here inhibits caching; this does not only
         ;; make sense from a compiler diagnostics point of view but
         ;; is also indispensable for proper workingness of
         ;; VALID-TYPE-SPECIFIER-P.)
         (return-from values-specifier-type
           result))))
#+sb-xc-host
(let ((table (make-hash-table :test 'equal)))
  (defun values-specifier-type (specifier)
    (multiple-value-bind (type yesp) (gethash specifier table)
      (if yesp
          type
          (setf (gethash specifier table)
                (!values-specifier-type-body specifier)))))
  (defun values-specifier-type-cache-clear ()
    (clrhash table)))
#-sb-xc-host
(defun-cached (values-specifier-type
               :hash-function #'sxhash :hash-bits 10)
    ((orig equal-but-no-car-recursion))
  (!values-specifier-type-body orig))

;;; This is like VALUES-SPECIFIER-TYPE, except that we guarantee to
;;; never return a VALUES type.
(defun specifier-type (type-specifier)
  (let ((ctype (values-specifier-type type-specifier)))
    (when (or (values-type-p ctype)
              ;; bootstrap magic :-(
              (and (named-type-p ctype)
                   (eq (named-type-name ctype) '*)))
      (error "VALUES type illegal in this context:~%  ~S" type-specifier))
    ctype))

(defun single-value-specifier-type (x)
  (if (eq x '*)
      *universal-type*
      (specifier-type x)))

(defun typexpand-1 (type-specifier &optional env)
  #!+sb-doc
  "Takes and expands a type specifier once like MACROEXPAND-1.
Returns two values: the expansion, and a boolean that is true when
expansion happened."
  (declare (type type-specifier type-specifier))
  (declare (ignore env))
  (let* ((spec type-specifier)
         (atom (if (listp spec) (car spec) spec))
         (expander (and (symbolp atom) (info :type :expander atom))))
               ;; We do not expand builtins even though it'd be
               ;; possible to do so sometimes (e.g. STRING) for two
               ;; reasons:
               ;;
               ;; a) From a user's point of view, CL types are opaque.
               ;;
               ;; b) so (EQUAL (TYPEXPAND 'STRING) (TYPEXPAND-ALL 'STRING))
    (if (and expander (not (info :type :builtin atom)))
        (values (funcall expander (if (symbolp spec) (list spec) spec)) t)
        (values type-specifier nil))))

(defun typexpand (type-specifier &optional env)
  #!+sb-doc
  "Takes and expands a type specifier repeatedly like MACROEXPAND.
Returns two values: the expansion, and a boolean that is true when
expansion happened."
  (declare (type type-specifier type-specifier))
  (multiple-value-bind (expansion flag)
      (typexpand-1 type-specifier env)
    (if flag
        (values (typexpand expansion env) t)
        (values expansion flag))))

;;; Note that the type NAME has been (re)defined, updating the
;;; undefined warnings and VALUES-SPECIFIER-TYPE cache.
(defun %note-type-defined (name)
  (declare (symbol name))
  (note-name-defined name :type)
  (values-specifier-type-cache-clear)
  (values))


(!defun-from-collected-cold-init-forms !early-type-cold-init)
