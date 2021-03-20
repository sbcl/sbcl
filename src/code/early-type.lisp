;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-KERNEL")

;; The following macros expand into either constructor calls,
;; if building the cross-compiler, or forms which reference
;; previously constructed objects, if running the cross-compiler.
#+sb-xc-host
(progn
  (defmacro literal-ctype (constructor &optional specifier)
    (declare (ignore specifier))
    `(load-time-value ,constructor))

  (defmacro literal-ctype-vector (var)
    `(load-time-value ,var nil)))

;; Omitting the specifier works only if the unparser method has been
;; defined in time to use it, and you're sure that constructor's result
;; can be unparsed - some unparsers may be confused if called on a
;; non-canonical object, such as an instance of (CONS T T) that is
;; not EQ to the interned instance.
#-sb-xc-host
(progn
  (defmacro literal-ctype (constructor &optional (specifier nil specifier-p))
    (if specifier-p (specifier-type specifier) (symbol-value constructor)))

  (defmacro literal-ctype-vector (var)
    (symbol-value var)))

(!begin-collecting-cold-init-forms)

;;;; representations of types

;; ENUMERABLE-P is T because a hairy type could be equivalent to a MEMBER type.
;; e.g. any SATISFIES with a predicate returning T over a finite domain.
;; But in practice there's nothing that can be done with this information,
;; because we don't call random predicates when performing operations on types
;; as objects, only when checking for inclusion of something in the type.
(define-type-class hairy :enumerable t :might-contain-other-types t)

;;; Without some special HAIRY cases, we massively pollute the type caches
;;; with objects that are all equivalent to *EMPTY-TYPE*. e.g.
;;;  (AND (SATISFIES LEGAL-FUN-NAME-P) (SIMPLE-ARRAY CHARACTER (*))) and
;;;  (AND (SATISFIES KEYWORDP) CONS). Since the compiler doesn't know
;;; that they're just *EMPTY-TYPE*, its keeps building more and more complex
;;; expressions involving them. I'm not sure why those two are so prevalent
;;; but they definitely seem to be.  We can improve performance by reducing
;;; them to *EMPTY-TYPE* which means we need a way to recognize those hairy
;;; types in order reason about them. Interning them is how we recognize
;;; them, as they can be compared by EQ.
#+sb-xc-host
(progn
  (defvar *satisfies-keywordp-type*
    (!make-interned-hairy-type '(satisfies keywordp)))
  (defvar *fun-name-type*
    (!make-interned-hairy-type '(satisfies legal-fun-name-p))))

;;; An UNKNOWN-TYPE is a type not known to the type system (not yet
;;; defined). We make this distinction since we don't want to complain
;;; about types that are hairy but defined.
(defstruct (unknown-type (:include hairy-type (%bits (pack-ctype-bits hairy)))
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
  (aver (symbolp type))
  (with-unique-names (new-type)
    `(let ((,new-type (maybe-reparse-specifier ,type)))
       (when ,new-type
         (setf ,type ,new-type)
         t))))

(defstruct (negation-type (:include ctype (%bits (pack-ctype-bits negation)))
                          (:copier nil)
                          (:constructor make-negation-type (type)))
  (type (missing-arg) :type ctype :read-only t))

;; Former comment was:
;;   FIXME: is this right?  It's what they had before, anyway
;; But I think the reason it's right is that "enumerable :t" is equivalent
;; to "maybe" which is actually the conservative assumption, same as HAIRY.
(define-type-class negation :enumerable t :might-contain-other-types t)

(defun canonicalize-args-type-args (required optional rest &optional keyp)
  (when (eq rest *empty-type*)
    ;; or vice-versa?
    (setq rest nil))
  (loop with last-not-rest = nil
        for i from 0
        for opt in optional
        do (cond ((eq opt *empty-type*)
                  (return (values required (subseq optional 0 i) rest)))
                 ((and (not keyp) (neq opt rest))
                  (setq last-not-rest i)))
        finally (return (values required
                                (cond (keyp
                                       optional)
                                      (last-not-rest
                                       (subseq optional 0 (1+ last-not-rest))))
                                rest))))

;;; CONTEXT is the cookie passed down from the outermost surrounding call
;;; of BASIC-PARSE-TYPE. INNER-CONTEXT-KIND is an indicator of whether
;;; we are currently parsing a FUNCTION or a VALUES compound type specifier.

;;; Why do we allow * for items in a list for FUNCTION type? I don't know
;;; and I don't think we should.
;;; VALUES is quite clear that it's not allowed. FUNCTION is less clear,
;;; but * is not a type specifier, it is merely a way to write something in a place
;;; where a specifier requires a positional argument that you don't want to supply,
;;; but must supply in order to supply following arguments.
;;; I would bet we're overly permissive.
(defun parse-args-types (context lambda-listy-thing inner-context-kind)
  (multiple-value-bind (llks required optional rest keys)
      (parse-lambda-list
       lambda-listy-thing
       :context inner-context-kind
       :accept (ecase inner-context-kind
                 (:values-type (lambda-list-keyword-mask '(&optional &rest)))
                 (:function-type (lambda-list-keyword-mask
                                  '(&optional &rest &key &allow-other-keys))))
       :silent t)
   (labels ((parse-list (list) (mapcar #'parse-one list))
            (parse-one (x)
              (if (eq inner-context-kind :function-type)
                  (single-value-specifier-type x context) ; allow *
                  ;; "* is not permitted as an argument to the VALUES type specifier."
                  (specifier-type x context 'values)))) ; forbid *
    (let ((required (parse-list required))
          (optional (parse-list optional))
          (rest (when rest (parse-one (car rest))))
          (keywords
           (collect ((key-info))
             (dolist (key keys)
               (unless (proper-list-of-length-p key 2)
                 (error "Keyword type description is not a two-list: ~S." key))
               (let ((kwd (first key)))
                 (when (find kwd (key-info) :key #'key-info-name)
                   (error (sb-format:tokens
                           "~@<repeated keyword ~S in lambda list: ~2I~_~
                            ~/sb-impl:print-lambda-list/~:>")
                          kwd lambda-listy-thing))
                 (key-info
                  (make-key-info
                   ;; MAKE-KEY-INFO will complain if KWD is not a symbol.
                   ;; That's good enough - we don't need an extra check here.
                   :name kwd
                   :type (single-value-specifier-type (second key) context)))))
             (key-info))))
      (multiple-value-bind (required optional rest)
          (canonicalize-args-type-args required optional rest
                                       (ll-kwds-keyp llks))
        (values llks required optional rest keywords))))))

(defstruct (values-type
            (:include args-type (%bits (pack-ctype-bits values)))
            (:constructor %make-values-type)
            (:copier nil)))

(declaim (freeze-type values-type))

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

(define-type-class values :enumerable nil
                    :might-contain-other-types nil)

(define-type-class function :enumerable nil
                    :might-contain-other-types nil)

#+sb-xc-host
(defvar *interned-fun-types*
  (flet ((fun-type (n)
           (!make-interned-fun-type (pack-interned-ctype-bits 'function)
                                    (make-list n :initial-element *universal-type*)
                                    nil nil nil nil nil nil *wild-type*)))
    (vector (fun-type 0) (fun-type 1) (fun-type 2) (fun-type 3))))

(defun make-fun-type (&key required optional rest
                           keyp keywords allowp
                           wild-args returns
                           designator)
  (let ((rest (if (eq rest *empty-type*) nil rest))
        (n (length required)))
    (cond (designator
           (make-fun-designator-type required optional rest keyp keywords
                                     allowp wild-args returns))
          ((and
            (<= n 3)
            (not optional) (not rest) (not keyp)
            (not keywords) (not allowp) (not wild-args)
            (eq returns *wild-type*)
            (not (find *universal-type* required :test #'neq)))
           (svref (literal-ctype-vector *interned-fun-types*) n))
          (t
           (%make-fun-type required optional rest keyp keywords
                           allowp wild-args returns)))))

;; This seems to be used only by cltl2, and within 'cross-type',
;; where it is never used, which makes sense, since pretty much we
;; never want this object, but instead the classoid FUNCTION
;; if we know nothing about a function's signature.
;; Maybe this should not exist unless cltl2 is loaded???
(define-load-time-global *universal-fun-type*
  (make-fun-type :wild-args t :returns *wild-type*))

;;; The CONSTANT-TYPE structure represents a use of the CONSTANT-ARG
;;; "type specifier", which is only meaningful in function argument
;;; type specifiers used within the compiler. (It represents something
;;; that the compiler knows to be a constant.)
(defstruct (constant-type
            (:include ctype (%bits (pack-ctype-bits constant)))
            (:copier nil))
  ;; The type which the argument must be a constant instance of for this type
  ;; specifier to win.
  (type (missing-arg) :type ctype :read-only t))

(define-type-class number :enumerable #'numeric-type-enumerable
                    :might-contain-other-types nil)

(defun interned-numeric-type (specifier &rest args)
  (apply '%make-numeric-type
         :%bits (pack-interned-ctype-bits
                 'number nil
                 (when specifier (sb-vm::saetp-index-or-lose specifier)))
         args))

#+sb-xc-host
(progn
  ;; Work around an ABCL bug. This fails to load:
  ;;   (macrolet ((foo-it (x) `(- ,x))) (defvar *var* (foo-it 3)))
  (defvar *interned-signed-byte-types*)
  (defvar *interned-unsigned-byte-types*)
  (macrolet ((int-type (low high)
               `(interned-numeric-type (when (sb-c::find-saetp spec) spec)
                                       :class 'integer :enumerable t
                                       :low ,low :high ,high)))
    (setq *interned-signed-byte-types*
          (do ((v (make-array sb-vm:n-word-bits))
               (i 1 (1+ i))
               (j -1))
              ((> i sb-vm:n-word-bits) v)
            (let ((spec (if (= i sb-vm:n-fixnum-bits)
                            'fixnum
                            `(signed-byte ,i))))
              (setf (svref v (1- i)) (int-type j (lognot j))
                    j (ash j 1)))))
    (setq *interned-unsigned-byte-types*
          (let ((v (make-array (1+ sb-vm:n-word-bits))))
            (dotimes (i (length v) v)
              (let ((spec (if (= i 1) 'bit `(unsigned-byte ,i))))
                (setf (svref v i) (int-type 0 (1- (ash 1 i))))))))))

;;; Coerce a numeric type bound to the given type while handling
;;; exclusive bounds.
(defun coerce-numeric-bound (bound type)
  (flet ((c (thing)
           (case type
             (rational (rational thing))
             ((float single-float)
              (cond #-sb-xc-host
                    ((<= most-negative-single-float thing most-positive-single-float)
                     (coerce thing 'single-float))
                    (t
                     (return-from coerce-numeric-bound))))
             (double-float
              (cond #-sb-xc-host
                    ((<= most-negative-double-float thing most-positive-double-float)
                     (coerce thing 'double-float))
                    (t
                     (return-from coerce-numeric-bound)))))))
    (when bound
      (if (consp bound)
          (list (c (car bound)))
          (c bound)))))

(declaim (inline bounds-unbounded-p))
(defun bounds-unbounded-p (low high)
  (and (null low) (eq high low)))

;;; Impose canonicalization rules for NUMERIC-TYPE. Note that in some
;;; cases, despite the name, we return *EMPTY-TYPE* or a UNION-TYPE instead of a
;;; NUMERIC-TYPE.
;;;
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
  (declare (type (member integer rational float nil) class))
  (macrolet ((unionize (types classes formats)
               `(let (types)
                  (loop for thing in ',types
                        for class in ',classes
                        for format in ',formats
                        do
                        (let ((low (coerce-numeric-bound low thing))
                              (high (coerce-numeric-bound high thing)))
                          (push (make-numeric-type
                                 :format format
                                 :class class
                                 :complexp complexp
                                 :low low
                                 :high high
                                 :enumerable enumerable)
                                types)))
                  (apply #'type-union types))))
    (when (and (null class) (member complexp '(:real :complex)))
      (return-from make-numeric-type
        (if (bounds-unbounded-p low high)
            (if (eq complexp :complex)
                (specifier-type 'complex)
                (specifier-type 'real))
            (unionize (rational single-float double-float)
                      (rational float float)
                      (nil single-float double-float)))))
    (when (and (eql class 'float) (member complexp '(:complex :real)) (eql format nil))
      (return-from make-numeric-type
        (if (bounds-unbounded-p low high)
            (if (eq complexp :complex)
                (specifier-type '(complex float))
                (specifier-type 'float))
            (unionize (single-float double-float #+long-float (error "long-float"))
                      (float float)
                      (single-float double-float)))))
    (when (and (null complexp)
               (or class format low high))
      (return-from make-numeric-type
        (type-union (make-numeric-type :class class :format format
                                       :low low :high high :enumerable enumerable
                                       :complexp :complex)
                    (make-numeric-type :class class :format format
                                       :low low :high high :enumerable enumerable
                                       :complexp :real)))))
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
    ;; if interval is empty
    (when (and low high
               (if (or (consp low) (consp high)) ; if either bound is exclusive
                   (sb-xc:>= (type-bound-number low) (type-bound-number high))
                   (sb-xc:> low high)))
      (return-from make-numeric-type *empty-type*))
    (when (and (eq class 'rational) (integerp low) (eql low high))
      (setf class 'integer))
    ;; Either lookup the canonical interned object for
    ;; a point in the type lattice, or construct a new one.
    (or (case class
          (float
           (macrolet ((float-type (fmt complexp
                                       &aux (spec (if (eq complexp :complex)
                                                      `(complex ,fmt) fmt)))
                        `(literal-ctype (interned-numeric-type ',spec
                                                               :class 'float :complexp ,complexp
                                                               :format ',fmt :enumerable nil)
                                        ,spec)))
             (when (bounds-unbounded-p low high)
               (ecase format
                 (single-float
                  (case complexp
                    (:real    (float-type single-float :real))
                    (:complex (float-type single-float :complex))))
                 (double-float
                  (case complexp
                    (:real    (float-type double-float :real))
                    (:complex (float-type double-float :complex))))))))
          (integer
           (macrolet ((int-type (low high)
                        `(literal-ctype
                          (interned-numeric-type nil
                                                 :class 'integer :low ,low :high ,high
                                                 :enumerable (if (and ,low ,high) t nil))
                          (integer ,(or low '*) ,(or high '*)))))
             (cond ((neq complexp :real) nil)
                   ((and (eql low 0) (eql high (1- array-dimension-limit)))
                    (int-type 0 #.(1- array-dimension-limit))) ; INDEX type
                   ((null high)
                    (cond ((not low) (int-type nil nil))
                          ((eql low 0) (int-type 0 nil))
                          ((eql low (1+ most-positive-fixnum))
                           ;; positive bignum
                           (int-type #.(1+ most-positive-fixnum) nil))))
                   ((or (eql high most-positive-word)
                        ;; is (1+ high) a power-of-2 ?
                        (and (typep high 'word) (zerop (logand (1+ high) high))))
                    (cond ((eql low 0)
                           (svref (literal-ctype-vector *interned-unsigned-byte-types*)
                                  (integer-length (truly-the word high))))
                          ((and (< high most-positive-word) (eql low (lognot high)))
                           (svref (literal-ctype-vector *interned-signed-byte-types*)
                                  (integer-length (truly-the word high))))))
                   ((and (not low) (eql high (1- most-negative-fixnum)))
                    ;; negative bignum
                    (int-type nil #.(1- most-negative-fixnum))))))
          (rational
           (cond ((and (eq complexp :real) (bounds-unbounded-p low high))
                  (literal-ctype (interned-numeric-type nil :class 'rational)
                      rational))
                 ((and (eq complexp :complex) (bounds-unbounded-p low high))
                  (literal-ctype (interned-numeric-type nil :complexp :complex
                                                            :class 'rational)
                      (complex rational)))))
          ((nil)
           (and (not format)
                (not complexp)
                (bounds-unbounded-p low high)
                (literal-ctype (interned-numeric-type nil :complexp nil) number))))
        (%make-numeric-type :class class :format format :complexp complexp
                            :low low :high high :enumerable enumerable))))

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

(defun make-character-set-type (pairs)
  ; (aver (equal (mapcar #'car pairs)
  ;              (sort (mapcar #'car pairs) #'<)))
  ;; aver that the cars of the list elements are sorted into increasing order
  (when pairs
    (do ((p pairs (cdr p)))
        ((null (cdr p)))
      (aver (<= (caar p) (caadr p)))))
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
                      ((>= low char-code-limit))
                      ((< high 0))
                      (t (push (cons (max 0 low)
                                     (min high (1- char-code-limit)))
                               result))))))))
    (unless pairs
      (return-from make-character-set-type *empty-type*))
    (unless (cdr pairs)
      (macrolet ((range (low high &optional saetp-index)
                   `(return-from make-character-set-type
                      (literal-ctype (!make-interned-character-set-type
                                      (pack-interned-ctype-bits 'character-set nil ,saetp-index)
                                      '((,low . ,high)))
                                     (character-set ((,low . ,high)))))))
        (let* ((pair (car pairs))
               (low (car pair))
               (high (cdr pair)))
          (cond ((eql high (1- char-code-limit))
                 (cond ((eql low 0)
                        (range 0 #.(1- char-code-limit)
                               (sb-vm::saetp-index-or-lose 'character)))
                       #+sb-unicode
                       ((eql low base-char-code-limit)
                        (range #.base-char-code-limit
                               #.(1- char-code-limit)))))
                #+sb-unicode
                ((and (eql low 0) (eql high (1- base-char-code-limit)))
                 (range 0 #.(1- base-char-code-limit)
                        (sb-vm::saetp-index-or-lose 'base-char)))))))
    (%make-character-set-type pairs)))

(define-type-class array :enumerable nil
                    :might-contain-other-types nil)

;; For all ctypes which are the element types of specialized arrays,
;; 3 ctype objects are stored for the rank-1 arrays of that specialization,
;; one for each of simple, maybe-simple, and non-simple (in that order),
;; and 2 ctype objects for unknown-rank arrays, one each for simple
;; and maybe-simple. (Unknown rank, known-non-simple isn't important)
#+sb-xc-host
(progn
(defvar *interned-array-types*
  (labels ((make-1 (type-index dims complexp type)
             (aver (= (type-saetp-index type) type-index))
             (!make-interned-array-type (pack-interned-ctype-bits 'array)
                                        dims complexp type type))
           (make-all (element-type type-index array)
             (replace array
                      (list (make-1 type-index '(*) nil    element-type)
                            (make-1 type-index '(*) :maybe element-type)
                            (make-1 type-index '(*) t      element-type)
                            (make-1 type-index '*   nil    element-type)
                            (make-1 type-index '*   :maybe element-type))
                      :start1 (* type-index 5)))
           (integer-range (low high)
             (make-numeric-type :class 'integer :complexp :real
                                :enumerable t :low low :high high)))
    (let ((array (make-array (* 32 5)))
          (index 0))
      ;; Index 31 is available to store *WILD-TYPE*
      ;; because there are fewer than 32 array widetags.
      (make-all *wild-type* 31 array)
      (dovector (saetp sb-vm:*specialized-array-element-type-properties*
                       (progn (aver (< index 31)) array))
        (make-all
         (let ((x (sb-vm:saetp-specifier saetp)))
           ;; Produce element-type representation without parsing a spec.
           ;; (SPECIFIER-TYPE doesn't work when bootstrapping.)
           ;; The MAKE- constructors return an interned object as appropriate.
           (etypecase x
             ((cons (eql unsigned-byte))
              (integer-range 0 (1- (ash 1 (second x)))))
             ((cons (eql signed-byte))
              (let ((lim (ash 1 (1- (second x)))))
                (integer-range (- lim) (1- lim))))
             ((eql bit) (integer-range 0 1))
             ;; FIXNUM is its own thing, why? See comment in vm-array
             ;; saying to "See the comment in PRIMITIVE-TYPE-AUX"
             ((eql fixnum) ; One good kludge deserves another.
              (integer-range most-negative-fixnum
                             most-positive-fixnum))
             ((member single-float double-float)
              (make-numeric-type :class 'float :format x :complexp :real))
             ((cons (eql complex))
              (make-numeric-type :class 'float :format (cadr x)
                                 :complexp :complex))
             ((eql character)
              (make-character-set-type `((0 . ,(1- char-code-limit)))))
             #+sb-unicode
             ((eql base-char)
              (make-character-set-type `((0 . ,(1- base-char-code-limit)))))
             ((eql t) *universal-type*)
             ((eql nil) *empty-type*)))
         index
         array)
        (incf index)))))
(defvar *parsed-specialized-array-element-types*
  (let ((a (make-array (length sb-vm:*specialized-array-element-type-properties*))))
    (loop for i below (length a)
          do (setf (aref a i) (array-type-specialized-element-type
                               (aref *interned-array-types* (* i 5)))))
    a)))

(declaim (ftype (sfunction (t &key (:complexp t)
                                   (:element-type t)
                                   (:specialized-element-type t))
                           ctype) make-array-type))
(defun make-array-type (dimensions &key (complexp :maybe) element-type
                                        (specialized-element-type *wild-type*))
  (if (and (eq element-type specialized-element-type)
           (or (and (eq dimensions '*) (neq complexp t))
               (typep dimensions '(cons (eql *) null))))
      (let ((res (svref (literal-ctype-vector *interned-array-types*)
                        (+ (* (type-saetp-index element-type) 5)
                           (if (listp dimensions) 0 3)
                           (ecase complexp ((nil) 0) ((:maybe) 1) ((t) 2))))))
        (aver (eq (array-type-element-type res) element-type))
        res)
      (%make-array-type dimensions
                        complexp element-type specialized-element-type)))

(define-type-class member :enumerable t
                    :might-contain-other-types nil)

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
          (let ((sign (float-sign-bit z))
                (pair-idx
                  (etypecase z
                    (single-float 0)
                    (double-float 2
                    #+long-float (long-float 4)))))
            (if (= pass 0)
                (setf (ldb (byte 1 (+ pair-idx sign)) presence) 1)
                (if (= (ldb (byte 2 pair-idx) presence) #b11)
                    (when (= sign 0)
                      (push (ctype-of z) float-types))
                    (push z unpaired)))))))
    (let ((member-type
           (block nil
             (unless unpaired
               (macrolet ((member-type (&rest elts)
                            `(literal-ctype
                              (!make-interned-member-type
                               (pack-interned-ctype-bits 'member) (xset-from-list ',elts) nil)
                              (member ,@elts))))
                 (let ((elts (xset-data xset)))
                   (when (singleton-p elts)
                     (case (first elts)
                       ((nil) (return (member-type nil)))
                       ((t) (return (member-type t)))))
                   (when (or (equal elts '(t nil)) (equal elts '(nil t)))
                     ;; Semantically this is fine - XSETs
                     ;; are not order-preserving except by accident
                     ;; (when not represented as a hash-table).
                     (return (member-type t nil))))))
             (when (or unpaired (not (xset-empty-p xset)))
               (%make-member-type xset unpaired)))))
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

;;; Return TYPE converted to canonical form for a situation where the
;;; "type" '* (which SBCL still represents as a type even though ANSI
;;; CL defines it as a related but different kind of placeholder) is
;;; equivalent to type T.
(defun type-*-to-t (type)
  (if (type= type *wild-type*)
      *universal-type*
      type))

(define-type-class cons :enumerable nil :might-contain-other-types nil)

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
         (literal-ctype (!make-interned-cons-type (pack-interned-ctype-bits 'cons)
                                                  *universal-type*
                                                  *universal-type*)
                        cons))
        (t
         (%make-cons-type car-type cdr-type))))

;;; A SIMD-PACK-TYPE is used to represent a SIMD-PACK type.
#+sb-simd-pack
(defstruct (simd-pack-type
            (:include ctype (%bits (pack-ctype-bits simd-pack)))
            (:constructor %make-simd-pack-type (element-type))
            (:copier nil))
  (element-type (missing-arg)
   :type (cons #||(member #.*simd-pack-element-types*) ||#)
   :read-only t))

#+sb-simd-pack-256
(defstruct (simd-pack-256-type
            (:include ctype (%bits (pack-ctype-bits simd-pack-256)))
            (:constructor %make-simd-pack-256-type (element-type))
            (:copier nil))
  (element-type (missing-arg)
   :type (cons #||(member #.*simd-pack-element-types*) ||#)
   :read-only t))

#+sb-simd-pack
(defun make-simd-pack-type (element-type)
  (aver (neq element-type *wild-type*))
  (if (eq element-type *empty-type*)
      *empty-type*
      (%make-simd-pack-type
       (dolist (pack-type *simd-pack-element-types*
                (error "~S element type must be a subtype of ~
                         ~{~/sb-impl:print-type-specifier/~#[~;, or ~
                         ~:;, ~]~}."
                       'simd-pack *simd-pack-element-types*))
         (when (csubtypep element-type (specifier-type pack-type))
           (return (list pack-type)))))))

#+sb-simd-pack-256
(defun make-simd-pack-256-type (element-type)
  (aver (neq element-type *wild-type*))
  (if (eq element-type *empty-type*)
      *empty-type*
      (%make-simd-pack-256-type
       (dolist (pack-type *simd-pack-element-types*
                (error "~S element type must be a subtype of ~
                         ~{~/sb-impl:print-type-specifier/~#[~;, or ~
                         ~:;, ~]~}."
                       'simd-pack-256 *simd-pack-element-types*))
         (when (csubtypep element-type (specifier-type pack-type))
           (return (list pack-type)))))))

;;;; type utilities

;;; Return the type structure corresponding to a type specifier.
;;;
;;; Note: VALUES-SPECIFIER-TYPE-CACHE-CLEAR must be called whenever a
;;; type is defined (or redefined).
;;;
;;; As I understand things, :FORTHCOMING-DEFCLASS-TYPE behaves contrarily
;;; to the CLHS intent, which is to make the type known to the compiler.
;;; If we compile in one file:
;;;  (DEFCLASS FRUITBAT () ())
;;;  (DEFUN FRUITBATP (X) (TYPEP X 'FRUITBAT))
;;; we see that it emits a call to %TYPEP with the symbol FRUITBAT as its
;;; argument, whereas it should involve CLASSOID-CELL-TYPEP and LAYOUT-OF,
;;; which (correctly) signals an error if the class were not defined by the
;;; time of the call. Delayed re-parsing of FRUITBAT into any random specifier
;;; at call time is wrong.
;;;
;;; FIXME: symbols which are :PRIMITIVE are inconsistently accepted as singleton
;;; lists. e.g. (BIT) and (ATOM) are considered legal, but (FIXNUM) and
;;; (CHARACTER) are not. It has to do with whether the primitive is actually
;;; a DEFTYPE. The CLHS glossary implies that the singleton is *always* legal.
;;;  "For every atomic type specifier, x, there is an _equivalent_ [my emphasis]
;;;   compound type specifier with no arguments supplied, (x)."
;;; By that same reasonining, is (x) accepted if x names a class?
;;;

;;; The xc host uses an ordinary hash table for memoization.
#+sb-xc-host
(let ((table (make-hash-table :test 'equal)))
  (defun !values-specifier-type-memo-wrapper (thunk specifier)
    (multiple-value-bind (type yesp) (gethash specifier table)
      (if yesp
          type
          (setf (gethash specifier table) (funcall thunk)))))
  (defun values-specifier-type-cache-clear ()
    (clrhash table)))
;;; This cache is sized extremely generously, which has payoff
;;; elsewhere: it improves the TYPE= and CSUBTYPEP functions,
;;; since EQ types are an immediate win.
#-sb-xc-host
(sb-impl::!define-hash-cache values-specifier-type
  ((orig equal-but-no-car-recursion)) ()
  :hash-function #'sxhash :hash-bits 10)

(declaim (inline make-type-context))
(defstruct (type-context
            (:constructor make-type-context
                          (spec &optional proto-classoid (cacheable t)))
            (:copier nil)
            (:predicate nil))
  (spec nil :read-only t)
  (proto-classoid nil :read-only t)
  (cacheable t))

;;; Maintain a table of symbols designating unknown types that have any references
;;; to them, making it easy to inquire whether such things exist. This is at a lower
;;; layer than the parser cache - it's a cache of the constructor itself - so we'll
;;; sitll signal that an unknown specifier is unknown on each reparse of the same.
;;; But as long as any reference enlivens the relevant CTYPE, we'll return that object.
(defglobal **unknown-type-atoms**
    ;; This table is specified as unsynchronized because we need to wrap the lock
    ;; around a read/modify/write. GETHASH and PUTHASH can't do that themselves.
    (sb-impl::make-system-hash-table :test 'eq :weakness :value :synchronized nil))

#-sb-xc-host
(progn (declaim (inline class-classoid))
       (defun class-classoid (class)
         (layout-classoid (sb-pcl::class-wrapper class))))

;;; Parsing of type specifiers comes in many variations:
;;;  SINGLE-VALUE-SPECIFIER-TYPE:
;;;    disallow VALUES even if single value, but allow *
;;;  SPECIFIER-TYPE:
;;;    disallow (VALUES ...) even if single value, and disallow *
;;;  VALUES-SPECIFIER-TYPE:
;;;    allow VALUES, disallow *
;;; TYPE-OR-NIL-IF-UNKNOWN:
;;;    like SPECIFIER-TYPE, but return NIL if contains unknown
;;; all the above are funneled through BASIC-PARSE-TYPESPEC.

;;; The recursive %PARSE-TYPE function is used for nested invocations
;;; of type spec parsing, passing the outermost context through on each call.
;;; Callers should use the BASIC-PARSE-TYPESPEC interface.

;;; Hint for when you bork this and/or bork the :UNPARSE methods - do:
;;; (remove-method #'print-object (find-method #'print-object nil
;;;    (list (find-class 'ctype) (find-class 't))))
;;; so that 'backtrace' doesn't encounter an infinite chain of errors.

(macrolet ((fail (spec)
             `(error "bad thing to be a type specifier: ~/sb-impl:print-type-specifier/"
                     ,spec)))
(defun %parse-type (spec context)
  (declare (type type-context context))
  (prog* ((head (if (listp spec) (car spec) spec))
          (builtin (if (symbolp head)
                       (info :type :builtin head)
                       (return (fail spec)))))
    (when (deprecated-thing-p 'type head)
      (setf (type-context-cacheable context) nil)
      (signal 'parse-deprecated-type :specifier spec))
    (when (atom spec)
      ;; If spec is non-atomic, the :BUILTIN value is inapplicable.
      ;; There used to be compound builtins, but not any more.
      (when builtin (return builtin))
      ;; Any spec that apparently refers to a defstruct form
      ;; that's being macroexpanded should refer to that type.
      (awhen (type-context-proto-classoid context)
        (when (eq (classoid-name it) spec) (return it)))
      (case (info :type :kind spec)
       (:instance (return (find-classoid spec)))
       (:forthcoming-defclass-type (go unknown))))
    ;; Expansion brings up an interesting question - should the cache
    ;; contain entries for intermediary types? Say A -> B -> REAL.
    ;; As it stands, we cache the ctype corresponding to A but not B.
    (awhen (info :type :expander head)
      (when (listp it) ; The function translates directly to a CTYPE.
        (return (or (funcall (car it) context spec) (fail spec))))
      ;; The function produces a type expression.
      (let ((expansion (funcall it (ensure-list spec))))
        (return (if (typep expansion 'instance)
                    (basic-parse-typespec expansion context)
                    (%parse-type expansion context)))))
    ;; If the spec is (X ...) and X has neither a translator
    ;; nor expander, and is a builtin, such as FIXNUM, fail now.
    ;; But - see FIXME at top - it would be consistent with
    ;; DEFTYPE to reject spec only if not a singleton.
    (when builtin (return (fail spec)))
    ;; SPEC has a legal form, so return an unknown type.
    (signal 'parse-unknown-type :specifier spec)
  UNKNOWN
    (setf (type-context-cacheable context) nil)
    (return (if (atom spec)
                (let ((table **unknown-type-atoms**))
                  (with-system-mutex ((hash-table-lock table))
                    (or (gethash spec table)
                        (progn #+sb-xc-host
                               (when cl:*compile-print*
                                 (format t "~&; NEW UNKNOWN-TYPE ~S~%" spec))
                               (setf (gethash spec table)
                                     (make-unknown-type :specifier spec))))))
                (make-unknown-type :specifier spec)))))

;;; BASIC-PARSE-TYPESPEC can grok some simple cases that involve turning an object
;;; used as a type specifier into an internalized type object (which might be
;;; the selfsame object, in the case of a CLASSOID).
(defun basic-parse-typespec (type-specifier context)
  (declare (type type-context context))
  (when (typep type-specifier 'instance)
    ;; An instance never needs the type parser cache, because it almost always
    ;; represents itself or a slot in itself.
    (flet ((classoid-to-ctype (classoid)
             ;; A few classoids have translations,
             ;; e.g. the classoid CONS is a CONS-TYPE.
             ;; Hmm, perhaps this should signal PARSE-UNKNOWN-TYPE
             ;; if CLASSOID is an instance of UNDEFINED-CLASSOID ?
             ;; Can that happen?
             (or (and (built-in-classoid-p classoid)
                      (built-in-classoid-translation classoid))
                 classoid)))
      (return-from basic-parse-typespec
       (cond ((classoid-p type-specifier) (classoid-to-ctype type-specifier))
             ;; Avoid TYPEP on SB-MOP:EQL-SPECIALIZER and CLASS because
             ;; the fake metaobjects do not allow type analysis, and
             ;; would cause a compiler error as it tries to decide
             ;; whether any clause of this COND subsumes another.
             ;; Moreover, we don't require the host to support MOP.
             #-sb-xc-host
             ((sb-pcl::classp type-specifier)
              ;; A CLOS class is translated to its CLASSOID, or the classoid's translation.
              (classoid-to-ctype (sb-pcl::class-classoid type-specifier)))
             #-sb-xc-host
             ((sb-pcl::eql-specializer-p type-specifier)
              ;; EQL specializers are are seldom used and not 100% portable,
              ;; though they are part of the AMOP.
              ;; See https://sourceforge.net/p/sbcl/mailman/message/11217378/
              ;; We implement the notion that an EQL-SPECIALIZER has-a CTYPE.
              ;; You might think that a cleverer way would be to say that
              ;; EQL-SPECIALIZER is-a CTYPE, i.e. incorporating EQL-SPECIALIZER
              ;; objects into the type machinary. Well, that's a problem -
              ;; it would mess up admissibility of the TYPE= optimization.
              ;; We don't want to create another way of representing
              ;; the type NULL = (MEMBER NIL), for example.
              (sb-pcl::eql-specializer-to-ctype type-specifier))
             ((layout-p type-specifier)
              (layout-classoid type-specifier))
             (t (fail type-specifier))))))
  (when (atom type-specifier)
    ;; Try to bypass the cache, which avoids using a cache line for standard
    ;; atomic specifiers. This is a trade-off- cache seek might be faster,
    ;; but this solves the problem that a full call to (TYPEP #\A 'FIXNUM)
    ;; consed a cache line every time the cache missed on FIXNUM (etc).
    (awhen (info :type :builtin type-specifier)
        (return-from basic-parse-typespec it)))

  ;; If CONTEXT was non-cacheable as supplied, the cache is bypassed
  ;; for any nested lookup, and we don't insert the result.
  (if (not (type-context-cacheable context))
      (%parse-type (uncross type-specifier) context)
      ;; Otherwise, try for a cache hit first, and usually update the cache.
      (!values-specifier-type-memo-wrapper
       (lambda ()
         (let ((answer (%parse-type (uncross type-specifier) context)))
           (if (type-context-cacheable context)
               answer
                 ;; Lookup was cacheable, but result isn't.
                 ;; Non-caching ensures that we see every occurrence of an unknown
                 ;; type no matter how deeply nested it is in the expression.
                 ;; e.g. (OR UNKNOWN-FOO CONS) and (OR INTEGER UNKNOWN-FOO)
                 ;; should both signal the PARSE-UNKNOWN condition, which would
                 ;; not happen if the first cached UNKNOWN-FOO.

                 ;; During make-host-2 I'm seeing the types &OPTIONAL-AND-&KEY-IN-LAMBDA-LIST,
                 ;; SIMPLE-ERROR, DISASSEM-STATE as non-cacheable,
                 ;; and much, much more during make-target-2.
                 ;; The condition types are obvious, because we mention them before
                 ;; defining them.
                 ;; DISASSEM-STATE comes from building **TYPE-SPEC-INTERR-SYMBOLS**
                 ;; where we have a fixed list of types which get assigned single-byte
                 ;; error codes.
               (progn
                   #+nil
                   (unless (type-context-cacheable context)
                     (format t "~&non-cacheable: ~S ~%" type-specifier))
                   (return-from basic-parse-typespec answer)))))
       type-specifier)))
) ; end MACROLET

;;; This takes no CONTEXT (which implies lack of recursion) because
;;; you can't reasonably place a VALUES type inside another type.
(defun values-specifier-type (type-specifier)
  ;; This catches uses of literal '* where it shouldn't appear, but it
  ;; accidentally lets other uses slip through. We'd have to catch '*
  ;; post-type-expansion to be more strict, but it isn't very important.
  (if (eq type-specifier '*)
      (error "* is not permitted as a type specifier")
      (dx-let ((context (make-type-context type-specifier)))
        (basic-parse-typespec type-specifier context))))

;;; This is like VALUES-SPECIFIER-TYPE, except that we guarantee to
;;; never return a VALUES type.
;;; CONTEXT is either an instance of TYPE-CONTEXT or NIL.
;;; SUBCONTEXT is a symbol denoting the head of the current expression, or NIL.
(defun specifier-type (type-specifier &optional context subcontext)
  (let* ((ctype
           (if context
               (basic-parse-typespec type-specifier context)
               (dx-let ((context (make-type-context type-specifier)))
                 (basic-parse-typespec type-specifier context))))
         (wildp (eq ctype *wild-type*)))
    ;; We have to see how it was spelled to give an intelligent message.
    ;; If it's instance of VALUES-TYPE, then it was spelled as VALUES
    ;; whereas if it isn't, the user either spelled it as (VALUES) or *.
    ;; The case where this heuristic doesn't work is a DEFTYPE that expands
    ;; to *, but that's not worth worrying about.
    (cond ((or (values-type-p ctype)
               (and wildp (consp type-specifier)))
           (error "VALUES type illegal in this context:~% ~
               ~/sb-impl:print-type-specifier/"
                  type-specifier))
          (wildp
           (if subcontext
               (error "* is not permitted as an argument to the ~S type specifier"
                      subcontext)
               (error "* is not permitted as a type specifier~@[ in the context ~S~]"
                      ;; If the entire surrounding context is * then there's not much
                      ;; else to say. Otherwise, show the original expression.
                      (when (and context (neq (type-context-spec context) '*))
                        (type-context-spec context))))))
    ctype))

(defun single-value-specifier-type (x &optional context)
  (if (eq x '*)
      *universal-type*
      (specifier-type x context)))

;;; Parse TYPE-SPECIFIER, returning NIL if any sub-part of it is unknown
(defun type-or-nil-if-unknown (type-specifier &optional allow-values)
  (dx-let ((context (make-type-context type-specifier)))
    (let ((result (if allow-values
                      (basic-parse-typespec type-specifier context)
                      (specifier-type type-specifier context))))
      ;; If it was non-cacheable, either it contained a deprecated type
      ;; or unknown type, or was a pending defstruct definition.
      (if (and (not (type-context-cacheable context))
               (contains-unknown-type-p result))
          nil
          result))))

(defun typexpand-1 (type-specifier &optional env)
  "Takes and expands a type specifier once like MACROEXPAND-1.
Returns two values: the expansion, and a boolean that is true when
expansion happened."
  (declare (type type-specifier type-specifier))
  (declare (type lexenv-designator env) (ignore env))
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
    (if (and (functionp expander) (not (info :type :builtin atom)))
        (values (funcall expander (if (symbolp spec) (list spec) spec)) t)
        (values type-specifier nil))))

(defun typexpand (type-specifier &optional env)
  "Takes and expands a type specifier repeatedly like MACROEXPAND.
Returns two values: the expansion, and a boolean that is true when
expansion happened."
  ;; TYPE-SPECIFIER is of type TYPE-SPECIFIER, but it is preferable to
  ;; defer to TYPEXPAND-1 for the typecheck. Similarly for ENV.
  (multiple-value-bind (expansion expanded)
      (typexpand-1 type-specifier env)
    (if expanded
        (values (typexpand expansion env) t)
        (values expansion expanded))))

;;; Note that the type NAME has been (re)defined, updating the
;;; undefined warnings and VALUES-SPECIFIER-TYPE cache.
(defun %note-type-defined (name)
  (declare (symbol name))
  (note-name-defined name :type)
  (values-specifier-type-cache-clear)
  (values))


(!defun-from-collected-cold-init-forms !early-type-cold-init)

;;; When cross-compiling SPECIFIER-TYPE with a quoted argument,
;;; it can be rendered as a literal object unless it mentions
;;; certain classoids.
;;;
;;; This is important for type system initialization.
;;;
;;; After the target is built, we remove this transform, both because calls
;;; to SPECIFIER-TYPE do not arise organically through user code,
;;; and because it is possible that user changes to types could make parsing
;;; return a different thing, e.g. changing a DEFTYPE to a DEFCLASS.
;;;
#+sb-xc-host
(labels ((xform (type-spec env parser)
           (if (not (constantp type-spec env))
               (values nil t)
               (let* ((expr (constant-form-value type-spec env))
                      (parse (funcall parser expr)))
                 (if (cold-dumpable-type-p parse)
                     parse
                     (values nil t)))))
         (cold-dumpable-type-p (ctype)
           (when (contains-unknown-type-p ctype)
             (bug "SPECIFIER-TYPE transform parsed an unknown type: ~S" ctype))
           (map-type (lambda (type)
                       (when (and (classoid-p type) (eq (classoid-name type) 'class))
                         (return-from cold-dumpable-type-p nil)))
                     ctype)
           t))
  (sb-c:define-source-transform specifier-type (type-spec &environment env)
    (xform type-spec env #'specifier-type))
  (sb-c:define-source-transform values-specifier-type (type-spec &environment env)
    (xform type-spec env #'values-specifier-type)))
