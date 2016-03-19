;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

(eval-when (:compile-toplevel #+sb-xc-host :load-toplevel :execute)
  ;; The following macros expand into either constructor calls,
  ;; if building the cross-compiler, or forms which reference
  ;; previously constructed objects, if running the cross-compiler.
  #+sb-xc-host
  (progn
    (defmacro literal-ctype (constructor &optional specifier)
      (declare (ignore specifier))
      ;; Technically the instances are not read-only,
      ;; because the hash-value slot is rewritten.
      `(load-time-value (mark-ctype-interned ,constructor) nil))

    (defmacro literal-ctype-vector (var)
      `(load-time-value ,var nil)))

  #-sb-xc-host
  (progn
    ;; Omitting the specifier works only if the unparser method has been
    ;; defined in time to use it, and you're sure that constructor's result
    ;; can be unparsed - some unparsers may be confused if called on a
    ;; non-canonical object, such as an instance of (CONS T T) that is
    ;; not EQ to the interned instance.
    (sb!xc:defmacro literal-ctype (constructor
                                   &optional (specifier nil specifier-p))
      ;; The source-transform for SPECIFIER-TYPE turns this call into
      ;; (LOAD-TIME-VALUE (!SPECIFIER-TYPE ',specifier)).
      ;; It's best to go through the transform rather than expand directly
      ;; into that, because the transform canonicalizes the spec,
      ;; ensuring correctness of the hash lookups performed during genesis.
      `(specifier-type ',(if specifier-p
                             specifier
                             (type-specifier (symbol-value constructor)))))

    (sb!xc:defmacro literal-ctype-vector (var)
      (let ((vector (symbol-value var)))
        `(truly-the (simple-vector ,(length vector))
          (load-time-value
           (vector ,@(map 'list
                          (lambda (x)
                            (if (ctype-p x)
                                `(!specifier-type ',(type-specifier x))
                                x)) ; allow NIL or 0 in the vector
                          vector)) t))))))

(!begin-collecting-cold-init-forms)

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
    (mark-ctype-interned (%make-hairy-type '(satisfies keywordp))))
  (defvar *fun-name-type*
    (mark-ctype-interned (%make-hairy-type '(satisfies legal-fun-name-p)))))

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
                          (:constructor make-negation-type (type))
                          #!+cmu (:pure nil))
  (type (missing-arg) :type ctype :read-only t))

;; Former comment was:
;;   FIXME: is this right?  It's what they had before, anyway
;; But I think the reason it's right is that "enumerable :t" is equivalent
;; to "maybe" which is actually the conservative assumption, same as HAIRY.
(!define-type-class negation :enumerable t :might-contain-other-types t)

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

;; CONTEXT is the cookie passed down from the outermost surrounding call
;; of VALUES-SPECIFIER-TYPE. INNER-CONTEXT-KIND is an indicator of whether
;; we are currently parsing a FUNCTION or a VALUES compound type specifier.
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
   (flet ((parse-list (list)
            (mapcar (lambda (x) (single-value-specifier-type-r context x))
                    list)))
    (let ((required (parse-list required))
          (optional (parse-list optional))
          (rest (when rest (single-value-specifier-type-r context (car rest))))
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
                   ;; MAKE-KEY-INFO will complain if KWD is not a symbol.
                   ;; That's good enough - we don't need an extra check here.
                   :name kwd
                   :type (single-value-specifier-type-r context (second key))))))
             (key-info))))
      (multiple-value-bind (required optional rest)
          (canonicalize-args-type-args required optional rest
                                       (ll-kwds-keyp llks))
        (values llks required optional rest keywords))))))

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

(!define-type-class function :enumerable nil
                    :might-contain-other-types nil)

#+sb-xc-host
(defvar *interned-fun-types*
  (flet ((fun-type (n)
           (mark-ctype-interned
            (%make-fun-type (make-list n :initial-element *universal-type*)
                            nil nil nil nil nil nil *wild-type*))))
    (vector (fun-type 0) (fun-type 1) (fun-type 2) (fun-type 3))))

(defun make-fun-type (&key required optional rest
                           keyp keywords allowp
                           wild-args returns)
  (let ((rest (if (eq rest *empty-type*) nil rest))
        (n (length required)))
    (if (and (<= n 3)
             (not optional) (not rest) (not keyp)
             (not keywords) (not allowp) (not wild-args)
             (eq returns *wild-type*)
             (not (find *universal-type* required :test #'neq)))
        (svref (literal-ctype-vector *interned-fun-types*) n)
        (%make-fun-type required optional rest keyp keywords
                        allowp wild-args returns))))

;; This seems to be used only by cltl2, and within 'cross-type',
;; where it is never used, which makes sense, since pretty much we
;; never want this object, but instead the classoid FUNCTION
;; if we know nothing about a function's signature.
;; Maybe this should not exist unless cltl2 is loaded???
(defvar *universal-fun-type*
  (make-fun-type :wild-args t :returns *wild-type*))

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

(!define-type-class number :enumerable #'numeric-type-enumerable
                    :might-contain-other-types nil)

#+sb-xc-host
(progn
  ;; Work around an ABCL bug. This fails to load:
  ;;   (macrolet ((foo-it (x) `(- ,x))) (defvar *var* (foo-it 3)))
  (defvar *interned-signed-byte-types*)
  (defvar *interned-unsigned-byte-types*)
  (macrolet ((int-type (low high)
               `(mark-ctype-interned
                 (%make-numeric-type :class 'integer :enumerable t
                                     :low ,low :high ,high))))
    (setq *interned-signed-byte-types*
          (let ((v (make-array sb!vm:n-word-bits))
                (j -1))
            (dotimes (i sb!vm:n-word-bits v)
              (setf (svref v i) (int-type j (lognot j)) j (ash j 1)))))
    (setq *interned-unsigned-byte-types*
          (let ((v (make-array (1+ sb!vm:n-word-bits))))
            (dotimes (i (length v) v)
              (setf (svref v i) (int-type 0 (1- (ash 1 i)))))))))

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
                   (>= (type-bound-number low) (type-bound-number high))
                   (> low high)))
      (return-from make-numeric-type *empty-type*))
    (when (and (eq class 'rational) (integerp low) (eql low high))
      (setf class 'integer))
        ;; Either lookup the canonical interned object for
        ;; a point in the type lattice, or construct a new one.
    (or (case class
          (float
           (macrolet ((float-type (fmt complexp)
                        `(literal-ctype
                          (%make-numeric-type :class 'float :complexp ,complexp
                                              :format ',fmt :enumerable nil)
                          ,(if (eq complexp :complex) `(complex ,fmt) fmt))))
             (when (and (null low) (null high))
               (case format
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
                          (%make-numeric-type
                           :class 'integer :low ,low :high ,high
                           :enumerable (if (and ,low ,high) t nil))
                          (integer ,(or low '*) ,(or high '*)))))
             (cond ((neq complexp :real) nil)
                   ((and (eql low 0) (eql high (1- sb!xc:array-dimension-limit)))
                    (int-type 0 #.(1- sb!xc:array-dimension-limit))) ; INDEX type
                   ((null high)
                    (cond ((not low) (int-type nil nil))
                          ((eql low 0) (int-type 0 nil))
                          ((eql low (1+ sb!xc:most-positive-fixnum))
                           ;; positive bignum
                           (int-type #.(1+ sb!xc:most-positive-fixnum) nil))))
                   ((or (eql high most-positive-word)
                        ;; is (1+ high) a power-of-2 ?
                        (and (typep high 'word) (zerop (logand (1+ high) high))))
                    (cond ((eql low 0)
                           (svref (literal-ctype-vector *interned-unsigned-byte-types*)
                                  (integer-length (truly-the word high))))
                          ((and (< high most-positive-word) (eql low (lognot high)))
                           (svref (literal-ctype-vector *interned-signed-byte-types*)
                                  (integer-length (truly-the word high))))))
                   ((and (not low) (eql high (1- sb!xc:most-negative-fixnum)))
                    ;; negative bignum
                    (int-type nil #.(1- sb!xc:most-negative-fixnum))))))
          (rational
           (when (and (eq complexp :real) (null low) (eq high low))
             (literal-ctype (%make-numeric-type :class 'rational) rational))))
        (let ((result (%make-numeric-type :class class :format format
                                          :complexp complexp
                                          :low low :high high
                                          :enumerable enumerable)))
          (setf (type-hash-value result)
                (logior (type-hash-value result) +type-admits-type=-optimization+))
          result))))

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

;; all character-set types are enumerable, but it's not possible
;; for one to be TYPE= to a MEMBER type because (MEMBER #\x)
;; is not internally represented as a MEMBER type.
;; So in case it wasn't clear already ENUMERABLE-P does not mean
;;  "possibly a MEMBER type in the Lisp-theoretic sense",
;; but means "could be implemented in SBCL as a MEMBER type".
(!define-type-class character-set :enumerable nil
                    :might-contain-other-types nil)

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
                      ((>= low sb!xc:char-code-limit))
                      ((< high 0))
                      (t (push (cons (max 0 low)
                                     (min high (1- sb!xc:char-code-limit)))
                               result))))))))
    (unless pairs
      (return-from make-character-set-type *empty-type*))
    (unless (cdr pairs)
      (macrolet ((range (low high)
                   `(return-from make-character-set-type
                      (literal-ctype (%make-character-set-type '((,low . ,high)))
                                     (character-set ((,low . ,high)))))))
        (let* ((pair (car pairs))
               (low (car pair))
               (high (cdr pair)))
          (cond ((eql high (1- sb!xc:char-code-limit))
                 (cond ((eql low 0) (range 0 #.(1- sb!xc:char-code-limit)))
                       #!+sb-unicode
                       ((eql low base-char-code-limit)
                        (range #.base-char-code-limit
                               #.(1- sb!xc:char-code-limit)))))
                #!+sb-unicode
                ((and (eql low 0) (eql high (1- base-char-code-limit)))
                 (range 0 #.(1- base-char-code-limit)))))))
    (%make-character-set-type pairs)))

(!define-type-class array :enumerable nil
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
             (setf (!ctype-saetp-index type) type-index)
             (mark-ctype-interned (%make-array-type dims complexp type type)))
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
      (dolist (x *specialized-array-element-types*
                 (progn (aver (< index 31)) array))
        (make-all
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
            (integer-range sb!xc:most-negative-fixnum
                           sb!xc:most-positive-fixnum))
           ((member single-float double-float)
            (make-numeric-type :class 'float :format x :complexp :real))
           ((cons (eql complex))
            (make-numeric-type :class 'float :format (cadr x)
                               :complexp :complex))
           ((eql character)
            (make-character-set-type `((0 . ,(1- sb!xc:char-code-limit)))))
           #!+sb-unicode
           ((eql base-char)
            (make-character-set-type `((0 . ,(1- base-char-code-limit)))))
           ((eql t) *universal-type*)
           ((eql nil) *empty-type*))
         index array)
        (incf index)))))
(defvar *parsed-specialized-array-element-types*
  (let ((a (make-array (length *specialized-array-element-types*))))
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
                        (+ (* (!ctype-saetp-index element-type) 5)
                           (if (listp dimensions) 0 3)
                           (ecase complexp ((nil) 0) ((:maybe) 1) ((t) 2))))))
        (aver (eq (array-type-element-type res) element-type))
        res)
      (%make-array-type dimensions
                        complexp element-type specialized-element-type)))

(!define-type-class member :enumerable t
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
               (macrolet ((member-type (&rest elts)
                            `(literal-ctype
                              (%make-member-type (xset-from-list ',elts) nil)
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

;;; Return TYPE converted to canonical form for a situation where the
;;; "type" '* (which SBCL still represents as a type even though ANSI
;;; CL defines it as a related but different kind of placeholder) is
;;; equivalent to type T.
(defun type-*-to-t (type)
  (if (type= type *wild-type*)
      *universal-type*
      type))

(!define-type-class cons :enumerable nil :might-contain-other-types nil)

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
         (literal-ctype (%make-cons-type *universal-type* *universal-type*)
                        cons))
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
(sb!impl::!define-hash-cache values-specifier-type
  ((orig equal-but-no-car-recursion)) ()
  :hash-function #'sxhash :hash-bits 10)

(defvar *pending-defstruct-type*)
(declaim (type classoid *pending-defstruct-type*))

;;; The recursive ("-R" suffixed) entry point for this function
;;; should be used for each nested parser invocation.
(defun values-specifier-type-r (context type-specifier)
  (declare (type cons context))
  (labels ((fail (spec) ; Q: Shouldn't this signal a TYPE-ERROR ?
             (error "bad thing to be a type specifier: ~S" spec))
           (instance-to-ctype (x)
             (flet ((translate (classoid)
                      ;; Hmm, perhaps this should signal PARSE-UNKNOWN-TYPE
                      ;; if CLASSOID is an instance of UNDEFINED-CLASSOID ?
                      ;; Can that happen?
                      (or (and (built-in-classoid-p classoid)
                               (built-in-classoid-translation classoid))
                          classoid)))
               (cond ((classoid-p x) (translate x))
                     ;; Avoid TYPEP on SB!MOP:EQL-SPECIALIZER and CLASS because
                     ;; the fake metaobjects do not allow type analysis, and
                     ;; would cause a compiler error as it tries to decide
                     ;; whether any clause of this COND subsumes another.
                     ;; Moreover, we don't require the host to support MOP.
                     #-sb-xc-host
                     ((sb!pcl::classp x) (translate (sb!pcl::class-classoid x)))
                     #-sb-xc-host
                     ((sb!pcl::eql-specializer-p type-specifier)
                      ;; FIXME: these aren't always cached. Should they be?
                      ;; It seems so, as "parsing" constructs a new object.
                      ;; Perhaps better, the EQL specializer itself could store
                      ;; (by memoizing, if not precomputing) a CTYPE
                      (make-eql-type
                       (sb!mop:eql-specializer-object type-specifier)))
                     (t (fail x))))))
    (when (typep type-specifier 'instance)
      (return-from values-specifier-type-r (instance-to-ctype type-specifier)))
    (when (atom type-specifier)
      ;; Try to bypass the cache, which avoids using a cache line for standard
      ;; atomic specifiers. This is a trade-off- cache seek might be faster,
      ;; but this solves the problem that a full call to (TYPEP #\A 'FIXNUM)
      ;; consed a cache line every time the cache missed on FIXNUM (etc).
      (awhen (info :type :builtin type-specifier)
        (return-from values-specifier-type-r it)))
    (!values-specifier-type-memo-wrapper
     (lambda ()
       (labels
         ((recurse (spec)
            (prog* ((head (if (listp spec) (car spec) spec))
                    (builtin (if (symbolp head)
                                 (info :type :builtin head)
                                 (return (fail spec)))))
              (when (deprecated-thing-p 'type head)
                (setf (cdr context) nil)
                (signal 'parse-deprecated-type :specifier spec))
              (when (atom spec)
                ;; If spec is non-atomic, the :BUILTIN value is inapplicable.
                ;; There used to be compound builtins, but not any more.
                (when builtin (return builtin))
                ;; Any spec that apparently refers to a defstruct form
                ;; that's being macroexpanded should refer to that type.
                (when (boundp '*pending-defstruct-type*)
                  (let ((classoid *pending-defstruct-type*))
                    (when (eq (classoid-name classoid) spec)
                      (setf (cdr context) nil) ; don't cache
                      (return classoid))))
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
                              (instance-to-ctype expansion)
                              (recurse expansion)))))
              ;; If the spec is (X ...) and X has neither a translator
              ;; nor expander, and is a builtin, such as FIXNUM, fail now.
              ;; But - see FIXME at top - it would be consistent with
              ;; DEFTYPE to reject spec only if not a singleton.
              (when builtin (return (fail spec)))
              ;; SPEC has a legal form, so return an unknown type.
              (signal 'parse-unknown-type :specifier spec)
             UNKNOWN
              (setf (cdr context) nil)
              (return (make-unknown-type :specifier spec)))))
        (let ((result (recurse (uncross type-specifier))))
          (if (cdr context) ; cacheable
              result
              ;; (The RETURN-FROM here inhibits caching; this makes sense
              ;; not only from a compiler diagnostics point of view,
              ;; but also for proper workingness of VALID-TYPE-SPECIFIER-P.
              (return-from values-specifier-type-r result)))))
     type-specifier)))
(defun values-specifier-type (type-specifier)
  (dx-let ((context (cons type-specifier t)))
    (values-specifier-type-r context type-specifier)))

;;; This is like VALUES-SPECIFIER-TYPE, except that we guarantee to
;;; never return a VALUES type.
(defun specifier-type-r (context type-specifier)
  (let ((ctype (values-specifier-type-r context type-specifier)))
    (when (values-type-p ctype)
      (error "VALUES type illegal in this context:~%  ~S" type-specifier))
    ctype))
(defun specifier-type (type-specifier)
  (dx-let ((context (cons type-specifier t)))
    (specifier-type-r context type-specifier)))

;;; Parse TYPE-SPECIFIER, returning NIL if any sub-part of it is unknown
(defun type-or-nil-if-unknown (type-specifier &optional allow-values)
  (dx-let ((context (cons type-specifier t)))
    (let ((result (values-specifier-type-r context type-specifier)))
      (when (and (not allow-values) (values-type-p result))
        (error "VALUES type illegal in this context:~%  ~S" type-specifier))
      ;; If it was non-cacheable, either it contained a deprecated type
      ;; or unknown type, or was a pending defstruct definition.
      (if (and (not (cdr context)) (contains-unknown-type-p result))
          nil
          result))))

(defun single-value-specifier-type-r (context x)
  (if (eq x '*) *universal-type* (specifier-type-r context x)))
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
  #!+sb-doc
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
;;; it can be rendered as a literal object unless it:
;;;  - mentions a classoid or unknown type
;;;  - uses a floating-point literal (perhaps positive zero could be allowed?)
;;;
;;; This is important for type system initialization, but it will also
;;; apply to hand-written calls and make-load-form expressions.
;;;
;;; After the target is built, we remove this transform, both because calls
;;; to SPECIFIER-TYPE do not arise organically through user code,
;;; and because it is possible that user changes to types could make parsing
;;; return a different thing, e.g. changing a DEFTYPE to a DEFCLASS.
;;;
#+sb-xc-host
(progn
(sb!c::define-source-transform specifier-type (type-spec &environment env)
  (or (and (sb!xc:constantp type-spec env)
           (let ((parse (specifier-type (constant-form-value type-spec env))))
             (cond
              ((contains-unknown-type-p parse)
               (bug "SPECIFIER-TYPE transform parsed an unknown type"))
              ((cold-dumpable-type-p parse)
               ;; Obtain a canonical form by unparsing so that TYPE= specs
               ;; coalesce in presence of DEFTYPEs. LOAD-TIME-VALUE in the
               ;; cross-compiler has a special-case to turn !SPECIFIER-TYPE
               ;; into a fop-funcall, which is handled by genesis.
               `(load-time-value (!specifier-type ',(type-specifier parse))
                                 t)))))
      (values nil t)))

(defun cold-dumpable-type-p (ctype)
  (named-let recurse ((ctype ctype))
    (typecase ctype
      (args-type
       (and (every #'recurse (args-type-required ctype))
            (every #'recurse (args-type-optional ctype))
            (acond ((args-type-rest ctype) (recurse it)) (t))
            (every (lambda (x) (recurse (key-info-type x)))
                   (args-type-keywords ctype))
            (if (fun-type-p ctype) (recurse (fun-type-returns ctype)) t)))
      (compound-type (every #'recurse (compound-type-types ctype)))
      (negation-type (recurse (negation-type-type ctype)))
      (array-type (recurse (array-type-element-type ctype)))
      (cons-type (and (recurse (cons-type-car-type ctype))
                      (recurse (cons-type-cdr-type ctype))))
      (member-type
       (and (listp (xset-data (member-type-xset ctype))) ; can't dump hashtable
            (not (member-type-fp-zeroes ctype)))) ; nor floats
      (numeric-type
       ;; Floating-point constants are not dumpable. (except maybe +0.0)
       (if (or (typep (numeric-type-low ctype) '(or float (cons float)))
               (typep (numeric-type-high ctype) '(or float (cons float))))
           nil
           t))
      (built-in-classoid t)
      (classoid nil)
      ;; HAIRY is just an s-expression, so it's dumpable. Same for simd-pack
      ((or named-type character-set-type hairy-type #!+sb-simd-pack simd-pack-type)
       t))))

(setf (get '!specifier-type :sb-cold-funcall-handler/for-value)
      (lambda (arg)
        (let ((specifier
               (if (symbolp arg) arg (sb!fasl::host-object-from-core arg))))
          (sb!fasl::ctype-to-core specifier (specifier-type specifier)))))

(setf (info :function :where-from '!specifier-type) :declared) ; lie
) ; end PROGN
