;;;; various extensions (including SB-INT "internal extensions")
;;;; available both in the cross-compilation host Lisp and in the
;;;; target SBCL

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;; Like DEFUN, but hides itself in the backtrace. This is meant for
;;; trivial functions which just do some argument parsing and call
;;; ERROR for real. Hence, having them and their locals in the
;;; backtrace would add no useful information pertaining to the error.
(defmacro define-error-wrapper (name lambda-list &body body)
  `(defun ,name (,@lambda-list
                 &aux #-sb-xc-host (sb-debug:*stack-top-hint* (or sb-debug:*stack-top-hint* ',name)))
     ,@body))

;;; FIXME: a lot of places in the code that should use ARRAY-RANGE
;;; instead use INDEX and vice-versa, but the thing is, we have
;;; a ton of fenceposts errors all over the place regardless.
;;; CLHS glosary reference:
;;; "array total size n. the total number of elements in an array,
;;;  computed by taking the product of the dimensions of the array."
;;; and ARRAY-TOTAL-SIZE-LIMIT:
;;; "The upper exclusive bound on the array total size of an array."

;;; Consider a reduced example for the sake of argument in which
;;; ARRAY-TOTAL-SIZE-LIMIT were 21.
;;; - the largest vector would have LENGTH 20.
;;; - the largest legal index for AREF on it would be 19.
;;; - the largest legal :END on sequence functions would be 20.
;;; Nothing should allow 21. So DEFTYPE ARRAY-RANGE is wrong
;;; and must never be used for anything.
;;; Unfortunately, it's all over mb-util and enc-utf etc.
;;;
;;; *Also* INDEX is wrong, as the comment there says.
;;; To fix these and also SEQUENCE-END:
;;; - INDEX should be `(integer 0 (,1- array-dimension-limit))
;;; - ARRAY-RANGE should be `(integer 0 (,array-dimension-limit))
;;; - SEQUENCE-END (in deftypes-for-target) should be
;;;   `(OR NULL ARRAY-RANGE)
;;;
;;; A further consideration for INDEX on 64-bit machines is that
;;; if INDEX were equivalent to (UNSIGNED-BYTE n) for some 'n'
;;; then (TYPEP X 'INDEX) can combines FIXNUMP and the range test into
;;; one bit-masking test, depending on the architecture.
;;; Probably it should be (UNSIGNED-BYTE 61) except on ppc64
;;; where it would have to be (UNSIGNED-BYTE 59).

;;; A number that can represent an index into a vector, including
;;; one-past-the-end
(deftype array-range ()
  `(integer 0 ,array-dimension-limit))

;;; a type used for indexing into sequences, and for related
;;; quantities like lengths of lists and other sequences.
;;;
;;; A more correct value for the exclusive upper bound for indexing
;;; would be (1- ARRAY-DIMENSION-LIMIT) since ARRAY-DIMENSION-LIMIT is
;;; the exclusive maximum *size* of one array dimension (As specified
;;; in CLHS entries for MAKE-ARRAY and "valid array dimensions"). The
;;; current value is maintained to avoid breaking existing code that
;;; also uses that type for upper bounds on indices (e.g. sequence
;;; length).
;;;
;;; In SBCL, ARRAY-DIMENSION-LIMIT is arranged to be a little smaller
;;; than MOST-POSITIVE-FIXNUM, for implementation (see comment above
;;; ARRAY-DIMENSION-LIMIT) and efficiency reasons: staying below
;;; MOST-POSITIVE-FIXNUM lets the system know it can increment a value
;;; of type INDEX without having to worry about using a bignum to
;;; represent the result.
(def!type index () `(integer 0 (,array-dimension-limit)))

;;; like INDEX, but augmented with -1 (useful when using the index
;;; to count downwards to 0, e.g. LOOP FOR I FROM N DOWNTO 0, with
;;; an implementation which terminates the loop by testing for the
;;; index leaving the loop range)
(def!type index-or-minus-1 () `(integer -1 (,array-dimension-limit)))

;;; The smallest power of two that is equal to or greater than X.
(declaim (inline power-of-two-ceiling))
(defun power-of-two-ceiling (x)
  (declare (type index x))
  (ash 1 (integer-length (1- x))))

(declaim (inline align-up))
(defun align-up (value granularity &aux (mask (1- granularity)))
  (logandc2 (+ value mask) mask))

;;; CHAR-CODE values for ASCII characters which we care about but
;;; which aren't defined in section "2.1.3 Standard Characters" of the
;;; ANSI specification for Lisp.
;;; These are typically used in the idiom (CODE-CHAR FOO-CHAR-CODE)
(defconstant bell-char-code 7)
(defconstant backspace-char-code 8)
(defconstant tab-char-code 9)
(defconstant line-feed-char-code 10)
(defconstant form-feed-char-code 12)
(defconstant return-char-code 13)
(defconstant escape-char-code 27) ; unused
(defconstant rubout-char-code 127)

;;;; type-ish predicates

;;; This is used for coalescing constants, check that the tree doesn't
;;; have cycles and isn't too large.
(defun coalesce-tree-p (x)
  (let ((depth-limit 12)
        (size-limit (expt 2 12)))
    (declare (fixnum size-limit))
    (and (consp x)
         (labels ((safe-cddr (cons)
                    (let ((cdr (cdr cons)))
                      (when (consp cdr)
                        (cdr cdr))))
                  (check-cycle (cdr seen depth)
                    (let ((object (car cdr)))
                      (when (and (consp object)
                                 (or (= depth depth-limit)
                                     (memq object seen)
                                     (let ((seen (cons cdr seen)))
                                       (declare (dynamic-extent seen))
                                       (recurse object seen
                                                (truly-the fixnum (1+ depth))))))
                        (return-from coalesce-tree-p))))
                  (recurse (list seen depth)
                    ;; Almost regular circular list detection, with a twist:
                    ;; we also check each element of the list for upward
                    ;; references using CHECK-CYCLE.
                    (do ((fast (cdr list) (safe-cddr fast))
                         (slow list (cdr slow)))
                        ((not (consp fast))
                         ;; Not CDR-circular, need to check remaining CARs yet
                         (do ((tail slow (cdr tail)))
                             ((not (consp tail)))
                           (check-cycle tail seen depth)))
                      (check-cycle slow seen depth)
                      (when (or (eq fast slow)
                                (zerop (setf size-limit
                                             (truly-the fixnum
                                                        (1- size-limit)))))
                        (return-from coalesce-tree-p)))))
           (declare (inline check-cycle))
           (recurse x (list x) 0)
           t))))

;;; Is LIST a (possibly-improper) list of at least LENGTH elements?
(declaim (ftype (sfunction (t index) boolean) list-of-length-at-least-p))
(defun list-of-length-at-least-p (list length)
  (named-let rec ((rest list) (n length))
    (declare (type index n))
    (or (zerop n) ; since anything can be considered an improper list of length 0
        (and (consp rest) (rec (cdr rest) (1- n))))))

(defun sequence-of-length-at-least-p (sequence length)
  (etypecase sequence
    (list
     (list-of-length-at-least-p sequence length))
    (vector
     (>= (length sequence) length))
    (sequence
     (>= (length sequence) length))))

;;; Could this object contain other objects? (This is important to
;;; the implementation of things like *PRINT-CIRCLE* and the dumper.)
(defun compound-object-p (x)
  (or (consp x)
      (%instancep x)
      (typep x '(array t *))))

;;;; the COLLECT macro
;;;;
;;;; comment from CMU CL: "the ultimate collection macro..."

;;; helper function for COLLECT, which becomes the expander of the
;;; MACROLET definitions created by COLLECT if collecting a list
;;; and an INITIAL-VALUE was specified, so we don't know at each step
;;; whether the tail is NIL or not.
;;; N-TAIL is the pointer to the current tail of the list,  or NIL
;;; if the list is empty.
(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
  (defun collect-list-expander (n-value n-tail forms)
    (let ((n-res (gensym)))
      `(progn
         ,@(mapcar (lambda (form)
                     `(let ((,n-res (cons ,form nil)))
                        (cond (,n-tail
                               (setf (cdr ,n-tail) ,n-res)
                               (setq ,n-tail ,n-res))
                              (t
                               (setq ,n-tail ,n-res  ,n-value ,n-res)))))
                   forms)
         ,n-value))))

;;; Collect some values somehow. Each of the collections specifies a
;;; bunch of things which collected during the evaluation of the body
;;; of the form. The name of the collection is used to define a local
;;; macro, a la MACROLET. Within the body, this macro will evaluate
;;; each of its arguments and collect the result, returning the
;;; current value after the collection is done. The body is evaluated
;;; as a PROGN; to get the final values when you are done, just call
;;; the collection macro with no arguments.
;;;
;;; INITIAL-VALUE is the value that the collection starts out with,
;;; which defaults to NIL. COLLECTOR is the function which does the
;;; collection. It is a function which will accept two arguments: the
;;; value to be collected and the current collection. The result of
;;; the function is made the new value for the collection. As a
;;; special-case, omitting COLLECTOR causes a list to be built in forward
;;; order. If INITIAL-VALUE is supplied for the default usage, new items
;;; will be RPLACD'd onto the end.
;;; Note that COLLECTOR may be anything that can appear in the functional
;;; position, including macros and lambdas.
;;; Also note that invocation of the collector macro for effect, i.e. other
;;; than with 0 arguments, is not prescribed to have any particular value.
(defmacro collect (collections &body body)
  (let ((macros ())
        (binds ())
        (dx ())
        (ignores ()))
    (dolist (spec collections)
      (destructuring-bind (name &optional initial-value (collector nil collectorp)
                                &aux (n-value (copy-symbol name)))
          spec
        (push `(,n-value ,(if (or initial-value collectorp)
                              initial-value
                              `(#-sb-xc-host unaligned-dx-cons
                                #+sb-xc-host list
                                nil)))
              binds)
        (let ((macro-body
               (cond
                 (collectorp
                   ``(progn
                       ,@(mapcar (lambda (x)
                                   `(setq ,',n-value (,',collector ,x ,',n-value)))
                                 args)
                       ,',n-value))
                 ((not initial-value)
                  ;; Use a dummy cons to skip the test for TAIL being NIL with each
                  ;; inserted item.
                  (push n-value dx)
                  (let ((n-tail (gensymify* name "-TAIL")))
                    (push n-tail ignores)
                    (push `(,n-tail ,n-value) binds)
                    `(if args
                         `(progn
                            ,@(mapcar (lambda (x)
                                        `(setf ,',n-tail (setf (cdr ,',n-tail)
                                                               (list ,x))))
                                      args))
                         `(cdr ,',n-value))))
                 ;; collecting a list given a list to start with.
                 ;; It's possible to use the "fancy" strategy to avoid testing for NIL
                 ;; at each step but I choose not to.  The initializer would have to be
                 ;; (cons nil initial-value). It's unimportant.
                 (initial-value
                  (let ((n-tail (gensymify* name "-TAIL")))
                    (push n-tail ignores)
                    (push `(,n-tail (last ,n-value)) binds)
                    `(collect-list-expander ',n-value ',n-tail args))))))
          (push `(,name (&rest args) ,macro-body) macros))))
    `(macrolet ,macros
       (let* ,(nreverse binds)
         ,@(if dx `((declare (dynamic-extent ,@dx))))
         ;; Even if the user reads each collection result,
         ;; reader conditionals might statically eliminate all writes.
         ;; Since we don't know, all the -n-tail variable are ignorable.
         ,@(if ignores `((declare (ignorable ,@ignores))))
         ,@body))))

;;; Functions for compatibility sake:

;;; Delete just one item
(defun delq1 (item list)
  (do ((prev nil x)
       (x list (cdr x)))
      ((null x) list)
    (when (eq item (car x))
      (if (null prev)
          (return (cdr x))
          (rplacd prev (cdr x)))
      (return list))))

(declaim (inline neq))
(defun neq (x y)
  (not (eq x y)))

(defun adjust-list (list length initial-element)
  (let ((old-length (length list)))
    (cond ((< old-length length)
           (append list (make-list (- length old-length)
                                   :initial-element initial-element)))
          ((> old-length length)
           (subseq list 0 length))
          (t list))))

;;;; miscellaneous iteration extensions

(defun filter-dolist-declarations (decls)
  (mapcar (lambda (decl)
            `(declare ,@(remove-if
                         (lambda (clause)
                           (and (consp clause)
                                (or (eq (car clause) 'type)
                                    (eq (car clause) 'ignore))))
                         (cdr decl))))
          decls))
;;; just like DOLIST, but with one-dimensional arrays
(defmacro dovector ((elt vector &optional result) &body body)
  (multiple-value-bind (forms decls) (parse-body body nil)
    (with-unique-names (index length vec)
      `(let ((,vec ,vector))
        (declare (type vector ,vec))
        (do ((,index 0 (1+ ,index))
             (,length (length ,vec)))
            ((>= ,index ,length) (let ((,elt nil))
                                   ,@(filter-dolist-declarations decls)
                                   ,elt
                                   ,result))
          (let ((,elt (aref ,vec ,index)))
            ,@decls
            (tagbody
               ,@forms)))))))

;;; Iterate over the entries in a HASH-TABLE, first obtaining the lock
;;; if the table is a synchronized table.
;;; An implicit block named NIL exists around the iteration, as is the custom.
(defmacro dohash (((key-var value-var) table &key result locked) &body body)
  (let* ((n-table (make-symbol "HT"))
         (iter-form `(block nil
                       (maphash (lambda (,key-var ,value-var) ,@body) ,n-table)
                       ,result)))
    `(let ((,n-table ,table))
       ,(if locked
            `(with-system-mutex ((hash-table-lock ,n-table)) ,iter-form)
            iter-form))))

;;; Executes BODY for all entries of PLIST with KEY and VALUE bound to
;;; the respective keys and values.
(defmacro doplist ((key val) plist &body body)
  (with-unique-names (tail)
    `(let ((,tail ,plist) ,key ,val)
       (declare (ignorable ,key ,val))
       (loop (when (null ,tail) (return nil))
             (setq ,key (pop ,tail))
             (when (null ,tail)
               (error "malformed plist, odd number of elements"))
             (setq ,val (pop ,tail))
             (progn ,@body)))))

;;; Like GETHASH if HASH-TABLE contains an entry for KEY.
;;; Otherwise, evaluate DEFAULT, store the resulting value in
;;; HASH-TABLE and return two values: 1) the result of evaluating
;;; DEFAULT 2) NIL.
(defmacro ensure-gethash (key hash-table default)
  (with-unique-names (n-key n-hash-table value foundp)
    `(let ((,n-key ,key)
           (,n-hash-table ,hash-table))
       (multiple-value-bind (,value ,foundp) (gethash ,n-key ,n-hash-table)
         (if ,foundp
             (values ,value t)
             (values (setf (gethash ,n-key ,n-hash-table) ,default) nil))))))

;;; (binding* ({(names initial-value [flag])}*) body)
;;; FLAG may be NIL or :EXIT-IF-NULL
;;;
;;; This form unites LET*, MULTIPLE-VALUE-BIND and AWHEN.
;;; Any name in a list of names may be NIL to ignore the respective value.
;;; If NAMES itself is nil, the initial-value form is evaluated only for effect.
;;;
;;; Clauses with no flag and one binding are equivalent to LET.
;;;
;;; Caution: don't use declarations of the form (<non-builtin-type-id> <var>)
;;; before the INFO database is set up in building the cross-compiler,
;;; or you will probably lose.
;;; Of course, since some other host Lisps don't seem to think that's
;;; acceptable syntax anyway, you're pretty much prevented from writing it.
;;;
(defmacro binding* ((&rest clauses) &body body)
  (unless clauses ; wrap in LET to preserve non-toplevelness
    (return-from binding* `(let () ,@body)))
  (multiple-value-bind (body decls) (parse-body body nil)
    ;; Generate an abstract representation that combines LET* clauses.
    (let (repr)
      (dolist (clause clauses)
        (destructuring-bind (symbols value-form &optional flag) clause
          (declare (type (member :exit-if-null nil) flag))
          (let* ((ignore nil)
                 (symbols
                  (cond ((not (listp symbols)) (list symbols))
                        ((not symbols) (setq ignore (list (gensym))))
                        (t (mapcar
                            (lambda (x) (or x (car (push (gensym) ignore))))
                            symbols))))
                 (flags (logior (if (cdr symbols) 1 0) (if flag 2 0)))
                 (last (car repr)))
            ;; EVENP => this clause does not entail multiple-value-bind
            (cond ((and (evenp flags) (eql (car last) 0))
                   (setf (first last) flags)
                   (push (car symbols) (second last))
                   (push value-form (third last))
                   (setf (fourth last) (nconc ignore (fourth last))))
                  (t
                   (push (list flags symbols (list value-form) ignore)
                         repr))))))
      ;; Starting with the innermost binding clause, snarf out the
      ;; applicable declarations. (Clauses are currently reversed)
      (dolist (abstract-clause repr)
        (when decls
          (multiple-value-bind (binding-decls remaining-decls)
              (extract-var-decls decls (second abstract-clause))
            (setf (cddddr abstract-clause) binding-decls)
            (setf decls remaining-decls))))
      ;; Generate sexprs from inside out.
      (loop with listp = t ; BODY is already a list
            for (flags symbols values ignore . binding-decls) in repr
            ;; Maybe test the last bound symbol in the clause for LET*
            ;; or 1st symbol for mv-bind. Either way, the first of SYMBOLS.
            for inner = (if (logtest flags 2) ; :EXIT-IF-NULL was specified.
                            (prog1 `(when ,(car symbols)
                                      ,@(if listp body (list body)))
                              (setq listp nil))
                            body)
         do (setq body
                  `(,.(if (evenp flags)
                          `(let* ,(nreverse (mapcar #'list symbols values)))
                          `(multiple-value-bind ,symbols ,(car values)))
                    ,@(when binding-decls (list binding-decls))
                    ,@(when ignore `((declare (ignorable ,@ignore))))
                    ,@decls ; anything leftover
                    ,@(if listp inner (list inner)))
                  listp nil
                  decls nil))
      body)))

;;;; macro writing utilities

(defmacro with-current-source-form ((&rest forms) &body body)
  "In a macroexpander, indicate that FORMS are being processed by BODY.

FORMS are usually sub-forms of the whole form passed to the expander.

If more than one form is supplied, FORMS should be ordered by
specificity, with the most specific form first. This allows the
compiler to try and obtain a source path using subsequent elements of
FORMS if it fails for the first one.

Indicating the processing of sub-forms lets the compiler report
precise source locations in case conditions are signaled during the
execution of BODY.

NOTE: This interface is experimental and subject to change."
  #-sb-xc-host `(sb-c::call-with-current-source-form
                 (lambda () ,@body) ,@forms)
  #+sb-xc-host `(progn (list ,@forms) ,@body))

;;;; hash cache utility

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *profile-hash-cache* nil))

;;; Define a hash cache that associates some number of argument values
;;; with a result value. The TEST-FUNCTION paired with each ARG-NAME
;;; is used to compare the value for that arg in a cache entry with a
;;; supplied arg. The TEST-FUNCTION must not error when passed NIL as
;;; its first arg, but need not return any particular value.
;;; TEST-FUNCTION may be any thing that can be placed in CAR position.
;;;
;;; This code used to store all the arguments / return values directly
;;; in the cache vector. This was both interrupt- and thread-unsafe, since
;;; it was possible that *-CACHE-ENTER would scribble over a region of the
;;; cache vector which *-CACHE-LOOKUP had only partially processed. Instead
;;; we now store the contents of each cache bucket as a separate array, which
;;; is stored in the appropriate cell in the cache vector. A new bucket array
;;; is created every time *-CACHE-ENTER is called, and the old ones are never
;;; modified. This means that *-CACHE-LOOKUP will always work with a set
;;; of consistent data. The overhead caused by consing new buckets seems to
;;; be insignificant on the grand scale of things. -- JES, 2006-11-02
;;;
;;; NAME is used to define these functions:
;;; <name>-CACHE-LOOKUP Arg*
;;;   See whether there is an entry for the specified ARGs in the
;;;   cache. If not present, the :DEFAULT keyword (default NIL)
;;;   determines the result(s).
;;; <name>-CACHE-ENTER Arg* Value*
;;;   Encache the association of the specified args with VALUE.
;;; <name>-CACHE-CLEAR
;;;   Reinitialize the cache, invalidating all entries and allowing
;;;   the arguments and result values to be GC'd.
;;;
;;; These other keywords are defined:
;;; :HASH-BITS <n>
;;;   The size of the cache as a power of 2.
;;; :HASH-FUNCTION function
;;;   Some thing that can be placed in CAR position which will compute
;;;   a fixnum with at least (* 2 <hash-bits>) of information in it.
;;; :VALUES <n>
;;;   the number of return values cached for each function call
(define-load-time-global *cache-vector-symbols* nil)

(defun drop-all-hash-caches ()
  #+sb-xc-host (values-specifier-type-cache-clear) ; it's not like the rest
  (dolist (name *cache-vector-symbols*)
    (set name nil)))

(defmacro sb-int-package () (find-package "SB-INT"))

;; Make a new hash-cache and optionally create the statistics vector.
(defun alloc-hash-cache (size symbol)
  (declare (type index size))
  (declare (sb-c::tlab :system))
  (let (cache)
    ;; It took me a while to figure out why infinite recursion could occur
    ;; in VALUES-SPECIFIER-TYPE. It's because SET calls VALUES-SPECIFIER-TYPE.
    (macrolet ((set! (symbol value)
                 `(#+sb-xc-host set
                   #-sb-xc-host sb-kernel:%set-symbol-global-value
                   ,symbol ,value))
               (reset-stats ()
                 ;; If statistics gathering is not not compiled-in,
                 ;; no sense in setting a symbol that is never used.
                 ;; While this uses SYMBOLICATE at runtime,
                 ;; it is inconsequential to performance.
                 (if *profile-hash-cache*
                     `(let ((statistics
                             (package-symbolicate (sb-int-package) symbol "-STATS")))
                        (unless (boundp statistics)
                          (set! statistics
                                (make-array 3 :element-type 'fixnum
                                              :initial-contents '(1 0 0))))))))
      ;; It would be bad if another thread sees MAKE-ARRAY's result in the
      ;; global variable before the vector's header+length have been set.
      ;; Without a barrier, this would be theoretically possible if the
      ;; architecture allows out-of-order memory writes.
      (sb-thread:barrier (:write)
        (reset-stats)
        (setq cache (make-array size :initial-element 0)))
      (set! symbol cache))))

;; At present we make a new vector every time a line is re-written,
;; to make it thread-safe and interrupt-safe. A multi-word compare-and-swap
;; is tricky to code and stronger than we need. It is possible instead
;; to provide multi-word reads that can detect failure of atomicity,
;; and on x86 it's possible to have atomic double-wide read/write,
;; so a 1-arg/1-result cache line needn't cons at all except once
;; (and maybe not even that if we make the cache into pairs of cells).
;; But this way is easier to understand, for now anyway.
(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
  (defun hash-cache-line-allocator (n)
    (aref #.(coerce (loop for i from 2 to 6
                          collect (symbolicate "ALLOC-HASH-CACHE-LINE/"
                                               (char "23456" (- i 2))))
                    'vector)
          (- n 2))))
(macrolet ((def (n)
             (let* ((ftype `(sfunction ,(make-list n :initial-element t) t))
                    (fn (hash-cache-line-allocator n))
                    (args (make-gensym-list n)))
               `(progn
                  (declaim (ftype ,ftype ,fn))
                  (defun ,fn ,args
                    (declare (sb-c::tlab :system))
                    (declare (optimize (safety 0)))
                    ,(if (<= n 3)
                         `(list* ,@args)
                         `(vector ,@args)))))))
  (def 2)
  (def 3)
  (def 4)
  (def 5)
  (def 6))

(defmacro !define-hash-cache (name args
                              &key hash-function hash-bits memoizer
                              (values 1))
  (declare (ignore memoizer))
  (dolist (arg args)
    ;; Each arg is (VARNAME EQUIVALENCE-TEST &OPTIONAL EXPR-TO-CACHE)
    ;; EXPR-TO-CACHE says how to copy a dynamic-extent arg into the cache line
    ;; (the default is to store each argument as-is)
    (unless (<= 2 (length arg) 3)
      (error "bad argument spec: ~S" arg)))
  ;; Disallow >12 hash bits. Supposing a 32-bit machine, a ctype hash uses 5
  ;; reserved bits for the type-class index, and then we need 2 independent
  ;; choices of hash, so supposing that's 12 bits each, we're at 29 bits.
  ;; But I want to take 2 bits for flags, so that's 31 bits.
  (assert (typep hash-bits '(integer 5 12))) ; reasonable bounds
  (unless (integerp values) ; non-numeric was only for CTYPE-OF-ARRAY
    (error "Values ~S unsupported" values))
  (let* ((var-name (symbolicate "**" name "-CACHE-VECTOR**"))
         (statistics-name (package-symbolicate (sb-int-package) var-name "-STATS"))
         (cache-type `(simple-vector ,(ash 1 hash-bits))))
    `(progn
       (eval-when (:compile-toplevel :execute)
         (setf (get ',name '!cache-definition)
               (list ',hash-bits ',hash-function ',args ',values)))
       (pushnew ',var-name *cache-vector-symbols*)
       (define-load-time-global ,var-name nil)
       ,@(when *profile-hash-cache*
           `((declaim (type (simple-array fixnum (3)) ,statistics-name))
             (defvar ,statistics-name)))
       (declaim (type (or null ,cache-type) ,var-name))
       (defun ,(symbolicate name "-CACHE-CLEAR") () (setq ,var-name nil))
       ',var-name)))

(#+sb-xc-host defmacro #-sb-xc-host sb-xc:defmacro
 with-cache ((fun-name &rest actual-args) &body computation)
  (let* ((var-name (package-symbolicate (cl:symbol-package fun-name)
                                        "**" fun-name "-CACHE-VECTOR**"))
         (cache-params (get fun-name '!cache-definition))
         (statistics-name
          (when *profile-hash-cache*
            (package-symbolicate (sb-int-package) var-name "-STATS")))
         (hash-bits (first cache-params))
         (hashfun (second cache-params))
         (arg-specs (third cache-params))
         (nargs (length arg-specs))
         (values (fourth cache-params))
         (size (ash 1 hash-bits))
         (hashval (make-symbol "HASH"))
         (cache (make-symbol "CACHE"))
         (entry (make-symbol "LINE"))
         (nvalues (if (listp values) (length values) values))
         (result-temps
          (if (listp values)
              values ; use the names provided by the user
              (loop for i from 1 to nvalues ; else invent some names
                    collect (make-symbol (format nil "R~D" i)))))
         (temps (append (mapcar #'copy-symbol actual-args) result-temps))
         ;; Mnemonic: (FIND x SEQ :test #'f) calls f with x as the LHS
         (tests (mapcar (lambda (spec temp actual) ; -> (EQx #:ARG ARG)
                          `(,(cadr spec) ,temp ,actual))
                        arg-specs temps actual-args))
         (cache-type `(simple-vector ,size))
         (line-type (let ((n (+ nargs nvalues)))
                      (if (<= n 3) 'cons `(simple-vector ,n))))
         ;; It's not really necessary to factor out PROBE-IT,
         ;; but it used to be possible to probe for an entry to forcefully
         ;; evict (unmemoize) it. That capability is no longer needed.
         (probe-it
          (lambda (action)
            `(when ,cache
               ;; Shift out 2 low bits because they won't be as random as other bits,
               ;; at least not for single-argument cached functions. They might be OK
               ;; for functions that involve mixing.
               (let ((,hashval (ash ,hashval -2)) ; gets clobbered in probe loop
                     (,cache (truly-the ,cache-type ,cache)))
                 ;; FIXME: redundant?
                 (declare (type sb-xc:fixnum ,hashval))
                 (loop repeat 2
                    do (let ((,entry
                              (svref ,cache
                                     (ldb (byte ,hash-bits 0) ,hashval))))
                         (unless (eql ,entry 0)
                           (let* ((,entry (truly-the ,line-type ,entry))
                                  ,@(case (length temps)
                                     (2 `((,(first temps) (car ,entry))
                                          (,(second temps) (cdr ,entry))))
                                     (3 (let ((arg-temp (gensym "ARGS")))
                                          `((,arg-temp (cdr ,entry))
                                            (,(first temps) (car ,entry))
                                            (,(second temps)
                                             (car (truly-the cons ,arg-temp)))
                                            (,(third temps) (cdr ,arg-temp)))))
                                     (t (loop for i from 0 for x in temps
                                              collect `(,x (svref ,entry ,i))))))
                               (when (and ,@tests) ,action)))
                         (setq ,hashval (ash ,hashval ,(- hash-bits))))))))))
    `(let ((,hashval (the sb-xc:fixnum (funcall ,hashfun ,@actual-args)))
           (,cache ,var-name))
       #-sb-xc-host (declare (optimize (sb-c::insert-array-bounds-checks 0)))
       ,@(when *profile-hash-cache* ; count seeks
           `((when (boundp ',statistics-name) (incf (aref ,statistics-name 0)))))
       (block seek
         ,(funcall probe-it `(return-from seek (values ,@result-temps)))
         (multiple-value-bind ,result-temps (progn ,@computation)
           ;; Decide if cacheable result. Lambda expressions and strings always are.
           ;; Parsing a type specifier nonlocally exits from COMPUTATION if it
           ;; wants not to cache. And of course CTYPE-OF always returns a good type.
           ;; It's unfortunate that this macro macro bakes in such knowledge.
           (when ,(if (member fun-name '(sb-format::tokenize-control-string
                                         sb-alien::coerce-to-interpreted-function
                                         values-specifier-type
                                         ctype-of))
                      t
                      `(and ,@(mapcar (lambda (x) `(sb-kernel::ok-to-memoize-p ,x))
                                      actual-args)))
             (let ((,entry (,(hash-cache-line-allocator (+ nargs nvalues))
                            ,@(mapcar (lambda (spec actual) (or (caddr spec) actual))
                                      arg-specs actual-args)
                            ,@result-temps))
                   (,cache (truly-the ,cache-type
                                      (or ,cache (alloc-hash-cache ,size ',var-name))))
                   (idx1 (ldb (byte ,hash-bits 2) ,hashval))
                   (idx2 (ldb (byte ,hash-bits ,(+ 2 hash-bits)) ,hashval)))
               ,@(when *profile-hash-cache*
                   `((incf (aref ,statistics-name 1)))) ; count misses
                   ;; Why a barrier: the pointer to 'entry' (a cons or vector)
                   ;; MUST NOT be observed by another thread before its cells
                   ;; are filled. Equally bad, the 'output' cells in the line
                   ;; could be 0 while the 'input' cells matched something.
               (sb-thread:barrier (:write))
               (setf (svref ,cache (cond ((eql (svref ,cache idx1) 0) idx1)
                                         ((eql (svref ,cache idx2) 0) idx2)
                                         (t ,@(when *profile-hash-cache* ; count evictions
                                                `((incf (aref ,statistics-name 2))))
                                            idx1)))
                     ,entry)))
           (values ,@result-temps))))))

;;; some syntactic sugar for defining a function whose values are
;;; cached by !DEFINE-HASH-CACHE
;;; These keywords are mostly defined at !DEFINE-HASH-CACHE.
;;; Additional options:
;;; :MEMOIZER <name>
;;;   If provided, it is the name of a local macro that must be called
;;;   within the body forms to perform cache lookup/insertion.
;;;   If not provided, then the function's behavior is to automatically
;;;   attempt cache lookup, and on miss, execute the body code and
;;;   insert into the cache.
;;;   Manual control over memoization is useful if there are cases for
;;;   which it is undesirable to pollute the cache.

;;; Possible FIXME: if the function has a type proclamation, it forces
;;; a type-check every time the cache finds something. Instead, values should
;;; be checked once only when inserted into the cache, and not when read out.
;;;
;;; N.B.: it is not obvious that the intended use of an explicit MEMOIZE macro
;;; is to call it exactly once or not at all. If you call it more than once,
;;; then you inline all of its logic every time. Probably the code generated
;;; by DEFINE-HASH-CACHE should be an FLET inside the body of DEFUN-CACHED,
;;; but the division of labor is somewhat inverted at present.
;;; Since we don't have caches that aren't in direct support of DEFUN-CACHED
;;; - did we ever? - this should be possible to change.
;;;
(defmacro defun-cached ((name &rest options &key memoizer &allow-other-keys)
                        args &body body-decls-doc)
  (binding* (((forms decls doc) (parse-body body-decls-doc t))
             ((inputs aux-vars)
              (let ((aux (member '&aux args)))
                (if aux
                    (values (ldiff args aux) aux)
                    (values args nil))))
             (arg-names (mapcar #'car inputs)))
    (assert (not aux-vars)) ; was only for CTYPE-OF-ARRAY
    `(progn
        (!define-hash-cache ,name ,inputs ,@options)
        (defun ,name ,arg-names
          ,@decls
          ,@(if doc (list doc))
          ,(if memoizer
               `(macrolet ((,memoizer (&body body)
                             `(with-cache ,',(list* name arg-names) ,@body)))
                  ,@forms)
               `(with-cache (,name ,@arg-names) ,@forms))))))

(defun list-elts-eq (x y)
  (loop (when (eq x y) (return t))
        (when (or (atom x) (atom y)) (return nil))
        (unless (eq (car x) (car y)) (return nil))
        (setq x (cdr x)
              y (cdr y))))
(defun list-elements-eql (x y)
  (loop (when (eq x y) (return t))
        (when (or (atom x) (atom y)) (return nil))
        (unless (eql (car x) (car y)) (return nil))
        (setq x (cdr x)
              y (cdr y))))

;;;; various operations on names

;;; Is NAME a legal variable/function name?
(declaim (inline legal-variable-name-p))
(defun legal-variable-name-p (name)
  (typep name '(and symbol (not keyword) (not null))))

(declaim (inline legal-fun-name-p))
(defun legal-fun-name-p (name)
  (values (valid-function-name-p name)))

(declaim (inline legal-class-name-p))
(defun legal-class-name-p (thing)
  (symbolp thing))

;;; * extended-function-designator: an object that denotes a function and that is one of:
;;;   a function name (denoting the function it names in the global environment),
;;;   or a function (denoting itself). The consequences are undefined if a function name
;;;   is used as an extended function designator but it does not have a global definition
;;;   as a function, or if it is a symbol that has a global definition as a macro
;;;   or a special form.
;;; Release 1.4.6 advertised that (disassemble 'a-macro) works, which entails a choice:
;;; - if (EXTENDED-FUNCTION-DESIGNATOR-P) is to return NIL, then we have to
;;;   add (OR (SATISFIES MACRO-FUNCTION) ...) to the signature of DISASSEMBLE.
;;; - if (EXTENDED-FUNCTION-DESIGNATOR-P) is to return T, then we must avoid
;;;   using this predicate in contexts that demand a function.
(defun extended-function-designator-p (x)
  (or (and (legal-fun-name-p x)
           (fboundp x)
           (not (and (symbolp x) (special-operator-p x))))
      (functionp x)))

(deftype function-name () '(satisfies legal-fun-name-p))

;;; Signal an error unless NAME is a legal function name.
(define-error-wrapper legal-fun-name-or-type-error (name)
  (unless (legal-fun-name-p name)
    (error 'simple-type-error
           :datum name
           :expected-type 'function-name
           :format-control "Invalid function name: ~S"
           :format-arguments (list name))))

;;; Given a function name, return the symbol embedded in it.
;;;
;;; The ordinary use for this operator (and the motivation for the
;;; name of this operator) is to convert from a function name to the
;;; name of the BLOCK which encloses its body.
;;;
;;; Occasionally the operator is useful elsewhere, where the operator
;;; name is less mnemonic. (Maybe it should be changed?)
(declaim (ftype (function ((or symbol cons)) symbol) fun-name-block-name))
(defun fun-name-block-name (fun-name)
  (if (symbolp fun-name)
      fun-name
      (multiple-value-bind (legalp block-name)
          (valid-function-name-p fun-name)
        (if legalp
            block-name
            (error "not legal as a function name: ~S" fun-name)))))

(defun looks-like-name-of-special-var-p (x)
  (and (symbolp x)
       (sb-xc:symbol-package x)
       (let ((name (symbol-name x)))
         (and (> (length name) 2) ; to exclude '* and '**
              (char= #\* (aref name 0))
              (char= #\* (aref name (1- (length name))))))))

;;;; ONCE-ONLY
;;;;
;;;; "The macro ONCE-ONLY has been around for a long time on various
;;;; systems [..] if you can understand how to write and when to use
;;;; ONCE-ONLY, then you truly understand macro." -- Peter Norvig,
;;;; _Paradigms of Artificial Intelligence Programming: Case Studies
;;;; in Common Lisp_, p. 853

;;; ONCE-ONLY is a utility useful in writing source transforms and
;;; macros. It provides a concise way to wrap a LET around some code
;;; to ensure that some forms are only evaluated once.
;;;
;;; Create a LET* which evaluates each value expression, binding a
;;; temporary variable to the result, and wrapping the LET* around the
;;; result of the evaluation of BODY. Within the body, each VAR is
;;; bound to the corresponding temporary variable.
(defmacro once-only (specs &body body)
  (named-let frob ((specs specs)
                   (body body))
    (if (null specs)
        `(progn ,@body)
        (let ((spec (first specs)))
          ;; FIXME: should just be DESTRUCTURING-BIND of SPEC
          (unless (proper-list-of-length-p spec 2)
            (error "malformed ONCE-ONLY binding spec: ~S" spec))
          (let* ((name (first spec))
                 (exp-temp (gensym "ONCE-ONLY")))
            `(let ((,exp-temp ,(second spec))
                   (,name (gensym ,(symbol-name name))))
               `(let ((,,name ,,exp-temp))
                  ,,(frob (rest specs) body))))))))

;;;; various error-checking utilities

;;; This function can be used as the default value for keyword
;;; arguments that must be always be supplied. Since it is known by
;;; the compiler to never return, it will avoid any compile-time type
;;; warnings that would result from a default value inconsistent with
;;; the declared type. When this function is called, it signals an
;;; error indicating that a required &KEY argument was not supplied.
;;; This function is also useful for DEFSTRUCT slot defaults
;;; corresponding to required arguments.
(defun missing-arg ()
  (/show0 "entering MISSING-ARG")
  (error "A required &KEY or &OPTIONAL argument was not supplied."))

;;; like CL:ASSERT and CL:CHECK-TYPE, but lighter-weight
;;;
;;; (As of sbcl-0.6.11.20, we were using some 400 calls to CL:ASSERT.
;;; The CL:ASSERT restarts and whatnot expand into a significant
;;; amount of code when you multiply them by 400, so replacing them
;;; with this should reduce the size of the system by enough to be
;;; worthwhile.)
(defmacro aver (expr)
  ;; Don't hold on to symbols, helping shake-packages.
  (labels ((replace-symbols (expr)
             (typecase expr
               (null expr)
               (symbol
                (symbol-name expr))
               (cons
                (cons (replace-symbols (car expr))
                      (replace-symbols (cdr expr))))
               (t
                expr))))
    `(unless ,expr
       (%failed-aver ',(replace-symbols expr)))))

(defun %failed-aver (expr)
  (bug "~@<failed AVER: ~2I~_~A~:>" expr))

(defun bug (format-control &rest format-arguments)
  (error 'bug
         :format-control format-control
         :format-arguments format-arguments))

;;; Return a function like FUN, but expecting its (two) arguments in
;;; the opposite order that FUN does.
(declaim (inline swapped-args-fun))
(defun swapped-args-fun (fun)
  (declare (type function fun))
  (lambda (x y)
    (funcall fun y x)))

;;; Return the numeric value of a type bound, i.e. an interval bound
;;; more or less in the format of bounds in ANSI's type specifiers,
;;; where a bare numeric value is a closed bound and a list of a
;;; single numeric value is an open bound.
;;;
;;; The "more or less" bit is that the no-bound-at-all case is
;;; represented by NIL (not by * as in ANSI type specifiers); and in
;;; this case we return NIL.
(declaim (inline type-bound-number))
(defun type-bound-number (x)
  (if (consp x)
      (car x)
      x))

;;;; utilities for two-VALUES predicates

(defmacro not/type (x)
  (let ((val (gensym "VAL"))
        (win (gensym "WIN")))
    `(multiple-value-bind (,val ,win)
         ,x
       (if ,win
           (values (not ,val) t)
           (values nil nil)))))

(defmacro and/type (x y)
  `(multiple-value-bind (val1 win1) ,x
     (if (and (not val1) win1)
         (values nil t)
         (multiple-value-bind (val2 win2) ,y
           (if (and val1 val2)
               (values t t)
               (values nil (and win2 (not val2))))))))

;;; sort of like ANY and EVERY, except:
;;;   * We handle two-VALUES predicate functions, as SUBTYPEP does.
;;;     (And if the result is uncertain, then we return (VALUES NIL NIL),
;;;     as SUBTYPEP does.)
;;;   * THING is just an atom, and we apply OP (an arity-2 function)
;;;     successively to THING and each element of LIST.
(defun any/type (op thing list)
  (declare (type function op))
  (let ((certain? t))
    (dolist (i list (values nil certain?))
      (multiple-value-bind (sub-value sub-certain?) (funcall op thing i)
        (if sub-certain?
            (when sub-value (return (values t t)))
            (setf certain? nil))))))
(defun every/type (op thing list)
  (declare (type function op))
  (let ((certain? t))
    (dolist (i list (if certain? (values t t) (values nil nil)))
      (multiple-value-bind (sub-value sub-certain?) (funcall op thing i)
        (if sub-certain?
            (unless sub-value (return (values nil t)))
            (setf certain? nil))))))

;;;; DEFPRINTER

;;; These functions are called by the expansion of the DEFPRINTER
;;; macro to do the actual printing.
(declaim (ftype (function (symbol t stream) (values))
                defprinter-prin1 defprinter-princ))
(defun defprinter-prin1 (name value stream)
  (defprinter-prinx #'prin1 name value stream))
(defun defprinter-princ (name value stream)
  (defprinter-prinx #'princ name value stream))
(defun defprinter-prinx (prinx name value stream)
  (declare (type function prinx))
  (when *print-pretty*
    (pprint-newline :linear stream))
  (format stream ":~A " name)
  (funcall prinx value stream)
  (values))
(defun defprinter-print-space (stream)
  (write-char #\space stream))

(defvar *print-ir-nodes-pretty* nil)

;;; Define some kind of reasonable PRINT-OBJECT method for a
;;; STRUCTURE-OBJECT class.
;;;
;;; NAME is the name of the structure class, and CONC-NAME is the same
;;; as in DEFSTRUCT.
;;;
;;; The SLOT-DESCS describe how each slot should be printed. Each
;;; SLOT-DESC can be a slot name, indicating that the slot should
;;; simply be printed. A SLOT-DESC may also be a list of a slot name
;;; and other stuff. The other stuff is composed of keywords followed
;;; by expressions. The expressions are evaluated with the variable
;;; which is the slot name bound to the value of the slot. These
;;; keywords are defined:
;;;
;;; :PRIN1    Print the value of the expression instead of the slot value.
;;; :PRINC    Like :PRIN1, only PRINC the value
;;; :TEST     Only print something if the test is true.
;;;
;;; If no printing thing is specified then the slot value is printed
;;; as if by PRIN1.
;;;
;;; The structure being printed is bound to STRUCTURE and the stream
;;; is bound to STREAM.
;;;
;;; If PRETTY-IR-PRINTER is supplied, the form is invoked when
;;; *PRINT-IR-NODES-PRETTY* is true.
(defmacro defprinter ((name
                       &key
                       (conc-name (concatenate 'simple-string
                                               (symbol-name name)
                                               "-"))
                       identity
                       pretty-ir-printer)
                      &rest slot-descs)
  (let ((first? t)
        maybe-print-space
        (reversed-prints nil))
    (flet ((sref (slot-name)
             `(,(symbolicate conc-name slot-name) structure)))
      (dolist (slot-desc slot-descs)
        (if first?
            (setf maybe-print-space nil
                  first? nil)
            (setf maybe-print-space `(defprinter-print-space stream)))
        (cond ((atom slot-desc)
               (push maybe-print-space reversed-prints)
               (push `(defprinter-prin1 ',slot-desc ,(sref slot-desc) stream)
                     reversed-prints))
              (t
               (let ((sname (first slot-desc))
                     (test t))
                 (collect ((stuff))
                   (do ((option (rest slot-desc) (cddr option)))
                       ((null option)
                        (push `(let ((,sname ,(sref sname)))
                                 (when ,test
                                   ,maybe-print-space
                                   ,@(or (stuff)
                                         `((defprinter-prin1
                                             ',sname ,sname stream)))))
                              reversed-prints))
                     (case (first option)
                       (:prin1
                        (stuff `(defprinter-prin1
                                  ',sname ,(second option) stream)))
                       (:princ
                        (stuff `(defprinter-princ
                                  ',sname ,(second option) stream)))
                       (:test (setq test (second option)))
                       (t
                        (error "bad option: ~S" (first option)))))))))))
    (let ((normal-printer `(pprint-logical-block (stream nil)
                             (print-unreadable-object (structure
                                                       stream
                                                       :type t
                                                       :identity ,identity)
                               ,@(nreverse reversed-prints))) ))
      `(defmethod print-object ((structure ,name) stream)
         ,(cond (pretty-ir-printer
                 `(if *print-ir-nodes-pretty*
                      ,pretty-ir-printer
                      ,normal-printer))
                (t
                 normal-printer))))))

;;; When cross-compiling, there is nothing out of the ordinary
;;; about compilling a DEFUN wrapped in PRESERVING-HOST-FUNCTION,
;;; so just remove the decoration.
#-sb-xc-host
(eval-when (:compile-toplevel)
  (sb-xc:defmacro sb-cold:preserving-host-function (form) form))

(sb-cold:preserving-host-function
(defun print-symbol-with-prefix (stream symbol &optional colon at)
  "For use with ~/: Write SYMBOL to STREAM as if it is not accessible from
  the current package."
  (declare (ignore colon at))
  ;; Only keywords should be accessible from the keyword package, and
  ;; keywords are always printed with colons, so this guarantees that the
  ;; symbol will not be printed without a prefix.
  (let ((*package* *keyword-package*))
    (write symbol :stream stream :escape t))))

(declaim (special sb-pretty:*pprint-quote-with-syntactic-sugar*))
(sb-cold:preserving-host-function
(defun print-type-specifier (stream type-specifier &optional colon at)
  (declare (ignore colon at))
  ;; Binding *PPRINT-QUOTE-WITH-SYNTACTIC-SUGAR* prevents certain
  ;; [f]types from being printed unhelpfully:
  ;;
  ;;   (function ())           => #'NIL
  ;;   (function *)            => #'*
  ;;   (function (function a)) => #'#'A
  ;;
  ;; Binding *PACKAGE* to the COMMON-LISP package causes specifiers
  ;; like CL:FUNCTION, CL:INTEGER, etc. to be printed without package
  ;; prefix but forces printing with package prefix for other
  ;; specifiers.
  (let ((sb-pretty:*pprint-quote-with-syntactic-sugar* nil)
        (*package* *cl-package*))
    (prin1 type-specifier stream))))

(sb-cold:preserving-host-function
(defun print-type (stream type &optional colon at)
  (print-type-specifier stream (type-specifier type) colon at)))

(defun print-lambda-list (stream lambda-list &optional colon at)
  (declare (ignore colon at))
  (let ((sb-pretty:*pprint-quote-with-syntactic-sugar* nil)
        (*package* *cl-package*))
    (format stream "~:A" lambda-list)))


;;;; Deprecating stuff

(deftype deprecation-state ()
  '(member :early :late :final))

(defun normalize-deprecation-since (since)
  (typecase since
    (string (values nil since))
    ((cons string (cons string null)) (values (car since) (cadr since)))
    (t (error 'simple-type-error
           :datum since
           :expected-type '(or string (cons string (cons string null)))
           :format-control "~@<The value ~S does not designate a ~
                            version or a software name and a version.~@:>"
           :format-arguments (list since)))))

(defun normalize-deprecation-replacements (replacements)
  (if (or (not (listp replacements))
          (eq 'setf (car replacements)))
      (list replacements)
      replacements))

(defstruct (deprecation-info
             (:constructor make-deprecation-info
                           (state software version &optional replacement-spec
                            &aux
                            (replacements (normalize-deprecation-replacements
                                           replacement-spec))))
             (:copier nil))
  (state        (missing-arg) :type deprecation-state :read-only t)
  (software     (missing-arg) :type (or null string)  :read-only t)
  (version      (missing-arg) :type string            :read-only t)
  (replacements '()           :type list              :read-only t))

;; Return the state of deprecation of the thing identified by
;; NAMESPACE and NAME, or NIL.
(defun deprecated-thing-p (namespace name)
  (multiple-value-bind (info infop)
      (ecase namespace
        (variable (info :variable :deprecated name))
        (function (info :function :deprecated name))
        (type     (info :type     :deprecated name)))
    (when infop
      (values (deprecation-info-state info)
              (list (deprecation-info-software info)
                    (deprecation-info-version info))
              (deprecation-info-replacements info)))))

;;; Without a proclaimed type, the call is "untrusted" and so the compiler
;;; would generate a post-call check that the function did not return.
(declaim (ftype (function (t t t t t) nil) deprecation-error))
(define-error-wrapper deprecation-error (software version namespace name replacements)
  (error 'deprecation-error
         :namespace namespace
         :name name
         :software software
         :version version
         :replacements (normalize-deprecation-replacements replacements)))

(defun deprecation-warn (state software version namespace name replacements
                         &key (runtime-error (neq :early state)))
  (warn (ecase state
          (:early 'early-deprecation-warning)
          (:late 'late-deprecation-warning)
          (:final 'final-deprecation-warning))
        :namespace namespace
        :name name
        :software software
        :version version
        :replacements (normalize-deprecation-replacements replacements)
        :runtime-error runtime-error))

(defun check-deprecated-thing (namespace name)
  (multiple-value-bind (state since replacements)
      (deprecated-thing-p namespace name)
    (when state
      (deprecation-warn
       state (first since) (second since) namespace name replacements)
      (values state since replacements))))

(defun check-deprecated-type (type-specifier)
  (typecase type-specifier
    ((or symbol cons)
     (%check-deprecated-type type-specifier))
    (class
     (let ((name (cl:class-name type-specifier)))
       (when (and name (symbolp name)
                  (eq type-specifier (find-class name nil)))
         (%check-deprecated-type name))))))

;;; STATE is one of
;;;
;;;   :EARLY, for a compile-time style-warning.
;;;   :LATE, for a compile-time full warning.
;;;   :FINAL, for a compile-time full warning and runtime error.
;;;
;;; Suggested duration of each stage is one year, but some things can move faster,
;;; and some widely used legacy APIs might need to move slower. Internals we don't
;;; usually add deprecation notes for, but sometimes an internal API actually has
;;; several external users, in which case we try to be nice about it.
;;;
;;; When you deprecate something, note it here till it is fully gone: makes it
;;; easier to keep things progressing orderly. Also add the relevant section
;;; (or update it when deprecation proceeds) in the manual, in
;;; deprecated.texinfo.
;;;
;;; EARLY:
;;; - SOCKINT::WIN32-BIND                          since 1.2.10 (03/2015)    -> Late: 08/2015
;;; - SOCKINT::WIN32-GETSOCKNAME                   since 1.2.10 (03/2015)    -> Late: 08/2015
;;; - SOCKINT::WIN32-LISTEN                        since 1.2.10 (03/2015)    -> Late: 08/2015
;;; - SOCKINT::WIN32-RECV                          since 1.2.10 (03/2015)    -> Late: 08/2015
;;; - SOCKINT::WIN32-RECVFROM                      since 1.2.10 (03/2015)    -> Late: 08/2015
;;; - SOCKINT::WIN32-SEND                          since 1.2.10 (03/2015)    -> Late: 08/2015
;;; - SOCKINT::WIN32-SENDTO                        since 1.2.10 (03/2015)    -> Late: 08/2015
;;; - SOCKINT::WIN32-CLOSE                         since 1.2.10 (03/2015)    -> Late: 08/2015
;;; - SOCKINT::WIN32-CONNECT                       since 1.2.10 (03/2015)    -> Late: 08/2015
;;; - SOCKINT::WIN32-GETPEERNAME                   since 1.2.10 (03/2015)    -> Late: 08/2015
;;; - SOCKINT::WIN32-IOCTL                         since 1.2.10 (03/2015)    -> Late: 08/2015
;;; - SOCKINT::WIN32-SETSOCKOPT                    since 1.2.10 (03/2015)    -> Late: 08/2015
;;; - SOCKINT::WIN32-GETSOCKOPT                    since 1.2.10 (03/2015)    -> Late: 08/2015

(defun setup-function-in-final-deprecation
    (software version name replacement-spec)
  #+sb-xc-host (declare (ignore software version name replacement-spec))
  #-sb-xc-host
  (setf (fdefinition name)
        (set-closure-name
         (lambda (&rest args)
           (declare (ignore args))
           (deprecation-error software version 'function name replacement-spec))
         t
         name)))

(defun setup-variable-in-final-deprecation
    (software version name replacement-spec)
  (sb-c::%define-symbol-macro
   name
   `(deprecation-error
     ,software ,version 'variable ',name
     (list ,@(mapcar
              (lambda (replacement)
                `',replacement)
              (normalize-deprecation-replacements replacement-spec))))
   nil))

(defun setup-type-in-final-deprecation
    (software version name replacement-spec)
  (declare (ignore software version replacement-spec))
  (%deftype name (constant-type-expander name t) nil))

;; Given DECLS as returned by from parse-body, and SYMBOLS to be bound
;; (with LET, MULTIPLE-VALUE-BIND, etc) return two sets of declarations:
;; those which pertain to the variables and those which don't.
;; The first returned value is NIL or a single expression headed by DECLARE.
;; The second is a list of expressions resembling the input DECLS.
(defun extract-var-decls (decls symbols)
  (unless symbols ; Don't bother filtering DECLS, just return them.
    (return-from extract-var-decls (values nil decls)))
  (labels ((applies-to-variables (decl)
             ;; If DECL is a variable-affecting declaration, then return
             ;; the subset of SYMBOLS to which DECL applies.
             (let ((id (car decl)))
               (remove-if (lambda (x) (not (memq x symbols)))
                          (cond ((eq id 'type)
                                 (cddr decl))
                                ((or (listp id) ; must be a type-specifier
                                     (memq id '(special ignorable ignore
                                                dynamic-extent
                                                sb-c::constant-value
                                                sb-c::no-constraints))
                                     (info :type :kind id))
                                 (cdr decl))))))
           (partition (spec)
             ;; If SPEC is a declaration affecting some variables in SYMBOLS
             ;; and some not, split it into two mutually exclusive declarations.
             (acond ((applies-to-variables spec)
                     (multiple-value-bind (decl-head all-symbols)
                         (if (eq (car spec) 'type)
                             (values `(type ,(cadr spec)) (cddr spec))
                             (values `(,(car spec)) (cdr spec)))
                       (let ((more (set-difference all-symbols it)))
                         (values `(,@decl-head ,@it)
                                 (and more `(,@decl-head ,@more))))))
                    (t
                     (values nil spec)))))
    ;; This loop is less inefficient than theoretically possible,
    ;; reconstructing the tree even if no need,
    ;; but it's just a macroexpander, so... fine.
    (collect ((binding-decls))
      (let ((filtered
             (mapcar (lambda (decl-expr) ; a list headed by DECLARE
                       (mapcan (lambda (spec)
                                 (multiple-value-bind (binding other)
                                     (partition spec)
                                   (when binding
                                     (binding-decls binding))
                                   (if other (list other))))
                               (cdr decl-expr)))
                     decls)))
        (values (awhen (binding-decls) `(declare ,@it))
                (mapcan (lambda (x) (if x (list `(declare ,@x)))) filtered))))))

;;; Delayed evaluation
(defmacro delay (form)
  `(cons nil (lambda () ,form)))

(defun force (promise)
  (cond ((not (consp promise)) promise)
        ((car promise) (cdr promise))
        (t (setf (car promise) t
                 (cdr promise) (funcall (cdr promise))))))

(defun promise-ready-p (promise)
  (or (not (consp promise))
      (car promise)))

;;; Bind a few "potentially dangerous" printer control variables to
;;; safe values, respecting current values if possible.
(defmacro with-sane-io-syntax (&body forms)
  `(call-with-sane-io-syntax (lambda () ,@forms)))

(defun call-with-sane-io-syntax (function)
  (declare (type function function))
  (declare (dynamic-extent function))
  ;; force BOUNDP to be tested by declaring maximal safety
  ;; in case unsafe code really screwed things up.
  (declare (optimize (safety 3)))
  (macrolet ((true (sym)
               `(and (boundp ',sym) ,sym)))
    (let ((*print-readably* nil)
          (*print-level* (or (true *print-level*) 6))
          (*print-length* (or (true *print-length*) 12))
          #-sb-xc-host (*print-vector-length* (or (true *print-vector-length*) 200)))
      (funcall function))))

;;; Returns a list of members of LIST. Useful for dealing with circular lists.
;;; For a dotted list returns a secondary value of T -- in which case the
;;; primary return value does not include the dotted tail.
;;; If the maximum length is reached, return a secondary value of :MAYBE.
(defun list-members (list &key max-length)
  (when list
    (do ((tail (cdr list) (cdr tail))
         (members (list (car list)) (cons (car tail) members))
         (count 0 (1+ count)))
        ((or (not (consp tail)) (eq tail list)
             (and max-length (>= count max-length)))
         (values members (or (not (listp tail))
                             (and (>= count max-length) :maybe)))))))

;; This is not my preferred name for this function, but chosen for harmony
;; with everything else that refers to these as 'hash-caches'.
;; Hashing is just one particular way of memoizing, and it would have been
;; slightly more abstract and yet at the same time more concrete to say
;; "memoized-function-caches". "hash-caches" is pretty nonspecific.
#.(if *profile-hash-cache*
'(defun show-hash-cache-statistics ()
  (flet ((cache-stats (symbol)
           (let* ((name (string symbol))
                  (statistics (package-symbolicate (sb-int-package) symbol "-STATS"))
                  (prefix
                   (subseq name 0 (- (length name) (length "VECTOR**")))))
             (values (if (boundp statistics)
                         (symbol-value statistics)
                         (make-array 3 :element-type 'fixnum))
                     (subseq prefix 2 (1- (length prefix)))))))
    (format t "~%Type function memoization:~%     Seek       Hit      (%)~:
    Evict      (%) Size    full~%")
    ;; Sort by descending seek count to rank by likely relative importance
    (dolist (symbol (sort (copy-list *cache-vector-symbols*) #'>
                          :key (lambda (x) (aref (cache-stats x) 0))))
      (binding* (((stats short-name) (cache-stats symbol))
                 (seek (aref stats 0))
                 (miss (aref stats 1))
                 (hit (- seek miss))
                 (evict (aref stats 2))
                 (cache (symbol-value symbol)))
          (format t "~9d ~9d (~5,1f%) ~8d (~5,1f%) ~4d ~6,1f% ~A~%"
                  seek hit
                  (if (plusp seek) (* 100 (/ hit seek)))
                  evict
                  (if (plusp seek) (* 100 (/ evict seek)))
                  (length cache)
                  (if (plusp (length cache))
                      (* 100 (/ (count-if-not #'fixnump cache)
                                (length cache))))
                  short-name))))))

;;; some commonly-occurring CONSTANTLY forms
(macrolet ((def-constantly-fun (name constant-expr)
             `(defun ,name (&rest arguments)
                (declare (ignore arguments))
                (declare (optimize (speed 3) (safety 0) (debug 0)))
                ,constant-expr)))
  (def-constantly-fun constantly-t t)
  (def-constantly-fun constantly-nil nil)
  (def-constantly-fun constantly-0 0))

(in-package "SB-KERNEL")

(defun fp-zero-p (x)
  (typecase x
    (single-float (zerop x))
    (double-float (zerop x))
    #+long-float
    (long-float (zerop x))
    (t nil)))

(declaim (inline schwartzian-stable-sort-list))
(defun schwartzian-stable-sort-list (list comparator &key key)
  (if (null key)
      (stable-sort (copy-list list) comparator)
      (let* ((key (if (functionp key)
                      key
                      (symbol-function key)))
             (wrapped (mapcar (lambda (x)
                                (cons x (funcall key x)))
                              list))
             (sorted (stable-sort wrapped comparator :key #'cdr)))
        (map-into sorted #'car sorted))))

(declaim (inline schwartzian-stable-sort-vector))
(defun schwartzian-stable-sort-vector (vector comparator &key key)
  (if (null key)
      (stable-sort (copy-seq vector) comparator)
      (let* ((key (if (functionp key)
                      key
                      (symbol-function key)))
             (wrapped (map 'vector (lambda (x)
                                     (cons x (funcall key x)))
                           vector))
             (sorted (stable-sort wrapped comparator :key #'cdr)))
        (map-into sorted #'car sorted))))

;;; Return T if X is an object that always evaluates to itself. Guard against:
;;;  (LET ((S 'XXX)) (SET S 9) (UNINTERN S) (IMPORT S 'KEYWORD) (SYMBOL-VALUE S)) => 9
;;; whereby :XXX satisfies KEWORDP and has value 9, or maybe even has no value
;;; if not assigned anything previously.
;;; Interestingly, CLISP and CCL cause the symbol-value to become itself
;;; (and MAKUNBOUND to be illegal). That's an interesting choice which closes
;;; a weird loophole, but is not universal in all implementations.
(defun self-evaluating-p (x)
  (typecase x
    (null t)
    (symbol
     (or (eq x t)
         (and (eq (cl:symbol-package x) *keyword-package*)
              (boundp x)
              (eq (symbol-value x) x))))
    (cons nil)
    (t t)))

(declaim (inline first-bit-set))
(defun first-bit-set (x)
  #+(and x86-64 (not sb-xc-host))
  (truly-the (values (mod #.sb-vm:n-word-bits) &optional)
             (%primitive sb-vm::unsigned-word-find-first-bit (the word x)))
  #-(and x86-64 (not sb-xc-host))
  (1- (integer-length (logand x (- x)))))

(defun integer-float-p (float)
  (and (floatp float)
       (multiple-value-bind (significand exponent) (integer-decode-float float)
         (or (plusp exponent)
             (<= (- exponent) (first-bit-set significand))))))


(defvar *top-level-form-p* nil)
