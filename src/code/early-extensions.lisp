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

(in-package "SB!IMPL")

(defvar *core-pathname* nil
  #!+sb-doc
  "The absolute pathname of the running SBCL core.")

(defvar *runtime-pathname* nil
  #!+sb-doc
  "The absolute pathname of the running SBCL runtime.")

;;; something not EQ to anything we might legitimately READ
(defglobal *eof-object* (make-symbol "EOF-OBJECT"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant max-hash sb!xc:most-positive-fixnum))

(def!type hash ()
  `(integer 0 ,max-hash))

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
(def!type index () `(integer 0 (,sb!xc:array-dimension-limit)))

;;; like INDEX, but only up to half the maximum. Used by hash-table
;;; code that does plenty to (aref v (* 2 i)) and (aref v (1+ (* 2 i))).
(def!type index/2 () `(integer 0 (,(floor sb!xc:array-dimension-limit 2))))

;;; like INDEX, but augmented with -1 (useful when using the index
;;; to count downwards to 0, e.g. LOOP FOR I FROM N DOWNTO 0, with
;;; an implementation which terminates the loop by testing for the
;;; index leaving the loop range)
(def!type index-or-minus-1 () `(integer -1 (,sb!xc:array-dimension-limit)))

;;; A couple of VM-related types that are currently used only on the
;;; alpha platform. -- CSR, 2002-06-24
(def!type unsigned-byte-with-a-bite-out (s bite)
  (cond ((eq s '*) 'integer)
        ((and (integerp s) (> s 0))
         (let ((bound (ash 1 s)))
           `(integer 0 ,(- bound bite 1))))
        (t
         (error "Bad size specified for UNSIGNED-BYTE type specifier: ~S." s))))

;;; Motivated by the mips port. -- CSR, 2002-08-22
(def!type signed-byte-with-a-bite-out (s bite)
  (cond ((eq s '*) 'integer)
        ((and (integerp s) (> s 1))
         (let ((bound (ash 1 (1- s))))
           `(integer ,(- bound) ,(- bound bite 1))))
        (t
         (error "Bad size specified for SIGNED-BYTE type specifier: ~S." s))))

(def!type load/store-index (scale lowtag min-offset
                                 &optional (max-offset min-offset))
  `(integer ,(- (truncate (+ (ash 1 16)
                             (* min-offset sb!vm:n-word-bytes)
                             (- lowtag))
                          scale))
            ,(truncate (- (+ (1- (ash 1 16)) lowtag)
                          (* max-offset sb!vm:n-word-bytes))
                       scale)))

#!+(or x86 x86-64)
(defun displacement-bounds (lowtag element-size data-offset)
  (let* ((adjustment (- (* data-offset sb!vm:n-word-bytes) lowtag))
         (bytes-per-element (ceiling element-size sb!vm:n-byte-bits))
         (min (truncate (+ sb!vm::minimum-immediate-offset adjustment)
                        bytes-per-element))
         (max (truncate (+ sb!vm::maximum-immediate-offset adjustment)
                        bytes-per-element)))
    (values min max)))

#!+(or x86 x86-64)
(def!type constant-displacement (lowtag element-size data-offset)
  (flet ((integerify (x)
           (etypecase x
             (integer x)
             (symbol (symbol-value x)))))
    (let ((lowtag (integerify lowtag))
          (element-size (integerify element-size))
          (data-offset (integerify data-offset)))
      (multiple-value-bind (min max) (displacement-bounds lowtag
                                                          element-size
                                                          data-offset)
        `(integer ,min ,max)))))

;;; the default value used for initializing character data. The ANSI
;;; spec says this is arbitrary, so we use the value that falls
;;; through when we just let the low-level consing code initialize
;;; all newly-allocated memory to zero.
;;;
;;; KLUDGE: It might be nice to use something which is a
;;; STANDARD-CHAR, both to reduce user surprise a little and, probably
;;; more significantly, to help SBCL's cross-compiler (which knows how
;;; to dump STANDARD-CHARs). Unfortunately, the old CMU CL code is
;;; shot through with implicit assumptions that it's #\NULL, and code
;;; in several places (notably both DEFUN MAKE-ARRAY and DEFTRANSFORM
;;; MAKE-ARRAY) would have to be rewritten. -- WHN 2001-10-04
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; an expression we can use to construct a DEFAULT-INIT-CHAR value
  ;; at load time (so that we don't need to teach the cross-compiler
  ;; how to represent and dump non-STANDARD-CHARs like #\NULL)
  (defparameter *default-init-char-form* '(code-char 0)))

;;; CHAR-CODE values for ASCII characters which we care about but
;;; which aren't defined in section "2.1.3 Standard Characters" of the
;;; ANSI specification for Lisp
;;;
;;; KLUDGE: These are typically used in the idiom (CODE-CHAR
;;; FOO-CHAR-CODE). I suspect that the current implementation is
;;; expanding this idiom into a full call to CODE-CHAR, which is an
;;; annoying overhead. I should check whether this is happening, and
;;; if so, perhaps implement a DEFTRANSFORM or something to stop it.
;;; (or just find a nicer way of expressing characters portably?) --
;;; WHN 19990713
(def!constant bell-char-code 7)
(def!constant backspace-char-code 8)
(def!constant tab-char-code 9)
(def!constant line-feed-char-code 10)
(def!constant form-feed-char-code 12)
(def!constant return-char-code 13)
(def!constant escape-char-code 27)
(def!constant rubout-char-code 127)

;;;; type-ish predicates

;;; X may contain cycles -- a conservative approximation. This
;;; occupies a somewhat uncomfortable niche between being fast for
;;; common cases (we don't want to allocate a hash-table), and not
;;; falling down to exponential behaviour for large trees (so we set
;;; an arbitrady depth limit beyond which we punt).
(defun maybe-cyclic-p (x &optional (depth-limit 12))
  (and (listp x)
       (labels ((safe-cddr (cons)
                  (let ((cdr (cdr cons)))
                    (when (consp cdr)
                      (cdr cdr))))
                (check-cycle (object seen depth)
                  (when (and (consp object)
                             (or (> depth depth-limit)
                                 (member object seen)
                                 (circularp object seen depth)))
                    (return-from maybe-cyclic-p t)))
                (circularp (list seen depth)
                  ;; Almost regular circular list detection, with a twist:
                  ;; we also check each element of the list for upward
                  ;; references using CHECK-CYCLE.
                  (do ((fast (cons (car list) (cdr list)) (safe-cddr fast))
                       (slow list (cdr slow)))
                      ((not (consp fast))
                       ;; Not CDR-circular, need to check remaining CARs yet
                       (do ((tail slow (and (cdr tail))))
                           ((not (consp tail))
                            nil)
                         (check-cycle (car tail) (cons tail seen) (1+ depth))))
                    (check-cycle (car slow) (cons slow seen) (1+ depth))
                    (when (eq fast slow)
                      (return t)))))
         (circularp x (list x) 0))))

;;; Is X a (possibly-improper) list of at least N elements?
(declaim (ftype (function (t index)) list-of-length-at-least-p))
(defun list-of-length-at-least-p (x n)
  (or (zerop n) ; since anything can be considered an improper list of length 0
      (and (consp x)
           (list-of-length-at-least-p (cdr x) (1- n)))))

(declaim (inline ensure-list))
(defun ensure-list (thing)
  (if (listp thing) thing (list thing)))

;;; Is X is a positive prime integer?
(defun positive-primep (x)
  ;; This happens to be called only from one place in sbcl-0.7.0, and
  ;; only for fixnums, we can limit it to fixnums for efficiency. (And
  ;; if we didn't limit it to fixnums, we should use a cleverer
  ;; algorithm, since this one scales pretty badly for huge X.)
  (declare (fixnum x))
  (if (<= x 5)
      (and (>= x 2) (/= x 4))
      (and (not (evenp x))
           (not (zerop (rem x 3)))
           (do ((q 6)
                (r 1)
                (inc 2 (logxor inc 6)) ;; 2,4,2,4...
                (d 5 (+ d inc)))
               ((or (= r 0) (> d q)) (/= r 0))
             (declare (fixnum inc))
             (multiple-value-setq (q r) (truncate x d))))))

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
;;; MACROLET definitions created by COLLECT if collecting a list.
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
;;; which defaults to NIL. FUNCTION is the function which does the
;;; collection. It is a function which will accept two arguments: the
;;; value to be collected and the current collection. The result of
;;; the function is made the new value for the collection. As a
;;; totally magical special-case, FUNCTION may be COLLECT, which tells
;;; us to build a list in forward order; this is the default. If an
;;; INITIAL-VALUE is supplied for COLLECT, the stuff will be RPLACD'd
;;; onto the end. Note that FUNCTION may be anything that can appear
;;; in the functional position, including macros and lambdas.
(defmacro collect (collections &body body)
  (let ((macros ())
        (binds ())
        (ignores ()))
    (dolist (spec collections)
      (destructuring-bind (name &optional default collector
                                &aux (n-value (copy-symbol name))) spec
        (push `(,n-value ,default) binds)
        (let ((macro-body
               (if (or (null collector) (eq collector 'collect))
                   (let ((n-tail
                          (make-symbol
                           (concatenate 'string (symbol-name name) "-TAIL"))))
                     (push n-tail ignores)
                     (push `(,n-tail ,(if default `(last ,n-value))) binds)
                     `(collect-list-expander ',n-value ',n-tail args))
                   ``(progn
                       ,@(mapcar (lambda (x)
                                   `(setq ,',n-value (,',collector ,x ,',n-value)))
                                 args)
                       ,',n-value))))
          (push `(,name (&rest args) ,macro-body) macros))))
    `(macrolet ,macros
       (let* ,(nreverse binds)
         ;; Even if the user reads each collection result,
         ;; reader conditionals might statically eliminate all writes.
         ;; Since we don't know, all the -n-tail variable are ignorable.
         ,@(if ignores `((declare (ignorable ,@ignores))))
         ,@body))))

;;;; some old-fashioned functions. (They're not just for old-fashioned
;;;; code, they're also used as optimized forms of the corresponding
;;;; general functions when the compiler can prove that they're
;;;; equivalent.)

;;; like (MEMBER ITEM LIST :TEST #'EQ)
(defun memq (item list)
  #!+sb-doc
  "Return tail of LIST beginning with first element EQ to ITEM."
  ;; KLUDGE: These could be and probably should be defined as
  ;;   (MEMBER ITEM LIST :TEST #'EQ)),
  ;; but when I try to cross-compile that, I get an error from
  ;; LTN-ANALYZE-KNOWN-CALL, "Recursive known function definition". The
  ;; comments for that error say it "is probably a botched interpreter stub".
  ;; Rather than try to figure that out, I just rewrote this function from
  ;; scratch. -- WHN 19990512
  (do ((i list (cdr i)))
      ((null i))
    (when (eq (car i) item)
      (return i))))

;;; like (ASSOC ITEM ALIST :TEST #'EQ):
;;;   Return the first pair of ALIST where ITEM is EQ to the key of
;;;   the pair.
(defun assq (item alist)
  ;; KLUDGE: CMU CL defined this with
  ;;   (DECLARE (INLINE ASSOC))
  ;;   (ASSOC ITEM ALIST :TEST #'EQ))
  ;; which is pretty, but which would have required adding awkward
  ;; build order constraints on SBCL (or figuring out some way to make
  ;; inline definitions installable at build-the-cross-compiler time,
  ;; which was too ambitious for now). Rather than mess with that, we
  ;; just define ASSQ explicitly in terms of more primitive
  ;; operations:
  (dolist (pair alist)
    ;; though it may look more natural to write this as
    ;;   (AND PAIR (EQ (CAR PAIR) ITEM))
    ;; the temptation to do so should be resisted, as pointed out by PFD
    ;; sbcl-devel 2003-08-16, as NIL elements are rare in association
    ;; lists.  -- CSR, 2003-08-16
    (when (and (eq (car pair) item) (not (null pair)))
      (return pair))))

;;; like (DELETE .. :TEST #'EQ):
;;;   Delete all LIST entries EQ to ITEM (destructively modifying
;;;   LIST), and return the modified LIST.
(defun delq (item list)
  (let ((list list))
    (do ((x list (cdr x))
         (splice '()))
        ((endp x) list)
      (cond ((eq item (car x))
             (if (null splice)
               (setq list (cdr x))
               (rplacd splice (cdr x))))
            (t (setq splice x)))))) ; Move splice along to include element.


;;; like (POSITION .. :TEST #'EQ):
;;;   Return the position of the first element EQ to ITEM.
(defun posq (item list)
  (do ((i list (cdr i))
       (j 0 (1+ j)))
      ((null i))
    (when (eq (car i) item)
      (return j))))

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

;;; like Scheme's named LET
;;;
;;; (CMU CL called this ITERATE, and commented it as "the ultimate
;;; iteration macro...". I (WHN) found the old name insufficiently
;;; specific to remind me what the macro means, so I renamed it.)
(defmacro named-let (name binds &body body)
  (dolist (x binds)
    (unless (proper-list-of-length-p x 2)
      (error "malformed NAMED-LET variable spec: ~S" x)))
  `(labels ((,name ,(mapcar #'first binds) ,@body))
     (,name ,@(mapcar #'second binds))))

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
            `(with-locked-system-table (,n-table) ,iter-form)
            iter-form))))

;;; Executes BODY for all entries of PLIST with KEY and VALUE bound to
;;; the respective keys and values.
(defmacro doplist ((key val) plist &body body)
  (with-unique-names (tail)
    `(let ((,tail ,plist) ,key ,val)
       (loop (when (null ,tail) (return nil))
             (setq ,key (pop ,tail))
             (when (null ,tail)
               (error "malformed plist, odd number of elements"))
             (setq ,val (pop ,tail))
             (progn ,@body)))))

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
(defvar *cache-vector-symbols* nil)

(defun drop-all-hash-caches ()
  (dolist (name *cache-vector-symbols*)
    (set name nil)))

;; Make a new hash-cache and optionally create the statistics vector.
(defun alloc-hash-cache (size symbol)
  (let (cache)
    ;; It took me a while to figure out why infinite recursion could occur
    ;; in VALUES-SPECIFIER-TYPE. It's because SET calls VALUES-SPECIFIER-TYPE.
    (macrolet ((set! (symbol value)
                 `(#+sb-xc-host set
                   #-sb-xc-host sb!kernel:%set-symbol-global-value
                   ,symbol ,value))
               (reset-stats ()
                 ;; If statistics gathering is not not compiled-in,
                 ;; no sense in setting a symbol that is never used.
                 ;; While this uses SYMBOLICATE at runtime,
                 ;; it is inconsequential to performance.
                 (if *profile-hash-cache*
                     `(let ((statistics
                             (let ((*package* (symbol-package symbol)))
                               (symbolicate symbol "STATISTICS"))))
                        (unless (boundp statistics)
                          (set! statistics
                                (make-array 3 :element-type 'fixnum
                                              :initial-contents '(1 0 0))))))))
      ;; It would be bad if another thread sees MAKE-ARRAY's result in the
      ;; global variable before the vector's header+length have been set.
      ;; Without a barrier, this would be theoretically possible if the
      ;; architecture allows out-of-order memory writes.
      (sb!thread:barrier (:write)
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
                    (declare (optimize (safety 0)))
                    ,(if (<= n 3)
                         `(list* ,@args)
                         `(vector ,@args)))))))
  (def 2)
  (def 3)
  (def 4)
  (def 5)
  (def 6))

(defmacro !define-hash-cache (name args aux-vars
                              &key hash-function hash-bits memoizer
                              flush-function (values 1))
  (declare (ignore memoizer))
  (dolist (arg args)
    (unless (<= 2 (length arg) 3)
      (error "bad argument spec: ~S" arg)))
  (assert (typep hash-bits '(integer 5 14))) ; reasonable bounds
  (let* ((fun-name (symbolicate "!" name "-MEMO-WRAPPER"))
         (var-name (symbolicate "**" name "-CACHE-VECTOR**"))
         (statistics-name
          (when *profile-hash-cache*
            (symbolicate var-name "STATISTICS")))
         (nargs (length args))
         (size (ash 1 hash-bits))
         (hashval (make-symbol "HASH"))
         (cache (make-symbol "CACHE"))
         (entry (make-symbol "LINE"))
         (thunk (make-symbol "THUNK"))
         (arg-vars (mapcar #'first args))
         (nvalues (if (listp values) (length values) values))
         (result-temps
          (if (listp values)
              values ; use the names provided by the user
              (loop for i from 1 to nvalues ; else invent some names
                    collect (make-symbol (format nil "R~D" i)))))
         (temps (append (mapcar (lambda (x) (make-symbol (string x)))
                                arg-vars)
                        result-temps))
         ;; Mnemonic: (FIND x SEQ :test #'f) calls f with x as the LHS
         (tests (mapcar (lambda (spec temp) ; -> (EQx ARG #:ARG)
                          `(,(cadr spec) ,(car spec) ,temp))
                        args temps))
         (cache-type `(simple-vector ,size))
         (line-type (let ((n (+ nargs nvalues)))
                      (if (<= n 3) 'cons `(simple-vector ,n))))
         (bind-hashval
          `((,hashval (the (signed-byte #.sb!vm:n-fixnum-bits)
                           (funcall ,hash-function ,@arg-vars)))
            (,cache ,var-name)))
         (probe-it
          (lambda (ignore action)
            `(when ,cache
               (let ((,hashval ,hashval) ; gets clobbered in probe loop
                     (,cache (truly-the ,cache-type ,cache)))
                 ;; FIXME: redundant?
                 (declare (type (signed-byte #.sb!vm:n-fixnum-bits) ,hashval))
                 (loop repeat 2
                    do (let ((,entry
                              (svref ,cache
                                     (ldb (byte ,hash-bits 0) ,hashval))))
                         (unless (eql ,entry 0)
                           ;; This barrier is a no-op on all multi-threaded SBCL
                           ;; architectures. No CPU except Alpha will move a
                           ;; load prior to a load on which it depends.
                           (sb!thread:barrier (:data-dependency))
                           (locally (declare (type ,line-type ,entry))
                             (let* ,(case (length temps)
                                     (2 `((,(first temps) (car ,entry))
                                          (,(second temps) (cdr ,entry))))
                                     (3 (let ((arg-temp (sb!xc:gensym "ARGS")))
                                          `((,arg-temp (cdr ,entry))
                                            (,(first temps) (car ,entry))
                                            (,(second temps)
                                             (car (truly-the cons ,arg-temp)))
                                            (,(third temps) (cdr ,arg-temp)))))
                                     (t (loop for i from 0 for x in temps
                                              collect `(,x (svref ,entry ,i)))))
                               ,@ignore
                               (when (and ,@tests) ,action))))
                         (setq ,hashval (ash ,hashval ,(- hash-bits)))))))))
         (fun
          `(defun ,fun-name (,thunk ,@arg-vars ,@aux-vars)
             ,@(when *profile-hash-cache* ; count seeks
                 `((when (boundp ',statistics-name)
                     (incf (aref ,statistics-name 0)))))
             (let ,bind-hashval
               ,(funcall probe-it nil
                         `(return-from ,fun-name (values ,@result-temps)))
               (multiple-value-bind ,result-temps (funcall ,thunk)
                 (let ((,entry
                        (,(hash-cache-line-allocator (+ nargs nvalues))
                         ,@(mapcar (lambda (spec) (or (caddr spec) (car spec)))
                                   args)
                         ,@result-temps))
                       (,cache
                        (truly-the ,cache-type
                         (or ,cache (alloc-hash-cache ,size ',var-name))))
                       (idx1 (ldb (byte ,hash-bits 0) ,hashval))
                       (idx2 (ldb (byte ,hash-bits ,hash-bits) ,hashval)))
                   ,@(when *profile-hash-cache*
                       `((incf (aref ,statistics-name 1)))) ; count misses
                   ;; Why a barrier: the pointer to 'entry' (a cons or vector)
                   ;; MUST NOT be observed by another thread before its cells
                   ;; are filled. Equally bad, the 'output' cells in the line
                   ;; could be 0 while the 'input' cells matched something.
                   (sb!thread:barrier (:write))
                   (cond ((eql (svref ,cache idx1) 0)
                          (setf (svref ,cache idx1) ,entry))
                         ((eql (svref ,cache idx2) 0)
                          (setf (svref ,cache idx2) ,entry))
                         (t
                           ,@(when *profile-hash-cache* ; count evictions
                               `((incf (aref ,statistics-name 2))))
                           (setf (svref ,cache idx1) ,entry))))
                 (values ,@result-temps))))))
    `(progn
       (pushnew ',var-name *cache-vector-symbols*)
       (defglobal ,var-name nil)
       ,@(when *profile-hash-cache*
           `((declaim (type (simple-array fixnum (3)) ,statistics-name))
             (defvar ,statistics-name)))
       (declaim (type (or null ,cache-type) ,var-name))
       (defun ,(symbolicate name "-CACHE-CLEAR") () (setq ,var-name nil))
       ,@(when flush-function
           `((defun ,flush-function ,arg-vars
               (let ,bind-hashval
                 ,(funcall probe-it
                   `((declare (ignore ,@result-temps)))
                   `(return (setf (svref ,cache
                                         (ldb (byte ,hash-bits 0) ,hashval))
                                  0)))))))
       (declaim (inline ,fun-name))
       ,fun)))

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

;;; FIXME: this macro holds onto the DEFINE-HASH-CACHE macro,
;;; but should not.
;;;
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
(defmacro defun-cached ((name &rest options &key
                              (memoizer (make-symbol "MEMOIZE")
                                        memoizer-supplied-p)
                              &allow-other-keys)
                        args &body body-decls-doc)
  (binding* (((forms decls doc) (parse-body body-decls-doc t))
             ((inputs aux-vars)
              (let ((aux (member '&aux args)))
                (if aux
                    (values (ldiff args aux) aux)
                    (values args nil))))
             (arg-names (mapcar #'car inputs)))
    `(progn
        (!define-hash-cache ,name ,inputs ,aux-vars ,@options)
        (defun ,name ,arg-names
          ,@decls
          ,@(if doc (list doc))
          (macrolet ((,memoizer (&body body)
                       ;; We don't need (DX-FLET ((,thunk () ,@body)) ...)
                       ;; This lambda is a single-use local call within
                       ;; the inline memoizing wrapper.
                       `(,',(symbolicate "!" name "-MEMO-WRAPPER")
                         (lambda () ,@body) ,@',arg-names)))
             ,@(if memoizer-supplied-p
                   forms
                   `((,memoizer ,@forms))))))))

;;; FIXME: maybe not the best place
;;;
;;; FIXME: think of a better name -- not only does this not have the
;;; CAR recursion of EQUAL, it also doesn't have the special treatment
;;; of pathnames, bit-vectors and strings.
;;;
;;; KLUDGE: This means that we will no longer cache specifiers of the
;;; form '(INTEGER (0) 4).  This is probably not a disaster.
;;;
;;; A helper function for the type system, which is the main user of
;;; these caches: we must be more conservative than EQUAL for some of
;;; our equality tests, because MEMBER and friends refer to EQLity.
;;; So:
(defun equal-but-no-car-recursion (x y)
  (do () (())
    (cond ((eql x y) (return t))
          ((and (consp x)
                (consp y)
                (eql (pop x) (pop y))))
          (t
           (return)))))

;;;; package idioms

;;; Note: Almost always you want to use FIND-UNDELETED-PACKAGE-OR-LOSE
;;; instead of this function. (The distinction only actually matters when
;;; PACKAGE-DESIGNATOR is actually a deleted package, and in that case
;;; you generally do want to signal an error instead of proceeding.)
(defun %find-package-or-lose (package-designator)
  (or (find-package package-designator)
      (error 'simple-package-error
             :package package-designator
             :format-control "The name ~S does not designate any package."
             :format-arguments (list package-designator))))

;;; ANSI specifies (in the section for FIND-PACKAGE) that the
;;; consequences of most operations on deleted packages are
;;; unspecified. We try to signal errors in such cases.
(defun find-undeleted-package-or-lose (package-designator)
  (let ((maybe-result (%find-package-or-lose package-designator)))
    (if (package-%name maybe-result)    ; if not deleted
        maybe-result
        (error 'simple-package-error
               :package maybe-result
               :format-control "The package ~S has been deleted."
               :format-arguments (list maybe-result)))))

;;;; various operations on names

;;; Is NAME a legal function name?
(declaim (inline legal-fun-name-p))
(defun legal-fun-name-p (name)
  (values (valid-function-name-p name)))

(deftype function-name () '(satisfies legal-fun-name-p))

;;; Signal an error unless NAME is a legal function name.
(defun legal-fun-name-or-type-error (name)
  (unless (legal-fun-name-p name)
    (error 'simple-type-error
           :datum name
           :expected-type 'function-name
           :format-control "invalid function name: ~S"
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
       (symbol-package x)
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
                   (,name (sb!xc:gensym ,(symbol-name name))))
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
(declaim (ftype (function () #+(and sb-xc-host ccl) *
                             #-(and sb-xc-host ccl) nil) missing-arg))
(defun missing-arg ()
  #!+sb-doc
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
  `(unless ,expr
     (%failed-aver ',expr)))

(defun %failed-aver (expr)
  (bug "~@<failed AVER: ~2I~_~S~:>" expr))

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
(defun type-bound-number (x)
  (if (consp x)
      (destructuring-bind (result) x result)
      x))

;;; some commonly-occurring CONSTANTLY forms
(macrolet ((def-constantly-fun (name constant-expr)
             `(progn
                (declaim (ftype (sfunction * (eql ,constant-expr)) ,name))
                (setf (symbol-function ',name)
                      (constantly ,constant-expr)))))
  (def-constantly-fun constantly-t t)
  (def-constantly-fun constantly-nil nil)
  (def-constantly-fun constantly-0 0))

;;; If X is a symbol, see whether it is present in *FEATURES*. Also
;;; handle arbitrary combinations of atoms using NOT, AND, OR.
(defun featurep (x)
  (typecase x
    (cons
     (case (car x)
       ((:not not)
        (cond
          ((cddr x)
           (error "too many subexpressions in feature expression: ~S" x))
          ((null (cdr x))
           (error "too few subexpressions in feature expression: ~S" x))
          (t (not (featurep (cadr x))))))
       ((:and and) (every #'featurep (cdr x)))
       ((:or or) (some #'featurep (cdr x)))
       (t
        (error "unknown operator in feature expression: ~S." x))))
    (symbol (not (null (memq x *features*))))
    (t
      (error "invalid feature expression: ~S" x))))


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
(defmacro defprinter ((name
                       &key
                       (conc-name (concatenate 'simple-string
                                               (symbol-name name)
                                               "-"))
                       identity)
                      &rest slot-descs)
  (let ((first? t)
        maybe-print-space
        (reversed-prints nil)
        (stream (sb!xc:gensym "STREAM")))
    (flet ((sref (slot-name)
             `(,(symbolicate conc-name slot-name) structure)))
      (dolist (slot-desc slot-descs)
        (if first?
            (setf maybe-print-space nil
                  first? nil)
            (setf maybe-print-space `(defprinter-print-space ,stream)))
        (cond ((atom slot-desc)
               (push maybe-print-space reversed-prints)
               (push `(defprinter-prin1 ',slot-desc ,(sref slot-desc) ,stream)
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
                                             ',sname ,sname ,stream)))))
                              reversed-prints))
                     (case (first option)
                       (:prin1
                        (stuff `(defprinter-prin1
                                  ',sname ,(second option) ,stream)))
                       (:princ
                        (stuff `(defprinter-princ
                                  ',sname ,(second option) ,stream)))
                       (:test (setq test (second option)))
                       (t
                        (error "bad option: ~S" (first option)))))))))))
    `(defmethod print-object ((structure ,name) ,stream)
       (pprint-logical-block (,stream nil)
         (print-unreadable-object (structure
                                   ,stream
                                   :type t
                                   :identity ,identity)
           ,@(nreverse reversed-prints))))))

(defun print-symbol-with-prefix (stream symbol &optional colon at)
  #!+sb-doc
  "For use with ~/: Write SYMBOL to STREAM as if it is not accessible from
  the current package."
  (declare (ignore colon at))
  ;; Only keywords should be accessible from the keyword package, and
  ;; keywords are always printed with colons, so this guarantees that the
  ;; symbol will not be printed without a prefix.
  (let ((*package* *keyword-package*))
    (write symbol :stream stream :escape t)))

;;;; etc.

;;; Given a pathname, return a corresponding physical pathname.
(defun physicalize-pathname (possibly-logical-pathname)
  (if (typep possibly-logical-pathname 'logical-pathname)
      (translate-logical-pathname possibly-logical-pathname)
      possibly-logical-pathname))

;;;; Deprecating stuff

(deftype deprecation-state ()
  '(member :early :late :final))

(deftype deprecation-software-and-version ()
  '(or string (cons string (cons string null))))

(defun normalize-deprecation-since (since)
  (unless (typep since 'deprecation-software-and-version)
    (error 'simple-type-error
           :datum since
           :expected-type 'deprecation-software-and-version
           :format-control "~@<The value ~S does not designate a ~
                            version or a software name and a version.~@:>"
           :format-arguments (list since)))
  (if (typep since 'string)
      (values nil since)
      (values-list since)))

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

(defun deprecation-error (software version namespace name replacements)
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

;;; For-effect-only variant of CHECK-DEPRECATED-THING for
;;; type-specifiers that descends into compound type-specifiers.
(defun %check-deprecated-type (type-specifier)
  (let ((seen '()))
    ;; KLUDGE: we have to use SPECIFIER-TYPE to sanely traverse
    ;; TYPE-SPECIFIER and detect references to deprecated types. But
    ;; then we may have to drop its cache to get the
    ;; PARSE-DEPRECATED-TYPE condition when TYPE-SPECIFIER is parsed
    ;; again later.
    ;;
    ;; Proper fix would be a
    ;;
    ;;   walk-type function type-specifier
    ;;
    ;; mechanism that could drive VALUES-SPECIFIER-TYPE but also
    ;; things like this function.
    (block nil
      (handler-bind
          ((sb!kernel::parse-deprecated-type
             (lambda (condition)
               (let ((type-specifier (sb!kernel::parse-deprecated-type-specifier
                                      condition)))
                 (aver (symbolp type-specifier))
                 (unless (memq type-specifier seen)
                   (push type-specifier seen)
                   (check-deprecated-thing 'type type-specifier)))))
           ((or error sb!kernel:parse-unknown-type)
             (lambda (condition)
               (declare (ignore condition))
               (return))))
        (specifier-type type-specifier)))))

(defun check-deprecated-type (type-specifier)
  (typecase type-specifier
    ((or symbol cons)
     (%check-deprecated-type type-specifier))
    (class
     ;; FIXME: this case does not acknowledge that improperly named classes
     ;; can exist. Suppose a few classes each have CLASS-NAME = FRED
     ;; but (FIND-CLASS 'FRED) does not return any of them; and simultaneously
     ;; FRED is a completely unrelated type specifier defined via DEFTYPE.
     ;; This should see that class-name does not properly name the class.
     (let ((name (class-name type-specifier)))
       (when (and name (symbolp name))
         (%check-deprecated-type name))))))

;; This is the moral equivalent of a warning from /usr/bin/ld that
;; "gets() is dangerous." You're informed by both the compiler and linker.
(defun loader-deprecation-warn (stuff whence)
  ;; Stuff is a list: ((<state> name . category) ...)
  ;; For now we only deal with category = :FUNCTION so we ignore it.
  (let ((warning-class
         ;; We're only going to warn once (per toplevel form),
         ;; so pick the most stern warning applicable.
         (if (every (lambda (x) (eq (car x) :early)) stuff)
             'simple-style-warning 'simple-warning)))
    (warn warning-class
          :format-control "Reference to deprecated function~P ~S~@[ from ~S~]"
          :format-arguments
          (list (length stuff) (mapcar #'second stuff) whence))))

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
;;;
;;; - SB-C::MERGE-TAIL-CALLS (policy)              since 1.0.53.74 (11/2011) -> Late: 11/2012
;;;
;;; LATE:
;;; - SB-C::STACK-ALLOCATE-DYNAMIC-EXTENT (policy) since 1.0.19.7            -> Final: anytime
;;; - SB-C::STACK-ALLOCATE-VECTOR (policy)         since 1.0.19.7            -> Final: anytime
;;; - SB-C::STACK-ALLOCATE-VALUE-CELLS (policy)    since 1.0.19.7            -> Final: anytime

(defun print-deprecation-replacements (stream replacements &optional colonp atp)
  (declare (ignore colonp atp))
  (apply #'format stream
         (!uncross-format-control
          "~#[~;~
             Use ~/sb!impl:print-symbol-with-prefix/ instead.~;~
             Use ~/sb!impl:print-symbol-with-prefix/ or ~
             ~/sb!impl:print-symbol-with-prefix/ instead.~:;~
             Use~@{~#[~; or~] ~
             ~/sb!impl:print-symbol-with-prefix/~^,~} instead.~
           ~]")
         replacements))

(defun print-deprecation-message (namespace name software version
                                  &optional replacements stream)
  (format stream
          (!uncross-format-control
           "The ~(~A~) ~/sb!impl:print-symbol-with-prefix/ has been ~
            deprecated as of ~@[~A ~]version ~A.~
            ~@[~2%~/sb!impl::print-deprecation-replacements/~]")
          namespace name software version replacements))

(defun setup-function-in-final-deprecation
    (software version name replacement-spec)
  #+sb-xc-host (declare (ignore software version name replacement-spec))
  #-sb-xc-host
  (setf (fdefinition name)
        (sb!impl::set-closure-name
         (lambda (&rest args)
           (declare (ignore args))
           (deprecation-error software version 'function name replacement-spec))
         name)))

(defun setup-variable-in-final-deprecation
    (software version name replacement-spec)
  (sb!c::%define-symbol-macro
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
  (%compiler-deftype name (constant-type-expander name t) nil))

(defmacro define-deprecated-function (state version name replacements lambda-list
                                      &body body)
  (declare (type deprecation-state state)
           (type string version)
           (type function-name name)
           (type (or function-name list) replacements)
           (type list lambda-list)
           #+sb-xc-host (ignore version replacements))
  `(progn
     #-sb-xc-host
     (declaim (deprecated
               ,state ("SBCL" ,version)
               (function ,name ,@(when replacements
                                   `(:replacement ,replacements)))))
     ,(ecase state
        ((:early :late)
         `(defun ,name ,lambda-list
            ,@body))
        ((:final)
         `',name))))

(defmacro define-deprecated-variable (state version name
                                      &key (value nil valuep) replacement)
  (declare (type deprecation-state state)
           (type string version)
           (type symbol name)
           #+sb-xc-host (ignore version replacement))
  `(progn
     #-sb-xc-host
     (declaim (deprecated
               ,state ("SBCL" ,version)
               (variable ,name ,@(when replacement
                                   `(:replacement ,replacement)))))
     ,(ecase state
        ((:early :late)
         `(defvar ,name ,@(when valuep (list value))))
        ((:final)
         `',name))))

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
                                                truly-dynamic-extent))
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

;;; toplevel helper
(defmacro with-rebound-io-syntax (&body body)
  `(%with-rebound-io-syntax (lambda () ,@body)))

(defun %with-rebound-io-syntax (function)
  (declare (type function function))
  (let ((*package* *package*)
        (*print-array* *print-array*)
        (*print-base* *print-base*)
        (*print-case* *print-case*)
        (*print-circle* *print-circle*)
        (*print-escape* *print-escape*)
        (*print-gensym* *print-gensym*)
        (*print-length* *print-length*)
        (*print-level* *print-level*)
        (*print-lines* *print-lines*)
        (*print-miser-width* *print-miser-width*)
        (*print-pretty* *print-pretty*)
        (*print-radix* *print-radix*)
        (*print-readably* *print-readably*)
        (*print-right-margin* *print-right-margin*)
        (*read-base* *read-base*)
        (*read-default-float-format* *read-default-float-format*)
        (*read-eval* *read-eval*)
        (*read-suppress* *read-suppress*)
        (*readtable* *readtable*))
    (funcall function)))

;;; Bind a few "potentially dangerous" printer control variables to
;;; safe values, respecting current values if possible.
(defmacro with-sane-io-syntax (&body forms)
  `(call-with-sane-io-syntax (lambda () ,@forms)))

(defun call-with-sane-io-syntax (function)
  (declare (type function function))
  (macrolet ((true (sym)
               `(and (boundp ',sym) ,sym)))
    (let ((*print-readably* nil)
          (*print-level* (or (true *print-level*) 6))
          (*print-length* (or (true *print-length*) 12)))
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

;;; Default evaluator mode (interpeter / compiler)

(declaim (type (member :compile #!+(or sb-eval sb-fasteval) :interpret)
               *evaluator-mode*))
(!defparameter *evaluator-mode* :compile
  #!+sb-doc
  "Toggle between different evaluator implementations. If set to :COMPILE,
an implementation of EVAL that calls the compiler will be used. If set
to :INTERPRET, an interpreter will be used.")

;; This is not my preferred name for this function, but chosen for harmony
;; with everything else that refers to these as 'hash-caches'.
;; Hashing is just one particular way of memoizing, and it would have been
;; slightly more abstract and yet at the same time more concrete to say
;; "memoized-function-caches". "hash-caches" is pretty nonspecific.
#.(if *profile-hash-cache*
'(defun show-hash-cache-statistics ()
  (flet ((cache-stats (symbol)
           (let* ((name (string symbol))
                  (statistics (let ((*package* (symbol-package symbol)))
                                (symbolicate symbol "STATISTICS")))
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

(in-package "SB!KERNEL")

(defun fp-zero-p (x)
  (typecase x
    (single-float (zerop x))
    (double-float (zerop x))
    #!+long-float
    (long-float (zerop x))
    (t nil)))

(defun neg-fp-zero (x)
  (etypecase x
    (single-float
     (if (eql x 0.0f0)
         (make-unportable-float :single-float-negative-zero)
         0.0f0))
    (double-float
     (if (eql x 0.0d0)
         (make-unportable-float :double-float-negative-zero)
         0.0d0))
    #!+long-float
    (long-float
     (if (eql x 0.0l0)
         (make-unportable-float :long-float-negative-zero)
         0.0l0))))

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

;;; Just like WITH-OUTPUT-TO-STRING but doesn't close the stream,
;;; producing more compact code.
(defmacro with-simple-output-to-string
    ((var &optional string)
     &body body)
  (multiple-value-bind (forms decls) (parse-body body nil)
    (if string
        `(let ((,var (sb!impl::make-fill-pointer-output-stream ,string)))
           ,@decls
           ,@forms)
        `(let ((,var (make-string-output-stream)))
           ,@decls
           ,@forms
           (truly-the (simple-array character (*))
                      (get-output-stream-string ,var))))))

(defun self-evaluating-p (x)
  (typecase x
    (null t)
    (symbol (or (eq x t) (eq (symbol-package x) *keyword-package*)))
    (cons nil)
    (t t)))
