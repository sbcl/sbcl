;;;; PACKAGEs and stuff like that
;;;;
;;;; Note: The code in this file signals many correctable errors. This
;;;; is not just an arbitrary aesthetic decision on the part of the
;;;; implementor -- many of these are specified by ANSI 11.1.1.2.5,
;;;; "Prevention of Name Conflicts in Packages":
;;;;   Within one package, any particular name can refer to at most one
;;;;   symbol. A name conflict is said to occur when there would be more
;;;;   than one candidate symbol. Any time a name conflict is about to
;;;;   occur, a correctable error is signaled.
;;;;

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;;; Thread safety
;;;;
;;;; ...this could still use work, but the basic idea is:
;;;;
;;;; *PACKAGE-GRAPH-LOCK* is held via WITH-PACKAGE-GRAPH while working on
;;;; package graph, including package -> package links, and interning and
;;;; uninterning symbols.
;;;;
;;;; Lock on *ALL-PACKAGES* is held via WITH-PACKAGE-NAMES while
;;;; frobbing name -> package associations.
;;;;
;;;; There should be no deadlocks due to ordering issues between these two, as
;;;; the latter is only held over operations guaranteed to terminate in finite
;;;; time.
;;;;
;;;; Errors may be signalled while holding on to the *PACKAGE-GRAPH-LOCK*,
;;;; which can still lead to pretty damned inconvenient situations -- but
;;;; since FIND-PACKAGE, FIND-SYMBOL from other threads isn't blocked by this,
;;;; the situation isn't *quite* hopeless.
;;;;
;;;; A better long-term solution seems to be in splitting the granularity of
;;;; the *PACKAGE-GRAPH-LOCK* down: for interning a per-package lock should be
;;;; sufficient, though interaction between parallel intern and use-package
;;;; needs to be considered with some care.

(define-load-time-global *package-graph-lock* nil)

(defmacro with-package-graph ((&key) &body forms)
  ;; FIXME: Since name conflicts can be signalled while holding the
  ;; mutex, user code can be run leading to lock ordering problems.
  `(sb-thread:with-recursive-lock (*package-graph-lock*)
     ,@forms))

;;; a map from package names to packages
(define-load-time-global *all-packages* #())
(define-load-time-global *package-table-lock* nil)
(declaim (type simple-vector *all-packages*))
(declaim (type sb-thread:mutex *package-table-lock*))

;;; a map of strings, each of which is a local nickname of some package,
;;; to small integers. For a constant string name, it is faster to search
;;; package local nicknames by the integer ID.
;;; Keys are never deleted from this table.
(define-load-time-global *package-nickname-ids* nil)
(declaim (type (cons info-hashtable fixnum) *package-nickname-ids*))

;;; Local nicknames are stored in an immutable cons of two immutable simple-vectors.
;;; There is a 1:1 correspondence between vector elements but the indices don't line up
;;; since the orderings are different.
;;; The car vector is #("nickname" index "nickname" index ...) ordered alphabetically
;;; where the index is a physical index to the cdr vector.
;;; The cdr is a weak vector and holds #(token #<pkg> token #<pkg> ...) ordered by
;;; token. A token is a fixnum whose low 9 bits are a logical index (half the
;;; physical index) into the car vector and whose upper bits are a nickname-id.
;;; The number of PLNs within a single package is thus constrained to 512,
;;; but there is no limit on the maximum nickname id.
;;; To search by:
;;;  - integer identifier, binary search the vector of packages.
;;;  - nickname, binary search the strings, and use the found index into the
;;;    vector of packages.
;;;  - package, linearly search the package vector's odd indices, use the low
;;;    bits of the found token to reference the string vector.

(defconstant pkgnick-index-bits 9)

;;; Produce an alist ordered by string
(defun package-local-nickname-alist (pkgnicks use-ids &aux result)
  (when pkgnicks
    (let ((strings (the simple-vector (car pkgnicks)))
          (packages (the weak-vector (cdr pkgnicks))))
      ;; Scan backwards so that pushing preserves order
      (loop for i downfrom (1- (length strings)) to 1 by 2
            do (let* ((token (weak-vector-ref packages (1- (aref strings i))))
                      (package (weak-vector-ref packages (aref strings i)))
                      (id (ash token (- pkgnick-index-bits))))
                 (aver (= (ldb (byte pkgnick-index-bits 0) token) (ash i -1)))
                 ;; cull deleted entries
                 (when (and (packagep package) (package-%name package))
                   (let ((pair (cons (svref strings (1- i)) package)))
                     (push (if use-ids (cons id pair) pair) result)))))
      result)))

;;; PKGNICK-UPDATE is the insert and delete operation on the nickname->package map.
;;; If OTHER-PACKAGE is a package, an entry for STRING is inserted; if NIL, the entry
;;; for STRING, if any, is removed. In either case, culling of deleted packages
;;; is performed. When STRING itself is NIL, then only culling is performed.
;;; This function entails quite a bit of work, but it is worth the trouble to maintain
;;; two binary searchable vectors since in comparison to the number of lookups,
;;; alterations to the mapping are exceedingly rare.
(defun pkgnick-update (this-package string other-package)
  (declare (type (or null package) other-package))
  (let ((entry
         (when other-package ; Ensure that STRING has a nickname-id
           (let* ((id-map *package-nickname-ids*)
                  (id (info-puthash (car id-map)
                                    (logically-readonlyize (copy-seq string))
                                    (lambda (old) (or old (atomic-incf (cdr id-map)))))))
             (list* id string other-package))))
        (old (package-%local-nicknames this-package)))
    (loop
     (let* ((alist (merge 'list
                          (let ((alist (package-local-nickname-alist old t)))
                            (if string
                                (remove string alist :test #'string= :key #'cadr)
                                alist))
                          (when entry (list entry))
                          #'string< :key #'cadr))
            (new
             (when alist
               ;; Create new sorted vectors:
               ;;  STRINGS  = #("nickname" index "nickname" index ...)
               ;;  PACKAGES = #(token #<pkg> token #<pkg> ...)
               (let* ((strings (coerce (mapcan (lambda (x) (list (cadr x) nil)) alist)
                                       'vector))
                      (packages (make-weak-vector (length strings))))
                 (loop for item in (sort (copy-list alist) #'< :key #'car)
                       for i from 0 by 2
                       do (let* ((id (car item))
                                 (string-num (position id alist :key #'car))
                                 (token (logior (ash id pkgnick-index-bits) string-num)))
                            (setf (aref strings (1+ (ash string-num 1))) (1+ i)
                                  (weak-vector-ref packages i) token
                                  (weak-vector-ref packages (1+ i)) (cddr item))))
                 (cons strings packages)))))
       (when (eq old (setf old (cas (package-%local-nicknames this-package) old new)))
         (return nil))))))

;;; A macro to help search the nickname vectors.
;;; Return the logical index of KEY in VECTOR (in which each two
;;; elements comprise a key/value pair)
(macrolet ((bsearch (key vector len accessfn)
             `(let ((v ,vector))
                ;; The search operates on a logical index, which is half the physical
                ;; index. The returned value is a physical index.
                (named-let recurse ((start 0) (end (ash ,len -1)))
                  (declare (type (unsigned-byte 9) start end)
                           (optimize (sb-c:insert-array-bounds-checks 0)))
                  (when (< start end)
                    (let* ((i (ash (+ start end) -1))
                           (elt (keyfn (,accessfn v (ash i 1)))))
                      (case (compare ,key elt)
                       (-1 (recurse start i))
                       (+1 (recurse (1+ i) end))
                       (t (1+ (ash i 1))))))))))

  (defun pkgnick-search-by-name (string base-package)
    (labels ((keyfn (x) x)
             (safe-char (s index)
               (if (< index (length s)) (char s index) (code-char 0)))
             (compare (a b)
               (declare (string a) (simple-string b))
               (let ((pos (string/= a b)))
                 (cond ((not pos) 0)
                       ((char< (safe-char a pos) (safe-char b pos)) -1)
                       (t +1)))))
      (declare (inline safe-char))
      (binding* ((nicks (package-%local-nicknames base-package) :exit-if-null)
                 (string->id (the simple-vector (car nicks)))
                 (found (bsearch string string->id (length string->id) svref) :exit-if-null)
                 (package (weak-vector-ref (cdr nicks) (svref string->id found))))
        (cond ((and package (package-%name package)) package)
              (t (pkgnick-update base-package nil nil))))))

  (defun pkgnick-search-by-id (id base-package)
    (flet ((keyfn (x) (ash x (- pkgnick-index-bits)))
           (compare (a b) (signum (- a b))))
      (binding* ((nicks (package-%local-nicknames base-package) :exit-if-null)
                 (id->obj (the weak-vector (cdr nicks)))
                 (found (bsearch id id->obj (weak-vector-len id->obj) weak-vector-ref)
                        :exit-if-null)
                 (package (weak-vector-ref id->obj found)))
        (cond ((and package (package-%name package)) package)
              (t (pkgnick-update base-package nil nil)))))))

;;; Return a nickname for PACKAGE with respect to CURRENT.
;;; This could in theory return a package that currently has no name. However,
;;; at the time of the call to this function from OUTPUT-SYMBOL - the only
;;; consumer - the symbol in question *did* have a non-nil package.
;;; So whatever we say as far as nickname, if any, is fine.
;;; If more than one nickname exists, then one that is alphabetically last wins.
;;; A better tiebreaker  might be either length in characters
;;; or whichever nickname was added most recently.
(defun package-local-nickname (package current)
  (binding* ((nicks (package-%local-nicknames current) :exit-if-null)
             (vector (the weak-vector (cdr nicks))))
    (loop for i downfrom (1- (weak-vector-len vector)) to 1 by 2
          when (eq package (weak-vector-ref vector i))
          return (svref (car nicks)
                        (let ((token (weak-vector-ref vector (1- i))))
                         (ash (ldb (byte pkgnick-index-bits 0) token) 1))))))

;;; This would have to be bumped ~ 2*most-positive-fixnum times to overflow.
(define-load-time-global *package-names-cookie* most-negative-fixnum)
(declaim (fixnum *package-names-cookie*))

;;; *ALL-PACKAGES* is a power-of-2-sized table (physically with 1 extra cell of metadata)
;;; as the backing storage of a closed-addressing hash-set with a peculiar aspect
;;; of allowing one key to appear in multiple buckets. This aspect allows each global name
;;; ("nickname" and "name" being synonymous in this usage) to appear in its respective
;;; hash bucket. By pure coincidence, names for one package could hash to the same bucket,
;;; so removal has to account for that - removal occurs only when the package does not
;;; belong in a bucket via any of its names.
(defconstant pkgtable-fixed-cells 1)
(defmacro pkgtable-count (table)
  `(truly-the fixnum (svref ,table (1- (length ,table)))))

(defmacro pkgtable-bucket-index (vector hash)
  `(logand ,hash (- (length ,vector) ,(1+ pkgtable-fixed-cells))))

(defmacro do-pkg-table (((package-var &optional (keys-var '#:keys)) table-var)
                        &body body
                        &aux (index-var '#:index))
  `(let ((.tbl. ,table-var)
         (cell (list nil)))
     (declare (dynamic-extent cell))
     (loop for ,index-var of-type index
           from 0 below (1- (length .tbl.))
           do (let ((data (svref .tbl. ,index-var)))
                (setf (car cell) data) ; in case DATA is a non-null atom
                (dolist (,package-var (if (%instancep data) cell data))
                  (let ((,keys-var (package-keys ,package-var)))
                    ;; Process representative entry only (where the primary name
                    ;; of the package hashes to this bucket)
                    (when (= ,index-var (pkgtable-bucket-index .tbl. (aref ,keys-var 1)))
                      ,@body)))))))

(defun pkgtable-insert (vector package keys)
  (declare (simple-vector keys))
  (flet ((insert (vector package keys)
           ;; Ensure that PACKAGE is in each bucket it belongs in
           (dotimes (j (floor (length keys) 2))
             (let* ((index (pkgtable-bucket-index vector (aref keys (1+ (* j 2)))))
                    (data (svref vector index)))
               (unless (memq package (ensure-list data))
                 (setf (svref vector index) (cond ((not data) package)
                                                  ((listp data) (cons package data))
                                                  (t (list package data))))
                 (incf (pkgtable-count vector)))))) ; count once for each insertion
         (new-table (size)
           (let* ((n (power-of-two-ceiling size))
                  (vector (make-array (+ n pkgtable-fixed-cells) :initial-element nil)))
             (setf (pkgtable-count vector) 0)
             vector)))
    ;; First maybe rehash, assuming all names need to be added to the table.
    ;; The rehash threshold is 1 item or more per bucket
    (let ((new-count (+ (pkgtable-count vector) (floor (length keys) 2))))
      (when (> new-count (- (length vector) pkgtable-fixed-cells))
        (let ((new-table (new-table new-count)))
          (do-pkg-table ((package keys) vector)
            (insert new-table package keys))
          (setq vector new-table))))
    (insert vector package keys)
    vector))

(defun %get-package (name table &aux (hash (sxhash name))
                                     (index (pkgtable-bucket-index table hash)))
  (declare (string name) (simple-vector table))
  (declare (optimize (sb-c::insert-array-bounds-checks 0) (sb-c::type-check 0)))
  (macrolet ((test (package)
               `(let ((keys (package-keys ,package)))
                  (do ((i (1- (length keys)) (- i 2)))
                      ((minusp i))
                    (declare (index-or-minus-1 i))
                    (when (and (eq (aref keys i) hash)
                               (string= (aref keys (1- i)) name))
                      (return-from %get-package ,package))))))
    (let ((data (svref table index)))
      (if (listp data)
          (dolist (candidate data) (test candidate))
          (test data))))
  (sb-thread:barrier (:read))
  (let ((current-table *all-packages*)) ; retry, maybe table was zapped
    (if (eq table current-table)
        nil
        (%get-package name current-table))))

;;; Use this only if you need to acquire the mutex. Otherwise just accessing
;;; the var is fine.  This no longer allows re-entrance. Let's keep it that way.
(defmacro with-package-names ((&optional table-var) &body body)
  (let ((guts `(sb-thread::with-system-mutex (*package-table-lock*) ,@body)))
    (if table-var
        `(let ((,table-var *all-packages*)) ,guts)
        guts)))

;;;; iteration macros

(define-thread-local *clear-resized-symbol-tables* t)
(defmacro with-package-iterator ((mname package-list &rest symbol-types) &body body)
  "Within the lexical scope of the body forms, MNAME is defined via macrolet
such that successive invocations of (MNAME) will return the symbols, one by
one, from the packages in PACKAGE-LIST. SYMBOL-TYPES may be any
of :INHERITED :EXTERNAL :INTERNAL."
  ;; SYMBOL-TYPES should really be named ACCESSIBILITY-TYPES.
  (when (null symbol-types)
    (%program-error "At least one of :INTERNAL, :EXTERNAL, or :INHERITED must be supplied."))
  (dolist (symbol symbol-types)
    (unless (member symbol '(:internal :external :inherited))
      (%program-error "~S is not one of :INTERNAL, :EXTERNAL, or :INHERITED."
                      symbol)))
  (let ((enable (logior (if (member :external  symbol-types) 1 0)
                        (if (member :internal  symbol-types) 2 0)
                        (if (member :inherited symbol-types) 4 0)))
        (iter '#:iter))
    `(let ((,iter (pkg-iter (package-listify ,package-list) ,enable))
           (*clear-resized-symbol-tables*))
       (declare (dynamic-extent ,iter))
       (macrolet ((,mname ()
                    ;; "status" is the term used in the doc of FIND-SYMBOL for its 2nd value
                    '(multiple-value-bind (status package symbol) (package-iter-step ,iter)
                       (when status (values t symbol status package)))))
         ,@body))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-pkg-iterator (range var body result-form)
    (multiple-value-bind (forms decls) (parse-body body nil)
      (with-unique-names (iterator winp next)
        `(block nil
           (with-package-iterator (,iterator ,@range)
             (tagbody
                  ,next
                  (multiple-value-bind (,winp ,var) (,iterator)
                    (declare (ignorable ,var))
                    ,@decls
                    (if ,winp
                        (tagbody ,@forms (go ,next))
                        (return ,result-form))))))))))

(defmacro do-symbols ((var &optional (package '*package*) result-form)
                           &body body-decls)
  "DO-SYMBOLS (VAR [PACKAGE [RESULT-FORM]]) {DECLARATION}* {TAG | FORM}*
   Executes the FORMs at least once for each symbol accessible in the given
   PACKAGE with VAR bound to the current symbol."
  (expand-pkg-iterator `((find-undeleted-package-or-lose ,package)
                       :internal :external :inherited)
                       var body-decls result-form))

(defmacro do-external-symbols ((var &optional (package '*package*) result-form)
                                    &body body-decls)
  "DO-EXTERNAL-SYMBOLS (VAR [PACKAGE [RESULT-FORM]]) {DECL}* {TAG | FORM}*
   Executes the FORMs once for each external symbol in the given PACKAGE with
   VAR bound to the current symbol."
  (expand-pkg-iterator `((find-undeleted-package-or-lose ,package) :external)
                       var body-decls result-form))

(defmacro do-all-symbols ((var &optional result-form) &body body-decls)
  "DO-ALL-SYMBOLS (VAR [RESULT-FORM]) {DECLARATION}* {TAG | FORM}*
   Executes the FORMs once for each symbol in every package with VAR bound
   to the current symbol."
  (expand-pkg-iterator '((list-all-packages) :internal :external)
                       var body-decls result-form))

;;; Utility iteration macros for detecting name conflicts
(defmacro do-symbol-name-groups (((name syms) symbols) &body body)
  (sb-int:with-unique-names (table sym next entryp k v)
    `(let ((,table (make-hash-table :test 'equal)))
       (dolist (,sym ,symbols)
         (pushnew ,sym (gethash (symbol-name ,sym) ,table)))
       (block nil
         (with-hash-table-iterator (,next ,table)
           (loop
             (multiple-value-bind (,entryp ,k ,v)
                 (,next)
               (unless ,entryp
                 (return))
               (let ((,name ,k)
                     (,syms (nreverse ,v)))
                 ,@body))))))))

(defmacro do-external-symbol-name-groups (((name syms) packages) &body body)
  (sb-int:with-unique-names (table pkg sym next entryp k v)
    `(let ((,table (make-hash-table :test 'equal)))
       (dolist (,pkg ,packages)
         (do-external-symbols (,sym ,pkg)
           (pushnew ,sym (gethash (symbol-name ,sym) ,table))))
       (block nil
         (with-hash-table-iterator (,next ,table)
           (loop
             (multiple-value-bind (,entryp ,k ,v)
                 (,next)
               (unless ,entryp
                 (return))
               (let ((,name ,k)
                     (,syms (nreverse ,v)))
                 ,@body))))))))

;;;; SYMBOL-TABLE stuff

(declaim (inline symtbl-cells))
(defun symtbl-cells (table) (truly-the simple-vector (cdr (symtbl-%cells table))))

(defun %symtbl-count (table)
  (the fixnum (- (symtbl-size table)
                 (the fixnum (+ (symtbl-deleted table)
                                (symtbl-free table))))))

(defmethod print-object ((table symbol-table) stream)
  (declare (type stream stream))
  (print-unreadable-object (table stream :type t :identity t)
    (let* ((n-live (%symtbl-count table))
           (n-deleted (symtbl-deleted table))
           (n-filled (+ n-live n-deleted))
           (n-cells (length (symtbl-cells table))))
      (format stream
              "(~D+~D)/~D [~@[~,3f words/sym,~]load=~,1f%]"
              n-live n-deleted n-cells
              (unless (zerop n-live) (/ n-cells n-live))
              (* 100 (/ n-filled n-cells))))))

(defun optimized-symtbl-remainder-params (denominator)
  ;; this does not work when DENOMINATOR is a power of 2
  (when (or (<= denominator 2) (= (logcount denominator) 1))
    (return-from optimized-symtbl-remainder-params (values 0 0)))
  ;; Get the number of bits neeeded to represent the denominator.
  ;; We want at least that many bits in the numerator, plus a couple.
  (let* ((min-numerator-bits (+ (integer-length denominator) 2))
         (n min-numerator-bits))
    (loop
     (multiple-value-bind (c frac-bits)
         (sb-c:compute-fastrem-coefficient denominator n :variable)
       (cond ((= frac-bits 32)
              (return (values (ldb (byte n 0) -1) c)))
             ((> frac-bits 32)
              (return (if (> n min-numerator-bits)
                          (values (ldb (byte (1- n) 0) -1)
                                  (sb-c:compute-fastrem-coefficient denominator (1- n) 32))
                          ;; Can't do fast algorithm, so set c=0
                          ;; which says to call REM. And use 32 bits
                          ;; in the dividend.
                          (values (ldb (byte 32 0) -1) 0))))
             (t
              (incf n)))))))

;;; Make a hashset having a prime number of entries at least
;;; as great as (/ SIZE +PACKAGE-REHASH-THRESHOLD+).
;;; The smallest table built here has three entries. This
;;; is necessary because the double hashing step size is calculated
;;; using a division by the table size minus two.
(defun make-symbol-table (size &optional (load-factor 3/4))
  (declare (sb-c::tlab :system)
           (inline make-symtbl-magic) ; to allow system-TLAB allocation
           (inline %make-symbol-table))
  (flet ((choose-good-size (size)
           (loop for n of-type fixnum
              from (logior (ceiling size load-factor) 1)
              by 2
              when (positive-primep n)
              return n)))
    ;; SIZE is how many symbols we'd like to be able to store,
    ;; but the number of physical cells is N, chosen for its primality.
    (binding* ((n (choose-good-size size))
               ((h1-mask h1-c) (optimized-symtbl-remainder-params n))
               ;;((h2-mask h2-c) (optimized-symtbl-remainder-params (- n 2)))
               (h2-mask (ldb (byte (1- (integer-length n)) 0) -1))
               (size (truncate (* n load-factor)))
               (reciprocals
                (if (= n 3) ; minimal table
                    (make-symtbl-magic 0 0 0) ; <-- should be LTV but can't be.
                                                ; (package-cold-init called before LTV fixups)
                    (make-symtbl-magic h1-mask h1-c h2-mask))))
      (%make-symbol-table (cons reciprocals (make-array n :initial-element 0)) size))))

(declaim (inline pkg-symbol-valid-p))
(defun pkg-symbol-valid-p (x) (not (fixnump x)))

;;; A symbol's name hash is the bitwise negation of the SXHASH
;;; of its print-name. I'm trying to be clearer with local variable naming
;;; as to whether a hash is the SXHASH of the symbol or its name.
;;; Additionally, on 64-bit architectures I want each symbol to have 2
;;; independent pieces to the hash:
;;; * 32 pseudo-random bits which will give better behavior for SXHASH.
;;;      Currently an EQUAL or EQUALP hash-table of same-named gensyms
;;;      will degenerate to a list.
;;; * 32 bits of name-based hash, essential for package operations
;;;      and also useful for compiling CASE expressions.
(defmacro symbol-table-hash (selector name-hash ncells
                             &aux (reciprocals 'reciprocals)) ; KLUDGE, unhygienic
  (declare (type (member 1 2) selector)) ; primary or secondary hash function
  (declare (ignorable reciprocals))
  (when (eql selector 2)
    (return-from symbol-table-hash
      `(truly-the index
        (1+ (logand (symtbl-hash2-mask (truly-the symtbl-magic reciprocals))
                    ,name-hash)))))
  (binding*
      (((get-mask get-c)
        (case selector
          (1 (values 'symtbl-hash1-mask 'symtbl-hash1-c))
          (2 (values 'symtbl-hash2-mask 'symtbl-hash2-c))))
       (remainder
         `(let* ((dividend
                  (logand (truly-the hash-code ,name-hash)
                          (,get-mask (truly-the symtbl-magic ,reciprocals))))
                 (divisor
                  (truly-the (and index (not (eql 0)))
                             ,(if (eq selector 2) `(- ,ncells 2) ncells)))
                 (c (,get-c (truly-the symtbl-magic ,reciprocals))))
            (if (= c 0)
                (truly-the index (rem dividend divisor))
                (sb-vm::fastrem-32 dividend c
                                   (truly-the (unsigned-byte 32) divisor))))))
    (if (eq selector 1)
        remainder
        `(truly-the index (1+ ,remainder)))))

;;; Destructively resize TABLE to have room for at least SIZE entries
;;; and rehash its existing entries.
;;; When rehashing, use the Robinhood insertion algorithm, though subsequent
;;; calls to ADD-SYMBOL make no attempt to preserve Robinhood's minimization
;;; of the maximum probe sequence length. We can do it now because the
;;; entire vector will be swapped, which is concurrent reader safe.
(defun resize-symbol-table (table size reason &optional (load-factor 3/4))
  (when (zerop size) ; Q: when do we resize to zero; everything gets uninterned?
    (return-from resize-symbol-table
      ;; Don't need a barrier here. Suppose a reader finished probing with a miss.
      ;; Whether it re-probes again or not, it will surely result in a miss.
      (let ((new (load-time-value (cons (make-symtbl-magic 0 0 0) #(0 0 0)) t)))
        (setf (symtbl-free table) 0
              (symtbl-deleted table) 0
              (symtbl-size table) 0
              (symtbl-%cells table) new))))
  (let* ((temp-table (make-symbol-table size load-factor))
         (cells (symtbl-%cells temp-table))
         (reciprocals (car cells))
         (vec (truly-the simple-vector (cdr cells)))
         (ncells (length vec))
         (psp-vector (make-array ncells :initial-element nil)))
    (labels ((ins (probe-sequence-pos symbol)
               ;; CAREFUL: probe-sequence-pos is 0-based, whereas the same thing
               ;; in 'hashset.lisp' is 1-based like in the reference algorithm.
               ;; It could be chalked up to the difference
               ;; between using LENGTH versus POSITION.
               (let* ((name-hash (symbol-name-hash symbol))
                      (h1 (symbol-table-hash 1 name-hash ncells))
                      (h2 (symbol-table-hash 2 name-hash ncells))
                      (index (rem (+ h1 (* probe-sequence-pos h2)) ncells))
                      (cell-psp (aref psp-vector index)))
                 (cond ((> probe-sequence-pos (or cell-psp -1))
                        (let ((occupant (aref vec index)))
                          (setf (aref psp-vector index) probe-sequence-pos
                                (aref vec index) symbol)
                          (unless (eql occupant 0)
                            #+nil
                            (format t "~&Symbol ~A @ psp ~D displaces ~A @ psp ~D~%"
                                    symbol probe-sequence-pos occupant cell-psp)
                            (ins (1+ cell-psp) occupant))))
                       (t
                        (ins (1+ probe-sequence-pos) symbol)))))
             (calculate-psp (symbol &aux (name-hash (symbol-name-hash symbol)) (pos 0))
               (let ((h (symbol-table-hash 1 name-hash ncells))
                     (h2 (symbol-table-hash 2 name-hash ncells)))
                 (loop (if (eq (svref vec h) symbol) (return (values pos h)))
                       (incf pos)
                       (setq h (rem (+ h h2) ncells)))))
             (verify-all-psps (&aux (max -1))
               (dovector (symbol vec max)
                 (when (symbolp symbol)
                   (binding* (((actual-psp index) (calculate-psp symbol))
                              (stored (aref psp-vector index)))
                     (setq max (max stored max))
                     (unless (= stored actual-psp)
                       (error "Messup @ ~A: ~D ~D~%" symbol actual-psp stored)))))))
      (let ((old (symtbl-cells table)))
        (dovector (sym old)
          (when (pkg-symbol-valid-p sym)
            (ins 0 sym)
            (decf (symtbl-free temp-table))))
        (let ((new (symtbl-%cells temp-table)))
          (setf (symtbl-%cells table) new)
          ;; Unlike above, we _do_ need this barrier. Readers must observe that the
          ;; change of %cells happens before the old contents are clobbered.
          (sb-thread:barrier (:write))
          (setf (symtbl-size table) (symtbl-size temp-table)
                (symtbl-free table) (symtbl-free temp-table)
                (symtbl-deleted table) 0)
          ;; Try to splat with 0 to reduce garbage tenuring based on a pre-test:
          ;; - If the current thread has bound *CLEAR-RESIZED-SYMBOL-TABLES* to NIL,
          ;;   meaning that it is within the dynamic extent of WITH-PACKAGE-ITERATOR,
          ;;   splat only if the package modification is NOT a permissible one.
          ;; - If the currrent thread has NOT bound the special variable, always splat.
          ;; Concurrent readers who searched the old VEC may miss spuriously, but will retry
          ;; because they can detect that %CELLS changed. In this manner, FIND-SYMBOL
          ;; remains lock-free while allowing resizing and splatting.
          (when (and (or *clear-resized-symbol-tables*
                         (not (memq reason '(unintern export unexport)))) ; permissible
                     (not (read-only-space-obj-p old)))
            (fill old 0))
          new)))))

;;;; package locking operations, built unconditionally now

(defun package-locked-p (package)
  "Returns T when PACKAGE is locked, NIL otherwise. Signals an error
if PACKAGE doesn't designate a valid package."
  (package-lock (find-undeleted-package-or-lose package)))

(defun lock-package (package)
  "Locks PACKAGE and returns T. Has no effect if PACKAGE was already
locked. Signals an error if PACKAGE is not a valid package designator"
  (flet ((unoptimize (table)
           (dovector (x (symtbl-cells table))
             ;; By some small miracle this was mostly correct for NIL
             ;; except that on ppc64le, (GET-HEADER-DATA NIL) returns #x52D00
             ;; which appears to have the +symbol-fast-bindable+ bit on.
             ;; What horrible fate befalls you for unsetting that.
             (when (and (typep x '(and symbol (not null)))
                        (test-header-data-bit x sb-vm::+symbol-fast-bindable+))
               (unset-symbol-progv-optimize x)))))
    (let ((p (find-undeleted-package-or-lose package)))
      (setf (package-lock p) t)
      (unoptimize (package-internal-symbols p))
      (unoptimize (package-external-symbols p))))
  t)

(defun unlock-package (package)
  "Unlocks PACKAGE and returns T. Has no effect if PACKAGE was already
unlocked. Signals an error if PACKAGE is not a valid package designator."
  (setf (package-lock (find-undeleted-package-or-lose package)) nil)
  t)

(defun package-implemented-by-list (package)
  "Returns a list containing the implementation packages of
PACKAGE. Signals an error if PACKAGE is not a valid package designator."
  (package-%implementation-packages (find-undeleted-package-or-lose package)))

(defun package-implements-list (package)
  "Returns the packages that PACKAGE is an implementation package
of. Signals an error if PACKAGE is not a valid package designator."
  (let ((package (find-undeleted-package-or-lose package)))
    (loop for x in (list-all-packages)
          when (member package (package-%implementation-packages x))
          collect x)))

(defun add-implementation-package (packages-to-add
                                   &optional (package *package*))
  "Adds PACKAGES-TO-ADD as implementation packages of PACKAGE. Signals
an error if PACKAGE or any of the PACKAGES-TO-ADD is not a valid
package designator."
  (let ((package (find-undeleted-package-or-lose package))
        (packages-to-add (package-listify packages-to-add)))
    (setf (package-%implementation-packages package)
          (union (package-%implementation-packages package)
                 (mapcar #'find-undeleted-package-or-lose packages-to-add)))))

(defun remove-implementation-package (packages-to-remove
                                      &optional (package *package*))
  "Removes PACKAGES-TO-REMOVE from the implementation packages of
PACKAGE. Signals an error if PACKAGE or any of the PACKAGES-TO-REMOVE
is not a valid package designator."
  (let ((package (find-undeleted-package-or-lose package))
        (packages-to-remove (package-listify packages-to-remove)))
    (setf (package-%implementation-packages package)
          (nset-difference
           (package-%implementation-packages package)
           (mapcar #'find-undeleted-package-or-lose packages-to-remove)))))

(defmacro with-unlocked-packages ((&rest packages) &body forms)
  "Unlocks PACKAGES for the dynamic scope of the body. Signals an
error if any of PACKAGES is not a valid package designator."
  (with-unique-names (unlocked-packages)
    `(let (,unlocked-packages)
      (unwind-protect
           (progn
             (dolist (p ',packages)
               (when (package-locked-p p)
                 (push p ,unlocked-packages)
                 (unlock-package p)))
             ,@forms)
        (dolist (p ,unlocked-packages)
          (when (find-package p)
            (lock-package p)))))))

(defun package-lock-violation (package &key (symbol nil symbol-p)
                               format-control format-arguments)
  (let* ((restart :continue)
         (cl-violation-p (eq package *cl-package*))
         (error-arguments
          (append (list (if symbol-p
                            'symbol-package-locked-error
                            'package-locked-error)
                        :package package
                        :format-control format-control
                        :format-arguments format-arguments)
                  (when symbol-p (list :symbol symbol))
                  (list :references
                        (append '((:sbcl :node "Package Locks"))
                                (when cl-violation-p
                                  '((:ansi-cl :section (11 1 2 1 2)))))))))
    (restart-case
        (apply #'cerror "Ignore the package lock." error-arguments)
      (:ignore-all ()
        :report "Ignore all package locks in the context of this operation."
        (setf restart :ignore-all))
      (:unlock-package ()
        :report "Unlock the package."
        (setf restart :unlock-package)))
    (ecase restart
      (:continue
       (pushnew package *ignored-package-locks*))
      (:ignore-all
       (setf *ignored-package-locks* t))
      (:unlock-package
       (unlock-package package)))))

(defun package-lock-violation-p (package &optional (symbol nil symbolp))
  (and package
       (package-lock package)
       ;; In package or implementation package
       (not (or (eq package *package*)
                (member *package* (package-%implementation-packages package))))
       ;; Runtime disabling
       (not (eq t *ignored-package-locks*))
       (or (eq :invalid *ignored-package-locks*)
           (not (member package *ignored-package-locks*)))
       ;; declarations for symbols
       (not (and symbolp (lexically-unlocked-symbol-p symbol)))))

(defun lexically-unlocked-symbol-p (symbol)
  (member symbol
          (if (boundp 'sb-c:*lexenv*)
              (let ((list (sb-c::lexenv-disabled-package-locks sb-c:*lexenv*)))
                ;; The so-called LIST might be an interpreter env.
                #+sb-fasteval
                (unless (listp list)
                  (return-from lexically-unlocked-symbol-p
                    (sb-interpreter::lexically-unlocked-symbol-p
                     symbol list)))
                list)
              sb-c::*disabled-package-locks*)))

;;;; more package-locking these are NOPs unless :sb-package-locks is
;;;; in target features. Cross-compiler NOPs for these are in cross-misc.

;;; The right way to establish a package lock context is
;;; WITH-SINGLE-PACKAGE-LOCKED-ERROR, defined in early-package.lisp
;;;
;;; Must be used inside the dynamic contour established by
;;; WITH-SINGLE-PACKAGE-LOCKED-ERROR
(defun assert-package-unlocked (package &optional format-control
                                &rest format-arguments)
  (when (package-lock-violation-p package)
    (package-lock-violation package
                            :format-control format-control
                            :format-arguments format-arguments))
  package)

;;; Must be used inside the dynamic contour established by
;;; WITH-SINGLE-PACKAGE-LOCKED-ERROR.
;;;
;;; FIXME: Maybe we should establish such contours for he toplevel
;;; and others, so that (setf fdefinition) and others could just use
;;; this.
(defun assert-symbol-home-package-unlocked (name &optional format-control
                                            &rest format-arguments)
  (let* ((symbol (etypecase name
                   (symbol name)
                   ;; Istm that the right way to declare that you want to allow
                   ;; overriding the lock on (SETF X) is to list (SETF X) in
                   ;; the declaration, not expect that X means itself and SETF.
                   ;; Worse still, the syntax ({ENABLE|DISABLE}-..-locks (SETF X))
                   ;; is broken, and yet we make no indication of it.
                   ((cons (eql setf) cons) (second name))
                   ;; Skip lists of length 1, single conses and
                   ;; (class-predicate foo), etc.  FIXME: MOP and
                   ;; package-lock interaction needs to be thought
                   ;; about.
                   (list
                    (return-from assert-symbol-home-package-unlocked
                      name))))
         (package (sb-xc:symbol-package symbol)))
    (when (package-lock-violation-p package symbol)
      (package-lock-violation package
                              :symbol symbol
                              :format-control format-control
                              :format-arguments (cons name format-arguments))))
  name)


;;;; miscellaneous PACKAGE operations

(defmethod print-object ((package package) stream)
  (let ((name (package-%name package)))
    (print-unreadable-object (package stream :type t :identity (not name))
      (if name (prin1 name stream) (write-string "(deleted)" stream)))))

;;; This is undocumented and unexported for now, but the idea is that by
;;; making this a generic function then packages with custom package classes
;;; could hook into this to provide their own resolution.
;;; (Any such generic solution will turn the performance to crap, so let's not)
(declaim (maybe-inline find-package-using-package))
(defun find-package-using-package (package-designator base)
  (let ((string (typecase package-designator
                  (package
                   (return-from find-package-using-package package-designator))
                  (symbol (symbol-name package-designator))
                  (string package-designator)
                  (character (string package-designator))
                  (t
                   (sb-c::%type-check-error package-designator '(or character package string symbol) nil)))))
    (or (and base
             (package-%local-nicknames base)
             (pkgnick-search-by-name string base))
        (%get-package string *all-packages*))))

(defun find-package (package-designator)
  "If PACKAGE-DESIGNATOR is a package, it is returned. Otherwise PACKAGE-DESIGNATOR
must be a string designator, in which case the package it names is located and returned.

As an SBCL extension, the current package may affect the way a package name is
resolved: if the current package has local nicknames specified, package names
matching those are resolved to the packages associated with them instead.

Example:

  (defpackage :a)
  (defpackage :example (:use :cl) (:local-nicknames (:x :a)))
  (let ((*package* (find-package :example)))
    (find-package :x)) => #<PACKAGE A>

See also: ADD-PACKAGE-LOCAL-NICKNAME, PACKAGE-LOCAL-NICKNAMES,
REMOVE-PACKAGE-LOCAL-NICKNAME, and the DEFPACKAGE option :LOCAL-NICKNAMES."
  (declare (explicit-check)
           (inline find-package-using-package))
  ;; We had a BOUNDP check on *PACKAGE* here, but it's effectless due to the
  ;; always-bound proclamation.
  (find-package-using-package package-designator *package*))

(defun package-%nicknames (package &aux (keys (package-keys package)))
  (loop for i from 2 below (length keys) by 2 collect (aref keys i)))

(defun package-%use-list (package)
  (map 'list #'symtbl-package (package-tables package)))

(defmacro do-packages ((package) &body body)
  ;; Even if iterating over the name -> package mapping were threadsafe
  ;; (which is isn't), there could nonetheless exist a race between iterators
  ;; and RENAME-PACKAGE. This is true whether first deleting the old name
  ;; and then inserting the new, or the other way around.
  ;; The examples below (only 2 out of many schedulings and choices of insert/delete
  ;; ordering) show that iteration can produce a package twice, or skip it entirely.
  ;;
  ;; Consider the table bins could have oldname in an earlier bin:
  ;;   bin 1 -> oldname, bin 2 -> newname
  ;; and suppose we first delete old, the insert new.
  ;;
  ;;  Thread1                Thread2
  ;;  ------------           -------------
  ;;  read bin 1 = oldname
  ;;                         delete oldname
  ;;                         insert newname
  ;;  read bin 2 = newname
  ;;
  ;; So it is produced twice even though it was not really in the table twice.
  ;; Or we can see it not at all. Suppose bin1 -> newname, bin 2 -> oldname
  ;;
  ;;  Thread1                Thread2
  ;;  ------------           -------------
  ;;  read bin 1 (initially empty)
  ;;                         delete oldname
  ;;                         insert newname
  ;;  read bin 2 (empty)
  ;;
  ;; The situation is further confounded by the fact that the primary name could
  ;; become a nickname or vice-versa (among all the myriad other things that
  ;; renaming can do) in which case there is ambiguity about the table cell
  ;; that is considered the representative one for the package.
  `(with-package-names ()
     (do-pkg-table ((,package) *all-packages*) ,@body)))

(defun package-%used-by-list (package &aux (wp (package-%used-by package)))
  (acond
     ((and wp (weak-pointer-value wp)) (if (eq it :none) nil it))
     (t
      ;; Ensure that the "uses" relation is fixed by acquiring the graph lock.
      ;; Additionally, DO-PACKAGES will acquire the name table lock.
      (with-package-graph ()
        (let ((me (package-external-symbols package))
              (list))
          (do-packages (user)
            (when (find me (package-tables user))
              (push user list)))
         (setf (package-%used-by package) (make-weak-pointer (or list :none)))
         list)))))

(macrolet ((def (ext real)
             `(defun ,ext (package-designator)
                (,real (%find-package-or-lose package-designator)))))
  (def package-name package-%name)
  ;; ANSI says (in the definition of DELETE-PACKAGE) that these, and
  ;; most other operations, are unspecified for deleted packages.
  ;; There's no harm in returning NIL from the slot accessors.
  (def package-nicknames package-%nicknames)
  (def package-use-list package-%use-list)
  (def package-used-by-list package-%used-by-list)
  (def package-shadowing-symbols package-%shadowing-symbols))

(defun package-local-nicknames (package-designator)
  "Returns an alist of \(local-nickname . actual-package) describing the
nicknames local to the designated package.

When in the designated package, calls to FIND-PACKAGE with the any of the
local-nicknames will return the corresponding actual-package instead. This
also affects all implied calls to FIND-PACKAGE, including those performed by
the reader.

When printing a package prefix for a symbol with a package local nickname, the
local nickname is used instead of the real name in order to preserve
print-read consistency.

See also: ADD-PACKAGE-LOCAL-NICKNAME, PACKAGE-LOCALLY-NICKNAMED-BY-LIST,
REMOVE-PACKAGE-LOCAL-NICKNAME, and the DEFPACKAGE option :LOCAL-NICKNAMES.

Experimental: interface subject to change."
  (package-local-nickname-alist
   (package-%local-nicknames (find-undeleted-package-or-lose package-designator))
   nil))

(defun signal-package-error (package format-control &rest format-args)
  (error 'simple-package-error
         :package package
         :format-control format-control
         :format-arguments format-args))

(defun signal-package-cerror (package continue-string
                              format-control &rest format-args)
  (cerror continue-string
          'simple-package-error
          :package package
          :format-control format-control
          :format-arguments format-args))

(defun package-locally-nicknamed-by-list (package-designator)
  "Returns a list of packages which have a local nickname for the designated
package.

See also: ADD-PACKAGE-LOCAL-NICKNAME, PACKAGE-LOCAL-NICKNAMES,
REMOVE-PACKAGE-LOCAL-NICKNAME, and the DEFPACKAGE option :LOCAL-NICKNAMES.

Experimental: interface subject to change."
  (let ((designee (find-undeleted-package-or-lose package-designator))
        (result))
    (do-packages (namer)
      ;; NAMER will be added once only to the result if there is more
      ;; than one nickname for designee.
      (let ((nicks (cdr (package-%local-nicknames namer))))
        (when (and nicks (weak-vector-find/eq designee nicks))
          (push namer result))))
    result))

(defun add-package-local-nickname (local-nickname actual-package
                                   &optional (package-designator (sane-package)))
  "Adds LOCAL-NICKNAME for ACTUAL-PACKAGE in the designated package, defaulting
to current package. LOCAL-NICKNAME must be a string designator, and
ACTUAL-PACKAGE must be a package designator.

Returns the designated package.

Signals a continuable error if LOCAL-NICKNAME is already a package local
nickname for a different package, or if LOCAL-NICKNAME is one of \"CL\",
\"COMMON-LISP\", or, \"KEYWORD\", or if LOCAL-NICKNAME is a global name or
nickname for the package to which the nickname would be added.

When in the designated package, calls to FIND-PACKAGE with the LOCAL-NICKNAME
will return the package the designated ACTUAL-PACKAGE instead. This also
affects all implied calls to FIND-PACKAGE, including those performed by the
reader.

When printing a package prefix for a symbol with a package local nickname,
local nickname is used instead of the real name in order to preserve
print-read consistency.

See also: PACKAGE-LOCAL-NICKNAMES, PACKAGE-LOCALLY-NICKNAMED-BY-LIST,
REMOVE-PACKAGE-LOCAL-NICKNAME, and the DEFPACKAGE option :LOCAL-NICKNAMES.

Experimental: interface subject to change."
  (let ((package (find-undeleted-package-or-lose package-designator)))
    (%add-package-local-nickname local-nickname actual-package package)
    (atomic-incf *package-names-cookie*)
    package))

;;; This is called repeatedly by defpackage's UPDATE-PACKAGE.
;;; We only need to bump the name cookie once. Synchronization of package lookups
;;; is pretty weak in the sense that concurrent FIND-PACKAGE and alteration
;;; of the name -> package association does not really promise much.
(defun %add-package-local-nickname (local-nickname actual-package package)
  (let ((nick (string local-nickname))
        (actual (find-package-using-package actual-package nil)))
    (unless actual
      (signal-package-error
       (package-name package)
       "The name ~S does not designate any package."
       actual-package))
    (unless (package-name actual)
      (signal-package-error
       actual
       "Cannot add ~A as local nickname for a deleted package: ~S"
       nick actual))
    (with-single-package-locked-error
        (:package package "adding ~A as a local nickname for ~A"
                  nick actual))
    (when (member nick '("CL" "COMMON-LISP" "KEYWORD") :test #'string=)
      (signal-package-cerror
       actual
       "Continue, use it as local nickname anyways."
       "Attempt to use ~A as a package local nickname (for ~A)."
       nick (package-name actual)))
    (when (string= nick (package-name package))
      (signal-package-cerror
       package
       "Continue, use it as a local nickname anyways."
       "Attempt to use ~A as a package local nickname (for ~A) in ~
        package named globally ~A."
       nick (package-name actual) nick))
    (when (member nick (package-nicknames package) :test #'string=)
      (signal-package-cerror
       package
       "Continue, use it as a local nickname anyways."
       "Attempt to use ~A as a package local nickname (for ~A) in ~
        package nicknamed globally ~A."
       nick (package-name actual) nick))
    (let ((old-actual (pkgnick-search-by-name nick package)))
      (when (and old-actual (neq actual old-actual))
         (restart-case
             (signal-package-error
              actual
              "~@<Cannot add ~A as local nickname for ~A in ~A: ~
               already nickname for ~A.~:@>"
              nick (package-name actual) (package-name package) (package-name old-actual))
          (keep-old ()
           :report (lambda (s)
                     (format s "Keep ~A as local nickname for ~A."
                             nick (package-name old-actual))
                     (return-from %add-package-local-nickname package)))
          (change-nick ()
            :report (lambda (s)
                      (format s "Use ~A as local nickname for ~A instead."
                              nick (package-name actual)))))))
    (pkgnick-update package nick actual)))

(defun remove-package-local-nickname (old-nickname
                                      &optional (package-designator (sane-package)))
  "If the designated package had OLD-NICKNAME as a local nickname for
another package, it is removed. Returns true if the nickname existed and was
removed, and NIL otherwise.

See also: ADD-PACKAGE-LOCAL-NICKNAME, PACKAGE-LOCAL-NICKNAMES,
PACKAGE-LOCALLY-NICKNAMED-BY-LIST, and the DEFPACKAGE option :LOCAL-NICKNAMES.

Experimental: interface subject to change."
  (let* ((nick (string old-nickname))
         (package (find-undeleted-package-or-lose package-designator))
         (existing (pkgnick-search-by-name nick package)))
    (when existing
      (with-single-package-locked-error
          (:package package "removing local nickname ~A for ~A" nick existing))
      (pkgnick-update package nick nil)
      (atomic-incf *package-names-cookie*)
      t)))

(defun package-internal-symbol-count (package)
  (%symtbl-count (package-internal-symbols package)))

(defun package-external-symbol-count (package)
  (%symtbl-count (package-external-symbols package)))

(defvar *package* (error "*PACKAGE* should be initialized in cold load!")
  "the current package")

(define-condition bootstrap-package-not-found (condition)
  ((name :initarg :name :reader bootstrap-package-name)))
(defun debootstrap-package (&optional condition)
  (declare (ignore condition))
  (bug "No such thing as DEBOOTSTRAP-PACKAGE"))

;;; Return a list of packages given a package designator or list of
;;; package designators, or die trying.
(defun package-listify (thing)
  (if (listp thing)
      (mapcar #'find-undeleted-package-or-lose thing)
      (list (find-undeleted-package-or-lose thing))))

;;;; operations on symbol hashsets

;;; Add a symbol to a hashset. The symbol MUST NOT be present.
;;; This operation is under the WITH-PACKAGE-GRAPH lock if called by %INTERN.
(defun add-symbol (table symbol reason &aux (cells (symtbl-%cells table)))
  (setf (symtbl-modified table) t)
  (when (or (zerop (symtbl-free table)) (/= 0 (symtbl-immutable (car cells))))
    ;; The hashtable is full. Resize it to be able to hold twice the
    ;; amount of symbols than it currently contains. The actual new size
    ;; can be smaller than twice the current size if the table contained
    ;; deleted entries.
    ;; N.B.: Never pass 0 for the new size, as that will assign the
    ;; constant read-only vector #(0 0 0) into the cells.
    (let ((new-size (max 1 (* (- (symtbl-size table) (symtbl-deleted table)) 2))))
      (setq cells (resize-symbol-table table new-size reason))))
  (let* ((reciprocals (car cells))
         (vec (truly-the simple-vector (cdr cells)))
         (len (length vec))
         (name-hash (symbol-name-hash symbol))
         (h1 (symbol-table-hash 1 name-hash len))
         (h2 (symbol-table-hash 2 name-hash len)))
    (declare (fixnum name-hash))
    ;; This REM could easily be changed to a test for wraparound and possible subtraction.
    ;; But ADD-SYMBOL isn't as performance critical as FIND-SYMBOL.
    (do ((i h1 (rem (+ i h2) len)))
        ((fixnump (svref vec i)) ; encountered an unoccupied cell
         (let ((old (svref vec i)))
           (setf (svref vec i) symbol)
           (if (eql old 0)
               (decf (symtbl-free table)) ; unused
               (decf (symtbl-deleted table))))) ; tombstone
      (declare (fixnum i)))))

;;; For each name (a string) in NAMELIST, insert a mapping to PACKAGE.
;;; The first element is the primary name and the rest are nicknames.
(defun package-registry-update (package namelist)
  (declare (type (or list (eql t)) namelist))
  (aver (sb-thread:holding-mutex-p *package-table-lock*))
  (when (and namelist (not (package-id package)))
    ;; Registration ensures a non-null id if it can (even for a "deferred" package)
    (let* ((vector *id->package*)
           ;; 30 is an arbitrary constant exceeding the number of wired IDs
           (new-id (position nil vector :start 30)))
      (when (and (null new-id) (< (length vector) +package-id-overflow+))
        (let* ((current-length (length vector))
               (new-length (min (+ current-length 10) +package-id-overflow+))
               (new-vector (make-array new-length :initial-element nil)))
          (replace new-vector vector)
          (setf *id->package* new-vector)
          (setf new-id current-length
                vector new-vector)))
      (when new-id
        (setf (package-id package) new-id
              (aref vector new-id) package))))
  (when (eq namelist 't)
    (return-from package-registry-update))
  (let* ((n (length namelist))
         (new-hashes (mapcar #'sxhash namelist))
         (new-keys (make-array (* 2 n)))
         (old-hashes (let ((old-keys (package-keys package)))
                       (loop for i from 1 below (length old-keys) by 2
                             collect (svref old-keys i)))))
    (loop for i from 0 by 2
          for name in namelist
          for hash in new-hashes
          do (setf (aref new-keys i) name)
             (setf (aref new-keys (1+ i)) hash))
    (when namelist
      (let* ((old-table *all-packages*)
             (new-table (pkgtable-insert old-table package new-keys)))
        (unless (eq old-table new-table)
          (setq *all-packages* new-table))
        ;; Do these assignments only when NAMELIST is non-nil - i.e. if not deleting
        ;; the package - so that we don't see a nameless package in the table.
        (setf (package-keys package) new-keys
              (package-%name package) (car namelist))
        ;; This uses the same algorithm for zapping the old table when growing
        ;; as with SYMTBL-%CELLS when growing those.
        (unless (eq old-table new-table)
          (sb-thread:barrier (:write))
          (fill old-table 0))))
    ;; Remove from buckets that it does not belong in based on the set difference
    ;; of old vs new BUCKETS and not difference of names. Never downsize the table.
    (let ((table *all-packages*))
      (flet ((buckets (hashes)
               (delete-duplicates
                (mapcar (lambda (hash) (pkgtable-bucket-index table hash))
                        hashes))))
        (dolist (index (set-difference (buckets old-hashes) (buckets new-hashes)))
          (let* ((old (ensure-list (svref table index)))
                 (new))
            (aver (memq package old))
            (setq new (delq1 package old))
            (setf (svref table index) (if (singleton-p new) (car new) new))
            (decf (pkgtable-count table))))))))

;;; Find where the symbol named STRING is stored in TABLE.
;;; INDEX-VAR is bound to the vector index if found, or -1 if not.
;;; LENGTH bounds the string, and NAME-HASH should be precomputed by
;;; calling CALC-SYMBOL-NAME-HASH and chopping to 32 bits
;;; If the symbol is found, then FORMS are executed; otherwise not.
(defmacro with-symbol (((symbol-var &optional index-var) table
                        string length name-hash) &body forms)
  (let ((lookup `(%lookup-symbol ,table ,string ,length
                                 (the symbol-name-hash ,name-hash))))
    `(,@(if index-var
            `(multiple-value-bind (,symbol-var ,index-var) ,lookup)
            `(let ((,symbol-var ,lookup))))
      (unless (eql ,symbol-var 0) ,@forms))))
#|
(declaim (fixnum *sym-lookups* *sym-hit-1st-try*))
(define-load-time-global *sym-lookups* 0)
(define-load-time-global *sym-hit-1st-try* 0)
|#

(defun %lookup-symbol (table string name-length name-hash)
  (declare (optimize (sb-c::verify-arg-count 0)
                     (sb-c::insert-array-bounds-checks 0)))
  (declare (symbol-table table) (simple-string string) (index name-length))
  #+nil (atomic-incf *sym-lookups*)
  (macrolet
      ((probe (metric)
         (declare (ignorable metric))
         `(let ((item (svref vec index)))
            (cond ((not (fixnump item))
                   (let ((symbol (truly-the symbol item)))
                     (when (eq (symbol-name-hash symbol) name-hash)
                       (let ((name (symbol-name symbol)))
                         ;; The pre-test for length is kind of an unimportant
                         ;; optimization, but passing it for both :end arguments
                         ;; requires that it be within bounds for the probed symbol.
                         (when (and (= (length name) name-length)
                                    (string= string name
                                             :end1 name-length :end2 name-length))
                           (return-from %lookup-symbol (values symbol index)))))))
                  ;; either a never used cell or a tombstone left by UNINTERN
                  ((eql item 0) ; never used
                   ;; Prevent CPU from speculating the re-load of %CELLS
                   (sb-thread:barrier (:read))
                   (return-from try
                     (let ((new (symtbl-%cells table)))
                       (if (eq new cells) ; had a consistent view of CELLS
                           (values 0 -1)
                           (try new)))))))))
      (named-let try ((cells (symtbl-%cells table)))
        (let* ((magic (car cells))
               (vec (truly-the simple-vector (cdr cells)))
               (len (length vec)))
          (when (functionp magic)
            (let ((perfect-hash (funcall magic name-hash)))
              (when (< perfect-hash len)
                (let ((symbol (truly-the symbol (svref vec perfect-hash))))
                  (when (and (eql (symbol-name-hash symbol) name-hash)
                             (string= (symbol-name symbol) string :end2 name-length))
                    (return-from try (values symbol perfect-hash))))))
            (return-from try (values 0 -1)))
          (let* ((reciprocals magic)
                 (index (symbol-table-hash 1 name-hash len)))
            (declare (index index))
            (probe (atomic-incf *sym-hit-1st-try*))
          ;; Compute a secondary hash H2, and add it successively to INDEX,
          ;; treating the vector as a ring. This loop is guaranteed to terminate
          ;; because there has to be at least one cell with a 0 in it.
          ;; Whenever we change a cell containing 0 to a symbol, the FREE count
          ;; is decremented. And FREE starts as a smaller number than the vector length.
            (let ((h2 (symbol-table-hash 2 name-hash len)))
              (declare (index h2))
              (loop (when (>= (incf (truly-the index index) h2) len)
                      (decf (truly-the index index) len))
                    (probe nil))))))))

;;; Almost like %lookup-symbol but compare by EQ, not by name.
(defun symbol-externalp (symbol package)
  (declare (symbol symbol) (package package))
  (declare (optimize (sb-c::insert-array-bounds-checks 0)))
  (macrolet ((probe ()
               `(let ((item (svref vec index)))
                  (when (eq item symbol) (return-from symbol-externalp t))
                  (when (eql item 0)
                    ;; Prevent CPU from speculating the re-load of %CELLS
                    (sb-thread:barrier (:read))
                    (return-from try
                      (let ((new (symtbl-%cells table)))
                        (if (eq new cells) nil (try new))))))))
    (let ((table (package-external-symbols package))
          (name-hash (symbol-name-hash symbol)))
      (named-let try ((cells (symtbl-%cells table)))
        (let* ((magic (car cells))
               (vec (truly-the simple-vector (cdr cells)))
               (len (length vec)))
          (when (functionp magic)
            (let ((perfect-hash (funcall magic name-hash)))
              (return-from try
                (and (< perfect-hash len) (eq (svref vec perfect-hash) symbol)))))
          (let* ((reciprocals magic)
                 (index (symbol-table-hash 1 name-hash len)))
            (declare (index index))
            (probe)
            (let ((h2 (symbol-table-hash 2 name-hash len)))
              (loop (when (>= (incf index h2) len) (decf index len))
                    (probe)))))))))

;;; Delete SYMBOL from TABLE, storing -1 in its place. SYMBOL must exist.
(defun nuke-symbol (table symbol reason)
  (let ((cells (symtbl-%cells table)))
    (when (functionp (car cells)) ; change table back to not-perfectly-hashed
      (resize-symbol-table table (length (cdr cells)) reason)))
  (let* ((string (symbol-name symbol))
         (length (length string))
         (hash (symbol-name-hash symbol)))
    (declare (type index length)
             (hash-code hash))
    (with-symbol ((symbol index) table string length hash)
      ;; It is suboptimal to grab the vectors again, but not broken,
      ;; because we have exclusive use of the table for writing.
      (let ((symvec (symtbl-cells table)))
        (setf (aref symvec index) -1))
      (incf (symtbl-deleted table))))
  ;; If the table is less than one quarter full, halve its size and
  ;; rehash the entries.
  (let ((size (symtbl-size table))
        (used (%symtbl-count table)))
    (when (< used (truncate size 4))
      (resize-symbol-table table (* used 2) reason))))

(defun list-all-packages ()
  "Return a list of all existing packages."
  (let ((result ()))
    (do-packages (package) (push package result))
    result))

;;; Check internal and external symbols, then scan down the list
;;; of hashtables for inherited symbols.
(defun %find-symbol (string length package)
  (declare (simple-string string)
           (type index length))
  (let ((hash (calc-symbol-name-hash string length)))
    (declare (hash-code hash))
    (with-symbol ((symbol) (package-internal-symbols package) string length hash)
      (return-from %find-symbol (values symbol :internal)))
    (with-symbol ((symbol) (package-external-symbols package) string length hash)
      (return-from %find-symbol (values symbol :external)))
    (let* ((tables (package-tables package))
           (n (length tables)))
      (unless (eql n 0)
        ;; Try the most-recently-used table, then others.
        ;; TABLES is treated as circular for this purpose.
        (let* ((mru (package-mru-table-index package))
               (start (if (< mru n) mru 0))
               (i start))
          (loop
           (with-symbol ((symbol) (locally (declare (optimize (safety 0)))
                                    (svref tables i))
                         string length hash)
             (setf (package-mru-table-index package) i)
             (return-from %find-symbol (values symbol :inherited)))
           (if (< (decf i) 0) (setq i (1- n)))
           (if (= i start) (return)))))))
  (values nil nil))

;;; When INTERN needs to make a new symbol, it always copies the argument string.
;;; This suits the use-case for the reader, which reuses the same input buffer over and over.
;;; It is theoretically possible to enforce immutability of symbol names,
;;; but for now it only happens in saved cores. The way to do it for new symbols is to
;;; keep a dually mapped memory range - one writable range for the allocator, and
;;; the readonly range for everything else. memfd_create() on Linux supports such usage.
;;; This is a stub for that eventual capability.
(defun make-readonly-string (length name elt-type)
  (declare (sb-c::tlab :system))
  (logically-readonlyize
   (replace (ecase elt-type
              (base-char
               (make-string (truly-the index length) :element-type 'base-char))
              (character
               (make-string (truly-the index length) :element-type 'character)))
            name)))

;;; If the symbol named by the first LENGTH characters of NAME doesn't exist,
;;; then create it, special-casing the keyword package.
;;; If a new symbol is created, its print name will be an array of ELT-TYPE.
;;; The fasloader always supplies NAME as a (SIMPLE-ARRAY <ELT-TYPE> 1),
;;; but the reader uses a buffer of CHARACTER, which, based on a flag,
;;; can be demoted to an array of BASE-CHAR.
(defun %intern (name length package elt-type ignore-lock &optional (allow-inherited t))
  ;; No type declarations, %find-symbol will perform the checks
  (multiple-value-bind (symbol where) (%find-symbol name length package)
    (if (and where (or allow-inherited (neq where :inherited)))
        (values symbol where)
        ;; Double-checked lock pattern: the common case has the symbol already interned,
        ;; but in case another thread is interning in parallel we need to check after
        ;; grabbing the lock.
        (with-package-graph ()
          (setf (values symbol where) (%find-symbol name length package))
          (if (and where (or allow-inherited (neq where :inherited)))
              (values symbol where)
              (let* ((symbol-name (make-readonly-string length name elt-type))
                     ;; optimistically create the symbol
                     (symbol ; Symbol kind: 1=keyword, 2=other interned
                       (%make-symbol (if (eq package *keyword-package*) 1 2) symbol-name))
                     (table (cond ((eq package *keyword-package*)
                                   (%set-symbol-value symbol symbol)
                                   (package-external-symbols package))
                                  (t
                                   (package-internal-symbols package)))))
                ;; Set the symbol's package before storing it into the package
                ;; so that (symbol-package (intern x #<pkg>)) = #<pkg>.
                ;; This matters in the case of concurrent INTERN.
                (%set-symbol-package symbol package)
                (if ignore-lock
                    (add-symbol table symbol 'intern)
                    (with-single-package-locked-error
                        (:package package "interning ~A" symbol-name)
                      (add-symbol table symbol 'intern)))
                (values symbol nil)))))))

(macrolet ((find/intern (function package-lookup &rest more-args)
             ;; Both %FIND-SYMBOL and %INTERN require a SIMPLE-STRING,
             ;; but accept a LENGTH. Given a non-simple string,
             ;; we need copy it only if the cumulative displacement
             ;; into the underlying simple-string is nonzero.
             ;; There are two things that can be improved
             ;; about the generated code here:
             ;; 1. if X is known to satisfy STRINGP (generally any rank-1 array),
             ;;    then testing SIMPLE-<base|character>-STRING-P should not
             ;;    re-test the lowtag. This is constrained by the backends,
             ;;    because there are no type vops that assume a known lowtag.
             ;; 2. if X is known to satisfy VECTORP, then
             ;;    (NOT (ARRAY-HEADER-P)) implies SIMPLE-P, but the compiler
             ;;    does not actually know that, and generates a check.
             ;;    This is more of a front-end issue.
             `(multiple-value-bind (name length)
                  (if (simple-string-p name)
                      (values name (length name))
                      (with-array-data ((name name) (start) (end)
                                        :check-fill-pointer t)
                        (if (eql start 0)
                            (values name end)
                            (values (subseq name start end)
                                    (- end start)))))
                (,function name length ,package-lookup ,@more-args))))

  (defun intern (name &optional (package (sane-package)))
  "Return a symbol in PACKAGE having the specified NAME, creating it
  if necessary."
    (find/intern %intern
                 ;; Avoid one function call if we have a known good package.
                 (if (and (packagep package) (package-%name package))
                     package
                     (find-undeleted-package-or-lose package))
                 (if (base-string-p name) 'base-char 'character)
                 nil))

  (defun find-symbol (name &optional (package (sane-package)))
  "Return the symbol named STRING in PACKAGE. If such a symbol is found
  then the second value is :INTERNAL, :EXTERNAL or :INHERITED to indicate
  how the symbol is accessible. If no symbol is found then both values
  are NIL."
    (find/intern %find-symbol
                 ;; Avoid one function call if we have a known good package.
                 (if (and (packagep package) (package-%name package))
                     package
                     (find-undeleted-package-or-lose package))))

  (defun intern2 (name cell)
    ;; INTERN2 will only receive a package object if it's one of the standard
    ;; ones. Hence no check for PACKAGE-%NAME validity (i.e. non-deleted)
    (find/intern %intern
                 (if (packagep cell) cell (cached-find-undeleted-package cell))
                 (if (base-string-p name) 'base-char 'character)
                 nil))

  (defun find-symbol2 (name cell)
    (find/intern %find-symbol ; likewise
                 (if (packagep cell) cell (cached-find-undeleted-package cell)))))

;;; Similar to FIND-SYMBOL, but only looks for an external symbol.
;;; Return the symbol if found, otherwise 0.
;;; This is used for fast name-conflict checking in this file.
(defun find-external-symbol (string package)
  (declare (simple-string string))
  (let* ((length (length string))
         (hash (calc-symbol-name-hash string length)))
    (declare (type index length) (hash-code hash))
    (with-symbol ((symbol) (package-external-symbols package) string length hash)
      (return-from find-external-symbol symbol)))
  0)

(define-condition name-conflict (reference-condition package-error)
  ((function :initarg :function :reader name-conflict-function)
   (datum :initarg :datum :reader name-conflict-datum)
   (symbols :initarg :symbols :reader name-conflict-symbols))
  (:default-initargs :references '((:ansi-cl :section (11 1 1 2 5))))
  (:report
   (lambda (c s)
     (format s "~@<~S ~S causes name-conflicts in ~S between the ~
                following symbols: ~2I~@:_~
                ~{~/sb-ext:print-symbol-with-prefix/~^, ~}~:@>"
             (name-conflict-function c)
             (name-conflict-datum c)
             (package-error-package c)
             (name-conflict-symbols c)))))

(defun name-conflict (package function datum &rest symbols)
  (flet ((import1p (c)
           (declare (ignore c))
           (and (eq 'import function)
                ;; exactly two symbols are involved in the conflict
                (null (cddr symbols)) (null (cdr datum))
                ;; one of the symbols is already accessible in the package
                (nth-value 1 (find-symbol (symbol-name (car datum)) package))))
         (use1-or-export-p (c)
           (declare (ignore c))
           (or (and (eq 'use-package function)
                    (null (cddr symbols)) (null (cdr datum))
                    (nth-value 1 (find-symbol (symbol-name (car datum)) package)))
               (eq 'export function)))
         (old-symbol ()
           (ecase function
             ((export) (car (remove datum symbols)))
             ((use-package import) (find-symbol (symbol-name (car datum)) package)))))
    (let ((pname (package-name package)))
      (restart-case
          (error 'name-conflict :package package :symbols symbols
                                :function function :datum datum)
        ;; USE-PACKAGE with a pair of symbols conflicting, and EXPORT
        (keep-old ()
          :report (lambda (s)
                    (ecase function
                      (export
                       (format s "Keep ~S accessible in ~A (shadowing ~S)."
                               (old-symbol) pname datum))
                      (use-package
                       (format s "Keep ~S accessible in ~A (shadowing ~S)."
                               (old-symbol) pname (car datum)))))
          :test use1-or-export-p
          (dolist (s (remove-duplicates symbols :test #'string=))
            (shadow (symbol-name s) package)))
        (take-new ()
          :report (lambda (s)
                    (ecase function
                      (export
                       (format s "Make ~S accessible in ~A (uninterning ~S)."
                               datum pname (old-symbol)))
                      (use-package
                       (format s "Make ~S accessible in ~A (uninterning ~S)."
                               (car datum) pname (old-symbol)))))
          :test use1-or-export-p
          (dolist (s symbols)
            (when (eq s (find-symbol (symbol-name s) package))
              (unintern s package))))
        ;; IMPORT with a pair of symbols conflicting.
        (shadowing-import-it ()
          :report (lambda (s)
                    (format s "Shadowing-import ~S, uninterning ~S."
                            (car datum) (old-symbol)))
          :test import1p
          (shadowing-import datum package))
        (dont-import-it ()
          :report (lambda (s)
                    (format s "Don't import ~S, keeping ~S."
                            (car datum) (old-symbol)))
          :test import1p)
        ;; General case. This is exposed via SB-EXT.
        (resolve-conflict (chosen-symbol)
          :report "Resolve conflict."
          :interactive
          (lambda ()
            (let* ((len (length symbols))
                   (nlen (length (write-to-string len :base 10)))
                   (*print-pretty* t))
              (format *query-io* "~&~@<Select a symbol to be made accessible in ~
                              package ~A:~2I~@:_~{~{~V,' D. ~
                              ~/sb-ext:print-symbol-with-prefix/~}~@:_~}~
                              ~@:>"
                      (package-name package)
                      (loop for s in symbols
                            for i upfrom 1
                            collect (list nlen i s)))
              (loop
                (format *query-io* "~&Enter an integer (between 1 and ~D): " len)
                (finish-output *query-io*)
                (let ((i (parse-integer (read-line *query-io*) :junk-allowed t)))
                  (when (and i (<= 1 i len))
                    (return (list (nth (1- i) symbols))))))))
          (multiple-value-bind (package-symbol status)
              (find-symbol (symbol-name chosen-symbol) package)
            (let* ((accessiblep status)
                   (presentp (and accessiblep
                                  (not (eq :inherited status)))))
              (ecase function
                ((unintern)
                 (if presentp
                     (if (eq package-symbol chosen-symbol)
                         (shadow (list package-symbol) package)
                         (shadowing-import (list chosen-symbol) package))
                     (shadowing-import (list chosen-symbol) package)))
                ((use-package export)
                 (if presentp
                     (if (eq package-symbol chosen-symbol)
                         (shadow (list package-symbol) package) ; CLHS 11.1.1.2.5
                         (if (eq (sb-xc:symbol-package package-symbol) package)
                             (unintern package-symbol package) ; CLHS 11.1.1.2.5
                             (shadowing-import (list chosen-symbol) package)))
                     (shadowing-import (list chosen-symbol) package)))
                ((import)
                 (if presentp
                     (if (eq package-symbol chosen-symbol)
                         nil                ; re-importing the same symbol
                         (shadowing-import (list chosen-symbol) package))
                     (import (list chosen-symbol) package)))))))))))

;;; If we are uninterning a shadowing symbol, then a name conflict can
;;; result, otherwise just nuke the symbol.
(defun unintern (symbol &optional (package (sane-package)))
  "Makes SYMBOL no longer present in PACKAGE. If SYMBOL was present then T is
returned, otherwise NIL. If PACKAGE is SYMBOL's home package, then it is made
uninterned."
  (with-package-graph ()
    (let* ((package (find-undeleted-package-or-lose package))
           (name (symbol-name symbol))
           (shadowing-symbols (package-%shadowing-symbols package)))
      (with-single-package-locked-error ()
        ;; this seems a little wrong - if NAME is inherited and not _present_
        ;; you're not actually doing anything to the package.
        ;; So why is that potentially a violation of a package lock?
        (when (nth-value 1 (find-symbol name package))
          (assert-package-unlocked package "uninterning ~A" name))

        ;; If a name conflict is revealed, give us a chance to
        ;; shadowing-import one of the accessible symbols.
        (when (member symbol shadowing-symbols)
          (let ((cset ())) ; conflict set
            (dovector (tbl (package-tables package))
              (let ((s (find-external-symbol name (symtbl-package tbl))))
                (unless (eql s 0) (pushnew s cset :test #'eq))))
            (when (cdr cset)
              (apply #'name-conflict package 'unintern symbol cset)
              (return-from unintern t)))
          (setf (package-%shadowing-symbols package)
                (remove symbol shadowing-symbols))) ; would not DELETE be ok?

        (multiple-value-bind (s w) (find-symbol name package)
          (cond ((not (eq symbol s)) nil)
                ((or (eq w :internal) (eq w :external))
                 (nuke-symbol (if (eq w :internal)
                                  (package-internal-symbols package)
                                  (package-external-symbols package))
                              symbol
                              'unintern)
                 (if (eq (sb-xc:symbol-package symbol) package)
                     (%set-symbol-package symbol nil))
                 t)
                (t nil)))))))

;;; Take a symbol-or-list-of-symbols and return a list, checking types.
(defun symbol-listify (thing)
  (cond ((listp thing)
         (dolist (s thing)
           (unless (symbolp s)
             (signal-package-error nil
                                   "~S is not a symbol." s)))
         thing)
        ((symbolp thing) (list thing))
        (t
         (signal-package-error nil
                               "~S is neither a symbol nor a list of symbols."
                               thing))))

(defun string-listify (thing)
  (mapcar #'string (ensure-list thing)))

(defun export (symbols &optional (package (sane-package)))
  "Exports SYMBOLS from PACKAGE, checking that no name conflicts result."
  (with-package-graph ()
    (let ((package (find-undeleted-package-or-lose package))
          (symbols (symbol-listify symbols))
          (syms ()))
      ;; Punt any symbols that are already external.
      (dolist (sym symbols)
        (let ((s (find-external-symbol (symbol-name sym) package)))
          (unless (eq s sym)
            (pushnew sym syms :test #'eq))))
      (with-single-package-locked-error ()
        (when syms
          (assert-package-unlocked package "exporting symbol~P ~{~A~^, ~}"
                                   (length syms) syms))
        ;; Find symbols and packages with conflicts.
        (let ((used-by (package-%used-by-list package)))
          (dolist (sym syms)
            (let ((name (symbol-name sym)))
              (dolist (p used-by)
                (multiple-value-bind (s w) (find-symbol name p)
                  (when (and w
                             (not (eq s sym))
                             (not (member s (package-%shadowing-symbols p))))
                    ;; Beware: the name conflict is in package P, not in
                    ;; PACKAGE.
                    (name-conflict p 'export sym sym s)))))))
        ;; Check that all symbols are accessible. If not, ask to import them.
        (let ((missing ())
              (imports ()))
          (dolist (sym syms)
            (multiple-value-bind (s w) (find-symbol (symbol-name sym) package)
              (cond ((not (and w (eq s sym)))
                     (push sym missing))
                    ((eq w :inherited)
                     (push sym imports)))))
          (when missing
            (signal-package-cerror
             package
             (format nil "~S these symbols into the ~A package."
                     'import (package-%name package))
             "~@<These symbols are not accessible in the ~A package:~2I~_~S~@:>"
             (package-%name package) missing)
            (import missing package))
          (import imports package))

        ;; And now, three pages later, we export the suckers.
        (let ((internal (package-internal-symbols package))
              (external (package-external-symbols package)))
          (dolist (sym syms)
            ;; no error if this condition is false: inaccessibility
            ;; and subsequent conflicts handled above by IMPORT can
            ;; mean that some symbols in the original export list are
            ;; not exportable.
            (when (eql (find-symbol (symbol-name sym) package) sym)
              (add-symbol external sym 'export)
              (nuke-symbol internal sym 'export)))))
      t)))

;;; Check that all symbols are accessible, then move from external to internal.
(defun unexport (symbols &optional (package (sane-package)))
  "Makes SYMBOLS no longer exported from PACKAGE."
  (with-package-graph ()
    (let ((package (find-undeleted-package-or-lose package))
          (symbols (symbol-listify symbols))
          (syms ()))
      (dolist (sym symbols)
        (multiple-value-bind (s w) (find-symbol (symbol-name sym) package)
          (cond ((or (not w) (not (eq s sym)))
                 (signal-package-error
                  package
                  "~S is not accessible in the ~A package."
                  sym (package-%name package)))
                ((eq w :external) (pushnew sym syms)))))
      (with-single-package-locked-error ()
        (when syms
          (assert-package-unlocked package "unexporting symbol~P ~{~A~^, ~}"
                                   (length syms) syms))
        (let ((internal (package-internal-symbols package))
              (external (package-external-symbols package)))
          (dolist (sym syms)
            (add-symbol internal sym 'unexport)
            (nuke-symbol external sym 'unexport))))
      t)))

;;; Check for name conflict caused by the import and let the user
;;; shadowing-import if there is.
;;;
;;; FIXME: we _might_ be wrong about importing into the KEYWORD package.
;;; """
;;; 11.1.2.3.1: Interning a Symbol in the KEYWORD Package
;;; The KEYWORD package is treated differently than other packages in that special actions are taken
;;; when a symbol is interned in it. In particular, when a symbol is interned in the KEYWORD package,
;;; it is automatically made to be an external symbol and is automatically made to be a constant
;;; variable with itself as a value.
;;; """
;;; It could be claimed that this use of the term "Interning" is specifically in
;;; reference to the function INTERN, and not the general concept of interning.
;;; Obviously we do make new keyword symbols external and constant.
;;; But if it means interning in the sense of making a symbol accessible at all
;;; in the keyword package, then IMPORT should share that characteristic of INTERN.
;;; As noted at SB-INT:SELF-EVALUATING-P, there is precedent for it.
;;;
(defun import (symbols &optional (package (sane-package)))
  "Make SYMBOLS accessible as internal symbols in PACKAGE. If a symbol is
already accessible then it has no effect. If a name conflict would result from
the importation, then a correctable error is signalled."
  (with-package-graph ()
    (let* ((package (find-undeleted-package-or-lose package))
           (symbols (symbol-listify symbols))
           (homeless (remove-if #'sb-xc:symbol-package symbols))
           (syms ()))
      (with-single-package-locked-error ()
        (do-symbol-name-groups ((n ss) symbols)
          (multiple-value-bind (s w) (find-symbol n package)
            (cond ((not w)
                   (if (not (null (cdr ss)))
                       (apply #'name-conflict package 'import ss ss)
                       (push (car ss) syms)))
                  ((and (eql (car ss) s) (null (cdr ss)))
                   (if (eq w :inherited) (push s syms)))
                  (t
                   (apply #'name-conflict package 'import ss (adjoin s ss))))))
        (when (or homeless syms)
          (let ((union (delete-duplicates (append homeless syms))))
            (assert-package-unlocked package "importing symbol~P ~{~A~^, ~}"
                                     (length union) union)))
        ;; Add the new symbols to the internal hashtable.
        (let ((internal (package-internal-symbols package)))
          (dolist (sym syms)
            (add-symbol internal sym 'import)))
        ;; If any of the symbols are uninterned, make them be owned by PACKAGE.
        (dolist (sym homeless)
          (%set-symbol-package sym package))
        t))))

;;; If a conflicting symbol is present, unintern it, otherwise just
;;; stick the symbol in.
(defun shadowing-import (symbols &optional (package (sane-package)))
  "Import SYMBOLS into package, disregarding any name conflict. If
  a symbol of the same name is present, then it is uninterned."
  (with-package-graph ()
    (let* ((package (find-undeleted-package-or-lose package))
           (internal (package-internal-symbols package))
           (symbols (symbol-listify symbols))
           (lock-asserted-p nil))
      (with-single-package-locked-error ()
        (dolist (sym symbols)
          (multiple-value-bind (s w) (find-symbol (symbol-name sym) package)
            (unless (or lock-asserted-p
                        (and (eq s sym)
                             (member s (package-shadowing-symbols package))))
              (assert-package-unlocked package "shadowing-importing symbol~P ~
                                           ~{~A~^, ~}" (length symbols) symbols)
              (setf lock-asserted-p t))
            (unless (and w (not (eq w :inherited)) (eq s sym))
              (when (or (eq w :internal) (eq w :external))
                ;; If it was shadowed, we don't want UNINTERN to flame out...
                (setf (package-%shadowing-symbols package)
                      (remove s (the list (package-%shadowing-symbols package))))
                (unintern s package))
              (add-symbol internal sym 'shadowing-import))
            (pushnew sym (package-%shadowing-symbols package)))))))
  t)

(defun shadow (symbols &optional (package (sane-package)))
  "Make an internal symbol in PACKAGE with the same name as each of the
specified SYMBOLS. If a symbol with the given name is already present in
PACKAGE, then the existing symbol is placed in the shadowing symbols list if
it is not already present."
  (with-package-graph ()
    (let* ((package (find-undeleted-package-or-lose package))
           (internal (package-internal-symbols package))
           (symbols (string-listify symbols))
           (lock-asserted-p nil))
      (flet ((present-p (w)
               (and w (not (eq w :inherited)))))
        (with-single-package-locked-error ()
          (dolist (name symbols)
            (multiple-value-bind (s w) (find-symbol name package)
              (unless (or lock-asserted-p
                          (and (present-p w)
                               (member s (package-shadowing-symbols package))))
                (assert-package-unlocked package "shadowing symbol~P ~{~A~^, ~}"
                                         (length symbols) symbols)
                (setf lock-asserted-p t))
              (unless (present-p w)
                (setq s (%make-symbol 2 name)) ; 2 = random interned symbol
                (%set-symbol-package s package)
                (add-symbol internal s 'shadow))
              (pushnew s (package-%shadowing-symbols package))))))))
  t)

;;; Do stuff to use a package, with all kinds of fun name-conflict checking.
(defun use-package (packages-to-use &optional (package (sane-package)))
  "Add all the PACKAGES-TO-USE to the use list for PACKAGE so that the
external symbols of the used packages are accessible as internal symbols in
PACKAGE."
  ;; These don't have to be continuable errors.
  ;; Don't signal them while holding the graph lock
  ;; (Note that we resolve the names outside of any lock.
  ;; If user code further affects the name -> package mapping while concurrently
  ;; doing a USE-PACKAGE, that not our problem)
  (let ((package
          (let ((pkg (find-undeleted-package-or-lose package)))
            ;; "package [...] cannot be the KEYWORD package."
            (when (eq pkg *keyword-package*)
              (error "~S can't use packages" pkg))
            pkg))
        (deduped-packages (remove-duplicates (package-listify packages-to-use) :from-end t))
        (packages))
    ;; "packages-to-use ... The KEYWORD package may not be supplied."
    (when (memq *keyword-package* packages)
      (error "Can not USE-PACKAGE ~S" *keyword-package*))
    (with-package-graph ()
      (with-single-package-locked-error ()
        (dolist (pkg deduped-packages)
          ;; Loop over each package, checking for lock and for
          ;; already-used one at a time...
          (unless (find (package-external-symbols pkg) (package-tables package))
            (assert-package-unlocked package "using package~P ~{~A~^, ~}"
                                     (length packages) packages)
            (push pkg packages)))
        (setq packages (nreverse packages)))
      ;; ... but USE all the packages in one go (after resolving any
      ;; name conflicts)
      (let ((shadowing-symbols (package-%shadowing-symbols package)))
        (do-external-symbol-name-groups ((n ss) packages)
          (multiple-value-bind (s w) (find-symbol n package)
            (cond
              ((not w)
               (when (not (null (cdr ss)))
                 (apply #'name-conflict package 'use-package ss ss)))
              ((and (eql s (car ss)) (null (cdr ss))))
              ((member s shadowing-symbols))
              (t (apply #'name-conflict package 'use-package ss (adjoin s ss)))))))

      (let ((tbls (package-tables package))
            (added-tbls (mapcar #'package-external-symbols packages)))
        (setf (package-tables package)
              (concatenate 'vector tbls added-tbls)))
      (dolist (pkg packages)
        (setf (package-%used-by pkg) nil)))) ; recomputed on demand
  t)

(defun unuse-package (packages-to-unuse &optional (package (sane-package)))
  "Remove PACKAGES-TO-UNUSE from the USE list for PACKAGE."
  (with-package-graph ()
    (let ((package (find-undeleted-package-or-lose package))
          (packages (package-listify packages-to-unuse)))
      (with-single-package-locked-error ()
        (dolist (p packages)
          (when (member p (package-use-list package))
            (assert-package-unlocked package "unusing package~P ~{~A~^, ~}"
                                     (length packages) packages))
          (setf (package-tables package)
                (delete (package-external-symbols p) (package-tables package)))
          (setf (package-%used-by p) nil))) ; recomputed on demand
      t)))

;;;; final initialization

;;;; Due to the relative difficulty - but not impossibility - of manipulating
;;;; symbol-tables in the cross-compilation host, all interning operations
;;;; are delayed until cold-init.
;;;; The cold loader (GENESIS) set *!INITIAL-SYMBOLS* to the target
;;;; representation of the hosts's *COLD-PACKAGE-SYMBOLS*.
;;;; The shape of this list is
;;;;    (uninterned-symbols . ((package . (externals . internals)) ...)
(defvar *!initial-symbols*)
;;; list of (string . list-of-string) to initialize the USE/USED-BY lists
(defvar *!initial-package-graph*)

(defun rebuild-package-vector ()
  (let ((max-id 0))
    (do-packages (pkg)
      (let ((id (package-id pkg)))
        (when id (setq max-id (max id max-id)))))
    (let ((a (make-array (1+ max-id) :initial-element nil)))
      (setf *id->package* a)
      (do-packages (pkg)
        (let ((id (package-id pkg)))
          (when id
            (setf (aref a id) pkg)))))))

(defun pkg-name= (a b) (and (not (eql a 0)) (string= a b)))
(defun !package-cold-init (&aux (specs (cdr *!initial-symbols*)))
  ;; (setq *sym-lookups* 0 *sym-hit-1st-try* 0)
  (setf *package-graph-lock* (sb-thread:make-mutex :name "Package Graph Lock"))
  (setf *package-table-lock* (sb-thread:make-mutex :name "Package Table Lock"))
  (setf *all-packages* #(0))
  (setf *package-nickname-ids* (cons (make-info-hashtable :comparator #'pkg-name=
                                                          :hash-function #'sxhash)
                                     1))
  (setf (sb-thread:mutex-name (info-env-mutex (car *package-nickname-ids*)))
        "package nicknames")
  (dolist (spec specs)
    (destructuring-bind (pkg external-v internal-v) spec
      ;; Genesis conveys the name strings via KEYS in an exceptional but type-correct
      ;; way. The slot has to be cleared to look like a newly made package.
      (with-package-names ()
        (package-registry-update
         pkg (elt (shiftf (package-keys pkg) #()) 0)))
      ;; the symbol MAKE-TABLE wouldn't magically disappear,
      ;; though its only use be to name an FLET in a function
      ;; hanging on an otherwise uninternable symbol. strange but true :-(
      (flet ((!make-table (input)
               (let ((table (make-symbol-table (length (the simple-vector input)))))
                 (dovector (symbol input table)
                   (add-symbol table symbol 'intern)))))
        (let ((externals (!make-table external-v)))
          (setf (package-external-symbols pkg) externals
                (package-internal-symbols pkg) (!make-table internal-v)
                (symtbl-package externals) pkg)))
      (setf (package-%local-nicknames pkg) nil
            (package-source-location pkg) nil
            (package-%implementation-packages pkg) nil)))

  ;; pass 2 - set the 'tables' slots only after all tables have been made
  (loop for (this . use) in *!initial-package-graph*
        do (setf (package-tables (find-package this))
                 (map 'vector (lambda (x) (package-external-symbols (find-package x)))
                              use)))

  (rebuild-package-vector)
  ;; Having made all packages, verify that symbol hashes are good.
  (flet ((check-hash-slot (symbols) ; a vector
           ;; type decl is critical here - can't invoke a hairy aref routine yet
           (dovector (symbol (the simple-vector symbols))
             (when symbol ; skip NIL because of its magic-ness
               (let* ((name (symbol-name symbol))
                      (computed-hash (calc-symbol-name-hash name (length name))))
                 (aver (= (symbol-name-hash symbol) computed-hash)))))))
    (check-hash-slot (car *!initial-symbols*)) ; uninterned symbols
    (dolist (spec specs)
      (check-hash-slot (second spec))
      (check-hash-slot (third spec))))

  ;; The joke's on you. They're pseudo-static, hence not GCable...
  (/show0 "about to MAKUNBOUND *!INITIAL-SYMBOLS*")
  (%makunbound '*!initial-symbols*))      ; (so that it gets GCed)

;;; support for WITH-PACKAGE-ITERATOR

(defun package-iter-step (iter)
  (declare (optimize speed))
  ;; Keeping in mind that (DO-SYMBOLS (s p) (EXPORT s p)) is a prevalent idiom among users,
  ;; the right thing for us is to produce external symbols before producing internals.
  ;; We'll never double-visit an external. It does mean, however, that we must solve the
  ;; double-visit problem for UNEXPORT of externals, as those symbols will appear in
  ;; internals. The difficulties are symmetrical, but solving the problem that occurs
  ;; hypothetically less often is better, as the solution involves consing.
  (labels
      ((advance-from-state (bits &aux (cur-pkg
                                       (truly-the package (car (pkg-iter-pkglist iter)))))
         ;; For most platforms, this CASE should become a jump-table, therefore the order
         ;; in which to test clauses is not as relevant as if may have once been.
         (case (logand bits #b11)
           (2 ; state 2 repeats as many times as there are packages used by cur-package
            (let* ((tables (package-tables cur-pkg))
                   (next-state (the fixnum (+ bits #b100)))
                   (table-idx (ash next-state -2)))
              (if (< table-idx (length tables)) ; remain in state 2
                  (start next-state (symtbl-cells (svref tables table-idx)))
                  (start-next-pkg))))
           (1 ; finished internals, switch to inherited if desired
            (let ((tables (package-tables cur-pkg)))
              (setf (pkg-iter-exclude iter) (package-%shadowing-symbols cur-pkg))
              (if (and (plusp (length tables)) (logbitp 2 (pkg-iter-enable iter)))
                  (start 2 (symtbl-cells (svref tables 0)))
                  (start-next-pkg)))) ; bypass state 2 entirely
           (0 ; finished externals, switch to internals if desired
            (if (logbitp 1 (pkg-iter-enable iter)) ; enter state 1
                (let ((snapshot (shiftf (pkg-iter-snapshot iter) nil)))
                  (start 1 (cond ((null snapshot)
                                  (symtbl-cells (package-internal-symbols cur-pkg)))
                                 (t (atomic-decf (symtbl-immutable (car snapshot)))
                                    (cdr snapshot)))))
                (advance-from-state 1))) ; skip the internals
           (3 ; initial state
            (let ((pkglist (pkg-iter-pkglist iter)))
              (cond ((not pkglist)
                     (setf (pkg-iter-bits iter) -1) ; ensure further -STEP calls drop to state 3
                     (values nil *cl-package* nil)) ; type-correct 2nd value!
                    ((not (logbitp 0 (pkg-iter-enable iter))) (advance-from-state 0))
                    (t ; enter state 0 (externals)
                     (let ((pkg (truly-the package (car pkglist))))
                       (when (logbitp 1 (pkg-iter-enable iter)) ; will visit internals
                         ;; - Capture the internal symbol vector as it exists now, not later.
                         ;; - Bump the copy-on-write count and undo it after visiting externals.
                         ;;   But in fact it's benign for the user to stop iteration early, even
                         ;;   through non-local exit. If that occurs, the C-O-W status sticks,
                         ;;   causing ADD-SYMBOL to copy the table as though immutable,
                         ;    which has no effect other than extra consing.
                         (let ((other (symtbl-%cells (package-internal-symbols pkg))))
                           (atomic-incf (symtbl-immutable (car other)))
                           (setf (pkg-iter-snapshot iter) other)))
                       (start 0 (symtbl-cells (package-external-symbols pkg))))))))))
       (start (state vec)
         (setf (pkg-iter-symbols iter) vec
               (pkg-iter-cur-index iter) (length vec)
               (pkg-iter-bits iter) state)
         (package-iter-step iter))
       (start-next-pkg ()
         (setf (pkg-iter-exclude iter) nil)
         (pop (pkg-iter-pkglist iter))
         (advance-from-state 3))
       (symbol-name= (a b)
         ;; Symbols won't be string= unless their name hashes are =
         ;; so I think it's worth guarding the slower test with a quicker test.
         (and (= (symbol-name-hash a) (symbol-name-hash b))
              (string= a b))))
    ;; INDEX is decremented before use, so if it's already zero upon entry,
    ;; then the current symbol set is exhausted.
    (let ((index (pkg-iter-cur-index iter)))
      (unless (zerop index)
        (let ((sym-vec (pkg-iter-symbols iter))
              (exclude (pkg-iter-exclude iter)))
          (declare (optimize (sb-c:insert-array-bounds-checks 0)))
          (loop
            (let ((sym (aref sym-vec (decf index))))
              (when (and (pkg-symbol-valid-p sym)
                         (not (and exclude (member sym exclude :test #'symbol-name=))))
                (setf (pkg-iter-cur-index iter) index)
                (return-from package-iter-step
                  (values (aref #(:external :internal :inherited)
                                (logand (pkg-iter-bits iter) 3))
                          (truly-the package (car (pkg-iter-pkglist iter)))
                          sym))))
            (when (zerop index) (return))))))
    (advance-from-state (pkg-iter-bits iter))))

(defun program-assert-symbol-home-package-unlocked (context symbol control)
  (handler-bind ((package-lock-violation
                  (lambda (condition)
                    (ecase context
                      (:compile
                       ;; FIXME: Code containing a lexically impermissible
                       ;; violation causes both a warning AND an error.
                       ;; The warning is enough. It's ugly that both happen.
                       (warn "Compile-time package lock violation:~%  ~A"
                             condition)
                       (sb-c:compiler-error condition))
                      (:eval
                       (eval-error condition))))))
    (with-single-package-locked-error (:symbol symbol control))))

(defmethod documentation ((x package) (doc-type (eql 't)))
  (package-doc-string x))
(defmethod (setf documentation) (new-value (x package) (doc-type (eql 't)))
  (setf (package-doc-string x) new-value))

;;; Note: Almost always you want to use FIND-UNDELETED-PACKAGE-OR-LOSE
;;; instead of this function. (The distinction only actually matters when
;;; PACKAGE-DESIGNATOR is actually a deleted package, and in that case
;;; you generally do want to signal an error instead of proceeding.)
(defun %find-package-or-lose (package-designator)
  (let ((package-designator package-designator))
    (prog () retry
       (let ((result (find-package package-designator)))
         (if result
             (return result)
             (find-package-restarts (package-designator)
               (error 'package-does-not-exist
                      :package package-designator
                      :format-control "The name ~S does not designate any package."
                      :format-arguments (list package-designator))))))))

;;; ANSI specifies (in the section for FIND-PACKAGE) that the
;;; consequences of most operations on deleted packages are
;;; unspecified. We try to signal errors in such cases.
(defun find-undeleted-package-or-lose (package-designator)
  (let ((package-designator package-designator))
    (prog () retry
       (let ((maybe-result (%find-package-or-lose package-designator)))
         (if (package-%name maybe-result) ; if not deleted
             (return maybe-result)
             (find-package-restarts (package-designator)
               (error 'package-does-not-exist
                      :package maybe-result
                      :format-control "The package ~S has been deleted."
                      :format-arguments (list maybe-result))))))))

;;; Given a cell containing memoization data for FIND-PACKAGE, perform that action
;;; with as little work as possible.
(macrolet ((def-finder (name sub-finder validp)
             `(defun ,name (cell)
                (declare (optimize speed))
                (declare (sb-c::tlab :system))
                (let ((memo (cdr cell)))
                  ;; weak vector: #(cookie nick-id #<pkg>)
                  #-weak-vector-readbarrier (declare (type (simple-vector 3) memo))
                  #+weak-vector-readbarrier (declare (weak-vector memo))
                  (when (eq (weak-vector-ref memo 0) *package-names-cookie*) ; valid cache entry
                    (binding* ((nick-id (weak-vector-ref memo 1) :exit-if-null)
                               (current-package *package*)
                               (pkg (and (package-%local-nicknames current-package)
                                         (pkgnick-search-by-id nick-id current-package))
                                    :exit-if-null))
                      (return-from ,name pkg))
                    (let ((pkg (weak-vector-ref memo 2)))
                      (when ,validp
                        (return-from ,name pkg))))
                  (let* ((string (car cell))
                         (pkg (,sub-finder string))
                         (new (sb-c::allocate-weak-vector 3)))
                    (setf (weak-vector-ref new 0) *package-names-cookie*
                          (weak-vector-ref new 1) (info-gethash string (car *package-nickname-ids*))
                          (weak-vector-ref new 2) pkg)
                    (sb-thread:barrier (:write))
                    (setf (cdr cell) new)
                    pkg)))))

  (def-finder cached-find-undeleted-package find-undeleted-package-or-lose
              (and (packagep pkg) (package-%name pkg)))
  (def-finder cached-find-package find-package
              ;; We can return a NIL as the cached answer. But NIL is also the value that
              ;; the garbage collector stuffs in when the element is otherwise unreferenced.
              ;; This is actually OK -  There is no way for the "correct" answer to become
              ;; other than NIL without invalidating the cache line by incrementing the
              ;; cookie. i.e. a cached NIL is correct until the next cache invalidation.
              (or (null pkg) (and (packagep pkg) (package-%name pkg)))))


;;;; special package hacks for the loader

(defvar *deferred-package-names*)

;;; A deferred package is a package which is not added to the normal
;;; package database until some later point in time. The current
;;; purpose for deferred packages is so that the loader symbol fasl
;;; ops can intern symbols into packages, without the packages
;;; necessarily being created yet. This is important for deferred
;;; top-level form loading, as to not cause loading symbols (which may
;;; appear as literal code constants) to fail early. Since the
;;; deferred package is not added to the normal package database until
;;; the package is actually created, we preserve package environment
;;; semantics at runtime, and give a reasonable error if the package
;;; has not been created by the end of the load.
(defun find-or-maybe-make-deferred-package (name)
  (or (find-package name)
      (let ((table *deferred-package-names*)) ; bind on demand
        (unless table
          (setq table (make-hash-table :test 'equal)
                *deferred-package-names* table))
        (or (gethash name table)
            (let ((package (%make-package (make-symbol-table 0)
                                          (make-symbol-table 0))))
              (setf (symtbl-package (package-external-symbols package)) package)
              (with-package-names ()
                (setf (package-%name package) name)
                ;; T is an indicator for doing nothing except assigning an ID
                (package-registry-update package t))
              (setf (gethash name table) package))))))

;;; Return the deferred package object for NAME if it exists, otherwise
;;; return NIL.
(defun resolve-deferred-package (name)
  (and (boundp '*deferred-package-names*)
       *deferred-package-names*
       (let ((package (gethash name *deferred-package-names*)))
         (when package (remhash name *deferred-package-names*))
         package)))

;;; Bind the deferred package name table and test to see
;;; if the deferred package table still has any unresolved entries
;;; after FUNCTION is called.
(defun call-with-loader-package-names (function)
  (let* ((boundp (boundp '*deferred-package-names*))
         ;; bind on demand
         (*deferred-package-names* (if boundp *deferred-package-names* nil)))
    (funcall function)
    (when (and (not boundp) *deferred-package-names*)
      (maphash
       (lambda (name package)
         (dovector (sym (symtbl-cells (package-internal-symbols package)))
           (when (symbolp sym)
               (error 'simple-package-error
                      :format-control
                      "The loader tried loading the symbol named ~a ~
                       into the package named ~a, but the package did ~
                       not get defined, and does not exist."
                      :format-arguments (list (symbol-name sym) name)))))
       *deferred-package-names*))))

;;; We don't benefit from these transforms because any time we have a constant
;;; package in our code, we refer to it via #.(FIND-PACKAGE).
;;; This is a necessity due to the fact that historically all packages got renamed
;;; during warm build, and all INTERN / FIND-SYMBOL calls had to continue to work.
;;; Now we rename packages for the _next_ build, but the principle remains.
(in-package "SB-C")
(defun find-package-xform (package)
  ;; If you make KEYWORD, CL, COMMON-LISP, CL-USER, or COMMON-LISP-USER
  ;; a local nickname of something, I kinda think you deserve to lose,
  ;; never mind that we permit it. You'll probably want to declare INTERN
  ;; and FIND-SYMBOL as notinline.
  (let* ((string (string (lvar-value package)))
         (std-pkg (cond ((string= string "KEYWORD") "KEYWORD")
                        ((or (string= string "COMMON-LISP") (string= string "CL"))
                         "CL")
                        ((or (string= string "COMMON-LISP-USER")
                             (string= string "CL-USER"))
                         "CL-USER"))))
    ;; the 2nd value return value of T means that this form's value is the
    ;; package, otherwise this form is an argument to the finding function.
    (if std-pkg
        (values `(load-time-value (find-undeleted-package-or-lose ,std-pkg) t) t)
        (values `(load-time-value (cons ,string (make-weak-vector 3))) nil))))

(deftransform intern ((name package-name) (t (constant-arg string-designator)))
  `(sb-impl::intern2 name ,(find-package-xform package-name)))

(deftransform find-symbol ((name package-name) (t (constant-arg string-designator)))
  `(sb-impl::find-symbol2 name ,(find-package-xform package-name)))

;;; As for the INTERN transform, you could be screwed if you use any of the
;;; standard package names as a local nickname of a random package.
(deftransform find-package ((name) ((constant-arg string-designator)))
  (multiple-value-bind (form constp) (find-package-xform name)
    ;; standard packages are effectively constant objects, otherwise
    ;; we have to invoke the find function.
    (if constp form `(sb-impl::cached-find-package ,form))))
