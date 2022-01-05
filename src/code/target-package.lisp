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
;;;; FIXME: The code contains a lot of type declarations. Are they
;;;; all really necessary?

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
;;;; Hash-table lock on *PACKAGE-NAMES* is held via WITH-PACKAGE-NAMES while
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
(define-load-time-global *package-names* nil)
(declaim (type info-hashtable *package-names*))

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
          (packages (the simple-vector (cdr pkgnicks))))
      ;; Scan backwards so that pushing preserves order
      (loop for i downfrom (1- (length strings)) to 1 by 2
            do (let* ((token (aref packages (1- (aref strings i))))
                      (package (aref packages (aref strings i)))
                      (id (ash token (- pkgnick-index-bits))))
                 (aver (= (ldb (byte pkgnick-index-bits 0) token) (ash i -1)))
                 ;; cull deleted entries
                 (when (and (packagep package) (package-%name package))
                   (let ((pair (cons (aref strings (1- i)) package)))
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
                                  (aref packages i) token
                                  (aref packages (1+ i)) (cddr item))))
                 (cons strings packages)))))
       (when (eq old (setf old (cas (package-%local-nicknames this-package) old new)))
         (return nil))))))

;;; A macro to help search the nickname vectors.
;;; Return the logical index of KEY in VECTOR (in which each two
;;; elements comprise a key/value pair)
(macrolet ((bsearch (key vector)
             `(let ((v (the simple-vector ,vector)))
                ;; The search operates on a logical index, which is half the physical
                ;; index. The returned value is a physical index.
                (named-let recurse ((start 0) (end (ash (length v) -1)))
                  (declare (type (unsigned-byte 9) start end)
                           (optimize (sb-c:insert-array-bounds-checks 0)))
                  (when (< start end)
                    (let* ((i (ash (+ start end) -1))
                           (elt (keyfn (aref v (ash i 1)))))
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
                 (string->id (car nicks))
                 (found (bsearch string string->id) :exit-if-null)
                 (package (svref (cdr nicks) (svref string->id found))))
        (cond ((and package (package-%name package)) package)
              (t (pkgnick-update base-package nil nil))))))

  (defun pkgnick-search-by-id (id base-package)
    (flet ((keyfn (x) (ash x (- pkgnick-index-bits)))
           (compare (a b) (signum (- a b))))
      (binding* ((id->obj (cdr (package-%local-nicknames base-package)) :exit-if-null)
                 (found (bsearch id id->obj) :exit-if-null)
                 (package (svref id->obj found)))
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
             (vector (the simple-vector (cdr nicks))))
    (loop for i downfrom (1- (length vector)) to 1 by 2
          when (eq package (aref vector i))
          return (aref (car nicks)
                       (let ((token (aref vector (1- i))))
                         (ash (ldb (byte pkgnick-index-bits 0) token) 1))))))

;;; This would have to be bumped ~ 2*most-positive-fixnum times to overflow.
(define-load-time-global *package-names-cookie* most-negative-fixnum)
(declaim (fixnum *package-names-cookie*))

(defmacro with-package-names ((table-var &key) &body body)
  `(let ((,table-var *package-names*))
     (sb-thread::with-recursive-system-lock ((info-env-mutex ,table-var))
       ,@body)))

;;;; iteration macros

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
  (with-unique-names (bits index sym-vec pkglist symbol kind)
    (let ((state (list bits index sym-vec pkglist))
          (select (logior (if (member :internal  symbol-types) 1 0)
                          (if (member :external  symbol-types) 2 0)
                          (if (member :inherited symbol-types) 4 0))))
      `(multiple-value-bind ,state (package-iter-init ,select ,package-list)
         (let (,symbol ,kind)
           (macrolet
               ((,mname ()
                   '(if (eql 0 (multiple-value-setq (,@state ,symbol ,kind)
                                 (package-iter-step ,@state)))
                        nil
                        (values t ,symbol ,kind
                                (car (truly-the list ,pkglist))))))
             ,@body))))))

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

;;;; PACKAGE-HASHTABLE stuff

(defmethod print-object ((table package-hashtable) stream)
  (declare (type stream stream))
  (print-unreadable-object (table stream :type t :identity t)
    (let* ((n-live (%package-hashtable-symbol-count table))
           (n-deleted (package-hashtable-deleted table))
           (n-filled (+ n-live n-deleted))
           (n-cells (length (package-hashtable-cells table))))
      (format stream
              "(~D+~D)/~D [~@[~,3f words/sym,~]load=~,1f%]"
              n-live n-deleted n-cells
              (unless (zerop n-live)
                (/ (* (1+ (/ sb-vm:n-word-bytes)) n-cells) n-live))
              (* 100 (/ n-filled n-cells))))))

;;; the maximum load factor we allow in a package hashtable
(defconstant +package-rehash-threshold+ 3/4)

;;; the load factor desired for a package hashtable when writing a
;;; core image
(defconstant +package-hashtable-image-load-factor+ 1/2)

;;; Make a package hashtable having a prime number of entries at least
;;; as great as (/ SIZE +PACKAGE-REHASH-THRESHOLD+). If RES is supplied,
;;; then it is destructively modified to produce the result. This is
;;; useful when changing the size, since there are many pointers to
;;; the hashtable.
;;; Actually, the smallest table built here has three entries. This
;;; is necessary because the double hashing step size is calculated
;;; using a division by the table size minus two.
(defun make-package-hashtable (size)
  (flet ((actual-package-hashtable-size (size)
           (loop for n of-type fixnum
              from (logior (ceiling size +package-rehash-threshold+) 1)
              by 2
              when (positive-primep n) return n)))
    (let* ((n (actual-package-hashtable-size size))
           ;; SIZE is how many symbols we'd like to be able to store,
           ;; but the number of physical cells is N, chosen for its primality.
           (size (truncate (* n +package-rehash-threshold+)))
           (table (make-array n :initial-element 0)))
      (%make-package-hashtable table size))))

(declaim (inline pkg-symbol-valid-p))
(defun pkg-symbol-valid-p (x) (not (fixnump x)))

;;; Destructively resize TABLE to have room for at least SIZE entries
;;; and rehash its existing entries.
(defun resize-package-hashtable (table size)
  (let ((temp-table (make-package-hashtable size)))
    (dovector (sym (package-hashtable-cells table))
      (when (pkg-symbol-valid-p sym)
        (add-symbol temp-table sym)))
    (setf (package-hashtable-cells table) (package-hashtable-cells temp-table)
          (package-hashtable-size table) (package-hashtable-size temp-table)
          (package-hashtable-free table) (package-hashtable-free temp-table)
          (package-hashtable-deleted table) 0)))

;;;; package locking operations, built unconditionally now

(defun package-locked-p (package)
  "Returns T when PACKAGE is locked, NIL otherwise. Signals an error
if PACKAGE doesn't designate a valid package."
  (package-lock (find-undeleted-package-or-lose package)))

(defun lock-package (package)
  "Locks PACKAGE and returns T. Has no effect if PACKAGE was already
locked. Signals an error if PACKAGE is not a valid package designator"
  (setf (package-lock (find-undeleted-package-or-lose package)) t))

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
;;; and others, so that %set-fdefinition and others could just use
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

;;; Perform (GETHASH NAME TABLE) and then unwrap the value if it is a list.
;;; List vs nonlist disambiguates a nickname from the primary name.
;;; And never return the symbol :DELETED.
(declaim (inline %get-package))
(defun %get-package (name table)
  (let ((found (info-gethash name table)))
    (cond ((listp found) (car found))
          ((neq found :deleted) found))))

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
        (%get-package string *package-names*))))

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

;;; ANSI says (in the definition of DELETE-PACKAGE) that these, and
;;; most other operations, are unspecified for deleted packages. We
;;; just do the easy thing and signal errors in that case.
(macrolet ((def (ext real)
             `(defun ,ext (package-designator)
                (,real (find-undeleted-package-or-lose package-designator)))))
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

(defmacro do-packages ((package) &body body)
  ;; INFO-MAPHASH is not intrinsically threadsafe - but actually
  ;; quite easy to fix - so meanwhile until it's fixed, grab the lock.
  `(with-package-names (.table.)
      (info-maphash
       (lambda (.name. ,package)
         (declare (ignore .name.))
         (unless (or (listp ,package) (eq ,package :deleted))
           ,@body))
       .table.)))

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
      (when (find designee (cdr (package-%local-nicknames namer)))
        (push namer result)))
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

(defun %package-hashtable-symbol-count (table)
  (let ((size (the fixnum
                (- (package-hashtable-size table)
                   (package-hashtable-deleted table)))))
    (the fixnum
      (- size (package-hashtable-free table)))))

(defun package-internal-symbol-count (package)
  (%package-hashtable-symbol-count (package-internal-symbols package)))

(defun package-external-symbol-count (package)
  (%package-hashtable-symbol-count (package-external-symbols package)))

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

;;; ANSI specifies (in the definition of DELETE-PACKAGE) that PACKAGE-NAME
;;; returns NIL (not an error) for a deleted package, so this is a special
;;; case where we want to use bare %FIND-PACKAGE-OR-LOSE instead of
;;; FIND-UNDELETED-PACKAGE-OR-LOSE.
(defun package-name (package-designator)
  (package-%name (%find-package-or-lose package-designator)))

;;;; operations on package hashtables

;;; Add a symbol to a package hashtable. The symbol MUST NOT be present.
(defun add-symbol (table symbol)
  (when (zerop (package-hashtable-free table))
    ;; The hashtable is full. Resize it to be able to hold twice the
    ;; amount of symbols than it currently contains. The actual new size
    ;; can be smaller than twice the current size if the table contained
    ;; deleted entries.
    (resize-package-hashtable table
                              (* (- (package-hashtable-size table)
                                    (package-hashtable-deleted table))
                                 2)))
  (let* ((symvec (package-hashtable-cells table))
         (len (length symvec))
         (sxhash (truly-the fixnum (ensure-symbol-hash symbol)))
         (h2 (1+ (rem sxhash (- len 2)))))
    (declare (fixnum sxhash h2))
    (do ((i (rem sxhash len) (rem (+ i h2) len)))
        ((fixnump (svref symvec i))
         ;; Interning has to pre-check whether the symbol existed in any
         ;; package used by the package in which intern is happening,
         ;; so it's not really useful to be lock-free here,
         ;; even though we could be. (We're already inside a mutex)
         (let ((old (svref symvec i)))
           (setf (svref symvec i) symbol)
           (if (eql old 0)
               (decf (package-hashtable-free table)) ; unused
               (decf (package-hashtable-deleted table))))) ; tombstone
      (declare (fixnum i)))))

;;; Insert a mapping from NAME (a string) to OBJECT (a package or singleton
;;; list of a package) into TABLE (the hashtable in *PACKAGE-NAMES*),
;;; taking care to adjust the count of phantom entries.
(defun %register-package (table name object)
  (let ((oldval (info-gethash name table)))
    (unless oldval ; if any value existed, no new physical cell is claimed
      (when (> (info-env-tombstones table)
               (floor (info-storage-capacity (info-env-storage table)) 4))
        ;; Otherwise, when >1/4th of the table consists of tombstones,
        ;; then rebuild the table.
        (%rebuild-package-names table)
        (when (eq oldval :deleted)
          (setq oldval nil))))
    (setf (info-gethash name table) object)
    (when (eq oldval :deleted)
      (decf (info-env-tombstones table)))))

;;; Rebuild the *PACKAGE-NAMES* table.
;;; The calling thread must own the mutex on *PACKAGE-NAMES* so that
;;; this is synchronized across all insertion and deletion operations.
(defun %rebuild-package-names (old)
  (let ((new (copy-structure old))
        (nondeleted-count (- (info-env-count old)
                             (info-env-tombstones old))))
    (setf (info-env-storage new) (make-info-storage nondeleted-count)
          (info-env-count new) 0
          (info-env-tombstones new) 0)
    (info-maphash (lambda (key value)
                    (unless (eq value :deleted)
                      (setf (info-gethash key new) value)))
                  old)
    (setf (info-env-storage old) (info-env-storage new)
          (info-env-count old) (info-env-count new)
          (info-env-tombstones old) 0)
    old))

;;; Resize the package hashtables of all packages so that their load
;;; factor is +PACKAGE-HASHTABLE-IMAGE-LOAD-FACTOR+. Called from
;;; SAVE-LISP-AND-DIE to optimize space usage in the image.
(defun tune-hashtable-sizes-of-all-packages ()
  (with-package-names (table)
    (when (plusp (info-env-tombstones table))
      (%rebuild-package-names table)))
  (flet ((tune-table-size (table)
           (resize-package-hashtable
            table
            (round (* (/ +package-rehash-threshold+
                         +package-hashtable-image-load-factor+)
                      (- (package-hashtable-size table)
                         (package-hashtable-free table)
                         (package-hashtable-deleted table)))))))
    (dolist (package (list-all-packages))
      (tune-table-size (package-internal-symbols package))
      (tune-table-size (package-external-symbols package)))))

;;; Find where the symbol named STRING is stored in TABLE. INDEX-VAR
;;; is bound to the index, or NIL if it is not present. SYMBOL-VAR
;;; is bound to the symbol. LENGTH and HASH are the length and sxhash
;;; of STRING. ENTRY-HASH is the entry-hash of the string and length.
;;; If the symbol is found, then FORMS are executed; otherwise not.

(defmacro with-symbol (((symbol-var &optional (index-var (sb-xc:gensym))) table
                       string length sxhash) &body forms)
  (with-unique-names (vec len h2 probed-thing name)
    `(let* ((,vec (package-hashtable-cells ,table))
            (,len (length ,vec))
            (,index-var (rem (the hash-code ,sxhash) ,len))
            (,h2 (1+ (the index (rem ,sxhash (the index (- ,len 2)))))))
       (declare (type index ,len ,h2 ,index-var))
       (loop
        (let ((,probed-thing (svref ,vec ,index-var)))
          (cond ((not (fixnump ,probed-thing))
                 (let ((,symbol-var (truly-the symbol ,probed-thing)))
                   (when (eq (symbol-hash ,symbol-var) ,sxhash)
                     (let ((,name (symbol-name ,symbol-var)))
                  ;; The pre-test for length is kind of an unimportant
                  ;; optimization, but passing it for both :end arguments
                  ;; requires that it be within bounds for the probed symbol.
                       (when (and (= (length ,name) ,length)
                                  (string= ,string ,name
                                           :end1 ,length :end2 ,length))
                         (return (progn ,@forms)))))))
              ;; either a never used cell or a tombstone left by UNINTERN
                ((eql ,probed-thing 0) ; really never used
                 (return))))
        (when (>= (incf ,index-var ,h2) ,len)
          (decf ,index-var ,len))))))

;;; Delete SYMBOL from TABLE, storing -1 in its place. SYMBOL must exist.
;;;
(defun nuke-symbol (table symbol)
  (let* ((string (symbol-name symbol))
         (length (length string))
         (hash (symbol-hash symbol)))
    (declare (type index length)
             (hash-code hash))
    (with-symbol ((symbol index) table string length hash)
      ;; It is suboptimal to grab the vectors again, but not broken,
      ;; because we have exclusive use of the table for writing.
      (let ((symvec (package-hashtable-cells table)))
        (setf (aref symvec index) -1))
      (incf (package-hashtable-deleted table))))
  ;; If the table is less than one quarter full, halve its size and
  ;; rehash the entries.
  (let* ((size (package-hashtable-size table))
         (deleted (package-hashtable-deleted table))
         (used (- size
                  (package-hashtable-free table)
                  deleted)))
    (declare (type fixnum size deleted used))
    (when (< used (truncate size 4))
      (resize-package-hashtable table (* used 2)))))

;;; Enter any new NICKNAMES for PACKAGE into *PACKAGE-NAMES*. If there is a
;;; conflict then give the user a chance to do something about it.
;;; Package names do not affect the uses/used-by relation,
;;; so this can be done without the package graph lock held.
(defun %enter-new-nicknames (package nicknames &aux (val (list package)))
  (declare (type list nicknames))
  (dolist (nickname nicknames)
    (let ((found (or (%get-package (the simple-string nickname) *package-names*)
                     (with-package-names (table)
                       (%register-package table nickname val)
                       (push nickname (package-%nicknames package))
                       package))))
      (cond ((eq found package))
            ((string= (the string (package-%name found)) nickname)
             (signal-package-cerror
              package
              "Ignore this nickname."
              "~S is a package name, so it cannot be a nickname for ~S."
              nickname (package-%name package)))
            (t
             (signal-package-cerror
              package
              "Leave this nickname alone."
              "~S is already a nickname for ~S."
              nickname (package-%name found)))))))

;;; ANSI specifies that:
;;;  (1) MAKE-PACKAGE and DEFPACKAGE use the same default package-use-list
;;;  (2) that it (as an implementation-defined value) should be documented,
;;;      which we do in the doc string.
;;; For OAOO reasons we give a name to this value and then use #. readmacro
;;; to splice it in as a constant. Anyone who actually wants a random value
;;; is free to :USE (PACKAGE-USE-LIST :CL-USER) or whatever.
;;;
;;; This must not use DEFGLOBAL, which calls SET-SYMBOL-GLOBAL-VALUE at
;;; compile-time. The xc stub for that is defined to error out on purpose,
;;; since there is no such thing as a global variable in portable CL.
;;; Anyway this is a trivial compile-time constant that disappears after
;;; self-build, so efficiency of reads (one of the concerns of DEFGLOBAL,
;;; the other being documentation of intent) doesn't matter in the least.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *!default-package-use-list* nil))

(defun make-package (name &key
                          (use '#.*!default-package-use-list*)
                          nicknames
                          (internal-symbols 10)
                          (external-symbols 10))
  #.(format nil
     "Make a new package having the specified NAME, NICKNAMES, and USE
list. :INTERNAL-SYMBOLS and :EXTERNAL-SYMBOLS are estimates for the number of
internal and external symbols which will ultimately be present in the package.
The default value of USE is implementation-dependent, and in this
implementation it is ~S." *!default-package-use-list*)
  (prog ((name (stringify-string-designator name))
         (nicks (stringify-string-designators nicknames))
         (package (%make-package (make-package-hashtable internal-symbols)
                                 (make-package-hashtable external-symbols)))
         clobber)
   :restart
     (when (find-package name)
       ;; ANSI specifies that this error is correctable.
       (signal-package-cerror
        name
        "Clobber existing package."
        "A package named ~S already exists" name)
       (setf clobber t))
     (when (and sb-c::*compile-time-eval*
                (eq (sb-c::block-compile sb-c::*compilation*) t))
       (style-warn "Package creation forms limit block compilation and ~
       may cause failure with the use of specified entry ~
       points. Consider moving the package creation form outside the ~
       scope of a block compilation."))
     (with-package-graph ()
         ;; Check for race, signal the error outside the lock.
         (when (and (not clobber) (find-package name))
           (go :restart))
         (setf (package-%name package) name)
         ;; Do a USE-PACKAGE for each thing in the USE list so that checking for
         ;; conflicting exports among used packages is done.
         (use-package use package)
         ;; FIXME: ENTER-NEW-NICKNAMES can fail (ERROR) if nicknames are illegal,
         ;; which would leave us with possibly-bad side effects from the earlier
         ;; USE-PACKAGE (e.g. this package on the used-by lists of other packages,
         ;; but not in *PACKAGE-NAMES*, and possibly import side effects too?).
         ;; Perhaps this can be solved by just moving ENTER-NEW-NICKNAMES before
         ;; USE-PACKAGE, but I need to check what kinds of errors can be caused by
         ;; USE-PACKAGE, too.
         (%enter-new-nicknames package nicks)
         ;; The name table is actually multi-writer concurrent, but due to
         ;; lazy removal of :DELETED entries we want to enforce a single-writer.
         ;; We're inside WITH-PACKAGE-GRAPH so this is already synchronized with
         ;; other MAKE-PACKAGE operations, but we need the additional lock
         ;; so that it synchronizes with RENAME-PACKAGE.
         (with-package-names (table)
           (let* ((vector *id->package*)
                  ;; 30 is an arbitrary constant exceeding the number of builtin packages
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
                     (aref vector new-id) package)))
           (%register-package table name package))
         (atomic-incf *package-names-cookie*)
         (return package))
     (bug "never")))

(flet ((remove-names (package name-table keep-primary-name)
         ;; An INFO-HASHTABLE does not support REMHASH. We can simulate it
         ;; by changing the value to :DELETED.
         ;; (NIL would be preferable, but INFO-GETHASH does not return
         ;; a secondary value indicating whether the NIL was by default
         ;; or found, not does it take a different default to return).
         ;; At some point the table might contain more deleted values than
         ;; useful values. We call %REBUILD-PACKAGE-NAMES to rectify that.
         (dx-let ((names (cons (package-name package)
                               (package-nicknames package)))
                  (i 0))
           (when keep-primary-name (pop names))
           (dolist (name names)
             ;; Aver that the following SETF doesn't insert a new <k,v> pair.
             (aver (info-gethash name name-table))
             (setf (info-gethash name name-table) :deleted)
             (incf i))
           (incf (info-env-tombstones name-table) i))
         nil))

;;; Change the name if we can, blast any old nicknames and then
;;; add in any new ones.
;;;
;;; The spec says that NAME is a package designator (not just a string designator)
;;; which is weird, but potentially meaningful if assigning new global nicknames.
;;; If not called for that purpose, then it's largely pointless, because you can't
;;; rename to any package-designator other than itself without causing a conflict.
;;; A survey of some other implementations suggests that we're in the minority
;;; as to the legality of (RENAME-PACKAGE "A" (FIND-PACKAGE "A") '("A-NICK")).
;;;
;;; ABCL:
;;;   The value #<PACKAGE A> is not of type (OR STRING SYMBOL CHARACTER).
;;; CCL:
;;;   Error: The value #<Package "A"> is not of the expected type (OR STRING SYMBOL CHARACTER).
;;; CMUCL:
;;;   #<The A package, 0/9 internal, 0/9 external> cannot be coerced to a string.
;;; ECL:
;;;   In function STRING, the value of the first argument is #<"A" package>
;;;   which is not of the expected type STRING
;;;
;;; CLISP agrees with us that this usage is permitted. If the new "name" is a
;;; different package, it is merely the same error as if any already-existing name
;;; was given. I see no reason to be more strict than the spec would have it be.
(defun rename-package (package-designator name &optional (nicknames ()))
  "Changes the name and nicknames for a package."
  (prog ((nicks (stringify-string-designators nicknames)))
   :restart
     (let ((package (find-undeleted-package-or-lose package-designator))
           ;; This is the "weirdness" alluded to. Do it in the loop in case
           ;; the stringified value changes on restart when NAME is a package.
           (name (stringify-package-designator name))
           (found (find-package name)))
       (unless (or (not found) (eq found package))
         (signal-package-error name
                               "A package named ~S already exists." name))
       (with-single-package-locked-error ()
         (unless (and (string= name (package-name package))
                      (null (set-difference nicks (package-nicknames package)
                                            :test #'string=)))
           (assert-package-unlocked
            package "renaming as ~A~@[ with nickname~*~P ~1@*~{~A~^, ~}~]"
            name nicks (length nicks)))
         (with-package-names (table)
           ;; Check for race conditions now that we have the lock.
           (unless (eq package (find-package package-designator))
             (go :restart))
           ;; Do the renaming.
           ;; As a special case, do not allow the package to transiently disappear
           ;; if PACKAGE-NAME is unchanged. This avoids glitches with build systems
           ;; which try to operate on subcomponents in parallel, where one of the
           ;; built subcomponents needs to add nicknames to an existing package.
           ;; We could be clever here as well by not removing nicknames that
           ;; will ultimately be re-added, but that didn't seem as critical.
           (let ((keep-primary-name (string= name (package-%name package))))
             (remove-names package table keep-primary-name)
             (unless keep-primary-name
               (%register-package table name package)))
           (setf (package-%name package) name
                 (package-%nicknames package) ()))
         ;; Adding each nickname acquires and releases the table lock,
         ;; because it's potentially interactive (on failure) and therefore
         ;; not ideal to hold the lock for the entire duration.
         (%enter-new-nicknames package nicks))
       (atomic-incf *package-names-cookie*)
       (return package))))

(defun delete-package (package-designator)
  "Delete the package designated by PACKAGE-DESIGNATOR from the package
  system data structures."
  (tagbody :restart
     (let ((package (find-package package-designator)))
       (cond ((not package)
              ;; This continuable error is required by ANSI.
              (signal-package-cerror
               package-designator
               "Ignore."
               "There is no package named ~S." package-designator)
              (return-from delete-package nil))
             ((not (package-name package)) ; already deleted
              (return-from delete-package nil))
             (t
              (with-single-package-locked-error
                  (:package package "deleting package ~A" package)
                (let ((use-list (package-used-by-list package)))
                  (when use-list
                    ;; This continuable error is specified by ANSI.
                    (signal-package-cerror
                     package
                     "Remove dependency in other packages."
                     "~@<Package ~S is used by package~P:~2I~_~S~@:>"
                     (package-name package)
                     (length use-list)
                     (mapcar #'package-name use-list))
                    (dolist (p use-list)
                      (unuse-package package p))))
                (dolist (p (package-implements-list package))
                  (remove-implementation-package package p))
                (with-package-graph ()
                  ;; Check for races, restart if necessary.
                  (let ((package2 (find-package package-designator)))
                    (when (or (neq package package2) (package-used-by-list package2))
                      (go :restart)))
                  (dolist (used (package-use-list package))
                    (unuse-package used package))
                  (setf (package-%local-nicknames package) nil)
                  (flet ((nullify-home (symbols)
                           (dovector (x (package-hashtable-cells symbols))
                             (when (and (symbolp x) (eq (symbol-package x) package))
                               (%set-symbol-package x nil)))))
                    (nullify-home (package-internal-symbols package))
                    (nullify-home (package-external-symbols package)))
                  (with-package-names (table)
                    (awhen (package-id package)
                      (setf (aref *id->package* it) nil (package-id package) nil))
                    (remove-names package table nil)
                    (setf (package-%name package) nil
                          ;; Setting PACKAGE-%NAME to NIL is required in order to
                          ;; make PACKAGE-NAME return NIL for a deleted package as
                          ;; ANSI requires. Setting the other slots to NIL
                          ;; and blowing away the PACKAGE-HASHTABLES is just done
                          ;; for tidiness and to help the GC.
                          (package-%nicknames package) nil))
                  (atomic-incf *package-names-cookie*)
                  (setf (package-%use-list package) nil
                        (package-tables package) #()
                        (package-%shadowing-symbols package) nil
                        (package-internal-symbols package)
                        (make-package-hashtable 0)
                        (package-external-symbols package)
                        (make-package-hashtable 0)))
                (return-from delete-package t)))))))
) ; end FLET

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
  (let ((hash (compute-symbol-hash string length)))
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

;;; If the symbol named by the first LENGTH characters of NAME doesn't exist,
;;; then create it, special-casing the keyword package.
;;; If a new symbol is created, its print name will be an array of ELT-TYPE.
;;; The fasloader always supplies NAME as a (SIMPLE-ARRAY <ELT-TYPE> 1),
;;; but the reader uses a buffer of CHARACTER, which, based on a flag,
;;; can be demoted to an array of BASE-CHAR.
(defun %intern (name length package elt-type ignore-lock)
  ;; No type declarations, %find-symbol will perform the checks
  (multiple-value-bind (symbol where) (%find-symbol name length package)
    (if where
        (values symbol where)
        ;; Double-checked lock pattern: the common case has the symbol already interned,
        ;; but in case another thread is interning in parallel we need to check after
        ;; grabbing the lock.
        (with-package-graph ()
          (setf (values symbol where) (%find-symbol name length package))
          (if where
              (values symbol where)
              (let* ((symbol-name
                       (logically-readonlyize
                        (replace (make-string length :element-type elt-type) name)))
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
                    (add-symbol table symbol)
                    (with-single-package-locked-error
                        (:package package "interning ~A" symbol-name)
                      (add-symbol table symbol)))
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
;;; This is used for fast name-conflict checking in this file and symbol
;;; printing in the printer.
;;; An optimization is possible here: by accepting either a string or symbol,
;;; if the symbol's hash slot is nonzero, we can avoid COMPUTE-SYMBOL-HASH.
(defun find-external-symbol (string package)
  (declare (simple-string string))
  (let* ((length (length string))
         (hash (compute-symbol-hash string length)))
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
  (flet ((importp (c)
           (declare (ignore c))
           (eq 'import function))
         (use-or-export-p (c)
           (declare (ignore c))
           (or (eq 'use-package function)
               (eq 'export function)))
         (old-symbol ()
           (car (remove datum symbols))))
    (let ((pname (package-name package)))
      (restart-case
          (error 'name-conflict :package package :symbols symbols
                                :function function :datum datum)
        ;; USE-PACKAGE and EXPORT
        (keep-old ()
          :report (lambda (s)
                    (ecase function
                      (export
                       (format s "Keep ~S accessible in ~A (shadowing ~S)."
                               (old-symbol) pname datum))
                      (use-package
                       (format s "Keep symbols already accessible in ~A (shadowing others)."
                               pname))))
          :test use-or-export-p
          (dolist (s (remove-duplicates symbols :test #'string=))
            (shadow (symbol-name s) package)))
        (take-new ()
          :report (lambda (s)
                    (ecase function
                      (export
                       (format s "Make ~S accessible in ~A (uninterning ~S)."
                               datum pname (old-symbol)))
                      (use-package
                       (format s "Make newly exposed symbols accessible in ~A, ~
                                  uninterning old ones."
                               pname))))
          :test use-or-export-p
          (dolist (s symbols)
            (when (eq s (find-symbol (symbol-name s) package))
              (unintern s package))))
        ;; IMPORT
        (shadowing-import-it ()
          :report (lambda (s)
                    (format s "Shadowing-import ~S, uninterning ~S."
                            datum (old-symbol)))
          :test importp
          (shadowing-import datum package))
        (dont-import-it ()
          :report (lambda (s)
                    (format s "Don't import ~S, keeping ~S."
                            datum
                            (car (remove datum symbols))))
          :test importp)
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
            (let* ((accessiblep status)     ; never NIL here
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
                     (shadowing-import (list chosen-symbol) package)))))))))))

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
        (when (nth-value 1 (find-symbol name package))
          (assert-package-unlocked package "uninterning ~A" name))

        ;; If a name conflict is revealed, give us a chance to
        ;; shadowing-import one of the accessible symbols.
        (when (member symbol shadowing-symbols)
          (let ((cset ()))
            (dolist (p (package-%use-list package))
              (let ((s (find-external-symbol name p)))
                (unless (eql s 0) (pushnew s cset :test #'eq))))
            (when (cdr cset)
              (apply #'name-conflict package 'unintern symbol cset)
              (return-from unintern t)))
          (setf (package-%shadowing-symbols package)
                (remove symbol shadowing-symbols)))

        (multiple-value-bind (s w) (find-symbol name package)
          (cond ((not (eq symbol s)) nil)
                ((or (eq w :internal) (eq w :external))
                 (nuke-symbol (if (eq w :internal)
                                  (package-internal-symbols package)
                                  (package-external-symbols package))
                              symbol)
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
            (add-symbol external sym)
            (nuke-symbol internal sym))))
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
            (add-symbol internal sym)
            (nuke-symbol external sym))))
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
        (dolist (sym symbols)
          (multiple-value-bind (s w) (find-symbol (symbol-name sym) package)
            (cond ((not w)
                   (let ((found (member sym syms :test #'string=)))
                     (if found
                         (when (not (eq (car found) sym))
                           (setf syms (remove (car found) syms))
                           (name-conflict package 'import sym sym (car found)))
                         (push sym syms))))
                  ((not (eq s sym))
                   (name-conflict package 'import sym sym s))
                  ((eq w :inherited) (push sym syms)))))
        (when (or homeless syms)
          (let ((union (delete-duplicates (append homeless syms))))
            (assert-package-unlocked package "importing symbol~P ~{~A~^, ~}"
                                     (length union) union)))
        ;; Add the new symbols to the internal hashtable.
        (let ((internal (package-internal-symbols package)))
          (dolist (sym syms)
            (add-symbol internal sym)))
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
              (add-symbol internal sym))
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
                (add-symbol internal s))
              (pushnew s (package-%shadowing-symbols package))))))))
  t)

;;; Do stuff to use a package, with all kinds of fun name-conflict checking.
(defun use-package (packages-to-use &optional (package (sane-package)))
  "Add all the PACKAGES-TO-USE to the use list for PACKAGE so that the
external symbols of the used packages are accessible as internal symbols in
PACKAGE."
  (with-package-graph ()
    (let ((packages (package-listify packages-to-use))
          (package (find-undeleted-package-or-lose package)))

      ;; Loop over each package, USE'ing one at a time...
      (with-single-package-locked-error ()
        (dolist (pkg packages)
          (unless (member pkg (package-%use-list package))
            (assert-package-unlocked package "using package~P ~{~A~^, ~}"
                                     (length packages) packages)
            (let ((shadowing-symbols (package-%shadowing-symbols package))
                  (use-list (package-%use-list package)))

              ;; If the number of symbols already accessible is less
              ;; than the number to be inherited then it is faster to
              ;; run the test the other way. This is particularly
              ;; valuable in the case of a new package USEing
              ;; COMMON-LISP.
              (cond
                ((< (+ (package-internal-symbol-count package)
                       (package-external-symbol-count package)
                       (let ((res 0))
                         (dolist (p use-list res)
                           (incf res (package-external-symbol-count p)))))
                    (package-external-symbol-count pkg))
                 (do-symbols (sym package)
                   (let ((s (find-external-symbol (symbol-name sym) pkg)))
                     (when (and (not (eql s 0))
                                (not (eq s sym))
                                (not (member sym shadowing-symbols)))
                       (name-conflict package 'use-package pkg sym s))))
                 (dolist (p use-list)
                   (do-external-symbols (sym p)
                     (let ((s (find-external-symbol (symbol-name sym) pkg)))
                       (when (and (not (eql s 0))
                                  (not (eq s sym))
                                  (not (member
                                        (find-symbol (symbol-name sym) package)
                                        shadowing-symbols)))
                         (name-conflict package 'use-package pkg sym s))))))
                (t
                 (do-external-symbols (sym pkg)
                   (multiple-value-bind (s w)
                       (find-symbol (symbol-name sym) package)
                     (when (and w
                                (not (eq s sym))
                                (not (member s shadowing-symbols)))
                       (name-conflict package 'use-package pkg sym s)))))))

            (push pkg (package-%use-list package))
            (setf (package-tables package)
                  (let ((tbls (package-tables package)))
                    (replace (make-array (1+ (length tbls))
                              :initial-element (package-external-symbols pkg))
                             tbls)))
            (push package (package-%used-by-list pkg)))))))
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
          (setf (package-%use-list package)
                (remove p (the list (package-%use-list package))))
          (setf (package-tables package)
                (delete (package-external-symbols p)
                        (package-tables package)))
          (setf (package-%used-by-list p)
                (remove package (the list (package-%used-by-list p))))))
      t)))

(defun find-all-symbols (string-or-symbol)
  "Return a list of all symbols in the system having the specified name."
  (let ((string (string string-or-symbol))
        (result ()))
    (do-packages (package)
      (multiple-value-bind (symbol found) (find-symbol string package)
        (when found (pushnew symbol result))))
    result))

;;;; APROPOS and APROPOS-LIST

(defun briefly-describe-symbol (symbol)
  (fresh-line)
  (prin1 symbol)
  (when (boundp symbol)
    (let ((value (symbol-value symbol)))
      (if (typep value '(or fixnum symbol hash-table))
          (format t " = ~S" value)
          (format t " (bound, ~S)" (type-of value)))))
  (when (fboundp symbol)
    (write-string " (fbound)")))

(defun apropos-list (string-designator
                     &optional
                     package-designator
                     external-only)
  "Like APROPOS, except that it returns a list of the symbols found instead
  of describing them."
  (if package-designator
      (let ((package (find-undeleted-package-or-lose package-designator))
            (string (stringify-string-designator string-designator))
            (result nil))
        (do-symbols (symbol package)
          (when (and (or (not external-only)
                         (and (eq (sb-xc:symbol-package symbol) package)
                              (eq (nth-value 1 (find-symbol (symbol-name symbol)
                                                            package))
                                  :external)))
                     (search string (symbol-name symbol) :test #'char-equal))
            (pushnew symbol result)))
        (sort result #'string-lessp))
      (delete-duplicates
       (mapcan (lambda (package)
                 (apropos-list string-designator package external-only))
               (sort (list-all-packages) #'string-lessp :key #'package-name)))))

(defun apropos (string-designator &optional package external-only)
  "Briefly describe all symbols which contain the specified STRING.
  If PACKAGE is supplied then only describe symbols present in
  that package. If EXTERNAL-ONLY then only describe
  external symbols in the specified package."
  ;; Implementing this in terms of APROPOS-LIST keeps things simple at the cost
  ;; of some unnecessary consing; and the unnecessary consing shouldn't be an
  ;; issue, since this function is is only useful interactively anyway, and
  ;; we can cons and GC a lot faster than the typical user can read..
  (dolist (symbol (apropos-list string-designator package external-only))
    (briefly-describe-symbol symbol))
  (values))

;;;; final initialization

;;;; Due to the relative difficulty - but not impossibility - of manipulating
;;;; package-hashtables in the cross-compilation host, all interning operations
;;;; are delayed until cold-init.
;;;; The cold loader (GENESIS) set *!INITIAL-SYMBOLS* to the target
;;;; representation of the hosts's *COLD-PACKAGE-SYMBOLS*.
;;;; The shape of this list is
;;;;    (uninterned-symbols . ((package . (externals . internals)) ...)
(defvar *!initial-symbols*)

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
  (setf *package-graph-lock* (sb-thread:make-mutex :name "Package Graph Lock"))
  (setf *package-names* (make-info-hashtable :comparator #'pkg-name=
                                             :hash-function #'sxhash))
  (setf *package-nickname-ids* (cons (make-info-hashtable :comparator #'pkg-name=
                                                          :hash-function #'sxhash)
                                     1))
  (setf (sb-thread:mutex-name (info-env-mutex *package-names*)) "package names"
        (sb-thread:mutex-name (info-env-mutex (car *package-nickname-ids*)))
        "package nicknames")
  (with-package-names (names)
    (dolist (spec specs)
      (let ((pkg (car spec)) (symbols (cdr spec)))
        ;; the symbol MAKE-TABLE wouldn't magically disappear,
        ;; though its only use be to name an FLET in a function
        ;; hanging on an otherwise uninternable symbol. strange but true :-(
        (flet ((!make-table (input)
                 (let ((table (make-package-hashtable
                               (length (the simple-vector input)))))
                   (dovector (symbol input table)
                     (add-symbol table symbol)))))
          (setf (package-external-symbols pkg) (!make-table (first symbols))
                (package-internal-symbols pkg) (!make-table (second symbols))))
        (setf (package-%local-nicknames pkg) nil
              (package-source-location pkg) nil
              (info-gethash (package-%name pkg) names) pkg)
        (dolist (nick (package-%nicknames pkg))
          (setf (info-gethash nick names) (list pkg)))
        (setf (package-%implementation-packages pkg) nil))))

  ;; pass 2 - set the 'tables' slots only after all tables have been made
  (dolist (spec specs)
    (let ((pkg (car spec)))
      (setf (package-tables pkg)
            (map 'vector #'package-external-symbols (package-%use-list pkg)))))

  (rebuild-package-vector)
  ;; Having made all packages, verify that symbol hashes are good.
  (flet ((check-hash-slot (symbols) ; a vector
           ;; type decl is critical here - can't invoke a hairy aref routine yet
           (dovector (symbol (the simple-vector symbols))
             (when symbol ; skip NIL because of its magic-ness
               (let* ((stored-hash (symbol-hash symbol))
                      (name (symbol-name symbol))
                      (computed-hash (compute-symbol-hash name (length name))))
                 (aver (= stored-hash computed-hash)))))))
    (check-hash-slot (car *!initial-symbols*))
    (dolist (spec specs)
      (check-hash-slot (second spec))
      (check-hash-slot (third spec))))

  ;; The joke's on you. They're pseudo-static, hence not GCable...
  (/show0 "about to MAKUNBOUND *!INITIAL-SYMBOLS*")
  (%makunbound '*!initial-symbols*))      ; (so that it gets GCed)

;;; support for WITH-PACKAGE-ITERATOR

(defun package-iter-init (access-types pkg-designator-list)
  (declare (type (integer 1 7) access-types)) ; a nonzero bitmask over types
  (values (logior (ash access-types 3) #b11) 0 #()
          (package-listify pkg-designator-list)))

;; The STATE parameter is comprised of 4 packed fields
;;  [0:1] = substate {0=internal,1=external,2=inherited,3=initial}
;;  [2]   = package with inherited symbols has shadowing symbols
;;  [3:5] = enabling bits for {internal,external,inherited}
;;  [6:]  = index into 'package-tables'
;;
(defconstant +package-iter-check-shadows+  #b000100)

(defun package-iter-step (start-state index sym-vec pkglist)
  ;; the defknown isn't enough
  (declare (type fixnum start-state) (type index index)
           (type simple-vector sym-vec) (type list pkglist))
  (declare (optimize speed) (muffle-conditions compiler-note))
  (labels
      ((advance (state) ; STATE is the one just completed
         (case (logand state #b11)
           ;; Test :INHERITED first because the state repeats for a package
           ;; as many times as there are packages it uses. There are enough
           ;; bits to count up to 2^23 packages if fixnums are 30 bits.
           (2
            (when (desired-state-p 2)
              (let* ((tables (package-tables (this-package)))
                     (next-state (the fixnum (+ state (ash 1 6))))
                     (table-idx (ash next-state -6)))
              (when (< table-idx (length tables))
                (return-from advance ; remain in state 2
                  (start next-state (svref tables table-idx))))))
            (pop pkglist)
            (advance 3)) ; start on next package
           (1 ; finished externals, switch to inherited if desired
            (when (desired-state-p 2)
              (let ((tables (package-tables (this-package))))
                (when (plusp (length tables)) ; inherited symbols
                  (return-from advance ; enter state 2
                    (start (if (package-%shadowing-symbols (this-package))
                               (logior 2 +package-iter-check-shadows+) 2)
                           (svref tables 0))))))
            (advance 2)) ; skip state 2
           (0 ; finished internals, switch to externals if desired
            (if (desired-state-p 1) ; enter state 1
                (start 1 (package-external-symbols (this-package)))
                (advance 1))) ; skip state 1
           (t ; initial state
            (cond ((endp pkglist) ; latch into returning NIL forever more
                   (values 0 0 #() '() nil nil))
                  ((desired-state-p 0) ; enter state 0
                   (start 0 (package-internal-symbols (this-package))))
                  (t (advance 0)))))) ; skip state 0
       (desired-state-p (target-state)
         (logtest start-state (ash 1 (+ target-state 3))))
       (this-package ()
         (truly-the package (car pkglist)))
       (start (next-state new-table)
         (let ((symbols (package-hashtable-cells new-table)))
           (package-iter-step (logior (mask-field (byte 3 3) start-state)
                                      next-state)
                              ;; assert that physical length was nonzero
                              (the index (length symbols))
                              symbols pkglist))))
    (declare (inline desired-state-p this-package))
    (if (zerop index)
        (advance start-state)
        (macrolet ((scan (&optional (guard t))
                   `(loop
                     (let ((sym (aref sym-vec (decf index))))
                       (when (and (pkg-symbol-valid-p sym) ,guard)
                         (return (values start-state index sym-vec pkglist sym
                                         (aref #(:internal :external :inherited)
                                               (logand start-state 3))))))
                     (when (zerop index)
                       (return (advance start-state))))))
          (declare (optimize (sb-c:insert-array-bounds-checks 0)))
          (if (logtest start-state +package-iter-check-shadows+)
              (let ((shadows (package-%shadowing-symbols (this-package))))
                (scan (not (member sym shadows :test #'string=))))
              (scan))))))

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
  (declare (optimize allow-non-returning-tail-call))
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
  (declare (optimize allow-non-returning-tail-call))
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
                (let ((memo (cdr cell)))
                  (declare (type (simple-vector 3) memo)) ; weak vector: #(cookie nick-id #<pkg>)
                  (when (eq (aref memo 0) *package-names-cookie*) ; valid cache entry
                    (binding* ((nick-id (aref memo 1) :exit-if-null)
                               (current-package *package*)
                               (pkg (and (package-%local-nicknames current-package)
                                         (pkgnick-search-by-id nick-id current-package))
                                    :exit-if-null))
                      (return-from ,name pkg))
                    (let ((pkg (aref memo 2)))
                      (when ,validp
                        (return-from ,name pkg))))
                  (let* ((string (car cell))
                         (pkg (,sub-finder string))
                         (new (make-weak-vector 3 :initial-element 0)))
                    (setf (elt new 0) *package-names-cookie*
                          (elt new 1) (info-gethash string (car *package-nickname-ids*))
                          (elt new 2) pkg)
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
        (values `(load-time-value (cons ,string (vector nil nil nil))) nil))))

(deftransform intern ((name package-name) (t (constant-arg string-designator)))
  `(sb-impl::intern2 name ,(find-package-xform package-name)))

(deftransform find-symbol ((name package-name) (t (constant-arg string-designator)))
  `(sb-impl::find-symbol2 name ,(find-package-xform package-name)))
