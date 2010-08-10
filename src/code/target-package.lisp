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

(in-package "SB!IMPL")

(!begin-collecting-cold-init-forms)

(!cold-init-forms
  (/show0 "entering !PACKAGE-COLD-INIT"))

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

(defvar *package-graph-lock*)
(!cold-init-forms
 (setf *package-graph-lock* (sb!thread:make-mutex :name "Package Graph Lock")))

(defun call-with-package-graph (function)
  (declare (function function))
  ;; FIXME: Since name conflicts can be signalled while holding the
  ;; mutex, user code can be run leading to lock ordering problems.
  ;;
  ;; This used to be a spinlock, but there it can be held for a long
  ;; time while the debugger waits for user input.
  (sb!thread:with-recursive-lock (*package-graph-lock*)
    (funcall function)))

;;; a map from package names to packages
(defvar *package-names*)
(declaim (type hash-table *package-names*))
(!cold-init-forms
 (setf *package-names* (make-hash-table :test 'equal :synchronized t)))

(defmacro with-package-names ((names &key) &body body)
  `(let ((,names *package-names*))
     (with-locked-hash-table (,names)
       ,@body)))

;;;; PACKAGE-HASHTABLE stuff

(def!method print-object ((table package-hashtable) stream)
  (declare (type stream stream))
  (print-unreadable-object (table stream :type t)
    (format stream
            ":SIZE ~S :FREE ~S :DELETED ~S"
            (package-hashtable-size table)
            (package-hashtable-free table)
            (package-hashtable-deleted table))))

;;; the maximum load factor we allow in a package hashtable
(defconstant +package-rehash-threshold+ 0.75)

;;; the load factor desired for a package hashtable when writing a
;;; core image
(defconstant +package-hashtable-image-load-factor+ 0.5)

;;; Make a package hashtable having a prime number of entries at least
;;; as great as (/ SIZE +PACKAGE-REHASH-THRESHOLD+). If RES is supplied,
;;; then it is destructively modified to produce the result. This is
;;; useful when changing the size, since there are many pointers to
;;; the hashtable.
;;; Actually, the smallest table built here has three entries. This
;;; is necessary because the double hashing step size is calculated
;;; using a division by the table size minus two.
(defun make-or-remake-package-hashtable (size
                                         &optional
                                         res)
  (flet ((actual-package-hashtable-size (size)
           (loop for n of-type fixnum
              from (logior (ceiling size +package-rehash-threshold+) 1)
              by 2
              when (positive-primep n) return n)))
    (let* ((n (actual-package-hashtable-size size))
           (size (truncate (* n +package-rehash-threshold+)))
           (table (make-array n))
           (hash (make-array n
                             :element-type '(unsigned-byte 8)
                             :initial-element 0)))
      (if res
          (setf (package-hashtable-table res) table
                (package-hashtable-hash res) hash
                (package-hashtable-size res) size
                (package-hashtable-free res) size
                (package-hashtable-deleted res) 0)
          (setf res (%make-package-hashtable table hash size)))
      res)))

;;; Destructively resize TABLE to have room for at least SIZE entries
;;; and rehash its existing entries.
(defun resize-package-hashtable (table size)
  (let* ((vec (package-hashtable-table table))
         (hash (package-hashtable-hash table))
         (len (length vec)))
    (make-or-remake-package-hashtable size table)
    (dotimes (i len)
      (when (> (aref hash i) 1)
        (add-symbol table (svref vec i))))))

;;;; package locking operations, built conditionally on :sb-package-locks

#!+sb-package-locks
(progn
(defun package-locked-p (package)
  #!+sb-doc
  "Returns T when PACKAGE is locked, NIL otherwise. Signals an error
if PACKAGE doesn't designate a valid package."
  (package-lock (find-undeleted-package-or-lose package)))

(defun lock-package (package)
  #!+sb-doc
  "Locks PACKAGE and returns T. Has no effect if PACKAGE was already
locked. Signals an error if PACKAGE is not a valid package designator"
  (setf (package-lock (find-undeleted-package-or-lose package)) t))

(defun unlock-package (package)
  #!+sb-doc
  "Unlocks PACKAGE and returns T. Has no effect if PACKAGE was already
unlocked. Signals an error if PACKAGE is not a valid package designator."
  (setf (package-lock (find-undeleted-package-or-lose package)) nil)
  t)

(defun package-implemented-by-list (package)
  #!+sb-doc
  "Returns a list containing the implementation packages of
PACKAGE. Signals an error if PACKAGE is not a valid package designator."
  (package-%implementation-packages (find-undeleted-package-or-lose package)))

(defun package-implements-list (package)
  #!+sb-doc
  "Returns the packages that PACKAGE is an implementation package
of. Signals an error if PACKAGE is not a valid package designator."
  (let ((package (find-undeleted-package-or-lose package)))
    (loop for x in (list-all-packages)
          when (member package (package-%implementation-packages x))
          collect x)))

(defun add-implementation-package (packages-to-add
                                   &optional (package *package*))
  #!+sb-doc
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
  #!+sb-doc
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
  #!+sb-doc
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
  ;; KLUDGE: (package-lock package) needs to be before
  ;; comparison to *package*, since during cold init this gets
  ;; called before *package* is bound -- but no package should
  ;; be locked at that point.
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
       (not (and symbolp (member symbol (disabled-package-locks))))))

(defun disabled-package-locks ()
  (if (boundp 'sb!c::*lexenv*)
      (sb!c::lexenv-disabled-package-locks sb!c::*lexenv*)
      sb!c::*disabled-package-locks*))

) ; progn

;;;; more package-locking these are NOPs unless :sb-package-locks is
;;;; in target features. Cross-compiler NOPs for these are in cross-misc.

;;; The right way to establish a package lock context is
;;; WITH-SINGLE-PACKAGE-LOCKED-ERROR, defined in early-package.lisp
;;;
;;; Must be used inside the dynamic contour established by
;;; WITH-SINGLE-PACKAGE-LOCKED-ERROR
(defun assert-package-unlocked (package &optional format-control
                                &rest format-arguments)
  #!-sb-package-locks
  (declare (ignore format-control format-arguments))
  #!+sb-package-locks
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
(defun assert-symbol-home-package-unlocked (name format)
  #!-sb-package-locks
  (declare (ignore format))
  #!+sb-package-locks
  (let* ((symbol (etypecase name
                   (symbol name)
                   (list (if (and (consp (cdr name))
                                  (eq 'setf (first name)))
                             (second name)
                             ;; Skip lists of length 1, single conses and
                             ;; (class-predicate foo), etc.
                             ;; FIXME: MOP and package-lock
                             ;; interaction needs to be thought about.
                             (return-from
                              assert-symbol-home-package-unlocked
                               name)))))
         (package (symbol-package symbol)))
    (when (package-lock-violation-p package symbol)
      (package-lock-violation package
                              :symbol symbol
                              :format-control format
                              :format-arguments (list name))))
  name)


;;;; miscellaneous PACKAGE operations

(def!method print-object ((package package) stream)
  (let ((name (package-%name package)))
    (if name
        (print-unreadable-object (package stream :type t)
          (prin1 name stream))
        (print-unreadable-object (package stream :type t :identity t)
          (write-string "(deleted)" stream)))))

;;; ANSI says (in the definition of DELETE-PACKAGE) that these, and
;;; most other operations, are unspecified for deleted packages. We
;;; just do the easy thing and signal errors in that case.
(macrolet ((def (ext real)
             `(defun ,ext (x) (,real (find-undeleted-package-or-lose x)))))
  (def package-nicknames package-%nicknames)
  (def package-use-list package-%use-list)
  (def package-used-by-list package-%used-by-list)
  (def package-shadowing-symbols package-%shadowing-symbols))

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
  #!+sb-doc "the current package")
;;; FIXME: should be declared of type PACKAGE, with no NIL init form,
;;; after I get around to cleaning up DOCUMENTATION

;;; This magical variable is T during initialization so that
;;; USE-PACKAGE's of packages that don't yet exist quietly win. Such
;;; packages are thrown onto the list *DEFERRED-USE-PACKAGES* so that
;;; this can be fixed up later.
;;;
;;; FIXME: This could be cleaned up the same way I do it in my package
;;; hacking when setting up the cross-compiler. Then we wouldn't have
;;; this extraneous global variable and annoying runtime tests on
;;; package operations. (*DEFERRED-USE-PACKAGES* would also go away.)
(defvar *in-package-init*)

;;; pending USE-PACKAGE arguments saved up while *IN-PACKAGE-INIT* is true
(defvar *!deferred-use-packages*)
(!cold-init-forms
  (setf *!deferred-use-packages* nil))

(define-condition bootstrap-package-not-found (condition)
  ((name :initarg :name :reader bootstrap-package-name)))
(defun debootstrap-package (&optional condition)
  (invoke-restart
   (find-restart-or-control-error 'debootstrap-package condition)))

(defun find-package (package-designator)
  (flet ((find-package-from-string (string)
           (declare (type string string))
           (let ((packageoid (gethash string *package-names*)))
             (when (and (null packageoid)
                        (not *in-package-init*) ; KLUDGE
                        (let ((mismatch (mismatch "SB!" string)))
                          (and mismatch (= mismatch 3))))
               (restart-case
                   (signal 'bootstrap-package-not-found :name string)
                 (debootstrap-package ()
                   (return-from find-package
                     (if (string= string "SB!XC")
                         (find-package "COMMON-LISP")
                         (find-package
                          (substitute #\- #\! string :count 1)))))))
             packageoid)))
    (typecase package-designator
      (package package-designator)
      (symbol (find-package-from-string (symbol-name package-designator)))
      (string (find-package-from-string package-designator))
      (character (find-package-from-string (string package-designator)))
      (t (error 'type-error
                :datum package-designator
                :expected-type '(or character package string symbol))))))

;;; Return a list of packages given a package designator or list of
;;; package designators, or die trying.
(defun package-listify (thing)
  (let ((res ()))
    (dolist (thing (if (listp thing) thing (list thing)) res)
      (push (find-undeleted-package-or-lose thing) res))))

;;; Make a package name into a simple-string.
(defun package-namify (n)
  (stringify-package-designator n))

;;; ANSI specifies (in the definition of DELETE-PACKAGE) that PACKAGE-NAME
;;; returns NIL (not an error) for a deleted package, so this is a special
;;; case where we want to use bare %FIND-PACKAGE-OR-LOSE instead of
;;; FIND-UNDELETED-PACKAGE-OR-LOSE.
(defun package-name (package-designator)
  (package-%name (%find-package-or-lose package-designator)))

;;;; operations on package hashtables

;;; Compute a number from the sxhash of the pname and the length which
;;; must be between 2 and 255.
(defmacro entry-hash (length sxhash)
  `(the fixnum
        (+ (the fixnum
                (rem (the fixnum
                          (logxor ,length
                                  ,sxhash
                                  (the fixnum (ash ,sxhash -8))
                                  (the fixnum (ash ,sxhash -16))
                                  (the fixnum (ash ,sxhash -19))))
                     254))
           2)))
;;; FIXME: should be wrapped in EVAL-WHEN (COMPILE EXECUTE)

;;; Add a symbol to a package hashtable. The symbol is assumed
;;; not to be present.
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
  (let* ((vec (package-hashtable-table table))
         (hash (package-hashtable-hash table))
         (len (length vec))
         (sxhash (%sxhash-simple-string (symbol-name symbol)))
         (h2 (1+ (rem sxhash (- len 2)))))
    (declare (fixnum sxhash h2))
    (do ((i (rem sxhash len) (rem (+ i h2) len)))
        ((< (the fixnum (aref hash i)) 2)
         (if (zerop (the fixnum (aref hash i)))
             (decf (package-hashtable-free table))
             (decf (package-hashtable-deleted table)))
         (setf (svref vec i) symbol)
         (setf (aref hash i)
               (entry-hash (length (symbol-name symbol))
                           sxhash)))
      (declare (fixnum i)))))

;;; Resize the package hashtables of all packages so that their load
;;; factor is +PACKAGE-HASHTABLE-IMAGE-LOAD-FACTOR+. Called from
;;; SAVE-LISP-AND-DIE to optimize space usage in the image.
(defun tune-hashtable-sizes-of-all-packages ()
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
(defmacro with-symbol ((index-var symbol-var table string length sxhash
                                  entry-hash)
                       &body forms)
  (let ((vec (gensym)) (hash (gensym)) (len (gensym)) (h2 (gensym))
        (name (gensym)) (name-len (gensym)) (ehash (gensym)))
    `(let* ((,vec (package-hashtable-table ,table))
            (,hash (package-hashtable-hash ,table))
            (,len (length ,vec))
            (,h2 (1+ (the index (rem (the hash ,sxhash)
                                      (the index (- ,len 2)))))))
       (declare (type index ,len ,h2))
       (prog ((,index-var (rem (the hash ,sxhash) ,len))
              ,symbol-var ,ehash)
         (declare (type (or index null) ,index-var))
         LOOP
         (setq ,ehash (aref ,hash ,index-var))
         (cond ((eql ,ehash ,entry-hash)
                (setq ,symbol-var (svref ,vec ,index-var))
                (let* ((,name (symbol-name ,symbol-var))
                       (,name-len (length ,name)))
                  (declare (type index ,name-len))
                  (when (and (= ,name-len ,length)
                             (string= ,string ,name
                                      :end1 ,length
                                      :end2 ,name-len))
                    (go DOIT))))
               ((zerop ,ehash)
                (setq ,index-var nil)
                (go DOIT)))
         (setq ,index-var (+ ,index-var ,h2))
         (when (>= ,index-var ,len)
           (setq ,index-var (- ,index-var ,len)))
         (go LOOP)
         DOIT
         (return (progn ,@forms))))))

;;; Delete the entry for STRING in TABLE. The entry must exist.
(defun nuke-symbol (table string)
  (declare (simple-string string))
  (let* ((length (length string))
         (hash (%sxhash-simple-string string))
         (ehash (entry-hash length hash)))
    (declare (type index length)
             (type hash hash))
    (with-symbol (index symbol table string length hash ehash)
      (setf (aref (package-hashtable-hash table) index) 1)
      (setf (aref (package-hashtable-table table) index) nil)
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
;;; conflict then give the user a chance to do something about it. Caller is
;;; responsible for having acquired the mutex via WITH-PACKAGES.
(defun %enter-new-nicknames (package nicknames)
  (declare (type list nicknames))
  (dolist (n nicknames)
    (let* ((n (package-namify n))
           (found (with-package-names (names)
                    (or (gethash n names)
                        (progn
                          (setf (gethash n names) package)
                          (push n (package-%nicknames package))
                          package)))))
      (cond ((eq found package))
            ((string= (the string (package-%name found)) n)
             (cerror "Ignore this nickname."
                     'simple-package-error
                     :package package
                     :format-control "~S is a package name, so it cannot be a nickname for ~S."
                     :format-arguments (list n (package-%name package))))
            (t
             (cerror "Leave this nickname alone."
                     'simple-package-error
                     :package package
                     :format-control "~S is already a nickname for ~S."
                     :format-arguments (list n (package-%name found))))))))

(defun make-package (name &key
                          (use '#.*default-package-use-list*)
                          nicknames
                          (internal-symbols 10)
                          (external-symbols 10))
  #!+sb-doc
  #.(format nil
     "Make a new package having the specified NAME, NICKNAMES, and USE
list. :INTERNAL-SYMBOLS and :EXTERNAL-SYMBOLS are estimates for the number of
internal and external symbols which will ultimately be present in the package.
The default value of USE is implementation-dependent, and in this
implementation it is ~S." *default-package-use-list*)
  (prog (clobber)
   :restart
     (when (find-package name)
       ;; ANSI specifies that this error is correctable.
       (cerror "Clobber existing package."
               "A package named ~S already exists" name)
       (setf clobber t))
     (with-packages ()
       ;; Check for race, signal the error outside the lock.
       (when (and (not clobber) (find-package name))
         (go :restart))
       (let* ((name (package-namify name))
              (package (internal-make-package
                        :%name name
                        :internal-symbols (make-or-remake-package-hashtable
                                           internal-symbols)
                        :external-symbols (make-or-remake-package-hashtable
                                           external-symbols))))

         ;; Do a USE-PACKAGE for each thing in the USE list so that checking for
         ;; conflicting exports among used packages is done.
         (if *in-package-init*
             (push (list use package) *!deferred-use-packages*)
             (use-package use package))

         ;; FIXME: ENTER-NEW-NICKNAMES can fail (ERROR) if nicknames are illegal,
         ;; which would leave us with possibly-bad side effects from the earlier
         ;; USE-PACKAGE (e.g. this package on the used-by lists of other packages,
         ;; but not in *PACKAGE-NAMES*, and possibly import side effects too?).
         ;; Perhaps this can be solved by just moving ENTER-NEW-NICKNAMES before
         ;; USE-PACKAGE, but I need to check what kinds of errors can be caused by
         ;; USE-PACKAGE, too.
         (%enter-new-nicknames package nicknames)
         (return (setf (gethash name *package-names*) package))))
     (bug "never")))

;;; Change the name if we can, blast any old nicknames and then
;;; add in any new ones.
;;;
;;; FIXME: ANSI claims that NAME is a package designator (not just a
;;; string designator -- weird). Thus, NAME could
;;; be a package instead of a string. Presumably then we should not change
;;; the package name if NAME is the same package that's referred to by PACKAGE.
;;; If it's a *different* package, we should probably signal an error.
;;; (perhaps (ERROR 'ANSI-WEIRDNESS ..):-)
(defun rename-package (package-designator name &optional (nicknames ()))
  #!+sb-doc
  "Changes the name and nicknames for a package."
  (let ((package nil))
  (tagbody :restart
       (setq package (find-undeleted-package-or-lose package-designator))
       (let* ((name (package-namify name))
            (found (find-package name))
            (nicks (mapcar #'string nicknames)))
       (unless (or (not found) (eq found package))
         (error 'simple-package-error
                :package name
                :format-control "A package named ~S already exists."
                :format-arguments (list name)))
       (with-single-package-locked-error ()
         (unless (and (string= name (package-name package))
                      (null (set-difference nicks (package-nicknames package)
                                            :test #'string=)))
           (assert-package-unlocked package "rename as ~A~@[ with nickname~P ~
                                           ~{~A~^, ~}~]"
                                    name (length nicks) nicks))
         (with-package-names (names)
           ;; Check for race conditions now that we have the lock.
           (unless (eq package (find-package package-designator))
             (go :restart))
           ;; Do the renaming.
           (remhash (package-%name package) names)
           (dolist (n (package-%nicknames package))
             (remhash n names))
           (setf (package-%name package) name
                 (gethash name names) package
                 (package-%nicknames package) ()))
           (%enter-new-nicknames package nicknames))))
    package))

(defun delete-package (package-designator)
  #!+sb-doc
  "Delete the package designated by PACKAGE-DESIGNATOR from the package
  system data structures."
  (tagbody :restart
     (let ((package (find-package package-designator)))
       (cond ((not package)
              ;; This continuable error is required by ANSI.
              (cerror
               "Return ~S."
               (make-condition
                'simple-package-error
                :package package-designator
                :format-control "There is no package named ~S."
                :format-arguments (list package-designator))
               (return-from delete-package nil)))
             ((not (package-name package)) ; already deleted
              (return-from delete-package nil))
             (t
              (with-single-package-locked-error
                  (:package package "deleting package ~A" package)
                (let ((use-list (package-used-by-list package)))
                  (when use-list
                    ;; This continuable error is specified by ANSI.
                    (cerror
                     "Remove dependency in other packages."
                     (make-condition
                      'simple-package-error
                      :package package
                      :format-control
                      "~@<Package ~S is used by package~P:~2I~_~S~@:>"
                      :format-arguments (list (package-name package)
                                              (length use-list)
                                              (mapcar #'package-name use-list))))
                    (dolist (p use-list)
                      (unuse-package package p))))
                (with-package-graph ()
                  ;; Check for races, restart if necessary.
                  (let ((package2 (find-package package-designator)))
                    (when (or (neq package package2) (package-used-by-list package2))
                      (go :restart)))
                  (dolist (used (package-use-list package))
                    (unuse-package used package))
                  (do-symbols (sym package)
                    (unintern sym package))
                  (with-package-names (names)
                    (remhash (package-name package) names)
                    (dolist (nick (package-nicknames package))
                      (remhash nick names))
                    (setf (package-%name package) nil
                          ;; Setting PACKAGE-%NAME to NIL is required in order to
                          ;; make PACKAGE-NAME return NIL for a deleted package as
                          ;; ANSI requires. Setting the other slots to NIL
                          ;; and blowing away the PACKAGE-HASHTABLES is just done
                          ;; for tidiness and to help the GC.
                          (package-%nicknames package) nil))
                  (setf (package-%use-list package) nil
                        (package-tables package) nil
                        (package-%shadowing-symbols package) nil
                        (package-internal-symbols package)
                        (make-or-remake-package-hashtable 0)
                        (package-external-symbols package)
                        (make-or-remake-package-hashtable 0)))
                (return-from delete-package t)))))))

(defun list-all-packages ()
  #!+sb-doc
  "Return a list of all existing packages."
  (let ((res ()))
    (with-package-names (names)
      (maphash (lambda (k v)
                 (declare (ignore k))
                 (pushnew v res))
               names))
    res))

(defun intern (name &optional (package (sane-package)))
  #!+sb-doc
  "Return a symbol in PACKAGE having the specified NAME, creating it
  if necessary."
  ;; We just simple-stringify the name and call INTERN*, where the real
  ;; logic is.
  (let ((name (if (simple-string-p name)
                  name
                  (coerce name 'simple-string)))
        (package (find-undeleted-package-or-lose package)))
    (declare (simple-string name))
      (intern* name
               (length name)
               package)))

(defun find-symbol (name &optional (package (sane-package)))
  #!+sb-doc
  "Return the symbol named STRING in PACKAGE. If such a symbol is found
  then the second value is :INTERNAL, :EXTERNAL or :INHERITED to indicate
  how the symbol is accessible. If no symbol is found then both values
  are NIL."
  ;; We just simple-stringify the name and call FIND-SYMBOL*, where the
  ;; real logic is.
  (let ((name (if (simple-string-p name) name (coerce name 'simple-string))))
    (declare (simple-string name))
    (find-symbol* name
                  (length name)
                  (find-undeleted-package-or-lose package))))

;;; If the symbol named by the first LENGTH characters of NAME doesn't exist,
;;; then create it, special-casing the keyword package.
(defun intern* (name length package)
  (declare (simple-string name))
  (multiple-value-bind (symbol where) (find-symbol* name length package)
    (cond (where
           (values symbol where))
          (t
           ;; Let's try again with a lock: the common case has the
           ;; symbol already interned, handled by the first leg of the
           ;; COND, but in case another thread is interning in
           ;; parallel we need to check after grabbing the lock.
           (with-package-graph ()
             (setf (values symbol where) (find-symbol* name length package))
             (if where
                 (values symbol where)
                 (let ((symbol-name (subseq name 0 length)))
                   (with-single-package-locked-error
                       (:package package "interning ~A" symbol-name)
                     (let ((symbol (make-symbol symbol-name)))
                       (%set-symbol-package symbol package)
                       (cond
                         ((eq package *keyword-package*)
                          (%set-symbol-value symbol symbol)
                          (add-symbol (package-external-symbols package) symbol))
                         (t
                          (add-symbol (package-internal-symbols package) symbol)))
                       (values symbol nil))))))))))

;;; Check internal and external symbols, then scan down the list
;;; of hashtables for inherited symbols.
(defun find-symbol* (string length package)
  (declare (simple-string string)
           (type index length))
  (let* ((hash (%sxhash-simple-substring string length))
         (ehash (entry-hash length hash)))
    (declare (type hash hash ehash))
    (with-symbol (found symbol (package-internal-symbols package)
                        string length hash ehash)
      (when found
        (return-from find-symbol* (values symbol :internal))))
    (with-symbol (found symbol (package-external-symbols package)
                        string length hash ehash)
      (when found
        (return-from find-symbol* (values symbol :external))))
    (let ((head (package-tables package)))
      (do ((prev head table)
           (table (cdr head) (cdr table)))
          ((null table) (values nil nil))
        (with-symbol (found symbol (car table) string length hash ehash)
          (when found
            ;; At this point we used to move the table to the
            ;; beginning of the list, probably on the theory that we'd
            ;; soon be looking up further items there. Unfortunately
            ;; that was very much non-thread safe. Since the failure
            ;; mode was nasty (corruption of the package in a way
            ;; which would make symbol lookups loop infinitely) and it
            ;; would be triggered just by doing reads to a resource
            ;; that users can't do their own locking on, that code has
            ;; been removed. If we ever add locking to packages,
            ;; resurrecting that code might make sense, even though it
            ;; didn't seem to have much of an performance effect in
            ;; normal use.
            ;;
            ;; -- JES, 2006-09-13
            (return-from find-symbol* (values symbol :inherited))))))))

;;; Similar to FIND-SYMBOL, but only looks for an external symbol.
;;; This is used for fast name-conflict checking in this file and symbol
;;; printing in the printer.
(defun find-external-symbol (string package)
  (declare (simple-string string))
  (let* ((length (length string))
         (hash (%sxhash-simple-string string))
         (ehash (entry-hash length hash)))
    (declare (type index length)
             (type hash hash))
    (with-symbol (found symbol (package-external-symbols package)
                        string length hash ehash)
      (values symbol found))))

(defun print-symbol-with-prefix (stream symbol colon at)
  #!+sb-doc
  "For use with ~/: Write SYMBOL to STREAM as if it is not accessible from
  the current package."
  (declare (ignore colon at))
  ;; Only keywords should be accessible from the keyword package, and
  ;; keywords are always printed with colons, so this guarantees that the
  ;; symbol will not be printed without a prefix.
  (let ((*package* *keyword-package*))
    (write symbol :stream stream :escape t)))

(define-condition name-conflict (reference-condition package-error)
  ((function :initarg :function :reader name-conflict-function)
   (datum :initarg :datum :reader name-conflict-datum)
   (symbols :initarg :symbols :reader name-conflict-symbols))
  (:default-initargs :references (list '(:ansi-cl :section (11 1 1 2 5))))
  (:report
   (lambda (c s)
     (format s "~@<~S ~S causes name-conflicts in ~S between the ~
                following symbols:~2I~@:_~
                ~{~/sb-impl::print-symbol-with-prefix/~^, ~}~:@>"
             (name-conflict-function c)
             (name-conflict-datum c)
             (package-error-package c)
             (name-conflict-symbols c)))))

(defun name-conflict (package function datum &rest symbols)
  (restart-case
      (error 'name-conflict :package package :symbols symbols
             :function function :datum datum)
    (resolve-conflict (chosen-symbol)
      :report "Resolve conflict."
      :interactive
      (lambda ()
        (let* ((len (length symbols))
               (nlen (length (write-to-string len :base 10)))
               (*print-pretty* t))
          (format *query-io* "~&~@<Select a symbol to be made accessible in ~
                              package ~A:~2I~@:_~{~{~V,' D. ~
                              ~/sb-impl::print-symbol-with-prefix/~}~@:_~}~
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
                     (if (eq (symbol-package package-symbol) package)
                         (unintern package-symbol package) ; CLHS 11.1.1.2.5
                         (shadowing-import (list chosen-symbol) package)))
                 (shadowing-import (list chosen-symbol) package)))
            ((import)
             (if presentp
                 (if (eq package-symbol chosen-symbol)
                     nil                ; re-importing the same symbol
                     (shadowing-import (list chosen-symbol) package))
                 (shadowing-import (list chosen-symbol) package)))))))))

;;; If we are uninterning a shadowing symbol, then a name conflict can
;;; result, otherwise just nuke the symbol.
(defun unintern (symbol &optional (package (sane-package)))
  #!+sb-doc
  "Makes SYMBOL no longer present in PACKAGE. If SYMBOL was present then T is
returned, otherwise NIL. If PACKAGE is SYMBOL's home package, then it is made
uninterned."
  (with-package-graph ()
    (let* ((package (find-undeleted-package-or-lose package))
           (name (symbol-name symbol))
           (shadowing-symbols (package-%shadowing-symbols package)))
      (declare (list shadowing-symbols))

      (with-single-package-locked-error ()
        (when (find-symbol name package)
          (assert-package-unlocked package "uninterning ~A" name))

        ;; If a name conflict is revealed, give us a chance to
        ;; shadowing-import one of the accessible symbols.
        (when (member symbol shadowing-symbols)
          (let ((cset ()))
            (dolist (p (package-%use-list package))
              (multiple-value-bind (s w) (find-external-symbol name p)
                (when w (pushnew s cset))))
            (when (cdr cset)
              (apply #'name-conflict package 'unintern symbol cset)
              (return-from unintern t)))
          (setf (package-%shadowing-symbols package)
                (remove symbol shadowing-symbols)))

        (multiple-value-bind (s w) (find-symbol name package)
          (declare (ignore s))
          (cond ((or (eq w :internal) (eq w :external))
                 (nuke-symbol (if (eq w :internal)
                                  (package-internal-symbols package)
                                  (package-external-symbols package))
                              name)
                 (if (eq (symbol-package symbol) package)
                     (%set-symbol-package symbol nil))
                 t)
                (t nil)))))))

;;; Take a symbol-or-list-of-symbols and return a list, checking types.
(defun symbol-listify (thing)
  (cond ((listp thing)
         (dolist (s thing)
           (unless (symbolp s) (error "~S is not a symbol." s)))
         thing)
        ((symbolp thing) (list thing))
        (t
         (error "~S is neither a symbol nor a list of symbols." thing))))

(defun string-listify (thing)
  (mapcar #'string (if (listp thing)
                       thing
                       (list thing))))

;;; This is like UNINTERN, except if SYMBOL is inherited, it chases
;;; down the package it is inherited from and uninterns it there. Used
;;; for name-conflict resolution. Shadowing symbols are not uninterned
;;; since they do not cause conflicts.
(defun moby-unintern (symbol package)
  (unless (member symbol (package-%shadowing-symbols package))
    (or (unintern symbol package)
        (let ((name (symbol-name symbol)))
          (multiple-value-bind (s w) (find-symbol name package)
            (declare (ignore s))
            (when (eq w :inherited)
              (dolist (q (package-%use-list package))
                (multiple-value-bind (u x) (find-external-symbol name q)
                  (declare (ignore u))
                  (when x
                    (unintern symbol q)
                    (return t))))))))))

(defun export (symbols &optional (package (sane-package)))
  #!+sb-doc
  "Exports SYMBOLS from PACKAGE, checking that no name conflicts result."
  (with-package-graph ()
    (let ((package (find-undeleted-package-or-lose package))
          (symbols (symbol-listify symbols))
          (syms ()))
      ;; Punt any symbols that are already external.
      (dolist (sym symbols)
        (multiple-value-bind (s w)
            (find-external-symbol (symbol-name sym) package)
          (declare (ignore s))
          (unless (or w (member sym syms))
            (push sym syms))))
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
            (cerror
             "~S these symbols into the ~A package."
             (make-condition
              'simple-package-error
              :package package
              :format-control
              "~@<These symbols are not accessible in the ~A package:~2I~_~S~@:>"
              :format-arguments (list (package-%name package) missing))
             'import (package-%name package))
            (import missing package))
          (import imports package))

        ;; And now, three pages later, we export the suckers.
        (let ((internal (package-internal-symbols package))
              (external (package-external-symbols package)))
          (dolist (sym syms)
            (nuke-symbol internal (symbol-name sym))
            (add-symbol external sym))))
      t)))

;;; Check that all symbols are accessible, then move from external to internal.
(defun unexport (symbols &optional (package (sane-package)))
  #!+sb-doc
  "Makes SYMBOLS no longer exported from PACKAGE."
  (with-package-graph ()
    (let ((package (find-undeleted-package-or-lose package))
          (symbols (symbol-listify symbols))
          (syms ()))
      (dolist (sym symbols)
        (multiple-value-bind (s w) (find-symbol (symbol-name sym) package)
          (cond ((or (not w) (not (eq s sym)))
                 (error 'simple-package-error
                        :package package
                        :format-control "~S is not accessible in the ~A package."
                        :format-arguments (list sym (package-%name package))))
                ((eq w :external) (pushnew sym syms)))))
      (with-single-package-locked-error ()
        (when syms
          (assert-package-unlocked package "unexporting symbol~P ~{~A~^, ~}"
                                   (length syms) syms))
        (let ((internal (package-internal-symbols package))
              (external (package-external-symbols package)))
          (dolist (sym syms)
            (add-symbol internal sym)
            (nuke-symbol external (symbol-name sym)))))
      t)))

;;; Check for name conflict caused by the import and let the user
;;; shadowing-import if there is.
(defun import (symbols &optional (package (sane-package)))
  #!+sb-doc
  "Make SYMBOLS accessible as internal symbols in PACKAGE. If a symbol is
already accessible then it has no effect. If a name conflict would result from
the importation, then a correctable error is signalled."
  (with-package-graph ()
    (let* ((package (find-undeleted-package-or-lose package))
           (symbols (symbol-listify symbols))
           (homeless (remove-if #'symbol-package symbols))
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
  #!+sb-doc
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
  #!+sb-doc
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
                (setq s (make-symbol name))
                (%set-symbol-package s package)
                (add-symbol internal s))
              (pushnew s (package-%shadowing-symbols package))))))))
  t)

;;; Do stuff to use a package, with all kinds of fun name-conflict checking.
(defun use-package (packages-to-use &optional (package (sane-package)))
  #!+sb-doc
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
                   (multiple-value-bind (s w)
                       (find-external-symbol (symbol-name sym) pkg)
                     (when (and w
                                (not (eq s sym))
                                (not (member sym shadowing-symbols)))
                       (name-conflict package 'use-package pkg sym s))))
                 (dolist (p use-list)
                   (do-external-symbols (sym p)
                     (multiple-value-bind (s w)
                         (find-external-symbol (symbol-name sym) pkg)
                       (when (and w
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
            (push (package-external-symbols pkg) (cdr (package-tables package)))
            (push package (package-%used-by-list pkg)))))))
  t)

(defun unuse-package (packages-to-unuse &optional (package (sane-package)))
  #!+sb-doc
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
                        (the list (package-tables package))))
          (setf (package-%used-by-list p)
                (remove package (the list (package-%used-by-list p))))))
      t)))

(defun find-all-symbols (string-or-symbol)
  #!+sb-doc
  "Return a list of all symbols in the system having the specified name."
  (let ((string (string string-or-symbol))
        (res ()))
    (with-package-names (names)
      (maphash (lambda (k v)
                 (declare (ignore k))
                 (multiple-value-bind (s w) (find-symbol string v)
                   (when w (pushnew s res))))
               names))
    res))

;;;; APROPOS and APROPOS-LIST

(defun briefly-describe-symbol (symbol)
  (fresh-line)
  (prin1 symbol)
  (when (boundp symbol)
    (write-string " (bound)"))
  (when (fboundp symbol)
    (write-string " (fbound)")))

(defun apropos-list (string-designator
                     &optional
                     package-designator
                     external-only)
  #!+sb-doc
  "Like APROPOS, except that it returns a list of the symbols found instead
  of describing them."
  (if package-designator
      (let ((package (find-undeleted-package-or-lose package-designator))
            (string (stringify-string-designator string-designator))
            (result nil))
        (do-symbols (symbol package)
          (when (and (eq (symbol-package symbol) package)
                     (or (not external-only)
                         (eq (nth-value 1 (find-symbol (symbol-name symbol)
                                                       package))
                             :external))
                     (search string (symbol-name symbol) :test #'char-equal))
            (push symbol result)))
        (sort result #'string-lessp))
      (mapcan (lambda (package)
                (apropos-list string-designator package external-only))
              (sort (list-all-packages) #'string-lessp :key #'package-name))))

(defun apropos (string-designator &optional package external-only)
  #!+sb-doc
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

;;;; The cold loader (GENESIS) makes the data structure in
;;;; *!INITIAL-SYMBOLS*. We grovel over it, making the specified
;;;; packages and interning the symbols. For a description of the
;;;; format of *!INITIAL-SYMBOLS*, see the GENESIS source.

(defvar *!initial-symbols*)

(!cold-init-forms

  (setq *in-package-init* t)

  (/show0 "about to loop over *!INITIAL-SYMBOLS* to make packages")
  (dolist (spec *!initial-symbols*)
    (let* ((pkg (apply #'make-package (first spec)))
           (internal (package-internal-symbols pkg))
           (external (package-external-symbols pkg)))
      (/show0 "back from MAKE-PACKAGE, PACKAGE-NAME=..")
      (/primitive-print (package-name pkg))

      ;; Put internal symbols in the internal hashtable and set package.
      (dolist (symbol (second spec))
        (add-symbol internal symbol)
        (%set-symbol-package symbol pkg))

      ;; External symbols same, only go in external table.
      (dolist (symbol (third spec))
        (add-symbol external symbol)
        (%set-symbol-package symbol pkg))

      ;; Don't set package for imported symbols.
      (dolist (symbol (fourth spec))
        (add-symbol internal symbol))
      (dolist (symbol (fifth spec))
        (add-symbol external symbol))

      ;; Put shadowing symbols in the shadowing symbols list.
      (setf (package-%shadowing-symbols pkg) (sixth spec))
      ;; Set the package documentation
      (setf (package-doc-string pkg) (seventh spec))))

  ;; FIXME: These assignments are also done at toplevel in
  ;; boot-extensions.lisp. They should probably only be done once.
  (/show0 "setting up *CL-PACKAGE* and *KEYWORD-PACKAGE*")
  (setq *cl-package* (find-package "COMMON-LISP"))
  (setq *keyword-package* (find-package "KEYWORD"))

  (/show0 "about to MAKUNBOUND *!INITIAL-SYMBOLS*")
  (%makunbound '*!initial-symbols*)       ; (so that it gets GCed)

  ;; Make some other packages that should be around in the cold load.
  ;; The COMMON-LISP-USER package is required by the ANSI standard,
  ;; but not completely specified by it, so in the cross-compilation
  ;; host Lisp it could contain various symbols, USE-PACKAGEs, or
  ;; nicknames that we don't want in our target SBCL. For that reason,
  ;; we handle it specially, not dumping the host Lisp version at
  ;; genesis time..
  (aver (not (find-package "COMMON-LISP-USER")))
  ;; ..but instead making our own from scratch here.
  (/show0 "about to MAKE-PACKAGE COMMON-LISP-USER")
  (make-package "COMMON-LISP-USER"
                :nicknames '("CL-USER")
                :use '("COMMON-LISP"
                       ;; ANSI encourages us to put extension packages
                       ;; in the USE list of COMMON-LISP-USER.
                       "SB!ALIEN" "SB!ALIEN" "SB!DEBUG"
                       "SB!EXT" "SB!GRAY" "SB!PROFILE"))

  ;; Now do the *!DEFERRED-USE-PACKAGES*.
  (/show0 "about to do *!DEFERRED-USE-PACKAGES*")
  (dolist (args *!deferred-use-packages*)
    (apply #'use-package args))

  ;; The Age Of Magic is over, we can behave ANSIly henceforth.
  (/show0 "about to SETQ *IN-PACKAGE-INIT*")
  (setq *in-package-init* nil)

  ;; For the kernel core image wizards, set the package to *CL-PACKAGE*.
  ;;
  ;; FIXME: We should just set this to (FIND-PACKAGE
  ;; "COMMON-LISP-USER") once and for all here, instead of setting it
  ;; once here and resetting it later.
  (setq *package* *cl-package*))

(!cold-init-forms
  (/show0 "done with !PACKAGE-COLD-INIT"))

(!defun-from-collected-cold-init-forms !package-cold-init)
