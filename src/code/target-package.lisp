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

;;;; PACKAGE-HASHTABLE stuff

(def!method print-object ((table package-hashtable) stream)
  (declare (type stream stream))
  (print-unreadable-object (table stream :type t)
    (format stream
	    ":SIZE ~S :FREE ~S :DELETED ~S"
	    (package-hashtable-size table)
	    (package-hashtable-free table)
	    (package-hashtable-deleted table))))

;;; the maximum density we allow in a package hashtable
(defconstant package-rehash-threshold 0.75)

;;; Make a package hashtable having a prime number of entries at least
;;; as great as (/ SIZE PACKAGE-REHASH-THRESHOLD). If RES is supplied,
;;; then it is destructively modified to produce the result. This is
;;; useful when changing the size, since there are many pointers to
;;; the hashtable.
(defun make-or-remake-package-hashtable (size
					 &optional
                                         res)
  (flet ((actual-package-hashtable-size (size)
           (loop for n of-type fixnum
              from (logior (truncate size package-rehash-threshold) 1)
              by 2
              when (positive-primep n) return n)))
    (let* ((n (actual-package-hashtable-size size))
           (size (truncate (* n package-rehash-threshold)))
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

;;; a map from package names to packages
(defvar *package-names*)
(declaim (type hash-table *package-names*))
(!cold-init-forms
  (setf *package-names* (make-hash-table :test 'equal)))

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

;;; FIXME: I rewrote this. Test it and the stuff that calls it.
(defun find-package (package-designator)
  (flet ((find-package-from-string (string)
	   (declare (type string string))
	   (values (gethash string *package-names*))))
    (declare (inline find-package-from-string))
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
  (stringify-name n "package"))

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
  (let* ((vec (package-hashtable-table table))
	 (hash (package-hashtable-hash table))
	 (len (length vec))
	 (sxhash (%sxhash-simple-string (symbol-name symbol)))
	 (h2 (the fixnum (1+ (the fixnum (rem sxhash
					      (the fixnum (- len 2))))))))
    (declare (fixnum len sxhash h2))
    (cond ((zerop (the fixnum (package-hashtable-free table)))
	   (make-or-remake-package-hashtable (* (package-hashtable-size table)
						2)
					     table)
	   (add-symbol table symbol)
	   (dotimes (i len)
	     (declare (fixnum i))
	     (when (> (the fixnum (aref hash i)) 1)
	       (add-symbol table (svref vec i)))))
	  (t
	   (do ((i (rem sxhash len) (rem (+ i h2) len)))
	       ((< (the fixnum (aref hash i)) 2)
		(if (zerop (the fixnum (aref hash i)))
		    (decf (package-hashtable-free table))
		    (decf (package-hashtable-deleted table)))
		(setf (svref vec i) symbol)
		(setf (aref hash i)
		      (entry-hash (length (symbol-name symbol))
				  sxhash)))
	     (declare (fixnum i)))))))

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
	    (,h2 (1+ (the index (rem (the index ,sxhash)
				      (the index (- ,len 2)))))))
       (declare (type index ,len ,h2))
       (prog ((,index-var (rem (the index ,sxhash) ,len))
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
    (declare (type index length hash))
    (with-symbol (index symbol table string length hash ehash)
      (setf (aref (package-hashtable-hash table) index) 1)
      (setf (aref (package-hashtable-table table) index) nil)
      (incf (package-hashtable-deleted table)))))

;;; Enter any new NICKNAMES for PACKAGE into *PACKAGE-NAMES*.
;;; If there is a conflict then give the user a chance to do
;;; something about it.
(defun enter-new-nicknames (package nicknames)
  (declare (type list nicknames))
  (dolist (n nicknames)
    (let* ((n (package-namify n))
	   (found (gethash n *package-names*)))
      (cond ((not found)
	     (setf (gethash n *package-names*) package)
	     (push n (package-%nicknames package)))
	    ((eq found package))
	    ((string= (the string (package-%name found)) n)
	     ;; FIXME: This and the next error needn't have restarts.
	     (with-simple-restart (continue "Ignore this nickname.")
	       (error 'simple-package-error
		      :package package
		      :format-control "~S is a package name, so it cannot be a nickname for ~S."
		      :format-arguments (list n (package-%name package)))))
	    (t
	     (with-simple-restart (continue "Redefine this nickname.")
	       (error 'simple-package-error
		      :package package
		      :format-control "~S is already a nickname for ~S."
		      :format-arguments (list n (package-%name found))))
	     (setf (gethash n *package-names*) package)
	     (push n (package-%nicknames package)))))))

(defun make-package (name &key
			  (use '#.*default-package-use-list*)
			  nicknames
			  (internal-symbols 10)
			  (external-symbols 10))
  #!+sb-doc
  #.(format nil
     "Make a new package having the specified NAME, NICKNAMES, and 
  USE list. :INTERNAL-SYMBOLS and :EXTERNAL-SYMBOLS are
  estimates for the number of internal and external symbols which
  will ultimately be present in the package. The default value of
  USE is implementation-dependent, and in this implementation
  it is ~S."
     *default-package-use-list*)

  ;; Check for package name conflicts in name and nicknames, then
  ;; make the package.
  (when (find-package name)
    ;; ANSI specifies that this error is correctable.
    (cerror "Leave existing package alone."
	    "A package named ~S already exists" name))
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
    (enter-new-nicknames package nicknames)
    (setf (gethash name *package-names*) package)))

;;; Change the name if we can, blast any old nicknames and then
;;; add in any new ones.
;;;
;;; FIXME: ANSI claims that NAME is a package designator (not just a
;;; string designator -- weird). Thus, NAME could
;;; be a package instead of a string. Presumably then we should not change
;;; the package name if NAME is the same package that's referred to by PACKAGE.
;;; If it's a *different* package, we should probably signal an error.
;;; (perhaps (ERROR 'ANSI-WEIRDNESS ..):-)
(defun rename-package (package name &optional (nicknames ()))
  #!+sb-doc
  "Changes the name and nicknames for a package."
  (let* ((package (find-undeleted-package-or-lose package))
	 (name (string name))
	 (found (find-package name)))
    (unless (or (not found) (eq found package))
      (error 'simple-package-error
	     :package name
	     :format-control "A package named ~S already exists."
	     :format-arguments (list name)))
    (remhash (package-%name package) *package-names*)
    (dolist (n (package-%nicknames package))
      (remhash n *package-names*))
     (setf (package-%name package) name)
    (setf (gethash name *package-names*) package)
    (setf (package-%nicknames package) ())
    (enter-new-nicknames package nicknames)
    package))

(defun delete-package (package-or-name)
  #!+sb-doc
  "Delete the package-or-name from the package system data structures."
  (let ((package (if (packagep package-or-name)
		     package-or-name
		     (find-package package-or-name))))
    (cond ((not package)
	   ;; This continuable error is required by ANSI.
	   (with-simple-restart (continue "Return NIL")
	     (error 'simple-package-error
		    :package package-or-name
		    :format-control "There is no package named ~S."
		    :format-arguments (list package-or-name))))
	  ((not (package-name package)) ; already deleted
	   nil)
	  (t
	   (let ((use-list (package-used-by-list package)))
	     (when use-list
	       ;; This continuable error is specified by ANSI.
	       (with-simple-restart
		   (continue "Remove dependency in other packages.")
		 (error 'simple-package-error
			:package package
			:format-control
			"Package ~S is used by package(s):~%  ~S"
			:format-arguments
			(list (package-name package)
			      (mapcar #'package-name use-list))))
	       (dolist (p use-list)
		 (unuse-package package p))))
	   (dolist (used (package-use-list package))
	     (unuse-package used package))
	   (do-symbols (sym package)
	     (unintern sym package))
	   (remhash (package-name package) *package-names*)
	   (dolist (nick (package-nicknames package))
	     (remhash nick *package-names*))
	   (setf (package-%name package) nil
		 ;; Setting PACKAGE-%NAME to NIL is required in order to
		 ;; make PACKAGE-NAME return NIL for a deleted package as
		 ;; ANSI requires. Setting the other slots to NIL
		 ;; and blowing away the PACKAGE-HASHTABLES is just done
		 ;; for tidiness and to help the GC.
		 (package-%nicknames package) nil
		 (package-%use-list package) nil
		 (package-tables package) nil
		 (package-%shadowing-symbols package) nil
		 (package-internal-symbols package)
		 (make-or-remake-package-hashtable 0)
		 (package-external-symbols package)
		 (make-or-remake-package-hashtable 0))
	   t))))

(defun list-all-packages ()
  #!+sb-doc
  "Return a list of all existing packages."
  (let ((res ()))
    (maphash (lambda (k v)
	       (declare (ignore k))
	       (pushnew v res))
	     *package-names*)
    res))

(defun intern (name &optional (package (sane-package)))
  #!+sb-doc
  "Return a symbol having the specified name, creating it if necessary."
  ;; We just simple-stringify the name and call INTERN*, where the real
  ;; logic is.
  (let ((name (if (simple-string-p name)
		name
		(coerce name 'simple-string))))
    (declare (simple-string name))
    (intern* name
	     (length name)
	     (find-undeleted-package-or-lose package))))

(defun find-symbol (name &optional (package (sane-package)))
  #!+sb-doc
  "Return the symbol named String in Package. If such a symbol is found
  then the second value is :internal, :external or :inherited to indicate
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
    (if where
	(values symbol where)
	(let ((symbol (make-symbol (subseq name 0 length))))
	  (%set-symbol-package symbol package)
	  (cond ((eq package *keyword-package*)
		 (add-symbol (package-external-symbols package) symbol)
		 (%set-symbol-value symbol symbol))
		(t
		 (add-symbol (package-internal-symbols package) symbol)))
	  (values symbol nil)))))

;;; Check internal and external symbols, then scan down the list
;;; of hashtables for inherited symbols. When an inherited symbol
;;; is found pull that table to the beginning of the list.
(defun find-symbol* (string length package)
  (declare (simple-string string)
	   (type index length))
  (let* ((hash (%sxhash-simple-substring string length))
	 (ehash (entry-hash length hash)))
    (declare (type index hash ehash))
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
	    (unless (eq prev head)
	      (shiftf (cdr prev) (cdr table) (cdr head) table))
	    (return-from find-symbol* (values symbol :inherited))))))))

;;; Similar to Find-Symbol, but only looks for an external symbol.
;;; This is used for fast name-conflict checking in this file and symbol
;;; printing in the printer.
(defun find-external-symbol (string package)
  (declare (simple-string string))
  (let* ((length (length string))
	 (hash (%sxhash-simple-string string))
	 (ehash (entry-hash length hash)))
    (declare (type index length hash))
    (with-symbol (found symbol (package-external-symbols package)
			string length hash ehash)
      (values symbol found))))

;;; If we are uninterning a shadowing symbol, then a name conflict can
;;; result, otherwise just nuke the symbol.
(defun unintern (symbol &optional (package (sane-package)))
  #!+sb-doc
  "Makes Symbol no longer present in Package. If Symbol was present
  then T is returned, otherwise NIL. If Package is Symbol's home
  package, then it is made uninterned."
  (let* ((package (find-undeleted-package-or-lose package))
	 (name (symbol-name symbol))
	 (shadowing-symbols (package-%shadowing-symbols package)))
    (declare (list shadowing-symbols))

    ;; If a name conflict is revealed, give use a chance to shadowing-import
    ;; one of the accessible symbols.
    (when (member symbol shadowing-symbols)
      (let ((cset ()))
	(dolist (p (package-%use-list package))
	  (multiple-value-bind (s w) (find-external-symbol name p)
	    (when w (pushnew s cset))))
	(when (cdr cset)
	  (loop
	   (cerror
	    "Prompt for a symbol to SHADOWING-IMPORT."
	    "Uninterning symbol ~S causes name conflict among these symbols:~%~S"
	    symbol cset)
	   (write-string "Symbol to shadowing-import: " *query-io*)
	   (let ((sym (read *query-io*)))
	     (cond
	      ((not (symbolp sym))
	       (format *query-io* "~S is not a symbol."))
	      ((not (member sym cset))
	       (format *query-io* "~S is not one of the conflicting symbols."))
	      (t
	       (shadowing-import sym package)
	       (return-from unintern t)))))))
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
	    (t nil)))))

;;; Take a symbol-or-list-of-symbols and return a list, checking types.
(defun symbol-listify (thing)
  (cond ((listp thing)
	 (dolist (s thing)
	   (unless (symbolp s) (error "~S is not a symbol." s)))
	 thing)
	((symbolp thing) (list thing))
	(t
	 (error "~S is neither a symbol nor a list of symbols." thing))))

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
  "Exports Symbols from Package, checking that no name conflicts result."
  (let ((package (find-undeleted-package-or-lose package))
	(syms ()))
    ;; Punt any symbols that are already external.
    (dolist (sym (symbol-listify symbols))
      (multiple-value-bind (s w)
	  (find-external-symbol (symbol-name sym) package)
	(declare (ignore s))
	(unless (or w (member sym syms))
	  (push sym syms))))
    ;; Find symbols and packages with conflicts.
    (let ((used-by (package-%used-by-list package))
	  (cpackages ())
	  (cset ()))
      (dolist (sym syms)
	(let ((name (symbol-name sym)))
	  (dolist (p used-by)
	    (multiple-value-bind (s w) (find-symbol name p)
	      (when (and w (not (eq s sym))
			 (not (member s (package-%shadowing-symbols p))))
		(pushnew sym cset)
		(pushnew p cpackages))))))
      (when cset
	(restart-case
	    (error
	     'simple-package-error
	     :package package
	     :format-control
	     "Exporting these symbols from the ~A package:~%~S~%~
	      results in name conflicts with these packages:~%~{~A ~}"
	     :format-arguments
	     (list (package-%name package) cset
		   (mapcar #'package-%name cpackages)))
	  (unintern-conflicting-symbols ()
	   :report "Unintern conflicting symbols."
	   (dolist (p cpackages)
	     (dolist (sym cset)
	       (moby-unintern sym p))))
	  (skip-exporting-these-symbols ()
	   :report "Skip exporting conflicting symbols."
	   (setq syms (nset-difference syms cset))))))

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
	(with-simple-restart
	    (continue "Import these symbols into the ~A package."
	      (package-%name package))
	  (error 'simple-package-error
		 :package package
		 :format-control
		 "These symbols are not accessible in the ~A package:~%~S"
		 :format-arguments
		 (list (package-%name package) missing)))
	(import missing package))
      (import imports package))

    ;; And now, three pages later, we export the suckers.
    (let ((internal (package-internal-symbols package))
	  (external (package-external-symbols package)))
      (dolist (sym syms)
	(nuke-symbol internal (symbol-name sym))
	(add-symbol external sym)))
    t))

;;; Check that all symbols are accessible, then move from external to internal.
(defun unexport (symbols &optional (package (sane-package)))
  #!+sb-doc
  "Makes Symbols no longer exported from Package."
  (let ((package (find-undeleted-package-or-lose package))
	(syms ()))
    (dolist (sym (symbol-listify symbols))
      (multiple-value-bind (s w) (find-symbol (symbol-name sym) package)
	(cond ((or (not w) (not (eq s sym)))
	       (error 'simple-package-error
		      :package package
		      :format-control "~S is not accessible in the ~A package."
		      :format-arguments (list sym (package-%name package))))
	      ((eq w :external) (pushnew sym syms)))))

    (let ((internal (package-internal-symbols package))
	  (external (package-external-symbols package)))
      (dolist (sym syms)
	(add-symbol internal sym)
	(nuke-symbol external (symbol-name sym))))
    t))

;;; Check for name conflict caused by the import and let the user
;;; shadowing-import if there is.
(defun import (symbols &optional (package (sane-package)))
  #!+sb-doc
  "Make Symbols accessible as internal symbols in Package. If a symbol
  is already accessible then it has no effect. If a name conflict
  would result from the importation, then a correctable error is signalled."
  (let ((package (find-undeleted-package-or-lose package))
	(symbols (symbol-listify symbols))
	(syms ())
	(cset ()))
    (dolist (sym symbols)
      (multiple-value-bind (s w) (find-symbol (symbol-name sym) package)
	(cond ((not w)
	       (let ((found (member sym syms :test #'string=)))
		 (if found
		     (when (not (eq (car found) sym))
		       (push sym cset))
		     (push sym syms))))
	      ((not (eq s sym)) (push sym cset))
	      ((eq w :inherited) (push sym syms)))))
    (when cset
      ;; ANSI specifies that this error is correctable.
      (with-simple-restart
	  (continue "Import these symbols with Shadowing-Import.")
	(error 'simple-package-error
	       :package package
	       :format-control
	       "Importing these symbols into the ~A package ~
		causes a name conflict:~%~S"
	       :format-arguments (list (package-%name package) cset))))
    ;; Add the new symbols to the internal hashtable.
    (let ((internal (package-internal-symbols package)))
      (dolist (sym syms)
	(add-symbol internal sym)))
    ;; If any of the symbols are uninterned, make them be owned by Package.
    (dolist (sym symbols)
      (unless (symbol-package sym) (%set-symbol-package sym package)))
    (shadowing-import cset package)))

;;; If a conflicting symbol is present, unintern it, otherwise just
;;; stick the symbol in.
(defun shadowing-import (symbols &optional (package (sane-package)))
  #!+sb-doc
  "Import Symbols into package, disregarding any name conflict. If
  a symbol of the same name is present, then it is uninterned.
  The symbols are added to the Package-Shadowing-Symbols."
  (let* ((package (find-undeleted-package-or-lose package))
	 (internal (package-internal-symbols package)))
    (dolist (sym (symbol-listify symbols))
      (multiple-value-bind (s w) (find-symbol (symbol-name sym) package)
	(unless (and w (not (eq w :inherited)) (eq s sym))
	  (when (or (eq w :internal) (eq w :external))
	    ;; If it was shadowed, we don't want UNINTERN to flame out...
	    (setf (package-%shadowing-symbols package)
		  (remove s (the list (package-%shadowing-symbols package))))
	    (unintern s package))
	  (add-symbol internal sym))
	(pushnew sym (package-%shadowing-symbols package)))))
  t)

(defun shadow (symbols &optional (package (sane-package)))
  #!+sb-doc
  "Make an internal symbol in Package with the same name as each of the
  specified symbols, adding the new symbols to the Package-Shadowing-Symbols.
  If a symbol with the given name is already present in Package, then
  the existing symbol is placed in the shadowing symbols list if it is
  not already present."
  (let* ((package (find-undeleted-package-or-lose package))
	 (internal (package-internal-symbols package)))
    (dolist (name (mapcar #'string
			  (if (listp symbols) symbols (list symbols))))
      (multiple-value-bind (s w) (find-symbol name package)
	(when (or (not w) (eq w :inherited))
	  (setq s (make-symbol name))
	  (%set-symbol-package s package)
	  (add-symbol internal s))
	(pushnew s (package-%shadowing-symbols package)))))
  t)

;;; Do stuff to use a package, with all kinds of fun name-conflict checking.
(defun use-package (packages-to-use &optional (package (sane-package)))
  #!+sb-doc
  "Add all the Packages-To-Use to the use list for Package so that
  the external symbols of the used packages are accessible as internal
  symbols in Package."
  (let ((packages (package-listify packages-to-use))
	(package (find-undeleted-package-or-lose package)))

    ;; Loop over each package, USE'ing one at a time...
    (dolist (pkg packages)
      (unless (member pkg (package-%use-list package))
	(let ((cset ())
	      (shadowing-symbols (package-%shadowing-symbols package))
	      (use-list (package-%use-list package)))

	  ;;   If the number of symbols already accessible is less than the
	  ;; number to be inherited then it is faster to run the test the
	  ;; other way. This is particularly valuable in the case of
	  ;; a new package USEing Lisp.
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
		(when (and w (not (eq s sym))
			   (not (member sym shadowing-symbols)))
		  (push sym cset))))
	    (dolist (p use-list)
	      (do-external-symbols (sym p)
		(multiple-value-bind (s w)
		    (find-external-symbol (symbol-name sym) pkg)
		  (when (and w (not (eq s sym))
			     (not (member (find-symbol (symbol-name sym)
						       package)
					  shadowing-symbols)))
		    (push sym cset))))))
	   (t
	    (do-external-symbols (sym pkg)
	      (multiple-value-bind (s w)
		  (find-symbol (symbol-name sym) package)
		(when (and w (not (eq s sym))
			   (not (member s shadowing-symbols)))
		  (push s cset))))))

	  (when cset
	    (cerror
	     "Unintern the conflicting symbols in the ~2*~A package."
	     "Use'ing package ~A results in name conflicts for these symbols:~%~S"
	     (package-%name pkg) cset (package-%name package))
	    (dolist (s cset) (moby-unintern s package))))

	(push pkg (package-%use-list package))
	(push (package-external-symbols pkg) (cdr (package-tables package)))
	(push package (package-%used-by-list pkg)))))
  t)

(defun unuse-package (packages-to-unuse &optional (package (sane-package)))
  #!+sb-doc
  "Remove PACKAGES-TO-UNUSE from the USE list for PACKAGE."
  (let ((package (find-undeleted-package-or-lose package)))
    (dolist (p (package-listify packages-to-unuse))
      (setf (package-%use-list package)
	    (remove p (the list (package-%use-list package))))
      (setf (package-tables package)
	    (delete (package-external-symbols p)
		    (the list (package-tables package))))
      (setf (package-%used-by-list p)
	    (remove package (the list (package-%used-by-list p)))))
    t))

(defun find-all-symbols (string-or-symbol)
  #!+sb-doc
  "Return a list of all symbols in the system having the specified name."
  (let ((string (string string-or-symbol))
	(res ()))
    (maphash (lambda (k v)
	       (declare (ignore k))
	       (multiple-value-bind (s w) (find-symbol string v)
		 (when w (pushnew s res))))
	     *package-names*)
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
	    (string (stringify-name string-designator "APROPOS search"))
	    (result nil))
	(do-symbols (symbol package)
	  (when (and (eq (symbol-package symbol) package)
		     (or (not external-only)
			 (eq (find-symbol (symbol-name symbol) package)
			     :external))
		     (search string (symbol-name symbol) :test #'char-equal))
	    (push symbol result)))
	result)
      (mapcan (lambda (package)
		(apropos-list string-designator package external-only))
	      (list-all-packages))))

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
      (setf (package-%shadowing-symbols pkg) (sixth spec))))

  ;; FIXME: These assignments are also done at toplevel in
  ;; boot-extensions.lisp. They should probably only be done once.
  (/show0 "setting up *CL-PACKAGE* and *KEYWORD-PACKAGE*")
  (setq *cl-package* (find-package "COMMON-LISP"))
  (setq *keyword-package* (find-package "KEYWORD"))

  (/show0 "about to MAKUNBOUND *!INITIAL-SYMBOLS*")
  (makunbound '*!initial-symbols*)       ; (so that it gets GCed)

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
