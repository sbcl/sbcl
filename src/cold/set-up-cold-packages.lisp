;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-COLD")

;;; an entry in the table which describes the non-standard part (i.e. not
;;; CL/CL-USER/KEYWORD) of the package structure of the SBCL system
;;;
;;; We make no attempt to be fully general; our table doesn't need to be
;;; able to express features which we don't happen to use.
(export '(package-data
	  package-data-name
	  package-data-nicknames
	  package-data-export
	  package-data-reexport
	  package-data-import-from
	  package-data-use))
(defstruct package-data
  ;; a string designator for the package name
  (name (error "missing PACKAGE-DATA-NAME datum"))
  ;; a doc string
  (doc (error "missing PACKAGE-DOC datum"))
  ;; a list of string designators for package nicknames
  nicknames
  ;; a tree containing names for exported symbols which'll be set up at package
  ;; creation time, and NILs, which are ignored. (This is a tree in order to
  ;; allow constructs like '("ENOSPC" #!+LINUX ("EDQUOT" "EISNAM" "ENAVAIL"
  ;; "EREMOTEIO")) to be used in initialization. NIL entries in the tree are
  ;; ignored for the same reason of notational convenience.)
  export
  ;; a list of string designators for exported symbols which don't necessarily
  ;; originate in this package (so their EXPORT operations should be handled
  ;; after USE operations have been done, so that duplicates aren't created)
  reexport
  ;; a list of sublists describing imports. Each sublist has the format as an
  ;; IMPORT-FROM list in DEFPACKAGE: the first element is the name of the
  ;; package to import from, and the remaining elements are the names of
  ;; symbols to import.
  import-from
  ;; a tree of string designators for package names of other packages
  ;; which this package uses
  use)

(let ((package-data-list (read-from-file "package-data-list.lisp-expr")))

    ;; Build all packages that we need, and initialize them as far as we
    ;; can without referring to any other packages.
    (dolist (package-data package-data-list)
      (let* ((package (make-package
		       (package-data-name package-data)
		       :nicknames (package-data-nicknames package-data)
		       :use nil)))
	#!+sb-doc (setf (documentation package t)
			(package-data-doc package-data))
	;; Walk the tree of exported names, exporting each name.
	(labels ((recurse (tree)
		   (etypecase tree
		     ;; FIXME: The comments above say the structure is a tree,
		     ;; but here we're sleazily treating it as though
		     ;; dotted lists never occur. Replace this LIST case
		     ;; with separate NULL and CONS cases to fix this.
		     (list (mapc #'recurse tree))
		     (string (export (intern tree package) package)))))
	  (recurse (package-data-export package-data)))))

    ;; Now that all packages exist, we can set up package-package
    ;; references.
    (dolist (package-data package-data-list)
      (use-package (package-data-use package-data)
		   (package-data-name package-data))
      (dolist (sublist (package-data-import-from package-data))
	(let* ((from-package (first sublist))
	       (symbol-names (rest sublist))
	       (symbols (mapcar (lambda (name)
				  ;; old way, broke for importing symbols
				  ;; like SB!C::DEBUG-SOURCE-FORM into
				  ;; SB!DI -- WHN 19990714
				  #+nil
				  (let ((s (find-symbol name from-package)))
				    (unless s
				      (error "can't find ~S in ~S"
					     name
					     from-package))
				    s)
				  ;; new way, works for SB!DI stuff
				  ;; -- WHN 19990714
				  (intern name from-package))
				symbol-names)))
	  (import symbols (package-data-name package-data)))))

    ;; Now that all package-package references exist, we can handle
    ;; REEXPORT operations. (We have to wait until now because they
    ;; interact with USE operations.) KLUDGE: This code doesn't detect
    ;; dependencies and do exports in proper order to work around them, so
    ;; it could break randomly (with build-time errors, not with silent
    ;; errors or runtime errors) if multiple levels of re-exportation are
    ;; used, e.g. package A exports X, package B uses A and reexports X,
    ;; and package C uses B and reexports X. That doesn't seem to be an
    ;; issue in the current code, and it's hard to see why anyone would
    ;; want to do it, and it should be straightforward (though tedious) to
    ;; extend the code here to deal with that if it ever becomes necessary.
    (dolist (package-data package-data-list)
      (let ((package (find-package (package-data-name package-data))))
	(dolist (symbol-name (package-data-reexport package-data))
	  (multiple-value-bind (symbol status)
	      (find-symbol symbol-name package)
	    (unless status
	      (error "No symbol named ~S is accessible in ~S."
		     symbol-name
		     package))
	    (when (eq (symbol-package symbol) package)
	      (error "~S is not inherited/imported, but native to ~S."
		     symbol-name
		     package))
	    (export symbol package))))))
