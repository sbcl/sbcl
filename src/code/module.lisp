;;;; REQUIRE, PROVIDE, and friends
;;;;
;;;; Officially these are deprecated, but in practice they're probably
;;;; even less likely to actually go away than there is to ever be
;;;; another revision of the standard.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;;; exported specials

(defvar *modules* ()
  #!+sb-doc
  "This is a list of module names that have been loaded into Lisp so far.
   It is used by PROVIDE and REQUIRE.")

(defvar sb!ext::*MODULE-PROVIDER-FUNCTIONS* '(module-provide-contrib)
  "See function documentation for REQUIRE")


;;;; PROVIDE and REQUIRE

(defun provide (module-name)
  #!+sb-doc
  "Adds a new module name to *MODULES* indicating that it has been loaded.
   Module-name is a string designator"
  (pushnew (string module-name) *modules* :test #'string=)
  t)

(defun require (module-name &optional pathnames)
  #!+sb-doc
  "Loads a module, unless it already has been loaded. PATHNAMES, if supplied,
   is a designator for a list of pathnames to be loaded if the module
   needs to be. If PATHNAMES is not supplied, functions from the list
   *MODULE-PROVIDER-FUNCTIONS* are called in order with MODULE-NAME
   as an argument, until one of them returns non-NIL."
  (unless (member (string module-name) *modules* :test #'string=)
    (cond (pathnames
	   (unless (listp pathnames) (setf pathnames (list pathnames)))
	   ;; ambiguity in standard: should we try all pathnames in the
	   ;; list, or should we stop as soon as one of them calls PROVIDE?
	   (dolist (ele pathnames t)
	     (load ele)))
	  (t
	   (unless (some (lambda (p) (funcall p module-name))
			 sb!ext::*module-provider-functions*)
	     (error "Don't know how to load ~A" module-name))))))


;;;; miscellany

(defun module-provide-contrib (name)
  "Stringify and downcase NAME if it is a symbol, then attempt to load
   the file $SBCL_HOME/name/name"
  (let ((name (if (symbolp name) (string-downcase (symbol-name name)) name)))
    (load
     (merge-pathnames (make-pathname :directory (list :relative name)
				     :name name)
		      (truename (posix-getenv "SBCL_HOME")))))
  (provide name))


