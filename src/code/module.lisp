;;;; REQUIRE, PROVIDE, and friends
;;;;
;;;; Note that this module file is based on the old system, and is being
;;;; spliced into the current sources to reflect the last minute deprecated
;;;; addition of modules to the X3J13 ANSI standard.
;;;;
;;;; FIXME: This implementation has cruft not required by the ANSI
;;;; spec, notably DEFMODULE. We should probably minimize it, since
;;;; it's deprecated anyway.

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

;;;; DEFMODULE
;;;; FIXME: Remove this.

(defvar *module-file-translations* (make-hash-table :test 'equal))
(defmacro defmodule (name &rest files)
  #!+sb-doc
  "Defines a module by registering the files that need to be loaded when
   the module is required. If name is a symbol, its print name is used
   after downcasing it."
  `(%define-module ,name ',files))

(defun %define-module (name files)
  (setf (gethash (module-name-string name) *module-file-translations*)
	files))

(defun module-files (name)
  (gethash name *module-file-translations*))

;;;; PROVIDE and REQUIRE

(defun provide (module-name)
  #!+sb-doc
  "Adds a new module name to *MODULES* indicating that it has been loaded.
   Module-name may be either a case-sensitive string or a symbol; if it is
   a symbol, its print name is downcased and used."
  (pushnew (module-name-string module-name) *modules* :test #'string=)
  t)

(defun require (module-name &optional pathname)
  #!+sb-doc
  "Loads a module when it has not been already. PATHNAME, if supplied,
   is a single pathname or list of pathnames to be loaded if the module
   needs to be. If PATHNAME is not supplied, then a list of files are
   looked for that were registered by a DEFMODULE form. If the module
   has not been defined, then a file will be loaded whose name is formed
   by merging \"modules:\" and MODULE-NAME (downcased if it is a symbol).
   This merged name will be probed with both a .lisp extension and any
   architecture-specific FASL extensions, and LOAD will be called on it
   if it is found."
  ;; KLUDGE: Does this really match the doc string any more? (Did it ever
  ;; match the doc string? Arguably this isn't a high priority question
  ;; since REQUIRE is deprecated anyway and I've not been very motivated
  ;; to maintain CMU CL extensions like DEFMODULE.. -- WHN 19990804
  (setf module-name
	(module-name-string module-name))
  (unless (member module-name *modules* :test #'string=)
    (if pathname
      (unless (listp pathname) (setf pathname (list pathname)))
      (let ((files (module-files module-name)))
	(if files
	  (setf pathname files)
	  (setf pathname (list (merge-pathnames "modules:" module-name))))))
    (dolist (ele pathname t)
      (load ele))))

;;;; miscellany

(defun module-name-string (name)
  (typecase name
    (string name)
    (symbol (string-downcase (symbol-name name)))
    (t (error 'simple-type-error
	      :datum name
	      :expected-type '(or string symbol)
	      :format-control "Module name must be a string or symbol: ~S"
	      :format-arguments (list name)))))
