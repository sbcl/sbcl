;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(def!macro sb!xc:deftype (name arglist &body body)
  #!+sb-doc
  "Define a new type, with syntax like DEFMACRO."
  (unless (symbolp name)
    (error "type name not a symbol: ~S" name))
  (with-unique-names (whole)
    (multiple-value-bind (body local-decs doc)
	(parse-defmacro arglist whole body name 'deftype :default-default ''*)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (%compiler-deftype ',name
			    (lambda (,whole)
			      ,@local-decs
			      ,body)
			    ,@(when doc `(,doc)))))))
