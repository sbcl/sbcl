;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(file-comment
  "$Header$")

(defmacro-mundanely destructuring-bind (lambda-list arg-list &rest body)
  #!+sb-doc
  "Bind the variables in LAMBDA-LIST to the contents of ARG-LIST."
  (let* ((arg-list-name (gensym "ARG-LIST-")))
    (multiple-value-bind (body local-decls)
	(parse-defmacro lambda-list arg-list-name body nil 'destructuring-bind
			:anonymousp t
			:doc-string-allowed nil)
      `(let ((,arg-list-name ,arg-list))
	 ,@local-decls
	 ,body))))
