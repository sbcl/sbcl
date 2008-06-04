;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(macrolet ((def (name value &optional doc)
             (declare (ignorable doc))
             `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
                #!+sb-doc
                ,@(when doc (list doc)))))
  (def sb!xc:lambda-list-keywords
      '(&allow-other-keys
        &aux
        &body
        &environment
        &key
        &more
        &optional
        &rest
        &whole)
    "A list of symbols used as lambda list keywords in SBCL."))
