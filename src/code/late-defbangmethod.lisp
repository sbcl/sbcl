;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;; DEF!METHOD = cold DEFMETHOD, a version of DEFMETHOD which, when used
;;; before real CLOS DEFMETHOD is available, saves up its definition to be
;;; executed later when CLOS is available
(defmacro-mundanely def!method (&rest args)
  `(push (cons (sb!c:source-location) ',args) *delayed-def!method-args*))
