;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!EVAL")

(file-comment
  "$Header$")

;;; This flag is used by EVAL-WHEN to keep track of when code has already been
;;; evaluated so that it can avoid multiple evaluation of nested EVAL-WHEN
;;; (COMPILE)s.
(defvar *already-evaled-this* nil)
