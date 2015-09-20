;;;; cross-compile-time-only replacements for threading stuff

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!THREAD")

(defmacro with-mutex ((mutex) &body body)
  (declare (ignore mutex))
  `(locally ,@body))

(defmacro with-recursive-lock ((mutex) &body body)
  (declare (ignore mutex))
  `(locally ,@body))

(defmacro barrier ((kind) &body body)
  (declare (ignore kind))
  `(progn ,@body))

(defun thread-yield () nil)
