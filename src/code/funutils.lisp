;;;; miscellaneous operations on functions, returning functions, or
;;;; primarily useful for functional programming

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(defun identity (thing)
  #!+sb-doc
  "This function simply returns what was passed to it."
  thing)

(defun complement (function)
  #!+sb-doc
  "Return a new function that returns T whenever FUNCTION returns NIL and
   NIL whenever FUNCTION returns non-NIL."
  (lambda (&optional (arg0 nil arg0-p) (arg1 nil arg1-p) (arg2 nil arg2-p)
                     &rest more-args)
    (not (cond (more-args (apply function arg0 arg1 arg2 more-args))
               (arg2-p (funcall function arg0 arg1 arg2))
               (arg1-p (funcall function arg0 arg1))
               (arg0-p (funcall function arg0))
               (t (funcall function))))))

(defun constantly (value)
  #!+sb-doc
  "Return a function that always returns VALUE."
  (lambda ()
    ;; KLUDGE: This declaration is a hack to make the closure ignore
    ;; all its arguments without consing a &REST list or anything.
    ;; Perhaps once DYNAMIC-EXTENT is implemented we won't need to
    ;; screw around with this kind of thing. -- WHN 2001-04-06
    (declare (optimize (speed 3) (safety 0)))
    value))
