(in-package :sb!thread)

(defun make-mutex (&key name value) nil)

(defmacro with-recursive-lock ((mutex) &body body)
  `(progn ,@body))

