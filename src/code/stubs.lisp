;;;; miscellaneous primitive stubs (ordinary FDEFINITIONs for full
;;;; call defined in terms of fundamental definitions of inline
;;;; expansions)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(macrolet ((def-frob (name &optional (args '(x)))
	     `(defun ,name ,args (,name ,@args))))
  (def-frob %code-code-size)
  (def-frob %code-debug-info)
  (def-frob %code-entry-points)
  (def-frob %funcallable-instance-function)
  (def-frob %funcallable-instance-layout)
  (def-frob %funcallable-instance-lexenv)
  (def-frob %function-next)
  (def-frob %function-self)
  (def-frob %set-funcallable-instance-function (fin new-val)))
