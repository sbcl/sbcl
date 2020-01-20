;;;; patches to work around implementation idiosyncrasies in our
;;;; cross-compilation host

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;;; CMU CL issues
#+cmu
(progn
  (setq *compile-print* nil) ; too much noise, can't see the actual warnings
)

;;; This is apparently quite old, according to
;;; <http://tunes.org/~nef/logs/lisp/03.10.22>:
;;;   <dan`b> (error "CMUCL on Alpha can't read floats in the format \"1.0l0\".
;;;   <dan`b> the warning relates to a random vinary produced from cvs of
;;;           around feb 2000, the corresponding sources to which I never found
;;; (But it seems harmless to leave it here forever just in case.)
#+(and cmu alpha)
(unless (ignore-errors (read-from-string "1.0l0"))
  (error "CMUCL on Alpha can't read floats in the format \"1.0l0\".  Patch your core file~%~%"))

#+(and cmu sparc)
(ext:set-floating-point-modes :traps '(:overflow :invalid :divide-by-zero))

