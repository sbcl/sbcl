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

;;;; CLISP issues

;;; as explained on #lisp ca. October 2003:
;;;   <Krystof> chandler: nope, I'm blaming another clisp bug
;;;   <Krystof> [8]> least-positive-short-float
;;;   <Krystof> 2.93874s-39
;;;   <Krystof> [9]> (coerce * 'single-float)
;;;   <Krystof> 0.0
;;;   <chandler> aah
;;;   <mwh> "oops"
;;;   <Krystof> yep
;;;   <mwh> tried that on clisp from fink:
;;;   <mwh> [1]> least-positive-short-float
;;;   <mwh> 2.93874s-39
;;;   <mwh> [2]> (coerce * 'single-float)
;;;   <mwh> *** - floating point underflow
;;;   <Krystof> yeah
;;;   <mwh> shall i not try to build sbcl with that?
;;;   <Krystof> if you turn off underflow traps, then you get 0.0
;;;   <mwh> well, that makes sense, i guess
;;;   <Krystof> #+clisp
;;;   <Krystof> (ext:without-package-lock ("SYSTEM")
;;;   <Krystof>   (setf system::*inhibit-floating-point-underflow* t))
;;;   <Krystof> (in src/cold/ansify.lisp)
#+clisp
(ext:without-package-lock ("SYSTEM")
  (setf system::*inhibit-floating-point-underflow* t))

;;;; CMU CL issues

;;; CMU CL, at least as of 18b, doesn't support PRINT-OBJECT. In
;;; particular, it refuses to compile :PRINT-OBJECT options to
;;; DEFSTRUCT, so we need to conditionalize such options on the
;;; :NO-ANSI-PRINT-OBJECT feature in order to get the code to compile.
;;; (It also fails to do anything useful with DEFMETHOD PRINT-OBJECT,
;;; but that doesn't matter much, since it doesn't stop the
;;; cross-compiler from working.)
#+cmu
(progn
  (setq *compile-print* nil) ; too much noise, can't see the actual warnings
  ;; #'IN-HOST-COMPILATION-MODE will push :NO-ANSI-PRINT-OBJECT into SB-XC:*FEATURES*
  (warn "CMU CL doesn't support the :PRINT-OBJECT option to DEFSTRUCT.~%"))

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

;;;; OpenMCL issues

;;; This issue in OpenMCL led to some SBCL bug reports ca. late 2003.
#+openmcl
(unless (ignore-errors (funcall (constantly t) 1 2 3))
  (error "please find a binary that understands CONSTANTLY to build from"))
