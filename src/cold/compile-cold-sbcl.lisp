;;;; Compile the fundamental system sources (not CLOS, and possibly
;;;; not some other warm-load-only stuff like DESCRIBE) to produce
;;;; object files. Also set *TARGET-OBJECT-FILES* to all of their
;;;; names.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-COLD")

(defvar *target-object-file-names*)

;;; KLUDGE..
;;;
;;; CMU CL (as of 2.4.6 for Debian, anyway) issues warnings (and not just
;;; STYLE-WARNINGs, either, alas) when it tries to interpret code containing
;;; references to undefined functions. The most common problem is that
;;; macroexpanded code refers to this function, which isn't defined until late.
;;;
;;; This
;;;   #+cmu (defun sb!kernel::do-arg-count-error (&rest rest)
;;;	   (error "stub version of do-arg-count-error, rest=~S" rest))
;;; doesn't work, with or without this
;;;   (compile 'sb!kernel::do-arg-count-error))
;;; so perhaps I should try
;;;   (declaim (ftype ..) ..)
;;; instead?
(declaim (ftype (function (&rest t) nil) sb!kernel::do-arg-count-error))

(let ((reversed-target-object-file-names nil))
  (for-stems-and-flags (stem flags)
    (unless (find :not-target flags)
      ;; FIXME: Remove these GC calls after fixing the problem of ridiculous
      ;; bootstrap memory bloat.
      (push (target-compile-stem stem
				 :assem-p (find :assem flags)
				 :ignore-failure-p (find :ignore-failure-p
							 flags))
	    reversed-target-object-file-names)
      #!+sb-show (warn-when-cl-snapshot-diff *cl-snapshot*)))
  (setf *target-object-file-names*
	(nreverse reversed-target-object-file-names)))
