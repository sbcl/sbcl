;;;; crude selective re-cross-compilation of the target system, like
;;;; Unix make(1), but much flakier because we don't keep track of the
;;;; (many!) dependencies between files

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package :sb-cold)

;;; (This file is intended to be loaded into an after-xc.lisp core, so
;;; we don't need to LOAD any machinery (e.g. "src/cold/shared.lisp")
;;; which was already loaded in the course of setting up the system
;;; state which was frozen into that core.)

;;; basic test for up-to-date-ness of output with respect to input in
;;; the sense of Unix make(1)
(defun output-up-to-date-wrt-input-p (output input)
  (and (probe-file output)
       ;; (Strict #'> and lax #'>= each have problems here, which
       ;; could become more noticeable as computation speed
       ;; accelerates while Common Lisp's 1-second granularity remains
       ;; the same. We use #'> because it's safer sometimes to
       ;; recompile unnecessarily than sometimes bogusly to assume
       ;; up-to-date-ness.)
       (> (file-write-date output)
	  (file-write-date input))))

(do-stems-and-flags (stem flags)
  (unless (position :not-target flags)
    (let ((srcname (concatenate 'string stem ".lisp"))
	  (objname (concatenate 'string
				*target-obj-prefix*
				stem
				*target-obj-suffix*)))
      (unless (output-up-to-date-wrt-input-p objname srcname)
	(let (
	      (*more-compile-file-flags* 
	       (if (position :trace-file flags)
		   '(:trace-file "/tmp/out.trace") nil)))
	  (target-compile-stem stem))))))
