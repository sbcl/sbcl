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

;;; One possible use-case for slam.sh is to generate a trace-file for
;;; a file that is suddenly of interest, but was not of interest
;;; before.  In order for this to work, we need to reload the stems
;;; and flags from build-order.lisp-expr, the user needs to have added
;;; :trace-file as a flag.
(setf *stems-and-flags* (read-from-file "build-order.lisp-expr"))

(do-stems-and-flags (stem flags)
  (unless (position :not-target flags)
    (let ((srcname (stem-source-path stem))
          (objname (stem-object-path stem flags :target-compile)))
      (unless (and (output-up-to-date-wrt-input-p objname srcname)
                   ;; Back to our "new-trace-file" case, also build if
                   ;; a trace file is desired but is out-of-date.
                   (or (not (position :trace-file flags))
                       (output-up-to-date-wrt-input-p
                        (concatenate 'string (stem-remap-target stem)
                                     ".trace")
                        srcname)))
        (target-compile-stem stem flags)))))
