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

(let ((reversed-target-object-file-names nil))
  (do-stems-and-flags (stem flags)
    (unless (position :not-target flags)
      (push (target-compile-stem stem flags)
            reversed-target-object-file-names)
      #!+sb-show (warn-when-cl-snapshot-diff *cl-snapshot*)))
  (setf *target-object-file-names*
        (nreverse reversed-target-object-file-names)))
