;;;; late happenning functionality for PROCLAIM.  We run through
;;;; queued-up type and ftype proclaims that were made before the type
;;;; system was initialized, and (since it is now initalized)
;;;; reproclaim them.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

(!begin-collecting-cold-init-forms)

(!cold-init-forms (aver *type-system-initialized*))
(!cold-init-forms (mapcar #'sb!xc:proclaim *queued-proclaims*))
;;; We only need this once, then it's set up for good.  We keep it
;;; around in the cross-compiler mostly so that we can inspect its
;;; value.
#-sb-xc-host
(!cold-init-forms (makunbound '*queued-proclaims*))

(!defun-from-collected-cold-init-forms !late-proclaim-cold-init)
