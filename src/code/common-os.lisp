;;;; OS interface functions for SBCL common to all target OSes

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!SYS")

(defvar *software-version* nil)

(defvar *core-pathname* nil
  #!+sb-doc
  "The absolute pathname of the running SBCL core.")

;;; if something ever needs to be done differently for one OS, then
;;; split out the different part into per-os functions.
(defun os-cold-init-or-reinit ()
  (/show0 "entering OS-COLD-INIT-OR-REINIT")
  (setf *software-version* nil)
  (/show0 "setting *DEFAULT-PATHNAME-DEFAULTS*")
  (setf *default-pathname-defaults*
        ;; (temporary value, so that #'NATIVE-PATHNAME won't blow up when
        ;; we call it below:)
        (make-trivial-default-pathname)
        *default-pathname-defaults*
        ;; (final value, constructed using #'NATIVE-PATHNAME:)
        (native-pathname (sb!unix:posix-getcwd/)))
  (/show0 "setting *CORE-PATHNAME*")
  (setf *core-pathname*
        (merge-pathnames (native-pathname *core-string*)))
  (/show0 "leaving OS-COLD-INIT-OR-REINIT"))
