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

(sb!alien:define-alien-variable ("posix_argv" *native-posix-argv*) (* (* char)))
(sb!alien:define-alien-variable ("core_string" *native-core-string*) (* char))
(sb!alien:define-alien-routine
 os-get-runtime-executable-path sb!alien:c-string (external-path boolean))
(sb!alien:define-alien-variable
 ("saved_runtime_path" *native-saved-runtime-path*) (* char))

;;; if something ever needs to be done differently for one OS, then
;;; split out the different part into per-os functions.
(defun os-cold-init-or-reinit ()
  (/show0 "setting *CORE-STRING*")
  (setf *core-string*
        (sb!alien:cast *native-core-string* sb!alien:c-string))
  (/show0 "setting *POSIX-ARGV*")
  (setf sb!ext:*posix-argv*
        (loop for i from 0
              for arg = (sb!alien:deref *native-posix-argv* i)
              until (sb!alien:null-alien arg)
              collect (sb!alien:cast arg sb!alien:c-string)))
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
  (/show0 "setting *RUNTIME-PATHNAME*")
  (let ((exe (os-get-runtime-executable-path t))
        (saved (sb!alien:cast *native-saved-runtime-path* sb!alien:c-string)))
    (setf *runtime-pathname*
          (when (or exe saved) (native-pathname (or exe saved)))))
  (/show0 "leaving OS-COLD-INIT-OR-REINIT"))
