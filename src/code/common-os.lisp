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

(sb!alien:define-alien-variable ("posix_argv" *native-posix-argv*) (* (* char)))
(sb!alien:define-alien-variable ("core_string" *native-core-string*) (* char))
(sb!alien:define-alien-routine
 os-get-runtime-executable-path sb!alien:c-string (external-path boolean))
(sb!alien:define-alien-variable
 ("saved_runtime_path" *native-saved-runtime-path*) (* char))

(defmacro init-var-ignoring-errors (variable
                                    form
                                    &key default
                                         explanation
                                         (condition 'error))
  `(setf ,variable
         (handler-case ,form
           (,condition (c)
             (let ((default ,default))
               (warn "Error initializing ~a~@[ ~a~]:~@
             ~a
             ~% Using ~s instead."
                     ',variable
                     ,explanation
                     c
                     default)
               default)))))

;;; If something ever needs to be done differently for one OS, then
;;; split out the different part into per-os functions.

(defun os-deinit ()
  (setf *default-pathname-defaults* (make-trivial-default-pathname)
        *core-string* ""
        *software-version* nil
        *runtime-pathname* nil
        *core-pathname* nil
        *posix-argv* nil)
  (sb!impl::%makunbound '*machine-version*))

(defun os-cold-init-or-reinit ()
  (/show0 "entering OS-COLD-INIT-OR-REINIT")
  (/show0 "setting *CORE-STRING*")
  (init-var-ignoring-errors
   *core-string*
   (sb!alien:cast *native-core-string* sb!alien:c-string)
   :default "")
  (/show0 "setting *POSIX-ARGV*")
  (init-var-ignoring-errors
   *posix-argv*
   (loop for i from 0
         for arg = (sb!alien:deref *native-posix-argv* i)
         until (sb!alien:null-alien arg)
         collect (sb!alien:cast arg sb!alien:c-string)))
  (/show0 "setting *DEFAULT-PATHNAME-DEFAULTS*")
  ;; Temporary value, so that #'PARSE-NATIVE-NAMESTRING won't blow up
  ;; when we call it below.
  (setf *default-pathname-defaults* (make-trivial-default-pathname))
  (init-var-ignoring-errors
   *default-pathname-defaults*
   (parse-native-namestring (sb!unix:posix-getcwd/))
   :default *default-pathname-defaults*
   :explanation "with the current directory")
  (/show0 "setting *CORE-PATHNAME*")
  (setf *core-pathname* (merge-pathnames (native-pathname *core-string*)))
  (/show0 "setting *RUNTIME-PATHNAME*")
  (init-var-ignoring-errors
   *runtime-pathname*
   (let ((exe (os-get-runtime-executable-path t))
         (saved (sb!alien:cast *native-saved-runtime-path* sb!alien:c-string)))
     (when (or exe saved) (native-pathname (or exe saved)))))
  (/show0 "leaving OS-COLD-INIT-OR-REINIT"))
