;;;; OS interface functions for SBCL common to all target OSes

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-SYS")

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

(define-load-time-global *core-string* "")
(define-load-time-global *core-pathname* nil
    "The absolute pathname of the running SBCL core.")
(define-load-time-global *software-version* nil)
(define-load-time-global *runtime-pathname* nil
    "The absolute pathname of the running SBCL runtime.")
(define-load-time-global *sbcl-homedir-pathname* nil)

(defun os-deinit ()
  (setf *default-pathname-defaults* (make-trivial-default-pathname)
        *core-string* ""
        *software-version* nil
        *runtime-pathname* nil
        *core-pathname* nil
        *posix-argv* nil
        *sbcl-homedir-pathname* nil)
  (sb-impl:%makunbound '*machine-version*))

(defun os-cold-init-or-reinit ()
  (/show0 "entering OS-COLD-INIT-OR-REINIT")
  (/show0 "setting *CORE-STRING*")
  (init-var-ignoring-errors
   *core-string*
   (sb-alien:extern-alien "core_string" sb-alien:c-string)
   :default "")
  (/show0 "setting *POSIX-ARGV*")
  (init-var-ignoring-errors
   *posix-argv*
   (loop for i from 0
         for arg = (sb-alien:deref (sb-alien:extern-alien posix_argv
                                                          (* (sb-alien:c-string
                                                              #+win32 :external-format #+win32 :ucs-2)))
                                   i)
         while arg
         collect arg))
  (/show0 "setting *DEFAULT-PATHNAME-DEFAULTS*")
  ;; Temporary value, so that #'PARSE-NATIVE-NAMESTRING won't blow up
  ;; when we call it below.
  (setf *default-pathname-defaults* (make-trivial-default-pathname))
  (init-var-ignoring-errors
   *default-pathname-defaults*
   (parse-native-namestring (sb-unix:posix-getcwd/))
   :default *default-pathname-defaults*
   :explanation "with the current directory")
  (/show0 "setting *CORE-PATHNAME*")
  (setf *core-pathname* (merge-pathnames (native-pathname *core-string*)))
  (/show0 "setting *RUNTIME-PATHNAME*")
  (init-var-ignoring-errors
   *runtime-pathname*
   (native-pathname (sb-alien:extern-alien "sbcl_runtime" sb-alien:c-string)))
  (init-var-ignoring-errors
   *sbcl-homedir-pathname* (sb-impl::%sbcl-homedir-pathname))
  (/show0 "leaving OS-COLD-INIT-OR-REINIT"))
