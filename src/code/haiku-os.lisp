;;;; OS interface functions for SBCL under Linux

(in-package "SB-SYS")

(defun software-type ()
  "Return a string describing the supporting software."
  "Haiku")

;;; Return system time, user time and number of page faults.
(defun get-system-info ()
  (multiple-value-bind (err? utime stime maxrss ixrss idrss
                             isrss minflt majflt)
                       (sb-unix:unix-getrusage sb-unix:rusage_self)
    (declare (ignore maxrss ixrss idrss isrss minflt))
    (unless err?
      (simple-perror "Unix system call getrusage() failed" :errno utime))
    (values utime stime majflt)))

;;; support for CL:MACHINE-VERSION defined OAOO elsewhere
(defun get-machine-version ()
  nil)
