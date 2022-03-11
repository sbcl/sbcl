;;;; OS interface functions for SBCL under SunOS

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-SYS")

;;; Check that target machine features are set up consistently with
;;; this file.
#-sunos (error "missing :SUNOS feature")

(defun software-type ()
  "Return a string describing the supporting software."
  (values "SunOS"))

;;; Return system time, user time and number of page faults.
(defun get-system-info ()
  (multiple-value-bind
      (err? utime stime maxrss ixrss idrss isrss minflt majflt)
      (sb-unix:unix-getrusage sb-unix:rusage_self)
    (declare (ignore maxrss ixrss idrss isrss minflt))
    (unless err? ; FIXME: nonmnemonic (reversed) name for ERR?
      (error "Unix system call getrusage failed: ~A." (strerror utime)))
    (values utime stime majflt)))

;;; support for CL:MACHINE-VERSION defined OAOO elsewhere
(defun get-machine-version ()
  nil)

(in-package "SB-UNIX")

;; SunOS defines CLOCK_PROCESS_CPUTIME_ID but you get EINVAL if you try to use it.
(defun system-internal-run-time ()
  (multiple-value-bind (utime-sec utime-usec stime-sec stime-usec)
      (with-alien ((usage (struct sb-unix::rusage)))
        (syscall* ("sb_getrusage" int (* (struct sb-unix::rusage)))
                  (values (slot (slot usage 'sb-unix::ru-utime) 'sb-unix::tv-sec)
                          (slot (slot usage 'sb-unix::ru-utime) 'sb-unix::tv-usec)
                          (slot (slot usage 'sb-unix::ru-stime) 'sb-unix::tv-sec)
                          (slot (slot usage 'sb-unix::ru-stime) 'sb-unix::tv-usec))
                  rusage_self (addr usage)))
    (+ (* (+ utime-sec stime-sec) internal-time-units-per-second)
       (floor (+ utime-usec stime-usec
                 (floor microseconds-per-internal-time-unit 2))
              microseconds-per-internal-time-unit))))
