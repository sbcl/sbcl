;;;; OS interface functions for SBCL under BSD Unix.

;;;; This code was written as part of the CMU Common Lisp project at
;;;; Carnegie Mellon University, and has been placed in the public
;;;; domain.

(in-package "SB-IMPL")

;;;; Check that target machine features are set up consistently with
;;;; this file.
#-bsd
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "The :BSD feature is missing, we shouldn't be doing this code."))

(define-alien-routine ("sysctl" %sysctl) int
  (name (* int))
  (namelen unsigned-int)
  (oldp (* t))
  (oldlenp (* sb-unix:size-t))
  (newp (* t))
  (newlen sb-unix:size-t))

#+darwin
(define-alien-routine ("sysctlbyname" %sysctlbyname) int
  (name c-string)
  (oldp (* t))
  (oldlenp (* sb-unix:size-t))
  (newp (* t))
  (newlen sb-unix:size-t))

(defun sysctl (type &rest name)
  "Retrieves an integer or string value with the given name."
  (let ((name-len (length name)))
    (when (> name-len ctl-maxname)
      (error "sysctl name ~S is too long" name))
    (with-alien ((name-array (array int #.ctl-maxname))
                 (result-len sb-unix:size-t))
      (dotimes (off name-len)
        (setf (deref name-array off) (elt name off)))
      (ecase type
        (:int
         (with-alien ((result int))
           (setf result-len (alien-size int :bytes))
           (unless (minusp (%sysctl (cast name-array (* int)) name-len
                                    (addr result) (addr result-len) nil 0))
             result)))
        (:str
         (unless (minusp (%sysctl (cast name-array (* int)) name-len
                                  nil (addr result-len) nil 0))
           (with-alien ((result (* char) (make-alien char result-len)))
             (if (minusp (%sysctl (cast name-array (* int)) name-len
                                  result (addr result-len) nil 0))
                 (free-alien result)
                 (sb-unix::newcharstar-string result)))))))))

#+darwin
(defun sysctlbyname (type name)
  "Retrieves an integer or string value with the given name."
  (with-alien ((result-len sb-unix:size-t))
    (ecase type
      (:int
       (with-alien ((result int))
         (setf result-len (alien-size int :bytes))
         (unless (minusp (%sysctlbyname name (addr result)
                                        (addr result-len) nil 0))
           result)))
      (:str
       (unless (minusp (%sysctlbyname name nil (addr result-len) nil 0))
         (with-alien ((result (* char) (make-alien char result-len)))
           (if (minusp (%sysctlbyname name result (addr result-len) nil 0))
               (free-alien result)
               (sb-unix::newcharstar-string result))))))))

(defun software-type ()
  "Return a string describing the supporting software."
  #-gnu-kfreebsd (sysctl :str ctl-kern kern-ostype)
  #+gnu-kfreebsd "GNU/kFreeBSD")

(defun software-version ()
  "Return a string describing version of the supporting software, or NIL
   if not available."
  (or sb-sys::*software-version*
      (setf sb-sys::*software-version*
            (sysctl :str ctl-kern kern-osrelease))))

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
  (or #+darwin (sysctlbyname :str "machdep.cpu.brand_string")
      (sysctl :str ctl-hw hw-model)))
