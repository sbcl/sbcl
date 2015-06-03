;;;; OS interface functions for SBCL under Linux

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!SYS")

;;; Check that target machine features are set up consistently with
;;; this file.
#!-android (error "missing :ANDROID feature")

(defun software-type ()
  #!+sb-doc
  "Return a string describing the supporting software."
  "Android")

;;; FIXME: More duplicated logic here vrt. other oses. Abstract into
;;; uname-software-version?
(defun software-version ()
  #!+sb-doc
  "Return a string describing version of the supporting software, or NIL
  if not available."
  (or *software-version*
      (setf *software-version*
            (sb!alien:with-alien
                ((ptr (* char)
                      (sb!alien:alien-funcall
                       (sb!alien:extern-alien "software_version"
                                              (function (* sb!alien:char))))))
              (and (not (sb!alien:null-alien ptr))
                   (unwind-protect
                        (sb!alien:with-alien ((c-string sb!alien:c-string ptr))
                          c-string)
                     (sb!alien:free-alien ptr)))))))

;;; Return user time, system time, and number of page faults.
(defun get-system-info ()
  (multiple-value-bind
      (err? utime stime maxrss ixrss idrss isrss minflt majflt)
      (sb!unix:unix-getrusage sb!unix:rusage_self)
    (declare (ignore maxrss ixrss idrss isrss minflt))
    (unless err? ; FIXME: nonmnemonic (reversed) name for ERR?
      (error "Unix system call getrusage failed: ~A." (strerror utime)))
    (values utime stime majflt)))

;;; Return the system page size.
(defun get-page-size ()
  sb!c:*backend-page-bytes*)

;;; support for CL:MACHINE-VERSION defined OAOO elsewhere
(defun get-machine-version ()
  (or
   #!+(and mips little-endian)
   "little-endian"
   #!+(and mips big-endian)
   "big-endian"
   (let ((marker
          ;; hoping "cpu" exists and gives something useful in
          ;; all relevant Linuxen...
          ;;
          ;; from Lars Brinkhoff sbcl-devel 26 Jun 2003:
          ;;   I examined different versions of Linux/PPC at
          ;;   http://lxr.linux.no/ (the file that outputs
          ;;   /proc/cpuinfo is arch/ppc/kernel/setup.c, if
          ;;   you want to check), and all except 2.0.x
          ;;   seemed to do the same thing as far as the
          ;;   "cpu" field is concerned, i.e. it always
          ;;   starts with the (C-syntax) string "cpu\t\t: ".
          #!+ppc "cpu"
          ;; The field "model name" exists on kernel 2.4.21-rc6-ac1
          ;; anyway, with values e.g.
          ;;   "AMD Athlon(TM) XP 2000+"
          ;;   "Intel(R) Pentium(R) M processor 1300MHz"
          ;; which seem comparable to the information in the example
          ;; in the MACHINE-VERSION page of the ANSI spec.
          #!+(or x86 x86-64) "model name"
          #!+(or arm arm64) "Processor"))
     (when marker
       (with-open-file (stream "/proc/cpuinfo"
                               ;; Even on Linux it's an option to build
                               ;; kernels without /proc filesystems, so
                               ;; degrade gracefully.
                               :if-does-not-exist nil)
         (loop with line while (setf line (read-line stream nil))
               when (eql (search marker line) 0)
               return (string-trim " " (subseq line (1+ (position #\: line))))))))))
