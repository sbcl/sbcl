;;;; OS interface functions for SBCL under Win32.

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
#!-win32 (error "missing :WIN32 feature")

(defun software-type ()
  #!+sb-doc
  "Return a string describing the supporting software."
  (values "Win32"))

(defun software-version ()
  #!+sb-doc
  "Return a string describing version of the supporting software, or NIL
  if not available."
  (or *software-version*
      (setf *software-version*
	    (multiple-value-bind (MajorVersion MinorVersion BuildNumber PlatformId CSDVersion)
		(sb!win32:get-version-ex)
	      (declare (ignore PlatformId))
	      (format nil (if (zerop (length CSDVersion)) "~A.~A.~A" "~A.~A.~A (~A)")
		      MajorVersion MinorVersion BuildNumber CSDVersion)))))

;;; Return user time, system time, and number of page faults.
(defun get-system-info ()
  ;; FIXME: number of page faults is always zero
  (multiple-value-bind (creation-time exit-time kernel-time user-time)
      (sb!win32:get-process-times)
    (declare (ignore creation-time exit-time))
    (values (floor user-time 10) (floor kernel-time 10) 0)))

;;; Return the system page size.
(defun get-page-size ()
  ;; probably should call getpagesize()
  ;; FIXME: Or we could just get rid of this, since the uses of it look
  ;; disposable.
  4096)
