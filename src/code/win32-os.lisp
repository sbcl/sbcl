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
            (multiple-value-bind
                  (major-version minor-version build-number platform-id csd-version)
                (sb!win32:get-version-ex)
              (declare (ignore platform-id))
              (format nil (if (zerop (length csd-version))
                              "~A.~A.~A"
                              "~A.~A.~A (~A)")
                      major-version minor-version build-number csd-version)))))

;;; Return user time, system time, and number of page faults.
(defun get-system-info ()
  (sb!win32:with-process-times (creation-time exit-time kernel-time user-time)
    (values (floor user-time 10) (floor kernel-time 10) 0)))

;;; Return the system page size.
(defun get-page-size ()
  ;; probably should call getpagesize()
  ;; FIXME: Or we could just get rid of this, since the uses of it look
  ;; disposable.
  4096)

;;; support for CL:MACHINE-VERSION defined OAOO elsewhere
(defun get-machine-version ()
  nil)
