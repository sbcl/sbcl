;;;; -*-  Lisp -*-
;;;;
;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package :sb-concurrency-test)
(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))

#+sb-thread
(progn

(defvar *cpus*
  (max 1
       #-win32 (sb-alien:alien-funcall
                (sb-alien:extern-alien "sysconf"
                                       (function sb-alien:long sb-alien:int))
                sb-unix::sc-nprocessors-onln)
       #+win32 (sb-alien:extern-alien "os_number_of_processors" sb-alien:int)))

(defparameter +timeout+ 30.0)

(defun make-threads (n name fn)
  (loop for i from 1 to n
        collect (make-thread fn :name (format nil "~A-~D" name i))))

(defun timed-join-thread (thread &optional (timeout +timeout+))
  (handler-case (sb-sys:with-deadline (:seconds timeout)
                  (join-thread thread :default :aborted))
    (sb-ext:timeout ()
      :timeout)))

(defun hang ()
  (join-thread *current-thread*))

(defun kill-thread (thread)
  (when (thread-alive-p thread)
    (ignore-errors
      (terminate-thread thread))))

) ;; #+sb-thread (progn ...
