;;;; unithread stub support for threads in the target machine

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!THREAD")

;;; used bu debug-int.lisp to access interrupt contexts
#!-sb-fluid (declaim (inline sb!vm::current-thread-offset-sap))
(defun sb!vm::current-thread-offset-sap (n) 
  (declare (type (unsigned-byte 27) n))
  (sb!sys:sap-ref-sap (alien-sap (extern-alien "all_threads" (* t))) 
	       (* n sb!vm:n-word-bytes)))

(defun current-thread-id ()
  ;; FIXME: 32/64
  (sb!sys:sap-ref-32 (alien-sap (extern-alien "all_threads" (* t))) 
	       (* sb!vm::thread-os-thread-slot sb!vm:n-word-bytes)))

(defun reap-dead-threads ())

;;;; queues, locks 

;; spinlocks use 0 as "free" value: higher-level locks use NIL
(defun get-spinlock (lock offset new-value)
  (declare (ignore lock offset new-value)))

(defmacro with-spinlock ((queue) &body body)
  (declare (ignore queue))
  `(progn ,@body))

;;;; the higher-level locking operations are based on waitqueues

(defstruct waitqueue
  (name nil :type (or null simple-string))
  (lock 0)
  (data nil))

(defstruct (mutex (:include waitqueue))
  (value nil))

;;;; mutex

(defun get-mutex (lock &optional new-value (wait-p t))
  (declare (type mutex lock))
  (let ((old-value (mutex-value lock)))
    (when (and old-value wait-p)
      (error "In unithread mode, mutex ~S was requested with WAIT-P ~S and ~
              new-value ~S, but has already been acquired (with value ~S)."
	     lock wait-p new-value old-value))
    (setf (mutex-value lock) new-value)
    t))

(defun release-mutex (lock)
  (declare (type mutex lock))
  (setf (mutex-value lock) nil))


;; FIXME need suitable stub or ERROR-signaling definitions for 
;; condition-wait (queue lock)
;; condition-notify (queue)

;;;; job control

(defun init-job-control () t)
(defun debugger-wait-until-foreground-thread (stream)
  (declare (ignore stream))
  t)
(defun get-foreground () t)
(defun release-foreground (&optional next)
  (declare (ignore next))
  t)
(defun terminate-session ())
