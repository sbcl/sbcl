;;;; prior to sbcl 0.9.15.39 a SIGSTOP and SIGCONT while waiting
;;;; on CONDITION-WAIT caused a spurious wakeup.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;
;;;; This software is in the public domain and is provided with
;;;; absoluely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(use-package :sb-thread)

(defvar *queue* (make-waitqueue))
(defvar *lock* (make-mutex :name "lock"))

(with-mutex (*lock*)
  (write-line "/waiting")
  (force-output)
  (delete-file "condition-wait-sigcont.tmp")
  #+sb-thread
  (condition-wait *queue* *lock*)
  #-sb-thread
  (loop (sleep 10))
  (write-line "/woken")
  (force-output)
  (with-open-file (f "condition-wait-sigcont.tmp" :direction :output)
    (write-line "woken!" f)))

(write-line "/oops!")
(force-output)

;; sleep a bit so our runner can kill us
(sleep 10)
(exit)
