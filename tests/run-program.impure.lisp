;;;; various RUN-PROGRAM tests with side effects

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(cl:in-package :cl-user)

;; Actually there's no real side-effect here. The impurity we're
;; avoiding is the sigchld handler that RUN-PROGRAM sets up, which
;; interfers with the manual unix process control done by the test
;; framework (sometimes the handler will manage to WAIT3 a process
;; before run-tests WAITPIDs it).

(let* ((process (sb-ext:run-program "/bin/cat" '() :wait nil
                                    :output :stream :input :stream))
       (out (process-input process))
       (in (process-output process)))
  (unwind-protect
       (loop for i from 0 to 255 do
             (write-byte i out)
             (force-output out)
             (assert (= (read-byte in) i)))
    (process-close process)))
