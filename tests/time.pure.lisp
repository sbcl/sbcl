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

(in-package "CL-USER")

;;; Test for monotonicity of GET-INTERNAL-RUN-TIME. (On OpenBSD, this
;;; is not a given, because of a longstanding bug in getrusage().)
(funcall (compile nil
                  '(lambda (n-seconds)
                     (declare (type fixnum n-seconds))
                     (let* ((n-internal-time-units
                             (* n-seconds
                                internal-time-units-per-second))
                            (time0 (get-internal-run-time))
                            (time1 (+ time0 n-internal-time-units)))
                       (loop
                        (let ((time (get-internal-run-time)))
                          (assert (>= time time0))
                          (when (>= time time1)
                            (return)))))))
         3)

(with-test (:name :time/lambdas-converted)
  (time (compile nil '(lambda () 42))))
