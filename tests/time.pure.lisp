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

;;; Test for monotonicity of GET-INTERNAL-RUN-TIME. (On OpenBSD, this
;;; is not a given, because of a longstanding bug in getrusage().)
(with-test (:name (get-internal-run-time :monotonic))
  (checked-compile-and-assert (:optimize nil)
      '(lambda (n-seconds)
         (let* ((n-internal-time-units
                 (* n-seconds
                    internal-time-units-per-second))
                (time0 (get-internal-run-time))
                (time1 (+ time0 n-internal-time-units)))
           (loop for time = (get-internal-run-time)
                 while (< time time1)
                 always (>= time time0))))
    ((0.5) t)))

(with-test (:name (time :lambdas-converted))
  (let ((output (with-output-to-string (*trace-output*)
                  (time (checked-compile '(lambda () 42))))))
    ;; We just want to confirm that some number of lambdas have been
    ;; converted. The exact number depends on the inner workings of
    ;; the compiler.
    (assert (search "converted" output))))

(with-test (:name :encode-decode-time)
  (let ((*random-state* (make-random-state t)))
    (loop repeat 200
          for time = (random (expt 2 (random 80)))
          for tz = (* 1/3600
                      (if (zerop (random 2))
                          -1
                          1)
                      (random (/ 24 1/3600)))
          do (multiple-value-bind (second minute hour date month year day daylight-p zone)
                 (decode-universal-time time tz)
               (assert (= day (mod (floor (- time (* tz 3600)) 86400) 7)))
               (assert (= zone tz))
               (assert (not daylight-p))
               (assert (= time
                          (encode-universal-time second minute hour date month year tz)))))))
