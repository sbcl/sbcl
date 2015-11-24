;;;; Tests for async signal safety.

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

(use-package :test-util)

(with-test (:name (:async-unwind :specials))
  (let ((*x0* nil) (*x1* nil) (*x2* nil) (*x3* nil) (*x4* nil))
    (declare (special *x0* *x1* *x2* *x3* *x4*))
    (loop repeat 10 do
          (loop repeat 10 do
                (catch 'again
                  (sb-ext:schedule-timer (sb-ext:make-timer
                                          (lambda ()
                                            (throw 'again nil)))
                                         (random 0.1))
                  (loop
                   (let ((*x0* (cons nil nil)) (*x1* (cons nil nil))
                         (*x2* (cons nil nil)) (*x3* (cons nil nil))
                         (*x4* (cons nil nil)))
                     (declare (special *x0* *x1* *x2* *x3* *x4*)))))
                (when (not (and (null *x0*) (null *x1*) (null *x2*) (null *x3*)
                                (null *x4*)))
                  (format t "~S ~S ~S ~S ~S~%" *x0* *x1* *x2* *x3* *x4*)
                  (assert nil)))
          (princ '*)
          (force-output))
    (terpri)))

(require :sb-posix)

(with-test (:name (:signal :errno)
                  ;; This test asserts that nanosleep behaves correctly
                  ;; for invalid values and sets EINVAL.  Well, we have
                  ;; nanosleep on Windows, but it depends on the caller
                  ;; (namely SLEEP) to produce known-good arguments, and
                  ;; even if we wanted to check argument validity,
                  ;; integration with `errno' is not to be expected.
                  :skipped-on :win32)
  (let* (saved-errno
         (returning nil)
         (timer (make-timer (lambda ()
                              (sb-unix:unix-open "~!@#$%^&*[]()/\\" 0 0)
                              (assert (= sb-unix:enoent
                                         (sb-unix::get-errno)))
                              (setq returning t)))))
    (schedule-timer timer 0.2)
    ;; Fail and set errno.
    (sb-unix:nanosleep -1 -1)
    (setq saved-errno (sb-unix::get-errno))
    (assert (= saved-errno sb-posix:einval))
    ;; Wait, but not with sleep because that will be interrupted and
    ;; we get EINTR.
    (loop until returning)
    (assert (= saved-errno (sb-unix::get-errno)))))

(with-test (:name :handle-interactive-interrupt
                  ;; It is desirable to support C-c on Windows, but SIGINT
                  ;; is not the mechanism to use on this platform.
                  :skipped-on :win32)
  (assert (eq :condition
              (handler-case
                  (progn
                    (sb-thread::kill-safely
                     (sb-thread::thread-os-thread sb-thread::*current-thread*)
                     sb-unix:sigint)
                    #+sb-safepoint-strictly
                    ;; In this case, the signals handler gets invoked
                    ;; indirectly through an INTERRUPT-THREAD.  Give it
                    ;; enough time to hit.
                    (sleep 1))
                (sb-sys:interactive-interrupt ()
                  :condition)))))

(with-test (:name :bug-640516)
  ;; On Darwin interrupting a SLEEP so that it took longer than
  ;; the requested amount caused it to hang.
  (assert
   (handler-case
       (sb-ext:with-timeout 10
         (let (to)
           (handler-bind ((sb-ext:timeout (lambda (c)
                                            (unless to
                                              (setf to t)
                                              (sleep 2)
                                              (continue c)))))
             (sb-ext:with-timeout 0.1 (sleep 1) t))))
     (sb-ext:timeout ()
       nil))))
