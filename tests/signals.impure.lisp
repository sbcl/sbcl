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

(sb-ext:finalize (list 1) (lambda ()))
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
                  ;; And this hangs on darwin + safepoint.
                  :skipped-on (or :win32 (:and :darwin :sb-safepoint)))
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

;; It is desirable to support C-c on Windows, but SIGINT
;; is not the mechanism to use on this platform.
;; This test used to call kill_safely() in the C runtime if using safepoints,
;; and perhaps at some point kill_safely() interacted with the safepoint state
;; for POSIX (i.e. not win32), but it doesn't, at least not now.
;; The special case in kill_safely() for the current thread is pthread_kill()
;; and not a thing more, unless on win32, which skips this test.
;; Note also that RAISE sends a thread-directed signal as per the man page
;; "In a multithreaded program it is equivalent to pthread_kill(pthread_self(), sig);"
;; but thread-directed SIGINT is not the right thing, as it does not accurately
;; model the effect of pressing control-C; hence we should use UNIX-KILL here,
;; which sends a process-directed signal, letting the OS pick a thread.
;; Whether it picks the finalizer thread or main thread, things should work,
;; because we forward to the signal to our foreground thread.
#+unix
(with-test (:name :handle-interactive-interrupt)
  (assert (eq :condition
              (handler-case
                  (progn
                    (sb-unix:unix-kill (sb-unix:unix-getpid) sb-unix:sigint)
                    #+sb-safepoint
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

#+unix
(with-test (:name :ignore-sigpipe)
  (multiple-value-bind (read-side write-side) (sb-unix:unix-pipe)
    (sb-unix:unix-close read-side)
    (sb-sys:enable-interrupt sb-unix:sigpipe :ignore)
    (let ((buffer "x"))
      (sb-sys:with-pinned-objects (buffer)
        (multiple-value-bind (nbytes errno)
            (sb-unix:unix-write write-side buffer 0 1)
          (assert (and (null nbytes)
                       (= errno sb-unix:epipe))))))
    (sb-unix:unix-close write-side)))
