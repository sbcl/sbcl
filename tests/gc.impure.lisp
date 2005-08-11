;;;; gc tests

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

(in-package :cl-user)

(defparameter *x* ())

(defun cons-madly ()
  (loop repeat 10000 do
        (setq *x* (make-string 100000))))

;; check that WITHOUT-INTERRUPTS doesn't block the gc trigger
(sb-sys:without-interrupts (cons-madly))

;; check that WITHOUT-INTERRUPTS doesn't block SIG_STOP_FOR_GC
#+sb-thread
(sb-sys:without-interrupts
  (let ((thread (sb-thread:make-thread (lambda () (sb-ext:gc)))))
    (loop while (sb-thread:thread-alive-p thread))))

(let ((gc-happend nil))
  (push (lambda () (setq gc-happend t)) sb-ext:*after-gc-hooks*)

  ;; check GC-{ON,OFF} works and gc is deferred
  (gc-off)
  (gc)
  (assert (not gc-happend))
  (gc-on)
  (assert gc-happend)

  ;; check that WITHOUT-GCING defers explicit gc
  (setq gc-happend nil)
  (sb-sys:without-gcing
    (gc)
    (assert (not gc-happend)))
  (assert gc-happend)

  ;; check that WITHOUT-GCING defers SIG_STOP_FOR_GC
  #+sb-thread
  (let ((in-without-gcing nil))
    (setq gc-happend nil)
    (sb-thread:make-thread (lambda ()
                             (loop while (not in-without-gcing))
                             (sb-ext:gc)))
    (sb-sys:without-gcing
      (setq in-without-gcing t)
      (sleep 3)
      (assert (not gc-happend)))
    ;; give the hook time to run
    (sleep 1)
    (assert gc-happend))

  ;; check GC-ON works even in a WITHOUT-GCING
  (setq gc-happend nil)
  (sb-sys:without-gcing
    (gc)
    (assert (not gc-happend))
    (gc-on)
    (assert gc-happend)
    (setq gc-happend nil))
  (assert (not gc-happend)))

(sb-ext:quit :unix-status 104)
