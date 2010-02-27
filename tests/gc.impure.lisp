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

  ;; check that WITHOUT-GCING defers explicit gc
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
    (assert gc-happend)))

;;; SB-EXT:GENERATION-* accessors returned bogus values for generation > 0
#+gencgc
(with-test (:name :bug-529014)
  ;; FIXME: These parameters are a) tunable in the source and b)
  ;; duplicated multiple times there and now here.  It would be good to
  ;; OAOO-ify them (probably to src/compiler/generic/params.lisp).
  (loop for i from 0 to sb-vm:+pseudo-static-generation+
     do (assert (= (sb-ext:generation-bytes-consed-between-gcs i) 2000000))
       (assert (= (sb-ext:generation-minimum-age-before-gc i) 0.75))
       (assert (= (sb-ext:generation-number-of-gcs-before-promotion i) 1))))
