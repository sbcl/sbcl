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

;;; Make sure MAP-REFERENCING-OBJECTS doesn't spuriously treat raw bits as
;;; potential pointers. Also make sure it sees the SYMBOL-INFO slot.
(defstruct afoo (slot nil :type sb-ext:word))
(defvar *afoo* (make-afoo :slot (sb-kernel:get-lisp-obj-address '*posix-argv*)))
(with-test (:name :map-referencing-objs)
  (sb-vm::map-referencing-objects (lambda (x) (assert (not (typep x 'afoo))))
                                  :dynamic '*posix-argv*)
  (let ((v (sb-kernel:symbol-info 'satisfies)) referers)
    (sb-vm::map-referencing-objects (lambda (referer) (push referer referers))
                                    #+gencgc :dynamic #-gencgc :static v)
    (assert (member 'satisfies referers))))

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
(with-test (:name :bug-529014 :skipped-on '(not :gencgc))
  (loop for i from 0 to sb-vm:+pseudo-static-generation+
     do (assert (= (sb-ext:generation-bytes-consed-between-gcs i)
                   (truncate (sb-ext:bytes-consed-between-gcs)
                             sb-vm:+highest-normal-generation+)))
        ;; FIXME: These parameters are a) tunable in the source and b)
        ;; duplicated multiple times there and now here.  It would be good to
        ;; OAOO-ify them (probably to src/compiler/generic/params.lisp).
        (assert (= (sb-ext:generation-minimum-age-before-gc i) 0.75))
        (assert (= (sb-ext:generation-number-of-gcs-before-promotion i) 1))))

(defun stress-gc ()
  ;; Kludge or not?  I don't know whether the smaller allocation size
  ;; for sb-safepoint is a legitimate correction to the test case, or
  ;; rather hides the actual bug this test is checking for...  It's also
  ;; not clear to me whether the issue is actually safepoint-specific.
  ;; But the main problem safepoint-related bugs tend to introduce is a
  ;; delay in the GC triggering -- and if bug-936304 fails, it also
  ;; causes bug-981106 to fail, even though there is a full GC in
  ;; between, which makes it seem unlikely to me that the problem is
  ;; delay- (and hence safepoint-) related. --DFL
  (let* ((x (make-array (truncate #-sb-safepoint (* 0.2 (dynamic-space-size))
                                  #+sb-safepoint (* 0.1 (dynamic-space-size))
                                  sb-vm:n-word-bytes))))
    (elt x 0)))

(with-test (:name :bug-936304)
  (gc :full t)
  (time
   (assert (eq :ok (handler-case
                       (progn
                         (loop repeat 50 do (stress-gc))
                         :ok)
                     (storage-condition ()
                       :oom))))))

(with-test (:name :bug-981106)
  (gc :full t)
  (time
   (assert (eq :ok
               (handler-case
                   (dotimes (runs 100 :ok)
                     (let* ((n (truncate (dynamic-space-size) 1200))
                            (len (length
                                  (with-output-to-string (string)
                                    (dotimes (i n)
                                      (write-sequence "hi there!" string))))))
                       (assert (eql len (* n (length "hi there!"))))))
                 (storage-condition ()
                   :oom))))))

(with-test (:name :gc-logfile :skipped-on '(not :gencgc))
  (assert (not (gc-logfile)))
  (let ((p #p"gc.log"))
    (assert (not (probe-file p)))
    (assert (equal p (setf (gc-logfile) p)))
    (gc)
    (let ((p2 (gc-logfile)))
      (assert (equal (truename p2) (truename p))))
    (assert (not (setf (gc-logfile) nil)))
    (assert (not (gc-logfile)))
    (delete-file p)))
