(defparameter *x* ())

(defun cons-madly ()
  (loop repeat 10000 do
        (setq *x* (make-string 100000))))

;; check that WITHOUT-INTERRUPTS doesn't block the gc trigger
(with-test (:name :cons-madly-without-interrupts)
  (sb-sys:without-interrupts (cons-madly)))

(define-alien-variable n-lisp-gcs int)
(with-test (:name :without-gcing
            :skipped-on :gc-stress)
  (let ((initial-gc-count n-lisp-gcs))

    ;; check that WITHOUT-GCING defers explicit gc
    (sb-sys:without-gcing
      (gc)
      (assert (= n-lisp-gcs initial-gc-count)))
    (assert (> n-lisp-gcs initial-gc-count))

    ;; check that WITHOUT-GCING defers SIG_STOP_FOR_GC
    #+sb-thread
    (let ((in-without-gcing nil))
      (setq initial-gc-count n-lisp-gcs)
      (sb-thread:make-thread (lambda ()
                               (loop while (not in-without-gcing))
                               (sb-ext:gc)))
      (sb-sys:without-gcing
        (setq in-without-gcing t)
        (sleep 3)
        (assert (= n-lisp-gcs initial-gc-count)))
      (sleep 1)
      (assert (> n-lisp-gcs initial-gc-count)))))

;;; After each iteration of FOO there are a few pinned conses.
;;; On alternate GC cycles, those get promoted to generation 1.
;;; When the logic for page-spanning-object zeroing incorrectly decreased
;;; the upper bound on bytes used for partially pinned pages, it caused
;;; an accumulation of pages in generation 1 each with 2 objects' worth
;;; of bytes, and the remainder waste. Because the waste was not accounted
;;; for, it did not trigger GC enough to avoid heap exhaustion.
(with-test (:name :smallobj-auto-gc-trigger)
  ;; Ensure that these are compiled functions because the interpreter
  ;; would make lots of objects of various sizes which is insufficient
  ;; to provoke the bug.
  (setf (symbol-function 'foo)
        (compile nil '(lambda () (list 1 2))))
  ;; 500 million iterations of this loop seems to be reliable enough
  ;; to show that GC happens.
  (setf (symbol-function 'callfoo)
        (compile nil '(lambda () (loop repeat 500000000 do (foo)))))
  (funcall 'callfoo))

#+sb-thread
(with-test (:name :concurrently-alloc-code)
  ;; this debug setting may or may not find a problem, but it can't hurt to try
  (setf (extern-alien "pre_verify_gen_0" int) 1)
  (let ((worker-th
         (sb-thread:make-thread
          (let ((stop (+ (get-internal-real-time)
                         (* 1.5 internal-time-units-per-second))))
            (lambda (&aux (n 0))
              (loop while (<= (get-internal-real-time) stop)
                    do (compile nil `(lambda () (print 20)))
                       (incf n))
              n)))))
    (let ((gcs 0))
      (loop (gc) (incf gcs)
            (unless (sb-thread:thread-alive-p worker-th)
              (return))
            (sb-unix:nanosleep 0 (+ 1000000 (random 100000))))
      (let ((compiles (sb-thread:join-thread worker-th)))
        (format t "~&Compiled ~D times, GC'ed ~D times~%"
                compiles gcs))))
  (setf (extern-alien "pre_verify_gen_0" int) 0))
