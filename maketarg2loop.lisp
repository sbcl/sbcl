
;;; Run this in the host lisp. It will invoke the compilation pass
;;; of make-target-2 a specified number of times.
;;; This is probably not super useful for actually debugging a
;;; failed build, but it is useful to identify the revision that
;;; introduced a GC regression that causes random invariant failures.
;;; If you have an estimate of how often builds fail at head or WIP,
;;; you could bisect using (MT2-HAMMER N) for sufficiently large N to
;;; decide whether each bisection step passes or fails. e.g.
;;; if your intuition tells you that you were seeing at least 1 in 20
;;; build failures at the latest revision, then:
;;; * (load "maketarget2loop.lisp")
;;; * (mt2-hammer 20)
;;; exiting with 0 probably means that it's a good build.
(defun mt2-hammer (n-iterations)
  (let (jobs)
    (dotimes (i n-iterations)
      (let ((dir (format nil "obj/attempt~D/" (1+ i))))
        (ensure-directories-exist dir)
        (let ((job
               (sb-ext:run-program
                "src/runtime/sbcl"
                `("--core" "output/cold-sbcl.core"
                           "--lose-on-corruption" "--no-sysinit" "--no-userinit"
                           "--eval" ,(format nil "(defvar *objfile-prefix* ~S)" dir)
                           "--load" "src/cold/warm.lisp")
                :output (format nil "~A/out" dir)
                :if-output-exists :supersede
                :error (format nil "~A/err" dir)
                :if-error-exists :supersede
                :wait nil)))
          (push job jobs))))
    (setq jobs (nreverse jobs))
    (loop
     (let ((n-running (count :running jobs :key #'process-status)))
       (when (zerop n-running) (return))
       (format t "~&Waiting for ~D job~:P~%" n-running)
       (sleep 2)))
    (dolist (job jobs)
      (unless (= (process-exit-code job) 0)
        (format t "~&~S did not exit with 0~%"
                job)))))
