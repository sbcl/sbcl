(in-package "SB-COVER-TEST")

;;; No instrumentation
(sb-cover:clear-coverage)
(compile-load "test-data-1")
(report-expect-failure)

;;; Instrument the file, try again -- first with a non-directory pathname

(proclaim '(optimize sb-cover:store-coverage-data))
(compile-load "test-data-1")

(catch 'ok
  (handler-case
      (sb-cover:report #p"/tmp/foo")
    (error (c)
      (when (search "does not designate a directory" (princ-to-string c))
        (throw 'ok nil))))
  (error "REPORT with a non-pathname directory did not signal an error."))

(report)

(assert (probe-file (merge-pathnames "cover-index.html" *output-directory*)))

;;; Only the top level forms have been executed
(assert (zerop (sb-cover::ok-of (getf sb-cover::*counts* :branch))))
(assert (zerop (sb-cover::all-of (getf sb-cover::*counts* :branch))))
(assert (= 2 (sb-cover::ok-of (getf sb-cover::*counts* :expression))))
(assert (plusp (sb-cover::all-of (getf sb-cover::*counts* :expression))))

;;; Call the function
(test1)
(report)

;;; And now we should have complete expression coverage
(assert (zerop (sb-cover::ok-of (getf sb-cover::*counts* :branch))))
(assert (zerop (sb-cover::all-of (getf sb-cover::*counts* :branch))))
(assert (plusp (sb-cover::ok-of (getf sb-cover::*counts* :expression))))
(assert (= (sb-cover::ok-of (getf sb-cover::*counts* :expression))
           (sb-cover::all-of (getf sb-cover::*counts* :expression))))

;;; Reset-coverage clears the instrumentation
(sb-cover:reset-coverage)

(report)

;;; So none of the code should be marked as executed
(assert (zerop (sb-cover::ok-of (getf sb-cover::*counts* :branch))))
(assert (zerop (sb-cover::all-of (getf sb-cover::*counts* :branch))))
(assert (zerop (sb-cover::ok-of (getf sb-cover::*counts* :expression))))
(assert (plusp (sb-cover::all-of (getf sb-cover::*counts* :expression))))

;;; Forget all about that file
(sb-cover:clear-coverage)
(report-expect-failure)

;;; Another file, with some branches
(compile-load "test-data-2")

(test2 1)
(report)

;; Complete expression coverage
(assert (plusp (sb-cover::ok-of (getf sb-cover::*counts* :expression))))
(assert (= (sb-cover::ok-of (getf sb-cover::*counts* :expression))
           (sb-cover::all-of (getf sb-cover::*counts* :expression))))
;; Partial branch coverage
(assert (plusp (sb-cover::ok-of (getf sb-cover::*counts* :branch))))
(assert (plusp (sb-cover::all-of (getf sb-cover::*counts* :branch))))
(assert (/= (sb-cover::ok-of (getf sb-cover::*counts* :branch))
            (sb-cover::all-of (getf sb-cover::*counts* :branch))))

(test2 0)
(report)

;; Complete branch coverage
(assert (= (sb-cover::ok-of (getf sb-cover::*counts* :branch))
           (sb-cover::all-of (getf sb-cover::*counts* :branch))))

;; Check for presence of constant coalescing bugs
(compile-load "test-data-3")
(let ((*standard-output* (make-broadcast-stream)))
  (test-2))

;;; Another file, with some branches
(compile-load "test-data-branching-forms")

(test-branching-forms)
(report)

;; Complete expression coverage
(assert (= 14
           (sb-cover::ok-of (getf sb-cover::*counts* :expression))
           (sb-cover::all-of (getf sb-cover::*counts* :expression))))

;; Make sure we simulate (in-package) correctly.
(sb-cover:clear-coverage)
(compile-load "test-data-4")
(test4)
(report)

;;; And now we should have complete expression coverage
(assert (zerop (sb-cover::ok-of (getf sb-cover::*counts* :branch))))
(assert (zerop (sb-cover::all-of (getf sb-cover::*counts* :branch))))
(assert (plusp (sb-cover::ok-of (getf sb-cover::*counts* :expression))))
(assert (= (sb-cover::ok-of (getf sb-cover::*counts* :expression))
           (sb-cover::all-of (getf sb-cover::*counts* :expression))))

;; Make sure we handle non-local exits from function calls correctly.
(sb-cover:clear-coverage)
(compile-load "test-data-5")
(outer)
(report)

(assert (zerop (sb-cover::ok-of (getf sb-cover::*counts* :branch))))
(assert (zerop (sb-cover::all-of (getf sb-cover::*counts* :branch))))
(assert (= 12 (sb-cover::ok-of (getf sb-cover::*counts* :expression))))
(assert (= 16 (sb-cover::all-of (getf sb-cover::*counts* :expression))))

;; And then ensure that non-local exits from local calls are handled
;; correctly as well.
(sb-cover:clear-coverage)
(compile-load "test-data-6")
(nlx-from-flet)
(report)

(assert (zerop (sb-cover::ok-of (getf sb-cover::*counts* :branch))))
(assert (zerop (sb-cover::all-of (getf sb-cover::*counts* :branch))))
(assert (= 7 (sb-cover::ok-of (getf sb-cover::*counts* :expression))))
(assert (= 11 (sb-cover::all-of (getf sb-cover::*counts* :expression))))

(sb-cover:clear-coverage)
(compile-load "test-data-sharp-plus-minus")
(sharp-plus-minus 3)
(report)

(assert (zerop (sb-cover::ok-of (getf sb-cover::*counts* :branch))))
(assert (zerop (sb-cover::all-of (getf sb-cover::*counts* :branch))))
(assert (= 5 (sb-cover::ok-of (getf sb-cover::*counts* :expression))))
(assert (= 5 (sb-cover::all-of (getf sb-cover::*counts* :expression))))

(sb-cover:clear-coverage)
(compile-load "test-data-sharp-c")
(sharp-c 4)
(report)

(assert (zerop (sb-cover::ok-of (getf sb-cover::*counts* :branch))))
(assert (zerop (sb-cover::all-of (getf sb-cover::*counts* :branch))))
(assert (= 7 (sb-cover::ok-of (getf sb-cover::*counts* :expression))))
(assert (= 7 (sb-cover::all-of (getf sb-cover::*counts* :expression))))

(sb-cover:clear-coverage)
(defun generate-code (x y)
  `(if (evenp ,x) (1+ ,y) (1- ,y)))
(compile-load "test-data-read-eval")
(report)
(assert (zerop (sb-cover::ok-of (getf sb-cover::*counts* :branch))))
(assert (= 2 (sb-cover::all-of (getf sb-cover::*counts* :branch))))
(assert (= 2 (sb-cover::ok-of (getf sb-cover::*counts* :expression))))
(assert (= 6 (sb-cover::all-of (getf sb-cover::*counts* :expression))))

(read-eval 3 4)
(report)
(assert (= 1 (sb-cover::ok-of (getf sb-cover::*counts* :branch))))
(assert (= 2 (sb-cover::all-of (getf sb-cover::*counts* :branch))))
(assert (= 5 (sb-cover::ok-of (getf sb-cover::*counts* :expression))))
(assert (= 6 (sb-cover::all-of (getf sb-cover::*counts* :expression))))
