(defpackage sb-cover-test (:use :cl :asdf :uiop))

(in-package sb-cover-test)

(defparameter *source-directory*
  (system-source-directory :sb-cover))
(defparameter *output-directory*
  (apply-output-translations *source-directory*))

(setf *default-pathname-defaults* (translate-logical-pathname *default-pathname-defaults*))

(defun compile-load (x)
  (flet ((in-dir (dir type)
           (translate-logical-pathname (subpathname dir x :type type))))
    (load (compile-file (in-dir *source-directory* "lisp")
                        :output-file (in-dir *output-directory* "fasl")))))

(defun report ()
  (handler-case
      (sb-cover:report *output-directory*)
    (warning (condition)
      (error "Unexpected warning: ~A" condition))))

(defun report-expect-failure ()
  (handler-case
      (progn
        (sb-cover:report *output-directory*)
        (error "Should've signaled a warning"))
    (warning ())))


;;; No instrumentation
(compile-load "test-data-1")
(report-expect-failure)

;;; Instrument the file, try again -- first with a non-directory pathname

(proclaim '(optimize sb-cover:store-coverage-data))
(compile-load "test-data-1")

(catch 'ok
  (handler-case
      (sb-cover:report #p"/tmp/foo")
    (error ()
      (throw 'ok nil)))
  (error "REPORT with a non-pathname directory did not signal an error."))

(report)

(assert (probe-file (subpathname *output-directory* "cover-index.html")))

;;; None of the code was executed
(assert (zerop (sb-cover::ok-of (getf sb-cover::*counts* :branch))))
(assert (zerop (sb-cover::all-of (getf sb-cover::*counts* :branch))))
(assert (zerop (sb-cover::ok-of (getf sb-cover::*counts* :expression))))
(assert (plusp (sb-cover::all-of (getf sb-cover::*counts* :expression))))

;;; Call the function again
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
(assert (= 13
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

;; Clean up after the tests
(map nil #'delete-file
     (directory (merge-pathnames #p"*.html" *output-directory*)))
