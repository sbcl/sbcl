(defpackage sb-cover-test
  (:use "CL"))

(in-package sb-cover-test)

(defparameter *path* #.(truename *compile-file-pathname*))
(defparameter *output-directory*
  (merge-pathnames (make-pathname :name nil
                                  :type nil
                                  :version nil
                                  :directory '(:relative "test-output"))
                   (make-pathname :directory (pathname-directory *path*))))

(defun report ()
  (handler-case
      (sb-cover:report *output-directory*)
    (warning ()
      (error "Unexpected warning"))))

(defun report-expect-failure ()
  (handler-case
      (progn
        (sb-cover:report *output-directory*)
        (error "Should've raised a warning"))
    (warning ())))

;;; No instrumentation
(load (compile-file (merge-pathnames #p"test-data-1.lisp" *path*)))
(report-expect-failure)

;;; Instrument the file, try again -- first with a non-directory pathname

(proclaim '(optimize sb-cover:store-coverage-data))
(load (compile-file (merge-pathnames #p"test-data-1.lisp" *path*)))

(catch 'ok
  (handler-case
      (sb-cover:report #p"/tmp/foo")
    (error ()
      (throw 'ok nil)))
  (error "REPORT with a non-pathname directory did not signal an error."))

(report)

(assert (probe-file (make-pathname :name "cover-index" :type "html"
                                   :defaults *output-directory*)))

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
(load (compile-file (merge-pathnames #p"test-data-2.lisp" *path*)))

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

(load (compile-file (merge-pathnames #p"test-data-3.lisp" *path*)))
(test-2)

;; Clean up after the tests

(map nil #'delete-file
     (directory (merge-pathnames #p"*.html" *output-directory*)))
