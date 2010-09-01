(defpackage :test-util
  (:use :cl :sb-ext)
  (:export #:with-test #:report-test-status #:*failures*
           #:really-invoke-debugger
           #:*break-on-failure* #:*break-on-expected-failure*))

(in-package :test-util)

(defvar *test-count* 0)
(defvar *test-file* nil)
(defvar *failures* nil)
(defvar *break-on-failure* nil)
(defvar *break-on-expected-failure* nil)

(defun log-msg (&rest args)
  (format *trace-output* "~&::: ")
  (apply #'format *trace-output* args)
  (terpri *trace-output*)
  (force-output *trace-output*))

(defmacro with-test ((&key fails-on name) &body body)
  (let ((block-name (gensym)))
    `(block ,block-name
       (handler-bind ((error (lambda (error)
                               (if (expected-failure-p ,fails-on)
                                   (fail-test :expected-failure ',name error)
                                   (fail-test :unexpected-failure ',name error))
                               (return-from ,block-name))))
         (progn
           (log-msg "Running ~S" ',name)
           (start-test)
           ,@body
           (if (expected-failure-p ,fails-on)
               (fail-test :unexpected-success ',name nil)
               (log-msg "Success ~S" ',name)))))))

(defun report-test-status ()
  (with-standard-io-syntax
      (with-open-file (stream "test-status.lisp-expr"
                              :direction :output
                              :if-exists :supersede)
        (format stream "~s~%" *failures*))))

(defun start-test ()
  (unless (eq *test-file* *load-pathname*)
    (setf *test-file* *load-pathname*)
    (setf *test-count* 0))
  (incf *test-count*))

(defun fail-test (type test-name condition)
  (log-msg "~@<~A ~S ~:_due to ~S: ~4I~:_\"~A\"~:>"
           type test-name condition condition)
  (push (list type *test-file* (or test-name *test-count*))
        *failures*)
  (when (or (and *break-on-failure*
                 (not (eq type :expected-failure)))
            *break-on-expected-failure*)
    (really-invoke-debugger condition)))

(defun expected-failure-p (fails-on)
  (sb-impl::featurep fails-on))

(defun really-invoke-debugger (condition)
  (with-simple-restart (continue "Continue")
    (let ((*invoke-debugger-hook* *invoke-debugger-hook*))
      (enable-debugger)
      (invoke-debugger condition))))

(defun test-env ()
  (cons (format nil "SBCL_MACHINE_TYPE=~A" (machine-type))
        (cons (format nil "SBCL_SOFTWARE_TYPE=~A" (software-type))
              (posix-environ))))
