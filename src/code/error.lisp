;;;; SBCL-specific parts of the condition system, i.e. parts which
;;;; don't duplicate/clobber functionality already provided by the
;;;; cross-compilation host Common Lisp

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

(define-condition simple-style-warning (simple-condition style-warning) ())

;;; not sure this is the right place, but where else?
(defun style-warn (format-control &rest format-arguments)
  (warn 'simple-style-warning
	:format-control format-control
	:format-arguments format-arguments))

(define-condition simple-type-error (simple-condition type-error) ())

(define-condition sb!kernel:layout-invalid (type-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "Layout-invalid error in ~S:~@
		     Type test of class ~S was passed obsolete instance:~%  ~S"
	     (condition-function-name condition)
	     (sb!kernel:class-proper-name (type-error-expected-type condition))
	     (type-error-datum condition)))))

(define-condition case-failure (type-error)
  ((name :reader case-failure-name :initarg :name)
   (possibilities :reader case-failure-possibilities :initarg :possibilities))
  (:report
    (lambda (condition stream)
      (format stream "~@<~S fell through ~S expression. ~:_Wanted one of ~:S.~:>"
	      (type-error-datum condition)
	      (case-failure-name condition)
	      (case-failure-possibilities condition)))))

(define-condition simple-file-error    (simple-condition file-error)())
(define-condition simple-program-error (simple-condition program-error)())
(define-condition simple-control-error (simple-condition control-error)())

;;; This condition is signalled whenever we make a UNKNOWN-TYPE so that
;;; compiler warnings can be emitted as appropriate.
(define-condition parse-unknown-type (condition)
  ((specifier :reader parse-unknown-type-specifier :initarg :specifier)))

