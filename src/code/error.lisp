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
  (/show0 "entering STYLE-WARN")
  (/show format-control format-arguments)
  (warn 'simple-style-warning
	:format-control format-control
	:format-arguments format-arguments))

(define-condition sb!kernel:layout-invalid (type-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream
	     "~@<invalid structure layout: ~
              ~2I~_A test for class ~4I~_~S ~
              ~2I~_was passed the obsolete instance ~4I~_~S~:>"
	     (sb!kernel:class-proper-name (type-error-expected-type condition))
	     (type-error-datum condition)))))

(define-condition case-failure (type-error)
  ((name :reader case-failure-name :initarg :name)
   (possibilities :reader case-failure-possibilities :initarg :possibilities))
  (:report
    (lambda (condition stream)
      (format stream "~@<~S fell through ~S expression. ~
                      ~:_Wanted one of ~:S.~:>"
	      (type-error-datum condition)
	      (case-failure-name condition)
	      (case-failure-possibilities condition)))))

(define-condition simple-control-error (simple-condition control-error) ())
(define-condition simple-file-error    (simple-condition file-error)    ())
(define-condition simple-program-error (simple-condition program-error) ())
(define-condition simple-stream-error  (simple-condition stream-error)  ())

;;; This condition is signalled whenever we make a UNKNOWN-TYPE so that
;;; compiler warnings can be emitted as appropriate.
(define-condition parse-unknown-type (condition)
  ((specifier :reader parse-unknown-type-specifier :initarg :specifier)))

(define-condition control-stack-exhausted (storage-condition)
  ()
  (:report
    (lambda (condition stream)
      (format stream
             "Control stack exhausted (no more space for function call frames).  This is probably due to heavily nested or infinitely recursive function calls, or a tail call that SBCL cannot or has not optimized away."))))

