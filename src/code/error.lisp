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

;;; not sure this is the right place, but where else?
(defun style-warn (format-control &rest format-arguments)
  (/show0 "entering STYLE-WARN")
  (/show format-control format-arguments)
  (with-sane-io-syntax
      (warn 'simple-style-warning
            :format-control format-control
            :format-arguments format-arguments)))

;;; a utility for SIGNAL, ERROR, CERROR, WARN, COMPILER-NOTIFY and
;;; INVOKE-DEBUGGER: Parse the hairy argument conventions into a
;;; single argument that's directly usable by all the other routines.
(defun coerce-to-condition (datum arguments default-type fun-name)
  (cond ((typep datum 'condition)
	 (when (and arguments (not (eq fun-name 'cerror)))
           (cerror "Ignore the additional arguments."
                   'simple-type-error
                   :datum arguments
                   :expected-type 'null
                   :format-control "You may not supply additional arguments ~
                                    when giving ~S to ~S."
                   :format-arguments (list datum fun-name)))
	 datum)
	((symbolp datum) ; roughly, (SUBTYPEP DATUM 'CONDITION)
	 (apply #'make-condition datum arguments))
	((or (stringp datum) (functionp datum))
	 (make-condition default-type
			 :format-control datum
			 :format-arguments arguments))
	(t
	 (error 'simple-type-error
		:datum datum
		:expected-type '(or symbol string)
		:format-control "bad argument to ~S: ~S"
		:format-arguments (list fun-name datum)))))

(define-condition layout-invalid (type-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream
	     "~@<invalid structure layout: ~
              ~2I~_A test for class ~4I~_~S ~
              ~2I~_was passed the obsolete instance ~4I~_~S~:>"
	     (classoid-proper-name (type-error-expected-type condition))
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

(define-condition compiled-program-error (program-error)
  ((message :initarg :message :reader program-error-message)
   (source :initarg :source :reader program-error-source))
  (:report (lambda (condition stream)
	     (format stream "Execution of a form compiled with errors.~%~
                             Form:~%  ~A~%~
                             Compile-time-error:~%  ~A"
		       (program-error-source condition)
		       (program-error-message condition)))))

(define-condition simple-control-error (simple-condition control-error) ())
(define-condition simple-file-error    (simple-condition file-error)    ())
(define-condition simple-program-error (simple-condition program-error) ())
(define-condition simple-stream-error  (simple-condition stream-error)  ())
(define-condition simple-parse-error   (simple-condition parse-error)   ())

(define-condition control-stack-exhausted (storage-condition)
  ()
  (:report
    (lambda (condition stream)
      (declare (ignore condition))
      (format stream
             "Control stack exhausted (no more space for function call frames).  This is probably due to heavily nested or infinitely recursive function calls, or a tail call that SBCL cannot or has not optimized away."))))

