;;;; error-handling machinery for PARSE-DEFMACRO, separated from
;;;; PARSE-DEFMACRO code itself because the happy path can be handled
;;;; earlier in the bootstrap sequence than DEFINE-CONDITION can be

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;; We save space in macro definitions by calling this function.
(defun arg-count-error (error-kind name arg lambda-list minimum maximum)
  (let (#-sb-xc-host
	(sb!debug:*stack-top-hint* (nth-value 1 (find-caller-name-and-frame))))
    (error 'arg-count-error
	   :kind error-kind
	   :name name
	   :argument arg
	   :lambda-list lambda-list
	   :minimum minimum :maximum maximum)))

(define-condition defmacro-lambda-list-bind-error (error)
  ((kind :reader defmacro-lambda-list-bind-error-kind
	 :initarg :kind)
   (name :reader defmacro-lambda-list-bind-error-name
	 :initarg :name
	 :initform nil)))

(defun print-defmacro-ll-bind-error-intro (condition stream)
  (format stream
	  "error while parsing arguments to ~A~@[ ~S~]:~%"
	  (defmacro-lambda-list-bind-error-kind condition)
	  (defmacro-lambda-list-bind-error-name condition)))

(define-condition defmacro-bogus-sublist-error
		  (defmacro-lambda-list-bind-error)
  ((object :reader defmacro-bogus-sublist-error-object :initarg :object)
   (lambda-list :reader defmacro-bogus-sublist-error-lambda-list
		:initarg :lambda-list))
  (:report
   (lambda (condition stream)
     (print-defmacro-ll-bind-error-intro condition stream)
     (format stream
	     "bogus sublist:~%  ~S~%to satisfy lambda-list:~%  ~:S~%"
	     (defmacro-bogus-sublist-error-object condition)
	     (defmacro-bogus-sublist-error-lambda-list condition)))))

(define-condition arg-count-error (defmacro-lambda-list-bind-error)
  ((argument :reader arg-count-error-argument :initarg :argument)
   (lambda-list :reader arg-count-error-lambda-list
		:initarg :lambda-list)
   (minimum :reader arg-count-error-minimum :initarg :minimum)
   (maximum :reader arg-count-error-maximum :initarg :maximum))
  (:report
   (lambda (condition stream)
     (print-defmacro-ll-bind-error-intro condition stream)
     (format stream
	     "invalid number of elements in:~%  ~:S~%~
	     to satisfy lambda list:~%  ~:S~%"
	     (arg-count-error-argument condition)
	     (arg-count-error-lambda-list condition))
     (cond ((null (arg-count-error-maximum condition))
	    (format stream "at least ~W expected"
		    (arg-count-error-minimum condition)))
	   ((= (arg-count-error-minimum condition)
	       (arg-count-error-maximum condition))
	    (format stream "exactly ~W expected"
		    (arg-count-error-minimum condition)))
	   (t
	    (format stream "between ~W and ~W expected"
		    (arg-count-error-minimum condition)
		    (arg-count-error-maximum condition))))
     (format stream ", but ~W found"
	     (length (arg-count-error-argument condition))))))

(define-condition defmacro-ll-broken-key-list-error
		  (defmacro-lambda-list-bind-error)
  ((problem :reader defmacro-ll-broken-key-list-error-problem
	    :initarg :problem)
   (info :reader defmacro-ll-broken-key-list-error-info :initarg :info))
  (:report (lambda (condition stream)
	     (print-defmacro-ll-bind-error-intro condition stream)
	     (format stream
		     ;; FIXME: These should probably just be three
		     ;; subclasses of the base class, so that we don't
		     ;; need to maintain the set of tags both here and
		     ;; implicitly wherever this macro is used.
		     (ecase
			 (defmacro-ll-broken-key-list-error-problem condition)
		       (:dotted-list
			"dotted keyword/value list: ~S")
		       (:odd-length
			"odd number of elements in keyword/value list: ~S")
		       (:unknown-keyword
			"~{unknown keyword: ~S; expected one of ~{~S~^, ~}~}"))
		     (defmacro-ll-broken-key-list-error-info condition)))))
