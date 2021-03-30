;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is derived from software originally released by Xerox
;;;; Corporation. Copyright and release statements follow. Later modifications
;;;; to the software are in the public domain and are provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for more
;;;; information.

;;;; copyright information from original PCL sources:
;;;;
;;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;;; All rights reserved.
;;;;
;;;; Use and copying of this software and preparation of derivative works based
;;;; upon this software are permitted. Any distribution of this software or
;;;; derivative works must comply with all applicable United States export
;;;; control laws.
;;;;
;;;; This software is made available AS IS, and Xerox Corporation makes no
;;;; warranty about the software, its performance or its conformity to any
;;;; specification.

(in-package "SB-PCL")

;;; CMUCL comment (Gerd Moellmann):
;;;
;;; The standard says it's an error if CALL-NEXT-METHOD is called with
;;; arguments, and the set of methods applicable to those arguments is
;;; different from the set of methods applicable to the original
;;; method arguments.  (According to Barry Margolin, this rule was
;;; probably added to ensure that before and around methods are always
;;; run before primary methods.)
;;;
;;; This could be optimized for the case that the generic function
;;; doesn't have hairy methods, does have standard method combination,
;;; is a standard generic function, there are no methods defined on it
;;; for COMPUTE-APPLICABLE-METHODS and probably a lot more of such
;;; preconditions.  That looks hairy and is probably not worth it,
;;; because this check will never be fast.

;;; Or maybe it will (Jan Moringen):
;;;
;;; The "CALL-NEXT-METHOD argument checker" is a generic function
;;; which has twice as many required parameters as the original
;;; generic function. Consider, for example
;;;
;;;   (defgeneric foo (bar baz &key fez))
;;;
;;; The cnm args checker for this generic function is a generic
;;; function roughly equivalent to
;;;
;;;   (defgeneric cnm-args-checker-for-foo (old-bar old-baz new-bar new-baz)
;;;     (:generic-function-class cnm-args-checker))
;;;
;;; The cnm args checker is applied to the concatenation of the
;;; original arguments and the arguments supplied to
;;; CALL-NEXT-METHOD:
;;;
;;;   ;; In the expansion of (call-next-method new-bar new-bar ...):
;;;   (when (funcall CNM-ARGS-CHECKER-FOR-FOO
;;;                  old-bar old-baz new-bar new-baz)
;;;     (error "This list of applicable methods ... differs."))
;;;
;;; A cnm args checker initially does not have any methods. When it is
;;; called with a particular sequence of arguments and does not have
;;; an applicable method for these arguments, it computes the
;;; applicable methods /of the original generic function/ for the old
;;; arguments and for the new arguments and adds a method that
;;; immediately returns the computed result when the same (in terms of
;;; CLOS dispatch) combination appears again.
;;;
;;; The cnm args checker must be invalidated when the results cached
;;; in its defined methods become invalid.

(defun %check-cnm-args (cnm-args orig-args method-cell)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type list cnm-args orig-args))
  ;; 1. Check for no arguments.
  (when cnm-args
    (let* ((gf (method-generic-function (car method-cell)))
           (nreq (generic-function-nreq gf)))
      (declare (type (integer 0 #.call-arguments-limit) nreq))
      ;; 2. Consider required arguments pairwise: if the old and new
      ;; arguments in all pairs are EQL, the applicable methods must
      ;; be the same. This takes care of the relatively common case of
      ;; twiddling with &KEY arguments.
      (unless (do ((orig orig-args (cdr orig))
                   (args cnm-args (cdr args))
                   (n nreq (1- n)))
                  ((zerop n) t)
                (declare (type (integer 0 #.call-arguments-limit) n))
                (unless (eql (car orig) (car args))
                  (return nil)))
        ;; 3. Only then make a cnm args checker and do the full check.
        ;; Disabled until problems with EQL specializers and method
        ;; "shadowing" are worked out.
        #+(or) (let ((result (%use-cnm-checker gf nreq cnm-args orig-args)))
                 (when result
                   (destructuring-bind (cnm-methods . orig-methods) result
                     (error "~@<The set of methods ~S applicable to argument~P ~
                             ~{~S~^, ~} to call-next-method is different from ~
                             the set of methods ~S applicable to the original ~
                             method argument~P ~{~S~^, ~}.~@:>"
                            cnm-methods (length cnm-args) cnm-args
                            orig-methods (length orig-args) orig-args))))
        (let ((orig-methods (compute-applicable-methods gf orig-args))
              (cnm-methods (compute-applicable-methods gf cnm-args)))
          (unless (equal orig-methods cnm-methods)
           (error "~@<The set of methods ~S applicable to argument~P ~
                   ~{~S~^, ~} to call-next-method is different from ~
                   the set of methods ~S applicable to the original ~
                   method argument~P ~{~S~^, ~}.~@:>"
                   cnm-methods (length cnm-args) cnm-args
                   orig-methods (length orig-args) orig-args)))))))

;;; CALL-NEXT-METHOD argument checker application

(defun %use-cnm-checker (generic-function nreq cnm-args orig-args)
  (declare (type (integer 1 #.call-arguments-limit) nreq))
  (let* ((info (gf-arg-info generic-function))
         ;; Setting GF-INFO-CNM-CHECKER is racy but should be OK.
         (checker (or (gf-info-cnm-checker info)
                      (setf (gf-info-cnm-checker info)
                            (%make-cnm-checker generic-function))))
         (args (make-list (* 2 nreq))))
    (declare (dynamic-extent args))
    ;; Construct the concatenation of the required arguments in
    ;; ORIG-ARGS and CNM-ARGS in ARGS.
    (loop repeat nreq
          for rest1 on args
          for arg in orig-args
          do (setf (car rest1) arg)
          finally (loop for rest2 on (rest rest1)
                        for arg in cnm-args
                        do (setf (car rest2) arg)))
    (apply checker args)))

(defun %cnm-checker-lambda-list (nreq)
  (append (map-into (make-list (* 2 nreq)) #'gensym) '(&rest rest)))

;;; CALL-NEXT-METHOD argument checker implementation

;;; The eval-when is due to a deficiency in compile-time handling of DEFCLASS which
;;; doesn't make the new class accessible to the optimizer for MAKE-INSTANCE with a
;;; quoted symbol as the argument. But we explicitly call pass ERROR = T in
;;; (SB-PCL::FIND-CLASS-FROM-CELL STREAM-FUNCTION NIL T) dynamically within a
;;; CLASS-NOT-FOUND handler. What the fsck? How about don't pass ERRORP = T ?
(eval-when (:compile-toplevel :load-toplevel :execute)
(defclass cnm-args-checker (standard-generic-function)
  ((%generic-function :initarg :generic-function
                      :reader cnm-args-checker-generic-function))
  (:metaclass funcallable-standard-class)))

(defun %make-cnm-checker (generic-function)
  (let ((nreq (generic-function-nreq generic-function)))
    (make-instance 'cnm-args-checker
                   :name nil
                   :lambda-list (%cnm-checker-lambda-list nreq)
                   :generic-function generic-function)))

(defmethod no-applicable-method ((generic-function cnm-args-checker)
                                 &rest args)
  ;; Construct a method for GENERIC-FUNCTION that, when applied to
  ;; ARGS, returns NIL (which indicates "fno error") if the
  ;; CALL-NEXT-METHOD call is fine for the generic function
  ;; (cnm-args-checker-generic-function generic-function) and a cons
  ;; (ORIG-METHODS . CNM-METHODS) otherwise.
  (let* ((original-generic-function (cnm-args-checker-generic-function
                                     generic-function))
         (nreq (generic-function-nreq original-generic-function))
         (orig-args (subseq args 0 nreq))
         (orig-methods (compute-applicable-methods
                        original-generic-function orig-args))
         (cnm-args (subseq args nreq))
         (cnm-methods (compute-applicable-methods
                       original-generic-function cnm-args))
         (result (if (equal orig-methods cnm-methods)
                     nil
                     (cons orig-methods cnm-methods)))
         (lambda-list (%cnm-checker-lambda-list nreq))
         (lambda (make-method-lambda
                  generic-function
                  (class-prototype (generic-function-method-class
                                    generic-function))
                  `(lambda ,lambda-list
                     (declare (ignore ,@(remove '&rest lambda-list))
                              (optimize (speed 3) (debug 0) (safety 0)))
                     ',result)
                  nil))
         (function (compile nil lambda))
         (specializers (append (method-specializers (first orig-methods))
                               (method-specializers (first cnm-methods))))
         (method (make-instance 'standard-method
                                :qualifiers   '()
                                :lambda-list  lambda-list
                                :specializers specializers
                                :function     function)))
    (add-method generic-function method)
    (apply generic-function args)))
