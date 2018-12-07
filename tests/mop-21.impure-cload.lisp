;;;; miscellaneous side-effectful tests of the MOP

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

;;; Pascal Costanza's implementation of beta methods, lightly
;;; modified.  Contains a specialization of MAKE-METHOD-LAMBDA.

(defclass beta-generic-function (standard-generic-function)
  ()
  (:metaclass sb-mop:funcallable-standard-class))

(defclass beta-method (standard-method)
  ((betap :reader betap :initarg :betap :initform nil)))

(defmethod initialize-instance :around
    ((method beta-method) &rest initargs &key qualifiers)
  (declare (dynamic-extent initargs))
  (if (equal qualifiers '(:beta))
      (apply #'call-next-method method
             :qualifiers ()
             :betap t
             initargs)
      (call-next-method)))

(defun collect-runs (methods)
  (let ((complete-runs nil)
        (current-run nil))
    (flet ((complete-run ()
             (when current-run
               (push (nreverse current-run) complete-runs)
               (setf current-run nil))))
      (loop for method in methods with seen-beta = nil do
            (when (betap method)
              (if seen-beta
                  (complete-run)
                  (setq seen-beta t current-run nil)))
            (push method current-run))
      (complete-run))
    complete-runs))

(define-method-combination beta ()
  ((around (:around))
   (before (:before))
   (primary () :required t)
   (after (:after)))
  (flet ((call-methods (methods)
           (mapcar (lambda (method) `(call-method ,method)) methods)))
    (let ((form (if (or before after (rest primary))
                  (let ((runs (collect-runs primary)))
                    `(multiple-value-prog1
                         (progn
                           ,@(call-methods before)
                           (call-method ,(first (first runs))
                                        ,(rest (first runs))
                                        ,(rest runs)))
                      ,@(call-methods (reverse after))))
                  `(call-method ,(first primary)))))
      (if around
          `(call-method ,(first around) (,@(rest around) (make-method ,form)))
          form))))

(defmethod sb-mop:make-method-lambda
    ((gf beta-generic-function) method-prototype lambda-expression environment)
  (declare (ignore method-prototype environment))
  (let ((method-args (gensym))
        (next-methods (gensym))
        (inner-runs (gensym)))
    `(lambda (,method-args &optional ,next-methods ,inner-runs)
       (declare (ignorable ,next-methods ,inner-runs))
       (flet ((call-next-method (&rest args)
                (declare (dynamic-extent args))
                (if (null ,next-methods)
                    (error "There is no next method for ~S." ,gf)
                    (funcall (sb-mop:method-function (car ,next-methods))
                             (if args args ,method-args)
                             (cdr ,next-methods)
                             ,inner-runs)))
              (next-method-p () (not (null ,next-methods)))
              (call-inner-method (&rest args)
                (declare (dynamic-extent args))
                (if (null ,inner-runs)
                    (error "There is no inner method for ~S." ,gf)
                    (funcall (sb-mop:method-function (caar ,inner-runs))
                             (if args args ,method-args)
                             (cdar ,inner-runs)
                             (cdr ,inner-runs))))
              (inner-method-p () (not (null ,inner-runs))))
         (declare (ignorable #'call-next-method #'next-method-p
                             #'call-inner-method #'inner-method-p))
         (apply ,lambda-expression ,method-args)))))

(defmacro define-beta-function (name (&rest args) &rest options)
  `(defgeneric ,name ,args
     ,@(unless (member :generic-function-class options :key #'car)
         '((:generic-function-class beta-generic-function)))
     ,@(unless (member :method-class options :key #'car)
         '((:method-class beta-method)))
     ,@(unless (member :method-combination options :key #'car)
         '((:method-combination beta)))
     ,@options))

(defclass top () ())
(defclass middle (top) ())
(defclass bottom (middle) ())

(define-beta-function test (object))

;;; MAKE-METHOD-LAMBDA acts at (DEFMETHOD-)expand-time, which is
;;; before DEFCLASS- and DEFGENERIC-load-time.
(mapcar #'eval
        (list
         '(defmethod test ((object top))
           (declare (ignore object))
           'top)
         '(defmethod test :beta ((object middle))
           (declare (ignore object))
           (list 'middle (call-inner-method) (call-next-method)))
         '(defmethod test :beta ((object bottom))
           (declare (ignore object))
           'bottom)))

(with-test (:name (:mop-21))
  (assert (equal '(middle bottom top) (test (make-instance 'bottom))))
  (assert (equal 'top (test (make-instance 'top))))
  (assert (null (ignore-errors (test (make-instance 'middle))))))
