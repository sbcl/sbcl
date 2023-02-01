(in-package #:sb-simd-internals)

;;; In this file, we define reffers for scalars that can be defined in
;;; terms of built-in Common Lisp functions.
(macrolet
    ((define-reffers ()
       `(progn
          ,@(loop for record in (filter-function-records
                                 (lambda (function-record)
                                   (eq (symbol-package
                                        (parse-function-name
                                         (function-record-name function-record)))
                                       (find-package "SB-SIMD"))))
                  for name = (function-record-name record)
                  for value-record = (first (function-record-result-records record))
                  for type = (value-record-name value-record)
                  when (aref-record-p record) collect
                    `(progn
                       (defun ,name (array &rest subscripts)
                         (declare (type (array ,type) array))
                         (apply #'aref array subscripts))
                       (define-compiler-macro ,name (array &rest subscripts)
                         `(aref (the (array ,',type) ,array) ,@subscripts)))
                  when (setf-aref-record-p record) collect
                    `(progn
                       (defun ,name (value array &rest subscripts)
                         (declare (type ,type value)
                                  (type (array ,type) array))
                         (setf (apply #'aref array subscripts) value))
                       (define-compiler-macro ,name (value array &rest subscripts)
                         (let ((v (gensym "VALUE")))
                           `(let ((,v ,value))
                              (setf (aref (the (array ,',type) ,array) ,@subscripts)
                                    ,v)))))
                  when (row-major-aref-record-p record) collect
                    `(define-inline ,name (array index)
                       (declare (type (array ,type) array))
                       (row-major-aref array index))
                  when (setf-row-major-aref-record-p record) collect
                    `(define-inline ,name (value array index)
                       (declare (type ,type value)
                                (type (array ,type) array))
                       (setf (row-major-aref array index) value))))))
  (define-reffers))
