(in-package #:sb-simd-internals)

(macrolet
    ((define-reducer (reducer-record-name)
       (with-accessors ((name reducer-record-name)
                        (binary-operation reducer-record-binary-operation)
                        (initial-element reducer-record-initial-element))
           (find-function-record reducer-record-name)
         (with-accessors ((binary-operation instruction-record-name)
                          (result-records instruction-record-result-records)
                          (argument-records instruction-record-argument-records)) binary-operation
           (destructuring-bind ((value-record) (arg1-record arg2-record))
               (list result-records argument-records)
             (assert (eq value-record arg1-record))
             (assert (eq value-record arg2-record))
             (let ((type (value-record-name value-record)))
               `(progn
                  (defun ,name (arg &rest more-args)
                    (if (null more-args)
                        (,binary-operation (,type ,initial-element) (,type arg))
                        (let ((result (,type arg)))
                          (declare (,type result))
                          (loop for arg in more-args
                                do (setf result (,binary-operation result (,type arg))))
                          result)))
                  (define-compiler-macro ,name (arg &rest more-args)
                    (cond ((null more-args)
                           `(,',binary-operation ,',initial-element (,',type ,arg)))
                          (t (reduce
                              (lambda (a b) `(,',binary-operation (,',type ,a) (,',type ,b)))
                              more-args
                              :initial-value `(,',type ,arg)))))))))))
     (define-reducers ()
       `(progn
          ,@(loop for reducer-record in (filter-function-records #'reducer-record-p)
                  collect `(define-reducer ,(reducer-record-name reducer-record))))))
  (define-reducers))
