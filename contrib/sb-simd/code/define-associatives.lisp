(in-package #:sb-simd-internals)

(macrolet
    ((define-associative (associative-record-name)
       (with-accessors ((name associative-record-name)
                        (binary-operation associative-record-binary-operation)
                        (identity-element associative-record-identity-element))
           (find-function-record associative-record-name)
         (with-accessors ((binary-operation instruction-record-name)
                          (result-records instruction-record-result-records)
                          (argument-records instruction-record-argument-records)
                          (associative instruction-record-associative)) binary-operation
           (assert associative)
           (destructuring-bind ((value-record) (arg1-record arg2-record))
               (list result-records argument-records)
             (assert (eq value-record arg1-record))
             (assert (eq value-record arg2-record))
             (let ((type (value-record-name value-record)))
               (if (not identity-element)
                   `(progn
                      (defun ,name (arg &rest more-args)
                        (let ((result (,type arg)))
                          (declare (,type result))
                          (loop for arg in more-args
                                do (setf result (,binary-operation result (,type arg))))
                          result))
                      (define-compiler-macro ,name (&whole whole &rest args)
                        (let ((n (length args)))
                          (case n
                            (0 whole)
                            (1 `(,',type ,(first args)))
                            (otherwise
                             `(,',binary-operation
                               (,',name ,@(subseq args 0 (floor n 2)))
                               (,',name ,@(subseq args (floor n 2)))))))))
                   `(progn
                      (defun ,name (&rest args)
                        (if (null args)
                            (,type ,identity-element)
                            (let ((result (,type (first args))))
                              (declare (,type result))
                              (loop for arg in (rest args)
                                    do (setf result (,binary-operation result (,type arg))))
                              result)))
                      (define-compiler-macro ,name (&rest args)
                        (let ((n (length args)))
                          (case n
                            (0 `(,',type ,,identity-element))
                            (1 `(,',type ,(first args)))
                            (otherwise
                             `(,',binary-operation
                               (,',name ,@(subseq args 0 (floor n 2)))
                               (,',name ,@(subseq args (floor n 2)))))))))))))))
     (define-associatives ()
       `(progn
          ,@(loop for associative-record in (filter-function-records #'associative-record-p)
                  collect `(define-associative ,(associative-record-name associative-record))))))
  (define-associatives))
