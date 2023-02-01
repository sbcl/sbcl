(in-package #:sb-simd-internals)

(macrolet
    ((define-unequal (unequal-record-name)
       (with-accessors ((name unequal-record-name)
                        (neq unequal-record-neq)
                        (and unequal-record-and)
                        (truth unequal-record-truth))
           (find-function-record unequal-record-name)
         (with-accessors ((neq instruction-record-name)
                          (result-records instruction-record-result-records)
                          (argument-records instruction-record-argument-records)) neq
           (destructuring-bind ((result-record) (argument-record other-argument-record))
               (list result-records argument-records)
             (assert (eq argument-record other-argument-record))
             (let ((and (function-record-name and))
                   (result-type (value-record-name result-record))
                   (argument-type (value-record-name argument-record)))
               `(progn
                  (defun ,name (arg &rest more-args)
                    (let ((args (list* (,argument-type arg) (mapcar #',argument-type more-args)))
                          (result (,result-type ,truth)))
                      (declare (,result-type result))
                      (loop for (a . rest) on args do
                        (loop for b in rest do
                          (setf result (,and result (,neq a b)))))
                      result))
                  (define-compiler-macro ,name (arg &rest more-args)
                    (if (null more-args)
                        `(progn (,',argument-type ,arg) (,',result-type ,',truth))
                        (let ((bindings
                                (loop for arg in (list* arg more-args)
                                      collect
                                      (list (gensym "ARG") (list ',argument-type arg)))))
                          `(let ,bindings
                             (,',and
                              ,@(loop for ((a nil) . rest) on bindings
                                      append
                                      (loop for (b nil) in rest
                                            collect `(,',neq ,a ,b))))))))))))))
     (define-unequals ()
       `(progn
          ,@(loop for unequal-record in (filter-function-records #'unequal-record-p)
                  collect
                  `(define-unequal ,(unequal-record-name unequal-record))))))
  (define-unequals))
