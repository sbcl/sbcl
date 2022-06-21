(in-package #:sb-simd-internals)

(macrolet
    ((define-comparison (comparision-record-name)
       (with-accessors ((name comparison-record-name)
                        (cmp comparison-record-cmp)
                        (and comparison-record-and)
                        (truth comparison-record-truth))
           (find-function-record comparision-record-name)
         (with-accessors ((cmp instruction-record-name)
                          (result-records instruction-record-result-records)
                          (argument-records instruction-record-argument-records)) cmp
           (destructuring-bind ((result-record) (argument-record other-argument-record))
               (list result-records argument-records)
             (assert (eq argument-record other-argument-record))
             (let ((and (function-record-name and))
                   (result-type (value-record-name result-record))
                   (argument-type (value-record-name argument-record)))
               `(progn
                  (defun ,name (arg &rest more-args)
                    (if (null more-args)
                        (progn (,argument-type arg) (,result-type ,truth))
                        (let* ((a (,argument-type arg))
                               (b (,argument-type (first more-args)))
                               (result (,cmp a b)))
                          (declare (,argument-type a b)
                                   (,result-type result))
                          (loop for elt in (rest more-args)
                                do (shiftf a b (,argument-type elt))
                                do (setf result (,and result (,cmp a b))))
                          result)))
                  (define-compiler-macro ,name (arg &rest more-args)
                    (if (null more-args)
                        `(progn (,',argument-type ,arg) (,',result-type ,',truth))
                        (let ((bindings
                                (loop for arg in (list* arg more-args)
                                      collect
                                      (list (gensym "ARG") (list ',argument-type arg)))))
                          `(let ,bindings
                             (,',and ,@(loop for ((a nil) (b nil) . rest) on bindings
                                             collect `(,',cmp ,a ,b)
                                             until (null rest)))))))))))))
     (define-comparisons ()
       `(progn
          ,@(loop for comparison-record in (filter-function-records #'comparison-record-p)
                  collect `(define-comparison ,(comparison-record-name comparison-record))))))
  (define-comparisons))
