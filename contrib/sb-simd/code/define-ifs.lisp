(in-package #:sb-simd-internals)

(macrolet
    ((define-if (if-record-name)
       (with-accessors ((name if-record-name)
                        (blend if-record-blend))
           (find-function-record if-record-name)
         (with-accessors ((blend instruction-record-name)
                          (result-records instruction-record-result-records)
                          (argument-records instruction-record-argument-records)) blend
           (destructuring-bind (a-record b-record mask-record) argument-records
             (assert (eq a-record b-record))
             (assert (= (value-record-bits a-record)
                        (value-record-bits mask-record)))
             (destructuring-bind (result-record) result-records
               (assert (eq result-record a-record))
               (let ((value-type (value-record-name a-record))
                     (mask-type (value-record-name mask-record)))
                 `(define-inline ,name (mask a b)
                    (the ,value-type
                         (,blend (,value-type b)
                                 (,value-type a)
                                 (,mask-type mask))))))))))
     (define-ifs ()
       `(progn
          ,@(loop for if-record in (filter-function-records #'if-record-p)
                  collect `(define-if ,(if-record-name if-record))))))
  (define-ifs))
