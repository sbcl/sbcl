(in-package #:sb-simd-internals)

(macrolet
    ((define-vref (name kind)
       (with-accessors ((name vref-record-name)
                        (instruction-set vref-record-instruction-set)
                        (value-record vref-record-value-record)
                        (vector-record vref-record-vector-record)
                        (vop vref-record-vop))
           (find-function-record name)
         (let* ((simd-width (value-record-simd-width value-record))
                (element-type
                  (second
                   (value-record-type vector-record))))
           (ecase kind
             (:load
              `(define-inline ,name (array index)
                 (declare (type (array ,element-type) array)
                          (index index))
                 (sb-kernel:check-bound array (array-total-size array) (+ index ,(1- simd-width)))
                 (multiple-value-bind (vector index)
                     (sb-kernel:%data-vector-and-index array index)
                   (declare (type (simple-array ,element-type (*)) vector))
                   (,vop vector index 0))))
             (:store
              `(define-inline ,name (value array index)
                 (declare (type (array ,element-type) array)
                          (index index))
                 (sb-kernel:check-bound array (array-total-size array) (+ index ,(1- simd-width)))
                 (multiple-value-bind (vector index)
                     (sb-kernel:%data-vector-and-index array index)
                   (declare (type (simple-array ,element-type (*)) vector))
                   (,vop (,(value-record-name value-record) value) vector index 0))))))))
     (define-vrefs ()
       `(progn
          ,@(loop for load-record in (filter-function-records #'load-record-p)
                  for name = (load-record-name load-record)
                  collect `(define-vref ,name :load))
          ,@(loop for store-record in (filter-function-records #'store-record-p)
                  for name = (store-record-name store-record)
                  collect `(define-vref ,name :store)))))
  (define-vrefs))
