(in-package #:sb-simd-internals)

(macrolet
    ((define-vref (name kind)
       (with-accessors ((name vref-record-name)
                        (instruction-set vref-record-instruction-set)
                        (value-record vref-record-value-record)
                        (vector-record vref-record-vector-record)
                        (vop vref-record-vop)
                        (sap vref-record-sap-ref))
           (find-function-record name)
         (let* ((simd-width (value-record-simd-width value-record))
                (element-type
                  (second
                   (value-record-type vector-record)))
                (sap-vop (when sap (mksym (symbol-package name) (if (eq kind :store) "%SET-" "%") sap))))
           (declare (ignorable simd-width element-type))
           (ecase kind
             (:load
              (if (not (instruction-set-available-p instruction-set))
                  `(define-missing-instruction ,name
                     :required-arguments (array index))
                  `(progn
                    (define-inline ,name (array index)
                       (declare (type (array ,element-type) array)
                                (index index))
                       (sb-kernel:check-bound array (array-total-size array) (+ index ,(1- simd-width)))
                       (multiple-value-bind (vector index)
                           (sb-kernel:%data-vector-and-index array index)
                         (declare (type (simple-array ,element-type (*)) vector))
                         (,vop vector index 0)))
                    ,@(when sap
                        `((define-inline ,sap (sap index)
                            (declare (type sb-alien:system-area-pointer sap) (type index index))
                            (,sap-vop sap index 0)))))))
             (:store
              (if (not (instruction-set-available-p instruction-set))
                  `(define-missing-instruction ,name
                     :required-arguments (value array index))
                  `(progn
                    (define-inline ,name (value array index)
                       (declare (type (array ,element-type) array)
                                (index index))
                       (sb-kernel:check-bound array (array-total-size array) (+ index ,(1- simd-width)))
                       (multiple-value-bind (vector index)
                           (sb-kernel:%data-vector-and-index array index)
                         (declare (type (simple-array ,element-type (*)) vector))
                         (,vop (,(value-record-name value-record) value) vector
                               index 0)))
                    ,@(when sap
                        `((define-inline (setf ,sap) (value sap index)
                            (declare (type sb-alien:system-area-pointer sap) (type index index))
                            (,sap-vop (,(value-record-name value-record) value) sap index 0)))))))))))
     (define-vrefs ()
       `(progn
          ,@(loop for load-record in (filter-function-records #'load-record-p)
                  for name = (load-record-name load-record)
                  for sap = (vref-record-sap-ref load-record)
                  collect `(define-vref ,name :load))
          ,@(loop for store-record in (filter-function-records #'store-record-p)
                  for name = (store-record-name store-record)
                  for sap = (vref-record-sap-ref store-record)
                  collect `(define-vref ,name :store)))))
  (define-vrefs))
