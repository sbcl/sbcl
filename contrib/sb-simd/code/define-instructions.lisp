(in-package #:sb-simd-internals)

(macrolet
    ((define-instruction (name)
       (with-accessors ((name instruction-record-name)
                        (vop instruction-record-vop)
                        (argument-records instruction-record-argument-records)
                        (encoding instruction-record-encoding)
                        (instruction-set instruction-record-instruction-set))
           (find-function-record name)
         (let ((argument-record-names (mapcar #'record-name argument-records))
               (argument-symbols (prefixed-symbols "ARGUMENT-" (length argument-records))))
           (if (not (instruction-set-available-p instruction-set))
               `(define-missing-instruction ,name
                  :required-arguments ,argument-symbols)
               ;; Define the actual instruction as a wrapper around the VOP
               ;; that attempts to cast all arguments to the correct types.
               `(define-inline ,name ,argument-symbols
                  (let ,(loop for argument-symbol in argument-symbols
                              for type in (mapcar #'value-record-name argument-records)
                              collect `(,argument-symbol (,type ,argument-symbol)))
                    (with-primitive-arguments
                        ,(mapcar #'list argument-symbols argument-record-names)
                      (,vop ,@argument-symbols))))))))
     (define-instructions ()
       `(progn
          ,@(loop for instruction-record in (filter-function-records #'instruction-record-p)
                  for name = (instruction-record-name instruction-record)
                  collect `(define-instruction ,name)))))
  (define-instructions))
