(in-package #:sb-simd-internals)

(macrolet
    ((define-types ()
       `(progn
          ,@(loop for value-record being the hash-values of *value-records*
                  collect
                  `(deftype ,(value-record-name value-record) ()
                     ',(value-record-type value-record))))))
  (define-types))
