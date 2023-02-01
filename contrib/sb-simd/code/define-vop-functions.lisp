(in-package #:sb-simd-internals)

;;; For constant folding, SBCL needs functions of the same name as the VOP.
;;; In this file, we define these functions.  Because some VOPs cannot
;;; translate the full range of arguments supported by such a function,
;;; e.g., because one argument is expected to be a constant, we also need
;;; some macrology to have each function dispatch only to calls that can be
;;; translated.

(defmacro with-primitive-arguments (alist &body body)
  ;; Each entry in ALIST is of the form (VARIABLE VALUE-RECORD-NAME).
  (if (null alist)
      `(progn ,@body)
      `(with-primitive-argument ,(first alist)
         (with-primitive-arguments ,(rest alist)
           ,@body))))

(defmacro with-primitive-argument ((symbol value-record) &body body)
  (with-accessors ((primitive-type value-record-primitive-type)
                   (simd-p simd-record-p))
      (find-value-record value-record)
    (etypecase primitive-type
      ;; Case 1: A symbol denoting a primitive type.
      (symbol
       (let ((alias (find primitive-type sb-c::*backend-primitive-type-aliases* :key #'car)))
         ;; ALIAS is either null or a list of the form (:OR PRIMITIVE-TYPE*)
         (if (or (not simd-p) (not alias))
             `(progn ,@body)
             `(etypecase ,symbol
                ,@(loop for pt in (rest (rest alias))
                        for type = (sb-c::primitive-type-specifier (sb-c:primitive-type-or-lose pt))
                        collect `(,type ,@body))))))
      ;; Case 2: A list of the form (:CONSTANT TYPE), where TYPE is either
      ;; of the form (SIGNED-BYTE N) or (UNSIGNED-BYTE N) for some positive
      ;; integer N.
      ((cons (eql :constant) (cons type-specifier null))
       (multiple-value-bind (low high)
           (integer-type-specifier-inclusive-bounds (second primitive-type))
         `(ecase ,symbol
            ,@(loop for value from low to high
                    collect `(,value (symbol-macrolet ((,symbol ,value)) ,@body)))))))))

(macrolet
    ((define-vop-function (name)
       (with-accessors ((name instruction-record-name)
                        (vop instruction-record-vop)
                        (argument-records instruction-record-argument-records)
                        (encoding instruction-record-encoding)
                        (instruction-set instruction-record-instruction-set))
           (find-function-record name)
         (let* ((argument-record-names (mapcar #'record-name argument-records))
                (argument-symbols (prefixed-symbols "ARGUMENT-" (length argument-records))))
           (unless (or (eq encoding :fake-vop)
                       (not (instruction-set-available-p instruction-set)))
             `(defun ,vop (,@argument-symbols)
                (declare
                 ,@(loop for argument-symbol in argument-symbols
                         for argument-record in argument-records
                         collect `(type ,(value-record-name argument-record) ,argument-symbol)))
                (with-primitive-arguments ,(mapcar #'list argument-symbols argument-record-names)
                  (,vop ,@argument-symbols)))))))

     (define-vop-functions ()
       `(progn
          ,@(loop for instruction-record in (filter-function-records #'instruction-record-p)
                  for name = (instruction-record-name instruction-record)
                  collect `(define-vop-function ,name)))))
  (define-vop-functions))
