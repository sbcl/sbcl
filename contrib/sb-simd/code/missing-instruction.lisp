(in-package #:sb-simd-internals)

;;; Our library always defines all instruction sets and instructions, but
;;; usually, only a subset of them are actually available.  All the
;;; remaining instructions are replaced by functions that signal a suitable
;;; error message.

(define-condition missing-instruction (error)
  ((%record
    :initarg :record
    :reader missing-instruction-record))
  (:report
   (lambda (c s)
     (with-accessors ((instruction-set instruction-record-instruction-set)
                      (instruction-name instruction-record-name))
         (missing-instruction-record c)
       (format s "Missing ~S instruction ~S."
               (instruction-set-name instruction-set)
               instruction-name)))))

(defun missing-instruction (instruction-record)
  (error 'missing-instruction :record instruction-record))

(defmacro define-missing-instruction
    (name &key (required-arguments '()) (optional-arguments '()) (rest-argument nil))
  (assert (find-function-record name))
  `(defun ,name (,@required-arguments
                 ,@optional-arguments
                 ,@(when rest-argument `(&rest ,rest-argument)))
     (declare (ignore ,@required-arguments
                      ,@optional-arguments
                      ,@(when rest-argument `(,rest-argument))))
     (missing-instruction
      (load-time-value
       (find-function-record ',name)))))
