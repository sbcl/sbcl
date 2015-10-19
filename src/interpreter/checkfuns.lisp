;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-INTERPRETER")

;;; The same as TYPE-ERROR but also show the variable name.
(define-condition interpreter-type-error (type-error)
  ((operation :reader type-error-operation :initarg :operation)
   (symbol :reader type-error-symbol :initarg :symbol))
  (:report
   (lambda (condition stream)
     (format stream
             (ecase (type-error-operation condition)
               (write "~@<The value ~S for ~A is not of type ~2I~_~S.~:>")
               (read "~@<The value of ~*~A, ~0@*~S, is not of type ~2I~_~*~S.~:>"))
             (type-error-datum condition)
             (type-error-symbol condition)
             (type-error-expected-type condition)))))

;;; We need a bidirectional mapping between "important" CTYPEs and
;;; functions that efficiently test the type, so that we aren't forced
;;; to redundantly store both.
;;; For example if X is declared of type INTEGER and we represented
;;; that as #'INTEGERP, it should be possible to locally declare
;;; that X is (MOD n) and compute that the intersection is just (MOD n),
;;; which requires that we know that #'INTEGERP is the test for INTEGER.

(defun type-checker (type)
  (let ((checkable-type (checkable-type type)))
    (cond ((type= checkable-type (specifier-type 'fixnum)) #'fixnump)
          ((type= checkable-type (specifier-type 'integer)) #'integerp)
          ((type= checkable-type (specifier-type 'number)) #'numberp)
          ((type= checkable-type (specifier-type 'list)) #'listp)
          ((type= checkable-type (specifier-type 'cons)) #'consp)
          (t checkable-type))))

(defun specifier-from-checkfun (fun-or-ctype)
  (if (ctype-p fun-or-ctype)
      (type-specifier fun-or-ctype)
      (ecase (%fun-name fun-or-ctype)
        (fixnump  'fixnum)
        (integerp 'integer)
        (numberp  'number)
        (listp    'list)
        (consp    'cons))))

;; Given a ctype TYPE, return a possibly modified ctype that is acceptable
;; as the second argument to TYPEP (and %%TYPEP) by collapsing subtypes
;; of function into just FUNCTION.  CL's function typing is not as powerful
;; as in Haskell or ML-style typing where distinct function types each occupy
;; a point in the type lattice. Although it sometimes works to inquire of the
;; runtime whether a function is (FUNCTION (FIXNUM) FIXNUM) for example,
;; it can not be assumed to work. When it doesn't, %%TYPEP says NIL and T
;; meanining "no" and "certain" when it might instead say "not sure".
(defun checkable-type (type)
  (values-specifier-type
   (let ((*unparse-fun-type-simplify* t)) (type-specifier type))))

(defun typecheck-fail (symbol value type)
  (error 'interpreter-type-error
         :datum value :expected-type (specifier-from-checkfun type)
         :symbol symbol :operation 'write))

(defun typecheck-fail/ref (symbol value type)
  (error 'interpreter-type-error
         :datum value :expected-type (specifier-from-checkfun type)
         :symbol symbol :operation 'read))

;; Signal an error about a form that was expected to produce multiple values
;; that did not accord with their type restriction.
(defun values-typecheck-fail (type &rest values)
  ;; Maybe want to say "Received N but the VALUES type specifies M" ?
  (let ((spec (type-specifier type)))
    (cond ((not values)
           (error 'simple-type-error
                  :format-control "Received no value for ~S"
                  :format-arguments (list spec)))
          ((not (cdr values))
           (error 'simple-type-error
                  :format-control
                  "The value ~S does not match the specifier ~_~S"
                  :format-arguments (list (car values) spec)))
          (t
           (error 'simple-type-error
                  :datum values
                  :expected-type spec
                  :format-control
                  "The values ~{~S~^, ~} do not match the specifier ~_~S"
                  :format-arguments (list values spec))))))

(declaim (inline itypep)) ; Interpreter TYPEP
;;; A test "function" can be be either genuinely a function, or a CTYPE.
(defun itypep (object test)
  (if (functionp test)
      (funcall test object)
      (%%typep object test)))

;; Check that VALUE is of the expected type and return it or signal an error.
(declaim (inline enforce-type))
(defun enforce-type (value type-vector index symbol-vector)
  (unless (eql type-vector +none+)
    (let ((type (svref type-vector index)))
      (unless (eql type +none+)
        (unless (itypep value type)
          (typecheck-fail (car (svref symbol-vector index)) value type)))))
  value)

;;;; Copy-and-pasted from sb-eval:

;;; We need separate conditions for the different *-TOO-COMPLEX-ERRORs to
;;; avoid spuriously triggering the handler in EVAL-IN-NATIVE-ENVIRONMENT
;;; on code like:
;;;
;;;   (let ((sb-ext:*evaluator-mode* :interpret))
;;;     (let ((fun (eval '(let ((a 1)) (lambda () a)))))
;;;         (eval `(compile nil ,fun))))
;;;
;;; FIXME: should these be exported?
(define-condition interpreter-environment-too-complex-error (simple-error)
  ())
(define-condition compiler-environment-too-complex-error (simple-error)
  ())

(defun ip-error (format-control &rest format-arguments)
  (error 'sb-int:simple-program-error
         :format-control format-control
         :format-arguments format-arguments))
