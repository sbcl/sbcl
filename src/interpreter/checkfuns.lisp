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
;;;
;;; We also need to ensure that TYPE is checkable at all,
;;; because FUNCTION types are not good for answering questions about
;;; the type of an object, only for declaring the type.
;;; Passing ":STRICT NIL" to %%TYPEP won't work, because interpreted functions
;;; know nothing about their type, and the non-strict test uses %FUN-TYPE.
;;;
;;; The easy way of simplifying hairy FUNCTION types, namely
;;; binding *UNPARSE-FUN-TYPE-SIMPLIFY* to T and then doing:
;;;   (VALUES-SPECIFIER-TYPE (TYPE-SPECIFIER x))
;;; does not work, because ALIEN-TYPE-TYPES are incompatibly altered -
;;; they fail to retain correct offsets that were manually specified.
;;; Probably nobody ever cared that round-tripping was not possible,
;;; because that is not an operation that the compiler needs to do,
;;; though that's surprising because plenty of code exists that normalizes
;;; types by unparsing and re-parsing.
;;;
(defun type-checker (type)
  (labels
      ((get-unary-predicate (type)
         (let ((pred (sb-c::backend-type-predicate type)))
           (when pred
             (symbol-function pred))))
       (simplify (type)
         (etypecase type
           ((or named-type numeric-union-type member-type classoid
                character-set-type unknown-type hairy-type
                alien-type-type #+sb-simd-pack simd-pack-type
                                #+sb-simd-pack-256 simd-pack-256-type)
            type)
           (fun-designator-type (specifier-type '(or function symbol)))
           (fun-type (specifier-type 'function))
           (compound-type
            (let* ((original (compound-type-types type))
                   (new (mapcar #'simplify original)))
              (if (every #'eq original new)
                  type
                  (apply (if (union-type-p type) #'type-union #'type-intersection)
                         new))))
           (cons-type
            (let* ((old-car (cons-type-car-type type))
                   (new-car (simplify old-car))
                   (old-cdr (cons-type-cdr-type type))
                   (new-cdr (simplify old-cdr)))
              (if (and (eq old-car new-car) (eq old-cdr new-cdr))
                  type
                  (make-cons-type new-car new-cdr))))
           (values-type
            (let* ((old-req (values-type-required type))
                   (new-req (mapcar #'simplify old-req))
                   (old-opt (values-type-optional type))
                   (new-opt (mapcar #'simplify old-opt))
                   (old-rest (values-type-rest type))
                   (new-rest (when old-rest (simplify old-rest))))
              (if (and (every #'eq old-req new-req)
                       (every #'eq old-opt new-opt)
                       (eq old-rest new-rest))
                  type
                  (make-values-type new-req new-opt new-rest))))
           (array-type
            (let* ((original (array-type-element-type type))
                   (new (simplify original)))
              (cond ((eq new original) type)
                    (t
                     ;; It must have been an (ARRAY T).
                     (aver (eq (array-type-specialized-element-type type)
                               *universal-type*))
                     (sb-kernel::%make-array-type (array-type-dimensions type)
                                                  (array-type-complexp type)
                                                  new *universal-type*)))))
           (negation-type
            (let* ((original (negation-type-type type))
                   (new (simplify original)))
              (if (eq new original) type (make-negation-type new)))))))
    (or (get-unary-predicate type)
        ;; If we simplify, try again for a predicate.
        (let ((simplified (simplify type)))
          (or (get-unary-predicate simplified) simplified)))))

(defun specifier-from-checkfun (fun-or-ctype)
  (type-specifier
   (if (functionp fun-or-ctype)
       (or (gethash (%fun-name fun-or-ctype)
                    sb-c::*backend-predicate-types*)
           (bug "No type specifier for function ~S" fun-or-ctype))
       fun-or-ctype)))

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
