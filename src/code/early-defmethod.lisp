;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-PCL")

;;;; Rudimentary DEFMETHOD

(sb-xc:defmacro defmethod (&whole form name lambda-list &rest body
                           &aux qualifier)
  (when (member name '((setf documentation) documentation) :test 'equal)
    (return-from defmethod `(push ',form *!documentation-methods*)))
  (when (keywordp lambda-list)
    ;; Allow an :AFTER method in 'condition.lisp'.
    ;; It's ignored during cold-init, but eventually takes effect.
    (assert (eq lambda-list :after))
    (setq qualifier lambda-list lambda-list (pop body)))
  (ecase name
    (make-load-form
     ;; Expect one mandatory class-name and the optional environment.
     (assert (typep lambda-list
                    '(cons (cons symbol (cons symbol null))
                           (cons (eql &optional) (cons symbol null))))))
    (print-object
     ;; Expect one unqualified mandatory arg and one unqualified.
     (assert (typep lambda-list '(cons (cons symbol (cons symbol null))
                                       (cons symbol null))))))
  (binding* ((specializer (cadar lambda-list)) ; only one allowd
             (unspecialized-ll `(,(caar lambda-list) ,@(cdr lambda-list)))
             ((forms decls) (parse-body body nil))) ; Note: disallowing docstring
    `(!trivial-defmethod
      ;; An extra NIL in front puts the GF name is the same position it would be in
      ;; for a normal LOAD-DEFMETHOD.
      nil ',name ',specializer ,qualifier ',unspecialized-ll
      ;; OAOO problem: compute the same lambda name as real DEFMETHOD would
      (named-lambda (fast-method ,name
                     (,specializer ,@(if (eq name 'print-object) '(t))))
          (.pv. .next-method-call. .arg0. ,@(cdr unspecialized-ll)
                ;; Rebind specialized arg with unchecked type assertion.
                &aux (,(car unspecialized-ll) (truly-the ,specializer .arg0.)))
        (declare (ignore .pv. .next-method-call.)
                 (ignorable ,(car unspecialized-ll)))
        ,@decls
        ;; Fail at compile-time if any transformational magic needs to happen.
        (macrolet ,(mapcar (lambda (f)
                             `(,f (&rest args)
                                  (declare (ignore args))
                                  (error "can't use ~A in trivial method" ',f)))
                           '(slot-boundp slot-value %set-slot-value call-next-method))
          (flet (((setf slot-value) (&rest args) `(%set-slot-value ,@args)))
            (declare (inline (setf slot-value)))
            (block ,name ,@forms))))
      ;; Why is SOURCE-LOC needed? Lambdas should know their location.
      (sb-c::source-location))))

(defvar *!trivial-methods* '()) ; necessary methods for system startup
(defvar *!documentation-methods* nil) ; saved up for after PCL bootstrap
(defun !trivial-defmethod (dummy name specializer qualifier lambda-list lambda source-loc)
  (declare (ignore dummy)) ; this would be the method class in LOAD-DEFMETHOD
  (let ((gf (assoc name *!trivial-methods*)))
    ;; Append the method but don't bother finding a predicate for it.
    ;; Methods occurring in early warm load (notably from SB-FASTEVAL)
    ;; wil be properly installed when 'pcl/print-object.lisp' is loaded.
    (rplacd gf (concatenate 'vector (cdr gf)
                            (list (list nil lambda specializer qualifier
                                        lambda-list source-loc))))))

;;; Slow-but-correct logic for single-dispatch sans method combination,
;;; allowing exactly one primary method. Methods are sorted most-specific-first,
;;; so we can stop looking as soon as a match is found.
(defun !call-a-method (gf-name specialized-arg &rest rest)
  (let* ((methods (the simple-vector
                    (cdr (or (assoc gf-name *!trivial-methods*)
                             (error "No methods on ~S" gf-name)))))
         (applicable-method
          (find specialized-arg methods
                :test (lambda (arg method &aux (guard (car method)))
                        (and (or (functionp guard) (fboundp guard))
                             (funcall guard arg))))))
    (unless applicable-method
      (error "No applicable method for ~S on ~S~%" gf-name
             (type-of specialized-arg)))
    ;; The "method" is a list: (GUARD LAMBDA . OTHER-STUFF)
    ;; Call using no permutation-vector / no precomputed next method.
    (apply (cadr applicable-method) nil nil specialized-arg rest)))

(defun make-load-form (object &optional environment)
  (!call-a-method 'make-load-form object environment))
(defun print-object (object stream)
  (!call-a-method 'print-object object stream))

;;; FIXME: this no longer holds methods, but it seems to have an effect
;;; on the caching of a discriminating function for PRINT-OBJECT
(defvar *!delayed-defmethod-args* nil)
