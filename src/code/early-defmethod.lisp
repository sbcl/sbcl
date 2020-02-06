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
    ;; will be properly installed when 'pcl/print-object.lisp' is loaded.
    (rplacd gf (concatenate 'vector (cdr gf)
                            (list (vector nil ; guard
                                          qualifier specializer lambda
                                          lambda-list source-loc))))))

;;; Slow-but-correct logic for single-dispatch sans method combination,
;;; allowing exactly one primary method. Methods are sorted most-specific-first,
;;; so we can stop looking as soon as a match is found.
;;; Sorting is performed during genesis.
(defun !call-a-method (gf-name specialized-arg &rest rest)
  (let* ((methods (the simple-vector
                    (cdr (or (assoc gf-name *!trivial-methods*)
                             (error "No methods on ~S" gf-name)))))
         (applicable-method
           ;; Find a method where the guard returns T. If that fails, find a method
           ;; which exactly matches TYPE-OF the specialized-arg.
           ;; It might be nice to rely only on the TYPE-OF test, but then we'd have to
           ;; concern ourselves with type hierarchies.
           ;; The "method" is a vector:
           ;;  #(#<GUARD> QUALIFIER SPECIALIZER #<FMF> LAMBDA-LIST SOURCE-LOC)
           (or (find-if (lambda (method &aux (guard (svref method 0)))
                          (and (or (functionp guard) (fboundp guard))
                               (funcall guard specialized-arg)))
                        methods)
               (find (type-of specialized-arg) methods
                     :key (lambda (x) (svref x 2))))))

    (if applicable-method
        ;; Call using no permutation-vector / no precomputed next method.
        (apply (svref applicable-method 3) nil nil specialized-arg rest)
        (error "No applicable method for ~S on ~S~%" gf-name
               (type-of specialized-arg)))))

;;; For any "trivial" PRINT-OBJECT method lacking a predicate to determine
;;; its applicability, try to install one now.
;;; (Question: could not genesis find a predicate for everything?)
(defun !fixup-print-object-method-guards ()
  ;; LAYOUT-OF has an ordinary definition appearing later, unless a vop translates
  ;; it, in which case it has to be inlined because the stub isn't ready for use.
  #-(vop-translates sb-kernel:layout-of) (declare (notinline layout-of))
  ;; Wouldn't you know, DOVECTOR is in early-extensions,
  ;; and it would need to go in primordial-extensions to use it here.
  (loop for method across (cdr (assoc 'print-object sb-pcl::*!trivial-methods*))
        unless (svref method 0)
        do (let ((classoid (find-classoid (elt method 2))))
             (setf (svref method 0)
                   (lambda (x) (classoid-typep (layout-of x) classoid x))))))

(defun make-load-form (object &optional environment)
  (!call-a-method 'make-load-form object environment))
(defun print-object (object stream)
  (!call-a-method 'print-object object stream))

;;; FIXME: this no longer holds methods, but it seems to have an effect
;;; on the caching of a discriminating function for PRINT-OBJECT
(defvar *!delayed-defmethod-args* nil)

;;; This exists only to show that the cross-compiler can constant-fold
;;; a constant index into a literal array without crashing.
(defun !test-svref-folding ()
  (let ((z #(42 test)))
    (if (< (svref z 0) 100) t nil)))
