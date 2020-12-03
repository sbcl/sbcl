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

;;; This stub ensures that:
;;; - Argument specializations are stored in a way that permits an extremely simple
;;;   single-dispatch implementation with correct behavior of MAKE-LOAD-FORM and
;;;   PRINT-OBJECT. Subject to not needing method combination or CALL-NEXT-METHOD,
;;;   exactly one primary method is chosen respecting class precedence order.
;;; - The simple methods can be installed later by the full CLOS implementation.
;;;   They play nice by using the same call signature for the "fast function"

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
        ;; Fail at compile-time if any fancy slot access would happen, if compiled
        ;; by the eventual implementation.
        ;; (SETF SLOT-VALUE) is not a legal macro name, so transform it as a
        ;; an ignorable function that uses a legal macro name.
        (macrolet ,(mapcar (lambda (f)
                             `(,f (&rest args)
                                  (declare (ignore args))
                                  (error "can't use ~A in trivial method" ',f)))
                           '(slot-boundp slot-value %set-slot-value call-next-method))
          (flet (((setf slot-value) (&rest args) `(%set-slot-value ,@args)))
            (declare (inline (setf slot-value)) (ignorable #'(setf slot-value)))
            (block ,name ,@forms))))
      ;; Why is SOURCE-LOC needed? Lambdas should know their location.
      (sb-c:source-location))))

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
(defmacro trivial-methods (gf-name)
  `(the simple-vector
        (cdr (or (assoc ,gf-name *!trivial-methods*)
                 (error "No methods on ~S" ,gf-name)))) )
(defun trivial-call-a-method (gf-name methods specialized-arg &rest rest)
  (let* ((applicable-method
           ;; Find a method where the guard returns T, or the object type is
           ;; EQ to the method's specializer. Trivial dispatch knows nothing
           ;; of superclass method applicability. In situations where we rely
           ;; on an ancestral type's method, the predicate will pick it out.
           ;; The "method" is a vector:
           ;;  #(#<GUARD> QUALIFIER SPECIALIZER #<FMF> LAMBDA-LIST SOURCE-LOC)
          (find-if (lambda (method)
                     ;; Most instance types lack a predicate.
                     ;; Non-instances should all have a predicate.
                     (let ((type (when (%instancep specialized-arg)
                                   (classoid-name
                                    (layout-classoid
                                     (%instance-layout specialized-arg)))))
                           (guard (svref method 0)))
                       (or (eq type (svref method 2))
                           (and (or (functionp guard) (fboundp guard))
                                (funcall guard specialized-arg)))))
                   methods)))
    (if applicable-method
        ;; Call using no permutation-vector / no precomputed next method.
        (apply (svref applicable-method 3) nil nil specialized-arg rest)
        (error "No applicable method for ~S on ~S~%" gf-name
               (type-of specialized-arg)))))

(defun make-load-form (object &optional environment)
  (trivial-call-a-method  'make-load-form (trivial-methods 'make-load-form)
                         object environment))
(defun print-object (object stream)
  (let ((methods (trivial-methods 'print-object)))
    ;; The method selected for CONDITION is basically useless because it picks out
    ;; STRUCTURE-OBJECT which prints as #<WARNING (no LAYOUT-INFO) #xBD2B8C9> or somesuch.
    ;; It's because trivial method selection is based on the first predicate in METHODS
    ;; which returns T (so we kinda get type hierarchies correct), but there is no
    ;; CONDITION-P predicate, so the candidate method is just skipped.
    ;; Hacking in an extra test here solves that (maybe?) until the real GF is installed.
    (if (typep object 'condition)
        (funcall (elt (find 'condition methods :key (lambda (x) (elt x 2))) 3)
                 nil nil object stream)
        (trivial-call-a-method 'print-object methods object stream))))

;;; FIXME: this no longer holds methods, but it seems to have an effect
;;; on the caching of a discriminating function for PRINT-OBJECT
(defvar *!delayed-defmethod-args* nil)

;;; This exists only to show that the cross-compiler can constant-fold
;;; a constant index into a literal array without crashing.
(defun !test-svref-folding ()
  (let ((z #(42 test)))
    (if (< (svref z 0) 100) t nil)))
