;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!PCL")

;;;; Rudimentary DEFMETHOD

(eval-when (:compile-toplevel) ; Don't want any load-time effect
(sb!xc:defmacro defmethod (name lambda-list &rest body)
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
      ',name ',specializer ',unspecialized-ll
      (named-lambda (fast-method ,name (,specializer))
          (.pv .next-method-call. ,@unspecialized-ll)
        (declare (ignore .pv .next-method-call.))
        ,@decls
        ;; Fail at compile-time if any transformational magic needs to happen.
        (macrolet ,(mapcar (lambda (f)
                             `(,f (&rest args)
                                  (declare (ignore args))
                                  (error "can't use ~A in trivial method" ',f)))
                           '(slot-boundp slot-value %set-slot-value call-next-method))
          (flet (((setf slot-value) (&rest args) `(%set-slot-value ,@args)))
            (declare (inline (setf slot-value)))
            (let ((,(car unspecialized-ll)
                   (truly-the ,specializer ,(car unspecialized-ll))))
              ,@forms))))
      ;; Why is SOURCE-LOC needed? Lambdas should know their location.
      (sb!c::source-location)))))

(defvar *!trivial-methods* '())
(defun !trivial-defmethod (name specializer lambda-list lambda source-loc)
  (let ((gf (assoc name *!trivial-methods*)))
    (unless gf
      (setq gf (cons name #()))
      (push gf *!trivial-methods*))
    (let ((entry (list specializer lambda-list lambda source-loc)))
      (setf (cdr gf)
            (merge '(simple-array t (*)) ; SIMPLE-VECTOR not DEFTYPEd yet!
                   (list entry) (cdr gf) #'>
                   :key (lambda (x) (layout-depthoid (find-layout (car x))))))
      entry)))

;;; Slow-but-correct logic for single-dispatch sans method combination,
;;; allowing exactly one primary method. Methods are sorted most-specific-first,
;;; so we can stop looking as soon as a match is found.
(defun !call-a-method (gf-name specialized-arg &rest rest)
  (let* ((methods (the simple-vector
                    (cdr (or (assoc gf-name *!trivial-methods*)
                             (error "No methods on ~S" gf-name)))))
         (applicable-method
          ;; First try matching the type name exactly. Failing that, use TYPEP.
          (or (find (type-of specialized-arg) methods :key #'car :test #'eq)
              (find-if (lambda (x) (typep specialized-arg (car x))) methods))))
    (assert applicable-method)
    ;; no permutation-vector / no precomputed next method
    (apply (third applicable-method) nil nil specialized-arg rest)))

(defun make-load-form (object &optional environment)
  (!call-a-method 'make-load-form object environment))
(defun print-object (object stream)
  (!call-a-method 'print-object object stream))

;;; This method gets removed by force-delayed-methods
(defmethod print-object ((self t) stream)
  (print-unreadable-object (self stream :type t :identity t)))

;;;; Complete DEFMETHOD, not usable until CLOS works.

(defvar *!delayed-defmethod-args* nil)
;;; By our convention, "DEF!METHOD" would imply behavior in both the
;;; host and target, but this is only for the target, so ...
(defmacro def*method (&rest args)
  `(push (cons (sb!c:source-location) ',args) *!delayed-defmethod-args*))
