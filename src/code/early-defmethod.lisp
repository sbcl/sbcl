;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-PCL")

(define-condition specialized-lambda-list-error
    (reference-condition simple-program-error)
  ()
  (:default-initargs :references '((:ansi-cl :section (3 4 3)))))

(defun specialized-lambda-list-error (format-control &rest format-arguments)
  (error 'specialized-lambda-list-error
         :format-control format-control
         :format-arguments format-arguments))

;; Return 3 values:
;; - the bound variables, without defaults, supplied-p vars, or &AUX vars.
;; - the lambda list without specializers.
;; - just the specializers
(defun parse-specialized-lambda-list (arglist)
  (binding* (((llks specialized optional rest key aux)
              (parse-lambda-list
               arglist
               :context 'defmethod
               :accept (lambda-list-keyword-mask
                        '(&optional &rest &key &allow-other-keys &aux))
               :silent t         ; never signal &OPTIONAL + &KEY style-warning
               :condition-class 'specialized-lambda-list-error))
             (required (mapcar (lambda (x) (if (listp x) (car x) x)) specialized))
             (specializers (mapcar (lambda (x) (if (listp x) (cadr x) t)) specialized)))
    (check-lambda-list-names
     llks required optional rest key aux nil nil
     :context "a method lambda list" :signal-via #'specialized-lambda-list-error)
    (values (append required
                    (mapcar #'parse-optional-arg-spec optional)
                    rest
                    ;; Preserve keyword-names when given as (:KEYWORD var)
                    (mapcar (lambda (x)
                              (if (typep x '(cons cons))
                                  (car x)
                                  (parse-key-arg-spec x)))
                            key))
            (make-lambda-list llks nil required optional rest key aux)
            specializers)))


;;; PARSE-DEFMETHOD is used by DEFMETHOD to parse the &REST argument
;;; into the 'real' arguments. This is where the syntax of DEFMETHOD
;;; is really implemented.
(defun parse-defmethod (cdr-of-form)
  (declare (list cdr-of-form))
  (let ((qualifiers ())
        (spec-ll ()))
    (loop (if (and (car cdr-of-form) (atom (car cdr-of-form)))
              (push (pop cdr-of-form) qualifiers)
              (return (setq qualifiers (nreverse qualifiers)))))
    (setq spec-ll (pop cdr-of-form))
    (values qualifiers spec-ll cdr-of-form)))

;;;; Rudimentary DEFMETHOD

;;; This expander avoids the use of the rest of CLOS in order to help
;;; with bootstrapping.
(sb-xc:defmacro defmethod (name &rest args)
  (check-designator name 'defmethod #'legal-fun-name-p "function name")
  (multiple-value-bind (qualifiers lambda-list body)
      (parse-defmethod args)
    (multiple-value-bind (parameters unspecialized-ll specializers)
        (parse-specialized-lambda-list lambda-list)
      (multiple-value-bind (forms decls)
          (parse-body body nil) ; Note: disallowing docstring
        `(!early-load-method 'standard-method
          ',name ',qualifiers ',specializers ',unspecialized-ll
          ;; OAOO problem: compute the same lambda name as real DEFMETHOD would
          (named-lambda (fast-method ,name ,@qualifiers ,specializers)
              (.pv. .next-method-call. ,@unspecialized-ll)
            ,@decls
            (declare (ignore .pv. .next-method-call.))
            ;; Rebind arguments, asserting specialized types. (unchecked)
            (let ,(mapcar (lambda (parameter specializer)
                            `(,parameter (truly-the ,specializer ,parameter)))
                   parameters specializers)
              (declare ,@(mapcar (lambda (parameter specializer)
                                   (declare (ignore specializer))
                                   `(ignorable ,parameter))
                                 parameters specializers))
              ;; Fail at compile-time if any fancy slot access would happen, if compiled
              ;; by the eventual implementation.
              ;; (SETF SLOT-VALUE) is not a legal macro name, so transform it as a
              ;; an ignorable function that uses a legal macro name.
              (macrolet ,(mapcar (lambda (f)
                                   `(,f (&rest args)
                                        (declare (ignore args))
                                        (error "can't use ~A in initial method" ',f)))
                                 '(slot-boundp slot-value %set-slot-value call-next-method))
                (flet (((setf slot-value) (&rest args) `(%set-slot-value ,@args)))
                  (declare (inline (setf slot-value)) (ignorable #'(setf slot-value)))
                  (block ,(fun-name-block-name name) ,@forms)))))
          (sb-c:source-location))))))

(define-source-context defmethod (name &rest stuff)
  (let ((arg-pos (position-if #'listp stuff)))
    (if arg-pos
        `(defmethod ,name ,@(subseq stuff 0 arg-pos)
           ,(handler-case
                (nth-value 2 (parse-specialized-lambda-list (elt stuff arg-pos)))
              (error () "<illegal syntax>")))
        `(defmethod ,name "<illegal syntax>"))))
