;;;; single stepper for SBCL

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;;; Single stepping works by having compiler insert STEP-CONDITION
;;;; signalling forms into code compiled at high debug settings, and
;;;; having a handler for them at the toplevel.

(in-package "SB-IMPL") ; in warm SBCL

(defvar *step-help* "The following commands are available at the single
stepper's prompt:

 S: Step into the current expression.
 N: Evaluate the current expression without stepping.
 C: Evaluate to finish without stepping.
 Q: Abort evaluation.
 B: Backtrace.
 ?: Display this message.
")

(defgeneric single-step (condition))

(defmethod single-step ((condition step-variable-condition))
  (format *debug-io* "; ~A => ~S~%"
          (step-condition-form condition)
          (step-condition-result condition)))

(defmethod single-step ((condition step-values-condition))
  (let ((values (step-condition-result condition)))
    (format *debug-io* "; ~A => ~:[#<no value>~;~{~S~^, ~}~]~%"
            (step-condition-form condition)
            values values)))

(defmethod single-step ((condition step-form-condition))
  (let ((form (step-condition-form condition)))
    (loop
     (format *debug-io* "; form ~A~%STEP] " form)
     (finish-output *debug-io*)
     (let ((line (read-line *debug-io*)))
       (if (plusp (length line))
           (case (char-upcase (schar line 0))
             (#\B
              (backtrace))
             (#\Q
              (abort condition))
             (#\C
              (step-continue condition))
             (#\N
              (step-next condition))
             (#\S
              (step-into condition))
             (#\?
              (write-line *step-help* *debug-io*))))))))

(defvar *stepper-hook* 'single-step
  #+sb-doc "Customization hook for alternative single-steppers.
*STEPPER-HOOK* is bound to NIL prior to calling the bound function
with the STEP-CONDITION as argument.")

(defun invoke-stepper (condition)
  (when (and *stepping* *stepper-hook*)
    (let ((hook *stepper-hook*)
          (*stepper-hook* nil))
      (funcall hook condition))))

(defmacro step (form)
  #+sb-doc
  "The form is evaluated with single stepping enabled. Function calls
outside the lexical scope of the form can be stepped into only if the
functions in question have been compiled with sufficient DEBUG policy
to be at least partially steppable."
  `(let ((*stepping* t)
         (*step* t))
    (declare (optimize (sb-c:insert-step-conditions 0)))
    (format t "Single stepping. Type ? for help.~%")
    (locally (declare (optimize (sb-c:insert-step-conditions 3)))
      ,form)))
