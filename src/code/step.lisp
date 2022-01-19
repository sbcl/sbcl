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

(in-package "SB-IMPL")

(defun step-form (form args)
  (restart-case
      (signal 'step-form-condition
              :form form
              :args args)
    (step-continue ()
      :report "Resume normal execution"
      (disable-stepping)
      (setf *step-out* nil))
    (step-out ()
      :report "Resume stepping after returning from this function"
      (ecase *step-out*
        ((nil)
         (error "Can't STEP-OUT: No STEP-IN on the call-stack"))
        ((t :maybe)
         (disable-stepping)
         (setf *step-out* t)))
      nil)
    (step-next ()
      :report "Step over call"
      nil)
    (step-into ()
      :report "Step into call"
      t)))

(defun step-values (form &rest values)
  (declare (truly-dynamic-extent values))
  (signal 'step-values-condition :form form :result values)
  (values-list values))

(defun step-finished ()
  (restart-case
      (signal 'step-finished-condition)
    (continue ())))

(defgeneric single-step (condition))

(defmethod single-step ((condition step-values-condition))
  (let ((values (step-condition-result condition)))
    (format *debug-io* "; ~A => ~:[#<no value>~;~{~S~^, ~}~]~%"
            (step-condition-form condition)
            values values)))

(defmethod single-step ((condition step-form-condition))
  (let ((form (step-condition-form condition))
        (args (step-condition-args condition)))
    (let ((*print-circle* t)
          (*print-pretty* t)
          (*print-readably* nil))
      (format *debug-io*
              "; Evaluating call:~%~<; ~@;  ~A~:>~%~
               ; ~:[With arguments:~%~<; ~@;~{  ~S~^~%~}~:>~;With unknown arguments~]~%"
              (list form)
              (eq args :unknown)
              (list args)))
    (finish-output *debug-io*)
    (let ((*stack-top-hint* (sb-di::find-stepped-frame)))
      (invoke-debugger condition))))

;;; In the TTY debugger we're not interested in STEP returning
(defmethod single-step ((condition step-finished-condition))
  (values))

(defvar *stepper-hook* 'single-step
  "Customization hook for alternative single-steppers.
*STEPPER-HOOK* is bound to NIL prior to calling the bound function
with the STEP-CONDITION as argument.")

(defun invoke-stepper (condition)
  (when (and (stepping-enabled-p)
             *stepper-hook*)
    (with-stepping-disabled
      (let ((hook *stepper-hook*)
            (*stepper-hook* nil))
        (funcall hook condition)))))

(defmacro step (form)
  "The form is evaluated with single stepping enabled. Function calls
outside the lexical scope of the form can be stepped into only if the
functions in question have been compiled with sufficient DEBUG policy
to be at least partially steppable."
  `(locally
    (declare (optimize debug (sb-c:insert-step-conditions 0)))
     ;; Allow stepping out of the STEP form.
     (let ((*step-out* :maybe))
       (unwind-protect
            (with-stepping-enabled
              (multiple-value-prog1
                  (locally (declare (optimize (sb-c:insert-step-conditions 3)))
                    ,form)
                (step-finished)))))))

