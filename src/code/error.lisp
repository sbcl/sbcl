;;;; SBCL-specific parts of the condition system, i.e. parts which
;;;; don't duplicate/clobber functionality already provided by the
;;;; cross-compilation host Common Lisp

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-KERNEL")

;;; a utility for SIGNAL, ERROR, CERROR, WARN, COMPILER-NOTIFY and
;;; INVOKE-DEBUGGER: Parse the hairy argument conventions into a
;;; single argument that's directly usable by all the other routines.
(defun coerce-to-condition (datum default-type fun-name &rest arguments)
  (declare (explicit-check)
           (dynamic-extent arguments))
  (cond ((and (%instancep datum)
              (let ((layout (%instance-layout datum)))
                (and (logtest +condition-layout-flag+ (layout-%bits layout))
                     ;; An invalid layout will drop into the (MAKE-CONDITION) branch
                     ;; which rightly fails because ALLOCATE-CONDITION asserts that
                     ;; the first argument is a condition-designator, which it won't be.
                     (not (layout-invalid layout)))))
         (when (and arguments (not (eq fun-name 'cerror)))
           (cerror "Ignore the additional arguments."
                   'simple-type-error
                   :datum (copy-list arguments)
                   :expected-type 'null
                   :format-control "You may not supply additional arguments ~
                                    when giving ~S to ~S."
                   :format-arguments (list datum fun-name)))
         datum)
        ((or (stringp datum) (functionp datum))
         (make-condition default-type
                         :format-control datum
                         :format-arguments (copy-list arguments)))
        (t
         (apply #'make-condition datum arguments))))

;;; This condition inherits from the hosts's classes when compiling
;;; the cross-compiler and the target's when cross-compiling.
(define-condition simple-program-error (simple-condition program-error) ())
(defun %program-error (&optional datum &rest arguments)
  (error (apply #'coerce-to-condition datum
                'simple-program-error '%program-error arguments)))
