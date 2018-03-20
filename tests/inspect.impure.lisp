;;;; tests for the INSPECT function

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(defun test-inspect (object control)
  (let* ((control (if (listp control)
                      (format nil "~{~A~%~}" control)
                      control))
         (*standard-input* (make-string-input-stream control))
         (output (make-string-output-stream))
         (*standard-output* output))
    (inspect object)
    (get-output-stream-string output)))

(defclass class-with-prototype-print-error ()
  ((will-be-unbound)))

(defmethod print-object ((object class-with-prototype-print-error) stream)
  (print-unreadable-object (object stream :type t)
    (princ (slot-value object 'will-be-unbound) stream)))

(with-test (:name (inspect :no-error print-object :lp-454682))
  (let ((class (find-class 'class-with-prototype-print-error)))
    (flet ((test ()
             (test-inspect class '("q"))))
      ;; Prototype may not be initialized at this point.
      (assert (search "PROTOTYPE: " (test)))
      ;; Force protocol initialization and test again.
      (sb-mop:class-prototype class)
      (assert (search "PROTOTYPE: " (test))))))
