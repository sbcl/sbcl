;;;; DESCRIBE-COMPILER-POLICY

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

(defun describe-compiler-policy (&optional spec)
  "Print all global optimization settings, augmented by SPEC."
  (let ((policy (process-optimize-decl (cons 'optimize spec) *policy*)))
    (fresh-line)
    (format t "  Basic qualities:~%")
    (dovector (quality +policy-primary-qualities+)
      (format t "~S = ~D~%" quality (policy-quality policy quality)))
    (format t "  Dependent qualities:~%")
    (loop for info across **policy-dependent-qualities**
       for quality = (policy-dependent-quality-name info)
       for values-documentation = (policy-dependent-quality-values-documentation info)
       for explicit-value = (policy-quality policy quality)
       do (if (= explicit-value 1)
              (let* ((getter (policy-dependent-quality-getter info))
                     (value (funcall getter policy))
                     (documentation (elt values-documentation value)))
                (format t "~S = ~D -> ~D (~A)~%"
                        quality explicit-value value documentation))
              (let ((documentation (elt values-documentation explicit-value)))
                (format t "~S = ~D (~A)~%"
                        quality explicit-value documentation)))))

  (values))
