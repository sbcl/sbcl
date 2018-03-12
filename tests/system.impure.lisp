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

(in-package "SB-VM")

#-(and (or x86 x86-64) (not interpreter)) (sb-ext:exit :code 104)

(test-util:with-test (:name :basic-cpuid)
  (flet ((to-ascii (bits)
           (let ((s (make-array 4 :element-type 'base-char)))
             (setf (sap-ref-32 (vector-sap s) 0) bits)
             s)))
    (multiple-value-bind (a b c d)
        (%cpu-identification 0 0)
      ;; There's nothing to assert here since the result can vary
      (format t "~S (max function = ~D)~%"
              (concatenate 'string (to-ascii b) (to-ascii d) (to-ascii c))
              a))))
