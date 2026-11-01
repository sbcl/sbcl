;;;; miscellaneous side-effectful tests of conditions

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

;;;; REFERENCE-CONDITIONs

(defvar *reference-and-expected-list*
  '(((:ansi-cl :function remove)
     ("The ANSI Standard, Function REMOVE"))
    ((:ansi-cl :section (1 2 3))
     ("The ANSI Standard, Section 1.2.3"))
    ((:ansi-cl :glossary "similar")
     ("The ANSI Standard, Glossary entry for \"similar\""))
    ((:sbcl :node "Handling of Types")
     ("The SBCL Manual, Node \"Handling of Types\"")
     #+sb-doc
     ("The SBCL Manual, Section SB-MANUAL:@HANDLING-OF-TYPES"))
    ((:sbcl :variable *ed-functions*)
     ("The SBCL Manual, Variable *ED-FUNCTIONS*")
     #+sb-doc
     ("The SBCL Manual, Node \"Customization Hooks for Users\""
      "The SBCL Manual, Section SB-MANUAL:@CUSTOMIZATION-HOOKS-FOR-USERS"))))

(defun check-reference-condition ()
  (loop for entry in *reference-and-expected-list*
        do (destructuring-bind (reference expecteds &optional
                                expecteds-with-sb-manual)
               entry
             (let ((c (make-condition 'sb-int:reference-condition
                                      :references (list reference))))
               (dolist (expected expecteds)
                 (assert (search expected (princ-to-string c))))
               (when (find-package "SB-MANUAL")
                 (dolist (expected expecteds-with-sb-manual)
                   (assert (search expected (princ-to-string c)))))))))

(with-test (:name :reference-condition)
  (check-reference-condition))

(require 'sb-manual)

(with-test (:name :reference-condition-with-sb-manual)
  (check-reference-condition))
