;;;; tests of the CASE family of macros without side effects

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

(cl:in-package :cl-user)

(with-test (:name (case :duplicate-key :compile-time-warning))
 (loop
    for (expected kind . clauses) in
      '((nil
         case (1 1)
              (2 2)
              (3 3))

        ("Duplicate key 1 in CASE form, occurring in the first clause:  (1 1), and the second clause:  (1 2)"
         case (1 1)
              (1 2))

        ("Duplicate key 2 in CASE form, occurring in the first clause:  ((1 2) 1), and the second clause:  ((2 3) 2)"
         case ((1 2) 1)
              ((2 3) 2))

        (nil
         case (#1=(1) 1)
              ((#1#) 2)))
    for form = `(lambda ()
                  (,kind *readtable*
                         ,@clauses))
    do
      (multiple-value-bind (fun failure-p warnings style-warnings)
          (checked-compile form :allow-style-warnings (when expected t))
        (declare (ignore failure-p warnings))
        (assert (functionp fun))
        (when expected
          (dolist (warning style-warnings)
            (assert (search expected
                            (with-standard-io-syntax
                              (let ((*print-right-margin* nil)
                                    (*print-pretty* t))
                                (remove #\Newline (princ-to-string warning)))))
                    ()
                    "~S should have warned ~S, but instead warned: ~A"
                    form expected warning))
          (assert style-warnings ()
                  "~S should have warned ~S, but didn't."
                  form expected)))))

(with-test (:name :duplicate-cases-load)
  (assert (load "case-test.lisp")))
