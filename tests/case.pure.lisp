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
    (multiple-value-bind (fun warnings-p failure-p)
        (handler-bind ((style-warning (lambda (c)
                        (if expected
                          (assert (search expected
                                          (with-standard-io-syntax
                                            (let ((*print-right-margin* nil)
                                                  (*print-pretty* t))
                                              (remove #\Newline (princ-to-string c)))))
                                  ()
                                  "~S should have warned ~S, but instead warned: ~A" form expected c)
                          (error "~S shouldn't give a warning, but did: ~A" form c))
                        (setf expected nil))))
          (compile nil form))
      (assert (functionp fun))
      (assert (null expected)
              ()
              "~S should have warned ~S, but didn't."
              form expected)
      (assert (not failure-p))))
