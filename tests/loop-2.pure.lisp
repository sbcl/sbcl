;;;; miscellaneous tests of LOOP-related stuff

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

;;; Bug 49b, reported by Peter Van Eynde 2000-07-25, was fixed by
;;; Alexey Dejneka's patch on sbcl-devel 2001-09-30.
;;;
(let ((package (make-package "loop-test-scratch")))
  (intern "blah" package)
  (let ((blah2 (intern "blah2" package)))
    (export blah2 package))
  (assert (equal '("blah" "blah2")
                 (sort (loop for sym being each present-symbol of package
                             for sym-name = (symbol-name sym)
                             collect sym-name)
                       #'string<)))
  (assert (equal '("blah2")
                 (sort (loop for sym being each external-symbol of package for
                             sym-name = (symbol-name sym) collect sym-name)
                       (function string<)))))

;;; success
