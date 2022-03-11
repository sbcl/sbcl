;;;; test of the pretty-printer

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

(with-test (:name :pprint-logical-block-#=nil)
  (assert (not (search "#1=" (write-to-string '(let () (let () x))
                                              :circle t
                                              :pretty t)))))
