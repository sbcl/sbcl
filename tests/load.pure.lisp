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

(in-package "CL-USER")

;;; As reported by Gregory Wright sbcl-devel 2002-07-15, SBCL 0.7.5
;;; didn't honor the LOAD :IF-DOES-NOT-EXIST argument when the type of
;;; the LOADed file was specified.
(load "i-am-not" :if-does-not-exist nil)
(load "i-am-not.lisp" :if-does-not-exist nil)
(load "i-am-not.fasl" :if-does-not-exist nil)
(load "i-am-not.misc-garbage" :if-does-not-exist nil)

;;; In 1.0.17, LOAD didn't error when the file didn't exist and
;;; IF-DOES-NOT-EXIST was true.
(assert (typep (nth-value 1 (ignore-errors (load "i-am-not"))) 'file-error))
(assert (typep (nth-value 1 (ignore-errors (load "i-am-not" :if-does-not-exist t))) 'file-error))

;; These tests are essentially the same as in compiler.pure.lisp
(with-test (:name :load-as-source-error-position-reporting)
  ;; These test errors that occur during READ
  (dolist (input '("data/wonky1.lisp" "data/wonky2.lisp" "data/wonky3.lisp"))
    (let ((expect (with-open-file (f input) (read f))))
      (assert (stringp expect))
      (let ((err-string
             (block foo
               ;; you can't query the stream position with HANDLER-CASE
               ;; because it closes before the condition is formatted.
               (handler-bind ((error (lambda (c)
                                       (return-from foo
                                         (write-to-string c :escape nil)))))
                 (load input)))))
        (assert (search expect err-string)))))

  ;; This tests an error that occur during EVAL
  (let ((s (with-output-to-string (*error-output*)
             (handler-bind ((error #'abort)) (load "data/wonky4.lisp")))))
    (assert (search "While evaluating the form starting at line 16, column 1"
                    s))))
