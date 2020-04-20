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

(eval-when (:compile-toplevel)
  (load "compiler-test-util.lisp"))

(with-test (:name (:block-compile :defstruct-slot-type-circularity))
  (with-scratch-file (fasl "fasl")
    (compile-file "block-compile-defstruct-test.lisp" :output-file fasl :block-compile t)
    (load fasl))
  (dolist (symbol '(make-s1 make-s2 make-s3))
    (let ((constants
           (ctu:find-code-constants (symbol-function symbol)
                                    :type 'sb-kernel:layout)))
      (assert (= (length constants) 3)))))

;;; Check an organic (not contrived) use of mutually referential types.
;;; NEWLINE is defined after SECTION-START, because it is a subtype.
;;; One of SECTION-START's slot setters refers to type NEWLINE.
(with-test (:name :pretty-stream-structs)
  (let ((layouts
         (ctu:find-code-constants #'(setf sb-pretty::section-start-section-end)
                                  :type 'sb-kernel:layout)))
    ;; expect 3 layouts: one for SECTION-START to check the instance itself,
    ;; one for NEWLINE and one for BLOCK-END.
    ;; It's entirely coincidental that the above test also has 3.
    (assert (= (length layouts) 3))
    (assert (find (sb-kernel:find-layout 'sb-pretty::newline)
                  layouts))))

(with-test (:name :mutex-owner-typecheck)
  (let ((layouts
         (ctu:find-code-constants #'(setf sb-thread::mutex-%owner)
                                  :type 'sb-kernel:layout)))
    ;; expect 3 layouts: one for THREAD, one for MUTEX, and one for FOREIGN-THREAD
    (assert (= (length layouts) 3))
    (assert (find (sb-kernel:find-layout 'sb-thread:thread)
                  layouts))))
