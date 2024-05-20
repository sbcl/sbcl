;;;; This file is for compiler tests of passing structs by value to/from
;;;; foreign functions.

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
;(in-package :cl-user)
;;;; Bug 313202: C struct pass/return by value
;;; Compile and load shared library

#+win32
(with-scratch-file (dll "dll")
  (sb-ext:run-program "gcc" `("-shared" "-o" ,dll "alien-struct-by-value.c")
                      :search t)
  (load-shared-object dll))
#-win32
(progn
  (unless (probe-file "alien-struct-by-value.so")
    (sb-ext:run-program "/bin/sh" '("run-compiler.sh" "-sbcl-pic" "-sbcl-shared"
                                    "-o" "alien-struct-by-value.so"
                                    "alien-struct-by-value.c")))
  (load-shared-object (truename "alien-struct-by-value.so")))

(defmacro assert-unimplemented (def)
  `(assert-error (eval ',def)))
;;; Tiny struct, alignment 8
(define-alien-type nil (struct tiny-align-8 (m0 (integer 64))))
(with-test (:name :struct-by-value-tiny-align-8-args)
  (assert-unimplemented (define-alien-routine tiny-align-8-get-m0 (integer 64) (m (struct tiny-align-8))))
  (assert-unimplemented (define-alien-routine tiny-align-8-mutate void (m (struct tiny-align-8))))
  (assert-unimplemented (define-alien-routine tiny-align-8-return (struct tiny-align-8))))
;;; Small struct, alignment 8
(define-alien-type nil (struct small-align-8 (m0 (integer 64)) (m1 (integer 64))))
(with-test (:name :struct-by-value-small-align-8-args)
  (assert-unimplemented (define-alien-routine small-align-8-get-m0 (integer 64) (m (struct small-align-8))))
  (assert-unimplemented (define-alien-routine small-align-8-get-m1 (integer 64) (m (struct small-align-8))))
  (assert-unimplemented (define-alien-routine small-align-8-mutate void (m (struct small-align-8)))))
;;; Large struct, alignment 8
(define-alien-type nil
    (struct large-align-8
            (m0 (integer 64)) (m4 (integer 64)) (m8 (integer 64)) (m12 (integer 64))
            (m1 (integer 64)) (m5 (integer 64)) (m9 (integer 64)) (m13 (integer 64))
            (m2 (integer 64)) (m6 (integer 64)) (m10 (integer 64)) (m14 (integer 64))
            (m3 (integer 64)) (m7 (integer 64)) (m11 (integer 64)) (m15 (integer 64))))
(with-test (:name :struct-by-value-large-align-8-args)
  (macrolet
      ((def-large-align-8-get (i)
         (let ((lisp-name (sb-int:symbolicate "LARGE-ALIGN-8-GET-M" i)))
           `(assert-unimplemented
             (define-alien-routine ,lisp-name (integer 64) (m (struct large-align-8))))))
       (defs-large-align-8-get ()
         "Test functions for each member"
         (let ((defs (loop for i upto 15 collect `(def-large-align-8-get ,i))))
           `(progn ,@defs))))
    (defs-large-align-8-get)
    (assert-unimplemented
     (define-alien-routine large-align-8-mutate void (m (struct large-align-8))))))

;;; Clean up
#-win32 (delete-file "alien-struct-by-value.so")
