(cl:in-package #:common-lisp-user)

(defpackage #:sb-simd-test-suite
  (:use #:common-lisp #:sb-simd-internals)
  (:export
   #:run-test-suite
   #:define-test
   #:is
   #:signals
   #:all-tests
   #:check-package
   #:run-tests
   #:define-simple-simd-test
   #:define-horizontal-test
   #:define-aref-test))
