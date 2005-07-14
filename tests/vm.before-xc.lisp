;;;; tests of the compiler vm internal consistency intended to be
;;;; executed as soon as the cross-compiler is built.

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

(in-package "SB!VM")

(/show "beginning tests/vm.before-xc.lisp")

(flet ((yes (x)
         (assert
          (eql (sc-number-or-lose 'immediate)
               (impl-of-vm-support-routine-immediate-constant-sc x))))
       (no (x)
         (assert
          (not (impl-of-vm-support-routine-immediate-constant-sc x)))))
  ;; target fixnums can be dealt with as immediates; target bignums
  ;; can not.
  (yes #.sb-xc:most-positive-fixnum)
  (yes #.sb-xc:most-negative-fixnum)
  (no #.(1+ sb-xc:most-positive-fixnum))
  (no #.(1- sb-xc:most-negative-fixnum)))

(/show "done with tests/vm.before-xc.lisp")
