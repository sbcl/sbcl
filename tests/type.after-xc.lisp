;;;; tests of the type system, intended to be executed in the
;;;; cross-compiler after cross-compilation

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

(in-package "SB-KERNEL")

(/show "beginning tests/type.after-xc.lisp")

;;; various dead bugs
(assert (eql *empty-type*
             (type-intersection *empty-type*
                                (specifier-type 'keyword))))
(assert (eql *empty-type*
             (type-intersection (specifier-type 'keyword)
                                *empty-type*)))
(assert (member-type-p (specifier-type '(or float-format null))))

(let ((fd-stream (specifier-type 'fd-stream)))
  (assert (type= fd-stream (type-intersection (specifier-type 'instance)
                                              fd-stream))))

(/show "done with tests/type.after-xc.lisp")
