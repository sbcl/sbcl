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

(in-package :cl-user)

;;; +MAGIC-HASH-VECTOR-VALUE+ is used to mark empty entries in the slot
;;; HASH-VECTOR of hash tables. It must be a value outside of the range
;;; of SXHASH. The range of SXHASH is the non-negative fixnums.
(assert (not (typep sb-impl::+magic-hash-vector-value+
                    '(and fixnum unsigned-byte))))
