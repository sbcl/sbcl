;;;; bit-vector hacking utilities, potentially implementation-dependent
;;;; for speed

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

(declaim (inline clear-bit-vector set-bit-vector bit-vector-replace
                 bit-vector-copy))

;;; Clear a SIMPLE-BIT-VECTOR to zeros.
(defun clear-bit-vector (vec)
  (declare (type simple-bit-vector vec))
  (fill vec 0))

;;; Fill a bit vector with ones.
(defun set-bit-vector (vec)
  (declare (type simple-bit-vector vec))
  (fill vec 1))

;;; Replace the bits in To with the bits in From.
(defun bit-vector-replace (to from)
  (declare (type simple-bit-vector to from))
  (replace to from))

;;; Copy a bit-vector.
(defun bit-vector-copy (vec)
  (declare (type simple-bit-vector vec))
  (copy-seq vec))
