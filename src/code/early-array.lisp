;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;; The ANSI-specified minimum is 8.
;;; Adding the overhead words for a fancy array header plus 9 dimensions
;;; yields a payload length of 15, which is 4 bits.
(defconstant array-rank-limit 10
  "the exclusive upper bound on the rank of an array")

;;; - 2 to leave space for the array header
(defconstant array-dimension-limit (- most-positive-fixnum 2)
  "the exclusive upper bound on any given dimension of an array")

(defconstant array-total-size-limit (- most-positive-fixnum 2)
  "the exclusive upper bound on the total number of elements in an array")
