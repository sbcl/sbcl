;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(def!constant sb!xc:array-rank-limit 65529
  #!+sb-doc
  "the exclusive upper bound on the rank of an array")

;;; - 2 to leave space for the array header
(def!constant sb!xc:array-dimension-limit (- sb!xc:most-positive-fixnum 2)
  #!+sb-doc
  "the exclusive upper bound on any given dimension of an array")

(def!constant sb!xc:array-total-size-limit (- sb!xc:most-positive-fixnum 2)
  #!+sb-doc
  "the exclusive upper bound on the total number of elements in an array")
