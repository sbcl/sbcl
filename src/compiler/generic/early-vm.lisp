;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

(eval-when (:compile-toplevel :execute :load-toplevel)

(defconstant lowtag-bits 3
  #!+sb-doc
  "Number of bits at the low end of a pointer used for type information.")

(defconstant lowtag-mask (1- (ash 1 lowtag-bits))
  #!+sb-doc
  "Mask to extract the low tag bits from a pointer.")

(defconstant lowtag-limit (ash 1 lowtag-bits)
  #!+sb-doc
  "Exclusive upper bound on the value of the low tag bits from a pointer.")

(defconstant type-bits 8
  #!+sb-doc
  "Number of bits used in the header word of a data block to store the type.")

(defconstant type-mask (1- (ash 1 type-bits))
  #!+sb-doc
  "Mask to extract the type from a header word.")

); eval-when

;;; FIXME: Couldn't/shouldn't these be DEFCONSTANT instead of DEFPARAMETER?
(defparameter *target-most-positive-fixnum* (1- (ash 1 29))
  #!+sb-doc
  "most-positive-fixnum in the target architecture.")
(defparameter *target-most-negative-fixnum* (ash -1 29)
  #!+sb-doc
  "most-negative-fixnum in the target architecture.")
