;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;; the number of bits at the low end of a pointer used for type
;;; information
(defconstant lowtag-bits 3)
;;; a mask to extract the low tag bits from a pointer
(defconstant lowtag-mask (1- (ash 1 lowtag-bits)))
;;; the exclusive upper bound on the value of the low tag bits from a
;;; pointer
(defconstant lowtag-limit (ash 1 lowtag-bits))

;;; the number of bits used in the header word of a data block to store
;;; the type
(defconstant type-bits 8)
;;; a mask to extract the type from a data block header word
(defconstant type-mask (1- (ash 1 type-bits)))

;;; FIXME: Couldn't/shouldn't these be DEFCONSTANT instead of
;;; DEFPARAMETER? (It might seem even more tempting to make them
;;; SB!XC:MOST-POSITIVE-FIXNUM and SB!XC:MOST-NEGATIVE-FIXNUM,
;;; but that's probably not a good idea, since then we'd need
;;; to worry about the effect of UNCROSS in expressions like
;;; (DEFTYPE INDX3 () `(INTEGER 3 ,SB!XC:MOST-POSITIVE-FIXNUM)).)
(defparameter *target-most-positive-fixnum* (1- (ash 1 29))
  #!+sb-doc
  "most-positive-fixnum in the target architecture")
(defparameter *target-most-negative-fixnum* (ash -1 29)
  #!+sb-doc
  "most-negative-fixnum in the target architecture")
