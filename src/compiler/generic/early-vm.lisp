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
(def!constant n-lowtag-bits 3)
;;; a mask to extract the low tag bits from a pointer
(def!constant lowtag-mask (1- (ash 1 n-lowtag-bits)))
;;; the exclusive upper bound on the value of the low tag bits from a
;;; pointer
(def!constant lowtag-limit (ash 1 n-lowtag-bits))

;;; the number of bits used in the header word of a data block to store
;;; the type
(def!constant n-widetag-bits 8)
;;; a mask to extract the type from a data block header word
(def!constant widetag-mask (1- (ash 1 n-widetag-bits)))

(def!constant sb!xc:most-positive-fixnum (1- (ash 1 29))
  #!+sb-doc
  "the fixnum closest in value to positive infinity")
(def!constant sb!xc:most-negative-fixnum (ash -1 29)
  #!+sb-doc
  "the fixnum closest in value to negative infinity")
