;;;; type-based constants

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;; FIXME: It's clever using :SUFFIX -TYPE for these things, but it's
;;; a pain for people just learning to find their way around the code
;;; who want to use lexical search to figure out where things like
;;; EVEN-FIXNUM-LOWTAG are defined. Remove the :SUFFIXes and just expand
;;; out the full names. Or even define them in DEF-FROB EVEN-FIXNUM-LOWTAG
;;; style so searches like 'def.*even-fixnum-lowtag' can find them.

;;; tags for the main low-level types, to be stored in the low three
;;; bits to identify the type of a machine word 
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; The EVAL-WHEN is necessary (at least for Lispworks), because the
  ;; second DEFENUM uses the value of OTHER-IMMEDIATE-0-LOWTAG, which is
  ;; defined in the first DEFENUM. -- AL 20000216
  (defenum (:suffix -lowtag)
    even-fixnum
    ;; Note: CMU CL, and SBCL < 0.pre7.39, had FUN-POINTER-LOWTAG
    ;; here. We swapped FUN-POINTER-LOWTAG and
    ;; INSTANCE-POINTER-LOWTAG in sbcl-0.pre7.39 in order to help with a
    ;; low-level pun in the function call sequence on the PPC port.
    ;; For more information, see the PPC port code. -- WHN 2001-10-03
    instance-pointer
    other-immediate-0
    list-pointer
    odd-fixnum
    fun-pointer
    other-immediate-1
    other-pointer))

;;; the heap types, stored in 8 bits of the header of an object on the
;;; heap, to identify the type of the heap object (which'll be at
;;; least two machine words, often more)
(defenum (:suffix -widetag
	  :start (+ (ash 1 n-lowtag-bits) other-immediate-0-lowtag)
	  :step (ash 1 (1- n-lowtag-bits)))
  bignum
  ratio
  single-float
  double-float
  #!+long-float long-float
  complex
  complex-single-float
  complex-double-float
  #!+long-float complex-long-float

  simple-array
  simple-string
  simple-bit-vector
  simple-vector
  simple-array-unsigned-byte-2
  simple-array-unsigned-byte-4
  simple-array-unsigned-byte-8
  simple-array-unsigned-byte-16
  simple-array-unsigned-byte-32
  simple-array-signed-byte-8
  simple-array-signed-byte-16
  simple-array-signed-byte-30
  simple-array-signed-byte-32
  simple-array-single-float
  simple-array-double-float
  #!+long-float simple-array-long-float
  simple-array-complex-single-float
  simple-array-complex-double-float
  #!+long-float simple-array-complex-long-float
  complex-string
  complex-bit-vector
  complex-vector
  complex-array

  code-header
  simple-fun-header
  closure-header
  funcallable-instance-header
  closure-fun-header

  return-pc-header
  value-cell-header
  symbol-header
  base-char
  sap
  unbound-marker
  weak-pointer
  instance-header
  fdefn)

;;; the different vector subtypes
(defenum (:prefix vector- :suffix -subtype)
  normal
  unused
  valid-hashing
  must-rehash)
