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

(file-comment
  "$Header$")

;;; FIXME: It's clever using :SUFFIX -TYPE for these things, but it's
;;; a pain for people just learning to find their way around the code
;;; who want to figure out where things like EVEN-FIXNUM type are
;;; defined. Remove the :SUFFIXes and just expand out the full names.

;;; the main types. These types are represented by the low three bits of the
;;; pointer or immeditate object.
(defenum (:suffix -type)
  even-fixnum
  function-pointer
  other-immediate-0
  list-pointer
  odd-fixnum
  instance-pointer
  other-immediate-1
  other-pointer)

;;; the heap types. Each of these types is in the header of objects in
;;; the heap.
(defenum (:suffix -type
	  :start (+ (ash 1 lowtag-bits) other-immediate-0-type)
	  :step (ash 1 (1- lowtag-bits)))
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
  function-header
  closure-header
  funcallable-instance-header
  byte-code-function
  byte-code-closure
  closure-function-header
  #!-gengc return-pc-header
  #!+gengc forwarding-pointer
  value-cell-header
  symbol-header
  base-char
  sap
  unbound-marker
  weak-pointer
  instance-header
  fdefn
  )

;;; the different vector subtypes
(defenum (:prefix vector- :suffix -subtype)
  normal
  unused
  valid-hashing
  must-rehash)
