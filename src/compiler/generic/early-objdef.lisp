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
;;; out the full names. Or even define them in DEF EVEN-FIXNUM-LOWTAG
;;; style so searches like 'def.*even-fixnum-lowtag' can find them.

;;; Tags for the main low-level types are stored in the low n (usually three)
;;; bits to identify the type of a machine word.  Certain constraints 
;;; apply:
;;;   * EVEN-FIXNUM-LOWTAG and ODD-FIXNUM-LOWTAG must be 0 and 4: code
;;;     which shifts left two places to convert raw integers to tagged
;;;     fixnums is ubiquitous.
;;;   * LIST-POINTER-LOWTAG + N-WORD-BYTES = OTHER-POINTER-LOWTAG: NIL 
;;;     is both a cons and a symbol (at the same address) and depends on this.
;;;     See the definition of SYMBOL in objdef.lisp
;;;   * OTHER-POINTER-LOWTAG > 4: Some code in the SPARC backend,
;;;     which uses bit 2 of the ALLOC register to indicate that
;;;     PSEUDO-ATOMIC is on, doesn't strip the low bits of reg_ALLOC
;;;     before ORing in OTHER-POINTER-LOWTAG within a PSEUDO-ATOMIC
;;;     section.
;;;   * OTHER-IMMEDIATE-0-LOWTAG are spaced 4 apart: various code wants to 
;;;     iterate through these
;;; (These are just the ones we know about as of sbcl-0.7.1.22. There
;;; might easily be more, since these values have stayed highly
;;; constrained for more than a decade, an inviting target for
;;; inventive abstraction-phobic maintainers.:-)
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; The EVAL-WHEN is necessary (at least for Lispworks), because the
  ;; second DEFENUM uses the value of OTHER-IMMEDIATE-0-LOWTAG, which is
  ;; defined in the first DEFENUM. -- AL 20000216
  #!+x86-64
  (defenum (:suffix -lowtag)
    even-fixnum
    instance-pointer
    other-immediate-0
    pad0 pad1 pad2
    other-immediate-1
    list-pointer
    odd-fixnum
    fun-pointer
    other-immediate-2
    pad3 pad4 pad5
    other-immediate-3
    other-pointer)
  #!-x86-64
  (defenum (:suffix -lowtag)
    even-fixnum
    instance-pointer
    other-immediate-0
    list-pointer
    odd-fixnum
    fun-pointer
    other-immediate-1
    other-pointer))

(def!constant nil-value
    (+ static-space-start n-word-bytes other-pointer-lowtag))

;;; the heap types, stored in 8 bits of the header of an object on the
;;; heap, to identify the type of the heap object (which'll be at
;;; least two machine words, often more)
;;;
;;; Note: the order specified here is not critical for correctness,
;;; but (FIXME) with %TEST-HEADERS as currently defined, BIGNUM must
;;; be first, and COMPLEX-ARRAY must be last.
;;;
;;; However, for efficiency, we prefer contiguous sets of widetags for
;;; "similar" objects, so that type checking can be done with a range
;;; check, rather than several individual checks.
;;;
;;; * BIGNUM + RATIO (+ FIXNUM) = RATIONAL
;;;
;;; * SINGLE-FLOAT + DOUBLE-FLOAT + LONG-FLOAT = FLOAT
;;;
;;; * RATIONAL + FLOAT = REAL
;;;
;;; * (FIXME: COMPLEX example, which needs fixing anyway -- see
;;;   UPGRADED-COMPLEX-PART-TYPE)
;;;
;;; * SIMPLE-ARRAY-* = (SIMPLE-ARRAY * (*))
;;;
;;; * SIMPLE-ARRAY-NIL + SIMPLE-BASE-STRING = SIMPLE-STRING
;;;
;;; * SIMPLE-ARRAY + COMPLEX-ARRAYOID = (SATISFIES ARRAY-HEADER-P)
;;;
;;; In addition, with
;;; sufficient care we can cause extra combinations to appear with
;;; differences in only one bit, permitting a more efficient type
;;; test.  As an example, if SIMPLE-BASE-STRING = 0xA6 and
;;; COMPLEX-BASE-STRING = 0xE6, then the type test for BASE-STRING is
;;;
;;;   AND   tag, ~0x40, tag
;;;   ANDcc tag,  0xA6, tag
;;;   JNE   tag, label
;;;
;;; rather than two separate tests and jumps 
(defenum (:suffix -widetag
	  :start (+ (ash 1 n-lowtag-bits) other-immediate-0-lowtag)
	  :step 4)
  bignum
  ratio
  single-float
  double-float
  complex
  complex-single-float
  complex-double-float

  code-header
  simple-fun-header
  closure-header
  funcallable-instance-header

  return-pc-header
  value-cell-header
  symbol-header
  base-char
  sap
  unbound-marker
  weak-pointer
  instance-header
  fdefn

  unused00
  unused01
  unused02
  unused03
  unused04
  unused05
  unused06
  unused07
  unused08
  unused09
  
  unused10
  unused11

  simple-array-unsigned-byte-2
  simple-array-unsigned-byte-4
  simple-array-unsigned-byte-7
  simple-array-unsigned-byte-8
  simple-array-unsigned-byte-15
  simple-array-unsigned-byte-16
  simple-array-nil
  simple-base-string
  simple-bit-vector
  simple-vector
  simple-array-unsigned-byte-29
  simple-array-unsigned-byte-31
  simple-array-unsigned-byte-32
  simple-array-signed-byte-8
  simple-array-signed-byte-16
  simple-array-signed-byte-30
  simple-array-signed-byte-32
  simple-array-single-float
  simple-array-double-float
  simple-array-complex-single-float
  simple-array-complex-double-float
  simple-array
  complex-vector-nil
  complex-base-string
  complex-bit-vector
  complex-vector
  complex-array
)

;;; the different vector subtypes
(defenum (:prefix vector- :suffix -subtype)
  normal
  unused
  valid-hashing
  must-rehash)
