;;;; signatures of machine-specific functions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;;; internal type predicates

;;; Simple TYPEP uses that don't have any standard predicate are
;;; translated into non-standard unary predicates.
(defknown (fixnump bignump ratiop
	   short-float-p single-float-p double-float-p long-float-p
	   complex-rational-p complex-float-p complex-single-float-p
	   complex-double-float-p #!+long-float complex-long-float-p
	   complex-vector-p
	   base-char-p %standard-char-p %instancep
	   array-header-p
	   simple-array-p simple-array-unsigned-byte-2-p
	   simple-array-unsigned-byte-4-p simple-array-unsigned-byte-8-p
	   simple-array-unsigned-byte-16-p simple-array-unsigned-byte-32-p
	   simple-array-signed-byte-8-p simple-array-signed-byte-16-p
	   simple-array-signed-byte-30-p simple-array-signed-byte-32-p
	   simple-array-single-float-p simple-array-double-float-p
	   #!+long-float simple-array-long-float-p
	   simple-array-complex-single-float-p
	   simple-array-complex-double-float-p
	   #!+long-float simple-array-complex-long-float-p
	   system-area-pointer-p realp unsigned-byte-32-p signed-byte-32-p
	   vector-t-p weak-pointer-p code-component-p lra-p
	   funcallable-instance-p)
  (t) boolean (movable foldable flushable))

;;;; miscellaneous "sub-primitives"

(defknown %sp-string-compare
  (simple-string index index simple-string index index)
  (or index null)
  (foldable flushable))

(defknown %sxhash-simple-string (simple-string) index
  (foldable flushable))

(defknown %sxhash-simple-substring (simple-string index) index
  (foldable flushable))

(defknown vector-length (vector) index (flushable))

(defknown vector-sap ((simple-unboxed-array (*))) system-area-pointer
  (flushable))

(defknown get-lowtag (t) (unsigned-byte #.sb!vm:n-lowtag-bits)
  (flushable movable))
(defknown get-type (t) (unsigned-byte #.sb!vm:n-widetag-bits)
  (flushable movable))

(defknown (get-header-data get-closure-length) (t) (unsigned-byte 24)
  (flushable))
(defknown set-header-data (t (unsigned-byte 24)) t
  (unsafe))


(defknown %make-instance (index) instance
  (unsafe))
(defknown %instance-layout (instance) layout
  (foldable flushable))
(defknown %set-instance-layout (instance layout) layout
  (unsafe))
(defknown %instance-length (instance) index
  (foldable flushable))
(defknown %instance-ref (instance index) t
  (flushable))
(defknown %instance-set (instance index t) t
  (unsafe))
(defknown %layout-invalid-error (t layout) nil)


(sb!xc:deftype raw-vector () '(simple-array (unsigned-byte 32) (*)))

(defknown %raw-ref-single (raw-vector index) single-float
  (foldable flushable))
(defknown %raw-ref-double (raw-vector index) double-float
  (foldable flushable))
#!+long-float
(defknown %raw-ref-long (raw-vector index) long-float
  (foldable flushable))
(defknown %raw-set-single (raw-vector index single-float) single-float
  (unsafe))
(defknown %raw-set-double (raw-vector index double-float) double-float
  (unsafe))
#!+long-float
(defknown %raw-set-long (raw-vector index long-float) long-float
  (unsafe))

(defknown %raw-ref-complex-single (raw-vector index) (complex single-float)
  (foldable flushable))
(defknown %raw-ref-complex-double (raw-vector index) (complex double-float)
  (foldable flushable))
#!+long-float
(defknown %raw-ref-complex-long (raw-vector index) (complex long-float)
  (foldable flushable))
(defknown %raw-set-complex-single (raw-vector index (complex single-float))
  (complex single-float)
  (unsafe))
(defknown %raw-set-complex-double (raw-vector index (complex double-float))
  (complex double-float)
  (unsafe))
#!+long-float
(defknown %raw-set-complex-long (raw-vector index (complex long-float))
  (complex long-float)
  (unsafe))

(defknown %raw-bits (t fixnum) (unsigned-byte 32)
  (foldable flushable))
(defknown (%set-raw-bits) (t fixnum (unsigned-byte 32)) (unsigned-byte 32)
  (unsafe))


(defknown allocate-vector ((unsigned-byte 8) index index) (simple-array * (*))
  (flushable movable))

(defknown make-array-header ((unsigned-byte 8) (unsigned-byte 24)) array
  (flushable movable))


(defknown make-weak-pointer (t) weak-pointer
  (flushable))

(defknown %make-complex (real real) complex
  (flushable movable))
(defknown %make-ratio (rational rational) ratio
  (flushable movable))
(defknown make-value-cell (t) t
  (flushable movable))

(defknown (dynamic-space-free-pointer binding-stack-pointer-sap
				      control-stack-pointer-sap)  ()
  system-area-pointer
  (flushable))

;;;; debugger support

(defknown current-sp () system-area-pointer (movable flushable))
(defknown current-fp () system-area-pointer (movable flushable))
(defknown stack-ref (system-area-pointer index) t (flushable))
(defknown %set-stack-ref (system-area-pointer index t) t (unsafe))
(defknown lra-code-header (t) t (movable flushable))
(defknown fun-code-header (t) t (movable flushable))
(defknown make-lisp-obj ((unsigned-byte 32)) t (movable flushable))
(defknown get-lisp-obj-address (t) (unsigned-byte 32) (movable flushable))
(defknown fun-word-offset (function) index (movable flushable))

;;;; 32-bit logical operations

(defknown merge-bits ((unsigned-byte 5) (unsigned-byte 32) (unsigned-byte 32))
  (unsigned-byte 32)
  (foldable flushable movable))

(defknown 32bit-logical-not ((unsigned-byte 32)) (unsigned-byte 32)
  (foldable flushable movable))

(defknown (32bit-logical-and 32bit-logical-nand
	   32bit-logical-or 32bit-logical-nor
	   32bit-logical-xor 32bit-logical-eqv
	   32bit-logical-andc1 32bit-logical-andc2
	   32bit-logical-orc1 32bit-logical-orc2)
	  ((unsigned-byte 32) (unsigned-byte 32)) (unsigned-byte 32)
  (foldable flushable movable))

(defknown (shift-towards-start shift-towards-end) ((unsigned-byte 32) fixnum)
  (unsigned-byte 32)
  (foldable flushable movable))

;;;; bignum operations

(defknown %allocate-bignum (bignum-index) bignum-widetag
  (flushable))

(defknown %bignum-length (bignum-widetag) bignum-index
  (foldable flushable movable))

(defknown %bignum-set-length (bignum-widetag bignum-index) bignum-widetag
  (unsafe))

(defknown %bignum-ref (bignum-widetag bignum-index) bignum-element-type
  (flushable))

(defknown %bignum-set (bignum-widetag bignum-index bignum-element-type)
  bignum-element-type
  (unsafe))

(defknown %digit-0-or-plusp (bignum-element-type) boolean
  (foldable flushable movable))

(defknown (%add-with-carry %subtract-with-borrow)
	  (bignum-element-type bignum-element-type (mod 2))
  (values bignum-element-type (mod 2))
  (foldable flushable movable))

(defknown %multiply-and-add
	  (bignum-element-type bignum-element-type bignum-element-type
			       &optional bignum-element-type)
  (values bignum-element-type bignum-element-type)
  (foldable flushable movable))

(defknown %multiply (bignum-element-type bignum-element-type)
  (values bignum-element-type bignum-element-type)
  (foldable flushable movable))

(defknown %lognot (bignum-element-type) bignum-element-type
  (foldable flushable movable))

(defknown (%logand %logior %logxor) (bignum-element-type bignum-element-type)
  bignum-element-type
  (foldable flushable movable))

(defknown %fixnum-to-digit (fixnum) bignum-element-type
  (foldable flushable movable))

(defknown %floor (bignum-element-type bignum-element-type bignum-element-type)
  (values bignum-element-type bignum-element-type)
  (foldable flushable movable))

(defknown %fixnum-digit-with-correct-sign (bignum-element-type)
  (signed-byte #.sb!vm:n-word-bits)
  (foldable flushable movable))

(defknown (%ashl %ashr %digit-logical-shift-right)
	  (bignum-element-type (mod 32)) bignum-element-type
  (foldable flushable movable))

;;;; bit-bashing routines

(defknown copy-to-system-area
	  ((simple-unboxed-array (*)) index system-area-pointer index index)
  null
  ())

(defknown copy-from-system-area
	  (system-area-pointer index (simple-unboxed-array (*)) index index)
  null
  ())

(defknown system-area-copy
	  (system-area-pointer index system-area-pointer index index)
  null
  ())

(defknown bit-bash-copy
	  ((simple-unboxed-array (*)) index
	   (simple-unboxed-array (*)) index index)
  null
  ())

;;; (not really a bit-bashing routine, but starting to take over from
;;; bit-bashing routines in byte-sized copies as of sbcl-0.6.12.29:)
(defknown %byte-blt
  ((or (simple-unboxed-array (*)) system-area-pointer) index
   (or (simple-unboxed-array (*)) system-area-pointer) index index)
  null
  ())

;;;; code/function/fdefn object manipulation routines

(defknown code-instructions (t) system-area-pointer (flushable movable))
(defknown code-header-ref (t index) t (flushable))
(defknown code-header-set (t index t) t ())

(defknown function-subtype (function) (unsigned-byte #.sb!vm:n-widetag-bits)
  (flushable))
(defknown ((setf function-subtype))
	  ((unsigned-byte #.sb!vm:n-widetag-bits) function)
  (unsigned-byte #.sb!vm:n-widetag-bits)
  ())

(defknown make-fdefn (t) fdefn (flushable movable))
(defknown fdefn-p (t) boolean (movable foldable flushable))
(defknown fdefn-name (fdefn) t (foldable flushable))
(defknown fdefn-fun (fdefn) (or function null) (flushable))
(defknown (setf fdefn-fun) (function fdefn) t (unsafe))
(defknown fdefn-makunbound (fdefn) t ())

(defknown %simple-fun-self (function) function
  (flushable))
(defknown (setf %simple-fun-self) (function function) function
  (unsafe))

(defknown %closure-fun (function) function
  (flushable))

(defknown %closure-index-ref (function index) t
  (flushable))

(defknown %make-funcallable-instance (index layout) function
  (unsafe))

(defknown %funcallable-instance-info (function index) t (flushable))
(defknown %set-funcallable-instance-info (function index t) t (unsafe))

;;;; mutator accessors

(defknown mutator-self () system-area-pointer (flushable movable))
