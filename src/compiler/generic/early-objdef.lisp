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

;;; FIXME: It's clever using :SUFFIX -LOWTAG for these things, but
;;; it's a pain for people just learning to find their way around the
;;; code who want to use lexical search to figure out where things
;;; like EVEN-FIXNUM-LOWTAG are defined. Remove the :SUFFIXes and just
;;; expand out the full names. Or even define them in DEF
;;; EVEN-FIXNUM-LOWTAG style so searches like
;;; 'def.*even-fixnum-lowtag' can find them.

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
  #!+#.(cl:if (cl:= 64 sb!vm:n-word-bits) '(and) '(or))
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
  #!+#.(cl:if (cl:= 32 sb!vm:n-word-bits) '(and) '(or))
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
          ;; The first widetag must be greater than SB!VM:LOWTAG-LIMIT
          ;; otherwise code in generic/early-type-vops will suffer
          ;; a long, horrible death.  --njf, 2004-08-09
	  :start (+ (ash 1 n-lowtag-bits) other-immediate-0-lowtag)
	  :step 4)
  ;; NOTE: the binary numbers off to the side are only valid for 32-bit
  ;; ports; add #x1000 if you want to know the values for 64-bit ports.
  ;; And note that the numbers get a little scrambled further down.
  ;;   --njf, 2004-08-09
  bignum                            ; 00001010
  ratio                             ; 00001110
  single-float                      ; 00010010
  double-float                      ; 00010110
  complex                           ; 00011010
  complex-single-float              ; 00011110
  complex-double-float              ; 00100010

  code-header                       ; 00100110

  simple-fun-header                 ; 00101010
  closure-header                    ; 00101110
  funcallable-instance-header       ; 00110010

  return-pc-header                  ; 00110110
  value-cell-header                 ; 00111010
  symbol-header                     ; 00111110
  character                         ; 01000010
  sap                               ; 01000110
  unbound-marker                    ; 01001010
  weak-pointer                      ; 01001110
  instance-header                   ; 01010010
  fdefn                             ; 01010110

  unused00                          ; 01011010
  unused01                          ; 01011110
  unused02                          ; 01100010
  unused03                          ; 01100110
  unused04                          ; 01101010
  unused05                          ; 01101110
  unused06                          ; 01110010
  unused07                          ; 01110110
  unused08                          ; 01111010
  unused09                          ; 01111110

  #!+#.(cl:if (cl:= 32 sb!vm:n-word-bits) '(and) '(or))
  unused10                          ; 10000010
  #!+#.(cl:if (cl:= 32 sb!vm:n-word-bits) '(and) '(or))
  unused11                          ; 10000110

  simple-array-unsigned-byte-2      ; 10001010
  simple-array-unsigned-byte-4      ; 10001110
  simple-array-unsigned-byte-7      ; 10010010
  simple-array-unsigned-byte-8      ; 10010110
  simple-array-unsigned-byte-15     ; 10011010
  simple-array-unsigned-byte-16     ; 10011110
  simple-array-nil                  ; 10100010
  simple-base-string                ; 10100110
  simple-character-string
  simple-bit-vector                 ; 10101010
  simple-vector                     ; 10101110
  #!+#.(cl:if (cl:= 32 sb!vm:n-word-bits) '(and) '(or))
  simple-array-unsigned-byte-29     ; 10110010
  simple-array-unsigned-byte-31     ; 10110110
  simple-array-unsigned-byte-32     ; 10111010
  #!+#.(cl:if (cl:= 64 sb!vm:n-word-bits) '(and) '(or))
  simple-array-unsigned-byte-60
  #!+#.(cl:if (cl:= 64 sb!vm:n-word-bits) '(and) '(or))
  simple-array-unsigned-byte-63
  #!+#.(cl:if (cl:= 64 sb!vm:n-word-bits) '(and) '(or))
  simple-array-unsigned-byte-64
  simple-array-signed-byte-8        ; 10111110
  simple-array-signed-byte-16       ; 11000010
  #!+#.(cl:if (cl:= 32 sb!vm:n-word-bits) '(and) '(or))
  simple-array-signed-byte-30       ; 11000110
  simple-array-signed-byte-32       ; 11001010
  #!+#.(cl:if (cl:= 64 sb!vm:n-word-bits) '(and) '(or))
  simple-array-signed-byte-61
  #!+#.(cl:if (cl:= 64 sb!vm:n-word-bits) '(and) '(or))
  simple-array-signed-byte-64
  simple-array-single-float         ; 11001110
  simple-array-double-float         ; 11010010
  simple-array-complex-single-float ; 11010110
  simple-array-complex-double-float ; 11011010
  simple-array                      ; 11011110
  complex-vector-nil                ; 11100010
  complex-base-string               ; 11100110
  complex-character-string
  complex-bit-vector                ; 11101010
  complex-vector                    ; 11101110
  complex-array                     ; 11110010

  #!+#.(cl:if (cl:= 32 sb!vm:n-word-bits) '(and) '(or))
  unused12                          ; 11110110
  #|
  #!+#.(cl:if (cl:= 32 sb!vm:n-word-bits) '(and) '(or))
  unused13                          ; 11111010
  #!+#.(cl:if (cl:= 32 sb!vm:n-word-bits) '(and) '(or))
  unused14                          ; 11111110
  |#
)

;;; the different vector subtypes
(defenum (:prefix vector- :suffix -subtype)
  normal
  unused
  valid-hashing
  must-rehash)
