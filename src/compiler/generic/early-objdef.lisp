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
;;;   * Allocation code on Alpha wants lowtags for heap-allocated
;;;     objects to be odd.
;;; (These are just the ones we know about as of sbcl-0.7.1.22. There
;;; might easily be more, since these values have stayed highly
;;; constrained for more than a decade, an inviting target for
;;; inventive abstraction-phobic maintainers.:-)
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; The EVAL-WHEN is necessary (at least for Lispworks), because the
  ;; second DEFENUM uses the value of OTHER-IMMEDIATE-0-LOWTAG, which is
  ;; defined in the first DEFENUM. -- AL 20000216
  #!+#.(cl:if (cl:= 64 sb!vm:n-word-bits) '(and) '(or))
  (defenum ()
    even-fixnum-lowtag
    instance-pointer-lowtag
    other-immediate-0-lowtag
    pad0-lowtag
    pad1-lowtag pad2-lowtag
    other-immediate-1-lowtag
    list-pointer-lowtag
    odd-fixnum-lowtag
    fun-pointer-lowtag
    other-immediate-2-lowtag
    pad3-lowtag
    pad4-lowtag
    pad5-lowtag
    other-immediate-3-lowtag
    other-pointer-lowtag)
  #!+#.(cl:if (cl:= 32 sb!vm:n-word-bits) '(and) '(or))
  (defenum ()
    even-fixnum-lowtag
    instance-pointer-lowtag
    other-immediate-0-lowtag
    list-pointer-lowtag
    odd-fixnum-lowtag
    fun-pointer-lowtag
    other-immediate-1-lowtag
    other-pointer-lowtag))

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
(defenum (;; The first widetag must be greater than SB!VM:LOWTAG-LIMIT
          ;; otherwise code in generic/early-type-vops will suffer
          ;; a long, horrible death.  --njf, 2004-08-09
          :start (+ (ash 1 n-lowtag-bits) other-immediate-0-lowtag)
          :step 4)
  ;; NOTE: the binary numbers off to the side are only valid for 32-bit
  ;; ports; add #b1000 if you want to know the values for 64-bit ports.
  ;; And note that the numbers get a little scrambled further down.
  ;;   --njf, 2004-08-09
  bignum-widetag                            ; 00001010
  ratio-widetag                             ; 00001110
  single-float-widetag                      ; 00010010
  double-float-widetag                      ; 00010110
  complex-widetag                           ; 00011010
  complex-single-float-widetag              ; 00011110
  complex-double-float-widetag              ; 00100010

  code-header-widetag                       ; 00100110

  simple-fun-header-widetag                 ; 00101010
  closure-header-widetag                    ; 00101110
  funcallable-instance-header-widetag       ; 00110010

  return-pc-header-widetag                  ; 00110110
  value-cell-header-widetag                 ; 00111010
  symbol-header-widetag                     ; 00111110
  character-widetag                         ; 01000010
  sap-widetag                               ; 01000110
  unbound-marker-widetag                    ; 01001010
  weak-pointer-widetag                      ; 01001110
  instance-header-widetag                   ; 01010010
  fdefn-widetag                             ; 01010110

  no-tls-value-marker-widetag               ; 01011010
  #!-(and sb-lutex sb-thread)
  unused01-widetag
  #!+(and sb-lutex sb-thread)
  lutex-widetag                             ; 01011110
  unused02-widetag                          ; 01100010
  unused03-widetag                          ; 01100110
  unused04-widetag                          ; 01101010
  unused05-widetag                          ; 01101110
  unused06-widetag                          ; 01110010
  unused07-widetag                          ; 01110110
  #!+#.(cl:if (cl:= 32 sb!vm:n-word-bits) '(and) '(or))
  unused08-widetag                          ; 01111010
  #!+#.(cl:if (cl:= 32 sb!vm:n-word-bits) '(and) '(or))
  unused09-widetag                          ; 01111110

  #!+#.(cl:if (cl:= 32 sb!vm:n-word-bits) '(and) '(or))
  unused10-widetag                          ; 10000010
  #!+#.(cl:if (cl:= 32 sb!vm:n-word-bits) '(and) '(or))
  unused11-widetag                          ; 10000110

  simple-array-unsigned-byte-2-widetag      ; 10001010
  simple-array-unsigned-byte-4-widetag      ; 10001110
  simple-array-unsigned-byte-7-widetag      ; 10010010
  simple-array-unsigned-byte-8-widetag      ; 10010110
  simple-array-unsigned-byte-15-widetag     ; 10011010
  simple-array-unsigned-byte-16-widetag     ; 10011110
  simple-array-nil-widetag                  ; 10100010
  simple-base-string-widetag                ; 10100110
  #!+sb-unicode simple-character-string-widetag
  simple-bit-vector-widetag                 ; 10101010
  simple-vector-widetag                     ; 10101110
  #!+#.(cl:if (cl:= 32 sb!vm:n-word-bits) '(and) '(or))
  simple-array-unsigned-byte-29-widetag     ; 10110010
  simple-array-unsigned-byte-31-widetag     ; 10110110
  simple-array-unsigned-byte-32-widetag     ; 10111010
  #!+#.(cl:if (cl:= 64 sb!vm:n-word-bits) '(and) '(or))
  simple-array-unsigned-byte-60-widetag
  #!+#.(cl:if (cl:= 64 sb!vm:n-word-bits) '(and) '(or))
  simple-array-unsigned-byte-63-widetag
  #!+#.(cl:if (cl:= 64 sb!vm:n-word-bits) '(and) '(or))
  simple-array-unsigned-byte-64-widetag
  simple-array-signed-byte-8-widetag        ; 10111110
  simple-array-signed-byte-16-widetag       ; 11000010
  #!+#.(cl:if (cl:= 32 sb!vm:n-word-bits) '(and) '(or))
  simple-array-signed-byte-30-widetag       ; 11000110
  simple-array-signed-byte-32-widetag       ; 11001010
  #!+#.(cl:if (cl:= 64 sb!vm:n-word-bits) '(and) '(or))
  simple-array-signed-byte-61-widetag
  #!+#.(cl:if (cl:= 64 sb!vm:n-word-bits) '(and) '(or))
  simple-array-signed-byte-64-widetag
  simple-array-single-float-widetag         ; 11001110
  simple-array-double-float-widetag         ; 11010010
  simple-array-complex-single-float-widetag ; 11010110
  simple-array-complex-double-float-widetag ; 11011010
  simple-array-widetag                      ; 11011110
  complex-vector-nil-widetag                ; 11100010
  complex-base-string-widetag               ; 11100110
  #!+sb-unicode complex-character-string-widetag
  complex-bit-vector-widetag                ; 11101010
  complex-vector-widetag                    ; 11101110
  complex-array-widetag                     ; 11110010

  #!+#.(cl:if (cl:= 32 sb!vm:n-word-bits) '(and) '(or))
  unused12-widetag                          ; 11110110
  #!+(and #.(cl:if (cl:= 32 sb!vm:n-word-bits) '(and) '(or))
          (not sb-unicode))
  unused13-widetag                          ; 11111010
  #!+(and #.(cl:if (cl:= 32 sb!vm:n-word-bits) '(and) '(or))
          (not sb-unicode))
  unused14-widetag                          ; 11111110
)

;;; the different vector subtypes
(defenum ()
  vector-normal-subtype
  vector-unused-subtype
  vector-valid-hashing-subtype)
