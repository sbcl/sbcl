;;;; type-based constants

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

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
;;;
;;; Another way to look at lowtags is that there is no one lowtag
;;; length.  On 32-bit platforms, fixnums and other-immediates have a
;;; lowtag length of two bits, and pointers have a lowtag length of
;;; three bits.  On 64-bit platforms, fixnums and pointers gain an
;;; extra bit, and six "pad" lowtags waste the extra encoding space so
;;; obtained.
;;;
;;;  x00 -- fixnum
;;;  x10 -- other-immediate
;;;  001 -- instance-pointer
;;;  011 -- list-pointer
;;;  101 -- fun-pointer
;;;  111 -- other-pointer
;;;
;;; If you change the tag layout, check the various functions in
;;; src/runtime/runtime.h to see if they need to be updated, along
;;; with print_obj() in src/runtime/print.c, possibly 'late-objdef.lisp'
;;; and possibly the code in src/code/room.
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; The EVAL-WHEN is necessary (at least for Lispworks), because the
  ;; second DEFENUM uses the value of OTHER-IMMEDIATE-0-LOWTAG, which is
  ;; defined in the first DEFENUM. -- AL 20000216
  #+64-bit
  (defenum ()
    even-fixnum-lowtag
    other-immediate-0-lowtag
    pad0-lowtag
    instance-pointer-lowtag
    pad1-lowtag
    other-immediate-1-lowtag
    pad2-lowtag
    list-pointer-lowtag
    odd-fixnum-lowtag
    other-immediate-2-lowtag
    pad3-lowtag
    fun-pointer-lowtag
    pad4-lowtag
    other-immediate-3-lowtag
    pad5-lowtag
    other-pointer-lowtag)
  #-64-bit
  (defenum ()
    even-fixnum-lowtag
    instance-pointer-lowtag
    other-immediate-0-lowtag
    list-pointer-lowtag
    odd-fixnum-lowtag
    fun-pointer-lowtag
    other-immediate-1-lowtag
    other-pointer-lowtag))

(defconstant nil-value
    (+ static-space-start n-word-bytes other-pointer-lowtag))

(defconstant-eqx fixnum-lowtags
    #.(let ((fixtags nil))
        (do-external-symbols (sym "SB-VM")
          (let* ((name (symbol-name sym))
                 (len (length name)))
            (when (and (boundp sym)
                       (integerp (symbol-value sym))
                       (> len 7)
                       (string= name "-LOWTAG" :start1 (- len 7))
                       (zerop (logand (symbol-value sym) fixnum-tag-mask)))
              (push sym fixtags))))
        `',(sort fixtags #'string< :key #'symbol-name))
  #'equal)

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
;;;   But: There's not a snowball's chance that #+long-float works.
;;;        changeset 7646ae obliterated LONG-FLOAT-WIDETAG.
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
;;; rather than two separate tests and jumps.
;;; However, the cases where this is possible are few, and I'm not sure if
;;; that happened before or after rearrangement of widetags in change 30eccf.
;;; At present on 64-bit target with unicode we have:
;;;   (logcount (logxor complex-character-string-widetag simple-character-string-widetag)) = 2
;;;   (logcount (logxor complex-base-string-widetag simple-base-string-widetag)) = 2
;;;   (logcount (logxor complex-vector-nil-widetag simple-array-nil-widetag)) = 3
;;;   (logcount (logxor complex-bit-vector-widetag simple-bit-vector-widetag)) = 1
;;; and we have one winner.  The situation is slightly different for 32-bit.

;; FIXME: our nomenclature is baffling in more ways than implied by comments in
;; package-data-list regarding use of the word "complex" in both a numerical
;; sense and "non-simple" sense:
;; - There is no widetag specific to (AND (VECTOR T) (NOT SIMPLE-ARRAY)), i.e.
;; COMPLEX-VECTOR-WIDETAG is not the complex version of SIMPLE-VECTOR-WIDETAG
;; which is, in part, why the comment in x86/type-vops about optimizing a test
;; for (VECTOR T) is wrong.
;; - simple-array-nil should be named simple-rank-1-array-nil, though I think
;; simple-vector-nil is fine despite some reluctance to glue the particles
;; "simple" and "vector" together due to overtones of the standard meaning.
;; We could rename things that mean vector of wild type to VECTOR-*
;; and simple vector of T to SIMPLE-VECTOR-T. Just because CL says that
;; SIMPLE-VECTOR means the latter doesn't make it right for SBCL internals.

(eval-when (:compile-toplevel :load-toplevel :execute)
(defenum (;; The first widetag must be greater than SB-VM:LOWTAG-LIMIT
          ;; otherwise code in generic/early-type-vops will suffer
          ;; a long, horrible death.  --njf, 2004-08-09
          :start #.(+ (ash 1 n-lowtag-bits) other-immediate-0-lowtag)
          :step 4)
                                            ; +unicode -unicode
;;                             Word bits    ;  32 | 64  32 | 64
                                            ;------------------
                                            ; [ all numbers are hex ]
  bignum-widetag                            ;  0A   11  0A   11
  ratio-widetag                             ;  0E   15  0E   15
  single-float-widetag                      ;  12   19  12   19
  double-float-widetag                      ;  16   1D  16   1D
  complex-widetag                           ;  1A   21  1A   21
  complex-single-float-widetag              ;  1E   25  1E   25
  complex-double-float-widetag              ;  22   29  22   29

  code-header-widetag                       ;  26   2D  26   2D

  simple-fun-widetag                        ;  2A   31  2A   31
  closure-widetag                           ;  2E   35  2E   35
  funcallable-instance-widetag              ;  32   39  32   39

  ;; x86[-64] does not have objects with this widetag,
  #+(or x86 x86-64) unused00-widetag
  #-(or x86 x86-64)
  return-pc-widetag                         ;  36   3D  36   3D

  value-cell-widetag                        ;  3A   41  3A   41
  symbol-widetag                            ;  3E   45  3E   45
  character-widetag                         ;  42   49  42   49
  sap-widetag                               ;  46   4D  46   4D
  unbound-marker-widetag                    ;  4A   51  4A   51
  weak-pointer-widetag                      ;  4E   55  4E   55
  instance-widetag                          ;  52   59  52   59
  fdefn-widetag                             ;  56   5D  56   5D

  no-tls-value-marker-widetag               ;  5A   61  5A   61
  #-sb-simd-pack
  unused01-widetag                          ;  5E       5E
  #+sb-simd-pack
  simd-pack-widetag                         ;       65       65
  #-sb-simd-pack-256
  unused03-widetag                          ;  62   69  62   69
  #+sb-simd-pack-256
  simd-pack-256-widetag                     ;  62   69  62   69
  filler-widetag                            ;  66   6D  66   6D
  unused04-widetag                          ;  6A   71  6A   71
  unused05-widetag                          ;  6E   75  6E   75
  unused06-widetag                          ;  72   79  72   79
  unused07-widetag                          ;  76   7D  76   7D
  #-64-bit
  unused08-widetag                          ;  7A       7A
  #-64-bit
  unused09-widetag                          ;  7E       7E

  simple-array-widetag                      ;  82   81  82   81
  simple-array-unsigned-byte-2-widetag      ;  86   85  86   85
  simple-array-unsigned-byte-4-widetag      ;  8A   89  8A   89
  simple-array-unsigned-byte-7-widetag      ;  8E   8D  8E   8D
  simple-array-unsigned-byte-8-widetag      ;  92   91  92   91
  simple-array-unsigned-byte-15-widetag     ;  96   95  96   95
  simple-array-unsigned-byte-16-widetag     ;  9A   99  9A   99

  #-64-bit
  simple-array-unsigned-fixnum-widetag      ;  9E   A5  9E   A5
  simple-array-unsigned-byte-31-widetag     ;  A2   9D  A2   9D
  simple-array-unsigned-byte-32-widetag     ;  A6   A1  A6   A1
  #+64-bit
  simple-array-unsigned-fixnum-widetag      ;  9E   A5  9E   A5
  #+64-bit
  simple-array-unsigned-byte-63-widetag     ;       A9       A9
  #+64-bit
  simple-array-unsigned-byte-64-widetag     ;       AD       AD
  simple-array-signed-byte-8-widetag        ;  AA   B1  AA   B1
  simple-array-signed-byte-16-widetag       ;  AE   B5  AE   B5
  #-64-bit
  simple-array-fixnum-widetag               ;  B2   BD  B2   BD
  simple-array-signed-byte-32-widetag       ;  B6   B9  B6   B9
  #+64-bit
  simple-array-fixnum-widetag               ;  B2   BD  B2   BD
  #+64-bit
  simple-array-signed-byte-64-widetag       ;       C1       C1
  simple-array-single-float-widetag         ;  BA   C5  BA   C5
  simple-array-double-float-widetag         ;  BE   C9  BE   C9
  simple-array-complex-single-float-widetag ;  C2   CD  C2   CD
  simple-array-complex-double-float-widetag ;  C6   D1  C6   D1
  simple-bit-vector-widetag                 ;  CA   D5  CA   D5
  simple-vector-widetag                     ;  CE   D9  CE   D9

  ;; Strings
  simple-array-nil-widetag                  ;  D2   DD  D2   DD
  simple-base-string-widetag                ;  D6   E1  D6   E1
  #+sb-unicode
  simple-character-string-widetag           ;  DA   E5
  #+sb-unicode
  complex-character-string-widetag          ;  DE   E9
  complex-base-string-widetag               ;  E2   ED  DA   E5
  complex-vector-nil-widetag                ;  E6   F1  DE   E9

  complex-bit-vector-widetag                ;  EA   F5  E2   ED
  complex-vector-widetag                    ;  EE   F9  E6   F1
  complex-array-widetag                     ;  F2   FD  EA   F5
))

(defconstant-eqx +function-widetags+
    '#.(list funcallable-instance-widetag simple-fun-widetag closure-widetag)
  #'equal)

;;; the different vector subtypes - these are flag bits, not an enumeration
(defconstant vector-normal-subtype  0)
;; If vector is weak but NOT a hash-table backing vector
(defconstant vector-weak-subtype 1)
(defconstant vector-weak-visited-subtype 2) ; weak + GC bit
;; a valid-hashing vector might also be weak,
;; but we set ONLY the hashing bit when it backs a hash-table
(defconstant vector-valid-hashing-subtype 4)

;;; These next two constants must not occupy the same byte of a
;;; vector header word as the values in the preceding defenum.

;; A vector tagged as +VECTOR-SHAREABLE+ is logically readonly,
;; and permitted to be shared with another vector per the CLHS standard
;; under the concept of similarity as constant. A vector so tagged is
;; often the print-name of a symbol, or was a literal in source code
;; and loaded from a fasl, or used in a few others situations
;; which warrant sharing.
(defconstant +vector-shareable+ #x100)

;; A vector tagged as +VECTOR-SHAREABLE-NONSTD+ is logically readonly,
;; and *not* technically permitted by the standard to be shared.
;; If, despite the apparent prohibition, the user opts to make these
;; shareable, we'll do it. This typically occurs with compilation
;; into memory, where the requirement is that the machine code
;; reference "the same" object as appeared in source, but where,
;; nonetheless, opportunities for sharing abound.
(defconstant +vector-shareable-nonstd+ #x200)

;;; This is so that COMPILE-FILE knows that things like :ALLOW-OTHER-KEYS
;;; can be immediate constants.
;;; Note also that sb-fasteval uses 2 bits of the symbol header.
#+(and immobile-space (not immobile-symbols))
(defconstant +initial-core-symbol-bit+ 8) ; bit index, not bit value

#+immobile-space
(progn
  ;; See 'doc/internal-notes/compact-instance' for rationale
  (defconstant layout-align #+64-bit 128 #-64-bit 256) ; in bytes

  ;; FUNCTION-LAYOUT is a fixnum whose bits are ORed in "as-is" with the
  ;; low half of a closure header to form the full header word.
  #-sb-thread
  (defglobal function-layout 0))        ; set by genesis

#|
;; Run this in the SB-VM or SB-VM package once for each target feature combo.
(defun rewrite-widetag-comments ()
  (rename-file "src/compiler/generic/early-objdef.lisp" "early-objdef.old")
  (with-open-file (in "src/compiler/generic/early-objdef.old")
    (with-open-file (out "src/compiler/generic/early-objdef.lisp"
                         :direction :output :if-exists :supersede)
      (let* ((target-features
              (if (find-package "SB-COLD")
                  (symbol-value (find-symbol "*FEATURES*" "SB-XC"))
                  *features*))
             (feature-bits
              (+ (if (= n-word-bits 64) 1 0)
                 (if (member :sb-unicode target-features) 0 2)))
             (comment-col)
             (comment-offset (aref #(3 8 12 17) feature-bits))
             (state 0))
        (loop
         (let* ((line (read-line in nil))
                (trimmed (and line (string-left-trim " " line)))
                symbol)
           (unless line
             (return))
           (when (and (zerop state) (eql 0 (search "bignum-widetag" trimmed)))
             (setq state 1 comment-col (position #\; line)))
           (if (and (= state 1) (plusp (length trimmed))
                    (alpha-char-p (char trimmed 0))
                    (boundp (setq symbol (read-from-string trimmed))))
               (let ((new (make-string (+ comment-col 19)
                                       :initial-element #\Space)))
                 (replace new line)
                 (replace new (format nil "~2,'0X" (symbol-value symbol))
                          :start1 (+ comment-col comment-offset))
                 (write-line new out))
               (progn
                 (write-line line out)
                 (if (and (= state 1) (string= line ")"))
                     (setq state 2))))))))))
|#
