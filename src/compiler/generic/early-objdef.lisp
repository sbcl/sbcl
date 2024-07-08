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
;;;   * OTHER-IMMEDIATE-0-LOWTAG are spaced 4 apart: various code wants to
;;;     iterate through these.  (This is not true on PPC64)
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
  #+(and 64-bit (not ppc64))
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
  #+ppc64
  (defenum ()
    even-fixnum-lowtag
    other-immediate-0-lowtag
    unused1-lowtag
    unused2-lowtag
    list-pointer-lowtag
    unused3-lowtag
    fun-pointer-lowtag
    unused4-lowtag
    odd-fixnum-lowtag
    unused5-lowtag
    unused6-lowtag
    unused7-lowtag
    instance-pointer-lowtag
    unused8-lowtag
    other-pointer-lowtag
    unused9-lowtag)
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

#+sb-xc-host
(defun lowtag-of (x)
  (etypecase x
    (symbol sb-vm:other-pointer-lowtag)
    (structure-object sb-vm:instance-pointer-lowtag)
    (list sb-vm:list-pointer-lowtag)
    ((eql 0) 0)))

#-sb-xc-host
(progn
(declaim (inline lowtag-of))
(defun lowtag-of (x) (logand (get-lisp-obj-address x) sb-vm:lowtag-mask)))

(defconstant-eqx fixnum-lowtags
    '#.(loop for i from 0 to lowtag-mask
             when (zerop (logand i fixnum-tag-mask)) collect i)
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
;;; * SIMPLE-CHARACTER-STRING + SIMPLE-BASE-STRING = SIMPLE-STRING
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
;;;   (logcount (logxor complex-bit-vector-widetag simple-bit-vector-widetag)) = 1
;;; and we have one winner.  The situation is slightly different for 32-bit.

;; FIXME: our nomenclature is baffling in more ways than implied by comments in
;; exports.lisp regarding use of the word "complex" in both a numerical
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

(defconstant widetag-spacing 4)
#+64-bit (defconstant unbound-marker-widetag 9)
(eval-when (:compile-toplevel :load-toplevel :execute)
(defenum (;; The first widetag must be greater than SB-VM:LOWTAG-LIMIT
          ;; otherwise code in generic/early-type-vops will suffer
          ;; a long, horrible death.  --njf, 2004-08-09
          :start #.(+ (ash 1 n-lowtag-bits) other-immediate-0-lowtag)
          :step 4)
;;                             Word bits          ;  32 | 64
                                                  ;---------
                                                  ; [ all numbers are hex ]

  ;; NOTE: If changing the widetags bracketed by the comment about EQL-hash,
  ;; check that the definition of stable_eql_hash_p() in gc-common is correct.
  ;; FIXME: EQL-HASH should treat SAP like a number
  ;; -- start of numeric widetags --
  bignum-widetag                                  ;  0A   11       \
  ratio-widetag                                   ;  0E   15       |
  single-float-widetag                            ;  12   19       |
  double-float-widetag                            ;  16   1D       | EQL-hash picks off this
  complex-rational-widetag                        ;  1A   21       | range of widetags.
  complex-single-float-widetag                    ;  1E   25       |
  complex-double-float-widetag                    ;  22   29       |
  ;; -- end of numeric widetags --                                 |
  symbol-widetag                                  ;  26   2D       /
  sap-widetag                                     ;  2A   31

  code-header-widetag                             ;  2E   35
  instance-widetag                                ;  32   39
  funcallable-instance-widetag                    ;  36   3D
  simple-fun-widetag                              ;  3A   41
  closure-widetag                                 ;  3E   45

  #-(or x86 x86-64 arm64 riscv) return-pc-widetag ;  42   49
  #+(or x86 x86-64 arm64 riscv) lra-widetag-notused

  value-cell-widetag                              ;  46   4D
  character-widetag                               ;  4A   51
  #-64-bit unbound-marker-widetag                 ;  4E   55
  #+64-bit unused00-widetag
  weak-pointer-widetag                            ;  52   59
  fdefn-widetag                                   ;  56   5D

  unused-widetag                                  ;  5A   61
  #+sb-simd-pack simd-pack-widetag                ;       65
  #-sb-simd-pack unused01-widetag                 ;  5E
  #+sb-simd-pack-256 simd-pack-256-widetag        ;       69
  #-sb-simd-pack-256 unused03-widetag             ;  62
  filler-widetag                                  ;  66   6D
  unused04-widetag                                ;  6A   71
  unused05-widetag                                ;  6E   75
  unused06-widetag                                ;  72   79
  unused07-widetag                                ;  76   7D
  #-64-bit unused08-widetag                       ;  7A
  #-64-bit unused09-widetag                       ;  7E

  simple-array-widetag                            ;  82   81
  ;; NIL element type is not in the contiguous range of widetags
  ;; corresponding to SIMPLE-UNBOXED-ARRAY
  simple-array-nil-widetag                        ;  86

  ;; IF YOU CHANGE THIS ORDER, THEN MANUALLY VERIFY CORRECTNESS OF:
  ;; - leaf_obj_widetag_p()
  ;; - readonly_unboxed_obj_p()
  ;; - conservative_root_p()
  ;; - anything else I forgot to mention
  simple-vector-widetag                           ;  8A
  simple-bit-vector-widetag                       ;  8E
  simple-array-unsigned-byte-2-widetag            ;  92
  simple-array-unsigned-byte-4-widetag            ;  96
  simple-array-unsigned-byte-7-widetag            ;  9A
  simple-array-unsigned-byte-8-widetag            ;  9E
  simple-array-unsigned-byte-15-widetag           ;  A2
  simple-array-unsigned-byte-16-widetag           ;  A6

  #-64-bit simple-array-unsigned-fixnum-widetag   ;
  simple-array-unsigned-byte-31-widetag           ;
  simple-array-unsigned-byte-32-widetag           ;
  #+64-bit simple-array-unsigned-fixnum-widetag   ;
  #+64-bit simple-array-unsigned-byte-63-widetag  ;
  #+64-bit simple-array-unsigned-byte-64-widetag  ;
  simple-array-signed-byte-8-widetag              ;
  simple-array-signed-byte-16-widetag             ;
  #-64-bit simple-array-fixnum-widetag            ;
  simple-array-signed-byte-32-widetag             ;
  #+64-bit simple-array-fixnum-widetag            ;
  #+64-bit simple-array-signed-byte-64-widetag    ;
  simple-array-single-float-widetag               ;
  simple-array-double-float-widetag               ;
  simple-array-complex-single-float-widetag       ;
  simple-array-complex-double-float-widetag       ;

  ;; WARNING: If you change the order of anything here,
  ;; be sure to examine COMPUTE-OBJECT-HEADER to see that it works
  ;; properly for all non-simple array headers.
  simple-base-string-widetag                      ;  D6   E1       \
  #+sb-unicode simple-character-string-widetag    ;  DA   E5       | Strings
  #-sb-unicode unused-simple-char-string          ;                |
  ;; From here down commence the non-simple array types            |
  complex-base-string-widetag                     ;  DE   E9       |
  #+sb-unicode complex-character-string-widetag   ;  E2   ED       /
  #-sb-unicode unused-complex-char-string

  complex-bit-vector-widetag                      ;  E6   F1
  complex-vector-widetag                          ;  EA   F5
  complex-array-widetag                           ;  EE   F9
  unused-array-widetag                            ;  F2   FD
))

;;; A filler cons whose first word is all 1s looks like this marker pattern,
;;; but there's no ambiguity, because no-tls-value can't appear in an object
;;; on the heap.
(defconstant no-tls-value-marker most-positive-word)

(defmacro unbound-marker-bits ()
  ;; By having unbound-marker-bits appear to be a valid pointer into static
  ;; space, it admits some optimizations that might "dereference" the value
  ;; before checking the widetag.
  ;; Architectures other than x86-64 do not take that liberty.
  #+x86-64 (logior (+ sb-vm:static-space-start #x100) unbound-marker-widetag)
  #-x86-64 unbound-marker-widetag)

;;; Map each widetag symbol to a string to go in 'tagnames.h'.
;;; I didn't want to mess with the formatting of the table above.
(defparameter *widetag-string-alist*
  `((bignum-widetag "bignum")
    (ratio-widetag "ratio")
    (single-float-widetag "sfloat")
    (double-float-widetag "dfloat")
    (complex-rational-widetag "cplxnum")
    (complex-single-float-widetag "cplx-sfloat")
    (complex-double-float-widetag "cplx-dfloat")
    (symbol-widetag "symbol")
    (instance-widetag "instance")
    (funcallable-instance-widetag "funinstance")
    (simple-fun-widetag "simplefun")
    (closure-widetag "closure")
    (code-header-widetag "codeblob")
    (return-pc-widetag "LRA")
    (value-cell-widetag "value-cell")
    (character-widetag "char")
    (sap-widetag "sap")
    (unbound-marker-widetag "unbound-marker")
    (weak-pointer-widetag "weakptr")
    (fdefn-widetag "fdefn")
    (simd-pack-widetag "SIMD-pack")
    (simd-pack-256-widetag "SIMD-pack256")
    (filler-widetag "filler")
    (simple-array-widetag "simple-array")
    (simple-array-nil-widetag "simple-array-NIL")
    (simple-vector-widetag "simple-vec")
    (simple-bit-vector-widetag "simple-bit-vec")
    (simple-array-fixnum-widetag "fixnum-vec")
    (simple-array-unsigned-fixnum-widetag "Ufixnum-vec")
    ;; Autogenerate the arrays. Not all values of N are used, but it's OK
    ,@(loop for n in '("2" "4" "7" "8" "15" "16" "31" "32" "63" "64")
            append `((,(symbolicate "SIMPLE-ARRAY-UNSIGNED-BYTE-" n "-WIDETAG")
                      ,(concatenate 'string "UB" n "-vec"))
                     (,(symbolicate "SIMPLE-ARRAY-SIGNED-BYTE-" n "-WIDETAG")
                      ,(concatenate 'string "SB" n "-vec"))))
    (simple-array-single-float-widetag "sfloat-vec")
    (simple-array-double-float-widetag "dfloat-vec")
    (simple-array-complex-single-float-widetag "cplx-sfloat-vec")
    (simple-array-complex-double-float-widetag "cplx-dfloat-vec")
    (simple-base-string-widetag "simple-base-str")
    (simple-character-string-widetag "simple-char-str")
    ;; I proposed on sbcl-devel some years ago to rename "complex" to "fancy"
    ;; to avoid confusion with complex numbers. That never gained traction.
    (complex-base-string-widetag "fancy-base-str")
    (complex-character-string-widetag "fancy-char-str")
    (complex-bit-vector-widetag "fancy-bit-vec")
    (complex-vector-widetag "fancy-vec")
    (complex-array-widetag "fancy-array")))

(defun widetag-string-name (symbol)
  ;; Asserting found
  (the string (second (assoc symbol *widetag-string-alist*))))

;;; Check that INSTANCE and FUNCALLABLE-INSTANCE differ at exactly 1 bit
;;; and that FUNCALLABLE-INSTANCE is the larger of the two widetags.
(eval-when (:compile-toplevel)
  (assert (= (logcount (logand (logxor instance-widetag funcallable-instance-widetag)
                               lowtag-mask))
             1))
  ;; Note: you must adjust FUNINSTANCE_SELECTOR_BIT_NUMBER (= 2) if the widetags change
  (assert (= funcallable-instance-widetag
             (logior instance-widetag (ash 1 2)))))

(defconstant-eqx +function-widetags+
    '#.(list funcallable-instance-widetag simple-fun-widetag closure-widetag)
  #'equal)

;;; the different vector flags can be ORed together.

;;; Byte index:           3           2           1           0
;;;                 +-----------------------------------+-----------+
;;;                 |  extra    |   flags   |    rank   |  widetag  |
;;;                 +-----------+-----------+-----------+-----------+
;;;                 |<---------- HEADER DATA ---------->|

;;; "extra" contain the following fields:
;;;  - generation number for immobile space (4 low bits of extra)
;;;  - VISITED bit (header bit index 30) for weak vectors, not used by Lisp
;;;  - ALLOC-DYNAMIC-EXTENT (bit index 29), not used by lisp
;;;  - ALLOC-MIXED-REGION (bit index 28), not used by Lisp

;; When I finish implementing "highly unsafe" dynamic-extent allocation, allowing
;; uninitialized SIMPLE-VECTORs on the stack, _correct_ Lisp code won't be affected
;; (it's an error to read before writing an element), nor will conservative GC.
;; But in order to make printing (Lisp and/or ldb) not crashy, we may need to act
;; as though *PRINT-ARRAY* were NIL on those.
;; (I don't know what to do if such a vector insists on being printed readably)
(defconstant +vector-alloc-dynamic-extent-bit+ (ash 1 29))

;; A vector with ALLOC-MIXED-REGION can never be moved to a purely boxed page,
;; and vectors not so marked could be. This would allow small simple-vectors
;; to benefit from the same GC optimization as large ones, wherein we scan
;; only their marked cards without regard to object base address.
;; A hash-table vector must not move to a pure boxed page, but we can't indicate
;; a vector as hashing until after it has been properly initialized.
;; The table algorithms expects to see certain values in the first and last elements,
;; and shoving all the initialization into pseudo-atomic would be nontrivial.
;; So we need a different flag bit that can be set immediately on creation,
;; just in case GC occurs before a hash table vector is flagged as hashing.
;; Note: ALLOCATE-VECTOR on 32-bit wants its first argument to be POSITIVE-FIXNUM.
;; so this is the highest bit index that can be used satisfying that restriction.
;; Once set, this bit can never be cleared.
(defconstant +vector-alloc-mixed-region-bit+ (ash 1 28))

;;; Rank and widetag adjacent lets SIMPLE-ARRAY-HEADER-OF-RANK-P be just one comparison.
(defconstant array-rank-position    8)
(defconstant array-flags-position  16)

(defconstant array-flags-data-position (- array-flags-position n-widetag-bits))

;; A vector tagged as +VECTOR-SHAREABLE+ is logically readonly,
;; and permitted to be shared with another vector per the CLHS standard
;; under the concept of similarity as constant. A vector so tagged is
;; often the print-name of a symbol, or was a literal in source code
;; and loaded from a fasl, or used in a few others situations
;; which warrant sharing.
(defconstant +vector-shareable+        #x20)

;; A vector tagged as +VECTOR-SHAREABLE-NONSTD+ is logically readonly,
;; and *not* technically permitted by the standard to be shared.
;; If, despite the apparent prohibition, the user opts to make these
;; shareable, we'll do it. This typically occurs with compilation
;; into memory, where the requirement is that the machine code
;; reference "the same" object as appeared in source, but where,
;; nonetheless, opportunities for sharing abound.
(defconstant +vector-shareable-nonstd+ #x10)

;; All arrays have a fill-pointer bit, but only vectors can have a 1 here.
(defconstant +array-fill-pointer-p+    #x80)
;; All hash-table backing vectors are marked with this bit.
;; Essentially it informs GC that the vector has a high-water mark.
(defconstant vector-hashing-flag       #x04)
;; Set if hash-table contains address-sensitive keys and possibly
;; an associated vector of 32-bit hashes.
;; When upsizing a table, both the old and new vector may have this bit set.
(defconstant vector-addr-hashing-flag  #x02)
;; Set if vector is weak. Weak hash tables have both this AND the hashing bit.
(defconstant vector-weak-flag          #x01)

;;; A symbol that is "unlocked" is neither constant, nor global,
;;; nor a global symbol-macro, nor in a locked package. Such symbols can be
;;; bound by PROGV without calling ABOUT-TO-MODIFY-SYMBOL-VALUE, except in the
;;; case where PROGV invokes UNBIND.
;;; This is a mask tested against GET-HEADER-DATA, so skip over the payload size byte.
(defconstant +symbol-fast-bindable+ #x100)
;;; bit index 30 peacefully co-exists with the immobile-space generation number
;;; in the low 4 bits of the same byte.
(defconstant +symbol-initial-core+ #x400000)

;;; Bit indices of the status bits in an INSTANCE header
;;; that implement lazily computed stable hash codes.
(defconstant stable-hash-required-flag 8)
(defconstant hash-slot-present-flag    9)

#+immobile-space
(progn
  ;; FUNCTION-LAYOUT is a fixnum whose bits are ORed in "as-is" with the
  ;; low half of a closure header to form the full header word.
  #-sb-thread
  (defglobal function-layout 0))        ; set by genesis

;;; These regions occupy the initial words of static space.
#-sb-thread
(progn
  (defconstant mixed-region-offset 0)
  (defconstant cons-region-offset (* 3 n-word-bytes))
  (defconstant boxed-region-offset (* 6 n-word-bytes)))

#|
;; Run this in the SB-VM package once for each target feature combo.
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
