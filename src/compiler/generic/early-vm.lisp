;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;; the number of bits per byte, where a byte is the smallest
;;; addressable object
(defconstant n-byte-bits 8)

;;; the number of bits at the low end of a pointer used for type
;;; information
(defconstant n-lowtag-bits
  (integer-length (1- (/ (* 2 n-word-bits) n-byte-bits))))
;;; a mask to extract the low tag bits from a pointer
(defconstant lowtag-mask (1- (ash 1 n-lowtag-bits)))
;;; the exclusive upper bound on the value of the low tag bits from a
;;; pointer
(defconstant lowtag-limit (ash 1 n-lowtag-bits))
;;; the number of tag bits used for a fixnum
(defconstant n-fixnum-tag-bits
  ;; On 64-bit targets, this may be as low as 1 (for 63-bit
  ;; fixnums) and as high as 3 (for 61-bit fixnums).  The
  ;; constraint on the low end is that we need at least one bit
  ;; to determine if a value is a fixnum or not, and the
  ;; constraint on the high end is that it must not exceed
  ;; WORD-SHIFT (defined below) due to the use of unboxed
  ;; word-aligned byte pointers as boxed values in various
  ;; places.
  #+64-bit (or #+ppc64 3 #-ppc64 1)
  ;; On 32-bit targets, this may be as low as 2 (for 30-bit
  ;; fixnums) and as high as 2 (for 30-bit fixnums).  The
  ;; constraint on the low end is simple overcrowding of the
  ;; lowtag space, and the constraint on the high end is that it
  ;; must not exceed WORD-SHIFT.
  #-64-bit (1- n-lowtag-bits))
;;; the fixnum tag mask
(defconstant fixnum-tag-mask (1- (ash 1 n-fixnum-tag-bits)))
;;; the bit width of fixnums
(defconstant n-fixnum-bits (- n-word-bits n-fixnum-tag-bits))
;;; the bit width of positive fixnums
(defconstant n-positive-fixnum-bits (1- n-fixnum-bits))

;;; the number of bits to shift between word addresses and byte addresses
(defconstant word-shift (1- (integer-length (/ n-word-bits n-byte-bits))))

;;; the number of bytes in a word
(defconstant n-word-bytes (/ n-word-bits n-byte-bits))

;;; the number of bytes in a machine word
(defconstant n-machine-word-bytes (/ n-machine-word-bits n-byte-bits))

;;; the number of bits used in the header word of a data block to store
;;; the type
(defconstant n-widetag-bits 8)
;;; a mask to extract the type from a data block header word
(defconstant widetag-mask (1- (ash 1 n-widetag-bits)))

(defconstant most-positive-fixnum
    (1- (ash 1 n-positive-fixnum-bits))
  "the fixnum closest in value to positive infinity")
(defconstant most-negative-fixnum
    (ash -1 n-positive-fixnum-bits)
  "the fixnum closest in value to negative infinity")

(defconstant most-positive-word (1- (expt 2 n-word-bits))
  "The most positive integer that is of type SB-EXT:WORD.")

(defconstant sb-bignum:maximum-bignum-length
  ;; 32-bit: leave one bit for a GC mark bit
  #-64-bit (ldb (byte (- n-word-bits n-widetag-bits 1) 0) -1)
  ;; 64-bit: restrict to a reasonably large theoretical size of 32GiB per bignum.
  ;; I haven't exercised our math routines to anywhere near that limit.
  #+64-bit #xFFFFFFFF)

;;; The ANSI-specified minimum is 8.
;;; Since the array rank is stored as rank-1 in the array header,
;;; having it stop at 128 ensures that adding 1 produces an unsigned
;;; result.
(defconstant array-rank-limit 129
  "the exclusive upper bound on the rank of an array")

(defconstant max-dynamic-space-size
  (or #+soft-card-marks
      (* (/ (expt 2 31) sb-vm::cards-per-page) sb-c:+backend-page-bytes+)
      (1- (expt 2 sb-vm:n-word-bits))))

;;; FIXME: these limits are wrong at the most basic level according to the spec,
;;; because if there are different limits for different element types, then
;;; this constant should be the _smallest_ of the limits for any element type.
;;; The largest element size is (COMPLEX DOUBLE-FLOAT) taking 16 bytes per element.
;;; If you had an array of this size on 32-bit machines, it would consume about
;;; 8GB of payload. So not only is it larger than a fixnum - a problem for the
;;; allocator vops - it exceeds the address space.
#-ubsan ; usual way
(progn
;;; - 2 to leave space for the array header
(defconstant array-dimension-limit (min (* max-dynamic-space-size
                                           ;; for bit-arrays
                                           n-byte-bits)
                                        (- most-positive-fixnum 2))
  "the exclusive upper bound on any given dimension of an array")

(defconstant array-total-size-limit array-dimension-limit
  "the exclusive upper bound on the total number of elements in an array")
)

#+ubsan ; only supported if 64-bit
(progn
(defconstant array-dimension-limit (ash 1 30))
(defconstant array-total-size-limit (ash 1 30)))

(defconstant char-code-limit #-sb-unicode 256 #+sb-unicode #x110000
  "the upper exclusive bound on values produced by CHAR-CODE")

(defconstant base-char-code-limit #-sb-unicode 256 #+sb-unicode 128)

;;; the size of the chunks returned by RANDOM-CHUNK
(defconstant n-random-chunk-bits 32)

;;; Internal time format.
;;; 61 bits should give
;;; seventy-three million one hundred seventeen thousand eight hundred two years of runtime
;;; It's dangerous to run SBCL for that long without updating.
;;; And it'll be a fixnum on 64-bit targets.
;;; The result from querying get-internal-run-time with multiple cores
;;; running full tilt will exhaust this faster, but it's still plenty enough.
(defconstant sb-kernel::internal-time-bits 61)

(defconstant most-positive-exactly-single-float-integer
  (expt 2 single-float-digits))
(defconstant most-negative-exactly-single-float-integer
  (- (expt 2 single-float-digits)))
(defconstant most-positive-exactly-double-float-integer
  (expt 2 double-float-digits))
(defconstant most-negative-exactly-double-float-integer
  (- (expt 2 double-float-digits)))

;; The lowest index that you can pass to %INSTANCE-REF accessing
;; a slot of data that is not the instance-layout.
;; To get a layout, you must call %INSTANCE-LAYOUT - don't assume index 0.
(defconstant instance-data-start (+ #-compact-instance-header 1))

;;; The largest number that may appear in the header-data for closures
;;; and funcallable instances.
;;; This constraint exists because for objects managed by the immobile GC,
;;; their generation number is stored in the header, so we have to know
;;; how much to mask off to obtain the payload size.
;;; Objects whose payload gets capped to this limit are considered
;;; "short_boxed" objects in the sizetab[] array in 'gc-common'.
;;; Additionally there are "tiny_boxed" objects, the payload length of
;;; which can be expressed in 8 bits.
(defconstant short-header-max-words #x7fff)

#+generational
(progn
  (defconstant gencgc-page-words (/ gencgc-page-bytes n-word-bytes))
  ;; Preventing use of the last 2 words on a page ensures that we never
  ;; "extend" an allocation region of conses from one page to the next page.
  ;; All other things being equal, it's better for each page to start
  ;; a contiguous block.
  (defconstant max-conses-per-page
    (1- (floor gencgc-page-words 2))))

;;; Amount to righ-shift an instance header to get the length.
;;; Similar consideration as above with regard to use of generation# byte.
(defconstant instance-length-shift 10)
(defconstant instance-length-mask #x3FFF)

;;; Is X a fixnum in the target Lisp?
#+sb-xc-host
(defun fixnump (x)
  (and (integerp x)
       (<= most-negative-fixnum x most-positive-fixnum)))

#+sb-safepoint
;;; The offset from the fault address reported to the runtime to the
;;; END of the global safepoint page.
(defconstant gc-safepoint-trap-offset n-word-bytes)

#+sb-xc-host
(progn
  (deftype sb-xc:fixnum () `(signed-byte ,n-fixnum-bits))
  ;; %char-code seems to belong in 'cross-char' but our CHAR-CODE-LIMIT
  ;; is not defined by then.
  (deftype %char-code () `(integer 0 (,sb-xc:char-code-limit))))

(declaim (inline sb-vm:is-lisp-pointer))
(defun sb-vm:is-lisp-pointer (addr) ; Same as is_lisp_pointer() in C
  #-64-bit (oddp addr)
  #+ppc64 (= (logand addr #b101) #b100)
  #+(and 64-bit (not ppc64)) (not (logtest (logxor addr 3) 3)))

;;; Convince some usage sites to perform fixnum arithmetic
(declaim (ftype (sfunction (t) (or (mod #.(length +static-symbols+)) boolean))
                static-symbol-p))
