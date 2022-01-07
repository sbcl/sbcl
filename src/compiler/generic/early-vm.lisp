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

(defconstant maximum-bignum-length
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
(defconstant array-dimension-limit (- most-positive-fixnum 2)
  "the exclusive upper bound on any given dimension of an array")

(defconstant array-total-size-limit (- most-positive-fixnum 2)
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

(defconstant most-positive-exactly-single-float-fixnum
  (min (expt 2 single-float-digits) most-positive-fixnum))
(defconstant most-negative-exactly-single-float-fixnum
  (max (- (expt 2 single-float-digits)) most-negative-fixnum))
(defconstant most-positive-exactly-double-float-fixnum
  (min (expt 2 double-float-digits) most-positive-fixnum))
(defconstant most-negative-exactly-double-float-fixnum
  (max (- (expt 2 double-float-digits)) most-negative-fixnum))

;;;; Point where continuous area starting at dynamic-space-start bumps into
;;;; next space. Computed for genesis/constants.h, not used in Lisp.
#+(and gencgc sb-xc-host)
(defconstant max-dynamic-space-end
    (let ((stop (1- (ash 1 n-word-bits)))
          (start dynamic-space-start))
      (dolist (other-start (list read-only-space-start static-space-start
                                 linkage-table-space-start))
        (declare (notinline <)) ; avoid dead code note
        (when (< start other-start)
          (setf stop (min stop other-start))))
      stop))

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

;;; Amount to righ-shift an instance header to get the length.
;;; Similar consideration as above with regard to use of generation# byte.
(defconstant instance-length-shift 10)
(defconstant instance-length-mask #x3FFF)

;;; Is X a fixnum in the target Lisp?
#+sb-xc-host
(defun fixnump (x)
  (and (integerp x)
       (<= most-negative-fixnum x most-positive-fixnum)))

;;; Helper macro for defining FIXUP-CODE-OBJECT with emulation of SAP
;;; accessors so that the host and target can use the same fixup logic.
#+(or x86 x86-64)
(defmacro with-code-instructions ((sap-var code-var) &body body)
  #+sb-xc-host
  `(macrolet ((self-referential-code-fixup-p (value self)
                `(let* ((base (sb-fasl::descriptor-base-address ,self))
                        (limit (+ base (1- (code-object-size ,self)))))
                   (<= base ,value limit)))
              (containing-memory-space (code)
                `(sb-fasl::descriptor-gspace-name ,code)))
     (let ((,sap-var (code-instructions ,code-var)))
       ,@body))
  #-sb-xc-host
  `(macrolet ((self-referential-code-fixup-p (value self)
                ;; Using SAPs keeps all the arithmetic machine-word-sized.
                ;; Otherwise it would potentially involve bignums on 32-bit
                ;; if code starts at #x20000000 and up.
                `(let* ((base (sap+ (int-sap (get-lisp-obj-address ,self))
                                    (- sb-vm:lowtag-mask)))
                        (limit (sap+ base (1- (code-object-size ,self))))
                        (value-sap (int-sap ,value)))
                   (and (sap>= value-sap base) (sap<= value-sap limit))))
              (containing-memory-space (code)
                (declare (ignore code))
                :dynamic))
    (let ((,sap-var (code-instructions ,code-var)))
      ,@body)))

#+sb-safepoint
;;; The offset from the fault address reported to the runtime to the
;;; END of the global safepoint page.
(defconstant gc-safepoint-trap-offset n-word-bytes)

#+sb-xc-host (deftype sb-xc:fixnum () `(signed-byte ,n-fixnum-bits))

#-darwin-jit
(progn
  (declaim (inline (setf sap-ref-word-jit)
                   (setf signed-sap-ref-32-jit)
                   signed-sap-ref-32-jit
                   (setf sap-ref-32-jit)
                   sap-ref-32-jit
                   (setf sap-ref-8-jit)))
  (defun (setf sap-ref-word-jit) (value sap offset)
    (setf (sap-ref-word sap offset) value))

  (defun (setf signed-sap-ref-32-jit) (value sap offset)
    (setf (signed-sap-ref-32 sap offset) value))

  (defun signed-sap-ref-32-jit (sap offset)
    (signed-sap-ref-32 sap offset))

  (defun (setf sap-ref-32-jit) (value sap offset)
    (setf (sap-ref-32 sap offset) value))

  (defun sap-ref-32-jit (sap offset)
    (sap-ref-32 sap offset))

  #-sb-xc-host
  (defun (setf sap-ref-8-jit) (value sap offset)
    (setf (sap-ref-8 sap offset) value)))

;;; Supporting code for LAYOUT allocated in metadata space a/k/a "metaspace".
;;; These objects are manually allocated and freed.
;;; Tracts are the unit of allocation requested from the OS.
;;; Following the nomenclature in https://en.wikipedia.org/wiki/Slab_allocation
;;; - A slab is the quantum requested within a tract.
;;; - Chunks are the units of allocation ("objects") within a slab.

(defconstant metaspace-tract-size (* 2 1024 1024)) ; 2 MiB
(defconstant metaspace-slab-size 2048) ; 2 KiB

;;; Chunks are all the same size per slab, which makes finding the next available
;;; slot simply a pop from a freelist. The object stored in the chunk can be anything
;;; not to exceed the chunk size. (You can't span chunks with one object)
;;; All slabs of a given size with any available space are kept in a doubly-linked
;;; list for the slab's chunk size.  A slab is removed from the doubly-linked list and
;;; moved to the slab recycle list when all chunks within it become free.
;;;
;;; There are 4 words of overhead per slab.
;;; At present only 64-bit word size is supported, so we assume
;;; that pointer-sized words in the slab header consume 8 bytes.
;;;   word 0: 2 bytes : sizeclass (as a FIXNUM)
;;;           2 bytes : capacity in objects
;;;           2 bytes : chunk size
;;;           2 bytes : number of objects allocated in it
;;;   word 1: head of freelist
;;;   word 2: next slab with any holes in the same sizeclass
;;;   word 3: previous slab with any holes in the same sizeclass
;;;
;;; The FIXNUM representation of sizeclass causes the entire lispword
;;; to read as a fixnum, which makes it possible to map-allocated-objects
;;; over the slabs in a chunk without fuss. The slab heade appears
;;; just like 2 conses. (The next/prev pointers look like fixnums)
;;;
;;; #+big-endian will need to flip the positions of these sub-fields
;;; to make the lispword read as fixnum.

;;; Keep in sync with the structure definition in src/runtime/alloc.c
(defconstant slab-overhead-words 4)
(defmacro slab-sizeclass (slab) `(sap-ref-16 ,slab 0))
(defmacro slab-capacity (slab) `(sap-ref-16 ,slab 2))
(defmacro slab-chunk-size (slab) `(sap-ref-16 ,slab 4))
(defmacro slab-usage (slab) `(sap-ref-16 ,slab 6))
(defmacro slab-freelist (slab) `(sap-ref-sap ,slab 8))
(defmacro slab-next (slab) `(sap-ref-sap ,slab 16))
(defmacro slab-prev (slab) `(sap-ref-sap ,slab 24))
(defmacro slab-usable-range-start (slab) `(sap+ ,slab (* 4 n-word-bytes)))

(defmacro init-slab-header (slab sizeclass chunksize capacity)
  `(setf (slab-sizeclass ,slab) (ash ,sizeclass n-fixnum-tag-bits)
         (slab-capacity ,slab) ,capacity
         (slab-chunk-size ,slab) ,chunksize))

#+gencgc
(defconstant gencgc-page-words (/ gencgc-page-bytes n-word-bytes))
