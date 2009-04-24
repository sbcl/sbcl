;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;; the number of bits at the low end of a pointer used for type
;;; information
(def!constant n-lowtag-bits
  (integer-length (1- (/ (* 2 n-word-bits) n-byte-bits))))
;;; a mask to extract the low tag bits from a pointer
(def!constant lowtag-mask (1- (ash 1 n-lowtag-bits)))
;;; the exclusive upper bound on the value of the low tag bits from a
;;; pointer
(def!constant lowtag-limit (ash 1 n-lowtag-bits))
;;; the number of tag bits used for a fixnum
(def!constant n-fixnum-tag-bits (1- n-lowtag-bits))
;;; the fixnum tag mask
(def!constant fixnum-tag-mask (1- (ash 1 n-fixnum-tag-bits)))
;;; the bit width of positive fixnums
(def!constant n-positive-fixnum-bits (- n-word-bits n-fixnum-tag-bits 1))

;;; the number of bits to shift between word addresses and byte addresses
(def!constant word-shift (1- (integer-length (/ n-word-bits n-byte-bits))))

;;; the number of bytes in a word
(def!constant n-word-bytes (/ n-word-bits n-byte-bits))

;;; the number of bits used in the header word of a data block to store
;;; the type
(def!constant n-widetag-bits 8)
;;; a mask to extract the type from a data block header word
(def!constant widetag-mask (1- (ash 1 n-widetag-bits)))

(def!constant sb!xc:most-positive-fixnum
    (1- (ash 1 (- n-word-bits n-lowtag-bits)))
  #!+sb-doc
  "the fixnum closest in value to positive infinity")
(def!constant sb!xc:most-negative-fixnum
    (ash -1 (- n-word-bits n-lowtag-bits))
  #!+sb-doc
  "the fixnum closest in value to negative infinity")

(def!constant most-positive-exactly-single-float-fixnum
  (min #xffffff sb!xc:most-positive-fixnum))
(def!constant most-negative-exactly-single-float-fixnum
  (max #x-ffffff sb!xc:most-negative-fixnum))
(def!constant most-positive-exactly-double-float-fixnum
  (min #x1fffffffffffff sb!xc:most-positive-fixnum))
(def!constant most-negative-exactly-double-float-fixnum
  (max #x-1fffffffffffff sb!xc:most-negative-fixnum))

;;;; Point where continuous area starting at dynamic-space-start bumps into
;;;; next space.
#!+gencgc
(def!constant max-dynamic-space-end
    (let ((stop (1- (ash 1 n-word-bits)))
          (start dynamic-space-start))
      (dolist (other-start (list read-only-space-start static-space-start linkage-table-space-start))
        (when (< start other-start)
          (setf stop (min stop other-start))))
      stop))
