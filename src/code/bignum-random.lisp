;;;; generation of random bignums
;;;;
;;;; The implementation assumes that the random chunk size is either
;;;; equal to the word size or equal to half the word size.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-BIGNUM")

;;; Return a nonnegative integer of DIGIT-SIZE many pseudo random bits.
(declaim (inline random-bignum-digit))
(defun random-bignum-digit (state)
  (if (= n-random-chunk-bits digit-size)
      (random-chunk state)
      (big-random-chunk state)))

;;; Return a nonnegative integer of N-BITS many pseudo random bits.
;;; N-BITS must be nonnegative and less than DIGIT-SIZE.
(declaim (inline random-bignum-partial-digit))
(defun random-bignum-partial-digit (n-bits state)
  (declare (type (integer 0 #.(1- digit-size)) n-bits)
           (type random-state state))
  (logand (1- (ash 1 n-bits))
          (if (<= n-bits n-random-chunk-bits)
              (random-chunk state)
              (big-random-chunk state))))

;;; Create a (nonnegative) bignum by concatenating RANDOM-CHUNK and
;;; BIT-COUNT many pseudo random bits, normalise and return it.
;;; RANDOM-CHUNK must fit into a bignum digit.
(declaim (inline concatenate-random-bignum))
(defun concatenate-random-bignum (random-chunk bit-count state)
  (declare (type bignum-element-type random-chunk)
           (type (integer 0 #.most-positive-fixnum) bit-count)
           (type random-state state))
  (let* ((n-total-bits (+ 1 n-random-chunk-bits bit-count)) ; sign bit
         (length (ceiling n-total-bits digit-size))
         (bignum (%allocate-bignum length)))
    ;; DO NOT ASSUME THAT %ALLOCATE-BIGNUM PREZEROS
    ;; [See example in MAKE-RANDOM-BIGNUM]
    (setf (%bignum-ref bignum (1- length)) 0)
    (multiple-value-bind (n-random-digits n-random-bits)
        (floor bit-count digit-size)
      (declare (type bignum-length n-random-digits))
      (dotimes (index n-random-digits)
        (setf (%bignum-ref bignum index)
              (random-bignum-digit state)))
      (if (zerop n-random-bits)
          (setf (%bignum-ref bignum n-random-digits) random-chunk)
          (progn
            (setf (%bignum-ref bignum n-random-digits)
                  (%logior (random-bignum-partial-digit n-random-bits
                                                        state)
                           (%ashl random-chunk n-random-bits)))
            (let ((shift (- digit-size n-random-bits)))
              (when (< shift n-random-chunk-bits)
                (setf (%bignum-ref bignum (1+ n-random-digits))
                      (%digit-logical-shift-right random-chunk shift)))))))
    (%normalize-bignum bignum length)))

;;; Create and return a (nonnegative) bignum of N-BITS many pseudo
;;; random bits. The result is normalised, so may be a fixnum, too.
(declaim (inline make-random-bignum))
(defun make-random-bignum (n-bits state)
  (declare (type (and fixnum (integer 0)) n-bits)
           (type random-state state))
  (let* ((n-total-bits (1+ n-bits)) ; sign bit
         (length (ceiling n-total-bits digit-size))
         (bignum (%allocate-bignum length)))
    (declare (type bignum-length length))
    ;; DO NOT ASSUME THAT %ALLOCATE-BIGNUM PREZEROS
    ;; Consider: n-bits = 64 -> n-total-bits = 65 -> length = 2
    ;; and n-digits = 1, n-bits-partial-digit = 0
    ;; so DOTIMES executes exactly once, leaving the final word untouched.
    (setf (%bignum-ref bignum (1- length)) 0)
    (multiple-value-bind (n-digits n-bits-partial-digit)
        (floor n-bits digit-size)
      (declare (type bignum-length n-digits))
      (dotimes (index n-digits)
        (setf (%bignum-ref bignum index)
              (random-bignum-digit state)))
      (unless (zerop n-bits-partial-digit)
        (setf (%bignum-ref bignum n-digits)
              (random-bignum-partial-digit n-bits-partial-digit state))))
    (%normalize-bignum bignum length)))

;;; Create and return a pseudo random bignum less than ARG. The result
;;; is normalised, so may be a fixnum, too. We try to keep the number of
;;; times RANDOM-CHUNK is called and the amount of storage consed to a
;;; minimum.
;;; Four cases are differentiated:
;;; * If ARG is a power of two and only one random chunk is needed to
;;;   supply a sufficient number of bits, a chunk is generated and
;;;   shifted to get the correct number of bits. This only conses if the
;;;   result is indeed a bignum. This case can only occur if the size of
;;;   the random chunks is equal to the word size.
;;; * If ARG is a power of two and multiple chunks are needed, we call
;;;   MAKE-RANDOM-BIGNUM. Here a bignum is always consed even if it
;;;   happens to normalize to a fixnum, which can't be avoided.
;;; * If ARG is not a power of two but one random chunk suffices an
;;;   accept-reject loop is used. Each time through the loop a chunk is
;;;   generated and shifted to get the correct number of bits. This only
;;;   conses if the final accepted result is indeed a bignum. This case
;;;   too can only occur if the size of the random chunks is equal to the
;;;   word size.
;;; * If ARG is not a power of two and multiple chunks are needed an
;;;   accept-reject loop is used that detects rejection early by
;;;   starting the generation with a random chunk aligned to the most
;;;   significant bits of ARG. If the random value is larger than the
;;;   corresponding chunk of ARG we don't need to generate the full
;;;   amount of random bits but can retry immediately. If the random
;;;   value is smaller than the ARG chunk we know already that the
;;;   result will be accepted independently of what the remaining random
;;;   bits will be, so we generate them and return. Only in the rare
;;;   case that the random value and the ARG chunk are equal we need to
;;;   generate and compare the complete random number and risk to reject
;;;   it.
(defun %random-bignum (arg state)
  (declare (type (integer #.(1+ most-positive-fixnum)) arg)
           (type random-state state)
           (inline bignum-lower-bits-zero-p))
  (let ((n-bits (bignum-integer-length arg)))
    (declare (type (integer #.sb-vm:n-fixnum-bits) n-bits))
    ;; Don't use (ZEROP (LOGAND ARG (1- ARG))) to test if ARG is a power
    ;; of two as that would cons.
    (cond ((bignum-lower-bits-zero-p arg (1- n-bits))
           ;; ARG is a power of two. We need one bit less than its
           ;; INTEGER-LENGTH. Not using (DECF N-BITS) here allows the
           ;; compiler to make optimal use of the type declaration for
           ;; N-BITS above.
           (let ((n-bits (1- n-bits)))
             (if (<= n-bits n-random-chunk-bits)
                 (%digit-logical-shift-right (random-chunk state)
                                             (- n-random-chunk-bits n-bits))
                 (make-random-bignum n-bits state))))
          ((<= n-bits n-random-chunk-bits)
           (let ((shift (- n-random-chunk-bits n-bits))
                 (arg (%bignum-ref arg 0)))
             (loop
               (let ((bits (%digit-logical-shift-right (random-chunk state)
                                                       shift)))
                 (when (< bits arg)
                   (return bits))))))
          (t
           ;; ARG is not a power of two and we need more than one random
           ;; chunk.
           (let* ((shift (- n-bits n-random-chunk-bits))
                  (arg-first-chunk (ldb (byte n-random-chunk-bits shift)
                                        arg)))
             (loop
               (let ((random-chunk (random-chunk state)))
                 ;; If the random value is larger than the corresponding
                 ;; chunk from the most significant bits of ARG we can
                 ;; retry immediately; no need to generate the remaining
                 ;; random bits.
                 (unless (> random-chunk arg-first-chunk)
                   ;; We need to generate the complete random number.
                   (let ((bits (concatenate-random-bignum random-chunk
                                                          shift state)))
                     ;; While the second comparison below subsumes the
                     ;; first, the first is faster and will nearly
                     ;; always be true, so it's worth it to try it
                     ;; first.
                     (when (or (< random-chunk arg-first-chunk)
                               (< bits arg))
                       (return bits)))))))))))
