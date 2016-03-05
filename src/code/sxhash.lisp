;;;; that part of SXHASH logic which runs not only in the target Lisp but
;;;; in the cross-compilation host Lisp

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;; SXHASH of FLOAT values is defined directly in terms of DEFTRANSFORM in
;;; order to avoid boxing.
(deftransform sxhash ((x) (single-float))
  '(let* ((val (+ 0.0f0 x))
          (bits (logand (single-float-bits val) #.(1- (ash 1 32)))))
     (logxor 66194023
             (sxhash (the fixnum
                          (logand most-positive-fixnum
                                  (logxor bits
                                          (ash bits -7))))))))
(deftransform sxhash ((x) (double-float))
  '(let* ((val (+ 0.0d0 x))
          (hi (logand (double-float-high-bits val) #.(1- (ash 1 32))))
          (lo (double-float-low-bits val))
          (hilo (logxor hi lo)))
     (logxor 475038542
             (sxhash (the fixnum
                          (logand most-positive-fixnum
                                  (logxor hilo
                                          (ash hilo -7))))))))

;;; SXHASH of FIXNUM values is defined as a DEFTRANSFORM because it's so
;;; simple.
(deftransform sxhash ((x) (fixnum))
  (let ((c (logand 1193941380939624010 sb!xc:most-positive-fixnum)))
    ;; shift by -1 to get sign bit into hash
    `(logand (logxor (ash x 4) (ash x -1) ,c) sb!xc:most-positive-fixnum)))

;;; SXHASH of SIMPLE-BIT-VECTOR values is defined as a DEFTRANSFORM
;;; because it is endian-dependent.
(deftransform sxhash ((x) (simple-bit-vector))
  `(let ((result 410823708)
         (length (length x)))
    (declare (type fixnum result))
    (cond
      ((zerop length)
       (mix result (sxhash 0)))
      (t
       (mixf result (sxhash length))
       (multiple-value-bind (n-full-words n-bits-remaining)
           (floor length sb!vm:n-word-bits)
         (flet ((mix-into-result (num)
                  (mixf result
                        ,(ecase sb!c:*backend-byte-order*
                                (:little-endian
                                 '(logand num most-positive-fixnum))
                                ;; FIXME: I'm not certain that
                                ;; N-LOWTAG-BITS is the clearest way of
                                ;; expressing this: it's essentially the
                                ;; difference between `(UNSIGNED-BYTE
                                ;; ,SB!VM:N-WORD-BITS) and (AND FIXNUM
                                ;; UNSIGNED-BYTE).
                                (:big-endian
                                 '(ash num (- sb!vm:n-lowtag-bits)))))))
           (declare (inline mix-into-result))
           ;; FIXME: should we respect DEPTHOID?  SXHASH on strings
           ;; doesn't seem to...
           (dotimes (i n-full-words)
             (mix-into-result (%vector-raw-bits x i)))
           (if (zerop n-bits-remaining)
               result
               (mix-into-result
                (logand (ash (1- (ash 1 n-bits-remaining))
                             ,(ecase sb!c:*backend-byte-order*
                                     (:little-endian 0)
                                     (:big-endian
                                      '(- sb!vm:n-word-bits
                                        n-bits-remaining))))
                        (%vector-raw-bits x n-full-words))))))))))

;;; Some other common SXHASH cases are defined as DEFTRANSFORMs in
;;; order to avoid having to do TYPECASE at runtime.
;;;
;;; We also take the opportunity to handle the cases of constant
;;; strings, and of symbols whose names are known at compile time;
;;; except that since SXHASH on the cross-compilation host is not in
;;; general compatible with SXHASH on the target SBCL, we can't so
;;; easily do this optimization in the cross-compiler, and SBCL itself
;;; doesn't seem to need this optimization, so we don't try.
(deftransform sxhash ((x) (simple-string))
  (if #+sb-xc-host nil #-sb-xc-host (constant-lvar-p x)
      (sxhash (lvar-value x))
      '(%sxhash-simple-string x)))
(deftransform sxhash ((x) (symbol))
  (if #+sb-xc-host nil #-sb-xc-host (constant-lvar-p x)
      (sxhash (lvar-value x))
      (if (csubtypep (lvar-type x) (specifier-type 'null))
          ;; FIXME: this isn't in fact as optimized as it could be;
          ;; this does a memory load, whereas (because we know the
          ;; layout of NIL) we could simply take the address of NIL
          ;; (or the contents of NULL-TN) and mask off the appropriate
          ;; bits, since SYMBOL-HASH of NIL is also NIL's CDR, which
          ;; is NIL.  -- CSR, 2004-07-14
          '(symbol-hash x)
          ;; Cache the value of the symbol's sxhash in the symbol-hash
          ;; slot.
          '(let ((result (symbol-hash x)))
            ;; 0 marks uninitialized slot. We can't use negative
            ;; values for the uninitialized slots since NIL might be
            ;; located so high in memory on some platforms that its
            ;; SYMBOL-HASH (which contains NIL itself) is a negative
            ;; fixnum.
            (if (= 0 result)
                (ensure-symbol-hash x)
                result)))))

(deftransform psxhash ((x &optional depthoid) (character &optional t))
  `(char-code (char-upcase x)))

(deftransform psxhash ((x &optional depthoid) (integer &optional t))
  `(sxhash x))
