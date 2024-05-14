;;;; hashing functions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;; the depthoid explored when calculating hash values
;;;
;;; "Depthoid" here is a sort of mixture of what Common Lisp ordinarily calls
;;; depth and what Common Lisp ordinarily calls length; it's incremented either
;;; when we descend into a compound object or when we step through elements of
;;; a compound object.
(defconstant +max-hash-depthoid+ 4)

;;; This is an out-of-line callable entrypoint that the compiler can
;;; transform SXHASH into when hashing a non-simple string.
(defun %sxhash-string (x)
  (declare (optimize speed) (type string x))
  (multiple-value-bind (string start end)
      (if (array-header-p x)
          (with-array-data ((string x) (start) (end) :check-fill-pointer t)
            (values string start end))
          (values x 0 (length x)))
    ;; I'm not sure I believe the comment about FLET TRICK being needed.
    ;; The generated code seems tight enough, and the comment is, after all,
    ;; >14 years old.
    (%sxhash-simple-substring string start end)))

;;;; the SXHASH function

;; simple cases
(declaim (ftype (sfunction (integer) hash-code) sxhash-bignum))

;;; Return a stable address-based hash for instances, using a 2-bit status
;;; indicator as to whether there was a hash slot appended by GC. States:
;;;   #b00 = never hashed
;;;   #b01 = hashed and not moved a/k/a "need stable hash"
;;;   #b11 = hashed and moved a/k/a "has stable hash"
;;;
;;; When we need to take the address, there are a few ways to get a consistent
;;; view of the object's hash status bits and its address:
;;; * PSEUDO-ATOMIC (requires a vop)
;;; * WITH-PINNED-OBJECTS
;;; * a very lightweight lockless algorithm that detects object movement
;;;   by copying the boxed register to an untagged register both
;;;   before and after reading the header word.
;;;   If the before and after values are the same and the header is marked
;;;   as "need stable hash" then the hash can only be the object address.
;;;   I'm not willing enough (or smart enough) to write a correctness proof.
;;;   It sounds something like our 'frlock' algorithm.
;;; Since WITH-PINNED-OBJECT costs nothing on conservative gencgc,
;;; that's what I'm going with.
;;;
(declaim (inline %instance-sxhash))
(defun %instance-sxhash (instance header-word)
  ;; LAYOUT must not acquire an extra slot for the stable hash,
  ;; because the bitmap length is derived from the instance length.
  ;; It would probably be simple to eliminate this as a special case
  ;; by ensuring that instances of LAYOUT commence life with a trailing
  ;; hash slot and the SB-VM:HASH-SLOT-PRESENT-FLAG set.
  (when (typep instance 'layout)
    ;; This might be wrong if the clos-hash was clobbered to 0
    (return-from %instance-sxhash (layout-clos-hash instance)))
  ;; Non-simple cases: no hash slot, and either unhashed or hashed-not-moved.
  (let* ((addr (sb-c::if-vop-existsp (:named sb-vm::set-instance-hashed-return-address)
                 (if (logbitp sb-vm:stable-hash-required-flag header-word)
                     (get-lisp-obj-address instance)
                     (%primitive sb-vm::set-instance-hashed-return-address instance))
                 (with-pinned-objects (instance)
                   ;; First we have to indicate that a hash was taken from the address
                   ;; if not already so marked.
                   (unless (logbitp sb-vm:stable-hash-required-flag header-word)
                     #-sb-thread (setf (sap-ref-word (int-sap (get-lisp-obj-address instance))
                                                     (- sb-vm:instance-pointer-lowtag))
                                       (logior (ash 1 sb-vm:stable-hash-required-flag)
                                               header-word))
                     #+sb-thread (%primitive sb-vm::set-instance-hashed instance))
                   (get-lisp-obj-address instance)))))
    ;; perturb the address
    (murmur-hash-word/+fixnum addr)))

(declaim (inline instance-sxhash))
(defun instance-sxhash (instance)
  (let ((header-word (instance-header-word (truly-the instance instance))))
    (if (logbitp sb-vm:hash-slot-present-flag header-word)
        ;; easy case: 1 word beyond the apparent length is a word added
        ;; by GC (which may have resized the object, but we don't need to know).
        (truly-the hash-code (%instance-ref instance (%instance-length instance)))
        (%instance-sxhash instance header-word))))

;;; Return a pseudorandom number that was assigned on allocation.
;;; FIN is a STANDARD-FUNCALLABLE-INSTANCE but we don't care to type-check it.
;;; You might rightly wonder - for what reason do we require good hash codes for
;;; funcallable instances, but not for all functions? I think the answer has to do
;;; with inserting GFs into weak tables for tracking when we need to invalidate them
;;; due to a change in the definition of a method-combination.
(declaim (inline fsc-instance-hash))
(defun fsc-instance-hash (fin)
  (truly-the hash-code
   #+executable-funinstances
   (with-pinned-objects (fin)
     (let ((hash (sb-vm::compact-fsc-instance-hash
                  (truly-the sb-pcl::standard-funcallable-instance fin))))
       ;; There is not more entropy imparted by doing a mix step on a value that had
       ;; at most 32 bits of randomness, but this makes more of the bits vary.
       ;; Some uses of the hash might expect the high bits to have randomness in them.
       ;; This returns a positive fixnum to conform with the requirement on SXHASH.
       (murmur-hash-word/+fixnum hash)))
   #-executable-funinstances
   (sb-pcl::standard-funcallable-instance-hash-code
    (truly-the sb-pcl::standard-funcallable-instance fin))))

(declaim (inline integer-sxhash))
(defun integer-sxhash (x)
  (if (fixnump x) (sxhash (truly-the fixnum x)) (sb-bignum:sxhash-bignum x)))

(defun number-sxhash (x)
  (declare (optimize (sb-c:verify-arg-count 0) speed))
  (declare (explicit-check))
  (labels ((hash-ratio (x)
             (let ((result 127810327))
               (declare (type fixnum result))
               (mixf result (integer-sxhash (numerator x)))
               (mixf result (integer-sxhash (denominator x)))
               result))
           (hash-rational (x)
             (if (ratiop x)
                 (hash-ratio x)
                 (integer-sxhash x))))
    (macrolet ((hash-complex-float (type)
                 `(let ((result 535698211))
                    (declare (type fixnum result))
                    (mixf result (sxhash (truly-the ,type (realpart x))))
                    (mixf result (sxhash (truly-the ,type (imagpart x))))
                    result)))
      (typecase x
        (fixnum (sxhash x)) ; (Should be picked off by main SXHASH)
        (integer (sb-bignum:sxhash-bignum x))
        (single-float (sxhash x)) ; through DEFTRANSFORM
        (double-float (sxhash x)) ; through DEFTRANSFORM
        #+long-float (long-float (error "stub: no LONG-FLOAT"))
        (ratio (hash-ratio x))
        #+long-float
        ((complex long-float) (hash-complex-float long-float))
        ((complex double-float) (hash-complex-float double-float))
        ((complex single-float) (hash-complex-float single-float))
        ((complex rational)
         (let ((result 535698211))
           (declare (type fixnum result))
           (mixf result (hash-rational (imagpart x)))
           (mixf result (hash-rational (realpart x)))
           result))
        (t 0)))))

(clear-info :function :inlinep 'integer-sxhash)

(macrolet ((with-hash ((var seed) &body body)
             `(let ((,var (word-mix 410823708 ,seed)))
                (declare (type word ,var))
                ,@body))
           (mix-chunk (word)
             `(setq result (word-mix ,word result)))
           (mix-remaining (word)
             ;; In the current implementation of bit operations, they may leave random
             ;; bits in an ignored suffix of bits, hence the need for a masking operation.
             ;; (See examples above DEF-BIT-ARRAY-OP)
             ;; N-BITS-REMAINING is between 1 inclusive and N-WORD-BITS exclusive.
             ;; Produce a mask of 1s spanning the remaining bits, which would be
             ;; (- n-word-bits n-bits-remaining) and logically AND it with word.
             ;; The mask is equal mod N-WORD-BITS to (- n-bits-remaining).
             ;; SHIFT-TOWARDS-START clips the shift count explicitly if the CPU doesn't.
             `(mix-chunk (logand (shift-towards-start most-positive-word
                                                      (- n-bits-remaining))
                                 ,word))))
(defun %sxhash-simple-bit-vector (x)
  (with-hash (result (length (truly-the simple-bit-vector x)))
    (multiple-value-bind (n-full-words n-bits-remaining) (floor (length x) sb-vm:n-word-bits)
      (dotimes (i n-full-words) (mix-chunk (%vector-raw-bits x i)))
      (when (plusp n-bits-remaining)
        (mix-remaining (%vector-raw-bits x n-full-words))))
    (logand result sb-xc:most-positive-fixnum)))
(defun %sxhash-bit-vector (bit-vector)
  (with-array-data ((x bit-vector) (start) (end) :check-fill-pointer t)
    (multiple-value-bind (start-word start-bit) (floor start sb-vm:n-word-bits)
      (cond ((= start-bit 0) ; relevant bits are word-aligned
             (multiple-value-bind (end-word n-bits-remaining) (floor end sb-vm:n-word-bits)
               (with-hash (result (- end start))
                 (do ((i start-word (1+ i)))
                     ((>= i end-word))
                   (mix-chunk (%vector-raw-bits x i)))
                 (when (plusp n-bits-remaining)
                   (mix-remaining (%vector-raw-bits x end-word)))
                 (logand result sb-xc:most-positive-fixnum))))
            #+(or arm64 x86 x86-64)
            ((not (logtest start-bit 7)) ; relevant bits are byte-aligned
             ;; The case is probably ok on all little-endian CPUs that permit
             ;; unaligned loads but I didn't try it on them all.
             (with-pinned-objects (x)
               (let ((byte-offset (ash start -3))
                     (n-bits-remaining (- end start))
                     (sap (vector-sap x)))
                 (with-hash (result (- end start))
                   (loop (unless (>= n-bits-remaining sb-vm:n-word-bits) (return))
                         #+nil
                         (format t "~& mixing middle word: [~a]~%"
                                 (nreverse (format nil "~64,'0b" (sap-ref-word sap byte-offset))))
                         ;; Since we have at least sb-vm:n-word-bits more to go,
                         ;; and the non-simple vector fits within its backing vector,
                         ;; it must be OK to read an entire word from that vector.
                         (mix-chunk (sap-ref-word sap byte-offset))
                         (incf byte-offset sb-vm:n-word-bytes)
                         (decf n-bits-remaining sb-vm:n-word-bits))
                   (when (plusp n-bits-remaining)
                     ;; Perform exactly one more word-sized load rather than N-BYTES-REMAINING
                     ;; byte-sized loads + shifts to reconstruct the final word. This load puts
                     ;; the final relevant byte into the MSB of the loaded word and is
                     ;; guaranteed neither to overrun nor underrun the backing vector.
                     ;; It might grab some bytes from the vector-length word as an edge case.
                     ;; Consider e.g. a non-simple vector of 8 bits with displaced-index-offset 16
                     ;; into an underlying vector of 30 bits.
                     (let* ((n-bytes-remaining (ceiling n-bits-remaining sb-vm:n-byte-bits))
                            ;; Compute how many bytes we didn't actually want to read. It could
                            ;; be 0 if we want all remaining bytes (but presumably not all bits)
                            (shift-out (- sb-vm:n-word-bytes n-bytes-remaining))
                            (word (ash (sap-ref-word
                                        sap
                                        (+ byte-offset n-bytes-remaining (- sb-vm:n-word-bytes)))
                                       (* -8 shift-out))))
                       #+nil (format t "~&  mixing final word: [~a]~%"
                                     (nreverse (format nil "~64,'0b" word)))
                       (mix-remaining word)))
                   (logand result sb-xc:most-positive-fixnum)))))
            (t ; not aligned in a way that this can deal with.
             ;; Fallback to the simple algorithm using a copy.
             ;; Nobody has complained in 17 years, ever since git rev a3ab89c1db when this
             ;; was corrected to hash more than 4 bits. Prior to that, the code was plain wrong,
             ;; violating constraint 1 in the spec for SXHASH.
             ;; If we do manage to improve this not to cons a new vector, the test
             ;; in hash.pure.lisp should be made more rigorous as well.
             (%sxhash-simple-bit-vector (copy-seq bit-vector))))))))

;;; To avoid "note: Return type not fixed values ..."
;;; PATHNAME-SXHASH can't easily be placed in pathname.lisp because that file
;;; depends on LOGICAL-HOST but the definition of LOGICAL-HOST is complicated
;;; and seems to belong where it is, in target-pathname.lisp, though maybe not.
(declaim (ftype (sfunction (t) hash-code) pathname-sxhash))

(defun sap-hash (x)
  ;; toss in a LOGNOT so that (the word a) and (int-sap a) hash differently
  (murmur-hash-word/+fixnum (logand (lognot (sap-int x)) most-positive-word)))

(defun sxhash (x)
  ;; profiling SXHASH is hard, but we might as well try to make it go
  ;; fast, in case it is the bottleneck somewhere.  -- CSR, 2003-03-14
  ;; So, yes, profiling is a little tough but not impossible with some added
  ;; instrumentation in each stanza of the COND, either manually or
  ;; automagically. Based on a manual approach, the order of the tests below
  ;; are now better arranged by approximate descending frequency in terms
  ;; of calls observed in certain test. Regardless of the fact that applications
  ;; will vary by use-cases, this seems like a good order because:
  ;;  * despite that INSTANCE is often the 2nd-most common object type in the heap
  ;;    (right behind CONS), there are probably at least as many heap words
  ;;    that are FIXNUM as instance pointers. So it stands to reason that
  ;;    SXHASH-RECURSE is invoked very often on FIXNUM.
  ;;  * SYMBOLs are extremely common as table keys, more so than INSTANCE,
  ;;    so we should pick off SYMBOL sooner than INSTANCE as well.
  ;;  * INSTANCE (except for PATHNAME) doesn't recurse anyway - in fact
  ;;    it is particularly dumb (by design), so performing that test later
  ;;    doesn't incur much of a penalty.
  ;; Anyway, afaiu, the code below was previously ordered by gut feeling
  ;; rather than than actual measurement, so having any rationale for ordering
  ;; is better than having no rationale. And as a further comment observes,
  ;; we could do away with the question of order if only we had jump tables.
  ;; (Also, could somebody perhaps explain how these magic numbers were chosen?)
  (declare (optimize speed))
  (labels ((sxhash-recurse (x depthoid)
             (declare (type index depthoid))
             (typecase x
               ;; we test for LIST here, rather than CONS, because the
               ;; type test for CONS is in fact the test for
               ;; LIST-POINTER-LOWTAG followed by a negated test for
               ;; NIL.  If we're going to have to test for NIL anyway,
               ;; we might as well do it explicitly and pick off the
               ;; answer.  -- CSR, 2004-07-14
               (list
                (if (null x)
                    (sxhash x)          ; through DEFTRANSFORM
                    (if (plusp depthoid)
                        (mix (sxhash-recurse (car x) (1- depthoid))
                             (sxhash-recurse (cdr x) (1- depthoid)))
                        261835505)))
               (symbol (sxhash x)) ; through DEFTRANSFORM
               (fixnum (sxhash x)) ; through DEFTRANSFORM
               (instance
                (if (pathnamep x)
                    (pathname-sxhash x)
                    (instance-sxhash x)))
               (array
                (typecase x
                  (string (%sxhash-string x))
                  (bit-vector (%sxhash-bit-vector x))
                  ;; Would it be legal to mix in the widetag?
                  (t (logxor 191020317 (sxhash (array-rank x))))))
               ;; general, inefficient case of NUMBER
               ;; There's a spurious FIXNUMP test here, as we've already picked it off.
               ;; Maybe the NUMBERP emitter could be informed that X can't be a fixnum,
               ;; because writing this case as (OR BIGNUM RATIO FLOAT COMPLEX)
               ;; produces far worse code.
               (number (number-sxhash x))
               (character
                (logxor 72185131
                        (sxhash (char-code x)))) ; through DEFTRANSFORM
               (funcallable-instance
                (if (logtest (layout-flags (%fun-layout x)) +pcl-object-layout-flag+)
                    ;; We have a hash code, so might as well use it.
                    (fsc-instance-hash x)
                    ;; funcallable structure, not funcallable-standard-object
                    9550684))
               (system-area-pointer (sap-hash x))
               (t 42))))
    (sxhash-recurse x +max-hash-depthoid+)))

;;;; the PSXHASH function

;;; like SXHASH, but for EQUALP hashing instead of EQUAL hashing
(macrolet ((hash-float (type key)
             ;; Floats that represent integers must hash as the integer would.
             (let ((lo (symbol-value (package-symbolicate :sb-kernel 'most-negative-fixnum- type)))
                   (hi (symbol-value (package-symbolicate :sb-kernel 'most-positive-fixnum- type)))
                   (bignum-hash (symbolicate 'sxhash-bignum- type)))
               `(let ((key ,key))
                  (declare (inline float-infinity-p))
                  (cond (;; This clause allows FIXNUM-sized integer
                         ;; values to be handled without consing.
                         (<= ,lo key ,hi)
                         (multiple-value-bind (q r) (truly-the fixnum (floor (the (,type ,lo ,hi) key)))
                           (if (zerop (the ,type r))
                               (sxhash q)
                               (sxhash (coerce key 'double-float)))))
                        ((float-infinity-p key)
                         ;; {single,double}-float infinities are EQUALP
                         (if (minusp key)
                             (sxhash sb-ext:single-float-negative-infinity)
                             (sxhash sb-ext:single-float-positive-infinity)))
                        #+64-bit
                        (t
                         (,bignum-hash key))
                        #-64-bit
                        (t
                         ,(if (eq type 'double-float)
                              `(multiple-value-bind (q r) (floor key)
                                 (if (zerop (the ,type r))
                                     (sxhash q)
                                     (sxhash key)))
                              `(,bignum-hash key))))))))
(defun psxhash (key)
  (declare (optimize speed))
  (labels
      ((data-vector-hash (data start end depthoid)
         (declare (optimize (sb-c:insert-array-bounds-checks 0)))
         (let ((result 572539))
           (declare (type hash-code result))
           (when (plusp depthoid)
             (decf depthoid)
             (macrolet ((traverse (et &aux (elt '(aref data i)))
                          `(let ((data (truly-the (simple-array ,et (*)) data)))
                             (loop for i fixnum from (truly-the fixnum start)
                                   below (truly-the fixnum end)
                                   do (mixf result
                                            ,(case et
                                               ((t) `(%psxhash ,elt depthoid))
                                               ((base-char character)
                                                `(char-code (char-upcase ,elt)))
                                               (single-float `(sfloat-psxhash ,elt))
                                               (double-float `(dfloat-psxhash ,elt))
                                               ;; the remaining types are integers and complex numbers.
                                               ;; COMPLEX will cons here, as will word-sized
                                               ;; integers. Nothing else should though.
                                               (t `(sxhash ,elt)))))))) ; xformed
               (typecase data
                 ;; There are two effects of this typecase:
                 ;;  1. using an optimized array reader
                 ;;  2. dispatching to a type-specific hash function
                 (simple-vector (traverse t)) ; effect #1 only
                 (simple-base-string (traverse base-char)) ; both effects
                 #+sb-unicode (simple-character-string (traverse character))  ; both
                 ((simple-array single-float (*)) (traverse single-float)) ; and so on
                 ((simple-array double-float (*)) (traverse double-float))
                 ;; (SIMPLE-ARRAY WORD (*)) would be helpful to avoid consing,
                 ;; but there is no SXHASH transform on word-sized integers.
                 ;; It might be possible to do something involving WORD-MIX.
                 ((simple-array fixnum (*)) (traverse fixnum))
                 (t
                  (let ((getter (svref %%data-vector-reffers%% (%other-pointer-widetag data))))
                    (loop for i fixnum from (truly-the fixnum start) below (truly-the fixnum end)
                          do (mixf result (number-psxhash (funcall getter data i)))))))))
             result))
       (structure-object-psxhash (key depthoid)
         ;; Compute a PSXHASH for KEY. Salient points:
         ;; * It's not enough to use the bitmap to figure out how to mix in raw slots.
         ;;   The floating-point types all need special treatment. And we want to avoid
         ;;   consing, so we can't very well call PSXHASH.
         ;; * Even though PSXHASH requires that numerically equal numbers have the same
         ;;   hash e.g. 12 and 12d0 and #c(12d0 0d0) all hash the same, structures can
         ;;   weaken that restriction: instances are EQUAL only if they are of the same
         ;;   type and slot-for-slot EQUAL. So a float in a raw slot can't be EQUAL
         ;;   to a word in a different raw slot. In fact we don't even require that
         ;;   SINGLE- and DOUBLE-float hash the same for a given numerical value,
         ;;   because a raw slot can't hold either/or. But -0 and +0 must hash the same.
         (declare (type structure-object key))
         (declare (type (integer 0 #.+max-hash-depthoid+) depthoid))
         (macrolet ((rsd-index+1 (dsd)
                      ;; Return 0 if the DSD is not raw, otherwise 1+ the index into
                      ;; *RAW-SLOT-DATA*. This is exactly the low 3 bits of DSD-BITS.
                      `(truly-the (mod ,(1+ (length sb-kernel::*raw-slot-data*)))
                         (ldb (byte 3 0) (sb-kernel::dsd-bits ,dsd))))
                    (raw-cases ()
                      (flet ((1+index-of (type)
                               (1+ (position type sb-kernel::*raw-slot-data*
                                             :key #'sb-kernel::raw-slot-data-raw-type)))
                             (mix-float (val zero)
                               `(let ((x ,val))
                                  (mixf result (sxhash (if (= x ,zero) ,zero x))))))
                        ;; This compiles to a jump table if supported
                        `(case rsd-index+1
                           ((,(1+index-of 'word) ,(1+index-of 'sb-vm:signed-word))
                            ;; Access as unsigned. +X and -X hash differently because
                            ;; of 2's complement, so disregarding the sign bit is fine.
                            (mixf result (logand (%raw-instance-ref/word key i)
                                                 most-positive-fixnum)))
                           (,(1+index-of 'single-float)
                            ,(mix-float '(%raw-instance-ref/single key i) 0f0))
                           (,(1+index-of 'double-float)
                            ,(mix-float '(%raw-instance-ref/double key i) 0d0))
                           (,(1+index-of 'sb-kernel:complex-single-float)
                            (let ((cplx (%raw-instance-ref/complex-single key i)))
                              ,(mix-float '(realpart cplx) 0f0)
                              ,(mix-float '(imagpart cplx) 0f0)))
                           (,(1+index-of 'sb-kernel:complex-double-float)
                            (let ((cplx (%raw-instance-ref/complex-double key i)))
                              ,(mix-float '(realpart cplx) 0d0)
                              ,(mix-float '(imagpart cplx) 0d0)))))))
           (let* ((layout (%instance-layout key))
                  (result (layout-clos-hash layout)))
             (declare (type fixnum result))
             (when (plusp depthoid)
               (let ((depthoid (1- depthoid))
                     (dd (layout-dd layout)))
                 (if (/= (sb-kernel::dd-bitmap dd) +layout-all-tagged+)
                     (let ((slots (dd-slots dd)))
                       (loop (unless slots (return))
                             (let* ((slot (pop slots))
                                    (rsd-index+1 (rsd-index+1 slot))
                                    (i (dsd-index slot)))
                               (cond ((= rsd-index+1 0) ; non-raw
                                      (mixf result (%psxhash (%instance-ref key i) depthoid)))
                                     (t
                                      (raw-cases))))))
                     (let ((len (%instance-length key))
                           ;; Don't mix in LAYOUT (if it takes a slot) because it was the seed value.
                           (i sb-vm:instance-data-start))
                       (declare (index i))
                       (loop (when (>= i len) (return))
                             (mixf result (%psxhash (%instance-ref key i) depthoid))
                             (incf i))))))
             result)))
       (sfloat-psxhash (key)
         (declare (single-float key))
         (hash-float single-float key))
       (dfloat-psxhash (key)
         (declare (double-float key))
         (hash-float double-float key))
       (number-psxhash (key)
         (declare (type number key)
                  (muffle-conditions compiler-note))
         (macrolet ((hash-complex (hasher)
                      `(if (zerop (imagpart key))
                           (,hasher (realpart key))
                           ;; I'm not sure what the point of an additional mix step
                           ;; with a constant was. Maybe trying to get it not to hash
                           ;; like a ratio whose num/den are equal to the real and imag
                           ;; parts of a complex number? That seems silly.
                           ;; But sure, let's do something like it, but simpler.
                           ;; (It might hash like a cons of these integers anyway)
                           (logand (lognot (mix (,hasher (realpart key)) (,hasher (imagpart key))))
                                   most-positive-fixnum))))
           (etypecase key
             (integer (sxhash key))
             (single-float (sfloat-psxhash key))
             (double-float (dfloat-psxhash key))
             (rational (if (and (<= most-negative-double-float
                                    key
                                    most-positive-double-float)
                                (= (coerce key 'double-float) key))
                           (sxhash (coerce key 'double-float))
                           ;; a rational for which '=' does not return T when compared
                           ;; to itself cast as double-float need to have the same hash
                           ;; as any float. That's why this case is legitimate.
                           (sxhash key)))
             ((complex double-float) (hash-complex dfloat-psxhash))
             ((complex single-float) (hash-complex sfloat-psxhash))
             ((complex rational)     (hash-complex number-psxhash)))))
       (%psxhash (key depthoid)
         (typecase key
           (array
            (if (vectorp key)
                (with-array-data ((a key) (start) (end) :force-inline t :check-fill-pointer t)
                  (mix (data-vector-hash a start end depthoid) (length key)))
                (with-array-data ((a key) (start) (end) :force-inline t :array-header-p t)
                  (let ((result (data-vector-hash a start end depthoid)))
                    (dotimes (i (array-rank key) result)
                      (mixf result (%array-dimension key i)))))))
           (structure-object
            (cond ((hash-table-p key)
                   ;; This is a purposely not very strong hash so that it does not make any
                   ;; distinctions that EQUALP does not make. Computing a hash of the k/v pair
                   ;; vector would incorrectly take insertion order into account.
                   (mix (mix 103924836 (hash-table-count key))
                        (sxhash (hash-table-test key))))
                  ((pathnamep key) (pathname-sxhash key))
                  (t
                   (structure-object-psxhash key depthoid))))
           (list
            (cond ((null key)
                   (the fixnum 480929))
                  ((eql depthoid 0)
                   (the fixnum 779578))
                  (t
                   (let ((depthoid (1- (truly-the (integer 0 #.+max-hash-depthoid+)
                                                  depthoid))))
                     (mix (%psxhash (car key) depthoid)
                          (%psxhash (cdr key) depthoid))))))
           (number (number-psxhash key))
           (character (char-code (char-upcase key)))
           (t (sxhash key)))))
    (%psxhash key +max-hash-depthoid+)))
) ; end MACROLET

;;; Semantic equivalent of SXHASH, but better-behaved for function names.
;;; It performs more work by not cutting off as soon in the CDR direction.
;;; More work here equates to less work in the global hashtable.
;;; To wit: (eq (sxhash '(foo a b c bar)) (sxhash '(foo a b c d))) => T
;;; but the corresponding globaldb-sxhashoids differ.
(defun globaldb-sxhashoid (name)
  (locally
      (declare (optimize (safety 0))) ; after the argc check
    ;; TRAVERSE will walk across more cons cells than RECURSE will descend.
    ;; That's why this isn't just one self-recursive function.
    (labels ((traverse (accumulator x length-limit)
               (declare (fixnum length-limit))
               (cond ((atom x) (mix (sxhash x) accumulator))
                     ((zerop length-limit) accumulator)
                     (t (traverse (mix (recurse (car x) 4) accumulator)
                                  (cdr x) (1- length-limit)))))
             (recurse (x depthoid) ; depthoid = a blend of level and length
               (declare (fixnum depthoid))
               (cond ((atom x) (sxhash x))
                     ((zerop depthoid)
                      #.(logand most-positive-fixnum #36Rglobaldbsxhashoid))
                     (t (mix (recurse (car x) (1- depthoid))
                             (recurse (cdr x) (1- depthoid)))))))
      (traverse 0 name 10))))

;;; Not needed post-build
(clear-info :function :inlining-data '%sxhash-simple-substring)
