;;;; code to implement bignum support

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-BIGNUM")

;;;; notes

;;; comments from CMU CL:
;;;   These symbols define the interface to the number code:
;;;       add-bignums multiply-bignums negate-bignum subtract-bignum
;;;       multiply-bignum-and-fixnum multiply-fixnums
;;;       bignum-ashift-right bignum-ashift-left bignum-gcd
;;;       bignum-to-float bignum-integer-length
;;;       bignum-logical-and bignum-logical-ior bignum-logical-xor
;;;       bignum-logical-not bignum-load-byte
;;;       bignum-truncate bignum-plus-p bignum-compare make-small-bignum
;;;       bignum-logbitp bignum-logcount
;;;   These symbols define the interface to the compiler:
;;;       bignum-element-type bignum-index %allocate-bignum
;;;       %bignum-length %bignum-set-length %bignum-ref %bignum-set
;;;       %digit-0-or-plusp %add-with-carry %subtract-with-borrow
;;;       %multiply-and-add %multiply
;;;       %bigfloor %fixnum-digit-with-correct-sign %ashl
;;;       %ashr %digit-logical-shift-right))

;;; The following interfaces will either be assembler routines or code
;;; sequences expanded into the code as basic bignum operations:
;;;    General:
;;;       %BIGNUM-LENGTH
;;;       %ALLOCATE-BIGNUM
;;;       %BIGNUM-REF
;;;       %NORMALIZE-BIGNUM
;;;       %BIGNUM-SET-LENGTH
;;;       %FIXNUM-DIGIT-WITH-CORRECT-SIGN
;;;       %SIGN-DIGIT
;;;       %ASHR
;;;       %ASHL
;;;       %BIGNUM-0-OR-PLUSP
;;;       %DIGIT-LOGICAL-SHIFT-RIGHT
;;;    General (May not exist when done due to sole use in %-routines.)
;;;       %DIGIT-0-OR-PLUSP
;;;    Addition:
;;;       %ADD-WITH-CARRY
;;;    Subtraction:
;;;       %SUBTRACT-WITH-BORROW
;;;    Multiplication
;;;       %MULTIPLY
;;;    Negation
;;;       %LOGNOT
;;;    Shifting (in place)
;;;       %NORMALIZE-BIGNUM-BUFFER
;;;    TRUNCATE
;;;       %BIGFLOOR
;;;
;;; Note: The floating routines know about the float representation.
;;;
;;; PROBLEM 1:
;;; There might be a problem with various LET's and parameters that take a
;;; digit value. We need to write these so those things stay in machine
;;; registers and number stack slots. I bind locals to these values, and I
;;; use function on them -- ZEROP, ASH, etc.
;;;
;;; PROBLEM 2:
;;; In shifting and byte operations, I use masks and logical operations that
;;; could result in intermediate bignums. This is hidden by the current system,
;;; but I may need to write these in a way that keeps these masks and logical
;;; operations from diving into the Lisp level bignum code.
;;;
;;; To do:
;;;    fixnums
;;;       logior, logxor, logand
;;;       depending on relationals, < (twice) and <= (twice)
;;;       or write compare thing (twice).
;;;       LDB on fixnum with bignum result.
;;;       DPB on fixnum with bignum result.
;;;       TRUNCATE returns zero or one as one value and fixnum or minus fixnum
;;;       for the other value when given (truncate fixnum bignum).
;;;       Returns (truncate bignum fixnum) otherwise.
;;;       addition
;;;       subtraction (twice)
;;;       multiply
;;;       GCD
;;;    Write MASK-FIELD and DEPOSIT-FIELD in terms of logical operations.
;;;    DIVIDE
;;;       IF (/ x y) with bignums:
;;;       do the truncate, and if rem is 0, return quotient.
;;;       if rem is non-0
;;;          gcd of x and y.
;;;          "truncate" each by gcd, ignoring remainder 0.
;;;          form ratio of each result, bottom is positive.

;;;; What's a bignum?

(defconstant digit-size sb-vm:n-word-bits)

(defconstant all-ones-digit most-positive-word)

#+bignum-assertions
(progn
(declaim (notinline %allocate-bignum))
(defmacro with-bignum-shadow-bits ((bits object &optional length) &body body)
  `(let ((b ,object))
     (with-pinned-objects (b)
       (let ((,bits
              (sb-sys:sap+ (sb-sys:sap+ (sb-sys:int-sap (sb-kernel:get-lisp-obj-address b))
                                        (- sb-vm:n-word-bytes sb-vm:other-pointer-lowtag))
                           ;; shadow bits always start on a double-lispword boundary,
                           ;; so ensure that payload length looks like an odd number.
                           (ash (logior ,(or length `(%bignum-length b)) 1)
                                sb-vm:word-shift))))
         ,@body))))

(defun %allocate-bignum (length)
  (declare (type bignum-length length))
  (declare (inline %allocate-bignum))
  (let ((bignum (%allocate-bignum length))) ; call the low-level allocator
    (multiple-value-bind (nwords nbits) (floor length sb-vm:n-word-bits)
      (with-bignum-shadow-bits (bit-base bignum length)
        (dotimes (i nwords)
          (setf (sb-sys:sap-ref-word bit-base 0) sb-ext:most-positive-word
                bit-base (sap+ bit-base sb-vm:n-word-bytes)))
        (when (plusp nbits)
          (setf (sb-sys:sap-ref-word bit-base 0)
                (#+little-endian shift-towards-start
                 #+big-endian shift-towards-end sb-ext:most-positive-word
                 (- nbits))))))
    (dotimes (i length)
      (%%bignum-set bignum i (logior #+64-bit #xc0fefe0000 i)))
    ;; If there is a padding word, then write junk in it, so we can assert that whenever
    ;; bignum-set-length is used, the padding word (if present) was cleared.
    (when (evenp length) (%%bignum-set bignum length #xdeadbeef))
    bignum))

(defun index-out-of-bounds (bignum i)
  (error "out-of-bounds bignum set @ ~x[~d], len=~d~%"
         (sb-kernel:get-lisp-obj-address bignum) i (%bignum-length bignum)))

(declaim (inline %bignum-set))
(defun %bignum-set (bignum i value)
  (declare (type bignum bignum)
           (type bignum-index i)
           (type bignum-element-type value))
  (cond ((< i (%bignum-length bignum))
         (with-bignum-shadow-bits (bit-base bignum)
           (multiple-value-bind (word-index bit-index) (floor i sb-vm:n-word-bits)
             (let ((sap (sap+ bit-base (* word-index sb-vm:n-word-bytes))))
               (setf (sap-ref-word sap 0)
                     (logandc2 (sap-ref-word sap 0) (ash 1 bit-index))))))
         (%%bignum-set bignum i value))
        (t
         (index-out-of-bounds bignum i)))
  (values))

(defun bignum-ref-trap (bignum i)
  ;; This error might happen too early in cold-init to report it normally
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "printf" (function void system-area-pointer
                                             unsigned unsigned))
   (sb-sys:vector-sap #.(format nil "Element %d of bignum %p was never written~%"))
   i (sb-kernel:get-lisp-obj-address bignum))
  (sb-vm:ldb-monitor)
  0)

(declaim (inline %bignum-ref))
(defun %bignum-ref (bignum i)
  (declare (type bignum bignum)
           (type bignum-index i))
  ;; We don't need to check %BIGNUM-LENGTH because the shadow bit won't be on
  ;; presuming that we only care to detct "small" off-by-1 errors,
  ;; and not egregious buffer overrun errors.
  (multiple-value-bind (word-index bit-index) (floor i sb-vm:n-word-bits)
    (with-bignum-shadow-bits (bit-base bignum)
      (let ((sap (sb-sys:sap+ bit-base (* word-index sb-vm:n-word-bytes))))
        (if (logbitp bit-index (sb-sys:sap-ref-word sap 0)) ; word was never assigned
            (truly-the sb-vm:word (bignum-ref-trap bignum i))
            (sap-ref-word (sb-sys:int-sap (sb-kernel:get-lisp-obj-address bignum))
                          (- (ash (+ i sb-vm:bignum-digits-offset) sb-vm:word-shift)
                             sb-vm:other-pointer-lowtag)))))))

(defun aver-zeroed-from-index (bignum index)
  (with-pinned-objects (bignum)
    (let* ((physical-start (sb-sys:sap+ (sb-sys:int-sap (sb-kernel:get-lisp-obj-address bignum))
                                        (- sb-vm:other-pointer-lowtag)))
           (physlen-bytes (ash (+ (* (%bignum-length bignum) 2) 2) sb-vm:word-shift))
           (physical-end (sb-sys:sap+ physical-start physlen-bytes))
           (word-ptr (sb-sys:sap+ physical-start (ash (1+ index) sb-vm:word-shift))))
      (loop while (sb-sys:sap< word-ptr physical-end)
            do (unless (= (sb-sys:sap-ref-word word-ptr 0) 0)
                 (sb-alien:alien-funcall
                  (sb-alien:extern-alien "printf"
                                         (function void sb-sys:system-area-pointer
                                                   unsigned unsigned))
                  (vector-sap #.(format nil "set-length %p,%d not properly zeroed~%"))
                  (sb-kernel:get-lisp-obj-address bignum) index)
                 (sb-vm:ldb-monitor))
               (setq word-ptr (sb-sys:sap+ word-ptr 8))))))
)

;;; DO NOT ASSUME THAT LOW-LEVEL ALLOCATOR PREZEROES THE MEMORY
(defmacro alloc-zeroing (length)
  `(let* ((l ,length) (new (%allocate-bignum l)))
     (dotimes (i l new)
       (setf (%bignum-ref new i) 0))))
(defmacro alloc-zeroing-below (length end)
  `(let ((new (%allocate-bignum ,length)))
     (dotimes (i ,end new)
       (setf (%bignum-ref new i) 0))))

(declaim (inline %bignum-0-or-plusp))
(defun %bignum-0-or-plusp (bignum len)
  (declare (type bignum bignum)
           (type bignum-length len))
  (%digit-0-or-plusp (%bignum-ref bignum (1- len))))

(declaim (inline bignum-plus-p))
(defun bignum-plus-p (bignum)
  (declare (type bignum bignum))
  (%bignum-0-or-plusp bignum (%bignum-length bignum)))

;;; This returns 0 or "-1" depending on whether the bignum is positive. This
;;; is suitable for infinite sign extension to complete additions,
;;; subtractions, negations, etc. This cannot return a -1 represented as
;;; a negative fixnum since it would then have to low zeros.
(declaim (inline %sign-digit))
(defun %sign-digit (bignum len)
  (declare (type bignum bignum)
           (type bignum-length len))
  (%ashr (%bignum-ref bignum (1- len)) (1- digit-size)))

(declaim (inline (setf %bignum-ref)))
(defun (setf %bignum-ref) (val bignum index)
  (%bignum-set bignum index val) ; valueless
  val)

(defmacro %lognot (x)
  `(ldb (byte digit-size 0) (lognot ,x)))

(declaim (optimize (speed 3) (safety 0)))

;;;; general utilities

;;; Internal in-place operations use this to fixup remaining digits in the
;;; incoming data, such as in-place shifting. This is basically the same as
;;; the first form in %NORMALIZE-BIGNUM, but we return the length of the buffer
;;; instead of shrinking the bignum.
(declaim (maybe-inline %normalize-bignum-buffer))
(defun %normalize-bignum-buffer (result len)
  (declare (type bignum result)
           (type bignum-length len))
  (unless (= len 1)
    (do ((next-digit (%bignum-ref result (- len 2))
                     (%bignum-ref result (- len 2)))
         (sign-digit (%bignum-ref result (1- len)) next-digit))
        ((not (zerop (logxor sign-digit (%ashr next-digit (1- digit-size))))))
      (decf len)
      (setf (%bignum-ref result len) 0)
      (when (= len 1) (return))))
  len)

;; Prior to calling %bignum-set-length we have to ensure that if the physical length
;; decreases, then final word that may never have been written does not contain junk.
(defmacro clear-padding-word (bignum oldlen newlen)
  (declare (ignorable newlen))
  `(progn
     (when (evenp ,oldlen)
       (#+bignum-assertions %%bignum-set #-bignum-assertions %bignum-set ,bignum ,oldlen 0))
     #+bignum-assertions (aver-zeroed-from-index ,bignum ,newlen)))

;;; This drops the last digit if it is unnecessary sign information. It repeats
;;; this as needed, possibly ending with a fixnum. If the resulting length from
;;; shrinking is one, see whether our one word is a fixnum. Shift the possible
;;; fixnum bits completely out of the word, and compare this with shifting the
;;; sign bit all the way through. If the bits are all 1's or 0's in both words,
;;; then there are just sign bits between the fixnum bits and the sign bit. If
;;; we do have a fixnum, shift it over for the two low-tag bits.
(defun %normalize-bignum (result len)
  (declare (type bignum result)
           (type bignum-length len)
           (muffle-conditions compiler-note)
           (inline %normalize-bignum-buffer))
  #+bignum-assertions (aver (= (%bignum-length result) len))
  (let ((newlen (%normalize-bignum-buffer result len)))
    (declare (type bignum-length newlen))
    (unless (= newlen len)
      ;; If the old length was even, then there is a word which was never accessed
      ;; and may contains random bits. Clear it to avoid a crash in GC.
      (clear-padding-word result len newlen)
      (%bignum-set-length result newlen))
    (if (= newlen 1)
        (let ((digit (%bignum-ref result 0)))
          (if (= (%ashr digit sb-vm:n-positive-fixnum-bits)
                 (%ashr digit (1- digit-size)))
              (%fixnum-digit-with-correct-sign digit)
              result))
        result)))

;;;; addition

(defun add-bignums (a b)
  (declare (type bignum a b))
  (let ((len-a (%bignum-length a))
        (len-b (%bignum-length b)))
    (multiple-value-bind (a len-a b len-b)
        (if (> len-a len-b)
            (values a len-a b len-b)
            (values b len-b a len-a))
      (declare (bignum-index len-a))
      (let* ((len-res (1+ len-a))
             (res (%allocate-bignum len-res)))
        (sb-c::if-vop-existsp (:named sb-vm::bignum-add-loop)
          (sb-sys:%primitive sb-vm::bignum-add-loop a b len-a len-b res)
          (let ((carry 0))
            (dotimes (i len-b)
              (setf (values (%bignum-ref res i) carry)
                    (%add-with-carry (%bignum-ref a i) (%bignum-ref b i) carry)))
            (do ((sign-digit-b (%sign-digit b len-b))
                 (i len-b (1+ i)))
                ((= i len-a)
                 (setf (%bignum-ref res len-a)
                       (%add-with-carry (%sign-digit a len-a) sign-digit-b carry)))
              (setf (values (%bignum-ref res i)
                            carry)
                    (%add-with-carry (%bignum-ref a i) sign-digit-b carry)))))
        (%normalize-bignum res len-res)))))

(defun add-bignum-fixnum (a b)
  (declare (type bignum a)
           (fixnum b))
  (let* ((len-a (%bignum-length a))
         (len-res (1+ len-a))
         (res (%allocate-bignum len-res)))
    (multiple-value-bind (v carry)
        (%add-with-carry (%bignum-ref a 0) (ldb (byte sb-vm:n-word-bits 0) b) 0)
      (setf (%bignum-ref res 0) v)
      (do ((sign-digit-b (ash b (- 1 digit-size)))
           (i 1 (1+ i)))
          ((= i len-a)
           (setf (%bignum-ref res len-a)
                 (%add-with-carry (%sign-digit a len-a) sign-digit-b carry)))
        (setf (values (%bignum-ref res i)
                      carry)
              (%add-with-carry (%bignum-ref a i) sign-digit-b carry))))
    (%normalize-bignum res len-res)))


;;;; subtraction

;;; This subtracts b from a plugging result into res. Return-fun is the
;;; function to call that fixes up the result returning any useful values, such
;;; as the result. This macro may evaluate its arguments more than once.
(defmacro subtract-bignum-loop (a len-a b len-b res len-res return-fun)
  (with-unique-names (borrow a-digit a-sign b-digit b-sign i v k)
    `(let* ((,borrow 1)
            (,a-sign (%sign-digit ,a ,len-a))
            (,b-sign (%sign-digit ,b ,len-b)))
       (declare (type bignum-element-type ,a-sign ,b-sign))
       (dotimes (,i ,len-res)
         (declare (type bignum-index ,i))
         (let ((,a-digit (if (< ,i ,len-a) (%bignum-ref ,a ,i) ,a-sign))
               (,b-digit (if (< ,i ,len-b) (%bignum-ref ,b ,i) ,b-sign)))
           (declare (type bignum-element-type ,a-digit ,b-digit))
           (multiple-value-bind (,v ,k)
               (%subtract-with-borrow ,a-digit ,b-digit ,borrow)
             (setf (%bignum-ref ,res ,i) ,v)
             (setf ,borrow ,k))))
       (,return-fun ,res ,len-res))))

(defun subtract-bignum (a b)
  (declare (type bignum a b))
  (let* ((len-a (%bignum-length a))
         (len-b (%bignum-length b))
         (len-res (1+ (max len-a len-b)))
         (res (%allocate-bignum len-res)))
    (declare (type bignum-length len-a len-b len-res)) ;Test len-res for bounds?
    (subtract-bignum-loop a len-a b len-b res len-res %normalize-bignum)))


(defun subtract-bignum-fixnum (a b)
  (declare (type bignum a)
           (fixnum b))
  (let* ((len-a (%bignum-length a))
         (len-b 1)
         (len-res (1+ len-a))
         (res (%allocate-bignum len-res)))
    (declare (type bignum-length len-a len-b len-res))
    (let* ((borrow 1)
           (a-sign (%sign-digit a len-a))
           (b-sign (ash b (- 1 digit-size))))
      (declare (type bignum-element-type a-sign b-sign))
      (dotimes (i len-res)
        (declare (type bignum-index i))
        (let ((a-digit
                (if (< i len-a)
                    (%bignum-ref a i)
                    a-sign))
              (b-digit
                (if (< i len-b)
                    b
                    b-sign)))
          (declare (type bignum-element-type a-digit b-digit))
          (multiple-value-bind (v k)
              (%subtract-with-borrow a-digit b-digit borrow)
            (setf (%bignum-ref res i) v)
            (setf borrow k))))
      (%normalize-bignum res len-res))))

(defun subtract-fixnum-bignum (a b)
  (declare (fixnum a)
           (type bignum b))
  (let* ((len-a 1)
         (len-b (%bignum-length b))
         (len-res (1+ len-b))
         (res (%allocate-bignum len-res)))
    (declare (type bignum-length len-a len-b len-res))
    (let* ((borrow 1)
           (a-sign (ash a (- 1 digit-size)))
           (b-sign (%sign-digit b len-b)))
      (declare (type bignum-element-type a-sign b-sign))
      (dotimes (i len-res)
        (declare (type bignum-index i))
        (let ((a-digit
                (if (< i len-a)
                    a
                    a-sign))
              (b-digit
                (if (< i len-b)
                    (%bignum-ref b i)
                    b-sign)))
          (declare (type bignum-element-type a-digit b-digit))
          (multiple-value-bind (v k)
              (%subtract-with-borrow a-digit b-digit borrow)
            (setf (%bignum-ref res i) v)
            (setf borrow k))))
      (%normalize-bignum res len-res))))

;;; Operations requiring a subtraction without the overhead of intermediate
;;; results, such as GCD, use this. It assumes Result is big enough for the
;;; result.
(defun subtract-bignum-buffers-with-len (a len-a b len-b result len-res)
  (declare (type bignum a b result)
           (type bignum-length len-a len-b len-res))
  (subtract-bignum-loop a len-a b len-b result len-res
                        %normalize-bignum-buffer))

(defun subtract-bignum-buffers (a len-a b len-b result)
  (declare (type bignum a b result)
           (type bignum-length len-a len-b))
  (subtract-bignum-loop a len-a b len-b result (max len-a len-b)
                        %normalize-bignum-buffer))

;;;; multiplication

(defun multiply-bignums (a b)
  (declare (type bignum a b))
  (let* ((a-plusp (bignum-plus-p a))
         (b-plusp (bignum-plus-p b))
         (a (if a-plusp a (negate-bignum-not-fully-normalized a)))
         (b (if b-plusp b (negate-bignum-not-fully-normalized b)))
         (len-a (%bignum-length a))
         (len-b (%bignum-length b))
         (len-res (+ len-a len-b))
         (res (alloc-zeroing len-res))
         (negate-res (not (eq a-plusp b-plusp))))
    (declare (type bignum-length len-a len-b len-res))
    (dotimes (i len-a)
      (declare (type bignum-index i))
      (let ((carry-digit 0)
            (x (%bignum-ref a i))
            (k i))
        (declare (type bignum-index k)
                 (type bignum-element-type carry-digit x))
        (dotimes (j len-b)
          (multiple-value-bind (big-carry res-digit)
              (%multiply-and-add x
                                 (%bignum-ref b j)
                                 (%bignum-ref res k)
                                 carry-digit)
            (declare (type bignum-element-type big-carry res-digit))
            (setf (%bignum-ref res k) res-digit)
            (setf carry-digit big-carry)
            (incf k)))
        (setf (%bignum-ref res k) carry-digit)))
    (when negate-res (negate-bignum-in-place res))
    (%normalize-bignum res len-res)))

(defun multiply-bignum-and-fixnum (bignum fixnum)
  (declare (type bignum bignum) (type fixnum fixnum))
  (let* ((bignum-plus-p (bignum-plus-p bignum))
         (fixnum-plus-p (not (minusp fixnum)))
         (bignum (if bignum-plus-p bignum (negate-bignum-not-fully-normalized bignum)))
         (bignum-len (%bignum-length bignum))
         (fixnum (if fixnum-plus-p fixnum (- fixnum)))
         (result (%allocate-bignum (1+ bignum-len)))
         (carry-digit 0))
    (declare (type bignum bignum result)
             (type bignum-element-type fixnum carry-digit))
    (dotimes (index bignum-len)
      (declare (type bignum-index index))
      (multiple-value-bind (next-digit low)
          (%multiply-and-add (%bignum-ref bignum index) fixnum carry-digit)
        (declare (type bignum-element-type next-digit low))
        (setf carry-digit next-digit)
        (setf (%bignum-ref result index) low)))
    (setf (%bignum-ref result bignum-len) carry-digit)
    (unless (eq bignum-plus-p fixnum-plus-p)
      (negate-bignum-in-place result))
    (%normalize-bignum result (1+ bignum-len))))

(sb-c::unless-vop-existsp (:named sb-vm::*/signed=>integer)
  (defun multiply-fixnums (a b)
    (declare (fixnum a b))
    (declare (muffle-conditions compiler-note)) ; returns lispobj, so what.
    (let* ((a-minusp (minusp a))
           (b-minusp (minusp b)))
      (multiple-value-bind (high low)
          (%multiply (if a-minusp (- a) a)
                     (if b-minusp (- b) b))
        (declare (type bignum-element-type high low))
        (if (and (zerop high)
                 (%digit-0-or-plusp low))
            (let ((low (truly-the (unsigned-byte #.(1- sb-vm:n-word-bits))
                                  (%fixnum-digit-with-correct-sign low))))
              (if (eq a-minusp b-minusp)
                  low
                  (- low)))
            (let ((res (%allocate-bignum 2)))
              (%bignum-set res 0 low)
              (%bignum-set res 1 high)
              (unless (eq a-minusp b-minusp) (negate-bignum-in-place res))
              (%normalize-bignum res 2)))))))

;;;; BIGNUM-REPLACE and WITH-BIGNUM-BUFFERS

#-bignum-assertions
(defmacro bignum-replace (dest src &key (start1 0) (end1 `(%bignum-length ,dest))
                                        (start2 0) (end2 `(%bignum-length ,src)))
  `(macrolet ((@ (obj index)
                `(sb-sys:sap+
                  (sb-sys:int-sap (sb-kernel:get-lisp-obj-address ,obj))
                  (- (ash (+ ,index sb-vm:bignum-digits-offset) sb-vm:word-shift)
                   sb-vm:other-pointer-lowtag))))
     (let ((count ,(if (and (eql start1 0) (eql start2 0))
                       `(min ,end1 ,end2)
                       `(min (- ,end1 ,start1) (- ,end2 ,start2)))))
       (cond ((= count 2) ; COUNT is almost always 2
              (setf (%bignum-ref ,dest ,start1) (%bignum-ref ,src ,start2)
                    (%bignum-ref ,dest (1+ ,start1)) (%bignum-ref ,src (1+ ,start2))))
             ((= count 1)
              (setf (%bignum-ref ,dest ,start1) (%bignum-ref ,src ,start2)))
             ((> count 0)
              (sb-alien:with-alien ((replace
                                     (function sb-alien:system-area-pointer
                                               sb-alien:system-area-pointer
                                               sb-alien:system-area-pointer
                                               sb-unix::size-t)
                                     :extern ,(if (eq dest src)
                                                  "memmove"
                                                  "memcpy")))
                (sb-sys:with-pinned-objects (,dest ,src)
                  (sb-alien:alien-funcall replace
                                          (@ ,dest ,start1)
                                          (@ ,src ,start2)
                                          (ash count sb-vm:word-shift)))))))))
#+bignum-assertions
(progn
(defmacro bignum-replace (dest src &key (start1 '0) (end1 `(%bignum-length ,dest))
                                        (start2 '0) (end2 `(%bignum-length ,src)))
  `(bignum-replace-impl ,dest ,start1 ,end1
                        ,src ,start2 ,end2))

(defun bignum-replace-impl (dest start1 end1 src start2 end2)
  (do ((i1 start1 (1+ i1))
       (i2 start2 (1+ i2)))
      ((or (>= i1 end1)
           (>= i2 end2)))
    (declare (type bignum-index i1 i2))
    (%bignum-set dest i1 (%bignum-ref src i2)))))

(defmacro with-bignum-buffers (specs &body body)
  "WITH-BIGNUM-BUFFERS ({(var size [init])}*) Form*"
  (collect ((binds) (inits))
    (dolist (spec specs)
      (let ((name (first spec))
            (size (second spec))
            (init (third spec)))
        (binds `(,name (%allocate-bignum ,size)))
        (when init
          (inits `(bignum-replace ,name ,init)))))
    `(let* ,(binds)
       ,@(inits)
       ,@body)))

;;;; GCD

  ;; The asserts in the GCD implementation are way too expensive to
  ;; check in normal use, and are disabled here.
(defmacro gcd-assert (&rest args)
  (declare (ignorable args))
  #+bignum-assertions `(assert ,@args))
  ;; We'll be doing a lot of modular arithmetic.
(defmacro modularly (form)
  `(logand all-ones-digit ,form))

;;; I'm not sure why I need this FTYPE declaration.  Compiled by the
;;; target compiler, it can deduce the return type fine, but without
;;; it, we pay a heavy price in BIGNUM-GCD when compiled by the
;;; cross-compiler. -- CSR, 2004-07-19
(declaim (ftype (sfunction (bignum bignum)
                           (and unsigned-byte fixnum))
                bignum-factors-of-two))
(defun bignum-factors-of-two (a b)
  (declare (type bignum a b))
  (do ((i 0 (1+ i)))
      (())
    (declare (type bignum-index i))
    (let ((or-digits (logior (%bignum-ref a i) (%bignum-ref b i))))
      (unless (zerop or-digits)
        (return (do ((j 0 (1+ j))
                     (or-digits or-digits (%ashr or-digits 1)))
                    ((oddp or-digits) (+ (* i digit-size) j))
                  (declare (type (mod #.sb-vm:n-word-bits) j))))))))

;;; Multiply a bignum buffer with a fixnum or a digit, storing the
;;; result in another bignum buffer, and without using any
;;; temporaries. Inlined to avoid boxing smallnum if it's actually a
;;; digit. Needed by GCD, should possibly OAOO with
;;; MULTIPLY-BIGNUM-AND-FIXNUM.
(declaim (inline multiply-bignum-buffer-and-smallnum-to-buffer))
(defun multiply-bignum-buffer-and-smallnum-to-buffer (bignum bignum-len
                                                             smallnum res)
  (declare (type bignum bignum))
  (let* ((bignum-plus-p (%bignum-0-or-plusp bignum bignum-len))
         (smallnum-plus-p (not (minusp smallnum)))
         (smallnum (if smallnum-plus-p smallnum (- smallnum)))
         (carry-digit 0))
    (declare (type bignum bignum res)
             (type bignum-length bignum-len)
             (type bignum-element-type smallnum carry-digit))
    (unless bignum-plus-p
      (negate-bignum-buffer-in-place bignum bignum-len))
    (dotimes (index bignum-len)
      (declare (type bignum-index index))
      (multiple-value-bind (next-digit low)
          (%multiply-and-add (%bignum-ref bignum index)
                             smallnum
                             carry-digit)
        (declare (type bignum-element-type next-digit low))
        (setf carry-digit next-digit)
        (setf (%bignum-ref res index) low)))
    (setf (%bignum-ref res bignum-len) carry-digit)
    (unless bignum-plus-p
      (negate-bignum-buffer-in-place bignum bignum-len))
    (let ((res-len (%normalize-bignum-buffer res (1+ bignum-len))))
      (unless (eq bignum-plus-p smallnum-plus-p)
        (negate-bignum-buffer-in-place res res-len))
      res-len)))

;;; Given U and V, return U / V mod 2^32. Implements the algorithm in the
;;; paper, but uses some clever bit-twiddling nicked from Nickle to do it.
(declaim (inline bmod))
(defun bmod (u v)
  (declare (muffle-conditions compiler-note)) ; returns lispobj, so what.
  (let ((ud (%bignum-ref u 0))
        (vd (%bignum-ref v 0))
        (umask 0)
        (imask 1)
        (m 0))
    (declare (type word ud vd umask imask m))
    (dotimes (i digit-size)
      (setf umask (logior umask imask))
      (when (logtest ud umask)
        (setf ud (modularly (- ud vd)))
        (setf m (modularly (logior m imask))))
      (setf imask (modularly (ash imask 1)))
      (setf vd (modularly (ash vd 1))))
    m))

(defun dmod (u u-len v v-len tmp1)
  (loop while (> (bignum-buffer-integer-length u u-len)
                 (+ (bignum-buffer-integer-length v v-len)
                    digit-size))
    do
    (unless (zerop (%bignum-ref u 0))
      (let* ((bmod (bmod u v))
             (tmp1-len (multiply-bignum-buffer-and-smallnum-to-buffer v v-len
                                                                      bmod
                                                                      tmp1)))
        (setf u-len (subtract-bignum-buffers u u-len
                                             tmp1 tmp1-len
                                             u))
        (bignum-abs-buffer u u-len)))
    (gcd-assert (zerop (%bignum-ref u 0)))
    (setf u-len (bignum-buffer-ashift-right u u-len digit-size)))
  (let* ((d (+ 1 (- (bignum-buffer-integer-length u u-len)
                    (bignum-buffer-integer-length v v-len))))
         (n (1- (ash 1 d))))
    (declare (type (unsigned-byte #.(integer-length #.sb-vm:n-word-bits)) d)
             (type word n))
    (gcd-assert (>= d 0))
    (when (logtest (%bignum-ref u 0) n)
      (let ((tmp1-len
             (multiply-bignum-buffer-and-smallnum-to-buffer v v-len
                                                            (logand n (bmod u
                                                                            v))
                                                            tmp1)))
        (setf u-len (subtract-bignum-buffers u u-len
                                             tmp1 tmp1-len
                                             u))
        (bignum-abs-buffer u u-len)))
    u-len))

(defconstant lower-ones-digit (1- (ash 1 (truncate sb-vm:n-word-bits 2))))

;;; Find D and N such that (LOGAND ALL-ONES-DIGIT (- (* D X) (* N Y))) is 0,
;;; (< 0 N LOWER-ONES-DIGIT) and (< 0 (ABS D) LOWER-ONES-DIGIT).
(defun reduced-ratio-mod (x y)
  (let* ((c (bmod x y))
         (n1 c)
         (d1 1)
         (n2 (modularly (1+ (modularly (lognot n1)))))
         (d2 (modularly -1)))
    (declare (type word n1 d1 n2 d2))
    (loop while (> n2 (expt 2 (truncate digit-size 2))) do
          (loop for i of-type fixnum
                downfrom (- (integer-length n1) (integer-length n2))
                while (>= n1 n2) do
                (when (>= n1 (modularly (ash n2 (truly-the (mod #.sb-vm:n-word-bits) i))))
                  (psetf n1 (modularly (- n1 (modularly (ash n2 i))))
                         d1 (modularly (- d1 (modularly (ash d2 i)))))))
          (psetf n1 n2
                 d1 d2
                 n2 n1
                 d2 d1))
    (values n2 (if (>= d2 (expt 2 (1- digit-size)))
                   (lognot (logand most-positive-fixnum (lognot d2)))
                   (logand lower-ones-digit d2)))))


(defun copy-bignum (a &optional (len (%bignum-length a)))
  (let ((b (%allocate-bignum len)))
    (bignum-replace b a)
    b))

;;; Allocate a single word bignum that holds fixnum. This is useful when
;;; we are trying to mix fixnum and bignum operands.
(declaim (inline make-small-bignum))
(defun make-small-bignum (fixnum)
  (let ((res (%allocate-bignum 1)))
    (setf (%bignum-ref res 0)
          (ldb (byte digit-size 0) (truly-the fixnum fixnum)))
    res))

;; When the larger number is less than this many bignum digits long, revert
;; to old algorithm.
(defconstant accelerated-gcd-cutoff 3)

;;; Alternate between k-ary reduction with the help of
;;; REDUCED-RATIO-MOD and digit modulus reduction via DMOD. Once the
;;; arguments get small enough, drop through to BIGNUM-MOD-GCD (since
;;; k-ary reduction can introduce spurious factors, which need to be
;;; filtered out). Reference: Kenneth Weber, "The accelerated integer
;;; GCD algorithm", ACM Transactions on Mathematical Software, volume
;;; 21, number 1, March 1995, epp. 111-122.
(defun bignum-gcd (u0 v0)
  (declare (type bignum u0 v0))
  (let* ((u1 (if (bignum-plus-p u0)
                 u0
                 (negate-bignum-not-fully-normalized u0)))
         (v1 (if (bignum-plus-p v0)
                 v0
                 (negate-bignum-not-fully-normalized v0))))
    (when (plusp (bignum-compare u1 v1))
      (rotatef u1 v1))
    (let ((n (rem v1 u1)))
      (when (eql n 0)
        (return-from bignum-gcd (%normalize-bignum u1
                                                   (%bignum-length u1))))
      (setf v1 (if (fixnump n)
                   (make-small-bignum n)
                   n)))
    (let* ((buffer-len (+ 2 (%bignum-length u1)))
           (u (%allocate-bignum buffer-len))
           (u-len (%bignum-length u1))
           (v (%allocate-bignum buffer-len))
           (v-len (%bignum-length v1))
           (tmp1 (%allocate-bignum buffer-len))
           (tmp1-len 0)
           (tmp2 (%allocate-bignum buffer-len))
           (tmp2-len 0)
           (factors-of-two
            (bignum-factors-of-two u1 v1)))
      (declare (type (or null bignum-length)
                     buffer-len u-len v-len tmp1-len tmp2-len))
      (bignum-replace u u1)
      (bignum-replace v v1)
      (setf u-len
            (make-gcd-bignum-odd u
                                 (bignum-buffer-ashift-right u u-len
                                                             factors-of-two)))
      (setf v-len
            (make-gcd-bignum-odd v
                                 (bignum-buffer-ashift-right v v-len
                                                             factors-of-two)))
      (loop until (or (< u-len accelerated-gcd-cutoff)
                      (not v-len)
                      (zerop v-len)
                      (and (= 1 v-len)
                           (zerop (%bignum-ref v 0))))
        do
        (gcd-assert (= buffer-len (%bignum-length u)
                       (%bignum-length v)
                       (%bignum-length tmp1)
                       (%bignum-length tmp2)))
        (if (> (bignum-buffer-integer-length u u-len)
               (+ #.(truncate sb-vm:n-word-bits 4)
                  (bignum-buffer-integer-length v v-len)))
            (setf u-len (dmod u u-len
                              v v-len
                              tmp1))
            (multiple-value-bind (n d) (reduced-ratio-mod u v)
              (setf tmp1-len
                    (multiply-bignum-buffer-and-smallnum-to-buffer v v-len
                                                                   n tmp1))
              (setf tmp2-len
                    (multiply-bignum-buffer-and-smallnum-to-buffer u u-len
                                                                   d tmp2))
              (gcd-assert (= (copy-bignum tmp2 tmp2-len)
                             (* (copy-bignum u u-len) d)))
              (gcd-assert (= (copy-bignum tmp1 tmp1-len)
                             (* (copy-bignum v v-len) n)))
              (setf u-len
                    (subtract-bignum-buffers-with-len tmp1 tmp1-len
                                                      tmp2 tmp2-len
                                                      u
                                                      (1+ (max tmp1-len
                                                               tmp2-len))))
              (gcd-assert (or (zerop (- (copy-bignum tmp1 tmp1-len)
                                        (copy-bignum tmp2 tmp2-len)))
                              (= (copy-bignum u u-len)
                                 (- (copy-bignum tmp1 tmp1-len)
                                    (copy-bignum tmp2 tmp2-len)))))
              (bignum-abs-buffer u u-len)
              ;; This assertion is strange, because we're trying to assert on
              ;; the least significant word of U, but MODULARLY might do a full call
              ;; to TWO-ARG-LOGAND, and it it does, that fails, because not all
              ;; words of U are set. And rightly so- they're set only below U-LEN.
              #+ppc (gcd-assert (zerop (modularly (copy-bignum u u-len))))
              #-ppc (gcd-assert (zerop (modularly u)))))
        (setf u-len (make-gcd-bignum-odd u u-len))
        (rotatef u v)
        (rotatef u-len v-len))
      (bignum-abs-buffer u u-len)
      (setf u (copy-bignum u u-len))
      (let ((n (bignum-mod-gcd v1 u)))
        (ash (bignum-mod-gcd u1 (if (fixnump n)
                                    (make-small-bignum n)
                                    n))
             factors-of-two)))))

(defun bignum-mod-gcd (a b)
  (declare (type bignum a b))
  (when (< a b)
    (rotatef a b))
  ;; While the length difference of A and B is sufficiently large,
  ;; reduce using MOD (slowish, but it should equalize the sizes of
  ;; A and B pretty quickly). After that, use the binary GCD
  ;; algorithm to handle the rest.
  (loop until (and (= (%bignum-length b) 1) (zerop (%bignum-ref b 0))) do
        (when (<= (%bignum-length a) (1+ (%bignum-length b)))
          (return-from bignum-mod-gcd (bignum-binary-gcd a b)))
        (let ((rem (mod a b)))
          (if (fixnump rem)
              (setf a (make-small-bignum rem))
              (setf a rem))
          (rotatef a b)))
  (if (= (%bignum-length a) 1)
      (%normalize-bignum a 1)
      a))

(defun bignum-binary-gcd (a b)
  (declare (type bignum a b))
  (let* ((len-a (%bignum-length a))
         (len-b (%bignum-length b)))
    (with-bignum-buffers ((a-buffer len-a a)
                          (b-buffer len-b b)
                          (res-buffer (max len-a len-b)))
      (let* ((factors-of-two
              (bignum-factors-of-two a-buffer b-buffer))
             (len-a (make-gcd-bignum-odd
                     a-buffer
                     (bignum-buffer-ashift-right a-buffer len-a
                                                 factors-of-two)))
             (len-b (make-gcd-bignum-odd
                     b-buffer
                     (bignum-buffer-ashift-right b-buffer len-b
                                                 factors-of-two))))
        (declare (type bignum-length len-a len-b))
        (let ((x a-buffer)
              (len-x len-a)
              (y b-buffer)
              (len-y len-b)
              (z res-buffer))
          (loop
            (multiple-value-bind (u v len-v r len-r)
                (bignum-gcd-order-and-subtract x len-x y len-y z)
              (declare (type bignum-length len-v len-r))
              (when (and (= len-r 1) (zerop (%bignum-ref r 0)))
                (if (zerop factors-of-two)
                    (let ((ret (%allocate-bignum len-v)))
                      (dotimes (i len-v)
                        (setf (%bignum-ref ret i) (%bignum-ref v i)))
                      (return (%normalize-bignum ret len-v)))
                    (return (bignum-ashift-left v factors-of-two len-v))))
              (setf x v  len-x len-v)
              (setf y r  len-y (make-gcd-bignum-odd r len-r))
              (setf z u))))))))

(defun bignum-gcd-order-and-subtract (a len-a b len-b res)
  (declare (type bignum-length len-a len-b) (type bignum a b))
  (cond ((= len-a len-b)
         (do ((i (1- len-a) (1- i)))
             ((= i -1)
              (setf (%bignum-ref res 0) 0)
              (values a b len-b res 1))
           (let ((a-digit (%bignum-ref a i))
                 (b-digit (%bignum-ref b i)))
             (cond ((= a-digit b-digit))
                   ((> a-digit b-digit)
                    (return
                     (values a b len-b res
                             (subtract-bignum-buffers a len-a b len-b
                                                      res))))
                   (t
                    (return
                     (values b a len-a res
                             (subtract-bignum-buffers b len-b
                                                      a len-a
                                                      res))))))))
        ((> len-a len-b)
         (values a b len-b res
                 (subtract-bignum-buffers a len-a b len-b res)))
        (t
         (values b a len-a res
                 (subtract-bignum-buffers b len-b a len-a res)))))

(defun make-gcd-bignum-odd (a len-a)
  (declare (type bignum a) (type bignum-length len-a))
  (dotimes (index len-a)
    (declare (type bignum-index index))
    (do ((digit (%bignum-ref a index) (%ashr digit 1))
         (increment 0 (1+ increment)))
        ((zerop digit))
      (declare (type (mod #.sb-vm:n-word-bits) increment))
      (when (oddp digit)
        (return-from make-gcd-bignum-odd
                     (bignum-buffer-ashift-right a len-a
                                                 (+ (* index digit-size)
                                                    increment)))))))


;;;; negation

;;; This negates bignum-len digits of bignum, storing the resulting digits into
;;; result (possibly EQ to bignum) and returning whatever end-carry there is.
(defmacro bignum-negate-loop
    (bignum bignum-len &optional (result nil resultp))
  (with-unique-names (carry end value last)
    `(let* (,@(if (not resultp) `(,last))
            (,carry
             (multiple-value-bind (,value ,carry)
                 (%add-with-carry (%lognot (%bignum-ref ,bignum 0)) 1 0)
               ,(if resultp
                    `(setf (%bignum-ref ,result 0) ,value)
                    `(setf ,last ,value))
               ,carry))
            (i 1)
            (,end ,bignum-len))
       (declare (type bit ,carry)
                (type bignum-index i)
                (type bignum-length ,end))
       (loop
         (when (= i ,end) (return))
         (multiple-value-bind (,value temp)
             (%add-with-carry (%lognot (%bignum-ref ,bignum i)) 0 ,carry)
           ,(if resultp
                `(setf (%bignum-ref ,result i) ,value)
                `(setf ,last ,value))
           (setf ,carry temp))
         (incf i))
       ,(if resultp carry `(values ,carry ,last)))))

(declaim (inline negate-bignum))
(defun negate-bignum (x &optional (fully-normalize t))
  (declare (type bignum x)
           (optimize (safety 0)))
  (let* ((len-x (%bignum-length x))
         (msd (%bignum-ref x (1- len-x))))
    (cond ((and fully-normalize
                (= len-x 1)
                (= msd (1+ most-positive-fixnum)))
           most-negative-fixnum)
          (t
           (let* ((len-res (if (= msd (ash 1 (1- digit-size)))
                               ;; only this can overflow, but not necessarily when (> len-x 1)
                               (1+ len-x)
                               len-x))
                  (res (%allocate-bignum len-res))
                  (last1 0)
                  (last2 0)
                  (carry 1)
                  (i 0))
             (loop (when (= i len-x)
                     (return))
                   (setf last1 last2)
                   (setf (values last2 carry)
                         (%add-with-carry (%lognot (%bignum-ref x i)) 0 carry))
                   (setf (%bignum-ref res i) last2)
                   (incf i))
             (when (/= len-res len-x)
               (setf (%bignum-ref res len-x) 0)
               (shiftf last1 last2 0))
             (when (= last2 (%ashr last1 (1- digit-size)))
               (let ((newlen (1- len-res)))
                 (setf (%bignum-ref res newlen) 0)
                 (clear-padding-word res len-res newlen)
                 (%bignum-set-length res newlen)))
             res)))))

;;; Always return a bignum, without any extraneous digits, and it never returns a fixnum.
(defun negate-bignum-not-fully-normalized (x)
  (negate-bignum x nil))

;;; This assumes bignum is positive; that is, the result of negating it will
;;; stay in the provided allocated bignum.
(declaim (maybe-inline negate-bignum-buffer-in-place))
(defun negate-bignum-buffer-in-place (bignum bignum-len)
  (bignum-negate-loop bignum bignum-len bignum)
  bignum)

(defun negate-bignum-in-place (bignum)
  (declare (inline negate-bignum-buffer-in-place))
  (negate-bignum-buffer-in-place bignum (%bignum-length bignum)))

(defun bignum-abs-buffer (bignum len)
  (unless (%bignum-0-or-plusp bignum len)
    (negate-bignum-buffer-in-place bignum len)))

;;;; shifting

;;; This macro is used by BIGNUM-ASHIFT-RIGHT, BIGNUM-BUFFER-ASHIFT-RIGHT, and
;;; BIGNUM-LDB-BIGNUM-RES. They supply a termination form that references
;;; locals established by this form. Source is the source bignum. Start-digit
;;; is the first digit in source from which we pull bits. Start-pos is the
;;; first bit we want. Res-len-form is the form that computes the length of
;;; the resulting bignum. Termination is a DO termination form with a test and
;;; body. When result is supplied, it is the variable to which this binds a
;;; newly allocated bignum.
;;;
;;; Given start-pos, 1-31 inclusively, of shift, we form the j'th resulting
;;; digit from high bits of the i'th source digit and the start-pos number of
;;; bits from the i+1'th source digit.
(defmacro shift-right-unaligned (source
                                       start-digit
                                       start-pos
                                       res-len-form
                                       termination
                                       &optional result)
  `(let* ((high-bits-in-first-digit (- digit-size ,start-pos))
          (res-len ,res-len-form)
          (res-len-1 (1- res-len))
          ,@(if result `((,result (%allocate-bignum res-len)))))
     (declare (type bignum-length res-len res-len-1))
     (do ((i ,start-digit (1+ i))
          (j 0 (1+ j)))
         ,termination
       (declare (type bignum-index i j))
       (setf (%bignum-ref ,(if result result source) j)
             (logior (%digit-logical-shift-right (%bignum-ref ,source i)
                                                  ,start-pos)
                      (%ashl (%bignum-ref ,source (1+ i))
                             high-bits-in-first-digit))))))

;;; First compute the number of whole digits to shift, shifting them by
;;; skipping them when we start to pick up bits, and the number of bits to
;;; shift the remaining digits into place. If the number of digits is greater
;;; than the length of the bignum, then the result is either 0 or -1. If we
;;; shift on a digit boundary (that is, n-bits is zero), then we just copy
;;; digits. The last branch handles the general case which uses a macro that a
;;; couple other routines use. The fifth argument to the macro references
;;; locals established by the macro.
(defun bignum-ashift-right (bignum count)
  (declare (type bignum bignum)
           (type unsigned-byte count))
  (let ((bignum-len (%bignum-length bignum)))
    (cond ((fixnump count)
           (multiple-value-bind (digits n-bits) (truncate count digit-size)
             (declare (type bignum-length digits))
             (cond
              ((>= digits bignum-len)
               (if (%bignum-0-or-plusp bignum bignum-len) 0 -1))
              ((zerop n-bits)
               (bignum-ashift-right-digits bignum digits))
              (t
               (shift-right-unaligned bignum digits n-bits (- bignum-len digits)
                                      ((= j res-len-1)
                                       (setf (%bignum-ref res j)
                                             (%ashr (%bignum-ref bignum i) n-bits))
                                       (%normalize-bignum res res-len))
                                      res)))))
          ((> count bignum-len)
           (if (%bignum-0-or-plusp bignum bignum-len) 0 -1))
           ;; Since a FIXNUM should be big enough to address anything in
           ;; memory, including arrays of bits, and since arrays of bits
           ;; take up about the same space as corresponding fixnums, there
           ;; should be no way that we fall through to this case: any shift
           ;; right by a bignum should give zero. But let's check anyway:
          (t (error "bignum overflow: can't shift right by ~S" count)))))

(defun bignum-ashift-right-digits (bignum digits)
  (declare (type bignum bignum)
           (type bignum-length digits))
  (let* ((res-len (- (%bignum-length bignum) digits))
         (res (%allocate-bignum res-len)))
    (declare (type bignum-length res-len)
             (type bignum res))
    (bignum-replace res bignum :start2 digits)
    (%normalize-bignum res res-len)))

;;; GCD uses this for an in-place shifting operation. This is different enough
;;; from BIGNUM-ASHIFT-RIGHT that it isn't worth folding the bodies into a
;;; macro, but they share the basic algorithm. This routine foregoes a first
;;; test for digits being greater than or equal to bignum-len since that will
;;; never happen for its uses in GCD. We did fold the last branch into a macro
;;; since it was duplicated a few times, and the fifth argument to it
;;; references locals established by the macro.
(defun bignum-buffer-ashift-right (bignum bignum-len x)
  (declare (type bignum-length bignum-len) (fixnum x))
  (multiple-value-bind (digits n-bits) (truncate x digit-size)
    (declare (type bignum-length digits))
    (cond
     ((zerop n-bits)
      (let ((new-end (- bignum-len digits)))
        (bignum-replace bignum bignum :end1 new-end :start2 digits
                        :end2 bignum-len)
        (%normalize-bignum-buffer bignum new-end)))
     (t
      (shift-right-unaligned bignum digits n-bits (- bignum-len digits)
                             ((= j res-len-1)
                              (setf (%bignum-ref bignum j)
                                    (%ashr (%bignum-ref bignum i) n-bits))
                              (%normalize-bignum-buffer bignum res-len)))))))

;;; This handles shifting a bignum buffer to provide fresh bignum data for some
;;; internal routines. We know bignum is safe when called with bignum-len.
;;; First we compute the number of whole digits to shift, shifting them
;;; starting to store farther along the result bignum. If we shift on a digit
;;; boundary (that is, n-bits is zero), then we just copy digits. The last
;;; branch handles the general case.
(defun bignum-ashift-left (bignum x &optional bignum-len)
  (declare (type bignum bignum)
           (type unsigned-byte x)
           (type (or null bignum-length) bignum-len))
  (if (fixnump x)
    (multiple-value-bind (digits n-bits) (truncate x digit-size)
      (let* ((bignum-len (or bignum-len (%bignum-length bignum)))
             (res-len (+ digits bignum-len 1)))
        (when (> res-len maximum-bignum-length)
          (error "can't represent result of left shift"))
        (if (zerop n-bits)
          (bignum-ashift-left-digits bignum bignum-len digits)
          (bignum-ashift-left-unaligned bignum digits n-bits res-len))))
    ;; Left shift by a number too big to be represented as a fixnum
    ;; would exceed our memory capacity, since a fixnum is big enough
    ;; to index any array, including a bit array.
    (error "can't represent result of left shift")))

(defun bignum-ashift-left-digits (bignum bignum-len digits)
  (declare (type bignum-length bignum-len digits))
  (let* ((res-len (+ bignum-len digits))
         (res (alloc-zeroing-below res-len digits)))
    (declare (type bignum-length res-len))
    (bignum-replace res bignum :start1 digits :end1 res-len :end2 bignum-len)
    res))

;;; BIGNUM-TRUNCATE uses this to store into a bignum buffer by supplying res.
;;; When res comes in non-nil, then this foregoes allocating a result, and it
;;; normalizes the buffer instead of the would-be allocated result.
;;;
;;; We start storing into one digit higher than digits, storing a whole result
;;; digit from parts of two contiguous digits from bignum. When the loop
;;; finishes, we store the remaining bits from bignum's first digit in the
;;; first non-zero result digit, digits. We also grab some left over high
;;; bits from the last digit of bignum.
(defun bignum-ashift-left-unaligned (bignum digits n-bits res-len
                                     &optional (res nil resp))
  (declare (type bignum-length digits res-len)
           (type (mod #.digit-size) n-bits))
  (let* ((remaining-bits (- digit-size n-bits))
         (res-len-1 (1- res-len))
         (res (or res (alloc-zeroing-below res-len digits))))
    (declare (type bignum-length res-len res-len-1))
    (do ((i 0 (1+ i))
         (j (1+ digits) (1+ j)))
        ((= j res-len-1)
         (setf (%bignum-ref res digits)
               (%ashl (%bignum-ref bignum 0) n-bits))
         (setf (%bignum-ref res j)
               (%ashr (%bignum-ref bignum i) remaining-bits))
         (if resp
             (%normalize-bignum-buffer res res-len)
             (%normalize-bignum res res-len)))
      (declare (type bignum-index i j))
      (setf (%bignum-ref res j)
            (logior (%digit-logical-shift-right (%bignum-ref bignum i)
                                                 remaining-bits)
                     (%ashl (%bignum-ref bignum (1+ i)) n-bits))))))

;;; FIXNUM is assumed to be non-zero and the result of the shift should be a bignum
(defun bignum-ashift-left-fixnum (fixnum count)
  (declare ((and unsigned-byte fixnum) count)
           (fixnum fixnum))
  (multiple-value-bind (right-zero-digits remaining)
      (truncate count digit-size)
    (let* ((right-half (ldb (byte digit-size 0)
                            (ash fixnum remaining)))
           (sign-bit-p
             (logbitp (1- digit-size) right-half))
           (left-half (ash fixnum
                           (- remaining digit-size)))
           ;; Even if the left-half is 0 or -1 it might need to be sign
           ;; extended based on the left-most bit of the right-half
           (left-half-p (if sign-bit-p
                            (/= left-half -1)
                            (/= left-half 0)))
           (length (+ right-zero-digits
                      (if left-half-p 2 1))))
      (when (> length maximum-bignum-length)
        (error "can't represent result of left shift"))
      (let ((result (alloc-zeroing-below length right-zero-digits)))
        (setf (%bignum-ref result right-zero-digits) right-half)
        (when left-half-p
          (setf (%bignum-ref result (1+ right-zero-digits))
                (ldb (byte digit-size 0) left-half)))
        result))))

;;;; relational operators

;;; This compares two bignums returning -1, 0, or 1, depending on
;;; whether a is less than, equal to, or greater than b.
(declaim (ftype (function (bignum bignum) (integer -1 1)) bignum-compare))
(defun bignum-compare (a b)
  (declare (type bignum a b))
  (let* ((len-a (%bignum-length a))
         (len-b (%bignum-length b))
         (a-plusp (%bignum-0-or-plusp a len-a))
         (b-plusp (%bignum-0-or-plusp b len-b)))
    (declare (type bignum-length len-a len-b))
    (cond ((not (eq a-plusp b-plusp))
           (if a-plusp 1 -1))
          ((= len-a len-b)
           (do ((i (1- len-a) (1- i)))
               (())
             (declare (type bignum-index i))
             (let ((a-digit (%bignum-ref a i))
                   (b-digit (%bignum-ref b i)))
               (declare (type bignum-element-type a-digit b-digit))
               (when (> a-digit b-digit)
                 (return 1))
               (when (> b-digit a-digit)
                 (return -1)))
             (when (zerop i) (return 0))))
          ((> len-a len-b)
           (if a-plusp 1 -1))
          (t (if a-plusp -1 1)))))

;;;; float conversion

;;; Return T if the least significant N-BITS bits of BIGNUM are all
;;; zero, else NIL. If the integer-length of BIGNUM is less than N-BITS,
;;; the result is NIL, too.
(declaim (inline bignum-lower-bits-zero-p))
(defun bignum-lower-bits-zero-p (bignum n-bits length)
  (declare (type bignum bignum)
           ((and (integer 1) bignum-length) length)
           (type bit-index n-bits))
  (multiple-value-bind (n-full-digits n-bits-partial-digit)
      (floor n-bits digit-size)
    (declare (type bignum-length n-full-digits))
    (when (> length n-full-digits)
      (dotimes (index n-full-digits)
        (declare (type bignum-index index))
        (unless (zerop (%bignum-ref bignum index))
          (return-from bignum-lower-bits-zero-p nil)))
      (zerop (logand (1- (ash 1 n-bits-partial-digit))
                     (%bignum-ref bignum n-full-digits))))))

(declaim (inline bignum-negate-last-two))
(defun bignum-negate-last-two (bignum &optional (len (%bignum-length bignum)))
  (let* ((last1 0)
         (last2 0)
         (carry 1)
         (i 0))
    (declare (type bit carry)
             (type bignum-index i))
    (loop (when (= i len)
            (return))
          (setf last1 last2)
          (setf (values last2 carry)
                (%add-with-carry (%lognot (%bignum-ref bignum i)) 0 carry))
          (incf i))
    (values last1 last2)))

;;; Make a single or double float with the specified significand,
;;; exponent and sign.
;;; FIXME: how are these not the same as {SINGLE,DOUBLE}-FROM-BITS ???
#-64-bit
(declaim (inline single-float-from-bits double-float-from-bits))
#-64-bit
(defun single-float-from-bits (bits exp plusp)
  (declare (fixnum exp))
  ;; "float to pointer coercion -> return value"
  (declare (muffle-conditions compiler-note))
  (let ((res (dpb exp
                  sb-vm:single-float-exponent-byte
                  (logandc2 (logand #xffffffff
                                    (%bignum-ref bits 1))
                            sb-vm:single-float-hidden-bit))))
    (sb-kernel:make-single-float
     (if plusp
         res
         (logior res (ash -1 sb-vm:float-sign-shift))))))
#-64-bit
(defun double-float-from-bits (bits exp plusp)
  (declare (fixnum exp))
  ;; "float to pointer coercion -> return value"
  (declare (muffle-conditions compiler-note))
  (let ((hi (dpb exp
                 sb-vm:double-float-hi-exponent-byte
                 (logandc2 (ecase sb-vm:n-word-bits
                             (32 (%bignum-ref bits 2))
                             (64 (ash (%bignum-ref bits 1) -32)))
                           (ash sb-vm:double-float-hidden-bit -32))))
        (lo (logand #xffffffff (%bignum-ref bits 1))))
    (sb-kernel:make-double-float (if plusp
                           hi
                           (logior hi (ash -1 sb-vm:float-sign-shift)))
                       lo)))
#+(and long-float x86)
(defun long-float-from-bits (bits exp plusp)
  (declare (fixnum exp))
  (sb-kernel:make-long-float
   (if plusp
       exp
       (logior exp (ash 1 15)))
   (%bignum-ref bits 2)
   (%bignum-ref bits 1)))

;;; Convert Bignum to a float in the specified Format, rounding to the best
;;; approximation.
#-64-bit
(macrolet ((def (type)
             `(defun ,(symbolicate 'bignum-to- type) (bignum)
               (let* ((plusp (bignum-plus-p bignum))
                      (x (if plusp bignum (negate-bignum-not-fully-normalized bignum)))
                      (len (bignum-integer-length x))
                      (digits ,(package-symbolicate :sb-vm type '-digits))
                      (keep (+ digits digit-size))
                      (shift (- keep len))
                      (shifted (if (minusp shift)
                                   (bignum-ashift-right x (- shift))
                                   (bignum-ashift-left x shift)))
                      (low (%bignum-ref shifted 0))
                      (round-bit (ash 1 (1- digit-size))))
                 (declare (type bignum-length len digits keep) (fixnum shift))
                 (labels ((round-up ()
                            (let ((rounded (add-bignums shifted round-bit)))
                              (if (> (integer-length rounded) keep)
                                  (float-from-bits (bignum-ashift-right rounded 1)
                                                   (1+ len))
                                  (float-from-bits rounded len))))
                          (float-from-bits (bits len)
                            (declare (type bignum-length len))
                            ,(case type
                               (single-float
                                `(single-float-from-bits
                                  bits
                                  (check-exponent len sb-vm:single-float-bias
                                                  sb-vm:single-float-normal-exponent-max)
                                  plusp))
                               (double-float
                                `(double-float-from-bits
                                  bits
                                  (check-exponent len sb-vm:double-float-bias
                                                  sb-vm:double-float-normal-exponent-max)
                                  plusp))
                               #+long-float
                               (long-float
                                `(long-float-from-bits
                                 bits
                                 (check-exponent len sb-vm:long-float-bias
                                                 sb-vm:long-float-normal-exponent-max)
                                 plusp))))
                          (check-exponent (exp bias max)
                            (declare (type bignum-length len))
                            (let ((exp (+ exp bias)))
                              (when (> exp max)
                                (error 'floating-point-overflow
                                       :operation 'float
                                       :operands (list x ',type)))
                              exp)))

                   (cond
                     ;; Round down if round bit is 0.
                     ((not (logtest round-bit low))
                      (float-from-bits shifted len))
                     ;; If only round bit is set, then round to even.
                     ((and (= low round-bit)
                           (dotimes (i (- (%bignum-length x) (ceiling keep digit-size))
                                       t)
                             (unless (zerop (%bignum-ref x i)) (return nil))))
                      (let ((next (%bignum-ref shifted 1)))
                        (if (oddp next)
                            (round-up)
                            (float-from-bits shifted len))))
                     ;; Otherwise, round up.
                     (t
                      (round-up))))))))
  (def single-float)
  (def double-float))

#+64-bit
(macrolet
    ((def (type)
       (flet ((const (name)
                (package-symbolicate :sb-vm type '- name)))
         `(defun ,(symbolicate 'bignum-to- type) (bignum)
            (let ((bignum-length (%bignum-length bignum)))
              ;; word-sized bignums shouldn't reach here
              (declare ((integer 2) bignum-length))
              (,(case type
                  (single-float 'sb-kernel:make-single-float)
                  (double-float 'sb-kernel:%make-double-float))
               (if (%bignum-0-or-plusp bignum bignum-length)
                   (let* ((length (truly-the bignum-length (bignum-buffer-integer-length bignum bignum-length)))
                          (shift (- length ,(const 'digits)))
                          ;; Get one more bit for rounding
                          (shifted (truly-the fixnum
                                              (last-bignum-part=>fixnum (- sb-bignum::digit-size ,(const 'digits))
                                                                        (1- shift) bignum)))
                          ;; Cut off the hidden bit
                          (signif (ldb ,(const 'significand-byte) (ash shifted -1)))
                          (exp (truly-the (unsigned-byte ,(byte-size sb-vm:double-float-exponent-byte))
                                          (+ ,(const 'bias) length)))
                          (bits (ash exp
                                     (byte-position ,(const 'exponent-byte)))))
                     (when (and (logtest shifted 1)
                                (or (logtest signif 1)
                                    (not (bignum-lower-bits-zero-p bignum shift bignum-length))))
                       ;; Round up
                       (incf signif))
                     ;; If rounding up overflows this will increase the exponent too
                     (let ((bits (+ bits signif)))
                       (when (or (> exp ,(const 'normal-exponent-max))
                                 ;; Overflow after rounding up
                                 (= bits (,(const 'bits) ,(const 'positive-infinity))))
                         (error 'floating-point-overflow
                                :operation 'float
                                :operands (list bignum ',type)))
                       (truly-the sb-vm:signed-word bits)))
                   (multiple-value-bind (last1 last2) (bignum-negate-last-two bignum bignum-length)
                     (let* ((last2-length (integer-length last2))
                            (length (+ last2-length (* (1- bignum-length) digit-size)))
                            (shift (- length ,(const 'digits)))
                            (bit-index (rem (1- shift) digit-size))
                            (shifted (cond ((zerop last2)
                                            (truly-the word (ash last1 (- bit-index))))
                                           ((<= bit-index (- digit-size (1+ ,(const 'digits))))
                                            (truly-the word (ash last2 (- bit-index))))
                                           (t
                                            (logand most-positive-word
                                                    (logior (ash last2 (- digit-size bit-index))
                                                            (ash last1 (- bit-index)))))))
                            (signif (ldb ,(const 'significand-byte) (ash shifted -1)))
                            (exp (truly-the (unsigned-byte 11) (+ ,(const 'bias) length)))
                            (bits (ash exp (byte-position ,(const 'exponent-byte)))))
                       (when (and (logtest shifted 1)
                                  (or (logtest signif 1)
                                      (not (bignum-lower-bits-zero-p bignum shift bignum-length))))
                         (incf signif))
                       (let ((bits (+ bits signif)))
                         (when (or (> exp ,(const 'normal-exponent-max))
                                   (= bits (,(const 'bits) ,(const 'positive-infinity))))
                           (error 'floating-point-overflow
                                  :operation 'float
                                  :operands (list bignum ',type)))
                         (logior (ash -1 ,(case type
                                            (double-float 63)
                                            (single-float 31)))
                                 (truly-the sb-vm:signed-word bits))))))))))))
  (def single-float)
  (def double-float))

;;;; integer length and logbitp/logcount

(defun bignum-buffer-integer-length (bignum len)
  (declare (type bignum bignum))
  (let* ((len-1 (1- len))
         (digit (%bignum-ref bignum len-1)))
    (declare (type bignum-length len len-1)
             (type bignum-element-type digit))
    (+ (integer-length (%fixnum-digit-with-correct-sign digit))
       (* len-1 digit-size))))

(defun bignum-integer-length (bignum)
  (declare (type bignum bignum))
  (bignum-buffer-integer-length bignum (%bignum-length bignum)))

(defun bignum-logbitp (index bignum)
  (declare (type bignum bignum)
           (type bignum-index index))
  (let ((len (%bignum-length bignum)))
    (declare (type bignum-length len))
    (multiple-value-bind (word-index bit-index)
        (floor index digit-size)
      (if (>= word-index len)
          (not (%bignum-0-or-plusp bignum len))
          (logbitp bit-index (%bignum-ref bignum word-index))))))

(defun bignum-logcount (bignum)
  (declare (type bignum bignum))
  (let ((length (%bignum-length bignum))
        (result 0))
    (declare (type bignum-length length)
             (type (integer 0 #.(* maximum-bignum-length digit-size))  result))
    (do ((index 0 (1+ index)))
        ((= index length)
         (if (%bignum-0-or-plusp bignum length)
             result
             (- (* length digit-size) result)))
      (let ((digit (%bignum-ref bignum index)))
        (declare (type bignum-element-type digit))
        (incf result (logcount digit))))))

;;;; logical operations

;;;; NOT

(defun bignum-logical-not (a)
  (declare (type bignum a))
  (let* ((len (%bignum-length a))
         (res (%allocate-bignum len)))
    (declare (type bignum-length len))
    (dotimes (i len res)
      (declare (type bignum-index i))
      (setf (%bignum-ref res i) (%lognot (%bignum-ref a i))))))

;;;; AND

(defun bignum-logical-and (a b)
  (declare (type bignum a b))
  (let* ((len-a (%bignum-length a))
         (len-b (%bignum-length b))
         (a-plusp (%bignum-0-or-plusp a len-a))
         (b-plusp (%bignum-0-or-plusp b len-b)))
    (declare (type bignum-length len-a len-b))
    (cond
     ((< len-a len-b)
      (if a-plusp
          (logand-shorter-positive a len-a b (%allocate-bignum len-a))
          (logand-shorter-negative a len-a b len-b (%allocate-bignum len-b))))
     ((< len-b len-a)
      (if b-plusp
          (logand-shorter-positive b len-b a (%allocate-bignum len-b))
          (logand-shorter-negative b len-b a len-a (%allocate-bignum len-a))))
     (t (logand-shorter-positive a len-a b (%allocate-bignum len-a))))))

;;; This takes a shorter bignum, a and len-a, that is positive. Because this
;;; is AND, we don't care about any bits longer than a's since its infinite 0
;;; sign bits will mask the other bits out of b. The result is len-a big.
(defun logand-shorter-positive (a len-a b res)
  (declare (type bignum a b res)
           (type bignum-length len-a))
  (dotimes (i len-a)
    (declare (type bignum-index i))
    (setf (%bignum-ref res i)
          (logand (%bignum-ref a i) (%bignum-ref b i))))
  (%normalize-bignum res len-a))

;;; This takes a shorter bignum, a and len-a, that is negative. Because this
;;; is AND, we just copy any bits longer than a's since its infinite 1 sign
;;; bits will include any bits from b. The result is len-b big.
(defun logand-shorter-negative (a len-a b len-b res)
  (declare (type bignum a b res)
           (type bignum-length len-a len-b))
  (dotimes (i len-a)
    (declare (type bignum-index i))
    (setf (%bignum-ref res i)
          (logand (%bignum-ref a i) (%bignum-ref b i))))
  (do ((i len-a (1+ i)))
      ((= i len-b))
    (declare (type bignum-index i))
    (setf (%bignum-ref res i) (%bignum-ref b i)))
  (%normalize-bignum res len-b))

;;;; IOR

(defun bignum-logical-ior (a b)
  (declare (type bignum a b))
  (let* ((len-a (%bignum-length a))
         (len-b (%bignum-length b))
         (a-plusp (%bignum-0-or-plusp a len-a))
         (b-plusp (%bignum-0-or-plusp b len-b)))
    (declare (type bignum-length len-a len-b))
    (cond
     ((< len-a len-b)
      (if a-plusp
          (logior-shorter-positive a len-a b len-b (%allocate-bignum len-b))
          (logior-shorter-negative a len-a b len-b (%allocate-bignum len-b))))
     ((< len-b len-a)
      (if b-plusp
          (logior-shorter-positive b len-b a len-a (%allocate-bignum len-a))
          (logior-shorter-negative b len-b a len-a (%allocate-bignum len-a))))
     (t (logior-shorter-positive a len-a b len-b (%allocate-bignum len-a))))))

;;; This takes a shorter bignum, a and len-a, that is positive. Because this
;;; is IOR, we don't care about any bits longer than a's since its infinite
;;; 0 sign bits will mask the other bits out of b out to len-b. The result
;;; is len-b long.
(defun logior-shorter-positive (a len-a b len-b res)
  (declare (type bignum a b res)
           (type bignum-length len-a len-b))
  (dotimes (i len-a)
    (declare (type bignum-index i))
    (setf (%bignum-ref res i)
          (logior (%bignum-ref a i) (%bignum-ref b i))))
  (do ((i len-a (1+ i)))
      ((= i len-b))
    (declare (type bignum-index i))
    (setf (%bignum-ref res i) (%bignum-ref b i)))
  (%normalize-bignum res len-b))

;;; This takes a shorter bignum, a and len-a, that is negative. Because this
;;; is IOR, we just copy any bits longer than a's since its infinite 1 sign
;;; bits will include any bits from b. The result is len-b long.
(defun logior-shorter-negative (a len-a b len-b res)
  (declare (type bignum a b res)
           (type bignum-length len-a len-b))
  (dotimes (i len-a)
    (declare (type bignum-index i))
    (setf (%bignum-ref res i)
          (logior (%bignum-ref a i) (%bignum-ref b i))))
  (do ((i len-a (1+ i))
       (sign (%sign-digit a len-a)))
      ((= i len-b))
    (declare (type bignum-index i))
    (setf (%bignum-ref res i) sign))
  (%normalize-bignum res len-b))

;;;; XOR

(defun bignum-logical-xor (a b)
  (declare (type bignum a b))
  (let ((len-a (%bignum-length a))
        (len-b (%bignum-length b)))
    (declare (type bignum-length len-a len-b))
    (if (< len-a len-b)
        (bignum-logical-xor-aux a len-a b len-b (%allocate-bignum len-b))
        (bignum-logical-xor-aux b len-b a len-a (%allocate-bignum len-a)))))

;;; This takes the shorter of two bignums in a and len-a. Res is len-b
;;; long. Do the XOR.
(defun bignum-logical-xor-aux (a len-a b len-b res)
  (declare (type bignum a b res)
           (type bignum-length len-a len-b))
  (dotimes (i len-a)
    (declare (type bignum-index i))
    (setf (%bignum-ref res i)
          (logxor (%bignum-ref a i) (%bignum-ref b i))))
  (do ((i len-a (1+ i))
       (sign (%sign-digit a len-a)))
      ((= i len-b))
    (declare (type bignum-index i))
    (setf (%bignum-ref res i) (logxor sign (%bignum-ref b i))))
  (%normalize-bignum res len-b))

;;;; There used to be a bunch of code to implement "efficient" versions of LDB
;;;; and DPB here.  But it apparently was never used, so it's been deleted.
;;;;   --njf, 2007-02-04

;; This could be used by way of a transform, though for now it's specifically
;; a helper for %LDB in the limited case that it recognizes as non-consing.
(defun ldb-bignum=>fixnum (byte-size byte-pos bignum)
  (declare (type (integer 0 #.sb-vm:n-positive-fixnum-bits) byte-size)
           (type bit-index byte-pos))
  (multiple-value-bind (word-index bit-index) (floor byte-pos digit-size)
    (let ((n-digits (%bignum-length bignum)))
      (cond ((>= word-index n-digits) ; load from the infinitely extended sign word
             (ldb (byte byte-size 0) (%sign-digit bignum n-digits)))
            ((<= (+ bit-index byte-size) digit-size) ; contained in one word
             ;; This case takes care of byte-size = 0 also.
             (ldb (byte byte-size bit-index) (%bignum-ref bignum word-index)))
            (t
             ;; At least one bit is obtained from each of two words,
             ;; and not more than two words.
             (let* ((low-part-size
                      (truly-the (integer 1 #.(1- sb-vm:n-positive-fixnum-bits))
                                 (- digit-size bit-index)))
                    (high-part-size
                      (truly-the (integer 1 #.(1- sb-vm:n-positive-fixnum-bits))
                                 (- byte-size low-part-size))))
               (logior
                ;; high part
                (truly-the fixnum
                           (let ((word-index (1+ word-index)))
                             (ash (ldb (byte high-part-size 0)
                                       (if (< word-index n-digits) ; next word exists
                                           (%bignum-ref bignum word-index)
                                           (%sign-digit bignum n-digits)))
                                  low-part-size)))
                (truly-the fixnum
                           (ash (%bignum-ref bignum word-index) (- bit-index))))))))))

;;; Basically shift the bignum right by byte-pos, but assumes it's
;;; right at the end of the bignum.
;;; byte-size-left is (- digit-size byte-size)
(defun last-bignum-part=>fixnum (byte-size-left byte-pos bignum)
  (declare (type bit-index byte-pos)
           (type (integer 0 #.sb-vm:n-positive-fixnum-bits) byte-size-left)
           (bignum bignum))
  (multiple-value-bind (word-index bit-index) (floor byte-pos digit-size)
    (let ((one (%bignum-ref bignum word-index)))
      (cond ((<= bit-index byte-size-left) ; contained in one word
             (truly-the fixnum (ash (sb-c::mask-signed-field digit-size one) (- bit-index))))
            (t
             ;; At least one bit is obtained from each of two words,
             ;; and not more than two words.
             (sb-c::mask-signed-field sb-vm:n-fixnum-bits
                                      (logior
                                       (ash (%bignum-ref bignum (1+ word-index))
                                            (truly-the (integer 0 (#.digit-size)) (- digit-size bit-index)))
                                       (ash one (- bit-index)))))))))

;;;; TRUNCATE

;;; This is the original sketch of the algorithm from which I implemented this
;;; TRUNCATE, assuming both operands are bignums. I should modify this to work
;;; with the documentation on my functions, as a general introduction. I've
;;; left this here just in case someone needs it in the future. Don't look at
;;; this unless reading the functions' comments leaves you at a loss. Remember
;;; this comes from Knuth, so the book might give you the right general
;;; overview.
;;;
;;; (truncate x y):
;;;
;;; If X's magnitude is less than Y's, then result is 0 with remainder X.
;;;
;;; Make x and y positive, copying x if it is already positive.
;;;
;;; Shift y left until there's a 1 in the 30'th bit (most significant, non-sign
;;;       digit)
;;;    Just do most sig digit to determine how much to shift whole number.
;;; Shift x this much too.
;;; Remember this initial shift count.
;;;
;;; Allocate q to be len-x minus len-y quantity plus 1.
;;;
;;; i = last digit of x.
;;; k = last digit of q.
;;;
;;; LOOP
;;;
;;; j = last digit of y.
;;;
;;; compute guess.
;;; if x[i] = y[j] then g = (1- (ash 1 digit-size))
;;; else g = x[i]x[i-1]/y[j].
;;;
;;; check guess.
;;; %UNSIGNED-MULTIPLY returns b and c defined below.
;;;    a = x[i-1] - (logand (* g y[j]) #xFFFFFFFF).
;;;       Use %UNSIGNED-MULTIPLY taking low-order result.
;;;    b = (logand (ash (* g y[j-1]) (- digit-size)) (1- (ash 1 digit-size))).
;;;    c = (logand (* g y[j-1]) (1- (ash 1 digit-size))).
;;; if a < b, okay.
;;; if a > b, guess is too high
;;;    g = g - 1; go back to "check guess".
;;; if a = b and c > x[i-2], guess is too high
;;;    g = g - 1; go back to "check guess".
;;; GUESS IS 32-BIT NUMBER, SO USE THING TO KEEP IN SPECIAL REGISTER
;;; SAME FOR A, B, AND C.
;;;
;;; Subtract g * y from x[i - len-y+1]..x[i]. See paper for doing this in step.
;;; If x[i] < 0, guess is screwed up.
;;;    negative g, then add 1
;;;    zero or positive g, then subtract 1
;;; AND add y back into x[len-y+1..i].
;;;
;;; q[k] = g.
;;; i = i - 1.
;;; k = k - 1.
;;;
;;; If k>=0, goto LOOP.
;;;
;;; Now quotient is good, but remainder is not.
;;; Shift x right by saved initial left shifting count.
;;;
;;; Check quotient and remainder signs.
;;; x pos y pos --> q pos r pos
;;; x pos y neg --> q neg r pos
;;; x neg y pos --> q neg r neg
;;; x neg y neg --> q pos r neg
;;;
;;; Normalize quotient and remainder. Cons result if necessary.


;;; This used to be split into multiple functions, which shared state
;;; in special variables *TRUNCATE-X* and *TRUNCATE-Y*. Having so many
;;; special variable accesses in tight inner loops was having a large
;;; effect on performance, so the helper functions have now been
;;; refactored into local functions and the special variables into
;;; lexicals.  There was also a lot of boxing and unboxing of
;;; (UNSIGNED-BYTE 32)'s going on, which this refactoring
;;; eliminated. This improves the performance on some CL-BENCH tests
;;; by up to 50%, which is probably signigicant enough to justify the
;;; reduction in readability that was introduced. --JES, 2004-08-07
(defun bignum-truncate (x y)
  (declare (type bignum x y))
  (declare (muffle-conditions compiler-note)) ; returns lispobj, so what.
  (let (truncate-x truncate-y)
    (labels
        ;; This returns a guess for the next division step. Y1 is the
         ;; highest y digit, and y2 is the second to highest y
         ;; digit. The x... variables are the three highest x digits
         ;; for the next division step.
         ;;
         ;; From Knuth, our guess is either all ones or x-i and x-i-1
         ;; divided by y1, depending on whether x-i and y1 are the
         ;; same. We test this guess by determining whether guess*y2
         ;; is greater than the three high digits of x minus guess*y1
         ;; shifted left one digit:
         ;;    ------------------------------
         ;;   |    x-i    |   x-i-1  | x-i-2 |
         ;;    ------------------------------
         ;;    ------------------------------
         ;; - | g*y1 high | g*y1 low |   0   |
         ;;    ------------------------------
         ;;             ...               <   guess*y2     ???
         ;; If guess*y2 is greater, then we decrement our guess by one
         ;; and try again.  This returns a guess that is either
         ;; correct or one too large.
        ((bignum-truncate-guess (y1 y2 x-i x-i-1 x-i-2)
           (declare (type bignum-element-type y1 y2 x-i x-i-1 x-i-2))
           (let ((guess (if (= x-i y1)
                            all-ones-digit
                            (%bigfloor x-i x-i-1 y1))))
             (declare (type bignum-element-type guess))
             (loop
              (multiple-value-bind (high-guess*y1 low-guess*y1)
                  (%multiply guess y1)
                (declare (type bignum-element-type low-guess*y1
                               high-guess*y1))
                (multiple-value-bind (high-guess*y2 low-guess*y2)
                    (%multiply guess y2)
                  (declare (type bignum-element-type high-guess*y2
                                 low-guess*y2))
                  (multiple-value-bind (middle-digit borrow)
                      (%subtract-with-borrow x-i-1 low-guess*y1 1)
                    (declare (type bignum-element-type middle-digit)
                             (fixnum borrow))
                    ;; Supplying borrow of 1 means there was no
                    ;; borrow, and we know x-i-2 minus 0 requires
                    ;; no borrow.
                    (let ((high-digit (%subtract-with-borrow x-i
                                                             high-guess*y1
                                                             borrow)))
                      (declare (type bignum-element-type high-digit))
                      (if (and (= high-digit 0)
                               (or (> high-guess*y2 middle-digit)
                                   (and (= middle-digit high-guess*y2)
                                        (> low-guess*y2 x-i-2))))
                          (setf guess (%subtract-with-borrow guess 1 1))
                          (return guess)))))))))
         ;; Divide TRUNCATE-X by TRUNCATE-Y, returning the quotient
         ;; and destructively modifying TRUNCATE-X so that it holds
         ;; the remainder.
         ;;
         ;; LEN-X and LEN-Y tell us how much of the buffers we care about.
         ;;
         ;; TRUNCATE-X definitely has at least three digits, and it has one
         ;; more than TRUNCATE-Y. This keeps i, i-1, i-2, and low-x-digit
         ;; happy. Thanks to SHIFT-AND-STORE-TRUNCATE-BUFFERS.
         (return-quotient-leaving-remainder (len-x len-y)
           (declare (type bignum-length len-x len-y))
           (let* ((len-q (- len-x len-y))
                  ;; Add one for extra sign digit in case high bit is on.
                  (q (%allocate-bignum (1+ len-q)))
                  (k (1- len-q))
                  (y1 (%bignum-ref truncate-y (1- len-y)))
                  (y2 (%bignum-ref truncate-y (- len-y 2)))
                  (i (1- len-x))
                  (i-1 (1- i))
                  (i-2 (1- i-1))
                  (low-x-digit (- i len-y)))
             (declare (type bignum-length len-q)
                      (type bignum-index k i i-1 i-2 low-x-digit)
                      (type bignum-element-type y1 y2))
             ;; DO NOT ASSUME THAT %ALLOCATE-BIGNUM PREZEROS
             (setf (%bignum-ref q len-q) 0)
             (loop
              (setf (%bignum-ref q k)
                    (try-bignum-truncate-guess
                     ;; This modifies TRUNCATE-X. Must access
                     ;; elements each pass.
                     (bignum-truncate-guess y1 y2
                                            (%bignum-ref truncate-x i)
                                            (%bignum-ref truncate-x i-1)
                                            (%bignum-ref truncate-x i-2))
                     len-y low-x-digit))
              (cond ((zerop k) (return))
                    (t (decf k)
                       (decf low-x-digit)
                       (shiftf i i-1 i-2 (1- i-2)))))
             q))
         ;; This takes a digit guess, multiplies it by TRUNCATE-Y for a
         ;; result one greater in length than LEN-Y, and subtracts this result
         ;; from TRUNCATE-X. LOW-X-DIGIT is the first digit of X to start
         ;; the subtraction, and we know X is long enough to subtract a LEN-Y
         ;; plus one length bignum from it. Next we check the result of the
         ;; subtraction, and if the high digit in X became negative, then our
         ;; guess was one too big. In this case, return one less than GUESS
         ;; passed in, and add one value of Y back into X to account for
         ;; subtracting one too many. Knuth shows that the guess is wrong on
         ;; the order of 3/b, where b is the base (2 to the digit-size power)
         ;; -- pretty rarely.
         (try-bignum-truncate-guess (guess len-y low-x-digit)
           (declare (type bignum-index low-x-digit)
                    (type bignum-length len-y)
                    (type bignum-element-type guess))
           (let ((carry-digit 0)
                 (borrow 1)
                 (i low-x-digit))
             (declare (type bignum-element-type carry-digit)
                      (type bignum-index i)
                      (fixnum borrow))
             ;; Multiply guess and divisor, subtracting from dividend
             ;; simultaneously.
             (dotimes (j len-y)
               (multiple-value-bind (high-digit low-digit)
                   (%multiply-and-add guess
                                      (%bignum-ref truncate-y j)
                                      carry-digit)
                 (declare (type bignum-element-type high-digit low-digit))
                 (setf carry-digit high-digit)
                 (multiple-value-bind (x temp-borrow)
                     (%subtract-with-borrow (%bignum-ref truncate-x i)
                                            low-digit
                                            borrow)
                   (declare (type bignum-element-type x)
                            (fixnum temp-borrow))
                   (setf (%bignum-ref truncate-x i) x)
                   (setf borrow temp-borrow)))
               (incf i))
             (setf (%bignum-ref truncate-x i)
                   (%subtract-with-borrow (%bignum-ref truncate-x i)
                                          carry-digit borrow))
             ;; See whether guess is off by one, adding one
             ;; Y back in if necessary.
             (cond ((%digit-0-or-plusp (%bignum-ref truncate-x i))
                    guess)
                   (t
                    ;; If subtraction has negative result, add one
                    ;; divisor value back in. The guess was one too
                    ;; large in magnitude.
                    (let ((i low-x-digit)
                          (carry 0))
                      (dotimes (j len-y)
                        (multiple-value-bind (v k)
                            (%add-with-carry (%bignum-ref truncate-y j)
                                             (%bignum-ref truncate-x i)
                                             carry)
                          (declare (type bignum-element-type v))
                          (setf (%bignum-ref truncate-x i) v)
                          (setf carry k))
                        (incf i))
                      (setf (%bignum-ref truncate-x i)
                            (%add-with-carry (%bignum-ref truncate-x i)
                                             0 carry)))
                    (%subtract-with-borrow guess 1 1)))))
         ;; This returns the amount to shift y to place a one in the
         ;; second highest bit. Y must be positive. If the last digit
         ;; of y is zero, then y has a one in the previous digit's
         ;; sign bit, so we know it will take one less than digit-size
         ;; to get a one where we want. Otherwise, we count how many
         ;; right shifts it takes to get zero; subtracting this value
         ;; from digit-size tells us how many high zeros there are
         ;; which is one more than the shift amount sought.
         ;;
         ;; Note: This is exactly the same as one less than the
         ;; integer-length of the last digit subtracted from the
         ;; digit-size.
         ;;
         ;; We shift y to make it sufficiently large that doing the
         ;; 2*digit-size by digit-size %BIGFLOOR calls ensures the quotient and
         ;; remainder fit in digit-size.
         (shift-y-for-truncate (y)
           (let* ((len (%bignum-length y))
                  (last (%bignum-ref y (1- len))))
             (declare (type bignum-length len)
                      (type bignum-element-type last))
             (- digit-size (integer-length last) 1)))
         ;; Stores two bignums into the truncation bignum buffers,
         ;; shifting them on the way in. This assumes x and y are
         ;; positive and at least two in length, and it assumes
         ;; truncate-x and truncate-y are one digit longer than x and
         ;; y.
         (shift-and-store-truncate-buffers (x len-x y len-y shift)
           (declare (type bignum-length len-x len-y)
                    (type (integer 0 (#.digit-size)) shift))
           (cond ((zerop shift)
                  (bignum-replace truncate-x x :end1 len-x)
                  (bignum-replace truncate-y y :end1 len-y))
                 (t
                  (bignum-ashift-left-unaligned x 0 shift (1+ len-x)
                                                truncate-x)
                  (bignum-ashift-left-unaligned y 0 shift (1+ len-y)
                                                truncate-y))))) ;; LABELS
      ;; Divide X by Y returning the quotient and remainder. In the
      ;; general case, we shift Y to set up for the algorithm, and we
      ;; use two buffers to save consing intermediate values. X gets
      ;; destructively modified to become the remainder, and we have
      ;; to shift it to account for the initial Y shift. After we
      ;; multiple bind q and r, we first fix up the signs and then
      ;; return the normalized results.
      (let* ((x-plusp (bignum-plus-p x))
             (y-plusp (bignum-plus-p y))
             (x (if x-plusp x (negate-bignum-not-fully-normalized x)))
             (y (if y-plusp y (negate-bignum-not-fully-normalized y)))
             (len-x (%bignum-length x))
             (len-y (%bignum-length y)))
        (multiple-value-bind (q r)
            (cond ((plusp (bignum-compare y x))
                   (let ((res (%allocate-bignum len-x)))
                     (dotimes (i len-x)
                       (setf (%bignum-ref res i) (%bignum-ref x i)))
                     (values 0 res)))
                  (t
                   (let ((len-x+1 (1+ len-x)))
                     ;; TODO: someone who understands this algorithm could probably figure out
                     ;; whether these allocations can prezero fewer words.
                     (setf truncate-x (alloc-zeroing len-x+1))
                     (setf truncate-y (alloc-zeroing (1+ len-y)))
                     (let ((y-shift (shift-y-for-truncate y)))
                       (shift-and-store-truncate-buffers x len-x y
                                                         len-y y-shift)
                       (values (return-quotient-leaving-remainder len-x+1
                                                                  len-y)
                               ;; Now that RETURN-QUOTIENT-LEAVING-REMAINDER
                               ;; has executed, we just tidy up the remainder
                               ;; (in TRUNCATE-X) and return it.
                               (cond
                                 ((zerop y-shift)
                                  (let ((res (%allocate-bignum len-y)))
                                    (declare (type bignum res))
                                    (bignum-replace res truncate-x :end2 len-y)
                                    (%normalize-bignum res len-y)))
                                 (t
                                  (shift-right-unaligned
                                   truncate-x 0 y-shift len-y
                                   ((= j res-len-1)
                                    (setf (%bignum-ref res j)
                                          (%ashr (%bignum-ref truncate-x i)
                                                 y-shift))
                                    (%normalize-bignum res res-len))
                                   res))))))))
          (let ((quotient (cond ((eq x-plusp y-plusp) q)
                                ((typep q 'fixnum) (the fixnum (- q)))
                                (t (negate-bignum-in-place q))))
                (rem (cond (x-plusp r)
                           ((typep r 'fixnum) (the fixnum (- r)))
                           (t (negate-bignum-in-place r)))))
            (values (if (typep quotient 'fixnum)
                        quotient
                        (%normalize-bignum quotient (%bignum-length quotient)))
                    (if (typep rem 'fixnum)
                        rem
                        (%normalize-bignum rem (%bignum-length rem))))))))))

;;; Divide X by Y when Y fits in a word.
(defun bignum-truncate-single-digit (x y)
  (declare (type bignum x)
           (type (or word sb-vm:signed-word) y)
           (optimize (safety 0)))
  (declare (muffle-conditions compiler-note)) ; returns lispobj, so what.
  (labels
      ((bignum-truncate-single-digit (x len-x y)
         (declare (type bignum-length len-x)
                  (type word y))
         (if (not (logtest y (1- y)))
             ;; Y is a power of two.
             ;; SHIFT-RIGHT-UNALIGNED won't do the right thing
             ;; with a shift count of 0 or -1, so special case this.
             (cond ((= y 0)
                    (error 'division-by-zero :operation 'truncate
                                             :operands (list x y)))
                   ((= y 1)
                    ;; We could probably get away with (VALUES X 0)
                    ;; here, but it's not clear that some of the
                    ;; normalization logic further down would avoid
                    ;; mutilating X.  Just go ahead and cons, consing's
                    ;; cheap.
                    (values (copy-bignum x len-x) 0))
                   (t
                    (let ((n-bits (1- (integer-length y))))
                      (values
                       (shift-right-unaligned x 0 n-bits len-x
                                              ((= j res-len-1)
                                               (setf (%bignum-ref res j)
                                                     (%ashr (%bignum-ref x i) n-bits))
                                               res)
                                              res)
                       (logand (%bignum-ref x 0) (1- y))))))
             (cond ((sb-c::when-vop-existsp (:translate %half-bigfloor)
                      (typep y 'half-bignum-element-type))
                    (do ((i (1- (* len-x 2)) (1- i))
                         (q (%allocate-bignum len-x))
                         (r 0))
                        ((minusp i)
                         (values q r))
                      (declare (type half-bignum-element-type r))
                      (multiple-value-bind (q-digit r-digit)
                          (%half-bigfloor r (%half-bignum-ref x i) y)
                        (declare (type half-bignum-element-type q-digit r-digit))
                        (setf (%half-bignum-ref q i) q-digit)
                        (setf r r-digit))))
                   (t
                    (do ((i (1- len-x) (1- i))
                         (q (%allocate-bignum len-x))
                         (r 0))
                        ((minusp i)
                         (values q r))
                      (declare (type bignum-element-type r))
                      (multiple-value-bind (q-digit r-digit)
                          (%bigfloor r (%bignum-ref x i) y)
                        (declare (type bignum-element-type q-digit r-digit))
                        (setf (%bignum-ref q i) q-digit)
                        (setf r r-digit))))))))
    (let* ((x-plusp (bignum-plus-p x))
           (y-plusp t)
           (x (if x-plusp x (negate-bignum-not-fully-normalized x)))
           (y (typecase y
                (fixnum
                 (cond ((minusp y)
                        (setf y-plusp nil)
                        (- y))
                       (t
                        y)))
                (sb-vm:signed-word
                 (cond ((minusp y)
                        (setf y-plusp nil)
                        (the word (- y)))
                       (t
                        y)))
                (t
                 y)))
           (len-x (%bignum-length x)))
      (declare (word y))
      (multiple-value-bind (q r)
          (bignum-truncate-single-digit x len-x y)
        (let ((quotient (cond ((eq x-plusp y-plusp) q)
                              (t (negate-bignum-in-place q))))
              (rem (cond (x-plusp r)
                         ((typep r 'fixnum) (the fixnum (- r)))
                         (t (- r)))))
          (values (%normalize-bignum quotient (%bignum-length quotient))
                  rem))))))

(macrolet ((def (type)
             (let ((decode (package-symbolicate "SB-KERNEL" 'integer-decode- type)))
               `(defun ,(symbolicate 'unary-truncate- type '-to-bignum) (number)
                  (declare (inline ,decode))
                  (multiple-value-bind (bits exp sign) (,decode number)
                    (let ((truncated ,(case type
                                        #-64-bit
                                        (double-float
                                         ;; Shifting negatives right is different
                                         `(let ((truncated (ash bits exp)))
                                            (if (minusp sign)
                                                (- truncated)
                                                truncated)))
                                        (t
                                         `(bignum-ashift-left-fixnum (if (minusp sign)
                                                                         (- bits)
                                                                         bits)
                                                                     exp)))))
                      (values
                       truncated
                       ,(case type
                          ((single-float #+64-bit double-float)
                           `(coerce 0 ',type))
                          (t
                           `(- number (coerce truncated ',type)))))))))))
  (def double-float)
  (def single-float))

(macrolet ((def (type)
             (let ((decode (package-symbolicate "SB-KERNEL" 'integer-decode- type)))
               `(defun ,(symbolicate '%unary-truncate- type '-to-bignum) (number)
                  (declare (inline ,decode))
                  (multiple-value-bind (bits exp sign) (,decode number)
                    ,(case type
                       #-64-bit
                       (double-float
                        ;; Shifting negatives right is different
                        `(let ((truncated (ash bits exp)))
                           (if (minusp sign)
                               (- truncated)
                               truncated)))
                       (t
                        `(bignum-ashift-left-fixnum (if (minusp sign)
                                                        (- bits)
                                                        bits)
                                                    exp))))))))
  (def double-float)
  (def single-float))


;;;; hashing

;;; Needs to be synchronized with sxhash-bignum
(macrolet ((def (type)
             (let ((decode (package-symbolicate "SB-KERNEL" 'integer-decode- type)))
               `(defun ,(symbolicate 'sxhash-bignum- type) (number)
                  #-sb-xc-host
                  (declare (inline ,decode))
                  (let ((result 316495330))
                    (declare (type fixnum result))
                    (multiple-value-bind (bits exp sign) (,decode number)
                      (let ((bits (if (minusp sign)
                                      (- bits)
                                      bits)))
                        (multiple-value-bind (digits remaining) (truncate exp digit-size)
                          (dotimes (i digits)
                            do (mixf result 0))
                          ;; Taken from bignum-ashift-left-fixnum.
                          (let* ((right-half (ldb (byte digit-size 0)
                                                  (ash bits remaining)))
                                 (sign-bit-p
                                   (logbitp (1- digit-size) right-half))
                                 (left-half (ash bits
                                                 (- remaining digit-size)))
                                 (left-half-p (if sign-bit-p
                                                  (/= left-half -1)
                                                  (/= left-half 0))))
                            (mixf result
                                  (logand most-positive-fixnum
                                          (logxor right-half
                                                  (ash right-half -7))))
                            (when left-half-p
                              (let ((left-half (ldb (byte digit-size 0) left-half)))
                                (mixf result
                                      (logand most-positive-fixnum
                                              (logxor left-half
                                                      (ash left-half -7))))))))))
                    result)))))
  (def double-float)
  (def single-float))

;;; the bignum case of the SXHASH function
;;; Needs to be synchronized with sxhash-bignum-double-float
(defun sxhash-bignum (x)
  (let ((result 316495330))
    (declare (type fixnum result))
    (dotimes (i (%bignum-length x))
      (declare (type index i))
      (let ((xi (%bignum-ref x i)))
        (mixf result
              (logand most-positive-fixnum
                      (logxor xi
                              (ash xi -7))))))
    result))

;;; NEGATE-BIGNUM-BUFFER-IN-PLACE has an inline expansion that is not expected
;;; to be used post-build. It references the BIGNUM-NEGATE-LOOP macro,
;;; which had been deliberately excluded from the target due to a surrounding
;;; (EVAL-WHEN (:COMPILE-TOPLEVEL :EXECUTE) ...) form.
(let ((s 'negate-bignum-buffer-in-place))
  (clear-info :function :inlining-data s)
  (clear-info :function :inlinep s)
  (clear-info :source-location :declaration s))

#|
(let (code-components)
  (do-symbols (s 'sb-bignum)
    (when (and (fboundp s)
               (eq (symbol-package s) (find-package "SB-BIGNUM")))
      (pushnew (sb-kernel:fun-code-header (symbol-function s)) code-components)))
  (let ((tot-size 0) (tot-consts 0))
    (dolist (code code-components)
      (let ((nconsts (sb-kernel:code-header-words code))
            (size (sb-ext:primitive-object-size code)))
        (incf tot-size size)
        (incf tot-consts nconsts)
        (format t "~5d ~3d ~a~%" size nconsts code)))
    (format t "~5d ~3d~%" tot-size tot-consts)))
; => 32208 565 ; without block-compile
; => 23184 214 ; with block-compile in normal self-build
|#
