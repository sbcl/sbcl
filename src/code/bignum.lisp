;;;; code to implement bignum support

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!BIGNUM")

;;;; notes

;;; comments from CMU CL:
;;;   These symbols define the interface to the number code:
;;;       add-bignums multiply-bignums negate-bignum subtract-bignum
;;;       multiply-bignum-and-fixnum multiply-fixnums
;;;       bignum-ashift-right bignum-ashift-left bignum-gcd
;;;       bignum-to-float bignum-integer-length
;;;       bignum-logical-and bignum-logical-ior bignum-logical-xor
;;;       bignum-logical-not bignum-load-byte bignum-deposit-byte
;;;       bignum-truncate bignum-plus-p bignum-compare make-small-bignum
;;;       bignum-logcount
;;;   These symbols define the interface to the compiler:
;;;       bignum-type bignum-element-type bignum-index %allocate-bignum
;;;       %bignum-length %bignum-set-length %bignum-ref %bignum-set
;;;       %digit-0-or-plusp %add-with-carry %subtract-with-borrow
;;;       %multiply-and-add %multiply %lognot %logand %logior %logxor
;;;       %fixnum-to-digit %floor %fixnum-digit-with-correct-sign %ashl
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
;;;	  %ASHR
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
;;;    GCD/Relational operators:
;;;       %DIGIT-COMPARE
;;;       %DIGIT-GREATER
;;;    Relational operators:
;;;       %LOGAND
;;;       %LOGIOR
;;;       %LOGXOR
;;;    LDB
;;;       %FIXNUM-TO-DIGIT
;;;    TRUNCATE
;;;       %FLOOR
;;;
;;; Note: The floating routines know about the float representation.
;;;
;;; PROBLEM 1:
;;; There might be a problem with various LET's and parameters that take a
;;; digit value. We need to write these so those things stay in 32-bit
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
;;;	  or write compare thing (twice).
;;;       LDB on fixnum with bignum result.
;;;       DPB on fixnum with bignum result.
;;;       TRUNCATE returns zero or one as one value and fixnum or minus fixnum
;;;	  for the other value when given (truncate fixnum bignum).
;;;	  Returns (truncate bignum fixnum) otherwise.
;;;       addition
;;;       subtraction (twice)
;;;       multiply
;;;       GCD
;;;    Write MASK-FIELD and DEPOSIT-FIELD in terms of logical operations.
;;;    DIVIDE
;;;       IF (/ x y) with bignums:
;;;	  do the truncate, and if rem is 0, return quotient.
;;;	  if rem is non-0
;;;	     gcd of x and y.
;;;	     "truncate" each by gcd, ignoring remainder 0.
;;;	     form ratio of each result, bottom is positive.

;;;; What's a bignum?

(defconstant digit-size 32)

(defconstant maximum-bignum-length (1- (ash 1 (- 32 sb!vm:n-widetag-bits))))

;;;; internal inline routines

;;; %ALLOCATE-BIGNUM must zero all elements.
(defun %allocate-bignum (length)
  (declare (type bignum-index length))
  (%allocate-bignum length))

;;; Extract the length of the bignum.
(defun %bignum-length (bignum)
  (declare (type bignum-type bignum))
  (%bignum-length bignum))

;;; %BIGNUM-REF needs to access bignums as obviously as possible, and it needs
;;; to be able to return 32 bits somewhere no one looks for real objects.
(defun %bignum-ref (bignum i)
  (declare (type bignum-type bignum)
	   (type bignum-index i))
  (%bignum-ref bignum i))
(defun %bignum-set (bignum i value)
  (declare (type bignum-type bignum)
	   (type bignum-index i)
	   (type bignum-element-type value))
  (%bignum-set bignum i value))

;;; Return T if digit is positive, or NIL if negative.
(defun %digit-0-or-plusp (digit)
  (declare (type bignum-element-type digit))
  (not (logbitp (1- digit-size) digit)))

#!-sb-fluid (declaim (inline %bignum-0-or-plusp))
(defun %bignum-0-or-plusp (bignum len)
  (declare (type bignum-type bignum)
	   (type bignum-index len))
  (%digit-0-or-plusp (%bignum-ref bignum (1- len))))

;;; This should be in assembler, and should not cons intermediate results. It
;;; returns a 32bit digit and a carry resulting from adding together a, b, and
;;; an incoming carry.
(defun %add-with-carry (a b carry)
  (declare (type bignum-element-type a b)
	   (type (mod 2) carry))
  (%add-with-carry a b carry))

;;; This should be in assembler, and should not cons intermediate results. It
;;; returns a 32bit digit and a borrow resulting from subtracting b from a, and
;;; subtracting a possible incoming borrow.
;;;
;;; We really do:  a - b - 1 + borrow, where borrow is either 0 or 1.
(defun %subtract-with-borrow (a b borrow)
  (declare (type bignum-element-type a b)
	   (type (mod 2) borrow))
  (%subtract-with-borrow a b borrow))

;;; Multiply two digit-size (32-bit) numbers, returning a 64-bit result
;;; split into two 32-bit quantities.
(defun %multiply (x y)
  (declare (type bignum-element-type x y))
  (%multiply x y))

;;; This multiplies x-digit and y-digit, producing high and low digits
;;; manifesting the result. Then it adds the low digit, res-digit, and
;;; carry-in-digit. Any carries (note, you still have to add two digits at a
;;; time possibly producing two carries) from adding these three digits get
;;; added to the high digit from the multiply, producing the next carry digit.
;;; Res-digit is optional since two uses of this primitive multiplies a single
;;; digit bignum by a multiple digit bignum, and in this situation there is no
;;; need for a result buffer accumulating partial results which is where the
;;; res-digit comes from.
(defun %multiply-and-add (x-digit y-digit carry-in-digit
			  &optional (res-digit 0))
  (declare (type bignum-element-type x-digit y-digit res-digit carry-in-digit))
  (%multiply-and-add x-digit y-digit carry-in-digit res-digit))

(defun %lognot (digit)
  (declare (type bignum-element-type digit))
  (%lognot digit))

;;; Each of these does the 32-bit unsigned op.
#!-sb-fluid (declaim (inline %logand %logior %logxor))
(defun %logand (a b)
  (declare (type bignum-element-type a b))
  (logand a b))
(defun %logior (a b)
  (declare (type bignum-element-type a b))
  (logior a b))
(defun %logxor (a b)
  (declare (type bignum-element-type a b))
  (logxor a b))

;;; This takes a fixnum and sets it up as an unsigned 32-bit quantity. In
;;; the new system this will mean shifting it right two bits.
(defun %fixnum-to-digit (x)
  (declare (fixnum x))
  (logand x (1- (ash 1 digit-size))))

#!-32x16-divide
;;; This takes three digits and returns the FLOOR'ed result of
;;; dividing the first two as a 64-bit integer by the third.
;;;
;;; Do weird LET and SETQ stuff to bamboozle the compiler into allowing
;;; the %FLOOR transform to expand into pseudo-assembler for which the
;;; compiler can later correctly allocate registers.
(defun %floor (a b c)
  (let ((a a) (b b) (c c))
    (declare (type bignum-element-type a b c))
    (setq a a b b c c)
    (%floor a b c)))

;;; Convert the digit to a regular integer assuming that the digit is signed.
(defun %fixnum-digit-with-correct-sign (digit)
  (declare (type bignum-element-type digit))
  (if (logbitp (1- digit-size) digit)
      (logior digit (ash -1 digit-size))
      digit))

;;; Do an arithmetic shift right of data even though bignum-element-type is
;;; unsigned.
(defun %ashr (data count)
  (declare (type bignum-element-type data)
	   (type (mod 32) count))
  (%ashr data count))

;;; This takes a 32-bit quantity and shifts it to the left, returning a 32-bit
;;; quantity.
(defun %ashl (data count)
  (declare (type bignum-element-type data)
	   (type (mod 32) count))
  (%ashl data count))

;;; Do an unsigned (logical) right shift of a digit by Count.
(defun %digit-logical-shift-right (data count)
  (declare (type bignum-element-type data)
	   (type (mod 32) count))
  (%digit-logical-shift-right data count))

;;; Change the length of bignum to be newlen. Newlen must be the same or
;;; smaller than the old length, and any elements beyond newlen must be zeroed.
(defun %bignum-set-length (bignum newlen)
  (declare (type bignum-type bignum)
	   (type bignum-index newlen))
  (%bignum-set-length bignum newlen))

;;; This returns 0 or "-1" depending on whether the bignum is positive. This
;;; is suitable for infinite sign extension to complete additions,
;;; subtractions, negations, etc. This cannot return a -1 represented as
;;; a negative fixnum since it would then have to low zeros.
#!-sb-fluid (declaim (inline %sign-digit))
(defun %sign-digit (bignum len)
  (declare (type bignum-type bignum)
	   (type bignum-index len))
  (%ashr (%bignum-ref bignum (1- len)) (1- digit-size)))

;;; These take two 32 bit quantities and compare or contrast them without
;;; wasting time with incorrect type checking.
#!-sb-fluid (declaim (inline %digit-compare %digit-greater))
(defun %digit-compare (x y)
  (= x y))
(defun %digit-greater (x y)
  (> x y))

(declaim (optimize (speed 3) (safety 0)))

;;;; addition

(defun add-bignums (a b)
  (declare (type bignum-type a b))
  (let ((len-a (%bignum-length a))
	(len-b (%bignum-length b)))
    (declare (type bignum-index len-a len-b))
    (multiple-value-bind (a len-a b len-b)
	(if (> len-a len-b)
	    (values a len-a b len-b)
	    (values b len-b a len-a))
      (declare (type bignum-type a b)
	       (type bignum-index len-a len-b))
      (let* ((len-res (1+ len-a))
	     (res (%allocate-bignum len-res))
	     (carry 0))
	(declare (type bignum-index len-res)
		 (type bignum-type res)
		 (type (mod 2) carry))
	(dotimes (i len-b)
	  (declare (type bignum-index i))
	  (multiple-value-bind (v k)
	      (%add-with-carry (%bignum-ref a i) (%bignum-ref b i) carry)
	    (declare (type bignum-element-type v)
		     (type (mod 2) k))
	    (setf (%bignum-ref res i) v)
	    (setf carry k)))
	(if (/= len-a len-b)
	    (finish-add a res carry (%sign-digit b len-b) len-b len-a)
	    (setf (%bignum-ref res len-a)
		  (%add-with-carry (%sign-digit a len-a)
				   (%sign-digit b len-b)
				   carry)))
	(%normalize-bignum res len-res)))))

;;; This takes the longer of two bignums and propagates the carry through its
;;; remaining high order digits.
(defun finish-add (a res carry sign-digit-b start end)
  (declare (type bignum-type a res)
	   (type (mod 2) carry)
	   (type bignum-element-type sign-digit-b)
	   (type bignum-index start end))
  (do ((i start (1+ i)))
      ((= i end)
       (setf (%bignum-ref res end)
	     (%add-with-carry (%sign-digit a end) sign-digit-b carry)))
    (declare (type bignum-index i))
    (multiple-value-bind (v k)
	(%add-with-carry (%bignum-ref a i) sign-digit-b carry)
      (setf (%bignum-ref res i) v)
      (setf carry k)))
  (values))

;;;; subtraction

(eval-when (:compile-toplevel :execute)

;;; This subtracts b from a plugging result into res. Return-fun is the
;;; function to call that fixes up the result returning any useful values, such
;;; as the result. This macro may evaluate its arguments more than once.
(sb!xc:defmacro subtract-bignum-loop (a len-a b len-b res len-res return-fun)
  (let ((borrow (gensym))
	(a-digit (gensym))
	(a-sign (gensym))
	(b-digit (gensym))
	(b-sign (gensym))
	(i (gensym))
	(v (gensym))
	(k (gensym)))
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

) ;EVAL-WHEN

(defun subtract-bignum (a b)
  (declare (type bignum-type a b))
  (let* ((len-a (%bignum-length a))
	 (len-b (%bignum-length b))
	 (len-res (1+ (max len-a len-b)))
	 (res (%allocate-bignum len-res)))
    (declare (type bignum-index len-a len-b len-res)) ;Test len-res for bounds?
    (subtract-bignum-loop a len-a b len-b res len-res %normalize-bignum)))

;;; Operations requiring a subtraction without the overhead of intermediate
;;; results, such as GCD, use this. It assumes Result is big enough for the
;;; result.
(defun subtract-bignum-buffers (a len-a b len-b result)
  (declare (type bignum-type a b)
	   (type bignum-index len-a len-b))
  (let ((len-res (max len-a len-b)))
    (subtract-bignum-loop a len-a b len-b result len-res
			  %normalize-bignum-buffer)))

;;;; multiplication

(defun multiply-bignums (a b)
  (declare (type bignum-type a b))
  (let* ((a-plusp (%bignum-0-or-plusp a (%bignum-length a)))
	 (b-plusp (%bignum-0-or-plusp b (%bignum-length b)))
	 (a (if a-plusp a (negate-bignum a)))
	 (b (if b-plusp b (negate-bignum b)))
	 (len-a (%bignum-length a))
	 (len-b (%bignum-length b))
	 (len-res (+ len-a len-b))
	 (res (%allocate-bignum len-res))
	 (negate-res (not (eq a-plusp b-plusp))))
    (declare (type bignum-index len-a len-b len-res))
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
  (declare (type bignum-type bignum) (type fixnum fixnum))
  (let* ((bignum-plus-p (%bignum-0-or-plusp bignum (%bignum-length bignum)))
	 (fixnum-plus-p (not (minusp fixnum)))
	 (bignum (if bignum-plus-p bignum (negate-bignum bignum)))
	 (bignum-len (%bignum-length bignum))
	 (fixnum (if fixnum-plus-p fixnum (- fixnum)))
	 (result (%allocate-bignum (1+ bignum-len)))
	 (carry-digit 0))
    (declare (type bignum-type bignum result)
	     (type bignum-index bignum-len)
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

(defun multiply-fixnums (a b)
  (declare (fixnum a b))
  (let* ((a-minusp (minusp a))
	 (b-minusp (minusp b)))
    (multiple-value-bind (high low)
	(%multiply (if a-minusp (- a) a)
		   (if b-minusp (- b) b))
      (declare (type bignum-element-type high low))
      (if (and (zerop high)
	       (%digit-0-or-plusp low))
	  (let ((low (sb!ext:truly-the (unsigned-byte 31)
				       (%fixnum-digit-with-correct-sign low))))
	    (if (eq a-minusp b-minusp)
		low
		(- low)))
	  (let ((res (%allocate-bignum 2)))
	    (%bignum-set res 0 low)
	    (%bignum-set res 1 high)
	    (unless (eq a-minusp b-minusp) (negate-bignum-in-place res))
	    (%normalize-bignum res 2))))))

;;;; BIGNUM-REPLACE and WITH-BIGNUM-BUFFERS

(eval-when (:compile-toplevel :execute)

(sb!xc:defmacro bignum-replace (dest
				src
				&key
				(start1 '0)
				end1
				(start2 '0)
				end2
				from-end)
  (sb!int:once-only ((n-dest dest)
		     (n-src src))
    (let ((n-start1 (gensym))
	  (n-end1 (gensym))
	  (n-start2 (gensym))
	  (n-end2 (gensym))
	  (i1 (gensym))
	  (i2 (gensym))
	  (end1 (or end1 `(%bignum-length ,n-dest)))
	  (end2 (or end2 `(%bignum-length ,n-src))))
      (if from-end
	  `(let ((,n-start1 ,start1)
		 (,n-start2 ,start2))
	     (do ((,i1 (1- ,end1) (1- ,i1))
		  (,i2 (1- ,end2) (1- ,i2)))
		 ((or (< ,i1 ,n-start1) (< ,i2 ,n-start2)))
	       (declare (fixnum ,i1 ,i2))
	       (%bignum-set ,n-dest ,i1
			    (%bignum-ref ,n-src ,i2))))
	  `(let ((,n-end1 ,end1)
		 (,n-end2 ,end2))
	     (do ((,i1 ,start1 (1+ ,i1))
		  (,i2 ,start2 (1+ ,i2)))
		 ((or (>= ,i1 ,n-end1) (>= ,i2 ,n-end2)))
	       (declare (type bignum-index ,i1 ,i2))
	       (%bignum-set ,n-dest ,i1
			    (%bignum-ref ,n-src ,i2))))))))

(sb!xc:defmacro with-bignum-buffers (specs &body body)
  #!+sb-doc
  "WITH-BIGNUM-BUFFERS ({(var size [init])}*) Form*"
  (sb!int:collect ((binds)
		   (inits))
    (dolist (spec specs)
      (let ((name (first spec))
	    (size (second spec)))
	(binds `(,name (%allocate-bignum ,size)))
	(let ((init (third spec)))
	  (when init
	    (inits `(bignum-replace ,name ,init))))))
    `(let* ,(binds)
       ,@(inits)
       ,@body)))

) ;EVAL-WHEN

;;;; GCD

(defun bignum-gcd (a b)
  (declare (type bignum-type a b))
  (let* ((a (if (%bignum-0-or-plusp a (%bignum-length a))
		a
		(negate-bignum a nil)))
	 (b (if (%bignum-0-or-plusp b (%bignum-length b))
		b
		(negate-bignum b nil)))
	 (len-a (%bignum-length a))
	 (len-b (%bignum-length b)))
      (declare (type bignum-index len-a len-b))
    (with-bignum-buffers ((a-buffer len-a a)
			  (b-buffer len-b b)
			  (res-buffer (max len-a len-b)))
      (let* ((factors-of-two
	      (bignum-factors-of-two a-buffer len-a
				     b-buffer len-b))
	     (len-a (make-gcd-bignum-odd
		     a-buffer
		     (bignum-buffer-ashift-right a-buffer len-a
						 factors-of-two)))
	     (len-b (make-gcd-bignum-odd
		     b-buffer
		     (bignum-buffer-ashift-right b-buffer len-b
						 factors-of-two))))
	(declare (type bignum-index len-a len-b))
	(let ((x a-buffer)
	      (len-x len-a)
	      (y b-buffer)
	      (len-y len-b)
	      (z res-buffer))
	  (loop
	    (multiple-value-bind (u v len-v r len-r)
		(bignum-gcd-order-and-subtract x len-x y len-y z)
	      (declare (type bignum-index len-v len-r))
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
  (declare (type bignum-index len-a len-b) (type bignum-type a b))
  (cond ((= len-a len-b)
	 (do ((i (1- len-a) (1- i)))
	     ((= i -1)
	      (setf (%bignum-ref res 0) 0)
	      (values a b len-b res 1))
	   (let ((a-digit (%bignum-ref a i))
		 (b-digit (%bignum-ref b i)))
	     (cond ((%digit-compare a-digit b-digit))
		   ((%digit-greater a-digit b-digit)
		    (return
		     (values a b len-b res
			     (subtract-bignum-buffers a len-a b len-b res))))
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
  (declare (type bignum-type a) (type bignum-index len-a))
  (dotimes (index len-a)
    (declare (type bignum-index index))
    (do ((digit (%bignum-ref a index) (%ashr digit 1))
	 (increment 0 (1+ increment)))
	((zerop digit))
      (declare (type (mod 32) increment))
      (when (oddp digit)
	(return-from make-gcd-bignum-odd
		     (bignum-buffer-ashift-right a len-a
						 (+ (* index digit-size)
						    increment)))))))

(defun bignum-factors-of-two (a len-a b len-b)
  (declare (type bignum-index len-a len-b) (type bignum-type a))
  (do ((i 0 (1+ i))
       (end (min len-a len-b)))
      ((= i end) (error "Unexpected zero bignums?"))
    (declare (type bignum-index i end))
    (let ((or-digits (%logior (%bignum-ref a i) (%bignum-ref b i))))
      (unless (zerop or-digits)
	(return (do ((j 0 (1+ j))
		     (or-digits or-digits (%ashr or-digits 1)))
		    ((oddp or-digits) (+ (* i digit-size) j))
		  (declare (type (mod 32) j))))))))

;;;; negation

(eval-when (:compile-toplevel :execute)

;;; This negates bignum-len digits of bignum, storing the resulting digits into
;;; result (possibly EQ to bignum) and returning whatever end-carry there is.
(sb!xc:defmacro bignum-negate-loop (bignum
				    bignum-len
				    &optional (result nil resultp))
  (let ((carry (gensym))
	(end (gensym))
	(value (gensym))
	(last (gensym)))
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
		(type bignum-index i ,end))
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

) ; EVAL-WHEN

;;; Fully-normalize is an internal optional. It cause this to always return
;;; a bignum, without any extraneous digits, and it never returns a fixnum.
(defun negate-bignum (x &optional (fully-normalize t))
  (declare (type bignum-type x))
  (let* ((len-x (%bignum-length x))
	 (len-res (1+ len-x))
	 (res (%allocate-bignum len-res)))
    (declare (type bignum-index len-x len-res)) ;Test len-res for range?
    (let ((carry (bignum-negate-loop x len-x res)))
      (setf (%bignum-ref res len-x)
	    (%add-with-carry (%lognot (%sign-digit x len-x)) 0 carry)))
    (if fully-normalize
	(%normalize-bignum res len-res)
	(%mostly-normalize-bignum res len-res))))

;;; This assumes bignum is positive; that is, the result of negating it will
;;; stay in the provided allocated bignum.
(defun negate-bignum-in-place (bignum)
  (bignum-negate-loop bignum (%bignum-length bignum) bignum)
  bignum)

;;;; shifting

(defconstant all-ones-digit #xFFFFFFFF)

(eval-when (:compile-toplevel :execute)

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
(sb!xc:defmacro shift-right-unaligned (source
				       start-digit
				       start-pos
				       res-len-form
				       termination
				       &optional result)
  `(let* ((high-bits-in-first-digit (- digit-size ,start-pos))
	  (res-len ,res-len-form)
	  (res-len-1 (1- res-len))
	  ,@(if result `((,result (%allocate-bignum res-len)))))
     (declare (type bignum-index res-len res-len-1))
     (do ((i ,start-digit i+1)
	  (i+1 (1+ ,start-digit) (1+ i+1))
	  (j 0 (1+ j)))
	 ,termination
       (declare (type bignum-index i i+1 j))
       (setf (%bignum-ref ,(if result result source) j)
	     (%logior (%digit-logical-shift-right (%bignum-ref ,source i)
						  ,start-pos)
		      (%ashl (%bignum-ref ,source i+1)
			     high-bits-in-first-digit))))))

) ; EVAL-WHEN

;;; First compute the number of whole digits to shift, shifting them by
;;; skipping them when we start to pick up bits, and the number of bits to
;;; shift the remaining digits into place. If the number of digits is greater
;;; than the length of the bignum, then the result is either 0 or -1. If we
;;; shift on a digit boundary (that is, n-bits is zero), then we just copy
;;; digits. The last branch handles the general case which uses a macro that a
;;; couple other routines use. The fifth argument to the macro references
;;; locals established by the macro.
(defun bignum-ashift-right (bignum count)
  (declare (type bignum-type bignum)
	   (type unsigned-byte count))
  (let ((bignum-len (%bignum-length bignum)))
    (declare (type bignum-index bignum-len))
    (cond ((fixnump count)
	   (multiple-value-bind (digits n-bits) (truncate count digit-size)
	     (declare (type bignum-index digits))
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
	   0)
	   ;; Since a FIXNUM should be big enough to address anything in
	   ;; memory, including arrays of bits, and since arrays of bits
	   ;; take up about the same space as corresponding fixnums, there
	   ;; should be no way that we fall through to this case: any shift
	   ;; right by a bignum should give zero. But let's check anyway:
	  (t (error "bignum overflow: can't shift right by ~S")))))

(defun bignum-ashift-right-digits (bignum digits)
  (declare (type bignum-type bignum)
	   (type bignum-index digits))
  (let* ((res-len (- (%bignum-length bignum) digits))
	 (res (%allocate-bignum res-len)))
    (declare (type bignum-index res-len)
	     (type bignum-type res))
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
  (declare (type bignum-index bignum-len) (fixnum x))
  (multiple-value-bind (digits n-bits) (truncate x digit-size)
    (declare (type bignum-index digits))
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
  (declare (type bignum-type bignum)
	   (type unsigned-byte x)
	   (type (or null bignum-index) bignum-len))
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
    ;; index any array, including a bit array.
    (error "can't represent result of left shift")))

(defun bignum-ashift-left-digits (bignum bignum-len digits)
  (declare (type bignum-index bignum-len digits))
  (let* ((res-len (+ bignum-len digits))
	 (res (%allocate-bignum res-len)))
    (declare (type bignum-index res-len))
    (bignum-replace res bignum :start1 digits :end1 res-len :end2 bignum-len
		    :from-end t)
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
  (declare (type bignum-index digits res-len)
	   (type (mod #.digit-size) n-bits))
  (let* ((remaining-bits (- digit-size n-bits))
	 (res-len-1 (1- res-len))
	 (res (or res (%allocate-bignum res-len))))
    (declare (type bignum-index res-len res-len-1))
    (do ((i 0 i+1)
	 (i+1 1 (1+ i+1))
	 (j (1+ digits) (1+ j)))
	((= j res-len-1)
	 (setf (%bignum-ref res digits)
	       (%ashl (%bignum-ref bignum 0) n-bits))
	 (setf (%bignum-ref res j)
	       (%ashr (%bignum-ref bignum i) remaining-bits))
	 (if resp
	     (%normalize-bignum-buffer res res-len)
	     (%normalize-bignum res res-len)))
      (declare (type bignum-index i i+1 j))
      (setf (%bignum-ref res j)
	    (%logior (%digit-logical-shift-right (%bignum-ref bignum i)
						 remaining-bits)
		     (%ashl (%bignum-ref bignum i+1) n-bits))))))

;;;; relational operators

;;; Return T iff bignum is positive.
(defun bignum-plus-p (bignum)
  (declare (type bignum-type bignum))
  (%bignum-0-or-plusp bignum (%bignum-length bignum)))

;;; This compares two bignums returning -1, 0, or 1, depending on
;;; whether a is less than, equal to, or greater than b.
(declaim (ftype (function (bignum bignum) (integer -1 1)) bignum-compare))
(defun bignum-compare (a b)
  (declare (type bignum-type a b))
  (let* ((len-a (%bignum-length a))
	 (len-b (%bignum-length b))
	 (a-plusp (%bignum-0-or-plusp a len-a))
	 (b-plusp (%bignum-0-or-plusp b len-b)))
    (declare (type bignum-index len-a len-b))
    (cond ((not (eq a-plusp b-plusp))
	   (if a-plusp 1 -1))
	  ((= len-a len-b)
	   (do ((i (1- len-a) (1- i)))
	       (())
	     (declare (type bignum-index i))
	     (let ((a-digit (%bignum-ref a i))
		   (b-digit (%bignum-ref b i)))
	       (declare (type bignum-element-type a-digit b-digit))
	       (when (%digit-greater a-digit b-digit)
		 (return 1))
	       (when (%digit-greater b-digit a-digit)
		 (return -1)))
	     (when (zerop i) (return 0))))
	  ((> len-a len-b)
	   (if a-plusp 1 -1))
	  (t (if a-plusp -1 1)))))

;;;; float conversion

;;; Make a single or double float with the specified significand,
;;; exponent and sign.
(defun single-float-from-bits (bits exp plusp)
  (declare (fixnum exp))
  (declare (optimize #-sb-xc-host (sb!ext:inhibit-warnings 3)))
  (let ((res (dpb exp
		  sb!vm:single-float-exponent-byte
		  (logandc2 (sb!ext:truly-the (unsigned-byte 31)
					      (%bignum-ref bits 1))
			    sb!vm:single-float-hidden-bit))))
    (make-single-float
     (if plusp
	 res
	 (logior res (ash -1 sb!vm:float-sign-shift))))))
(defun double-float-from-bits (bits exp plusp)
  (declare (fixnum exp))
  (declare (optimize #-sb-xc-host (sb!ext:inhibit-warnings 3)))
  (let ((hi (dpb exp
		 sb!vm:double-float-exponent-byte
		 (logandc2 (sb!ext:truly-the (unsigned-byte 31)
					     (%bignum-ref bits 2))
			   sb!vm:double-float-hidden-bit))))
    (make-double-float
     (if plusp
	 hi
	 (logior hi (ash -1 sb!vm:float-sign-shift)))
     (%bignum-ref bits 1))))
#!+(and long-float x86)
(defun long-float-from-bits (bits exp plusp)
  (declare (fixnum exp))
  (declare (optimize #-sb-xc-host (sb!ext:inhibit-warnings 3)))
  (make-long-float
   (if plusp
       exp
       (logior exp (ash 1 15)))
   (%bignum-ref bits 2)
   (%bignum-ref bits 1)))

;;; Convert Bignum to a float in the specified Format, rounding to the best
;;; approximation.
(defun bignum-to-float (bignum format)
  (let* ((plusp (bignum-plus-p bignum))
	 (x (if plusp bignum (negate-bignum bignum)))
	 (len (bignum-integer-length x))
	 (digits (float-format-digits format))
	 (keep (+ digits digit-size))
	 (shift (- keep len))
	 (shifted (if (minusp shift)
		      (bignum-ashift-right x (- shift))
		      (bignum-ashift-left x shift)))
	 (low (%bignum-ref shifted 0))
	 (round-bit (ash 1 (1- digit-size))))
    (declare (type bignum-index len digits keep) (fixnum shift))
    (labels ((round-up ()
	       (let ((rounded (add-bignums shifted round-bit)))
		 (if (> (integer-length rounded) keep)
		     (float-from-bits (bignum-ashift-right rounded 1)
				      (1+ len))
		     (float-from-bits rounded len))))
	     (float-from-bits (bits len)
	       (declare (type bignum-index len))
	       (ecase format
		 (single-float
		  (single-float-from-bits
		   bits
		   (check-exponent len sb!vm:single-float-bias
				   sb!vm:single-float-normal-exponent-max)
		   plusp))
		 (double-float
		  (double-float-from-bits
		   bits
		   (check-exponent len sb!vm:double-float-bias
				   sb!vm:double-float-normal-exponent-max)
		   plusp))
		 #!+long-float
		 (long-float
		  (long-float-from-bits
		   bits
		   (check-exponent len sb!vm:long-float-bias
				   sb!vm:long-float-normal-exponent-max)
		   plusp))))
	     (check-exponent (exp bias max)
	       (declare (type bignum-index len))
	       (let ((exp (+ exp bias)))
		 (when (> exp max)
		   ;; Why a SIMPLE-TYPE-ERROR? Well, this is mainly
		   ;; called by COERCE, which requires an error of
		   ;; TYPE-ERROR if the conversion can't happen
		   ;; (except in certain circumstances when we are
		   ;; coercing to a FUNCTION) -- CSR, 2002-09-18
		   (error 'simple-type-error
			  :format-control "Too large to be represented as a ~S:~%  ~S"
			  :format-arguments (list format x)
			  :expected-type format
			  :datum x))
		 exp)))

    (cond
     ;; Round down if round bit is 0.
     ((zerop (logand round-bit low))
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
      (round-up))))))

;;;; integer length and logcount

(defun bignum-integer-length (bignum)
  (declare (type bignum-type bignum))
  (let* ((len (%bignum-length bignum))
	 (len-1 (1- len))
	 (digit (%bignum-ref bignum len-1)))
    (declare (type bignum-index len len-1)
	     (type bignum-element-type digit))
    (+ (integer-length (%fixnum-digit-with-correct-sign digit))
       (* len-1 digit-size))))

(defun bignum-logcount (bignum)
  (declare (type bignum-type bignum))
  (let* ((length (%bignum-length bignum))
	 (plusp (%bignum-0-or-plusp bignum length))
	 (result 0))
    (declare (type bignum-index length)
	     (fixnum result))
    (do ((index 0 (1+ index)))
	((= index length) result)
      (let ((digit (%bignum-ref bignum index)))
	(declare (type bignum-element-type digit))
	(incf result (logcount (if plusp digit (%lognot digit))))))))

;;;; logical operations

;;;; NOT

(defun bignum-logical-not (a)
  (declare (type bignum-type a))
  (let* ((len (%bignum-length a))
	 (res (%allocate-bignum len)))
    (declare (type bignum-index len))
    (dotimes (i len res)
      (declare (type bignum-index i))
      (setf (%bignum-ref res i) (%lognot (%bignum-ref a i))))))

;;;; AND

(defun bignum-logical-and (a b)
  (declare (type bignum-type a b))
  (let* ((len-a (%bignum-length a))
	 (len-b (%bignum-length b))
	 (a-plusp (%bignum-0-or-plusp a len-a))
	 (b-plusp (%bignum-0-or-plusp b len-b)))
    (declare (type bignum-index len-a len-b))
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
  (declare (type bignum-type a b res)
	   (type bignum-index len-a))
  (dotimes (i len-a)
    (declare (type bignum-index i))
    (setf (%bignum-ref res i)
	  (%logand (%bignum-ref a i) (%bignum-ref b i))))
  (%normalize-bignum res len-a))

;;; This takes a shorter bignum, a and len-a, that is negative. Because this
;;; is AND, we just copy any bits longer than a's since its infinite 1 sign
;;; bits will include any bits from b. The result is len-b big.
(defun logand-shorter-negative (a len-a b len-b res)
  (declare (type bignum-type a b res)
	   (type bignum-index len-a len-b))
  (dotimes (i len-a)
    (declare (type bignum-index i))
    (setf (%bignum-ref res i)
	  (%logand (%bignum-ref a i) (%bignum-ref b i))))
  (do ((i len-a (1+ i)))
      ((= i len-b))
    (declare (type bignum-index i))
    (setf (%bignum-ref res i) (%bignum-ref b i)))
  (%normalize-bignum res len-b))

;;;; IOR

(defun bignum-logical-ior (a b)
  (declare (type bignum-type a b))
  (let* ((len-a (%bignum-length a))
	 (len-b (%bignum-length b))
	 (a-plusp (%bignum-0-or-plusp a len-a))
	 (b-plusp (%bignum-0-or-plusp b len-b)))
    (declare (type bignum-index len-a len-b))
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
  (declare (type bignum-type a b res)
	   (type bignum-index len-a len-b))
  (dotimes (i len-a)
    (declare (type bignum-index i))
    (setf (%bignum-ref res i)
	  (%logior (%bignum-ref a i) (%bignum-ref b i))))
  (do ((i len-a (1+ i)))
      ((= i len-b))
    (declare (type bignum-index i))
    (setf (%bignum-ref res i) (%bignum-ref b i)))
  (%normalize-bignum res len-b))

;;; This takes a shorter bignum, a and len-a, that is negative. Because this
;;; is IOR, we just copy any bits longer than a's since its infinite 1 sign
;;; bits will include any bits from b. The result is len-b long.
(defun logior-shorter-negative (a len-a b len-b res)
  (declare (type bignum-type a b res)
	   (type bignum-index len-a len-b))
  (dotimes (i len-a)
    (declare (type bignum-index i))
    (setf (%bignum-ref res i)
	  (%logior (%bignum-ref a i) (%bignum-ref b i))))
  (do ((i len-a (1+ i))
       (sign (%sign-digit a len-a)))
      ((= i len-b))
    (declare (type bignum-index i))
    (setf (%bignum-ref res i) sign))
  (%normalize-bignum res len-b))

;;;; XOR

(defun bignum-logical-xor (a b)
  (declare (type bignum-type a b))
  (let ((len-a (%bignum-length a))
	(len-b (%bignum-length b)))
    (declare (type bignum-index len-a len-b))
    (if (< len-a len-b)
	(bignum-logical-xor-aux a len-a b len-b (%allocate-bignum len-b))
	(bignum-logical-xor-aux b len-b a len-a (%allocate-bignum len-a)))))

;;; This takes the shorter of two bignums in a and len-a. Res is len-b
;;; long. Do the XOR.
(defun bignum-logical-xor-aux (a len-a b len-b res)
  (declare (type bignum-type a b res)
	   (type bignum-index len-a len-b))
  (dotimes (i len-a)
    (declare (type bignum-index i))
    (setf (%bignum-ref res i)
	  (%logxor (%bignum-ref a i) (%bignum-ref b i))))
  (do ((i len-a (1+ i))
       (sign (%sign-digit a len-a)))
      ((= i len-b))
    (declare (type bignum-index i))
    (setf (%bignum-ref res i) (%logxor sign (%bignum-ref b i))))
  (%normalize-bignum res len-b))

;;;; LDB (load byte)

#|
FOR NOW WE DON'T USE LDB OR DPB. WE USE SHIFTS AND MASKS IN NUMBERS.LISP WHICH
IS LESS EFFICIENT BUT EASIER TO MAINTAIN. BILL SAYS THIS CODE CERTAINLY WORKS!

(defconstant maximum-fixnum-bits #!+ibm-rt-pc 27 #!-ibm-rt-pc 30)

(defun bignum-load-byte (byte bignum)
  (declare (type bignum-type bignum))
  (let ((byte-len (byte-size byte))
	(byte-pos (byte-position byte)))
    (if (< byte-len maximum-fixnum-bits)
	(bignum-ldb-fixnum-res bignum byte-len byte-pos)
	(bignum-ldb-bignum-res bignum byte-len byte-pos))))

;;; This returns a fixnum result of loading a byte from a bignum. In order, we
;;; check for the following conditions:
;;;    Insufficient bignum digits to start loading a byte --
;;;       Return 0 or byte-len 1's depending on sign of bignum.
;;;    One bignum digit containing the whole byte spec --
;;;       Grab 'em, shift 'em, and mask out what we don't want.
;;;    Insufficient bignum digits to cover crossing a digit boundary --
;;;       Grab the available bits in the last digit, and or in whatever
;;;       virtual sign bits we need to return a full byte spec.
;;;    Else (we cross a digit boundary with all bits available) --
;;;       Make a couple masks, grab what we want, shift it around, and
;;;       LOGIOR it all together.
;;; Because (< maximum-fixnum-bits digit-size) and
;;;	 (< byte-len maximum-fixnum-bits),
;;; we only cross one digit boundary if any.
(defun bignum-ldb-fixnum-res (bignum byte-len byte-pos)
  (multiple-value-bind (skipped-digits pos) (truncate byte-pos digit-size)
    (let ((bignum-len (%bignum-length bignum))
	  (s-digits+1 (1+ skipped-digits)))
      (declare (type bignum-index bignum-len s-digits+1))
      (if (>= skipped-digits bignum-len)
	  (if (%bignum-0-or-plusp bignum bignum-len)
	      0
	      (%make-ones byte-len))
	  (let ((end (+ pos byte-len)))
	    (cond ((<= end digit-size)
		   (logand (ash (%bignum-ref bignum skipped-digits) (- pos))
			   ;; Must LOGAND after shift here.
			   (%make-ones byte-len)))
		  ((>= s-digits+1 bignum-len)
		   (let* ((available-bits (- digit-size pos))
			  (res (logand (ash (%bignum-ref bignum skipped-digits)
					    (- pos))
				       ;; LOGAND should be unnecessary here
				       ;; with a logical right shift or a
				       ;; correct unsigned-byte-32 one.
				       (%make-ones available-bits))))
		     (if (%bignum-0-or-plusp bignum bignum-len)
			 res
			 (logior (%ashl (%make-ones (- end digit-size))
					available-bits)
				 res))))
		  (t
		   (let* ((high-bits-in-first-digit (- digit-size pos))
			  (high-mask (%make-ones high-bits-in-first-digit))
			  (low-bits-in-next-digit (- end digit-size))
			  (low-mask (%make-ones low-bits-in-next-digit)))
		     (declare (type bignum-element-type high-mask low-mask))
		     (logior (%ashl (logand (%bignum-ref bignum s-digits+1)
					    low-mask)
				    high-bits-in-first-digit)
			     (logand (ash (%bignum-ref bignum skipped-digits)
					  (- pos))
				     ;; LOGAND should be unnecessary here with
				     ;; a logical right shift or a correct
				     ;; unsigned-byte-32 one.
				     high-mask))))))))))

;;; This returns a bignum result of loading a byte from a bignum. In order, we
;;; check for the following conditions:
;;;    Insufficient bignum digits to start loading a byte --
;;;    Byte-pos starting on a digit boundary --
;;;    Byte spec contained in one bignum digit --
;;;       Grab the bits we want and stick them in a single digit result.
;;;       Since we know byte-pos is non-zero here, we know our single digit
;;;       will have a zero high sign bit.
;;;    Else (unaligned multiple digits) --
;;;       This is like doing a shift right combined with either masking
;;;       out unwanted high bits from bignum or filling in virtual sign
;;;       bits if bignum had insufficient bits. We use SHIFT-RIGHT-ALIGNED
;;;       and reference lots of local variables this macro establishes.
(defun bignum-ldb-bignum-res (bignum byte-len byte-pos)
  (multiple-value-bind (skipped-digits pos) (truncate byte-pos digit-size)
    (let ((bignum-len (%bignum-length bignum)))
      (declare (type bignum-index bignum-len))
      (cond
       ((>= skipped-digits bignum-len)
	(make-bignum-virtual-ldb-bits bignum bignum-len byte-len))
       ((zerop pos)
	(make-aligned-ldb-bignum bignum bignum-len byte-len skipped-digits))
       ((< (+ pos byte-len) digit-size)
	(let ((res (%allocate-bignum 1)))
	  (setf (%bignum-ref res 0)
		(logand (%ashr (%bignum-ref bignum skipped-digits) pos)
			(%make-ones byte-len)))
	  res))
       (t
	(make-unaligned-ldb-bignum bignum bignum-len
				   byte-len skipped-digits pos))))))

;;; This returns bits from bignum that don't physically exist. These are
;;; all zero or one depending on the sign of the bignum.
(defun make-bignum-virtual-ldb-bits (bignum bignum-len byte-len)
  (if (%bignum-0-or-plusp bignum bignum-len)
      0
      (multiple-value-bind (res-len-1 extra) (truncate byte-len digit-size)
	(declare (type bignum-index res-len-1))
	(let* ((res-len (1+ res-len-1))
	       (res (%allocate-bignum res-len)))
	  (declare (type bignum-index res-len))
	  (do ((j 0 (1+ j)))
	      ((= j res-len-1)
	       (setf (%bignum-ref res j) (%make-ones extra))
	       (%normalize-bignum res res-len))
	    (declare (type bignum-index j))
	    (setf (%bignum-ref res j) all-ones-digit))))))

;;; Since we are picking up aligned digits, we just copy the whole digits
;;; we want and fill in extra bits. We might have a byte-len that extends
;;; off the end of the bignum, so we may have to fill in extra 1's if the
;;; bignum is negative.
(defun make-aligned-ldb-bignum (bignum bignum-len byte-len skipped-digits)
  (multiple-value-bind (res-len-1 extra) (truncate byte-len digit-size)
    (declare (type bignum-index res-len-1))
    (let* ((res-len (1+ res-len-1))
	   (res (%allocate-bignum res-len)))
      (declare (type bignum-index res-len))
      (do ((i skipped-digits (1+ i))
	   (j 0 (1+ j)))
	  ((or (= j res-len-1) (= i bignum-len))
	   (cond ((< i bignum-len)
		  (setf (%bignum-ref res j)
			(logand (%bignum-ref bignum i)
				(the bignum-element-type (%make-ones extra)))))
		 ((%bignum-0-or-plusp bignum bignum-len))
		 (t
		  (do ((j j (1+ j)))
		      ((= j res-len-1)
		       (setf (%bignum-ref res j) (%make-ones extra)))
		    (setf (%bignum-ref res j) all-ones-digit))))
	   (%normalize-bignum res res-len))
      (declare (type bignum-index i j))
      (setf (%bignum-ref res j) (%bignum-ref bignum i))))))

;;; This grabs unaligned bignum bits from bignum assuming byte-len causes at
;;; least one digit boundary crossing. We use SHIFT-RIGHT-UNALIGNED referencing
;;; lots of local variables established by it.
(defun make-unaligned-ldb-bignum (bignum
				  bignum-len
				  byte-len
				  skipped-digits
				  pos)
  (multiple-value-bind (res-len-1 extra) (truncate byte-len digit-size)
    (shift-right-unaligned
     bignum skipped-digits pos (1+ res-len-1)
     ((or (= j res-len-1) (= i+1 bignum-len))
      (cond ((= j res-len-1)
	     (cond
	      ((< extra high-bits-in-first-digit)
	       (setf (%bignum-ref res j)
		     (logand (ash (%bignum-ref bignum i) minus-start-pos)
			     ;; Must LOGAND after shift here.
			     (%make-ones extra))))
	      (t
	       (setf (%bignum-ref res j)
		     (logand (ash (%bignum-ref bignum i) minus-start-pos)
			     ;; LOGAND should be unnecessary here with a logical
			     ;; right shift or a correct unsigned-byte-32 one.
			     high-mask))
	       (when (%bignum-0-or-plusp bignum bignum-len)
		 (setf (%bignum-ref res j)
		       (logior (%bignum-ref res j)
			       (%ashl (%make-ones
				       (- extra high-bits-in-first-digit))
				      high-bits-in-first-digit)))))))
	    (t
	     (setf (%bignum-ref res j)
		   (logand (ash (%bignum-ref bignum i) minus-start-pos)
			   ;; LOGAND should be unnecessary here with a logical
			   ;; right shift or a correct unsigned-byte-32 one.
			   high-mask))
	     (unless (%bignum-0-or-plusp bignum bignum-len)
	       ;; Fill in upper half of this result digit with 1's.
	       (setf (%bignum-ref res j)
		     (logior (%bignum-ref res j)
			     (%ashl low-mask high-bits-in-first-digit)))
	       ;; Fill in any extra 1's we need to be byte-len long.
	       (do ((j (1+ j) (1+ j)))
		   ((>= j res-len-1)
		    (setf (%bignum-ref res j) (%make-ones extra)))
		 (setf (%bignum-ref res j) all-ones-digit)))))
      (%normalize-bignum res res-len))
     res)))

;;;; DPB (deposit byte)

(defun bignum-deposit-byte (new-byte byte-spec bignum)
  (declare (type bignum-type bignum))
  (let* ((byte-len (byte-size byte-spec))
	 (byte-pos (byte-position byte-spec))
	 (bignum-len (%bignum-length bignum))
	 (bignum-plusp (%bignum-0-or-plusp bignum bignum-len))
	 (byte-end (+ byte-pos byte-len))
	 (res-len (1+ (max (ceiling byte-end digit-size) bignum-len)))
	 (res (%allocate-bignum res-len)))
    (declare (type bignum-index bignum-len res-len))
    ;; Fill in an extra sign digit in case we set what would otherwise be the
    ;; last digit's last bit. Normalize at the end in case this was
    ;; unnecessary.
    (unless bignum-plusp
      (setf (%bignum-ref res (1- res-len)) all-ones-digit))
    (multiple-value-bind (end-digit end-bits) (truncate byte-end digit-size)
      (declare (type bignum-index end-digit))
      ;; Fill in bits from bignum up to byte-pos.
      (multiple-value-bind (pos-digit pos-bits) (truncate byte-pos digit-size)
	(declare (type bignum-index pos-digit))
	(do ((i 0 (1+ i))
	     (end (min pos-digit bignum-len)))
	    ((= i end)
	     (cond ((< i bignum-len)
		    (unless (zerop pos-bits)
		      (setf (%bignum-ref res i)
			    (logand (%bignum-ref bignum i)
				    (%make-ones pos-bits)))))
		   (bignum-plusp)
		   (t
		    (do ((i i (1+ i)))
			((= i pos-digit)
			 (unless (zerop pos-bits)
			   (setf (%bignum-ref res i) (%make-ones pos-bits))))
		      (setf (%bignum-ref res i) all-ones-digit)))))
	  (setf (%bignum-ref res i) (%bignum-ref bignum i)))
	;; Fill in bits from new-byte.
	(if (typep new-byte 'fixnum)
	    (deposit-fixnum-bits new-byte byte-len pos-digit pos-bits
				 end-digit end-bits res)
	    (deposit-bignum-bits new-byte byte-len pos-digit pos-bits
				 end-digit end-bits res)))
      ;; Fill in remaining bits from bignum after byte-spec.
      (when (< end-digit bignum-len)
	(setf (%bignum-ref res end-digit)
	      (logior (logand (%bignum-ref bignum end-digit)
			      (%ashl (%make-ones (- digit-size end-bits))
				     end-bits))
		      ;; DEPOSIT-FIXNUM-BITS and DEPOSIT-BIGNUM-BITS only store
		      ;; bits from new-byte into res's end-digit element, so
		      ;; we don't need to mask out unwanted high bits.
		      (%bignum-ref res end-digit)))
	(do ((i (1+ end-digit) (1+ i)))
	    ((= i bignum-len))
	  (setf (%bignum-ref res i) (%bignum-ref bignum i)))))
    (%normalize-bignum res res-len)))

;;; This starts at result's pos-digit skipping pos-bits, and it stores bits
;;; from new-byte, a fixnum, into result. It effectively stores byte-len
;;; number of bits, but never stores past end-digit and end-bits in result.
;;; The first branch fires when all the bits we want from new-byte are present;
;;; if byte-len crosses from the current result digit into the next, the last
;;; argument to DEPOSIT-FIXNUM-DIGIT is a mask for those bits. The second
;;; branch handles the need to grab more bits than the fixnum new-byte has, but
;;; new-byte is positive; therefore, any virtual bits are zero. The mask for
;;; bits that don't fit in the current result digit is simply the remaining
;;; bits in the bignum digit containing new-byte; we don't care if we store
;;; some extra in the next result digit since they will be zeros. The last
;;; branch handles the need to grab more bits than the fixnum new-byte has, but
;;; new-byte is negative; therefore, any virtual bits must be explicitly filled
;;; in as ones. We call DEPOSIT-FIXNUM-DIGIT to grab what bits actually exist
;;; and to fill in the current result digit.
(defun deposit-fixnum-bits (new-byte byte-len pos-digit pos-bits
			    end-digit end-bits result)
  (declare (type bignum-index pos-digit end-digit))
  (let ((other-bits (- digit-size pos-bits))
	(new-byte-digit (%fixnum-to-digit new-byte)))
    (declare (type bignum-element-type new-byte-digit))
    (cond ((< byte-len maximum-fixnum-bits)
	   (deposit-fixnum-digit new-byte-digit byte-len pos-digit pos-bits
				 other-bits result
				 (- byte-len other-bits)))
	  ((or (plusp new-byte) (zerop new-byte))
	   (deposit-fixnum-digit new-byte-digit byte-len pos-digit pos-bits
				 other-bits result pos-bits))
	  (t
	   (multiple-value-bind (digit bits)
	       (deposit-fixnum-digit new-byte-digit byte-len pos-digit pos-bits
				     other-bits result
				     (if (< (- byte-len other-bits) digit-size)
					 (- byte-len other-bits)
					 digit-size))
	     (declare (type bignum-index digit))
	     (cond ((< digit end-digit)
		    (setf (%bignum-ref result digit)
			  (logior (%bignum-ref result digit)
				  (%ashl (%make-ones (- digit-size bits)) bits)))
		    (do ((i (1+ digit) (1+ i)))
			((= i end-digit)
			 (setf (%bignum-ref result i) (%make-ones end-bits)))
		      (setf (%bignum-ref result i) all-ones-digit)))
		   ((> digit end-digit))
		   ((< bits end-bits)
		    (setf (%bignum-ref result digit)
			  (logior (%bignum-ref result digit)
				  (%ashl (%make-ones (- end-bits bits))
					 bits))))))))))

;;; This fills in the current result digit from new-byte-digit. The first case
;;; handles everything we want fitting in the current digit, and other-bits is
;;; the number of bits remaining to be filled in result's current digit. This
;;; number is digit-size minus pos-bits. The second branch handles filling in
;;; result's current digit, and it shoves the unused bits of new-byte-digit
;;; into the next result digit. This is correct regardless of new-byte-digit's
;;; sign. It returns the new current result digit and how many bits already
;;; filled in the result digit.
(defun deposit-fixnum-digit (new-byte-digit byte-len pos-digit pos-bits
			     other-bits result next-digit-bits-needed)
  (declare (type bignum-index pos-digit)
	   (type bignum-element-type new-byte-digit next-digit-mask))
  (cond ((<= byte-len other-bits)
	 ;; Bits from new-byte fit in the current result digit.
	 (setf (%bignum-ref result pos-digit)
	       (logior (%bignum-ref result pos-digit)
		       (%ashl (logand new-byte-digit (%make-ones byte-len))
			      pos-bits)))
	 (if (= byte-len other-bits)
	     (values (1+ pos-digit) 0)
	     (values pos-digit (+ byte-len pos-bits))))
	(t
	 ;; Some of new-byte's bits go in current result digit.
	 (setf (%bignum-ref result pos-digit)
	       (logior (%bignum-ref result pos-digit)
		       (%ashl (logand new-byte-digit (%make-ones other-bits))
			      pos-bits)))
	 (let ((pos-digit+1 (1+ pos-digit)))
	   ;; The rest of new-byte's bits go in the next result digit.
	   (setf (%bignum-ref result pos-digit+1)
		 (logand (ash new-byte-digit (- other-bits))
			 ;; Must LOGAND after shift here.
			 (%make-ones next-digit-bits-needed)))
	   (if (= next-digit-bits-needed digit-size)
	       (values (1+ pos-digit+1) 0)
	       (values pos-digit+1 next-digit-bits-needed))))))

;;; This starts at result's pos-digit skipping pos-bits, and it stores bits
;;; from new-byte, a bignum, into result. It effectively stores byte-len
;;; number of bits, but never stores past end-digit and end-bits in result.
;;; When handling a starting bit unaligned with a digit boundary, we check
;;; in the second branch for the byte spec fitting into the pos-digit element
;;; after after pos-bits; DEPOSIT-UNALIGNED-BIGNUM-BITS expects at least one
;;; digit boundary crossing.
(defun deposit-bignum-bits (bignum-byte byte-len pos-digit pos-bits
			    end-digit end-bits result)
  (declare (type bignum-index pos-digit end-digit))
  (cond ((zerop pos-bits)
	 (deposit-aligned-bignum-bits bignum-byte pos-digit end-digit end-bits
				      result))
	((or (= end-digit pos-digit)
	     (and (= end-digit (1+ pos-digit))
		  (zerop end-bits)))
	 (setf (%bignum-ref result pos-digit)
	       (logior (%bignum-ref result pos-digit)
		       (%ashl (logand (%bignum-ref bignum-byte 0)
				      (%make-ones byte-len))
			      pos-bits))))
	(t (deposit-unaligned-bignum-bits bignum-byte pos-digit pos-bits
					  end-digit end-bits result))))

;;; This deposits bits from bignum-byte into result starting at pos-digit and
;;; the zero'th bit. It effectively only stores bits to end-bits in the
;;; end-digit element of result. The loop termination code takes care of
;;; picking up the last digit's bits or filling in virtual negative sign bits.
(defun deposit-aligned-bignum-bits (bignum-byte pos-digit end-digit end-bits
				    result)
  (declare (type bignum-index pos-digit end-digit))
  (let* ((bignum-len (%bignum-length bignum-byte))
	 (bignum-plusp (%bignum-0-or-plusp bignum-byte bignum-len)))
    (declare (type bignum-index bignum-len))
    (do ((i 0 (1+ i ))
	 (j pos-digit (1+ j)))
	((or (= j end-digit) (= i bignum-len))
	 (cond ((= j end-digit)
		(cond ((< i bignum-len)
		       (setf (%bignum-ref result j)
			     (logand (%bignum-ref bignum-byte i)
				     (%make-ones end-bits))))
		      (bignum-plusp)
		      (t
		       (setf (%bignum-ref result j) (%make-ones end-bits)))))
	       (bignum-plusp)
	       (t
		(do ((j j (1+ j)))
		    ((= j end-digit)
		     (setf (%bignum-ref result j) (%make-ones end-bits)))
		  (setf (%bignum-ref result j) all-ones-digit)))))
      (setf (%bignum-ref result j) (%bignum-ref bignum-byte i)))))

;;; This assumes at least one digit crossing.
(defun deposit-unaligned-bignum-bits (bignum-byte pos-digit pos-bits
				      end-digit end-bits result)
  (declare (type bignum-index pos-digit end-digit))
  (let* ((bignum-len (%bignum-length bignum-byte))
	 (bignum-plusp (%bignum-0-or-plusp bignum-byte bignum-len))
	 (low-mask (%make-ones pos-bits))
	 (bits-past-pos-bits (- digit-size pos-bits))
	 (high-mask (%make-ones bits-past-pos-bits))
	 (minus-high-bits (- bits-past-pos-bits)))
    (declare (type bignum-element-type low-mask high-mask)
	     (type bignum-index bignum-len))
    (do ((i 0 (1+ i))
	 (j pos-digit j+1)
	 (j+1 (1+ pos-digit) (1+ j+1)))
	((or (= j end-digit) (= i bignum-len))
	 (cond
	  ((= j end-digit)
	   (setf (%bignum-ref result j)
		 (cond
		  ((>= pos-bits end-bits)
		   (logand (%bignum-ref result j) (%make-ones end-bits)))
		  ((< i bignum-len)
		   (logior (%bignum-ref result j)
			   (%ashl (logand (%bignum-ref bignum-byte i)
					  (%make-ones (- end-bits pos-bits)))
				  pos-bits)))
		  (bignum-plusp
		   (logand (%bignum-ref result j)
			   ;; 0's between pos-bits and end-bits positions.
			   (logior (%ashl (%make-ones (- digit-size end-bits))
					  end-bits)
				   low-mask)))
		  (t (logior (%bignum-ref result j)
			     (%ashl (%make-ones (- end-bits pos-bits))
				    pos-bits))))))
	  (bignum-plusp)
	  (t
	   (setf (%bignum-ref result j)
		 (%ashl (%make-ones bits-past-pos-bits) pos-bits))
	   (do ((j j+1 (1+ j)))
	       ((= j end-digit)
		(setf (%bignum-ref result j) (%make-ones end-bits)))
	     (declare (type bignum-index j))
	     (setf (%bignum-ref result j) all-ones-digit)))))
      (declare (type bignum-index i j j+1))
      (let ((digit (%bignum-ref bignum-byte i)))
	(declare (type bignum-element-type digit))
	(setf (%bignum-ref result j)
	      (logior (%bignum-ref result j)
		      (%ashl (logand digit high-mask) pos-bits)))
	(setf (%bignum-ref result j+1)
	      (logand (ash digit minus-high-bits)
		      ;; LOGAND should be unnecessary here with a logical right
		      ;; shift or a correct unsigned-byte-32 one.
		      low-mask))))))
|#

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
;;; if x[i] = y[j] then g = #xFFFFFFFF
;;; else g = x[i]x[i-1]/y[j].
;;;
;;; check guess.
;;; %UNSIGNED-MULTIPLY returns b and c defined below.
;;;    a = x[i-1] - (logand (* g y[j]) #xFFFFFFFF).
;;;       Use %UNSIGNED-MULTIPLY taking low-order result.
;;;    b = (logand (ash (* g y[j-1]) -32) #xFFFFFFFF).
;;;    c = (logand (* g y[j-1]) #xFFFFFFFF).
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

;;; These are used by BIGNUM-TRUNCATE and friends in the general case.
(defvar *truncate-x*)
(defvar *truncate-y*)

;;; Divide X by Y returning the quotient and remainder. In the
;;; general case, we shift Y to set up for the algorithm, and we use
;;; two buffers to save consing intermediate values. X gets
;;; destructively modified to become the remainder, and we have to
;;; shift it to account for the initial Y shift. After we multiple
;;; bind q and r, we first fix up the signs and then return the
;;; normalized results.
(defun bignum-truncate (x y)
  (declare (type bignum-type x y))
  (let* ((x-plusp (%bignum-0-or-plusp x (%bignum-length x)))
	 (y-plusp (%bignum-0-or-plusp y (%bignum-length y)))
	 (x (if x-plusp x (negate-bignum x nil)))
	 (y (if y-plusp y (negate-bignum y nil)))
	 (len-x (%bignum-length x))
	 (len-y (%bignum-length y)))
    (multiple-value-bind (q r)
	(cond ((< len-y 2)
	       (bignum-truncate-single-digit x len-x y))
	      ((plusp (bignum-compare y x))
	       (let ((res (%allocate-bignum len-x)))
		 (dotimes (i len-x)
		   (setf (%bignum-ref res i) (%bignum-ref x i)))
		 (values 0 res)))
	      (t
	       (let ((len-x+1 (1+ len-x)))
		 (with-bignum-buffers ((*truncate-x* len-x+1)
				       (*truncate-y* (1+ len-y)))
		   (let ((y-shift (shift-y-for-truncate y)))
		     (shift-and-store-truncate-buffers x len-x y len-y y-shift)
		     (values (return-quotient-leaving-remainder len-x+1 len-y)
			     ;; Now that RETURN-QUOTIENT-LEAVING-REMAINDER
			     ;; has executed, we just tidy up the remainder
			     ;; (in *TRUNCATE-X*) and return it.
			     (cond
			      ((zerop y-shift)
			       (let ((res (%allocate-bignum len-y)))
				 (declare (type bignum-type res))
				 (bignum-replace res *truncate-x* :end2 len-y)
				 (%normalize-bignum res len-y)))
			      (t
			       (shift-right-unaligned
				*truncate-x* 0 y-shift len-y
				((= j res-len-1)
				 (setf (%bignum-ref res j)
				       (%ashr (%bignum-ref *truncate-x* i)
					      y-shift))
				 (%normalize-bignum res res-len))
				res)))))))))
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
		    (%normalize-bignum rem (%bignum-length rem))))))))

;;; Divide X by Y when Y is a single bignum digit. BIGNUM-TRUNCATE
;;; fixes up the quotient and remainder with respect to sign and
;;; normalization.
;;;
;;; We don't have to worry about shifting Y to make its most
;;; significant digit sufficiently large for %FLOOR to return 32-bit
;;; quantities for the q-digit and r-digit. If Y is a single digit
;;; bignum, it is already large enough for %FLOOR. That is, it has
;;; some bits on pretty high in the digit.
(defun bignum-truncate-single-digit (x len-x y)
  (declare (type bignum-index len-x))
  (let ((q (%allocate-bignum len-x))
	(r 0)
	(y (%bignum-ref y 0)))
    (declare (type bignum-element-type r y))
    (do ((i (1- len-x) (1- i)))
	((minusp i))
      (multiple-value-bind (q-digit r-digit) (%floor r (%bignum-ref x i) y)
	(declare (type bignum-element-type q-digit r-digit))
	(setf (%bignum-ref q i) q-digit)
	(setf r r-digit)))
    (let ((rem (%allocate-bignum 1)))
      (setf (%bignum-ref rem 0) r)
      (values q rem))))

;;; a helper function for BIGNUM-TRUNCATE
;;;
;;; Divide *TRUNCATE-X* by *TRUNCATE-Y*, returning the quotient
;;; and destructively modifying *TRUNCATE-X* so that it holds
;;; the remainder.
;;;
;;; LEN-X and LEN-Y tell us how much of the buffers we care about.
;;;
;;; *TRUNCATE-X* definitely has at least three digits, and it has one
;;; more than *TRUNCATE-Y*. This keeps i, i-1, i-2, and low-x-digit
;;; happy. Thanks to SHIFT-AND-STORE-TRUNCATE-BUFFERS.
(defun return-quotient-leaving-remainder (len-x len-y)
  (declare (type bignum-index len-x len-y))
  (let* ((len-q (- len-x len-y))
	 ;; Add one for extra sign digit in case high bit is on.
	 (q (%allocate-bignum (1+ len-q)))
	 (k (1- len-q))
	 (y1 (%bignum-ref *truncate-y* (1- len-y)))
	 (y2 (%bignum-ref *truncate-y* (- len-y 2)))
	 (i (1- len-x))
	 (i-1 (1- i))
	 (i-2 (1- i-1))
	 (low-x-digit (- i len-y)))
    (declare (type bignum-index len-q k i i-1 i-2 low-x-digit)
	     (type bignum-element-type y1 y2))
    (loop
      (setf (%bignum-ref q k)
	    (try-bignum-truncate-guess
	     ;; This modifies *TRUNCATE-X*. Must access elements each pass.
	     (bignum-truncate-guess y1 y2
				    (%bignum-ref *truncate-x* i)
				    (%bignum-ref *truncate-x* i-1)
				    (%bignum-ref *truncate-x* i-2))
	     len-y low-x-digit))
      (cond ((zerop k) (return))
	    (t (decf k)
	       (decf low-x-digit)
	       (shiftf i i-1 i-2 (1- i-2)))))
    q))

;;; This takes a digit guess, multiplies it by *TRUNCATE-Y* for a
;;; result one greater in length than LEN-Y, and subtracts this result
;;; from *TRUNCATE-X*. LOW-X-DIGIT is the first digit of X to start
;;; the subtraction, and we know X is long enough to subtract a LEN-Y
;;; plus one length bignum from it. Next we check the result of the
;;; subtraction, and if the high digit in X became negative, then our
;;; guess was one too big. In this case, return one less than GUESS
;;; passed in, and add one value of Y back into X to account for
;;; subtracting one too many. Knuth shows that the guess is wrong on
;;; the order of 3/b, where b is the base (2 to the digit-size power)
;;; -- pretty rarely.
(defun try-bignum-truncate-guess (guess len-y low-x-digit)
  (declare (type bignum-index low-x-digit len-y)
	   (type bignum-element-type guess))
  (let ((carry-digit 0)
	(borrow 1)
	(i low-x-digit))
    (declare (type bignum-element-type carry-digit)
	     (type bignum-index i)
	     (fixnum borrow))
    ;; Multiply guess and divisor, subtracting from dividend simultaneously.
    (dotimes (j len-y)
      (multiple-value-bind (high-digit low-digit)
	  (%multiply-and-add guess
			     (%bignum-ref *truncate-y* j)
			     carry-digit)
	(declare (type bignum-element-type high-digit low-digit))
	(setf carry-digit high-digit)
	(multiple-value-bind (x temp-borrow)
	    (%subtract-with-borrow (%bignum-ref *truncate-x* i)
				   low-digit
				   borrow)
	  (declare (type bignum-element-type x)
		   (fixnum temp-borrow))
	  (setf (%bignum-ref *truncate-x* i) x)
	  (setf borrow temp-borrow)))
      (incf i))
    (setf (%bignum-ref *truncate-x* i)
	  (%subtract-with-borrow (%bignum-ref *truncate-x* i)
				 carry-digit borrow))
    ;; See whether guess is off by one, adding one Y back in if necessary.
    (cond ((%digit-0-or-plusp (%bignum-ref *truncate-x* i))
	   guess)
	  (t
	   ;; If subtraction has negative result, add one divisor value back
	   ;; in. The guess was one too large in magnitude.
	   (let ((i low-x-digit)
		 (carry 0))
	     (dotimes (j len-y)
	       (multiple-value-bind (v k)
		   (%add-with-carry (%bignum-ref *truncate-y* j)
				    (%bignum-ref *truncate-x* i)
				    carry)
		 (declare (type bignum-element-type v))
		 (setf (%bignum-ref *truncate-x* i) v)
		 (setf carry k))
	       (incf i))
	     (setf (%bignum-ref *truncate-x* i)
		   (%add-with-carry (%bignum-ref *truncate-x* i) 0 carry)))
	   (%subtract-with-borrow guess 1 1)))))

;;; This returns a guess for the next division step. Y1 is the highest y
;;; digit, and y2 is the second to highest y digit. The x... variables are
;;; the three highest x digits for the next division step.
;;;
;;; From Knuth, our guess is either all ones or x-i and x-i-1 divided by y1,
;;; depending on whether x-i and y1 are the same. We test this guess by
;;; determining whether guess*y2 is greater than the three high digits of x
;;; minus guess*y1 shifted left one digit:
;;;    ------------------------------
;;;   |    x-i    |   x-i-1  | x-i-2 |
;;;    ------------------------------
;;;    ------------------------------
;;; - | g*y1 high | g*y1 low |   0   |
;;;    ------------------------------
;;;		...		  <   guess*y2     ???
;;; If guess*y2 is greater, then we decrement our guess by one and try again.
;;; This returns a guess that is either correct or one too large.
(defun bignum-truncate-guess (y1 y2 x-i x-i-1 x-i-2)
  (declare (type bignum-element-type y1 y2 x-i x-i-1 x-i-2))
  (let ((guess (if (%digit-compare x-i y1)
		   all-ones-digit
		   (%floor x-i x-i-1 y1))))
    (declare (type bignum-element-type guess))
    (loop
      (multiple-value-bind (high-guess*y1 low-guess*y1) (%multiply guess y1)
	(declare (type bignum-element-type low-guess*y1 high-guess*y1))
	(multiple-value-bind (high-guess*y2 low-guess*y2)
	    (%multiply guess y2)
	  (declare (type bignum-element-type high-guess*y2 low-guess*y2))
	  (multiple-value-bind (middle-digit borrow)
	      (%subtract-with-borrow x-i-1 low-guess*y1 1)
	    (declare (type bignum-element-type middle-digit)
		     (fixnum borrow))
	    ;; Supplying borrow of 1 means there was no borrow, and we know
	    ;; x-i-2 minus 0 requires no borrow.
	    (let ((high-digit (%subtract-with-borrow x-i high-guess*y1 borrow)))
	      (declare (type bignum-element-type high-digit))
	      (if (and (%digit-compare high-digit 0)
		       (or (%digit-greater high-guess*y2 middle-digit)
			   (and (%digit-compare middle-digit high-guess*y2)
				(%digit-greater low-guess*y2 x-i-2))))
		  (setf guess (%subtract-with-borrow guess 1 1))
		  (return guess)))))))))

;;; This returns the amount to shift y to place a one in the second highest
;;; bit. Y must be positive. If the last digit of y is zero, then y has a
;;; one in the previous digit's sign bit, so we know it will take one less
;;; than digit-size to get a one where we want. Otherwise, we count how many
;;; right shifts it takes to get zero; subtracting this value from digit-size
;;; tells us how many high zeros there are which is one more than the shift
;;; amount sought.
;;;
;;; Note: This is exactly the same as one less than the integer-length of the
;;; last digit subtracted from the digit-size.
;;;
;;; We shift y to make it sufficiently large that doing the 64-bit by 32-bit
;;; %FLOOR calls ensures the quotient and remainder fit in 32-bits.
(defun shift-y-for-truncate (y)
  (let* ((len (%bignum-length y))
	 (last (%bignum-ref y (1- len))))
    (declare (type bignum-index len)
	     (type bignum-element-type last))
    (- digit-size (integer-length last) 1)))

;;; Stores two bignums into the truncation bignum buffers, shifting them on the
;;; way in. This assumes x and y are positive and at least two in length, and
;;; it assumes *truncate-x* and *truncate-y* are one digit longer than x and y.
(defun shift-and-store-truncate-buffers (x len-x y len-y shift)
  (declare (type bignum-index len-x len-y)
	   (type (integer 0 (#.digit-size)) shift))
  (cond ((zerop shift)
	 (bignum-replace *truncate-x* x :end1 len-x)
	 (bignum-replace *truncate-y* y :end1 len-y))
	(t
	 (bignum-ashift-left-unaligned x 0 shift (1+ len-x) *truncate-x*)
	 (bignum-ashift-left-unaligned y 0 shift (1+ len-y) *truncate-y*))))

;;;; %FLOOR primitive for BIGNUM-TRUNCATE

;;; When a machine leaves out a 64-bit by 32-bit divide instruction (that is,
;;; two bignum-digits divided by one), we have to roll our own (the hard way).
;;; Basically, we treat the operation as four 16-bit digits divided by two
;;; 16-bit digits. This means we have duplicated most of the code above to do
;;; this nearly general 16-bit digit bignum divide, but we've unrolled loops
;;; and made use of other properties of this specific divide situation.

;;;; %FLOOR for machines with a 32x32 divider.

#!-sb-fluid
(declaim (inline 32x16-subtract-with-borrow 32x16-add-with-carry
		 32x16-divide 32x16-multiply 32x16-multiply-split))

#!+32x16-divide
(defconstant 32x16-base-1 #xFFFF)

;;; This is similar to %SUBTRACT-WITH-BORROW. It returns a 16-bit difference
;;; and a borrow. Returning a 1 for the borrow means there was no borrow, and
;;; 0 means there was one.
#!+32x16-divide
(defun 32x16-subtract-with-borrow (a b borrow)
  (declare (type (unsigned-byte 16) a b)
	   (type (integer 0 1) borrow))
  (let ((diff (+ (- a b) borrow 32x16-base-1)))
    (declare (type (unsigned-byte 17) diff))
    (values (logand diff #xFFFF)
	    (ash diff -16))))

;;; This adds a and b, 16-bit quantities, with the carry k. It returns a
;;; 16-bit sum and a second value, 0 or 1, indicating whether there was a
;;; carry.
#!+32x16-divide
(defun 32x16-add-with-carry (a b k)
  (declare (type (unsigned-byte 16) a b)
	   (type (integer 0 1) k))
  (let ((res (the fixnum (+ a b k))))
    (declare (type (unsigned-byte 17) res))
    (if (zerop (the fixnum (logand #x10000 res)))
	(values res 0)
	(values (the (unsigned-byte 16) (logand #xFFFF res))
		1))))

;;; This is probably a 32-bit by 32-bit divide instruction.
#!+32x16-divide
(defun 32x16-divide (a b c)
  (declare (type (unsigned-byte 16) a b c))
  (floor (the bignum-element-type
	      (logior (the bignum-element-type (ash a 16))
		      b))
	 c))

;;; This basically exists since we know the answer won't overflow
;;; bignum-element-type. It's probably just a basic multiply instruction, but
;;; it can't cons an intermediate bignum. The result goes in a non-descriptor
;;; register.
#!+32x16-divide
(defun 32x16-multiply (a b)
  (declare (type (unsigned-byte 16) a b))
  (the bignum-element-type (* a b)))

;;; This multiplies a and b, 16-bit quantities, and returns the result as two
;;; 16-bit quantities, high and low.
#!+32x16-divide
(defun 32x16-multiply-split (a b)
  (let ((res (32x16-multiply a b)))
    (declare (the bignum-element-type res))
    (values (the (unsigned-byte 16) (logand #xFFFF (ash res -16)))
	    (the (unsigned-byte 16) (logand #xFFFF res)))))

;;; The %FLOOR below uses this buffer the same way BIGNUM-TRUNCATE uses
;;; *truncate-x*. There's no y buffer since we pass around the two 16-bit
;;; digits and use them slightly differently than the general truncation
;;; algorithm above.
#!+32x16-divide
(defvar *32x16-truncate-x* (make-array 4 :element-type '(unsigned-byte 16)
				       :initial-element 0))

;;; This does the same thing as the %FLOOR above, but it does it at Lisp level
;;; when there is no 64x32-bit divide instruction on the machine.
;;;
;;; It implements the higher level tactics of BIGNUM-TRUNCATE, but it makes use
;;; of special situation provided, four 16-bit digits divided by two 16-bit
;;; digits.
#!+32x16-divide
(defun %floor (a b c)
  (declare (type bignum-element-type a b c))
  ;; Setup *32x16-truncate-x* buffer from a and b.
  (setf (aref *32x16-truncate-x* 0)
	(the (unsigned-byte 16) (logand #xFFFF b)))
  (setf (aref *32x16-truncate-x* 1)
	(the (unsigned-byte 16)
	     (logand #xFFFF
		     (the (unsigned-byte 16) (ash b -16)))))
  (setf (aref *32x16-truncate-x* 2)
	(the (unsigned-byte 16) (logand #xFFFF a)))
  (setf (aref *32x16-truncate-x* 3)
	(the (unsigned-byte 16)
	     (logand #xFFFF
		     (the (unsigned-byte 16) (ash a -16)))))
  ;; From DO-TRUNCATE, but unroll the loop.
  (let* ((y1 (logand #xFFFF (ash c -16)))
	 (y2 (logand #xFFFF c))
	 (q (the bignum-element-type
		 (ash (32x16-try-bignum-truncate-guess
		       (32x16-truncate-guess y1 y2
					     (aref *32x16-truncate-x* 3)
					     (aref *32x16-truncate-x* 2)
					     (aref *32x16-truncate-x* 1))
		       y1 y2 1)
		      16))))
    (declare (type bignum-element-type q)
	     (type (unsigned-byte 16) y1 y2))
    (values (the bignum-element-type
		 (logior q
			 (the (unsigned-byte 16)
			      (32x16-try-bignum-truncate-guess
			       (32x16-truncate-guess
				y1 y2
				(aref *32x16-truncate-x* 2)
				(aref *32x16-truncate-x* 1)
				(aref *32x16-truncate-x* 0))
			       y1 y2 0))))
	    (the bignum-element-type
		 (logior (the bignum-element-type
			      (ash (aref *32x16-truncate-x* 1) 16))
			 (the (unsigned-byte 16)
			      (aref *32x16-truncate-x* 0)))))))

;;; This is similar to TRY-BIGNUM-TRUNCATE-GUESS, but this unrolls the two
;;; loops. This also substitutes for %DIGIT-0-OR-PLUSP the equivalent
;;; expression without any embellishment or pretense of abstraction. The first
;;; loop is unrolled, but we've put the body of the loop into the function
;;; 32X16-TRY-GUESS-ONE-RESULT-DIGIT.
#!+32x16-divide
(defun 32x16-try-bignum-truncate-guess (guess y-high y-low low-x-digit)
  (declare (type bignum-index low-x-digit)
	   (type (unsigned-byte 16) guess y-high y-low))
  (let ((high-x-digit (+ 2 low-x-digit)))
    ;; Multiply guess and divisor, subtracting from dividend simultaneously.
    (multiple-value-bind (guess*y-hold carry borrow)
	(32x16-try-guess-one-result-digit guess y-low 0 0 1 low-x-digit)
      (declare (type (unsigned-byte 16) guess*y-hold)
	       (fixnum carry borrow))
      (multiple-value-bind (guess*y-hold carry borrow)
	  (32x16-try-guess-one-result-digit guess y-high guess*y-hold
					    carry borrow (1+ low-x-digit))
	(declare (type (unsigned-byte 16) guess*y-hold)
		 (fixnum borrow)
		 (ignore carry))
	(setf (aref *32x16-truncate-x* high-x-digit)
	      (32x16-subtract-with-borrow (aref *32x16-truncate-x* high-x-digit)
					  guess*y-hold borrow))))
    ;; See whether guess is off by one, adding one Y back in if necessary.
    (cond ((zerop (logand #x8000 (aref *32x16-truncate-x* high-x-digit)))
	   ;; The subtraction result is zero or positive.
	   guess)
	  (t
	   ;; If subtraction has negative result, add one divisor value back
	   ;; in. The guess was one too large in magnitude.
	   (multiple-value-bind (v carry)
	       (32x16-add-with-carry y-low
				     (aref *32x16-truncate-x* low-x-digit)
				     0)
	     (declare (type (unsigned-byte 16) v))
	     (setf (aref *32x16-truncate-x* low-x-digit) v)
	     (multiple-value-bind (v carry)
		 (32x16-add-with-carry y-high
				       (aref *32x16-truncate-x*
					     (1+ low-x-digit))
				       carry)
	       (setf (aref *32x16-truncate-x* (1+ low-x-digit)) v)
	       (setf (aref *32x16-truncate-x* high-x-digit)
		     (32x16-add-with-carry (aref *32x16-truncate-x* high-x-digit)
					   carry 0))))
	   (if (zerop (logand #x8000 guess))
	       (1- guess)
	       (1+ guess))))))

;;; This is similar to the body of the loop in TRY-BIGNUM-TRUNCATE-GUESS that
;;; multiplies the guess by y and subtracts the result from x simultaneously.
;;; This returns the digit remembered as part of the multiplication, the carry
;;; from additions done on behalf of the multiplication, and the borrow from
;;; doing the subtraction.
#!+32x16-divide
(defun 32x16-try-guess-one-result-digit (guess y-digit guess*y-hold
					 carry borrow x-index)
  (multiple-value-bind (high-digit low-digit)
      (32x16-multiply-split guess y-digit)
    (declare (type (unsigned-byte 16) high-digit low-digit))
    (multiple-value-bind (low-digit temp-carry)
	(32x16-add-with-carry low-digit guess*y-hold carry)
      (declare (type (unsigned-byte 16) low-digit))
      (multiple-value-bind (high-digit temp-carry)
	  (32x16-add-with-carry high-digit temp-carry 0)
	(declare (type (unsigned-byte 16) high-digit))
	(multiple-value-bind (x temp-borrow)
	    (32x16-subtract-with-borrow (aref *32x16-truncate-x* x-index)
					low-digit borrow)
	  (declare (type (unsigned-byte 16) x))
	  (setf (aref *32x16-truncate-x* x-index) x)
	  (values high-digit temp-carry temp-borrow))))))

;;; This is similar to BIGNUM-TRUNCATE-GUESS, but instead of computing the
;;; guess exactly as described in the its comments (digit by digit), this
;;; massages the 16-bit quantities into 32-bit quantities and performs the
#!+32x16-divide
(defun 32x16-truncate-guess (y1 y2 x-i x-i-1 x-i-2)
  (declare (type (unsigned-byte 16) y1 y2 x-i x-i-1 x-i-2))
  (let ((guess (if (= x-i y1)
		   #xFFFF
		   (32x16-divide x-i x-i-1 y1))))
    (declare (type (unsigned-byte 16) guess))
    (loop
      (let* ((guess*y1 (the bignum-element-type
			    (ash (logand #xFFFF
					 (the bignum-element-type
					      (32x16-multiply guess y1)))
				 16)))
	     (x-y (%subtract-with-borrow
		   (the bignum-element-type
			(logior (the bignum-element-type
				     (ash x-i-1 16))
				x-i-2))
		   guess*y1
		   1))
	     (guess*y2 (the bignum-element-type (%multiply guess y2))))
	(declare (type bignum-element-type guess*y1 x-y guess*y2))
	(if (%digit-greater guess*y2 x-y)
	    (decf guess)
	    (return guess))))))

;;;; general utilities

;;; Allocate a single word bignum that holds fixnum. This is useful when
;;; we are trying to mix fixnum and bignum operands.
#!-sb-fluid (declaim (inline make-small-bignum))
(defun make-small-bignum (fixnum)
  (let ((res (%allocate-bignum 1)))
    (setf (%bignum-ref res 0) (%fixnum-to-digit fixnum))
    res))

;;; Internal in-place operations use this to fixup remaining digits in the
;;; incoming data, such as in-place shifting. This is basically the same as
;;; the first form in %NORMALIZE-BIGNUM, but we return the length of the buffer
;;; instead of shrinking the bignum.
#!-sb-fluid (declaim (sb!ext:maybe-inline %normalize-bignum-buffer))
(defun %normalize-bignum-buffer (result len)
  (declare (type bignum-type result)
	   (type bignum-index len))
  (unless (= len 1)
    (do ((next-digit (%bignum-ref result (- len 2))
		     (%bignum-ref result (- len 2)))
	 (sign-digit (%bignum-ref result (1- len)) next-digit))
	((not (zerop (logxor sign-digit (%ashr next-digit (1- digit-size))))))
	(decf len)
	(setf (%bignum-ref result len) 0)	
	(when (= len 1)
	      (return))))
  len)

;;; This drops the last digit if it is unnecessary sign information. It repeats
;;; this as needed, possibly ending with a fixnum. If the resulting length from
;;; shrinking is one, see whether our one word is a fixnum. Shift the possible
;;; fixnum bits completely out of the word, and compare this with shifting the
;;; sign bit all the way through. If the bits are all 1's or 0's in both words,
;;; then there are just sign bits between the fixnum bits and the sign bit. If
;;; we do have a fixnum, shift it over for the two low-tag bits.
(defun %normalize-bignum (result len)
  (declare (type bignum-type result)
	   (type bignum-index len)
	   (inline %normalize-bignum-buffer))
  (let ((newlen (%normalize-bignum-buffer result len)))
    (declare (type bignum-index newlen))
    (unless (= newlen len)
      (%bignum-set-length result newlen))
    (if (= newlen 1)
	(let ((digit (%bignum-ref result 0)))
	  (if (= (%ashr digit 29) (%ashr digit (1- digit-size)))
	      (%fixnum-digit-with-correct-sign digit)
	      result))
	result)))

;;; This drops the last digit if it is unnecessary sign information. It
;;; repeats this as needed, possibly ending with a fixnum magnitude but never
;;; returning a fixnum.
(defun %mostly-normalize-bignum (result len)
  (declare (type bignum-type result)
	   (type bignum-index len)
	   (inline %normalize-bignum-buffer))
  (let ((newlen (%normalize-bignum-buffer result len)))
    (declare (type bignum-index newlen))
    (unless (= newlen len)
      (%bignum-set-length result newlen))
    result))

;;;; hashing

;;; the bignum case of the SXHASH function
(defun sxhash-bignum (x)
  (let ((result 316495330))
    (declare (type fixnum result))
    (dotimes (i (%bignum-length x))
      (declare (type index i))
      (let ((xi (%bignum-ref x i)))
	(mixf result
	      (logand most-positive-fixnum
		      xi
		      (ash xi -7)))))
    result))
