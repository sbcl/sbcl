;;;; This file contains DERIVE-TYPE methods for LOGAND, LOGIOR, and
;;;; friends.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;; Return the maximum number of bits an integer of the supplied type
;;; can take up, or NIL if it is unbounded. The second (third) value
;;; is T if the integer can be positive (negative) and NIL if not.
;;; Zero counts as positive.
(defun integer-type-length (type)
  (if (and (numeric-type-p type)
           (eq (numeric-type-class type) 'integer))
      (let ((min (numeric-type-low type))
            (max (numeric-type-high type)))
        (values (and min max (max (integer-length min) (integer-length max)))
                (or (null max) (not (minusp max)))
                (or (null min) (minusp min))))
      (values nil t t)))

;;;; Generators for simple bit masks

;;; Return an integer consisting of zeroes in its N least significant
;;; bit positions and ones in all others. If N is negative, return -1.
(declaim (inline zeroes))
(defun zeroes (n)
  (ash -1 n))

;;; Return an integer consisting of ones in its N least significant
;;; bit positions and zeroes in all others. If N is negative, return 0.
(declaim (inline ones))
(defun ones (n)
  (lognot (ash -1 n)))

;;; The functions LOG{AND,IOR,XOR}-DERIVE-UNSIGNED-BOUNDS below use
;;; algorithms derived from those in the chapter "Propagating Bounds
;;; through Logical Operations" from _Hacker's Delight_, Henry S.
;;; Warren, Jr., 2nd ed., pp 87-90.
;;;
;;; We used to implement the algorithms from that source (then its first
;;; edition) very faithfully here which exposed a weakness of theirs,
;;; namely worst case quadratical runtime in the number of bits of the
;;; input values, potentially leading to excessive compilation times for
;;; expressions involving bignums. To avoid that, I have devised and
;;; implemented variations of these algorithms that achieve linear
;;; runtime in all cases.
;;;
;;; Like Warren, let's start with the high bound on LOGIOR to explain
;;; how this is done. To follow, please read Warren's explanations on
;;; his "maxOR" function and compare this with how the second return
;;; value of LOGIOR-DERIVE-UNSIGNED-BOUNDS below is calculated.
;;;
;;; "maxOR" loops starting from the left until it finds a position where
;;; both B and D are 1 and where it is possible to decrease one of these
;;; bounds by setting this bit in it to 0 and all following ones to 1
;;; without the resulting value getting below the corresponding lower
;;; bound (A or C). This is done by calculating the modified values
;;; during each iteration where both B and D are 1 and comparing them
;;; against the lower bounds.
;;; The trick to avoid the loop is to exchange the order of the steps:
;;; First determine from which position rightwards it would be allowed
;;; to change B or D in this way and have the result be larger or equal
;;; than A or C respectively and then find the leftmost position equal
;;; to this or to the right of it where both B and D are 1.
;;; It is quite simple to find from where rightwards B could be modified
;;; this way: This is the leftmost position where B has a 1 and A a 0,
;;; or, cheaper to calculate, the leftmost position where A and B
;;; differ. Thus (INTEGER-LENGTH (LOGXOR A B)) gives us this position
;;; where a result of 1 corresponds to the rightmost bit position. As we
;;; don't care which of B or D we modify we can take the maximum of this
;;; value and of (INTEGER-LENGTH (LOGXOR C D)).
;;; The rest is equally simple: Build a mask of 1 bits from the thusly
;;; found position rightwards, LOGAND it with B and D and feed that into
;;; INTEGER-LENGTH. From this build another mask and LOGIOR it with B
;;; and D to set the desired bits.
;;; The special cases where A equals B and/or C equals D are covered by
;;; the same code provided the mask generator treats an argument of -1
;;; the same as 0, which both ZEROES and ONES do.
;;;
;;; To calculate the low bound on LOGIOR we need to treat X and Y
;;; independently for longer but the basic idea stays the same.
;;;
;;; LOGAND-DERIVE-UNSIGNED-BOUNDS can be derived by sufficiently many
;;; applications of DeMorgan's law from LOGIOR-DERIVE-UNSIGNED-BOUNDS.
;;; The implementation additionally avoids work (that is, calculations
;;; of one's complements) by using the identity (INTEGER-LENGTH X) =
;;; (INTEGER-LENGTH (LOGNOT X)) and observing that ZEROES is cheaper
;;; than ONES.
;;;
;;; For the low bound on LOGXOR we use Warren's formula
;;;   minXOR(a, b, c, d) = minAND(a, b, !d, !c) | minAND(!b, !a, c, d)
;;; where "!" is bitwise negation and "|" is bitwise or. Both minANDs
;;; are implemented as in LOGAND-DERIVE-UNSIGNED-BOUNDS (the part for
;;; the first result), sharing the first LOGXOR and INTEGER-LENGTH
;;; calculations as (LOGXOR A B) = (LOGXOR (LOGNOT B) (LOGNOT A)).
;;;
;;; For the high bound on LOGXOR Warren's formula seems unnecessarily
;;; complex. Instead, with (LOGNOT (LOGXOR X Y)) = (LOGXOR X (LOGNOT Y))
;;; we have
;;;   maxXOR(a, b, c, d) = !minXOR(a, b, !d, !c)
;;; and rewriting minXOR as above yields
;;;   maxXOR(a, b, c, d) = !(minAND(a, b, c, d) | minAND(!b, !a, !d, !c))
;;; This again shares the first LOGXOR and INTEGER-LENGTH calculations
;;; between both minANDs and with the ones for the low bound.
;;;
;;; LEU, 2013-04-29.

(defun logand-derive-unsigned-bounds (x y)
  (let* ((a (numeric-type-low x))
         (b (numeric-type-high x))
         (c (numeric-type-low y))
         (d (numeric-type-high y))
         (length-xor-x (integer-length (logxor a b)))
         (length-xor-y (integer-length (logxor c d))))
    (values
     (let* ((mask (zeroes (max length-xor-x length-xor-y)))
            (index (integer-length (logior mask a c))))
       (logand a c (zeroes (1- index))))
     (let* ((mask-x (ones length-xor-x))
            (mask-y (ones length-xor-y))
            (index-x (integer-length (logand mask-x b (lognot d))))
            (index-y (integer-length (logand mask-y d (lognot b)))))
       (cond ((= index-x index-y)
              ;; Both indexes are 0 here.
              (logand b d))
             ((> index-x index-y)
              (logand (logior b (ones (1- index-x))) d))
             (t
              (logand (logior d (ones (1- index-y))) b)))))))

(defun logand-derive-type-aux (x y &optional same-leaf)
  (when same-leaf
    (return-from logand-derive-type-aux x))
  (flet ((minus-one (x y)
           (when (eql (nth-value 1 (type-singleton-p x))
                      -1)
             (return-from logand-derive-type-aux y))))
    (minus-one x y)
    (minus-one y x))
  (multiple-value-bind (x-len x-pos x-neg) (integer-type-length x)
    (declare (ignore x-pos))
    (multiple-value-bind (y-len y-pos y-neg) (integer-type-length y)
      (declare (ignore y-pos))
      (if (not x-neg)
          ;; X must be positive.
          (if (not y-neg)
              ;; They must both be positive.
              (cond ((and (null x-len) (null y-len))
                     (specifier-type 'unsigned-byte))
                    ((null x-len)
                     (specifier-type `(unsigned-byte* ,y-len)))
                    ((null y-len)
                     (specifier-type `(unsigned-byte* ,x-len)))
                    (t
                     (multiple-value-bind (low high)
                         (logand-derive-unsigned-bounds x y)
                       (specifier-type `(integer ,low ,high)))))
              ;; X is positive, but Y might be negative.
              (cond ((null x-len)
                     (specifier-type 'unsigned-byte))
                    (t
                     (specifier-type `(unsigned-byte* ,x-len)))))
          ;; X might be negative.
          (if (not y-neg)
              ;; Y must be positive.
              (cond ((null y-len)
                     (specifier-type 'unsigned-byte))
                    (t (specifier-type `(unsigned-byte* ,y-len))))
              ;; Either might be negative.
              (if (and x-len y-len)
                  ;; The result is bounded.
                  (specifier-type `(signed-byte ,(1+ (max x-len y-len))))
                  ;; We can't tell squat about the result.
                  (specifier-type 'integer)))))))

(defun logior-derive-unsigned-bounds (x y)
  (let* ((a (numeric-type-low x))
         (b (numeric-type-high x))
         (c (numeric-type-low y))
         (d (numeric-type-high y))
         (length-xor-x (integer-length (logxor a b)))
         (length-xor-y (integer-length (logxor c d))))
    (values
     (let* ((mask-x (ones length-xor-x))
            (mask-y (ones length-xor-y))
            (index-x (integer-length (logand mask-x (lognot a) c)))
            (index-y (integer-length (logand mask-y (lognot c) a))))
       (cond ((= index-x index-y)
              ;; Both indexes are 0 here.
              (logior a c))
             ((> index-x index-y)
              (logior (logand a (zeroes (1- index-x))) c))
             (t
              (logior (logand c (zeroes (1- index-y))) a))))
     (let* ((mask (ones (max length-xor-x length-xor-y)))
            (index (integer-length (logand mask b d))))
       (logior b d (ones (1- index)))))))

(defun logior-derive-type-aux (x y &optional same-leaf)
  (when same-leaf
    (return-from logior-derive-type-aux x))
  (multiple-value-bind (x-len x-pos x-neg) (integer-type-length x)
    (multiple-value-bind (y-len y-pos y-neg) (integer-type-length y)
      (cond
       ((and (not x-neg) (not y-neg))
        ;; Both are positive.
        (if (and x-len y-len)
            (multiple-value-bind (low high)
                (logior-derive-unsigned-bounds x y)
              (specifier-type `(integer ,low ,high)))
            (specifier-type `(unsigned-byte* *))))
       ((not x-pos)
        ;; X must be negative.
        (if (not y-pos)
            ;; Both are negative. The result is going to be negative
            ;; and be the same length or shorter than the smaller.
            (if (and x-len y-len)
                ;; It's bounded.
                (specifier-type `(integer ,(ash -1 (min x-len y-len)) -1))
                ;; It's unbounded.
                (specifier-type '(integer * -1)))
            ;; X is negative, but we don't know about Y. The result
            ;; will be negative, but no more negative than X.
            (specifier-type
             `(integer ,(or (numeric-type-low x) '*)
                       -1))))
       (t
        ;; X might be either positive or negative.
        (if (not y-pos)
            ;; But Y is negative. The result will be negative.
            (specifier-type
             `(integer ,(or (numeric-type-low y) '*)
                       -1))
            ;; We don't know squat about either. It won't get any bigger.
            (if (and x-len y-len)
                ;; Bounded.
                (specifier-type `(signed-byte ,(1+ (max x-len y-len))))
                ;; Unbounded.
                (specifier-type 'integer))))))))

(defun logxor-derive-unsigned-bounds (x y)
  (let* ((a (numeric-type-low x))
         (b (numeric-type-high x))
         (c (numeric-type-low y))
         (d (numeric-type-high y))
         (not-b (lognot b))
         (not-d (lognot d))
         (length-xor-x (integer-length (logxor a b)))
         (length-xor-y (integer-length (logxor c d)))
         (mask (zeroes (max length-xor-x length-xor-y))))
    (values
     (let ((index-ad (integer-length (logior mask a not-d)))
           (index-bc (integer-length (logior mask not-b c))))
       (logior (logand a not-d (zeroes (1- index-ad)))
               (logand not-b c (zeroes (1- index-bc)))))
     (let ((index-ac (integer-length (logior mask a c)))
           (index-bd (integer-length (logior mask not-b not-d))))
       (lognor (logand a c (zeroes (1- index-ac)))
               (logand not-b not-d (zeroes (1- index-bd))))))))

(defun logxor-derive-type-aux (x y &optional same-leaf)
  (when same-leaf
    (return-from logxor-derive-type-aux (specifier-type '(eql 0))))
  (multiple-value-bind (x-len x-pos x-neg) (integer-type-length x)
    (multiple-value-bind (y-len y-pos y-neg) (integer-type-length y)
      (cond
        ((and (not x-neg) (not y-neg))
         ;; Both are positive
         (if (and x-len y-len)
             (multiple-value-bind (low high)
                 (logxor-derive-unsigned-bounds x y)
               (specifier-type `(integer ,low ,high)))
             (specifier-type '(unsigned-byte* *))))
        ((and (not x-pos) (not y-pos))
         ;; Both are negative.  The result will be positive, and as long
         ;; as the longer.
         (specifier-type `(unsigned-byte* ,(if (and x-len y-len)
                                               (max x-len y-len)
                                               '*))))
        ((or (and (not x-pos) (not y-neg))
             (and (not y-pos) (not x-neg)))
         ;; Either X is negative and Y is positive or vice-versa. The
         ;; result will be negative.
         (specifier-type `(integer ,(if (and x-len y-len)
                                        (ash -1 (max x-len y-len))
                                        '*)
                           -1)))
        ;; We can't tell what the sign of the result is going to be.
        ;; All we know is that we don't create new bits.
        ((and x-len y-len)
         (specifier-type `(signed-byte ,(1+ (max x-len y-len)))))
        (t
         (specifier-type 'integer))))))

(macrolet ((deffrob (logfun)
             (let ((fun-aux (symbolicate logfun "-DERIVE-TYPE-AUX")))
             `(defoptimizer (,logfun derive-type) ((x y))
                (two-arg-derive-type x y #',fun-aux #',logfun)))))
  (deffrob logand)
  (deffrob logior)
  (deffrob logxor))

(defoptimizer (logeqv derive-type) ((x y))
  (two-arg-derive-type x y (lambda (x y same-leaf)
                             (lognot-derive-type-aux
                              (logxor-derive-type-aux x y same-leaf)))
                       #'logeqv))
(defoptimizer (lognand derive-type) ((x y))
  (two-arg-derive-type x y (lambda (x y same-leaf)
                             (lognot-derive-type-aux
                              (logand-derive-type-aux x y same-leaf)))
                       #'lognand))
(defoptimizer (lognor derive-type) ((x y))
  (two-arg-derive-type x y (lambda (x y same-leaf)
                             (lognot-derive-type-aux
                              (logior-derive-type-aux x y same-leaf)))
                       #'lognor))
(defoptimizer (logandc1 derive-type) ((x y))
  (two-arg-derive-type x y (lambda (x y same-leaf)
                             (if same-leaf
                                 (specifier-type '(eql 0))
                                 (logand-derive-type-aux
                                  (lognot-derive-type-aux x) y)))
                       #'logandc1))
(defoptimizer (logandc2 derive-type) ((x y))
  (two-arg-derive-type x y (lambda (x y same-leaf)
                             (if same-leaf
                                 (specifier-type '(eql 0))
                                 (logand-derive-type-aux
                                  x (lognot-derive-type-aux y))))
                       #'logandc2))
(defoptimizer (logorc1 derive-type) ((x y))
  (two-arg-derive-type x y (lambda (x y same-leaf)
                             (if same-leaf
                                 (specifier-type '(eql -1))
                                 (logior-derive-type-aux
                                  (lognot-derive-type-aux x) y)))
                       #'logorc1))
(defoptimizer (logorc2 derive-type) ((x y))
  (two-arg-derive-type x y (lambda (x y same-leaf)
                             (if same-leaf
                                 (specifier-type '(eql -1))
                                 (logior-derive-type-aux
                                  x (lognot-derive-type-aux y))))
                       #'logorc2))
