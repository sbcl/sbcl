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

(in-package "SB!C")

;;; Return the maximum number of bits an integer of the supplied type
;;; can take up, or NIL if it is unbounded. The second (third) value
;;; is T if the integer can be positive (negative) and NIL if not.
;;; Zero counts as positive.
(defun integer-type-length (type)
  (if (numeric-type-p type)
      (let ((min (numeric-type-low type))
            (max (numeric-type-high type)))
        (values (and min max (max (integer-length min) (integer-length max)))
                (or (null max) (not (minusp max)))
                (or (null min) (minusp min))))
      (values nil t t)))

;;; See _Hacker's Delight_, Henry S. Warren, Jr. pp 58-63 for an
;;; explanation of LOG{AND,IOR,XOR}-DERIVE-UNSIGNED-{LOW,HIGH}-BOUND.
;;; Credit also goes to Raymond Toy for writing (and debugging!) similar
;;; versions in CMUCL, from which these functions copy liberally.

(defun logand-derive-unsigned-low-bound (x y)
  (let ((a (numeric-type-low x))
        (b (numeric-type-high x))
        (c (numeric-type-low y))
        (d (numeric-type-high y)))
    (loop for m = (ash 1 (integer-length (lognor a c))) then (ash m -1)
          until (zerop m) do
          (unless (zerop (logand m (lognot a) (lognot c)))
            (let ((temp (logandc2 (logior a m) (1- m))))
              (when (<= temp b)
                (setf a temp)
                (loop-finish))
              (setf temp (logandc2 (logior c m) (1- m)))
              (when (<= temp d)
                (setf c temp)
                (loop-finish))))
          finally (return (logand a c)))))

(defun logand-derive-unsigned-high-bound (x y)
  (let ((a (numeric-type-low x))
        (b (numeric-type-high x))
        (c (numeric-type-low y))
        (d (numeric-type-high y)))
    (loop for m = (ash 1 (integer-length (logxor b d))) then (ash m -1)
          until (zerop m) do
          (cond
            ((not (zerop (logand b (lognot d) m)))
             (let ((temp (logior (logandc2 b m) (1- m))))
               (when (>= temp a)
                 (setf b temp)
                 (loop-finish))))
            ((not (zerop (logand (lognot b) d m)))
             (let ((temp (logior (logandc2 d m) (1- m))))
               (when (>= temp c)
                 (setf d temp)
                 (loop-finish)))))
          finally (return (logand b d)))))

(defun logand-derive-type-aux (x y &optional same-leaf)
  (when same-leaf
    (return-from logand-derive-type-aux x))
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
                     (let ((low (logand-derive-unsigned-low-bound x y))
                           (high (logand-derive-unsigned-high-bound x y)))
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

(defun logior-derive-unsigned-low-bound (x y)
  (let ((a (numeric-type-low x))
        (b (numeric-type-high x))
        (c (numeric-type-low y))
        (d (numeric-type-high y)))
    (loop for m = (ash 1 (integer-length (logxor a c))) then (ash m -1)
          until (zerop m) do
          (cond
            ((not (zerop (logandc2 (logand c m) a)))
             (let ((temp (logand (logior a m) (1+ (lognot m)))))
               (when (<= temp b)
                 (setf a temp)
                 (loop-finish))))
            ((not (zerop (logandc2 (logand a m) c)))
             (let ((temp (logand (logior c m) (1+ (lognot m)))))
               (when (<= temp d)
                 (setf c temp)
                 (loop-finish)))))
          finally (return (logior a c)))))

(defun logior-derive-unsigned-high-bound (x y)
  (let ((a (numeric-type-low x))
        (b (numeric-type-high x))
        (c (numeric-type-low y))
        (d (numeric-type-high y)))
    (loop for m = (ash 1 (integer-length (logand b d))) then (ash m -1)
          until (zerop m) do
          (unless (zerop (logand b d m))
            (let ((temp (logior (- b m) (1- m))))
              (when (>= temp a)
                (setf b temp)
                (loop-finish))
              (setf temp (logior (- d m) (1- m)))
              (when (>= temp c)
                (setf d temp)
                (loop-finish))))
          finally (return (logior b d)))))

(defun logior-derive-type-aux (x y &optional same-leaf)
  (when same-leaf
    (return-from logior-derive-type-aux x))
  (multiple-value-bind (x-len x-pos x-neg) (integer-type-length x)
    (multiple-value-bind (y-len y-pos y-neg) (integer-type-length y)
      (cond
       ((and (not x-neg) (not y-neg))
        ;; Both are positive.
        (if (and x-len y-len)
            (let ((low (logior-derive-unsigned-low-bound x y))
                  (high (logior-derive-unsigned-high-bound x y)))
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

(defun logxor-derive-unsigned-low-bound (x y)
  (let ((a (numeric-type-low x))
        (b (numeric-type-high x))
        (c (numeric-type-low y))
        (d (numeric-type-high y)))
    (loop for m = (ash 1 (integer-length (logxor a c))) then (ash m -1)
          until (zerop m) do
          (cond
            ((not (zerop (logandc2 (logand c m) a)))
             (let ((temp (logand (logior a m)
                                 (1+ (lognot m)))))
               (when (<= temp b)
                 (setf a temp))))
            ((not (zerop (logandc2 (logand a m) c)))
             (let ((temp (logand (logior c m)
                                 (1+ (lognot m)))))
               (when (<= temp d)
                 (setf c temp)))))
          finally (return (logxor a c)))))

(defun logxor-derive-unsigned-high-bound (x y)
  (let ((a (numeric-type-low x))
        (b (numeric-type-high x))
        (c (numeric-type-low y))
        (d (numeric-type-high y)))
    (loop for m = (ash 1 (integer-length (logand b d))) then (ash m -1)
          until (zerop m) do
          (unless (zerop (logand b d m))
            (let ((temp (logior (- b m) (1- m))))
              (cond
                ((>= temp a) (setf b temp))
                (t (let ((temp (logior (- d m) (1- m))))
                     (when (>= temp c)
                       (setf d temp)))))))
          finally (return (logxor b d)))))

(defun logxor-derive-type-aux (x y &optional same-leaf)
  (when same-leaf
    (return-from logxor-derive-type-aux (specifier-type '(eql 0))))
  (multiple-value-bind (x-len x-pos x-neg) (integer-type-length x)
    (multiple-value-bind (y-len y-pos y-neg) (integer-type-length y)
      (cond
        ((and (not x-neg) (not y-neg))
         ;; Both are positive
         (if (and x-len y-len)
             (let ((low (logxor-derive-unsigned-low-bound x y))
                   (high (logxor-derive-unsigned-high-bound x y)))
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
                                  (lognot-derive-type-aux x) y nil)))
                       #'logandc1))
(defoptimizer (logandc2 derive-type) ((x y))
  (two-arg-derive-type x y (lambda (x y same-leaf)
                             (if same-leaf
                                 (specifier-type '(eql 0))
                                 (logand-derive-type-aux
                                  x (lognot-derive-type-aux y) nil)))
                       #'logandc2))
(defoptimizer (logorc1 derive-type) ((x y))
  (two-arg-derive-type x y (lambda (x y same-leaf)
                             (if same-leaf
                                 (specifier-type '(eql -1))
                                 (logior-derive-type-aux
                                  (lognot-derive-type-aux x) y nil)))
                       #'logorc1))
(defoptimizer (logorc2 derive-type) ((x y))
  (two-arg-derive-type x y (lambda (x y same-leaf)
                             (if same-leaf
                                 (specifier-type '(eql -1))
                                 (logior-derive-type-aux
                                  x (lognot-derive-type-aux y) nil)))
                       #'logorc2))
