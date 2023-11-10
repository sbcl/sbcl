(in-package "SB-ROTATE-BYTE")

;;; Ensure we don't bug out with an identity rotation.
(assert (= (rotate-byte 0 (byte 32 0) 3) 3))

(assert (= (rotate-byte 3 (byte 32 0) 3) 24))
(assert (= (rotate-byte 3 (byte 16 0) 3) 24))
(assert (= (rotate-byte 3 (byte 2 0) 3) 3))
(assert (= (rotate-byte 3 (byte 5 5) 3) 3))
(assert (= (rotate-byte 6 (byte 8 0) -3) -129))

(flet ((opaque-identity (x) x))
  (declare (notinline opaque-identity))
  (assert (= (rotate-byte 3 (opaque-identity (byte 32 0)) 3) 24))
  (assert (= (rotate-byte 3 (opaque-identity (byte 16 0)) 3) 24))
  (assert (= (rotate-byte 3 (opaque-identity (byte 2 0)) 3) 3))
  (assert (= (rotate-byte 3 (opaque-identity (byte 5 5)) 3) 3))
  (assert (= (rotate-byte 6 (opaque-identity (byte 8 0)) -3) -129)))

(defun pfixnum/c (integer)
  (declare (type (unsigned-byte 29) integer))
  (rotate-byte 5 (byte 32 0) integer))

(assert (= (pfixnum/c 5) 160))
(assert (= (pfixnum/c 1) 32))
(assert (= (pfixnum/c (ash 1 26)) (ash 1 31)))
(assert (= (pfixnum/c (ash 1 27)) 1))

(defun pfixnum (count integer)
  (declare (type (unsigned-byte 29) integer)
           (type (integer -31 31) count))
  (rotate-byte count (byte 32 0) integer))

(assert (= (pfixnum 5 5) 160))
(assert (= (pfixnum 5 1) 32))
(assert (= (pfixnum 5 (ash 1 26)) (ash 1 31)))
(assert (= (pfixnum 5 (ash 1 27)) 1))

(defun ub32/c (integer)
  (declare (type (unsigned-byte 32) integer))
  (rotate-byte 5 (byte 32 0) integer))

(assert (= (ub32/c 5) 160))
(assert (= (ub32/c 1) 32))
(assert (= (ub32/c (ash 1 26)) (ash 1 31)))
(assert (= (ub32/c (ash 1 27)) 1))

(defun ub32/-c (integer)
  (declare (type (unsigned-byte 32) integer))
  (rotate-byte -5 (byte 32 0) integer))

(assert (= (ub32/-c 320) 10))
(assert (= (ub32/-c 64) 2))
(assert (= (ub32/-c (ash 1 31)) (ash 1 26)))
(assert (= (ub32/-c 1) (ash 1 27)))

(defun ub32 (count integer)
  (declare (type (unsigned-byte 32) integer)
           (type (integer -31 31) count))
  (rotate-byte count (byte 32 0) integer))

(assert (= (ub32 5 5) 160))
(assert (= (ub32 5 1) 32))
(assert (= (ub32 5 (ash 1 26)) (ash 1 31)))
(assert (= (ub32 5 (ash 1 27)) 1))

;;; test with (contrived) register pressure on the x86 to ensure that the
;;; rotatee doesn't get clobbered by the count.

(defun ub32-reg-pressure (count integer)
  (declare (type (unsigned-byte 32) integer)
           (type (integer -31 31) count))
  (rotate-byte count (byte 32 0) (ldb (byte 32 0) (+ (* 67 count)
                                                     integer))))

(assert (= (ub32-reg-pressure 1 5) 144))
(assert (= (ub32-reg-pressure 5 5) 10880))
(assert (= (ub32-reg-pressure 5 (ash 1 26)) 2147494368))
(assert (= (ub32-reg-pressure 5 (ash 1 27)) 10721))

(defun ub64/c (integer)
  (declare (type (unsigned-byte 64) integer))
  (rotate-byte 6 (byte 64 0) integer))

(defun ub64/-c (integer)
  (declare (type (unsigned-byte 64) integer))
  (rotate-byte -6 (byte 64 0) integer))

(assert (= (ub64/-c 320) 5))
(assert (= (ub64/-c 64) 1))
(assert (= (ub64/-c (ash 1 63)) (ash 1 57)))
(assert (= (ub64/-c 1) (ash 1 58)))

(defun ub64 (count integer)
  (declare (type (unsigned-byte 64) integer)
           (type (integer -63 63) count))
  (rotate-byte count (byte 64 0) integer))

(assert (= (ub64 6 5) 320))
(assert (= (ub64 6 1) 64))
(assert (= (ub64 6 (ash 1 57)) (ash 1 63)))
(assert (= (ub64 6 (ash 1 58)) 1))

(defun non-zero-posn (c x)
  (declare ((unsigned-byte 8) x))
  (typep (rotate-byte c (byte 8 3) x) '(unsigned-byte 8)))
(assert (not (non-zero-posn 1 #xFF)))

(declaim (notinline rb))
(defun rb (c s p i)
  (rotate-byte c (byte s p) i))

(dolist (x '(#x5AE12D96 #x-5AE12D96))
  (dotimes (p 64)
    (assert (= (rb 1 32 p x)
               (let ((chunk (ldb (byte 32 p) x)))
                 (dpb (logand (logior (ash chunk 1) (ash chunk -31)) #xffffffff)
                      (byte 32 p)
                      x))))))
(defconstant +large+
  (min 100000000000
       (ash most-positive-fixnum -1)))

(assert (= (rotate-byte 1 (byte 2 +large+) 17) 17))
(assert (= (rotate-byte 2 (byte 13 +large+) (1- most-negative-fixnum))
           (1- most-negative-fixnum)))

(defun enormous-byte-rotate (c)
  (declare (type (signed-byte 32) c))
  (rotate-byte 2 (byte 3 +large+) c))

(assert (= (enormous-byte-rotate #x7fffffff) #x7fffffff))
(assert (= (enormous-byte-rotate #x-80000000) #x-80000000))
