(in-package "SB-ROTATE-BYTE")

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

(defun ub32 (count integer)
  (declare (type (unsigned-byte 32) integer)
	   (type (integer -31 31) count))
  (rotate-byte count (byte 32 0) integer))

(assert (= (ub32 5 5) 160))
(assert (= (ub32 5 1) 32))
(assert (= (ub32 5 (ash 1 26)) (ash 1 31)))
(assert (= (ub32 5 (ash 1 27)) 1))
