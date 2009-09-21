;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(in-package :cl-user)

;;; +MAGIC-HASH-VECTOR-VALUE+ is used to mark empty entries in the slot
;;; HASH-VECTOR of hash tables. It must be a value outside of the range
;;; of SXHASH. The range of SXHASH is the non-negative fixnums.
(assert (not (typep sb-impl::+magic-hash-vector-value+
                    '(and fixnum unsigned-byte))))

;;; The return value of SXHASH on non-string/bitvector arrays should not
;;; change when the contents of the array change.
(let* ((a (make-array '(1) :initial-element 1))
       (sxhash (sxhash a))
       (hash (make-hash-table :test 'equal)))
  (setf (gethash a hash) t)
  (setf (aref a 0) 0)
  (assert (= sxhash (sxhash a)))
  ;; Need to make another access to the hash to disable the last-seen-element
  ;; cache.
  (setf (gethash 'y hash) t)
  (assert (gethash a hash)))

;;; Minimum quality checks
(assert (/= (sxhash "foo") (sxhash "bar")))
(assert (/= (sxhash (pathname "foo.txt")) (sxhash (pathname "bar.txt"))))
(assert (/= (sxhash (list 1 2 3)) (sxhash (list 3 2 1))))
(assert (/= (sxhash #*1010) (sxhash #*0101)))
