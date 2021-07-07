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

(with-test (:name :length)
    (funcall (lambda ()
               (let ((simple-t (make-array 35))
                     (simple-u32 (make-array 50
                                             :element-type '(unsigned-byte 32)))
                     (simple-character (make-string 44))
                     (complex-t (make-array 4 :fill-pointer 3))
                     (complex-u32 (make-array 88
                                              :adjustable t
                                              :element-type '(unsigned-byte 32)))
                     (complex-character (make-array 14
                                                    :element-type 'character
                                                    :fill-pointer t)))
                 (assert (= (length simple-t) 35))
                 (assert (= (length simple-u32) 50))
                 (assert (= (length simple-character) 44))
                 (assert (= (length complex-t) 3))
                 (assert (= (length complex-u32) 88))
                 (assert (= (length complex-character) 14))
                 (vector-push-extend #\a complex-t)
                 (assert (= (length complex-t) 4))
                 (assert-error (vector-push-extend #\b simple-t))))))

(with-test (:name :fill-pointer)
  (multiple-value-bind (fp1 index fp2 bool)
      (let ((a (make-array '(5) :fill-pointer 5 :adjustable 5
                                :initial-contents '(a b c d e))))
        (values (fill-pointer a)
                (vector-push-extend 'x a)
                (fill-pointer a)
                (<= (array-total-size a) 5)))
    (assert (= fp1 5))
    (assert (= index 5))
    (assert (= fp2 6))
    (assert (not bool))))

(with-test (:name :svref-unknown-type)
  (compile nil `(lambda (a)
                  (declare ((vector undefined-type) a))
                  (svref a 0)))
  (compile nil `(lambda (a)
                  (declare ((vector undefined-type) a))
                  (setf (svref a 0) 10))))

(with-test (:name :svref-negative-index)
  (let ((vector #(1)))
    (flet ((test (index)
             (funcall (compile nil `(lambda (vector index)
                                      (svref vector index)))
                      vector index)))
      (assert-error (test -1))
      (assert (= (test 0) 1))
      (assert-error (test 1)))))

(with-test (:name :fill-pointer-transform)
  (assert-error
   (funcall (checked-compile `(lambda (x)
                                (setf (fill-pointer x) 0)))
            (make-array 2 :adjustable t))
   type-error))

(with-test (:name :concatenate-to-vector)
  (assert (sb-kernel:%concatenate-to-vector sb-vm:simple-bit-vector-widetag
                                           '(1 1) '(1 0))))
