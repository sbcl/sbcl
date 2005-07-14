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

(cl:in-package :cl-user)

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
             (assert (raises-error? (vector-push-extend #\b simple-t))))))

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
  (assert (not bool)))
