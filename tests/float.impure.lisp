;;;; This file is for floating-point-related tests which have side
;;;; effects (e.g. executing DEFUN).

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

;;; Hannu Rummukainen reported a CMU CL bug on cmucl-imp@cons.org 26
;;; Jun 2000. This is the test case for it.
;;;
;;; The bug was listed as "39: .. Probably the same bug exists in
;;; SBCL" for a while until Martin Atzmueller showed that it's not
;;; present after all, presumably because the bug was introduced into
;;; CMU CL after the fork. But we'll test for it anyway, in case
;;; e.g. someone inadvertently ports the bad code.
(defun point39 (x y)
  (make-array 2
	      :element-type 'double-float
              :initial-contents (list x y)))

(declaim (inline point39-x point39-y))
(defun point39-x (p)
  (declare (type (simple-array double-float (2)) p))
  (aref p 0))
(defun point39-y (p)
  (declare (type (simple-array double-float (2)) p))
  (aref p 1))
(defun order39 (points)
  (sort points  (lambda (p1 p2)
		  (let* ((y1 (point39-y p1))
			 (y2 (point39-y p2)))
		    (if (= y1 y2)
			(< (point39-x p1)
			   (point39-x p2))
			(< y1 y2))))))
(defun test39 ()
  (order39 (make-array 4
		       :initial-contents (list (point39 0.0d0 0.0d0)
					       (point39 1.0d0 1.0d0)
					       (point39 2.0d0 2.0d0)
					       (point39 3.0d0 3.0d0)))))
(assert (equalp (test39)
		#(#(0.0d0 0.0d0)
		  #(1.0d0 1.0d0)
		  #(2.0d0 2.0d0)
		  #(3.0d0 3.0d0))))

(defun complex-double-float-ppc (x y)
  (declare (type (complex double-float) x y))
  (declare (optimize speed))
  (+ x y))
(compile 'complex-double-float-ppc)
(assert (= (complex-double-float-ppc #c(0.0d0 1.0d0) #c(2.0d0 3.0d0))
           #c(2.0d0 4.0d0)))

(defun single-float-ppc (x)
  (declare (type (signed-byte 32) x) (optimize speed))
  (float x 1f0))
(compile 'single-float-ppc)
(assert (= (single-float-ppc -30) -30f0))

;;; success
(quit :unix-status 104)
