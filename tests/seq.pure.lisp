;;;; tests related to sequences

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

;;; As reported by Paul Dietz from his ansi-test suite for gcl, REMOVE
;;; malfunctioned when given :START, :END and :FROM-END arguments.
;;; Make sure it doesn't happen again.
(let* ((orig '(1 2 3 2 6 1 2 4 1 3 2 7))
       (x (copy-seq orig))
       (y (remove 3 x :from-end t :start 1 :end 5))
       (z (remove 2 x :from-end t :start 1 :end 5)))
  (assert (equalp orig x))
  (assert (equalp y '(1 2 2 6 1 2 4 1 3 2 7)))
  (assert (equalp z '(1 3 6 1 2 4 1 3 2 7))))

;;; Similarly, NSUBSTITUTE and friends were getting things wrong with
;;; :START, :END and :FROM-END:
(assert
 (loop for i from 0 to 9 always
       (loop for j from i to 10 always
	     (loop for c from 0 to (- j i) always
		   (let* ((orig '(a a a a a a a a a a))
			  (x (copy-seq orig))
			  (y (nsubstitute 'x 'a x :start i :end j :count c)))
		     (equal y (nconc (make-list i :initial-element 'a)
				     (make-list c :initial-element 'x)
				     (make-list (- 10 (+ i c))
						:initial-element 'a))))))))

(assert
 (loop for i from 0 to 9 always
       (loop for j from i to 10 always
	     (loop for c from 0 to (- j i) always
		   (let* ((orig '(a a a a a a a a a a))
			  (x (copy-seq orig))
			  (y (nsubstitute-if 'x (lambda (x) (eq x 'a)) x
					     :start i :end j
					     :count c :from-end t)))
		     (equal y (nconc (make-list (- j c) :initial-element 'a)
				     (make-list c :initial-element 'x)
				     (make-list (- 10 j)
						:initial-element 'a))))))))
(assert
 (loop for i from 0 to 9 always
       (loop for j from i to 10 always
	     (loop for c from 0 to (- j i) always
		   (let* ((orig '(a a a a a a a a a a))
			  (x (copy-seq orig))
			  (y (nsubstitute-if-not 'x (lambda (x)
						      (not (eq x 'a))) x
						 :start i :end j
						 :count c :from-end t)))
		     (equal y (nconc (make-list (- j c) :initial-element 'a)
				     (make-list c :initial-element 'x)
				     (make-list (- 10 j)
						:initial-element 'a))))))))

