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

;;; tests of COUNT
(assert (= 1 (count 1 '(1 2 3))))
(assert (= 2 (count 'z #(z 1 2 3 z))))
(assert (= 0 (count 'y '(z 1 2 3 z))))

;;; tests of COUNT-IF and COUNT-IF-NOT
(macrolet (;; the guts of CCI, abstracted over whether we're testing
	   ;; COUNT-IF or COUNT-IF-NOT
	   (%cci (expected count-if test sequence-as-list &rest keys)
             `(let* ((list ',sequence-as-list)
		     (simple-vector (coerce list 'simple-vector))
		     (length (length list))
		     (vector (make-array (* 2 length) :fill-pointer length)))
		(replace vector list :end1 length)
		(dolist (seq (list list simple-vector vector))
		  (assert (= ,expected (,count-if ,test seq ,@keys))))))
	   ;; "Check COUNT-IF"
	   (cci (expected test sequence-as-list &rest keys) 
	     `(progn
                (format t "~&SEQUENCE-AS-LIST=~S~%" ',sequence-as-list)
		(%cci ,expected
		      count-if
		      ,test
		      ,sequence-as-list
		      ,@keys)
		(%cci ,expected
		      count-if-not
		      (complement ,test)
		      ,sequence-as-list
		      ,@keys))))
  (cci 1 #'consp (1 (12) 1))
  (cci 3 #'consp (1 (2) 3 (4) (5) 6))
  (cci 3 #'consp (1 (2) 3 (4) (5) 6) :from-end t)
  (cci 2 #'consp (1 (2) 3 (4) (5) 6) :start 2)
  (cci 0 #'consp (1 (2) 3 (4) (5) 6) :start 2 :end 3)
  (cci 1 #'consp (1 (2) 3 (4) (5) 6) :start 1 :end 3)
  (cci 1 #'consp (1 (2) 3 (4) (5) 6) :start 1 :end 2)
  (cci 0 #'consp (1 (2) 3 (4) (5) 6) :start 2 :end 2)
  (cci 2 #'zerop (0 10 0 11 12))
  (cci 1 #'zerop (0 10 0 11 12) :start 1)
  (cci 2 #'minusp (0 10 0 11 12) :key #'1-)
  (cci 1 #'minusp (0 10 0 11 12) :key #'1- :end 2))
(multiple-value-bind (v e)
    (ignore-errors (count-if #'zerop '(0 a 0 b c) :start 1))
  (declare (ignore v))
  (assert (eql (type-error-datum e) 'a)))
(multiple-value-bind (v e)
    (ignore-errors (count-if #'zerop #(0 a 0 b c) :start 1 :from-end 11))
  (declare (ignore v))
  (assert (eql (type-error-datum e) 'c)))

;;; :COUNT may be negative and BIGNUM
(assert (equal (remove 1 '(1 2 3 1) :count 1) '(2 3 1)))
(assert (equal (remove 1 '(1 2 3 1) :count (* 2 most-positive-fixnum)) '(2 3)))
(assert (equal (remove 1 '(1 2 3 1) :count (* -2 most-positive-fixnum)) '(1 2 3 1)))
