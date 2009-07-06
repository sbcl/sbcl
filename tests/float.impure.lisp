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

;;; constant-folding irrational functions
(declaim (inline df))
(defun df (x)
  ;; do not remove the ECASE here: the bug this checks for indeed
  ;; depended on this configuration
  (ecase x (1 least-positive-double-float)))
(macrolet ((test (fun)
             (let ((name (intern (format nil "TEST-CONSTANT-~A" fun))))
               `(progn
                  (defun ,name () (,fun (df 1)))
                  (,name)))))
  (test sqrt)
  (test log)
  (test sin)
  (test cos)
  (test tan)
  (test asin)
  (test acos)
  (test atan)
  (test sinh)
  (test cosh)
  (test tanh)
  (test asinh)
  (test acosh)
  (test atanh)
  (test exp))

;;; Broken move-arg-double-float for non-rsp frame pointers on x86-64
(defun test (y)
  (declare (optimize speed))
  (multiple-value-bind (x)
      (labels ((aux (x)
                 (declare (double-float x))
                 (etypecase y
                   (double-float
                    nil)
                   (fixnum
                    (aux x))
                   (complex
                    (format t "y=~s~%" y)))
                 (values x)))
        (aux 2.0d0))
    x))

(assert (= (test 1.0d0) 2.0d0))

(deftype myarraytype (&optional (length '*))
  `(simple-array double-float (,length)))
(defun new-pu-label-from-pu-labels (array)
  (setf (aref (the myarraytype array) 0)
        sb-ext:double-float-positive-infinity))

;;; bug 407
;;;
;;; FIXME: it may be that TYPE-ERROR is wrong, and we should
;;; instead signal an overflow or coerce into an infinity.
(defun bug-407a ()
  (loop for n from (expt 2 1024) upto (+ 10 (expt 2 1024))
        do (handler-case
               (coerce n 'single-float)
             (simple-type-error ()
               (return-from bug-407a :type-error)))))
(assert (eq :type-error (bug-407a)))
(defun bug-407b ()
  (loop for n from (expt 2 1024) upto (+ 10 (expt 2 1024))
        do (handler-case
               (format t "~E~%" (coerce n 'single-float))
             (simple-type-error ()
               (return-from bug-407b :type-error)))))
(assert (eq :type-error (bug-407b)))

;; 1.0.29.44 introduces a ton of changes for complex floats
;; on x86-64. Huge test of doom to help catch weird corner
;; cases.
;; Abuse the framework to also test some float arithmetic
;; changes wrt constant arguments in 1.0.29.54.
(defmacro def-compute (name real-type
                       &optional (complex-type `(complex ,real-type)))
  `(defun ,name (x y r)
     (declare (type ,complex-type x y)
              (type ,real-type r))
     (flet ((reflections (x)
              (values x
                      (conjugate x)
                      (complex (- (realpart x)) (imagpart x))
                      (- x)))
            (compute (x y r)
              (declare (type ,complex-type x y)
                       (type ,real-type r))
              (list (1+ x) (* 2 x) (/ x 2) (= 1 x)
                    (+ x y) (+ r x) (+ x r)
                    (- x y) (- r x) (- x r)
                    (* x y) (* x r) (* r x)
                    (unless (zerop y)
                      (/ x y))
                    (unless (zerop r)
                      (/ x r))
                    (unless (zerop x)
                      (/ r x))
                    (conjugate x) (conjugate r)
                    (abs r) (- r) (= 1 r)
                    (- x) (1+ r) (* 2 r) (/ r 2)
                    (complex r) (complex r r) (complex 0 r)
                    (= x y) (= r x) (= y r) (= x (complex 0 r))
                    (= r (realpart x)) (= (realpart x) r)
                    (> r (realpart x)) (< r (realpart x))
                    (> (realpart x) r) (< (realpart x) r)
                    (eql x y) (eql x (complex r)) (eql y (complex r))
                    (eql x (complex r r)) (eql y (complex 0 r))
                    (eql r (realpart x)) (eql (realpart x) r))))
       (declare (inline reflections))
       (multiple-value-bind (x1 x2 x3 x4) (reflections x)
         (multiple-value-bind (y1 y2 y3 y4) (reflections y)
           #.(let ((form '(list)))
               (dolist (x '(x1 x2 x3 x4) (reverse form))
                 (dolist (y '(y1 y2 y3 y4))
                   (push `(list ,x ,y r
                                (append (compute ,x ,y r)
                                        (compute ,x ,y (- r))))
                         form)))))))))

(def-compute compute-number real number)
(def-compute compute-single single-float)
(def-compute compute-double double-float)

(labels ((equal-enough (x y)
           (cond ((eql x y))
                 ((or (complexp x)
                      (complexp y))
                  (or (eql (coerce x '(complex double-float))
                           (coerce y '(complex double-float)))
                      (and (equal-enough (realpart x) (realpart y))
                           (equal-enough (imagpart x) (imagpart y)))))
                 ((numberp x)
                  (or (eql (coerce x 'double-float) (coerce y 'double-float))
                      (< (abs (- x y))  1d-5))))))
  (let* ((reals     '(0 1 2))
         (complexes '#.(let ((reals '(0 1 2))
                             (cpx   '()))
                         (dolist (x reals (nreverse cpx))
                           (dolist (y reals)
                             (push (complex x y) cpx))))))
    (declare (notinline every))
    (dolist (r reals)
      (dolist (x complexes)
        (dolist (y complexes)
          (let ((value  (compute-number x y r))
                (single (compute-single (coerce x '(complex single-float))
                                        (coerce y '(complex single-float))
                                        (coerce r 'single-float)))
                (double (compute-double (coerce x '(complex double-float))
                                        (coerce y '(complex double-float))
                                        (coerce r 'double-float))))
            (assert (every (lambda (pos ref single double)
                             (declare (ignorable pos))
                             (every (lambda (ref single double)
                                      (or (and (equal-enough ref single)
                                               (equal-enough ref double))
                                          (and (not (numberp single)) ;; -ve 0s
                                               (equal-enough single double))))
                                    (fourth ref) (fourth single) (fourth double)))
                           '((0 0) (0 1) (0 2) (0 3)
                             (1 0) (1 1) (1 2) (1 3)
                             (2 0) (2 1) (2 2) (2 3)
                             (3 0) (3 1) (3 2) (3 3))
                           value single double))))))))
