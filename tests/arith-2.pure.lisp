;;;; arithmetic tests with side effects

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

(defmacro define-compiled-fun (fun name)
  `(progn
    (declaim (notinline ,name))
    (defun ,name (&rest args)
     (declare (optimize safety))
     (case (length args)
       (1 (,fun (car args)))
       (2 (,fun (car args) (cadr args)))
       (t (apply #',fun args))))))

(define-compiled-fun min compiled-min)
(define-compiled-fun max compiled-max)
(define-compiled-fun + compiled-+)
(define-compiled-fun * compiled-*)
(define-compiled-fun logand compiled-logand)
(define-compiled-fun logior compiled-logior)
(define-compiled-fun logxor compiled-logxor)

(assert (null (ignore-errors (compiled-min '(1 2 3)))))
(assert (= (compiled-min -1) -1))
(assert (null (ignore-errors (compiled-min 1 #(1 2 3)))))
(assert (= (compiled-min 10 11) 10))
(assert (null (ignore-errors (compiled-min (find-package "CL") -5.0))))
(assert (= (compiled-min 5.0 -3) -3))
(assert (null (ignore-errors (compiled-max #c(4 3)))))
(assert (= (compiled-max 0) 0))
(assert (null (ignore-errors (compiled-max "MIX" 3))))
(assert (= (compiled-max -1 10.0) 10.0))
(assert (null (ignore-errors (compiled-max 3 #'max))))
(assert (= (compiled-max -3 0) 0))

(assert (null (ignore-errors (compiled-+ "foo"))))
(assert (= (compiled-+ 3f0) 3f0))
(assert (null (ignore-errors (compiled-+ 1 #p"tmp"))))
(assert (= (compiled-+ 1 2) 3))
(assert (null (ignore-errors (compiled-+ '(1 2 3) 3))))
(assert (= (compiled-+ 3f0 4f0) 7f0))
(assert (null (ignore-errors (compiled-* "foo"))))
(assert (= (compiled-* 3f0) 3f0))
(assert (null (ignore-errors (compiled-* 1 #p"tmp"))))
(assert (= (compiled-* 1 2) 2))
(assert (null (ignore-errors (compiled-* '(1 2 3) 3))))
(assert (= (compiled-* 3f0 4f0) 12f0))

(assert (null (ignore-errors (compiled-logand #(1)))))
(assert (= (compiled-logand 1) 1))
(assert (null (ignore-errors (compiled-logior 3f0))))
(assert (= (compiled-logior 4) 4))
(assert (null (ignore-errors (compiled-logxor #c(2 3)))))
(assert (= (compiled-logxor -6) -6))

(with-test (:name (coerce :overflow))
  (checked-compile-and-assert
      ()
      '(lambda (n) (coerce n 'single-float))
    (((expt 10 1000)) (condition 'floating-point-overflow))))

(defun are-we-getting-ash-right (x y)
  (declare (optimize speed)
           (type (unsigned-byte 32) x)
           (type (integer -40 0) y))
  (ash x y))
(defun what-about-with-constants (x)
  (declare (optimize speed) (type (unsigned-byte 32) x))
  (ash x -32))

(dotimes (i 41)
  (assert (= (are-we-getting-ash-right (1- (ash 1 32)) (- i))
             (if (< i 32)
                 (1- (ash 1 (- 32 i)))
                 0))))
(assert (= (what-about-with-constants (1- (ash 1 32))) 0))

(defun one-more-test-case-to-catch-sparc (x y)
  (declare (optimize speed (safety 0))
           (type (unsigned-byte 32) x) (type (integer -40 2) y))
  (the (unsigned-byte 32) (ash x y)))
(assert (= (one-more-test-case-to-catch-sparc (1- (ash 1 32)) -40) 0))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *n-fixnum-bits* (- sb-vm:n-word-bits sb-vm::n-fixnum-tag-bits))
  (defvar *shifts* (let ((list (list 0
                                     1
                                     (1- sb-vm:n-word-bits)
                                     sb-vm:n-word-bits
                                     (1+ sb-vm:n-word-bits))))
                     (append list (mapcar #'- list)))))

(macrolet ((nc-list ()
             `(list ,@(loop for i from 0 below (length *shifts*)
                         collect `(frob (nth ,i *shifts*)))))
           (c-list ()
             `(list ,@(loop for i from 0 below (length *shifts*)
                         collect `(frob ,(nth i *shifts*))))))
  (defun nc-ash (x)
    (macrolet ((frob (y)
                 `(list x ,y (ash x ,y))))
      (nc-list)))
  (defun c-ash (x)
    (macrolet ((frob (y)
                 `(list x ,y (ash x ,y))))
      (c-list)))
  (defun nc-modular-ash-ub (x)
    (macrolet ((frob (y)
                 `(list x ,y (logand most-positive-fixnum (ash x ,y)))))
      (nc-list)))
  (defun c-modular-ash-ub (x)
    (declare (type (and fixnum unsigned-byte) x)
             (optimize speed))
    (macrolet ((frob (y)
                 `(list x ,y (logand most-positive-fixnum (ash x ,y)))))
      (c-list))))

(let* ((values (list 0 1 most-positive-fixnum))
       (neg-values (cons most-negative-fixnum
                         (mapcar #'- values))))
  (labels ((test (value fun1 fun2)
             (let ((res1 (funcall fun1 value))
                   (res2 (funcall fun2 value)))
               (mapcar (lambda (a b)
                         (unless (equalp a b)
                           (error "ash failure for ~A vs ~A: ~A not EQUALP ~A"
                                  fun1 fun2
                                  a b)))
                       res1 res2))))
    (loop for x in values do
         (test x 'nc-ash 'c-ash)
         (test x 'nc-modular-ash-ub 'c-modular-ash-ub))
    (loop for x in neg-values do
         (test x 'nc-ash 'c-ash))))


(declaim (inline ppc-ldb-2))

(defun ppc-ldb-2 (fun value)
  (declare (type (signed-byte 32) value)
           (optimize (speed 3) (safety 0) (space 1) (debug 1)
                     (compilation-speed 0)))
  (funcall fun (ldb (byte 8 24) value))
  (funcall fun (ldb (byte 8 16) value))
  (funcall fun (ldb (byte 8 8) value))
  (funcall fun (ldb (byte 8 0) value))
  (values))

(defun ppc-ldb-1 (fun)
  (declare (optimize (speed 3) (safety 0) (space 1) (debug 1)
                     (compilation-speed 0)))
  (loop
     for param :across (make-array 1 :initial-element nil)
     for size :across (make-array 1 :element-type 'fixnum :initial-element 3)
     do (ppc-ldb-2 fun (if param size -1))))

(with-test (:name :ppc-ldb)
 (let ((acc '()))
   (ppc-ldb-1 (lambda (x)
                (push x acc)))
   (assert (equal acc '(#xff #xff #xff #xff)))))

