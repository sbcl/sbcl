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

(load "assertoid.lisp")

(defmacro define-compiled-fun (fun name)
  `(progn
    (declaim (notinline ,name))
    (defun ,name (&rest args)
     (declare (optimize safety))
     (case (length args)
       (0 (,fun))
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

(assert (raises-error? (coerce (expt 10 1000) 'single-float) type-error))

(sb-ext:quit :unix-status 104)