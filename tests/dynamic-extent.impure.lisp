;;;; tests that dynamic-extent functionality works.

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

;;; &REST lists
(defmacro defun-with-dx (name arglist &body body)
  `(locally
     (declare (optimize sb-c::stack-allocate-dynamic-extent))
     (defun ,name ,arglist
       ,@body)))

(defun-with-dx dxlength (&rest rest)
  (declare (dynamic-extent rest))
  (length rest))

(assert (= (dxlength 1 2 3) 3))
(assert (= (dxlength t t t t t t) 6))
(assert (= (dxlength) 0))

(defun callee (list)
  (destructuring-bind (a b c d e f &rest g) list
    (+ a b c d e f (length g))))

(defun-with-dx dxcaller (&rest rest)
  (declare (dynamic-extent rest))
  (callee rest))

(assert (= (dxcaller 1 2 3 4 5 6 7) 22))

(sb-ext:quit :unix-status 104)