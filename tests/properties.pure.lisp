;;;; miscellaneous tests of symbol properties

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

(defun test-symbol (symbol)
  (setf (symbol-plist symbol) nil)
  (setf (get symbol 'foo) '(my list))
  (setf (get symbol 'bar) 10)
  (setf (get symbol 'baz) t)
  (assert (eql (get symbol 'bar) 10))
  (assert (= (length (symbol-plist symbol)) 6))
  (remprop symbol 'foo)
  (assert (not (get symbol 'foo))))
(dolist (s '(foo :keyword || t nil))
  (let ((save (symbol-plist s)))
    (unwind-protect (test-symbol s)
      (setf (symbol-plist s) save))))
;;; In early 0.7 versions on non-x86 ports, setting the property list
;;; of 'NIL would trash (CDR NIL), due to a screwup in the low-level
;;; layout of SYMBOL. (There are several low-level punnish tricks used
;;; to make NIL work both as a cons and as a symbol without requiring
;;; a lot of conditional branching at runtime.)
(defparameter *nil-that-the-compiler-cannot-constant-fold* nil)
(assert (not (car *nil-that-the-compiler-cannot-constant-fold*)))
(assert (not (cdr *nil-that-the-compiler-cannot-constant-fold*)))

;;; success
