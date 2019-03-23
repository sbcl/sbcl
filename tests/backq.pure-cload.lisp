;;;; tests of backquote readmacro within the compiler

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

;; In the expression `(,@l1 a b ,@l2) is it preferable that we expand this
;; as (APPEND L1 (LIST* 'A 'B L2)) or as (APPEND L1 '(A) '(B) L2)?
;; The IR1 transform is designed to catch the latter case, but the expander
;; returns the former, which probably favors speed over code size.
;; This is perhaps an interesting reason to make expansion policy-sensitive.
;; I'll test a case that definitely triggers the IR1 transform.

(defun list-backq-expr (l1) `(,@l1 ,most-positive-fixnum a))
(defun obviously-constant-list () '(#.most-positive-fixnum a))

(defun vector-backq-expr () `#(foo ,char-code-limit)) ; no xform, but folded
(defun obviously-constant-vector () #(foo #.char-code-limit))

(with-test (:name :backquote-ir1-simplifier)
  ;; The pre-tests show that the backquoted expression did not compress
  ;; (,char-code-limit x) into a constant, nor for the vector.
  ;; Thus the IR1 optimization must do it in order for this test to pass.
  (assert (equal (macroexpand '`(,@l1 ,char-code-limit x))
                 '(APPEND l1 (LIST* char-code-limit '(X)))))
  (assert (equal (macroexpand '`#(,char-code-limit foo))
                 '(VECTOR char-code-limit 'foo)))

  (assert (eq (obviously-constant-list) (list-backq-expr nil)))

  ;; FIXME: do we not coalesce vector constants? I thought this should pass.
  #+nil
  (assert (eq (obviously-constant-vector) (vector-backq-expr)))

  ;; Compiled code should reference a constant vector.
  (assert (member (vector 'foo char-code-limit)
                  (ctu:find-code-constants #'vector-backq-expr)
                  :test #'equalp)))
