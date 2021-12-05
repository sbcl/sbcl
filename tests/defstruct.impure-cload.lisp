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

(eval-when (:compile-toplevel)
  (load "compiler-test-util.lisp"))

;;; This test asserts that each constructor for a defstruct involved in a
;;; mutually referential cycle with other defstructs inlines the test of the
;;; as-yet-unseen type when the DEFSTRUCT form is first read.
;;; We can tell that they're inlined because they use layout-IDs, so the only
;;; boxed constant is for the constructor to deposit a layout.
(with-test (:name (:block-compile :defstruct-slot-type-circularity))
  (with-scratch-file (fasl "fasl")
    (compile-file "block-compile-defstruct-test.lisp" :output-file fasl :block-compile t)
    (load fasl))
  (dolist (symbol '(make-s1 make-s2 make-s3))
    (let ((constants
           (ctu:find-code-constants (symbol-function symbol)
                                    :type 'sb-kernel:wrapper)))
      (assert (= (length constants) 1)))))

(with-test (:name :mutex-owner-typecheck)
  (let ((layouts
         (ctu:find-code-constants #'(setf sb-thread::mutex-%owner)
                                  :type 'sb-kernel:wrapper)))
    ;; expect exactly 1 layout, that of MUTEX, for signaling OBJECT-NOT-TYPE.
    ;; To be really pedantic we'd want to assert that in the source file
    ;; the defstruct of MUTEX appears prior to the defstruct of THREAD,
    ;; proving without a doubt that block compilation worked.
    (assert (= (length layouts) 1))
    (assert (find (sb-kernel:find-layout 'sb-thread:mutex)
                  layouts))))

(defstruct (parent)
  (bv #* :type bit-vector)
  (x 0d0 :type double-float))
(defstruct (child (:include parent))
  (w 0 :type word))
(defstruct (child2 (:include parent
                    (bv #* :type simple-bit-vector))))

#|
Timing result:
(defparameter *l1*
  (coerce (loop repeat 1000
                for i from 2
                collect (make-child :x (coerce i 'double-float)
                                    :w (1+ i)))
          'vector))

(defparameter *l2* (map 'vector 'copy-structure *l1*))

(defun test (n)
  (loop repeat (The fixnum n)
        sum (loop for s1 across *l1*
                  for s2 across *l2*
                  count (equalp s1 s2) fixnum) fixnum))

* (time (test 1000))
Old:
Evaluation took:
  0.046 seconds of real time
  127,769,104 processor cycles
New:
Evaluation took:
  0.024 seconds of real time
  66,055,457 processor cycles
|#

(with-test (:name :custom-equalp)
  (assert (equalp (make-child :x -0d0 :w #xf00f :bv #*10101)
                  (make-child :x +0d0 :w #xf00f :bv #*10101))))

(with-test (:name :no-equalp-calls)
  (dolist (type '(parent child child2))
    (let* ((equalp-impl
            (sb-kernel:wrapper-equalp-impl (sb-kernel:find-layout type)))
           (constants
            (ctu:find-code-constants equalp-impl)))
      (case type
        ((parent child)
         (assert (and (sb-int:singleton-p constants)
                      (eq (car constants)
                          (sb-int:find-fdefn 'sb-int:bit-vector-=)))))
        (child2
         ;; FIXME: why does CHILD2-EQUALP reference a boxed constant
         ;; equal to MOST-POSITIVE-WORD as a bignum?
         ;; Something strange about the bit-vector-= xform on simple-bit-vector.
         (assert (or (not constants)
                     (and (sb-int:singleton-p constants)
                          (not (typep (car constants)
                                      'sb-kernel:fdefn))))))))))
