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

;;; This test asserted that each constructor for a defstruct
;;; involved in a mutually referential cycle with other types
;;; is able to inline the type test of the as-yet-unseen type
;;; when the DEFSTRUCT form is processed.
;;; That's kind of tough to do with the typep optimizations now
;;; because the code header constants will not contain for #<LAYOUT for TYPE>
;;; The architectures that don't get that optimization still pass,
;;; but I need to fix this somehow.
(with-test (:name (:block-compile :defstruct-slot-type-circularity)
                  :fails-on (:or :x86 :x86-64))
  (with-scratch-file (fasl "fasl")
    (compile-file "block-compile-defstruct-test.lisp" :output-file fasl :block-compile t)
    (load fasl))
  (dolist (symbol '(make-s1 make-s2 make-s3))
    (let ((constants
           (ctu:find-code-constants (symbol-function symbol)
                                    :type 'sb-kernel:layout)))
      (assert (= (length constants) 3)))))

;;; Check an organic (not contrived) use of mutually referential types.
;;; NEWLINE is defined after SECTION-START, because it is a subtype.
;;; One of SECTION-START's slot setters refers to type NEWLINE.
(with-test (:name :pretty-stream-structs
                  :fails-on (:or :x86 :x86-64)) ; Same issue as the preceding test
  (let ((layouts
         (ctu:find-code-constants #'(setf sb-pretty::section-start-section-end)
                                  :type 'sb-kernel:layout)))
    ;; expect 3 layouts: one for SECTION-START to check the instance itself,
    ;; one for NEWLINE and one for BLOCK-END.
    ;; It's entirely coincidental that the above test also has 3.
    (assert (= (length layouts) 3))
    (assert (find (sb-kernel:find-layout 'sb-pretty::newline)
                  layouts))))

(with-test (:name :mutex-owner-typecheck
                  :fails-on (:or :x86 :x86-64)) ; Same issue as the preceding test
  (let ((layouts
         (ctu:find-code-constants #'(setf sb-thread::mutex-%owner)
                                  :type 'sb-kernel:layout)))
    ;; expect 3 layouts: one for THREAD, one for MUTEX, and one for FOREIGN-THREAD
    (assert (= (length layouts) 3))
    (assert (find (sb-kernel:find-layout 'sb-thread:thread)
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
            (sb-kernel:layout-equalp-impl (sb-kernel:find-layout type)))
           (constants
            (ctu:find-code-constants equalp-impl)))
      (case type
        ((parent child)
         (assert (and (sb-int:singleton-p constants)
                      (eq (car constants)
                          (sb-kernel::find-fdefn 'sb-int:bit-vector-=)))))
        (child2
         ;; FIXME: why does CHILD2-EQUALP reference a boxed constant
         ;; equal to MOST-POSITIVE-WORD as a bignum?
         ;; Something strange about the bit-vector-= xform on simple-bit-vector.
         (assert (or (not constants)
                     (and (sb-int:singleton-p constants)
                          (not (typep (car constants)
                                      'sb-kernel:fdefn))))))))))
