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

(in-package "SB-VM")

;;; This file defines a structure, so is an 'impure' test
(defstruct my-struct one two three four)

#-(and (or x86 x86-64) (not interpreter)) (sb-ext:exit :code 104)

(test-util:with-test (:name :basic-cpuid)
  (flet ((to-ascii (bits)
           (let ((s (make-array 4 :element-type 'base-char)))
             (setf (sap-ref-32 (vector-sap s) 0) bits)
             s)))
    (multiple-value-bind (a b c d)
        (%cpu-identification 0 0)
      ;; There's nothing to assert here since the result can vary
      (format t "~S (max function = ~D)~%"
              (concatenate 'string (to-ascii b) (to-ascii d) (to-ascii c))
              a))))

(progn
  (defun test-a-cons (acons oldcar oldcdr newcar newcdr)
    (declare (optimize (safety 0)))
    (%cons-cas-pair acons oldcar oldcdr newcar newcdr))
  (defun test-a-vect (avect ind old1 old2 new1 new2)
    (declare (optimize (safety 0)))
    (%vector-cas-pair avect ind old1 old2 new1 new2))
  (defun test-a-struct (inst ind old1 old2 new1 new2)
    (declare (optimize (safety 0)))
    (%instance-cas-pair inst ind old1 old2 new1 new2))

  (defun test-wide-cmpxchg ()
    (let ((x (cons 'a 'b)))
      (multiple-value-bind (old1 old2) (test-a-cons x 'a 'b 'foo 'bar)
        (assert (and (eq old1 'a) (eq old2 'b) (equal x '(foo . bar)))))
      (multiple-value-bind (old1 old2) (test-a-cons x 0 0 1 2)
        (assert (and (eq old1 'foo) (eq old2 'bar) (equal x '(foo . bar))))))

    ;; This is just testing that the offsets are correct.
    ;; Correct working of the instruction is tested by the CONS example.
    (let ((x (make-array 6 :initial-element nil)))
      (multiple-value-bind (old1 old2) (test-a-vect x 2 nil nil 'foo 'bar)
        (assert (and (null old1) (null old2) (equalp x #(nil nil foo bar nil nil))))))

    ;; Same remark applies - just check that the offset to the slot is right.
    (let ((s (make-my-struct :three 'the :four 'floor)))
      ;; in slots 3 and 4 put your bootee (a baby shoe, i.e.) on the floor
      (multiple-value-bind (old1 old2) (test-a-struct s 3 'the 'floor 'your 'bootee)
        (assert (and (eq old1 'the) (eq old2 'floor)
                     (eq (my-struct-three s) 'your)
                     (eq (my-struct-four s) 'bootee)))))
    t))

(test-util:with-test (:name :wide-compare-and-exchange)
  (multiple-value-bind (a b c d) (%cpu-identification 0 0)
    (declare (ignore b c d))
    ;; paranoidly check for whether we can execute function ID 1
    (or (and (>= a 1) ; the highest function ID
             (multiple-value-bind (a b c d) (%cpu-identification 1 0)
               (declare (ignore a b) (ignorable c d))
               ;; paranoidly check for CMPXCHGxB presence
               ;; constants from Table 3-20 and 3-21 of Intel manual
               (and #+x86(logbitp 8 d) #+x86-64(logbitp 13 c)
                    (test-wide-cmpxchg))))
        (format t "Double-width compare-and-swap NOT TESTED~%"))))
