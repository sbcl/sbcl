(defstruct (lottaslot
             (:predicate is-a-lottaslot))
  a b c d e f g h i j k l m n o p q r s t u v w x y z)

(defun slow (instance slots)
  (sb-int:dovector (x (the simple-vector slots))
    (if (eq (slot-value instance (truly-the symbol x)) 'flerkin) (return t))))
(defun fast (instance slots)
  (declare (structure-object instance))
  (sb-int:dovector (x (the simple-vector slots))
    (if (eq (slot-value instance (truly-the symbol x)) 'flerkin) (return t))))

(defun loopslow (n)
  (let ((inst (make-lottaslot)))
    (loop repeat (the fixnum n)
          count (slow inst #(z q i j l c e g)))))
(defun loopfast (n)
  (let ((inst (make-lottaslot)))
    (loop repeat (the fixnum n)
          count (fast inst #(z q i j l c e g)))))

#|
* (time (loopslow 100000))
Evaluation took:
  0.012 seconds of real time
  0.015585 seconds of total run time (0.015585 user, 0.000000 system)
  133.33% CPU
  42,017,049 processor cycles
  0 bytes consed

* (time (loopfast 100000))
Evaluation took:
  0.004 seconds of real time
  0.007433 seconds of total run time (0.007433 user, 0.000000 system)
  175.00% CPU
  20,026,278 processor cycles
  0 bytes consed
|#
