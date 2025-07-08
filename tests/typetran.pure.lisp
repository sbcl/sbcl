(defstruct root)
(defstruct (subtype1 (:include root)))
(defstruct (subtype2 (:include root)))

(defun swap-subtype (x)
  (declare (root x))
  (cond ((subtype1-p x)
         (setf (sb-kernel:%instance-layout x) #.(sb-kernel:find-layout 'subtype2)))
        ((subtype2-p x)
         (setf (sb-kernel:%instance-layout x) #.(sb-kernel:find-layout 'subtype1)))))

(defun way1 (a b) (and (root-p a) (root-p b) (eq (type-of a) (type-of b)))) ; transforms
(defun way2.1 (a b) (declare (root a)) (eq (type-of a) (type-of b))) ; transforms
(defun way2.2 (a b) (eq (type-of (the root a)) (type-of b))) ; transforms
(defun way2.3 (a b) (eq (class-of (the root a)) (class-of b))) ; transforms
(defun way3.1 (a b) (declare (root b)) (eq (type-of a) (type-of b))) ; transforms
(defun way3.2 (a b) (eq (type-of a) (type-of (the root b)))) ; does not transform but should
(defun way4 (a b) ; does not and should not transform
  (and (root-p a)
       (root-p b)
       (eq (prog1 (type-of a) (swap-subtype a)) (type-of b))))

(defun assert-not-calls-typeof (f)
  (compile f)
  (assert (null (ctu:find-named-callees (symbol-function f)))))

(with-test (:name :eq-type-of-type-of.1)
  (assert-not-calls-typeof 'way1)
  (assert (way1 (make-root) (make-root)))
  (assert (way1 (make-subtype1) (make-subtype1)))
  (assert (not (way1 (make-subtype2) (make-root))))
  (assert (not (way1 #p"a" #p"b"))))

(with-test (:name :eq-type-of-type-of.2)
  (macrolet ((try (way)
               `(progn
                  (assert-not-calls-typeof ',way)
                  (assert (,way (make-root) (make-root)))
                  (assert (,way (make-subtype1) (make-subtype1)))
                  (assert (not (,way (make-subtype1) (make-root))))
                  (assert (not (,way (make-subtype1) #p"b"))) ; 2nd arg is arbitrary
                  (assert (not (,way (make-subtype1) #'car)))
                  (assert (not (,way (make-subtype1) 5))))))
    (try way2.1)
    (try way2.2)
    (try way2.3)))

(with-test (:name :eq-type-of-type-of.3)
  (macrolet ((try (way)
               `(progn (assert-not-calls-typeof ',way)
                       (assert (,way (make-root) (make-root)))
                       (assert (,way (make-subtype1) (make-subtype1)))
                       (assert (not (,way (make-subtype1) (make-root))))
                       (assert (not (,way #p"a" (make-subtype1)))) ; 1st arg is arbitrary
                       (assert (not (,way #'car (make-subtype1))))
                       (assert (not (,way 5 (make-subtype1)))))))
    (try way3.1)
    ;; try (way3.2)
    ))

(with-test (:name :eq-type-of-type-of.4)
  (compile 'way4)
  ;; linkage-space does not store a usage to TYPE-OF in the code because its symbol is immortal.
  ;; I don't feel like making CTU:FIND-NAMED-CALLEES perform disassembly to divine the answer.
  #-linkage-space (assert (find 'type-of (ctu:find-named-callees #'way4)))
  (let ((x (make-subtype1))
        (y (make-subtype2)))
    ;; the funky function changes the layout of X which means that if the test of its layout
    ;; were delayed until the EQ as the vop would do, it would incorrectly get T as the answer.
    (assert (not (way4 x y)))
    (assert (eq (sb-kernel:layout-of x) (sb-kernel:layout-of y)))))
