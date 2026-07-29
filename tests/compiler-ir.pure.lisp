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

(enable-test-parallelism)


(import '(sb-c::combination-fun-debug-name
          sb-c::combination-fun-source-name
          sb-c::*compile-component-hook*
          sb-c::basic-combination-p
          sb-c::combination-p
          sb-c::basic-combination-info
          sb-c::node-tail-p
          sb-c::%check-bound
          sb-kernel:%bit-pos-fwd/1))

(import 'ctu:inspect-ir)

(defun ir-full-calls (form)
  (let (calls)
    (inspect-ir
     form
     (lambda (component)
       (ctu:do-blocks (block component)
         (ctu:do-nodes (node nil block)
           (when (and (basic-combination-p node)
                      (eq (basic-combination-info node) :full))
             (push node calls))))))
    calls))

(defun ir-calls (form)
  (let (calls)
    (inspect-ir
     form
     (lambda (component)
       (ctu:do-blocks (block component)
         (ctu:do-nodes (node nil block)
           (when (basic-combination-p node)
             (push node calls))))))
    calls))

(defun ir2-vops (form)
  (let (vops)
    (inspect-ir
     form
     (lambda (component)
       (ctu:do-ir2-blocks (block component)
         (do ((vop (sb-c::ir2-block-start-vop block)
                   (sb-c:vop-next vop)))
             ((null vop))
           (push (sb-c:vop-name vop) vops)))))
    vops))

(with-test (:name :%bit-pos-fwd/1-tail-called)
  (destructuring-bind (combination)
      (ir-full-calls `(lambda (x)
                        (declare (optimize (debug 2)))
                        (position 1 (the simple-bit-vector x))))
    (assert (eql (combination-fun-debug-name combination) '%bit-pos-fwd/1))
    (assert (node-tail-p combination))))

(with-test (:name :local-call-tail-call)
  (destructuring-bind (combination)
      (ir-full-calls `(lambda ()
                        (flet ((x ()
                                 (terpri)))
                          (declare (notinline x))
                          (x)
                          10)))
    (assert (eql (combination-fun-debug-name combination) 'terpri))
    (assert (node-tail-p combination))))

(with-test (:name :fold-derived-logand)
  (assert (not (find 'logand
                     (ir-calls `(lambda (x)
                                  (declare ((integer 1 4) x))
                                  (logand #xF00 x)))
                     :key #'combination-fun-debug-name)))
  (assert (not (find 'logand
                     (ir-calls `(lambda (x)
                                  (declare ((integer 1 4) x))
                                  (logand #xFF (1+ x))))
                     :key #'combination-fun-debug-name)))
  (assert (not (find 'logand
                     (ir-calls `(lambda (x)
                                  (declare ((integer 1 4) x))
                                  (logand #xFF (ash 1 x))))
                     :key #'combination-fun-debug-name))))

(with-test (:name :mod-ash
                      :skipped-on (not (or :arm64 :x86-64)))
  (assert (not (ir-full-calls `(lambda (x y)
                                 (declare (fixnum x y))
                                 (logand #xFF (ash x y)))))))

(with-test (:name :exit-reoptimize-uses)
  (assert (not (find 'cdr
                     (ir-calls `(lambda (a b)
                                  (/ (unwind-protect (if a
                                                         (values b (cdr a))
                                                         (values 1 0))
                                       a)
                                     1)))
                     :key (lambda (x)
                            (and (combination-p x)
                                 (combination-fun-debug-name x)))))))

(with-test (:name :no-arg-count-checking)
  (assert (not (find 'sb-c:verify-arg-count
                     (ir2-vops '(lambda (&rest args)
                                 (block nil
                                   (handler-bind ((error (lambda (c) (return c))))
                                     (funcall (car args)))))))))
  (assert (not (find 'sb-c:verify-arg-count
                     (ir2-vops '(lambda (&rest args)
                                 (reduce #'+
                                  (car args)
                                  :key (lambda (x) (sqrt x))))))))
  (assert (not (find 'sb-c:verify-arg-count
                     (ir2-vops '(lambda (&rest args)
                                 (map 'list (lambda (x &optional z)
                                              (declare (ignore z))
                                              x)
                                  (car args)))))))
  (assert (not (find 'sb-c:verify-arg-count
                     (ir2-vops '(lambda (&rest args)
                                 (find 0 (car args)
                                  :key
                                  (lambda (x &rest z)
                                    (declare (ignore z))
                                    x)))))))
  (assert (not (find 'sb-c:verify-arg-count
                     (ir2-vops '(lambda (&rest args)
                                 (remove 0 (car args)
                                  :key
                                  (lambda (&optional z)
                                    z))))))))

#+sb-devel
(with-test (:name (:assignment-convert :iterative-tail))
  (let ((converted nil))
    (let ((fun (inspect-ir
                '(lambda (n)
                  (labels ((fact (n acc)
                             (if (zerop n)
                                 acc
                                 (fact (1- n) (* acc n)))))
                    (fact n 1)))
                (lambda (component)
                  (dolist (lambda (sb-c::component-lambdas component))
                    (dolist (lambda-let (sb-c::lambda-lets lambda))
                      (when (sb-c::functional-kind-eq lambda-let sb-c::assignment)
                        (setq converted t))))))))
      (assert (= (funcall fun 9) 362880))
      (assert converted))))

#+sb-devel
(with-test (:name (:assignment-convert :iterative-non-tail))
  (let ((converted nil))
    (let ((fun (inspect-ir
                '(lambda (n)
                  (labels ((fact (n acc)
                             (if (zerop n)
                                 acc
                                 (fact (1- n) (* acc n)))))
                    (1+ (fact n 1))))
                (lambda (component)
                  (dolist (lambda (sb-c::component-lambdas component))
                    (dolist (lambda-let (sb-c::lambda-lets lambda))
                      (when (sb-c::functional-kind-eq lambda-let sb-c::assignment)
                        (setq converted t))))))))
      (assert (= (funcall fun 9) 362881))
      (assert converted))))

#+sb-devel
(with-test (:name (:assignment-convert :multiple-use))
  (let ((converted nil))
    (let ((fun (inspect-ir
                '(lambda (b x y)
                  (labels ((f (n x)
                             (if (zerop n)
                                 x
                                 (f (1- n) (1+ x)))))
                    (+ 2 (if (= b 5)
                             (f x x)
                             (f b y)))))
                (lambda (component)
                  (dolist (lambda (sb-c::component-lambdas component))
                    (dolist (lambda-let (sb-c::lambda-lets lambda))
                      (when (sb-c::functional-kind-eq lambda-let sb-c::assignment)
                        (setq converted t))))))))
      (assert (= (funcall fun 5 3 4) 8))
      (assert (= (funcall fun 6 3 4) 12))
      (assert converted))))

#+sb-devel
(with-test (:name (:assignment-convert :optional-dispatch))
  (let ((converted 0))
    (let ((fun (inspect-ir
                '(lambda (mod r/m)
                  (flet ((make-machine-ea (base &optional disp index scale)
                           (list base
                                 disp
                                 index
                                 scale)))
                    (cond ((= r/m #b100)
                           (make-machine-ea :so :here :we :are))
                          ((/= mod #b00) (make-machine-ea :full-reg :tbf))
                          ((= r/m #b101) (make-machine-ea :rip :another))
                          (t (make-machine-ea :full-reg)))))
                (lambda (component)
                  (dolist (lambda (sb-c::component-lambdas component))
                    (dolist (lambda-let (sb-c::lambda-lets lambda))
                      (when (sb-c::functional-kind-eq lambda-let sb-c::assignment)
                        (incf converted))))))))
      (assert (equal (funcall fun 0 5) '(:rip :another nil nil)))
      ;; There should be two converted :ASSIGNMENT lambdas: one for
      ;; (BASE DISP), and one for (BASE DISP INDEX SCALE). The latter
      ;; gets assignment converted because it is also tail called by
      ;; the entry point for (BASE DISP INDEX), which in turn was let
      ;; converted into the entry point for (BASE DISP).
      (assert (= converted 2)))))

#+sb-devel
(with-test (:name (:assignment-convert :no-self-tr))
  (let ((converted nil))
    (let ((fun (inspect-ir
                '(lambda (n)
                  (labels ((id (n)
                             n))
                    (case n
                      ((a b c d e f g)
                       (id 1))
                      ((h i j k l m n)
                       (id 2))
                      ((o p q r s t u)
                       (id 3))
                      ((v w x y z)
                       (id 4)))))
                (lambda (component)
                  (dolist (lambda (sb-c::component-lambdas component))
                    (dolist (lambda-let (sb-c::lambda-lets lambda))
                      (when (sb-c::functional-kind-eq lambda-let sb-c::assignment)
                        (setf converted t))))))))
      (assert (= (funcall fun 'a) 1))
      (assert (= (funcall fun 'l) 2))
      (assert (= (funcall fun 's) 3))
      (assert (= (funcall fun 'w) 4))
      (assert converted))))

;;; Check that we are able to promote assignment lambdas into LETs.
#+sb-devel
(with-test (:name (:assignment-convert :can-become-let))
  (let ((assignment nil)
        (let nil))
    (inspect-ir
     '(lambda (x)
       (labels ((id (n)
                  (+ n n)))
         (1+ (if t
                 (id (read))
                 (id (+ x x))))))
     (lambda (component)
       (dolist (lambda (sb-c::component-lambdas component))
         (dolist (lambda-let (sb-c::lambda-lets lambda))
           (when (sb-c::functional-kind-eq lambda-let sb-c::assignment)
             (setf assignment t))
           (when (sb-c::functional-kind-eq lambda-let let)
             (setf let t))))))
    (assert (not assignment))
    (assert let)))

;;; Check assignment conversion of functions which don't return.
#+sb-devel
(with-test (:name (:assignment-convert :non-local-exit))
  (let ((assignment nil))
    (let* ((*standard-output* (make-broadcast-stream))
           (fun (inspect-ir
                 '(lambda (z)
                   (block hey
                     (flet ((f (x)
                              (print x)
                              (return-from hey (values 'GOOD (+ x x)))))
                       (values
                        'BAD
                        (if (plusp z)
                            (f z)
                            (+ 1 (f (+ z z))))))))
                 (lambda (component)
                   (dolist (lambda (sb-c::component-lambdas component))
                     (dolist (lambda-let (sb-c::lambda-lets lambda))
                       (when (sb-c::functional-kind-eq lambda-let sb-c::assignment)
                         (setf assignment t))))))))
      (assert (eq (funcall fun 3) 'GOOD))
      (assert (eq (funcall fun -3) 'GOOD))
      (assert assignment))))

;;; The example in 5.1 of Fluet and Weeks "Contification using
;;; Dominators", which the A_cont analysis can handle, but A_call
;;; cannot. In this case, FM, F, G and H all have the same
;;; continuation.
#+sb-devel
(with-test (:name (:assignment-convert :fluet-weeks-5.1))
  (let ((converted '()))
    (let ((fun (inspect-ir
                '(lambda (b x y flag)
                  (labels ((fm (n x)
                             (if flag
                                 (f (1- n) (1+ x) t)
                                 (g (1+ n) (1- x) t)))
                           (f (n x flag1)
                             (if flag1
                                 (g (* 2 n) (- 2 x) nil)
                                 (h (* n n))))
                           (g (n x flag2)
                             (if flag2
                                 (f (1+ n) (+ x 2) nil)
                                 (h (* n x))))
                           (h (y)
                             (* y y)))
                    (+ 2
                       (if (= b 5)
                           (fm x x)
                           (fm b y)))))
                (lambda (component)
                  (dolist (lambda (sb-c::component-lambdas component))
                    (dolist (lambda-let (sb-c::lambda-lets lambda))
                      (when (sb-c::functional-kind-eq lambda-let sb-c::assignment)
                        (push lambda-let converted))))))))
      (assert (= (funcall fun 1 2 3 t) 2))
      (assert (= (funcall fun 1 2 3 nil) 83))
      (assert (= (length converted) 4))))) ; F, G, FM, H

;;; A modified version of the above test, but with an outside call for
;;; H.
#+sb-devel
(with-test (:name (:assignment-convert :fluet-weeks-5.1-modified))
  (let ((converted '()))
    (let ((fun (inspect-ir
                '(lambda (b x y flag)
                  (labels ((fm (n x)
                             (if flag
                                 (f (1- n) (1+ x) t)
                                 (g (1+ n) (1- x) t)))
                           (f (n x flag1)
                             (if flag1
                                 (g (* 2 n) (- 2 x) nil)
                                 (h (* n n))))
                           (g (n x flag2)
                             (if flag2
                                 (f (1+ n) (+ x 2) nil)
                                 (h (* n x))))
                           (h (y)
                             (* y y)))
                    (+ 2
                       (cond ((= b 5)
                              (fm x x))
                             ((= (mod b 10) 9)
                              (h x))
                             (t
                              (fm b y))))))
                (lambda (component)
                  (dolist (lambda (sb-c::component-lambdas component))
                    (dolist (lambda-let (sb-c::lambda-lets lambda))
                      (when (sb-c::functional-kind-eq lambda-let sb-c::assignment)
                        (push lambda-let converted))))))))
      (assert (= (funcall fun 1 2 3 t) 2))
      (assert (= (funcall fun 1 2 3 nil) 83))
      (assert (= (length converted) 4))))) ; F, G, FM, H

;;; The example in 5.2 of Fluet and Weeks "Contification via
;;; Dominators", which both the A_cont analysis and A_call analyses
;;; cannot handle. In light of this, they present the maximal A_dom
;;; analysis. In this case, F, G1, G2 and H all have the same
;;; continuation.
#+sb-devel
(with-test (:name (:assignment-convert :fluet-weeks-5.2))
  (let ((converted '()))
    (let ((fun (inspect-ir
                '(lambda (b x y flag)
                  (labels ((fm (n x flag1)
                             (if flag1
                                 (f (1- n) (1+ x))
                                 (f (1+ n) (1- x))))
                           (f (n x)
                             (cond ((= flag 0)
                                    (g1 (* 2 n) x))
                                   ((= flag 1)
                                    (g1 (* n x) x))
                                   ((= flag 2)
                                    (g2 (* 2 n) x))
                                   ((= flag 3)
                                    (g2 (* n x) x))))
                           (g1 (n x)
                             (h (* n x)))
                           (g2 (n x)
                             (h (- n x)))
                           (h (y)
                             (* y y)))
                    (+ 2
                       (fm x x t)
                       (fm b y nil))))
                (lambda (component)
                  (dolist (lambda (sb-c::component-lambdas component))
                    (dolist (lambda-let (sb-c::lambda-lets lambda))
                      (when (sb-c::functional-kind-eq lambda-let sb-c::assignment)
                        (push lambda-let converted))))))))
      (assert (= (funcall fun 1 2 3 0) 102))
      (assert (= (funcall fun 1 2 3 1) 147))
      (assert (= (funcall fun 1 2 3 2) 7))
      (assert (= (funcall fun 1 2 3 3) 6))
      (assert (= (length converted) 4))))) ; F, G1, G2, H

;;; A, B and C are mutually tail recursive, so all three return to the
;;; continuation of the LIST call, which is where the A_dom analysis
;;; contifies them. C is only ever called from A and B, that is, from
;;; inside its own group of mutually tail recursive lambdas, so it has no
;;; call from outside the group to be spliced in at until A and B have
;;; been converted and the tail calls to C have become ordinary calls
;;; returning to that continuation. C cannot be LET converted instead
;;; either, as it has more than one reference.
#+sb-devel
(with-test (:name (:assignment-convert :no-call-from-outside-group))
  (let ((converted '()))
    (let ((fun (inspect-ir
                '(lambda (n)
                  (labels ((a (i)
                             (if (<= i 0) :a (c (1- i))))
                           (b (i)
                             (if (<= i 0) :b (c (- i 2))))
                           (c (i)
                             (if (oddp i) (a (1- i)) (b (1- i)))))
                    (list (if (plusp n)
                              (a n)
                              (b n)))))
                (lambda (component)
                  (dolist (lambda (sb-c::component-lambdas component))
                    (dolist (lambda-let (sb-c::lambda-lets lambda))
                      (when (sb-c::functional-kind-eq lambda-let sb-c::assignment)
                        (push lambda-let converted))))))))
      (assert (equal (funcall fun 6) '(:a)))
      (assert (equal (funcall fun -3) '(:b)))
      (assert (= (length converted) 3))))) ; A, B, C

;;; Like the previous test, except that the members with no call from
;;; outside the group form a cycle: D1 is called from A and D2 while D2 is
;;; called from B and D1, and neither can be converted on its own
;;; beforehand the way C is above, as their callers are in two different
;;; environments. Converting a member only needs one of its callers to
;;; have been converted already though, so D1 can follow A and D2 can
;;; follow B, and the cycle between the two of them does not matter.
#+sb-devel
(with-test (:name (:assignment-convert :cycle-of-calls-from-inside-group))
  (let ((converted '()))
    (let ((fun (inspect-ir
                '(lambda (n)
                  (labels ((a (i)
                             (if (<= i 0) :a (d1 (1- i))))
                           (b (i)
                             (if (<= i 0) :b (d2 (1- i))))
                           (d1 (i)
                             (if (oddp i) (d2 (1- i)) (a (1- i))))
                           (d2 (i)
                             (if (oddp i) (d1 (1- i)) (b (1- i)))))
                    (list (if (plusp n)
                              (a n)
                              (b n)))))
                (lambda (component)
                  (dolist (lambda (sb-c::component-lambdas component))
                    (dolist (lambda-let (sb-c::lambda-lets lambda))
                      (when (sb-c::functional-kind-eq lambda-let sb-c::assignment)
                        (push lambda-let converted))))))))
      (assert (equal (funcall fun 7) '(:a)))
      (assert (equal (funcall fun 6) '(:b)))
      (assert (equal (funcall fun -2) '(:b)))
      (assert (= (length converted) 4))))) ; A, B, D1, D2

(with-test (:name :empty-special-bindings)
  (assert (not (find 'sb-c::%special-unbind
                     (ir-calls
                      `(lambda ()
                         (let (*))
                         10))
                     :key (lambda (x) (combination-fun-source-name x nil))))))

(with-test (:name :flushable-alien-fp-math)
  (assert (not (find 'sb-c:%alien-funcall
                     (ir-calls
                      `(lambda (x)
                         (declare (double-float x))
                         (exp x)
                         10))
                     :key (lambda (x) (combination-fun-source-name x nil))))))

(with-test (:name :values-let-conversion-reoptimization)
  (assert (not (find 'list
                     (ir-calls
                      `(lambda (a)
                         (values
                          (flet ((f ()
                                   (values a (catch 'c)
                                           (list 1))))
                            (f)))))
                     :key (lambda (x) (combination-fun-source-name x nil))))))

(defun count-type-checks (lambda)
  (count-if (lambda (name)
              (member name '(sb-c::%type-check-error/c sb-c::%type-check-error)))
         (ir-calls lambda)
         :key (lambda (x) (combination-fun-source-name x nil))))

(with-test (:name :instance-constraint-intersection)
  (assert (zerop (count-type-checks
                  `(lambda (x)
                     (typecase x
                       (stream 2)
                       (hash-table 1)))))))

(with-test (:name :aref-full-call-no-type-check)
  (assert (zerop (count-type-checks
                  `(lambda (x)
                     (aref x 0))))))

(with-test (:name :call-full-like-p-constants)
  (assert (zerop (count-type-checks
                  `(lambda (a b)
                     (< (truly-the double-float a) b))))))

#+sb-devel
(with-test (:name :constant-substitution)
  (let ((calls (ir-calls
                `(lambda (a b)
                   (or (eq a 2)
                       (eq b 10))))))
    (assert (not (find-if
                  (lambda (call)
                    (let ((fun (sb-c::ref-leaf (sb-c::lvar-uses (sb-c::combination-fun call)))))
                      (and (sb-c::functional-p fun)
                           (sb-c::functional-kind-eq fun let))))
                  calls)))))

(with-test (:name :unused-flet-values)
  (let ((calls (ir-full-calls
                `(lambda (x y)
                   (flet ((f ()
                            (values x (+ x y))))
                     (declare (notinline f))
                     (values (f)))))))
    (assert (not calls))))

(with-test (:name :overflow-arith
            :skipped-on (not (or :arm64 :x86-64)))
  (let* ((types '(sb-vm:word sb-vm:signed-word))
         (the-types `(fixnum (unsigned-byte 16) (signed-byte 16) ,@types)))
    (loop
      for op in '(+ - * negate)
      do
      (loop
        for a-type in types
        do
        (loop
          for b-type in types
          do
          (loop for the-type in the-types
                for lambda = (if (eq op 'negate)
                                 `(lambda (a)
                                    (declare (,a-type a))
                                    (the ,the-type (- a)))
                                 `(lambda (a b)
                                    (declare (,a-type a)
                                             (,b-type b))
                                    (the ,the-type (,op a b))))
                do (unless (find-if (lambda (x)
                                      (eql (search "OVERFLOW" (string x)) 0))
                                    (ir2-vops lambda))
                     (cerror "" "~s" lambda))))))))

(with-test (:name :type-diff-testing)
  (assert
   (= (count 'sb-int:double-float-p
             (ir2-vops '(lambda (x)
                         (declare ((or fixnum double-float) x))
                         (typep x 'double-float))))
      1))
  (assert
   (= (count 'numberp
             (ir2-vops '(lambda (x)
                         (declare ((or double-float array) x))
                         (typep x 'number))))
      0))
  (assert
   (= (count 'integerp
             (ir2-vops '(lambda (x)
                         (declare ((or array (signed-byte 8)) x))
                         (typep x 'integer))))
      0)))

(with-test (:name :let-no-typecheck)
  (assert (zerop (count-type-checks
                  `(lambda (x)
                     (let ((m (the sequence x)))
                       (values (length m)
                               m))))))
  (assert (eql (count-type-checks
                `(lambda (x l)
                   (let ((m (the sequence x))
                         (l (the integer l)))
                     (values (length m)
                             l))))
               1)))

(with-test (:name :pop-special-once)
  (assert
   (= (count 'symbol-value
             (ir2-vops '(lambda (s)
                         (declare (special s))
                         (pop s))))
      1)))

#+(or x86-64 arm64)
(with-test (:name :overflow+make-array)
  (assert
   (= (count 'sb-vm::overflow+t
             (ir2-vops '(lambda (y)
                         (make-array (1+ y)))))
      1)))

(with-test (:name :other-pointer-p)
  (assert (zerop (count-type-checks
                  `(lambda (x)
                     (when (and (stringp (truly-the (or simple-string (member #\a)) x))
                                (zerop (length x)))
                       x))))))

(with-test (:name :external-type-checks-across-functions)
  (assert (zerop (count-type-checks
                  `(lambda (a b)
                     (declare (number a b)
                              (optimize speed))
                     (+ a b))))))

(with-test (:name :consecutive-casts)
  (assert (= (count-type-checks
              `(lambda (x)
                 (the fixnum (the integer x))))
             1))
  #+(or arm64 x86-64)
  (assert (= (count-type-checks
              `(lambda (x)
                 (logand (the number x) 2)))
             0))
  (assert (= (count-type-checks
              `(lambda (x)
                 (the integer (the (real 5) x))))
             1)))

(with-test (:name :sign-extend
            :fails-on (or :ppc :riscv :loongarch64 :sparc :mips))
  (assert (= (count 'sb-c::mask-signed-field
                    (ir-calls
                     `(lambda (a)
                        (declare ((unsigned-byte 32) a))
                        (logior a (- (mask-field (byte 1 31) a)))))
                    :key (lambda (x) (combination-fun-source-name x nil)))
             1))
  (assert (= (count 'sb-c::mask-signed-field
                    (ir-calls
                     `(lambda (a)
                        (declare ((unsigned-byte 32) a))
                        (logior (- (mask-field (byte 1 31) a)) a)))
                    :key (lambda (x) (combination-fun-source-name x nil)))
             1))
  #+64-bit
  (assert (= (count 'sb-c::mask-signed-field
                    (ir-calls
                     `(lambda (a)
                        (declare ((unsigned-byte 64) a))
                        (logior a (- (mask-field (byte 1 63) a)))))
                    :key (lambda (x) (combination-fun-source-name x nil)))
             1)))

(with-test (:name :optional-type-checks)
  (assert (= (count-type-checks
              `(lambda (&optional x y)
                 (declare (list x))
                 (values x y)))
             1)))

(with-test (:name :flush-multiple-callables)
  (assert (not (ir-full-calls
                `(lambda (a b c)
                   (declare (vector b))
                   (find a b :test (if c #'eq #'eql))
                   10)))))

(with-test (:name :stack-allocate-make-array-reverse)
  (let ((vops (ir2-vops `(lambda (l)
                           (let ((j (make-array 5 :initial-contents (nreverse l))))
                             (declare (dynamic-extent j))
                             (opaque-identity j)
                             10)))))
    (assert (= (count 'sb-vm::allocate-vector-on-stack vops) 1))
    (assert (= (count 'sb-vm::allocate-vector-on-heap vops) 0))))


(with-test (:name :no-type-check-tail-call)
  (destructuring-bind (combination)
      (ir-full-calls `(lambda (x)
                        (truly-the fixnum (funcall (the function x)))))
    (assert (node-tail-p combination))))

(with-test (:name :evenp+arithmetic
            :fails-on (or :arm :riscv :loongarch64 :ppc64 :ppc :sparc :mips))
  (assert (not (ir-full-calls `(lambda (x)
                                 (evenp (+ x 3))))))
  (assert (not (ir-full-calls `(lambda (x)
                                 (logbitp 0 (+ x 3)))))))

(with-test (:name :modarith-unknown-types
            :fails-on (or :arm :ppc :sparc :mips))
  (assert (not (ir-full-calls `(lambda (x)
                                 (logand (+ x 10) 20)))))
  (assert (not (ir-full-calls `(lambda (m x)
                                 (logand (if m (+ x 3) (- x 2)) 20))))))

(with-test (:name :reoptimize-complement)
  (assert (not (ir-full-calls `(lambda (x)
                                 (declare ((simple-array fixnum (*)) x)
                                          (optimize speed))
                                 (position 1 x :test-not #'=))))))

(with-test (:name :complement-multiple-calls)
  (assert (= (count 'complement
                    (ir-calls
                     `(lambda (m n)
                        (let ((c (complement #'=)))
                          (values (funcall c 1 m)
                                  (funcall c 3 n)))))
                    :key (lambda (x) (combination-fun-source-name x nil)))
             0))
  (assert (= (count 'complement
                    (ir-calls
                     `(lambda (l)
                        (declare (list l)
                                 (optimize speed))
                        (position 10 l :test (complement #'=))))
                    :key (lambda (x) (combination-fun-source-name x nil)))
             0)))

(with-test (:name :local-calls-to-&rest)
  (assert (not (ir-full-calls
                `(lambda (a)
                   (flet ((a (&rest args)
                            (apply #'eql args)))
                     (list (a a 1)
                           (a a 2))))))))

(with-test (:name :inline-local-call-with-casts)
  (assert (not (ir-full-calls
                `(lambda (n)
                   (funcall (the (function (fixnum) fixnum)
                                 (lambda (x) (1+ x)))
                            n))))))

(with-test (:name :truncate-signed-word-error
            :fails-on (or :ppc64 :riscv :loongarch64))
  (assert (not (find 'sb-vm::move-from-signed
                     (ir2-vops '(lambda (x d)
                                 (declare ((signed-byte 64) x d))
                                 (values (the fixnum (truncate x d)))))))))

(with-test (:name :cast-movement)
  (assert (not (find 'sb-vm::move-from-unsigned
                     (ir2-vops '(lambda (d x c)
                                 (declare ((simple-array word (4)) x))
                                 (let* ((b (aref x 1))
                                        (a (if d c b)))
                                   (> (the word a) 10)))))))
  (assert (find 'sb-vm::move-from-unsigned
                (ir2-vops '(lambda (d x c)
                            (declare ((simple-array word (4)) x))
                            (let* ((b (aref x 1))
                                   (a (if d c b)))
                              (print 1)
                              (> (the word a) 10)))))))

(with-test (:name :overflow-svref
            :skipped-on (not (or :arm64 :x86-64)))
  (assert (not (ir-full-calls
                `(lambda (x n)
                   (svref x (+ n 1)))))))

(with-test (:name :setf-aref-type-checks)
  (assert (= (count-type-checks
              `(lambda (a n)
                 (setf (aref (the (OR (ARRAY SINGLE-FLOAT) (ARRAY DOUBLE-FLOAT)) a) 0)
                       n)))
             1)))

(with-test (:name :constant-fold-multiple-value-uses)
  (assert (not (ir-full-calls
                `(lambda (a)
                   (1+ (if a
                           1
                           2d0))))))
  (assert (not (ir-full-calls
                `(lambda (b)
                   (truncate (if b
                                 10 20d0)
                             2)))))
  (assert (not
           (ir-full-calls
            `(lambda (d)
               (multiple-value-bind (v w) (if d
                                              (values 1 6)
                                              (values 2 5))
                 (values v
                         (+ w 1/2)))))))
  (assert (not
           (ir-full-calls
            `(lambda (d)
               (let ((x (if d
                            20
                            40)))
                 (setf * 20)
                 (values (truncate x 1/3))))))))

(with-test (:name :make-array-et-list)
  (assert (= (count 'list
                    (ir-calls
                     `(lambda (n x)
                        (make-array n :element-type `(unsigned-byte ,x))))
                    :key (lambda (x) (and (combination-p x)
                                          (combination-fun-source-name x nil))))
             0)))

(with-test (:name :xep-calls-no-arg-count-checking)
  (assert (= (count 'sb-c:verify-arg-count
                    (ir2-vops '(lambda (n)
                                (funcall (if n
                                             (lambda (a) (+ a 2))
                                             (lambda (b) (+ b 1)))
                                 1))))
             1)))

(with-test (:name :complex-constants-to-locals)
  (assert (= (count 'sb-vm::move-from-complex-double
                    (ir2-vops '(lambda ()
                                (flet ((f (p)
                                         p))
                                  (let ((p #c(1d0 2d0)))
                                    (f p)
                                    (f p))))))
             0)))
