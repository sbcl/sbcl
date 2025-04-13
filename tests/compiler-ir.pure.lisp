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

#-sb-devel
(invoke-restart 'run-tests::skip-file)

(import '(sb-c::combination-fun-debug-name
          sb-c::combination-fun-source-name
          sb-c::*compile-component-hook*
          sb-c::basic-combination-p
          sb-c::combination-p
          sb-c::basic-combination-info
          sb-c::node-tail-p
          sb-c::do-blocks
          sb-c::do-nodes
          sb-c::%check-bound
          sb-kernel:%bit-pos-fwd/1))

(import 'ctu:inspect-ir)

(defun ir-full-calls (form)
  (let (calls)
    (inspect-ir
     form
     (lambda (component)
       (do-blocks (block component)
         (do-nodes (node nil block)
           (when (and (basic-combination-p node)
                      (eq (basic-combination-info node) :full))
             (push node calls))))))
    calls))

(defun ir-calls (form)
  (let (calls)
    (inspect-ir
     form
     (lambda (component)
       (do-blocks (block component)
         (do-nodes (node nil block)
           (when (basic-combination-p node)
             (push node calls))))))
    calls))

(defun ir2-vops (form)
  (let (vops)
    (inspect-ir
     form
     (lambda (component)
       (sb-c::do-ir2-blocks (block component)
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

(with-test (:name :instance-constraint-intersection)
  (assert (not (find 'sb-c::%type-check-error/c
                     (ir-calls
                      `(lambda (x)
                         (typecase x
                           (stream 2)
                           (hash-table 1))))
                     :key (lambda (x) (combination-fun-source-name x nil))))))

(with-test (:name :aref-full-call-no-type-check)
  (assert (not (find 'sb-c::%type-check-error/c
                     (ir-calls
                      `(lambda (x)
                         (aref x 0)))
                     :key (lambda (x) (combination-fun-source-name x nil))))))

(with-test (:name :call-full-like-p-constants)
  (assert (not (find 'sb-c::%type-check-error/c
                     (ir-calls
                      `(lambda (a b)
                         (< (truly-the double-float a) b)))
                     :key (lambda (x) (combination-fun-source-name x nil))))))

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
  (assert (not (find 'sb-c::%type-check-error/c
                     (ir-calls
                      `(lambda (x)
                         (let ((m (the sequence x)))
                           (values (length m)
                                   m))))
                     :key (lambda (x) (combination-fun-source-name x nil)))))
  (assert (eql (count 'sb-c::%type-check-error/c
                      (ir-calls
                       `(lambda (x l)
                          (let ((m (the sequence x))
                                (l (the integer l)))
                            (values (length m)
                                    l))))
                      :key (lambda (x) (combination-fun-source-name x nil)))
               1)))

(with-test (:name :pop-special-once)
  (assert
   (= (count 'symbol-value
             (ir2-vops '(lambda (s)
                         (declare (special s))
                         (pop s))))
      1)))

(with-test (:name :overflow+make-array
                  :fails-on (not :64-bit))
  (assert
   (= (count 'sb-vm::overflow+t
             (ir2-vops '(lambda (y)
                         (make-array (1+ y)))))
      1)))

(with-test (:name :other-pointer-p)
  (assert (not (find 'sb-c::%type-check-error/c
                     (ir-calls
                      `(lambda (x)
                         (when (and (stringp (truly-the (or simple-string (member #\a)) x))
                                    (zerop (length x)))
                           x)))
                     :key (lambda (x) (combination-fun-source-name x nil))))))

(with-test (:name :external-type-checks-across-functions)
  (assert (not (find 'sb-c::%type-check-error/c
                     (ir-calls
                      `(lambda (a b)
                         (declare (number a b)
                                  (optimize speed))
                         (+ a b)))
                     :key (lambda (x) (combination-fun-source-name x nil))))))

(with-test (:name :consecutive-casts)
  (assert (= (count 'sb-c::%type-check-error/c
                    (ir-calls
                     `(lambda (x)
                        (the fixnum (the integer x))))
                    :key (lambda (x) (combination-fun-source-name x nil)))
             1)))

(with-test (:name :sign-extend)
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

