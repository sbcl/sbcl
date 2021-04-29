(load "compiler-test-util.lisp")

(defun compiles-with-warning (lambda)
  (assert (nth-value 2 (checked-compile lambda :allow-warnings t))))

(defstruct (a-test-structure-foo
            (:constructor make-a-foo-1)
            (:constructor make-a-foo-2 (b &optional a)))
  (a 0 :type symbol)
  (b nil :type integer))

(with-test (:name :improperly-initialized-slot-warns)
  ;; should warn because B's default is NIL, not an integer.
  (compiles-with-warning '(lambda () (make-a-foo-1 :a 'what)))
  ;; should warn because A's default is 0
  (compiles-with-warning '(lambda () (make-a-foo-2 3))))

(with-test (:name (inline structure :ctor :no declaim))
  (let ((f (checked-compile '(lambda ()
                               (make-a-foo-1 :a 'wat :b 3)))))
    (assert (ctu:find-named-callees f)))
  (let ((f (checked-compile '(lambda ()
                               (declare (inline make-a-foo-1))
                               (make-a-foo-1 :a 'wat :b 3)))))
    (assert (not (ctu:find-named-callees f)))))

(defstruct %instance-ref-eq (n 0))

(with-test (:name :%instance-ref-eq-immediately-used)
  (checked-compile-and-assert
   ()
   `(lambda (s)
      (let ((n (%instance-ref-eq-n s)))
        (incf (%instance-ref-eq-n s))
        (eql n 0)))
   (((make-%instance-ref-eq)) t)))

(with-test (:name :%instance-ref-eq-load-immediate)
  (checked-compile-and-assert
   ()
   `(lambda (s)
      (eql (%instance-ref-eq-n s)
           most-positive-fixnum))
   (((make-%instance-ref-eq :n most-positive-fixnum)) t)
   (((make-%instance-ref-eq :n -1)) nil))
  (checked-compile-and-assert
   ()
   `(lambda (s)
      (eql (%instance-ref-eq-n s)
           (1- (expt 2 31))))
   (((make-%instance-ref-eq :n (1- (expt 2 31)))) t)
   (((make-%instance-ref-eq :n -1)) nil)))

(declaim (inline make-mystruct))
(macrolet ((def-mystruct () `(defstruct mystruct a b c)))
  (def-mystruct)) ; MAKE-MYSTRUCT captures a lexenv (rather pointlessly)

;;; Assert that throwaway code in compiled macrolets does not go in immobile space
#+immobile-code
(with-test (:name :macrolet-not-immobile-space :serial t
            :skipped-on :interpreter)
  (labels ((count-code-objects ()
             (length (sb-vm::list-allocated-objects
                      :immobile
                      :test (lambda (x)
                              (and (sb-kernel:code-component-p x)
                                   (/= (sb-kernel:generation-of x)
                                       sb-vm:+pseudo-static-generation+))))))
           (test (lambda)
             (sb-sys:without-gcing
              (let* ((start-count (count-code-objects))
                     (result
                       (let ((sb-c::*compile-to-memory-space* :immobile))
                         (compile nil lambda)))
                     (end-count (count-code-objects)))
                (assert (= end-count (1+ start-count)))
                result))))
    (sb-ext:gc :full t)
    ;; Test 1: simple macrolet
    (test '(lambda (x) (macrolet ((baz (arg) `(- ,arg))) (list (baz x)))))
    ;; Test 2: inline a function that captured a macrolet
    (test '(lambda (x) (make-mystruct :a x)))))
