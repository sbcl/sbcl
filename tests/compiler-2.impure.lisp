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

(with-test (:name (reduce :type-deriver :wild-array-upgraded-type))
  (checked-compile-and-assert
   ()
   `(lambda (x) (declare (type vector x)) (reduce #'+ x))
   ((#(1 2 3)) 6)
   (((make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(4 5 6))) 15)))

;;; We do not want functions closing over top level bindings to retain
;;; load-time code in the component when not necessary.
(with-test (:name :top-level-closure-separate-component)
  (ctu:file-compile
   `((let ((x (random 10)))
       (defun top-level-closure-1 ()
         x)
       (setq x 4)))
   :load t)
  ;; Check there's no top level code hanging out.
  (assert (= 1 (sb-kernel::code-n-entries (sb-kernel::fun-code-header (sb-kernel::%closure-fun #'top-level-closure-1)))))
  (assert (= (top-level-closure-1) 4)))

(with-test (:name :top-level-closure-separate-component.2)
  (ctu:file-compile
   `((let ((x (random 10)))
       (flet ((bar () x))
         (defun top-level-closure-2 ()
           #'bar))
       (setq x 4)))
   :load t)
  ;; Check there's no top level code hanging out. (We expect to only
  ;; have (FLET BAR) and TOP-LEVEL-CLOSURE-2 present.)
  (assert (= 2 (sb-kernel::code-n-entries (sb-kernel::fun-code-header (sb-kernel::%closure-fun #'top-level-closure-2)))))
  (assert (= (funcall (top-level-closure-2)) 4)))

(with-test (:name :dead-code-dfo-puking)
  (ctu:file-compile
   `((defun dead-code-puke-1 ()
       (let ((bar (read)))
         (labels ((emplace (thing)
                    (print thing))
                  (visit (thing)
                    (case thing
                      (0 (visit-code thing))
                      (1 (visit-code thing))
                      (2 (visit thing))
                      (3 (visit thing))))
                  (visit-code (thing)
                    (when (read)
                      (return-from visit-code))
                    (print bar)
                    (case thing
                      (1 (visit thing))
                      (2 (visit thing))
                      (3 (map nil #'visit (list thing thing))))))
           (emplace nil)))))
   :load t)
  ;; EMPLACE will have been LET-converted. VISIT and VISIT-CODE should
  ;; have been separated out or simply deleted.
  (assert (= 1 (sb-kernel::code-n-entries (sb-kernel::fun-code-header #'dead-code-puke-1)))))
