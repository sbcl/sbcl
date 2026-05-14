;;;; Tests for sb-fiber, one per public entry point and error case.
;;;; Stress and regression coverage lives in fiber.impure.lisp.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

(unless (and (member :sb-thread *features*)
             (member :sb-fiber *features*)
             (or (member :x86-64 *features*)
                 (member :arm64 *features*))
             (not (member :win32 *features*)))
  (invoke-restart 'run-tests::skip-file))

(require :sb-fiber)
(use-package :sb-fiber)

;;; --- Lifecycle ---

(with-test (:name (:fiber :make-and-destroy))
  (let ((f (make-fiber (lambda ()))))
    (assert (fiber-alive-p f))
    (assert (= (fiber-state f) +fiber-new+))
    (destroy-fiber f)
    (assert (not (fiber-alive-p f)))
    ;; Double destroy is a no-op.
    (destroy-fiber f)))

(with-test (:name (:fiber :make-fiber :custom-sizes))
  (let ((f (make-fiber (lambda ()) :stack-size 131072 :binding-stack-size 16384)))
    (assert (fiber-alive-p f))
    (destroy-fiber f)))

(with-test (:name (:fiber :with-fiber-cleans-up))
  (let (captured)
    (with-fiber (f (lambda ()) :stack-size 65536)
      (setf captured f)
      (assert (fiber-alive-p f)))
    (assert (not (fiber-alive-p captured)))))

(with-test (:name (:fiber :make-main-fiber))
  (let ((main (make-main-fiber)))
    (assert (fiber-alive-p main))
    (assert (eq main *current-fiber*))
    (destroy-fiber main)))

(with-test (:name (:fiber :name :is-accessible-and-printed))
  (let ((f (make-fiber (lambda ()) :name "worker")))
    (assert (equal "worker" (fiber-name f)))
    (assert (search "worker" (princ-to-string f))
            ()
            "expected PRINT-OBJECT to mention the name, got ~S"
            (princ-to-string f))
    (destroy-fiber f))
  (let ((f (make-fiber (lambda ()))))
    (assert (null (fiber-name f)))
    (destroy-fiber f)))

(with-test (:name (:fiber :join-fiber))
  (let* ((main (make-main-fiber))
         (child (make-fiber (lambda () (yield-fiber :step) :done))))
    ;; join-fiber resumes until the entry function returns; its
    ;; value is the entry-fn's return value.
    (assert (eq :done (join-fiber child)))
    (assert (= (fiber-state child) +fiber-dead+))
    (destroy-fiber child)
    (destroy-fiber main)))

;;; --- Generators ---

(with-test (:name (:fiber :make-fiber-generator))
  (let* ((main (make-main-fiber))
         (gen  (make-fiber-generator
                (lambda () (yield-fiber 1) (yield-fiber 2) 3))))
    (assert (equal '(1 2 3)
                   (loop for v = (funcall gen) while v collect v)))
    ;; Auto-destroyed.
    (assert (null (funcall gen)))
    (destroy-fiber main)))

(with-test (:name (:fiber :do-fiber-generator))
  (let ((main (make-main-fiber))
        (out  '()))
    (do-fiber-generator (v (make-fiber-generator
                            (lambda () (yield-fiber :a) (yield-fiber :b))))
      (push v out))
    (assert (equal '(:a :b) (nreverse out)))
    (destroy-fiber main)))

;;; --- Interrupt / condition staging ---

(with-test (:name (:fiber :interrupt-fiber :new-never-runs))
  (let* ((main (make-main-fiber))
         (ran  nil)
         (f    (make-fiber (lambda () (setf ran t) :unreached))))
    (interrupt-fiber f (make-condition 'simple-error
                                       :format-control "cancelled"))
    (handler-case (switch-fiber main f)
      (simple-error (e)
        (assert (search "cancelled" (princ-to-string e)))))
    (assert (null ran))
    (destroy-fiber f)
    (destroy-fiber main)))

(with-test (:name (:fiber :interrupt-fiber :runnable-fires-in-context))
  (let* ((main   (make-main-fiber))
         (caught nil)
         (f      (make-fiber
                  (lambda ()
                    (handler-case (yield-fiber)
                      (simple-error (e) (setf caught (princ-to-string e))))
                    :done))))
    (switch-fiber main f)               ; run to the yield
    (interrupt-fiber f (make-condition 'simple-error
                                       :format-control "ouch"))
    (assert (eq :done (join-fiber f)))
    (assert (string= caught "ouch"))
    (destroy-fiber f)
    (destroy-fiber main)))

(with-test (:name (:fiber :fiber-condition))
  (let ((f (make-fiber (lambda ()))))
    (assert (null (fiber-condition f)))
    (let ((c (make-condition 'simple-error :format-control "x")))
      (interrupt-fiber f c)
      (assert (eq c (fiber-condition f))))
    (destroy-fiber f)))

;;; --- Control transfer ---

(with-test (:name (:fiber :switch-fiber :basic))
  (let* ((main (make-main-fiber))
         (log  '())
         (child (make-fiber (lambda ()
                              (push :child log)
                              (switch-fiber *current-fiber*
                                            (fiber-return-fiber *current-fiber*))
                              (push :child-again log)))))
    (push :main log)
    (switch-fiber main child)
    (push :main-again log)
    (switch-fiber main child)
    (assert (equal (nreverse log) '(:main :child :main-again :child-again)))
    (destroy-fiber child)
    (destroy-fiber main)))

(with-test (:name (:fiber :switch-fiber :value-in))
  ;; SWITCH-FIBER delivers VALUE to the resumed fiber as the return
  ;; value of its own (preceding) YIELD-FIBER / SWITCH-FIBER call.
  (let* ((main (make-main-fiber))
         (received nil)
         (child (make-fiber
                 (lambda ()
                   (setf received (yield-fiber))))))
    (switch-fiber main child)           ; first entry, no value
    (switch-fiber main child :hello)    ; resumed: yield returns :hello
    (assert (eq received :hello))
    (destroy-fiber child)
    (destroy-fiber main)))

(with-test (:name (:fiber :yield-fiber :value-out))
  ;; YIELD-FIBER's argument becomes SWITCH-FIBER's return value in
  ;; the resumer.
  (let* ((main (make-main-fiber))
         (child (make-fiber (lambda () (yield-fiber :from-child)))))
    (assert (eq :from-child (switch-fiber main child)))
    (destroy-fiber child)
    (destroy-fiber main)))

(with-test (:name (:fiber :switch-fiber :entry-fn-return-propagates))
  ;; When a fiber's entry function returns normally, its value is
  ;; delivered to the resumer on auto-return.
  (let* ((main (make-main-fiber))
         (child (make-fiber (lambda () :done))))
    (assert (eq :done (switch-fiber main child)))
    (assert (= (fiber-state child) +fiber-dead+))
    (assert (not (fiber-alive-p child)))
    (destroy-fiber child)
    (destroy-fiber main)))

(with-test (:name (:fiber :yield-fiber :no-return-fiber-errors))
  (let ((main (make-main-fiber)))
    (assert-error (yield-fiber))
    (destroy-fiber main)))

;;; --- Switch validation ---

(with-test (:name (:fiber :switch-fiber :destroyed-errors))
  (let* ((main (make-main-fiber))
         (child (make-fiber (lambda ()))))
    (destroy-fiber child)
    (assert-error (switch-fiber main child))
    (destroy-fiber main)))

(with-test (:name (:fiber :switch-fiber :wrong-thread-errors))
  (let* ((main (make-main-fiber))
         (other (sb-thread:make-thread
                 (lambda () (make-fiber (lambda ()))))))
    (let ((foreign (sb-thread:join-thread other)))
      (assert-error (switch-fiber main foreign))
      (destroy-fiber foreign))
    (destroy-fiber main)))

;;; --- Dynamic environment isolation across switches ---

(defvar *fiber-test-var* :main-value)

(with-test (:name (:fiber :special-variable-isolation))
  (let* ((main (make-main-fiber))
         (seen nil)
         (child nil))
    (setf child (make-fiber
                 (lambda ()
                   (setf seen *fiber-test-var*)
                   (switch-fiber child main))))
    (let ((*fiber-test-var* :rebound))
      (switch-fiber main child)
      (assert (eq *fiber-test-var* :rebound)))
    (assert (eq seen :rebound))
    (destroy-fiber child)
    (destroy-fiber main)))

(with-test (:name (:fiber :handler-case-across-switch))
  (let* ((main (make-main-fiber))
         (child nil)
         (caught nil))
    (setf child (make-fiber
                 (lambda ()
                   (handler-case
                       (progn (switch-fiber child main)
                              (error "test error"))
                     (error (c) (setf caught (princ-to-string c))))
                   (switch-fiber child main))))
    (switch-fiber main child)
    (switch-fiber main child)
    (assert (string= caught "test error"))
    (destroy-fiber child)
    (destroy-fiber main)))

(with-test (:name (:fiber :unwind-protect-across-switch))
  (let* ((main (make-main-fiber))
         (child nil)
         (cleanup-ran nil))
    (setf child (make-fiber
                 (lambda ()
                   (unwind-protect (switch-fiber child main)
                     (setf cleanup-ran t))
                   (switch-fiber child main))))
    (switch-fiber main child)
    (switch-fiber main child)
    (assert cleanup-ran)
    (destroy-fiber child)
    (destroy-fiber main)))

(with-test (:name (:fiber :throw-cannot-escape-fiber))
  ;; THROW out of a fiber to a CATCH on the caller's stack is
  ;; intercepted (control-error) -- it would otherwise unwind to a
  ;; frame on a different stack.  Distinguish "outer caught" vs
  ;; "fell through" via the entry-fn's terminal sentinel value.
  (let* ((main (make-main-fiber))
         (child-caught nil)
         (child (make-fiber
                 (lambda ()
                   (handler-case (throw 'outer :outer-fired)
                     (control-error () (setf child-caught t)))
                   :fell-through))))
    (let ((result (catch 'outer (switch-fiber main child))))
      (assert child-caught () "child did not catch CONTROL-ERROR")
      (assert (eq result :fell-through) ()
              "throw escaped to outer catch (got ~S)" result))
    (destroy-fiber child)
    (destroy-fiber main)))

;;; --- Pinning ---

(with-test (:name (:fiber :with-fiber-pinned :predicate))
  (let ((main (make-main-fiber)))
    (assert (not (fiber-pinned-p main)))
    (with-fiber-pinned ()
      (assert (fiber-pinned-p main))
      (with-fiber-pinned ()
        (assert (= 2 (sb-fiber::fiber-pin-count main))))
      (assert (fiber-pinned-p main)))
    (assert (not (fiber-pinned-p main)))
    (destroy-fiber main)))

(with-test (:name (:fiber :with-fiber-pinned :no-current-fiber-errors))
  (let ((sb-fiber::*current-fiber* nil))
    (assert-error (with-fiber-pinned () :unreachable))))

(with-test (:name (:fiber :with-fiber-pinned :switch-from-pinned-errors))
  (let* ((main  (make-main-fiber))
         (child (make-fiber (lambda ()))))
    (with-fiber-pinned ()
      (assert-error (switch-fiber main child)))
    (destroy-fiber child)
    (destroy-fiber main)))

(with-test (:name (:fiber :with-fiber-pinned :released-on-non-local-exit))
  (let ((main (make-main-fiber)))
    (block nil
      (with-fiber-pinned ()
        (return)))
    (assert (not (fiber-pinned-p main)))
    (destroy-fiber main)))
