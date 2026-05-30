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

(defmacro with-main-fiber ((main) &body body)
  `(let ((,main (make-main-fiber)))
     (unwind-protect (progn ,@body)
       (release-fiber ,main))))

(defmacro with-main+child ((main child entry-fn &rest make-args) &body body)
  `(with-main-fiber (,main)
     (with-fiber (,child ,entry-fn ,@make-args) ,@body)))

;;; --- Lifecycle ---

(with-test (:name (:fiber :make-and-destroy))
  (with-fiber-thread ()
    (let ((f (make-fiber (lambda ()))))
      (assert (fiber-alive-p f))
      (assert (eq (fiber-state f) :new))
      (release-fiber f)
      (assert (not (fiber-alive-p f)))
      ;; Double destroy is a no-op.
      (release-fiber f))))

(with-test (:name (:fiber :make-fiber :custom-sizes))
  (with-fiber-thread ()
    (let ((f (make-fiber (lambda ()) :stack-size 131072 :binding-stack-size 16384)))
      (assert (fiber-alive-p f))
      (release-fiber f))))

(with-test (:name (:fiber :make-fiber :requires-main-fiber))
  (let ((sb-fiber::*current-fiber* nil))
    (assert-error (make-fiber (lambda ())))))

(with-test (:name (:fiber :with-fiber-cleans-up))
  (with-fiber-thread ()
    (let (captured)
      (with-fiber (f (lambda ()) :stack-size 65536)
        (setf captured f)
        (assert (fiber-alive-p f)))
      (assert (not (fiber-alive-p captured))))))

(with-test (:name (:fiber :with-fiber-thread :creates-and-releases))
  (let ((sb-fiber::*current-fiber* nil))
    (with-fiber-thread ()
      (assert (typep *current-fiber* 'fiber))
      (assert (fiber-alive-p *current-fiber*)))
    (assert (null *current-fiber*))))

(with-test (:name (:fiber :with-fiber-thread :nested-is-noop))
  ;; If *CURRENT-FIBER* is already bound, the inner WITH-FIBER-THREAD
  ;; must not create or release anything.
  (with-main-fiber (outer)
    (with-fiber-thread ()
      (assert (eq outer *current-fiber*)))
    (assert (eq outer *current-fiber*))
    (assert (fiber-alive-p outer))))

(with-test (:name (:fiber :make-main-fiber))
  (with-main-fiber (main)
    (assert (fiber-alive-p main))
    (assert (eq main *current-fiber*))))

(with-test (:name (:fiber :fiber-thread))
  (with-main+child (main f (lambda ()))
    (assert (eq sb-thread:*current-thread* (fiber-thread main)))
    (assert (eq sb-thread:*current-thread* (fiber-thread f))))
  ;; Foreign fiber carries its creating thread.
  (let* ((other (sb-thread:make-thread
                 (lambda ()
                   (make-main-fiber)
                   (make-fiber (lambda ())))
                 :name "fiber-thread-test"))
         (foreign (sb-thread:join-thread other)))
    (assert (eq other (fiber-thread foreign)))
    (release-fiber foreign)
    (assert (null (fiber-thread foreign)))))

(with-test (:name (:fiber :name :is-accessible-and-printed))
  (with-fiber-thread ()
    (with-fiber (f (lambda ()) :name "worker")
      (assert (equal "worker" (fiber-name f)))
      (assert (search "worker" (princ-to-string f))
              ()
              "expected PRINT-OBJECT to mention the name, got ~S"
              (princ-to-string f)))
    (with-fiber (f (lambda ()))
      (assert (null (fiber-name f))))))

(with-test (:name (:fiber :join-fiber))
  ;; join-fiber drives FIBER to completion, propagates entry-fn return
  ;; values (including zero), and leaves the fiber DEAD.
  (with-main+child (main child (lambda () (yield-fiber :step) :done))
    (assert (eq :done (join-fiber child)))
    (assert (eq (fiber-state child) :dead)))
  (with-main+child (main child (lambda ()
                                 (yield-fiber :step)
                                 (values :a 1 #\x)))
    (multiple-value-bind (k n c) (join-fiber child)
      (assert (eq :a k))
      (assert (eql 1 n))
      (assert (eql #\x c))))
  (with-main+child (main child (lambda () (yield-fiber :step) (values)))
    (assert (equal '() (multiple-value-list (join-fiber child))))))

(with-test (:name (:fiber :yield-fiber :zero-values))
  (with-main+child (main child (lambda () (yield-fiber)))
    (assert (equal '() (multiple-value-list (switch-fiber main child))))))

;;; --- Interrupt / condition staging ---

(with-test (:name (:fiber :interrupt-fiber :new-never-runs))
  (let (ran)
    (with-main+child (main f (lambda () (setf ran t) :unreached))
      (interrupt-fiber f (make-condition 'simple-error
                                         :format-control "cancelled"))
      (handler-case (switch-fiber main f)
        (simple-error (e)
          (assert (search "cancelled" (princ-to-string e)))))
      (assert (null ran)))))

(with-test (:name (:fiber :interrupt-fiber :runnable-fires-in-context))
  (let (caught)
    (with-main+child (main f (lambda ()
                               (handler-case (yield-fiber)
                                 (simple-error (e) (setf caught (princ-to-string e))))
                               :done))
      (switch-fiber main f)             ; run to the yield
      (interrupt-fiber f (make-condition 'simple-error
                                         :format-control "ouch"))
      (assert (eq :done (join-fiber f)))
      (assert (string= caught "ouch")))))

(with-test (:name (:fiber :fiber-condition))
  (with-fiber-thread ()
    (with-fiber (f (lambda ()))
      (assert (null (fiber-condition f)))
      (let ((c (make-condition 'simple-error :format-control "x")))
        (interrupt-fiber f c)
        (assert (eq c (fiber-condition f)))))))

(with-test (:name (:fiber :interrupt-fiber :stays-staged-across-yield))
  ;; A condition staged via INTERRUPT-FIBER must fire only when the
  ;; target actually resumes -- not when the target yields back to
  ;; its resumer.  Regression: the interrupt and escape channels are
  ;; separate; only an entry-function escape propagates to the
  ;; resumer.
  (let ((c (make-condition 'simple-error :format-control "staged"))
        caught-in-child)
    (with-main+child (main child
                           (lambda ()
                             (interrupt-fiber *current-fiber* c)
                             (handler-case (yield-fiber)
                               (simple-error (e) (setf caught-in-child e)))
                             :done))
      (handler-case (switch-fiber main child)
        (simple-error (e)
          (error "condition leaked to resumer: ~S" e)))
      (assert (eq c (fiber-condition child)) ()
              "condition no longer staged on child after yield")
      (assert (eq :done (join-fiber child)))
      (assert (eq c caught-in-child) ()
              "child did not see staged condition on resume"))))

(with-test (:name (:fiber :interrupt-fiber :dead-errors))
  (with-fiber-thread ()
    (let ((f (make-fiber (lambda ()))))
      (release-fiber f)
      (assert-error
       (interrupt-fiber f (make-condition 'simple-error
                                          :format-control "x"))
       dead-fiber-error))))

;;; --- Control transfer ---

(with-test (:name (:fiber :switch-fiber :basic))
  (let (log)
    (with-main+child (main child
                           (lambda ()
                             (push :child log)
                             (switch-fiber *current-fiber*
                                           (fiber-return-fiber *current-fiber*))
                             (push :child-again log)))
      (push :main log)
      (switch-fiber main child)
      (push :main-again log)
      (switch-fiber main child)
      (assert (equal (nreverse log) '(:main :child :main-again :child-again))))))

(with-test (:name (:fiber :switch-fiber :value-in))
  ;; SWITCH-FIBER delivers VALUE to the resumed fiber as the return
  ;; value of its own (preceding) YIELD-FIBER / SWITCH-FIBER call.
  (let (received)
    (with-main+child (main child
                           (lambda () (setf received (yield-fiber))))
      (switch-fiber main child)           ; first entry, no value
      (switch-fiber main child :hello)    ; resumed: yield returns :hello
      (assert (eq received :hello)))))

(with-test (:name (:fiber :yield-fiber :value-out))
  ;; YIELD-FIBER's argument becomes SWITCH-FIBER's return value in
  ;; the resumer.
  (with-main+child (main child (lambda () (yield-fiber :from-child)))
    (assert (eq :from-child (switch-fiber main child)))))

(with-test (:name (:fiber :switch-fiber :entry-fn-return-propagates))
  ;; When a fiber's entry function returns normally, its value is
  ;; delivered to the resumer on auto-return.
  (with-main+child (main child (lambda () :done))
    (assert (eq :done (switch-fiber main child)))
    (assert (eq (fiber-state child) :dead))
    (assert (not (fiber-alive-p child)))))

(with-test (:name (:fiber :yield-fiber :no-return-fiber-errors))
  (with-main-fiber (main)
    (assert (eq main *current-fiber*))
    (assert-error (yield-fiber))))

(with-test (:name (:fiber :resume-fiber :basic))
  ;; RESUME-FIBER is (SWITCH-FIBER *CURRENT-FIBER* TO . VALUES); test
  ;; both directions of the value channel including multiple values.
  (with-main+child (main child
                         (lambda ()
                           (let ((v (multiple-value-list (yield-fiber :a 1 #\x))))
                             (list :got v))))
    (multiple-value-bind (k n c) (resume-fiber child)
      (assert (eq :a k))
      (assert (eql 1 n))
      (assert (eql #\x c)))
    (assert (equal '(:got (99)) (resume-fiber child 99)))))

(with-test (:name (:fiber :resume-fiber :no-current-fiber-errors))
  (with-main+child (main f (lambda ()))
    (assert (eq main *current-fiber*))
    (let ((sb-fiber::*current-fiber* nil))
      (assert-error (resume-fiber f) no-current-fiber-error))))

(with-test (:name (:fiber :yield-preserves-resumer-return-fiber))
  (with-main-fiber (main)
    (let* ((g2 (make-fiber (lambda () (yield-fiber 42))))
           (g1 (make-fiber
                (lambda ()
                  (let ((v (resume-fiber g2)))
                    (yield-fiber v))))))
      (assert (eql 42 (resume-fiber g1)))
      (release-fiber g1)
      (release-fiber g2))))

(with-test (:name (:fiber :stack-usage-accessors))
  (let* ((stack-size 131072)
         (bs-size    16384))
    (with-main-fiber (main)
      (let ((f (make-fiber (lambda ()
                             (progv '(*print-base*) '(16) (yield-fiber)))
                           :stack-size stack-size
                           :binding-stack-size bs-size)))
        (assert (= stack-size (fiber-control-stack-size f)))
        (assert (= bs-size    (fiber-binding-stack-size f)))
        (assert (zerop (fiber-binding-stack-usage f)))
        (switch-fiber main f)             ; run to the yield
        (assert (= stack-size (fiber-control-stack-size f)))
        (assert (plusp (fiber-control-stack-usage f)))
        (assert (plusp (fiber-binding-stack-usage f))
                ()
                "expected PROGV to grow the binding stack; got ~D"
                (fiber-binding-stack-usage f))
        (release-fiber f)
        (assert (zerop (fiber-control-stack-size f)))
        (assert (zerop (fiber-control-stack-usage f)))
        (assert (zerop (fiber-binding-stack-size f)))
        (assert (zerop (fiber-binding-stack-usage f))))
      ;; Main fiber: binding-stack-size = 0 (not owned); other three
      ;; should be queryable without error.
      (assert (zerop (fiber-binding-stack-size main)))
      (fiber-control-stack-size main)
      (fiber-control-stack-usage main)
      (fiber-binding-stack-usage main))))

;;; --- Switch validation ---

(with-test (:name (:fiber :switch-fiber :destroyed-errors))
  (with-main-fiber (main)
    (let ((child (make-fiber (lambda ()))))
      (release-fiber child)
      (assert-error (switch-fiber main child) dead-fiber-error))))

(with-test (:name (:fiber :switch-fiber :wrong-thread-errors))
  (with-main-fiber (main)
    (let* ((other (sb-thread:make-thread
                   (lambda ()
                     (make-main-fiber)
                     (make-fiber (lambda ())))))
           (foreign (sb-thread:join-thread other)))
      (assert-error (switch-fiber main foreign) fiber-thread-mismatch-error)
      (release-fiber foreign))))

;;; --- Dynamic environment isolation across switches ---

(defvar *fiber-test-var* :main-value)

(with-test (:name (:fiber :special-variable-isolation))
  (with-main-fiber (main)
    (let* ((seen nil)
           (child nil))
      (setf child (make-fiber
                   (lambda ()
                     (setf seen *fiber-test-var*)
                     (switch-fiber child main))))
      (let ((*fiber-test-var* :rebound))
        (switch-fiber main child)
        (assert (eq *fiber-test-var* :rebound)))
      (assert (eq seen :rebound))
      (release-fiber child))))

(with-test (:name (:fiber :handler-case-across-switch))
  (with-main-fiber (main)
    (let* ((child nil)
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
      (release-fiber child))))

(with-test (:name (:fiber :unwind-protect-across-switch))
  (with-main-fiber (main)
    (let* ((child nil)
           (cleanup-ran nil))
      (setf child (make-fiber
                   (lambda ()
                     (unwind-protect (switch-fiber child main)
                       (setf cleanup-ran t))
                     (switch-fiber child main))))
      (switch-fiber main child)
      (switch-fiber main child)
      (assert cleanup-ran)
      (release-fiber child))))

(with-test (:name (:fiber :throw-cannot-escape-fiber))
  ;; THROW out of a fiber to a CATCH on the caller's stack is
  ;; intercepted (control-error) -- it would otherwise unwind to a
  ;; frame on a different stack.  Distinguish "outer caught" vs
  ;; "fell through" via the entry-fn's terminal sentinel value.
  (let (child-caught)
    (with-main+child (main child
                           (lambda ()
                             (handler-case (throw 'outer :outer-fired)
                               (control-error () (setf child-caught t)))
                             :fell-through))
      (let ((result (catch 'outer (switch-fiber main child))))
        (assert child-caught () "child did not catch CONTROL-ERROR")
        (assert (eq result :fell-through) ()
                "throw escaped to outer catch (got ~S)" result)))))

;;; --- Pinning ---

(with-test (:name (:fiber :with-fiber-pinned :predicate))
  (with-main-fiber (main)
    (assert (not (fiber-pinned-p main)))
    (with-fiber-pinned ()
      (assert (fiber-pinned-p main))
      (with-fiber-pinned ()
        (assert (= 2 (fiber-pin-count main))))
      (assert (fiber-pinned-p main)))
    (assert (not (fiber-pinned-p main)))))

(with-test (:name (:fiber :with-fiber-pinned :no-current-fiber-errors))
  (let ((sb-fiber::*current-fiber* nil))
    (assert-error (with-fiber-pinned () :unreachable) no-current-fiber-error)))

(with-test (:name (:fiber :with-fiber-pinned :switch-from-pinned-errors))
  (with-main+child (main child (lambda ()))
    (with-fiber-pinned ()
      (assert-error (switch-fiber main child) pinned-fiber-error))))

;;; --- Conditions ---------------------------------------------------------

(with-test (:name (:fiber :conditions :base-catches-subclasses))
  ;; A scheduler can HANDLER-CASE on FIBER-ERROR to catch all sb-fiber
  ;; precondition failures uniformly.
  (with-main-fiber (main)
    (let ((dead (make-fiber (lambda ()))))
      (release-fiber dead)
      (let ((caught
              (handler-case (switch-fiber main dead)
                (fiber-error (c) c))))
        (assert (typep caught 'dead-fiber-error))
        (assert (eq dead (fiber-error-fiber caught)))))))

(with-test (:name (:fiber :conditions :state-error-slots))
  ;; FIBER-STATE-ERROR exposes the actual and expected states.
  (with-main+child (main child (lambda ()))
    (switch-fiber main child)           ; runs to completion, now :dead
    (handler-case (switch-fiber main child)
      (fiber-state-error (c)
        (assert (eq child (fiber-error-fiber c)))
        (assert (eq :dead (fiber-state-error-state c)))
        (assert (equal '(:runnable :new) (fiber-state-error-expected c)))))))

(with-test (:name (:fiber :conditions :pinned-error-depth))
  (with-main+child (main child (lambda ()))
    (with-fiber-pinned ()
      (with-fiber-pinned ()
        (handler-case (switch-fiber main child)
          (pinned-fiber-error (c)
            (assert (eq main (fiber-error-fiber c)))
            (assert (= 2 (pinned-fiber-error-depth c)))))))))

(with-test (:name (:fiber :conditions :no-current-fiber))
  (let ((sb-fiber::*current-fiber* nil))
    (let ((c (handler-case (make-fiber (lambda ()))
               (fiber-error (e) e))))          ; caught via the base class
      (assert (typep c 'no-current-fiber-error))
      (assert (eq 'make-fiber (no-current-fiber-error-operation c)))
      (assert (null (fiber-error-fiber c))))
    (let ((c (handler-case (yield-fiber)
               (no-current-fiber-error (e) e))))
      (assert (eq 'yield-fiber (no-current-fiber-error-operation c))))))

(with-test (:name (:fiber :conditions :thread-mismatch-role))
  (with-main-fiber (main)
    (let* ((other (sb-thread:make-thread
                   (lambda ()
                     (make-main-fiber)
                     (make-fiber (lambda ())))))
           (foreign (sb-thread:join-thread other)))
      (handler-case (switch-fiber main foreign)
        (fiber-thread-mismatch-error (c)
          (assert (eq foreign (fiber-error-fiber c)))
          (assert (eq :to (fiber-thread-mismatch-error-role c)))))
      (release-fiber foreign))))

(with-test (:name (:fiber :with-fiber-pinned :released-on-non-local-exit))
  (with-main-fiber (main)
    (block nil
      (with-fiber-pinned ()
        (return)))
    (assert (not (fiber-pinned-p main)))))
