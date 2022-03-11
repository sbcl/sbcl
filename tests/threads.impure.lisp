;;;; miscellaneous tests of thread stuff

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;
;;;; This software is in the public domain and is provided with
;;;; absoluely no warranty. See the COPYING and CREDITS files for
;;;; more information.


;;;; STRUCTURAL TESTS

(shadowing-import 'assertoid:assert-error)
(use-package "SB-THREAD")
(use-package "SB-SYS")

(setf sb-unix::*on-dangerous-wait* :error)

(with-test (:name (:threads :trivia))
  (assert (eq *current-thread*
              (find (thread-name *current-thread*) (list-all-threads)
                    :key #'thread-name :test #'equal)))

  (assert (thread-alive-p *current-thread*)))

(with-test (:name (with-mutex :basics))
  (let ((mutex (make-mutex)))
    (with-mutex (mutex)
      mutex)))

(sb-alien:define-alien-routine "check_deferrables_blocked_or_lose"
    void
  (where sb-alien:unsigned-long))
(sb-alien:define-alien-routine "check_deferrables_unblocked_or_lose"
    void
  (where sb-alien:unsigned-long))

(with-test (:name (interrupt-thread :basics :no-unwinding))
  (let ((a 0))
    (interrupt-thread *current-thread* (lambda () (setq a 1)))
    (process-all-interrupts)
    (assert (eql a 1))))

(with-test (:name (interrupt-thread :deferrables-blocked))
  (interrupt-thread *current-thread*
                    (lambda ()
                      ;; Make sure sb-ext:gc doesn't leave the
                      ;; deferrables unblocked
                      (sb-ext:gc)
                      (check-deferrables-blocked-or-lose 0)))
  (process-all-interrupts))

(with-test (:name (interrupt-thread :deferrables-unblocked))
  (interrupt-thread *current-thread*
                    (lambda ()
                      (with-interrupts
                        (check-deferrables-unblocked-or-lose 0))))
  (process-all-interrupts))

(with-test (:name (interrupt-thread :nlx))
  (catch 'xxx
    (interrupt-thread *current-thread*
                      (lambda ()
                        (check-deferrables-blocked-or-lose 0)
                        (throw 'xxx nil)))
    (process-all-interrupts))
  (check-deferrables-unblocked-or-lose 0))

#-sb-thread (sb-ext:exit :code 104)

;;;; Now the real tests...

(with-test (:name (with-mutex :timeout))
  (let ((m (make-mutex)))
    (with-mutex (m)
      (assert (null (join-thread (make-thread
                                  (lambda ()
                                    (with-mutex (m :timeout 0.1)
                                      t)))))))
    (assert (join-thread (make-thread
                          (lambda ()
                            (with-mutex (m :timeout 0.1)
                              t)))))))

;;; compare-and-swap

(defmacro defincf (name accessor &rest args)
  `(defun ,name (x)
     (let* ((old (,accessor x ,@args))
            (new (1+ old)))
       (loop until (eq old (sb-ext:compare-and-swap (,accessor x ,@args) old new))
             do (setf old (,accessor x ,@args)
                      new (1+ old)))
       new)))

(defstruct cas-struct (slot 0))

(defincf incf-car car)
(defincf incf-cdr cdr)
(defincf incf-slot cas-struct-slot)
(defincf incf-symbol-value symbol-value)
(defincf incf-svref/1 svref 1)
(defincf incf-svref/0 svref 0)

(macrolet
    ((def (name init incf op)
       `(with-test (:name ,name)
          (let* ((n 200000)
                 (x ,init)
                 (run nil)
                 (threads
                  (loop repeat 10
                        collect (make-thread
                                 (lambda ()
                                   (loop until run do (thread-yield))
                                   (loop repeat n do (,incf x)))))))
            (setf run t)
            (map nil #'join-thread threads)
            (assert (= (,op x) (* 10 n)))))))

  (def (cas car) (cons 0 nil) incf-car car)
  (def (cas cdr) (cons nil 0) incf-cdr cdr)
  (def (cas :slot) (make-cas-struct) incf-slot cas-struct-slot)
  (def (cas :value)
      (let ((x '.x.))
        (set x 0)
        x)
    incf-symbol-value
    symbol-value)
  (def (cas :svref/0) (vector 0 nil) incf-svref/0 (lambda (x) (svref x 0)))
  (def (cas :svref/1) (vector nil 0) incf-svref/1 (lambda (x) (svref x 1))))

(with-test (:name (:threads :more-trivia))
  (let ((old-threads (list-all-threads))
        (thread (make-thread
                 (lambda ()
                   ;; I honestly have no idea what this is testing.
                   ;; It seems to be nothing more than an implementation change detector test.
                   (assert (sb-thread::avl-find
                            (sb-thread::thread-primitive-thread sb-thread:*current-thread*)
                            sb-thread::*all-threads*))
                   (sleep 2))))
        (new-threads (list-all-threads)))
    (assert (thread-alive-p thread))
    ;; there is no order guarantee
    ;;(assert (eq thread (first new-threads)))
    (assert (= (1+ (length old-threads)) (length new-threads)))
    (sleep 3)
    (assert (not (thread-alive-p thread)))))

(with-test (:name (join-thread :abort :default))
  (let* ((sym (gensym))
         (thread (make-thread (lambda () (abort-thread)))))
    (assert (equal (multiple-value-list
                    (join-thread thread :default sym))
                   (list sym :abort)))))

(with-test (:name (join-thread :abort :error))
  (assert-error (join-thread (make-thread (lambda () (abort-thread))))
                join-thread-error))

(with-test (:name (join-thread :timeout :default))
  (let* ((sym (gensym))
         (sem (make-semaphore))
         (thread (make-thread (lambda () (wait-on-semaphore sem)))))
    (assert (equal (multiple-value-list
                    (join-thread thread :timeout .001 :default sym))
                   (list sym :timeout)))
    (signal-semaphore sem)
    (assert (join-thread thread))))

(with-test (:name (join-thread :timeout :error))
  (let* ((sem (make-semaphore))
         (thread (make-thread (lambda () (wait-on-semaphore sem)))))
    (assert-error (join-thread thread :timeout .001) join-thread-error)
    (signal-semaphore sem)
    (assert (join-thread thread))))

(with-test (:name (join-thread :multiple-values))
  (assert (equal '(1 2 3)
                 (multiple-value-list
                  (join-thread (make-thread (lambda () (values 1 2 3))))))))

;; Used to signal a SIMPLE-ERROR about a recursive lock attempt.
(with-test (:name (join-thread :self-join))
  (assert-error (join-thread *current-thread*) join-thread-error))

;;; For one of the interupt-thread tests, we want a foreign function
;;; that does not make syscalls

#-win32
(progn
  ;; When running from a read-only filesystem, and/or under a test scaffold in which
  ;; no C compiler exists, we'll trust that the extra "test data file" was prepared
  ;; already. Moreover we have to assume that it can't get out-of-date with respect
  ;; to this lisp file. i.e. it always contains at least the one C function neccesary.
  ;; (There's logically no "make clean ; make")
  (unless (probe-file "threads-foreign.so")
    (with-open-file (o "threads-foreign.c" :direction :output :if-exists :supersede)
      (format o "void loop_forever() { while(1) ; }~%"))
    (sb-ext:run-program "/bin/sh"
                        '("run-compiler.sh" "-sbcl-pic" "-sbcl-shared"
                          "-o" "threads-foreign.so" "threads-foreign.c"))
    (delete-file "threads-foreign.c"))
  (sb-alien:load-shared-object (truename "threads-foreign.so"))
  (sb-alien:define-alien-routine loop-forever sb-alien:void))

;;; elementary "can we get a lock and release it again"
(with-test (:name (:mutex :basics))
  (let ((l (make-mutex :name "foo"))
        (p *current-thread*))
    (assert (eql (mutex-owner l) nil) nil "1")
    (grab-mutex l)
    (assert (eql (mutex-owner l) p) nil "3")
    (release-mutex l)
    (assert (eql (mutex-owner l) nil) nil "5")))

(with-test (:name (with-recursive-lock :basics))
  (labels ((ours-p (value)
             (eq *current-thread* value)))
    (let ((l (make-mutex :name "rec")))
      (assert (eql (mutex-owner l) nil) nil "1")
      (with-recursive-lock (l)
        (assert (ours-p (mutex-owner l)) nil "3")
        (with-recursive-lock (l)
          (assert (ours-p (mutex-owner l)) nil "4"))
        (assert (ours-p (mutex-owner l)) nil "5"))
      (assert (eql (mutex-owner l) nil) nil "6"))))

(with-test (:name (with-recursive-lock :wait-p))
  (let ((m (make-mutex)))
    (with-mutex (m)
      (assert (null (join-thread (make-thread
                                  (lambda ()
                                    (with-recursive-lock (m :wait-p nil)
                                      t)))))))
    (assert (join-thread (make-thread
                          (lambda ()
                            (with-recursive-lock (m :wait-p nil)
                              t)))))))

(with-test (:name (with-recursive-lock :wait-p :recursive))
  (let ((m (make-mutex)))
    (assert (join-thread (make-thread
                          (lambda ()
                            (with-recursive-lock (m :wait-p nil)
                              (with-recursive-lock (m :wait-p nil)
                                t))))))))

(with-test (:name (with-recursive-lock :timeout))
  (let ((m (make-mutex)))
    (with-mutex (m)
      (assert (null (join-thread (make-thread
                                  (lambda ()
                                    (with-recursive-lock (m :timeout 0.1)
                                      t)))))))
    (assert (join-thread (make-thread
                          (lambda ()
                            (with-recursive-lock (m :timeout 0.1)
                              t)))))))

(with-test (:name (with-recursive-lock :timeout :recursive))
  (let ((m (make-mutex)))
    (assert (join-thread (make-thread
                          (lambda ()
                            (with-recursive-lock (m :timeout 0.1)
                              (with-recursive-lock (m :timeout 0.1)
                                t))))))))

(with-test (:name (:mutex :nesting-mutex-and-recursive-lock))
  (let ((l (make-mutex :name "a mutex")))
    (with-mutex (l)
      (with-recursive-lock (l)))))

(with-test (:name (condition-wait :basics-1))
  (let ((queue (make-waitqueue :name "queue"))
        (lock (make-mutex :name "lock"))
        (n 0))
    (labels ((in-new-thread ()
               (with-mutex (lock)
                 (assert (eql (mutex-owner lock) *current-thread*))
                 (format t "~A got mutex~%" *current-thread*)
                 ;; now drop it and sleep
                 (condition-wait queue lock)
                 ;; after waking we should have the lock again
                 (assert (eql (mutex-owner lock) *current-thread*))
                 (assert (eql n 1))
                 (decf n))))
      (make-join-thread #'in-new-thread)
      (sleep 2)            ; give it  a chance to start
      ;; check the lock is free while it's asleep
      (format t "parent thread ~A~%" *current-thread*)
      (assert (eql (mutex-owner lock) nil))
      (with-mutex (lock)
        (incf n)
        (condition-notify queue))
      (sleep 1))))

(with-test (:name (condition-wait :basics-2))
  (let ((queue (make-waitqueue :name "queue"))
        (lock (make-mutex :name "lock")))
    (labels ((ours-p (value)
               (eq *current-thread* value))
             (in-new-thread ()
               (with-recursive-lock (lock)
                 (assert (ours-p (mutex-owner lock)))
                 (format t "~A got mutex~%" (mutex-owner lock))
                 ;; now drop it and sleep
                 (condition-wait queue lock)
                 ;; after waking we should have the lock again
                 (format t "woken, ~A got mutex~%" (mutex-owner lock))
                 (assert (ours-p (mutex-owner lock))))))
      (make-join-thread #'in-new-thread)
      (sleep 2)            ; give it  a chance to start
      ;; check the lock is free while it's asleep
      (format t "parent thread ~A~%" *current-thread*)
      (assert (eql (mutex-owner lock) nil))
      (with-recursive-lock (lock)
        (condition-notify queue))
      (sleep 1))))

;;; GRAB-MUTEX

(with-test (:name (grab-mutex :waitp nil))
  (let ((m (make-mutex)))
    (with-mutex (m)
      (assert (null (join-thread (make-thread
                                  #'(lambda ()
                                      (grab-mutex m :waitp nil)))))))))

(with-test (:name (grab-mutex :timeout :acquisition-fail))
  (let ((m (make-mutex))
        (w (make-semaphore)))
    (with-mutex (m)
      (let ((th (make-thread
                 #'(lambda ()
                     (prog1
                         (grab-mutex m :timeout 0.1)
                       (signal-semaphore w))))))
        ;; Wait for it to -- otherwise the detect the deadlock chain
        ;; from JOIN-THREAD.
        (wait-on-semaphore w)
        (assert (null (join-thread th)))))))

(with-test (:name (grab-mutex :timeout :acquisition-success))
  (let ((m (make-mutex))
        (child))
    (with-mutex (m)
      (setq child (make-thread #'(lambda () (grab-mutex m :timeout 1.0))))
      (sleep 0.2))
    (assert (eq (join-thread child) 't))))

(with-test (:name (grab-mutex :timeout+deadline :lp-1727789))
  (flet ((test (deadline)
           (let ((m (make-mutex))
                 (w (make-semaphore)))
             (with-mutex (m)
               (let ((th (make-thread #'(lambda ()
                                          (sb-sys:with-deadline (:seconds 0.0)
                                            (handler-case
                                                (grab-mutex m :timeout deadline)
                                              (sb-sys:deadline-timeout ()
                                                (signal-semaphore w)
                                                :deadline)))))))
                 (wait-on-semaphore w)
                 (assert (eq (join-thread th) :deadline)))))))
    (test 0.0)
    (test 10000000000000000000000)))

(with-test (:name (grab-mutex :waitp+deadline))
  (let ((m (make-mutex)))
    (with-mutex (m)
      (assert (eq (join-thread
                   (make-thread #'(lambda ()
                                    (sb-sys:with-deadline (:seconds 0.0)
                                      (handler-case
                                          (grab-mutex m :waitp nil)
                                        (sb-sys:deadline-timeout ()
                                          :deadline))))))
                  'nil)))))

;;; semaphores

(defmacro raises-timeout-p (&body body)
  `(handler-case (progn (progn ,@body) nil)
    (sb-ext:timeout () t)))

(with-test (:name (semaphore :wait-forever))
  (let ((sem (make-semaphore :count 0)))
    (assert (raises-timeout-p
              (sb-ext:with-timeout 0.1
                (wait-on-semaphore sem))))))

(with-test (:name (semaphore :initial-count))
  (let ((sem (make-semaphore :count 1)))
    (sb-ext:with-timeout 0.1
      (assert (= 0 (wait-on-semaphore sem))))))

(with-test (:name (semaphore :wait-then-signal))
  (let ((sem (make-semaphore))
        (signalled-p nil))
    (make-join-thread (lambda ()
                        (sleep 0.1)
                        (setq signalled-p t)
                        (signal-semaphore sem)))
    (assert (= 0 (wait-on-semaphore sem)))
    (assert signalled-p)))

(with-test (:name (semaphore :signal-then-wait))
  (let ((sem (make-semaphore))
        (signalled-p nil))
    (make-join-thread (lambda ()
                        (signal-semaphore sem)
                        (setq signalled-p t)))
    (loop until signalled-p)
    (assert (= 0 (wait-on-semaphore sem)))
    (assert signalled-p)))

(defun test-semaphore-multiple-signals (wait-on-semaphore)
  (let* ((sem (make-semaphore :count 5))
         (threads (loop repeat 20 collecting
                        (make-join-thread (lambda ()
                                            (funcall wait-on-semaphore sem))))))
    (flet ((count-live-threads ()
             (count-if #'thread-alive-p threads)))
      (sleep 0.5)
      (assert (= 15 (count-live-threads)))
      (signal-semaphore sem 10)
      (sleep 0.5)
      (assert (= 5 (count-live-threads)))
      (signal-semaphore sem 3)
      (sleep 0.5)
      (assert (= 2 (count-live-threads)))
      (signal-semaphore sem 4)
      (sleep 0.5)
      (assert (= 0 (count-live-threads))))))

(with-test (:name (semaphore :multiple-signals))
  (test-semaphore-multiple-signals #'wait-on-semaphore))

(with-test (:name (try-semaphore :trivial-fail))
  (assert (eq (try-semaphore (make-semaphore :count 0)) 'nil)))

(with-test (:name (try-semaphore :trivial-success))
  (let ((sem (make-semaphore :count 1)))
    (assert (= 0 (try-semaphore sem)))
    (assert (zerop (semaphore-count sem)))))

(with-test (:name (try-semaphore :trivial-fail :n>1))
  (assert (eq (try-semaphore (make-semaphore :count 1) 2) 'nil)))

(with-test (:name (try-semaphore :trivial-success :n>1))
  (let ((sem (make-semaphore :count 10)))
    (assert (= 5 (try-semaphore sem 5)))
    (assert (= 0 (try-semaphore sem 5)))
    (assert (zerop (semaphore-count sem)))))

(with-test (:name (try-semaphore :emulate-wait-on-semaphore))
  (flet ((busy-wait-on-semaphore (sem)
           (loop until (try-semaphore sem) do (sleep 0.001))))
    (test-semaphore-multiple-signals #'busy-wait-on-semaphore)))

;;; Here we test that interrupting TRY-SEMAPHORE does not leave a
;;; semaphore in a bad state.
(with-test (:name (try-semaphore :interrupt-safe)
            :broken-on :win32)
  (flet ((make-threads (count fn)
           (loop repeat count collect (make-thread fn)))
         (kill-thread (thread)
           (when (thread-alive-p thread)
             (ignore-errors (terminate-thread thread))))
         (count-live-threads (threads)
           (count-if #'thread-alive-p threads)))
    ;; WAITERS will already be waiting on the semaphore while
    ;; threads-being-interrupted will perform TRY-SEMAPHORE on that
    ;; semaphore, and MORE-WAITERS are new threads trying to wait on
    ;; the semaphore during the interruption-fire.
    (let* ((sem (make-semaphore :count 100))
           (waiters (make-threads 20 #'(lambda ()
                                         (wait-on-semaphore sem))))
           (triers  (make-threads 40 #'(lambda ()
                                         (sleep (random 0.01))
                                         (try-semaphore sem (1+ (random 5))))))
           (more-waiters
            (loop repeat 10
                  do (kill-thread (nth (random 40) triers))
                  collect (make-thread #'(lambda () (wait-on-semaphore sem)))
                  do (kill-thread (nth (random 40) triers)))))
      (sleep 0.5)
      ;; Now ensure that the waiting threads will all be waked up,
      ;; i.e. that the semaphore is still working.
      (loop repeat (+ (count-live-threads waiters)
                      (count-live-threads more-waiters))
            do (signal-semaphore sem))
      (sleep 0.5)
      (assert (zerop (count-live-threads triers)))
      (assert (zerop (count-live-threads waiters)))
      (assert (zerop (count-live-threads more-waiters))))))

;; At some point %DECREMENT-SEMAPHORE did not adjust the remaining
;; timeout after spurious wakeups, potentially leading to
;; longer/infinite waiting despite the specified timeout.
(with-test (:name (semaphore :timeout :spurious-wakeup))
  (let* ((semaphore (make-semaphore))
         (done nil)
         (thread (make-thread (lambda ()
                                (let ((mutex (sb-thread::semaphore-mutex semaphore))
                                      (queue (sb-thread::semaphore-queue semaphore)))
                                  (loop :until done :do
                                     (with-mutex (mutex)
                                       (condition-notify queue))))))))
    (assert (eq nil (wait-on-semaphore semaphore :timeout .5)))
    (setf done t)
    (join-thread thread)))

;; separate tests for (a) interrupting Lisp code, (b) C code, (c) a syscall,
;; (d) waiting on a lock, (e) some code which we hope is likely to be
;; in pseudo-atomic

(with-test (:name (interrupt-thread :more-basics)
            :broken-on :win32)
  (let ((child (test-interrupt (lambda () (loop)))))
    (terminate-thread child)))

(with-test (:name (interrupt-thread :interrupt-foreign-loop)
                  ;; This feature is explicitly unsupported on Win32.
            :broken-on :win32)
  (test-interrupt #'loop-forever :quit))

(with-test (:name (interrupt-thread :interrupt-sleep)
                  :broken-on :win32)
  (let ((child (test-interrupt (lambda () (loop (sleep 2000))))))
    (terminate-thread child)
    (wait-for-threads (list child))))

(defvar *runningp* nil)

(with-test (:name (interrupt-thread :no-nesting) :broken-on :win32)
  (let ((thread (make-thread (lambda ()
                               (catch 'xxx
                                 (loop))))))
    (declare (special runningp))
    (sleep 0.2)
    (interrupt-thread thread (lambda ()
                               (let ((*runningp* t))
                                 (sleep 1))))
    (sleep 0.2)
    (interrupt-thread thread (lambda ()
                               (throw 'xxx *runningp*)))
    (assert (not (join-thread thread)))))

(with-test (:name (interrupt-thread :nesting) :broken-on :win32)
  (let ((thread (make-thread (lambda ()
                               (catch 'xxx
                                 (loop))))))
    (declare (special runningp))
    (sleep 0.2)
    (interrupt-thread thread (lambda ()
                               (let ((*runningp* t))
                                 (sb-sys:with-interrupts
                                   (sleep 1)))))
    (sleep 0.2)
    (interrupt-thread thread (lambda ()
                               (throw 'xxx *runningp*)))
    (assert (join-thread thread))))

(with-test (:name :all-threads-have-abort-restart
                  :broken-on :win32)
  ;; This test can fail with without the extra semaphore.
  ;; See also TEST-INTERRUPT in test-util for further explanation.
  (let* ((sem (make-semaphore))
         (thread (make-kill-thread
                  (lambda ()
                    (signal-semaphore sem)
                    (sleep 100000000)))))
    (wait-on-semaphore sem)
    (interrupt-thread thread (lambda ()
                               (assert (find-restart 'abort))))
    (process-all-interrupts thread)))

(sb-ext:gc :full t)

;; expose thread creation races by exiting quickly
(with-test (:name (:no-thread-creation-race :light))
  (make-join-thread (lambda ())))

(with-test (:name (:no-thread-creation-race :heavy))
  (loop repeat 20 do
        (wait-for-threads
         (loop for i below 100 collect
               (make-thread (lambda ()))))))

;; interrupt handlers are per-thread with pthreads, make sure the
;; handler installed in one thread is global
(with-test (:name (:global-interrupt-handler))
  (make-join-thread
   (lambda ()
     (sb-ext:run-program "sleep" '("1") :search t :wait nil))))

;;;; Binding stack safety

(defparameter *x* nil)
(defparameter *n-gcs-requested* 0)
(defparameter *n-gcs-done* 0)

(let ((counter 0))
  (defun make-something-big ()
    (let ((x (make-string 32000)))
      (incf counter)
      (let ((counter counter))
        (sb-ext:finalize x (lambda () (format t " ~S" counter)
                                   (force-output)))))))

(defmacro wait-for-gc ()
  `(progn
     (incf *n-gcs-requested*)
     (loop while (< *n-gcs-done* *n-gcs-requested*))))

(defun send-gc ()
  (loop until (< *n-gcs-done* *n-gcs-requested*))
  (format t "G")
  (force-output)
  (sb-ext:gc)
  (incf *n-gcs-done*))

#+(or x86 x86-64 riscv) ;the only platforms with a *binding-stack-pointer* variable
(defun exercise-binding ()
  (loop
   (let ((*x* (make-something-big)))
     (let ((*x* 42))
       ;; at this point the binding stack looks like this:
       ;; NO-TLS-VALUE-MARKER, *x*, SOMETHING, *x*
       t))
   (wait-for-gc)
   ;; sig_stop_for_gc_handler binds FREE_INTERRUPT_CONTEXT_INDEX. By
   ;; now SOMETHING is gc'ed and the binding stack looks like this: 0,
   ;; 0, SOMETHING, 0 (because the symbol slots are zeroed on
   ;; unbinding but values are not).
   (let ((*x* nil)
         (binding-pointer-delta (ash 2 (- sb-vm:word-shift sb-vm:n-fixnum-tag-bits))))
     ;; bump bsp as if a BIND had just started
     (incf sb-vm::*binding-stack-pointer* binding-pointer-delta)
     (wait-for-gc)
     (decf sb-vm::*binding-stack-pointer* binding-pointer-delta))))

#+(or x86 x86-64 riscv) ;the only platforms with a *binding-stack-pointer* variable
(with-test (:name (:binding-stack-gc-safety)
            :broken-on :win32)
  (let (threads)
    (unwind-protect
         (progn
           (push (make-kill-thread #'exercise-binding) threads)
           (push (make-kill-thread (lambda ()
                                     (loop
                                      (sleep 0.1)
                                      (send-gc))))
                 threads)
           (sleep 3))
      (mapc #'terminate-thread threads))))

(with-test (:name :test-%thread-local-references)
  (let ((mysym (gensym))
        (fool1 (cons 1 2))
        (fool2 (cons 2 3)))
    (progv (list mysym) '(nil)
      (let* ((i (sb-kernel:symbol-tls-index mysym))
             (j (+ i sb-vm:n-word-bytes)))
        (assert (eql (sap-ref-word (sb-thread::current-thread-sap) j)
                     sb-vm:no-tls-value-marker-widetag))
        (setf (sap-ref-lispobj (sb-thread::current-thread-sap) i) fool1
              (sap-ref-lispobj (sb-thread::current-thread-sap) j) fool2)
        ;; assert that my pointer arithmetic worked as expected
        (assert (eq (symbol-value mysym) fool1))
        ;; assert that FOOL1 is found by the TLS scan and that FOOL2 is not.
        (let ((list (sb-thread::%thread-local-references)))
          (assert (member fool1 list))
          (assert (not (member fool2 list))))
        ;; repair the TLS entry that was corrupted by the test
        (setf (sap-ref-word (sb-thread::current-thread-sap) j)
              sb-vm:no-tls-value-marker-widetag)))))


#|  ;; a cll post from eric marsden
| (defun crash ()
|   (setq *debugger-hook*
|         (lambda (condition old-debugger-hook)
|           (debug:backtrace 10)
|           (unix:unix-exit 2)))
|   #+live-dangerously
|   (mp::start-sigalrm-yield)
|   (flet ((roomy () (loop (with-output-to-string (*standard-output*) (room)))))
|     (mp:make-process #'roomy)
|     (mp:make-process #'roomy)))
|#

;;; Make sure that a deadline handler is not invoked twice in a row in
;;; CONDITION-WAIT. See LP #512914 for a detailed explanation.
;;;
(with-test (:name (condition-wait :deadlines :LP-512914))
  (let ((n 2)                      ; was empirically enough to trigger the bug
        (mutex (make-mutex))
        (waitq (make-waitqueue))
        (threads nil)
        (deadline-handler-run-twice? nil))
    (dotimes (i n)
      (let ((child
              (make-thread
               (lambda ()
                 (handler-bind
                     ((sb-sys:deadline-timeout
                       (let ((already? nil))
                         #'(lambda (c)
                             (when already?
                               (setq deadline-handler-run-twice? t))
                             (setq already? t)
                             (sleep 0.2)
                             (sb-thread:condition-broadcast waitq)
                             (sb-sys:defer-deadline 10.0 c)))))
                   (sb-sys:with-deadline (:seconds 0.1)
                     (with-mutex (mutex)
                       (condition-wait waitq mutex))))))))
        (push child threads)))
    (mapc #'join-thread threads)
    (assert (not deadline-handler-run-twice?))))

(with-test (:name (:mutex :finalization))
  (let ((a nil))
    (dotimes (i 500000)
      (setf a (make-mutex)))))

;; You have to shoehorn this arbitrary sexpr into a feature expression
;; to have the test summary show that a test was disabled.
#+gencgc
(unless (eql (extern-alien "verify_gens" int)
             (+ sb-vm:+highest-normal-generation+ 2))
  (pushnew :verify-gens *features*))

(with-test (:name :backtrace :broken-on :verify-gens)
  ;; Printing backtraces from several threads at once used to hang the
  ;; whole SBCL process (discovered by accident due to a timer.impure
  ;; test misbehaving). The cause was that packages weren't even
  ;; thread-safe for only doing FIND-SYMBOL, and while printing
  ;; backtraces a loot of symbol lookups need to be done due to
  ;; *PRINT-ESCAPE*.
  (let* ((threads (loop repeat 10
                        collect (make-thread
                                 (lambda ()
                                   (dotimes (i 1000)
                                     (with-output-to-string (*debug-io*)
                                       (sb-debug:print-backtrace :count 10))))))))
    (wait-for-threads threads)))



(defun subtypep-hash-cache-test ()
  (dotimes (i 10000)
    (let ((type1 (random-type 500))
          (type2 (random-type 500)))
      (let ((a (subtypep type1 type2)))
        (dotimes (i 100)
          (assert (eq (subtypep type1 type2) a))))))
  (write-char #\.)
  (force-output))

(with-test (:name (:hash-cache subtypep))
  (mapc #'join-thread
        ;; this didn't "reliably fail" with a small number of threads.
        ;; 30 is a compromise between running time and confidence in the result.
        (loop repeat 30
              collect (make-thread #'subtypep-hash-cache-test)))
  (terpri))

;;;; FUNCTIONAL TESTS

(with-test (:name (:parallel defclass))
  (write-line "WARNING, WILL HANG ON FAILURE!")
  (defclass test-1 () ((a :initform :orig-a)))
  (defclass test-2 () ((b :initform :orig-b)))
  (defclass test-3 (test-1 test-2) ((c :initform :orig-c)))
  ;; This test is more likely to pass on Windows with the FORCE-OUTPUT
  ;; calls disabled in the folloving code.  (As seen on a Server 2012
  ;; installation.)  Clearly, this sort of workaround in a test is
  ;; cheating, and might be hiding the underlying bug that the test is
  ;; exposing.  Let's review this later.
  (let* ((run t)
         (output nil)
         (d1 (sb-thread:make-thread (lambda ()
                                      (loop while run
                                            do (defclass test-1 () ((a :initform :new-a)))
                                            (when output (write-char #\1))
                                            #-win32 (force-output)))
                                    :name "d1"))
         (d2 (sb-thread:make-thread (lambda ()
                                      (loop while run
                                            do (defclass test-2 () ((b :initform :new-b)))
                                               (when output (write-char #\2))
                                               #-win32 (force-output)))
                                    :name "d2"))
         (d3 (sb-thread:make-thread (lambda ()
                                      (loop while run
                                            do (defclass test-3 (test-1 test-2) ((c :initform :new-c)))
                                               (when output (write-char #\3))
                                               #-win32 (force-output)))
                                    :name "d3"))
         (i (sb-thread:make-thread (lambda ()
                                     (loop while run
                                           do (let ((i (make-instance 'test-3)))
                                                (assert (member (slot-value i 'a) '(:orig-a :new-a)))
                                                (assert (member (slot-value i 'b) '(:orig-b :new-b)))
                                                (assert (member (slot-value i 'c) '(:orig-c :new-c))))
                                              (when output (write-char #\i))
                                              #-win32 (force-output)))
                                   :name "i")))
    (format t "~%sleeping!~%")
    (sleep 2.0)
    (format t "~%stopping!~%")
    (setf run nil)
    (mapc (lambda (th)
            (sb-thread:join-thread th)
            (format t "~&joined ~S~%" (sb-thread:thread-name th)))
          (list d1 d2 d3 i))
    (force-output)))

(with-test (:name :spinlock-api)
  (handler-bind ((warning #'error))
    (destructuring-bind (with make get release)
        (assert-signal
         (list (compile nil `(lambda (lock)
                               (sb-thread::with-spinlock (lock)
                                 t)))
               (compile nil `(lambda ()
                               (sb-thread::make-spinlock :name "foo")))
               (compile nil `(lambda (lock)
                               (sb-thread::get-spinlock lock)))
               (compile nil `(lambda (lock)
                               (sb-thread::release-spinlock lock))))
         early-deprecation-warning 4)
      (let ((lock (funcall make)))
        (funcall get lock)
        (funcall release lock)
        (assert (eq t (funcall with lock)))))))

(with-test (:name :interrupt-io-unnamed-pipe
                  :broken-on :win32)
  (let (result)
    (labels
        ((reader (fd)
           (let ((stream (sb-sys:make-fd-stream fd
                                                :element-type :default
                                                :serve-events nil)))
             (time
              (let ((ok (handler-case
                            (catch 'stop
                              (progn
                                (read-char stream)
                                (sleep 0.1)
                                (sleep 0.1)
                                (sleep 0.1)))
                          (error (c)
                            c))))
                (setf result ok)
                (progn
                  (format *trace-output* "~&=> ~A~%" ok)
                  (force-output *trace-output*))))
             (sleep 2)
             (ignore-errors (close stream))))

         (writer ()
           (multiple-value-bind (read write)
               (sb-unix:unix-pipe)
             (let* ((reader (sb-thread:make-thread (lambda () (reader read))))
                    (stream (sb-sys:make-fd-stream write
                                                   :output t
                                                   :element-type :default
                                                   :serve-events nil))
                    (ok :ok))
               (sleep 1)
               (sb-thread:interrupt-thread reader (lambda ()
                                                    (print :throwing)
                                                    (force-output)
                                                    (throw 'stop ok)))
               (sleep 1)
               (setf ok :not-ok)
               (write-char #\x stream)
               (close stream)
               (sb-thread:join-thread reader)))))
      (writer))
    (assert (eq result :ok))))

(with-test (:name :thread-alloca)
  (unless (probe-file "alloca.so")
    (sb-ext:run-program "sh"
                        '("run-compiler.sh" "-sbcl-pic" "-sbcl-shared"
                          "alloca.c" "-o" "alloca.so")
                        :search t))
  (load-shared-object (truename "alloca.so"))
  (alien-funcall (extern-alien "alloca_test" (function void)))
  (sb-thread:join-thread
   (sb-thread:make-thread
    (lambda ()
      (alien-funcall (extern-alien "alloca_test" (function void)))))))

(with-test (:name :fp-mode-inheritance-threads)
  (labels ((fp-mode ()
             (let ((reserved-bits #+x86 (ash #b1110000011000000 16)
                                  #-x86 0))
               (logandc2 (dpb 0 sb-vm:float-sticky-bits (sb-vm:floating-point-modes))
                         reserved-bits)))
           (test ()
             (let* ((fp-mode (fp-mode))
                    (thread-fp-mode
                      (sb-thread:join-thread
                       (sb-thread:make-thread
                        (lambda ()
                          (fp-mode))))))
               (assert (= fp-mode thread-fp-mode)))))
    (test)
    (sb-int:with-float-traps-masked (:divide-by-zero)
      (test))
    (setf (sb-vm:floating-point-modes)
          (dpb sb-vm:float-divide-by-zero-trap-bit
               sb-vm:float-traps-byte
               (sb-vm:floating-point-modes)))
    (test)))
