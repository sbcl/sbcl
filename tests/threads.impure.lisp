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

(in-package "SB-THREAD") ; this is white-box testing, really

(defun wait-for-threads (threads)
  (loop while (some #'sb-thread:thread-alive-p threads) do (sleep 0.01)))

(assert (eql 1 (length (list-all-threads))))

(assert (eq *current-thread*
            (find (thread-name *current-thread*) (list-all-threads)
                  :key #'thread-name :test #'equal)))

(assert (thread-alive-p *current-thread*))

(let ((a 0))
  (interrupt-thread *current-thread* (lambda () (setq a 1)))
  (assert (eql a 1)))

(let ((spinlock (make-spinlock)))
  (with-spinlock (spinlock)))

(let ((mutex (make-mutex)))
  (with-mutex (mutex)
    mutex))

#-sb-thread (sb-ext:quit :unix-status 104)

(let ((old-threads (list-all-threads))
      (thread (make-thread (lambda ()
                             (assert (find *current-thread* *all-threads*))
                             (sleep 2))))
      (new-threads (list-all-threads)))
  (assert (thread-alive-p thread))
  (assert (eq thread (first new-threads)))
  (assert (= (1+ (length old-threads)) (length new-threads)))
  (sleep 3)
  (assert (not (thread-alive-p thread))))

;;; We had appalling scaling properties for a while.  Make sure they
;;; don't reappear.
(defun scaling-test (function &optional (nthreads 5))
  "Execute FUNCTION with NTHREADS lurking to slow it down."
  (let ((queue (sb-thread:make-waitqueue))
        (mutex (sb-thread:make-mutex)))
    ;; Start NTHREADS idle threads.
    (dotimes (i nthreads)
      (sb-thread:make-thread (lambda ()
                               (with-mutex (mutex)
                                 (sb-thread:condition-wait queue mutex))
                               (sb-ext:quit))))
    (let ((start-time (get-internal-run-time)))
      (funcall function)
      (prog1 (- (get-internal-run-time) start-time)
        (sb-thread:condition-broadcast queue)))))
(defun fact (n)
  "A function that does work with the CPU."
  (if (zerop n) 1 (* n (fact (1- n)))))
(let ((work (lambda () (fact 15000))))
  (let ((zero (scaling-test work 0))
        (four (scaling-test work 4)))
    ;; a slightly weak assertion, but good enough for starters.
    (assert (< four (* 1.5 zero)))))

;;; For one of the interupt-thread tests, we want a foreign function
;;; that does not make syscalls

(with-open-file (o "threads-foreign.c" :direction :output :if-exists :supersede)
  (format o "void loop_forever() { while(1) ; }~%"))
(sb-ext:run-program
 "cc"
 (or #+linux '("-shared" "-o" "threads-foreign.so" "threads-foreign.c")
     (error "Missing shared library compilation options for this platform"))
 :search t)
(sb-alien:load-shared-object "threads-foreign.so")
(sb-alien:define-alien-routine loop-forever sb-alien:void)


;;; elementary "can we get a lock and release it again"
(let ((l (make-mutex :name "foo"))
      (p *current-thread*))
  (assert (eql (mutex-value l) nil) nil "1")
  (sb-thread:get-mutex l)
  (assert (eql (mutex-value l) p) nil "3")
  (sb-thread:release-mutex l)
  (assert (eql (mutex-value l) nil) nil "5"))

(labels ((ours-p (value)
           (sb-vm:control-stack-pointer-valid-p
            (sb-sys:int-sap (sb-kernel:get-lisp-obj-address value)))))
  (let ((l (make-mutex :name "rec")))
    (assert (eql (mutex-value l) nil) nil "1")
    (sb-thread:with-recursive-lock (l)
      (assert (ours-p (mutex-value l)) nil "3")
      (sb-thread:with-recursive-lock (l)
        (assert (ours-p (mutex-value l)) nil "4"))
      (assert (ours-p (mutex-value l)) nil "5"))
    (assert (eql (mutex-value l) nil) nil "6")))

(let ((l (make-spinlock :name "spinlock"))
      (p *current-thread*))
  (assert (eql (spinlock-value l) 0) nil "1")
  (with-spinlock (l)
    (assert (eql (spinlock-value l) p) nil "2"))
  (assert (eql (spinlock-value l) 0) nil "3"))

;; test that SLEEP actually sleeps for at least the given time, even
;; if interrupted by another thread exiting/a gc/anything
(let ((start-time (get-universal-time)))
  (make-thread (lambda () (sleep 1) (sb-ext:gc :full t)))
  (sleep 5)
  (assert (>= (get-universal-time) (+ 5 start-time))))


(let ((queue (make-waitqueue :name "queue"))
      (lock (make-mutex :name "lock"))
      (n 0))
  (labels ((in-new-thread ()
             (with-mutex (lock)
               (assert (eql (mutex-value lock) *current-thread*))
               (format t "~A got mutex~%" *current-thread*)
               ;; now drop it and sleep
               (condition-wait queue lock)
               ;; after waking we should have the lock again
               (assert (eql (mutex-value lock) *current-thread*))
               (assert (eql n 1))
               (decf n))))
    (make-thread #'in-new-thread)
    (sleep 2)                           ; give it  a chance to start
    ;; check the lock is free while it's asleep
    (format t "parent thread ~A~%" *current-thread*)
    (assert (eql (mutex-value lock) nil))
    (with-mutex (lock)
      (incf n)
      (condition-notify queue))
    (sleep 1)))

(let ((queue (make-waitqueue :name "queue"))
      (lock (make-mutex :name "lock")))
  (labels ((ours-p (value)
             (sb-vm:control-stack-pointer-valid-p
              (sb-sys:int-sap (sb-kernel:get-lisp-obj-address value))))
           (in-new-thread ()
             (with-recursive-lock (lock)
               (assert (ours-p (mutex-value lock)))
               (format t "~A got mutex~%" (mutex-value lock))
               ;; now drop it and sleep
               (condition-wait queue lock)
               ;; after waking we should have the lock again
               (format t "woken, ~A got mutex~%" (mutex-value lock))
               (assert (ours-p (mutex-value lock))))))
    (make-thread #'in-new-thread)
    (sleep 2)                           ; give it  a chance to start
    ;; check the lock is free while it's asleep
    (format t "parent thread ~A~%" *current-thread*)
    (assert (eql (mutex-value lock) nil))
    (with-recursive-lock (lock)
      (condition-notify queue))
    (sleep 1)))

(let ((mutex (make-mutex :name "contended")))
  (labels ((run ()
             (let ((me *current-thread*))
               (dotimes (i 100)
                 (with-mutex (mutex)
                   (sleep .1)
                   (assert (eql (mutex-value mutex) me)))
                 (assert (not (eql (mutex-value mutex) me))))
               (format t "done ~A~%" *current-thread*))))
    (let ((kid1 (make-thread #'run))
          (kid2 (make-thread #'run)))
      (format t "contention ~A ~A~%" kid1 kid2)
      (wait-for-threads (list kid1 kid2)))))

(defun test-interrupt (function-to-interrupt &optional quit-p)
  (let ((child  (make-thread function-to-interrupt)))
    ;;(format t "gdb ./src/runtime/sbcl ~A~%attach ~A~%" child child)
    (sleep 2)
    (format t "interrupting child ~A~%" child)
    (interrupt-thread child
                      (lambda ()
                        (format t "child pid ~A~%" *current-thread*)
                        (when quit-p (sb-ext:quit))))
    (sleep 1)
    child))

;; separate tests for (a) interrupting Lisp code, (b) C code, (c) a syscall,
;; (d) waiting on a lock, (e) some code which we hope is likely to be
;; in pseudo-atomic

(let ((child (test-interrupt (lambda () (loop)))))  (terminate-thread child))

(test-interrupt #'loop-forever :quit)

(let ((child (test-interrupt (lambda () (loop (sleep 2000))))))
  (terminate-thread child)
  (wait-for-threads (list child)))

(let ((lock (make-mutex :name "loctite"))
      child)
  (with-mutex (lock)
    (setf child (test-interrupt
                 (lambda ()
                   (with-mutex (lock)
                     (assert (eql (mutex-value lock) *current-thread*)))
                   (assert (not (eql (mutex-value lock) *current-thread*)))
                   (sleep 10))))
    ;;hold onto lock for long enough that child can't get it immediately
    (sleep 5)
    (interrupt-thread child (lambda () (format t "l ~A~%" (mutex-value lock))))
    (format t "parent releasing lock~%"))
  (terminate-thread child)
  (wait-for-threads (list child)))

(format t "~&locking test done~%")

(defun alloc-stuff () (copy-list '(1 2 3 4 5)))

(progn
  (let ((thread (sb-thread:make-thread (lambda () (loop (alloc-stuff))))))
    (let ((killers
           (loop repeat 4 collect
                 (sb-thread:make-thread
                  (lambda ()
                    (loop repeat 25 do
                          (sleep (random 0.1d0))
                          (princ ".")
                          (force-output)
                          (sb-thread:interrupt-thread thread (lambda ()))))))))
      (wait-for-threads killers)
      (sb-thread:terminate-thread thread)
      (wait-for-threads (list thread))))
  (sb-ext:gc :full t))

(format t "~&multi interrupt test done~%")

(let ((c (make-thread (lambda () (loop (alloc-stuff))))))
  ;; NB this only works on x86: other ports don't have a symbol for
  ;; pseudo-atomic atomicity
  (dotimes (i 100)
    (sleep (random 0.1d0))
    (interrupt-thread c
                      (lambda ()
                        (princ ".") (force-output)
                        (assert (eq (thread-state *current-thread*) :running))
                        (assert (zerop SB-KERNEL:*PSEUDO-ATOMIC-ATOMIC*)))))
  (terminate-thread c)
  (wait-for-threads (list c)))

(format t "~&interrupt test done~%")

(defparameter *interrupt-count* 0)

(declaim (notinline check-interrupt-count))
(defun check-interrupt-count (i)
  (declare (optimize (debug 1) (speed 1)))
  ;; This used to lose if eflags were not restored after an interrupt.
  (unless (typep i 'fixnum)
    (error "!!!!!!!!!!!")))

(let ((c (make-thread
          (lambda ()
            (handler-bind ((error #'(lambda (cond)
                                      (princ cond)
                                      (sb-debug:backtrace
                                       most-positive-fixnum))))
              (loop (check-interrupt-count *interrupt-count*)))))))
  (let ((func (lambda ()
                (princ ".")
                (force-output)
                (sb-impl::atomic-incf/symbol *interrupt-count*))))
    (setq *interrupt-count* 0)
    (dotimes (i 100)
      (sleep (random 0.1d0))
      (interrupt-thread c func))
    (loop until (= *interrupt-count* 100) do (sleep 0.1))
    (terminate-thread c)
    (wait-for-threads (list c))))

(format t "~&interrupt count test done~%")

(let (a-done b-done)
  (make-thread (lambda ()
                 (dotimes (i 100)
                   (sb-ext:gc) (princ "\\") (force-output))
                 (setf a-done t)))
  (make-thread (lambda ()
                 (dotimes (i 25)
                   (sb-ext:gc :full t)
                   (princ "/") (force-output))
                 (setf b-done t)))
  (loop
   (when (and a-done b-done) (return))
   (sleep 1)))

(terpri)

(defun waste (&optional (n 100000))
  (loop repeat n do (make-string 16384)))

(loop for i below 100 do
      (princ "!")
      (force-output)
      (sb-thread:make-thread
       #'(lambda ()
           (waste)))
      (waste)
      (sb-ext:gc))

(terpri)

(defparameter *aaa* nil)
(loop for i below 100 do
      (princ "!")
      (force-output)
      (sb-thread:make-thread
       #'(lambda ()
           (let ((*aaa* (waste)))
             (waste))))
      (let ((*aaa* (waste)))
        (waste))
      (sb-ext:gc))

(format t "~&gc test done~%")

;; this used to deadlock on session-lock
(sb-thread:make-thread (lambda () (sb-ext:gc)))
;; expose thread creation races by exiting quickly
(sb-thread:make-thread (lambda ()))

(defun exercise-syscall (fn reference-errno)
  (sb-thread:make-thread
   (lambda ()
     (loop do
          (funcall fn)
          (let ((errno (sb-unix::get-errno)))
            (sleep (random 0.1d0))
            (unless (eql errno reference-errno)
              (format t "Got errno: ~A (~A) instead of ~A~%"
                      errno
                      (sb-unix::strerror)
                      reference-errno)
              (force-output)
              (sb-ext:quit :unix-status 1)))))))

(let* ((nanosleep-errno (progn
                          (sb-unix:nanosleep -1 0)
                          (sb-unix::get-errno)))
       (open-errno (progn
                     (open "no-such-file"
                           :if-does-not-exist nil)
                     (sb-unix::get-errno)))
       (threads
        (list
         (exercise-syscall (lambda () (sb-unix:nanosleep -1 0)) nanosleep-errno)
         (exercise-syscall (lambda () (open "no-such-file"
                                            :if-does-not-exist nil))
                           open-errno)
         (sb-thread:make-thread (lambda () (loop (sb-ext:gc) (sleep 1)))))))
  (sleep 10)
  (princ "terminating threads")
  (dolist (thread threads)
    (sb-thread:terminate-thread thread)))

(format t "~&errno test done~%")

(loop repeat 100 do
      (let ((thread (sb-thread:make-thread (lambda () (sleep 0.1)))))
        (sb-thread:interrupt-thread
         thread
         (lambda ()
           (assert (find-restart 'sb-thread:terminate-thread))))))

(sb-ext:gc :full t)

(format t "~&thread startup sigmask test done~%")

(sb-debug::enable-debugger)
(let* ((main-thread *current-thread*)
       (interruptor-thread
        (make-thread (lambda ()
                       (sleep 2)
                       (interrupt-thread main-thread #'break)
                       (sleep 2)
                       (interrupt-thread main-thread #'continue)))))
  (with-session-lock (*session*)
    (sleep 3))
  (loop while (thread-alive-p interruptor-thread)))

(format t "~&session lock test done~%")

(sb-ext:gc :full t)
(loop repeat 20 do
      (wait-for-threads
       (loop for i below 100 collect
             (sb-thread:make-thread (lambda ()))))
      (sb-ext:gc :full t)
      (princ "+")
      (force-output))

(format t "~&creation test done~%")

;; watch out for *current-thread* being the parent thread after exit
(let ((thread (sb-thread:make-thread (lambda ()))))
  (wait-for-threads (list thread))
  (assert (null (symbol-value-in-thread 'sb-thread:*current-thread*
                                        thread))))

;; interrupt handlers are per-thread with pthreads, make sure the
;; handler installed in one thread is global
(sb-thread:make-thread
 (lambda ()
   (sb-ext:run-program "sleep" '("1") :search t :wait nil)))

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
