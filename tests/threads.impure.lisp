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

#-sb-thread (quit :unix-status 104)

(in-package "SB-THREAD") ; this is white-box testing, really

;;; We had appalling scaling properties for a while.  Make sure they
;;; don't reappear.
(defun scaling-test (function &optional (nthreads 5))
  "Execute FUNCTION with NTHREADS lurking to slow it down."
  (let ((queue (sb-thread:make-waitqueue))
        (mutex (sb-thread:make-mutex)))
    ;; Start NTHREADS idle threads.
    (dotimes (i nthreads)
      (sb-thread:make-thread (lambda ()
                               (sb-thread:condition-wait queue mutex)
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
      (p (current-thread-id)))
  (assert (eql (mutex-value l) nil) nil "1")
  (assert (eql (mutex-lock l) 0) nil "2")
  (sb-thread:get-mutex l)
  (assert (eql (mutex-value l) p) nil "3")
  (assert (eql (mutex-lock l) 0) nil "4")
  (sb-thread:release-mutex l)
  (assert (eql (mutex-value l) nil) nil "5")
  (assert (eql (mutex-lock l) 0)  nil "6")
  (describe l))

;; test that SLEEP actually sleeps for at least the given time, even
;; if interrupted by another thread exiting/a gc/anything
(let ((start-time (get-universal-time)))
  (make-thread (lambda () (sleep 1))) ; kid waits 1 then dies ->SIG_THREAD_EXIT
  (sleep 5)
  (assert (>= (get-universal-time) (+ 5 start-time))))


(let ((queue (make-waitqueue :name "queue"))
      (lock (make-mutex :name "lock")))
  (labels ((in-new-thread ()
	     (with-mutex (lock)
	       (assert (eql (mutex-value lock) (current-thread-id)))
	       (format t "~A got mutex~%" (current-thread-id))
	       ;; now drop it and sleep
	       (condition-wait queue lock)
	       ;; after waking we should have the lock again
	       (assert (eql (mutex-value lock) (current-thread-id))))))
    (make-thread #'in-new-thread)
    (sleep 2)				; give it  a chance to start
    ;; check the lock is free while it's asleep
    (format t "parent thread ~A~%" (current-thread-id))
    (assert (eql (mutex-value lock) nil))    
    (assert (eql (mutex-lock lock) 0))
    (with-mutex (lock)
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
    (sleep 2)				; give it  a chance to start
    ;; check the lock is free while it's asleep
    (format t "parent thread ~A~%" (current-thread-id))
    (assert (eql (mutex-value lock) nil))    
    (assert (eql (mutex-lock lock) 0))
    (with-recursive-lock (lock)
      (condition-notify queue))
    (sleep 1)))

(let ((mutex (make-mutex :name "contended")))
  (labels ((run ()
	     (let ((me (current-thread-id)))
	       (dotimes (i 100)
		 (with-mutex (mutex)
		   (sleep .1)
		   (assert (eql (mutex-value mutex) me)))
		 (assert (not (eql (mutex-value mutex) me))))
	       (format t "done ~A~%" (current-thread-id)))))
    (let ((kid1 (make-thread #'run))
	  (kid2 (make-thread #'run)))
      (format t "contention ~A ~A~%" kid1 kid2))))

(defun test-interrupt (function-to-interrupt &optional quit-p)
  (let ((child  (make-thread function-to-interrupt)))
    ;;(format t "gdb ./src/runtime/sbcl ~A~%attach ~A~%" child child)
    (sleep 2)
    (format t "interrupting child ~A~%" child)
    (interrupt-thread child
		      (lambda ()
			(format t "child pid ~A~%" (current-thread-id))
			(when quit-p (sb-ext:quit))))
    (sleep 1)
    child))

;;; separate tests for (a) interrupting Lisp code, (b) C code, (c) a syscall,
;;; (d) waiting on a lock, (e) some code which we hope is likely to be
;;; in pseudo-atomic

(let ((child (test-interrupt (lambda () (loop)))))  (terminate-thread child))

(test-interrupt #'loop-forever :quit)

(let ((child (test-interrupt (lambda () (loop (sleep 2000))))))
  ;; Interrupting a sleep form causes it to return early.  Welcome to Unix.
  ;; Just to be sure our LOOP form works, let's check the child is still
  ;; there
  (assert (zerop (sb-unix:unix-kill child 0)))
  (terminate-thread child))
		
(let ((lock (make-mutex :name "loctite"))
      child)
  (with-mutex (lock)
    (setf child (test-interrupt
		 (lambda ()
		   (with-mutex (lock)
		     (assert (eql (mutex-value lock) (current-thread-id))))
		   (assert (not (eql (mutex-value lock) (current-thread-id))))
		   (sleep 60))))
    ;;hold onto lock for long enough that child can't get it immediately
    (sleep 20)
    (interrupt-thread child (lambda () (format t "l ~A~%" (mutex-value lock))))
    (format t "parent releasing lock~%"))
  (terminate-thread child))

(defun alloc-stuff () (copy-list '(1 2 3 4 5)))

(let ((c (test-interrupt (lambda () (loop (alloc-stuff))))))
  ;; NB this only works on x86: other ports don't have a symbol for
  ;; pseudo-atomic atomicity
  (format t "new thread ~A~%" c)
  (dotimes (i 100)
    (sleep (random 1d0))
    (interrupt-thread c
		      (lambda ()
			(princ ".") (force-output)
			(assert (zerop SB-KERNEL:*PSEUDO-ATOMIC-ATOMIC*)))))
  (terminate-thread c))

(format t "~&interrupt test done~%")

(let (a-done b-done)
  (make-thread (lambda ()
		 (dotimes (i 100) 
		   (sb-ext:gc) (princ "\\") (force-output) )
		 (setf a-done t)))
  (make-thread (lambda ()
		 (dotimes (i 25) 
		   (sb-ext:gc :full t)
		   (princ "/") (force-output))
		 (setf b-done t)))
  (loop
   (when (and a-done b-done) (return))
   (sleep 1)))
(format t "~&gc test done~%")

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

;; give the other thread time to die before we leave, otherwise the
;; overall exit status is 0, not 104
(sleep 2) 

(sb-ext:quit :unix-status 104)
