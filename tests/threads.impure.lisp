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

;;; For one of the interupt-thread tests, we want a foreign function
;;; that does not make syscalls

(setf SB-INT:*REPL-PROMPT-FUN* #'sb-thread::thread-repl-prompt-fun)
(with-open-file (o "threads-foreign.c" :direction :output)
  (format o "void loop_forever() { while(1) ; }~%"))
(sb-ext:run-program	
 "cc"
 (or #+linux '("-shared" "-o" "threads-foreign.so" "threads-foreign.c")
     (error "Missing shared library compilation options for this platform"))
 :search t)
(sb-alien:load-1-foreign "threads-foreign.so")
(sb-alien:define-alien-routine loop-forever sb-alien:void)


;;; elementary "can we get a lock and release it again"
(let ((l (make-mutex :name "foo"))
      (p (current-thread-id)))
  (assert (eql (mutex-value l) nil))
  (assert (eql (mutex-lock l) 0))
  (sb-thread:get-mutex l)
  (assert (eql (mutex-value l) p))
  (assert (eql (mutex-lock l) 0))
  (sb-thread:release-mutex l)
  (assert (eql (mutex-value l) nil))
  (assert (eql (mutex-lock l) 0)))

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
  (dotimes (i 100)
    (sleep (random 1d0))
    (interrupt-thread c
		      (lambda ()
			(princ ".") (force-output)
			(assert (zerop SB-KERNEL:*PSEUDO-ATOMIC-ATOMIC*)))))
  (terminate-thread c))

;; I'm not sure that this one is always successful.  Note race potential:
;; I haven't checked if decf is atomic here
(let ((done 2))
  (make-thread (lambda () (dotimes (i 100) (sb-ext:gc)) (decf done)))
  (make-thread (lambda () (dotimes (i 25) (sb-ext:gc :full t)) (decf done)))
  (loop
   (when (zerop done) (return))
   (sleep 1)))

;; give the other thread time to die before we leave, otherwise the
;; overall exit status is 0, not 104
(sleep 2) 

(sb-ext:quit :unix-status 104)
