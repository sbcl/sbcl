(in-package "SB!THREAD")

(sb!alien::define-alien-routine ("create_thread" %create-thread)
     sb!alien:unsigned-long
  (lisp-fun-address sb!alien:unsigned-long))

(defun make-thread (function)
  (%create-thread
   (sb!kernel:get-lisp-obj-address (coerce function 'function))))

(defun destroy-thread (thread-id)
  (sb!unix:unix-kill thread-id :sigterm)
  ;; may have been stopped on tty io, so now wake it up to deliver the TERM
  (sb!unix:unix-kill thread-id :sigcont))

(defun suspend-thread (thread-id)
  (sb!unix:unix-kill thread-id :sigstop))

(defun resume-thread (thread-id)
  (sb!unix:unix-kill thread-id :sigcont))

(defun current-thread-id ()
  (sb!sys:sap-int
   (sb!vm::current-thread-offset-sap sb!vm::thread-pid-slot)))

;;;; iterate over the in-memory threads

(defun mapcar-threads (function)
  "Call FUNCTION once for each known thread, giving it the thread structure as argument"
  (let ((function (coerce function 'function)))
    (loop for thread = (alien-sap (extern-alien "all_threads" (* t)))
	  then  (sb!sys:sap-ref-sap thread (* 4 sb!vm::thread-next-slot))
	  until (sb!sys:sap= thread (sb!sys:int-sap 0))
	  collect (funcall function thread))))

#+nil
(defun thread-stopped-p (thread)
  (sb!kernel:make-lisp-obj
   (sb!sys:sap-ref-32 thread
		      (* 4 sb!vm::thread-stopped-p-slot))))
 ; (declare (optimize (speed 3) (safety 0)))
;  (thread-stopped-p thread))


(defvar *foreground-thread-stack* nil)

#+nil
(defun background-thread (&optional restart)
  (if restart
    (let* ((real-restart (sb!kernel::find-restart-or-lose restart))
	   (args (sb!kernel::interactive-restart-arguments real-restart)))
      (thread-to-tty (pop *foreground-thread-stack*))
      (apply (sb!kernel::restart-function real-restart) args))
    (thread-to-tty (pop *foreground-thread-stack*))))

#+nil
(defun make-listener-thread (tty-name)
  (assert (probe-file tty-name))
  (let* ((in (sb!unix:unix-open (namestring tty-name) sb!unix:o_rdwr #o666))
	 (out (sb!unix:unix-dup in))
	 (err (sb!unix:unix-dup in)))
    (labels ((thread-repl () 
	       ;;; XXX also need to set up new *foreground-thread-stack*
	       (let* ((sb!impl::*stdin* 
		       (sb!sys:make-fd-stream in :input t :buffering :line))
		      (sb!impl::*stdout* 
		       (sb!sys:make-fd-stream out :output t :buffering :line))
		      (sb!impl::*stderr* 
		       (sb!sys:make-fd-stream err :output t :buffering :line))
		      (sb!impl::*tty* 
		       (sb!sys:make-fd-stream err :input t :output t :buffering :line))
		      (sb!impl::*descriptor-handlers* nil))
		 (sb!impl::handling-end-of-the-world
		  (with-simple-restart 
		      (destroy-thread
		       (format nil "~~@<Destroy this thread (~A)~~@:>"
			       (current-thread-id)))
		    (sb!impl::toplevel-repl nil))))))
      (make-thread #'thread-repl))))

;;;; mutex and read/write locks, originally inspired by CMUCL multi-proc.lisp

;;; in true OOAOM style, this is also defined in C.  Don't change this
;;; defn without referring also to add_thread_to_queue
(defstruct mutex
  (name nil :type (or null simple-base-string))
  (value nil)
  (queuelock 0)
  (queue nil))

;;; add_thread_to_queue needs to do lots of sigmask manipulation 
;;; which we don't have the right alien gubbins to do in lisp
(sb!alien:define-alien-routine
    ("add_thread_to_queue" add-thread-to-queue) void
  (pid int) (mutex system-area-pointer))

;; spinlocks use 0 as "free" value: higher-level locks use NIL
(defun get-spinlock (lock offset new-value)
  (declare (optimize (speed 3) (safety 0)))
  (loop until
	(eql (sb!vm::%instance-set-conditional lock offset 0 new-value) 0)))

(defun get-mutex (lock &optional new-value  timeout)
  (declare (type mutex lock))
  (let ((timeout (and timeout (+ (get-internal-real-time) timeout)))
	(pid (current-thread-id)))
    (unless new-value (setf new-value pid))
    (loop
     (unless
	 ;; args are object slot-num old-value new-value
	 (sb!vm::%instance-set-conditional lock 2 nil new-value)
       ;; success, remove us from the wait queue 
       (get-spinlock lock 3 pid)
       (when (eql (car (mutex-queue lock)) pid)
	 ;; ... if we were there
	 (setf (mutex-queue lock) (cdr (mutex-queue lock))))
       (setf (mutex-queuelock lock) 0)
       (return t))
     (add-thread-to-queue
      pid (sb!sys:int-sap (sb!kernel:get-lisp-obj-address lock))))))

(defun free-mutex (lock &optional (new-value nil))
  (declare (type mutex lock))
  (let ((old-value (mutex-value lock))
	(pid (current-thread-id))
	(t1 nil))
    (loop
     (unless
	 ;; args are object slot-num old-value new-value
	 (eql old-value
	      (setf t1
		    (sb!vm::%instance-set-conditional lock 2 old-value new-value)))       
       (get-spinlock lock 3 pid)
       (when (mutex-queue lock)
	 (sb!unix:unix-kill (car (mutex-queue lock)) :sigalrm))
       (setf (mutex-queuelock lock) 0)
       (return t))
     (setf old-value t1))))

;;;; job control

(defvar *background-threads-wait-for-debugger* t)
;;; may be T, NIL, or a function called with sb-sys::*stdin* and thread id 
;;; as its two arguments, returns NIL or T

;;; called from top of invoke-debugger

(defun maybe-wait-until-foreground-thread ()
  (when (not (eql (mutex-value
		   (sb!impl::fd-stream-owner-thread sb!sys::*stdin*))
		  (CURRENT-THREAD-ID)))
    (let* ((wait-p *background-threads-wait-for-debugger*)
	   (*background-threads-wait-for-debugger* nil))
      (when (functionp wait-p) 
	(setf wait-p 
	      (funcall wait-p sb!sys::*stdin* (CURRENT-THREAD-ID))))
      (cond (wait-p
	     (get-mutex (sb!impl::fd-stream-owner-thread sb!sys::*stdin*))
	     #+nil
	     (sb!sys:enable-interrupt :sigint *sigint-handler*))
	    (t
	     (invoke-restart (car (compute-restarts))))))))


(defstruct rwlock
  (name nil :type (or null simple-base-string))
  (value 0 :type fixnum)
  (max-readers nil :type (or fixnum null))
  (max-writers 1 :type fixnum))

(macrolet
    ((make-rwlocking-function (lock-fn unlock-fn increment limit test)
       (let ((do-update '(when (eql old-value
				(sb!vm::%instance-set-conditional
				 lock 2 old-value new-value))
			  (return (values t old-value))))
	     (vars `((timeout (and timeout (+ (get-internal-real-time) timeout)))
		     old-value
		     new-value
		     (limit ,limit))))
	 (labels ((do-setfs (v) `(setf old-value (rwlock-value lock)
				  new-value (,v old-value ,increment))))
	   `(progn
	     (defun ,lock-fn (lock timeout)
	       (declare (type rwlock lock))
	       (let ,vars
		 (loop
		  ,(do-setfs '+)
		  (when ,test
		    ,do-update)
		  (when (sleep-a-bit timeout) (return nil)) ;expired
		  )))
	     ;; unlock doesn't need timeout or test-in-range
	     (defun ,unlock-fn (lock)
	       (declare (type rwlock lock))
	       (declare (ignorable limit))
	       (let ,(cdr vars)
		 (loop
		  ,(do-setfs '-)
		  ,do-update))))))))
    
  (make-rwlocking-function %lock-for-reading %unlock-for-reading 1
			   (rwlock-max-readers lock)
			   (and (>= old-value 0)
				(or (null limit) (<= new-value limit))))
  (make-rwlocking-function %lock-for-writing %unlock-for-writing -1
			   (- (rwlock-max-writers lock))
			   (and (<= old-value 0)
				(>= new-value limit))))
  
(defun get-rwlock (lock direction &optional timeout)
  (ecase direction
    (:read (%lock-for-reading lock timeout))
    (:write (%lock-for-writing lock timeout))))

(defun free-rwlock (lock direction)
  (ecase direction
    (:read (%unlock-for-reading lock))
    (:write (%unlock-for-writing lock))))

;;; Lock-Wait-With-Timeout  --  Internal
;;;
;;; Wait with a timeout for the lock to be free and acquire it for the
;;; *current-process*.
;;;
#+nil
(defun lock-wait-with-timeout (lock whostate timeout)
  (declare (type lock lock))
  (process-wait-with-timeout
   whostate timeout
   #'(lambda ()
       (declare (optimize (speed 3)))
       #-i486
       (unless (lock-process lock)
	 (setf (lock-process lock) *current-process*))
       #+i486
       (null (kernel:%instance-set-conditional
	      lock 2 nil *current-process*)))))

;;; With-Lock-Held  --  Public
;;;
#+nil
(defmacro with-lock-held ((lock &optional (whostate "Lock Wait")
				&key (wait t) timeout)
			  &body body)
  "Execute the body with the lock held. If the lock is held by another
  process then the current process waits until the lock is released or
  an optional timeout is reached. The optional wait timeout is a time in
  seconds acceptable to process-wait-with-timeout.  The results of the
  body are return upon success and NIL is return if the timeout is
  reached. When the wait key is NIL and the lock is held by another
  process then NIL is return immediately without processing the body."
  (let ((have-lock (gensym)))
    `(let ((,have-lock (eq (lock-process ,lock) *current-process*)))
      (unwind-protect
	   ,(cond ((and timeout wait)
		   `(progn
		      (when (and (error-check-lock-p ,lock) ,have-lock)
			(error "Dead lock"))
		      (when (or ,have-lock
				 #+i486 (null (kernel:%instance-set-conditional
					       ,lock 2 nil *current-process*))
				 #-i486 (seize-lock ,lock)
				 (if ,timeout
				     (lock-wait-with-timeout
				      ,lock ,whostate ,timeout)
				     (lock-wait ,lock ,whostate)))
			,@body)))
		  (wait
		   `(progn
		      (when (and (error-check-lock-p ,lock) ,have-lock)
		        (error "Dead lock"))
		      (unless (or ,have-lock
				 #+i486 (null (kernel:%instance-set-conditional
					       ,lock 2 nil *current-process*))
				 #-i486 (seize-lock ,lock))
			(lock-wait ,lock ,whostate))
		      ,@body))
		  (t
		   `(when (or (and (recursive-lock-p ,lock) ,have-lock)
			      #+i486 (null (kernel:%instance-set-conditional
					    ,lock 2 nil *current-process*))
			      #-i486 (seize-lock ,lock))
		      ,@body)))
	(unless ,have-lock
	  #+i486 (kernel:%instance-set-conditional
		  ,lock 2 *current-process* nil)
	  #-i486 (when (eq (lock-process ,lock) *current-process*)
		   (setf (lock-process ,lock) nil)))))))

