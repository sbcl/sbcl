(in-package "SB!THREAD")

(sb!alien::define-alien-routine ("create_thread" %create-thread)
     sb!alien:unsigned-long (lisp-fun-address sb!alien:unsigned-long))

(defun make-thread (function)
  (%create-thread
   (sb!kernel:get-lisp-obj-address (coerce function 'function))))
(defun destroy-thread (thread-id)
  (sb!unix:unix-kill thread-id :sigterm))
(defun suspend-thread (thread-id)
  (sb!unix:unix-kill thread-id :sigstop))
(defun resume-thread (thread-id)
  (sb!unix:unix-kill thread-id :sigcont))

(defun current-thread-id ()
  (sb!sys:sap-int
   (sb!vm::current-thread-offset-sap sb!vm::thread-pid-slot)))

(defun make-listener-thread (tty-name)
  (assert (probe-file tty-name))
  (let* ((in (sb!unix:unix-open (namestring tty-name) sb!unix:o_rdwr #o666))
	 (out (sb!unix:unix-dup in))
	 (err (sb!unix:unix-dup in))
	 (sb!impl::*stdin* 
	  (sb!sys:make-fd-stream in :input t :buffering :line))
	 (sb!impl::*stdout* 
	  (sb!sys:make-fd-stream out :output t :buffering :line))
	 (sb!impl::*stderr* 
	  (sb!sys:make-fd-stream err :output t :buffering :line))
	 (sb!impl::*tty* 
	  (sb!sys:make-fd-stream err :input t :output t :buffering :line)))
    (labels ((thread-repl () 
	       (handling-end-of-the-world
		(with-simple-restart 
		    (destroy-thread
		     (format nil "~~@<Destroy this thread (~A)~~@:>"
			     (current-thread-id)))
		  (sb!impl::toplevel-repl nil)))))
      (make-thread #'thread-repl))))


;;;; mutex and read/write locks, originally inspired by CMUCL multi-proc.lisp

(defun sleep-a-bit (timeout)
  (if (> (get-internal-real-time) timeout)
      t
      (progn (sleep .1) nil)))

(defstruct mutex
  (name nil :type (or null simple-base-string))
  (value nil))

(defun get-mutex (lock &optional new-value  timeout)
  (declare (type mutex lock))
  (let ((timeout (and timeout (+ (get-internal-real-time) timeout))))
    (unless new-value (setf new-value (current-thread-id)))
    (loop
     (unless
	 ;; args are object slot-num old-value new-value
	 (sb!vm::%instance-set-conditional lock 2 nil new-value)
       (return t))
     (sleep-a-bit timeout))))

(defun free-mutex (lock &optional (new-value nil))
  (declare (type mutex lock))
  (let ((old-value (mutex-value lock))
	(t1 nil))
    (loop
     (unless
	 ;; args are object slot-num old-value new-value
	 (eql old-value
	      (setf t1
		    (sb!vm::%instance-set-conditional lock 2 nil new-value)))
       (return t))
     (setf old-value t1))))

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
