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

;;; success
(sb-ext:quit :unix-status 104)
