(in-package "SB!THREAD")

;; opaque type
(sb!alien:define-alien-type thread (struct thread-struct))

(sb!alien::define-alien-routine ("create_thread" %create-thread)
     (* thread) (lisp-fun-address sb!alien:unsigned-long))

(defun make-thread (function)
  (%create-thread (sb!kernel:get-lisp-obj-address (coerce function 'function))))

#|
(defvar *foo* nil)
(defun thread-nnop () (loop (setf *foo* (not *foo*)) (sleep 1)))
(make-thread #'thread-nnop)
|#
