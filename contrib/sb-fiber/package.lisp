;;;; -*-  Lisp -*-

(defpackage :sb-fiber
  (:use :cl :sb-alien :sb-ext)
  (:export
   #:*current-fiber*
   #:current-fiber
   #:*default-fiber-stack-size*
   #:*default-fiber-binding-stack-size*
   #:fiber
   #:fiberp
   #:make-fiber
   #:make-main-fiber
   #:with-fiber-thread
   #:release-fiber
   #:fiber-name
   #:fiber-thread
   #:fiber-return-fiber
   #:switch-fiber
   #:resume-fiber
   #:yield-fiber
   #:join-fiber
   #:interrupt-fiber
   #:with-interrupted-fiber
   #:fiber-condition
   #:fiber-alive-p
   #:fiber-main-p
   #:fiber-released-p
   #:fiber-state
   #:fiber-control-stack-size
   #:fiber-control-stack-usage
   #:fiber-binding-stack-size
   #:fiber-binding-stack-usage
   #:with-fiber
   #:with-fiber-pinned
   #:fiber-pinned-p
   #:fiber-pin-count
   #:fiber-migrate
   ;; Conditions
   #:fiber-error
   #:fiber-error-fiber
   #:dead-fiber-error
   #:pinned-fiber-error
   #:pinned-fiber-error-depth
   #:fiber-thread-mismatch-error
   #:fiber-thread-mismatch-error-role
   #:fiber-state-error
   #:fiber-state-error-state
   #:fiber-state-error-expected
   #:no-current-fiber-error
   #:no-current-fiber-error-operation))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (sb-int:system-package-p (find-package "SB-FIBER")) t))
