;;;; -*-  Lisp -*-

(defpackage :sb-fiber
  (:use :cl :sb-alien :sb-ext)
  (:export
   #:*current-fiber*
   #:fiber
   #:make-fiber
   #:make-main-fiber
   #:destroy-fiber
   #:fiber-name
   #:fiber-return-fiber
   #:switch-fiber
   #:yield-fiber
   #:join-fiber
   #:interrupt-fiber
   #:fiber-condition
   #:make-fiber-generator
   #:do-fiber-generator
   #:fiber-alive-p
   #:fiber-state
   #:with-fiber
   #:with-fiber-pinned
   #:fiber-pinned-p))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (sb-int:system-package-p (find-package "SB-FIBER")) t))
