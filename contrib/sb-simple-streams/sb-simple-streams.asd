;;; -*- lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-grovel))
(defpackage #:sb-simple-stream-system (:use #:asdf #:cl #:sb-grovel))
(in-package #:sb-simple-stream-system)


(defsystem sb-simple-streams
  :depends-on (sb-rt sb-grovel)
  :components ((:file "package")
               (:file "fndb")
               (grovel-constants-file "constants"
                                      :package :sb-simple-streams
                                      :pathname "constants.lisp"
                                      :depends-on ("package"))
               ;; (:file "stuff_grovelled_from_headers")
               (:file "unix" :depends-on ("constants"))
               ;;(:file "pcl")
               ;;(:file "ext-format" :depends-on ("package"))
               (:file "classes" :depends-on ("package"))
               (:file "internal" :depends-on ("classes"))
               (:file "strategy" :depends-on ("internal"))
               (:file "cl" :depends-on ("internal" "fndb"))
               (:file "simple-streams" :depends-on ("cl" "strategy" "unix"))
               ;;(:file "gray-compat" :depends-on ("package"))
               ;;(:file "iodefs" :depends-on ("package"))
               (:file "simple-stream-tests" :depends-on ("simple-streams"))
               ))

(defmethod perform ((o test-op) (c (eql (find-system :sb-simple-streams))))
  (or (funcall (intern "DO-TESTS" (find-package "SB-RT")))
      (error "test-op failed")))


