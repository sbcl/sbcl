;;; -*- lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-grovel))
(defpackage #:sb-simple-stream-system (:use #:asdf #:cl #:sb-grovel))
(in-package #:sb-simple-stream-system)


(defsystem sb-simple-streams
  :depends-on (sb-grovel sb-bsd-sockets)
  :components ((:file "package")
               (:file "fndb")
               (grovel-constants-file "constants"
                                      :package :sb-simple-streams
                                      :pathname "constants.lisp"
                                      :depends-on ("package"))
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
               ))

(defmethod perform :after ((o load-op)
                           (c (eql (find-system :sb-simple-streams))))
  (provide 'sb-simple-streams))

(defmethod perform ((o test-op) (c (eql (find-system :sb-simple-streams))))
  (operate 'load-op 'sb-simple-streams-tests)
  (operate 'test-op 'sb-simple-streams-tests))


(defsystem sb-simple-streams-tests
  :depends-on (sb-rt sb-simple-streams)
  :components ((:file "simple-stream-tests")))

(defmethod perform ((o test-op)
                    (c (eql (find-system :sb-simple-streams-tests))))
  (or (funcall (intern "DO-TESTS" (find-package "SB-RT")))
      (error "test-op failed")))


