;;; -*- lisp -*-

(defpackage #:sb-simple-stream-system (:use #:asdf #:cl))
(in-package #:sb-simple-stream-system)


(defsystem sb-simple-streams
  :depends-on (sb-bsd-sockets sb-posix)
  :components ((:file "package")
               (:file "fndb")
               ;;(:file "pcl")
               ;;(:file "ext-format" :depends-on ("package"))
               (:file "classes" :depends-on ("package"))
               (:file "internal" :depends-on ("classes"))
               (:file "strategy" :depends-on ("internal"))
               (:file "cl" :depends-on ("internal" "fndb"))
               (:file "simple-streams" :depends-on ("cl" "strategy"))
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


