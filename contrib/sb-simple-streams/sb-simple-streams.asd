;;; -*- lisp -*-

(defpackage #:sb-simple-stream-system (:use #:asdf #:cl))
(in-package #:sb-simple-stream-system)


(defsystem sb-simple-streams
  :depends-on (sb-bsd-sockets sb-posix)
  #+sb-building-contrib :pathname
  #+sb-building-contrib #p"SYS:CONTRIB;SB-SIMPLE-STREAMS;"
  :components ((:file "package")
               (:file "fndb")
               (:file "iodefs" :depends-on ("package"))
               ;;(:file "pcl")
               ;;(:file "ext-format" :depends-on ("package"))
               (:file "classes" :depends-on ("iodefs"))
               (:file "internal" :depends-on ("classes"))
               (:file "strategy" :depends-on ("internal"))
               (:file "impl" :depends-on ("internal" "fndb" "file" "string"))
               (:file "file" :depends-on ("strategy"))
               (:file "direct" :depends-on ("strategy"))
               (:file "null" :depends-on ("strategy"))
               (:file "socket" :depends-on ("strategy"))
               (:file "string" :depends-on ("strategy"))
               (:file "terminal" :depends-on ("strategy"))
               ;;(:file "gray-compat" :depends-on ("package"))
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
