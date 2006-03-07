;;; -*-  Lisp -*-
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-grovel))
(defpackage #:sb-bsd-sockets-system (:use #:asdf #:sb-grovel #:cl))
(in-package #:sb-bsd-sockets-system)

(defsystem sb-bsd-sockets
    :version "0.58"
    :depends-on (sb-grovel)
    #+sb-building-contrib :pathname
    #+sb-building-contrib "SYS:CONTRIB;SB-BSD-SOCKETS;"
    :components ((:file "defpackage")
		 (:file "split" :depends-on ("defpackage"))
		 (:file "malloc" :depends-on ("defpackage"))
		 (sb-grovel:grovel-constants-file
		  "constants"
		  :package :sockint
		  :depends-on  ("defpackage"))
		 (:file "sockets"
			:depends-on ("constants"))
		 
		 (:file "sockopt" :depends-on ("sockets"))
		 (:file "inet" :depends-on ("sockets" "split"  "constants" ))
		 (:file "local" :depends-on ("sockets" "split" "constants" ))
		 (:file "name-service" :depends-on ("sockets" "constants"))
		 (:file "misc" :depends-on ("sockets" "constants"))

		 (:static-file "NEWS")
		 ;; (:static-file "INSTALL")
		 ;; (:static-file "README")
		 ;; (:static-file "index" :pathname "index.html")
		 (:static-file "doc" :pathname "doc.lisp")
		 (:static-file "TODO")))

(defmethod perform :after ((o load-op) (c (eql (find-system :sb-bsd-sockets))))
  (provide 'sb-bsd-sockets))

(defmethod perform ((o test-op) (c (eql (find-system :sb-bsd-sockets))))
  (operate 'load-op 'sb-bsd-sockets-tests)
  (operate 'test-op 'sb-bsd-sockets-tests))

(defsystem sb-bsd-sockets-tests
  :depends-on (sb-rt sb-bsd-sockets sb-posix)
  :components ((:file "tests")))

(defmethod perform ((o test-op) (c (eql (find-system :sb-bsd-sockets-tests))))
  (or (funcall (intern "DO-TESTS" (find-package "SB-RT")))
      (error "test-op failed")))
