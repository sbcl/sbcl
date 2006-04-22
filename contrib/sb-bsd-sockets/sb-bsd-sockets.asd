;;; -*-  Lisp -*-
#-win32 (eval-when (:compile-toplevel :load-toplevel :execute)
          (require :sb-grovel))
(defpackage #:sb-bsd-sockets-system (:use #:asdf #-win32 #:sb-grovel #:cl))
(in-package #:sb-bsd-sockets-system)

(defsystem sb-bsd-sockets
    :version "0.58"
    :depends-on #-win32 (sb-grovel) #+win32 ()
    #+sb-building-contrib :pathname
    #+sb-building-contrib "SYS:CONTRIB;SB-BSD-SOCKETS;"
    :components ((:file "defpackage")
                 #+win32 (:file "win32-constants" :depends-on ("defpackage"))
                 #+win32 (:file "win32-sockets" :depends-on ("win32-constants"))
		 (:file "split" :depends-on ("defpackage"))
		 (:file "malloc" :depends-on ("defpackage"))
		 #-win32 (sb-grovel:grovel-constants-file
		  "constants"
		  :package :sockint
		  :depends-on  ("defpackage"))
		 (:file "sockets"
                        :depends-on #-win32 ("constants") 
                                    #+win32 ("win32-sockets"))
		 (:file "sockopt" :depends-on ("sockets"))
                 (:file "inet" :depends-on ("sockets" "split"))
                 (:file "local" :depends-on ("sockets" "split"))
                 (:file "name-service" :depends-on ("sockets" #-win32 "constants"))
                 (:file "misc" :depends-on ("sockets"))

		 (:static-file "NEWS")
		 ;; (:static-file "INSTALL")
		 ;; (:static-file "README")
		 ;; (:static-file "index" :pathname "index.html")
		 (:static-file "doc" :pathname "doc.lisp")
		 (:static-file "TODO")))

(defmethod perform :after ((o load-op) (c (eql (find-system :sb-bsd-sockets))))
  (provide 'sb-bsd-sockets))

#-win32
(defmethod perform ((o test-op) (c (eql (find-system :sb-bsd-sockets))))
  (operate 'load-op 'sb-bsd-sockets-tests)
  (operate 'test-op 'sb-bsd-sockets-tests))

#-win32
(defsystem sb-bsd-sockets-tests
  :depends-on (sb-rt sb-bsd-sockets #-win32 sb-posix)
  :components ((:file "tests")))

#-win32
(defmethod perform ((o test-op) (c (eql (find-system :sb-bsd-sockets-tests))))
  (or (funcall (intern "DO-TESTS" (find-package "SB-RT")))
      (error "test-op failed")))
