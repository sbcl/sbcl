;;; -*-  Lisp -*-

(defpackage #:asdf-install-system 
  (:use #:cl #:asdf))

(in-package #:asdf-install-system)
(require 'sb-executable)

(defsystem asdf-install
  :depends-on (sb-posix sb-bsd-sockets)
  :version "0.2"
  #+sb-building-contrib :pathname
  #+sb-building-contrib "SYS:CONTRIB;ASDF-INSTALL;"
  :components ((:file "defpackage")
	       (:file "installer" :depends-on ("defpackage"))))
	       
(defmethod perform :after ((o load-op) (c (eql (find-system :asdf-install))))
  (provide 'asdf-install))

(defmethod perform ((o test-op) (c (eql (find-system :asdf-install))))
  t)
