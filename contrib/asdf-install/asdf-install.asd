;;; -*-  Lisp -*-

(defpackage #:asdf-install-system
  (:use #:cl #:asdf))

(in-package #:asdf-install-system)

(defsystem asdf-install
  :depends-on (sb-posix sb-bsd-sockets)
  :version "0.2"
  #+sb-building-contrib :pathname
  #+sb-building-contrib #p"SYS:CONTRIB;ASDF-INSTALL;"
  :components ((:file "defpackage")
               (:file "installer" :depends-on ("defpackage"))))

(defmethod perform :after ((o load-op) (c (eql (find-system :asdf-install))))
  (provide 'asdf-install))

(defmethod perform ((o test-op) (c (eql (find-system :asdf-install))))
  t)
