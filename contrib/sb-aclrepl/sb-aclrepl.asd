;;; -*-  Lisp -*-

(defpackage #:sb-aclrepl-system (:use #:asdf #:cl))
(in-package #:sb-aclrepl-system)

(require 'sb-rt)

(defsystem sb-aclrepl
    :version "0.6"
    :author "Kevin Rosenberg <kevin@rosenberg.net>"
    :description "An AllegroCL compatible REPL"
    :components ((:file "repl")
		 (:file "inspect" :depends-on ("repl"))
		 (:file "debug" :depends-on ("repl"))
		 (:file "tests" :depends-on ("debug" "inspect"))))

(defmethod perform ((o test-op) (c (eql (find-system :sb-aclrepl))))
  (or (funcall (intern "DO-TESTS" (find-package "SB-RT")))
      (error "test-op failed")))

