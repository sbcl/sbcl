;;; -*-  Lisp -*-

(defpackage #:sb-aclrepl-system (:use #:asdf #:cl))
(in-package #:sb-aclrepl-system)

(defsystem sb-aclrepl
    :version "0.6"
    :author "Kevin Rosenberg <kevin@rosenberg.net>"
    :description "An AllegroCL compatible REPL"
    :components ((:file "repl")
		 (:file "inspect" :depends-on ("repl"))
		 (:file "debug" :depends-on ("repl"))))

(defmethod perform ((o test-op) (c (eql (find-system :sb-aclrepl))))
  (or (load "aclrepl-tests.lisp")
      (error "test-op failed")))

