;;; -*-  Lisp -*-

(defpackage #:sb-aclrepl-system (:use #:asdf #:cl))
(in-package #:sb-aclrepl-system)

(defsystem sb-aclrepl
    :version "0.5"
    :components ((:file "repl")
		 (:file "inspect" :depends-on ("repl"))))

