;;; -*-  Lisp -*-

(defpackage #:sb-aclrepl-system (:use #:asdf #:cl))
(in-package #:sb-aclrepl-system)

;; Work-around for optimization note from EQL specializer
(declaim (optimize (sb-ext:inhibit-warnings 3)))

(defsystem sb-aclrepl
    :author "Kevin Rosenberg <kevin@rosenberg.net>"
    :description "An AllegroCL compatible REPL"
    :components ((:file "toplevel")
		 (:file "repl" :depends-on ("toplevel"))
		 (:file "inspect" :depends-on ("repl"))
		 (:file "debug" :depends-on ("repl"))))

(defmethod perform ((o test-op) (c (eql (find-system :sb-aclrepl))))
  (oos 'load-op 'sb-aclrepl-tests)
  (oos 'test-op 'sb-aclrepl-tests))

(defsystem sb-aclrepl-tests
    :depends-on (sb-rt)
    :components ((:file "tests")))

(defmethod perform ((o test-op) (c (eql (find-system :sb-aclrepl-tests))))
  (or (funcall (intern "DO-TESTS" (find-package "SB-RT")))
      (error "test-op failed")))

(declaim (optimize (sb-ext:inhibit-warnings 0)))
