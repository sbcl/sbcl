;;; -*-  Lisp -*-

(defpackage #:sb-aclrepl-system (:use #:asdf #:cl))
(in-package #:sb-aclrepl-system)

(defsystem sb-aclrepl
    :author "Kevin Rosenberg <kevin@rosenberg.net>"
    :description "An AllegroCL compatible REPL"
    #+sb-building-contrib :pathname
    #+sb-building-contrib #p"SYS:CONTRIB;SB-ACLREPL;"
    :components ((:file "toplevel")
                 (:file "repl" :depends-on ("toplevel"))
                 (:file "inspect" :depends-on ("repl"))
                 (:file "debug" :depends-on ("repl"))))

(defmethod perform :after ((o load-op) (c (eql (find-system :sb-aclrepl))))
  (provide 'sb-aclrepl))

(defmethod perform ((o test-op) (c (eql (find-system :sb-aclrepl))))
  (oos 'load-op 'sb-aclrepl-tests)
  (oos 'test-op 'sb-aclrepl-tests))

(defsystem sb-aclrepl-tests
    :depends-on (sb-rt)
    :components ((:file "tests")))

(defmethod perform ((o test-op) (c (eql (find-system :sb-aclrepl-tests))))
  (or (funcall (intern "DO-TESTS" (find-package "SB-RT")))
      (error "test-op failed")))
