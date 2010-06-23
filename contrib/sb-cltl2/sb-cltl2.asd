;;; -*-  Lisp -*-

(defpackage #:sb-cltl2-system (:use #:asdf #:cl))
(in-package #:sb-cltl2-system)

(defsystem sb-cltl2
    :description "Functionality mentioned in CLtL2 but not present in ANSI."
    #+sb-building-contrib :pathname
    #+sb-building-contrib #p"SYS:CONTRIB;SB-CLTL2;"
    :components ((:file "defpackage")
                 (:file "compiler-let" :depends-on ("defpackage"))
                 (:file "macroexpand" :depends-on ("defpackage"))
                 (:file "env" :depends-on ("defpackage"))))

(defmethod perform :after ((o load-op) (c (eql (find-system :sb-cltl2))))
  (provide 'sb-cltl2))

(defmethod perform ((o test-op) (c (eql (find-system :sb-cltl2))))
  (oos 'load-op 'sb-cltl2-tests)
  (oos 'test-op 'sb-cltl2-tests))

(defsystem sb-cltl2-tests
    :depends-on (sb-rt)
    :components ((:file "tests")))

(defmethod perform ((o test-op) (c (eql (find-system :sb-cltl2-tests))))
  (or (funcall (find-symbol "DO-TESTS" "SB-RT"))
      (error "test-op failed")))
