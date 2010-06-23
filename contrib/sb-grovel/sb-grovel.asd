;;; -*-  Lisp -*-

(defpackage #:sb-grovel-system (:use #:asdf #:cl))
(in-package #:sb-grovel-system)

(defsystem sb-grovel
    :version "0.01"
    #+sb-building-contrib :pathname
    #+sb-building-contrib #p"SYS:CONTRIB;SB-GROVEL;"
    :components ((:file "defpackage")
                 (:file "def-to-lisp" :depends-on ("defpackage"))
                 (:file "foreign-glue" :depends-on ("defpackage"))))

(defmethod perform :after ((o load-op) (c (eql (find-system :sb-grovel))))
  (provide 'sb-grovel))

(defmethod perform ((o test-op) (c (eql (find-system :sb-grovel))))
  t)

