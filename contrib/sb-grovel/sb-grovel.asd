;;; -*-  Lisp -*-

#-(or sb-testing-contrib sb-building-contrib)
(error "Can't build contribs with ASDF")

(defsystem "sb-grovel"
  :version "0.2"
  :depends-on ("asdf")
  #+sb-building-contrib :pathname
  #+sb-building-contrib #p"SYS:CONTRIB;SB-GROVEL;"
  :components ((:file "defpackage")
               (:file "def-to-lisp" :depends-on ("defpackage"))
               (:file "foreign-glue" :depends-on ("defpackage")))
  :perform (load-op :after (o c) (provide 'sb-grovel))
  :perform (test-op (o c) t))
