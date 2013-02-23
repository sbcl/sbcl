;;; -*-  Lisp -*-
(defsystem asdf-install
  :depends-on (sb-posix sb-bsd-sockets)
  #+sb-building-contrib :pathname
  #+sb-building-contrib #p"SYS:CONTRIB;ASDF-INSTALL;"
  :version "0.2"
  :components ((:file "defpackage")
               (:file "installer" :depends-on ("defpackage")))
  :perform (load-op :after (o c) (provide 'asdf-install))
  :perform (test-op (o c) t))
