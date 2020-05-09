;;; -*- Lisp -*-

#-(or sb-testing-contrib sb-building-contrib)
(error "Can't build contribs with ASDF")

(defsystem "sb-rt"
  :version "0.1.7" ; our version "0", GCL CVS version "1.7"
  #+sb-building-contrib :pathname
  #+sb-building-contrib #p"SYS:CONTRIB;SB-RT;"
  :components ((:file "rt"))
  :perform (load-op :after (o c) (provide 'sb-rt))
  :perform (test-op (o c)
             ;; FIXME: Maybe also import rt-tests.lisp?
             t))
