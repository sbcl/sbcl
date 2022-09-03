;;; -*-  Lisp -*-
#-(or sb-testing-contrib sb-building-contrib)
(error "Can't build contribs with ASDF")

(defsystem "sb-cltl2"
    :description "Functionality mentioned in CLtL2 but not present in ANSI."
    #+sb-building-contrib :pathname
    #+sb-building-contrib #p"SYS:CONTRIB;SB-CLTL2;"
    :components ((:file "defpackage")
                 (:file "compiler-let" :depends-on ("defpackage"))
                 (:file "env" :depends-on ("defpackage")))
    :perform (load-op :after (o c) (provide 'sb-cltl2)))
