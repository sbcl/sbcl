;;; -*-  Lisp -*-

#-(or sb-testing-contrib sb-building-contrib)
(error "Can't build contribs with ASDF")

(defsystem "sb-aclrepl"
    :author "Kevin Rosenberg <kevin@rosenberg.net>"
    :description "An AllegroCL compatible REPL"
    #+sb-building-contrib :pathname
    #+sb-building-contrib #p"SYS:CONTRIB;SB-ACLREPL;"
    :components ((:file "toplevel")
                 (:file "repl" :depends-on ("toplevel"))
                 (:file "inspect" :depends-on ("repl"))
                 (:file "debug" :depends-on ("repl")))
    :perform (load-op :after (o c) (provide 'sb-aclrepl)))
