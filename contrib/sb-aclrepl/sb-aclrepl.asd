;;; -*-  Lisp -*-

(error "Can't build contribs with ASDF")

(defsystem "sb-aclrepl"
    :author "Kevin Rosenberg <kevin@rosenberg.net>"
    :description "An AllegroCL compatible REPL"
    :components ((:file "toplevel")
                 (:file "repl" :depends-on ("toplevel"))
                 (:file "inspect" :depends-on ("repl"))
                 (:file "debug" :depends-on ("repl"))))
