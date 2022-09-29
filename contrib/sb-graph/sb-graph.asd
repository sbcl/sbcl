;;; -*-  Lisp -*-
#-(or sb-testing-contrib sb-building-contrib)
(error "Can't build contribs with ASDF")

(defsystem "sb-graph"
  :serial t
  :components
  ((:module "src"
            :serial t
            :components
            ((:file "package")
             (:file "graphing")
             (:file "hooking"))))
  :perform (load-op :after (o c) (provide 'sb-graph)))
