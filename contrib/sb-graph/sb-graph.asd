;;; -*-  Lisp -*-
(error "Can't build contribs with ASDF")

(defsystem "sb-graph"
  :serial t
  :components
  ((:module "src"
            :serial t
            :components
            ((:file "package")
             (:file "graphing")
             (:file "hooking")))))
