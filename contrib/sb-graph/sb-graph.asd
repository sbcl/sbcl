;;; -*-  Lisp -*-
#-(or sb-testing-contrib sb-building-contrib)
(error "Can't build contribs with ASDF")

(asdf:defsystem "sb-graph"
  :serial t
  :components
  ((:module src
            :serial t
            :components
            ((:file "package")
             (:file "graphing")
             (:file "hooking"))))
  :perform (load-op :after (o c) (provide 'sb-graph)))

;;; The tests for sb-graph are under tests/sb-graph.impure.lisp to
;;; take advantage of the sbcl regression tester features.
