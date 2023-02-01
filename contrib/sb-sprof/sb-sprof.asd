(error "Can't build contribs with ASDF")

(defsystem "sb-sprof"
  :description "A statistical profiler."
  :serial t
  :components ((:file "package")
               (:file "record")
               (:file "call-counting")
               (:file "graph")
               (:file "report")
               (:file "interface")
               (:file "disassemble")))
