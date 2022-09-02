#-(or sb-testing-contrib sb-building-contrib)
(error "Can't build contribs with ASDF")

(defsystem "sb-sprof"
  :description "A statistical profiler."
  #+sb-building-contrib :pathname
  #+sb-building-contrib #p"SYS:CONTRIB;SB-SPROF;"
  :serial t
  :components ((:file "package")
               (:file "record")
               (:file "call-counting")
               (:file "graph")
               (:file "report")
               (:file "interface")
               (:file "disassemble"))
  :perform (load-op :after (o c) (provide 'sb-sprof)))
