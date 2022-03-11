#-(or sb-testing-contrib sb-building-contrib)
(error "Can't build contribs with ASDF")

(defsystem "sb-executable"
  :description "Concatenate FASLs into an executable file."
  #+sb-building-contrib :pathname
  #+sb-building-contrib #p"SYS:CONTRIB;SB-EXECUTABLE;"
  :components ((:file "sb-executable"))
  :perform (load-op :after (o c) (provide 'sb-executable))
  :perform (test-op (o c) t))
