(error "Can't build contribs with ASDF")

(defsystem "sb-executable"
  :description "Concatenate FASLs into an executable file."
  :components ((:file "sb-executable")))
