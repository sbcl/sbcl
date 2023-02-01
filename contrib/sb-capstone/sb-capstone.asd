(error "Can't build contribs with ASDF")

(defsystem "sb-capstone"
  :name "SB-CAPSTONE"
  :version "0.1"
  :description "Multi-target disassembly for SBCL using Capstone library"
  :serial t
  :components ((:file "capstone")))
