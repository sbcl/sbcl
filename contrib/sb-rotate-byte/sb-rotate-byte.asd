;;; -*-  Lisp -*-

#-(or sb-testing-contrib sb-building-contrib)
(error "Can't build contribs with ASDF")

(defsystem "sb-rotate-byte"
  :version "0.1"
  #+sb-building-contrib :pathname
  #+sb-building-contrib #p"SYS:CONTRIB;SB-ROTATE-BYTE;"
  :components
  ((:file "package")
   (:file "compiler" :depends-on ("package"))
   (:module "vm"
    :depends-on ("compiler")
    :pathname ""
    :components
    ((:file "arm-vm" :if-feature :arm)
     (:file "arm64-vm" :if-feature :arm64)
     (:file "x86-vm" :if-feature :x86)
     (:file "x86-64-vm" :if-feature :x86-64)
     (:file "riscv-vm" :if-feature :riscv)
     (:file "ppc-vm" :if-feature :ppc)
     (:file "ppc64-vm" :if-feature :ppc64)))
   (:file "rotate-byte" :depends-on ("vm")))
  :perform (load-op :after (o c) (provide 'sb-rotate-byte))
  :in-order-to ((test-op (test-op "sb-rotate-byte/tests"))))

(defsystem "sb-rotate-byte/tests"
  #+sb-building-contrib :pathname
  #+sb-building-contrib #p"SYS:CONTRIB;SB-ROTATE-BYTE;"
  :depends-on ("sb-rotate-byte")
  :components ((:file "rotate-byte-tests")))
