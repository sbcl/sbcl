;;; -*-  Lisp -*-

(error "Can't build contribs with ASDF")

(defsystem "sb-rotate-byte"
  :version "0.1"
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
     (:file "ppc64-vm" :if-feature :ppc64)
     (:file "loongarch64-vm" :if-feature :loongarch64)))
   (:file "rotate-byte" :depends-on ("vm"))))
