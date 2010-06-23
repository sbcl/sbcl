;;; -*-  Lisp -*-

(cl:defpackage #:sb-rotate-byte-system
  (:use #:asdf #:cl))
(cl:in-package #:sb-rotate-byte-system)

(defsystem sb-rotate-byte
  :version "0.1"
  #+sb-building-contrib :pathname
  #+sb-building-contrib #p"SYS:CONTRIB;SB-ROTATE-BYTE;"
  :components
  ((:file "package")
   (:file "compiler" :depends-on ("package"))
   (:module "vm"
            :depends-on ("compiler")
            :components
            ((:file "x86-vm"
                    :in-order-to ((compile-op (feature :x86))))
             (:file "x86-64-vm"
                    :in-order-to ((compile-op (feature :x86-64))))
             (:file "ppc-vm"
                    :in-order-to ((compile-op (feature :ppc)))))
            :pathname
            #+sb-building-contrib #p"SYS:CONTRIB;SB-ROTATE-BYTE;"
            #-sb-building-contrib #.(make-pathname :directory '(:relative))
            :if-component-dep-fails :ignore)
   (:file "rotate-byte" :depends-on ("compiler"))))

(defmethod perform :after ((o load-op) (c (eql (find-system :sb-rotate-byte))))
  (provide 'sb-rotate-byte))

(defmethod perform ((o test-op) (c (eql (find-system :sb-rotate-byte))))
  (or (load (compile-file "rotate-byte-tests.lisp"))
      (error "test-op failed")))
