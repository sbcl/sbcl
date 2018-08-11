;; If you change this, note that 'slam.lisp' has to be able to parse this form
(mapc (lambda (x)
        (load (merge-pathnames (stem-remap-target x)
                               (make-pathname :type "lisp"))
              :verbose nil :print nil))
      '("src/assembly/target/tramps"
        "src/assembly/target/assem-rtns"
        "src/assembly/target/array"
        "src/assembly/target/arith"
        "src/assembly/target/alloc"))
