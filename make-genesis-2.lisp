(load "src/cold/shared.lisp")
(in-package "SB-COLD")
(setf *host-obj-prefix* "obj/from-host/"
      *target-obj-prefix* "obj/from-xc/")
(load "src/cold/set-up-cold-packages.lisp")
(load "src/cold/defun-load-or-cload-xcompiler.lisp")
(load-or-cload-xcompiler #'host-load-stem)
(load "tools-for-build/corefile.lisp" :verbose nil)
(host-cload-stem "src/compiler/generic/genesis" nil)

(genesis :object-file-names (let (list)
                              (do-stems-and-flags (stem flags 2)
                                (unless (member :not-target flags)
                                  (push (stem-object-path stem flags :target-compile)
                                        list)))
                              (nreverse list))
         :defstruct-descriptions (stem-object-path
                                  "defstructs.lisp-expr" '(:extra-artifact) :target-compile)
         :tls-init (read-from-file
                    (stem-object-path "tls-init.lisp-expr"
                                      '(:extra-artifact) :target-compile))
         :c-header-dir-name "output/genesis-2"
         :symbol-table-file-name "src/runtime/sbcl.nm"
         :core-file-name "output/cold-sbcl.core"
         ;; The map file is not needed by the system, but can be
         ;; very handy when debugging cold init problems.
         :map-file-name "output/cold-sbcl.map")
#+cmu (ext:quit)
#+clisp (ext:quit)
#+abcl (ext:quit)
