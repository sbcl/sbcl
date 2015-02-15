(setf *print-level* 5 *print-length* 5)
(load "src/cold/shared.lisp")
(in-package "SB-COLD")
(setf *host-obj-prefix* "obj/from-host/"
      *target-obj-prefix* "obj/from-xc/")
(load "src/cold/set-up-cold-packages.lisp")
(load "src/cold/defun-load-or-cload-xcompiler.lisp")
(load-or-cload-xcompiler #'host-load-stem)
(defparameter *target-object-file-names*
  (with-open-file (s "output/object-filenames-for-genesis.lisp-expr"
                     :direction :input)
    (read s)))
(host-cload-stem "src/compiler/generic/genesis" nil)
(sb!vm:genesis :object-file-names *target-object-file-names*
               :c-header-dir-name "output/genesis-2"
               :symbol-table-file-name "src/runtime/sbcl.nm"
               :core-file-name "output/cold-sbcl.core"
               ;; The map file is not needed by the system, but can be
               ;; very handy when debugging cold init problems.
               :map-file-name "output/cold-sbcl.map")
#+cmu (ext:quit)
#+clisp (ext:quit)
#+abcl (ext:quit)
