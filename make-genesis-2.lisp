(load "src/cold/shared.lisp")
(in-package "SB-COLD")
(setf *host-obj-prefix* "obj/from-host/"
      *target-obj-prefix* "obj/from-xc/")
(load "src/cold/set-up-cold-packages.lisp")
(load "src/cold/defun-load-or-cload-xcompiler.lisp")
(load-or-cload-xcompiler #'host-load-stem)
(load "tools-for-build/corefile.lisp" :verbose nil)
(host-cload-stem "src/compiler/generic/genesis" nil)

(let (object-file-names foptrace-file-names)
  (do-stems-and-flags (stem flags 2)
    (unless (member :not-target flags)
      (push (stem-object-path stem flags :target-compile)
            object-file-names)
      (when (member :foptrace-file flags)
        (push (stem-object-path stem flags :target-compile)
              foptrace-file-names))))
  (setf object-file-names (nreverse object-file-names))
  (genesis :object-file-names object-file-names
           :foptrace-file-names foptrace-file-names
           :defstruct-descriptions (find-bootstrap-file "output/defstructs.lisp-expr" t)
           :tls-init (read-from-file "output/tls-init.lisp-expr" :build-dependent t)
           :c-header-dir-name "output/genesis-2"
           :core-file-name "output/cold-sbcl.core"
           ;; The map file is not needed by the system, but can be
           ;; very handy when debugging cold init problems.
           :map-file-name "output/cold-sbcl.map"))

#+cmu (ext:quit)
#+clisp (ext:quit)
#+abcl (ext:quit)
