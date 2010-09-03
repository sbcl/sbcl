;;; (We want to have some limit on print length and print level during
;;; bootstrapping because PRINT-OBJECT only gets set up rather late,
;;; and running without PRINT-OBJECT it's easy to fall into printing
;;; enormous (or infinitely circular) low-level representations of
;;; things.)
(setf *print-level* 5 *print-length* 5)

(load "src/cold/shared.lisp")
(load "tools-for-build/ldso-stubs.lisp")
(in-package "SB-COLD")
(setf *host-obj-prefix* "obj/from-host/")
(load "src/cold/set-up-cold-packages.lisp")
(load "src/cold/defun-load-or-cload-xcompiler.lisp")

(set-dispatch-macro-character #\# #\+ #'she-reader)
(set-dispatch-macro-character #\# #\- #'she-reader)

;; Supress function/macro redefinition warnings under clisp.
#+clisp (setf custom:*suppress-check-redefinition* t)

(load-or-cload-xcompiler #'host-cload-stem)

;;; Let's check that the type system, and various other things, are
;;; reasonably sane. (It's easy to spend a long time wandering around
;;; confused trying to debug cross-compilation if it isn't.)
(when (find :sb-test *shebang-features*)
  (load "tests/type.before-xc.lisp")
  (load "tests/info.before-xc.lisp")
  (load "tests/vm.before-xc.lisp"))
(load "tools-for-build/ucd.lisp")

;;; Generate character database tables.
(sb-cold::slurp-ucd)
(sb-cold::output)

;;; propagate structure offset and other information to the C runtime
;;; support code.
(host-cload-stem "src/compiler/generic/genesis" nil)
(sb!vm:genesis :c-header-dir-name "src/runtime/genesis")
#+cmu (ext:quit)
#+clisp (ext:quit)
#+abcl (ext:quit)
