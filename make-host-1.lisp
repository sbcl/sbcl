;;; (We want to have some limit on print length and print level during
;;; bootstrapping because PRINT-OBJECT only gets set up rather late,
;;; and running without PRINT-OBJECT it's easy to fall into printing
;;; enormous (or infinitely circular) low-level representations of
;;; things.)
(setf *print-level* 5 *print-length* 5)

(progn (load "src/cold/shared.lisp")
       (load "tools-for-build/ldso-stubs.lisp"))
(in-package "SB-COLD")
(setf *host-obj-prefix* "obj/from-host/")
(progn (load "src/cold/set-up-cold-packages.lisp")
       (load "src/cold/defun-load-or-cload-xcompiler.lisp"))

(progn (set-dispatch-macro-character #\# #\+ #'she-reader)
       (set-dispatch-macro-character #\# #\- #'she-reader))

;; Supress function/macro redefinition warnings under clisp.
#+clisp
(progn (setf custom:*suppress-check-redefinition* t)
       ;; A compilation-unit seems to kill the compile. I'm not sure if it's
       ;; running out of memory or what. I don't care to find out,
       ;; but it's most definitely the cause of the breakage.
       (defmacro maybe-with-compilation-unit (&body forms)
         `(progn ,@forms)))
#-clisp
(defmacro maybe-with-compilation-unit (&body forms)
  `(with-compilation-unit () ,@forms))

(maybe-with-compilation-unit
 (load-or-cload-xcompiler #'host-cload-stem)

;;; Let's check that the type system, and various other things, are
;;; reasonably sane. (It's easy to spend a long time wandering around
;;; confused trying to debug cross-compilation if it isn't.)
 (when (find :sb-test *shebang-features*)
   (load "tests/type.before-xc.lisp")
   (load "tests/info.before-xc.lisp")
   (load "tests/vm.before-xc.lisp"))
;; When building on a slow host using a slow Lisp,
;; the wait time in slurp-ucd seems interminable - over a minute.
;; Compiling seems to help a bit, but maybe it's my imagination.
 (load (compile-file "tools-for-build/ucd.lisp"))

;;; Generate character database tables.
 (dolist (s '(sb-cold::slurp-ucd sb-cold::slurp-proplist sb-cold::output))
   (funcall s))

;;; propagate structure offset and other information to the C runtime
;;; support code.
 (host-cload-stem "src/compiler/generic/genesis" nil)
) ; END with-compilation-unit

(sb!vm:genesis :c-header-dir-name "src/runtime/genesis")
#+cmu (ext:quit)
#+clisp (ext:quit)
#+abcl (ext:quit)
