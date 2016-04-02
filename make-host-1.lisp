;;; (We want to have some limit on print length and print level during
;;; bootstrapping because PRINT-OBJECT only gets set up rather late,
;;; and running without PRINT-OBJECT it's easy to fall into printing
;;; enormous (or infinitely circular) low-level representations of
;;; things.)
(setf *print-level* 5 *print-length* 5)

(progn (load "src/cold/shared.lisp")
       (load "tools-for-build/ldso-stubs.lisp"))
(in-package "SB-COLD")
(progn
  (setf *host-obj-prefix* "obj/from-host/")
  (load "src/cold/set-up-cold-packages.lisp")
  (load "src/cold/defun-load-or-cload-xcompiler.lisp")

  ;; Supress function/macro redefinition warnings under clisp.
  #+clisp (setf custom:*suppress-check-redefinition* t)

  (defmacro maybe-with-compilation-unit (&body forms)
    ;; A compilation-unit seems to kill the compile. I'm not sure if it's
    ;; running out of memory or what. I don't care to find out,
    ;; but it's most definitely the cause of the breakage.
    #+clisp `(progn ,@forms)

    #+sbcl
    ;; Watch for deferred warnings under SBCL.
    ;; UNDEFINED-VARIABLE does not cause COMPILE-FILE to return warnings-p
    ;; unless outside a compilation unit. You find out about it only upon
    ;; exit of SUMMARIZE-COMPILATION-UNIT. So we set up a handler for that.
    `(let (in-summary fail)
       (handler-bind (((and simple-warning (not style-warning))
                       (lambda (c)
                         ;; hack for PPC. See 'build-order.lisp-expr'
                         ;; Ignore the warning, and the warning about the warning.
                         (unless (or (search "not allowed by the operand type"
                                             (simple-condition-format-control c))
                                     (search "ignoring FAILURE-P return"
                                             (simple-condition-format-control c)))
                           (setq fail 'warning))))
                      ;; Prevent regressions on a couple platforms
                      ;; that are known to build cleanly.
                      #!+(or x86 x86-64)
                      (sb-int:simple-style-warning
                       (lambda (c)
                         (when (and in-summary
                                    (search "undefined"
                                            (simple-condition-format-control c)))
                           (unless (eq fail 'warning)
                             (setq fail 'style-warning))))))
         (with-compilation-unit ()
           (multiple-value-prog1 (progn ,@forms) (setq in-summary t))))
       (when fail
         (cerror "Proceed anyway"
                 "make-host-1 stopped due to unexpected ~A." fail)))

    #-(or clisp sbcl) `(with-compilation-unit () ,@forms))

  ;; Now we can set the #[+-] readers to our precautionary
  ;; readers that prohibit use of ":sbcl" as the condition.
  (set-dispatch-macro-character #\# #\+ #'she-reader)
  (set-dispatch-macro-character #\# #\- #'she-reader))

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
 (let ((object (compile-file "tools-for-build/ucd.lisp")))
   (load object)
   (delete-file object))

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
