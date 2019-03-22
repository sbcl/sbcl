(progn
  (load "src/cold/shared.lisp")
  (let ((*print-pretty* nil)
        (*print-length* nil))
    (dolist (thing '(("SB-XC" "*FEATURES*")
                     ("SB-COLD" "*SHEBANG-BACKEND-SUBFEATURES*")))
      (let* ((sym (intern (cadr thing) (car thing)))
             (val (symbol-value sym)))
        (when val
          (format t "~&target ~S = ~S~%" sym  val))))))
(in-package "SB-COLD")
(progn
  (let ((*readtable* *xc-readtable*)) (load "tools-for-build/ldso-stubs.lisp"))
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
                       (lambda (c &aux (fc (simple-condition-format-control c)))
                         ;; hack for PPC. See 'build-order.lisp-expr'
                         ;; Ignore the warning, and the warning about the warning.
                         (unless (and (stringp fc)
                                      (or (search "not allowed by the operand type" fc)
                                          (search "ignoring FAILURE-P return" fc)))
                           (setq fail 'warning))))
                      ;; Prevent regressions on a few platforms
                      ;; that are known to build cleanly.
                      (sb-int:simple-style-warning
                       (lambda (c &aux (fc (simple-condition-format-control c)))
                         (when (and (feature-in-list-p '(:or :x86 :x86-64 :arm64)
                                                       :target)
                                    in-summary
                                    (stringp fc)
                                    (search "undefined" fc))
                           (unless (eq fail 'warning)
                             (setq fail 'style-warning))))))
         (with-compilation-unit ()
           (multiple-value-prog1 (progn ,@forms) (setq in-summary t))))
       (when fail
         (cerror "Proceed anyway"
                 "make-host-1 stopped due to unexpected ~A." fail)))

    #-(or clisp sbcl) `(with-compilation-unit () ,@forms)))

;;; Build the unicode database now. It depends on nothing in the cross-compiler
;;; (and let's keep it that way). This code is slow to run, so compile it.
(let ((object (compile-file "tools-for-build/ucd.lisp")))
  (load object :verbose t)
  (delete-file object))
(dolist (s '(sb-cold::slurp-ucd sb-cold::slurp-proplist sb-cold::output))
  (funcall s))

;;; I don't know the best combination of OPTIMIZE qualities to produce a correct
;;; and reasonably fast cross-compiler in ECL. At over half an hour to complete
;;; make-host-{1,2}, I don't really want to waste any more time finding out.
;;; These settings work, while the defaults do not.
#+ecl (proclaim '(optimize (safety 2) (debug 2)))

(maybe-with-compilation-unit
 (let ((*feature-evaluation-results* nil))
  (load-or-cload-xcompiler #'host-cload-stem)
  (write-feature-eval-results))

 ;; Let's check that the type system, and various other things, are
 ;; reasonably sane. (It's easy to spend a long time wandering around
 ;; confused trying to debug cross-compilation if it isn't.)
 (let ((*readtable* *xc-readtable*)
       (*load-verbose* t))
   (with-math-journal
     (load "tests/type.before-xc.lisp")
     (load "tests/info.before-xc.lisp")
     (load "tests/vm.before-xc.lisp")))

 ;; propagate structure offset and other information to the C runtime
 ;; support code.
 (load "tools-for-build/corefile.lisp" :verbose nil)
 (host-cload-stem "src/compiler/generic/genesis" nil)
) ; END with-compilation-unit

(sb-cold:genesis :c-header-dir-name "src/runtime/genesis")
