;;; Set up the cross-compiler.
(setf *print-level* 5 *print-length* 5)
(load "src/cold/shared.lisp")
(in-package "SB-COLD")
;;; FIXME: these prefixes look like non-pathnamy ways of defining a
;;; relative pathname.  Investigate whether they can be made relative
;;; pathnames.
(setf *host-obj-prefix* "obj/from-host/"
      *target-obj-prefix* "obj/from-xc/")
(load "src/cold/set-up-cold-packages.lisp")
(load "src/cold/defun-load-or-cload-xcompiler.lisp")
(load-or-cload-xcompiler #'host-load-stem)

(defun proclaim-target-optimization ()
  (let ((debug (if (position :sb-show *shebang-features*) 2 1)))
    (sb-xc:proclaim
     `(optimize
       (compilation-speed 1) (debug ,debug)
       ;; CLISP's pretty-printer is fragile and tends to cause stack
       ;; corruption or fail internal assertions, as of 2003-04-20; we
       ;; therefore turn off as many notes as possible.
       (sb!ext:inhibit-warnings #-clisp 2 #+clisp 3)
       ;; SAFETY = SPEED (and < 3) should provide reasonable safety,
       ;; but might skip some unreasonably expensive stuff
       ;; (e.g. %DETECT-STACK-EXHAUSTION in sbcl-0.7.2).
       (safety 2) (space 1) (speed 2)
       ;; sbcl-internal optimization declarations:
       ;;
       ;; never insert stepper conditions
       (sb!c:insert-step-conditions 0)))))
(compile 'proclaim-target-optimization)

(defun in-target-cross-compilation-mode (fun)
  "Call FUN with everything set up appropriately for cross-compiling
   a target file."
  (let (;; In order to increase microefficiency of the target Lisp,
        ;; enable old CMU CL defined-function-types-never-change
        ;; optimizations. (ANSI says users aren't supposed to
        ;; redefine our functions anyway; and developers can
        ;; fend for themselves.)
        #!-sb-fluid
        (sb!ext:*derive-function-types* t)
        ;; Let the target know that we're the cross-compiler.
        (*features* (cons :sb-xc *features*))
        ;; We need to tweak the readtable..
        (*readtable* (copy-readtable)))
    ;; ..in order to make backquotes expand into target code
    ;; instead of host code.
    ;; FIXME: Isn't this now taken care of automatically by
    ;; toplevel forms in the xcompiler backq.lisp file?
    (set-macro-character #\` #'sb!impl::backquote-macro)
    (set-macro-character #\, #'sb!impl::comma-macro)

    (set-dispatch-macro-character #\# #\+ #'she-reader)
    (set-dispatch-macro-character #\# #\- #'she-reader)
    ;; Control optimization policy.
    (proclaim-target-optimization)
    ;; Specify where target machinery lives.
    (with-additional-nickname ("SB-XC" "SB!XC")
      (funcall fun))))
(compile 'in-target-cross-compilation-mode)


;; Supress function/macro redefinition warnings under clisp.
#+clisp (setf custom:*suppress-check-redefinition* t)

(setf *target-compile-file* #'sb-xc:compile-file)
(setf *target-assemble-file* #'sb!c:assemble-file)
(setf *in-target-compilation-mode-fn* #'in-target-cross-compilation-mode)

;;; Run the cross-compiler to produce cold fasl files.
(load "src/cold/compile-cold-sbcl.lisp")


;;; miscellaneous tidying up and saving results
(let ((filename "output/object-filenames-for-genesis.lisp-expr"))
  (ensure-directories-exist filename :verbose t)
  (with-open-file (s filename :direction :output :if-exists :supersede)
    (write *target-object-file-names* :stream s :readably t)))

;;; Let's check that the type system was reasonably sane. (It's easy
;;; to spend a long time wandering around confused trying to debug
;;; cold init if it wasn't.)
(when (position :sb-test *shebang-features*)
  (load "tests/type.after-xc.lisp"))

;;; If you're experimenting with the system under a cross-compilation
;;; host which supports CMU-CL-style SAVE-LISP, this can be a good
;;; time to run it. The resulting core isn't used in the normal build,
;;; but can be handy for experimenting with the system. (See slam.sh
;;; for an example.)
(when (position :sb-after-xc-core *shebang-features*)
  #+cmu (ext:save-lisp "output/after-xc.core" :load-init-file nil)
  #+sbcl (sb-ext:save-lisp-and-die "output/after-xc.core")
  #+openmcl (ccl::save-application "output/after-xc.core")
  #+clisp (ext:saveinitmem "output/after-xc.core"))
#+cmu (ext:quit)
#+clisp (ext:quit)
#+abcl (ext:quit)
