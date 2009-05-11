;;; Now that we use the compiler for macros, interpreted /SHOW doesn't
;;; work until later in init.
#+sb-show (print "/hello, world!")

;;; Until PRINT-OBJECT and other machinery is set up, we want limits
;;; on printing to avoid infinite output.  (Don't forget to undo these
;;; tweaks after the printer is set up. It'd be cleaner to use LET to
;;; make sure that happens automatically, but LET is implemented in
;;; terms of the compiler, and the compiler isn't initialized yet.)
(setq *print-length* 10)
(setq *print-level* 5)
(setq *print-circle* t)

;;; Do warm init without compiling files.
(defvar *compile-files-p* nil)
#+sb-show (print "/about to LOAD warm.lisp (with *compile-files-p* = NIL)")
(load "src/cold/warm.lisp")

;;; Unintern no-longer-needed stuff before the possible PURIFY in
;;; SAVE-LISP-AND-DIE.
#-sb-fluid (sb-impl::!unintern-init-only-stuff)

;;; Now that the whole system is built, we don't need to hobble the
;;; printer any more, so we can restore printer control variables to
;;; their ANSI defaults.
(setq *print-length* nil)
(setq *print-level* nil)
(setq *print-circle* nil)

(sb-int:/show "done with warm.lisp, about to GC :FULL T")
(sb-ext:gc :full t)

;;; resetting compilation policy to neutral values in preparation for
;;; SAVE-LISP-AND-DIE as final SBCL core (not in warm.lisp because
;;; SB-C::*POLICY* has file scope)
(sb-int:/show "setting compilation policy to neutral values")
(proclaim
 '(optimize
   (compilation-speed 1) (debug 1) (inhibit-warnings 1)
   (safety 1) (space 1) (speed 1)))

;;; Lock internal packages
#+sb-package-locks
(dolist (p (list-all-packages))
  (unless (member p (mapcar #'find-package '("KEYWORD" "CL-USER")))
    (sb-ext:lock-package p)))

(sb-int:/show "done with warm.lisp, about to SAVE-LISP-AND-DIE")
;;; Even if /SHOW output was wanted during build, it's probably
;;; not wanted by default after build is complete. (And if it's
;;; wanted, it can easily be turned back on.)
#+sb-show (setf sb-int:*/show* nil)
;;; The system is complete now, all standard functions are
;;; defined.
(sb-kernel::ctype-of-cache-clear)
(setq sb-c::*flame-on-necessarily-undefined-thing* t)

;;; Clean up stray symbols from the CL-USER package.
(do-symbols (symbol "CL-USER")
  (when (eq (symbol-package symbol) (find-package "CL-USER"))
    (unintern symbol "CL-USER")))

(sb-ext:save-lisp-and-die "output/sbcl.core")
