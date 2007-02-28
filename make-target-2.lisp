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

;;; Do warm init.
(defvar *compile-files-p* t)
#+sb-show (print "/about to LOAD warm.lisp (with *compile-files-p* = T)")
(load "src/cold/warm.lisp")
