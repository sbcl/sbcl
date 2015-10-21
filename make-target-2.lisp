;;; Now that we use the compiler for macros, interpreted /SHOW doesn't
;;; work until later in init.
#+sb-show (print "/hello, world!")

;;; Until PRINT-OBJECT and other machinery is set up, we want limits
;;; on printing to avoid infinite output.
(let ((*print-length*   10)
      (*print-level*     5)
      (*print-circle*    t)
      (*compile-files-p* t))
  (declare (special *compile-files-p*))
;;; Do warm init.
  (print "/about to LOAD warm.lisp (with *compile-files-p* = T)")
  (load "src/cold/warm.lisp"))
