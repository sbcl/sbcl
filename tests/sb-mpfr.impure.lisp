#+(and linux gc-stress) (invoke-restart 'run-tests::skip-file)

(handler-case (require :sb-mpfr)
  (warning (c)
    (when (or (search "requires at least MPFR vers" (princ-to-string c))
              (search "not loaded" (princ-to-string c)))
      (invoke-restart 'run-tests::skip-file))))
(load "../contrib/sb-mpfr/tests.lisp")
