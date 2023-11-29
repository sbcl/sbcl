#+(and linux gc-stress) (invoke-restart 'run-tests::skip-file)

(handler-case (require :sb-gmp)
  (warning (c)
    (when (search "GMP not loaded" (princ-to-string c))
      (invoke-restart 'run-tests::skip-file))))

;; FIXME: do we also want to load sb-gmp/tests-stress.lisp?
;; Those tests are wicked slow, taking about 47 seconds on my computer
(load "../contrib/sb-gmp/tests.lisp")
