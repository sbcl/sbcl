(in-package :sb-manual)

;;; FIXME: Write some documentation about how to interpret the results.
(defsection @sb-cover (:title "sb-cover")
  "The `SB-COVER` module provides a code coverage tool for SBCL. The
  tool has support for expression coverage, and for some branch
  coverage. Coverage reports are only generated for code compiled
  using COMPILE-FILE with the value of the
  SB-COVER:STORE-COVERAGE-DATA optimization quality set to 3.

  As of SBCL 1.0.6, `SB-COVER` is still experimental, and the
  interfaces documented here might change in later versions.

  How to use it:

      ;;; Load SB-COVER
      (require :sb-cover)

      ;;; Turn on generation of code coverage instrumentation in the compiler
      (declaim (optimize sb-cover:store-coverage-data))

      ;;; Load some code, ensuring that it's recompiled with the new optimization
      ;;; policy.
      (asdf:oos 'asdf:load-op :cl-ppcre-test :force t)

      ;;; Run the test suite.
      (cl-ppcre-test:test)

      ;;; Produce a coverage report
      (sb-cover:report \"/tmp/report/\")

      ;;; Turn off instrumentation
      (declaim (optimize (sb-cover:store-coverage-data 0)))"
  (sb-cover:report function)
  (sb-cover:reset-coverage function)
  (sb-cover:clear-coverage function)
  (sb-cover:save-coverage function)
  (sb-cover:save-coverage-in-file function)
  (sb-cover:restore-coverage function)
  (sb-cover:restore-coverage-from-file function)
  (sb-cover:merge-coverage function)
  (sb-cover:merge-coverage-from-file function))
