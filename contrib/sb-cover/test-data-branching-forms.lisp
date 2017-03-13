(in-package sb-cover-test)

;; Total of 13 forms have coverage data (the defun plus 12 subforms)
(defun test-branching-forms ()
  ;; The tests forms here need to be a cons to have coverage data
  ;; instrumentation, but the progn-like clauses should allow for coverage
  ;; data instrumentation if non-empty, even if they contain a single
  ;; atom.

  ;; This should have coverage data for 4 forms (when, progn, t, and :foo)
  (when (progn t)
    :foo)

  ;; This should have coverage data for 4 forms (unless, progn, nil, and :foo)
  (unless (progn nil)
    :foo)

  ;; This should have coverage data for 4 forms (cond, progn, t, and :foo)
  (cond
    ((progn t) :foo)))
