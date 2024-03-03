(with-test (:name :symbol-case-as-jump-table
                  :skipped-on (not (or :x86 :x86-64)))
  ;; Assert that a prototypical example of (CASE symbol ...)
  ;; was converted to a jump table.
  (let ((c (sb-kernel:fun-code-header #'sb-debug::parse-trace-options)))
    (assert (>= (sb-kernel:code-jump-table-words c) 14))))

(with-test (:name :hash-case-compiled-only)
  (let ((evaluator-expansion
         (macroexpand-1 '(case x (a 1) (b 2) (c (print'hi)) (d 3))))
        (compiler-expansion
         (macroexpand-1 '(case x (a 1) (b 2) (c (print'hi)) (d 3))
                        (sb-kernel:make-null-lexenv))))
    (assert (eq (car (fourth evaluator-expansion)) 'cond))
    (when (gethash 'sb-c:multiway-branch-if-eq sb-c::*backend-parsed-vops*)
      (assert (eq (car compiler-expansion) 'let*)))))
