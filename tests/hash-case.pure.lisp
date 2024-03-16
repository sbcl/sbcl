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
          (funcall (compile nil `(lambda ()
                                   (macrolet ((x (x) `',(macroexpand-1 x)))
                                     (x (case x (a 1) (b 2) (c (print'hi)) (d 3)))))))))
    (assert (eq (car (fourth evaluator-expansion)) 'cond))
    (when (gethash 'sb-c:multiway-branch-if-eq sb-c::*backend-parsed-vops*)
      (assert (eq (car compiler-expansion) 'let*)))))

(with-test (:name :type-derivation)
  (when (gethash 'sb-c:jump-table sb-c::*backend-parsed-vops*)
   (assert-type
    (lambda (x)
      (declare ((member a b c d) x))
      (case x
        (a (print 1))
        (b (print 2))
        (c (print 4))
        (d (print 3))
        (e (print 5))))
    (integer 1 4))))

(with-test (:name :type-derivation-constraints)
  (when (gethash 'sb-c:jump-table sb-c::*backend-parsed-vops*)
    (assert-type
     (lambda (x)
       (declare ((not (member b)) x)
                (optimize speed))
       (unless (eq x 'a)
         (case x
           (a (print 1))
           (b (print 2))
           (c (print 3))
           (d (print 4))
           (e (print 6))
           (g (print 5)))))
     (or null (integer 3 6)))))
