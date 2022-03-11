
(defmacro get-policy (&rest args &environment env)
  (declare (ignore args))
  ;; Return the _ordinary_ compilation policy as a sexpr.
  ;; You can't detect the macro policy from a lexenv because the macro's ENV arg
  ;; is the policy for compiling target code, not for compiling the macro.
  (list 'quote
        ;; Test should pass irrespective of whether the baseline policy
        ;; asks for allocation profiling.
        (remove 'sb-c::instrument-consing
                (sb-c::policy-to-decl-spec (sb-c::lexenv-policy env))
                :key 'car)))

(declaim (optimize (speed 2) sb-c:store-coverage-data))

(with-test (:name :obey-declaim-1)
  (assert (equal (get-policy)
                 '((sb-c::inhibit-warnings 1)
                   (speed 2) (space 1) (safety 1) (debug 1)
                   (compilation-speed 1) (sb-c:store-coverage-data 3)))))

(eval-when (:compile-toplevel)
  (set-macro-policy '((speed 0) (safety 3))))

(with-test (:name :obey-declaim-2) ; *POLICY* is unchanged at this point
  (assert (equal (get-policy)
                 '((sb-c::inhibit-warnings 1)
                   (speed 2) (space 1) (safety 1) (debug 1)
                   (compilation-speed 1) (sb-c:store-coverage-data 3)))))

(declaim (optimize (speed 1) (sb-c:store-coverage-data 0)))

;; Now it's changed
(with-test (:name :obey-declaim-3)
  (assert (equal (get-policy)
                 '((sb-c::inhibit-warnings 1)
                   (speed 1) (space 1) (safety 1) (debug 1)
                   (compilation-speed 1) (sb-c:store-coverage-data 0)))))

;; And again
(declaim (optimize (sb-c:store-coverage-data 2)))

(with-test (:name :obey-declaim-4)
  (assert (equal (get-policy)
                 '((sb-c::inhibit-warnings 1)
                   (speed 1) (space 1) (safety 1) (debug 1)
                   (compilation-speed 1) (sb-c:store-coverage-data 2)))))
