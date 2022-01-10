(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *breadcrumbs* nil))

(defglobal **a-global**
  (progn (push 'global-initform *breadcrumbs*) 'foo1))

(define-load-time-global **a-load-time-global**
  (progn (push 'ltg-initform *breadcrumbs*) 'foo2))

(eval-when (:compile-toplevel)
  ;; In the compiler, DEFGLOBAL evals its value form at compile-time
  ;; DEFINE-LOAD-TIME-GLOBAL does not
  (assert (equal *breadcrumbs* '(global-initform)))
  (assert (eq (sb-int:info :variable :always-bound '**a-global**)
              :always-bound))
  (assert (eq (sb-int:info :variable :always-bound '**a-load-time-global**)
              :eventually)))

(eval-when (:compile-toplevel :load-toplevel)
  (defun test-use-ltg () (null **a-load-time-global**))
  ;; At compile-time, the load-time-global should be unbound,
  ;; and importantly the function that was compiled that uses
  ;; it should signal an error. The latter implies the former
  ;; so we needn't check both assertions.
  (eval-when (:compile-toplevel)
    (assert (eq :win (handler-case (test-use-ltg) (error () :win))))))

(with-test (:name :load-time-global-1)
  (assert (equal *breadcrumbs* '(ltg-initform global-initform)))

  ;; Can not legally make **a-load-time-global** unbound
  (assert (eq :win (handler-case (makunbound '**a-load-time-global**)
                     (error () :win))))

  ;; Make it unbound "illegally" which will give circumstantial evidence
  ;; that the function accessing it has assumed :ALWAYS-BOUND.
  (sb-impl::%makunbound '**a-load-time-global**)

  ;; Finally, verify that TEST-USE-GLOBAL2 does *NOT* contain the boundp
  ;; check. It just returns NIL because the unbound marker is not EQ to nil.
  (assert (not (test-use-ltg))))

;; :ALWAYS-BOUND takes precedence over :EVENTUALLY
(defglobal **anotherone** 3)
(define-load-time-global **anotherone** 4)
(eval-when (:compile-toplevel)
  ;; Assert that the second of those two forms did basically nothing.
  (assert (eq (sb-int:info :variable :always-bound '**anotherone**)
              :always-bound)))
(with-test (:name :load-time-global-2)
  (assert (eql **anotherone** 3)))
