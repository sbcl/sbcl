(defvar *mysym*)
(declaim (fixnum *mysym*))

(defun f-unsafe (list-of-syms list-of-vals thunk)
  (declare (optimize (safety 0)))
  (progv list-of-syms list-of-vals
    (funcall thunk)))

(compile 'f-unsafe)

(with-test (:name :unsafe-progv-no-typecheck)
  (f-unsafe '(*mysym*) '("hi")
   (lambda ()
     (assert (stringp (symbol-value (opaque-identity '*mysym*))))))
  (assert (logtest (sb-kernel:get-header-data '*mysym*)
                   sb-vm::+symbol-fast-bindable+)))

(defun f-safe (list-of-syms list-of-vals)
  (progv list-of-syms list-of-vals
    (opaque-identity 'foo)))
(compile 'f-safe)

(with-test (:name :safe-progv)
  ;; first, a package operation should clear the fast-bindable bit
  (lock-package cl:*package*)
  (unlock-package cl:*package*)
  (assert (not (logtest (sb-kernel:get-header-data '*mysym*)
                        sb-vm::+symbol-fast-bindable+)))
  ;; second, whether fast bindable or not, there should be a type-error
  (assert-error (f-safe '(*mysym*) '("hi")))
  ;; however, now it's fast-bindable again
  (assert (logtest (sb-kernel:get-header-data '*mysym*)
                   sb-vm::+symbol-fast-bindable+))
  ;; and still gets a type-error
  (assert-error (f-safe '(*mysym*) '("hi"))))

(with-test (:name :full-call-to-set-symbol-value-does-not-imply-fast-bindable)
  (let ((s (opaque-identity 'flerb)))
    (set s 3)
    (assert (not (logtest (sb-kernel:get-header-data s) sb-vm::+symbol-fast-bindable+)))))
