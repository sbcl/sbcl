;;; this test is slow

;;; It's extremely unlikely that a user would make >2^16 packages, but test that it works.
(defun grow-id->package-vector ()
  (let ((table (make-array 65535 :initial-element nil))) ; grow once only. Sorry for cheating
    (replace table sb-impl:*id->package*)
    (setf sb-impl:*id->package* table)))
(compile 'grow-id->package-vector)

(with-test (:name :ridiculous-amount-of-packages)
  (make-package "WATPACKAGE")
  (grow-id->package-vector) ; grow once only. Sorry for cheating
  (loop
    ;; This loop unfortunately takes 2 seconds, which kind of speaks to
    ;; the slowness of package creation. I don't think we need to improve that,
    ;; but we _do_ need to test this, so ... it's a minor point of pain.
    ;; If I had to guess, resizing the mostly-lockfree hash-table is the issue.
    ;; We could grow it all at once to improve the performance.
    (let* ((package (make-package "STRANGE"))
           (id (sb-impl::package-id package))
           (new-name (format nil "TEST-PKGID-~D" id)))
      (unless id (return))
      (rename-package package new-name)))
  (let ((p (find-package "STRANGE")))
    (assert (not (sb-impl::package-id p)))
    (let ((symbol (intern "WAT123" p)))
      (assert (eq (symbol-package symbol) p))
      (delete-package p)
      (assert (not (symbol-package symbol)))
      (import symbol "WATPACKAGE")
      (assert (eq (symbol-package symbol) (find-package "WATPACKAGE")))
      ;; assert that the symbol got a small ID
      (assert (not (sb-int:info :symbol :package symbol)))))
  (delete-package "WATPACKAGE")
  (let ((p (make-package "ANOTHERPACKAGE")))
    (assert (sb-impl::package-id p)))
  (let ((p (make-package "YETANOTHERPACKAGE")))
    (assert (not (sb-impl::package-id p))))
  ;; Now for every package named TEST-PKGIDnm, check that a symbol interned
  ;; in that package can read the bits back correctly (because vops are confusing)
  (let ((n 0))
    (dolist (package (list-all-packages))
      (when (search "TEST-PKGID-" (package-name package))
        (incf n)
        (let ((the-symbol (intern "FROBOLA" package)))
          (assert (eq (symbol-package the-symbol) package)))))
    (assert (> n 65450)))) ; assert that we exercised lots of bit patterns
