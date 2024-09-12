
;;; Assert that "old" symbol tables do not cause garbage retention
;;; (even if pseudo-static!) because they get zero-filled.
;;; CL-USER's internals aren't empty, merely because of READINg this file.
;;; So use the externals, of which there should be none.
(defun extern (name)
  (let ((s (make-symbol name))
        (p *package*))
    (sb-impl::add-symbol (sb-impl::package-external-symbols *package*) s 'intern)
    (sb-impl::%set-symbol-package s p)
    s))

(with-test (:name :empty-package-starts-with-readonly-tables)
  (extern "X")
  (extern "Y")
  (extern "Z")
  (let ((wps (mapcar (lambda (name) (make-weak-pointer (find-symbol name)))
                     '("X" "Y" "Z"))))
    (unintern (find-symbol "Z"))
    (unintern (find-symbol "X"))
    (unintern (find-symbol "Y"))
    (sb-sys:scrub-control-stack)
    (gc)
    (assert (< (count-if #'weak-pointer-value wps) 3))))
