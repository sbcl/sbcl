;;; Consider that the CL-USER package (among others) starts out as empty.
;;; So the table-cells are #(0 0 0). Then perform the following operations:
;;; * (intern "X") ; cells => #(0 X 0)
;;; * (intern "Y") ; cells => #(Y X 0)
;;; * (intern "Z") ; cells => #(Z 0 X 0 Y 0 0)
;;; unintern writes a tombstone as required by open-addressing tables
;;; * (unintern 'z) ; cells => #(-1 0 X 0 Y 0 0)
;;; and now you can try to unintern X and Y until you're blue in the face,
;;; but you CAN NOT convince a finalizer attached to either one to execute,
;;; because the initial table holding 2 symbols is in +PSEUDO-STATIC-GENERATION+.
;;; We can't 0-fill symbol vectors because FIND-SYMBOL is lock-free
;;; sans hazard pointers, and we don't want symbols to seem to transiently turn
;;; nonexistent when up-sizing a table.

;;; But it is so incredibly confusing and unintuitive that playing around
;;; with intern/unintern does not have the effect of letting go of strong references
;;; to symbols in initially empty packages. Granted this is a particular edge case.
;;; The GC limitation is actually far more pervasive, extending to hash-tables
;;; and everything else. But empty packages are excruciatingly and visibly
;;; badly behaved.

;;; CL-USER's internals aren't empty, merely because of READINg this file.
;;; So use the externals, of which there should be none.
(defun extern (name)
  (let ((s (make-symbol name))
        (p *package*))
    (sb-impl::add-symbol (sb-impl::package-external-symbols *package*) s)
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
