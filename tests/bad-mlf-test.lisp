(defun zook ()
  (declare (optimize sb-c::store-source-form))
  (macrolet ((foo (x)
               (declare (ignore x))
               ''hi))
    ;; This is a structure whose MAKE-LOAD-FORM signals an error.
    ;; The function itself should be compilable because
    ;; the post-macroexpansion expression is trivial.
    (foo #.(make-dump-me))))
