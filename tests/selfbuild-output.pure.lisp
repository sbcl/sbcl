
(with-test (:name :make-list-%make-list-not-called
                  :fails-on (:not :x86-64))
  (assert (not (ctu:find-named-callees #'make-list))))
