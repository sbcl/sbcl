(with-test (:name :tree-shaker :skipped-on :sb-devel)
  ;; Assert that even without the "!" prefix convention
  ;; these used-only-at-cross-compile-time macros disappear.
  (dolist (s '("DEFINE-FOP"
               "DEFINE-TYPE-CLASS"
               "DEFINE-TYPE-METHOD"
               "DEF-TYPE-TRANSLATOR"
               "DEFINE-TYPE-VOP"
               "DEFINE-PRIMITIVE-OBJECT"))
    (assert (not (apropos-list s)))))

(with-test (:name :no-v0p-ex1stsp-in-build ; spelled L33t Hax0r style on purpose
            :skipped-on (or :sb-devel
                            :sb-xref-for-internals))
  (assert (null (apropos-list "VOP-EXISTSP"))))
