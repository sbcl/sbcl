;;;; a test of EVAL-WHEN inside a local environment (which will be
;;;; compiled and loaded, and have its side effects checked, by some
;;;; other file which runs automatically as part of the test suite)

(cl:in-package :cl-user)

(macrolet ((def (x)
             (pushnew `(:expanded ,x) *bug204-test-status* :test #'equalp)
             `(pushnew `(:called ,',x) *bug204-test-status* :test #'equalp)))
  (eval-when (:compile-toplevel)
    (def :compile-toplevel))
  (eval-when (:load-toplevel)
    (def :load-toplevel)))
