;;;; Test of EVAL-WHEN inside a local environment
(cl:in-package :cl-user)

(macrolet ((def (x)
             (pushnew `(:expanded ,x) *bug204-test-status* :test #'equalp)
             `(pushnew `(:called ,',x) *bug204-test-status* :test #'equalp)))
  (eval-when (:compile-toplevel)
    (def :compile-toplevel))
  (eval-when (:load-toplevel)
    (def :load-toplevel)))
