;;;; Test of EVAL-WHEN inside a local environment
(cl:in-package :cl-user)

(macrolet ((def (x)
             (push `(:expanded ,x) *bug204-test-status*)
             `(push `(:called ,',x) *bug204-test-status*)))
  (eval-when (:compile-toplevel)
    (def :compile-toplevel))
  (eval-when (:load-toplevel)
    (def :load-toplevel)))
