(in-package sb-cover-test)

(defmethod method-walk ((o myclass))
  (if (null *print-level*)
      (slot-value o 'slot)
      (1+ *print-level*)))
