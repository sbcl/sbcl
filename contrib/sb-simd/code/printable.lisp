(in-package #:sb-simd-internals)

(defclass printable () ())

;;; Gather a plist of all class slots.  We use this plist to conveniently
;;; define PRINT-OBJECT.
(defgeneric printable-slot-plist (printable)
  (:method-combination append :most-specific-last))

(defmethod printable-slot-plist append (printable)
  '())

(defmethod print-object ((printable printable) stream)
  (print-unreadable-object (printable stream :type t)
    (pprint-logical-block (stream (printable-slot-plist printable))
      (loop
        (pprint-exit-if-list-exhausted)
        (write (pprint-pop) :stream stream)
        (pprint-exit-if-list-exhausted)
        (write-char #\space stream)
        (write (pprint-pop) :stream stream)
        (pprint-exit-if-list-exhausted)
        (write-char #\space stream)
        (pprint-newline :linear stream)))))
