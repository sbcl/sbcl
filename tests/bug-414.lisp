;;; compiling and disassembling this used to give
;;;
;;;    WARNING: bogus form-number in form!  The source file has probably
;;;    been changed too much to cope with.
;;;
;;; but the symptoms have disappeared.
(defun bug-414 (x y z)
  (declare (optimize debug))
  (list x y z))
