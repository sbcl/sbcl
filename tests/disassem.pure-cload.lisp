(defun disassemble-annotate-funs-f ()
  1)
(defun disassemble-annotate-funs-f2 ()
  2)

(defun disassemble-annotate-funs (m)
  (disassemble-annotate-funs-f)
  (princ 30)
  (if m
      (disassemble-annotate-funs-f2)
      (print 20)))

(with-test (:name :disassemble-annotate-funs)
  (let ((dis (with-output-to-string (str) (disassemble #'disassemble-annotate-funs :use-labels nil :stream str))))
    (assert (search "PRINC" dis))
    (assert (search "PRINT" dis))
    (assert (search "DISASSEMBLE-ANNOTATE-FUNS-F" dis))
    (assert (search "DISASSEMBLE-ANNOTATE-FUNS-F2" dis))))
