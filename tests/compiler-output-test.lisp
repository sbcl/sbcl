;;; compiled by compiler.impure.lisp

(defun square (x)
  (declare (optimize speed))
  (* x x))

(defun unused-var (x)
  1)

(defun style-thing (&optional x &key y)
  (cons x y))

(defun (bad name) ())

;; "fatal error" from this one
)
