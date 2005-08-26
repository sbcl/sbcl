;;;; Compiler-macro tests

;;; taken from CLHS example
(defun square (x)
  (expt x 2))

(define-compiler-macro square (&whole form arg)
  (if (atom arg)
      `(expt ,arg 2)
      (case (car arg)
        (square (if (= (length arg) 2)
                    `(expt ,(nth 1 arg) 4)
                    form))
        (expt   (if (= (length arg) 3)
                    (if (numberp (nth 2 arg))
                        `(expt ,(nth 1 arg) ,(* 2 (nth 2 arg)))
                         `(expt ,(nth 1 arg) (* 2 ,(nth 2 arg))))
                    form))
        (otherwise `(expt ,arg 2)))))

(assert (eql 81 (square (square 3))))

(multiple-value-bind (expansion expanded-p) (macroexpand '(square x))
  (assert (equal '(square x) expansion))
  (assert (not expanded-p)))

(assert (equal '(expt x 2)
               (funcall (compiler-macro-function 'square)
                        '(square x)
                        nil)))

(assert (equal '(expt x 4)
               (funcall (compiler-macro-function 'square)
                        '(square (square x))
                        nil)))

(assert (equal '(expt x 2)
               (funcall (compiler-macro-function 'square)
                        '(funcall #'square x)
                        nil)))

