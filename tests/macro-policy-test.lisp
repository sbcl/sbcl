(declaim (optimize speed))

(eval-when (:compile-toplevel :execute)
  (if (and (boundp '*frob-macro-policy*)
           (eq (symbol-value '*frob-macro-policy*) t))
      (set-macro-policy '((speed 0))))
  ;; RANDOM-EYES is purely a "macro helper" and should never generate efficiency
  ;; notes. If all three situations were listed in the EVAL-WHEN - presumably
  ;; because there are uses of RANDOM-EYES in the generated code- there would be
  ;; an efficiency note regarding the code compiled into the fasl,
  ;; but no note about the inefficiency of the in-memory code.
  ;; Problem is, that's somewhat difficult to check for in an assertion.
  (defun random-eyes (x) (length (string x))))

;; All these macros use generic + which is "inefficient"
(defmacro fruitbat (arg)
  `(cons ,arg ,(+ (random-eyes arg) 100)))

;; Here we're just going to assert that no efficiency note is
;; produced regarding the fact that FOO-EXPANDER uses GENERIC+.
(define-compiler-macro foo-expander (&whole form x)
  (if (constantp x)
      (+ (eval x) 19)
      form))

(defun way1 (x)
  (fruitbat x))

(defun way2 (x)
  (macrolet ((local-foo (arg)
               `(cons ,arg ,(+ (random-eyes arg) 100))))
    (local-foo x)))

(macrolet ((local-foo (arg)
             `(cons ,arg ,(+ (random-eyes arg) 100))))
  (local-foo *print-base*))
