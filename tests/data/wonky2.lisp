"(in form starting at line: 11, column: 0, position: 262)" ; expect this
;;; Randomness
#|
 More randomness
|#


;; It would be really nice if this reported that the
;; error occurred at the "#." but it's better than before,
;; which reported "line 1 column 0"
(macrolet ()
  (defvar *foo* #.(no-such-function)))

