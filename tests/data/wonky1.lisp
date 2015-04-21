"Line: 8, Column: 23, File-Position: 108" ; expect this
#|
 More randomness
|#


;;;
(progn (defvar *foo* #[ hi ]) x)
                   ;; ^ NO SUCH MACRO
