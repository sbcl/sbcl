;;; fasl-dumper and circular trees, reported by Marco Monteiro
(COMPILE NIL
         '(LAMBDA ()
           (LIST '#5= (#5# #5# . #5#))
           (LIST '#6= (#7= (#6#) . #7#))))
