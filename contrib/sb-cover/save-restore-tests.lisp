(in-package "SB-COVER-TEST")

(sb-cover:clear-coverage)

(proclaim '(optimize sb-cover:store-coverage-data))
(compile-load "test-data-1")

(let ((saved (sb-cover:save-coverage)))
  (sb-cover:reset-coverage)
  (assert (equalp (get-states "test-data-1")
                  ;;( i n - p a c k a g e   s b - c o v e r - t e s t )
                  #(2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
                    0
                    ;;( d e f u n   t e s t 2   ( )
                    0 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
                    ;;    ( +   1   1 ) )
                    0 2 2 2 2 2 2 2 2 2 2 0)))
  (sb-cover:restore-coverage saved)
  (assert (equalp (get-states "test-data-1")
                  ;;( i n - p a c k a g e   s b - c o v e r - t e s t )
                  #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                    0
                    ;;( d e f u n   t e s t 1   ( )
                    0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                    ;;    ( +   1   2 ) )
                    0 1 1 2 2 2 2 2 2 2 1 0))))
