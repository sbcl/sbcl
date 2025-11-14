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

(sb-cover:clear-coverage)
(compile-load "test-data-if")
(let (false true)
  (sb-cover:reset-coverage)
  (test-if 1)
  (assert (equalp (get-states "test-data-if")
                  ;;( i n - p a c k a g e   s b - c o v e r - t e s t )
                  #(2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
                    0
                    ;;( d e f u n   t e s t - i f   ( x )
                    0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                    ;;    ( i f   ( e v e n p   x )
                    0 1 1 1 1 1 1 6 6 6 6 6 6 6 6 6
                    ;;            ( 1 +   x )
                    0 1 1 1 1 1 1 2 2 2 2 2 2
                    ;;            ( 1 -   x ) ) )
                    0 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))
  (setf false (sb-cover:save-coverage))
  (sb-cover:reset-coverage)
  (test-if 2)
  (assert (equalp (get-states "test-data-if")
                  ;;( i n - p a c k a g e   s b - c o v e r - t e s t )
                  #(2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
                    0
                    ;;( d e f u n   t e s t - i f   ( x )
                    0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                    ;;    ( i f   ( e v e n p   x )
                    0 1 1 1 1 1 1 9 9 9 9 9 9 9 9 9
                    ;;            ( 1 +   x )
                    0 1 1 1 1 1 1 1 1 1 1 1 1
                    ;;            ( 1 -   x ) ) )
                    0 1 1 1 1 1 1 2 2 2 2 2 2 1 1)))
  (setf true (sb-cover:save-coverage))
  (sb-cover:reset-coverage)
  (assert (equalp (get-states "test-data-if")
                  ;;( i n - p a c k a g e   s b - c o v e r - t e s t )
                  #(2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
                    0
                    ;;( d e f u n   t e s t - i f   ( x )
                    0 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
                    ;;    ( i f   (  e  v  e  n  p     x  )
                    0 2 2 2 2 2 2 10 10 10 10 10 10 10 10 10
                    ;;            ( 2 +   x )
                    0 2 2 2 2 2 2 2 2 2 2 2 2
                    ;;            ( 2 -   x ) ) )
                    0 2 2 2 2 2 2 2 2 2 2 2 2 2 2)))
  (sb-cover:merge-coverage false)
  (sb-cover:merge-coverage true)
  (assert (equalp (get-states "test-data-if")
                  ;;( i n - p a c k a g e   s b - c o v e r - t e s t )
                  #(2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
                    0
                    ;;( d e f u n   t e s t - i f   ( x )
                    0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                    ;;    ( i f   ( e v e n p   x )
                    0 1 1 1 1 1 1 5 5 5 5 5 5 5 5 5
                    ;;            ( 1 +   x )
                    0 1 1 1 1 1 1 1 1 1 1 1 1
                    ;;            ( 1 -   x ) ) )
                    0 1 1 1 1 1 1 1 1 1 1 1 1 1 1))))
