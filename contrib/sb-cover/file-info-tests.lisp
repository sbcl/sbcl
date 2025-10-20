;;;; Tests that we compute the right character-by-character state

(in-package "SB-COVER-TEST")

(sb-cover:clear-coverage)

(proclaim '(optimize sb-cover:store-coverage-data))
(compile-load "test-data-1")
(assert (equalp (get-states "test-data-1")
                ;;( i n - p a c k a g e   s b - c o v e r - t e s t )
                #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  0
                  ;;( d e f u n   t e s t 1   ( )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  ;;    ( +   1   2 ) )
                  0 1 1 2 2 2 2 2 2 2 1 0)))

(test1)
(assert (equalp (get-states "test-data-1")
                ;;( i n - p a c k a g e   s b - c o v e r - t e s t )
                #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  0
                  ;;( d e f u n   t e s t 1   ( )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  ;;    ( +   1   2 ) )
                  0 1 1 1 1 1 1 1 1 1 1 0)))

(sb-cover:clear-coverage)
(compile-load "test-data-2-only")
(assert (equalp (get-states "test-data-2-only")
                ;;( i n - p a c k a g e   s b - c o v e r - t e s t )
                #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  0
                  ;;( d e f u n   t e s t 2   ( x )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  ;;    ( l e t   ( ( a   0 ) )
                  0 1 1 2 2 2 2 2 2 2 2 2 2 2 2
                  ;;        ( w h e n   (  p  l  u  s  p     x  )
                  0 1 1 2 2 2 2 2 2 2 2 10 10 10 10 10 10 10 10 10
                  ;;            ( i n c f   a ) )
                  0 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2
                  ;;        a ) )
                  0 1 1 2 2 2 2 1)))

(test2 1)
(assert (equalp (get-states "test-data-2-only")
                ;;( i n - p a c k a g e   s b - c o v e r - t e s t )
                #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  0
                  ;;( d e f u n   t e s t 2   ( x )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  ;;    ( l e t   ( ( a   0 ) )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  ;;        ( w h e n   ( p l u s p   x )
                  0 1 1 1 1 1 1 1 1 1 1 9 9 9 9 9 9 9 9 9
                  ;;            ( i n c f   a ) )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  ;;        a ) )
                  0 1 1 1 1 1 1 1)))

(test2 -1)
(assert (equalp (get-states "test-data-2-only")
                ;;( i n - p a c k a g e   s b - c o v e r - t e s t )
                #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  0
                  ;;( d e f u n   t e s t 2   ( x )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  ;;    ( l e t   ( ( a   0 ) )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  ;;        ( w h e n   ( p l u s p   x )
                  0 1 1 1 1 1 1 1 1 1 1 5 5 5 5 5 5 5 5 5
                  ;;            ( i n c f   a ) )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  ;;        a ) )
                  0 1 1 1 1 1 1 1)))

(sb-cover:clear-coverage)
(compile-load "test-data-suppressed")
(assert (equalp (get-states "test-data-suppressed")
                ;;( i n - p a c k a g e   s b - c o v e r - t e s t )
                #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  0
                  ;;# - s b c l
                  0 0 0 0 0 0 0
                  ;;(  d  e  f  u  n     s  u  p  p  r  e  s  s  e  d     (  x  )
                  0 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15
                  ;;      (  1  +     x  )  )
                  0 15 15 15 15 15 15 15 15 15)))

(sb-cover:clear-coverage)
(compile-load "test-data-comma")
(assert (equalp (get-states "test-data-comma")
                ;;( i n - p a c k a g e   s b - c o v e r - t e s t )
                #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  0
                  ;;( d e f u n   c o m m a   ( x )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  ;;    ` ( 0   1   , ( 1 +   x )   3   .   , ( i f   (  e  v  e  n  p     x  )    ( l i s t   4 )   ( l i s t   5 ) ) ) )
                  0 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 10 10 10 10 10 10 10 10 10 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1)))

(comma 1)
(assert (equalp (get-states "test-data-comma")
                ;;( i n - p a c k a g e   s b - c o v e r - t e s t )
                #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  0
                  ;;( d e f u n   c o m m a   ( x )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  ;;    ` ( 0   1   , ( 1 +   x )   3   .   , ( i f   ( e v e n p   x )   ( l i s t   4 )   ( l i s t   5 ) ) ) )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 6 6 6 6 6 6 6 6 6 1 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1)))

(comma 2)
(assert (equalp (get-states "test-data-comma")
                ;;( i n - p a c k a g e   s b - c o v e r - t e s t )
                #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  0
                  ;;( d e f u n   c o m m a   ( x )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  ;;    ` ( 0   1   , ( 1 +   x )   3   .   , ( i f   ( e v e n p   x )   ( l i s t   4 )   ( l i s t   5 ) ) ) )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 5 5 5 5 5 5 5 5 5 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))
