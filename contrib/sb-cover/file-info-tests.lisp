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

(sb-cover:clear-coverage)
(compile-load "test-data-quote")
(assert (equalp (get-states "test-data-quote")
                ;;( i n - p a c k a g e   s b - c o v e r - t e s t )
                #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  0
                  ;;( d e f u n   q u o t e - c o m m a   ( x )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  ;;    ` ( 1   ' , ( i f   (  e  v  e  n  p     x  )    ( 1 +   x )   ( +   x   2 ) )   , ( +   x   3 ) ) )
                  0 1 1 2 2 2 2 2 2 2 2 2 2 10 10 10 10 10 10 10 10 10 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1)))

(quote-comma 0)
(assert (equalp (get-states "test-data-quote")
                ;;( i n - p a c k a g e   s b - c o v e r - t e s t )
                #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  0
                  ;;( d e f u n   q u o t e - c o m m a   ( x )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  ;;    ` ( 1   ' , ( i f   ( e v e n p   x )   ( 1 +   x )   ( +   x   2 ) )   , ( +   x   3 ) ) )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 9 9 9 9 9 9 9 9 9 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1)))

(quote-comma 1)
(assert (equalp (get-states "test-data-quote")
                ;;( i n - p a c k a g e   s b - c o v e r - t e s t )
                #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  0
                  ;;( d e f u n   q u o t e - c o m m a   ( x )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  ;;    ` ( 1   ' , ( i f   ( e v e n p   x )   ( 1 +   x )   ( +   x   2 ) )   , ( +   x   3 ) ) )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 5 5 5 5 5 5 5 5 5 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))

(sb-cover:clear-coverage)
(defclass myclass () ((slot :initarg :slot)))
(compile-load "test-data-method-walk")
(assert (equalp (get-states "test-data-method-walk")
                ;;( i n - p a c k a g e   s b - c o v e r - t e s t )
                #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  0
                  ;;( d e f m e t h o d   m e t h o d - w a l k   ( ( o   m y c l a s s ) )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  ;;    ( i f   (  n  u  l  l     *  p  r  i  n  t  -  l  e  v  e  l  *  )
                  0 1 1 2 2 2 2 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10
                  ;;            ( s l o t - v a l u e   o   ' s l o t )
                  0 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
                  ;;            ( 1 +   * p r i n t - l e v e l * ) ) )
                  0 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1)))

(let ((*print-level* nil))
  (method-walk (make-instance 'myclass :slot 3)))
(assert (equalp (get-states "test-data-method-walk")
                ;;( i n - p a c k a g e   s b - c o v e r - t e s t )
                #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  0
                  ;;( d e f m e t h o d   m e t h o d - w a l k   ( ( o   m y c l a s s ) )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  ;;    ( i f   ( n u l l   * p r i n t - l e v e l * )
                  0 1 1 1 1 1 1 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9
                  ;;            ( s l o t - v a l u e   o   ' s l o t )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  ;;            ( 1 +   * p r i n t - l e v e l * ) ) )
                  0 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1)))

(let ((*print-level* 3))
  (method-walk (make-instance 'myclass :slot 4)))
(assert (equalp (get-states "test-data-method-walk")
                ;;( i n - p a c k a g e   s b - c o v e r - t e s t )
                #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  0
                  ;;( d e f m e t h o d   m e t h o d - w a l k   ( ( o   m y c l a s s ) )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  ;;    ( i f   ( n u l l   * p r i n t - l e v e l * )
                  0 1 1 1 1 1 1 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
                  ;;            ( s l o t - v a l u e   o   ' s l o t )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  ;;            ( 1 +   * p r i n t - l e v e l * ) ) )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))
