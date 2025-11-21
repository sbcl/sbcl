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

(sb-cover:clear-coverage)
(defun generate-code (x y)
  `(if (evenp ,x) (1+ ,y) (1- ,y)))
(compile-load "test-data-read-eval")
;; It's a bit difficult to decide what the state of the READ-EVAL'd
;; code should be without ever running the function; if something
;; changes this test, that might be OK.
(assert (equalp (get-states "test-data-read-eval")
                ;;( i n - p a c k a g e   s b - c o v e r - t e s t )
                #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  0
                  ;;( d e f u n   r e a d - e v a l   ( z   w )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  ;;    # . ( g e n e r a t e - c o d e   ' z   ' w ) )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))

(read-eval 2 3)
;; ... but probably the main thing is that this shouldn't show any
;; read-suppressed states, or ideally any conditional states either.
(assert (equalp (get-states "test-data-read-eval")
                ;;( i n - p a c k a g e   s b - c o v e r - t e s t )
                #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  0
                  ;;( d e f u n   r e a d - e v a l   ( z   w )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  ;;    # . ( g e n e r a t e - c o d e   ' z   ' w ) )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))

(sb-cover:clear-coverage)
(compile-load "test-data-read-eval-cdr")
;; see related comments about "test-data-read-eval"
(assert (equalp (get-states "test-data-read-eval-cdr")
                ;;( i n - p a c k a g e   s b - c o v e r - t e s t )
                #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  0
                  ;;( d e f u n   r e a d - e v a l - c d r   ( z   w )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  ;;    ( p r o g n   .   # . ( l i s t   ( g e n e r a t e - c o d e   ' z   ' w ) ) ) )
                  0 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1)))

(read-eval-cdr 2 3)
(assert (equalp (get-states "test-data-read-eval-cdr")
                ;;( i n - p a c k a g e   s b - c o v e r - t e s t )
                #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  0
                  ;;( d e f u n   r e a d - e v a l - c d r   ( z   w )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  ;;    ( p r o g n   .   # . ( l i s t   ( g e n e r a t e - c o d e   ' z   ' w ) ) ) )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))

(sb-cover:clear-coverage)
(defvar *flag* t)
(compile-load "test-data-sharp-plus-sharp-dot")
(assert (equalp (get-states "test-data-sharp-plus-sharp-dot")
                ;;( i n - p a c k a g e   s b - c o v e r - t e s t )
                #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  0
                  ;;( d e f u n   s h a r p - p l u s - s h a r p - d o t   ( x )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  ;;    # + # . ( c l : i f   s b - c o v e r - t e s t : : * f l a g *   ' ( a n d )   ' ( o r ) )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  ;;    ( 1 +   x )
                  0 1 1 2 2 2 2 2 2
                  ;;    # - # . ( c l : i f   s b - c o v e r - t e s t : : * f l a g *   ' ( a n d )   ' ( o r ) )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  ;;    (  1  -     x  )  )
                  0 1 1 15 15 15 15 15 15 1)))

(setf *flag* nil)
(assert (equalp (get-states "test-data-sharp-plus-sharp-dot")
                ;;( i n - p a c k a g e   s b - c o v e r - t e s t )
                #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  0
                  ;;( d e f u n   s h a r p - p l u s - s h a r p - d o t   ( x )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  ;;    # + # . ( c l : i f   s b - c o v e r - t e s t : : * f l a g *   ' ( a n d )   ' ( o r ) )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  ;;    (  1  +     x  )
                  0 1 1 15 15 15 15 15 15
                  ;;    # - # . ( c l : i f   s b - c o v e r - t e s t : : * f l a g *   ' ( a n d )   ' ( o r ) )
                  0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                  ;;    ( 1 -   x ) )
                  0 1 1 2 2 2 2 2 2 1)))
