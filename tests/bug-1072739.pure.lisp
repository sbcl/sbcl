;; win32 is very specific about the order in which catch blocks
;; must be allocated on the stack
;;
;; This test is extremely slow, so it deserves its own file.
(with-test (:name (compile :bug-1072739) :slow t)
  (checked-compile-and-assert (:optimize :safe)
      `(lambda ()
         (STRING=
          (LET ((% 23))
            (WITH-OUTPUT-TO-STRING (G13908)
              (PRINC
               (LET ()
                 (DECLARE (OPTIMIZE (SB-EXT:INHIBIT-WARNINGS 3)))
                 (HANDLER-CASE
                     (WITH-OUTPUT-TO-STRING (G13909) (PRINC %A%B% G13909) G13909)
                   (UNBOUND-VARIABLE NIL
                     (HANDLER-CASE
                         (WITH-OUTPUT-TO-STRING (G13914)
                           (PRINC %A%B% G13914)
                           (PRINC "" G13914)
                           G13914)
                       (UNBOUND-VARIABLE NIL
                         (HANDLER-CASE
                             (WITH-OUTPUT-TO-STRING (G13913)
                               (PRINC %A%B G13913)
                               (PRINC "%" G13913)
                               G13913)
                           (UNBOUND-VARIABLE NIL
                             (HANDLER-CASE
                                 (WITH-OUTPUT-TO-STRING (G13912)
                                   (PRINC %A% G13912)
                                   (PRINC "b%" G13912)
                                   G13912)
                               (UNBOUND-VARIABLE NIL
                                 (HANDLER-CASE
                                     (WITH-OUTPUT-TO-STRING (G13911)
                                       (PRINC %A G13911)
                                       (PRINC "%b%" G13911)
                                       G13911)
                                   (UNBOUND-VARIABLE NIL
                                     (HANDLER-CASE
                                         (WITH-OUTPUT-TO-STRING (G13910)
                                           (PRINC % G13910)
                                           (PRINC "a%b%" G13910)
                                           G13910)
                                       (UNBOUND-VARIABLE NIL
                                         (ERROR "Interpolation error in \"%a%b%\"
"))))))))))))))
               G13908)))
          "23a%b%"))
     (() t)))
