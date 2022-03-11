(with-test (:name (constantp :conservatively nil))
  (assert (not (constantp '(if))))
  (assert (not (constantp '(if . 1))))
  (assert (not (constantp '(if 1))))
  (assert (not (constantp '(if 1 . 2))))
  (assert (not (constantp '(if 1 2 . 3))))
  (assert (not (constantp '(if 1 2 3 4)))))

(with-test (:name (constantp :bogus-block))
  (assert
   (nth-value 1
              (checked-compile `(lambda (&optional (x (block 1 10))) x)
                               :allow-failure t)))
  (assert (not (constantp '(block 1 10)))))

(sb-ext:defglobal *some-global-var* nil)
(with-test (:name (constantp progv))
  (assert
   (not (constantp '(progv '(*s*) nil *s*))))
  (assert
   (not (constantp '(progv 10 '(10) 10))))
  (assert
   (not (constantp '(progv '(10) 10 10))))
  (assert
   (not (constantp '(progv '(10) '(10) 10))))
  (assert
   (not (constantp '(progv '(10 . 20) '(10) 10))))
  (assert
   (not (constantp '(progv '(10) '(10 . 30) 10))))
  (assert
   (not (constantp '(progv '(/) '(10) /))))
  (assert
   (not (constantp '(progv '(pi) '(10) 10))))
  (assert
   (not (constantp '(progv '(*some-global-var*) '(10) 10)))))

(declaim (muffle-conditions style-warning)) ; unknown type ABC
(with-test (:name (constantp the))
  (assert
   (not (constantp '(the (satisfies eval) 10))))
  (assert
   (not (constantp '(the (array abc) #()))))
  (assert
   (not (constantp '(the (cons (satisfies error)) '("a"))))))
