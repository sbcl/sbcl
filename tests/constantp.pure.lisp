(in-package :cl-user)

(with-test (:name :constantp-conservatively-nil)
  (assert (not (constantp '(if))))
  (assert (not (constantp '(if . 1))))
  (assert (not (constantp '(if 1))))
  (assert (not (constantp '(if 1 . 2))))
  (assert (not (constantp '(if 1 2 . 3))))
  (assert (not (constantp '(if 1 2 3 4)))))

(with-test (:name (:bogus-block constantp))
  (assert
   (nth-value 1
              (checked-compile `(lambda (&optional (x (block 1 10))) x)
                               :allow-failure t)))
  (assert (not (constantp '(block 1 10)))))

(with-test (:name :progv)
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
   (not (constantp '(progv '(sb-c::**world-lock**) '(10) 10)))))

(with-test (:name :the)
  (assert
   (not (constantp '(the (satisfies eval) 10))))
  (assert
   (not (constantp '(the (array abc) #()))))
  (assert
   (not (constantp '(the (cons (satisfies error)) '("a"))))))

(with-test (:name :bad-macros)
  (assert
   (nth-value 1
              (checked-compile
              `(lambda () (coerce 'integer (restart-bind foo)))
               :allow-failure t))))
