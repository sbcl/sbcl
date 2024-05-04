
(defun tryit (fname)
  (sb-int:encapsulate
   fname
   'foomfa
   (lambda (realfun &rest args)
     (apply realfun args)))
  (assert (sb-int:encapsulated-p fname 'foomfa))
  (assert (not (sb-int:encapsulated-p fname 'nope)))
  (sb-int:unencapsulate fname 'no-such-encapsulation)
  (assert (sb-int:encapsulated-p fname 'foomfa))
  (sb-int:unencapsulate fname 'foomfa)
  (assert (not (sb-int:encapsulated-p fname 'foomfa))))

(with-test (:name :encapsulated-p-simple-fun)
  (tryit 'ed))

(defgeneric zerk (arg))
(with-test (:name :encapsulated-p-gf)
  (tryit 'zerk))

(with-test (:name :encapsulated-p-closure)
  (setf (symbol-function 'fleem)
        (locally (declare (notinline constantly))
          (constantly 'gazonk)))
  (assert (sb-kernel:closurep (symbol-function 'fleem)))
  (tryit 'fleem))

(with-test (:name :encapsulation-type-is-symbol)
  (assert-error (sb-int:encapsulate 'e "foo" #'cons)))

(defun install-testfun ()
  (fmakunbound 'thing)
  (defun thing (x) (list :hi x))
  (dolist (suffix '(d c b a)) ; install so that the list comes out A B C D
    (let ((kwd (sb-int:keywordicate suffix)))
      (sb-int:encapsulate 'thing (sb-int:symbolicate "ENCAP." suffix)
       (lambda (realfun arg) (cons kwd (funcall realfun arg)))))))

(install-testfun)

(with-test (:name :delete-encap)
  (let ((expect '(:a :b :c :d :hi :x)))
    (assert (equal (thing :x) expect))
    (dolist (sym '(:a :b :c :d))
      (sb-int:unencapsulate 'thing (sb-int:symbolicate "ENCAP." sym))
      (assert (equal (thing :x) (remove sym expect)))
      (install-testfun)))) ; restore it

(with-test (:name :change-encap)
  (let ((expect '(:a :b :c :d :hi :x)))
    (dolist (sym '(:a :b :c :d))
      (install-testfun)
      ;; alter it
      (sb-int:encapsulate 'thing (sb-int:symbolicate "ENCAP." sym)
       (lambda (realfun arg) (cons (string sym) (funcall realfun arg))))
      (assert (equal (thing :x) (subst (string sym) sym expect)))))
  ;; keeping the replaced fourth encapsulation, also replace the first
  (sb-int:encapsulate 'thing 'encap.a
   (lambda (realfun arg) (cons :new-a (funcall realfun arg))))
  (assert (equal (thing :x) '(:new-a :b :c "D" :hi :x))))
