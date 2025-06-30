
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

;;; Check that ENCAPSULATE on an undefined functions works, and that
;;; UNENCAPSULATE can make it undefined.  The use-case it to allow test-related
;;; mocks to be agnostic of whether a required function in two loosely coupled
;;; modules exists. e.g. module A calls into module B, and A expects that B exposes
;;; a certain global function F. You would like to unit-test A and B separately
;;; and together.  To test together, can mock (via encapsulation) F to observe
;;; inputs and outputs of the interface. To test A alone, you can similarly mock F
;;; despite that it had no global definition, as B is not loaded.
;;; This feature of encapsulate is strictly more useful than just erring when a
;;; function to be encapsulated has no global definition.
(with-test (:name :encapsulate-undef)
  (sb-int:encapsulate 'this-function-is-not-defined 'testing
    (lambda (realfun) (declare (ignore realfun)) 'ok))
  (assert (eq (funcall 'this-function-is-not-defined) 'ok))
  (sb-int:unencapsulate 'this-function-is-not-defined 'testing)
  (assert (not (fboundp 'this-function-is-not-defined)))
  ;; furthermore, it works to create two encapsulations on the
  ;; undefined function, and regardless of which order the
  ;; encapsulations are stripped off, the function becomes undefined.
  (assert (not (fboundp 'beeboh)))
  (sb-int:encapsulate 'beeboh 'inner (lambda (realfun) realfun 'this-is-inner))
  (sb-int:encapsulate 'beeboh 'outer (lambda (realfun) realfun 'this-is-outer))
  (sb-int:unencapsulate 'beeboh 'outer)
  (eq (funcall 'beeboh) 'this-is-inner)
  (sb-int:unencapsulate 'beeboh 'inner)
  (assert (not (fboundp 'beeboh)))
  (sb-int:encapsulate 'beeboh 'inner (lambda (realfun) realfun 'this-is-inner))
  (sb-int:encapsulate 'beeboh 'outer (lambda (realfun) realfun 'this-is-outer))
  (sb-int:unencapsulate 'beeboh 'inner)
  (eq (funcall 'beeboh) 'this-is-outer)
  (sb-int:unencapsulate 'beeboh 'outer)
  (assert (not (fboundp 'beeboh))))
