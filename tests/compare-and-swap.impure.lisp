;;; Basics

(defstruct xxx yyy)

(macrolet ((test (init op)
             `(let ((x ,init)
                    (y (list 'foo))
                    (z (list 'bar)))
                (assert (eql nil (compare-and-swap (,op x) nil y)))
                (assert (eql y (compare-and-swap (,op x) nil z)))
                (assert (eql y (,op x)))
                (let ((x "foo"))
                  (multiple-value-bind (res err)
                     (ignore-errors (compare-and-swap (,op x) nil nil))
                    (assert (not res))
                    (assert (typep err 'type-error)))))))
  (test (cons nil :no) car)
  (test (cons nil :no) first)
  (test (cons :no nil) cdr)
  (test (cons :no nil) rest)
  (test '.foo. symbol-plist)
  (test (progn (set '.bar. nil) '.bar.) symbol-value)
  (test (make-xxx) xxx-yyy))

(defvar *foo*)

;;; thread-local bindings

(let ((*foo* 42))
  (let ((*foo* nil))
    (assert (eql nil (compare-and-swap (symbol-value '*foo*) nil t)))
    (assert (eql t (compare-and-swap (symbol-value '*foo*) nil :foo)))
    (assert (eql t *foo*)))
  (assert (eql 42 *foo*)))

;;; unbound symbols + symbol-value

(assert (not (boundp '*foo*)))

(multiple-value-bind (res err)
    (ignore-errors (compare-and-swap (symbol-value '*foo*) nil t))
  (assert (not res))
  (assert (typep err 'unbound-variable)))

(defvar *bar* t)

(let ((*bar* nil))
   (makunbound '*bar*)
   (multiple-value-bind (res err)
       (ignore-errors (compare-and-swap (symbol-value '*bar*) nil t))
     (assert (not res))
     (assert (typep err 'unbound-variable))))

;;; SVREF

(defvar *v* (vector 1))

;; basics
(assert (eql 1 (compare-and-swap (svref *v* 0) 1 2)))
(assert (eql 2 (compare-and-swap (svref *v* 0) 1 3)))
(assert (eql 2 (svref *v* 0)))

;; bounds
(multiple-value-bind (res err)
    (ignore-errors (compare-and-swap (svref *v* -1) 1 2))
  (assert (not res))
  (assert (typep err 'type-error)))
(multiple-value-bind (res err)
    (ignore-errors (compare-and-swap (svref *v* 1) 1 2))
  (assert (not res))
  (assert (typep err 'type-error)))

;; type of the first argument
(multiple-value-bind (res err)
    (ignore-errors (compare-and-swap (svref "foo" 1) 1 2))
    (assert (not res))
    (assert (typep err 'type-error)))
