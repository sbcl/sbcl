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

;; Check that we don't modify constants
(defconstant +a-constant+ 42)
(assert
 (eq :error
     (handler-case
         (sb-ext:compare-and-swap (symbol-value '+a-constant+) 42 13)
       (error () :error))))
(let ((name '+a-constant+))
  (assert
   (eq :error
       (handler-case
           (sb-ext:compare-and-swap (symbol-value name) 42 13)
         (error () :error)))))

;; Check that we don't mess declaimed types
(declaim (boolean *a-boolean*))
(defparameter *a-boolean* t)
(assert
 (eq :error
     (handler-case
         (sb-ext:compare-and-swap (symbol-value '*a-boolean*) t 42)
       (error () :error))))
(let ((name '*a-boolean*))
  (assert
   (eq :error
       (handler-case
           (sb-ext:compare-and-swap (symbol-value name) t 42)
         (error () :error)))))

;;;; ATOMIC-INCF and ATOMIC-DECF (we should probably rename this file atomic-ops...)

(defstruct box
  (word 0 :type sb-vm:word))

(defun inc-box (box n)
  (declare (fixnum n) (box box))
  (loop repeat n
        do (sb-ext:atomic-incf (box-word box))))

(defun dec-box (box n)
  (declare (fixnum n) (box box))
  (loop repeat n
        do (sb-ext:atomic-decf (box-word box))))

(let ((box (make-box)))
  (inc-box box 10000)
  (assert (= 10000 (box-word box)))
  (dec-box box 10000)
  (assert (= 0 (box-word box))))

(with-test (:name :atomic-incf-wraparound)
  (let ((box (make-box :word (1- (ash 1 sb-vm:n-word-bits)))))
    (sb-ext:atomic-incf (box-word box) 2)
    (assert (= 1 (box-word box)))))

(with-test (:name :atomic-decf-wraparound)
  (let ((box (make-box :word 0)))
    (sb-ext:atomic-decf (box-word box) 2)
    (assert (= (- (ash 1 sb-vm:n-word-bits) 2) (box-word box)))))

#+sb-thread
(let* ((box (make-box))
       (threads (loop repeat 64
                      collect (sb-thread:make-thread (lambda ()
                                                       (inc-box box 1000)
                                                       (dec-box box 10000)
                                                       (inc-box box 10000)
                                                       (dec-box box 1000))
                                                     :name "inc/dec thread"))))
  (mapc #'sb-thread:join-thread threads)
  (assert (= 0 (box-word box))))
