;;;; tests related to setf

;;;; This file is impure because we want to be able to use DEFUN.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(in-package :cl-user)

(defvar *foo* nil)
(defun (setf foo) (bar)
    (setf *foo* bar))

;;; Regression test for get-setf-expansion without explicit
;;; environment object.
(assert (multiple-value-list (get-setf-expansion '(foo))))

;;; Regression test for SHIFTF of values.
(let ((x (list 1))
      (y (list 2)))
  (shiftf (values (car x) (car y)) (values (car y) (car x)))
  (assert (equal (list x y) '((2) (1)))))

;;; SETF of values with multiple-value place forms
(let ((a t) (b t) (c t) (d t))
  (let ((list (multiple-value-list
               (setf (values (values a b) (values c d)) (values 1 2 3 4)))))
    (assert (equal list '(1 2)))
    (assert (eql a 1))
    (assert (eql c 2))
    (assert (null b))
    (assert (null d))))

;;; SETF of THE with VALUES.
(let (x y)
  (setf (the (values fixnum fixnum) (values x y))
        (values 1 2))
  (assert (= x 1))
  (assert (= y 2)))

;;; SETF of MACRO-FUNCTION must accept a NIL environment
(let ((fun (constantly 'ok)))
  (setf (macro-function 'nothing-at-all nil) fun)
  (assert (eq fun (macro-function 'nothing-at-all nil))))


;;; DEFSETF accepts &ENVIRONMENT but not &AUX
(defsetf test-defsetf-env-1  (&environment env) (new)
  (declare (ignore new))
  (if (macro-function 'defsetf-env-trick env)
      :local
      :global))

(defsetf test-defsetf-env-2  (local global &environment env) (new)
  (declare (ignore new))
  (if (macro-function 'defsetf-env-trick env)
      local
      global))

(assert (eq :local (macrolet ((defsetf-env-trick ()))
                     (setf (test-defsetf-env-1) 13))))

(assert (eq :global (setf (test-defsetf-env-1) 13)))

(assert (eq :local (macrolet ((defsetf-env-trick ()))
                     (setf (test-defsetf-env-2 :local :oops) 13))))

(assert (eq :global (setf (test-defsetf-env-2 :oops :global) 13)))

(assert (eq :error
            (handler-case
                (eval '(defsetf test-defsetf-aux (&aux aux) (new) nil))
              (error ()
                :error))))

(handler-bind ((style-warning #'error))
  (compile nil '(lambda ()
                 (defsetf test-defsetf-no-env (foo) (new)
                   `(set-foo ,foo ,new))))
  (compile nil '(lambda ()
                 (defsetf test-defsetf-ignore-env (foo &environment env) (new)
                   (declare (ignore env))
                   `(set-foo ,foo ,new)))))

;;; Not required by the spec, but allowes compiler-macros for SETF-functiosn
;;; to see their constant argument forms.
(with-test (:name :constantp-aware-get-setf-expansion)
  (multiple-value-bind (temps values stores set get)
      (get-setf-expansion '(foo 1 2 3))
    (assert (not temps))
    (assert (not values))
    (assert (equal `(funcall #'(setf foo) ,@stores 1 2 3) set))
    (assert (equal '(foo 1 2 3) get))))

(with-test (:name :update-fn-should-be-a-symbol-in-defsetf)
  (assert (eq :error
            (handler-case
                (eval '(defsetf access-fn 5))
              (error ()
                :error)))))

(with-test (:name :getf-unused-default-variable)
  (handler-bind ((style-warning #'error))
    (compile nil `(lambda (x y)
                    (setf (gethash :x x 0) 4)
                    (setf (getf y :y 0) 4)
                    (setf (get 'z :z 0) 4)))))

(with-test (:name :setf-fun-and-macro-full-warn)
  (multiple-value-bind (fun warnings-p failure-p)
      (compile nil '(lambda (x) (setf (shoe-color x) 'cordovan)))
    (assert (and fun warnings-p (not failure-p))))
  (assert (typep (handler-case (eval '(defsetf shoe-color set-shoe-color))
                   (warning (c) c))
                 '(and warning (not style-warning)))))

(with-test (:name :setf-fun-and-macro-style-1)
  (eval '(defun (setf shoe-size) (new x) x new))
  (assert (typep (handler-case (eval '(defsetf shoe-size set-shoe-size))
                   (warning (c) c))
                 'style-warning)))

;; This is a test of the compiler, but it belongs with the above.
(defvar *tmpfile* "setf-tmp.lisp")
(with-test (:name :setf-fun-and-macro-style-2)
  (unwind-protect
       (progn
         ;; verify the test's precondition, for sanity
         (assert (not (fboundp '(setf shoe-count))))
         (with-open-file (f *tmpfile* :direction :output
                                      :if-exists :supersede)
           (prin1 '(defun (setf shoe-count) (new x) (print x) new) f)
           (prin1 '(defsetf shoe-count set-shoe-count) f))
         ;; Expect a warning because the compiler knows about
         ;; (SETF SHOE-COUNT), which isn't yet FBOUNDP,
         ;; and then we also compile a SETF inverse.
         (multiple-value-bind (output warnings-p failure-p)
             (compile-file *tmpfile*)
           (ignore-errors (delete-file output))
           (assert (and output warnings-p (not failure-p)))))
      (ignore-errors (delete-file *tmpfile*))))

;; Make sure that the second values of INFO :SETF :EXPANDER/:INVERSE
;; are not both T.  Each of :EXPANDER and :INVERSE set the other one
;; to NIL but the WINP return value from INFO was still T so could not
;; reliably be used to test existence or non-existence.
(defsetf foo1 set-foo1)
(define-setf-expander foo1 (a b) (declare (ignore a b)))

(define-setf-expander foo2 (a b) (declare (ignore a b)))
(defsetf foo2 set-foo2)

(with-test (:name :setf-inverse-clears-expander-and-vice-versa)
  (multiple-value-bind (val winp) (sb-int:info :setf :inverse 'foo1)
    (assert (and (not val) (not winp))))
  (multiple-value-bind (val winp) (sb-int:info :setf :expander 'foo2)
    (assert (and (not val) (not winp)))))

;; The expander for (CADAR x) should behave as (CAR (CDAR x)) etc.
;; This mainly affects read/modify/write semantics.
(with-test (:name :car+cdr-compositions-lp1450968)
  (flet ((maketree (n &aux (count -1))
           (labels ((recurse (n)
                      (if (zerop n)
                          (incf count)
                          (cons (recurse (1- n)) (recurse (1- n))))))
             (recurse n))))
    (loop
     for n-ops from 2 to 4
     do (dotimes (bitmask (ash 1 n-ops))
          (let* ((ops (coerce (loop for i below n-ops
                                    collect (if (logbitp i bitmask) #\D #\A))
                              'string))
                 (accessor (sb-int:symbolicate "C" ops "R"))
                 (tree (maketree n-ops))
                 (left (car tree))
                 (right (cdr tree)))
            (assert (eql (funcall accessor tree) bitmask))
            (let ((f (compile nil
                              `(lambda (obj)
                                 (incf (,accessor obj)
                                       (progn (rplaca obj nil)
                                              (rplacd obj nil)
                                              1000))))))
              (funcall f tree)
              (let ((tree* (cons left right)))
                (assert (eql (funcall accessor tree*)
                             (+ bitmask 1000))))))))))

(define-symbol-macro %foofy1% (values a b c))
(define-symbol-macro %foofy2% (values x y z))
;; PSETF and PSETQ eliminate vacuous LET* forms.
(with-test (:name :psetf-expansion-maximally-concise)
  (dolist (op '(psetq psetf))
    (let* ((form `(,op %foofy1% (f) %foofy2% (g)))
           (expansion (let ((*gensym-counter* 1)) (macroexpand-1 form)))
           (expect '(multiple-value-bind (new1 new2 new3) (f)
                     (multiple-value-bind (new4 new5 new6) (g)
                       (setq a new1) (setq b new2) (setq c new3)
                       (setq x new4) (setq y new5) (setq z new6)
                       nil))))
      (assert (equal (read-from-string (write-to-string expansion :gensym nil))
                     expect)))))

(with-test (:name :defsetf-syntax-errors)
  (dolist (test '((defsetf foo set-foo junk other-junk) ; would accept
                  (defsetf foo set-foo . junk))) ; would crash
    (assert (search "Ill-formed DEFSETF"
                    (simple-condition-format-control
                     (nth-value 1 (ignore-errors (macroexpand-1 test)))))))
  ;; no (SETF (SETF f)) methods
  (assert-error (macroexpand-1 '(defsetf (setf foo) set-setf-foo))))

(defmacro mymacro () '*x*)
(define-symbol-macro foox (car *x*))
(with-test (:name :setf-of-symbol-macro)
  (assert (equal (macroexpand-1 '(setf foox 3)) '(sb-kernel:%rplaca *x* 3))))
(with-test (:name :setf-of-macro)
  (assert (equal (macroexpand-1 '(setf (mymacro) 3)) '(setq *x* 3))))

(defvar *x* (list 1))
(defun set-foox (x)
  (declare (type (integer 1 20) foox))
  (setq foox x))
(with-test (:name :setf-of-symbol-macro-typecheck)
  ;; this was not broken, but since I've deleted the comment
  ;;  "FIXME: [Free] type declaration. -- APD, 2002-01-26"
  ;; from ir1-translators, it's probably worth a test
  ;; since at some point it must not have worked as intended.
  (assert-error (set-foox 99)))

(declaim (special *foo-array*))
;; When dealing with symbol-macros, compiled SETQ would locate the leaf
;; for the symbol and then covertly stuff in the expansion to a SETF form.
;; *MACROEXPAND-HOOK* would see the SETF but not the expansion
;; of the symbol, except those expansions occurring with GET-SETF-EXPANSION.
;; Now it can see the first-round expansion too.
(with-test (:name :compiled-setq-macroexpand-hook)
  (sb-int:collect ((expansions))
    (let ((*macroexpand-hook*
           (lambda (expander form env)
             (let ((new (funcall expander form env)))
               (when (or (symbolp form) (eq (car form) 'setf))
                 (expansions (list form new)))
               new))))
      (compile nil '(lambda (x)
                     (symbol-macrolet ((ref-it (aref a 0))
                                       (a *foo-array*)
                                       (thing ref-it))
                       (setq thing x)))))
    (let ((readback (read-from-string
                     (write-to-string (expansions) :gensym nil))))
      (assert (equal readback
                     '((thing ref-it)
                       (ref-it (aref a 0))
                       (a *foo-array*)
                       ((setf thing x)
                        (let* ((a1 a))
                          (multiple-value-bind (new0) x
                            (funcall #'(setf aref) new0 a1 0))))))))))

(with-test (:name :incf-argument-eval-order)
  (let ((testvar 1))
    (flet ((double-it () (setq testvar (* 2 testvar))))
      (incf testvar (double-it)))
    ;; testvar should be 4, not 3, because the read for INCF
    ;; occurs after doubling.
    (assert (eql testvar 4))))

;;; success
