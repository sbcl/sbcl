;;;; This file is for macroexpander tests which have side effects

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

;;; From Matthew Swank on cll 2005-10-06

(defmacro defglobal* (name &optional value)
  (let ((internal (gensym)))
    `(progn
       (defparameter ,internal ,value)
       (define-symbol-macro ,name ,internal))))

(defglobal* glob)

(assert (= (let ((glob 4)) glob)))
(assert (null glob))
(assert (equal (let ((glob nil)) (setf glob (cons 'foo glob)) glob) '(foo)))
(assert (null glob))
(assert (equal (let ((glob nil)) (push 'foo glob) glob) '(foo)))
(assert (null glob))



;;; CLHS 3.1.2.1.1 specifies that symbol macro expansion must also
;;; go through *MACROEXPAND-HOOK*. (2007-09-22, -TCR.)

(define-symbol-macro .foo. 'foobar)

(let* ((expanded-p nil)
      (*macroexpand-hook* #'(lambda (fn form env)
                              (when (eq form '.foo.)
                                (setq expanded-p t))
                              (funcall fn form env))))
  (multiple-value-bind (expansion flag) (macroexpand '.foo.)
    (assert (equal expansion '(quote foobar)))
    (assert flag)
    (assert expanded-p)))

#+sb-eval
(let ((sb-ext::*evaluator-mode* :interpret))
  (let* ((expanded-p nil)
         (*macroexpand-hook* #'(lambda (fn form env)
                                 (when (eq form '.foo.)
                                   (setq expanded-p t))
                                 (funcall fn form env))))
    (eval '.foo.)
    (assert expanded-p)))

(let* ((expanded-p nil)
       (*macroexpand-hook* #'(lambda (fn form env)
                               (when (eq form '/foo/)
                                 (setq expanded-p t))
                               (funcall fn form env))))
  (compile nil '(lambda ()
                 (symbol-macrolet ((/foo/ 'foobar))
                   (macrolet ((expand (symbol &environment env)
                                (macroexpand symbol env)))
                     (expand /foo/)))))
  (assert expanded-p))

;; Check that DEFINE-SYMBOL-MACRO on a variable whose global :KIND
;; was :ALIEN gets a sane error message instead of ECASE failure.
(sb-alien:define-alien-variable ("posix_argv" foo-argv) (* (* char)))
(handler-case (define-symbol-macro foo-argv (silly))
  (error (e)
    (assert (string= "Symbol FOO-ARGV is already defined as an alien variable."
                     (write-to-string e :escape nil))))
  (:no-error () (error "Expected an error")))

(with-test (:name :binding*-expander)
(assert (equal (macroexpand-1
                '(sb-int:binding* (((foo x bar zz) (f) :exit-if-null)
                                   ((baz y) (g bar)))
                  (declare (integer x foo) (special foo y))
                  (declare (special zz bar l) (real q foo))
                  (thing)))
               '(MULTIPLE-VALUE-BIND (FOO X BAR ZZ) (F)
                 (DECLARE
                  (INTEGER X FOO) (SPECIAL FOO) (SPECIAL ZZ BAR) (REAL FOO))
                 (WHEN FOO (MULTIPLE-VALUE-BIND (BAZ Y) (G BAR)
                             (DECLARE (SPECIAL Y))
                             (DECLARE (SPECIAL L) (REAL Q)) (THING))))))

(assert (equal (macroexpand-1
                '(sb-int:binding* (((x y) (f))
                                   (x (g y x)))
                  (declare (integer x))
                  (foo)))
               '(MULTIPLE-VALUE-BIND (X Y) (F)
                 (LET* ((X (G Y X)))
                   (DECLARE (INTEGER X))
                   (FOO)))))

;; The conversion of a trailing sequence of individual bindings
;; into one LET* failed to remove declarations that were already
;; injected pertinent to ealier bound variables.
(assert (equal-mod-gensyms
         (macroexpand-1
          '(sb-int:binding* (((v1 v2 nil) (foo))
                             (a (f v1))
                             (b (g v2)))
            (declare (special fred) (optimize speed)
                     (optimize (debug 3)))
            (declare (integer v1 v2))
            (body)))
         '(multiple-value-bind (v1 v2 #1=#:g538) (foo)
           (declare (integer v1 v2))
           (declare (ignorable #1#))
           (let* ((a (f v1)) (b (g v2)))
             (declare (special fred) (optimize speed) (optimize (debug 3)))
             (body)))))

;; :EXIT-IF-NULL was inserting declarations into the WHEN expression.
(assert (equal-mod-gensyms
         (macroexpand-1
          '(sb-int:binding* (((a1 a2) (f))
                             (b (g))
                             ((c1 nil c2) (h) :exit-if-null)
                             ((d1 d1) (f))
                             (nil (e) :exit-if-null))
            (declare (special fff c2) (integer d1))
            (declare (fixnum a2)
                     (special *x* *y* c1))
            (declare (cons b) (type integer *y* a1))
            (a-body-form)
            (another-body-form)))
         '(multiple-value-bind (a1 a2) (f)
           (declare (fixnum a2) (type integer a1))
           (let* ((b (g)))
             (declare (cons b))
             (multiple-value-bind (c1 #2=#:dummy-1 c2) (h)
               (declare (special c2) (special c1))
               (declare (ignorable #2#))
               (when c1
                 (multiple-value-bind (d1 d1) (f)
                   (declare (integer d1))
                   (let* ((#3=#:dummy-2 (e)))
                     (declare (ignorable #3#))
                     (declare (special fff))
                     (declare (special *y* *x*))
                     (declare (type integer *y*))
                     (when #3#
                       (a-body-form) (another-body-form))))))))))

) ; end BINDING*-EXPANDER test
