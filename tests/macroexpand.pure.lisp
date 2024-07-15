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

;;; An evaluated macroexpand-hook leads to infinite recursion.
;;; These tests used to be runnable only if *evaluator-mode* started out
;;; as :compile, but now we support running the test suite with any
;;; *evaluator-mode*, so must explicitly COMPILE the macroexpand hook.
;;; Notice that the lambda expressions being compiled are closures.
;;; This is allowed by sb-interpreter. sb-eval gets an error
;;; "Unhandled INTERPRETER-ENVIRONMENT-TOO-COMPLEX-ERROR:
;;;  Lexical environment of #<INTERPRETED-FUNCTION NIL {1001850EBB}>
;;   is too complex to compile."

;;; Like CHECKED-COMPILE, this disallows unexpected warnings.
;;; But unlike CHECKED-COMPILE, it allows the argument to be a function.
(defun compilefun (fun)
  (multiple-value-bind (result warnp errorp)
      (compile nil fun)
    (assert (not warnp))
    (assert (not errorp))
    result))

(let* ((expanded-p nil)
       (*macroexpand-hook*
        (compilefun  #'(lambda (fn form env)
                         (when (eq form '.foo.)
                           (setq expanded-p t))
                         (funcall fn form env)))))
  (multiple-value-bind (expansion flag) (macroexpand '.foo.)
    (assert (equal expansion '(quote foobar)))
    (assert flag)
    (assert expanded-p)))

#+(or sb-eval sb-fasteval)
(let ((sb-ext:*evaluator-mode* :interpret))
  (let* ((expanded-p nil)
         (*macroexpand-hook*
          (compilefun  #'(lambda (fn form env)
                           (when (eq form '.foo.)
                             (setq expanded-p t))
                           (funcall fn form env)))))
    (eval '.foo.)
    (assert expanded-p)))

(let* ((expanded-p nil)
       (*macroexpand-hook*
        (compilefun  #'(lambda (fn form env)
                         (when (eq form '/foo/)
                           (setq expanded-p t))
                         (funcall fn form env)))))
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(sb-int:&more sb-int:parse-lambda-list)))

(with-test (:name :parse-lambda-list)
  ;; 3.4.1 - ordinary lambda list
  (assert-error (parse-lambda-list '(foo &body bar)))
  (assert-error (parse-lambda-list '(foo &whole bar)))
  (assert-error (parse-lambda-list '(foo &environment bar)))
  ;; &more expects exactly two following symbols
  (assert-error (parse-lambda-list '(foo &more)))
  (assert-error (parse-lambda-list '(foo &more c)))
  (assert-error (parse-lambda-list '(foo &more ctxt ct junk)))
  ;; &more and &rest are mutually exclusive
  (assert-error (parse-lambda-list '(foo &rest foo &more ctxt n)))
  (assert-error (parse-lambda-list '(foo &more ctxt n &rest foo)))

  ;; 3.4.2 - generic function lambda lists
  (macroexpand-1 '(defgeneric foo (a b &key size &allow-other-keys)))
  (assert-error (macroexpand-1 '(defgeneric foo (a b &aux x)))
                sb-pcl::generic-function-lambda-list-error)
  ;; 3.4.3 - FIXME: add tests

  ;; 3.4.4 - doesn't use PARSE-LAMBDA-LIST yet
  ;; 3.4.5 - same

  ;; 3.4.6 - BOA lambda list is a function lambda list,
  ;; but the expander silently disregarded the internal &MORE keyword,
  ;; which has no place in DEFSTRUCT.
  (assert-error
   (macroexpand-1 '(defstruct (s (:constructor
                                  make-s (a b &more ctxt n)))
                    a b ctxt n)))

  ;; 3.4.7 - DEFSETF disallows &AUX
  (assert-error (macroexpand-1
                 '(defsetf foof (a b &optional k &aux) (v1 v2) (forms))))

  ;; 3.4.8 - DEFTYPE is exactly like DEFMACRO
  ;;         except for the implied default-default of '*

  ;; 3.4.9 - DEFINE-MODIFY-MACRO allows only &OPTIONAL and &REST
  (assert-error (macroexpand-1
                 '(define-modify-macro foof (a b &optional k &key) foo)))
  (assert-error (macroexpand-1
                 '(define-modify-macro foof (a b &optional k &body) foo)))

  ;; 3.4.10 - DEFINE-METHOD-COMBINATION. Not even sure what this does.

  )

(defstruct foo (a 0 :type fixnum))
(defstruct bar (a 0 :type fixnum))
(declaim (notinline (setf bar-a)))
;; This macro definition is technically violating the dynamic-extent
;; nature of environment objects (as per X3J13), but of course ours don't.
(defmacro capture-env (&environment e &rest r)
  (declare (ignore r))
  e)
(with-test (:name :macroexpand-setf-instance-ref.1)
  (assert (equal-mod-gensyms
           (macroexpand-1 '(setf (foo-a x) 3))
           `(let ((#1=instance (the foo x))
                  (#2=val (sb-kernel:the* (fixnum :context (sb-kernel::struct-context foo . a)) 3)))
              (sb-kernel:%instance-set #1# #.sb-vm:instance-data-start #2#)
              #2#))))
(with-test (:name :macroexpand-setf-instance-ref.2)
  ;; Lexical definition of (SETF FOO-A) inhibits source-transform.
  ;; This is not required behavior - SETF of structure slots
  ;; do not necessarily go through a function named (SETF your-slot),
  ;; but it's this implementation's behavior, so should be asserted.
  (flet (((setf foo-a) (new obj) (declare (ignore obj)) new))
    (assert (equal-mod-gensyms
             (macroexpand-1 '(setf (foo-a x) 3) (capture-env))
             '(let* ((#1=#:x x) (new 3)) (funcall #'(setf foo-a) new #1#)))))

  ;; Same, not required behavior - NOTINLINE inhibits transform.
  (assert (equal-mod-gensyms
           (macroexpand-1 '(setf (bar-a x) 3))
           '(let* ((#2=#:x x) (new 3)) (funcall #'(setf bar-a) new #2#)))))

;;; WITH-CURRENT-SOURCE-FORM tests

(defmacro warnings-in-subforms (a b)
  (with-current-source-form (a)
    (warn "a warning"))
  (with-current-source-form (b)
    (warn "a warning"))
  `(progn ,a ,b))

(with-test (:name (with-current-source-form :smoke))
  (assert (equal (checked-compile-condition-source-paths
                  '(lambda () (warnings-in-subforms 1 2)))
                 '((2 0) (2 0))))
  (assert (equal (checked-compile-condition-source-paths
                  '(lambda () (warnings-in-subforms (progn 1) (progn 2))))
                 '((1 2 0) (2 2 0))))
  (assert (equal (checked-compile-condition-source-paths
                  '(lambda ()
                    (warnings-in-subforms
                     (warnings-in-subforms (progn 1) (progn 2))
                     (progn 3))))
                 '((1 2 0) (2 2 0) (1 1 2 0) (2 1 2 0)))))

(with-test (:name :symbol-case-clause-ordering)
  (let ((f (checked-compile
            '(lambda (x) (case x ((a z) 1) ((y b w) 2) ((b c) 3)))
            :allow-style-warnings t)))
    (assert (eql (funcall f 'b) 2))))

(deftype zook () '(member :a :b :c))
;; TYPECASE should become CASE when it can, even if the resulting CASE
;; will not expand using symbol-hash.
(with-test (:name :typecase-to-case)
  ;; TYPECASE without a final T clause
  (assert (equal (macroexpand-1 '(typecase x ((eql z) 1) ((member 2 3) hi) (zook :z)))
                 '(case x ((z) 1) ((2 3) hi) ((:a :b :c) :z))))
  ;; with final T
  (assert (equal (macroexpand-1 '(typecase x ((eql z) 1) ((member 2 3) hi) (zook :z) (t 'def)))
                 '(case x ((z) 1) ((2 3) hi) ((:a :b :c) :z) (t 'def))))
  ;; with final OTHERWISE
  (assert (equal (macroexpand-1 '(typecase x
                                  ((eql z) 1) ((member 2 3) hi) (zook :z) (otherwise 'def)))
                 '(case x ((z) 1) ((2 3) hi) ((:a :b :c) :z) (t 'def))))

  ;; ETYPECASE without final T
  (assert (equal (macroexpand-1 '(etypecase x ((eql z) 1) ((member 2 3) hi) (zook :z)))
                 '(ecase x ((z) 1) ((2 3) hi) ((:a :b :c) :z))))
  ;; and with
  (assert (equal (macroexpand-1 '(etypecase x ((eql z) 1) ((member 2 3) hi) (zook :z) (t 'def)))
                 '(case x ((z) 1) ((2 3) hi) ((:a :b :c) :z) (t 'def)))))

(with-test (:name :cypecase-never-err)
  (assert (eq (let ((x 1)) (ctypecase x (t 'a))) 'a)))

(with-test (:name :typecase-t-shadows-rest)
  (assert-signal (macroexpand-1 '(typecase x (atom 1) (t 2) (cons 3))) warning))

(with-test (:name :symbol-case-default-form)
  (let ((f (checked-compile
            '(lambda (x)
              (case x ((a b c) 1) ((d e f) 2) (t #*10101))))))
    (assert (equal (funcall f 30) #*10101))))

(with-test (:name :memq-as-case)
  (let* ((f (checked-compile
             '(lambda (x)
               (if (sb-int:memq x '(a b c d e f g h i j k l m n o p)) 1 2))))
         (code (sb-kernel:fun-code-header f))
         (constant
          (sb-kernel:code-header-ref code sb-vm:code-constants-offset)))
    ;; should have a vector of symbols, not references to each symbol
    (assert (vectorp constant))
    (assert (eql (funcall f 'j) 1))
    (assert (eql (funcall f 42) 2)))

  (let* ((f (checked-compile
             '(lambda (x)
               (or (member x '(a b c d e f g h i j k nil t l m n o p) :test 'eq)
                   -1))))
         (code (sb-kernel:fun-code-header f))
         (constant1
           (sb-kernel:code-header-ref code sb-vm:code-constants-offset))
         (constant2
           (sb-kernel:code-header-ref code (1+ sb-vm:code-constants-offset))))
    ;; These accesses are safe because if the transform happened,
    ;; there should be 2 constants, and if it didn't, then at least 2 constants.
    (assert (and (vectorp constant1) (vectorp constant2)))
    (assert (equal (funcall f 'o) '(o p)))
    (assert (eql (funcall f 42) -1))))

(defmacro macro-with-dotted-list (&rest args)
  args)
(with-test (:name :macro-with-dotted-list)
  (let ((expansion (macroexpand '(macro-with-dotted-list . 1))))
    (assert (equal expansion 1))))

(with-test (:name :typecase)
  (declare (muffle-conditions style-warning))
  (assert
   (equal (loop for x in '(a 1 1.4 "c")
                collect (typecase x
                          (t :good)
                          (otherwise :bad)))
          '(:good :good :good :good))))

(with-test (:name :typecase-nonfinal-otherwise-errs)
  (assert-error
   (macroexpand-1 '(typecase x (cons 1) (otherwise 2) (t 3)))))

(with-test (:name :dolist-type-decls-better)
  (checked-compile
   '(lambda (input &aux (r 0))
     (dolist (x input (- r)) ; no conflict when X = NIL
       (declare (string x))
       (incf r (length x))))))
