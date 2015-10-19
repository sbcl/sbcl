;;;; lambda-list parsing tests with no side-effects

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

(let ((*macroexpand-hook*
       (compile nil
                (lambda (fun form env)
                  (handler-bind ((error (lambda (c)
                                          (when (eq 'destructuring-bind (car form))
                                            (throw 'd-b-error c)))))
                    (funcall fun form env))))))
  (macrolet ((maybe-funcall (&rest args)
               ;; The interpreters will delay lambda-list checks until
               ;; the lambda is actually called.
               (if (eq sb-ext:*evaluator-mode* :compile)
                   `(progn ,@args)
                   `(funcall ,@args)))
             (error-p (ll)
               `(progn
                  (multiple-value-bind (result error)
                      (ignore-errors (maybe-funcall (eval `(lambda ,',ll 'ok))))
                    (unless (and (not result) error)
                      (error "No error from lambda ~S." ',ll)))
                  (catch 'd-b-error
                    (maybe-funcall
                     (eval `(lambda (x) (destructuring-bind ,',ll x 'ok)))
                     nil)
                    (error "No error from d-b ~S." ',ll)))))
    (error-p (&aux (foo 1) &aux (bar 2)))
    (error-p (&aux (foo 1) &key bar))
    (error-p (&aux (foo 1) &optional bar))
    (error-p (&aux (foo 1) &rest bar))
    (error-p (&key foo &allow-other-keys &allow-other-keys))
    (error-p (&key foo &key bar))
    (error-p (&key foo &optional bar))
    (error-p (&key foo &rest bar))
    (error-p (&optional foo &optional bar))
    (error-p (&rest foo &rest bar))
    (error-p (&rest foo &optional bar))))

(with-test (:name :supplied-p-order)
  (let ((* 10))
    (assert (eql ((lambda (&key (x * *)) () x)) 10))
    (assert (eql ((lambda (&key (y * *) (x *)) () x) :y 1) t))
    (assert (eql ((lambda (&key (x *) (y * *)) () x) :y 1) 10))

    (assert (eql (destructuring-bind (&key (x * *)) () x) 10))
    (assert (eql (destructuring-bind (&key (y * *) (x *)) '(:y 1) x) t))
    (assert (eql (destructuring-bind (&key (x *) (y * *)) '(:y 1) x) 10))

    (assert (eql ((lambda (&optional (x * *)) () x)) 10))
    (assert (eql ((lambda (&optional (y * *) (x *)) () x) 1) t))
    (assert (eql ((lambda (&optional (x *) (y * *)) () x)) 10))

    (assert (eql (destructuring-bind (&optional (x * *)) () x) 10))
    (assert (eql (destructuring-bind (&optional (y * *) (x *)) '(1) x) t))
    (assert (eql (destructuring-bind (&optional (x *) (y * *)) () x) 10))))

(with-test (:name :supplied-p-order)
  (assert-no-signal
   (compile nil '(lambda ()
                  (destructuring-bind (&optional (x nil xp)) '()
                    (declare (ignore x xp))
                    nil)))
   warning))

(with-test (:name :aux-not-destructured)
  (assert-error (sb-c::parse-lambda-list
                 '(a &aux ((foo)))
                 :context 'destructuring-bind
                 :accept  (sb-int:lambda-list-keyword-mask 'destructuring-bind))))

(with-test (:name :exact-unparse)
  (labels ((round-trip (list)
             (multiple-value-bind (llks req opt rest keys aux)
                 (sb-c::parse-lambda-list
                  list
                  :accept (sb-c::lambda-list-keyword-mask 'destructuring-bind)
                  :context 'destructuring-bind)
               (sb-c::make-lambda-list llks nil req opt rest keys aux)))
           (try (list)
             (assert (equal list (round-trip list)))))
    (try '(a b . c))
    (try '(a b &rest r))
    (try '(a b &body b))
    (try '(a b &body b &key foo))))

(with-test (:name :fun-type-from-lambda-list)
  (assert (equal
           (sb-c::type-specifier
            (sb-c::ftype-from-lambda-list
             '(&key (size 1) color ((secret foo) 3 ssp) ((:what baz) nil)
               &allow-other-keys)))
           '(function (&key (:size t) (:color t) (secret t) (:what t)
                            &allow-other-keys) *))))

;; CLHS 3.4.4 says
;; "&whole is followed by a single variable that is bound to the entire
;;  macro-call form; this is the value that the macro function receives
;;  as its first argument."
;;
;; but 3.4.4.1.2 says
;; "&whole - The next element is a destructuring pattern that matches
;; the entire form in a macro, or the entire subexpression at inner levels."
;;
;; So one paragraph says "a single variable" and the other "a pattern".
;;
;; If it can be a pattern, then it constrains the expected shape of input
;; in a way that can conflict with the remainder of the pattern.
;; e.g. Given (FOO (&WHOLE (BAZ BAR) X &OPTIONAL Y) MUM), would the
;; outer list's second element need to be a list that matches both
;; (BAZ BAR) and (X &OPTIONAL Y)?  Implementations disagree on this.
;;
;; Further 3.4.4 says "&whole can appear at any level of a macro
;; lambda list. At inner levels, the &whole variable is bound to the
;; corresponding part of the argument, as with &rest, but unlike &rest,
;; other arguments are also allowed."
;; This makes a strange implication that "&rest" does NOT allow
;; "other arguments", when clearly &REST can be followed by &KEY and
;; &AUX (if it means "formal" arguments), and followed by anything at
;; all if it means "actual" arguments. So it's not obvious from this
;; how &whole is supposed to be "unlike" &rest.
;;
;; And finally
;;   "The use of &whole does not affect the pattern of arguments specified."
;; which is is inconsistent in the case where you write
;; (&WHOLE (A B) ...) which certainly seems to require that the whole
;; form be exactly a 2-list. What it was trying to clarify - reasonably
;; in the case where &whole binds one symbol - is that
;;    (DEFMACRO MUMBLE (&WHOLE FOO) ...)
;; in terms of the pattern accepted, is exactly the same as
;;    (DEFMACRO MUMBLE () ...)
;; which means that MUMBLE accepts zero arguments.
;; This is a justified point because the equivalence of &WHOLE
;; and &REST at inner levels suggests that (&WHOLE FOO) actually means that
;; MUMBLE accepts anything when in fact it does not.
;;
;; To resolve these problems, we'll say that &WHOLE at the outermost level
;; of a macro can only bind one symbol, which fits the mental model that it
;; receives the input form and nothing but that.
;; Whoever uses &WHOLE with a non-symbol after it deserves a kick in the head.

(with-test (:name :destructuring-whole)
  (let* ((accept-inner
          (sb-int:lambda-list-keyword-mask 'destructuring-bind))
         (accept-outer
          (logior (sb-int:lambda-list-keyword-mask '&environment)
                  accept-inner)))
    (sb-c::parse-lambda-list '(&whole w a b x) :accept accept-outer)
    (sb-c::parse-lambda-list '(&whole (w) a b x) :accept accept-inner)
    (assert-error
     (sb-c::parse-lambda-list '(&whole 5 a b x) :accept accept-outer))
    (assert-error
     (sb-c::parse-lambda-list '(&whole (w) a b x) :accept accept-outer))))

;; Unparsing a destructuring lambda list does not retain default values,
;; supplied-p variables, or &AUX.
;; This has a practical benefit of not saving source code unwittingly
;; in (X &OPTIONAL (A (MOAR-BIG-FN (DO-ALL-THE-THINGS (...)))))
;; as well as showing just what the lambda list expects as an interface.
(with-test (:name :destructuring-parse/unparse)
  (flet ((try (input &optional (expect input))
           (let ((parse (sb-c::parse-ds-lambda-list input)))
             (assert (equal (sb-c::unparse-ds-lambda-list parse) expect)))))

    (try '((a (b c)) . d)) ; parse/unparse undergoes no change

    (try '(a &optional ((&optional)))) ; likewise

    (try '(&optional . rest)) ; ... and even wackier

    (try '(a (&rest foo) (&whole baz x y))
         '(a (&rest foo) (x y)))

    (try '((&body foo) (&whole (a . d) x y) &aux)
         '((&body foo) (&whole (a . d) x y)))

    (try '(&optional a ((bb1 bb2) (f)) (c 'c) (d 'd dsp) &aux foo (baz))
         '(&optional a ((bb1 bb2)) (c 'c) (d 'd)))

    (try '(&key ((:bork (zook mook)) def bsp) (e 'e esp)
                ((:name fred)) (color x csp))
         '(&key ((:bork (zook mook))) (e 'e) ((:name fred)) (color)))

    (try '(x &optional (y) (((&whole (&whole w z . r) &body b) (c)) (def)))
         ;;                           ^ this &WHOLE variable is irrelevant
         ;;                   ^ but this one isn't
         '(x &optional (y) (((&whole (z . r) &body b) (c)))))

    ;; Expanding a ds-bind of (((X))) re-conses the innermost list
    ;; list thrice, to generate code which produces three distinct
    ;; messages: "problem in (((X)))", "... in ((X))", "... in (X)"
    ;; This isn't great. Ideally the code should entail at most one
    ;; error message, but in general it's not easy to have a single point
    ;; at which the error is signaled, if you must already have pulled apart
    ;; the input to find the error. Thus, ds-bind expands into a sequence
    ;; of checks whether at each level, the structure is right.
    ;; In this limited case, it seems a particularly stupid technique.
    ;;
    ;; At any rate, the unparser memoizes intermediate results,
    ;; since the cost of doing that is virtually nothing.
    ;; This asserts that sharing works during re-construction.
    (let ((parse (sb-c::parse-ds-lambda-list '(((a)))))
          (cache (list nil)))
      (assert (eq (sb-c::unparse-ds-lambda-list parse cache)
                  (sb-c::unparse-ds-lambda-list parse cache))))))

(with-test (:name :macro-lambda-list)
  ;; This only parses the surface level, which suffices to check for
  ;; some edge cases at the toplevel of a macro lambda list.
  (flet ((parse (list)
           sb-c::
           (multiple-value-list
            (parse-lambda-list
             list :accept (logior (lambda-list-keyword-mask 'destructuring-bind)
                                  (lambda-list-keyword-mask '&environment))))))
    ;; The bitmasks of lambda-list-keywords differ, so don't compare them.
    (assert (equal (cdr (parse '(&environment e &rest rest)))
                   (cdr (parse '(&environment e . rest)))))
    (assert (equal (cdr (parse '(&whole w &rest r)))
                   (cdr (parse '(&whole w . r)))))
    (assert (equal (cdr (parse '(&whole w &environment e &rest r)))
                   (cdr (parse '(&whole w &environment e . r)))))
    (assert (equal (cdr (parse '(a b c &environment foo &rest rest)))
                   (cdr (parse '(a b c &environment foo . rest)))))
    (assert (equal (cdr (parse '(a b c &environment foo &body rest)))
                   (cdr (parse '(a b c &environment foo . rest)))))
    (assert-error (parse '(a b c &environment foo &rest r . rest)))

    ;; lp# 707556
    (assert-error (parse '(a &key b &allow-other-keys c)))))

(with-test (:name :ds-lambda-list-symbols)
  (flet ((try (list expect)
           (assert (equal sb-c::(ds-lambda-list-variables
                                 (parse-ds-lambda-list list))
                         expect))))
    (try '(a ((b c))
           &rest (r1 r2)
           &key k1 (k2) (k3 'foo) (k4 'baz k4p) ((:boo (k5 k6)) '(1 2) k56p))
         '(a b c r1 r2 k1 k2 k3 k4 k4p k5 k6 k56p))

    (try '(a &optional x (y) (z 'foo zp) ((w1 w2) (f)) &aux foo (bar) (baz 3))
         '(a x y z zp w1 w2 foo bar baz))))

(with-test (:name :ds-lambda-list-possible-mismatch-warning)
  (assert-signal (sb-c::parse-ds-lambda-list '(a &optional ((b c) 'foo)))
                 style-warning)
  ;; Suppose this meant: one required arg followed by one optional arg
  ;; which is a list of one required and one optional arg.
  ;; But it's accidentally missing a pair of disambiguating parentheses.
  (assert-signal (sb-c::parse-ds-lambda-list '(a &optional (b &optional c)))
                 style-warning)
  ;; Similarly...
  (assert-signal (sb-c::parse-ds-lambda-list '(a &optional (b c &key)))
                 style-warning))

(with-test (:name :ds-bind-list-checkers)
  (labels ((gen-check (lambda-list macro-context)
             (sb-c::emit-ds-bind-check (sb-c::parse-ds-lambda-list lambda-list)
                                       :ignore macro-context nil))
           (try (winp lambda-list input)
             (let ((check (gen-check lambda-list nil)))
               (if winp
                   (apply (car check) input (cddr check))
                   (assert-error (apply (car check) input (cddr check)))))))
    (try t '(a b . rest) '(1 2))
    (try t '(a b . rest) '(1 2 3))
    (try t '(a b . rest) '(1 2 . foo))
    (try nil '(a b . rest) '(1))
    (try t '(a &optional b . rest) '(1 2))
    (try nil '(a &key b) '(1 :b)) ; odd
    (try nil '(a &key b) '(1 :b . bar)) ; dotted
    (try nil '(a &key b) '(1 :b bar . baz)) ; dotted
    (try t '(a &key b) '(1 :b bar :allow-other-keys nil))

    (let ((check (gen-check '(bar &key ((secret v)))
                            '(:macro whatever . define-compiler-macro))))
      (apply (car check) '(a secret 3) (cddr check))
      (assert-signal (apply (car check) '(a secret 3) (cddr check))
                     sb-c::compiler-macro-keyword-problem))))

;; The same lambda lists and test inputs are each run two different ways.
(macrolet ((with-test-ll ((name lambda-list) &body body)
             `(with-test (:name (:ds-bind-shape ,name))
                (let ((fun
                       (lambda (args)
                         (sb-int:binding*
                             ,(sb-c::expand-ds-bind lambda-list 'args nil 'the)
                           (list ,@(sb-c::ds-lambda-list-variables
                                    (sb-c::parse-ds-lambda-list lambda-list))))))
                      (ast (sb-c::meta-abstractify-ds-lambda-list
                            (sb-c::parse-ds-lambda-list ',lambda-list))))
                  ,@body)))
           (win (x &optional expect)
             `(progn (assert (sb-c::ds-lambda-list-match-p ,x ast))
                     ,(if expect
                          `(assert (equal (funcall fun ,x) ,expect))
                          `(funcall fun ,x)))) ; don't crash is all
           (lose (x)
             `(progn (assert (not (sb-c::ds-lambda-list-match-p ,x ast)))
                     (assert-error (funcall fun ,x)))))

  (with-test-ll (:want-0-args ()) ; this only allows NIL as its input
    (win '())
    (lose 'foo)
    (lose '(a)))

  (with-test-ll (:want-1-arg (a))
    (lose '())
    (lose 'foo)
    (win '(a))
    (lose '(a . b))
    (lose '(a b)))

  (with-test-ll (:want-1-or-2-args (a &optional b))
    (lose '())
    (lose 'foo)
    (win '(a))
    (lose '(a . b))
    (win '(a b))
    (lose '(a b . c))
    (lose '(a b c)))

  (with-test-ll (:want-3-args (a b c))
    (lose '())
    (lose '(a))
    (lose '(a b))
    (lose '(a b . c))
    (win '(a b c))
    (lose '(a b c . d))
    (lose '(a b c d)))

  (with-test-ll (:want-3-or-4-args (a b c &optional d))
    (lose '())
    (lose '(a))
    (lose '(a b))
    (lose '(a b . c))
    (win '(a b c))
    (lose '(a b c . d))
    (win '(a b c d))
    (lose '(a b c d . e))
    (lose '(a b c d ee)))

  (with-test-ll (:want-3-or-more-args (a b c &optional d . r))
    (lose '())
    (lose '(a))
    (lose '(a b))
    (lose '(a b . c))
    (win '(a b c))
    (lose '(a b c . d))
    (win '(a b c d))
    (win '(a b c d . e))
    (win '(a b c d ee)))

  (with-test-ll (:destructured-rest (a b &rest (c d)))
    (lose '())
    (lose '(a b))
    (lose '(a b . c))
    (lose '(a b c . d))
    (win '(a b c d))
    (lose '(a b c d . e))
    (lose '(a b c d ee)))

  (with-test-ll (:hairy-1 ((a) ((b . c)) &optional ((x y) '(vx vy))))
    (win '((1) ((2 . whatever)) (3 4)))
    (win '((1) ((2 . whatever)) (3 4)))
    (lose '((1) ((2)) (3))))

  (with-test-ll (:hairy-2 ((a) ((b . c)) &optional ((x &optional y) '(f))))
    (win '((1) ((2 . whatever)) (3)))
    (win '((1) ((2 . whatever))))
    (lose '((1) ((2 . whatever)) 3)))

  ;; This destructuring &WHOLE pattern demands at least 2 args,
  ;; despite that the containing pattern otherwise accepts 0 or more.
  (with-test-ll (:destructured-whole (&whole (a b . c) &optional x &rest y))
    (lose '())
    (lose '(a))
    (lose '(a . b))
    (win '(a b))
    (win '(a b c)))

  (with-test-ll (:destructured-key (a b &rest r
                                      &key ((:point (x y &optional z))
                                            (list 'foo 'bar))))
    (lose '(a b . foo))
    (win '(a b :point (1 2)))
    (lose '(a b :point (1 2 . 3)))
    (win '(a b :point (1 2 3)))
    (lose '(a b :point (1 2 3 4)))
    (lose '(a b :point (1 2 3) :baz 9))
    (win '(a b :point (1 2 3) :baz 9 :allow-other-keys t))
    (win '(a b :point (1 2 3) :baz 9 :allow-other-keys t
           :allow-other-keys nil)))

  ;; This bizarro lambda lists expects that if you give the :FRUITS keyword,
  ;; then its value is NIL, because it has to match a lambda list that
  ;; accepts exactly zero required arguments, zero optionals, and no &REST.
  (with-test-ll (:fruity (&key ((:fruits (&optional)))))
    (win '())
    (lose 'a)
    (lose '(a))
    (lose '(a . b))
    (lose '(:fruits))
    (lose '(:fruits 3))
    (lose '(:fruits (3)))
    (win '(:fruits nil)))

  ;; Test an EXPAND-DS-BIND that is hairier than you should ever write.
  (with-test-ll (:insane-hair
                 ((a ((b) &key)) &optional (((c &rest r)) '((wat)) csp)
                  &aux (bork 'end)))
    (win '((1 ((2)))) '(1 2 wat nil nil end))
    (win '((1 ((2) :size 3 :allow-other-keys t)))
         '(1 2 wat nil nil end))
    (win '((1 ((2))) ((3 . more))) '(1 2 3 more t end))
    (lose '((1 ((2 3)))))
    (lose '((1 ((2) 3))))
    (lose '((1 ((2)) 3)))
    (lose '((1 ((2))) wat))
    (lose '((1 ((2))) (wat))))
  )

(with-test (:name :arg-count-error-tail-calls-error)
 (assert
  (null
   (block found
     (handler-bind ((error
                     (lambda (c)
                       (declare (ignore c))
                       (return-from found
                         (assoc 'sb-c::ds-bind-error
                                (sb-debug::list-backtrace))))))
       (sb-c::ds-bind-error '(foo) 2 3 '((:macro baz . deftype))))))))

(with-test (:name :destructuring-optional/key-warn-once-only)
  (let ((count 0))
    (handler-bind ((warning (lambda (c) (incf count) (muffle-warning c))))
      (macroexpand-1
       '(defmacro defx (name ll &optional (types '*) &key node) 1)))
    (assert (= count 1))))

(with-test (:name :silent-pcl-internals)
  (assert-no-signal
   (sb-int:parse-lambda-list
    '(sb-pcl::.pv. sb-pcl::.next-method-call. self &optional o &key k
       &allow-other-keys))))
