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
               ;; The evaluator will delay lambda-list checks until
               ;; the lambda is actually called.
               (if (eq sb-ext:*evaluator-mode* :interpret)
                   `(funcall ,@args)
                   `(progn ,@args)))
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
               (sb-c::build-lambda-list llks req opt rest keys aux)))
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
          (logior (sb-int:lambda-list-keyword-mask '(&environment))
                  accept-inner)))
    (sb-c::parse-lambda-list '(&whole w a b  x) :accept accept-outer)
    (sb-c::parse-lambda-list '(&whole (w) a b  x) :accept accept-inner)
    (assert-error
     (sb-c::parse-lambda-list '(&whole 5 a b  x) :accept accept-outer))
    (assert-error
     (sb-c::parse-lambda-list '(&whole (w) a b  x) :accept accept-outer))))
