;;;; various compiler tests without side effects

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

;;;; This file of tests was added because the tests in 'compiler.pure.lisp'
;;;; are a total hodgepodge- there is often no hugely compelling reason for
;;;; their being tests of the compiler per se, such as whether
;;;; INPUT-ERROR-IN-COMPILED-FILE is a subclass of SERIOUS-CONDITION;
;;;; in addition to which it is near impossible to wade through the
;;;; ton of nameless, slow, and noisy tests.

;;;; This file strives to do better on all fronts:
;;;; the tests should be fast, named, and not noisy.

(cl:in-package :cl-user)

(load "compiler-test-util.lisp")

(with-test (:name :ldb-recognize-local-macros)
  ;; Should not call %LDB
  (assert (not (ctu:find-named-callees
                (compile nil
                         '(lambda (x)
                           (declare (optimize speed))
                           (macrolet ((b () '(byte 2 2)))
                             (ldb (b) (the fixnum x)))))))))

;; lp#1458190
(with-test (:name :dbp-eval-order)
  (sb-int:collect ((calls))
    (flet ((f (new old)
             (dpb (progn (calls 'eval-new) new)
                  (progn (calls 'eval-byte) (byte 10 10))
                  (progn (calls 'eval-old) old))))
      (f 20 0)
      (assert (equal (calls)
                     '(eval-new eval-byte eval-old))))))

;; Best practice treats TRULY-THE as a special operator, not a macro,
;; in a context such as (DPB X (TRULY-THE SB-KERNEL:BYTE-SPECIFIER ...) Y).
;; DPB used to expand its second argument using MACROEXPAND and lose
;; the nuance of TRULY-THE. Strictly speaking, byte-specifier is not a
;; type specifier that users are supposed to know about, so portable code
;; should not care, but this might affect internal code.
(with-test (:name :dpb-inner-macro)
  (flet ((source-xform (sexpr)
           (funcall (sb-int:info :function :source-transform (car sexpr))
                    sexpr (sb-kernel:make-null-lexenv))))
    (assert (equal-mod-gensyms
             (source-xform
              '(dpb (new) (truly-the sb-kernel:byte-specifier bspec) (old)))
             '(let ((new (new))
                    (byte (truly-the sb-kernel:byte-specifier bspec)))
               (sb-kernel:%dpb new (byte-size byte) (byte-position byte)
                               (old)))))))

(with-test (:name :inline-satisfies-predicate)
  ;; If we remove the indirections in these functions,
  ;; this test should visibly break so that we can write a new test
  ;; that asserts that inlining F works in (THE (SATISFIES F) obj).
  (assert (equal (sb-ext:typexpand 'sb-impl::function-name)
                 '(satisfies sb-int:legal-fun-name-p)))
  (let ((f (compile nil '(lambda (x) (the sb-impl::function-name x)))))
    (assert (equal (list (symbol-function 'sb-int:valid-function-name-p))
                   (ctu:find-named-callees f))))
  (let ((f (compile nil '(lambda (x)
                           (declare (notinline sb-int:legal-fun-name-p))
                           (the sb-impl::function-name x)))))
    (assert (equal (list (symbol-function 'sb-int:legal-fun-name-p))
                   (ctu:find-named-callees f)))))

(with-test (:name :make-array-untestable-type-no-warning)
  (assert-no-signal
   (compile nil `(lambda () (make-array '(2 2)
                                        :element-type `(satisfies foofa))))))

(with-test (:name :make-array-nil-no-warning)
  (assert-no-signal
   (compile nil '(lambda () (make-array '(2 2) :element-type nil)))))

(with-test (:name :nth-value-huge-n-works)
  (flet ((return-a-ton-of-values ()
           (values-list (loop for i below 5000 collect i))))
    (assert (= (nth-value 1 (return-a-ton-of-values)) 1))
    (assert (= (nth-value 4000 (return-a-ton-of-values)) 4000))))

(defstruct (a-test-structure-foo
            (:constructor make-a-foo-1)
            (:constructor make-a-foo-2 (b &optional a)))
  (a 0 :type symbol)
  (b nil :type integer))

(with-test (:name :improperly-initialized-slot-warns)
  (with-open-stream (*error-output* (make-broadcast-stream))
    (multiple-value-bind (f warn err)
        (compile nil '(lambda () (make-a-foo-1 :a 'what)))
      ;; should warn because B's default is NIL, not an integer.
      (assert (and f warn err)))
    (multiple-value-bind (f warn err)
        (compile nil '(lambda () (make-a-foo-2 3)))
      ;; should warn because A's default is 0
      (assert (and f warn err)))))

(with-test (:name :inline-structure-ctor-no-declaim)
  (let ((f (compile nil
                    '(lambda ()
                       (make-a-foo-1 :a 'wat :b 3)))))
    (assert (ctu:find-named-callees f)))
  (let ((f (compile nil
                    '(lambda ()
                       (declare (inline make-a-foo-1))
                       (make-a-foo-1 :a 'wat :b 3)))))
    (assert (not (ctu:find-named-callees f)))))
