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

(enable-test-parallelism)

(defun compiles-with-warning (lambda)
  (assert (nth-value 2 (checked-compile lambda :allow-warnings t))))

(with-test (:name :duplicate-labels)
  (dolist (operator '(labels flet macrolet))
    (multiple-value-bind (fun warn err)
        (let ((*error-output* (make-broadcast-stream)))
          (compile nil `(lambda (x)
                          (declare (ignorable x))
                          (,operator ((f (z) z 2)
                                      (f (z) z 3))
                            (f x)))))
      ;; I'm not asserting on the result of calling FUN
      ;; because I don't really care what it is.
      (declare (ignore fun))
      (assert (and warn err)))))

(with-test (:name (position :derive-type))
  (checked-compile '(lambda (x)
                      (ash 1 (position (the (member a b c) x) #(a b c )))))
  (checked-compile '(lambda (x)
                      (ash 1 (position x #(a b c ))))
                   :allow-style-warnings t)
  ;; The sequence must contain a mixture of symbols and non-symbols
  ;; to call %FIND-POSITION. If only symbols, it makes no calls.
  (let ((calls (ctu:ir1-funargs '(lambda (x) (position x '(1 2 3 a b c 4 5 6 d e f g) :from-end t)))))
    ;; Assert that the default :TEST of #'EQL was strength-reduced to #'EQ
    (assert (equal calls '((sb-kernel:%find-position identity eq)))))
  (checked-compile-and-assert ()
      '(lambda (x)
        (position x '(a b c d e d c b a) :from-end t))
    (('a) 8)
    (('b) 7)))

(with-test (:name (ldb :recognize-local-macros))
  ;; Should not call %LDB
  (assert (equal
           (ctu:ir1-named-calls
                '(lambda (x)
                   (declare (optimize speed))
                   (macrolet ((b () '(byte 2 2)))
                     (ldb (b) (the fixnum x)))))
           '(sb-c::check-ds-list)))) ; why does this remain in the IR?

(with-test (:name (dpb :eval-order :lp-1458190))
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
(with-test (:name (dpb :inner-macro))
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
  (let ((f `(lambda (x) (the sb-impl::function-name x))))
    (assert (equal (ctu:ir1-named-calls f) '(sb-int:valid-function-name-p))))
  (let ((f `(lambda (x)
              (declare (notinline sb-int:legal-fun-name-p))
              (the sb-impl::function-name x))))
    (assert (equal (ctu:ir1-named-calls f) '(sb-int:legal-fun-name-p)))))

(with-test (:name (make-array :untestable-type :no-warning))
  (checked-compile `(lambda () (make-array '(2 2)
                                           :element-type `(satisfies foofa)))))

(with-test (:name (make-array nil :no-warning))
  (checked-compile '(lambda () (make-array '(2 2) :element-type nil))))

(with-test (:name (nth-value :huge-n :works))
  (flet ((return-a-ton-of-values ()
           (values-list (loop for i below 5000 collect i))))
    (assert (= (nth-value 1 (return-a-ton-of-values)) 1))
    (assert (= (nth-value 4000 (return-a-ton-of-values)) 4000))))

(with-test (:name :internal-name-p :skipped-on :sb-xref-for-internals)
  (assert (sb-c::internal-name-p 'sb-int:neq)))

(with-test (:name (:coerce-callable-to-fun :note))
  (flet ((try (form what)
           (multiple-value-bind (fun failure-p warnings style-warnings notes)
               (checked-compile `(lambda (x)
                                  (declare (optimize speed))
                                  (funcall ,form)))
             (declare (ignore fun failure-p warnings style-warnings))
             (assert (search (format nil "~A is not known to be" what)
                             (princ-to-string (first notes)))))))

    (try '(eval `(work-with ,x)) "callable expression")
    (try 'x "X")
    ;; For this I'd accept either Z or X in the message.
    (try '(progn (let ((z x)) (identity z))) "X")))

(with-test (:name (princ-to-string :unflushable))
  ;; Ordinary we'll flush it
  (assert (not (ctu:ir1-named-calls '(lambda (x) (princ-to-string x) x))))
  ;; But in high safety it should be called for effect
  (let ((f `(lambda (x)
              (declare (optimize safety)) (princ-to-string x) x)))
    (assert (equal (ctu:ir1-named-calls f) '(princ-to-string)))))

(with-test (:name :space-bounds-no-consing
            :serial t
            :skipped-on :interpreter)
  ;; Asking for the size of a heap space should not cost anything!
  (ctu:assert-no-consing (sb-vm:%space-bounds :static))
  (ctu:assert-no-consing (sb-vm:space-bytes :static)))

(with-test (:name (sb-vm:map-allocated-objects :no-consing)
            :serial t
            :fails-on (or :cheneygc (not :sb-thread))
            :skipped-on :interpreter)
  (let ((n 0))
    (sb-int:dx-flet ((f (obj type size)
                       (declare (ignore obj type size))
                       (incf n)))
      (ctu:assert-no-consing
       (sb-vm:map-allocated-objects #'f :dynamic)
       5))))

(with-test (:name :pack-varints-as-bignum
                  :skipped-on :interpreter) ; too slow
  (dotimes (i 500) ; do some random testing this many times
    (let* ((random-numbers (loop repeat (+ (random 20) 3)
                                 collect (1+ (random 4000))))
           (test-list (sort (delete-duplicates random-numbers) #'<))
           (packed-int (sb-c:pack-code-fixup-locs test-list nil nil))
           (result (make-array 1 :element-type '(unsigned-byte 32))))
      (sb-sys:with-pinned-objects (packed-int result)
        ;; Now exercise the C unpacker.
        ;; This hack of allocating 4 longs is terrible, but whatever.
        (let ((unpacker (make-alien long 4))
              (prev-loc 0))
          (alien-funcall (extern-alien "varint_unpacker_init"
                                       (function void (* long) unsigned))
                         unpacker
                         (sb-kernel:get-lisp-obj-address packed-int))
          (sb-int:collect ((unpacked))
            (loop
             (let ((status
                    (alien-funcall
                     (extern-alien "varint_unpack"
                                   (function int (* long) system-area-pointer))
                     unpacker (sb-sys:vector-sap result))))
               (let ((val (aref result 0)))
                 ;; status of 0 is EOF, val = 0 means a decoded value was 0,
                 ;; which can't happen, so it's effectively EOF.
                 (when (or (eql status 0) (eql val 0)) (return))
                 (let ((loc (+ prev-loc val)))
                   (unpacked loc)
                   (setq prev-loc loc)))))
            (assert (equal (unpacked) test-list))))))))

(with-test (:name (symbol-value symbol-global-value :quoted-constant))
  (let ((f (checked-compile '(lambda () (symbol-value 'char-code-limit)))))
    (assert (not (ctu:find-code-constants f :type 'symbol))))
  (let ((f (checked-compile '(lambda () (symbol-global-value 'char-code-limit)))))
    (assert (not (ctu:find-code-constants f :type 'symbol)))))

(with-test (:name (:set symbol-value :of defglobal))
  (let ((s 'sb-c::*recognized-declarations*))
    (assert (eq (sb-int:info :variable :kind s) :global)) ; verify precondition
    (let ((f (checked-compile `(lambda () (setf (symbol-value ',s) nil)))))
      ;; Should not have a call to SET-SYMBOL-GLOBAL-VALUE>
      (assert (not (ctu:find-code-constants f :type 'sb-kernel:fdefn))))))

(with-test (:name :alien-linkage-table-bogosity)
  (let ((strings (map 'list (lambda (x) (if (consp x) (car x) x))
                      sb-vm::+required-foreign-symbols+)))
    (assert (= (length (remove-duplicates strings :test 'string=))
               (length strings)))))

(with-test (:name (:no style-warning :for inline :cl-fun))
  (checked-compile '(lambda (x)
                      (declare (optimize (speed 3)) (inline length)
                      (muffle-conditions compiler-note))
                      (length x))))

(with-test (:name :deleted-return-use)
  (checked-compile-and-assert ()
      `(lambda ()
         (block nil
           (return 345)
           (let ((a (catch 'x)))
             (flet ((%f (a &optional b)
                      a))
               (%f 0 (%f 123))))))
    (() 345)))

(with-test (:name :shift-right-transform-nil-type)
  (checked-compile-and-assert (:optimize nil)
      `(lambda (b c)
         (declare (type (integer -10 -6) c)
                  (optimize (debug 2)))
         (catch 'c
           (flet ((f1 (a &optional (b (shiftf b 0)) c d)
                    (declare (ignore a b c d))
                    (throw 'c 780)))
             (flet ((f2 (a b)
                      (f1 a b 0)))
               (ash
                (f1 (if t
                        c
                        (f1 (f2 1 0) 0))
                    b)
                (+ c))))))
      ((-3 -7) 780)))

(with-test (:name :move-lvar-result-through-unused-cast)
  (checked-compile-and-assert (:optimize nil)
      `(lambda ()
         (declare (optimize (debug 0)))
         (labels ((f (a b)
                    a b)
                  (x ()
                    (apply #'f (list 2 3))))
           (declare (notinline f))
           (the integer (x)))
         132)
    (() 132)))

(with-test (:name (:type-conflict funcall :external-lambda))
  (compiles-with-warning `(lambda ()
                            (let ((x (lambda (x) (declare (fixnum x)) x)))
                              (funcall x 'a)))))

(with-test (:name (:type-conflict :callable :external-lambda))
  (compiles-with-warning `(lambda ()
                            (let ((x (lambda (x) (declare (fixnum x)) x)))
                              (find-if x "abca")))))

(with-test (:name (:type-conflict map :result-type))
  (compiles-with-warning `(lambda (str)
                            (map 'string (lambda (x) (declare (ignore x)) nil)
                                 str))))

(with-test (:name (:type-conflict :by-name))
  (compiles-with-warning `(lambda (str)
                            (map 'string 'evenp str))))

(with-test (:name (:type-conflict :callable :reporting))
  (multiple-value-bind (fun failure-p warnings)
      (checked-compile '(lambda (x) (map-into (make-string 10) #'evenp x))
                       :allow-warnings 'warning)
    (declare (ignore fun))
    (assert failure-p)
    (assert (= (length warnings) 1))
    (search "Derived type of EVENP is"
            (princ-to-string (first warnings)))))

(with-test (:name (:type-conflict string :union-type))
  (compiles-with-warning `(lambda (x)
                            (find-if #'evenp (the string x)))))

(with-test (:name (:type-conflict map-into :let))
  (compiles-with-warning `(lambda (z)
                            (let ((x "abc"))
                              (map-into z #'evenp x)))))

(with-test (:name (:type-conflict map-into :result))
  (compiles-with-warning `(lambda (z)
                            (map-into (make-string 10) #'evenp z))))

(with-test (:name (:type-conflict complement))
  (assert (nth-value 3
                     (checked-compile
                      `(lambda (z)
                         (find z "l" :test (complement #'=)))
                      :allow-style-warnings t))))

(with-test (:name :type-across-hairy-lambda-transforms)
  (assert (subtypep (sb-kernel:%simple-fun-type
                     (checked-compile `(lambda (x) (find 1 (the vector x)))))
                    '(function * (values (or (integer 1 1) null) &optional)))))

(with-test (:name :lea-type-derivation)
  (checked-compile-and-assert ()
      `(lambda (b)
         (declare ((integer -3755795408964870057 -3391381516052960895)
                   b))
         (ldb (byte 22 10) (* b 9)))
    ((-3391381516052980893) 2826685)))

(with-test (:name (:unused &optional :and &key))
  (checked-compile-and-assert (:allow-style-warnings t)
      `(lambda (&optional x &key)
         (declare (ignore x))
         10)
    (() 10)))

(with-test (:name (:unknown values :coercion))
  (checked-compile-and-assert ()
      `(lambda (a)
         (declare (notinline values typep))
         (the integer (values a 2305843009213693946 a -207)))
    ((123) (values 123 2305843009213693946 123 -207))))

(with-test (:name :deleted-block-during-generate-type-checks)
  (checked-compile-and-assert (:allow-warnings t)
      `(lambda (a b)
         (declare (notinline min ash conjugate oddp >=))
         (if (and (or t (>= a)) (oddp 0))
             (prog2 0
                 0
               (labels ((f (a b c &key)
                          (declare (ignore a b c))
                          6965670824543402))
                 (f a 0 b)))
             (conjugate
              (dotimes (i 0 0)
                (catch 'c
                  (ash
                   (the integer
                        (ignore-errors
                          (ignore-errors (throw 'c 1))))
                   (min a)))))))
    ((1 2) 0)))

(with-test (:name :block-delete-twice)
  (checked-compile-and-assert ()
      `(lambda ()
         (declare (notinline >=))
         (block nil
           (lambda (x &key (key (if (>= 0 1)
                                    (return (catch 'ct5 0)))))
             (declare (ignore key))
             x)))
    (() 123 :test (lambda (values expected)
                    (equal (multiple-value-list
                            (funcall (first values) (first expected)))
                           expected)))))

(with-test (:name :dead-lvars-and-stack-analysis)
  (checked-compile-and-assert ()
    `(lambda (b)
       (catch 'ct2
         (block b5
           (return-from b5
             (multiple-value-prog1 19
               (if (or b t)
                   (return-from b5 333)))))))
    ((11) 333)))

(with-test (:name :mv-call-more-values)
  (checked-compile-and-assert ()
    `(lambda (z)
       (multiple-value-call (lambda (&optional x y &rest args)
                              (declare (ignore args))
                              (+ y x))
         2 (truncate z 30)))
    ((2345) 80)))

(with-test (:name :unused-casts-at-ir2-convert)
  (checked-compile-and-assert ()
    `(lambda ()
       (unwind-protect 123
         (the integer
              (labels ((%f (x &key)
                         (declare (ignore x))
                         (svref #(46 32) 0)))
                (unwind-protect (%f (%f 0)))))))
    (() 123)))

(with-test (:name :cmov-constants-different-primitive-type)
  (checked-compile-and-assert ()
    `(lambda (b)
       (case b
         ((2030) 4611686018427387908)
         ((572) b)
         (t 0)))
    ((572) 572)
    ((123) 0)
    ((2030) 4611686018427387908)))

(with-test (:name :mv-bind-skipping-vars-on-reoptimize)
  (checked-compile-and-assert ()
    `(lambda ()
       (let (lv1)
         (apply (lambda (&rest args)
                  (declare (ignore args)))
                0
                (list 3 lv1))
         (setf lv1 10)))
    (() 10)))

(with-test (:name :transform-on-a-nil-arg)
  (checked-compile-and-assert ()
   `(lambda ()
      (block nil
        (logtest
         (multiple-value-prog1
             (unwind-protect (return 32))
           (catch 'tag (return 33)))
         1)
        34))
   (() 32)))

(with-test (:name :nesteted-dx-deleted-uses)
  (checked-compile-and-assert ()
    `(lambda (a)
       (block b2
         (let* ((v1 (make-array nil :initial-element
                                (let ((a a))
                                  (return-from b2 a)))))
           (declare (dynamic-extent v1))
           (aref v1))))
    ((342) 342)))

(with-test (:name :deleted-during-locall-analyze-fun-1)
  (checked-compile-and-assert (:allow-warnings t)
    `(lambda ()
       (flet ((a ()))
         (a 1)
         (a 2)))
    (() (condition 'program-error))))

(with-test (:name :delete-return-without-flush-dest)
  (assert (eql
           (catch 'c
             (funcall (checked-compile
                       '(lambda ()
                         (labels ((%f () 40))
                           (multiple-value-prog1 *
                             (throw 'c (%f))
                             (%f)
                             30))))))
           40)))

(with-test (:name :let-conversion-inside-deleted-lambda.1)
  (checked-compile-and-assert ()
    `(lambda ()
       (block nil
         (catch 'c)
         (flet ((f (x &key)
                  (when x
                    (progv '(*) '(0)
                      (return)))))
           (f (return 123))
           (f 0))))
    (() 123)))

(with-test (:name :let-conversion-inside-deleted-lambda.2)
  (checked-compile-and-assert ()
    `(lambda ()
       (block nil
         (block nil
           (lambda () (return)))
         (labels ((l () (l))
                  (%f (a &key)
                    (l)
                    (return a)))
           (%f (return 321))
           (%f 1))))
    (() 321)))

(with-test (:name :assignment-conversion-inside-deleted-lambda)
  (checked-compile-and-assert
   (:allow-style-warnings t)
   `(lambda (b)
      (tagbody
         (labels ((%f13 (&optional (f13-1 0) &key &allow-other-keys)
                    (declare (ignore f13-1))
                    b))
           (if nil
               (%f13 (go tag8))
               (%f13)))
       tag8))
   ((1) nil)))

(with-test (:name :nil-type-derived-before-assignment-conversion)
  (checked-compile-and-assert ()
   `(lambda (a)
      (declare (ignore a))
      (tagbody
        (labels ((f (a)
                   (declare (ignore a))
                   (go tag1)))
          (apply #'f 1 (list))
          (apply #'f (catch 'ct (go tag1)) (list)))
       tag1))
   ((1) nil)))

(with-test (:name :assignment-convert-untail-outside-calls)
  (checked-compile-and-assert ()
   `(lambda ()
      (flet ((%f17 (&optional f17-1)
               (declare (ignore f17-1))
               (block block608
                 (block block606
                   (flet ((h0 ()
                            (return-from block606)))
                     (declare (dynamic-extent #'h0))
                     (return-from block608
                       (progn
                         (print #'h0 (make-broadcast-stream))
                         nil)))))))
        (when nil (%f17))
        (if t
            (%f17)
            (when nil
              (%f17)))))
    (() nil)))

(with-test (:name :assignment-convert-lambda-with-deleted-bind-block)
  (checked-compile-and-assert ()
   `(lambda ()
      (flet ((%f5 ()
               (flet ((%f2 (&optional (f2-2 (return-from %f5 1)))
                        0))
                 (let ((g624 1))
                   (cond ((eql g624 '1)
                          (%f2))
                         ((eql g624 '2)
                          (%f2)))))))
        0))
    (() 0)))

(with-test (:name :unconvert-tail-calls)
  (checked-compile-and-assert ()
    `(lambda ()
       (block nil
         (labels ((f (&optional (a (return))
                                (b (if t (return)))
                                c
                      &rest args)
                    (declare (ignore a b c args))
                    (return 0)))
           (let (x)
             (equal 10 (f 0 3))
             (f 123 0 0)
             (f 0)
             x))))
    (() 0)))

(with-test (:name :deleting-exits-with-multiple-users)
  (checked-compile-and-assert ()
    `(lambda (a b)
       (block nil
         (multiple-value-prog1 b
           (tagbody (return (multiple-value-prog1 3
                              (if a (go z)))) z))))
    ((nil :good) 3)
    ((t :good) :good)))

(with-test (:name :merge-tail-sets-deleted-functional)
  (checked-compile-and-assert ()
    `(lambda (a)
       (block nil
         (tagbody
            (go g549)
          g549
            (return-from nil
              (block b3
                (let ((x (progn (lambda (&optional (x a)) x)
                                (unwind-protect 10)
                                (return-from b3 a))))
                  (unwind-protect x)))))))
    ((321) 321)))

(with-test (:name :float-remainders-rounding-errors)
  (loop for fun in '(ceiling truncate floor
                     fceiling ftruncate ffloor
                     round fround)
        do
        (assert (member (second
                         (third (sb-kernel:%simple-fun-type
                                 (checked-compile
                                  `(lambda (x)
                                     (nth-value 1 (,fun (the double-float x) 1/2)))))))
                        '(double-float real)))))

(with-test (:name :complex-float-contagion)
  (checked-compile-and-assert ()
    `(lambda (p1)
       (declare (type (or double-float integer) p1))
       (complex p1 2.0))
    ((1d0) #c(1d0 2d0))))

(with-test (:name :equal-transform-member-types)
  (let* ((s1 "abc")
         (s2 (copy-seq s1)))
    (checked-compile-and-assert ()
      `(lambda (p1 p2)
         (declare (type (member ,s1) p1)
                  (type (member ,s2 #*10) p2))
         (equal p1 p2))
      ((s1 s2) t))))

(with-test (:name :equalp-transform-numeric-types)
  (checked-compile-and-assert ()
    `(lambda (p1 p2)
       (declare (type (or fixnum list) p1)
                (type double-float p2))
       (equalp p1 p2))
    ((1 1d0) t)))

(with-test (:name :equalp-transform-zero-array)
  (checked-compile-and-assert ()
    `(lambda (a b)
       (declare (simple-string a)
                (simple-bit-vector b))
       (equalp a b))
    (("" #*) t)))

(with-test (:name :equalp-transform-zero-string)
  (checked-compile-and-assert
   ()
   `(lambda (a)
      (equalp "" a))
   ((#*) t)
   ((#()) t)))

(with-test (:name :fill-transform-returning-array-data)
  (let ((vector (make-array 10 :fill-pointer 2)))
    (checked-compile-and-assert ()
      `(lambda (v)
           (declare (type (vector t) v))
           (fill v nil))
      ((vector) vector))))

(with-test (:name :missing-error-context)
  (flet ((run ()
          (let ((string
                 (with-output-to-string (*error-output*)
                   (compile nil '(sb-int:named-lambda bob () (otherfun) 3)))))
            (assert (search "in: SB-INT:NAMED-LAMBDA BOB" string)))))
    (run)
    ;; Unrepeatability is confusing:
    ;; The first compiler invocation used to leave *last-format-string*
    ;; with a toplevel value, so the second would not print enough context
    ;; because the format control and args were the same.
    (run)))

(with-test (:name :cast-deletion-notes)
  (checked-compile-and-assert
      (:allow-notes nil)
      `(lambda (m)
         (setf m (list 1 2 3))
         (the simple-vector
              (coerce m 'vector)))
    ((nil) #(1 2 3) :test #'equalp)))

(with-test (:name :cast-deletion-notes.2)
  (multiple-value-bind (fun fail warn style notes)
      (checked-compile
       `(lambda (m)
          (setf m (list 1 2 3))
          (the simple-vector
               (if (vectorp m)
                   m
                   #(1)))))
    (declare (ignore fail warn style))
    (assert (equalp (funcall fun nil)
                    #(1)))
    (assert (= (length notes) 1))
    (assert (typep (car notes) 'code-deletion-note))))

(with-test (:name :array-call-type-deriver)
  (checked-compile-and-assert
      ()
      `(lambda (vector)
         (funcall (the (function (t t)) #'aref)
                  vector
                  0))
    (((vector 333)) 333)))

(with-test (:name :function-designator-cast-removal)
  (let ((fun (checked-compile
              `(lambda (vectors x)
                 (declare (list vectors x))
                 (map 'list #'svref vectors x)))))
    (assert (notany (lambda (c)
                      (typecase c
                        (sb-kernel:fdefn
                         (eq (sb-c::fdefn-name c) 'svref))
                        (function
                         (eq c #'svref))))
                    (ctu:find-code-constants fun)))
    (assert (equal (funcall fun '(#(44)) '(0)) '(44)))))

(with-test (:name :zombie-casts)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (flet ((f (a b)
                  (declare (ignore a))
                  b))
           (multiple-value-call #'f
             (values (the integer (unwind-protect (f 10 20)))
                     322))))
    (() 322)))

(with-test (:name :zombie-casts.2)
  (let ((sb-c::*max-optimize-iterations* 1))
    (checked-compile-and-assert
        ()
        `(lambda (a b)
           (declare (type fixnum a b))
           (elt '(167992664 119771479)
                (max 0
                     (catch 'ct2
                       (if (typep b '(integer -52))
                           a
                           0)))))
      ((1 2) 119771479))))


(with-test (:name :find-dfo-on-deleted-lambda)
  (assert (= (funcall
              (funcall (checked-compile
                        `(lambda ()
                           (declare (notinline <))
                           (block nil
                             (lambda (&key (key
                                            (unwind-protect
                                                 (if (< 0)
                                                     34
                                                     (return (catch 'c))))))
                               key))))))
             34)))

(with-test (:name :ir1-ir2-dead-code-consistency)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (loop for x below 2
               count (zerop (min x x x x x x x x x x))))
    (() 1)))

(with-test (:name :ir1-ir2-dead-code-consistency)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (loop for x below 2
               count (zerop (min x x x x x x x x x x))))
    (() 1)))

(with-test (:name (setf svref :constant-modification))
  (assert
   (= (length (nth-value 2
                         (checked-compile
                          `(lambda (x)
                             (setf (svref #(a b c) 1) x))
                          :allow-warnings 'sb-int:constant-modified)))
            1)))

(with-test (:name (debug :constant-modification))
  (assert
   (= (length (nth-value 2
                         (checked-compile
                          `(lambda (x)
                             (declare (optimize (debug 2)))
                             (let ((m "abc"))
                               (delete x m)))
                          :allow-warnings 'sb-int:constant-modified)))
      1)))

(with-test (:name (debug :constant-modification.2))
  (assert
   (= (length (nth-value 2
                         (checked-compile
                          `(lambda (x)
                             (declare (optimize (debug 2)))
                             (let ((m (if x
                                          "abc"
                                          "fgh")))
                               (delete x m)))
                          :allow-warnings 'sb-int:constant-modified)))
      1)))

(with-test (:name (debug :unused-tn-long-arglist))
  (checked-compile-and-assert
      ()
      `(lambda (n x)
         (declare (sb-vm:word n))
         (log (float n))
         (nth-value 33 (funcall x . #.(loop for i to 35 collect i))))
    ((10 (lambda (&rest args) (values-list args))) 33)))

(with-test (:name (debug :unused-tn-very-long-arglist))
  (checked-compile-and-assert
      ()
      `(lambda (n x)
         (declare (sb-vm:word n))
         (log (float n))
         (nth-value 33 (funcall x . #.(loop for i to 350 collect i))))
    ((10 (lambda (&rest args) (values-list args))) 33)))

(with-test (:name (dynamic-extent :recursive-local-functions))
  (checked-compile
   `(lambda ()
      (let ((s (labels ((%f () (%f)))
                 (%f))))
        (declare (dynamic-extent s))
        (car s)))))

(with-test (:name (:ctypep :hairy-types))
  (checked-compile
   `(lambda ()
      (the (cons (satisfies error)) '("a"))))
  (assert
   (nth-value 3
              (checked-compile
               `(lambda () (the (array abc) #()))
               :allow-style-warnings t))))

(with-test (:name (catch :evaluate-tag-before-%catch))
  (checked-compile-and-assert
      (:allow-style-warnings t)
      `(lambda (z)
         (catch (multiple-value-call #'+
                  (if z 1 (values 1 2)))
           :done))
    ((t) :done)
      ((nil) :done)))

(with-test (:name :nested-catch-progv-compile)
  (checked-compile
   `(lambda (a b)
      (catch 'ct
        (flet ((f (x &key) x (throw 'ct b)))
          (dotimes (i 1)
            (if (< (progv '() (f a) 1) a)
                a
                (catch 'ct (f a)))))))))

(with-test (:name (tagbody :tag-dynamic-extent))
  (checked-compile-and-assert
   (:optimize '(:safety 3 :debug 2))
   `(lambda (b)
      (declare (optimize (safety 3) (debug 2)))
      (tagbody
         (labels ((f (x &key) x (go tag6)))
           (tagbody
              (catch 'ct2 (f b))
            2)
           (dotimes (i 1) (f 1))
           -1)
       tag6))
   ((1) nil)))

(with-test (:name :fewer-cast-conversions)
  (multiple-value-bind (fun failed)
      (checked-compile
       `(lambda ()
          (let* ((v (cons 0 (catch 'ct (the integer nil)))))
            (declare (dynamic-extent v))
            (flet ((%f (x) x))
              (%f (cdr v)))))
       :allow-warnings t)
    (assert failed)
    (handler-bind ((error (lambda (c) c (throw 'ct 33))))
      (assert (= (funcall fun) 33)))))

(with-test (:name :constant-folding-with-callable-args)
  (checked-compile '(lambda () (count #'%f '(a)))
                   :allow-style-warnings t))

(with-test (:name :flushable-with-callable-args)
  (assert (not (ctu:ir1-named-calls
                '(lambda (y) (let ((x (count y '(1 2 3))))
                            (declare (ignore x))))))))

(with-test (:name (remove :count))
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (remove x "aaa" :count 2))
   ((#\a) "a"))
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (remove-if (lambda (y) (eql y x)) "aaa" :count 2))
   ((#\a) "a")))

(with-test (:name (:constant-fold :allow-other-keys))
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (reduce #'+ '(1 2 3)  :allow-other-keys t :bad x))
   ((1) 6)))

(with-test (:name (:constant-fold :allow-other-keys.2))
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (reduce #'+ '(1 2 3)  :allow-other-keys x))
   ((1) 6)))

(with-test (:name (:constant-fold :repeat-keys))
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (member nil '(1 2 3) :key #'evenp :key x))
   ((1) '(1 2 3) :test #'equal)))



(with-test (:name :function-and-instance-primitive-type)
  (checked-compile-and-assert
      ()
      `(lambda (f)
         (declare (function f))
         (the standard-object f)
         (funcall f #'list t))
    ((#'documentation) (documentation #'list t))))

(with-test (:name :mv-call-safety-0)
  (checked-compile-and-assert
      ()
      `(lambda (a)
         (flet ((%f1 (x y) (+ x y)))
           (apply #'%f1 a (list 0))))
    ((3) 3)))

(with-test (:name :cast-type-check-external)
  (checked-compile-and-assert
      ()
      `(lambda (x)
         (declare (notinline +))
         (gcd
          (loop for lv2 below 1
                count (logbitp 0
                               (if x
                                   (return x)
                                   1)))
          0))
    ((334) 334)))

(with-test (:name :flush-combination-non-fun-type)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (rassoc-if-not #'values '((1 . a)) :allow-other-keys t)
         1)
    (() 1)))

(with-test (:name :symeval-nil)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (symbol-value nil))
    (() nil)))

(with-test (:name (:environment-analyze :deleted-lambda))
  (checked-compile-and-assert
      ()
      `(lambda (log)
         (loop for str in nil
               for i from 0
               do
               (ignore-errors (format log ""))))
    ((t) nil)))

(with-test (:name (:ensure-lvar-fun-form :lvar-uses))
  (checked-compile-and-assert
      ()
      `(lambda (op) (funcall (case op (equal '=) (t '=)) 1 2))
    (('equal) nil)
    ((t) nil)))

(with-test (:name :substitute-let-funargs-during-find-initial-dfo)
  (checked-compile
   `(lambda ()
      (labels ((%r (f)
                 (loop)
                 (%r f)))
        (%r (lambda ()))))))

(with-test (:name :split-ir2-blocks-cmov)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (let ((v (list 0)))
           (if (block nil
                 (eq v (cdr v)))
               1
               2)))
    (() 2)))

(with-test (:name :=-rational-complex-rational-fold)
  (let ((fun (checked-compile '(lambda (x)
                                (declare ((complex integer) x))
                                (= x 10))))
        (fun2 (checked-compile '(lambda (x)
                                (declare ((complex rational) x))
                                (= x 10d0)))))
    (assert (equal (sb-kernel:%simple-fun-type fun)
                   '(function ((complex integer)) (values null &optional))))
    (assert (not (funcall fun #C(10 10))))
    (assert (equal (sb-kernel:%simple-fun-type fun2)
                   '(function ((complex rational)) (values null &optional))))
    (assert (not (funcall fun2 #C(10 10))))))

(with-test (:name (:numeric float rational :contagion))
  (flet ((check (operator type argument)
           (let ((fun (checked-compile
                       `(lambda (x)
                          (declare (type ,type x))
                          ,(ecase argument
                             (1 `(,operator x 1/2))
                             (2 `(,operator 1/2 x)))))))
             (assert (null (ctu:find-code-constants fun :type 'ratio))))))
    (dolist (operator '(+ * / - = < > <= >=))
      (dolist (type '(single-float double-float))
        (check operator type 1)
        (check operator type 2)
        (when (member operator '(+ * / - =))
          (check operator `(complex ,type) 1)
          (check operator `(complex ,type) 2))))))

(with-test (:name (:numeric float float :contagion))
  (flet ((check (operator type argument)
           (let ((fun (checked-compile
                       `(lambda (x)
                          (declare (type ,type x))
                          ,(ecase argument
                             (1 `(,operator x 1.0f0))
                             (2 `(,operator 1.0f0 x)))))))
             (assert (null (ctu:find-code-constants fun :type 'single-float))))))
    (dolist (operator '(+ * / - = < > <= >=))
      (check operator 'double-float 1)
      (check operator 'double-float 2)
      (when (member operator '(+ * / - =))
        (check operator '(complex double-float) 1)
        (check operator '(complex double-float) 2)))))

(with-test (:name :find-type-deriver)
  (checked-compile-and-assert
      ()
      `(lambda (x)
         (find 1 x :key #'values))
    (('(1)) 1)))

(with-test (:name :tail-call-ltn-annotation)
  (checked-compile-and-assert
      ()
      `(lambda (x)
         (labels ((ff1 ()
                    (multiple-value-call #'print
                      (if x
                          (values t t)
                          nil))
                    (ff1)))
           (identity (ff1))))))

(with-test (:name (:substitute-lvar-uses :deleted-code-and-dx-lvars))
  (assert (nth-value 1
                     (checked-compile
                      `(lambda ()
                         (let ((v (values
                                   (the integer
                                        (flet ((%f5 (x) x))
                                          (%f5)))
                                   (unwind-protect 1))))
                           (declare (dynamic-extent v))
                           v))
                      :allow-warnings t))))

(with-test (:name (restart-case :declaration-processing))
  (checked-compile-and-assert
      ()
      `(lambda ()
         (restart-case (list)
           (my-restart (x) "foo" "bar" x)))
    (() ()))
  (checked-compile-and-assert
      ()
      `(lambda ()
         (restart-case (list)
           (my-restart () (declare))))
    (() ())))

(with-test (:name (handler-case :declaration-processing))
  (checked-compile-and-assert
      ()
      `(lambda ()
         (handler-case (list 1 2) (error (e) "foo" "bar" e)))
    (() '(1 2)))
  (assert (nth-value 1
                     (checked-compile
                      `(lambda ()
                         (handler-case (declare)))
                      :allow-failure t))))

(with-test (:name (:unconvert-tail-calls :deleted-call))
  (assert (nth-value 1
                     (checked-compile
                      '(lambda ()
                        (labels ((%f (&optional (x (* 2 nil (%f)))) x))
                          (%f)
                          (%f 1)))
                      :allow-warnings t))))

(with-test (:name (:equal-transform :nil-types))
  (assert (nth-value 1
                     (checked-compile
                      '(lambda ()
                        (loop for y below 3
                              count (or
                                     (not (or (>= y y) (equal y -787357528)))
                                     (the integer (or (>= y y) (equal y -787357528))))))
                      :allow-warnings t))))



(with-test (:name (:delete-recursive-optional))
  (checked-compile '(lambda (x)
                     (lambda ()
                       (labels ((f (&optional a) (values x a #'f))))))))

(with-test (:name (:combination-args-flow-cleanly-p :unused-result))
  (checked-compile-and-assert
      ()
      `(lambda ()
         (let ((v (flet ((%f (x)
                           (list x)
                           (list 1)))
                    (%f 2))))
           (declare (dynamic-extent v))
           (car v)))
    (() 1)))

(with-test (:name (:delete-ref :maintain-lambda-calls-or-closes))
  (checked-compile `(lambda (c y)
                      (labels ((f1 ()
                                 (if y
                                     (f3 2)))
                               (l () (loop))
                               (f2 ()
                                 (l)
                                 (f3 3))
                               (f3 (x)
                                 (f3 x))
                               (f4 ()
                                 (f1)
                                 (f2)))
                        (f4)
                        c))))

(with-test (:name (the :nil-type))
  (checked-compile
   `(lambda ()
      (flet ((f () (the nil 0)))
        (oddp (f))))))

(with-test (:name :concatenate-transform-hairy-type)
  (checked-compile
      '(lambda (x)
        (concatenate '(and string (satisfies eval)) x))))

(with-test (:name :make-array-transform-deletion-notes)
  (checked-compile
   `(lambda (vector)
      (let* ((length (length vector))
             (new (make-array length :adjustable t
                                     :fill-pointer length)))
        new))
   :allow-notes nil))

(with-test (:name :ltn-analyze-cast-unlink)
  (assert (nth-value 1 (checked-compile
                        `(lambda (n)
                           (* 2 n)
                           (let ((p (make-array n :element-type 'double-float)))
                             (dotimes (i n)
                               (setf (aref p i)
                                     (ignore-errors i)))))
                        :allow-warnings t))))

(with-test (:name :call-type-validation)
  (checked-compile
   `(lambda ()
      (funcall (the (or cons function) *debugger-hook*)))))

(with-test (:name :setf-schar-hairy-types)
  (checked-compile-and-assert
      ()
      `(lambda (s v)
         (setf (schar (the (satisfies eval) s) 0) v)
         s)
    (((copy-seq "abc") #\m) "mbc" :test #'equal)))

(with-test (:name :replace-zero-elements)
  (checked-compile-and-assert
      ()
      '(lambda (x)
        (declare ((simple-vector 2) x))
        (replace x x :start1 2))
    (((vector 1 2)) #(1 2) :test #'equalp))
  (checked-compile-and-assert
      ()
      '(lambda (x)
        (replace x x :start1 2))
    (((vector 1 2)) #(1 2) :test #'equalp)))

(with-test (:name :error-in-xep)
  (checked-compile-and-assert
      (:optimize :safe)
      '(lambda (x)
        (declare (type (satisfies error) x))
        x)
    (("") (condition 'error))))

(with-test (:name :lifetime-analyze-tn-overflow-unused-tns)
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (multiple-value-bind (a b c)
          (funcall x 1 2 3 ,@(make-list 58))
        (declare (ignore b))
        (values a c)))
   ((#'values) (values 1 3))))

(with-test (:name :constraints-not-enough-args)
  (checked-compile-and-assert
   ()
   `(lambda (list)
      (delete-if #'> (the list list)))
   (((list 1)) nil)))

(with-test (:name :%coerce-callable-for-call-removal-order-mv-call)
  (checked-compile-and-assert
      ()
      `(lambda (fun args)
         (loop
          (let ((result (apply fun args)))
            (when result
              (return result))
            (setf args result))))
    (('list '(1)) '(1) :test #'equal)))

(with-test (:name :constraint-loop)
  (checked-compile-and-assert
      ()
      `(lambda (a b)
         (check-type a list)
         (when a
           (mapcar #'identity a)
           (loop for c from 0 do (loop for d in b do
                                       (loop for e in a)))))))

(with-test (:name :primitive-type-fun-designator)
  (checked-compile-and-assert
      ()
      `(lambda  (fun)
         (map 'vector fun '(1 2 3)))
    (('1+) #(2 3 4) :test #'equalp)))

(with-test (:name :mv-call-lambda-type-derivation)
  (assert
   (equal (sb-kernel:%simple-fun-type
           (checked-compile
            '(lambda (x)
              (multiple-value-call
                  (lambda () 133)
                (funcall x)))))
          '(function (t) (values (integer 133 133) &optional)))))

(with-test (:name :mv-call-lambda-type-derivation.closure)
  (assert
   (equal (sb-kernel:%simple-fun-type
           (checked-compile
            '(lambda (x)
              (multiple-value-call
                  (lambda () (print x) 133)
                (funcall x)))))
          '(function (t) (values (integer 133 133) &optional)))))

(with-test (:name :constant-folding-and-hairy-types)
  (checked-compile-and-assert
      ()
      '(lambda ()
        (> 0 (the (satisfies eval) (- 1))))
    (() t)))

(with-test (:name :type-approximate-interval-and-hairy-types)
  (checked-compile-and-assert
      ()
      '(lambda (x)
        (declare (fixnum x))
        (<= (the (satisfies eval) 65) x))
    ((66) t)))

(with-test (:name :remove-equivalent-blocks-constraints)
  (checked-compile-and-assert
      ()
      `(lambda (c)
         (declare (integer c))
         (= (case c
              ((-10) (abs c))
              (t c))
            -1))
    ((-1) t)))

(with-test (:name :typep-singleton-intersect-types)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (keywordp t))
    (() nil)))

(with-test (:name :constants-and-cmp)
  (checked-compile-and-assert
      ()
      '(lambda (l)
        (declare (fixnum l))
        (let ((v 0))
          (labels ((change ()
                     (setf v 10)
                     #'change))
            (> v l))))
    ((1) nil))
  (checked-compile-and-assert
      ()
      '(lambda (l)
        (declare (fixnum l))
        (let ((v 0))
          (labels ((change ()
                     (setf v 10)
                     #'change))
            (> l v))))
    ((1) t)))

(with-test (:name :inlining-and-substituted-block-lvars)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (let ((z (block nil
                    (labels ((f (x)
                               (return x)))
                      (declare (inline f))
                      (funcall (the function #'f) t)
                      (funcall (the function #'f) t)))))
           (and z
                1)))
    (() 1)))

(with-test (:name :inlining-reanlyzing-optionals)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (labels ((w (x)
                    x
                    #'s)
                  (fun2 (f x)
                    (funcall f x))
                  (s (&optional x)
                    (fun2 #'w x)))
           (declare (inline w))
           (s)))))

(with-test (:name :vector-fill/t-fast-safe)
  (let ((sb-c::*policy-min* sb-c::*policy-min*))
    (sb-ext:restrict-compiler-policy 'safety 1)
    (checked-compile-and-assert
     ()
     '(lambda ()
       (make-array 2 :initial-element 10))
     (() #(10 10) :test #'equalp))))

(with-test (:name :deleted-tail-sets)
  (checked-compile-and-assert
   ()
   '(lambda ()
     (labels ((f (&optional (a (catch t 6))
                            (b (error ""))
                            (c (unwind-protect 1)))
                (+ a b c)))
       (unwind-protect (f 4))))
   (() (condition 'error))))

;;; The SLEEP source transform barfed on float positive infinity
;;; values.
(with-test (:name (compile sleep float :infinity :lp-1754081))
  (checked-compile '(lambda () (sleep single-float-positive-infinity)))
  (checked-compile '(lambda () (sleep double-float-positive-infinity))))
;;; And it didn't work at all after the fix for aforementioned
(with-test (:name :sleep-float-transform
                  :skipped-on (and :win32 (not :sb-thread)))
  (let* ((xform (car (sb-c::fun-info-transforms (sb-int:info :function :info 'sleep))))
         (type (car (sb-kernel:fun-type-required (sb-c::transform-type xform)))))
    (assert (sb-kernel:constant-type-p type))
    ;; CONSTANT-TYPE isn't actually testable through CTYPEP.
    ;; So pull out the actual type as the compiler would do.
    (assert (sb-kernel:ctypep 1.5 (sb-kernel:constant-type-type type)))))

(with-test (:name :atanh-type-derivation)
  (checked-compile-and-assert
   ()
   '(lambda (x)
     (atanh (coerce x '(double-float * (0.0d0)))))))

(with-test (:name :ir1-optimize-combination-unknown-keys)
  (checked-compile-and-assert
      ()
      '(lambda (p x y)
        (let ((f (when p #'string-equal)))
          (when f
            (funcall f "a" "b" x y))))
    ((t :start1 0) nil)))

(with-test (:name :member-transform)
  (let ((list '(2 1 3)))
    (checked-compile-and-assert
     ()
     '(lambda (list &key key)
       (member 1 list :key key))
     ((list) (cdr list)))))

(with-test (:name :note-no-stack-allocation-casts)
  (checked-compile-and-assert
   ()
   `(lambda ()
     (let ((*s* (the integer (catch 'ct1 0))))
       (declare (dynamic-extent *s*)
                (special *s*))))))

(with-test (:name :dxify-downward-funargs-variable-name)
  (checked-compile-and-assert
      ()
      '(lambda () ((lambda (map) (funcall map)) #'list))))

(with-test (:name :dxify-downward-funargs-malformed)
  (checked-compile
      '(lambda () (sb-debug:map-backtrace))
      :allow-style-warnings t))

(with-test (:name :dxify-downward-funargs-casts)
  (checked-compile-and-assert
   ()
   '(lambda (f x)
     (flet ((f (y) (funcall f y)))
       (funcall (the (satisfies eval) #'every) #'f x)))
   ((#'evenp '(2 2 4)) t)))

(with-test (:name :array-call-type-deriver-non-fun-type)
  (checked-compile-and-assert
      ()
      '(lambda (x) (funcall (the compiled-function #'aref) x))
    ((#0A123) 123)))

(with-test (:name :nth-&rest-overflow)
  (checked-compile-and-assert
      ()
      '(lambda (&rest s) (nth 536870908 s))
    (() nil)))


(with-test (:name :array-in-bounds-p-transform-hairy-types)
  (checked-compile-and-assert
      ()
      '(lambda ()
        (let ((a (the (satisfies eval) (make-array 4 :fill-pointer 0))))
          (and (array-in-bounds-p a 0)
               (array-in-bounds-p a 1))))
    (() t)))

(with-test (:name :array-type-dimensions-or-give-up-hairy-types)
  (checked-compile-and-assert
      ()
      '(lambda (a i)
        (declare ((or (array * (1)) (satisfies eval)) a))
        (array-row-major-index a i))
    ((#(a b) 1) 1)))

(with-test (:name :array-type-dimensions-0-rank)
  (checked-compile-and-assert
      ()
      '(lambda (p1)
        (declare ((or (array bit 1) (array * 0)) p1))
        (array-total-size p1))
    ((#0a3) 1)))

(with-test (:name :type-derivation-hairy-types)
  (checked-compile-and-assert
   ()
   `(lambda (n s)
      (declare (fixnum n))
      (ash (the (satisfies eval) n)
           (the (integer * 0) s)))
   ((1234 -4) 77))
  (checked-compile-and-assert
   ()
   `(lambda (p)
      (declare (type (member #c(0.5d0 4.0d0) #c(0 -1)) p))
      (/ (the (satisfies eval) p)))
   ((#c(0 -1)) #C(0 1))))

(with-test (:name :assert-lvar-type-intersection)
  (checked-compile-and-assert
      ()
      `(lambda (x y)
         (write-sequence nil (the standard-object x) y nil))))

(with-test (:name :or-bignum-single-float-no-notes
            :skipped-on (not (or :arm64 ppc :x86 :x86-64)))
  (checked-compile
   '(lambda (x) (declare (optimize speed)) (typep x '(or bignum single-float)))
   :allow-notes nil))


(with-test (:name :vertices-best-color/general-default-value)
  (checked-compile-and-assert
      ()
      `(lambda (a)
         (declare ((simple-array (complex double-float)) a))
         (* (aref a 0)
            (let ((z (aref a 0)))
              (complex (realpart z) (imagpart z)))))))

(with-test (:name :copy-list-inlined)
  (let ((f (checked-compile
            `(lambda (x) (declare (optimize speed)) (copy-list x)))))
    ;; Should not have a call to COPY-LIST (or anything)
    (assert (not (ctu:find-code-constants f :type 'sb-kernel:fdefn)))))

(with-test (:name :move-from-fixnum+-1)
  (checked-compile-and-assert
   (:allow-notes nil)
   `(lambda (x)
      (declare (fixnum x))
      (1- x))
   ((0) -1)
   ((most-positive-fixnum) (1- most-positive-fixnum))
   ((most-negative-fixnum) (1- most-negative-fixnum)))
  (checked-compile-and-assert
   (:allow-notes nil)
   `(lambda (x)
      (declare (fixnum x))
      (1+ x))
   ((0) 1)
   ((most-positive-fixnum) (1+ most-positive-fixnum))
   ((most-negative-fixnum) (1+ most-negative-fixnum)))
  (checked-compile-and-assert
   (:allow-notes nil)
   `(lambda (a x)
      (declare (fixnum x))
      (if a
          10
          (1+ x)))
   ((nil 0) 1)
   ((t 0) 10)
   ((nil most-positive-fixnum) (1+ most-positive-fixnum))
   ((nil most-negative-fixnum) (1+ most-negative-fixnum))))

(with-test (:name :coalesce-more-ltn-numbers-constants)
  (checked-compile-and-assert
      ()
      `(lambda (x)
         (list 1 1 ,@(make-list 100 :initial-element 'x)))
    ((1) (make-list 102 :initial-element 1) :test #'equal)))

(with-test (:name (:lambda-var-ref-lvar :multiple-refs))
  (checked-compile-and-assert
   ()
   `(lambda (vector index)
      (labels ((update (index)
                 (let ((old (svref vector index)))
                   (if (eq old 10)
                       (update index)
                       old)))
               (wrap (index)
                 (update index)))
        (wrap index)))
   ((#(1 2 3) 1) 2)))

(with-test (:name :string-type-unparsing)
  (checked-compile-and-assert
      ()
      `(lambda (s)
         (declare (type (string 1) s))
         (the (or simple-array (member 1/2 "ba" 0 #\3)) s))
    ((#1="a") #1#)))

(with-test (:name :primitive-type-function)
  (checked-compile-and-assert
      ()
      `(lambda (x)
         (funcall (the (and atom (not null)) x))
         )
    ((#'list) nil)
    (('list) nil)))

(with-test (:name :copyprop-sc-mismatch-between-moves
            :serial t
            :skipped-on :interpreter)
  (let ((f (checked-compile
            '(lambda (f x)
              (let ((x (the double-float x)))
                (values (funcall f x) (> x 1d0)))))))
    (ctu:assert-no-consing (funcall f #'identity 1d0))))

(with-test (:name (:infer-iteration-var-type :step-is-range))
  (let ((f (checked-compile
            '(lambda (s)
              (declare ((integer 1 2) s))
              (let ((r 16))
                (loop for i from 16 below 32 by s
                      do (setf r i))
                r)))))
    (assert (equal (sb-impl::%simple-fun-type f)
                   '(function ((integer 1 2)) (values (integer 16 31) &optional))))))

(with-test (:name (:infer-iteration-var-type :multiple-sets))
  (let ((f (checked-compile
            '(lambda (x)
               (declare (optimize speed)
                        (type (integer 3 10) x))
               (let ((y x))
                 (tagbody
                  :start
                    (when (plusp y)
                      (decf y)
                      (when (plusp y)
                        (decf y)
                        (go :start))))
                 y))
            :allow-notes nil)))
    (assert (equal (sb-impl::%simple-fun-type f)
                   '(function ((integer 3 10)) (values (integer 0 0) &optional))))))

(with-test (:name (:infer-iteration-var-type :incompatible-sets))
  (checked-compile-and-assert ()
      '(lambda (input-total missing-amount)
         (declare (fixnum input-total) (fixnum missing-amount))
         (loop with tot = 0
               repeat 1
               do (let ((difference input-total))
                    (setq difference (max difference 0))
                    (setq tot (+ tot difference)))
               finally (when (plusp missing-amount)
                         (decf tot missing-amount))
                       (return (if (plusp tot) :good :bad))))
    ((0 0) :bad)
    ((1 0) :good)
    ((0 1) :bad)
    ((1 1) :bad)))

(with-test (:name :delay-transform-until-constraint-loop)
  (checked-compile-and-assert
      ()
      `(lambda (str)
         (declare (string str))
         (when (plusp (length str))
           (make-array (1- (length str))
                       :element-type (array-element-type str)
                       :displaced-to str)))
    (("abc") "ab" :test #'equal)))

(with-test (:name :lambda-var-ref-lvar-loop)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (labels ((z (a)
                    (when (>= 0 (the integer a))
                      (values #'z a))))))
    (() nil)))

(with-test (:name :vector-length-fill-pointer-type-derivation)
  (checked-compile-and-assert
      ()
      `(lambda (s)
         (= (length (the (string 1) s)) 1))
    (((make-array 1 :element-type 'character :fill-pointer 0)) nil)))

(with-test (:name :function-designator-loop)
  (checked-compile-and-assert
      ()
      `(lambda (p1 p3 p4)
         (declare (type (or (eql #.#'oddp)
                            (satisfies eval)) p1))
         (find-if-not p1 nil p3 p4))
       ((#'oddp :from-end t) nil)))

(with-test (:name :lvar-constants-nested-funs)
  (checked-compile-and-assert
      ()
      `(lambda (x)
         (labels ((f (x &optional result)
                    (if x
                        (f x result)
                        (nreverse result))))
           (f x)))
      ((nil) nil)))

(with-test (:name :nested-indirect-var-fp-coalescence)
  (checked-compile-and-assert
   ()
   `(lambda ()
      (let ((x 1)
            (j 2))
        (labels ((m ()
                   (incf x 32)
                   (incf j 44)
                   (let ((z 1))
                     (labels ((m ()
                                (incf x 32)
                                (incf z)))
                       (declare (notinline m))
                       (m)
                       (incf j z)))))
          (declare (notinline m))
          (m)
          (values x j))))
    (() (values 65 48))))

(with-test (:name :non-returning-functions-conflict)
  (checked-compile-and-assert
   ()
   `(lambda (x) (map nil #'error x))
    ((nil) nil)))

(with-test (:name :array-typep-other-pointer-widetag)
  (checked-compile-and-assert
      ()
      `(lambda (x)
         (typep x '(and base-string (not simple-array))))
    ((10) nil)
    (((make-array 10 :element-type 'base-char :adjustable t)) t)))

(with-test (:name :constraint-after-checkgen)
  (let ((v #(10 20)))
    (checked-compile-and-assert
        ()
        `(lambda (p1 p2 p3 p4)
           (declare (type (satisfies eval) p2)
                    (type (member :from-end 2) p3))
           (position p1
                     (the (member ,v 3) p2)
                     (the (member 1 :from-end) p3) nil
                     :test-not p4))
      ((20 v :from-end #'/=) 1))))

(with-test (:name :cast-multiple-uses-no-dest)
  (checked-compile-and-assert
      (:allow-style-warnings t)
      `(lambda (x)
         (the integer
              (when x
                (if (> x 0) 1 2)))
         (the integer x))
    ((23) 23))
  (checked-compile-and-assert
      (:allow-style-warnings t)
      `(lambda (a b)
         (declare (optimize (debug 1)))
         (logand
          (when b
            (if (> (the integer a) 0) 10 20))
          0)
         (the integer b))
    ((24 23) 23)))


(with-test (:name :maybe-delete-exit-after-let-conversion)
  (checked-compile-and-assert
      ()
      `(lambda (m)
         (flet ((out ()
                  (flet ((in (a)
                           (dotimes (i 3 a)
                             (if m
                                 (return-from out)
                                 (return-from out)))
                           (labels ((f (&optional (a m))
                                      a
                                      m)))))
                    (in (in 10)))))

           (out)
           33))
    ((t) 33))
  (checked-compile-and-assert
      ()
      `(lambda ()
         (unwind-protect
              (flet ((f (a b &optional c)
                       (values a b c)))
                (f 1 2 (f 0 0)))))
    (() (values 1 2 0))))

(with-test (:name :make-array-hairy-cons)
  (checked-compile-and-assert
      ()
      `(lambda (type)
         (make-array 4 :element-type type :initial-element 0))
    (('(or (cons (satisfies eval)) atom)) #(0 0 0 0) :test #'equalp)))

(with-test (:name :substitute-single-use-lvar-exit-cleanups)
  (checked-compile-and-assert
      ()
      `(lambda (z)
         (block nil
           (let ((b (1+ (funcall z))))
             (catch 'c (return b)))))
    (((constantly 33)) 34)))

(with-test (:name :substitute-single-use-lvar-unknown-exits)
  (checked-compile-and-assert
      ()
      `(lambda (f)
         (block nil
           (let ((x (evenp (funcall f)))
                 (y (catch 'c
                      (return (catch 'c (block nil 11))))))
             (declare (ignore y))
             x)))
    (((constantly 33)) 11)))

(with-test (:name :substitute-single-use-lvar-unknown-exits.2)
  (checked-compile-and-assert
      ()
      `(lambda (b)
         (block nil
           (if (catch 'c 0)
               (return
                 (let ((x (the real b)))
                   (let ((* (list 1)))
                     (declare (dynamic-extent *))
                     (catch 'ct5
                       (if t (return 34))))
                   x))
               (catch 'c 0))))
    ((1) 34)))

(with-test (:name :substitute-single-use-lvar-unknown-exits.3)
  (checked-compile-and-assert
      ()
      `(lambda (b)
         (let ((a b))
           (block nil
             (let ((* (list 1)))
               (declare (dynamic-extent *))
               (if b
                   (let ((j a))
                     (let ((* (list 1)))
                       (declare (dynamic-extent *))
                       (if b (return 44))
                       (setf a nil))
                     (let ((z j)) z))
                   (eval 2))))))
      ((33) 44)))

(with-test (:name :substitute-single-use-lvar-unknown-exits.4)
  (checked-compile-and-assert
   ()
   `(lambda (a)
      (block nil
        (flet ((f ()
                 (let ((p (1+ a)))
                   (let ((* (list 1)))
                     (declare (dynamic-extent *))
                     (if a
                         (return 45)))
                   p)))
          (let ((* (lambda ()
                     (return (eval a)))))
            (f)))))
   ((33) 45)))

(with-test (:name :substitute-single-use-lvar-unknown-exits.5)
  (checked-compile-and-assert
   ()
   `(lambda (b c)
      (block nil
        (flet ((f ()
                 (return (catch 'c (block b b)))))
          (return
            (block b5
              (let ((o c))
                (setf c
                      (catch 'c
                        (flet ((g ()
                                 (return)))
                          (f))))
                (let ((x o)) x)))))))
   ((10 20) 10)))

(with-test (:name :substitute-single-use-lvar-unknown-exits.6)
  (checked-compile-and-assert
   ()
   `(lambda ()
      (block b
        (return-from b
          (let ((lv3 (random 10))
                *)
            (boole boole-1 lv3
                   (the integer
                        (catch 'ct4
                          (let ((x (list '*)))
                            (declare (dynamic-extent x))
                            (return-from b (eval x))))))))))
   (() 1)))

(with-test (:name :lambda-let-inline)
  (let ((fun (checked-compile
              `(lambda ()
                 (let ((x (lambda () 1)))
                   (funcall x))))))
    (assert (null (ctu:find-anonymous-callees fun)))
    (assert (= (funcall fun) 1))))

(with-test (:name :external-cast-deletion)
  (checked-compile-and-assert
   ()
   `(lambda (a c)
      (declare (notinline elt logior))
      (logior
       (if c
           (the integer (elt '(10 20) a))
           (let ((v1 (loop repeat 3 count t)))
             (declare (dynamic-extent v1))
             v1))))
   ((0 t) 10)
   ((1 nil) 3)))

(with-test (:name :fixnump-instance-ref-immediately-used)
  (checked-compile-and-assert
   ()
   `(lambda (a b c)
      (let (z)
        (and
         (typep
          (let ((y (let ((s (cons a b)))
                     (declare (dynamic-extent s))
                     (cdr s))))
            (unwind-protect
                 (let ((s (list c)))
                   (declare (dynamic-extent s))
                   (setf z (car s))))
            y)
          'fixnum)
         z)))
   ((1 2 'a) 'a)))

(with-test (:name :fixnump-instance-ref-immediately-used.2)
  (checked-compile-and-assert
   ()
   `(lambda (a b c)
      (let* ((l (cons a b))
             (cdr (cdr l)))
        (setf (cdr l) c)
        (typep cdr 'fixnum)))
   ((1 2 'a) t)))

(with-test (:name :round-numeric-bound)
  (checked-compile-and-assert
   ()
   `(lambda (a c f)
      (declare (type (integer -1111868182375 1874303539234) a))
      (- (rem (funcall f) (max 23 (* 45092832376540563 a -4469591966)))
         (signum c)))
   ((1874303539234 2 (constantly 123)) 7)))

(with-test (:name :ir2-optimize-jumps-to-nowhere)
  (checked-compile-and-assert
   ()
   `(lambda (a)
      (declare (type fixnum a))
      (if (< a 0 a)
          (block a (shiftf a 1))
          0))
   ((0) 0)))

(with-test (:name :double-float-bits-stub)
  (checked-compile-and-assert
   ()
   `(lambda (x)
     (float-sign 5.0d0 (the double-float x)))
   ((3d0) 3d0)))

(with-test (:name :typep-word)
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (typep x 'sb-vm:word))
   ((1) t)
   (((1- (expt 2 sb-vm:n-word-bits))) t)
   (((expt 2 sb-vm:n-word-bits)) nil)
   ((-1) nil)
   (('a) nil)
   ((()) nil)
   (((1- most-negative-fixnum)) nil)))

(with-test (:name :fixnum-mod-p-word-descriptor)
  (checked-compile-and-assert
   ()
   `(lambda (a)
      (declare (type sb-vm:signed-word a))
      (typep a '(integer 0 ,(1- most-positive-fixnum))))
   (((1- most-positive-fixnum)) t)
   ((0) t)
   ((1) t)
   ((most-positive-fixnum) nil)
   (((1+ most-positive-fixnum)) nil)
   ((most-negative-fixnum) nil)
   (((1+ most-negative-fixnum)) nil)
   (((1- (expt 2 (1- sb-vm:n-word-bits)))) nil)
   ((-1) nil)))

(with-test (:name :check-bound-zero-safety-notes)
  (checked-compile-and-assert
      (:allow-notes nil
       :optimize '(:speed 3 :safety 0))
      `(lambda (a x y z)
         (declare (fixnum x y z)
                  ((simple-array t (*)) a)
                  (optimize (speed 3) (safety 0)))
         (aref a (+ x (- y z))))
    ((#(1 2 3) 1 0 0) 2)))

(with-test (:name :convert-mv-bind-to-let-multiple-uses)
  (checked-compile-and-assert
   ()
   `(lambda (f)
      (let* ((a (eval 1))
             (b (eval 2)))
        (multiple-value-bind (x y) (if f
                                       (values a 1)
                                       (values b 2))
          (values x y))))
   ((t) (values 1 1))
   ((nil) (values 2 2))))

(with-test (:name :substitute-single-use-lvar-multiple-uses)
  (checked-compile-and-assert
   ()
   `(lambda (f)
      (let* ((a (eval 1))
             (b (eval 2))
             (m (if f
                    (values a)
                    (values b))))
        m))
   ((t) 1)
   ((nil) 2)))

(with-test (:name :tn-ref-type-multiple-moves)
  (checked-compile-and-assert
   ()
   `(lambda  (a c)
      (declare (type (integer 546181490258163 937632934000433) c))
      (let ((v8 c))
        (multiple-value-bind (v9 v6)
            (if (/= a v8)
                (values 0 10983313414045189807)
                (values 0 c))
          (declare (ignore v9))
          (loop repeat 2
                do (eval v6))
          v6)))
   ((0 571816791704489) 10983313414045189807)))

(with-test (:name :substitute-single-use-lvar-cast-chains)
  (checked-compile-and-assert
   ()
   `(lambda (f a b)
      (labels ((fun (z)
                 (let ((m z))
                   ;; delays type derivation of FUN as FIXNUM until constraint propagation
                   ;; making sure SUBSTITUTE-SINGLE-USE-LVAR runs first.
                   (if (typep m 'fixnum)
                       m
                       0))))
        (declare (inline fun))
        (let* ((a (fun a))
               (b (fun b)))
          (let ((m
                  (if f
                      (the fixnum (the integer a))
                      (the fixnum (the integer b)))))
            m))))
   ((t 1 2) 1)
   ((nil 1 2) 2)))

(with-test (:name :m-v-bind-multi-use-unused-values.1)
  (multiple-value-bind (calls f)
      (ctu:ir1-named-calls
            '(lambda (z m)
              (multiple-value-bind (a b)
                  (if z
                      10
                      (values (sxhash m) m))
                (declare (ignore a))
                b)))
    (assert (eql (funcall f t 33) nil))
    (assert (eql (funcall f nil 33) 33))
    (assert (not calls))))

(with-test (:name :m-v-bind-multi-use-unused-values.2)
  (multiple-value-bind (calls f)
      (ctu:ir1-named-calls
           '(lambda (z m)
              (multiple-value-bind (a b c)
                  (if z
                      (values 10)
                      (values (sxhash m) m))
                (declare (ignore a))
                (list b c))))
    (assert (equal (funcall f t 33) '(nil nil)))
    (assert (equal (funcall f nil 33) '(33 nil)))
    (assert (not calls))))

(with-test (:name :m-v-bind-multi-use-unused-values.3)
  (multiple-value-bind (calls f)
      (ctu:ir1-named-calls
           '(lambda (z m)
              (multiple-value-bind (a b)
                  (if z
                      10
                      (values m (sxhash m)))
                (declare (ignore b))
                a)))
    (assert (eql (funcall f t 33) 10))
    (assert (eql (funcall f nil 33) 33))
    (assert (not calls))))

(with-test (:name :m-v-bind-multi-use-unused-values.4
            :skipped-on :sbcl)
  (multiple-value-bind (calls f)
      (ctu:ir1-named-calls
            '(lambda (z m)
              (nth-value 1
               (if z
                   (funcall (the function z))
                   (values (sxhash m) m)))))
    (assert (eql (funcall f (lambda () (values 1 22)) 33) 22))
    (assert (eql (funcall f nil 34) 34))
    (assert (not calls))))

(with-test (:name :m-v-bind-multi-use-unused-values.5
            :skipped-on :sbcl)
  (multiple-value-bind (calls f)
      (ctu:ir1-named-calls
            '(lambda (z m)
              (nth-value 1
               (if z
                   (funcall (the function z))
                   (sxhash m)))))
    (assert (eql (funcall f (lambda () (values 1 22)) 33) 22))
    (assert (eql (funcall f nil 34) nil))
    (assert (not calls))))

(with-test (:name :m-v-bind-multi-use-variable-type-change)
  (checked-compile-and-assert
   ()
   '(lambda (p)
     (when (position #\a (the (or (simple-string 1) (simple-string 2)) p))
       nil))
   (("a") nil)
   (("ab") nil)))

(with-test (:name :array-element-type-cons.1)
  (checked-compile-and-assert
   (:allow-notes nil)
   '(lambda (vector)
     (declare ((or (simple-array (unsigned-byte 32) (2))
                   (simple-array (unsigned-byte 32) (4))) vector))
     (make-array 10 :element-type (array-element-type vector)))
   (((make-array 2 :element-type '(unsigned-byte 32)))
    '(unsigned-byte 32) :test (lambda (x y)
                                (equal (array-element-type (car x)) (car y))))))

(with-test (:name :array-element-type-cons.2)
  (checked-compile-and-assert
   (:allow-notes nil)
   '(lambda (vector)
     (declare ((and (simple-array (unsigned-byte 32) (2))
                (satisfies eval)) vector))
     (make-array 10 :element-type (array-element-type vector)))
   (((make-array 2 :element-type '(unsigned-byte 32)))
    '(unsigned-byte 32) :test (lambda (x y)
                                (equal (array-element-type (car x)) (car y))))))

(with-test (:name :about-to-modify-symbol-value-relax-fun-type)
  (let* ((compiled-lambda (compile nil '(lambda (&rest x) x 'hi)))
         (sb-c::*compiler-error-bailout*
          (lambda (&optional c) (error c))))
    (declare (notinline set))
    (set 'sb-c::*compiler-error-bailout* compiled-lambda)))

(with-test (:name :self-evaluating-p-not)
  (let ((s (gensym)))
    (set s 9)
    (unintern s)
    (import s 'keyword)
    (assert (not (sb-int:self-evaluating-p s)))))

(with-test (:name :lea-modfx-constant-folding)
  (checked-compile-and-assert
      ()
      '(lambda (c)
        (if (if c
                c
                (if 444
                    nil
                    99))
            11
            (logand 3
                    (logxor
                     (* 5
                        (if c
                            0
                            (ash most-positive-fixnum -2)))
                     3))))
    ((t) 11)
    ((nil) 0)))

(with-test (:name :setup-environment-tn-conflicts)
  (checked-compile-and-assert
      ()
      '(lambda (z)
        (let ((c 0))
          (flet ((bar ()
                   (let ((m (eval :bad)))
                     (eval m)
                     (and m c))))
            (declare (notinline bar))
            (cond (z
                   (setf c 10)
                   (bar))
                  (44)))))
    ((t) 10)
    ((nil) 44)))

(with-test (:name :setup-environment-tn-conflicts.2)
  (checked-compile-and-assert
      ()
      '(lambda (z)
        (let ((c 0)
              (b 0)
              (a 0)
              (d 0))
          (labels ((bar ()
                     (let ((m (eval :bad)))
                       (eval m)
                       (if m
                           (values a b c d))))
                   (jam ()
                     (multiple-value-list (bar))))
            (declare (notinline bar
                                jam))
            (cond (z
                   (setf a 10
                         c 10
                         b 10
                         d 10)
                   (jam))
                  (44)))))
    ((t) '(10 10 10 10) :test #'equal)
    ((nil) 44)))

(with-test (:name :setup-environment-tn-conflicts.3)
  (checked-compile-and-assert
      ()
      '(lambda (b)
        (flet ((%f7 ()
                 (flet ((%f10 ()
                          (setf b b)))
                   (declare (dynamic-extent #'%f10))
                   (funcall (eval #'%f10)))))
          (declare (notinline %f7))
          (%f7)))
    ((10) 10)))

(with-test (:name :dead-sets)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (logtest
          ((lambda (v &rest args)
             (declare (ignore args))
             (setf v
                   ((lambda (&rest args) (declare (ignore args)) (error "")) v)))
           1)
          1))
    (() (condition 'simple-error))))

(with-test (:name :inlining-multiple-refs)
  (checked-compile
   `(lambda (x)
      (labels ((%s (y &rest r)
                 (some
                  (lambda (r) (apply #'%s (1+ y) r))
                  (apply #'eql x r))))
        (%s 1)))))

(with-test (:name :update-lvar-dependencies-delete-lvar)
  (checked-compile-and-assert
      ()
      '(lambda (x y)
        (let ((x x))
          (block nil
            (flet ((proc (thing)
                     (when thing
                       (return (eval thing)))))
              (declare (inline proc))
              (if x
                  (proc y)
                  (proc y)))))
        t)
    ((1 2) t)))

(with-test (:name :car-type-on-or-null)
  (assert
   (equal (sb-kernel:%simple-fun-type
           (checked-compile
            '(lambda (x)
              (declare (type (or null (cons fixnum)) x))
              (if x
                  (car x)
                  0))))
          '(function ((or null (cons fixnum t))) (values fixnum &optional)))))

(with-test (:name :nlx-entry-zero-values)
  (checked-compile-and-assert
      ()
      '(lambda (x)
        (multiple-value-call (lambda (&optional x) x)
          (block nil
            (funcall (eval (lambda ()
                             (return (if x
                                         (values)
                                         10))))))))
    ((t) nil)
    ((nil) 10)))

(with-test (:name :find-test-to-eq-with-key)
  (checked-compile-and-assert
   ()
   '(lambda (x)
     (position (1- (expt x 64)) '((#xFFFFFFFFFFFFFFFF)) :key #'car))
   ((2) 0)
   ((1) nil)))

(with-test (:name :maybe-infer-iteration-var-type-on-union)
  (checked-compile-and-assert
      (:allow-notes nil :optimize '(:speed 3 :compilation-speed 1 :space 1))
      `(lambda (a)
         (loop repeat (if a 2 0) count 1))
    ((t) 2)
    ((nil) 0)))

(with-test (:name :maybe-infer-iteration-var-type-on-union.2)
  (checked-compile-and-assert
      ()
      `(lambda (a)
         (let ((v4 (the (or (single-float (1.0) (3.0)) (single-float 4.0 5.0)) a)))
           (incf v4 1.0)))
      ((4.0) 5.0)))

(with-test (:name :derive-array-rank-negation)
  (checked-compile-and-assert
   ()
   `(lambda (a)
      (declare ((not (simple-array * (* *))) a))
      (eql (array-rank a) 2))
   (((make-array '(2 2) :adjustable t)) t))
  (checked-compile-and-assert
   ()
   `(lambda (a)
      (declare ((not (simple-array fixnum (* *))) a))
      (eql (array-rank a) 2))
   (((make-array '(2 2))) t))
  (checked-compile-and-assert
   ()
   `(lambda (a)
      (declare ((not (and (array * (* *)) (not simple-array))) a))
      (eql (array-rank a) 2))
   (((make-array '(2 2))) t)))

(with-test (:name :derive-array-rank-negation.2)
  (assert
   (type-specifiers-equal
          (sb-kernel:%simple-fun-type
           (checked-compile
            '(lambda (x)
              (declare ((and simple-array
                         (not (simple-array * (* *))))
                        x))
              (eql (array-rank x) 2))))
          '(function ((and simple-array (not (simple-array * (* *)))))
            (values null &optional)))))

(with-test (:name :known-fun-no-fdefn)
  (assert (equal (ctu:find-code-constants (checked-compile '(lambda () #'+))
                                          :type 'function)
                 (list #'+))))

(with-test (:name :double-float-p-weakening)
  (checked-compile-and-assert
   (:optimize '(:speed 3 :safety 1))
   '(lambda (x)
     (declare (double-float x))
     x)
   ((0.0) (condition 'type-error))
      ((1d0) 1d0)))

(declaim (inline inline-fun-arg-mismatch))
(defun inline-fun-arg-mismatch (x)
  (declare (optimize (debug 0)))
  x)

(with-test (:name :inline-fun-arg-mismatch)
  (checked-compile-and-assert
      (:allow-warnings '(or sb-int:local-argument-mismatch
                         #+interpreter simple-warning)) ;; why?
      '(lambda ()
        (multiple-value-call #'inline-fun-arg-mismatch 1 2))
    (() (condition 'program-error))))

(with-test (:name :principal-lvar-ref-use-loop)
  (checked-compile-and-assert ()
   '(lambda (vector)
     (labels ((f (count)
                (when (< (aref vector 0) count)
                  (f count))))))
   ((1) nil)))

(with-test (:name (:mv-call :more-arg))
  (checked-compile-and-assert
   ()
   '(lambda (&rest rest)
     (multiple-value-bind (a b c) (values-list rest)
       (declare (ignore c))
       (list a b)))
   ((1 3) '(1 3) :test #'equal)))

(with-test (:name (:mv-call :more-arg-unused))
  (checked-compile-and-assert
   ()
   '(lambda (&rest rest)
     (multiple-value-bind (a b) (values-list rest)
       (list a b)))
   (() '(nil nil) :test #'equal)
   ((1) '(1 nil) :test #'equal)
   ((1 3) '(1 3) :test #'equal)))

(with-test (:name :truncate-deriver-on-number-type)
  (checked-compile-and-assert
   ()
   '(lambda (i)
     (truncate
      (labels ((f (&optional (o i))
                 (declare (ignore o))
                 (complex 0 0)))
        (declare (dynamic-extent (function f)))
        (the integer
             (multiple-value-call #'f (values))))
      3))
   ((0) (values 0 0))))

(with-test (:name :signum-type-deriver)
  (checked-compile-and-assert
   ()
   '(lambda (n)
     (typep (signum n) 'complex))
   ((#C(1 2)) t)
   ((1d0) nil)
   ((10) nil)))

(with-test (:name :array-header-p-derivation)
  (checked-compile-and-assert
   ()
   '(lambda (q)
     (and (typep q '(not simple-array))
      (sb-kernel:array-header-p q)))
   ((10) nil)
   (((make-array 10 :adjustable t)) t)))

(with-test (:name :phase-type-derivation)
  (checked-compile-and-assert
   ()
   '(lambda (x)
     (= (phase (the (integer -1 0) x))
      (coerce pi 'single-float)))
   ((-1) t)
   ((0) nil)))

(with-test (:name :maybe-negate-check-fun-type)
  (checked-compile-and-assert
   ()
   '(lambda (m)
     (declare ((or (function (number)) (eql #.#'symbolp)) m))
     (the (member 3/4 4/5 1/2 #.#'symbolp) m))
   ((#'symbolp) #'symbolp)))

(with-test (:name :equal-to-eql)
  (let ((f (checked-compile
            `(lambda (x y)
               (equal (the hash-table x) y)))))
    (assert (not (ctu:find-code-constants f :type 'sb-kernel:fdefn))))
  (let ((f (checked-compile
            `(lambda (x y)
               (equalp (the function x) y)))))
    (assert (not (ctu:find-code-constants f :type 'sb-kernel:fdefn)))))

(with-test (:name :multiway-branch-duplicate-case)
  (let ((f (checked-compile '(lambda (b)
                              (case b
                                ((1 2) :good)
                                ((3 2) :bad)))
                            :allow-style-warnings t)))
    (assert (eq (funcall f 2) :good))))

(with-test (:name :modular-arith-type-derivers
                  :fails-on :ppc64)
  (let ((f (checked-compile
            `(lambda (x)
               (declare ((and fixnum
                              unsigned-byte) x)
                        (optimize speed))
               (rem x 10)))))
        (assert (not (ctu:find-code-constants f :type 'bignum)))))

(with-test (:name :deduplicated-fdefns)
  (flet ((scan-range (c start end)
           (let (dup-fdefns names)
             (loop for i from start below end
                   do (let ((obj (sb-kernel:code-header-ref c i)))
                        (when (sb-kernel:fdefn-p obj)
                          (let ((name (sb-kernel:fdefn-name obj)))
                            (when (member name names)
                              (push obj dup-fdefns))
                            (push name names)))))
             (assert (not dup-fdefns)))))
    (dolist (c (sb-vm:list-allocated-objects :all :type sb-vm:code-header-widetag))
      (sb-int:binding* (((start count) (ctu:code-header-fdefn-range c))
                        (end (+ start count)))
        ;; Within each subset of FDEFNs there should be no duplicates
        ;; by name. But there could be an fdefn that is in the union of the two sets.
        (scan-range c start end)
        (scan-range c end (sb-kernel:code-header-words c))))))

(with-test (:name :map-all-lvar-dests)
  (checked-compile-and-assert
   ()
   `(lambda (&key (pred (constantly 44)))
      (declare (type function pred))
      (funcall pred))
   (() 44)))

(with-test (:name (:lvar-fun-name :constant-leaf-not-constant-lvar-p))
  (assert (nth-value 1
                     (checked-compile
                      `(lambda ()
                         (funcall
                          (the (function (t) t)
                               ,(checked-compile '(lambda ())))))
                      :allow-warnings t
                      :allow-style-warnings t))))

(with-test (:name (:%logbitp :signed-and-unsigned))
  (checked-compile-and-assert
      ()
      `(lambda (p2)
         (declare (type (integer ,(expt -2 (1- sb-vm:n-word-bits))
                                 ,(1- (expt 2 sb-vm:n-word-bits))) p2))
         (logbitp 26 p2))
    ((3) nil)
    (((ash 1 26)) t)))

(with-test (:name :vop-return-constant-boxing)
  (checked-compile
   `(lambda (x)
      (declare (optimize speed))
      (setf (aref (the (simple-array double-float (*)) x) 0)
            10d0))
   :allow-notes nil)
  (checked-compile
   `(lambda (x)
      (declare (optimize speed))
      (setf (aref (the (simple-array sb-vm:word (*)) x) 0)
            (1- (expt 2 sb-vm:n-word-bits))))
   :allow-notes nil)
  (checked-compile
   `(lambda (x y)
      (declare (optimize speed))
      (setf (svref y 0)
            (setf (aref (the (simple-array double-float (*)) x) 0)
                  10d0)))
   :allow-notes nil)
  (checked-compile
   `(lambda (f a)
      (declare (optimize speed))
      (funcall (the function f)
               1 2 3 4 5 6 7 8 9 10
               (setf (aref (the (simple-array double-float (*)) a) 0)
                     10d0)))
   :allow-notes nil))

(with-test (:name :make-constant-tn-force-boxed)
  (checked-compile-and-assert
   ()
   `(lambda (c)
      (declare (type character c))
      (list 1 1 1 1 1 1 1 1 1 1 1 (the (eql #\() c)))
   ((#\() '(1 1 1 1 1 1 1 1 1 1 1 #\() :test #'equal)))

(with-test (:name :jump-over-move-coercion
            :serial t
            :skipped-on :interpreter)
  (let ((f (checked-compile
            '(lambda (number)
              (declare ((or fixnum double-float single-float) number))
              (cond ((typep number 'double-float)
                     number)
                    ((typep number 'single-float)
                     (coerce number 'double-float))
                    ((typep number 'fixnum)
                     (coerce number 'double-float)))))))
    (ctu:assert-no-consing (funcall f 1d0)))
  (let ((f (checked-compile
            '(lambda (v number)
              (declare ((or fixnum double-float single-float) number))
              (setf (svref v 0)
               (cond ((typep number 'double-float)
                      number)
                     ((typep number 'single-float)
                      (coerce number 'double-float))
                     ((typep number 'fixnum)
                      (coerce number 'double-float))))))))
    (let ((v (vector 0)))
      (ctu:assert-no-consing (funcall f v 1d0)))))

(with-test (:name :jump-over-move-coercion-match-type)
  (checked-compile-and-assert
      ()
      `(lambda (a b)
         (declare (type (or sb-vm:word sb-vm:signed-word) a))
         (declare (type (and fixnum unsigned-byte) b))
         (lognand (max 0 a) b))
    (((expt 2 (1- sb-vm:n-word-bits)) #xFFFFFF) -1)
    (((1- (expt 2 (1- sb-vm:n-word-bits))) #xFFFFFF) -16777216)))

#+#.(cl:if (cl:gethash 'sb-c:jump-table sb-c::*backend-template-names*)
           '(:and)
           '(:or))
(with-test (:name :typecase-to-case-preserves-type)
  (let ((f (checked-compile
            '(lambda (x)
              ;; This illustrates another possible improvement-
              ;; there are not actually 6 different slot indices
              ;; that we might load. Some of them are the same
              (typecase x
                (sb-pretty:pprint-dispatch-table (sb-pretty::pp-dispatch-entries x))
                (sb-impl::comma (sb-impl::comma-expr x))
                (sb-vm:primitive-object (sb-vm:primitive-object-slots x))
                (sb-kernel:defstruct-description (sb-kernel::dd-name x))
                (sb-kernel:lexenv (sb-c::lexenv-vars x))
                (broadcast-stream (broadcast-stream-streams x))
                (t :none))))))
    ;; There should be no #<layout> referenced directly from the code header
    ;; (which implies that no type-check occurs when accessing a structure instance).
    ;; There is of course a vector of layouts in there to compare against.
    (assert (not (ctu:find-code-constants f :type 'sb-kernel:layout)))
    ;; The function had better work.
    (assert (eq (funcall f 'wat) :none))
    (assert (equal (funcall f (make-broadcast-stream *error-output*))
                   (list *error-output*)))))


(with-test (:name :=-interval-derivation-and-complex)
  (checked-compile-and-assert
      ()
      `(lambda (p1)
         (declare ((complex (integer -1 -1)) p1))
         (= -1 p1))
    ((#C(-1 -1)) nil)))

(with-test (:name :cmov-move-hoisting)
  (checked-compile-and-assert
      ()
      `(lambda (p)
         (declare ((or (eql 0.0)
                       sb-vm:word) p))
         (if (> p 51250)
             p
             1))
    ((0.0) 1)
      ((#1=(1- (expt 2 sb-vm:n-word-bits))) #1#))
  (checked-compile-and-assert
   ()
   `(lambda (p)
      (declare (type (member 4801112936349103672 -9474680540642044437) p))
      (max 0 p -1.0))
   ((4801112936349103672) 4801112936349103672)
   ((-9474680540642044437) 0)))

(with-test (:name :logior-derive-type-widening-tail-set-types)
  (checked-compile-and-assert
      ()
      `(lambda (a b c)
         (labels ((q (x y)
                    (let ((* (lambda () x y)))
                      (the integer a)))
                  (p ()
                    (logior (apply #'q (list a b))
                            (if b
                                (return-from p (q b c))
                                1))))
           (if c
               0.0
               (p))))
    ((44 nil nil) 45)
    ((3 2 1) 0.0)
    ((30 2 nil) 30)))

(with-test (:name :if-eq-optimization-consistency)
  (let ((sb-c::*check-consistency* t))
    (checked-compile-and-assert
     ()
     `(lambda ()
        (eval (and (if (eval 0) (eval 0) (eval 0)) t)))
     (() t))))

(with-test (:name :make-array-half-finished-transform)
  (checked-compile-and-assert
      (:allow-warnings t)
      `(lambda ()
         (make-array 6 :fill-pointer 33))
    (() (condition '(not program-error)))))

(with-test (:name :nested-if+let)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (let (x)
           (when x
             (setq x 1))
           (let ((y (if x
                        t
                        nil)))
             (if y
                 y
                 (let ((x x))
                   x)))))
      (() nil)))

(with-test (:name :let-var-immediately-used-p-deleted-lambda)
  (checked-compile-and-assert
   ()
   `(lambda (c)
      (if (and nil
               (or
                (zerop (count (unwind-protect 1) '(1)))
                c))
          1
          0))
   ((2) 0)))

(with-test (:name :dce-local-functions)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (block out
           (labels ((mmm (z vars)
                      (when vars
                        (mmm z vars))))
             (mmm 1 (progn
                      (dotimes (a 1) (return-from out 10))
                      (dotimes (b 3) (catch 'b))))
             (dotimes (c 3) (catch 'c)))))
    (() 10)))

(with-test (:name :dce-more-often)
  (checked-compile-and-assert
   ()
   `(lambda (a)
      (+ 1
         (if t
             0
             (progn
               (tagbody
                p
                  (tagbody
                     (let ((a (lambda () (go o))))
                       (declare (special a)))
                   o)
                  (when (< a 1)
                    (go p)))
               2))))
    ((1) 1)
    (:return-type (values (integer 1 1) &optional))))

(with-test (:name :dce-more-often.2)
  (checked-compile-and-assert
   ()
   `(lambda (b)
      (declare (fixnum b))
      (- (case 0
           (1
            (dotimes (i 1 b) (ignore-errors)))
           (t 0))))
    ((3) 0)
    (:return-type (values (integer 0 0) &optional))))

(with-test (:name :ir1-optimize-constant-fold-before-giving-up)
  (checked-compile-and-assert
   ()
   `(lambda (a)
      (+ 2 (- (let ((sum 0))
                (declare (type fixnum sum))
                (block nil
                  (tagbody
                   next
                     (cond ((>= sum '0)
                            (go end))
                           (a
                            (ceiling 1 (unwind-protect 2))
                            (incf sum)))
                     (go next)
                   end))
                sum))))
   ((1) 2)))

(with-test (:name :position-case-otherwise)
  (checked-compile-and-assert
      ()
      `(lambda (x)
         (position x '(a otherwise b t nil)))
    (('a) 0)
    (('otherwise) 1)
    ((nil) 4)
    ((t) 3)))

(with-test (:name :unreachable-component-propagate-let-args)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (let ((p 0))
           (flet ((f (&key)
                    (flet ((g (&optional
                                 (z
                                  (return-from f (+ (dotimes (i 0 0)) p))))
                             p))))))
           p))
    (() 0)))

(with-test (:name :dce-through-optional-dispatch)
  (checked-compile-and-assert
      ()
      `(lambda (x)
         (flet ((z (&optional a)
                  (declare (ignore a))
                  123))
           (let ((z #'z))
             (when x
               (unless x
                 (setf z 10)))
                   (funcall z))))
    ((nil) 123)
    ((t) 123)))

(with-test (:name :values-list+cons)
  (assert
   (equal (sb-kernel:%simple-fun-type
           (checked-compile
            `(lambda ()
               (values-list (cons 1 nil)))))
          '(function () (values (integer 1 1) &optional))))
  (assert
   (equal (sb-kernel:%simple-fun-type
           (checked-compile
            `(lambda (x) (values-list (list* x 1 x nil)))))
          '(function (t) (values t (integer 1 1) t &optional)))))

(with-test (:name :xeps-and-inlining)
  (checked-compile-and-assert
   ()
   `(lambda (args)
      (flet ((fun () args))
        (declare (inline fun))
        (multiple-value-call #'fun (values-list args))
        #'fun))))

(with-test (:name :split-let-ctran-kind)
  (checked-compile-and-assert
   ()
   `(lambda (a b)
      (let ((a-n (null a))
            (b-n (null b)))
        (cond (b-n 1)
              (a-n a)
              (t a))))
   ((nil nil) 1)
   ((nil t) nil)))

(with-test (:name :dead-component-unused-closure)
  (checked-compile-and-assert
   ()
   `(lambda ()
      (labels ((%f1 ())
               (%f2 (&key)
                 (flet ((%f3 ()
                          (unwind-protect 1)
                          (return-from %f2 (%f1)))))))
        (%f1)))
   (() nil)))

(with-test (:name :references-to-inline-funs-copied)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (and (inline-fun-arg-mismatch t)
              #'inline-fun-arg-mismatch))
    (() #'inline-fun-arg-mismatch)))

(with-test (:name :eliminate-dead-code-before-initial-dfo)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (block nil
          (flet ((f (&key (k1 (catch 'c)))
                   (max 0
                        (let ((v9 10))
                          (return))))))))
    (() nil)))

(with-test (:name :%coerce-callable-to-fun-movement)
  (checked-compile-and-assert
   ()
   `(lambda (y x)
      (let ((x (sb-kernel:%coerce-callable-to-fun x)))
        (when y
          (funcall x))))
    ((nil (make-symbol "UNDEF")) (condition 'undefined-function))))

(with-test (:name :jump-table-use-labels)
  (checked-compile-and-assert
   ()
   `(lambda (x m)
      (case x
        ((a b c)
         (if m
             (error ""))
         x)
        ((d e f)
         (eval 10)
         x)))
    (('a nil) 'a)
    (('d 30) 'd)))

(with-test (:name :dfo-deleted-lambda-home)
  (assert
   (nth-value 5 (checked-compile
                 `(lambda (c)
                    (flet ((f (&optional (o c))
                             (lambda (&key)
                               (+ (restart-bind nil (go missing-tag))
                                  (progv nil nil o)))))))
                 :allow-failure t))))


(with-test (:name :split-let-unused-vars)
  (checked-compile-and-assert
      ()
      `(lambda (x y)
         (let ((a
                 (if x y))
               (b)
               (c
                 (if y
                     x)))
           (declare (ignore b))
           (if c (if a a c))))
    ((t t) t)
    ((t nil) nil)
    ((nil t) nil)
    ((nil nil) nil)))

(with-test (:name :sequence-lvar-dimensions-on-arrays)
  (checked-compile-and-assert
      ()
      `(lambda (x a)
         (count a (make-string x :initial-element a)))
    ((10 #\a) 10)))

(with-test (:name :length-transform-on-arrays)
  (checked-compile-and-assert
   ()
   `(lambda () (length (make-sequence '(string *) 10 :initial-element #\a)))
   (() 10)))

(with-test (:name :constant-fold-unknown-types)
  (checked-compile-and-assert
   (:allow-style-warnings t)
   `(lambda ()
      (oddp (the (or a b) -1)))))

(with-test (:name :dead-code-no-constant-fold-errors)
  (assert
   (typep (nth-value 4
                     (checked-compile
                      `(lambda (z)
                         (when (and (eq z 0)
                                    (not (eq z 0)))
                           (/ 10 0)))))
          '(cons sb-ext:code-deletion-note null))))

(with-test (:name :unused-assignment)
  (flet ((try (expr &aux (warned 0))
           (handler-bind ((style-warning
                           (lambda (c)
                            (if (search "assigned but never read" (princ-to-string c))
                                (incf warned)
                                (error "That's unexpected")))))
             (multiple-value-bind (fun warn error)
               (let ((*error-output* (make-broadcast-stream))) (compile nil expr))
               (declare (ignore fun))
               (assert (and warn (not error) (eql warned 1)))))))
    (try '(lambda (x) (let* ((a (+ x 5)) (b a)) (setq b 3) (eval ''z))))
    ;; Even if the initializer is necessary to call, it's still warning-worthy.
    (try '(lambda (x) (let* ((a (+ x 5))
                             (b (opaque-identity a)))
                        (setq b 3)
                        (eval ''z))))
    (try '(lambda (x) (let* ((a (+ x 5)) (b a))
                        (setq b (opaque-identity 3))
                        (eval ''z)))))
  ;; This one uses the value of B
  (checked-compile '(lambda (x) (let* ((a (+ x 5)) (b a))
                                  (setq b (opaque-identity 3))))))

(with-test (:name :unconvert-tail-calls-terminate-block)
  (checked-compile-and-assert
   ()
   `(lambda (x y)
      (flet ((f ()
               (labels ((a ()
                          (error "~a" x))
                        (b ()
                          (a)))
                 (if nil
                     (b)
                     (if y
                         (a)
                         (b))))))
        (block nil
          (return (f)))))
   ((t t) (condition 'error))))

(with-test (:name :unconvert-tail-calls-terminate-block.2)
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (flet ((f ()
               (labels ((a ()
                          (error "foo ~a" x))
                        (b ()
                          (let (*)
                            (a))))
                 (if nil
                     (b)
                     (if nil
                         (a)
                         (if x
                             (a)
                             (b)))))))
        (f)
        10))
   ((t t) (condition 'error))))

(with-test (:name :fixnum-checking-boxing
                  :skipped-on (not :x86-64))
  (checked-compile
   `(lambda (x y)
      (declare (optimize speed)
               (fixnum x y))
      (the fixnum (+ x y)))
   :allow-notes nil))

(with-test (:name :ltn-analyze-mv-bind)
  (checked-compile-and-assert
   ()
   `(lambda ()
      (multiple-value-call #'list
        10 (apply #'values '(44 33d0))))
   (() '(10 44  33d0) :test #'equal)))


(with-test (:name :lp719585)
  ;; Iteration variables are always "used"
  (checked-compile '(lambda () (do (var) (t))))
  (checked-compile '(lambda () (do* (var) (t))))
  (checked-compile '(lambda () (do-all-symbols (var))))
  (checked-compile '(lambda () (do-external-symbols (var))))
  (checked-compile '(lambda () (do-symbols (var))))
  (checked-compile '(lambda () (dolist (var '(1 2 3))))))

(with-test (:name :key-default-type)
  (let ((name (gensym)))
    (proclaim `(ftype (function (double-float &key (:y double-float))) ,name))
    (checked-compile-and-assert
        (:optimize :default)
        `(sb-int:named-lambda ,name (x &key (y x))
           (values x y))
        ((1d0 :y nil) (condition 'error)))))

(with-test (:name :deleting-unreachable-floats)
  (let ((name (gensym)))
    (proclaim `(inline ,name))
    (eval `(defun ,name (&key (k (eval 0f0)))
             k))
    (checked-compile-and-assert
     (:allow-notes nil)
     `(lambda ()
        (,name :k 0f0))
     (() 0f0))))

(with-test (:name :no-*-as-type)
  (multiple-value-bind (fun errorp warnings)
      (checked-compile '(lambda (x) (the * x))
                       :allow-failure t :allow-warnings t)
    (declare (ignore fun))
    (assert errorp)
    (assert (= (length warnings) 1)))
  ;; (values t) parses into *wild-type* and has to be allowed
  ;; even though * which parses into *wild-type* isn't.
  (checked-compile '(lambda () (the (values t) t))))

(with-test (:name :hairy-data-vector-set-t-upgrade)
  (checked-compile
   '(lambda (x) (sb-kernel:hairy-data-vector-set
                 (the (simple-array symbol) x) 1 'hey))))

(with-test (:name :ir2-convert-reffer-no-lvar)
  (checked-compile-and-assert
   (:allow-style-warnings t)
   `(lambda (a)
      (/ (unwind-protect (if a
                             (values nil (cdr a))
                             (values 1 0))
           a)
         1))
   ((nil) 1)))

(with-test (:name :%eql-integer-fold)
  (checked-compile-and-assert
   ()
   `(lambda (d)
      (declare (type fixnum d))
      (or (find d '(-98 27749116333474161060))
          t))
   ((-98) -98)
   ((95) t)))

(with-test (:name :svref-with-addend+if-eq-immediate)
  (checked-compile-and-assert
   ()
   `(lambda (a d)
      (eql (svref a d) -276932090860495638))
   ((#(1 0) 0) nil)
   ((#(-276932090860495638) 0) t)))

(with-test (:name :zeroize-stack-tns)
  (checked-compile-and-assert
   ()
   `(lambda (a b d e)
      (declare (type fixnum a))
      (dpb
       (ash
        (truncate 562949953421316  (max 97 d))
        (min 81 (expt (boole boole-and e b) 2)))
       (byte 7 5)
       (dotimes (i 2 a)
         (count i #(61) :test '>=))))
   ((1 2 3 4) 1985)))

(with-test (:name :logtest-derive-type-nil)
  (checked-compile-and-assert
   (:allow-warnings t)
   `(lambda (c)
      (block nil
        (evenp (the integer (ignore-errors (return c))))))
   ((1) 1)))

(with-test (:name :cast-filter-lvar)
  (checked-compile-and-assert
   (:allow-warnings t)
   `(lambda ()
      (block nil
        (equal
         (the integer (tagbody
                         (let ((* (lambda () (go tag))))
                           (return))
                       tag))
         (the integer (block nil
                        (return))))))
   (() nil)))

;;; EXPLICIT-CHECK + ETYPECASE should not produce a error message
;;; which reveals whether type-checking on entry to a standard function
;;; was performed this way or that way.
(with-test (:name :etypecase-error-simplify)
  (let ((x (nth-value 1 (ignore-errors (logcount (opaque-identity #\a)))))
        (y (nth-value 1 (ignore-errors (oddp (opaque-identity #\a))))))
    (assert (string= (princ-to-string x) (princ-to-string y)))))

(with-test (:name :set-exclusive-or-inlined)
  (checked-compile-and-assert
   ()
   `(lambda (set1 set2)
      (declare (inline set-exclusive-or))
      (set-exclusive-or set1 set2))))

(declaim (inline inline-deletion-note))
(defun inline-deletion-note (x y)
  (if y
      10
      x))

(with-test (:name :inline-deletion-note)
  (checked-compile-and-assert
   (:allow-notes nil)
   `(lambda (x)
      (inline-deletion-note x t))
   ((t) 10)))

(with-test (:name :inline-type-mismatch)
  (checked-compile-and-assert
      (:allow-notes nil)
      `(lambda (x y)
         (car (inline-deletion-note x y)))
    (('(a) nil) 'a))
  (checked-compile-and-assert
      ()
      `(lambda (x y)
         (1+ (position x (the list y))))
    ((1 '(1)) 1)))

(with-test (:name :lvar-annotation-inline-type-mismatch)
  (checked-compile-and-assert
      ()
      `(lambda (x y)
         (sb-kernel:the* (float :use-annotations t) (inline-deletion-note x y)))
    ((1.0 nil) 1.0)))

(with-test (:name :cast-type-preservation)
  (assert
   (equal (caddr
           (sb-kernel:%simple-fun-type
            (checked-compile
             `(lambda (b)
                (declare ((integer 1 1000) b))
                (declare (optimize (space 0)))
                (gcd 2 b)))))
          '(values (integer 1 2) &optional))))

(with-test (:name :lvar-substituting-non-deletable-casts)
  (checked-compile-and-assert
   ()
   `(lambda (b)
      (the integer
           (let (*)
             (rem 2
                  (let ((m
                          (flet ((f ()
                                   (truncate (the (integer -10 0) b) -4)))
                            (f))))
                    (if (> m 1)
                        1
                        m)))))
      10)
   ((-10) 10)))

(with-test (:name :convert-mv-bind-to-let-no-casts)
  (checked-compile-and-assert
   ()
   `(lambda (a)
      (declare (type (integer 7693489 168349189459797431) a))
      (max
       (floor a
              (min -14
                   (loop for lv3 below 3
                         sum (mod 77196223293181
                                  (max 75 (mod a (min -57 lv3)))))))))
   ((8000000) -571429)))

(with-test (:name :values-length-mismatch)
  (checked-compile-and-assert
   (:allow-style-warnings t :optimize :default)
   `(lambda (a)
      (declare (values t &optional))
      (when a
        (values 1 2)))
   ((nil) nil)
   ((t) (condition 'type-error))))

(with-test (:name :substitute-single-use-lvar-type-cast-movement)
  (checked-compile-and-assert
   ()
   `(lambda (a)
      (block nil
        (let ((x (multiple-value-prog1 a)))
          (when (< a 0)
            (return :good))
          (if (minusp x)
              1
              (+ x 1)))))
   ((-1) :good)
   ((0) 1)))

(with-test (:name :fold-ash-mod-0)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (loop for i below 3 sum
               (ldb (byte 6 6)
                    (ash i (mask-field (byte 5 8) i)))))
    (() 0)))

(with-test (:name :substitute-single-use-lvar-type-multiple-uses)
  (checked-compile-and-assert
   ()
   `(lambda (c)
      (let ((z
              (ceiling
               (truncate 655
                         (min -7
                              (if c
                                  -1000
                                  3)))
               3)))
        z))
   ((t) 0)
   ((nil) -31)))

(with-test (:name :division-by-multiplication-type-derivation)
  (assert
   (type-specifiers-equal
    (caddr
     (sb-kernel:%simple-fun-type
      (checked-compile
       `(lambda (c)
          (declare (optimize speed))
          (ceiling
           (truncate 65527
                     (min -78
                          (if c
                              -913097464
                              5)))
           39)))))
    '(values (or (integer -21 -21) (integer 0 0)) (integer #+(or arm64 x86-64) -21
                                                   #-(or arm64 x86-64) -38 0)
      &optional)))
  (assert
   (type-specifiers-equal
    (caddr
     (sb-kernel:%simple-fun-type
      (checked-compile
       `(lambda (c)
          (declare (optimize speed))
          (ceiling
           (truncate 65527
                     (min 78
                          (if c
                              913097464
                              5)))
           39)))))
    '(values (or (integer 22 22) (integer 337 337)) (integer -38 -1) &optional))))

(with-test (:name :boundp-ir2-optimizer)
  (checked-compile-and-assert
   ()
   `(lambda (v)
      (flet ((f (s)
               (when (boundp s)
                 (symbol-value s))))
        (f v)
        (f v)
        v))
   ((t) t)))

(with-test (:name :nfp-in-unwinding)
  (catch 'z
    (checked-compile-and-assert
        ()
        `(lambda (x y f)
           (declare (double-float x y))
           (block nil
             (let ((z (+ x y)))
               (unwind-protect  (funcall f)
                 (return (+ z 1d0))))))
      ((4d0 1d0 (lambda () (throw 'z 1))) 6d0))))

(with-test (:name :ir1-optimize-if-same-target-type-derivation)
  (checked-compile-and-assert
      ()
      `(lambda (b c)
         (declare (notinline equal))
         (multiple-value-bind (v7 v2)
             (if (equal 0 0)
                 (values c 0)
                 (values b 0))
           (declare (ignore v2))
           (tagbody (progn v7))
           b))
    ((1 2) 1)))

(with-test (:name :delete-let-source-paths)
  (checked-compile-and-assert
      ()
      `(lambda (a)
         (declare (type (member -3 -54972 3) a))
         (values (floor -98740440 a)))
    ((-3) 32913480)
    ((3) -32913480)
    ((-54972) 1796)))

(with-test (:name :unused-debug-tns)
  (checked-compile-and-assert
      ()
      `(lambda (d)
         (flet ((f (x)
                  (unwind-protect d
                    (eval x))))
           (dotimes (i 3)
             (f (1+ most-positive-fixnum)))))
    ((3) nil)))

(with-test (:name :exit-becomes-single-value)
  (checked-compile-and-assert
      ()
      `(lambda (x z)
         (max
          (block nil
            (flet ((x () (return (floor 1020 z))))
              (funcall x #'x))
            nil)
          10))
    (((lambda (x) (funcall x)) 4) 255)))

(with-test (:name :principal-lvar-single-valuify-exit)
  (checked-compile-and-assert
   ()
   `(lambda ()
      ((lambda (a)
         (flet ((a ()
                  (let ((v3 a))
                    (block nil (truncate (flet ((b ()
                                                  (return (block b3 (values 1 v3)))))
                                           (declare (inline b))
                                           (b)))))))
           (declare (inline a))
           (values (a))))
       t))
   (() 1)))

(with-test (:name :%coerce-callable-for-call-with-casts
            :skipped-on (not :call-symbol))
  (multiple-value-bind (calls f)
      (ctu:ir1-named-calls
            `(lambda (x y)
              (apply x 1 2 y)))
    (assert (equal (funcall f #'list '(3)) '(1 2 3)))
    (assert (equal calls '(x)))))

(with-test (:name :local-fun-type-check-eliminatetion)
  (let ((fun (checked-compile '(lambda ()
                                (flet ((f (x)
                                         (declare (fixnum x))
                                         (1+ x)))
                                  (declare (inline f))
                                  (funcall
                                   (the (function (&optional fixnum)) #'f)
                                   10))))))
    (assert (= (sb-kernel:code-n-entries (sb-kernel:fun-code-header fun))
               1))))

(with-test (:name :%cleanup-point-transform)
  (checked-compile-and-assert
   ()
   `(lambda (a b c)
      (declare ((integer -14 49702337) a)
               ((integer -5376440588342 5921272101558) b)
               ((integer 3395101368955 8345185767296289) c))
      (if (and (< c b) (> a b))
          (progv nil
              (list 288230376151711735 c)
            (restart-bind nil a))
          c))
   ((49702337 5921272101558 8345185767296289) 8345185767296289)))

;;; Test from git rev e47ffa8855d4139f88f5982fe4b82a05c3498ed3.
;;; I have absolutely zero understanding of what this was doing,
;;; but the are bunch of "undefined variable" warnings, so it can't
;;; go at toplevel in a .cload test.
(with-test (:name :bug-226)
  (with-scratch-file (lisp "lisp")
    (with-open-file (f lisp :direction :output)
      (write '(defun bug226 ()
  (declare (optimize (speed 0) (safety 3) (debug 3)))
  (flet ((safe-format (stream string &rest r)
           (unless (ignore-errors (progn
                                    (apply #'format stream string r)
                                    t))
             (format stream "~&foo ~S" string))))
    (cond
      ((eq my-result :ERROR)
       (cond
         ((ignore-errors (typep condition result))
          (safe-format t "~&bar ~S" result))
         (t
          (safe-format t "~&baz ~S (~A) ~S" condition condition result)))))))
             :stream f :readably t))
    (with-scratch-file (fasl "fasl")
      (compile-file lisp :output-file fasl))))

;;; I think these tests had to be present in a COMPILE-FILE (as opposed to COMPILE)
;;; to prove that the bug was fixed.
;;; Anway it's no longer going to be allowed to have deliberately bad code in '.cload'
;;; files, because any condition of type warnings or error is considered failure
;;; of the compile step.
(with-test (:name :lp-1276282)
  (with-scratch-file (lisp "lisp")
    (with-open-file (f lisp :direction :output)
      ;; from git rev feb31fb6cfc8f89e2d75b5f2cc2ee569ac975033
      (format f "(lambda () (the string (+ 1 x)))~%")
      ;; from git rev fbea35e879891723259dfa55589b498228390bb9
      (format f
"(lambda ()
  (macrolet ((x (&rest args)
               (declare (ignore args))
               'a))
    (let (a)
      (declare (type vector a))
      (x #.#'list))))~%"))
    (with-scratch-file (fasl "fasl")
      (compile-file lisp :output-file fasl))))

(with-test (:name :substitute-single-use-lvar-mv-cast)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (let ((r (random 10))
               (x (list 1)))
           (declare (special x)
                    (dynamic-extent x))
           (throw 'c (the (integer 0 10) r))))))

(with-test (:name :list-ir2-convert)
  (checked-compile '(lambda ()
                     (declare (notinline list +))
                     (list (loop for i below 2 count t)))))

(with-test (:name :bignump-integer-<)
  (checked-compile-and-assert
   ()
   `(lambda (a)
      (declare (type integer a))
      (if (and (typep a 'bignum) (< a 0))
          t
          nil))
   ((-1) nil)
   (((- (expt 2 300))) t)))

(with-test (:name :cmov-branch)
  (checked-compile-and-assert
   ()
   `(lambda (x y)
      (assert
       (let ((res nil))
         (when x (setf res (not res)))
         (when y (setf res (not res)))
         (not res))))
   ((1 2) nil)
   ((nil nil) nil)))

(with-test (:name :constant-type-proclamation)
  (ctu:file-compile
   `((defconstant +foo+ 4)

     (defun bar () +foo+)

     (declaim (type integer +foo+)))
   :load t)
  (assert (eq (funcall 'bar) 4)))

(with-test (:name :if-split-let-blocks)
  (checked-compile-and-assert
      ()
      `(lambda (a e)
         (labels ((f1 (b)
                    (map nil
                         (lambda (n)
                           (return-from f1 n))
                         b)
                    nil)
                  (f (n)
                    (let ((x (eval e))
                          (y (f1 n)))
                      (if y
                          y
                          x))))
           (f a)))
    ((() 1) 1)
    (('(2) 1) 2)))

(with-test (:name :duplicate-more-local-tn-overflow)
  (let ((vars (loop repeat 200 collect (gensym)))
        (args (loop repeat 201 for i from (random 30000)
                    collect i)))
    (assert
     (equal
      (apply
       (compile
        ()
        `(lambda (a ,@vars)
           (list a a ,@vars)))
       args)
      (cons (car args) args)))))

(with-test (:name :aref-single-value-type)
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (aref (the (values (and (not simple-array) vector)) x) 0))
   (((make-array 10 :adjustable t :initial-element 3)) 3)))

(with-test (:name :restoring-tns-after-cleanups)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (declare (notinline values))
         (unwind-protect 1
           (let ((a (list 'list)))
             (declare (dynamic-extent a))
             (unwind-protect 1 (eval a)))
           (eval 1)
           (eval 2)))
    (() 1)))

(defun noflush-symbol-function ()
  (declare (optimize safety))
  (if (functionp (symbol-function '#:notathing)) 1))
(defun flush-symbol-function ()
  (if (functionp (symbol-function '#:notathing)) 1))
(with-test (:name :flush-symbol-function :skipped-on :interpreter)
  (assert (ctu:find-code-constants #'noflush-symbol-function))
  (assert (not (ctu:find-code-constants #'flush-symbol-function))))

(with-test (:name :symbolp-other-pointer)
  (checked-compile-and-assert
      ()
      `(lambda (x)
         (declare ((or symbol bit-vector) x))
         (the symbol x))
    ((t) t)))

(with-test (:name :non-nil-symbolp-other-pointer)
  (checked-compile-and-assert
      ()
      `(lambda (x)
         (declare ((or bignum symbol) x))
         (sb-kernel:non-null-symbol-p x))
    ((t) t)
    (((1+ most-positive-fixnum)) nil)
    ((nil) nil)))

(with-test (:name :list-constant-coalesce)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (list -13303942049971317088
               -6714119381493
               -13303942049971317088))
    (() '(-13303942049971317088 -6714119381493 -13303942049971317088) :test #'equal)))

(with-test (:name :list-constant-coalesce.2)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (list -3819610816126750017 -7639221632253500034))
    (() '(-3819610816126750017 -7639221632253500034) :test #'equal))
  (checked-compile-and-assert
      ()
      `(lambda ()
         (list -7639221632253500034 -3819610816126750017))
    (() '(-7639221632253500034 -3819610816126750017) :test #'equal)))

(with-test (:name :constraint-loop)
  (checked-compile-and-assert
      ()
      `(lambda (b)
         (let ((v (elt '(2444 2740 3237 8155 3296 7304 7612 2949) b)))
           (progv '(*) (list (ceiling v 40))
             *)))
    ((3) 204)))

(with-test (:name :unused-local-fun-results)
  (let ((f  `(lambda (x)
               (flet ((x ()
                        (expt x x)))
                 (x)
                 (x)
                 10))))
    (assert (not (ctu:ir1-named-calls f)))))

(with-test (:name :ir2opt-tns-without-sc)
  (checked-compile-and-assert
      ()
      `(lambda (a)
         (boole boole-set (the rational a) a))
    ((1) -1)))

(with-test (:name set-slot-old-p-optionals)
  (checked-compile-and-assert
   ()
   `(lambda (x &key)
      (let ((list (list 1)))
        (setf (car list) x)
        list))
   ((2) '(2) :test #'equal)))

(with-test (:name :tn-ref-type-ir2opt)
  (checked-compile-and-assert
   ()
   `(lambda (p)
      (the unsigned-byte
           (the (or (array * (1)) real) p)))
   ((5) 5)))

(with-test (:name :qword-to-dword-cut)
  (checked-compile-and-assert
   (:allow-warnings t)
   `(lambda (a b)
      (declare (fixnum b))
      (logxor
       (lognor (setq a -336272099380508247)
               (shiftf b (logorc1 1073741832 a)))
       (the (integer -504635362412860905 -99686857090873309) (lognand b 11))))))

(with-test (:name :not-folded-vops)
  (assert
   (type-specifiers-equal
    (caddr
     (sb-kernel:%simple-fun-type
      (checked-compile
       `(lambda ()
          (floor
           (dpb 42
                (byte 15 7)
                (block b
                  (loop for lv below 1 count
                        (floor
                         (flet ((%f (f1)
                                  (- (floor f1 f1) (return-from b -9))))
                           (multiple-value-call #'%f (values (block b3 lv))))
                         42))))
           42)))))
    '(values (integer -99734 -99734) (integer 19 19) &optional))))

(with-test (:name :bit-ir2opt)
  (checked-compile-and-assert
   ()
   `(lambda (a c)
      (declare (fixnum c))
      (setf a -14)
      (logior
       (shiftf a (bit #*01 (max 0 c)))
       (max 0 c)))
   ((1 1) -13)))

(with-test (:name :find-initial-dfo-ignore-let-converted-funs)
  (checked-compile-and-assert
   ()
   `(lambda (c)
      (tagbody
         (flet ((%f7 (f7-3
                      &optional (f7-4 (go tag5))
                                (f7-5 ((lambda (&rest args) (go tag5))))
                                (f7-6 0))
                  0))
           ((lambda (v10) (%f7 (go tag5) -63522127 v10)) c))
       tag5))
   ((9) nil)))

(with-test (:name :find-initial-dfo-ignore-assignment-converted-funs)
  (checked-compile-and-assert
   ()
   `(lambda ()
      (values
       (catch 'c 0)
       (labels ((%f (&optional (x 0) (y 0)) y))
         (case 0
           ((1) (%f 0))
           ((2) (%f))))))
   (() (values 0 nil))))

(with-test (:name :find-initial-dfo-ignore-assignment-converted-funs.2)
  (checked-compile-and-assert
   (:allow-style-warnings t)
   `(lambda (a)
      (let ((v (make-array 1 :initial-element (catch 'ct 42))))
        (labels ((f (&optional (x 4) &key (k a)) x))
          (if nil (f) (f a)))))
   ((9) 9)))

(with-test (:name :if-eq-optimizer-nil)
  (checked-compile-and-assert
      ()
      `(lambda (x)
         (let (b)
           (unless (eq x b)
             (error ""))
           x))
    ((nil) nil)))

(with-test (:name :assignment-convert-check-same-lvar)
  (checked-compile-and-assert
   (:allow-style-warnings t)
   `(lambda (c)
      (flet ((%f9 (f9-1 f9-2 &optional (key1 0))
               f9-2))
        (multiple-value-prog1 (%f9 701021570480035 c)
          (if t
              0
              (progn
                (%f9 1048572 2385880201)
                (if t
                    (%f9 777238289903386671 -15131644893)
                    0))))))
   ((10) 10)))

(with-test (:name :range<)
  (checked-compile-and-assert
      ()
      `(lambda (l h v)
         (declare (fixnum l h))
         (if (< l v)
             (if (> h v)
                 :bad
                 :good)
             :bad))
    ((-1 -1 0) :good)))

(with-test (:name :range<.2)
  (checked-compile-and-assert
      ()
      `(lambda (a h)
         (declare (fixnum h))
         (and (>= a 0)
              (not (> h a))))
    ((1 0) t)
    ((-1 0) nil)))

(with-test (:name :range<.3)
  (checked-compile-and-assert
      ()
      `(lambda (b c)
         (and (or (not b) (< 0 c)) (<= c 0)))
    ((nil -1) t)
    ((t -1) nil)
    ((nil 1) nil)))

(with-test (:name :range<.4)
  (checked-compile-and-assert
      ()
      `(lambda (f m)
         (declare (fixnum f))
         (<= (truncate 10 f) m 0))
    ((-1 0) t)
    ((1 0) nil)
    ((-1 10) nil)))

(with-test (:name :range<-equal-bounds)
  (checked-compile-and-assert
      ()
      `(lambda (l x h)
         (sb-kernel:range< l x h))
    ((0 0 0) nil)
    ((0 0.5 0) nil)
    ((0 -0.5 0) nil)
    ((0 1/2 0) nil)
    ((0 -1/2 0) nil))
  (checked-compile-and-assert
      ()
      `(lambda (l x h)
         (sb-kernel:range<<= l x h))
    ((0 0 0) nil)
    ((0 0.5 0) nil)
    ((0 -0.5 0) nil)
    ((0 1/2 0) nil)
    ((0 -1/2 0) nil))
  (checked-compile-and-assert
      ()
      `(lambda (l x h)
         (sb-kernel:range<=< l x h))
    ((0 0 0) nil)
    ((0 0.5 0) nil)
    ((0 -0.5 0) nil)
    ((0 1/2 0) nil)
    ((0 -1/2 0) nil))
  (checked-compile-and-assert
      ()
      `(lambda (l x h)
         (sb-kernel:range<= l x h))
    ((0 0 0) t)
    ((0 0.5 0) nil)
    ((0 -0.5 0) nil)
    ((0 1/2 0) nil)
    ((0 -1/2 0) nil)))

(with-test (:name :move-from-word/fixnum-ir2opt)
  (checked-compile-and-assert
   ()
   `(lambda (c)
      (declare (type (integer -10 10) c))
      (let ((v5 (logior 2305843195621877482 c)))
        (values v5
                (abs (shiftf v5 (+ v5 1))))))
   ((-10) (values -2 2))))

(with-test (:name :values-list-type-check
            :skipped-on (not (or :x86-64 :arm64)))
  (assert (find-if (lambda (line)
                     (search "BOGUS-ARG-TO-VALUES-LIST-ERROR" line :test #'equal))
                   (ctu:disassembly-lines
                    (checked-compile
                     `(lambda (l)
                        (let (*)
                          (values-list l)))))))
  (assert (not (find-if (lambda (line)
                          (search "BOGUS-ARG-TO-VALUES-LIST-ERROR" line :test #'equal))
                        (ctu:disassembly-lines
                         (checked-compile
                          `(lambda (l)
                             (declare (optimize (safety 0)))
                             (let (*)
                               (values-list l)))))))))

(with-test (:name :explicit-value-cell-top-level)
  (ctu:file-compile
   `((defvar *x*)
     (let ((v 0))
       (loop repeat 1
             do
             (setf *x* (lambda () (incf v)))))
     (assert (eql (funcall *x*) 1))
     (assert (eql (funcall *x*) 2)))
   :load t))

(with-test (:name :load-store-two-words-reused-load-tn)
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (funcall x 1 2 3 4 'a t t))
   (('list) '(1 2 3 4 a t t) :test #'equal)))

(with-test (:name :closures-unreachable-components)
  (checked-compile-and-assert
      ()
      `(lambda (f)
         (catch 'c
           (block nil
             (labels ((f11 () f)
                      (b (&key)
                        (catch 'd
                          (lambda () #'f11))
                        (return #'f11)))))))))

(with-test (:name :flushable-nil-funs)
  (checked-compile-and-assert
      ()
      `(lambda (a b)
         (eq (the (or) (car a))
             (the (or) (car b))))))

(with-test (:name :cmov-modifying-input)
  (checked-compile-and-assert
      ()
      `(lambda (a b d)
         (declare (double-float d))
         (values (if (not (> d 10d0))
                     b
                     a)
                 a))
    ((1 2 1d0) (values 2 1))))

(with-test (:name :ir1-optimize-return-type-widening)
  (checked-compile-and-assert
      ()
      `(lambda (a b)
         (flet ((f ()
                  (ceiling a b)))
           (values (the integer (f)))))
    ((1 2) 1)))

(with-test (:name :reuse-coercion)
  (multiple-value-bind (fun fail warn style notes)
      (checked-compile `(lambda (x d)
                          (declare (double-float d)
                                   (fixnum x)
                                   (optimize speed))
                          (cond ((= x 1)
                                 (+ d 1))
                                ((= x 2)
                                 (+ d 2))
                                ((= x 3)
                                 (+ d 3)))))
    (declare (ignore fail warn style))
    (assert (= (length notes) 1))
    (assert (= (funcall fun 1 0d0) 1d0))
    (assert (= (funcall fun 2 0d0) 2d0))
    (assert (= (funcall fun 3 0d0) 3d0))
    (assert (null (funcall fun 4 0d0)))))

(with-test (:name :reorder-keywordp)
  (checked-compile-and-assert
      ()
      `(lambda (a)
         (cond ((stringp a)
                1)
               ((keywordp a)
                2)
               ((symbolp a)
                3)))
    (("a") 1)
    ((:a) 2)
    (('m) 3)
    ((1) nil)))

(with-test (:name :reorder-same-block)
  (checked-compile-and-assert
      ()
      `(lambda (a)
         (typecase a
           (double-float 1)
           (fixnum 2)
           (bignum 3)
           (t 2)))
    ((1d0) 1)
    ((1) 2)
    (((1+ most-positive-fixnum)) 3)
    ((t) 2)))

(with-test (:name :unlink-node-in-delete-block)
  (checked-compile-and-assert
   ()
   `(lambda (b)
      (tagbody
         ((lambda (v)
            (declare (ignore v))
            ((lambda (a b &rest c)
               a b c
               (go 7))
             (catch 'c 0)
             (case b ((-424 -278) b) (t 0))))
          ((lambda () (go 7))))
       7))))

(with-test (:name :multiple-call-unboxed-calls)
  (checked-compile-and-assert
   ()
   `(lambda (m j)
      (declare (double-float m))
      (let (*)
        (if j
            (funcall j)
            (truncate m))))
   ((1d0 nil) (values 1 0d0))
   ((4d38 nil) (values 399999999999999990995239293824136118272 0d0)))
  (checked-compile-and-assert
   ()
   `(lambda (m j)
      (declare (ratio m))
      (let (*)
        (if j
            (funcall j)
            (coerce m 'double-float))))
   ((1/2 nil) 0.5d0))
  (checked-compile-and-assert
   ()
   `(lambda (m j)
      (declare (double-float m))
      (let (*)
        (if j
            (funcall j)
            (scale-float m 2))))
   ((1d0 nil) 4d0)
   ((2d0 nil) 8d0)))

(with-test (:name :structure-typep*-deleted-branch)
  (checked-compile-and-assert
      ()
      `(lambda (x)
         (cond
           ((typep x 'random-state)
            1)
           ((typep x 'hash-table)
            2)
           (t x)))
    ((*random-state*) 1)
    (((make-hash-table)) 2)
    ((423444) 423444)))

(with-test (:name :deleted-call-type)
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (labels ((foo (x)
                 x))
        (foo 1)
        (when x
          (unless x
            (foo 3)))
        (foo 2)))
    (:return-type (values (integer 1 2) &optional))))

(with-test (:name :optional-type-propagation)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (labels ((foo (&optional x)
                    x))
           (foo 1)
           (foo 2)))
    (:return-type (values (integer 1 2) &optional)))
  (checked-compile-and-assert
      ()
      `(lambda ()
         (labels ((foo (&key x)
                    x))
           (foo :x 1)
           (foo :x 2)))
    (:return-type (values (integer 1 2) &optional))))

(with-test (:name :local-function-declaration)
  (checked-compile-and-assert
      (:optimize :safe)
      `(lambda (n)
         (declare ((function * fixnum) n))
         (typep (funcall n) 'fixnum))
    ((#'list) (condition 'type-error))))

(declaim (inline member-type-derivation))
(defun member-type-derivation (x)
  (member x '(a b c d)))

(with-test (:name :member-type-derivation)
  (checked-compile-and-assert
   ()
   `(lambda (n)
      (when (member-type-derivation n)
        t))
   (('a) t)
   (('b) t)
   (('c) t)
   (('d) t)
   (('e) nil)))

(with-test (:name :equal-not-null-transform)
  (checked-compile-and-assert
      ()
      `(lambda (x y)
         (declare (atom x) (list y))
         (equalp x y))
    ((nil nil) t)
    ((nil '(1)) nil)
    ((1 nil) nil))
  (checked-compile-and-assert
      ()
      `(lambda (x y)
         (declare (atom x) (list y))
         (equal y x))
    ((nil nil) t)
    ((nil '(1)) nil)
    ((1 nil) nil)))

(with-test (:name :optimize-return-deleted-lambda)
  (checked-compile-and-assert
      ()
      `(lambda (x)
         (labels ((f1 ()
                    (case x (:star (f1))))
                  (f2 (d n)
                    (case x (:open (f1))))
                  (f3 (d n)
                    (case x
                      (:backquote (f4 d 0))
                      (:nest
                       (f3 d n))
                      (t (f2 d n))))
                  (f4 (d n)
                    (case x
                      (:nest (f3 d n))
                      (t (f2 d n))))
                  (f5 (d)
                    (case x
                      (:backquote (f4 d 0))
                      (:nest (f5 d)))))))
    ((1) nil)))

(with-test (:name :type-derivers-type-widening)
  (checked-compile-and-assert
      ()
      `(lambda (b c)
         (logbitp 0
                  (if (eql c 0)
                      (max (ignore-errors c) 0)
                      b)))
      ((1 2) t)
      ((0 0) nil)))

(with-test (:name :propagate-to-refs-hairy)
  (checked-compile-and-assert
      ()
      `(lambda (y)
         (declare (fixnum y))
         (let ((d (max 1 (the (satisfies eval) y))))
           (the fixnum (* d 8))))
      ((2) 16)))

(with-test (:name :complicated-cons-function-unions)
  (checked-compile-and-assert
      ()
      `(lambda (w)
         (car (member w '#.(list #'< #'= #'eql #'equalp))))
    ((#'=) #'=)))


(with-test (:name :tail-calls-terminated-blocks)
  (prog* ((f (checked-compile `(lambda (f)
                                 (declare (optimize  (debug 1)))
                                 (labels ((f1 (f)
                                            (funcall f)
                                            (f1 f)))
                                   (f1 (f1 f))))))
          (x 0))
     (assert (funcall f (lambda () (when (= (incf x) 2) (return t)))))))

(with-test (:name :the*-exits)
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (if x
          10
          (block nil
            (hash-table-test (return)))))
   ((t) 10)
   ((nil) nil)))

(with-test (:name :inlining-deleted-go-tag)
  (checked-compile-and-assert
   ()
   `(lambda (a)
     (tagbody
        (labels ((f () (go t)))
          (declare (inline f))
          (funcall a #'f)
          (multiple-value-call #'f (values)))
      t)
     2)
   ((#'list) 2)))

(with-test (:name :inling-non-convertible-locals)
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (labels ((f (&key m)
                 (values m x)))
        (declare (inline f))
        (eval (f))
        (f x 30)))
   ((:m) (values 30 :m))
   ((:allow-other-keys) (values nil :allow-other-keys))))

(with-test (:name :undeleted-exits)
  (checked-compile-and-assert
   ()
   `(lambda ()
      (tagbody
         (flet ((f (a) a (go 5)))
           (print (list #'f (loop for i in (f 1)
                                  do (print i)))))
       5))
   (() nil)))

(with-test (:name :unused-initial-values)
  (checked-compile-and-assert
    (:allow-notes nil :optimize '(:debug 2 :speed 3 :safety 1))
    `(lambda (v)
       (declare ((simple-array double-float (*)) v))
       (loop for e across v count (> e 0)))
    (((make-array 9 :element-type 'double-float :initial-element 1d0)) 9)))

(with-test (:name :consecutive-cast)
  (checked-compile-and-assert
   ()
   `(lambda (f)
      (the fixnum (the integer (funcall f))))
   ((#'+) 0))
  (checked-compile-and-assert
   ()
   `(lambda (a)
      (abs (catch 'c (the (satisfies eval) a))))
   ((-1) 1))
  (checked-compile-and-assert
   ()
   `(lambda (f x)
      (the fixnum
           (if f
               (funcall f)
               (the real x))))
   ((#'* 0) 1)
   ((nil 2) 2))
  (checked-compile-and-assert
   (:optimize :safe)
   `(lambda (x)
      (the vector (the array x)))
   ((1) (condition 'type-error)))
  (checked-compile-and-assert
   (:optimize :safe)
   `(lambda (x)
      (let ((m (the array x)))
        (values (the vector m)
                m)))
   ((1) (condition 'type-error)))
  (checked-compile-and-assert
   (:optimize :safe)
   `(lambda (c d m)
      (declare (type fixnum c d m))
      (the (unsigned-byte 62)
           (values
            (let ((v (logxor c -7322529 d 9223372036854775805)))
              (if (> v 0)
                  (the unsigned-byte m)
                  (logior 80827861226 v))))))
   ((-3462512952 -77 0) (condition 'type-error)))
  (checked-compile-and-assert
    (:optimize :safe)
    `(lambda (x m)
       (the fixnum
            (if x
                (let ((j (the integer m)))
                  j)
                m)))
    ((nil 'a) (condition 'type-error))
    ((t 1d0) (condition 'type-error))
    ((nil 1) 1)
    ((t 2) 2))
  (checked-compile-and-assert
    (:optimize :safe)
   `(lambda (f x)
     (the (values fixnum &optional) (the (values integer &rest t) (funcall f x))))
    ((#'identity .0) (condition 'type-error))
    ((#'identity 1) 1)
    ((#'identity (expt 2 1000)) (condition 'type-error))))

(with-test (:name :pop-values-unused)
  (checked-compile-and-assert
   ()
   `(lambda (j l r)
      (declare ((function (fixnum &rest t)) j))
      (apply j l r))
   ((#'+ 1 '(2)) 3)))

(with-test (:name :disabling-arg-count-checking)
  (checked-compile-and-assert
      (:optimize :safe)
      `(lambda (x d)
         (let ((f (lambda (x y)
                    (< x y))))
           (funcall d f)
           (sort x f)))
    ((nil #'funcall) (condition 'program-error))
    ((nil #'list) nil))
  (checked-compile-and-assert
      (:optimize :default)
      `(lambda (x d f)
         (multiple-value-bind (f key)
             (if f
                 (values f #'car)
                 (values (lambda (x y)
                           (< x y))
                         #'cdr))
           (funcall d f)
           (sort x f :key key)))
    ((nil #'funcall nil) (condition 'program-error))
    ((nil #'list nil) nil)))

(with-test (:name :dont-rebind)
  (let ((* :dont-rebind))
   (checked-compile-and-assert
    ()
    `(lambda ()
       (let* ((x *)
              (* x))
         x))
    (() :dont-rebind))))

(with-test (:name :multiple-uses-type-derivation)
  (assert-type
   (lambda (x a b)
     (funcall (ecase x
                (0 #'+)
                (1 (lambda (x y) (- x y)))
                (2 'logand))
              a b))
   number)
  (assert-type
   (lambda (x a b)
     (let ((f (ecase x
                (0 '+)
                (2 'logand))))
       (when a
         (funcall f a b))))
   (or null number)))

(with-test (:name :multiple-uses-to-fdefn)
  (checked-compile-and-assert
      (:optimize :safe)
      `(lambda (x a b)
         (let ((fun (ecase x
                      (0 'a)
                      (1 'b))))
           (when a
             (funcall fun a b))))
    ((0 nil 2) nil)
    ((0 1 2) (condition 'undefined-function))))

(with-test (:name :undefined-system-fun)
  (checked-compile-and-assert
      (:optimize :safe :allow-warnings t)
      `(lambda ()
         #'nil)
    (() (condition 'undefined-function)))
  (checked-compile-and-assert
      (:optimize :safe :allow-warnings t)
      `(lambda ()
         #'(setf *standard-output*))
    (() (condition 'undefined-function)))
  (checked-compile-and-assert
      (:optimize :safe :allow-warnings t)
      `(lambda ()
         (nil))
    (() (condition 'undefined-function))))

(with-test (:name :multiple-uses-type-mismatch-from-transforms)
  (assert (nth-value 3
                     (checked-compile
                      `(lambda (m)
                         (sb-kernel:the* (fixnum :use-annotations t) (or m (make-array 10))))
                      :allow-style-warnings t)))
  (checked-compile
   `(lambda (m s)
      (declare (optimize speed))
      (sb-kernel:the* (fixnum :use-annotations t) (or m (position 10 (the list s))))))
  (assert (nth-value 3
                     (checked-compile
                      `(lambda (m)
                         (the fixnum (or m (make-array 10))))
                      :allow-style-warnings t)))
  (checked-compile
   `(lambda (m s)
      (declare (optimize speed))
      (the fixnum (or m (position 10 (the list s)))))))

(with-test (:name :if-bypass)
  (assert-type
   (lambda (x)
     (declare ((or null vector) x)
              (optimize speed))
     (if (> (length x) 10)
         x
         (error "m")))
   vector)
  (checked-compile-and-assert
   ()
   `(lambda (a)
      (evenp (if a 1 0)))
   ((t) nil)
   ((nil) t)))

(declaim (inline inline-losing-type))
(defun inline-losing-type (array)
  (block nil
    (cond ((typep array '(simple-array * (*)))
           (return array))
          (t (error "x")))))

(with-test (:name :inline-losing-type)
  (checked-compile-and-assert
      ()
      `(lambda (array)
         (declare (type (or null array) array))
         (aref (inline-losing-type array) 0))
    ((#(1)) 1)))

(with-test (:name :lvar-fun-type-specials)
  (checked-compile `(lambda (y) (find t y :test *))))

(with-test (:name :notinline-with-source-transforms)
  (checked-compile `(lambda (x y z)
                      (declare (notinline make-array))
                      (make-array x y z))))
