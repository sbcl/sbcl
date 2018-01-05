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

(defun compiles-with-warning (lambda)
  (assert (nth-value 2 (checked-compile lambda :allow-warnings t))))

(with-test (:name (ldb :recognize-local-macros))
  ;; Should not call %LDB
  (assert (not (ctu:find-named-callees
                (checked-compile
                 '(lambda (x)
                   (declare (optimize speed))
                   (macrolet ((b () '(byte 2 2)))
                     (ldb (b) (the fixnum x)))))))))

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
  (let ((f (checked-compile '(lambda (x) (the sb-impl::function-name x)))))
    (assert (equal (list (symbol-function 'sb-int:valid-function-name-p))
                   (ctu:find-named-callees f))))
  (let ((f (checked-compile '(lambda (x)
                               (declare (notinline sb-int:legal-fun-name-p))
                               (the sb-impl::function-name x)))))
    (assert (equal (list (symbol-function 'sb-int:legal-fun-name-p))
                   (ctu:find-named-callees f)))))

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

(defstruct (a-test-structure-foo
            (:constructor make-a-foo-1)
            (:constructor make-a-foo-2 (b &optional a)))
  (a 0 :type symbol)
  (b nil :type integer))

(with-test (:name :improperly-initialized-slot-warns)
  ;; should warn because B's default is NIL, not an integer.
  (compiles-with-warning '(lambda () (make-a-foo-1 :a 'what)))
  ;; should warn because A's default is 0
  (compiles-with-warning '(lambda () (make-a-foo-2 3))))

(with-test (:name (inline structure :ctor :no declaim))
  (let ((f (checked-compile '(lambda ()
                               (make-a-foo-1 :a 'wat :b 3)))))
    (assert (ctu:find-named-callees f)))
  (let ((f (checked-compile '(lambda ()
                               (declare (inline make-a-foo-1))
                               (make-a-foo-1 :a 'wat :b 3)))))
    (assert (not (ctu:find-named-callees f)))))

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
  (let ((f (checked-compile '(lambda (x) (princ-to-string x) x))))
    (assert (not (ctu:find-named-callees f :name 'princ-to-string))))
  ;; But in high safety it should be called for effect
  (let ((f (checked-compile '(lambda (x)
                               (declare (optimize safety)) (princ-to-string x) x))))
    (assert (ctu:find-named-callees f :name 'princ-to-string))))

(with-test (:name :space-bounds-no-consing
                  :skipped-on :interpreter)
  ;; Asking for the size of a heap space should not cost anything!
  (ctu:assert-no-consing (sb-vm::%space-bounds :static))
  (ctu:assert-no-consing (sb-vm::space-bytes :static)))

(with-test (:name (sb-vm::map-allocated-objects :no-consing)
                  :skipped-on :interpreter)
  (let ((n 0))
    (sb-int:dx-flet ((f (obj type size)
                       (declare (ignore obj type size))
                       (incf n)))
      (ctu:assert-no-consing
       (sb-vm::map-allocated-objects #'f :dynamic)
       5))))

(with-test (:name :pack-varints-as-bignum)
  (dotimes (i 500) ; do some random testing this many times
    (let* ((random-numbers (loop repeat (+ (random 20) 3)
                                 collect (1+ (random 4000))))
           (test-list (sort (delete-duplicates random-numbers) #'<))
           (packed-int (sb-c::pack-code-fixup-locs test-list))
           (result (make-array 1 :element-type 'sb-ext:word)))
      ;; The packer intrinsically self-checks the packing
      ;; so we don't need to assert anything about that.
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

(with-test (:name :layout-constants
                  :skipped-on (not (and :x86-64 :immobile-space)))
  (let ((addr-of-pathname-layout
         (write-to-string
          (sb-kernel:get-lisp-obj-address (sb-kernel:find-layout 'pathname))
          :base 16 :radix t))
        (count 0))
    ;; The constant should appear in two CMP instructions
    (dolist (line (split-string
                   (with-output-to-string (s)
                     (let ((sb-disassem:*disassem-location-column-width* 0))
                       (disassemble 'pathnamep :stream s)))
                   #\newline))
      (when (and (search "CMP" line) (search addr-of-pathname-layout line))
        (incf count)))
    (assert (= count 2))))

(with-test (:name :set-symbol-value-imm :skipped-on (not :x86-64))
  (let (success)
    (dolist (line (split-string
                   (with-output-to-string (s)
                     (let ((sb-disassem:*disassem-location-column-width* 0))
                       (disassemble '(lambda () (setq *print-base* 8)) :stream s)))
                   #\newline))
      (when (and #+sb-thread (search "MOV QWORD PTR [R" line)
                 #-sb-thread (search "MOV QWORD PTR [" line)
                 (search (format nil ", ~D" (ash 8 sb-vm:n-fixnum-tag-bits)) line))
        (setq success t)))
    (assert success)))

(with-test (:name :linkage-table-bogosity :skipped-on (not :sb-dynamic-core))
  (let ((strings (map 'list (lambda (x) (if (consp x) (car x) x))
                      #+sb-dynamic-core sb-vm::+required-foreign-symbols+
                      #-sb-dynamic-core '())))
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

(with-test (:name :interval-div-zero)
  (checked-compile-and-assert (:optimize :safe)
    `(lambda (x y)
       (truncate (the (integer 0 0) x)
                 (the (rational (1) (2)) y)))
   ((0 3/2) (values 0 0))))

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

(with-test (:name :float-quotient-rounding-errors)
  (checked-compile-and-assert (:optimize :safe)
   `(lambda ()
      (floor -114658225103614 84619.58))
    (() (values -1354984705 8473228.0)))
  (checked-compile-and-assert (:optimize :safe)
   `(lambda ()
      (ceiling 114658225103614 84619.58))
    (() (values 1354984705 -8473228.0))))

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
  (let ((fun (checked-compile '(lambda (y) (let ((x (count y '(1 2 3))))
                                             (declare (ignore x)))))))
    (assert (not (ctu:find-named-callees fun)))))

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
         (sb-kernel:symeval nil))
    (() nil)))

(with-test (:name (:physenv-analyze :deleted-lambda))
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
