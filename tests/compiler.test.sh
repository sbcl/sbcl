#!/bin/sh

# This software is part of the SBCL system. See the README file for
# more information.
#
# While most of SBCL is derived from the CMU CL system, the test
# files (like this one) were written from scratch after the fork
# from CMU CL.
#
# This software is in the public domain and is provided with
# absolutely no warranty. See the COPYING and CREDITS files for
# more information.

. ./expect.sh

use_test_subdirectory

# It's unclear why the majority of these tests are written in shell script when
# many look as though they would be perfectly happy as lisp tests.
# This test, on the other hand, would have a tough time as a lisp test,
# because it needs to make a symlink which would either mean calling run-program
# or using alien-funcall on symlink(). Shell is easier.
mkdir -p inscrutable/f00
echo '(defun zook (x) (declare (integer x)) (length x))' > inscrutable/f00/f00_xyz_bad
ln -s inscrutable/f00/f00_xyz_bad good.lisp
run_sbcl --eval '(setq *default-pathname-defaults* #P"")' \
  --eval '(compile-file "good.lisp" :verbose t)' --quit >stdout.out 2>stderr.out
egrep -q 'compiling file ".*good' stdout.out
stdout_ok=$?
egrep -q 'file:.+good' stderr.out
stderr_ok=$?
if [ $stdout_ok = 0 -a $stderr_ok = 0 ] ; then
    rm -r good.* stdout.out stderr.out inscrutable
    echo "untruenames: PASS"
else
    cat stdout.out stderr.out
    echo "untruenames: FAIL"
    exit $EXIT_LOSE
fi

## FIXME: all these tests need to be more silent. Too much noise to parse

tmpfilename="$TEST_FILESTEM.lisp"

# This should fail, as type inference should show that the call to FOO
# will return something of the wrong type.
cat > $tmpfilename <<EOF
    (in-package :cl-user)
    (defun foo (x) (list x))
    (defun bar (x) (1+ (foo x)))
EOF
expect_failed_compile $tmpfilename

# This should fail, as type inference should show that the call to FOO
# has a wrong number of args.
cat > $tmpfilename <<EOF
    (in-package :cl-user)
    (defun foo (x) (or x (foo x x)))
EOF
expect_failed_compile $tmpfilename

# This should fail, as we define a function multiply in the same file
# (CLHS 3.2.2.3).
cat > $tmpfilename <<EOF
    (in-package :cl-user)
    (defun foo (x) (list x))
    (defun foo (x) (cons x x))
EOF
expect_failed_compile $tmpfilename

# This shouldn't fail, as the inner FLETs should not be treated as
# having the same name.
cat > $tmpfilename <<EOF
    (in-package :cl-user)
    (defun foo (x)
      (flet ((baz (y) (load y)))
        (declare (notinline baz))
        (baz x)))
    (defun bar (x)
      (flet ((baz (y) (load y)))
        (declare (notinline baz))
        (baz x)))
EOF
expect_clean_compile $tmpfilename

# This shouldn't fail because it's not really a multiple definition
cat > $tmpfilename <<EOF
    (in-package :cl-user)
    (eval-when (:compile-toplevel :load-toplevel :execute)
      (defun foo (x) x))
EOF
expect_clean_compile $tmpfilename

# Likewise
cat > $tmpfilename <<EOF
    (in-package :cl-user)
    (eval-when (:compile-toplevel)
      (defun foo (x) x))
    (defun foo (x) x)
EOF
expect_clean_compile $tmpfilename

# This shouldn't fail despite the apparent type mismatch, because of
# the NOTINLINE declamation.
cat > $tmpfilename <<EOF
    (in-package :cl-user)
    (defun foo (x) (list x))
    (declaim (notinline foo))
    (defun bar (x) (1+ (foo x)))
EOF
expect_clean_compile $tmpfilename

# This shouldn't fail, but did until sbcl-0.8.10.4x
cat > $tmpfilename <<EOF
    (in-package :cl-user)
    (declaim (inline foo))
    (defun foo (x)
      (1+ x))
    (defun bar (y)
      (list (foo y) (if (> y 1) (funcall (if (> y 0) #'foo #'identity) y))))
EOF
expect_clean_compile $tmpfilename

# This shouldn't fail despite the apparent type mismatch, because of
# the NOTINLINE declaration.
cat > $tmpfilename <<EOF
    (in-package :cl-user)
    (defun foo (x) (list x))
    (defun bar (x)
      (declare (notinline foo))
      (1+ (foo x)))
EOF
expect_clean_compile $tmpfilename

# This in an ideal world would fail (that is, return with FAILURE-P
# set), but at present it doesn't.
cat > $tmpfilename <<EOF
    (in-package :cl-user)
    (defun foo (x) (list x))
    (defun bar (x)
      (declare (notinline foo))
      (locally
        (declare (inline foo))
        (1+ (foo x))))
EOF
# expect_failed_compile $tmpfilename

# This used to not warn, because the VALUES derive-type optimizer was
# insufficiently precise.
cat > $tmpfilename <<EOF
    (in-package :cl-user)
    (defun foo (x) (declare (ignore x)) (values))
    (defun bar (x) (1+ (foo x)))
EOF
expect_failed_compile $tmpfilename

# Even after making the VALUES derive-type optimizer more precise, the
# following should still be clean.
cat > $tmpfilename <<EOF
    (in-package :cl-user)
    (defun foo (x) (declare (ignore x)) (values))
    (defun bar (x) (car x))
EOF
expect_clean_compile $tmpfilename

# NOTINLINE on known functions shouldn't inhibit type inference
# (spotted by APD sbcl-devel 2003-06-14)
cat > $tmpfilename <<EOF
    (in-package :cl-user)
    (defun foo (x)
      (declare (notinline list))
      (1+ (list x)))
EOF
expect_failed_compile $tmpfilename

# ERROR wants to check its format string for sanity...
cat > $tmpfilename <<EOF
    (in-package :cl-user)
    (defun foo (x)
      (when x
        (error "~S")))
EOF
expect_failed_compile $tmpfilename

# ... but it (ERROR) shouldn't complain about being unable to optimize
# when it's uncertain about its argument's type
cat > $tmpfilename <<EOF
    (in-package :cl-user)
    (defun foo (x)
      (error x))
EOF
fail_on_condition_during_compile sb-ext:compiler-note $tmpfilename

# test case from Rudi for some CLOS WARNINGness that shouldn't have
# been there
cat > $tmpfilename <<EOF
    (eval-when (:compile-toplevel)
       (setf sb-ext:*evaluator-mode* :compile))

    (eval-when (:compile-toplevel :load-toplevel :execute)
      (defstruct buffer-state
        (output-index 0)))

    (defclass buffered-stream-mixin ()
      ((buffer-state :initform (make-buffer-state))))

    (defgeneric frob (stream))
    (defmethod frob ((stream t))
      nil)
    (defmethod frob ((stream buffered-stream-mixin))
      (symbol-macrolet
            ((index (buffer-state-output-index (slot-value stream 'buffer-state))))
          (setf index 0))
      (call-next-method))
EOF
expect_clean_compile $tmpfilename

# undeclared unbound variables should cause a full warning, as they
# invoke undefined behaviour
cat > $tmpfilename <<EOF
    (defun foo () x)
EOF
expect_failed_compile $tmpfilename

cat > $tmpfilename <<EOF
    (declaim (special *x*))
    (defun foo () *x*)
EOF
expect_clean_compile $tmpfilename

cat > $tmpfilename <<EOF
    (defun foo () (declare (special x)) x)
EOF
expect_clean_compile $tmpfilename

# MUFFLE-CONDITIONS tests
cat > $tmpfilename <<EOF
    (defun foo ()
      (declare (muffle-conditions style-warning))
      (bar))
EOF
expect_clean_compile $tmpfilename

cat > $tmpfilename <<EOF
    (defun foo ()
      (declare (muffle-conditions code-deletion-note))
      (if t (foo) (foo)))
EOF
fail_on_condition_during_compile sb-ext:compiler-note $tmpfilename

cat > $tmpfilename <<EOF
    (defun foo (x y)
      (declare (muffle-conditions compiler-note))
      (declare (optimize speed))
      (+ x y))
EOF
fail_on_condition_during_compile sb-ext:compiler-note $tmpfilename

cat > $tmpfilename <<EOF
    (declaim (muffle-conditions compiler-note))
    (defun foo (x y)
      (declare (optimize speed))
      (+ x y))
EOF
fail_on_condition_during_compile sb-ext:compiler-note $tmpfilename

cat > $tmpfilename <<EOF
    (declaim (muffle-conditions compiler-note))
    (defun foo (x y)
      (declare (unmuffle-conditions compiler-note))
      (declare (optimize speed))
      (+ x y))
EOF
expect_condition_during_compile sb-ext:compiler-note $tmpfilename

# undefined variable causes a WARNING
cat > $tmpfilename <<EOF
    (declaim (muffle-conditions warning))
    (declaim (unmuffle-conditions style-warning))
    (defun foo () x)
EOF
expect_clean_compile $tmpfilename

# top level LOCALLY behaves nicely
cat > $tmpfilename <<EOF
    (locally
      (declare (muffle-conditions warning))
      (defun foo () x))
EOF
expect_clean_compile $tmpfilename

cat > $tmpfilename <<EOF
    (locally
      (declare (muffle-conditions warning))
      (defun foo () x))
    (defun bar () x)
EOF
expect_failed_compile $tmpfilename

# This should fail, and fail nicely -- not eg. loop trying to dump
# references to the unbound variable.
cat > $tmpfilename <<EOF
(defmacro macro-with-unbound-variables (foo)
  \`(print ,bar))

(macro-with-unbound-variables 'xxx)
EOF
expect_failed_compile $tmpfilename

# This should fail, as the MAKE-LOAD-FORM must be used for
# externalizing conditions, and the method for CONDITION must signal
# an error.
cat > $tmpfilename <<EOF
(defvar *oops* #.(make-condition 'condition))
EOF
expect_failed_compile $tmpfilename

# This should fail, as the MAKE-LOAD-FORM must be used for objects,
# and the method for STANDARD.OBJECT is required to signal an error.
cat > $tmpfilename <<EOF
(defvar *oops* #.(make-instance 'standard-object))
EOF
expect_failed_compile $tmpfilename

# This should be clean
cat > $tmpfilename <<EOF
(defvar *string* (make-string 10 :element-type 'base-char))
EOF
expect_clean_compile $tmpfilename

# This should style-warn (but not warn or otherwise fail) as the call
# to FORMAT has too many arguments, which is bad style but not
# otherwise fatal.
cat > $tmpfilename <<EOF
(defun foo (a b)
  (format nil "abc~~def" a b))
EOF
expect_warned_compile $tmpfilename

# Tests that destructive-functions on known-constant data cause
# compile-time warnings.
cat > $tmpfilename <<EOF
(let ((string "foo"))
  (defun foo ()
    (setf string "bar")))
EOF
expect_clean_compile $tmpfilename

cat > $tmpfilename <<EOF
(defun foo ()
  (let (result)
    (nreverse result)))
EOF
expect_clean_compile $tmpfilename

cat > $tmpfilename <<EOF
(defun bar ()
  (let ((result ""))
    (nreverse result)))
EOF
expect_clean_compile $tmpfilename

cat > $tmpfilename <<EOF
(let ((string "foo"))
  (defun foo ()
    (replace string "bar")))
EOF
expect_failed_compile $tmpfilename

cat > $tmpfilename <<EOF
(defun foo ()
  (setf (char "bar" 0) #\1))
EOF
expect_failed_compile $tmpfilename

cat > $tmpfilename <<EOF
(let ((foo '(1 2 3)))
  (defun foo ()
    (nconc foo foo)))
EOF
expect_failed_compile $tmpfilename

cat > $tmpfilename <<EOF
x
y
z
EOF
expect_failed_compile $tmpfilename

cat > $tmpfilename <<EOF
(declaim (optimize (speed 3) (space 0) (safety 0)))

(defun foo (bar)
  (last bar))
EOF
expect_clean_compile $tmpfilename

cat > $tmpfilename <<EOF
(defstruct foo
  (bar #p"/tmp/"))
EOF
expect_clean_compile $tmpfilename

cat > $tmpfilename <<EOF
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct foox)
  (defmethod make-load-form ((foo foox) &optional env)
    (declare (ignore env))
    '(make-foox)))
(defstruct bar
  (foo #.(make-foox)))
EOF
expect_clean_compile $tmpfilename

cat > $tmpfilename <<EOF
(defun something (x) x)
...
(defun something-more (x) x)
EOF
expect_aborted_compile $tmpfilename

cat > $tmpfilename <<EOF
(if t (locally))
EOF
expect_clean_cload $tmpfilename

cat > $tmpfilename <<EOF
(defconstant cl-package (find-package :cl))
(defun cl-symbol-p (x)
  (eq (symbol-package x) cl-package))
EOF
expect_clean_cload $tmpfilename

cat > $tmpfilename <<EOF
(and (eval-when (:compile-toplevel) (error "oops AND")))
(or (eval-when (:compile-toplevel) (error "oops OR")))
(cond (t (eval-when (:compile-toplevel) (error "oops COND"))))
EOF
expect_clean_cload $tmpfilename

# Test correct fasl-dumping of literals in arglist defaulting.
# (LP Bug #310132)
cat > $tmpfilename <<EOF
(in-package :cl-user)

;; These are CLHS examples from the dictionary entry for MAKE-LOAD-FORM.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct my-struct a b c)
  (defmethod make-load-form ((s my-struct) &optional environment)
    (make-load-form-saving-slots s :environment environment))
  (defclass my-class ()
    ((x :initarg :x :reader obj-x)
     (y :initarg :y :reader obj-y)
     (dist :accessor obj-dist)))
  (defmethod make-load-form ((self my-class) &optional environment)
    (make-load-form-saving-slots self
                                 :slot-names '(x y)
                                 :environment environment)))

(defun bar1 (&optional (x #.(make-my-struct)))
  x)

(defun bar2 (&optional (x #.(make-instance 'my-class)))
  x)

;; Packages are externalizable.
(defun bar3 (&optional (x #.*package*))
  x)

(assert (typep (bar1) 'my-struct))
(assert (typep (bar2) 'my-class))
(assert (eq (bar3) *package*))

EOF
expect_clean_cload $tmpfilename

cat > $tmpfilename <<EOF
(in-package :cl-user)
(defmacro foo () (error "ERROR at macroexpansion time."))
(defun bar () (foo))
EOF
expect_condition_during_compile sb-c:compiler-error $tmpfilename

cat > $tmpfilename <<EOF
(eval-when (:compile-toplevel)
  (error "ERROR within EVAL-WHEN."))
EOF
expect_condition_during_compile simple-error $tmpfilename

cat > $tmpfilename <<EOF
(defun slot-name-incf (s)
  (with-slots (no-such-slot) s
    (incf no-such-slot)))
EOF
expect_clean_cload $tmpfilename

cat > $tmpfilename <<EOF
(in-package :cl-user)

(defun foo ()
  (declare (muffle-conditions warning))
  (let ((em 0d0))
    (declare (type double-float em))
    (dotimes (i 42)
      (setf em (float (1+ i))))))
EOF
expect_clean_compile $tmpfilename

cat > $tmpfilename <<EOF
(in-package :cl-user)

(defun foo ()
  (declare (muffle-conditions warning))
  (flet ((foo ()
           (declare (values fixnum))
           nil))
    (foo)))
EOF
expect_clean_compile $tmpfilename

cat > $tmpfilename <<EOF
(in-package :cl-user)

(defun foo (x)
  (declare (muffle-conditions warning)
           (type (vector (mod 7) 1) x))
  (setf (aref x 0) 8)
  x)
EOF
expect_clean_compile $tmpfilename

cat > $tmpfilename <<EOF
(in-package :cl-user)

(declaim (notinline foo))
(let ((i 0)) (defun foo (x) (incf i x)))
(defun bar (x) (foo x))
EOF
fail_on_condition_during_compile sb-ext:compiler-note $tmpfilename

cat > $tmpfilename <<EOF
(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass subclass (superclass) ((zslot2 :initarg :zslot2 :accessor zslot2))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass superclass () ((c :initarg c :accessor c))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-kernel::%invalidate-layout (sb-pcl::class-wrapper (find-class 'subclass))))

;; This test file is weird. It expects to see warnings from a COMPILE inside
;; a method body that is compiled (and not necessarily invoked).
;; To force the warnings to happen, we have to force the method to get called,
;; which we can do by asking a SUBTYPEP question.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod sb-mop:finalize-inheritance :after (class)
    (eval '(defmethod z (x) (abcde)))
    (funcall (compile nil '(lambda () (defun zz (x) (defgh))))))
  (assert (not (sb-kernel:csubtypep (sb-kernel:specifier-type 'subclass)
                                    (sb-kernel:specifier-type 'condition)))))

(defun subclass-p (x)
  (typep x 'subclass))
EOF
expect_warned_compile $tmpfilename

cat > $tmpfilename <<EOF
(defun foo () 128)
(let ((a (load-time-value (foo))))
  (declare (fixnum a))
  (print a)
  (terpri))
EOF
expect_clean_cload $tmpfilename

# success
exit $EXIT_TEST_WIN
