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

base_tmpfilename="compiler-test-$$-tmp"
tmpfilename="$base_tmpfilename.lisp"
compiled_tmpfilename="$base_tmpfilename.fasl"

# This should fail, as type inference should show that the call to FOO
# will return something of the wrong type.
cat > $tmpfilename <<EOF
    (in-package :cl-user)
    (defun foo (x) (list x))
    (defun bar (x) (1+ (foo x)))
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
fail_on_compiler_note $tmpfilename

# test case from Rudi for some CLOS WARNINGness that shouldn't have
# been there
cat > $tmpfilename <<EOF
    #+sb-eval (eval-when (:compile-toplevel)
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
fail_on_compiler_note $tmpfilename

cat > $tmpfilename <<EOF
    (defun foo (x y)
      (declare (muffle-conditions compiler-note))
      (declare (optimize speed))
      (+ x y))
EOF
fail_on_compiler_note $tmpfilename

cat > $tmpfilename <<EOF
    (declaim (muffle-conditions compiler-note))
    (defun foo (x y)
      (declare (optimize speed))
      (+ x y))
EOF
fail_on_compiler_note $tmpfilename

cat > $tmpfilename <<EOF
    (declaim (muffle-conditions compiler-note))
    (defun foo (x y)
      (declare (unmuffle-conditions compiler-note))
      (declare (optimize speed))
      (+ x y))
EOF
expect_compiler_note $tmpfilename

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

rm $tmpfilename
rm $compiled_tmpfilename

# success
exit 104
