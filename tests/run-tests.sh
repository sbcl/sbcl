#!/bin/sh

# Run the regression tests in this directory.

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

# how we invoke SBCL
sbcl=${1:-../src/runtime/sbcl --core ../output/sbcl.core --noinform --sysinit /dev/null --userinit /dev/null --noprint --noprogrammer}

# "Ten four" is the closest numerical slang I can find to "OK", so
# it's the Unix status value that we expect from a successful test.
# (Of course, zero is the usual success value, but we don't want to
# use that because SBCL returns that by default, so we might think
# we passed a test when in fact some error caused us to exit SBCL
# in a weird unexpected way. In contrast, 104 is unlikely to be
# returned unless we exit through the intended explicit "test
# successful" path.
tenfour () {
    if [ $? = 104 ]; then
	echo ok
    else
	echo test failed: $?
	exit 1
    fi
}

# *.pure.lisp files are ordinary Lisp code with no side effects,
# and we can run them all in a single Lisp process.
echo //running '*.pure.lisp' tests
echo //i.e. *.pure.lisp
(
echo "(progn"
for f in *.pure.lisp; do
    if [ -f $f ]; then
        echo "  (progn (format t \"//running $f test~%\") (load \"$f\"))"
    fi
done
echo "  (sb-ext:quit :unix-status 104)) ; Return status=success."
) | $sbcl ; tenfour

# *.impure.lisp files are Lisp code with side effects (e.g. doing DEFSTRUCT
# or DEFTYPE or DEFVAR). Each one needs to be run as a separate
# invocation of Lisp.
echo //running '*.impure.lisp' tests
for f in *.impure.lisp; do
    if [ -f $f ]; then
        echo //running $f test
        echo "(load \"$f\")" | $sbcl ; tenfour
    fi
done

# *.test.sh files are scripts to test stuff, typically stuff which can't
# so easily be tested within Lisp itself. A file foo.test.sh
# may be associated with other files foo*, e.g. foo.lisp, foo-1.lisp,
# or foo.pl.
echo //running '*.test.sh' tests
for f in *.test.sh; do
    if [ -f $f ]; then
	echo //running $f test
	sh $f "$sbcl"; tenfour
    fi
done

# *.assertoids files contain ASSERTOID statements to test things
# interpreted and at various compilation levels.
echo //running '*.assertoids' tests
for f in *.assertoids; do
    if [ -f $f ]; then
	echo //running $f test
	echo "(load \"$f\")" | $sbcl --eval '(load "assertoid.lisp")' ; tenfour
    fi
done

# *.pure-cload.lisp files want to be compiled, then loaded. They 
# can all be done in the same invocation of Lisp.
echo //running '*.pure-cload.lisp' tests
for f in *.pure-cload.lisp; do
    if [ -f $f ]; then
	echo //running $f test
	$sbcl <<EOF ; tenfour
		(compile-file "$f")
		(progn (load *) (sb-ext:quit :unix-status 104))
EOF
    fi
done

echo '//apparent success (reached end of run-tests.sh normally)'
