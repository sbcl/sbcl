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

# how we invoke SBCL in the tests
#
# Until sbcl-0.6.12.8, the shell variable SBCL was bound to a relative
# pathname, but now we take care to bind it to an absolute pathname (still
# generated relative to `pwd` in the tests/ directory) so that tests
# can chdir before invoking SBCL and still work.
sbclstem=`pwd`/../src/runtime/sbcl
SBCL="${1:-$sbclstem --core `pwd`/../output/sbcl.core --noinform --sysinit /dev/null --userinit /dev/null --noprint --disable-debugger}"
export SBCL
echo /running tests on SBCL=\'$SBCL\'
# more or less like SBCL, but without enough grot removed that appending
# a --core command line argument works
#
# (KLUDGE: and also without any magic to suppress --userinit and
# --sysinit, so if you use it in a test, you need to add those
# yourself if you want things to be clean. If many tests start using
# this, we can redo it as a shell function or something so that the
# magic can be done once and only once.)
SBCL_ALLOWING_CORE=${1:-$sbclstem}
export SBCL_ALLOWING_CORE
echo /with SBCL_ALLOWING_CORE=\'$SBCL_ALLOWING_CORE\'

# "Ten four" is the closest numerical slang I can find to "OK", so
# it's the Unix status value that we expect from a successful test.
# (Of course, zero is the usual success value, but we don't want to
# use that because SBCL returns that by default, so we might think
# we passed a test when in fact some error caused us to exit SBCL
# in a weird unexpected way. In contrast, 104 is unlikely to be
# returned unless we exit through the intended explicit "test
# successful" path.
tenfour () {
    if [ $1 = 104 ]; then
	echo ok
    else
	echo test $2 failed, expected 104 return code, got $1
	exit 1
    fi
}

# *.pure.lisp files are ordinary Lisp code with no side effects,
# and we can run them all in a single Lisp process.
echo //running '*.pure.lisp' tests
echo //i.e. *.pure.lisp
(
echo "(progn"
echo "  (progn (format t \"//loading assertoid.lisp~%\") (load \"assertoid.lisp\"))"
echo "  (use-package \"ASSERTOID\")"
for f in *.pure.lisp; do
    if [ -f $f ]; then
        echo "  (progn (format t \"//running $f test~%\") (load \"$f\"))"
    fi
done
echo "  (sb-ext:quit :unix-status 104)) ; Return status=success."
) | $SBCL ; tenfour $? "(pure.lisp files)"

# *.impure.lisp files are Lisp code with side effects (e.g. doing
# DEFSTRUCT or DEFTYPE or DEFVAR, or messing with the read table).
# Each one should be LOADed in a separate invocation of Lisp, so 
# that we don't need to worry about them interfering with each
# other.
echo //running '*.impure.lisp' tests
for f in *.impure.lisp; do
    if [ -f $f ]; then
        echo //running $f test
        echo "(load \"$f\")" | $SBCL ; tenfour $? $f
    fi
done

# *.test.sh files are scripts to test stuff, typically stuff which 
# can't so easily be tested within Lisp itself. A file foo.test.sh
# may be associated with other files foo*, e.g. foo.lisp, foo-1.lisp,
# or foo.pl.
echo //running '*.test.sh' tests
for f in *.test.sh; do
    if [ -f $f ]; then
	echo //running $f test
	sh $f "$SBCL"; tenfour $? $f
    fi
done

# *.assertoids files contain ASSERTOID statements to test things
# interpreted and at various compilation levels.
echo //running '*.assertoids' tests
for f in *.assertoids; do
    if [ -f $f ]; then
	echo //running $f test
	echo "(load \"$f\")" | $SBCL --eval '(load "assertoid.lisp")' ; tenfour $? $f
    fi
done

# *.pure-cload.lisp files want to be compiled, then loaded. They 
# can all be done in the same invocation of Lisp.
echo //running '*.pure-cload.lisp' tests
for f in *.pure-cload.lisp; do
    # (Actually here we LOAD each one into a separate invocation
    # of Lisp just because I haven't figured out a concise way
    # to LOAD them all into the same Lisp.)
    if [ -f $f ]; then
	echo //running $f test
	$SBCL <<EOF ; tenfour $? $f
		(compile-file "$f")
                (progn
                  (unwind-protect
		  (load *)
                   (ignore-errors (delete-file (compile-file-pathname "$f"))))
                  (sb-ext:quit :unix-status 104))
EOF
    fi
done

# *.impure-cload.lisp files want to be compiled, then loaded. They
# can have side effects, so each one should be done in a separate
# invocation of Lisp so that they don't interfere.
echo //running '*.impure-cload.lisp' tests
for f in *.impure-cload.lisp; do
    if [ -f $f ]; then
	echo //running $f test
	$SBCL <<EOF ; tenfour $? $f
		(compile-file "$f")
                (progn
                  (unwind-protect
		  (load *)
                   (ignore-errors (delete-file (compile-file-pathname "$f"))))
                  (sb-ext:quit :unix-status 104))
EOF
    fi
done

# (*.before-xc.lisp and *.after-xc.lisp files aren't handled in this
# script at all. They're tests intended to run in the cross-compiler,
# so that some functionality can be tested even when cold init doesn't
# work.)

echo '//apparent success (reached end of run-tests.sh normally)'
date
