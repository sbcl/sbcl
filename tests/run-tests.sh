#!/bin/sh

# Run the regression tests in this directory.

# how we invoke SBCL
sbcl=${1:-sbcl --noprint --noprogrammer}

# *.pure.lisp files are ordinary Lisp code with no side effects,
# and we can run them all in a single Lisp process.
(for f in *.pure.lisp; do echo \"$f\"; done) | $sbcl < pure.lisp

# *.impure.lisp files are Lisp code with side effects (e.g. doing DEFSTRUCT
# or DEFTYPE or DEFVAR). Each one needs to be run as a separate
# invocation of Lisp.
for f in *.impure.lisp; do
    echo $f | $sbcl < pure.lisp
done

# *.test.sh files are scripts to test stuff, typically stuff which can't
# so easily be tested within Lisp itself. A file foo.test.sh
# may be associated with other files foo*, e.g. foo.lisp, foo-1.lisp,
# or foo.pl.
for f in *.test.sh; do
    sh $f || exit failed test $f
done

# *.assertoids files contain ASSERTOID statements to test things
# interpreted and at various compilation levels.
for f in *.assertoids; do
    echo "(load \"$f\")" | $sbcl --eval '(load "assertoid.lisp")'
done
