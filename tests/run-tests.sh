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

# *.test.sh files are scripts to test stuff. A file foo.test.sh
# may be associated with other files foo*, e.g. foo.lisp, foo-1.lisp,
# or foo.pl.
for f in *.test.sh; do
    sh $f
done
