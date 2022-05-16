# Don't try to run sbcl from /tmp on openbsd as it's unlikely to be
# mounted with wxallowed
if [ "$SBCL_SOFTWARE_TYPE" != OpenBSD ]; then
    export TEST_BASEDIR=${TMPDIR:-/tmp}
fi
. ./subr.sh

this_file=`pwd`/save9.test.sh
use_test_subdirectory

tmpcore=$TEST_FILESTEM.core

run_sbcl <<EOF
  (defvar *s* (open "$this_file"))
  (save-lisp-and-die "$tmpcore")
EOF
set -e
answer=`run_sbcl_with_core "$tmpcore" --noinform --disable-ldb --no-userinit --no-sysinit \
--eval '(print (open-stream-p *s*))' --quit`
rm "$tmpcore"
if [ $answer = NIL ]
then
    exit $EXIT_TEST_WIN
fi
