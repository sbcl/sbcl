. ./subr.sh

this_file=`pwd`/save9.test.sh
use_test_subdirectory

tmpcore=$TEST_FILESTEM.core

run_sbcl <<EOF
  (defvar *s* (open #+unix "$this_file"
                    #-unix (format nil "~A/run-tests.lisp"
                            (posix-getenv "SBCL_PWD"))))
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
