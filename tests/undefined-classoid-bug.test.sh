# This file run a regression test for a bug in loading
# forward-referenced layouts.

. ./subr.sh

use_test_subdirectory

FILES='"undefined-classoid-bug-1.lisp" "undefined-classoid-bug-2.lisp"'
FASLS='"undefined-classoid-bug-1.fasl" "undefined-classoid-bug-2.fasl"'

for f in $FILES; do
    (cd "$SBCL_PWD"; cp `eval "echo $f"` "$TEST_DIRECTORY");
done

run_sbcl <<EOF
(let ((files (list $FILES)))
  (mapc #'load files)
  (mapc #'compile-file files))
(exit :code 52)
EOF

run_sbcl <<EOF
(mapc #'load (list $FASLS))
(exit :code $EXIT_LISP_WIN)
EOF
check_status_maybe_lose undefined-classoid-bug $?

exit $EXIT_TEST_WIN
