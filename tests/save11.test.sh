. ./subr.sh

create_test_subdirectory

tmpfasl=$TEST_DIRECTORY/$TEST_FILESTEM.fasl
tmpcore=$TEST_DIRECTORY/$TEST_FILESTEM.core
set -e

run_sbcl <<EOF
  (load (compile-file "../src/code/repack-xref" :output-file "$tmpfasl"))
  (save-lisp-and-die "$tmpcore")
EOF
run_sbcl <<EOF
  (load "../tools-for-build/editcore")
  (let ((result (sb-editcore::scan-for-end-of-page-garbage "$tmpcore")))
    ;; result = number of junk words found
    (sb-sys:os-exit (if (> result 0) 1 $EXIT_TEST_WIN)))
EOF

exit $?
