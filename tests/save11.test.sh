. ./subr.sh

create_test_subdirectory

tmpfasl=$TEST_DIRECTORY/$TEST_FILESTEM.fasl
tmpcore=$TEST_DIRECTORY/$TEST_FILESTEM.core

run_sbcl <<EOF
  #-(and linux x86-64 sb-thread) (exit :code 2)
  (let ((*features* (union *features* sb-impl:+internal-features+)))
    (load (compile-file "../src/code/repack-xref" :output-file "$tmpfasl")))
  (save-lisp-and-die "$tmpcore")
EOF

status=$?

if [ $status -eq 2 ]; then
    # skip
    exit $EXIT_TEST_WIN
elif [ $status != 0 ]; then
    exit $EXIT_LOSE
fi

run_sbcl <<EOF
  (load "../tools-for-build/editcore")
  (let ((result (sb-editcore::scan-for-end-of-page-garbage "$tmpcore")))
    ;; result = number of junk words found
    (sb-sys:os-exit (if (> result 0) 1 $EXIT_TEST_WIN)))
EOF

exit $?
