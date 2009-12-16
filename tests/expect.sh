# file to be sourced by scripts wanting to test the compiler

. ./subr.sh

# Check that compiling and loading the file $1 generates an error
# at load time; also that just loading it directly (into the
# interpreter) generates an error.
expect_load_error ()
{
    # Test compiling and loading.
    run_sbcl <<EOF
        (compile-file "$1")
        ;;; But loading the file should fail.
        (multiple-value-bind (value0 value1) (ignore-errors (load *))
            (assert (null value0))
            (format t "VALUE1=~S (~A)~%" value1 value1)
            (assert (typep value1 'error)))
        (sb-ext:quit :unix-status $EXIT_LISP_WIN)
EOF
    check_status_maybe_lose compile-and-load $?

    # Test loading into the interpreter.
    run_sbcl <<EOF
        (multiple-value-bind (value0 value1) (ignore-errors (load "$1"))
            (assert (null value0))
            (format t "VALUE1=~S (~A)~%" value1 value1)
            (assert (typep value1 'error)))
        (sb-ext:quit :unix-status $EXIT_LISP_WIN)
EOF
    check_status_maybe_lose load-into-interpreter $?
}

expect_clean_cload ()
{
    expect_clean_compile $1
    run_sbcl <<EOF
        (multiple-value-bind (value0 value1) 
            (ignore-errors (load (compile-file-pathname "$1")))
          (assert value0)
          (assert (null value1)))
        (sb-ext:quit :unix-status $EXIT_LISP_WIN)
EOF
    check_status_maybe_lose load-compiled $?
}

# Test that a file compiles cleanly, with no ERRORs, WARNINGs or
# STYLE-WARNINGs.
expect_clean_compile ()
{
    run_sbcl <<EOF
        (multiple-value-bind (pathname warnings-p failure-p)
            (compile-file "$1")
          (declare (ignore pathname))
          (assert (not warnings-p))
          (assert (not failure-p))
          (sb-ext:quit :unix-status $EXIT_LISP_WIN))
EOF
    check_status_maybe_lose clean-compile $?
}

expect_warned_compile ()
{
    run_sbcl <<EOF
        (multiple-value-bind (pathname warnings-p failure-p)
            (compile-file "$1")
          (declare (ignore pathname))
          (assert warnings-p)
          (assert (not failure-p))
          (sb-ext:quit :unix-status $EXIT_LISP_WIN))
EOF
    check_status_maybe_lose warn-compile $?
}

expect_failed_compile ()
{
    run_sbcl <<EOF
        (multiple-value-bind (pathname warnings-p failure-p)
            (compile-file "$1")
          (declare (ignore pathname warnings-p))
          (assert failure-p)
          (sb-ext:quit :unix-status $EXIT_LISP_WIN))
EOF
    check_status_maybe_lose fail-compile $?
}

expect_aborted_compile ()
{
    run_sbcl <<EOF
        (let* ((lisp "$1")
               (fasl (compile-file-pathname lisp)))
          (multiple-value-bind (pathname warnings-p failure-p)
              (compile-file "$1" :print t)
            (assert (not pathname))
            (assert failure-p)
            (assert warnings-p)
            (assert (not (probe-file fasl))))
          (sb-ext:quit :unix-status $EXIT_LISP_WIN))
EOF
    check_status_maybe_lose abort-compile $?
}

fail_on_condition_during_compile ()
{
    run_sbcl <<EOF
        (handler-bind (($1 #'error))
          (compile-file "$2")
          (sb-ext:quit :unix-status $EXIT_LISP_WIN))
EOF
    check_status_maybe_lose "fail-on-condition_$1" $?
}

expect_condition_during_compile ()
{
    run_sbcl <<EOF
        (handler-bind (($1 (lambda (c)
                             (declare (ignore c))
                             (sb-ext:quit :unix-status $EXIT_LISP_WIN))))
          (compile-file "$2"))
EOF
    check_status_maybe_lose "expect-condition_$1" $?
}

