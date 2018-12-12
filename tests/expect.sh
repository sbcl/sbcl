# file to be sourced by scripts wanting to test the compiler

. ./subr.sh

# Check that compiling and loading the file $1 generates an error
# at load time; also that just loading it directly (into the
# interpreter) generates an error.

# In bash,
#
#   function callee() { cat }
#   function caller() { callee bar <<EOF \n $1 \n EOF \n }
#   caller foo
#
# will print "foo".  In certain versions of sh, however, it will print
# "bar" instead.  Hence variables f and c in the following code.

expect_load_error ()
{
    # Test compiling and loading.
    f="$1"
    run_sbcl <<EOF
        (compile-file "$f")
        ;;; But loading the file should fail.
        (multiple-value-bind (value0 value1) (ignore-errors (load *))
            (assert (null value0))
            ;(format t "VALUE1=~S (~A)~%" value1 value1)
            (assert (typep value1 'error)))
        (sb-ext:exit :code $EXIT_LISP_WIN)
EOF
    check_status_maybe_lose compile-and-load $?

    # Test loading into the interpreter.
    f="$1"
    run_sbcl <<EOF
        (multiple-value-bind (value0 value1) (ignore-errors (load "$f"))
            (assert (null value0))
            ;(format t "VALUE1=~S (~A)~%" value1 value1)
            (assert (typep value1 'error)))
        (sb-ext:exit :code $EXIT_LISP_WIN)
EOF
    check_status_maybe_lose load-into-interpreter $?
}

expect_clean_cload ()
{
    expect_clean_compile $1
    f="$1"
    run_sbcl <<EOF
        (multiple-value-bind (value0 value1) 
            (ignore-errors (load (compile-file-pathname "$f")))
          (assert value0)
          (assert (null value1)))
        (sb-ext:exit :code $EXIT_LISP_WIN)
EOF
    check_status_maybe_lose load-compiled $?
}

# Test that a file compiles cleanly, with no ERRORs, WARNINGs or
# STYLE-WARNINGs.
expect_clean_compile ()
{
    f="$1"
    run_sbcl <<EOF
        (multiple-value-bind (pathname warnings-p failure-p)
            (compile-file "$f")
          (declare (ignore pathname))
          (assert (not warnings-p))
          (assert (not failure-p))
          (sb-ext:exit :code $EXIT_LISP_WIN))
EOF
    check_status_maybe_lose clean-compile $?
}

expect_warned_compile ()
{
    f="$1"
    run_sbcl <<EOF
        (multiple-value-bind (pathname warnings-p failure-p)
            (compile-file "$f")
          (declare (ignore pathname))
          (assert warnings-p)
          (assert (not failure-p))
          (sb-ext:exit :code $EXIT_LISP_WIN))
EOF
    check_status_maybe_lose warn-compile $?
}

expect_failed_compile ()
{
    f="$1"
    run_sbcl <<EOF
        (multiple-value-bind (pathname warnings-p failure-p)
            (compile-file "$f")
          (declare (ignore pathname warnings-p))
          (assert failure-p)
          (sb-ext:exit :code $EXIT_LISP_WIN))
EOF
    check_status_maybe_lose fail-compile $?
}

expect_aborted_compile ()
{
    f="$1"
    run_sbcl <<EOF
        (let* ((lisp "$f")
               (fasl (compile-file-pathname lisp)))
          (multiple-value-bind (pathname warnings-p failure-p)
              (compile-file "$f" :print t)
            (assert (not pathname))
            (assert failure-p)
            (assert warnings-p)
            (assert (not (probe-file fasl))))
          (sb-ext:exit :code $EXIT_LISP_WIN))
EOF
    check_status_maybe_lose abort-compile $?
}

fail_on_condition_during_compile ()
{
    c="$1"
    f="$2"
    run_sbcl <<EOF
        (handler-bind (($c #'error))
          (compile-file "$f")
          (sb-ext:exit :code $EXIT_LISP_WIN))
EOF
    check_status_maybe_lose "fail-on-condition_$1" $?
}

expect_condition_during_compile ()
{
    c="$1"
    f="$2"
    run_sbcl <<EOF
        (handler-bind (($c (lambda (c)
                             (declare (ignore c))
                             (sb-ext:exit :code $EXIT_LISP_WIN))))
          (compile-file "$f"))
EOF
    check_status_maybe_lose "expect-condition_$1" $?
}

