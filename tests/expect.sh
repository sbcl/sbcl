# file to be sourced by scripts wanting to test the compiler

# Check that compiling and loading the file $1 generates an error
# at load time; also that just loading it directly (into the
# interpreter) generates an error.
expect_load_error ()
{
    # Test compiling and loading.
    $SBCL <<EOF
	(compile-file "$1")
	;;; But loading the file should fail.
	(multiple-value-bind (value0 value1) (ignore-errors (load *))
	    (assert (null value0))
	    (format t "VALUE1=~S (~A)~%" value1 value1)
	    (assert (typep value1 'error)))
	(sb-ext:quit :unix-status 52)
EOF
    if [ $? != 52 ]; then
	echo compile-and-load $1 test failed: $?
	exit 1
    fi

    # Test loading into the interpreter.
    $SBCL <<EOF
	(multiple-value-bind (value0 value1) (ignore-errors (load "$1"))
	    (assert (null value0))
	    (format t "VALUE1=~S (~A)~%" value1 value1)
	    (assert (typep value1 'error)))
	(sb-ext:quit :unix-status 52)
EOF
    if [ $? != 52 ]; then
	echo load-into-interpreter $1 test failed: $?
	exit 1
    fi
}

# Test that a file compiles cleanly, with no ERRORs, WARNINGs or
# STYLE-WARNINGs.
#
# Maybe this wants to be in a compiler.test.sh script?  This function
# was originally written to test APD's patch for slot readers and
# writers not being known to the compiler. -- CSR, 2002-08-14
expect_clean_compile () 
{
    $SBCL <<EOF
        (multiple-value-bind (pathname warnings-p failure-p)
            (compile-file "$1")
          (declare (ignore pathname))
          (assert (not warnings-p))
          (assert (not failure-p))
          (sb-ext:quit :unix-status 52))
EOF
    if [ $? != 52 ]; then
        echo clean-compile $1 test failed: $?
        exit 1
    fi
}

expect_warned_compile ()
{
    $SBCL <<EOF
        (multiple-value-bind (pathname warnings-p failure-p)
            (compile-file "$1")
          (declare (ignore pathname))
          (assert warnings-p)
          (assert (not failure-p))
          (sb-ext:quit :unix-status 52))
EOF
    if [ $? != 52 ]; then
        echo warn-compile $1 test failed: $?
        exit 1
    fi
}

expect_failed_compile ()
{
    $SBCL <<EOF
        (multiple-value-bind (pathname warnings-p failure-p)
            (compile-file "$1")
          (declare (ignore pathname warnings-p))
          (assert failure-p)
          (sb-ext:quit :unix-status 52))
EOF
    if [ $? != 52 ]; then
        echo fail-compile $1 test failed: $?
        exit 1
    fi
}

fail_on_compiler_note ()
{
    $SBCL <<EOF
        (handler-bind ((sb-ext:compiler-note #'error))
          (compile-file "$1")
          (sb-ext:quit :unix-status 52))
EOF
    if [ $? != 52]; then
        echo compiler-note $1 test failed: $?
        exit 1
    fi
}

