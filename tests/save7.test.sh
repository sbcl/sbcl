# Don't try to run sbcl from /tmp on openbsd as it's unlikely to be
# mounted with wxallowed
if [ "$SBCL_SOFTWARE_TYPE" != OpenBSD ]; then
    export TEST_BASEDIR=${TMPDIR:-/tmp}
fi
. ./subr.sh

use_test_subdirectory

tmpcore=$TEST_FILESTEM.core

# executable core used as "--core" option should not save the memory sizes
# that were originally saved, but the sizes in the process doing the save.
run_sbcl_with_args --noinform --control-stack-size 320KB --dynamic-space-size 200MB \
    --disable-debugger --no-userinit --no-sysinit --noprint <<EOF
  (save-lisp-and-die "$tmpcore" :executable t :save-runtime-options t)
EOF

chmod u+x "$tmpcore"
./"$tmpcore" --no-userinit --no-sysinit --noprint <<EOF
  (assert (eql (extern-alien "thread_control_stack_size" unsigned) (* 320 1024)))
  ; allow slight shrinkage if heap relocation has to adjust for alignment
  (assert (<= 0 (- (* 200 1048576) (dynamic-space-size)) 65536))
EOF

# next, check that tmpcore will accept memory size options.
# Use --control-stack-size for the test, since we don't know the valid range
# of dynamic space size for the OS/arch.
# Test that we find the arg in the midst of the command-line args as well.
./"$tmpcore" --no-userinit --control-stack-size 3MB --no-sysinit --noprint <<EOF
(let* ((end (sb-vm::current-thread-offset-sap sb-vm::thread-control-stack-end-slot))
       (start (sb-vm::current-thread-offset-sap sb-vm::thread-control-stack-start-slot))
       (diff (sb-sys:sap- end start)))
  (assert (= diff (* 3 1048576))))
EOF
# Test that we don't parse after a "--"
./"$tmpcore" --no-userinit --no-sysinit --noprint -- --dynamic-space-size 1000GB foo <<EOF
(let* ((end (sb-vm::current-thread-offset-sap sb-vm::thread-control-stack-end-slot))
       (start (sb-vm::current-thread-offset-sap sb-vm::thread-control-stack-start-slot))
       (diff (sb-sys:sap- end start)))
  ;; We better not have gotten a terabyte of dynamic space!
  (assert (< diff (* 8 1024 1024 1024))) ; assert less than 8GB
  (assert (find "--" sb-ext:*posix-argv* :test 'string=)))
EOF

run_sbcl_with_core "$tmpcore" --noinform --control-stack-size 640KB \
    --tls-limit 5000 \
    --dynamic-space-size 250MB --no-userinit --no-sysinit --noprint <<EOF
  (assert (eql (extern-alien "thread_control_stack_size" unsigned) (* 640 1024)))
  (assert (eql (extern-alien "dynamic_values_bytes" (unsigned 32))
               (* 5000 sb-vm:n-word-bytes)))
  ; allow slight shrinkage if heap relocation has to adjust for alignment
  (defun dynamic-space-size-good-p ()
    (<= 0 (- (* 250 1048576) (dynamic-space-size)) 65536))
  (assert (dynamic-space-size-good-p))
  (save-lisp-and-die "${tmpcore}2" :executable t :save-runtime-options t)
EOF
chmod u+x "${tmpcore}2"
./"${tmpcore}2" --no-userinit --no-sysinit --noprint <<EOF
  (when (and (eql (extern-alien "thread_control_stack_size" unsigned) (* 640 1024))
             (eql (extern-alien "dynamic_values_bytes" (unsigned 32))
                  (* 5000 sb-vm:n-word-bytes))
             (dynamic-space-size-good-p))
    (exit :code 42))
EOF
status=$?
rm "$tmpcore" "${tmpcore}2"
if [ $status -ne 42 ]; then
    echo "re-saved executable used wrong memory size options"
    exit 1
fi

exit $EXIT_TEST_WIN
