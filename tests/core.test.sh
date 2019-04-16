#!/bin/sh

# tests related to .core files

# This software is part of the SBCL system. See the README file for
# more information.
#
# While most of SBCL is derived from the CMU CL system, the test
# files (like this one) were written from scratch after the fork
# from CMU CL.
#
# This software is in the public domain and is provided with
# absolutely no warranty. See the COPYING and CREDITS files for
# more information.

# FIXME: this file takes >30 seconds to complete.
# We should run each test in a background shell and wait, or something.

. ./subr.sh

use_test_subdirectory

tmpcore=$TEST_FILESTEM.core
tmpoutput=$TEST_FILESTEM.txt

run_sbcl <<EOF
  (save-lisp-and-die "$tmpcore" :toplevel (lambda () 42))
EOF
run_sbcl_with_core "$tmpcore" --noinform --no-userinit --no-sysinit \
    --eval "(setf sb-ext:*evaluator-mode* :${TEST_SBCL_EVALUATOR_MODE:-compile})"
check_status_maybe_lose "SAVE-LISP-AND-DIE :TOPLEVEL" $? 0 "(saved core ran)"

run_sbcl <<EOF
  (require :asdf)
  (require :sb-bsd-sockets)
  (save-lisp-and-die "$tmpcore")
EOF
run_sbcl_with_core "$tmpcore" --noinform --no-userinit --no-sysinit \
    --eval "(require :sb-posix)" --quit
check_status_maybe_lose "SAVE-LISP-AND-DIE" $? 0 "(saved core ran)"

# In sbcl-0.7.7 SAVE-LISP-AND-DIE didn't work at all because of
# flakiness caused by consing/GC/purify twice-and-at-least-twice
# mismatch grot.
#
# "serves yall right for fiddling with too much stuff"
#   -- Eric Marsden, <http://tunes.org/~nef/logs/lisp/02.09.15>
#
# diagnosed and fixed by Dan Barlow in sbcl-0.7.7.29
run_sbcl <<EOF
  (defun foo (x) (+ x 11))
  ;; The basic smoke test includes a test that immobile-space defragmentation
  ;; supports calls to "static" functions - those called without reference
  ;; to an fdefn, from a caller in dynamic space.
  ;; dynamic space should be the default for compilation to memory,
  ;; but maybe someone changed it :immobile, so bind it to be certain.
  (let (#+immobile-code (sb-c::*compile-to-memory-space* :dynamic))
     (defvar *afun* (compile nil '(lambda (x) (- (length x))))))
  (save-lisp-and-die "$tmpcore")
EOF
run_sbcl_with_core "$tmpcore" --noinform --no-userinit --no-sysinit --noprint \
    --eval "(setf sb-ext:*evaluator-mode* :${TEST_SBCL_EVALUATOR_MODE:-compile})" \
    <<EOF
  (exit :code (foo 10))
EOF
check_status_maybe_lose "Basic SAVE-LISP-AND-DIE" $? 21 "(saved core ran)"

# Expose potential failure that could happen in save-lisp-and-die in an image
# that was restarted from one that underwent number coalescing during a
# previous save-lisp-and-die: A bignum as a layout bitmap can be forwarded
# while using that bignum as the bitmap to decide what to scan in that selfsame
# instance. Aside from random failure, this could be detected by enabling
# 'verify_gens' which printed "Ptr sees free page" after GC failed to scavenge
# all pointer slots. I believe that it was a coincidence that my test croaked
# specifically while scanning layout-of-layout. It could have been any
# structure having a slot holding a bignum EQ to its own layout-bitmap.
run_sbcl --load ../heap-reloc/embiggen.lisp <<EOF
  #+gencgc (setf (extern-alien "verify_gens" char) 0)
  (save-lisp-and-die "$tmpcore")
EOF
run_sbcl_with_core "$tmpcore" --noinform --no-userinit --no-sysinit --eval "(exit)"
check_status_maybe_lose "Crash GC" $? 0 "(saved core ran)"

# In sbcl-0.9.8 saving cores with callbacks didn't work on gencgc platforms
run_sbcl <<EOF
  (defun bar ()
    (format t "~&Callbacks not supported, skipping~%")
    (exit :code 42))
  #+alien-callbacks
  (progn
    (fmakunbound 'bar)
    (sb-alien::define-alien-callback foo int () 42)
    (defun bar () (exit :code (alien-funcall foo))))
  (save-lisp-and-die "$tmpcore")
EOF
run_sbcl_with_core "$tmpcore" --noinform --no-userinit --no-sysinit --noprint \
    --eval "(setf sb-ext:*evaluator-mode* :${TEST_SBCL_EVALUATOR_MODE:-compile})" \
    <<EOF
  (bar)
EOF
check_status_maybe_lose "Callbacks after SAVE-LISP-AND-DIE" $? \
    42 "(callback function ran)"

# test suppression of banner in executable cores
run_sbcl <<EOF
  (save-lisp-and-die "$tmpcore" :executable t)
EOF
chmod u+x "$tmpcore"
# FIXME: Shouldn't this test use "run_sbcl_with_core" ?
./"$tmpcore" > "$tmpoutput" --no-userinit --no-sysinit --noprint <<EOF
  (exit :code 71)
EOF
status=$?
if [ $status != 71 ]; then
  echo "failure in banner suppression: $status"
  exit 1
elif [ -s "$tmpoutput" ]; then
  echo "failure in banner suppression: nonempty output:"
  echo ---
  cat "$tmpoutput"
  echo ---
  exit 1
elif [ -f "$tmpoutput" ]; then
  echo "/Executable suppressed banner, good."
else
  echo "failure in banner suppression: $tmpoutput was not created or something funny happened."
  exit 1
fi

# saving runtime options _from_ executable cores
run_sbcl <<EOF
  (save-lisp-and-die "$tmpcore" :executable t)
EOF
chmod u+x "$tmpcore"
./"$tmpcore" --no-userinit --no-sysinit --noprint <<EOF
  (save-lisp-and-die "$tmpcore" :executable t :save-runtime-options t)
EOF
chmod u+x "$tmpcore"
./"$tmpcore" --no-userinit --no-sysinit --noprint --version --eval '(exit)' <<EOF
  (when (equal *posix-argv* '("./$tmpcore" "--version" "--eval" "(exit)"))
    (exit :code 42))
EOF
status=$?
rm "$tmpcore"
if [ $status != 42 ]; then
    echo "saving runtime options from executable failed"
    exit 1
fi

# executable core used as "--core" option should not save the memory sizes
# that were originally saved, but the sizes in the process doing the save.
run_sbcl_with_args --noinform --control-stack-size 160KB --dynamic-space-size 200MB \
    --disable-debugger --no-userinit --no-sysinit --noprint <<EOF
  (save-lisp-and-die "$tmpcore" :executable t :save-runtime-options t)
EOF
chmod u+x "$tmpcore"
./"$tmpcore" --no-userinit --no-sysinit --noprint <<EOF
  (assert (eql (extern-alien "thread_control_stack_size" unsigned) (* 160 1024)))
  ; allow slight shrinkage if heap relocation has to adjust for alignment
  (assert (<= 0 (- (* 200 1048576) (dynamic-space-size)) 65536))
EOF
run_sbcl_with_core "$tmpcore" --noinform --control-stack-size 200KB \
    --tls-limit 5000 \
    --dynamic-space-size 250MB --no-userinit --no-sysinit --noprint <<EOF
  (assert (eql (extern-alien "thread_control_stack_size" unsigned) (* 200 1024)))
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
  (when (and (eql (extern-alien "thread_control_stack_size" unsigned) (* 200 1024))
             (eql (extern-alien "dynamic_values_bytes" (unsigned 32))
                  (* 5000 sb-vm:n-word-bytes))
             (dynamic-space-size-good-p))
    (exit :code 42))
EOF
status=$?
rm "$tmpcore" "${tmpcore}2"
if [ $status != 42 ]; then
    echo "re-saved executable used wrong memory size options"
    exit 1
fi

run_sbcl <<EOF
  (save-lisp-and-die "$tmpcore" :toplevel (lambda () 42)
                      :compression (and (member :sb-core-compression *features*) t))
EOF
run_sbcl_with_core "$tmpcore" --noinform --no-userinit --no-sysinit \
    --eval "(setf sb-ext:*evaluator-mode* :${TEST_SBCL_EVALUATOR_MODE:-compile})"
check_status_maybe_lose "SAVE-LISP-AND-DIE :COMPRESS" $? 0 "(compressed saved core ran)"

rm "$tmpcore"
run_sbcl <<EOF
  (save-lisp-and-die "$tmpcore" :toplevel (lambda () 42) :executable t
                     :compression (and (member :sb-core-compression *features*) t))
EOF
chmod u+x "$tmpcore"
./"$tmpcore" --no-userinit --no-sysinit
check_status_maybe_lose "SAVE-LISP-AND-DIE :EXECUTABLE-COMPRESS" $? 0 "(executable compressed saved core ran)"

# Verify that for funcallable instances which were moved into the
# immobile varyobj space by SAVE-LISP-AND-DIE, setting the layout
# updates the GC card touched bit.
# Going through instance-obsolescence stuff makes things mostly work
# by accident, because (SETF %FUNCALLABLE-INSTANCE-INFO) touches
# the card dirty bit when reassigning the CLOS slot vector.
# We need to simulate a GC interrupt after altering the layout,
# but prior to affecting the slot vector.
run_sbcl <<EOF
  (defclass subgf (standard-generic-function) (a)
    (:metaclass sb-mop:funcallable-standard-class))
  (defgeneric myfun (a)
    (:generic-function-class subgf)
    (:method ((self integer)) 'hey-integer))
  (defun assign-layout ()
    #+gencgc (setf (extern-alien "verify_gens" char) 0)
    (defclass subgf (standard-generic-function) (a b) ; add a slot
      (:metaclass sb-mop:funcallable-standard-class))
    (defclass subgf (standard-generic-function) (a) ; remove a slot
      (:metaclass sb-mop:funcallable-standard-class))
    (let ((nl (sb-kernel:find-layout 'subgf))) ; new layout
      (assert (not (eq (sb-kernel:%funcallable-instance-layout #'myfun)
                       nl)))
      (setf (sb-kernel:%funcallable-instance-layout #'myfun) nl)
      (gc)))
  (save-lisp-and-die "$tmpcore" :toplevel #'assign-layout)
EOF
run_sbcl_with_core "$tmpcore" --noinform --no-userinit --no-sysinit
check_status_maybe_lose "SET-FIN-LAYOUT" $? 0 "(saved core ran)"

exit $EXIT_TEST_WIN
