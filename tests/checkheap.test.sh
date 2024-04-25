#!/bin/sh

. ./subr.sh

# figure out if immobile-space is supported
run_sbcl <<EOF
(unless (member :immobile-space sb-impl:+internal-features+) (sb-sys:os-exit 1))
EOF
status=$?
if [ $status -eq 0 ]
then
  use_test_subdirectory
  stdout=$TEST_DIRECTORY/$TEST_FILESTEM.out
  stderr=$TEST_DIRECTORY/$TEST_FILESTEM.err
  run_sbcl >$stdout 2>$stderr <<EOF
;; Immobile pages have physical protection, so it's unlikely that it would be wrong.
;; However, the verifier was never actually checking.
(assert (= (sb-kernel:generation-of '*posix-argv*) sb-vm:+pseudo-static-generation+))
(assert (< (sb-kernel:generation-of *posix-argv*) sb-vm:+pseudo-static-generation+))
(define-alien-type nil
    ;; ... and yet another place for Lisp to become out-of-sync with C.
    (struct immobile-page
            (free-index (unsigned 32))
            (flags (unsigned 8))
            (obj-align (unsigned 8))
            (padding (unsigned 8))
            (generations (unsigned 8))))
(define-alien-variable "fixedobj_pages" (* (struct immobile-page)))
(setf (extern-alien "pre_verify_gen_0" char) 1)
(let ((i (floor (- (sb-kernel:get-lisp-obj-address '*posix-argv*)
                   sb-vm:fixedobj-space-start)
                4096)))
  ;; Mark it as-if write-protected (though without using mprotect)
  ;; This constant is defined in immobile-space.c: #define WRITE_PROTECT 0x80
  (setf (slot (deref fixedobj-pages i) 'flags) 128)
  (gc)) ; should fail pre-GC assertions
EOF
  grep -q 'pre-GC failure' $stderr
  status=$?
  rm $stdout $stderr
  if [ $status -eq 0 ]; then
     echo PASS: Heap assertions caught a bad card mark
     exit $EXIT_TEST_WIN
  fi
else
  # as always, we lack a status code for inapplicable shell tests
  exit $EXIT_TEST_WIN
fi
