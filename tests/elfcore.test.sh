#!/bin/sh

# tests related to ELFinated .core files

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

. ./subr.sh

run_sbcl <<EOF
  #+(and linux elf sb-thread)
  (let ((s (find-symbol "IMMOBILE-SPACE-OBJ-P" "SB-KERNEL")))
    (when (and s (funcall s #'car)) (exit :code 0))) ; good
 (exit :code 2) ; otherwise
EOF
status=$?
if [ $status != 0 ]; then # test can't be executed
    # we don't have a way to exit shell tests with "inapplicable" as the result
    exit $EXIT_TEST_WIN
fi

set -e # exit on error

# Ensure that we're not running a stale shrinkwrap-sbcl
(cd $SBCL_PWD/../src/runtime ; rm -f shrinkwrap-sbcl* ; make shrinkwrap-sbcl)

# Prevent style-warnings in the editcore script, but don't assume that it
# can be compiled in the first place unless actually doing the ELFcore tests.
run_sbcl <<EOF
  (let ((*evaluator-mode* :interpret))
    (load "../tests/test-util")
    (load "../tools-for-build/corefile"))
  (test-util:with-scratch-file (fasl "fasl")
    (assert (not (nth-value 1
                  (compile-file "../tools-for-build/editcore"
                                :output-file fasl :print nil)))))
EOF

$SBCL_PWD/../src/runtime/shrinkwrap-sbcl --disable-debugger --no-sysinit --no-userinit --noprint <<EOF
#+x86-64 (sb-vm::%alloc-immobile-symbol "junk") ; crashed 'cause I forgot to use rip-relative-EA

;; Test that the link step did not use --export-dynamic
(assert (gethash '("TEXT_SPACE_START") (car sb-sys:*linkage-info*))) ; C symbol exists
; but dlsym() can't see it
(assert (not (sb-sys:find-dynamic-foreign-symbol-address "TEXT_SPACE_START")))
; but we can read the value
(assert (funcall (compile nil '(lambda () sb-vm:text-space-start))))

;; Test that CODE-SERIAL# is never 0 except for simple-fun-less objects
(sb-vm:map-allocated-objects
 (lambda (obj type size)
   (declare (ignore size))
   (when (and (= type sb-vm:code-header-widetag)
              (> (sb-kernel:code-n-entries obj) 0))
     (assert (/= (sb-kernel:%code-serialno obj) 0))))
 :all)

;; Test that lisp linkage cells were bypassed.
;; (This test was copied from elf-sans-immobile)
(in-package sb-impl)
(defun asm-string () (with-output-to-string (ss) (disassemble 'y-or-n-p :stream ss)))
#+x86-64 (assert (search "#'QUERY-READ-CHAR" (asm-string)))
(defun query-read-char () #\y) ; will undo static linkage
#+x86-64 (assert (search "; QUERY-READ-CHAR" (asm-string)))
(assert (eq (let ((*query-io* (make-broadcast-stream))) (y-or-n-p)) t))
(format t "~&Function redef OK~%")
EOF
(cd $SBCL_PWD/../src/runtime ; rm -f shrinkwrap-sbcl shrinkwrap-sbcl.s shrinkwrap-sbcl-core.o shrinkwrap-sbcl.core)

# reaching here means no crash happened in the allocator
# and that the fixups were rewritten into C data space
echo Basic smoke test: PASS

create_test_subdirectory
tmpcore=$TEST_DIRECTORY/$TEST_FILESTEM.tmpcore

run_sbcl <<EOF
  (setq sb-c:*compile-to-memory-space* :dynamic)
  ;; Call an assembly routine from dynamic space
  (defun f (x y z) (+ x (- y z)))
  (compile 'f)
  ;; :AUTO physically allocates code in immobile space (unless it fills up)
  ;; using instruction forms that do not assume immobile space
  ;; (for that same reason) which caused a glitch in editcore.
  (setq sb-c:*compile-to-memory-space* :auto)
  (defun g (fun &rest args) (apply fun args)) ; exercise TAIL-CALL-VARIABLE
  (compile 'g)
#|
;; The SETF below is sort of illegal. It was an attempt to assert that defrag
;; of immobile-space followed by ELF conversion did not accidentally affect
;; the SYMBOL-HASH bits which were erroneously getting descriptor treatment.
;; The problems in making a test case are as follows: to create a bit pattern
;; _incorrectly_ altered to point to ELF text space, it would have to be a raw
;; word that seems to overlap text space after defrag_immobile_space.
;; Firstly that's technically impossible with 48-bit virtual address space unless
;; all the highest bits of the quasi-random hash are all zero, which has 1/(2^16)
;; chance; and near-impossibility of that aside, you'd need a hash word randomly
;; matching a function address *after* defrag whose address you can not possibly
;; guess predictably enough to make into a test.

;; What if you set the hash word to the address of the assembler routines?
;; An interesting trick that almost works. The problem is that the word appears to
;; have a linkage index of 1 because the low 4 bits are #b1111, the 3 low being
;; ignored. Now come the next two problems: (1) it breaks the 1:1 mapping from
;; symbol to linkage-index, and (2) defrag occurs from the space _onto_ itself,
;; reusing addresses. A temporary copy of the defragged spaces physically resides
;; somewhere random, but we deposit logical addresses when writing FPs into the
;; current space. (The temp space is copied en masse back)

;; So during computation of new addresses, rearranging and compacting as much as
;; possible, we may forwarding a word to an ever-so-slightly higher address
;; that now holds a completely random and irrelevant bit pattern.
;; Consider this actual example from trying to make the test pass:
;; - linkage index 1 holds #xb80072d530 for simple-fun #xb80072d52b
;; - defrag forwards that function to #xb80072d600 depositing a FP in #xb80072d520
;; - processing the symbol which names that function calls adjust_linkage_cell
;;   which sees linkage cell 1 and rewrites it to #xb80072d600
;; - processing #:GOOFY symbol sees cell 1 and asserts that #xb80072d600 is
;;   forwarded. But it isn't! It's random data.
;;
;; The problem is that we have no record of whether a linkage cell was visited.
;; They're not really objects, and in real usage this error can't occur.
;; I suppose one way to do this might just be to skip calling adjust_linkage_cell
;; when visiting symbols and fdefns, and instead perform one pass over all cells
;; as a separate space.  Maybe that's the right thing. But not now.
|#
  (defvar *the-bits* (sb-kernel:get-lisp-obj-address sb-fasl:*assembler-routines*))
  (defvar *s* (make-symbol "GOOFY"))
  ;; bits of symbol-hash slot in #:GOOFY become *the-bits*
  #+nil (setf sb-vm::(sap-ref-word
             (int-sap (get-lisp-obj-address cl-user::*s*))
             (- (ash symbol-hash-slot word-shift) other-pointer-lowtag)) *the-bits*)

  (save-lisp-and-die "${tmpcore}")
EOF

m_arg=`run_sbcl --eval '(progn #+sb-core-compression (princ " -lzstd") #+x86 (princ " -m32"))' --quit`

(cd $SBCL_PWD/../src/runtime ; rm -f libsbcl.a; make libsbcl.a)
run_sbcl --script ../tools-for-build/elftool.lisp split \
  ${tmpcore} $TEST_DIRECTORY/elfcore-test.s
# I guess we're going to have to hardwire the system libraries
# until I can figure out how to get a Makefile to work, which is fine
# for now because elfination is only supported on linux/x86-64.
./run-compiler.sh -no-pie -g -o $TEST_DIRECTORY/elfcore-test \
  $TEST_DIRECTORY/elfcore-test.s \
  $TEST_DIRECTORY/elfcore-test-core.o \
  $SBCL_PWD/../src/runtime/libsbcl.a -lm -lpthread ${m_arg}

$TEST_DIRECTORY/elfcore-test $SBCL_ARGS --eval '(assert (zerop (f 1 2 3)))' --quit
echo Custom core: PASS

./run-compiler.sh -no-pie -g -o $TEST_DIRECTORY/relocating-elfcore-test \
  $TEST_DIRECTORY/elfcore-test.s \
  $TEST_DIRECTORY/elfcore-test-core.o \
  $SBCL_PWD/../tests/heap-reloc/fake-mman.c \
  $SBCL_PWD/../src/runtime/libsbcl.a -lm -lpthread ${m_arg}

(cd $SBCL_PWD/../src/runtime ; rm -f libsbcl.a)

export SBCL_FAKE_MMAP_INSTRUCTION_FILE=heap-reloc/fakemap_64
i=1
while [ $i -le 6 ]
do
  echo Trial $i
  i=`expr $i + 1`
  $TEST_DIRECTORY/relocating-elfcore-test $SBCL_ARGS --eval '(assert (zerop (f 1 2 3)))' --quit
done

exit $EXIT_TEST_WIN
