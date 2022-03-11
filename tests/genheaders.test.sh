#!/bin/sh

set -e
export TEST_BASEDIR=${TMPDIR:-/tmp}
. ./subr.sh
create_test_subdirectory

run_sbcl <<EOF
;; technically this is "skipped" if not fasteval, but we don't have "skipped"
;; as a status code from shell tests.
(unless (and (find-package "SB-INTERPRETER")
             ;; host with #+sb-devel hangs, not sure why
             (not (member :sb-devel *features*)))
 ;; exit normally so that the shell doesn't exit abnormally (as per "set -e")
 (exit))
(setq *evaluator-mode* :interpret)
(defvar *sbcl-local-target-features-file* "../local-target-features.lisp-expr")
(load "../src/cold/shared.lisp")
(load "../src/cold/set-up-cold-packages.lisp")
(load "../tools-for-build/corefile.lisp")
(in-package "SB-COLD")
(defvar *target-sbcl-version* (read-from-file "../version.lisp-expr"))
(in-host-compilation-mode
 (lambda (&aux (sb-xc:*features* (cons :c-headers-only sb-xc:*features*)))
   (do-stems-and-flags (stem flags 1)
     (when (member :c-headers flags)
       (handler-bind ((style-warning (function muffle-warning)))
         (load (merge-pathnames (stem-remap-target stem)
                                (make-pathname :directory '(:relative :up) :type "lisp"))))))
   (load "../src/compiler/generic/genesis.lisp")))
(genesis :c-header-dir-name "$TEST_DIRECTORY/" :verbose t)
(assert (probe-file "$TEST_DIRECTORY/gc-tables.h"))
(dolist (pathname (directory "$TEST_DIRECTORY/*.*")) (delete-file pathname))
EOF

exit $EXIT_TEST_WIN
