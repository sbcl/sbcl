#!/bin/sh

# This is a script to be run as part of make.sh. The only time you'd
# want to run it by itself is if you're trying to cross-compile the
# system or if you're doing some kind of troubleshooting.

# This software is part of the SBCL system. See the README file for
# more information.
#
# This software is derived from the CMU CL system, which was
# written at Carnegie Mellon University and released into the
# public domain. The software is in the public domain and is
# provided with absolutely no warranty. See the COPYING and CREDITS
# files for more information.

echo //entering make-host-1.sh

# Compile and load the cross-compiler. (We load it here not because we're
# about to use it, but because it's written under the assumption that each
# file will be loaded before the following file is compiled.)
#
# Also take the opportunity to compile and load genesis, to create the
# header file sbcl.h which will be needed to create the C runtime
# environment.
echo //building cross-compiler, and doing first genesis
$SBCL_XC_HOST <<-'EOF' || exit 1
	;; (We want to have some limit on print length and print level
	;; during bootstrapping because PRINT-OBJECT only gets set
	;; up rather late, and running without PRINT-OBJECT it's easy
	;; to fall into printing enormous (or infinitely circular)
	;; low-level representations of things.)
	(setf *print-level* 5 *print-length* 5)
	(load "src/cold/shared.lisp")
	(in-package "SB-COLD")
	(setf *host-obj-prefix* "obj/from-host/")
	(load "src/cold/set-up-cold-packages.lisp")
	(load "src/cold/defun-load-or-cload-xcompiler.lisp")
	(load-or-cload-xcompiler #'host-cload-stem)
        ;; Let's check that the type system is reasonably sane. (It's
	;; easy to spend a long time wandering around confused trying
	;; to debug cross-compilation if it isn't.)
	(when (find :sb-test *shebang-features*)
	  (load "tests/type.before-xc.lisp"))
        (host-cload-stem "src/compiler/generic/genesis")
	(sb!vm:genesis :c-header-file-name "src/runtime/sbcl.h")
	EOF
