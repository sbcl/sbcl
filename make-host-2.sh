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

echo //entering make-host-2.sh

# In some cases, a debugging build of the system will creates a core
# file output/after-xc.core in the next step. In cases where it
# doesn't, it's confusing and basically useless to have any old copies
# lying around, so delete:
rm -f output/after-xc.core

# In a fresh host Lisp invocation, load and run the cross-compiler to
# create the target object files describing the target SBCL.
#
# (There are at least three advantages to running the cross-compiler in a
# fresh host Lisp invocation instead of just using the same Lisp invocation
# that we used to compile it:
#   (1) It reduces the chance that the cross-compilation process
#       inadvertently comes to depend on some weird compile-time
#       side effect.
#   (2) It reduces peak memory demand (because definitions wrapped in
#       (EVAL-WHEN (:COMPILE-TOPLEVEL :EXECUTE) ..) aren't defined
#       in the fresh image).
#   (3) It makes it easier to jump in and retry a step when tweaking
#       and experimenting with the bootstrap procedure.
# Admittedly, these don't seem to be enormously important advantages, but
# the only disadvantage seems to be the extra time required to reload
# the fasl files into the new host Lisp, and that doesn't seem to be
# an enormously important disadvantage, either.)
echo //running cross-compiler to create target object files
$SBCL_XC_HOST <<-'EOF' || exit 1

        ;;;
        ;;; Set up the cross-compiler.
        ;;;
	(setf *print-level* 5 *print-length* 5)
	(load "src/cold/shared.lisp")
	(in-package "SB-COLD")
	(setf *host-obj-prefix* "obj/from-host/"
	      *target-obj-prefix* "obj/from-xc/")
	(load "src/cold/set-up-cold-packages.lisp")
	(load "src/cold/defun-load-or-cload-xcompiler.lisp")
	(load-or-cload-xcompiler #'host-load-stem)
        (defun proclaim-target-optimization ()
          (let ((debug (if (position :sb-show *shebang-features*) 2 1)))
	    (sb-xc:proclaim `(optimize (compilation-speed 1)
	                               (debug ,debug)
				       (sb!ext:inhibit-warnings 2)
				       ;; SAFETY = SPEED (and < 3) should 
				       ;; reasonable safety, but might skip 
				       ;; some unreasonably expensive stuff
				       ;; (e.g. %DETECT-STACK-EXHAUSTION
				       ;; in sbcl-0.7.2).
                                       (safety 2)
                                       (space 1)
				       (speed 2)))))
        (compile 'proclaim-target-optimization)
	(defun in-target-cross-compilation-mode (fn)
	  "Call FN with everything set up appropriately for cross-compiling
	  a target file."
	  (let (;; In order to increase microefficiency of the target Lisp, 
		;; enable old CMU CL defined-function-types-never-change
		;; optimizations. (ANSI says users aren't supposed to
		;; redefine our functions anyway; and developers can
		;; fend for themselves.)
		#!-sb-fluid (sb!ext:*derive-function-types* t)
		;; Let the target know that we're the cross-compiler.
		(*features* (cons :sb-xc *features*))
		;; We need to tweak the readtable..
		(*readtable* (copy-readtable)))
            ;; ..in order to make backquotes expand into target code
            ;; instead of host code.
            ;; FIXME: Isn't this now taken care of automatically by
            ;; toplevel forms in the xcompiler backq.lisp file?
            (set-macro-character #\` #'sb!impl::backquote-macro)
            (set-macro-character #\, #'sb!impl::comma-macro)
	    ;; Control optimization policy.
            (proclaim-target-optimization)
            ;; Specify where target machinery lives.
            (with-additional-nickname ("SB-XC" "SB!XC")
              (funcall fn))))
	(compile 'in-target-cross-compilation-mode)
	(setf *target-compile-file* 'sb-xc:compile-file)
	(setf *target-assemble-file* 'sb!c:assemble-file)
	(setf *in-target-compilation-mode-fn*
	      #'in-target-cross-compilation-mode)

        ;;;
        ;;; Run the cross-compiler to produce cold fasl files.
        ;;;
	(load "src/cold/compile-cold-sbcl.lisp")
 
        ;;; 
        ;;; miscellaneous tidying up and saving results
        ;;; 
	(let ((filename "output/object-filenames-for-genesis.lisp-expr"))
	  (ensure-directories-exist filename :verbose t)
	  (with-open-file (s filename :direction :output)
	    (write *target-object-file-names* :stream s :readably t)))
        ;; Let's check that the type system was reasonably sane. (It's
	;; easy to spend a long time wandering around confused trying
	;; to debug cold init if it wasn't.)
	(when (position :sb-test *shebang-features*)
	  (load "tests/type.after-xc.lisp"))
	;; If you're experimenting with the system under a
        ;; cross-compilation host which supports CMU-CL-style SAVE-LISP,
        ;; this can be a good time to run it. The resulting core isn't
	;; used in the normal build, but can be handy for experimenting
	;; with the system. (See slam.sh for an example.)
	(when (position :sb-after-xc-core *shebang-features*)
          #+cmu (ext:save-lisp "output/after-xc.core" :load-init-file nil)
          #+sbcl (sb-ext:save-lisp-and-die "output/after-xc.core")
	  )
	EOF

# Run GENESIS (again) in order to create cold-sbcl.core. (The first
# time was before we ran the cross-compiler, in order to create the
# header file which was needed in order to run gcc on the runtime
# code.)
sh make-genesis-2.sh
