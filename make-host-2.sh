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

# In a fresh host Lisp invocation, load and run the cross-compiler to
# create the target object files describing the target SBCL.
#
# (There are at least three advantages to running the cross-compiler in a
# fresh host Lisp invocation instead of just using the same Lisp invocation
# that we used to compile it:
#   (1) It reduces the chance that the cross-compilation process
#       inadvertently comes to depend on some weird compile-time
#       side-effect.
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
	(setf *print-level* 5 *print-length* 5)
	(load "src/cold/shared.lisp")
	(in-package "SB-COLD")
	(setf *host-obj-prefix* "obj/from-host/"
	      *target-obj-prefix* "obj/from-xc/")
	(load "src/cold/set-up-cold-packages.lisp")
	(load "src/cold/defun-load-or-cload-xcompiler.lisp")
	(load-or-cload-xcompiler #'host-load-stem)
        (defun proclaim-target-optimization ()
          (let ((debug (if (find :sb-show *shebang-features*) 2 1)))
	    (sb-xc:proclaim `(optimize (compilation-speed 1)
	                               (debug ,debug)
				       (sb!ext:inhibit-warnings 2)
                                       (safety 3)
                                       (space 1)
				       (speed 2)))))
        (compile 'proclaim-target-optimization)
	(defun in-target-cross-compilation-mode (fn)
	  "Call FN with everything set up appropriately for cross-compiling
	  a target file."
	  (let (;; Life is simpler at genesis/cold-load time if we
	        ;; needn't worry about byte-compiled code.
	        (sb!ext:*byte-compile-top-level* nil)
		;; Let the target know that we're the cross-compiler.
		(*features* (cons :sb-xc *features*))
                ;; We need to tweak the readtable..
                (*readtable* (copy-readtable))
		;; In order to reduce peak memory usage during GENESIS,
		;; it helps to stuff several toplevel forms together 
                ;; into the same function.
		(sb!c::*top-level-lambda-max* 10))
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
	(load "src/cold/compile-cold-sbcl.lisp")
	(let ((filename "output/object-filenames-for-genesis.lisp-expr"))
	  (ensure-directories-exist filename :verbose t)
	  (with-open-file (s filename :direction :output)
	    (write *target-object-file-names* :stream s :readably t)))
	;; If you're experimenting with the system under a
        ;; cross-compilation host which supports CMU-CL-style SAVE-LISP,
        ;; this can be a good time to run it,
	;; The resulting core isn't used in the normal build, but
        ;; can be handy for experimenting with the system.
	(when (find :sb-show *shebang-features*)
          #+cmu (ext:save-lisp "output/after-xc.core" :load-init-file nil)
          #+sbcl (sb-ext:save-lisp-and-die "output/after-xc.core"))
	EOF

# Run GENESIS again in order to create cold-sbcl.core.
#
# In a fresh host Lisp invocation, load the cross-compiler (in order
# to get various definitions that GENESIS needs, not in order to
# cross-compile GENESIS, compile and load GENESIS, then run GENESIS.
# (We use a fresh host Lisp invocation here for basically the same
# reasons we did before when loading and running the cross-compiler.)
#
# (This second invocation of GENESIS is done because in order to
# create a .core file, as opposed to just a .h file, GENESIS needs
# symbol table data on the C runtime, which we can get only after the 
# C runtime has been built.)
echo //loading and running GENESIS to create cold-sbcl.core
$SBCL_XC_HOST <<-'EOF' || exit 1
	(setf *print-level* 5 *print-length* 5)
	(load "src/cold/shared.lisp")
	(in-package "SB-COLD")
	(setf *host-obj-prefix* "obj/from-host/"
	      *target-obj-prefix* "obj/from-xc/")
	(load "src/cold/set-up-cold-packages.lisp")
	(load "src/cold/defun-load-or-cload-xcompiler.lisp")
	(load-or-cload-xcompiler #'host-load-stem)
	(defparameter *target-object-file-names*
	  (with-open-file (s "output/object-filenames-for-genesis.lisp-expr"
	                     :direction :input)
	    (read s)))
	(host-load-stem "compiler/generic/genesis")
	(sb!vm:genesis :object-file-names *target-object-file-names*
		       :c-header-file-name "output/sbcl2.h"
		       :symbol-table-file-name "src/runtime/sbcl.nm"
		       :core-file-name "output/cold-sbcl.core"
		       ;; The map file is not needed by the system, but can
		       ;; be very handy when debugging cold init problems.
		       :map-file-name "output/cold-sbcl.map")
	EOF

echo //testing for consistency of first and second GENESIS passes
if cmp src/runtime/sbcl.h output/sbcl2.h; then
    echo //sbcl2.h matches sbcl.h -- good.
else
    echo error: sbcl2.h does not match sbcl.h.
    exit 1
fi
