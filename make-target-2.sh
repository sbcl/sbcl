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

echo //entering make-target-2.sh

# Do warm init stuff, e.g. building and loading CLOS, and stuff which
# can't be done until CLOS is running.
#
# Note that it's normal for the newborn system to think rather hard at
# the beginning of this process (e.g. using nearly 100Mb of virtual memory
# and >30 seconds of CPU time on a 450MHz CPU), and unless you built the
# system with the :SB-SHOW feature enabled, it does it rather silently,
# without trying to tell you about what it's doing. So unless it hangs
# for much longer than that, don't worry, it's likely to be normal.
echo //doing warm init
./src/runtime/sbcl \
--core output/cold-sbcl.core \
--sysinit /dev/null --userinit /dev/null <<-'EOF' || exit 1

        (sb!int:/show "hello, world!")

        ;; Do warm init.
	(let ((*print-length* 10)
	      (*print-level* 10))
          (sb!int:/show "about to LOAD warm.lisp")
	  (load "src/cold/warm.lisp"))

        ;; Unintern no-longer-needed stuff before the possible PURIFY
        ;; in SAVE-LISP-AND-DIE.
        #-sb-fluid (sb-impl::!unintern-init-only-stuff)

	;; FIXME: Why is it that, at least on x86 sbcl-0.6.12.46,
	;; GC :FULL T isn't nearly as effective as PURIFY here?
	;; (GC :FULL T gets us down to about 38 Mbytes, but PURIFY
	;; gets us down to about 19 Mbytes.)
	(let ((*gc-notify-stream* *standard-output*))
	  (sb-int:/show "done with warm.lisp, about to GC :FULL T")
	  (gc :full t))

        (sb-int:/show "done with warm.lisp, about to SAVE-LISP-AND-DIE")
	;; Even if /SHOW output was wanted during build, it's probably
	;; not wanted by default after build is complete. (And if it's
	;; wanted, it can easily be turned back on.)
	#+sb-show (setf sb-int:*/show* nil)
	(sb-ext:save-lisp-and-die "output/sbcl.core" :purify t)
	EOF
