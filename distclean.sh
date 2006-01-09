#!/bin/sh
set -e

# a superset of clean.sh, cleaning up not only automatically
# generated files but other things (e.g. customization files)
# which shouldn't be in the distribution

# It's not unheard of to copy the old CMU CL docs into their
# traditional sbcl-0.6.x place. If so, it shouldn't end up in
# the distribution.
rm -rf doc/cmucl

# miscellaneous other customizations which shouldn't be propagated
rm -rf customize-target-features.lisp

# Since the CVS/ subdirectories on my (WHN) machine have CVS/Root
# containing
#    :ext:wnewman@cvs.sbcl.sourceforge.net:/cvsroot/sbcl
# they're not useful for anyone else, so blow them away. (And even
# if we could set them up to refer to CVS in a way that someone else
# could use, perhaps referring to SourceForge anoncvs, what'd be the
# point? I'd expect a comfortable majority of those who want to do
# CVS operations would be inclined to start with "cvs co" anyway.)
find . \( -type d -a -name CVS \) -print | xargs rm -r

# Fall through to ordinary cleanup.
sh clean.sh
