#!/bin/sh

# a superset of clean.sh, cleaning up not only automatically 
# generated files but other things (e.g. customization files)
# which shouldn't be in the distribution

# It's not unheard of to copy the old CMU CL docs into their
# traditional sbcl-0.6.x place. If so, it shouldn't end up in
# the distribution.
rm -rf doc/cmucl

# miscellaneous other customizations which shouldn't be propagated
rm -rf customize-target-features.lisp

# Fall through to ordinary cleanup.
sh clean.sh
