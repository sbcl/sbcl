#!/bin/sh

# a superset of clean.sh, cleaning up not only automatically 
# generated files but other things (e.g. customization files)
# which shouldn't be in the distribution

rm customize-target-features.lisp
sh clean.sh
