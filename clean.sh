#!/bin/sh

# Remove everything in directories which are only used for output.
# In most cases, we can remove the directories, too.
#
# (We don't remove all the directories themselves for a stupid technical
# reason: "gmake clean" in the src/runtime directory gets unhappy if the
# output/ directory doesn't exist, because it tries to build Depends
# before it cleans itself, and src/c-runtime/sbcl.h is a symlink into
# the output/ directory, and it gets the gcc dependency processing gets
# all confused trying to figure out a header file which is a symlink
# into a directory which doesn't exist. We'd like to be able to run
# this script (including "gmake clean" in the src/runtime directory)
# several times in a row without failure.. so we leave the output/
# directory in place.)
rm -rf obj/* output/* doc/user-manual \
  doc/user-manual.junk doc/DBTOHTML_OUTPUT_DIR*
# (The doc/user-manual.junk and doc/DBTOHTML_OUTPUT_DIR* directories
# are created by the Cygnus db2html script when it formats the the
# user manual, and since this db2html script is the one which is
# currently used to format the manual for the standard binary
# distribution, we automatically clean up after it here in the 
# standard clean.sh file.)

# Ask some other directories to clean themselves up.
original_pwd=`pwd`
for d in tools-for-build; do
    cd $d > /dev/null
    # I hope the -s option is standard. At least GNU make and BSD make
    # support it. It silences make, since otherwise the output from
    # this script is just the operations done by these make's, which
    # is misleading when this script does lotso other operations too.
    # -- WHN
    make -s clean
    cd $original_pwd  > /dev/null
done

# Within all directories, remove things which don't look like source
# files. Some explanations:
#   (symlinks)
#     are never in the sources; they must've been created
#   sbcl
#     the runtime environment, created by compiling C code
#   sbcl.h 
#     information about Lisp code needed to build the runtime environment,
#     created by running GENESIS
#   Config, target
#     architecture-dependent or OS-dependent symlinks
#   *.htm, *.html
#     probably machine-generated translation of DocBook (*.sgml) files
#   core
#     probably a Unix core dump -- not part of the sources anyway
#   *.o, *.lib, *.nm
#     results of C-style linking, assembling, etc.
#   *.core, *.map
#     looks like SBCL SAVE-LISP-AND-DIE or GENESIS output, and
#     certainly not source
#   *~, #*#, TAGS
#     common names for editor temporary files
#   .#*
#     rubbish left behind by CVS updates
#   *.htm, *.html
#     The system doc sources are SGML, any HTML is
#     automatically-generated output.
#   depend
#     made by "make depend" (or "gmake depend" or some such thing)
#   *.x86f, *.axpf, *.lbytef, *.fasl
#     typical extensions for fasl files
find . \( \
	-type l -or \
	-name '*~' -or \
	-name '#*#' -or \
	-name '.#*' -or \
	-name '?*.x86f' -or \
	-name '?*.axpf' -or \
	-name '?*.lbytef' -or \
	-name '?*.fasl' -or \
	-name 'core' -or \
	-name '?*.core' -or \
	-name '*.map' -or \
	-name '*.nm' -or \
	-name '*.host-obj' -or \
	-name '*.lisp-obj' -or \
	-name '*.target-obj' -or \
	-name '*.lib' -or \
	-name '*.tmp' -or \
	-name '*.o' -or \
	-name 'sbcl' -or \
	-name 'sbcl.h' -or \
	-name 'depend' -or \
	-name '*.htm' -or \
	-name '*.html' -or \
	-name 'TAGS' -or \
	-name 'local-target-features.lisp-expr' \) -print | xargs rm -f
