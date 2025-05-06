. ./subr.sh

run_sbcl --eval '(exit :code (or #+unix 0 2))'
if [ $? -eq 2 ] ; then echo $0: SKIPPING ; exit $EXIT_TEST_WIN ; fi

set -e

# run-sbcl.sh's pathname munging turns out always to have been
# insufficient in one way or another.
test_run_sbcl() (
    set -e
    args='--noinform --no-userinit --no-sysinit --disable-debugger --quit'
    #set -x # uncomment for eyeball debugging
    ## There are three ways run-sbcl.sh can fail or set things up badly.
    # (1) run-sbcl.sh will refuse to run because there are no build
    # artifacts relative to $run_sbcl_path.
    "$1" $args
    # (2) SBCL will start, but if SBCL_HOME is wrong, will be unable to
    # load contribs. (This is what 9d5be5e953 partially addressed.)
    "$1" $args --eval '(require :sb-posix)'
    # (3) SBCL will start, but if SBCL_HOME is a relative pathname,
    # then loading contribs will be sensitive to the dynamic value of
    # *DEFAULT-PATHNAME-DEFAULTS*. (There appears not to be consensus
    # whether SBCL ought to make SBCL-HOMEDIR-PATHNAME absolute during
    # startup, so holding that aside, run-sbcl.sh has to.)
    #
    # In order to ensure that MODULE-PROVIDE-CONTRIB can't succeed
    # when SBCL-HOMEDIR-PATHNAME is relative, we'll construct a
    # default directory pathname known not to exist, so that any merge
    # with it will also not exist.
    "$1" $args --eval '(setq *default-pathname-defaults*
                             (loop for i upfrom 0 below 1000
                                   as directory
                                     = (pathname
                                        (format nil "/nosuchdir.~d/" i))
                                   unless (probe-file directory)
                                   return directory
                                   finally
                                (error "test setup failure")))' \
               --eval '(require :sb-posix)'
)

# Sanity check: test the file in our build tree a couple of ways.
echo "testing run-sbcl.sh with an absolute path"
test_run_sbcl "$PWD"/../run-sbcl.sh

echo "testing run-sbcl.sh with a relative path"
test_run_sbcl ../run-sbcl.sh

# Next, test a couple ways that run-sbcl.sh could be invoked via a
# symlink. We'll need an absolute path to run-sbcl.sh in our build
# tree in order to construct symlinks that point to it.
if expr "$0" : "^[^/].*" > /dev/null; then
    test_file_directory_path="$(cd ./$(dirname "$0") && pwd)"
fi
# Now we can derive an absolute path to run-sbcl.sh relative to this
# file.
run_sbcl_path="$test_file_directory_path/../run-sbcl.sh"

# Now we're going to frob the file system, so let's do it properly.
# (for some definition of "properly?")
use_test_subdirectory_in_source_tree

# 9d5be5e953 (lp#1242643) addressed the possibility that run-sbcl.sh
# was a symlink into the repo. Let's exercise it, 8 years later.
echo "testing run-sbcl.sh when it's a symlink to an absolute path"
ln -s "$run_sbcl_path" ./run-sbcl-absolute-symlink.sh
test_run_sbcl ./run-sbcl-absolute-symlink.sh
rm ./run-sbcl-absolute-symlink.sh

# Test whether we can run-sbcl.sh through a symlink to a relative
# path.
echo "testing run-sbcl.sh when it's a symlink to a relative path"
# good god, what does this sed command even do?
ln -s $(pwd | sed 's|^/||; s|[^/][^/]*|..|g')/"$run_sbcl_path" ./run-sbcl-relative-symlink.sh
test_run_sbcl ./run-sbcl-relative-symlink.sh
rm ./run-sbcl-relative-symlink.sh

# Test whether we can run-sbcl.sh named using a path to the build
# directory that contains a space.
echo "testing run-sbcl.sh named by a path containing spaces"
ln -s `dirname $run_sbcl_path` ./'a b'
# Prior to 4a30189fbc, run-sbcl.sh would start the runtime using the
# core file path ./a b/output/sbcl.core, but unquoted.
# After 4a30189fbc, the path to the core file will be
# ${PWD}/a b/output/sbcl.core, still unquoted.
# 546416b34d changed the core file path handling to ensure
# it's always quoted.
test_run_sbcl "./a b/run-sbcl.sh"

exit $EXIT_TEST_WIN
