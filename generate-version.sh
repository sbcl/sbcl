#!/bin/sh

git_available_p() {
    # Check that (1) we have git (2) this is a git tree.
    if ( command -v git >/dev/null && git describe >/dev/null 2>/dev/null )
    then
        echo "ok"
    else
        echo ""
    fi
}

AVAILABLE=`git_available_p`
if [ -f version.lisp-expr -a -z "$AVAILABLE" ]
then
    # Relase tarball, leave version.lisp-expr alone.
    exit 0
elif [ -z "$AVAILABLE" ]
then
    echo "Can't run 'git describe' and version.lisp-expr is missing." >&2
    echo "To fix this, either install git or create a fake version.lisp-expr file." >&2
    echo "You can create a fake version.lisp-expr file like this:" >&2
    echo "    \$ echo '\"1.0.99.999\"' > version.lisp-expr" >&2
    exit 1
fi
# Build it.
version_head=`git rev-parse HEAD`
if grep -q "ref: refs/heads/.*" .git/HEAD > /dev/null 2>&1
then
    version_branchname=`cut -d / -f 3- < .git/HEAD`
else
    # Detached head.
    version_branchname="HEAD"
fi
if [ -z "$SBCL_BUILDING_RELEASE_FROM" ]
then
    if [ "`git rev-list HEAD --not origin/master`" = '' ]
    then
        # If origin/master contains all the commits on current
        # branch, use current head as the root instead.
        version_root="$version_branchname"
    else
        version_root="origin/master"
    fi
else
    version_root="$SBCL_BUILDING_RELEASE_FROM"
fi
version_base=`git rev-parse "$version_root"`
version_tag=`git describe --tags --match="sbcl*" --abbrev=0 $version_base`
version_release=`echo $version_tag | sed -e 's/sbcl[_-]//' | sed -e 's/_/\./g'`
# Using wc -l instead of --count argument to rev-list because
# pre-1.7.2 Gits are still common out in the wilderness.
version_n_root=`git rev-list $version_base --not $version_tag | wc -l | sed -e 's/[ \t]//g'`
version_n_branch=`git rev-list HEAD --not $version_base | wc -l | sed -e 's/[ \t]//g'`
if [ -z "$NO_GIT_HASH_IN_VERSION" ]
then
    version_hash="-`git rev-parse --short $version_head`"
else
    version_hash=""
fi
if git diff HEAD --no-ext-diff --quiet --exit-code
then
    version_dirty=""
else
    version_dirty="-dirty"
fi
# Now that we have all the pieces, put them together.
cat >version.lisp-expr <<EOF
;;; This file is auto-generated using generate-version.sh. Every time
;;; you re-run make.sh, this file will be overwritten if you are
;;; working from a Git checkout.
EOF
if [ "$version_base" = "$version_head" ]
then
    if [ "0" = "$version_n_root" ]
    then
        printf "\"%s%s\"\n" \
            $version_release $version_dirty >>version.lisp-expr
    else
        printf "\"%s.%s%s%s\"\n" \
            $version_release $version_n_root \
            $version_hash $version_dirty >>version.lisp-expr
    fi
else
    echo "base=$version_base"
    echo "head=$version_head"
    printf "\"%s.%s.%s.%s%s%s\"\n" \
        $version_release $version_n_root \
        $version_branchname $version_n_branch \
        $version_hash $version_dirty >>version.lisp-expr
fi
