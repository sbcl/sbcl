# Git workflow for SBCL

## Version numbering

Historically each SBCL commit incremented the number in
version.lisp-expr, and prepended that version number to the first line
of the commit message. For CVS this served us well, but since Git
makes it easier for anyone to create branches that run in parallel to
the current "master" timeline, it destroys the illusion of a single
official timeline defined through version.lisp-expr.

In Git workflow, version.lisp-expr no longer exists in the repository,
nor is the version number prepended to commit messages.

Instead, we construct a version number as follows when building SBCL
or generating a source tarball:

For branch master:

  release.commits-on-master-since-release.sha1

  Eg. 1.0.48.20-152c97d

                          Last release: 1.0.48
       Commits on master after release: 20
          SHA1 abbrev for current HEAD: 152c97d

  If there are no commits on master since the last release, both the
  count and the SHA1 are dropped.

For other branches:

  release.commits-on-branch-and-master-since-release.branch.commits-on-branch.sha1

  Eg. 1.0.44.26.wip-pretty-backtraces.4-674f875

                          Last release: 1.0.44
       Commits on master after release: 26
                                Branch: wip-pretty-backtraces
   Commits on branch but not on master: 4
          SHA1 abbrev for current HEAD: 674f875

In both cases -dirty is appended to the version number if the tree
isn't clean when building.

Anyone who publishes binaries built using an altered version, should
do so on a branch named appropriately, so that the binaries identify
themselves as 1.0.50.debian.2 or whatever. If they wish to use a
source release instead working from Git, they should identify their
changes with an appropriate edit to version.lisp-expr.

To cater for those whose existing processes really don't like the
SHA1s part in version numbers, setting NO_GIT_HASH_IN_VERSION=t in the
environment for make.sh will make the version number generator leave
out the hash.

## Making a release (release.sh)

Short story: use `release.sh`.

`release.sh` makes a release *locally.* This means it will perform all
actions required for the release in the local git checkout, and will
then instruct you how to proceed in order to push the release to the
outside world.

###Synopsis:

    ./release.sh VERSION [-s]

**VERSION** is the version to make a release for. Example: `1.0.46`.

**-s** instructs `git tag` to create a gpg-signed tag for this
release. Highly recommended.

###Description:

`release.sh` will perform these actions:

* Check that the local checkout is clean.
* Update NEWS and make a commit stating the release version number
* Make an sbcl.<VERSION> tag and optionally sign it.
* Build SBCL
* Run tests
* Build SBCL with the SBCL that just had tests pass
* Build docs
* Create source, binary, documentation tarballs
* Sign these tarballs
* Give you further instructions.

After release.sh is done, you can inspect the results, and commence
struggling with the SF.net file release system from hell. You are very
brave.
