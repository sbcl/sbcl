ASDF: Another System Definition Facility
========================================

What is ASDF?
-------------

ASDF is the de facto standard build facility for Common Lisp.
Your Lisp implementation probably contains a copy of ASDF,
which you can load using `(require "asdf")`.

If you come from the C/C++ world, ASDF covers a bit of what each of
make, autoconf, dlopen and libc do for C programs:
it orchestrates the compilation and dependency management,
handles some of the portability issues, dynamically finds and loads code,
and offers some portable system access library.
Except everything is different in Common Lisp, and ultimately much simpler,
though it requires acquiring some basic concepts.
Importantly, ASDF builds all software in the current Lisp image,
as opposed to building software into separate processes.

To use ASDF, read our manual:
    <http://common-lisp.net/project/asdf/asdf.html>

The first few sections, Loading ASDF, Configuring ASDF and Using ASDF,
will get you started as a simple user.
If you want to define your own systems, further read the section
Defining systems with defsystem.

The manual is also in the [doc/](doc/) subdirectory, and can be prepared with:

    make doc


ASDF 3 now includes an extensive runtime support library:
[UIOP, the Utilities for Implementation- and OS- Portability](uiop/).
Its documentation unhappily lies mainly in the source code and docstrings.
See [uiop/README.md](uiop/README.md) for an introduction.

More information and additional links can be found on ASDF's home page at:
    <http://common-lisp.net/project/asdf/>


Quick Start
-----------

Just use `(require "asdf")` to load your implementation-provided ASDF.

If it is recent enough (3.0 or later, check its `(asdf:asdf-version)`),
then it will automatically upgrade to the ASDF provided as source code,
assuming the source code in under a path registered by the source-registry.
If it isn't present or isn't recent enough, we recommend you install a recent
ASDF release using [tools/install-asdf.lisp](tools/install-asdf.lisp)


Building and testing it
-----------------------

First, make sure ASDF is checked out under a path registered by the source-registry,
if that isn't the case yet (see the manual). One place would be:

    ~/.local/share/common-lisp/source/asdf/

or, assuming your implementation provides ASDF 3.1 or later:

    ~/common-lisp/asdf/


If you cloned our git repository, bootstrap a copy of `build/asdf.lisp` with:

    make

Before you may run tests, you need a few CL libraries.
The simplest way to get them is as follows, but read below:

    make ext

The above make target uses `git submodule update --init` to download
all these libraries using git. If you don't otherwise maintain your
own set of carefully controlled CL libraries, that's what you want to use.
However, if you do maintain your own set of carefully controlled CL libraries
then you will want to use whichever tools you use (e.g. `quicklisp`, `clbuild`,
or your own scripts around `git`) to download these libraries:
`alexandria`, `closer-mop`, `cl-ppcre`, `fare-mop`, `fare-quasiquote`,
`fare-utils`, `inferior-shell`, `lisp-invocation`, `named-readtables`, `optima`.

If you are a CL developer, you may already have them, or may want
to use your own tools to download a version of them you control.
If you use [Quicklisp](https://www.quicklisp.org/), you may let
Quicklisp download those you don't have.
In these cases, you do NOT want to use the git submodules from `make ext`.
Otherwise, if you want to let ASDF download known-working versions
of its dependencies, you can do it with:

    make ext

To run all the tests on your favorite Lisp implementation `$L`,
choose your most elaborate installed system `$S`, and try:

    make t u l=$L s=$S


Debugging tip
-------------

To interactively debug ASDF, you may load it in such a way that `M-.` will work,
by installing the source code, and running:

    (asdf:load-system :uiop) ;; loading uiop is simple
    (map () 'load ;; loading asdf/defsystem is tricky
     (mapcar 'asdf:component-pathname
      (asdf::required-components :asdf/defsystem :keep-component 'asdf:cl-source-file)))

Note that the above can be adapted in a general recipe to get all the files in a system, in order.
To also have the files in systems it transitively depends on, add the `:other-systems t` keyword
argument to the call to `asdf::required-components`.


What has changed?
-----------------

You can consult the [debian/changelog](debian/changelog) for an overview of the
significant changes in each release, and
the `git log` for a detailed description of each commit.


How do I navigate this source tree?
-----------------------------------

* [asdf.asd](asdf.asd)
    * The system definition for building ASDF with ASDF.

* `*.lisp`
    * The source code files for `asdf/defsystem`.
      See [asdf.asd](asdf.asd) for the order in which they are loaded.

* [uiop/](uiop/)
    * Utilities of Implementation- and OS- Portability,
      the portability layer of ASDF. It has its own [README](uiop/README.md),
      and functions all have docstrings.

* [Makefile](Makefile)
    * a `Makefile` for bootstrap and development purposes.

* [tools/](tools/)
    * Some scripts to help ASDF users
        * [load-asdf.lisp](tools/load-asdf.lisp) -- a build script to load, configure and use ASDF
        * [install-asdf.lisp](install-asdf.lisp) -- replace and update an implementation's ASDF
        * [cl-source-registry-cache.lisp](cl-source-registry-cache.lisp) -- update a cache for the source-registry

* [build.xcvb](build.xcvb)
    * The system definition for building ASDF with XCVB.
      It hasn't been tested or maintained for years and has bitrotten.

* [version.lisp-expr](version.lisp-expr)
    * The current version. Bumped up every time the code changes, using:

          ./tools/asdf-builder bump

* [doc/](doc/)
    * documentation for ASDF, including:
        * [index.html](doc/index.html) -- the web page for <http://common-lisp.net/project/asdf/>
        * [asdf.texinfo](doc/asdf.texinfo) -- our manual
        * [Makefile](doc/Makefile) -- how to build the manual
        * [cclan.png](doc/cclan.png) [lisp-logo120x80.png](doc/lisp-logo120x80.png)
          [style.css](doc/style.css) [favicon.ico](doc/favicon.ico)
	  -- auxiliaries of [index.html](doc/index.html)

* [test/](test/)
    * regression test scripts (and ancillary files) for developers to check
      that they don't unintentionally break any of the functionality of ASDF.
      Far from covering all of ASDF.

* [contrib/](contrib/)
    * a few contributed files that show case how to use ASDF.

* [debian/](debian/)
    * files for packaging on Debian, Ubuntu, etc.

* [build/](build/)
    * where the `Makefile` and `asdf-tools` store their output files, including
        * `asdf.lisp` -- the current one-file deliverable of ASDF
        * `asdf-XXX.lisp` -- for upgrade test purposes, old versions
        * `results/` -- logs of tests that have been run
        * `fasls/` -- output files while running tests.

* [ext/](ext/)
    * external dependencies, that can be populated with `make ext`
      or equivalently with `git submodule update --init`.
      Depopulate it with `make noext`.

* [README.md](README.md)
    * this file

* [TODO](TODO)
    * plenty of ideas for how to further improve ASDF.


Last updated Tuesday, June 9th, 2015.
