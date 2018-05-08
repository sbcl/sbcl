# Installing SBCL

## Contents

1. Binary Distribution

  1.1. Quick start

  1.2. Finding ancillary files

  1.3. Anatomy of SBCL

2. Source Distribution

  2.1. Quick start

  2.2. Customizing SBCL

  2.3. Troubleshooting

  2.4. Tracking SBCL sources

  2.5. Supported platforms


# 1. Binary Distribution

## 1.1. Quick start:

To run SBCL without installing it, from the top of binary distribution
directory:

```shell
sh run-sbcl.sh
```

The following command installs SBCL and related documentation under
the "/usr/local" directory (typically run as root):

```shell
INSTALL_ROOT=/usr/local sh install.sh
```

You can also install SBCL as a user, under your home directory:

```shell
INSTALL_ROOT=/home/me sh install.sh
```

In other words, `install.sh` installs SBCL under the directory named
by the environment variable `INSTALL_ROOT`.

If `INSTALL_ROOT` is not specified, SBCL is installed into location
configured at build-time: for official binary distributions under
"/usr/local" directory.

If you install SBCL from binary distribution in other location than
"/usr/local", see section 1.2, "Finding ancillary files".

## 1.2. Finding ancillary files

The SBCL runtime needs to be able to find the ancillary files
associated with it: the "sbcl.core" file, and the contrib modules.

Finding core can happen in three ways:

1. By default, in a location configured when the system was built.
   For binary distributions this is in "/usr/local/lib/sbcl".

2. By environment variable, in the directory named by the
   environment variable "SBCL_HOME". Example:

```shell
export SBCL_HOME=/foo/bar/lib/sbcl
sbcl
```

If your `INSTALL_ROOT` was `FOO`, then your `SBCL_HOME` is
`FOO/lib/sbcl`.

3. By command line option:

```shell
sbcl --core /foo/bar/sbcl.core
```

The usual, recommended approach is method #1. Method #2 is useful if
you're installing SBCL on a system in a non-standard location
(e.g. in your user account), instead of installing SBCL on an entire
system.  Method #3 is mostly useful for testing or other special
cases.

Contributed modules are primarily looked for in `SBCL_HOME`, or the
directory the core resides in if `SBCL_HOME` is not set.
`ASDF:*CENTRAL-REGISTRY*` serves as an additional fallback for
ASDF-based modules.

## 1.3. Anatomy of SBCL

  The two files that SBCL needs to run, at minimum, are:

    src/runtime/sbcl
    output/sbcl.core

  In addition, there are a number of modules that extend the basic
  sbcl functionality, in

    contrib/

  The "src/runtime/sbcl" is a standard executable, built by compiling
  and linking an ordinary C program. It provides the runtime
  environment for the running Lisp image, but it doesn't know much
  about high-level Lisp stuff (like symbols and printing and objects)
  so it's pretty useless by itself. The "output/sbcl.core" is a dump
  file written in a special SBCL format which only sbcl understands,
  and it contains all the high-level Lisp stuff.

  The standard installation procedure, outlined in section 1.1 "Quick
  start", is to run the "install.sh", which copies all the files to
  right places, including documentation and contrib-modules that have
  passed their tests. If you need to install by hand, see "install.sh"
  for details.

  Documentation consists of a man-page, the SBCL Manual (in info, pdf
  and html formats), and a few additional text files.

# 2. Source Distribution

## 2.1. Quick start

To build SBCL you need a working toolchain and a Common Lisp system
(see section 2.5 "Supported platforms"). You also need approximately
128 Mb of free RAM+swap.

To build SBCL using an already installed SBCL:

```shell
sh make.sh
```

To configure SBCL to install to a non-standard location, you can use
the --prefix option:

```shell
sh make.sh --prefix=/opt/mysbcl
```

This also sets the default SBCL_HOME to prefix/lib/sbcl/ for the
built binaries.

To configure SBCL with a non-standard default dynamic-space size,
use the --dynamic-space-size option:

```shell
sh make.sh --dynamic-space-size=4Gb
sh make.sh --dynamic-space-size=800Mb
```

If mega- or gigabytes are not specified, the number is taken to be
in megabytes. The standard default is 512Mb for 32-bit systems, and
1Gb for 64-bit systems (with the exception of OpenBSD where 444Mb
are used to fit under default ulimits.)

If you don't already have an SBCL binary installed as "sbcl" on your
system, you'll need to tell make.sh what Lisp to use as the
cross-compilation host (cf. make.sh for detailed instructions on
how to cross compile). For example, to use CMUCL (assuming has
been installed under its default name "lisp") as the
cross-compilation host:

```shell
sh make.sh --xc-host='lisp -batch -noinit'
```

The build may take a long time, especially on older hardware. A
successful build ends with a message beginning: "The build seems to
have finished successfully...".

To run the regression tests:

```shell
cd tests && sh run-tests.sh
```

To build documentation:

```shell
cd doc/manual && make
```

This builds the Info, HTML and PDF documentation from the Texinfo
sources. The manual includes documentation string from the build
SBCL, but if SBCL itself has not been yet built, but one if found
installed documentation strings from the installed version are used.

Now you should have the same src/runtime/sbcl and output/sbcl.core
files that come with the binary distribution, and you can install
them as described in the section 1. "BINARY DISTRIBUTION".

## 2.2. Customizing SBCL

You can tweak the `*FEATURES*` set for the resulting Lisp system,
enabling or disabling features like documentation strings, threads,
or extra debugging code.

The preferred way to do this is using commandline arguments to make.sh:

```text
--fancy                    Enables all supported feature enhancements.
--with-<feature>           Enables a specific feature.
--without-<feature>        Disables a specific feature.
```

Some features of interest:

```text
  :SB-THREAD (--with-sb-thread, --without-sb-thread)

    Native threads. Enabled by default on x86[-64] Linux only, also
    available on x86[-64] Max OS X, x86[-64] FreeBSD, x86 Solaris,
    and PPC Linux.

    NOTE: --fancy enables threads on all platforms where they can be
    built, even if they aren't 100% stable on that platform.

  :SB-CORE-COMPRESSION (--with-sb-core-compression)

    Adds zlib as a build-dependency, and makes SBCL able to save
    compressed cores. Not enabled by default.

  :SB-XREF-FOR-INTERNALS (--with-sb-xref-for-internals)

    XREF data for SBCL internals. Not enabled by default, increases
    core size by 5-6mb.

  :SB-UNICODE (--without-sb-unicode)

    Unicode support. Enabled by default. Disabling this feature
    limits characters to the 8-bit ISO-8859-1 set.
```

A catalog of available features and their meaning can be found in
"base-target-features.lisp-expr".

Please do NOT edit base-target-features.lisp-expr in order to enable
or disable build features.

## 2.3. Troubleshooting

### "GNU Make not found"

If the GNU make command is not available under the names "make",
"gmake", or "gnumake", then define the environment variable
GNUMAKE to a name where it can be found.

### Segfaults on Fedora

Try disabling exec-shield. The easiest way is to use
setarch: "setarch i386 -R sbcl".

Build crashes mysteriously, machine becomes unstable, etc

You may be running out of memory. Try increasing swap, or
building SBCL with fewer other programs running simultaneously.

Other

  * Check that the host lisp you're building with is known to work as
    an SBCL build host, and that your operating system is supported.

  * Try to do a build without loading any initialization files
    for the cross-compilation host (for example
    "sh make.sh 'sbcl --userinit /dev/null --sysinit /dev/null'").

  * Some GCC versions are known to have bugs that affect SBCL
    compilation: if the error you're encountering seems related to
    files under "src/runtime", down- or upgrading GCC may help.

  * Ask for help on the mailing lists referenced from
    <http://www.sbcl.org/>.

## 2.4. Tracking SBCL sources

  If you want to be on the bleeding edge, you can update your sources
  to the latest development snapshot (or any previous development
  snapshot, for that matter) by using anonymous CVS to
  SourceForge. (This is not recommended if you're just using SBCL as a
  tool for other work, but if you're interested in working on SBCL
  itself, it's a good idea.) Follow the "CVS Repository" link on
  <http://sourceforge.net/projects/sbcl> for instructions.

## 2.5. Supported platforms

Last updated for SBCL 0.9.3.74 (2005-08-20).

All of the following platforms are supported in the sense of "should
work", but some things like loading foreign object files may lag
behind on less-used operating systems.

Supported toolchains:

  GNU toolchain
  Sun toolchain with GCC

Supported build hosts are:

  SBCL
  CMUCL
  CCL (formerly known as OpenMCL)
  ABCL (recent versions only)
  CLISP (only some versions: 2.44.1 is OK, 2.47 is not)
  XCL


  Note that every release isn't tested with every possible host
  compiler.  You're most likely to get a clean build with SBCL itself
  as host, otherwise CCL on a PPC and CMUCL elsewhere.

Supported operating systems and architectures:

```
                         x86 x86-64 PPC Sparc Alpha MIPS MIPSel
  Linux 2.6               X     X    X    X     X     X     X
  Darwin (Mac OS X)       X     X    X
  Solaris                 X               X
  FreeBSD                 X     X
  NetBSD                  X          X
  OpenBSD 3.4, 3.5        X
  Windows                 X
```

Some operating systems are more equal than others: most of the
development and testing is done on x86/x86-64 Linux and x86/PPC
Mac OS X.

If an underprivileged platform is important to you, you can help
by e.g. testing during the monthly freeze periods, and most
importantly by reporting any problems.

For further support, see Getting Support and Reporting Bugs in the manual, or
http://www.sbcl.org/manual/Getting-Support-and-Reporting-Bugs.html if you do not
have the manual for some reason.


### System-Specific Hints

NetBSD 2.0 and above are required because of the lack of needed signal APIs in
NetBSD 1.6 and earlier.
