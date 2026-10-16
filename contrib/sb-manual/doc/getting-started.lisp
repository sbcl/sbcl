(in-package :sb-manual)

;;; This together with @PARTS-OF-SBCL and @FINDING-PARTS-OF-SBCL
;;; overlaps quite a bit with the INSTALL file. Maybe we should reduce
;;; the duplication by generating INSTALL.
(defsection @getting-started (:title "Getting Started")
  (@supported-platforms section)
  (@obtaining-sbcl section)
  (@building-sbcl section)
  (@installing-sbcl section))

(defsection @supported-platforms (:title "Supported Platforms")
  "See <https://sbcl.org/platform-table.html> for the list of supported
  operating systems and architectures.

  Some operating systems are more equal than others: most of the
  development and testing is done on x86-64 Linux and x86-64/arm64 Mac
  OS X.

  If an underprivileged platform is important to you, you can help by
  e.g. testing during the monthly freeze periods and, most
  importantly, by reporting any problems. See @SUPPORT-AND-BUGS.")

(defsection @obtaining-sbcl (:title "Obtaining SBCL")
  "Your package manager may provide prepackaged binaries. On Debian, for
  example, you can install a stable, though possibly older, version of
  SBCL with `apt install sbcl`. The exact details vary depending on
  your system."
  (@binary-distribution section)
  (@source-distribution section)
  (@sbcl-repository section))

(defsection @binary-distribution (:title "Binary Distribution")
  "The latest official binary distributions (tar and zip archives of
  pre-compiled SBCL and its documentation) for selected platforms can
  be downloaded from

  - <https://sbcl.org/platform-table.html>

  Some older binaries are available at

  - <https://sourceforge.net/projects/sbcl/files/sbcl/>

  Note that many platforms do not have recent binaries released. In
  that case, get a @SOURCE-DISTRIBUTION or check out the
  @SBCL-REPOSITORY, and [build SBCL][@BUILDING-SBCL].

  If you were lucky enough to have new enough binary distributions,
  unpack the archive with

      bzip2 -cd sbcl-2.6.9-x86-linux-binary.tar.bz2 | tar xvf -

  replacing `sbcl-2.6.9-x86-linux-binary.tar.bz2` with the name of the
  tarball you downloaded. Now, you can immediately try
  @RUNNING-WITHOUT-INSTALLATION.

  Then, enter the directory which was unpacked and skip ahead to
  @INSTALLING-SBCL to put SBCL in its proper place.

  On Windows, you may want to use the MSI installer to unpack and
  install in one step:

      msiexec /i sbcl-2.6.9-x86-64-windows.msi")

(defsection @source-distribution (:title "Source Distribution")
  "If there is no suitable @BINARY-DISTRIBUTION for your platform,
  your next choice is to download a source distribution (a tar or zip
  file with the SBCL sources) from

  - <https://sourceforge.net/projects/sbcl/files/sbcl/> or
  - <https://github.com/sbcl/sbcl/releases>.

  See @BUILDING-SBCL for the next steps.")

(defsection @sbcl-repository (:title "SBCL Repository")
  "If you want to be on the bleeding edge, you can check out the sources
  of the latest development version (or any previous development
  version, for that matter) by using anonymous GIT to SourceForge:

      $ git clone git://git.code.sf.net/p/sbcl/sbcl sbcl

  See <https://sourceforge.net/p/sbcl/sbcl/ci/master/tree/> for
  instructions.

  Alternatively, check out the official GitHub mirror at
  <https://github.com/sbcl/sbcl>:

      $ git clone https://github.com/sbcl/sbcl.git

  See @BUILDING-SBCL for the next steps.

  > _Note_: Tracking the repository is not recommended if you're just
  > using SBCL as a tool for other work, but if you're interested in
  > working on SBCL itself, it's a good idea.")

(defsection @installing-sbcl (:title "Installing SBCL")
  "The installation instructions are the same whether you are have
  downloaded a @BINARY-DISTRIBUTION or [built SBCL][ @building-sbcl]
  yourself.

  The following command installs SBCL and related documentation under
  the `/usr/local` directory (typically run as root):

      # INSTALL_ROOT=/usr/local sh install.sh

  You can also install SBCL as a user, under your home directory:

      $ INSTALL_ROOT=/home/me sh install.sh

  That is, `install.sh` installs SBCL under the directory named by the
  environment variable `\\\\INSTALL_ROOT`.

  If `\\\\INSTALL_ROOT` is not specified, SBCL is installed into
  location configured when @BUILDING-SBCL with the `--prefix` argument
  of `make.sh`: for official binary distributions, this is the
  `/usr/local` directory.

  The standard installation procedure, outlined above, is to run
  `install.sh`, which copies all the files to right places, including
  documentation and contrib modules that have passed their tests. If
  you need to install by hand, see `install.sh` for details.

  Documentation consists of a man page, the SBCL Manual (in info, pdf
  and html formats), and a few additional text files.")

(defsection @building-sbcl (:title "Building SBCL")
  "To build SBCL, you need a @SOURCE-DISTRIBUTION or the @SBCL-REPOSITORY
  checked out. In any case, the instructions are the same.

  You also need a working C toolchain and a Common Lisp system
  (see @BUILD-PREREQUISITES). You also need approximately 128 MB of
  free RAM+swap.

  To build SBCL using another, already installed SBCL version:

      $ sh make.sh

  To configure SBCL to [install][@installing-sbcl] to a non-standard
  location, you can use the `--prefix` option:

      $ sh make.sh --prefix=/opt/mysbcl

  To configure SBCL with a non-standard default dynamic-space size,
  use the `--dynamic-space-size` option:

      $ sh make.sh --dynamic-space-size=4gb
      $ sh make.sh --dynamic-space-size=800mb

  If mega- or gigabytes are not specified, the number is taken to be
  in megabytes. The standard default is 512Mb for 32-bit systems, and
  1GB for 64-bit systems (with the exception of OpenBSD where 444MB
  are used to fit under default ulimits.)

  If you don't already have an SBCL binary installed as `sbcl` on your
  system, you'll need to tell `make.sh` what Lisp to use as the
  cross-compilation host (cf. `make.sh` for detailed instructions on how
  to cross compile). For example, to use CMUCL (assuming it has been
  installed under its default name `lisp`) as the
  cross-compilation host:

      $ sh make.sh --xc-host='lisp -batch -noinit'

  The build may take a long time, especially on older hardware. A
  successful build ends with a message beginning: \"The build seems to
  have finished successfully...\".

  To run the regression tests:

      $ cd ./tests && sh run-tests.sh

  To build documentation:

      $ cd ./doc/manual && make

  This builds the Info, HTML and PDF documentation from the @SB-MANUAL
  contrib. The manual includes documentation strings from the built
  SBCL. If SBCL itself has not been built yet, but an installed one is
  found, documentation strings from the installed version are used.

  Now you should have the same `src/runtime/sbcl` and
  `obj/sbcl-home/sbcl.core` files that come with the binary
  distribution, the next thing is @INSTALLING-SBCL."
  (@customizing-the-build section)
  (@build-options section)
  (@troubleshooting-the-build section)
  (@build-prerequisites section))

(defsection @customizing-the-build (:title "Customizing the Build")
  "You can tweak the *FEATURES* set for the resulting Lisp system,
  enabling or disabling features like documentation strings, threads,
  or extra debugging code.

  The preferred way to do this is using command-line arguments to
  `make.sh`:

      --fancy                    Enables a curated set of features.
      --with-<feature>           Enables a specific feature.
      --without-<feature>        Disables a specific feature.")

(defsection @troubleshooting-the-build (:title "Troubleshooting the Build")
  "- \"GNU Make not found\"

      If the GNU make command is not available under the names `make`,
      `gmake`, or `gnumake`, then define the environment variable
      `\\\\GNUMAKE` to a name where it can be found.

  - Segfaults on Linux with ASLR

      If SBCL segfaults immediately upon startup due to Address Space
      Layout Randomization (ASLR), you can bypass it by executing the
      host compiler binary via `setarch`:

          setarch x86_64 -R sbcl

      See also :ASLR in @BUILD-OPTIONS.

  - Build crashes mysteriously, machine becomes unstable, etc

      You may be running out of memory. Try increasing swap, or
      building SBCL with fewer other programs running simultaneously.

  - Other

      * Check that the host lisp you're building with is known to work
        as an SBCL build host, and that your operating system is
        supported.

      * Try to do a build without loading any initialization files for
        the cross-compilation host (for example
        `sh make.sh 'sbcl --userinit /dev/null --sysinit /dev/null'`).

      * Some GCC versions are known to have bugs that affect SBCL
        compilation: if the error you're encountering seems related to
        files under `src/runtime`, down- or upgrading GCC may help.

      * Ask for help on the mailing lists referenced in
        @SUPPORT-AND-BUGS.")

(defsection @build-prerequisites (:title "Build Prerequisites")
  "All of the following platforms are supported in the sense of
  \"should work\", but some things like loading foreign object files
  may lag behind on less-used operating systems.

  Supported toolchains:

  - GNU toolchain
  - Sun toolchain with GCC

  SBCL can be compiled from source code using (as the host compiler)
  another ANSI-compliant Common Lisp implementation such as

  - SBCL
  - CMUCL
  - CCL (formerly known as OpenMCL)
  - CLISP (only some versions: 2.44.1 is OK, 2.47 is not)
  - ECL
  - XCL
  - ABCL (recent versions only, very slow)

  Often, it is most convenient to install an SBCL with your system's
  package manager to use as the host.

  Note that not every release is tested with every possible host
  compiler. You're most likely to get a clean build with SBCL itself
  as host, otherwise CCL on a PPC and CMUCL elsewhere.")


;;;; Getting the build options from base-target-features.lisp-expr

(eval-when (:compile-toplevel)
  (defvar *truename* *compile-file-truename*))

(defun base-target-features ()
  (with-open-file (s (merge-pathnames
                      "../../../src/cold/base-target-features.lisp-expr"
                      #.*truename*))
    (read s)))

;;; Add PREFIX to every line in STRING.
(defun prefix-lines (prefix string)
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop
        (multiple-value-bind (line missing-newline-p) (read-line in nil nil)
          (unless line
            (return))
          (format out "~a~a" prefix line)
          (unless missing-newline-p
            (terpri out)))))))

(defun feature-docstring ()
  (with-output-to-string (out)
    (dolist (feature-def (base-target-features))
      (destructuring-bind (feature enabledp &key public documentation)
          feature-def
        (declare (ignore enabledp))
        (when public
          (assert documentation ()
                  "No documentation for public build feature ~S."
                  feature-def)
          (format out "- ~S~%~%~A~&~%" feature
                  (prefix-lines "    " (reindent-docstring
                                        documentation))))))))

(eval
 `(defsection @build-options (:title "Build Options")
    ,(feature-docstring)))
