(in-package :sb-manual)

(defsection @starting-and-stopping (:title "Starting and Stopping")
  (@starting-sbcl section)
  (@stopping-sbcl section)
  (@command-line-options section)
  (@initialization-files section)
  (@initialization-and-exit-hooks section)
  (@core-images section))

(defsection @starting-sbcl (:title "Starting SBCL")
  "If SBCL is installed properly and it is in your `PATH`, you can simply
  type `sbcl` at the command line. You should end up in the toplevel
  _REPL_ (read-eval-print loop), where you can interact with SBCL by
  typing expressions.

      $ sbcl
      This is SBCL 0.8.13.60, an implementation of ANSI Common Lisp.
      More information about SBCL is available at <http://www.sbcl.org/>.

      SBCL is free software, provided as is, with absolutely no warranty.
      It is mostly in the public domain; some portions are provided under
      BSD-style licenses.  See the CREDITS and COPYING files in the
      distribution for more information.
      * (+ 2 2)
      4
      * (exit)
      $

  See also @COMMAND-LINE-OPTIONS and @STOPPING-SBCL."
  (@running-without-installation section)
  (@running-from-emacs section)
  (@shebang-scripts section)
  (@parts-of-sbcl section)
  (@finding-parts-of-sbcl section))

(defsection @running-without-installation
    (:title "Running without Installation")
  "You can quickly run SBCL without installing it by invoking the
  `run-sbcl.sh` script. This works both with an unpacked
  @BINARY-DISTRIBUTION and after @BUILDING-SBCL yourself:

      $ sh run-sbcl.sh")

(defsection @running-from-emacs (:title "Running from Emacs")
  "To run SBCL as an `inferior-lisp` from Emacs, add something like this
  to your `.emacs`:

      ;;; The SBCL binary and command-line arguments
      (setq inferior-lisp-program \"/usr/local/bin/sbcl --noinform\")

  For more information on using SBCL with Emacs, see
  @EDITOR-INTEGRATION.")

(defsection @shebang-scripts (:title "Shebang Scripts")
  "Standard Unix tools that are interpreters follow a common command-line
  protocol that is necessary to work with \"shebang scripts\". SBCL
  supports this via the `--script` command-line option (see
  @COMMAND-LINE-OPTIONS).

  Example file (`hello.lisp`):

      #!/usr/local/bin/sbcl --script
      (write-line \"Hello, World!\")

  Usage from the command line:

      $ ./hello.lisp
      Hello, World!

  Note that SBCL skips the shebang line when it reads the file:

      $ sbcl --script hello.lisp
      Hello, World!")

(defsection @parts-of-sbcl (:title "Parts of SBCL")
  "If you only ever use SBCL packaged for your operating system,
  run it from an installation created with [`install.sh`][
  @installing-sbcl], run it with [`run-sbcl.sh`][
  @running-without-installation], there is no need to worry about the
  files necessary at run-time. If you, for example, want to install
  SBCL manually, package it in a way that deviates from the file
  hierarchy created by `install.sh`, or run SBCL with your own core
  file saved by SB-EXT:SAVE-LISP-AND-DIE, then read on.

  The following files play a part in running SBCL.

  - _The `sbcl` binary_

      This is a standard executable built by compiling and linking an
      ordinary C program. It provides the runtime environment for the
      running Lisp image, but it doesn't know much about high-level
      Lisp stuff (like symbols and printing and objects), so it's
      pretty useless by itself.

      The binary is `src/runtime/sbcl` in @BINARY-DISTRIBUTIONs and
      @SOURCE-DISTRIBUTIONs; and it is [installed][@INSTALLING-SBCL]
      to `/usr/local/bin/sbcl` by default.

  - _The `sbcl.core` file_

      This is an [image dump][sb-ext:save-lisp-and-die] written in a
      special SBCL format that only SBCL understands, and it contains
      all the high-level Lisp stuff.

      The core file is `obj/sbcl-home/sbcl.core` in distributions;
      installed to `/usr/local/lib/sbcl/sbcl.core` by default. In
      addition, there are a number of @CONTRIBUTED-MODULES that extend
      the basic SBCL functionality.

  - _The `contrib/` directory_

      This holds the compiled fasl files for the @CONTRIBUTED-MODULES,
      to be loaded on demand.

      The contrib fasl files are in `obj/sbcl-home/contrib/` in
      distributions; installed to `/usr/local/lib/sbcl/contrib/` by
      default.

  > _Note_: Only the binary and the core file are absolutely necessary
  > to start SBCL; the contrib fasls are optional.")

(defsection @finding-parts-of-sbcl (:title "Finding Parts of SBCL")
  "When you invoke the SBCL binary, it needs to be able to find the other
  @PARTS-OF-SBCL, namely the `sbcl.core` file and the
  @CONTRIBUTED-MODULES in the `contrib/` directory.

  - Finding `sbcl.core`

      If the `--core` [runtime option][@runtime-options] is specified,
      then its value is used. Example:

          $ sbcl --core /foo/bar/sbcl.core

      Else, the runtime looks for `sbcl.core` in the following
      directories until the first match.

      - The directory denoted by the `\\\\SBCL_HOME` environment
        variable if it is set

      - The current directory

      - The directory of the `sbcl` binary

      - The `../lib/sbcl/` directory relative to the `sbcl` binary

      If no core file was found by the above rules, then the runtime
      exits with an error message.

  - Finding `contrib/`

      Once the runtime loads the core file, the Lisp looks for
      `contrib/` in the following directories until the first match.

      - The directory denoted by the `\\\\SBCL_HOME` environment
        variable if it is set

      - _The directory of `sbcl.core`_

      - _The directory `../lib/sbcl` relative to `sbcl.core`_

      - The current directory

      - The directory of the `sbcl` binary

      - The directory `../lib/sbcl` relative to the `sbcl` binary

      The list of directories is the same as for finding the core file
      except here we also try directories relative to `sbcl.core`.

      If the `contrib/` directory is not found, then no error is
      signalled immediately, but loading modules will fail.")

(defsection @stopping-sbcl (:title "Stopping SBCL")
  (@exit section)
  (@end-of-file section)
  (@exit-on-errors section))

(defsection @exit (:title "Exit")
  "SBCL can be stopped at any time by calling SB-EXT:EXIT,
  optionally returning a specified numeric value to the calling
  process. See @THREADING for information about terminating individual
  threads."
  (sb-ext:exit function))

(defsection @end-of-file (:title "End of File")
  "By default, SBCL also exits upon reaching the end of input. This can
  be caused either by the user pressing `Control-D` on an attached
  terminal, or by the input stream ending when SBCL is used as part of
  a shell pipeline.")

(defsection @exit-on-errors (:title "Exit on Errors")
  "SBCL can also be configured to exit if an unhandled error occurs,
  which is mainly useful for acting as part of a shell pipeline; doing
  so under most other circumstances would mean giving up large parts
  of the flexibility and robustness of Common Lisp. See
  @DEBUGGER-ENTRY and the command-line option `--disable-debugger` in
  @TOPLEVEL-OPTIONS.")

(defsection @command-line-options (:title "Command-line Options")
  "Command-line options can be considered an advanced topic; for ordinary
  interactive use, no command-line arguments should be necessary.

  In order to understand the command-line argument syntax for SBCL, it
  is helpful to understand that the SBCL system is implemented as two
  components, a low-level runtime environment written in C and a
  higher-level system written in Common Lisp. Arguments are processed
  during the respective initializations of the runtime and Lisp
  system, with any remainder available to user code via
  SB-EXT:*POSIX-ARGV*.

  The full, unambiguous syntax for invoking SBCL at the command line
  is:

      sbcl <runtime-option>* --end-runtime-options \\
           <toplevel-option>* --end-toplevel-options \\
           <user-option>*

  The `--end-runtime-options` and `--end-toplevel-options` can be
  omitted, provided that no ambiguities are possible with the option
  values used. Omitting these elements is probably a bad idea for any
  batch file where any of the options are under user control because
  it makes it impossible for SBCL to detect erroneous command-line
  arguments, which will then be passed on to the user program even if
  they were intended for the runtime system or the Lisp system."
  (@runtime-options section)
  (@toplevel-options section))

(defsection @runtime-options (:title "Runtime Options")
  "- `--core <corefilename>`

      Run the specified Lisp core file instead of the default. Note
      that if the Lisp core file is a user-created core file, it may
      run a nonstandard toplevel that does not recognize the standard
      toplevel options.

  - `--dynamic-space-size <size>`

      Size of the dynamic space reserved on startup. The default value
      is platform-dependent. The units supported are kB (or KiB),
      MB (or MiB), GB (or GiB), and TB (or TiB). The names are
      compared case-insensitively, and the default unit is megabytes.
      Thus, `8`, `8mb`, and `8mib` all mean 8 * 2^20 bytes.

  - `--control-stack-size <size>`

      Size of control stack reserved for each thread. The `<size>`
      argument has the same format as in `--dynamic-space-size`. The
      default is 2MB.

  - `--tls-limit <positive integer>`

      Maximum number of thread-local @SPECIAL-VARIABLES in threaded
      builds. Default value is 4096.

  - `--noinform`

      Suppress the printing of any banner or other informational
      message at startup. This makes it easier to write Lisp programs
      which work cleanly in Unix pipelines. See also the `--noprint`
      and `--disable-debugger` options.

  - `--disable-ldb`

      Disable the @LDB, the low-level debugger. Only effective if SBCL
      is compiled with \\LDB. When \\LDB is not available or is
      disabled, SBCL exits with a non-zero exit code whenever it would
      enter \\LDB. ~DISABLING-LDB

  - `--lose-on-corruption`

      There are some dangerous low-level errors (for instance, control
      stack exhausted, memory fault) that (or whose handlers) can
      corrupt the image. By default, SBCL prints a warning, then tries
      to continue and handle the error in Lisp. However, this will not
      always work, and SBCL may subsequently malfunction or even hang.
      With this option, upon encountering such an error, SBCL will
      exit instead of signalling a Lisp error.

  - `--script <filename>`

      As a _runtime_ option, this is equivalent to `--noinform`
      `--disable-ldb` `--lose-on-corruption`
      `--end-runtime-options` `--script` `<filename>`. See
      the description of `--script` as a _toplevel_ option below.
      If there are no other command-line arguments following
      `--script`, the filename argument can be omitted.

  - `--merge-core-pages`

      When platform support is present, provide hints to the operating
      system that identical pages may be shared between processes
      until they are written to. This can be useful to reduce the
      memory usage on systems with multiple SBCL processes started
      from similar but differently-named core files, or from
      compressed cores. Without platform support, do nothing. By
      default only compressed cores trigger hinting.

  - `--no-merge-core-pages`

      Ensures that no sharing hint is provided to the operating
      system.

  - `--help`

      Print some basic information about SBCL, then exit.

  - `--version`

      Print SBCL's version information, then exit.

  - `--end-runtime-options`

      End processing of runtime options and pass the rest of the
      command-line arguments to the toplevel function.

  In the future, runtime options may be added to control behaviour
  such as lazy allocation of memory.")

(defsection @toplevel-options (:title "Toplevel Options")
  "The following options are supported by the @DEFAULT-TOPLEVEL. If the
  toplevel function was overriden with SB-EXT:SAVE-LISP-AND-DIE, the
  set of available toplevel options is likely very different.

  - `--sysinit <filename>`

      Load `<filename>` instead of the default system initialization
      file (see @INITIALIZATION-FILES).

  - `--no-sysinit`

      Don't load a system-wide initialization file. If this option is
      given, the `--sysinit` option is ignored.

  - `--userinit <filename>`

      Load `<filename>` instead of the default user initialization
      file (see @INITIALIZATION-FILES.)

  - `--no-userinit`

      Don't load a user initialization file. If this option is given,
      the `--userinit` option is ignored.

  - `--eval <command>`

      After executing any initialization file, but before starting the
      read-eval-print loop on standard input, read and evaluate
      `<command>`. More than one `--eval` option can be used, and all
      will be read and executed, in the order they appear on the
      command line.

  - `--load <filename>`

      This is equivalent to `--eval '(load \"<filename>\")'`. The
      special syntax is intended to reduce quoting headaches when
      invoking SBCL from shell scripts.

  - `--noprint`

      When ordinarily the toplevel \"read-eval-print loop\" would be
      executed, execute a \"read-eval loop\" instead, i.e. don't print
      a prompt and don't echo results. Combined with the `--noinform`
      runtime option, this makes it easier to write Lisp scripts that
      work cleanly in Unix pipelines.

  - `--disable-debugger`

      By default, when SBCL encounters an error, it enters the builtin
      debugger, allowing interactive diagnosis and possible
      intercession. This option disables the debugger, causing errors
      to print a backtrace and exit with status 1 instead. When given,
      this option takes effect before loading initialization files or
      processing `--eval` and `--load` options. See
      SB-EXT:DISABLE-DEBUGGER and @DEBUGGER-ENTRY.

  - `--script <filename>`

      Implies `--no-userinit` `--no-sysinit` `--disable-debugger`
      `--end-toplevel-options`.

      Causes the system to load the specified file instead of entering
      the read-eval-print-loop, and exit afterwards. If the file
      begins with a shebang line, it is ignored.

      If there are no other command-line arguments following, the
      filename can be omitted: this causes the script to be loaded
      from standard input instead. In this case, shebang lines are
      currently _not_ ignored.

      In either case, if there is an unhandled error (such as an end
      of file condition or a broken pipe) on standard input, standard
      output, or standard error, the script silently exits with code
      0. This allows piping output from SBCL to another command
      without unexpected errors.

      Additionally, the option sets *COMPILE-VERBOSE* and
      *LOAD-VERBOSE* to NIL while loading the file to avoid
      potentially verbose diagnostic messages printed on the standard
      output.

  - `--end-toplevel-options`

      End processing of toplevel options, leaving the rest of the
      command-line arguments in SB-EXT:*POSIX-ARGV*.")

(defsection @initialization-files (:title "Initialization Files")
  "The @DEFAULT-TOPLEVEL supports customization via the system and user
  initialization files. These files with READ and EVAL, not LOAD;
  hence initialization files can be used to set startup *PACKAGE* and
  *READTABLE*, and for proclaiming a global optimization policy.

  - __System Initialization File:__ Defaults to `sbclrc` in the
    directory where `contrib/` is found (see @FINDING-PARTS-OF-SBCL),
    or if that doesn't exist to `/etc/sbclrc`. Can be overridden with
    the command-line option `--sysinit` or `--no-sysinit` (see
    @TOPLEVEL-OPTIONS). Set SB-EXT:*SYSINIT-PATHNAME-FUNCTION* to
    override the above defaulting behavior.

      The system initialization file is intended for system
      administrators and software packagers to configure locations of
      installed third party modules, etc.

  - __User Initialization File:__ Defaults to `$HOME/.sbclrc`. Can be
    overridden with the command-line option `--userinit` or
    `--no-userinit` (see @TOPLEVEL-OPTIONS). Set
    SB-EXT:*USERINIT-PATHNAME-FUNCTION* to override the above
    defaulting behavior.

      The user initialization file is intended for personal
      customizations, such as loading certain modules at startup,
      defining convenience functions to use in the @REPL, handling
      automatic recompilation of fasls (see @FASL-FORMAT), etc.

  Neither initialization file is required."
  (sb-ext:*sysinit-pathname-function* variable)
  (sb-ext:*userinit-pathname-function* variable))

(defsection @initialization-and-exit-hooks
    (:title "Initialization and Exit Hooks")
  "SBCL provides hooks into the system initialization and exit."
  ;; With the NIL, hide tha actual value, which some contribs do change.
  (sb-ext:*init-hooks* (variable nil))
  (sb-ext:*exit-hooks* variable))

(defsection @core-images (:title "Core Images")
  (@saving-a-core-image section)
  (@default-toplevel section))

(defsection @saving-a-core-image (:title "Saving a Core Image")
  "SBCL has the ability to save its state as a file for later
  execution. This functionality is important for its bootstrapping
  process and is also provided as an extension to the user."
  (sb-ext:save-lisp-and-die function)
  ;; When Swank is loaded, it sets this variable.
  (sb-ext:*save-hooks* (variable nil))
  "To facilitate distribution of SBCL applications using external
  resources, the filesystem location of the SBCL core file being used
  is available from Lisp."
  (sb-ext:*core-pathname* (variable "<varies>")))

(defsection @default-toplevel (:title "Default Toplevel")
  "SBCL's default toplevel function processes @TOPLEVEL-OPTIONS,
  runs @INITIALIZATION-FILES, and provides a @REPL.

  The toplevel function of @CORE-IMAGES can be specified with the
  :TOPLEVEL argument of SB-EXT:SAVE-LISP-AND-DIE.")
