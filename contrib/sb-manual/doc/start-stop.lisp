(in-package :sb-manual)

(defsection @starting-and-stopping (:title "Starting and Stopping")
  (@starting-sbcl section)
  (@stopping-sbcl section)
  (@command-line-options section)
  (@initialization-files section)
  (@initialization-and-exit-hooks section))

(defsection @starting-sbcl (:title "Starting SBCL")
  (@running-from-shell section)
  (@running-from-emacs section)
  (@shebang-scripts section))

(defsection @running-from-shell (:title "Running from Shell")
  "To run SBCL, type `sbcl` at the command line.

  You should end up in the toplevel _REPL_ (read-eval-print loop),
  where you can interact with SBCL by typing expressions.

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

  Also see @COMMAND-LINE-OPTIONS and @STOPPING-SBCL.")

(defsection @running-from-emacs (:title "Running from Emacs")
  "To run SBCL as an `inferior-lisp` from Emacs, in your `.emacs` do
  something like:

      ;;; The SBCL binary and command-line arguments
      (setq inferior-lisp-program \"/usr/local/bin/sbcl --noinform\")

  For more information on using SBCL with Emacs, see
  @EDITOR-INTEGRATION.")

(defsection @shebang-scripts (:title "Shebang Scripts")
  "Standard Unix tools that are interpreters follow a common command line
  protocol that is necessary to work with \"shebang scripts\". SBCL
  supports this via the `--script` command line option (see
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

(defsection @stopping-sbcl (:title "Stopping SBCL")
  (@exit section)
  (@end-of-file section)
  (@saving-a-core-image section)
  (@exit-on-errors section))

(defsection @exit (:title "Exit")
  "SBCL can be stopped at any time by calling SB-EXT:EXIT,
  optionally returning a specified numeric value to the calling
  process. See @THREADING for information about terminating individual
  threads."
  (sb-ext:exit function))

(defsection @end-of-file (:title "End of File")
  "By default SBCL also exits on end of input, caused either by user
  pressing `Control-D` on an attached terminal, or end of input when
  using SBCL as part of a shell pipeline.")

(defsection @saving-a-core-image (:title "Saving a Core Image")
  "SBCL has the ability to save its state as a file for later
  execution. This functionality is important for its bootstrapping
  process, and is also provided as an extension to the user."
  (sb-ext:save-lisp-and-die function)
  ;; When Swank is loaded, it sets this variable.
  (sb-ext:*save-hooks* (variable nil))
  "In cases where the standard initialization files have already been loaded
  into the saved core, and alternative ones should be used (or none at
  all), SBCL allows customizing the initfile pathname computation."
  (sb-ext:*sysinit-pathname-function* variable)
  (sb-ext:*userinit-pathname-function* variable)
  "To facilitate distribution of SBCL applications using external
  resources, the filesystem location of the SBCL core file being used
  is available from Lisp."
  (sb-ext:*core-pathname* (variable "<site-specific>")))

(defsection @exit-on-errors (:title "Exit on Errors")
  "SBCL can also be configured to exit if an unhandled error occurs,
  which is mainly useful for acting as part of a shell pipeline; doing
  so under most other circumstances would mean giving up large parts
  of the flexibility and robustness of Common Lisp. See
  @DEBUGGER-ENTRY and the command line option `--disable-debugger` in
  @RUNTIME-OPTIONS.")

(defsection @command-line-options (:title "Command Line Options")
  "Command line options can be considered an advanced topic; for ordinary
  interactive use, no command line arguments should be necessary.

  In order to understand the command line argument syntax for SBCL, it
  is helpful to understand that the SBCL system is implemented as two
  components, a low-level runtime environment written in \\C and a
  higher-level system written in Common Lisp itself. Some command line
  arguments are processed during the initialization of the low-level
  runtime environment, some command line arguments are processed
  during the initialization of the Common Lisp system, and any
  remaining command line arguments are made available to user code via
  SB-EXT:*POSIX-ARGV*.

  The full, unambiguous syntax for invoking SBCL at the command line
  is:

      sbcl <runtime-option>* --end-runtime-options \\
           <toplevel-option>* --end-toplevel-options \\
           <user-option>*

  For convenience, `--end-runtime-options` and
  `--end-toplevel-options` can be omitted, which can be convenient
  when you are running the program interactively, and you can see that
  no ambiguities are possible with the option values you are using.
  Omitting these elements is probably a bad idea for any batch file
  where any of the options are under user control, since it makes it
  impossible for SBCL to detect erroneous command line input, so that
  erroneous command line arguments will be passed on to the user
  program even if they was intended for the runtime system or the Lisp
  system."
  (@runtime-options section)
  (@toplevel-options section))

(defsection @runtime-options (:title "Runtime Options")
  "- `--core <corefilename>`

      Run the specified Lisp core file instead of the default. Note
      that if the Lisp core file is a user-created core file, it may
      run a nonstandard toplevel which does not recognize the standard
      toplevel options.

  - `--dynamic-space-size <megabytes>`

      Size of the dynamic space reserved on startup in megabytes.
      Default value is platform dependent.

  - `--control-stack-size <megabytes>`

      Size of control stack reserved for each thread in megabytes.
      Default value is 2.

  - `--tls-limit <positive integer>`

      Maximum number of thread-local symbols in threaded builds.
      Default value is 4096.

  - `--noinform`

      Suppress the printing of any banner or other informational
      message at startup. This makes it easier to write Lisp programs
      which work cleanly in Unix pipelines. See also the `--noprint`
      and `--disable-debugger` options.

  - `--disable-ldb`

      Disable the low-level debugger. Only effective if SBCL is
      compiled with LDB.

  - `--lose-on-corruption`

      There are some dangerous low-level errors (for instance, control
      stack exhausted, memory fault) that (or whose handlers) can
      corrupt the image. By default, SBCL prints a warning, then tries
      to continue and handle the error in Lisp, but this will not
      always work, and SBCL may malfunction or even hang. With this
      option, upon encountering such an error, SBCL will exit instead
      of invoking LDB (if present and enabled).

  - `--script <filename>`

      As a _runtime_ option, this is equivalent to `--noinform`
      `--disable-ldb` `--lose-on-corruption`
      `--end-runtime-options` `--script` `<filename>`. See
      the description of `--script` as a _toplevel_ option below.
      If there are no other command line arguments following
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

  In the future, runtime options may be added to control behaviour
  such as lazy allocation of memory.

  Runtime options, including any `--end-runtime-options` option, are
  stripped out of the command line before the Lisp toplevel logic gets
  a chance to see it.")

(defsection @toplevel-options (:title "Toplevel Options")
  "The following options are processed and removed by the default
  toplevel (see SB-EXT:SAVE-LISP-AND-DIE).

  - `--sysinit <filename>`

      Load `FILENAME` instead of the default system initialization
      file (see @INITIALIZATION-FILES).

  - `--no-sysinit`

      Don't load a system-wide initialization file. If this option is
      given, the `--sysinit` option is ignored.

  - `--userinit <filename>`

      Load `FILENAME` instead of the default user initialization file
      (see @INITIALIZATION-FILES.)

  - `--no-userinit`

      Don't load a user initialization file. If this option is given,
      the `--userinit` option is ignored.

  - `--eval <command>`

      After executing any initialization file, but before starting the
      read-eval-print loop on standard input, read and evaluate
      `COMMAND`. More than one `--eval` option can be used, and all
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
      runtime option, this makes it easier to write Lisp \"scripts\"
      which work cleanly in Unix pipelines.

  - `--disable-debugger`

      By default when SBCL encounters an error, it enters the builtin
      debugger, allowing interactive diagnosis and possible
      intercession. This option disables the debugger, causing errors
      to print a backtrace and exit with status 1 instead. When given,
      this option takes effect before loading of initialization files
      or processing `--eval` and `--load` options. See
      SB-EXT:DISABLE-DEBUGGER and @DEBUGGER-ENTRY.

  - `--script <filename>`

      Implies `--no-userinit` `--no-sysinit` `--disable-debugger`
      `--end-toplevel-options`.

      Causes the system to load the specified file instead of entering
      the read-eval-print-loop, and exit afterwards. If the file
      begins with a shebang line, it is ignored.

      If there are no other command line arguments following, the
      filename can be omitted: this causes the script to be loaded
      from standard input instead. Shebang lines in standard input
      script are currently _not_ ignored.

      In either case, if there is an unhandled error (e.g. end of
      file, or a broken pipe) on either standard input, standard
      output, or standard error, the script silently exits with code
      0. This allows e.g. safely piping output from SBCL to `head -n1`
      or similar.

      Additionally, the option sets *COMPILE-VERBOSE* and
      *LOAD-VERBOSE* to NIL while loading the file to avoid
      potentially verbose diagnostic messages printed on the standard
      output.")

(defsection @initialization-files (:title "Initialization Files")
  "SBCL processes initialization files with READ and EVAL,
  not LOAD; hence initialization files can be used to set startup
  *PACKAGE* and *READTABLE*, and for proclaiming a global optimization
  policy.

  - __System Initialization File:__ Defaults to `$SBCL_HOME/sbclrc`,
    or if that doesn't exist to `/etc/sbclrc`. Can be overridden with
    the command line option `--sysinit` or `--no-sysinit` (see
    @TOPLEVEL-OPTIONS).

      The system initialization file is intended for system
      administrators and software packagers to configure locations of
      installed third party modules, etc.

  - __User Initialization File:__ Defaults to `$HOME/.sbclrc`. Can be
    overridden with the command line option `--userinit` or
    `--no-userinit` (see @TOPLEVEL-OPTIONS).

      The user initialization file is intended for personal
      customizations, such as loading certain modules at startup,
      defining convenience functions to use in the REPL, handling
      automatic recompilation of FASLs (see @FASL-FORMAT), etc.

  Neither initialization file is required.")

(defsection @initialization-and-exit-hooks
    (:title "Initialization and Exit Hooks")
  "SBCL provides hooks into the system initialization and exit."
  (sb-ext:*init-hooks* variable)
  (sb-ext:*exit-hooks* variable))
