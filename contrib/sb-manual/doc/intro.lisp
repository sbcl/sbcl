(in-package :sb-manual)

(defsection @introduction (:title "Introduction")
  "Steel Bank Common Lisp (SBCL) is a free and open-source ANSI Common
  Lisp implementation with a high-performance native @COMPILER. In
  addition, it provides @UNICODE-SUPPORT, @THREADING, an interactive
  environment including a @DEBUGGER, a @STATISTICAL-PROFILER, a [code
  coverage tool][@sb-cover], and many other extensions.

  - Homepage: <http://sbcl.org>

  - @REPORTING-BUGS: <https://bugs.launchpad.net/sbcl>

  - Mailing lists: <https://sourceforge.net/p/sbcl/mailman/>

  - Repository: <https://sourceforge.net/p/sbcl/sbcl/>

  - GitHub mirror: <https://github.com/sbcl/sbcl>

  - Supported platforms: <https://www.sbcl.org/platform-table.html>

  This manual focuses on behavior specific to SBCL, not on behavior
  common to all implementations of ANSI Common Lisp. See
  @FURTHER-RESOURCES for information about @THE-SBCL-PROJECT and
  [Common Lisp][ @COMMON-LISP-RESOURCES]."
  (@beyond-this-manual section)
  (@highlights section))

(defsection @beyond-this-manual (:title "Beyond this Manual")
  "Documentation for non-ANSI extensions for various commands is
  available interactively. Tools that have their own command
  prompts (e.g. the @DEBUGGER, and INSPECT) provide a `help` command.
  Extensions without a command prompt (such as TRACE) are described in
  their DOCUMENTATION strings, unless your SBCL was compiled with the
  `--without-sb-doc` option to omit them (see @CUSTOMIZING-THE-BUILD),
  in which case they are only readable in the source code.

  This manual itself is available interactively via the @SB-MANUAL
  module.

  Besides this user manual, the following files are included in
  @SOURCE-DISTRIBUTIONs, @BINARY-DISTRIBUTIONs and the
  @SBCL-REPOSITORY. When SBCL is [installed][@installing-sbcl], they
  may be found in e.g. `/usr/local/share/doc/sbcl/` or
  `/usr/share/doc/sbcl/`.

  - `\\\\COPYING`: License and copyright summary

  - `\\\\CREDITS`: Authorship information on various parts of SBCL

  - `\\\\NEWS`: Summaries of changes between various SBCL versions

  And, of course, don't forget the @LANGUAGE-REFERENCE.")

(defsection @highlights (:title "Highlights")
  "SBCL comes with numerous extensions, some in core and some in modules
  loadable with REQUIRE. We only list some of the highlights here.
  Unfortunately, not all of these extensions have proper documentation
  yet.

  - __Foreign Function Interface:__ The `SB-ALIEN` package allows
    interfacing with C-code, loading shared object files, etc. See
    @FOREIGN-FUNCTION-INTERFACE.

      @SB-GROVEL can be used to partially automate generation of
      foreign function interface definitions.

  - __Native Threads:__ SBCL has native threads on numerous platforms,
    capable of taking advantage of SMP on multiprocessor machines. See
    @THREADING.

  - __Network Interface:__ The `SB-BSD-SOCKETS` module is a low-level
    networking interface, providing both TCP and UDP sockets. See
    @NETWORKING.

  - __Operating System Interface:__ The `SB-EXT` package contains a
    number of functions for running external processes, accessing
    environment variables, etc.

      The @SB-POSIX module provides a lispy interface to standard
      POSIX facilities.

  - __Profiling:__ The `SB-PROFILE` package provides an exact,
    per-function @DETERMINISTIC-PROFILER, while the `SB-SPROF` module
    is SBCL's @STATISTICAL-PROFILER, capable of call-graph generation
    and instruction level profiling, which also supports allocation
    profiling.

  - __Executable Delivery:__ The :EXECUTABLE argument to
    SB-EXT:SAVE-LISP-AND-DIE can produce a stand-alone executable
    containing both an image of the current Lisp session and an SBCL
    runtime.

  - __Timeouts and Deadlines:__ SBCL allows restricting the execution
    time of individual operations or parts of a computation using
    :TIMEOUT arguments to certain blocking operations, synchronous
    timeouts and asynchronous timeouts. The latter two affect operations
    without explicit timeout support (such as standard functions and
    macros). See @TIMEOUTS-AND-DEADLINES.

  - __Metaobject Protocol:__ The `SB-MOP` package provides an
    implementation of the metaobject protocol for the Common Lisp
    Object System as described in _The Art of the Metaobject Protocol_
    by Kiczales et al.

  - __Extensible Sequences:__ SBCL allows users to define subclasses
    of the SEQUENCE class. See @EXTENSIBLE-SEQUENCES.

  - __Extensible Streams:__ The package `SB-GRAY` provides an
    implementation of @GRAY-STREAMS.

      The @SB-SIMPLE-STREAMS module is an implementation of the Simple
      Streams API proposed by Franz Inc.

  - __CLTL2 Compatibility Layer:__ The `SB-CLTL2` module provides
    SB-CLTL2:COMPILER-LET and environment access functionality
    described in _Common Lisp The Language, 2nd Edition_ which were
    removed from the language during the ANSI standardization process.

  - __Recursive Event Loop:__ SBCL provides a recursive event
    loop (`SERVE-EVENT`) for doing non-blocking IO on multiple streams
    without using threads.

  - __System Definition Tool:__
    [ASDF](https://www.sbcl.org/manual/asdf.html) is a flexible and
    popular protocol-oriented system definition tool by Daniel Barlow.
    It is bundled with SBCL. Load it with `(REQUIRE :ASDF)`.")
