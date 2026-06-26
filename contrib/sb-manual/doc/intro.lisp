(in-package :sb-manual)

(defsection @introduction (:title "Introduction")
  "SBCL is a mostly-conforming implementation of the ANSI Common Lisp
  standard. This manual focuses on behavior which is specific to SBCL,
  not on behavior which is common to all implementations of ANSI Common
  Lisp."
  (@ansi-conformance section)
  (@extensions section)
  (@idiosyncrasies section)
  (@development-tools section)
  (@more-sbcl-information section)
  (@more-common-lisp-information section)
  (@history-and-implementation-of-sbcl section))

(defsection @ansi-conformance (:title "ANSI Conformance")
  "Essentially every type of non-conformance is considered a bug. (The
  exceptions involve internal inconsistencies in the standard.) See
  @REPORTING-BUGS.

  - PROG2 returns the primary value of its second form, as
    specified in the _Arguments and Values_ section of the
    specification for that operator, not that of its first form, as
    specified in the _Description_.

  - The STRING type is considered to be the union of all types
    `(ARRAY C (SIZE))` for all non-`NIL` subtypes `C` of
     [CHARACTER][type], excluding arrays specialized to the empty
     type.

  - The `:ORDER` long form option in DEFINE-METHOD-COMBINATION method
    group specifiers accepts the value NIL as well as
    :MOST-SPECIFIC-FIRST and :MOST-SPECIFIC-LAST, in order to allow
    programmers to declare that the order of methods playing that role
    in the method combination does not matter.")

;;; FIXME: Document SERVE-EVENT?
(defsection @extensions (:title "Extensions")
  "SBCL comes with numerous extensions, some in core and some in modules
  loadable with REQUIRE. Unfortunately, not all of these extensions
  have proper documentation yet.

  - __System Definition Tool:__ ASDF is a flexible and popular
    protocol-oriented system definition tool by Daniel Barlow.

  - __Foreign Function Interface:__ The `SB-ALIEN` package allows
    interfacing with C-code, loading shared object files, etc. See
    @FOREIGN-FUNCTION-INTERFACE.

      @SB-GROVEL can be used to partially automate generation of
      foreign function interface definitions.

  - __Recursive Event Loop:__ SBCL provides a recursive event
    loop (`SERVE-EVENT`) for doing non-blocking IO on multiple streams
    without using threads.

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

  - __Native Threads:__ SBCL has native threads on numerous platforms,
    capable of taking advantage of SMP on multiprocessor machines. See
    @THREADING.

  - __Network Interface:__ The `SB-BSD-SOCKETS` module is a low-level
    networking interface, providing both TCP and UDP sockets. See
    @NETWORKING.

  - __Introspective Facilities:__ The @SB-INTROSPECT module offers
    numerous introspective extensions, including access to function
    lambda-lists and a cross referencing facility.

  - __Operating System Interface:__ The `SB-EXT` package contains a
    number of functions for running external processes, accessing
    environment variables, etc.

      The @SB-POSIX module provides a lispy interface to standard
      POSIX facilities.

  - __Extensible Streams:__ The package `SB-GRAY` provides an
    implementation of @GRAY-STREAMS.

      The @SB-SIMPLE-STREAMS module is an implementation of the Simple
      Streams API proposed by Franz Inc.

  - __Profiling:__ The `SB-PROFILE` package provides an exact,
    per-function @DETERMINISTIC-PROFILER.

      The `SB-SPROF` module is SBCL's @STATISTICAL-PROFILER, capable
      of call-graph generation and instruction level profiling, which
      also supports allocation profiling.

  - __Customization Hooks:__ SBCL contains a number of extra-standard
    customization hooks that can be used to tweak the behaviour of the
    system. See @CUSTOMIZATION-HOOKS-FOR-USERS.

  - __sb-aclrepl:__ The @SB-ACLREPL module provides an Allegro-style
    toplevel for SBCL, as an alternative to the classic CMUCL-style
    one.

  - __CLTL2 Compatibility Layer:__ The SB-CLTL2 module provides
    SB-CLTL2:COMPILER-LET and environment access functionality
    described in _Common Lisp The Language, 2nd Edition_ which were
    removed from the language during the ANSI standardization process.

  - __Executable Delivery:__ The :EXECUTABLE argument to
    SB-EXT:SAVE-LISP-AND-DIE can produce a \"standalone\" executable
    containing both an image of the current Lisp session and an SBCL
    runtime.

  - __Bitwise Rotation:__ The @SB-ROTATE-BYTE module provides an
    efficient primitive for bitwise rotation of integers, an operation
    required by e.g. numerous cryptographic algorithms but not
    available as a primitive in ANSI Common Lisp.

  - __Test Harness:__ The `SB-RT` module is a simple yet attractive
    regression and unit-test framework.

  - __MD5 Sums:__ The @SB-MD5 module provides an implementation of the
    MD5 message digest algorithm for Common Lisp, using the
    @MODULAR-ARITHMETIC optimizations provided by SBCL.")

(defsection @idiosyncrasies (:title "Idiosyncrasies")
  "The information in this section describes some of the ways that SBCL
  deals with choices that the ANSI standard leaves to the
  implementation."
  (@declarations section)
  (@fasl-format section)
  (@compiler-only-implementation section)
  (@defining-constants section)
  (@style-warnings section))

(defsection @declarations (:title "Declarations")
  "Declarations are generally treated as assertions. This general
  principle, and its implications, and the bugs which still keep the
  compiler from quite satisfying this principle, are discussed in
  @DECLARATIONS-AS-ASSERTIONS.")

(defsection @fasl-format (:title "FASL format")
  "SBCL fasl-format is binary compatible only with the exact SBCL version
  it was generated with. While this is obviously suboptimal, it has
  proven more robust than trying to maintain fasl compatibility across
  versions: accidentally breaking things is far too easy, and can lead
  to hard to diagnose bugs.

  The following snippet handles fasl recompilation automatically for
  ASDF-based systems, and makes a good candidate for inclusion in the
  user or system initialization file (see @INITIALIZATION-FILES).

      (require :asdf)

      ;;; If a fasl was stale, try to recompile and load (once).
      (defmethod asdf:perform :around ((o asdf:load-op)
                                       (c asdf:cl-source-file))
         (handler-case (call-next-method o c)
            ;; If a fasl was stale, try to recompile and load (once).
            (sb-ext:invalid-fasl ()
               (asdf:perform (make-instance 'asdf:compile-op) c)
               (call-next-method))))")

(defsection @compiler-only-implementation
    (:title "Compiler-only Implementation")
  "SBCL is essentially a compiler-only implementation of Common Lisp.
  That is, for all but a few special cases, EVAL creates a lambda
  expression, calls COMPILE on the lambda expression to create a
  compiled function, and then calls FUNCALL on the resulting function
  object. A more traditional interpreter is also available on default
  builds; it is usually only called internally. This is explicitly
  allowed by the ANSI standard but leads to some oddities; e.g. at
  default settings, FUNCTIONP and COMPILED-FUNCTION-P are equivalent,
  and they collapse into the same function when SBCL is built without
  the interpreter.")

(defsection @defining-constants (:title "Defining Constants")
  "SBCL is quite strict about ANSI's definition of DEFCONSTANT.
  ANSI says that doing DEFCONSTANT of the same symbol more than once
  is undefined unless the new value is EQL to the old value.
  Conforming to this specification is a nuisance when the \"constant\"
  value is only constant under some weaker test like STRING= or EQUAL.

  It's especially annoying because, in SBCL, DEFCONSTANT takes effect
  not only at load time but also at compile time, so that just
  compiling and loading reasonable code like

      (defconstant +foobyte+ '(1 4))

  runs into this undefined behavior. Many implementations of Common
  Lisp try to help the programmer around this annoyance by silently
  accepting the undefined code and trying to do what the programmer
  probably meant.

  SBCL instead treats the undefined behavior as an error. Often such
  code can be rewritten in portable ANSI Common Lisp which has the
  desired behavior. E.g., the code above can be given an exactly
  defined meaning by replacing DEFCONSTANT either with DEFPARAMETER or
  with a customized macro which does the right thing, e.g.

      (defmacro define-constant (name value &optional doc)
        `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
                            ,@(when doc (list doc))))

  or possibly along the lines of the SB-INT:DEFCONSTANT-EQX macro used
  internally in the implementation of SBCL itself. In circumstances
  where this is not appropriate, the programmer can handle the
  condition type SB-EXT:DEFCONSTANT-UNEQL and choose either the
  CONTINUE restart or ABORT restart as appropriate.")

(defsection @style-warnings (:title "Style Warnings")
  "SBCL gives style warnings about various kinds of perfectly legal code,
  e.g.

  - multiple DEFUNs of the same symbol in different units;

  - special variables not named in the conventional `*foo*` style, and
    lexical variables unconventionally named in the `*FOO*` style.

  This causes friction with people who point out that other ways of
  organizing code (especially avoiding the use of DEFGENERIC) are just
  as aesthetically stylish. However, these warnings should be read not
  as _warning, bad aesthetics detected, you have no style_ but as
  _warning, this style keeps the compiler from understanding the code
  as well as you might like_. That is, unless the compiler warns about
  such conditions, there's no way for the compiler to warn about some
  programming errors which would otherwise be easy to
  overlook. (Related bug: The warning about multiple DEFUNs is
  pointlessly annoying when you compile and then load a function
  containing DEFUN wrapped in EVAL-WHEN, and ideally should be
  suppressed in that case, but still isn't as of SBCL 0.7.6.)")

(defsection @development-tools (:title "Development Tools")
  (@editor-integration section)
  (@language-reference section)
  (@generating-executables section))

(defsection @editor-integration (:title "Editor Integration")
  "Though SBCL can be used running \"bare\", the recommended mode of
  development is with an editor connected to SBCL, supporting not
  only basic lisp editing (paren-matching, etc), but providing among
  other features an integrated debugger, interactive compilation, and
  automated documentation lookup.

  Currently _SLIME_ (Superior Lisp Interaction Mode for Emacs)
  together with Emacs is recommended for use with SBCL, though other
  options exist as well. Historically, the ILISP package at
  <http://ilisp.cons.org/> provided similar functionality, but it does
  not support modern SBCL versions.

  SLIME can be downloaded from <https://slime.common-lisp.dev/>.")

(defsection @language-reference (:title "Language Reference")
  "_\\CLHS_ (Common Lisp Hyperspec) is a hypertext version of the ANSI
  standard, made freely available by LispWorks -- an invaluable
  reference.

  See <https://www.lispworks.com/documentation/HyperSpec/Front/index.htm>.")

(defsection @generating-executables (:title "Generating Executables")
  "SBCL can generate stand-alone executables. The generated executables
  include the SBCL runtime itself, so no restrictions are placed on
  program functionality. For example, a deployed program can call
  COMPILE and LOAD, which requires the compiler to be present in the
  executable. For further information, SB-EXT:SAVE-LISP-AND-DIE.")

(defsection @more-sbcl-information (:title "More SBCL Information")
  (@sbcl-homepage section)
  (@online-documentation section)
  (@additional-documentation-files section)
  (@internals-documentation section))

(defsection @sbcl-homepage (:title "SBCL Homepage")
  "The SBCL website at <http://www.sbcl.org/> has some general
  information, plus links to mailing lists devoted to SBCL, and to
  archives of these mailing lists. Subscribing to the mailing lists
  `sbcl-help` and `sbcl-announce` is recommended: both are fairly
  low-volume, and help you keep abreast with SBCL development.")

(defsection @online-documentation (:title "Online Documentation")
  "Documentation for non-ANSI extensions for various commands is
  available online from the SBCL executable itself. The extensions for
  functions which have their own command prompts (e.g. the debugger,
  and INSPECT) are documented in text available by typing `help` at
  their command prompts. The extensions for functions which don't have
  their own command prompt (such as TRACE) are described in their
  documentation strings, unless your SBCL was compiled with an option
  not to include documentation strings, in which case the
  documentation strings are only readable in the source code.")

(defsection @additional-documentation-files
    (:title "Additional Documentation Files")
  "Besides this user manual both SBCL source and binary distributions
  include some other SBCL-specific documentation files, which should
  be installed along with this manual on your system, e.g. in
  `/usr/local/share/doc/sbcl/`.

  - `COPYING`: Licence and copyright summary.

  - `CREDITS`: Authorship information on various parts of SBCL.

  - `INSTALL`: Covers installing SBCL from both source and binary
     distributions on your system, and also has some installation
     related troubleshooting information.

  - `NEWS`: Summarizes changes between various SBCL versions.")

(defsection @internals-documentation (:title "Internals Documentation")
  "If you're interested in the development of the SBCL system itself,
  then subscribing to `sbcl-devel` is a good idea.

  SBCL internals documentation -- besides comments in the source -- is
  available in the Web Archive:

  <https://web.archive.org/web/20120814000933/http://sbcl-internals.cliki.net/index>.

  Some low-level information describing the programming details of the
  conversion from CMUCL to SBCL is available in the
  `doc/FOR-CMUCL-DEVELOPERS` file.")

(defsection @more-common-lisp-information
    (:title "More Common Lisp Information")
  (@internet-community section)
  (@third-party-libraries section)
  (@common-lisp-books section))

(defsection @internet-community (:title "Internet Community")
  "IRC channels on <https://libera.chat/>:

  - `#common-lisp`: \"Common Lisp, the #1=(programmable . #1#)
    programming language\"

  - `#lispcafe`: \"The Lisp Café; sit down, have a drink, chat about
    anything, and enjoy your stay. | <https://www.cliki.net/lispcafe> |
    Be insuperable to each other\".

  - `#sbcl`: \"Steel Bank Common Lisp Dev Hangout\"

  You can use <https://web.libera.chat> or a normal IRC client.

  Also, see <https://www.reddit.com/r/Common_Lisp/>, as well as
  <https://www.lisp.org> and <https://cliki.net>, which contain
  numerous pointers places in the net where lispers talks shop.")

(defsection @third-party-libraries (:title "Third-party Libraries")
  "For a wealth of information about free Common Lisp libraries and tools
  we recommend checking out _CLiki_: <https://cliki.net/>.

  The most popular library manager is Quicklisp:
  <https://www.quicklisp.org/beta/>.")

(defsection @common-lisp-books (:title "Common Lisp Books")
  "If you're not a programmer and you're trying to learn, many
  introductory Lisp books are available. However, we don't have any
  standout favorites.

  If you are an experienced programmer in other languages but need to
  learn about Common Lisp, some books stand out:

  - Practical Common Lisp, by Peter Seibel

      An excellent introduction to the language, covering both the
      basics and \"advanced topics\" like macros, CLOS, and packages.
      Available both in print format and on the web:
      <https://gigamonkeys.com/book/>.

  - Paradigms Of Artificial Intelligence Programming, by Peter Norvig

      Good information on general Common Lisp programming, and many
      nontrivial examples. Whether or not your work is AI, it's a very
      good book to look at.

  - On Lisp, by Paul Graham

      An in-depth treatment of macros, but not recommended as a first
      Common Lisp book, since it is slightly pre-ANSI so you need to
      be on your guard against non-standard usages, and since it
      doesn't really even try to cover the language as a whole,
      focusing solely on macros. Downloadable from
      <https://www.paulgraham.com/onlisp.html>.

  - Object-Oriented Programming In Common Lisp, by Sonya Keene

      With the exception of _Practical Common Lisp_, most introductory
      books don't emphasize CLOS. This one does. Even if you're very
      knowledgeable about object oriented programming in the abstract,
      it's worth looking at this book if you want to do any OO in
      Common Lisp. Some abstractions in CLOS (especially multiple
      dispatch) go beyond anything you'll see in most OO systems, and
      there are a number of lesser differences as well. This book
      tends to help with the culture shock.

  - Art Of Metaobject Programming, by Gregor Kiczales et al.

      Currently the prime source of information on the Common Lisp
      Metaobject Protocol, which is supported by SBCL. Section
      2 (Chapters 5 and 6) are freely available at
      <http://mop.lisp.se/www.alu.org/mop/>.")

(defsection @history-and-implementation-of-sbcl
    (:title "History and Implementation of SBCL")
  "You can work productively with SBCL without knowing or understanding
  anything about where it came from, how it is implemented, or how it
  extends the ANSI Common Lisp standard. However, a little knowledge
  can be helpful in order to understand error messages, to
  troubleshoot problems, to understand why some parts of the system
  are better debugged than others, and to anticipate which known bugs,
  known performance problems, and missing extensions are likely to be
  fixed, tuned, or added.

  SBCL is descended from CMUCL, which is itself descended from Spice
  Lisp, including early implementations for the Mach operating system on
  the IBM RT, back in the 1980s. Some design decisions from that time are
  still reflected in the current implementation:

  - The system expects to be loaded into a fixed-at-compile-time
    location in virtual memory, and also expects the location of all
    of its heap storage to be specified at compile time.

  - The system overcommits memory, allocating large amounts of address
    space from the system (often more than the amount of virtual
    memory available) and then failing if it ends up using too much of
    the allocated storage.

  - The system is implemented as a C program which is responsible for
    supplying low-level services and loading a Lisp `.core` file.

  SBCL also inherited some newer architectural features from CMUCL.
  The most important is that on some architectures it has a
  @GENERATIONAL-GC, which has various implications (mostly good) for
  performance. These are discussed in another chapter, @EFFICIENCY.

  SBCL has diverged from CMUCL in that SBCL is now essentially a
  compiler-only implementation of Common Lisp. This is a change in
  implementation strategy, taking advantage of the freedom \"any of
  these facilities might share the same execution strategy\"
  guaranteed in CLHS `3.1` (Evaluation). It does not mean SBCL can't
  be used interactively, and in fact the change is largely invisible
  to the casual user, since SBCL still can and does execute code
  interactively by compiling it on the fly. (It is visible if you know
  how to look, like using COMPILED-FUNCTION-P; and it is visible in
  the way that SBCL doesn't have many bugs which behave differently in
  interpreted code than in compiled code.) What it means is that in
  SBCL, the EVAL function only truly \"interprets\" a few easy kinds
  of forms, such as symbols which are BOUNDP. More complicated forms
  are evaluated by calling COMPILE and then calling FUNCALL on the
  returned result.

  The direct ancestor of SBCL is the x86 port of CMUCL. This port was in
  some ways the most cobbled-together of all the CMUCL ports, since a
  number of strange changes had to be made to support the register-poor
  x86 architecture. Some things (like tracing and debugging) do not work
  particularly well there. SBCL should be able to improve in these areas
  (and has already improved in some other areas), but it takes a while.

  On the x86 SBCL -- like the x86 port of CMUCL -- uses a
  _@CONSERVATIVE-GC. This means that it doesn't maintain a strict
  separation between tagged and untagged data, instead treating some
  untagged data (e.g. raw floating point numbers) as possibly-tagged
  data and so not collecting any Lisp objects that they point to. This
  has some negative consequences for average time efficiency (though
  possibly no worse than the negative consequences of trying to
  implement an exact GC on a processor architecture as register-poor
  as the X86) and also has potentially unlimited consequences for
  worst-case memory efficiency. In practice, @CONSERVATIVE-GCs work
  reasonably well, not getting anywhere near the worst case. But they
  can occasionally cause odd patterns of memory usage.

  The fork from CMUCL was based on a major rewrite of the system
  bootstrap process. CMUCL has for many years tolerated a very unusual
  \"build\" procedure which doesn't actually build the complete system
  from scratch, but instead progressively overwrites parts of a
  running system with new versions. This quasi-build procedure can
  cause various bizarre bootstrapping hangups, especially when a major
  change is made to the system. It also makes the connection between
  the current source code and the current executable more tenuous than
  in other software systems -- it's easy to accidentally build a CMUCL
  system containing characteristics not reflected in the current
  version of the source code.

  Other major changes since the fork from CMUCL include:

  - SBCL has removed many CMUCL extensions, (e.g. IP networking,
    remote procedure call, Unix system interface, and X11 interface)
    from the core system. Most of these are available as contributed
    modules (distributed with SBCL) or third-party modules instead.

  - SBCL has deleted or deprecated some nonstandard features and code
    complexity which helped efficiency at the price of
    maintainability. For example, the SBCL compiler no longer
    implements memory pooling internally (and so is simpler and more
    maintainable, but generates more garbage and runs more slowly).")
