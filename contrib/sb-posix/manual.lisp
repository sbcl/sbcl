(in-package :sb-manual)

(defsection @sb-posix (:title "sb-posix")
  "Sb-posix is the supported interface for calling out to the operating
  system.

  > _Note_: The functionality contained in the package `SB-UNIX` is
  > for SBCL internal use only; its contents are likely to change from
  > version to version.

  The scope of this interface is \"operating system calls on a typical
  Unixlike platform\". This is section 2 of the Unix manual, plus
  section 3 calls that are (a) typically found in libc, but (b) not
  part of the C standard. For example, we intend to provide support
  for `opendir(3)` and `readdir(3)` but not for `printf(3)`. That
  said, if your favourite system call is not included yet, you are
  encouraged to submit a patch to the SBCL mailing list.

  Some facilities are omitted where they offer absolutely no
  additional use over some portable function, or would be actively
  dangerous to the consistency of Lisp. Not all functions are
  available on all platforms.

  Sb-posix functions do not implicitly take measures to provide
  thread-safety or reentrancy beyond whatever the underlying C library
  does, except in cases where doing so is necessary to maintain the
  consistency of the Lisp image. For example, the bindings to the user
  and group database accessing functions are neither thread-safe nor
  reentrant unless the underlying libc happens to make them so (but
  see @SB-POSIX-EXTENSIONS-TO-POSIX)."
  (@sb-posix-lisp-names section)
  (@sb-posix-types section)
  (@sb-posix-function-parameters section)
  (@sb-posix-function-return-values section)
  (@sb-posix-lisp-objects-and-c-structures section)
  (@sb-posix-idiosyncracies section)
  (@sb-posix-extensions-to-posix section))

(defsection @sb-posix-lisp-names (:title "Lisp names for C names")
  "All symbols are in the `SB-POSIX` package. This package contains a
  Lisp function for each supported Unix system call or function, a
  variable or constant for each supported Unix constant, an object
  type for each supported Unix structure type, and a slot name for
  each supported Unix structure member. A symbol name is derived from
  the C binding's name, by (a) uppercasing, then (b) removing leading
  underscores (`#\\_`) then replacing remaining underscore characters
  with the hyphen (`#\\-`). The requirement to uppercase is so that in
  a standard upcasing reader the user may write `sb-posix:creat`
  instead of `sb-posix:|creat|` as would otherise be required.

  No other changes to \"Lispify\" symbol names are made, so
  `creat` becomes `\\\\CREAT`, not `\\\\CREATE`.

  The user is encouraged not to `(USE-PACKAGE :SB-POSIX)` but instead
  to use the `SB-POSIX:` prefix on all references, as some of the
  symbols symbols contained in the `SB-POSIX` package have the same
  name as CL symbols (e.g. OPEN, CLOSE, SIGNAL). Also, see
  @PACKAGE-LOCAL-NICKNAMES.")

(defsection @sb-posix-types (:title "Types")
  "Generally, marshalling between Lisp and C data types is done using
  SBCL's FFI. See @FOREIGN-FUNCTION-INTERFACE.

  Some functions accept objects such as filenames or file descriptors.
  In the C binding to POSIX, these are represented as strings and
  small integers respectively. For the Lisp programmer's convenience
  we introduce designators such that CL pathnames or open streams can
  be passed to these functions. For example, SB-POSIX:RENAME accepts
  both pathnames and strings as its arguments."
  (@sb-posix-file-descriptors section)
  (@sb-posix-filenames section))

(defsection @sb-posix-file-descriptors (:title "File-descriptors")
  (sb-posix:file-descriptor type)
  (sb-posix:file-descriptor-designator type)
  (sb-posix:file-descriptor function))

(defsection @sb-posix-filenames (:title "Filenames")
  (sb-posix:filename type)
  (sb-posix:filename-designator type)
  (sb-posix:filename function))

(defsection @sb-posix-function-parameters (:title "Function Parameters")
  "The calling convention is modelled after that of CMUCL's `UNIX`
  package: in particular, it's like the C interface except that:

  - Length arguments are omitted or optional where the sensible value
    is obvious. For example, `\\read` would be defined this way:

          (read fd buffer &optional (length (length buffer))) => bytes-read

  - Where C simulates \"out\" parameters using pointers (for instance,
    in `pipe(2)` or `socketpair(2)`), these may be optional or omitted
    in the Lisp interface: if not provided, appropriate objects will
    be allocated and returned (using multiple return values if
    necessary).

  - Some functions accept objects such as filenames or file
    descriptors. Wherever these are specified as such in the C
    bindings, the Lisp interface accepts designators for them as
    specified in the @SB-POSIX-TYPES section above.

  - A few functions have been included in sb-posix that do not
    correspond exactly with their C counterparts. These are described
    in @SB-POSIX-IDIOSYNCRACIES.")

(defsection @sb-posix-function-return-values (:title "Function Return Values")
  "The return value is usually the same as for the C binding, except in
  error cases: where the C function is defined as returning some
  sentinel value and setting `errno` on error, we instead signal an
  error of type SB-POSIX:SYSCALL-ERROR. The actual error
  value (`errno`) is stored in this condition and can be accessed with
  SB-POSIX:SYSCALL-ERRNO.

  We do not automatically translate the returned value into lispy
  objects -- for example, SB-POSIX:OPEN returns a small integer, not a
  stream. Exception: boolean-returning functions (or, more commonly,
  macros) do not return a C integer but instead a Lisp boolean.")

(defsection @sb-posix-lisp-objects-and-c-structures
    (:title "Lisp Objects and C structures")
  "Sb-posix provides various Lisp object types to stand in for C
  structures in the POSIX library. Lisp bindings to C functions that
  accept, manipulate, or return C structures accept, manipulate, or
  return instances of these Lisp types instead of instances of alien
  types.

  The names of the Lisp types are chosen according to the general
  rules described above. For example Lisp objects of type
  SB-POSIX:STAT stand in for C structures of type `struct stat`.

  Accessors are provided for each standard field in the structure.
  These are named `<STRUCTURE-NAME>-<FIELD-NAME>` where the two
  components are chosen according to the general name conversion
  rules, with the exception that in cases where all fields in a given
  structure have a common prefix, that prefix is omitted. For example,
  `stat.st_dev` in C becomes `\\STAT-DEV` in Lisp.

  Because sb-posix might not support all semi-standard or
  implementation-dependent members of all structure types on your
  system (patches welcome), here is an enumeration of all supported
  Lisp objects corresponding to supported POSIX structures, and the
  supported slots for those structures."
  #-(or android win32)
  (sb-posix:flock class)
  #-(or android win32)
  (sb-posix:passwd class)
  #-(or android win32)
  (sb-posix:group class)
  (sb-posix:stat class)
  #-win32
  (sb-posix:termios class)
  #-win32
  (sb-posix:timeval class))

(defsection @sb-posix-idiosyncracies
    (:title "Functions with Idiosyncratic Bindings")
  "A few functions in sb-posix don't correspond directly to their C
  counterparts."
  (sb-posix:getcwd function)
  #-win32
  (sb-posix:readlink function)
  #-win32
  (sb-posix:syslog function))

(defsection @sb-posix-extensions-to-posix (:title "Extensions to POSIX")
  "Some of POSIX's standardized operators are not safe to use on their
  own, so `SB-POSIX` exports a few helpers that do not correspond
  exactly to functionality present in the POSIX standard.

  The user and group database accessing routines are not required to
  be thread-safe or reentrant and so can only be used safely if all
  clients coordinate around their use. Since it would be logically
  impossible for independently developed programs to coordinate,
  `SB-POSIX` exports two iteration macros, SB-POSIX:DO-PASSWDS and
  SB-POSIX:DO-GROUPS, each of which iterates over the respective
  database while preventing the keyed accesses (SB-POSIX:GETPWNAM,
  SB-POSIX:GETPWUID, SB-POSIX:GETGRNAM, SB-POSIX:GETGRGID) from
  running until iteration completes."
  #-(or android win32)
  (sb-posix:do-passwds macro)
  #-(or android win32)
  (sb-posix:do-groups macro))
