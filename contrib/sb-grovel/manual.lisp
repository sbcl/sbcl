(in-package :sb-manual)

(defsection @sb-grovel (:title "sb-grovel")
  "The `SB-GROVEL` module helps in generation of foreign function
  interfaces. It aids in extracting constants' values from the C
  compiler and in generating sb-alien structure and union types,
  @DEFINING-FOREIGN-TYPES.

  The ASDF (<http://www.cliki.net/ASDF>) component type
  GROVEL-CONSTANTS-FILE has its ASDF:PERFORM operation defined to
  write out a C source file, compile it, and run it. The output from
  this program is Lisp, which is then itself compiled and loaded.

  `SB-GROVEL` is used in a few contributed modules, and it is
  currently compatible only to SBCL. However, if you want to use it,
  here are a few directions."
  (@using-sb-grovel section)
  (@sb-grovel-constants-file section)
  (@sb-grovel-structures section)
  (@sb-grovel-traps section))

(defsection @using-sb-grovel (:title "Using sb-grovel in your own ASDF System")
  "- Create a Lisp package for the foreign constants/functions to go
  into.

  - Make your system depend on the `SB-GROVEL` system.

  - Create a grovel-constants data file -- for an example, see
    `example-constants.lisp` in the `contrib/sb-grovel/` directory in
    the SBCL source distribution.

  - Add it as a component in your system. For example:

          (eval-when (:compile-toplevel :load-toplevel :execute)
            (require :sb-grovel))

          (defpackage :example-package.system
                      (:use :cl :asdf :sb-grovel :sb-alien))

          (in-package :example-package.system)

          (defsystem example-system
              :depends-on (sb-grovel)
              :components
              ((:module \"sbcl\"
                        :components
                        ((:file \"defpackage\")
                         (grovel-constants-file \"example-constants\"
                                                :package :example-package)))))

  Make sure to specify the package you chose in step 1.

  - Build stuff.")

(defsection @sb-grovel-constants-file
    (:title "Contents of a grovel-constants-file")
  "The grovel-constants-file, typically named `constants.lisp`,
  comprises lisp expressions describing the foreign things that you
  want to grovel for. A `constants.lisp` file contains two sections:

  - a list of headers to include in the C program, for example:

          (\"sys/types.h\" \"sys/socket.h\" \"sys/stat.h\" \"unistd.h\" \"sys/un.h\"
           \"netinet/in.h\" \"netinet/in_systm.h\" \"netinet/ip.h\" \"net/if.h\"
           \"netdb.h\" \"errno.h\" \"netinet/tcp.h\" \"fcntl.h\" \"signal.h\")

  - A list of sb-grovel clauses describing the things you want to
    grovel from the C compiler, for example:

          ((:integer af-local
                     #+(or sunos solaris) \"AF_UNIX\"
                     #-(or sunos solaris) \"AF_LOCAL\"
                     \"Local to host (pipes and file-domain).\")
           (:structure stat (\"struct stat\"
                             (integer dev \"dev_t\" \"st_dev\")
                             (integer atime \"time_t\" \"st_atime\")))
           (:function getpid (\"getpid\" int )))

  There are two types of things that sb-grovel can sensibly extract
  from the C compiler: constant integers and structure layouts. It is
  also possible to define foreign functions in the constants.lisp
  file, but these definitions don't use any information from the C
  program; they expand directly to SB-ALIEN:DEFINE-ALIEN-ROUTINE
  forms.

  Here's how to use the grovel clauses:

  - :INTEGER: constant expressions in C. Used in this form:

           (:integer lisp-variable-name \"C expression\" &optional doc export)

      `\"C expression\"` will be typically be the name of a constant,
      but other forms are possible.

  - :ENUM:

           (:enum lisp-type-name ((lisp-enumerated-name c-enumerated-name) ...)))

      An SB-ALIEN:ENUM type with name `LISP-TYPE-NAME` will be
      defined. The symbols are the `LISP-ENUMERATED-NAME`s, and the
      values are grovelled from the `C-ENUMERATED-NAME`s.

  - :STRUCTURE: alien structure definitions look like this:

           (:structure lisp-struct-name (\"struct c_structure\"
                                         (type-designator lisp-element-name
                                          \"c_element_type\" \"c_element_name\"
                                          :distrust-length nil)
                                         ; ...
                                         ))

      `TYPE-DESIGNATOR` is a reference to a type whose size (and type
      constraints) will be groveled for. sb-grovel accepts a form of
      type designator that doesn't quite conform to either lisp nor
      sb-alien's type specifiers. Here's a list of type designators
      that sb-grovel currently accepts:

      - `\\INTEGER`: a C integral type; sb-grovel will infer the exact
        type from size information extracted from the C program. All
        common C integer types can be grovelled for with this type
        designator, but it is not possible to grovel for bit fields
        yet.

      - `(UNSIGNED N)`: an unsigned integer variable that is `N` bytes
        long. No size information from the C program will be used.

      - `(SIGNED N)`: an signed integer variable that is `N` bytes
        long. No size information from the C program will be used.

      - `\\C-STRING`: an array of `\\char` in the structure. sb-grovel
        will use the array's length from the C program, unless you
        pass it the :DISTRUST-LENGTH keyword argument with non-`NIL`
        value (this might be required for structures such as solaris's
        `struct dirent`).

      - SB-GROVEL::C-STRING-POINTER: a pointer to a C string,
        corresponding to the SB-ALIEN:C-STRING type (see
        @FOREIGN-TYPE-SPECIFIERS).

      - `(ARRAY ALIEN-TYPE)`: an array of the previously-declared
        `ALIEN-TYPE`. The array's size will be determined from the
        output of the C program and the alien type's size.

      - `(ARRAY ALIEN-TYPE N):` an array of the previously-declared
        `ALIEN-TYPE`. The array's size will be assumed as being `N`.

  Note that `\\C-STRING` and SB-GROVEL::C-STRING-POINTER do not have
  the same meaning. If you declare that an element is of type
  C-STRING, it will be treated as if the string is a part of the
  structure, whereas if you declare that the element is of type
  SB-GROVEL::C-STRING-POINTER, a _pointer to a string_ will be the
  structure member.

  - :FUNCTION: alien function definitions are similar to
    DEFINE-ALIEN-ROUTINE definitions, because they expand to such
    forms when the lisp program is loaded. See
    @FOREIGN-FUNCTION-CALLS.

          (:function lisp-function-name
                     (\"alien_function_name\" alien-return-type
                                            (argument alien-type)
                                            (argument2 alien-type)))")

(defsection @sb-grovel-structures
    (:title "Programming with sb-grovel's structure types")
  "Let us assume that you have a grovelled structure definition:

      (:structure mystruct (\"struct my_structure\"
                            (integer myint \"int\" \"st_int\")
                            (c-string mystring \"char[]\" \"st_str\")))

  What can you do with it? Here's a short interface document:

  - Creating and destroying objects:

      - Function `(ALLOCATE-MYSTRUCT)` allocates an object of type
        `mystruct` and returns a system area pointer to it.

      - Macro `(WITH-MYSTRUCT VAR ((MEMBER INIT) [...]) &BODY BODY)`
        allocates an object of type `MYSTRUCT` that is valid in
        `BODY`. If `BODY` terminates or performs an non-local exit,
        the object pointed to by `VAR` will be deallocated.

  - Accessing structure members:

      - `(MYSTRUCT-MYINT VAR)` and `(MYSTRUCT-MYSTRING VAR)` return
        the value of the respective fields in `MYSTRUCT`.

      - `(SETF (MYSTRUCT-MYINT VAR) NEW-VAL)` and
        `(SETF (MYSTRUCT-MYSTRING VAR) NEW-VAL)` sets the value of the
        respective structure member to the value of `NEW-VAL`. Notice
        that in `(SETF (MYSTRUCT-MYSTRING VAR) NEW-VAL)`'s case,
        `NEW-VAL` is a lisp string.")

(defsection @sb-grovel-traps (:title "Traps and Pitfalls")
  "Basically, you can treat functions and data structure definitions that
  sb-grovel spits out as if they were alien routines and types. This has
  a few implications that might not be immediately obvious (especially
  if you have programmed in a previous version of sb-grovel that didn't
  use alien types):

  - You must take care of grovel-allocated structures yourself. They
    are alien types, so the garbage collector will not collect them
    when you drop the last reference.

  - If you use the `WITH-MYSTRUCT` macro, be sure that no references
    to the variable thus allocated leaks out. It will be deallocated
    when the block exits.")
