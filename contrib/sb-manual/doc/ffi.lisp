(in-package :sb-manual)

(defsection @foreign-function-interface
    (:title "Foreign Function Interface")
  "This chapter describes SBCL's interface to C programs and
  libraries (and, since C interfaces are a sort of _lingua franca_
  of the Unix world, to other programs and libraries in general).

  > _Note_: In the modern Lisp world, the usual term for this
  > functionality is Foreign Function Interface, or FFI, where despite
  > the mention of _function_ in this term, FFI also refers to direct
  > manipulation of C data structures as well as functions. The
  > traditional CMUCL terminology is Alien Interface, and while that
  > older terminology is no longer used much in the system
  > documentation, it still reflected in names in the implementation,
  > notably in the name of the `SB-ALIEN` package."
  (@introduction-to-the-foreign-function-interface section)
  (@foreign-types section)
  (@operations-on-foreign-values section)
  (@foreign-variables section)
  (@foreign-data-structure-examples section)
  (@loading-shared-object-files section)
  (@foreign-function-calls section)
  (@calling-lisp-from-c section)
  (@step-by-step-example-of-the-foreign-function-interface section))

(defsection @introduction-to-the-foreign-function-interface
    (:title "Introduction to the Foreign Function Interface")
  ;; AKA Introduction to Aliens in the CMU CL manual
  "Because of Lisp's emphasis on dynamic memory allocation and garbage
  collection, Lisp implementations use non-C-like memory
  representations for objects. This representation mismatch creates
  friction when a Lisp program must share objects with programs which
  expect C data. There are three common approaches to establishing
  communication:

  - The burden can be placed on the foreign program (and programmer)
    by requiring the knowledge and use of the representations used
    internally by the Lisp implementation. This can require a
    considerable amount of \"glue\" code on the C side, and that code
    tends to be sensitively dependent on the internal implementation
    details of the Lisp system.

  - The Lisp system can automatically convert objects back and forth
    between the Lisp and foreign representations. This is convenient,
    but translation becomes prohibitively slow when large or complex
    data structures must be shared. This approach is supported by the
    SBCL FFI, and used automatically when passing integers and
    strings.

  - The Lisp program can directly manipulate foreign objects through
    the use of extensions to the Lisp language.

  SBCL, like CMUCL before it, relies primarily on the automatic
  conversion and direct manipulation approaches. The `SB-ALIEN`
  package provides a facility wherein foreign values of simple scalar
  types are automatically converted and complex types are directly
  manipulated in their foreign representation. Additionally the
  lower-level System Area Pointers (or SAPs) can be used where
  necessary to provide untyped access to foreign memory.

  Any foreign objects that can't automatically be converted into Lisp
  values are represented by objects of type
  SB-ALIEN-INTERNALS:ALIEN-VALUE Since Lisp is a dynamically typed
  language, even foreign objects must have a run-time type; this type
  information is provided by encapsulating the raw pointer to the
  foreign data within an SB-ALIEN-INTERNALS:ALIEN-VALUE object.

  The type language and operations on foreign types are intentionally
  similar to those of the C language.")

(defsection @foreign-types (:title "Foreign Types")
  "Alien types have a description language based on nested list
  structure. For example the C type

      struct foo {
          int a;
          struct foo *b[100];
      };

  has the corresponding SBCL FFI type

      (struct foo
        (a int)
        (b (array (* (struct foo)) 100)))"
  (@defining-foreign-types section)
  (@foreign-types-and-lisp-types section)
  (@foreign-type-specifiers section))

(defsection @defining-foreign-types (:title "Defining Foreign Types")
  "Types may be either named or anonymous. With structure and union
  types, the name is part of the type specifier, allowing recursively
  defined types such as:

      (struct foo (a (* (struct foo))))

  An anonymous structure or union type is specified by using the name
  NIL. The WITH-ALIEN macro defines a local scope which _captures_ any
  named type definitions. Other types are not inherently named, but
  can be given named abbreviations using the DEFINE-ALIEN-TYPE macro.")

(defsection @foreign-types-and-lisp-types
    (:title "Foreign Types and Lisp Types")
  "The foreign types form a subsystem of the SBCL type system. An
  ALIEN type specifier provides a way to use any foreign type as a
  Lisp type specifier. For example,

      (typep foo '(alien (* int)))

  can be used to determine whether `FOO` is a pointer to a foreign
  `int`. ALIEN type specifiers can be used in the same ways as
  ordinary Lisp type specifiers (like STRING.) Alien type declarations
  are subject to the same precise type checking as any other
  declaration. See @PRECISE-TYPE-CHECKING.

  Note that the type identifiers used in the foreign type system
  overlap with native Lisp type specifiers in some cases. For example,
  the type specifier `(ALIEN SINGLE-FLOAT)` is identical to
  SINGLE-FLOAT, since foreign floats are automatically converted to
  Lisp floats. When TYPE-OF is called on an alien value that is not
  automatically converted to a Lisp value, then it will return an
  ALIEN type specifier.")

(defsection @foreign-type-specifiers (:title "Foreign Type Specifiers")
  "> _Note_: All foreign type names are exported from the `SB-ALIEN`
  > package. Some foreign type names are also symbols in the
  > `COMMON-LISP` package, in which case they are reexported from the
  > `SB-ALIEN` package, so that e.g. it is legal to refer to
  > SINGLE-FLOAT.

  These are the basic foreign type specifiers:

  - The foreign type specifier `(* <FOO>)` describes a pointer to an
    object of type `<FOO>`. A pointed-to type `<FOO>` of T indicates a
    pointer to anything, similar to `void *` in ANSI C. A null alien
    pointer can be detected with the NULL-ALIEN function.

  - The foreign type specifier `(ARRAY <FOO> &REST <DIMENSIONS>)`
    describes array of the specified `<DIMENSIONS>`, holding elements
    of type `<FOO>`. Note that (unlike in C) `(* <FOO>)` and
    `(ARRAY <FOO>)` are considered to be different types when
    type checking is done. If equivalence of pointer and array types
    is desired, it may be explicitly coerced using CAST.

  Arrays are accessed using DEREF, passing the indices
  as additional arguments.  Elements are stored in column-major order
  (as in C), so the first dimension determines only the size of the
  memory block, and not the layout of the higher dimensions. An array
  whose first dimension is variable may be specified by using NIL as
  the first dimension. Fixed-size arrays can be allocated as array
  elements, structure slots or WITH-ALIEN variables. Dynamic arrays
  can only be allocated using MAKE-ALIEN.

  - The foreign type specifier `(STRUCT <NAME> &REST <FIELDS>)`
    describes a structure type with the specified `<NAME>` and
    `<FIELDS>`. Fields are allocated at the same offsets used by the
    implementation's C compiler, as guessed by the SBCL internals.
    An optional :ALIGNMENT keyword argument can be specified for each
    field to explicitly control the alignment of a field. If `<NAME>`
    is NIL then the structure is anonymous.

      If a named foreign STRUCT specifier is passed to
      DEFINE-ALIEN-TYPE or WITH-ALIEN, then this defines,
      respectively, a new global or local foreign structure type. If
      no `<FIELDS>` are specified, then the fields are taken from the
      current (local or global) alien structure type definition of
      `<NAME>`.

  - The foreign type specifier `(UNION <NAME> &REST <FIELDS>)` is
    similar to STRUCT but describes a union type. All fields are
    allocated at the same offset, and the size of the union is the
    size of the largest field. The programmer must determine which
    field is active from context.

  - The foreign type specifier `(ENUM <NAME> &REST <SPECS>)` describes
    an enumeration type that maps between integer values and symbols.
    If `<NAME>` is NIL, then the type is anonymous. Each element of
    the `<SPECS>` list is either a Lisp symbol, or a list
    `(<symbol> <value>)`. `<value>` is an integer. If `<value>` is not
    supplied, then it defaults to one greater than the value for the
    preceding spec (or to zero if it is the first spec).

  - The foreign type specifier `(SIGNED &OPTIONAL <BITS>)` specifies a
    signed integer with the specified number of `<BITS>` precision.
    The upper limit on integer precision is determined by the
    machine's word size. If `<BITS>` is not specified, the maximum
    size will be used.

  - The foreign type specifier `(INTEGER &OPTIONAL <BITS>)` is
    equivalent to the corresponding type specifier using SIGNED
    instead of INTEGER.

  - The foreign type specifier `(UNSIGNED &OPTIONAL <BITS>)` is like
    corresponding type specifier using SIGNED except that the variable
    is treated as an unsigned integer.

  - The foreign type specifier `(BOOLEAN &OPTIONAL <BITS>)` is similar
    to an enumeration type but maps from Lisp NIL and T to C 0 and 1
    respectively. `<BITS>` determines the amount of storage allocated
    to hold the truth value.

  - The foreign type specifier `\\SINGLE-FLOAT` describes a
    floating-point number in IEEE single-precision format.

  - The foreign type specifier `\\DOUBLE-FLOAT` describes a
    floating-point number in IEEE double-precision format.

  - The foreign type specifier `(FUNCTION <RESULT-TYPE> &REST
    <ARG-TYPES>)` describes a foreign function that takes arguments of
    the specified `<ARG-TYPES>` and returns a result of type
    `<RESULT-TYPE>`. Note that the only context where a foreign
    `\\FUNCTION` type is directly specified is in the argument to
    ALIEN-FUNCALL. In all other contexts, foreign functions are
    represented by foreign function pointer types: `(* (FUNCTION
    ...))`.

  - The foreign type specifier `\\SYSTEM-AREA-POINTER` describes a
    pointer which is represented in Lisp as a SYSTEM-AREA-POINTER
    object. SBCL exports this type from `SB-ALIEN` because CMUCL did,
    but tentatively (as of the first draft of this section of the
    manual, SBCL 0.7.6) it is deprecated, since it doesn't seem to be
    required by user code.

  - The foreign type specifier VOID is used in function types to
    declare that no useful value is returned. Using ALIEN-FUNCALL to
    call a VOID foreign function will return zero values.

  - The foreign type specifier `(C-STRING &KEY <external-format>
    <element-type> <not-null>)` is similar to `(* CHAR)` but is
    interpreted as a null-terminated string, and is automatically
    converted into a Lisp string when accessed; or if the pointer is C
    `\\NULL` or 0, then accessing it gives Lisp NIL unless
    `<not-null>` is true, in which case a TYPE-ERROR is signalled.

      External format conversion is automatically done when Lisp
      strings are passed to foreign code, or when foreign strings are
      passed to Lisp code. If the type specifier has an explicit
      `<external-format>`, that external format will be used.
      Otherwise SB-EXT:*DEFAULT-C-STRING-EXTERNAL-FORMAT* will be
      used. For example, when the following alien routine is called,
      the Lisp string given as argument is converted to an \\EBCDIC
      octet representation.

          (define-alien-routine test int (str (c-string :external-format :ebcdic-us)))

      Lisp strings of type BASE-STRING are stored with a trailing
      `\\\\NUL` termination, so no copying (either by the user or the
      implementation) is necessary when passing them to foreign code,
      assuming that the `<EXTERNAL-FORMAT>` and `<ELEMENT-TYPE>` of
      the C-STRING type are compatible with the internal
      representation of the string. For an SBCL built with Unicode
      support that means an `<external-format>` of :ASCII and an
      `<ELEMENT-TYPE>` of BASE-CHAR. Without Unicode support the
      `<EXTERNAL-FORMAT>` can also be :ISO-8859-1, and the
      `<ELEMENT-TYPE>` can also be [CHARACTER][type]. If
      `<EXTERNAL-FORMAT>` and `<ELEMENT-TYPE>` are not compatible, or
      the string is a `(SIMPLE-ARRAY CHARACTER (*))`, this data is
      copied by the implementation as required.

      Assigning a Lisp string to a C-STRING structure field or
      variable stores the contents of the string to the memory already
      pointed to by that variable. When a foreign object of type
      `(* CHAR)` is assigned to a C-STRING, then the C-STRING pointer
      is assigned to. This allows C-STRING pointers to be initialized.
      For example:

          (cl:in-package \"CL-USER\") ; which USEs package \"SB-ALIEN\"

          (define-alien-type nil (struct foo (str c-string)))

          (defun make-foo (str)
            (let ((my-foo (make-alien (struct foo))))
              (setf (slot my-foo 'str) (make-alien char (length str))
                    (slot my-foo 'str) str)
              my-foo))

      Storing Lisp NIL in a C-STRING writes C `\\\\NULL` to the
      variable."
  "- `SB-ALIEN` also exports translations of these C type
    specifiers as foreign type specifiers:

      CHAR, SHORT, INT, LONG, UNSIGNED-CHAR, UNSIGNED-SHORT,
      UNSIGNED-INT, UNSIGNED-LONG, FLOAT, DOUBLE, SIZE-T, OFF-T")

(defsection @operations-on-foreign-values
    (:title "Operations On Foreign Values")
  "This section describes how to read foreign values as Lisp values,
  how to coerce foreign values to different kinds of foreign values,
  and how to dynamically allocate and free foreign variables."
  (@accessing-foreign-values section)
  (@coercing-foreign-values section)
  (@foreign-dynamic-allocation section))

(defsection @accessing-foreign-values (:title "Accessing Foreign Values")
  (sb-alien:deref function)
  (sb-alien:slot function)
  (@untyped-memory section))

(defsection @untyped-memory (:title "Untyped memory")
  "As noted at the beginning of the chapter, the System Area Pointer
  facilities allow untyped access to foreign memory. SAPs can be
  converted to and from the usual typed foreign values using SAP-ALIEN
  and ALIEN-SAP, and also to and from integers (raw machine
  addresses). They should thus be used with caution; corrupting the
  Lisp heap or other memory with SAPs is trivial."
  (sb-sys:int-sap function)
  (sb-sys:sap-ref-32 function)
  (sb-sys:sap= function)
  "Similarly named functions exist for accessing other sizes of word,
  other comparisons, and other conversions. The reader is invited to
  use APROPOS and DESCRIBE for more details:

      (apropos \"sap\" :sb-sys)")

(defsection @coercing-foreign-values (:title "Coercing Foreign Values")
  (addr macro)
  (cast macro)
  (sap-alien macro)
  (alien-sap function))

(defsection @foreign-dynamic-allocation (:title "Foreign Dynamic Allocation")
  "Lisp code can call the C standard library functions `malloc`
  and `free` to dynamically allocate and deallocate foreign variables.
  The Lisp code uses the same allocator as foreign C code, so it's
  OK for foreign code to call `free` on the result of Lisp MAKE-ALIEN,
  or for Lisp code to call FREE-ALIEN on foreign objects allocated by
  C code."
  (make-alien macro)
  (make-alien-string function)
  (free-alien function))

(defsection @foreign-variables (:title "Foreign Variables")
  "Both local (stack allocated) and external (C global) foreign
  variables are supported."
  (@local-foreign-variables section)
  (@external-foreign-variables section))

(defsection @local-foreign-variables (:title "Local Foreign Variables")
  (with-alien macro))

(defsection @external-foreign-variables (:title "External Foreign Variables")
  "External foreign names are strings, and Lisp names are symbols. When
  an external foreign value is represented using a Lisp variable, there
  must be a way to convert from one name syntax into the other. The
  macros EXTERN-ALIEN, DEFINE-ALIEN-VARIABLE and
  DEFINE-ALIEN-ROUTINE use this conversion heuristic:

  - Alien names are converted to Lisp names by uppercasing and
    replacing underscores with hyphens.

  - Conversely, Lisp names are converted to alien names by lowercasing
    and replacing hyphens with underscores.

  - Both the Lisp symbol and alien string names may be separately
    specified by using a list of the form

          (<alien-string> <lisp-symbol>)"
  (define-alien-variable macro)
  (get-errno function)
  (extern-alien macro))

(defsection @foreign-data-structure-examples
    (:title "Foreign Data Structure Examples")
  "Now that we have alien types, operations and variables, we can
  manipulate foreign data structures. This C declaration

      struct foo {
          int a;
          struct foo *b[100];
      };

  can be translated into the following alien type:

      (define-alien-type nil
        (struct foo
          (a int)
          (b (array (* (struct foo)) 100))))

  Once the `FOO` alien type has been defined as above, the C
  expression

      struct foo f;
      f.b[7].a;

  can be translated in this way:

      (with-alien ((f (struct foo)))
        (slot (deref (slot f 'b) 7) 'a)
        ;;
        ;; Do something with f...
        )

  Or consider this example of an external C variable and some accesses:

      struct c_struct {
              short x, y;
              char a, b;
              int z;
              c_struct *n;
      };
      extern struct c_struct *my_struct;
      my_struct->x++;
      my_struct->a = 5;
      my_struct = my_struct->n;

  which can be manipulated in Lisp like this:

      (define-alien-type nil
        (struct c-struct
                (x short)
                (y short)
                (a char)
                (b char)
                (z int)
                (n (* c-struct))))
      (define-alien-variable \"my_struct\" (* c-struct))
      (incf (slot my-struct 'x))
      (setf (slot my-struct 'a) 5)
      (setq my-struct (slot my-struct 'n))")

(defsection @loading-shared-object-files (:title "Loading Shared Object Files")
  "Foreign object files can be loaded into the running Lisp process by
  calling LOAD-SHARED-OBJECT."
  (load-shared-object function)
  (unload-shared-object function))

(defsection @foreign-function-calls (:title "Foreign Function Calls")
  "The foreign function call interface allows a Lisp program to call
  many functions written in languages that use the C calling convention.

  Lisp sets up various signal handling routines and other environment
  information when it first starts up, and expects these to be in
  place at all times. The C functions called by Lisp should not change
  the environment, especially the signal handlers: the signal handlers
  installed by Lisp typically have interesting flags set (e.g to
  request machine context information, or for signal delivery on an
  alternate stack) which the Lisp runtime relies on for correct
  operation. Precise details of how this works may change without
  notice between versions; the source, or the brain of a friendly SBCL
  developer, is the only documentation. Users of a Lisp built with the
  :SB-THREAD feature should also read the section about threads,
  @THREADING."
  (alien-funcall function)
  (alien-funcall-into function)
  (define-alien-routine macro))

;; <!-- FIXME: This is a \"changebar\" section from the CMU CL manual.
;;      I (WHN 2002-07-14) am not very familiar with this content, so
;;      I'm not immediately prepared to try to update it for SBCL, and
;;      I'm not feeling masochistic enough to work to encourage this
;;      kind of low-level hack anyway. However, I acknowledge that callbacks
;;      are sometimes really really necessary, so I include the original
;;      text in case someone is hard-core enough to benefit from it. If
;;      anyone brings the information up to date for SBCL, it belong
;;      either in the main manual or on a CLiki SBCL Internals page.
;; LaTeX \subsection{Accessing Lisp Arrays}
;; LaTeX
;; LaTeX Due to the way \cmucl{} manages memory, the amount of memory that can
;; LaTeX be dynamically allocated by \code{malloc} or \funref{make-alien} is
;; LaTeX limited\footnote{\cmucl{} mmaps a large piece of memory for it's own
;; LaTeX   use and this memory is typically about 8 MB above the start of the C
;; LaTeX   heap.  Thus, only about 8 MB of memory can be dynamically
;; LaTeX   allocated.}.
;;
;; Empirically determined to be considerably >8Mb on this x86 linux
;; machine, but I don't know what the actual values are - dan 2003.09.01
;;
;; Note that this technique is used in SB-GROVEL in the SBCL contrib
;;
;; LaTeX
;; LaTeX To overcome this limitation, it is possible to access the content of
;; LaTeX Lisp arrays which are limited only by the amount of physical memory
;; LaTeX and swap space available.  However, this technique is only useful if
;; LaTeX the foreign function takes pointers to memory instead of allocating
;; LaTeX memory for itself.  In latter case, you will have to modify the
;; LaTeX foreign functions.
;; LaTeX
;; LaTeX This technique takes advantage of the fact that \cmucl{} has
;; LaTeX specialized array types (\pxlref{specialized-array-types}) that match
;; LaTeX a typical C array.  For example, a \code{(simple-array double-float
;; LaTeX   (100))} is stored in memory in essentially the same way as the C
;; LaTeX array \code{double x[100]} would be.  The following function allows us
;; LaTeX to get the physical address of such a Lisp array:
;; LaTeX \begin{example}
;; LaTeX (defun array-data-address (array)
;; LaTeX   \"Return the physical address of where the actual data of an array is
;; LaTeX stored.
;; LaTeX
;; LaTeX ARRAY must be a specialized array type in CMU Lisp.  This means ARRAY
;; LaTeX must be an array of one of the following types:
;; LaTeX
;; LaTeX                   double-float
;; LaTeX                   single-float
;; LaTeX                   (unsigned-byte 32)
;; LaTeX                   (unsigned-byte 16)
;; LaTeX                   (unsigned-byte  8)
;; LaTeX                   (signed-byte 32)
;; LaTeX                   (signed-byte 16)
;; LaTeX                   (signed-byte  8)
;; LaTeX \"
;; LaTeX   (declare (type (or #+signed-array (array (signed-byte 8))
;; LaTeX                      #+signed-array (array (signed-byte 16))
;; LaTeX                      #+signed-array (array (signed-byte 32))
;; LaTeX                      (array (unsigned-byte 8))
;; LaTeX                      (array (unsigned-byte 16))
;; LaTeX                      (array (unsigned-byte 32))
;; LaTeX                      (array single-float)
;; LaTeX                      (array double-float))
;; LaTeX                  array)
;; LaTeX            (optimize (speed 3) (safety 0))
;; LaTeX            (ext:optimize-interface (safety 3)))
;; LaTeX   ;; with-array-data will get us to the actual data.  However, because
;; LaTeX   ;; the array could have been displaced, we need to know where the
;; LaTeX   ;; data starts.
;; LaTeX   (lisp::with-array-data ((data array)
;; LaTeX                           (start)
;; LaTeX                           (end))
;; LaTeX     (declare (ignore end))
;; LaTeX     ;; DATA is a specialized simple-array.  Memory is laid out like this:
;; LaTeX     ;;
;; LaTeX     ;;   byte offset    Value
;; LaTeX     ;;        0         type code (should be 70 for double-float vector)
;; LaTeX     ;;        4         4 * number of elements in vector
;; LaTeX     ;;        8         1st element of vector
;; LaTeX     ;;      ...         ...
;; LaTeX     ;;
;; LaTeX     (let ((addr (+ 8 (logandc1 7 (kernel:get-lisp-obj-address data))))
;; LaTeX           (type-size (let ((type (array-element-type data)))
;; LaTeX                        (cond ((or (equal type '(signed-byte 8))
;; LaTeX                                   (equal type '(unsigned-byte 8)))
;; LaTeX                               1)
;; LaTeX                              ((or (equal type '(signed-byte 16))
;; LaTeX                                   (equal type '(unsigned-byte 16)))
;; LaTeX                               2)
;; LaTeX                              ((or (equal type '(signed-byte 32))
;; LaTeX                                   (equal type '(unsigned-byte 32)))
;; LaTeX                               4)
;; LaTeX                              ((equal type 'single-float)
;; LaTeX                               4)
;; LaTeX                              ((equal type 'double-float)
;; LaTeX                               8)
;; LaTeX                              (t
;; LaTeX                               (error \"Unknown specialized array element type\"))))))
;; LaTeX       (declare (type (unsigned-byte 32) addr)
;; LaTeX                (optimize (speed 3) (safety 0) (ext:inhibit-warnings 3)))
;; LaTeX       (system:int-sap (the (unsigned-byte 32)
;; LaTeX                         (+ addr (* type-size start)))))))
;; LaTeX \end{example}
;; LaTeX
;; LaTeX Assume we have the C function below that we wish to use:
;; LaTeX \begin{example}
;; LaTeX   double dotprod(double* x, double* y, int n)
;; LaTeX   \{
;; LaTeX     int k;
;; LaTeX     double sum = 0;
;; LaTeX
;; LaTeX     for (k = 0; k < n; ++k) \{
;; LaTeX       sum += x[k] * y[k];
;; LaTeX     \}
;; LaTeX   \}
;; LaTeX \end{example}
;; LaTeX The following example generates two large arrays in Lisp, and calls the C
;; LaTeX function to do the desired computation.  This would not have been
;; LaTeX possible using \code{malloc} or \code{make-alien} since we need about
;; LaTeX 16 MB of memory to hold the two arrays.
;; LaTeX \begin{example}
;; LaTeX   (define-alien-routine \"dotprod\" double
;; LaTeX     (x (* double-float) :in)
;; LaTeX     (y (* double-float) :in)
;; LaTeX     (n int :in))
;; LaTeX
;; LaTeX   (let ((x (make-array 1000000 :element-type 'double-float))
;; LaTeX         (y (make-array 1000000 :element-type 'double-float)))
;; LaTeX     ;; Initialize X and Y somehow
;; LaTeX     (let ((x-addr (system:int-sap (array-data-address x)))
;; LaTeX           (y-addr (system:int-sap (array-data-address y))))
;; LaTeX       (dotprod x-addr y-addr 1000000)))
;; LaTeX \end{example}
;; LaTeX In this example, it may be useful to wrap the inner \code{let}
;; LaTeX expression in an \code{unwind-protect} that first turns off garbage
;; LaTeX collection and then turns garbage collection on afterwards.  This will
;; LaTeX prevent garbage collection from moving \code{x} and \code{y} after we
;; LaTeX have obtained the (now erroneous) addresses but before the call to
;; LaTeX \code{dotprod} is made.
;; LaTeX


(defsection @calling-lisp-from-c (:title "Calling Lisp From C")
  "SBCL supports the calling of Lisp functions using the C calling
  convention. This is useful for both defining callbacks and for creating
  an interface for calling into Lisp as a shared library directly from C.

  The DEFINE-ALIEN-CALLABLE macro wraps Lisp code and creates a C
  foreign function which can be called with the C calling convention.
  On x86-64 and ARM64, callbacks may receive and return structures by
  value."
  (define-alien-callable macro)
  "The ALIEN-CALLABLE-FUNCTION function returns the foreign callable
  value associated with any name defined by DEFINE-ALIEN-CALLABLE, so
  that we can, for example, pass the callable value to C as a
  callback."
  (alien-callable-function function)
  "The WITH-ALIEN-CALLABLE macro wraps Lisp code and establishes
  local C foreign functions which can be called with the C calling
  convention. This macro is handy for passing callbacks which close over
  Lisp values into C."
  (with-alien-callable macro)
  "Note that the garbage collector moves objects, and won't be able to fix
  up any references in C variables. There are three mechanisms for
  coping with this:

  - SB-EXT:PURIFY moves all live Lisp data into static or read-only
    areas such that it will never be moved (or freed) again in the
    life of the Lisp session

  - SB-SYS:WITH-PINNED-OBJECTS is a macro which arranges for some set
    of objects to be pinned in memory for the dynamic extent of its
    body forms. On ports which use the generational garbage
    collector (most, as of this writing) this affects exactly the
    specified objects. On other ports it is implemented by turning off
    GC for the duration (so could be said to have a whole-world
    granularity).

  - Disable GC, using the SB-EXT:WITHOUT-GCING macro."
  (@lisp-as-a-shared-library section))

(defsection @lisp-as-a-shared-library (:title "Lisp as a Shared Library")
  "SBCL supports the use of Lisp as a shared library that can be used by
  C programs using the DEFINE-ALIEN-CALLABLE interface. See the
  :CALLABLE-EXPORTS argument of SB-EXT:SAVE-LISP-AND-DIE for how to
  save the Lisp image in a way that allows a C program to initialize
  the Lisp runtime and the exported symbols. When SBCL is built as a
  library, it exposes the symbol `initialize_lisp` which can be used
  in conjunction with a core initializing global symbols to foreign
  callables as function pointers and with object code allocating those
  symbols to initialize the runtime properly. The arguments to
  `initialize_lisp` are the same as the arguments to the main `sbcl`
  program.

  > _Note_: There is currently no way to run exit hooks or otherwise
  > undo Lisp initialization gracefully from C.")

(defsection @step-by-step-example-of-the-foreign-function-interface
    (:title "Step-By-Step Example of the Foreign Function Interface")
  "This section presents a complete example of an interface to a somewhat
  complicated C function.

  Suppose you have the following C function which you want to be able
  to call from Lisp in the file `test.c`:

      struct c_struct
      {
        int x;
        char *s;
      };

      struct c_struct *c_function (i, s, r, a)
          int i;
          char *s;
          struct c_struct *r;
          int a[10];
      {
        int j;
        struct c_struct *r2;

        printf(\"i = %d\n\", i);
        printf(\"s = %s\n\", s);
        printf(\"r->x = %d\n\", r->x);
        printf(\"r->s = %s\n\", r->s);
        for (j = 0; j < 10; j++) printf(\"a[%d] = %d.\n\", j, a[j]);
        r2 = (struct c_struct *) malloc (sizeof(struct c_struct));
        r2->x = i + 5;
        r2->s = \"a C string\";
        return(r2);
      };

  It is possible to call this C function from Lisp using the file
  `test.lisp` containing

      (cl:defpackage \"TEST-C-CALL\" (:use \"CL\" \"SB-ALIEN\" \"SB-C-CALL\"))
      (cl:in-package \"TEST-C-CALL\")

      ;;; Define the record C-STRUCT in Lisp.
      (define-alien-type nil
          (struct c-struct
                  (x int)
                  (s c-string)))

      ;;; Define the Lisp function interface to the C routine.  It returns a
      ;;; pointer to a record of type C-STRUCT.  It accepts four parameters:
      ;;; I, an int; S, a pointer to a string; R, a pointer to a C-STRUCT
      ;;; record; and A, a pointer to the array of 10 ints.
      ;;;
      ;;; The INLINE declaration eliminates some efficiency notes about heap
      ;;; allocation of alien values.
      (declaim (inline c-function))
      (define-alien-routine c-function
          (* (struct c-struct))
        (i int)
        (s c-string)
        (r (* (struct c-struct)))
        (a (array int 10)))

      ;;; a function which sets up the parameters to the C function and
      ;;; actually calls it
      (defun call-cfun ()
        (with-alien ((ar (array int 10))
                     (c-struct (struct c-struct)))
          (dotimes (i 10)                     ; Fill array.
            (setf (deref ar i) i))
          (setf (slot c-struct 'x) 20)
          (setf (slot c-struct 's) \"a Lisp string\")

          (with-alien ((res (* (struct c-struct))
                            (c-function 5 \"another Lisp string\" (addr c-struct) ar)))
            (format t \"~&back from C function~%\")
            (multiple-value-prog1
                (values (slot res 'x)
                        (slot res 's))

              ;; Deallocate result. (after we are done referring to it:
              ;; \"Pillage, *then* burn.\")
              (free-alien res)))))

  To execute the above example, it is necessary to compile the C
  routine, e.g. with `cc -c test.c && ld -shared -o test.so test.o`.
  In order to enable incremental loading with some linkers, you may
  need to say `cc -G 0 -c test.c`.

  Once the C code has been compiled, you can start up Lisp and load it
  in: `sbcl`. Lisp should start up with its normal prompt.

  Within Lisp, compile the Lisp file:

      (compile-file \"test.lisp\")

  This step can be done separately. You don't have to recompile every
  time.

  Within Lisp, load the foreign object file to define the necessary
  symbols:

      (load-shared-object \"test.so\")

  Now you can load the compiled Lisp (fasl) file into Lisp:

      (load \"test.fasl\")

  And once the Lisp file is loaded, you can call the Lisp routine
  that sets up the parameters and calls the C function:

      (test-c-call::call-cfun)

  The C routine should print the following information to standard output:

      i = 5
      s = another Lisp string
      r->x = 20
      r->s = a Lisp string
      a[0] = 0.
      a[1] = 1.
      a[2] = 2.
      a[3] = 3.
      a[4] = 4.
      a[5] = 5.
      a[6] = 6.
      a[7] = 7.
      a[8] = 8.
      a[9] = 9.

  After return from the C function,
  the Lisp wrapper function should print the following output:

      back from C function

  And upon return from the Lisp wrapper function,
  before the next prompt is printed, the
  Lisp read-eval-print loop should print the following return values:

      10
      \"a C string\"")
