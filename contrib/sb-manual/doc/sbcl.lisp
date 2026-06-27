(in-package :sb-manual)

(defsection @sbcl-manual (:title "SBCL Manual")
  ;; This docstring is not used in the Texinfo version (see
  ;; EMIT-TEXINFO-FOR-SECTION).
  #.(format nil "This is an unofficial rendering of the SBCL Manual
  using [MGL-PAX][pax::@pax-manual] with the same content as the
  official version at <https://www.sbcl.org/> but with heavy linking
  internally, to the CLHS, and to the source code on
  [GitHub](https://github.com/sbcl/sbcl).

  The output is for SBCL version `~A`, generated _~A_. See
  <https://fixnum.com> for this document in other formats.

  This manual is part of the SBCL software system. See the
  `\\\\README` file for more information. The manual is largely
  derived from the manual for the [CMUCL](https://cmucl.org/) system,
  which was produced at Carnegie Mellon University and later released
  into the public domain. The manual is in the public domain and is
  provided with absolutely no warranty. See the `\\\\COPYING` and
  `\\\\CREDITS` files for more information."
            (lisp-implementation-version)
            (documentation-generation-date-string :long t))
  (@support-and-bugs section)
  (@introduction section)
  (@starting-and-stopping section)
  (@compiler section)
  (@debugger section)
  (@efficiency section)
  (@beyond-the-ansi-standard section)
  (@external-formats section)
  (@foreign-function-interface section)
  (@pathnames section)
  (@streams section)
  (@package-locks section)
  (@threading section)
  (@timers section)
  (@networking section)
  (@profiling section)
  (@contributed-modules section)
  (@deprecation section))
