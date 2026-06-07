(in-package :sb-manual)

(defsection @sbcl-manual (:title "SBCL Manual")
  #.(format nil "This manual – for SBCL version `~A`, generated _~A_ –
  is part of the SBCL software system. See the `\\\\README` file for
  more information.

  This manual is largely derived from the manual for the CMUCL system,
  which was produced at Carnegie Mellon University and later released
  into the public domain. This manual is in the public domain and is
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
