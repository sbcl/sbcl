(in-package :sb-manual)

(defsection @contributed-modules (:title "Contributed Modules")
  "SBCL comes with a number of modules that are not part of the core
  system. These are loaded via `(REQUIRE :<MODULENAME>)`
  (see @CUSTOMIZATION-HOOKS-FOR-USERS). This section contains
  documentation (or pointers to documentation) for some of the
  contributed modules."
  #+#.(sb-manual::package-exists-p/reader '#:sb-aclrepl)
  (@sb-aclrepl section)
  #+#.(sb-manual::package-exists-p/reader '#:sb-concurrency)
  (@sb-concurrency section)
  #+#.(sb-manual::package-exists-p/reader '#:sb-cover)
  (@sb-cover section)
  #+#.(sb-manual::package-exists-p/reader '#:sb-grovel)
  (@sb-grovel section)
  #+#.(sb-manual::package-exists-p/reader '#:sb-introspect)
  (@sb-introspect section)
  (@sb-manual section)
  #+#.(sb-manual::package-exists-p/reader '#:sb-md5)
  (@sb-md5 section)
  #+#.(sb-manual::package-exists-p/reader '#:sb-posix)
  (@sb-posix section)
  #+#.(sb-manual::package-exists-p/reader '#:sb-queue)
  (@sb-queue section)
  #+#.(sb-manual::package-exists-p/reader '#:sb-rotate-byte)
  (@sb-rotate-byte section)
  #+#.(sb-manual::package-exists-p/reader '#:sb-simd)
  (@sb-simd section))
