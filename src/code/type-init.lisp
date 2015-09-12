;;;; When this file's top level forms are run, it precomputes the
;;;; translations for commonly used type specifiers. This stuff is
;;;; split off from the other type stuff to get around problems with
;;;; everything needing to be loaded before everything else. This
;;;; stuff is also somewhat implementation-dependent in that
;;;; implementations may want to precompute other types which are
;;;; important to them.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;; built-in symbol type specifiers

;;; Predefined types that are of kind :INSTANCE can't have their
;;; :BUILTIN property set, so we cull them out. This used to operate
;;; on all *!STANDARD-TYPE-NAMES* because !PRECOMPUTE-TYPES was ok to
;;; call on unknown types. This relied upon the knowledge that
;;; VALUES-SPECIFIER-TYPE avoided signaling a PARSE-UNKNOWN-TYPE
;;; condition while in cold-init. This is terrible! It means that
;;; (a) we have to know that something wouldn't signal
;;;     when it otherwise should, and
;;; (b) we can call that thing when the very data that it depends on
;;;     are actually wrong.
;;; Well, in as much as we have to do this suspicious action,
;;; at least let's not get into a state where we *know*
;;; that it ought to signal.

(/show0 "precomputing built-in symbol type specifiers")
(!precompute-types
 (remove-if (lambda (x)
              (memq x '(hash-table package pathname random-state readtable)))
            *!standard-type-names*))

#+sb-xc-host (setf *type-system-initialized* t)

(/show0 "done with type-init.lisp")
