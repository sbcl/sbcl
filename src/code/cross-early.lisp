;;;; cross-compile-time-only stuff that is needed before anything else

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;; The STRUCTURE!OBJECT abstract class is the base of the hierarchy
;;; of objects that need to be identifiable as SBCL system objects
;;; in the host Lisp. This type does not exist in the target.
(defstruct (structure!object (:constructor nil) (:copier nil) (:predicate nil)))

(declaim (declaration truly-dynamic-extent))

;;; MAYBE-INLINE and FREEZE-TYPE declarations can be safely ignored
;;; (possibly at some cost in efficiency).
(declaim (declaration freeze-type maybe-inline))

;;; INHIBIT-WARNINGS declarations can be safely ignored (although we
;;; may then have to wade through some irrelevant warnings).
(declaim (declaration inhibit-warnings))

;;; SB!C::LAMBDA-LIST declarations can be ignored.
;;; Cross-compilation does not rely on introspection for anything.
(declaim (declaration sb!c::lambda-list))

(declaim (declaration explicit-check always-bound))

(defgeneric sb!xc:make-load-form (obj &optional env))
