;;;; that part of the parms.lisp file from original CMU CL which is defined in
;;;; terms of the BACKEND structure
;;;;
;;;; FIXME: When we break up the BACKEND structure, this might be mergeable
;;;; back into the parms.lisp file.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; compiler constants

(def!constant +backend-fasl-file-implementation+ :x86)

(setf *backend-register-save-penalty* 3)

(setf *backend-byte-order* :little-endian)

;;; KLUDGE: It would seem natural to set this by asking our C runtime
;;; code for it, but mostly we need it for GENESIS, which doesn't in
;;; general have our C runtime code running to ask, so instead we set
;;; it by hand. -- WHN 2001-04-15
;;;
;;; Actually any information that we can retrieve C-side would be
;;; useless in SBCL, since it's possible for otherwise binary
;;; compatible systems to return different values for getpagesize().
;;; -- JES, 2007-01-06
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *backend-page-bytes* 4096))
;;; comment from CMU CL:
;;;
;;;   in case we ever wanted to do this for Windows NT..
;;;
;;;   Windows NT uses a memory system granularity of 64K, which means
;;;   everything that gets mapped must be a multiple of that. The real
;;;   page size is 512, but that doesn't do us a whole lot of good.
;;;   Effectively, the page size is 64K.
;;;
;;;   would be: (setf *backend-page-bytes* 65536)

;;; The size in bytes of GENCGC cards, i.e. the granularity at which
;;; writes to old generations are logged.  With mprotect-based write
;;; barriers, this must be a multiple of the OS page size.
(def!constant gencgc-card-bytes *backend-page-bytes*)
;;; The minimum size of new allocation regions.  While it doesn't
;;; currently make a lot of sense to have a card size lower than
;;; the alloc granularity, it will, once we are smarter about finding
;;; the start of objects.
(def!constant gencgc-alloc-granularity 0)
;;; The minimum size at which we release address ranges to the OS.
;;; This must be a multiple of the OS page size.
(def!constant gencgc-release-granularity *backend-page-bytes*)
