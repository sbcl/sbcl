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

(def!constant +backend-fasl-file-implementation+ :x86-64)

(setf *backend-register-save-penalty* 3)

(setf *backend-byte-order* :little-endian)

;;; KLUDGE: It would seem natural to set this by asking our C runtime
;;; code for it, but mostly we need it for GENESIS, which doesn't in
;;; general have our C runtime code running to ask, so instead we set
;;; it by hand. -- WHN 2001-04-15
;;;
;;; Though note that POSIX specifies (as far as I can tell)
;;;
;;;   sysconf(_SC_PAGE_SIZE);
;;;
;;; as a portable way of retrieving this information; a call to this
;;; could be made in grovel-headers (which, strictly speaking, would
;;; no longer solely be grovelling headers), though the question of
;;; how to make this information appear in GENESIS, which is built and
;;; run from host-1 files (which are made before grovel-headers runs)
;;; would remain.  -- CSR, 2002-09-01
(setf *backend-page-size* 4096)
;;; comment from CMU CL:
;;;
;;;   in case we ever wanted to do this for Windows NT..
;;;
;;;   Windows NT uses a memory system granularity of 64K, which means
;;;   everything that gets mapped must be a multiple of that. The real
;;;   page size is 512, but that doesn't do us a whole lot of good.
;;;   Effectively, the page size is 64K.
;;;
;;;   would be: (setf *backend-page-size* 65536)
